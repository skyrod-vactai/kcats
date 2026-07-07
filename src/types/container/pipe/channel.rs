use crate::axiom;
use crate::derivation::*;
use crate::fit;
use crate::types::container as coll;
use crate::types::container::error::Error;
use crate::types::container::pipe::FutureTake;
use crate::types::container::{associative as assoc, environment::Environment, error, pipe};
use crate::types::number::Int;
use crate::types::{self, Item};

use flume;
use std::future;
use std::pin::Pin;
use std::ptr;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use tokio::task::JoinHandle;
use tokio::time::{sleep, Duration};

#[derive(Debug, Clone)]
// Use Option because we want to be able to drop senders/receivers to
// close the channel
pub struct Handoff<T> {
    pub receiver: Option<flume::Receiver<T>>,
    pub sender: Option<flume::Sender<T>>,
    pub bidirectional: bool,
    pub id: usize,
}

impl<T> PartialEq for Handoff<T> {
    fn eq(&self, other: &Self) -> bool {
        match (&self.receiver, &other.receiver, &self.sender, &other.sender) {
            (Some(sr), Some(or), Some(ss), Some(os)) => ptr::eq(&sr, &or) && ptr::eq(&ss, &os),
            _ => false,
        }
    }
}

static ID: AtomicUsize = AtomicUsize::new(0);

impl FutureTake for Handoff<Item> {
    type Item = Item;
    fn take_future<'a>(
        &'a mut self,
    ) -> Pin<Box<dyn std::future::Future<Output = Result<Option<Self::Item>, Error>> + Send + 'a>>
    {
        // println!(
        //     "Taking from channel: {:?} on {:?}",
        //     self,
        //     thread::current().id()
        // );
        if !self.bidirectional {
            self.close_put();
        }
        if let Some(ch) = self.receiver.clone() {
            Box::pin(async move { ch.recv_async().await.map(Some).or_else(|_| Ok(None)) })
        } else {
            Box::pin(future::ready(Ok(None)))
        }
    }
}

impl Handoff<Item> {
    pub fn new(bidirectional: bool) -> Handoff<Item> {
        let (sender, receiver) = flume::bounded::<Item>(0);
        let id = ID.fetch_add(1, Ordering::Relaxed);
        Handoff::<Item> {
            sender: Some(sender),
            receiver: Some(receiver),
            bidirectional,
            id,
        }
    }

    pub fn put(&mut self, i: Item) -> types::Sometime<'static, Result<(), error::Error>> {
        // println!(
        //     "Putting into channel: {} into {:?} on {:?}",
        //     i.clone(),
        //     self,
        //     thread::current().id()
        // );
        if !self.bidirectional {
            self.close_take()
        };
        if let Some(ch) = self.sender.clone() {
            if axiom::is_truthy(&i) {
                types::Sometime::Future(Box::pin(async move {
                    ch.send_async(i)
                        .await
                        .map_err(|_| pipe::closed_error(false))
                }))
            } else {
                // If we're putting 'nothing', that indicates end of
                // input, so we drop the sender.
                self.close_put();
                types::Sometime::Now(Ok(()))
            }
        } else {
            types::Sometime::Now(Err(pipe::closed_error(false)))
        }
    }

    pub fn close_take(&mut self) {
        if self.receiver.is_some() {
            //println!("Dropping receiver");
            self.receiver = None;
        }
    }

    pub fn close_put(&mut self) {
        if self.sender.is_some() {
            //println!("Dropping sender");
            self.sender = None;
        }
    }
}

impl crate::serialize::Display for Handoff<Item> {
    fn representation(&self) -> Item {
        let mut props = [
            (fit!("type"), fit!("pipe")),
            (
                fit!("senders"),
                self.sender
                    .as_ref()
                    .map(|s| s.sender_count() as Int)
                    .unwrap_or_default()
                    .fit(),
            ),
            (
                fit!("receivers"),
                self.receiver
                    .as_ref()
                    .map(|s| s.receiver_count() as Int)
                    .unwrap_or_default()
                    .fit(),
            ),
            (
                fit!("endpoint"),
                assoc::Association::derive_iter([(fit!("handoff"), (self.id as Int).fit())]).fit(),
            ),
        ]
        .to_vec();

        if self.sender.is_some() {
            props.push((fit!("in"), fit!("item")));
            props.push((
                fit!("capacity"),
                (self.sender.as_ref().unwrap().capacity().unwrap_or_default() as Int).fit(),
            ));
        }
        if self.receiver.is_some() {
            props.push((fit!("out"), fit!("item")));
            props.push((
                fit!("capacity"),
                (self
                    .receiver
                    .as_ref()
                    .unwrap()
                    .capacity()
                    .unwrap_or_default() as Int)
                    .fit(),
            ));
        }

        assoc::Association::derive_iter(props).fit()
    }
}

pub fn handoff(mut env: Environment) -> types::Sometime<'static, Environment> {

    env.push(pipe::Tunnel::Handoff(Box::new(Handoff::new(false))));
    env.fit()
}

impl From<flume::RecvError> for error::Error {
    fn from(_: flume::RecvError) -> Self {
        pipe::closed_error(false) // todo fix this
    }
}

impl From<flume::SendError<Item>> for error::Error {
    fn from(_: flume::SendError<Item>) -> Self {
        pipe::closed_error(false)
    }
}

enum ChannelOp<T> {
    Send(Arc<flume::Sender<T>>, T),
    Receive(Arc<flume::Receiver<T>>),
}

/// Given a list of pipes (channels) on top of stack, use flume's
/// selector to choose the next ready pipe.  A pipe means it's a
/// receive, a pipe/item pair means it's a send.
pub fn select(i: Item) -> axiom::ItemResult {
    let l = coll::List::try_derive(i)?;
    let original = l.clone();

    //Create references out of any [pipe item] pairs
    let lr = l
        .iter()
        .cloned()
        .map(move |i| match i {
            Item::Handoff(p) => Ok(ChannelOp::Receive(Arc::new(p.receiver.unwrap()))),

            // Handle timeout channels - start the timer and add receive op
            Item::Timer(t) => {
                let mut t = t.clone();
                t.start();
                Ok(ChannelOp::Receive(Arc::new(t.receiver.unwrap())))
            }
            i => {
                let l = coll::List::try_derive(i.clone())?;
                let p = l.front();
                let i = l.get(1);
                match (p, i) {
                    (Some(p), Some(i)) => match (p, i) {
                        (Item::Handoff(p), i) => Ok(ChannelOp::Send(
                            Arc::new(p.sender.clone().unwrap()),
                            i.clone(),
                        )),

                        (p, _i) => Err(error::Error::expected(fit!("handoff"), p.clone())),
                    },
                    _ => Err(error::Error::short_list(2)),
                }
            }
        })
        .collect::<Result<Vec<ChannelOp<Item>>, error::Error>>()?;

    let (res, idx) = {
        let mut selector = flume::Selector::new();

        // loop over the operations and add them to the selector. Each one
        // returns the original index in the list, so we can use that to
        // fetch the original item from the list.
        for (idx, item) in lr.iter().enumerate() {
            let idx_clone = idx;
            match item {
                ChannelOp::Receive(r) => {
                    selector = selector.recv(r, move |i| {
                        (i.map(Some).map_err(error::Error::from), idx_clone)
                    });
                }
                ChannelOp::Send(s, i) => {
                    selector = selector.send(s, i.clone(), move |i| {
                        (i.map(|_| None).map_err(error::Error::from), idx)
                    });
                }
            }
        }

        selector.wait()
    };
    let selected = original.get(idx).unwrap().clone();
    match res {
        Ok(Some(i)) => {
            let l: Item = coll::List::derive_iter(vec![selected, i]).fit();
            Ok(l)
        }
        Ok(None) => Ok(selected),
        Err(e) => Err(e),
    }
}

impl TryDerive<Item> for Handoff<Item> {
    fn try_derive(i: Item) -> Result<Self, Error> {
        match i {
            Item::Handoff(p) => Ok(*p),
            i => Err(Error::expected(fit!("handoff"), i)),
        }
    }
}

// drop the receiver side of the handoff and return the handoff item
pub fn sender(i: Item) -> axiom::ItemResult {
    let mut h = Handoff::try_derive(i).map_err(Error::derive)?;
    h.close_take();
    Ok(Item::Handoff(Box::new(h)))
}

// drop the sender side of the handoff and return the handoff item
pub fn receiver(i: Item) -> axiom::ItemResult {
    let mut h = Handoff::try_derive(i).map_err(Error::derive)?;
    h.close_put();
    Ok(Item::Handoff(Box::new(h)))
}

#[derive(Debug)]
pub struct Timer {
    receiver: Option<flume::Receiver<Item>>,
    handle: Option<JoinHandle<()>>,
    duration: Duration,
}

// Cloning a timeout makes a new one, clears state
impl Clone for Timer {
    fn clone(&self) -> Self {
        Self {
            receiver: None,
            handle: None,
            duration: self.duration,
        }
    }
}
impl FutureTake for Timer {
    type Item = Item;
    fn take_future<'a>(
        &'a mut self,
    ) -> Pin<Box<dyn std::future::Future<Output = Result<Option<Self::Item>, Error>> + Send + 'a>>
    {
        self.start();
        let receiver = self.receiver.clone().unwrap();
        Box::pin(async move {
            //println!("Receiving");
            receiver.recv_async().await.map(Some).or_else(|_| Ok(None))
        })
    }
}

impl Timer {
    fn new(duration: Duration) -> Timer {
        Timer {
            receiver: None,
            handle: None,
            duration,
        }
    }

    fn start(&mut self) {
        if self.handle.is_none() {
            let (sender, receiver) = flume::bounded(1);
            let duration = self.duration;
            self.receiver = Some(receiver);
            self.handle = Some(tokio::spawn(async move {
                sleep(duration).await;
                //TODO handle error condition on send
                let _ = sender.send(Item::default());
            }));
        }
    }
}

impl Derive<Timer> for Item {
    fn derive(t: Timer) -> Self {
        Item::Timer(Box::new(t))
    }
}

impl crate::serialize::Display for Timer {
    fn representation(&self) -> Item {
        assoc::Association::derive_iter([
            (fit!("type"), fit!("pipe")),
            (fit!("timeout"), (self.duration.as_millis() as Int).fit()),
        ])
        .fit()
    }
}

pub fn timer(i: Item) -> axiom::ItemResult {
    let ms = Int::try_derive(i).map_err(Error::derive)?;
    //TODO: check for negative values
    Ok(Timer::new(Duration::from_millis(ms as u64)).fit())
}
