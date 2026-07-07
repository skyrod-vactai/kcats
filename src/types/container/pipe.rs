use super::error::Error;
use crate::derivation::*;
use crate::fit;
use crate::types::container::{self as coll, SimpleTake};
use crate::types::{self, Item};
use std::pin::Pin;
use std::sync::Arc;
use tokio::sync::RwLock;

use futures::{executor, future};
pub mod channel;

#[cfg(feature = "database")]
pub mod db;

pub mod fs;
pub mod net;
pub mod standard;
pub mod time;

pub trait FutureTake {
    type Item;
    fn take_future(&mut self) -> types::Future<'_, Result<Option<Self::Item>, Error>>;
}

/// A pipe that accepts items.
#[derive(Debug, Clone)]
pub enum In {
    /// A pipe that takes bytes to write to a file on disk
    StaticFile(Arc<RwLock<fs::StaticFile>>),
    /// A pipe that takes bytes to write to a TCP/IP socket
    Socket(Arc<RwLock<net::Socket>>),
    /// A pipe that takes items to send through a channel to another
    /// part of the running program
    Handoff(Box<channel::Handoff<Item>>),
    /// A pipe that takes bytes to write to standard out
    Standard,
}

impl PartialEq for In {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (In::StaticFile(s1), In::StaticFile(s2)) => Arc::ptr_eq(s1, s2),
            (In::Socket(s1), In::Socket(s2)) => Arc::ptr_eq(s1, s2),
            (In::Handoff(h1), In::Handoff(h2)) => h1 == h2,
            _ => false,
        }
    }
}
/// A pipe that produces items.
#[derive(Debug, Clone)]
pub enum Out {
    /// A pipe that produces bytes from a file on disk
    StaticFile(Arc<RwLock<fs::StaticFile>>),
    /// A pipe that produces bytes from a TCP/IP socket
    Socket(Arc<RwLock<net::Socket>>),
    /// A pipe that produces sockets from a TCP/IP server socket
    ServerSocket(Arc<RwLock<net::ServerSocket>>),
    /// A pipe that produces items from a channel that comes from
    /// another part of the program
    Handoff(Box<channel::Handoff<Item>>),
    /// A pipe that produces a dummy value after a given amount of
    /// time. Can be used as a timeout mechanism when waiting on
    /// multiple pipes at once.
    Timer(Box<channel::Timer>),
    /// A pipe that produces timestamps of the current UNIX time
    Time,
    /// A pipe that produces bytes from standard in.
    Standard,
}

impl PartialEq for Out {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Out::StaticFile(s1), Out::StaticFile(s2)) => Arc::ptr_eq(s1, s2),
            (Out::Socket(s1), Out::Socket(s2)) => Arc::ptr_eq(s1, s2),
            (Out::ServerSocket(s1), Out::ServerSocket(s2)) => Arc::ptr_eq(s1, s2),
            (Out::Handoff(h1), Out::Handoff(h2)) => h1 == h2,
            (Out::Time, Out::Time) => true,
            (Out::Standard, Out::Standard) => true,
            _ => false,
        }
    }
}

/// A bi-directional pipe that can accept and produce Items.
#[derive(Debug, Clone)]
pub enum Tunnel {
    /// A pipe that can both produce and accept bytes to read/write from a file.
    StaticFile(Arc<RwLock<fs::StaticFile>>),
    /// A pipe that can both produce and accept bytes to read/write
    /// from a TCP/IP socket.
    Socket(Arc<RwLock<net::Socket>>),
    /// A pipe that produces or accepts values to/from a channel that
    /// connects to another part of the program.
    Handoff(Box<channel::Handoff<Item>>),
    /// A pipe to standard in/out that produces/accepts bytes.
    Standard,
}

impl PartialEq for Tunnel {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Tunnel::StaticFile(s1), Tunnel::StaticFile(s2)) => Arc::ptr_eq(s1, s2),
            (Tunnel::Socket(s1), Tunnel::Socket(s2)) => Arc::ptr_eq(s1, s2),
            (Tunnel::Handoff(h1), Tunnel::Handoff(h2)) => h1 == h2,
            (Tunnel::Standard, Tunnel::Standard) => true,
            _ => false,
        }
    }
}

impl Derive<Tunnel> for Out {
    fn derive(t: Tunnel) -> Self {
        match t {
            Tunnel::StaticFile(f) => Out::StaticFile(f),
            Tunnel::Socket(s) => Out::Socket(s),
            Tunnel::Handoff(h) => Out::Handoff(h),
            Tunnel::Standard => Out::Standard,
        }
    }
}

impl Derive<Tunnel> for In {
    fn derive(t: Tunnel) -> Self {
        match t {
            Tunnel::StaticFile(f) => In::StaticFile(f),
            Tunnel::Socket(s) => In::Socket(s),
            Tunnel::Handoff(h) => In::Handoff(h),
            Tunnel::Standard => In::Standard,
        }
    }
}

impl In {
    /// Puts the [Item] into the pipe. Blocks if the pipe is full.
    pub fn put(self, i: Item) -> types::Sometime<'static, Result<In, Error>> {
        match self {
            In::StaticFile(f) => types::Sometime::Future(Box::pin(async move {
                f.write()
                    .await
                    .put(i)
                    .await
                    .map(|_| In::StaticFile(f.clone()))
            })),
            In::Socket(s) => types::Sometime::Future(Box::pin(async move {
                s.write().await.put(i).await.map(|_| In::Socket(s.clone()))
            })),
            In::Handoff(mut h) => types::Sometime::Future(Box::pin(async move {
                h.put(i).await.map(|_| In::Handoff(h.clone()))
            })), //_ => Err(Error::expected("foo")),
            In::Standard => standard::put(i).map(|r| r.map(|_| In::Standard)),
        }
    }
}

impl FutureTake for Tunnel {
    /// Takes an [Item] from the tunnel, blocks if the receive side of
    /// the tunnel is empty.
    type Item = Item;
    /// Takes an [Item] from the pipe, blocks if the pipe is empty.
    fn take_future<'a>(
        &'a mut self,
    ) -> Pin<Box<dyn std::future::Future<Output = Result<Option<Self::Item>, Error>> + Send + 'a>>
    {
        match self {
            Tunnel::StaticFile(f) => Box::pin(async move {
                f.write()
                    .await
                    .take_future()
                    .await
                    .map(|i| Some(Item::derive(i)))
            }),
            Tunnel::Socket(f) => Box::pin(async move {
                f.write()
                    .await
                    .take_future()
                    .await
                    .map(|i| Some(Item::derive(i)))
            }),

            Tunnel::Handoff(h) => Box::pin(h.take_future()),
            Tunnel::Standard => standard::take_future(),
        }
    }
}

impl Tunnel {
    /// Puts the [Item] into the tunnel, blocks if the send side of
    /// the tunnel is full.
    pub fn put(self, i: Item) -> types::Sometime<'static, Result<Tunnel, Error>> {
        match self {
            Tunnel::StaticFile(f) => types::Sometime::Future(Box::pin(async move {
                f.write()
                    .await
                    .put(i)
                    .await
                    .map(|_| Tunnel::StaticFile(f.clone()))
            })),
            Tunnel::Socket(s) => types::Sometime::Future(Box::pin(async move {
                s.write()
                    .await
                    .put(i)
                    .await
                    .map(|_| Tunnel::Socket(s.clone()))
            })),
            Tunnel::Handoff(mut h) => types::Sometime::Future(Box::pin(async move {
                h.put(i).await.map(|_| Tunnel::Handoff(h.clone()))
            })), //_ => Err(Error::expected("foo")),
            Tunnel::Standard => standard::put(i).map(|r| r.map(|_| Tunnel::Standard)),
        }
    }
}

impl FutureTake for Out {
    type Item = Item;
    /// Takes an [Item] from the pipe, blocks if the pipe is empty.
    fn take_future<'a>(
        &'a mut self,
    ) -> Pin<Box<dyn std::future::Future<Output = Result<Option<Self::Item>, Error>> + Send + 'a>>
    {
        match self {
            Out::StaticFile(f) => {
                Box::pin(
                    async move { f.write().await.take_future().await.map(|bs| Some(bs.fit())) },
                )
            }
            Out::Socket(f) => {
                Box::pin(
                    async move { f.write().await.take_future().await.map(|bs| Some(bs.fit())) },
                )
            }
            Out::ServerSocket(f) => {
                Box::pin(async move { f.write().await.take_future().await.map(|s| Some(s.fit())) })
            }
            Out::Handoff(h) => Box::pin(h.take_future()),
            Out::Timer(ref mut t) => Box::pin(t.take_future()),
            Out::Time => Box::pin(future::ready(Ok(time::Time::new()
                .take_simple()
                .map(Item::derive)))),
            Out::Standard => Box::pin(standard::take_future()),
        }
    }
}

impl crate::serialize::Display for In {
    fn representation(&self) -> Item {
        match self {
            In::StaticFile(f) => executor::block_on(async move { f.read().await.representation() }),
            In::Socket(f) => executor::block_on(async move { f.read().await.representation() }),
            In::Handoff(h) => h.representation(),
            In::Standard => standard::representation(),
        }
    }
}

impl crate::serialize::Display for Out {
    fn representation(&self) -> Item {
        match self {
            Out::StaticFile(f) => {
                executor::block_on(async move { f.read().await.representation() })
            }
            Out::Socket(f) => executor::block_on(async move { f.read().await.representation() }),
            Out::ServerSocket(f) => {
                executor::block_on(async move { f.read().await.representation() })
            }
            Out::Handoff(h) => h.representation(),
            Out::Timer(t) => t.representation(),
            Out::Time => time::representation(),
            Out::Standard => standard::representation(),
        }
    }
}

impl crate::serialize::Display for Tunnel {
    fn representation(&self) -> Item {
        match self {
            Tunnel::StaticFile(f) => {
                executor::block_on(async move { f.read().await.representation() })
            }
            Tunnel::Socket(f) => executor::block_on(async move { f.read().await.representation() }),
            Tunnel::Handoff(h) => h.representation(),
            Tunnel::Standard => standard::representation(),
        }
    }
}
/* Pipes can be "closed", from either end to signal that either the
 * putter or taker has gone away. Sometimes the type of pipe
 * may not really support this concept but an implementation is
 * required.  For example, files. When you open a file for writing and
 * then "close" it, that doesn't really do anything. Rust doesn't have
 * an explicit file close. You have to drop the reference to it, which
 * in kcats you can do by popping the pipe off the stack. Rust will
 * clean up automatically, other impls might have to reference count.
 *
 * The contract here is as follows:
 * 1. After calling close, put on the pipe returns an error
 *
 * 2. After calling close, take on the pipe will return still-buffered
 * items (if the pipe has a buffer), but once buffer is exhausted it
 * will return error.
 *
 * 2. Errors cannot be put into a pipe (the taker can't distinguish
 * between io error and an error value). To work around this, wrap the
 * error value in a list to quote it. Putting error into a pipe will
 * return an io error.
 *
 * 3. Once closed pipes cannot be ever be put into again. closed? will always
 * return true thereafter.
 *
 * One use case that has to be handled specially is a file we've fully
 * read but later someone else might write more bytes to the end. Does
 * the pipe close when we reach EOF? I think we might need to support
 * both types (a type that closes when hitting eof and one that
 * doesn't). The former is the "normal" use case, which will be the
 * default.
 *
 * These two types are basically static vs dynamic content. Either all
 * the content is known now, or it isn't.
 *
*/

fn closed_error(on_take: bool) -> Error {
    let take_or_put: Item = if on_take { fit!("take") } else { fit!("put") };
    Error::create(
        coll::List::derive_iter([fit!("close"), take_or_put]),
        "attempt to use closed pipe",
        Option::<Item>::None,
    )
}

impl Derive<Tunnel> for Item {
    fn derive(t: Tunnel) -> Self {
        match t {
            Tunnel::Handoff(h) => Item::Handoff(h),
            Tunnel::Socket(s) => Item::Socket(s),
            Tunnel::StaticFile(f) => Item::StaticFile(f),
            Tunnel::Standard => Item::Standard,
        }
    }
}

impl Derive<Out> for Item {
    fn derive(t: Out) -> Self {
        match t {
            Out::Handoff(h) => Item::Handoff(h),
            Out::ServerSocket(s) => Item::ServerSocket(s),
            Out::Socket(s) => Item::Socket(s),
            Out::StaticFile(f) => Item::StaticFile(f),
            Out::Timer(t) => Item::Timer(t),
            Out::Standard => Item::Standard,
            Out::Time => Item::Time,
        }
    }
}

impl Derive<In> for Item {
    fn derive(t: In) -> Self {
        match t {
            In::Handoff(h) => Item::Handoff(h),
            In::Socket(s) => Item::Socket(s),
            In::StaticFile(f) => Item::StaticFile(f),
            In::Standard => Item::Standard,
        }
    }
}

impl TryDerive<Item> for In {
    fn try_derive(i: Item) -> Result<Self, Error> {
        match i {
            Item::Handoff(h) => Ok(In::Handoff(h)),
            Item::Socket(s) => Ok(In::Socket(s)),
            Item::StaticFile(f) => Ok(In::StaticFile(f)),
            Item::Standard => Ok(In::Standard),
            i => Err(Error::expected(fit!("pipe"), i)),
        }
    }
}

impl TryDerive<Item> for Out {
    fn try_derive(i: Item) -> Result<Self, Error> {
        match i {
            Item::Handoff(h) => Ok(Out::Handoff(h)),
            Item::ServerSocket(s) => Ok(Out::ServerSocket(s)),
            Item::Socket(s) => Ok(Out::Socket(s)),
            Item::StaticFile(f) => Ok(Out::StaticFile(f)),
            Item::Timer(t) => Ok(Out::Timer(t)),
            Item::Standard => Ok(Out::Standard),
            Item::Time => Ok(Out::Time),
            i => Err(Error::expected(fit!("pipe"), i)),
        }
    }
}
