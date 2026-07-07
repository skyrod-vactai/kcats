use crate::types::container::pipe::FutureTake;
use crate::types::container::Error;
use flume::{Receiver, Sender};
use libp2p::{
    core::transport::Transport,
    identity, noise,
    swarm::{Swarm, SwarmEvent},
    tcp, yamux, PeerId, StreamProtocol,
};
use std::io;
use std::pin::Pin;

pub struct P2P {
    read_receiver: Receiver<Vec<u8>>,
    write_sender: Sender<Vec<u8>>,
    peer_id: PeerId,
    protocol: StreamProtocol,
    local_peer_id: PeerId,
}

// Implement FutureTake for the LibP2PPipe
impl FutureTake for P2P {
    type Item = Vec<u8>;

    fn take_future<'a>(
        &'a mut self,
    ) -> Pin<Box<dyn std::future::Future<Output = Result<Option<Self::Item>, Error>> + Send + 'a>>
    {
        Box::pin(async move {
            match self.read_receiver.recv_async().await {
                Ok(data) => Ok(Some(data)),
                Err(flume::RecvError::Disconnected) => Ok(None), // Channel closed normally
            }
        })
    }
}
