use crate::axiom::ItemResult;
use crate::derivation::*;
use crate::types::container::associative as assoc;
use crate::types::container::error::Error;
use crate::types::container::pipe::FutureTake;
use crate::types::number::Int;
use crate::types::{self, Bytes, Item};
use crate::{fit, list};
use futures::future::FutureExt;
use std::future::{self};
use std::net::{Ipv4Addr, SocketAddrV4};
use std::pin::Pin;
use std::ptr;
use std::str::FromStr;
use std::sync::Arc;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::net::{TcpListener, TcpStream};
use tokio::sync::RwLock;

#[derive(Debug)]
pub struct Socket {
    pub socket: TcpStream,
    pub addr: (String, u16),
}

impl PartialEq for Socket {
    fn eq(&self, other: &Self) -> bool {
        // Check if the 'socket' fields of both structs are the same by reference
        ptr::eq(&self.socket, &other.socket)
    }
}

impl FutureTake for Socket {
    type Item = Bytes;
    fn take_future<'a>(
        &'a mut self,
    ) -> Pin<Box<dyn std::future::Future<Output = Result<Option<Self::Item>, Error>> + Send + 'a>>
    {
        let mut bs = [0u8; 1024];
        Box::pin(async move {
            let n = self.socket.read(&mut bs).await?;
            if n == 0 {
                Ok(None)
            } else {
                Ok(Some(bs[..n].to_vec()))
            }
        })
    }
}

impl Socket {
    pub fn put(&mut self, i: Item) -> types::Future<'_, Result<(), Error>> {
        //println!("Putting {:?}", i);
        let b = types::Bytes::try_derive(i);
        match b {
            Ok(bs) => {
                Box::pin(async move { self.socket.write_all(&bs).await.map_err(|e| e.into()) })
            }
            Err(e) => Box::pin(future::ready(Err(e))),
        }
    }
}

impl crate::serialize::Display for Socket {
    fn representation(&self) -> Item {
        assoc::Association::derive_iter([
            (fit!("type"), fit!("tunnel")),
            (fit!("realm"), fit!("tcp")),
            (fit!("address"), self.addr.0.to_string().fit()),
            (fit!("port"), self.addr.1.to_string().fit()),
        ])
        .fit()
    }
}

// Server sockets
#[derive(Debug)]
pub struct ServerSocket {
    pub socket: TcpListener,
}

impl PartialEq for ServerSocket {
    fn eq(&self, other: &Self) -> bool {
        // Check if the 'socket' fields of both structs are the same by reference
        ptr::eq(&self.socket, &other.socket)
    }
}

impl FutureTake for ServerSocket {
    type Item = Socket;

    fn take_future<'a>(
        &'a mut self,
    ) -> Pin<Box<dyn std::future::Future<Output = Result<Option<Self::Item>, Error>> + Send + 'a>>
    {
        Box::pin(async move {
            let (socket, addr) = self.socket.accept().await?;

            Ok(Some(Socket {
                socket,
                addr: (addr.ip().to_string(), addr.port()),
            }))
        })
    }
}

impl crate::serialize::Display for ServerSocket {
    fn representation(&self) -> Item {
        let r = self.socket.local_addr();
        assoc::Association::derive_iter([
            (fit!("type"), fit!("pipe")),
            (fit!("realm"), fit!("tcp")),
            (fit!("produces"), fit!("socket")),
            (fit!("pipe-type"), fit!("serversocket")),
            (
                fit!("address"),
                match r {
                    Ok(a) => a.ip().to_string().fit(),
                    Err(_) => fit!("unknown"),
                },
            ),
            (
                fit!("port"),
                match r {
                    Ok(a) => a.port().to_string().fit(),
                    Err(_) => fit!("unknown"),
                },
            ),
        ])
        .fit()
    }
}

fn socket_addr(i: Item, j: Item) -> Result<SocketAddrV4, Error> {
    //println!("socket: {:?} {:?}", i, j);
    let addr = Ipv4Addr::from_str(String::try_derive(j)?.as_str())?;
    let port = Int::try_derive(i)? as u16;
    Ok(SocketAddrV4::new(addr, port))
}

fn host_addr(i: Item, j: Item) -> Result<(String, u16), Error> {
    //println!("socket: {:?} {:?}", i, j);
    let addr = String::try_derive(j)?;
    let port = Int::try_derive(i)? as u16;
    Ok((addr, port))
}

pub fn server_socket(i: Item, j: Item) -> types::Sometime<'static, ItemResult> {
    match socket_addr(i, j) {
        Ok(addr) => types::Sometime::Future(Box::pin(TcpListener::bind(addr).map(|r| match r {
            Ok(s) => Ok(
                super::Out::ServerSocket(Arc::new(RwLock::new(ServerSocket { socket: s }))).fit(),
            ),
            Err(e) => Err(e.into()),
        }))),
        Err(e) => types::Sometime::Now(Err(e)),
    }
}

pub fn socket(i: Item, j: Item) -> types::Sometime<'static, ItemResult> {
    match host_addr(i, j) {
        Ok(addr) => types::Sometime::Future(Box::pin(TcpStream::connect(addr.clone()).map(
            move |r| match r {
                Ok(s) => Ok(
                    super::Tunnel::Socket(Arc::new(RwLock::new(Socket { socket: s, addr }))).fit(),
                ),
                Err(e) => Err(e.into()),
            },
        ))),
        Err(e) => types::Sometime::Now(Err(e)),
    }
}

// pub fn server_socket(env: Environment) -> environment::Future {
//     let addr = env.pop();

//     let inner_env = Environment::try_derive(tos);
//     match inner_env {
//         Ok(inner) => Box::pin(eval_step(inner).map(|inner_next| env.push(Item::Env(inner_next)))),
//         Err(e) => env.push(Item::Error(e)).fit(),
//     }
// }

impl From<std::net::AddrParseError> for Error {
    fn from(err: std::net::AddrParseError) -> Error {
        Error::create(list!("addrparse"), &err.to_string(), Option::<Item>::None)
    }
}

impl Derive<Socket> for Item {
    fn derive(ss: Socket) -> Item {
        Item::Socket(Arc::new(RwLock::new(ss)))
    }
}
