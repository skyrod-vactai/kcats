use crate::types::container::associative as assoc;
use crate::types::container::error::Error;
use crate::types::{self, *};
use std::future;
use std::io::{self, Read};
use tokio::io::AsyncWriteExt; // AsyncWriteExt brings in write_all

pub fn take_future<'a>(
) -> Pin<Box<dyn std::future::Future<Output = Result<Option<Item>, Error>> + Send + 'a>> {
    let mut buf = [0u8];
    let n = io::stdin().read(&mut buf);
    let f = match n {
        Ok(0) => Ok(None),
        Ok(n) => Ok(Some(buf[..n].to_vec().fit())),
        Err(e) => Err(e.into()),
    };
    Box::pin(future::ready(f))
}

pub fn put(i: Item) -> types::Sometime<'static, Result<(), Error>> {
    let bs = Bytes::try_derive(i);
    match bs {
        Ok(bs) => types::Sometime::Future(Box::pin(async move {
            tokio::io::stdout()
                .write_all(&bs)
                .await
                .map_err(|e| e.into())
        })),
        Err(e) => types::Sometime::Now(Err(e)),
    }
}

pub fn representation() -> Item {
    assoc::Association::derive_iter([
        (fit!("type"), fit!("pipe")),
        (fit!("in"), fit!("bytes")),
        (fit!("out"), fit!("bytes")),
        (fit!("endpoint"), fit!("standard")),
    ])
    .fit()
}
