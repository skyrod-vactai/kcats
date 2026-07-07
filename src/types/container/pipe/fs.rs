use crate::axiom::ItemResult;
use crate::types::container::associative as assoc;
use crate::types::container::error::Error;
use crate::types::*;

use std::future;
use std::ptr;
use std::sync::Arc;
use tokio::fs::File;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::sync::RwLock;

use super::{closed_error, FutureTake};

#[derive(Debug)]
pub struct StaticFile {
    pub file: Option<File>,
    pub path: String,
}

impl PartialEq for StaticFile {
    fn eq(&self, other: &Self) -> bool {
        // Check if the 'file' fields of both structs are the same by reference
        ptr::eq(&self.file, &other.file)
    }
}

impl FutureTake for StaticFile {
    type Item = Bytes;
    fn take_future(&mut self) -> Future<'_, Result<Option<Self::Item>, Error>> {
        match self.file.as_mut() {
            Some(f) => {
                let mut bs = [0u8; 102400];
                Box::pin(async move {
                    let ct = f.read(&mut bs).await?;
                    if ct == 0 {
                        // EOF, no more takes since it's static
                        Ok(None)
                    } else {
                        Ok(Some(bs[0..ct].to_vec().fit()))
                    }
                })
            }
            None => Box::pin(future::ready(Err(closed_error(false)))),
        }
    }
}

impl StaticFile {
    pub fn put(&mut self, i: Item) -> Future<'_, Result<(), Error>> {
        match self.file.as_mut() {
            Some(f) => {
                let b = Bytes::try_derive(i);

                match b {
                    Ok(bs) => Box::pin(async move { f.write_all(&bs).await.map_err(|e| e.into()) }),
                    Err(e) => Box::pin(future::ready(Err(e))),
                }
            }
            None => Box::pin(future::ready(Err(closed_error(false)))),
        }
    }
}

impl crate::serialize::Display for StaticFile {
    fn representation(&self) -> Item {
        assoc::Association::derive_iter([
            (fit!("type"), fit!("pipe")),
            (fit!("in"), fit!("bytes")),
            (fit!("out"), fit!("bytes")),
            (
                fit!("endpoint"),
                assoc::Association::derive_iter([(fit!("file"), self.path.clone().fit())]).fit(),
            ),
        ])
        .fit()
    }
}

pub fn file_in(i: Item) -> ItemResult {
    let path = String::try_derive(i)?;
    let file = std::fs::File::options()
        .read(true)
        .write(true)
        .create_new(true)
        .open(path.clone())?;
    Ok(super::In::StaticFile(Arc::new(RwLock::new(StaticFile {
        file: Some(File::from_std(file)),
        path,
    })))
    .fit())
}

pub fn file_out(i: Item) -> ItemResult {
    let path = String::try_derive(i)?;
    let file = std::fs::File::open(path.clone())?;
    Ok(super::Out::StaticFile(Arc::new(RwLock::new(StaticFile {
        file: Some(File::from_std(file)),
        path,
    })))
    .fit())
}

impl Derive<StaticFile> for Item {
    fn derive(f: StaticFile) -> Self {
        super::Out::StaticFile(Arc::new(RwLock::new(f))).fit()
    }
}
