use crate::axiom::ItemResult;
use crate::derivation::*;
use crate::fit;
use crate::traits::*;
use crate::types::container::{associative as assoc, error::Error};
use crate::types::number::Int;
use crate::types::{Bytes, Item};
use core::ops::Deref;
use ed25519_dalek as signing;
use ed25519_dalek::{Signer, Verifier};
use rand::rngs::OsRng; // Import OsRng
use rand::RngCore as RandRngCore;
use rand_core::{CryptoRng, RngCore};
use sha2::{self, Digest}; // Import RngCore for the fill_bytes method
use std::sync::Arc;

pub fn hash(i: Item) -> ItemResult {
    let b = Bytes::try_derive(i).unwrap();
    Ok(sha2::Sha256::digest(&b).deref().to_vec().fit())
}

type Value = Vec<u8>;

pub struct SeededRNG {
    seed: Value,
    salt: Value,
}

impl SeededRNG {
    // Hash of seed|value
    fn hash(&self) -> Vec<u8> {
        let mut v = self.seed.clone();
        v.extend(self.salt.clone());
        sha2::Sha256::digest(v.as_slice()).deref().to_vec()
    }
}

impl RngCore for SeededRNG {
    fn next_u32(&mut self) -> u32 {
        rand_core::impls::next_u32_via_fill(self)
    }

    fn next_u64(&mut self) -> u64 {
        rand_core::impls::next_u64_via_fill(self)
    }

    fn fill_bytes(&mut self, dest: &mut [u8]) {
        let l = dest.len();
        dest.copy_from_slice(&self.hash()[..l]);
    }

    fn try_fill_bytes(&mut self, dest: &mut [u8]) -> Result<(), rand_core::Error> {
        self.fill_bytes(dest);
        Ok(())
    }
}

#[allow(dead_code)]
pub fn hash_bytes(contents: &[u8]) -> Vec<u8> {
    let mut hasher = sha2::Sha256::new();
    //let mut buffer = [0; 1024]; // Read in chunks of 1024 bytes
    let count = contents.len();
    hasher.update(&contents[..count]);

    hasher.finalize().to_vec()
}

impl CryptoRng for SeededRNG {}

pub fn key(seed: Item) -> ItemResult {
    let sbs: Bytes = seed.try_fit().map_err(Error::derive)?;
    let kp = signing::Keypair::generate(&mut SeededRNG {
        seed: vec![],
        salt: sbs,
    });
    Ok(Arc::new(
        [
            (fit!("type"), fit!("elliptic-curve-key")),
            (fit!("secret"), kp.secret.as_ref().to_vec().fit()),
            (fit!("public"), kp.public.as_ref().to_vec().fit()),
        ]
        .rewrap::<assoc::AssociationContent, assoc::Entry>(),
    )
    .fit())
}

impl TryDerive<Item> for signing::Keypair {
    fn try_derive(i: Item) -> Result<Self, Error> {
        let sk: signing::SecretKey = i.try_fit()?;
        let pk: signing::PublicKey = (&sk).into();
        Ok(signing::Keypair {
            secret: sk,
            public: pk,
        })
    }
}

impl TryDerive<Item> for signing::SecretKey {
    fn try_derive(i: Item) -> Result<Self, Error> {
        let a = assoc::Associative::try_derive(i)?;
        if a.get(&fit!("type")) == Some(fit!("elliptic-curve-key")) {
            let sk = signing::SecretKey::from_bytes(
                &Bytes::try_derive(a.get(&fit!("secret")).ok_or_else(
                    || Error::expected(fit!("secret"), a), //Error::expected(fit!("secret"), None::<Item>)
                )?)?[..],
            )?;
            Ok(sk)
        } else {
            Err(Error::expected(fit!("elliptic-curve-key"), a))
        }
    }
}

impl From<signing::ed25519::Error> for Error {
    fn from(_e: signing::ed25519::Error) -> Error {
        Error::expected(fit!("secret-key"), Item::default())
    }
}

impl TryDerive<Item> for signing::PublicKey {
    fn try_derive(i: Item) -> Result<Self, Error> {
        let a = assoc::Associative::try_derive(i)?;
        if a.get(&fit!("type")) == Some(fit!("elliptic-curve-key")) {
            let pk = signing::PublicKey::from_bytes(
                &Bytes::try_derive(
                    a.get(&fit!("public"))
                        .ok_or_else(|| Error::expected(fit!("public"), Item::default()))?,
                )?[..],
            )
            .map_err(|_e| Error::expected(fit!("valid-public-key"), Item::default()))?;
            Ok(pk)
        } else {
            Err(Error::expected(fit!("public-key"), Item::derive(a)))
        }
    }
}
//TODO: we can only call sign from a keypair, so we may want to assume
// that we have either the kp, or just the secret key.
pub fn sign(k: Item, m: Item) -> ItemResult {
    let kp: signing::Keypair = k.try_fit().map_err(Error::derive)?;
    let message: Bytes = m.try_fit().map_err(Error::derive)?;
    let signature: signing::Signature = kp.sign(&message);
    Ok(signature.as_ref().to_vec().fit())
}

pub fn verify(k: Item, m: Item, s: Item) -> ItemResult {
    let mret = m.clone();
    let pk: signing::PublicKey = k.try_fit().map_err(Error::derive)?;
    let mbs: Bytes = m.try_fit().map_err(Error::derive)?;
    let sbs: Bytes = s.try_fit().map_err(Error::derive)?;
    let sig = signing::Signature::from_bytes(&sbs)
        .map_err(|_e| Error::expected(fit!("signature"), None::<Item>))?;
    Ok(pk.verify(&mbs, &sig).map(|_| mret).unwrap_or_default())
}

fn random_bytes(n: usize) -> Vec<u8> {
    let mut bytes = vec![0u8; n]; // Create a vector of n zeros
    OsRng.fill_bytes(&mut bytes); // Fill the vector with random bytes
    bytes
}

pub fn random(n: Item) -> ItemResult {
    let n: Int = n.try_fit().map_err(Error::derive)?;
    Ok(random_bytes(n as usize).fit())
}
