use directories::ProjectDirs;
use sha2::{self};
use std::error::Error;
use std::fs::{self};
use std::path::Path;

use sha2::{Digest, Sha256};
use std::fs::File;
use std::io::Write;
use std::io::{self, Read};
use std::path::PathBuf;

#[derive(Debug)]
enum MyError {
    CustomError(String),
}

impl std::fmt::Display for MyError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            MyError::CustomError(ref err) => write!(f, "Build Error: {}", err),
        }
    }
}

impl std::error::Error for MyError {}

fn hash_file<P: AsRef<Path>>(path: P) -> io::Result<Vec<u8>> {
    let mut file = File::open(path)?;
    let mut hasher = Sha256::new();
    let mut buffer = [0; 1024]; // Read in chunks of 1024 bytes

    loop {
        let count = file.read(&mut buffer)?;
        if count == 0 {
            break;
        }
        hasher.update(&buffer[..count]);
    }

    Ok(hasher.finalize().to_vec())
}

fn vec_to_byte_literal(vec: Vec<u8>) -> String {
    let mut literal = String::from("vec![");
    for byte in vec.iter() {
        literal.push_str(&format!("0x{:02X}, ", byte));
    }
    literal.push(']');
    literal
}

fn main() -> Result<(), Box<dyn Error>> {
    //panic!("oh noes");
    println!("Starting build.rs!");

    if let Some(proj_dirs) = ProjectDirs::from("org", "skyrod", "kcats") {
        let data_dir = proj_dirs.data_dir();
        fs::create_dir_all(data_dir).expect("Failed to create data directory");

        let std_path = data_dir.join("stdlib");
        fs::create_dir_all(&std_path).expect("Failed to create standard library directory");
        // Specify the path to your project's stdlib folder
        let src = "src/kcats/stdlib";
        let src_stdlib_path = Path::new(src);

        // Iterate over the contents of the source stdlib directory
        if src_stdlib_path.exists() && src_stdlib_path.is_dir() {
            let entries = fs::read_dir(src_stdlib_path)?;
            // Write a source file hardcoding the hashes of the stdlib files
            let out_dir = Path::new("src");
            let dest_path = Path::new(&out_dir).join("module_map.rs");
            let mut f = File::create(dest_path)?;

            writeln!(f, "use std::collections::HashMap;\n")?;
            writeln!(
                f,
                "pub fn module_hashes() -> HashMap<&'static str, Vec<u8>> {{"
            )?;
            writeln!(f, "    let mut map = HashMap::new();")?;
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_file() {
                    if let Some(filename) = path.file_name() {
                        let dest_path = std_path.join(filename);
                        fs::copy(&path, &dest_path)
                            .expect(&format!("Failed to copy file {:?}", filename));
                    }
                    let module_name = <PathBuf as AsRef<Path>>::as_ref(&path)
                        .file_stem()
                        .unwrap()
                        .to_str()
                        .unwrap();
                    let hash = hash_file(path.clone()).unwrap();
                    writeln!(
                        f,
                        "    map.insert(\"{}\", {});",
                        module_name,
                        vec_to_byte_literal(hash)
                    )?;
                }
            }
            writeln!(f, "    map")?;
            writeln!(f, "}}")?;
            Ok(())
        } else {
            Err(Box::new(MyError::CustomError(
                "Source standard library directory does not exist or is not a directory"
                    .to_string(),
            )))
        }
    } else {
        Err(Box::new(MyError::CustomError(
            "Can't find project data dir".to_string(),
        )))
    }
}
