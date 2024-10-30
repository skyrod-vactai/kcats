use cache::cache;
use directories::ProjectDirs;
use prost_build;
use std::error::Error;
use std::fs::{self};
use std::path::Path;
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

fn main() -> Result<(), Box<dyn Error>> {
    //println!("cargo:rustc-link-lib=sqlite3");
    println!("cargo:rerun-if-changed=proto/kcats.proto");

    // build the protocol buffers
    prost_build::compile_protos(&["proto/kcats.proto"], &["proto"]).unwrap();

    let project_dirs = ProjectDirs::from("org", "skyrod", "kcats").unwrap();
    let project_dir = project_dirs.data_dir();
    std::fs::create_dir_all(project_dir).unwrap();
    let cache_dir = ProjectDirs::from("org", "skyrod", "kcats")
        .map(|proj_dirs| proj_dirs.data_dir().join("cache"))
        .unwrap_or_else(|| Path::new(".").join("cache"));

    let cache = cache::Cache::new(cache_dir)?;
    let src = "src/kcats/stdlib";
    let src_stdlib_path = Path::new(src);

    // Iterate over the contents of the source stdlib directory
    if src_stdlib_path.exists() && src_stdlib_path.is_dir() {
        let entries = fs::read_dir(src_stdlib_path)?;

        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_file() && path.file_name().is_some() {
                let module_name = <PathBuf as AsRef<Path>>::as_ref(&path)
                    .file_stem()
                    .unwrap()
                    .to_str()
                    .unwrap();
                cache.put_from_path(&path, Some(module_name.to_string()))?;
            }
        }
        Ok(())
    } else {
        Err(Box::new(MyError::CustomError(
            "Cache directory does not exist or is not a directory".to_string(),
        )))
    }
}
