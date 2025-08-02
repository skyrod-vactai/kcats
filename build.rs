use cache::cache;
use directories::ProjectDirs;
use std::error::Error;
use std::fs::{self};
use std::path::Path;
//use std::path::PathBuf;

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
    // Print the current directory for debugging
    println!("Current dir: {:?}", std::env::current_dir()?);

    // Create a fallback cache directory in the target directory
    let cache_dir = if let Some(project_dirs) = ProjectDirs::from("org", "skyrod", "kcats") {
        let dir = project_dirs.data_dir().join("cache");
        println!("Using project cache dir: {:?}", dir);
        dir
    } else {
        let dir = Path::new("target").join("build_cache");
        println!("Using fallback cache dir: {:?}", dir);
        dir
    };

    // Create cache directory
    std::fs::create_dir_all(&cache_dir)
        .map_err(|e| MyError::CustomError(format!("Failed to create cache directory: {}", e)))?;

    let cache = cache::Cache::new(cache_dir)?;
    let src = "src/kcats/stdlib";
    let src_stdlib_path = Path::new(src);

    // Add debug information
    println!("Stdlib path: {:?}", src_stdlib_path);
    println!("Stdlib exists: {}", src_stdlib_path.exists());

    if src_stdlib_path.exists() && src_stdlib_path.is_dir() {
        let entries = fs::read_dir(src_stdlib_path)?;

        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_file() && path.file_name().is_some() {
                let formatted = std::fs::read_to_string(path.clone())?;

                let module_name = path.file_stem().unwrap().to_str().unwrap();
                println!("cargo:print=Processing module: {}", module_name);
                let canonical = edn_format::canonicalize(formatted)?;
                cache.put(
                    &canonical.as_bytes().to_vec(),
                    Some(module_name.to_string()),
                )?;
                //cache.put_from_path(&path, Some(module_name.to_string()))?;
            }
        }
        Ok(())
    } else {
        Err(Box::new(MyError::CustomError(format!(
            "Stdlib path {:?} does not exist or is not a directory",
            src_stdlib_path
        ))))
    }
}
