use cache::cache;
use directories::ProjectDirs;
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
    //copy source files to the interpreter's cache, and hardcode the hashes
    if let Some(proj_dirs) = ProjectDirs::from("org", "skyrod", "kcats") {
        let data_dir = proj_dirs.data_dir();
        fs::create_dir_all(data_dir).expect("Failed to create data directory");

        let std_path = data_dir.join("cache");
        fs::create_dir_all(&std_path).expect("Failed to create cache directory");
        // Specify the path to your project's stdlib folder
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
                    cache::put_from_path(&path, Some(module_name.to_string()))?;
                }
            }
            Ok(())
        } else {
            Err(Box::new(MyError::CustomError(
                "Cache directory does not exist or is not a directory".to_string(),
            )))
        }
    } else {
        Err(Box::new(MyError::CustomError(
            "Can't find project data dir".to_string(),
        )))
    }
}
