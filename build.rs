use directories::ProjectDirs;
use std::fs::{self};
use std::path::Path;

fn main() {
    if let Some(proj_dirs) = ProjectDirs::from("org", "skyrod", "kcats") {
        let data_dir = proj_dirs.data_dir();
        fs::create_dir_all(data_dir).expect("Failed to create data directory");

        let std_path = data_dir.join("stdlib");
        fs::create_dir_all(&std_path).expect("Failed to create standard library directory");
        // Specify the path to your project's stdlib folder
        let src_stdlib_path = Path::new("src/kcats/stdlib");

        // Iterate over the contents of the source stdlib directory
        if src_stdlib_path.exists() && src_stdlib_path.is_dir() {
            if let Ok(entries) = fs::read_dir(src_stdlib_path) {
                for entry in entries {
                    if let Ok(entry) = entry {
                        let path = entry.path();
                        if path.is_file() {
                            if let Some(filename) = path.file_name() {
                                let dest_path = std_path.join(filename);
                                fs::copy(&path, &dest_path)
                                    .expect(&format!("Failed to copy file {:?}", filename));
                            }
                        }
                    }
                }
            }
        } else {
            eprintln!("Source standard library directory does not exist or is not a directory");
        }
    }
}
