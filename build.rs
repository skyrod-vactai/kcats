use cache::cache;
use directories::ProjectDirs;
//use std::env;
use edn_format;
use std::error::Error;
use std::fs::File;
use std::fs::{self};
use std::io::Write;
use std::path::Path;
//use std::path::PathBuf;
use std::collections::HashMap;

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

fn hash_and_cache(
    cache: &mut cache::Cache,
    path: &Path,
    //mut core_module_file: Option<&mut File>,
    should_parse: bool,
    hashes: &mut HashMap<String, Vec<u8>>,
) -> Result<HashMap<String, (Vec<u8>, Vec<String>)>, Box<dyn Error>> {
    if path.exists() && path.is_dir() {
        let mut r = HashMap::new();
        let entries = fs::read_dir(path)?;
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_file() {
                let module_name = path.file_stem().unwrap().to_str().unwrap().to_string();
                let formatted = std::fs::read_to_string(&path)?;
                let canonical = edn_format::canonicalize(formatted)?;
                println!("Parsing module {:?}", path);
                // 1. Put in Cache and ESCALATE the Hash return value
                let hash_string =
                    cache.put(&canonical.as_bytes().to_vec(), Some(module_name.clone()))?; // returns Ok(String) e.g. "QmHash..."

                // 2. Parse EDN to find defined names
                // This is the only logic specific to your format.
                // You need to extract ["postbox", "put"] from the file content.
                if should_parse {
                    {
                        let parsed: Vec<edn_format::Value> = edn_format::Parser::from_iter(
                            canonical.chars(),
                            edn_format::ParserOptions::default(),
                        )
                        .collect::<Result<_, edn_format::ParserError>>()?;
                        let block = parsed
                            .first()
                            .ok_or_else(|| edn_format::ParserError::InvalidSymbol)?;
                        let defs = match block {
                            edn_format::Value::Vector(defs) => Ok(defs),
                            i => {
                                println!("Couldn't parse outer {:?}", i);
                                Err(edn_format::ParserError::InvalidSymbol)
                            }
                        }?;
                        let names = defs
                            .into_iter()
                            .map(|i| match i {
                                edn_format::Value::Vector(l) => match l.first().cloned() {
                                    Some(edn_format::Value::Symbol(s)) => Ok(s.name().to_string()),
                                    i => {
                                        println!("Couldn't parse inner {:?}", i);
                                        Err(edn_format::ParserError::InvalidSymbol)
                                    }
                                },
                                i => {
                                    println!("Couldn't parse outer {:?}", i);
                                    Err(edn_format::ParserError::InvalidSymbol)
                                }
                            })
                            .collect::<Result<Vec<String>, edn_format::ParserError>>()?;
                        // for name in names {
                        //     writeln!(
                        //         f,
                        //         "    m.insert(\"{}\", &{:?});",
                        //         name,
                        //         hash_string.as_slice()
                        //     )
                        //     .unwrap();
                        // }
                        r.insert(module_name.clone(), (hash_string.clone(), names));
                    }
                }

                // 2. Collect for codegen
                hashes.insert(module_name, hash_string);
            }
        }
        Ok(r)
    } else {
        Err(Box::new(MyError::CustomError(format!(
            "stdlib dir not a dir or not found: {:?}",
            path
        ))))
    }
}

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

    let mut cache = cache::Cache::new(cache_dir)?;
    let core = "src/kcats/core";
    let core_path = Path::new(core);
    let stdlib = "src/kcats/stdlib";
    let stdlib_path = Path::new(stdlib);

    // Add debug information
    //println!("Stdlib path: {:?}", stdlib_path);
    //println!("Stdlib exists: {}", stdlib_path.exists());

    // Store mappings to generate code later
    let mut alias_to_hash: HashMap<String, Vec<u8>> = HashMap::new();

    let dest_path = Path::new("src").join("stdlib_hashes.rs");
    let mut f = File::create(&dest_path)?;

    // We will generate a map: "function_name" -> "file_hash"
    writeln!(f, "/// Auto-generated by build.rs. DO NOT EDIT.")?;
    writeln!(f, "use std::collections::HashMap;")?;

    let r = hash_and_cache(&mut cache, core_path, true, &mut alias_to_hash)?;
    // Generate some consts
    for (module_name, (hash, _)) in r.iter() {
        let sanitized_name = module_name.replace("-", "_").to_uppercase();
        writeln!(f, "const {}: &[u8] = {:?};", sanitized_name, hash).unwrap();
    }
    writeln!(
        f,
        "pub fn get_manifest() -> HashMap<&'static str, &'static [u8]> {{"
    )
    .unwrap();
    writeln!(
        f,
        "    let mut m: HashMap<&'static str, &'static [u8]> = HashMap::new();"
    )
    .unwrap();
    for (module_name, (_, fn_names)) in r.iter() {
        let sanitized_name = module_name.replace("-", "_").to_uppercase();
        for name in fn_names.into_iter() {
            writeln!(f, "    m.insert(\"{}\", {});", name, sanitized_name).unwrap();
        }
    }
    hash_and_cache(&mut cache, stdlib_path, false, &mut alias_to_hash)?;

    writeln!(f, "    m")?;
    writeln!(f, "}}")?;

    Ok(())
}
