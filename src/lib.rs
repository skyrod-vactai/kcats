pub mod axiom;
mod crypto;
//mod enumtest;
pub mod derivation;
pub mod serialize;
pub mod stdlib_hashes;
pub mod traits;
pub mod types;
pub mod compile;
#[cfg(target_os = "android")]
mod android {
    use crate::axiom;
    use crate::config::PlatformConfig;
    use crate::serialize::{self, Emit};
    use crate::types::container::environment::Environment;
    use std::ffi::CString;
    use std::path::PathBuf;

    use cache::cache;
    use jni::objects::{JClass, JString};
    use jni::sys::jstring;
    use jni::JNIEnv;
    use libc::c_char as lc_char;
    use once_cell::sync::Lazy;
    use tokio::runtime::Runtime;

    static RUNTIME: Lazy<Runtime> =
        Lazy::new(|| Runtime::new().expect("Failed to create Tokio runtime"));

    #[link(name = "log")]
    extern "C" {
        fn __android_log_print(prio: i32, tag: *const lc_char, fmt: *const lc_char, ...) -> i32;
    }

    const ANDROID_LOG_INFO: i32 = 4;

    pub fn log(message: &str) {
        let tag = CString::new("kcats").unwrap();
        let message = CString::new(message).unwrap();

        unsafe {
            __android_log_print(ANDROID_LOG_INFO, tag.as_ptr(), message.as_ptr());
        }
    }

    #[no_mangle]
    pub extern "system" fn Java_org_skyrod_subverse_MainActivity_kcatsEval<'local>(
        mut jnienv: JNIEnv<'local>,
        _class: JClass<'local>,
        env: *mut Environment,
        program: JString,
    ) -> jstring {
        log("Starting eval");
        let mut program: String = jnienv
            .get_string(&program)
            .expect("Couldn't get java string!")
            .into();
        // to ensure errors are handled by the repl- so that the
        // user can continue with more input.
        program.push_str(" handle");

        log(format!("Got program {:?}", program).as_str());
        if env.is_null() {
            return jnienv
                .new_string("Invalid environment pointer")
                .unwrap()
                .as_raw();
        }

        log("Taking pointer ownership");
        // Take ownership of the Environment safely
        unsafe {
            let env_ref = &mut *env;
            let mut env_val = std::mem::replace(env_ref, Environment::default());
            log("Parsing input");
            match serialize::parse_input(&mut env_val, program) {
                Ok(_) => {
                    // Execute the eval and re-assign the result back to the env pointer
                    log("Executing environment");
                    env_val = RUNTIME.block_on(async { axiom::eval(env_val).await });
                    log("Formatting result");
                    let result = serialize::auto_format(
                        env_val.stack.to_list().iter().emit().as_str(),
                        10,
                        40,
                    );
                    // Write the updated environment back to the pointer
                    *env_ref = env_val;

                    // Convert the evaluation result back to a C string
                    jnienv.new_string(result).unwrap().as_raw()
                }
                Err(e) => {
                    *env_ref = env_val;
                    jnienv
                        .new_string(format!("Error: {:?}", e))
                        .unwrap()
                        .as_raw()
                }
            }
        }
    }

    #[no_mangle]
    pub extern "system" fn Java_org_skyrod_subverse_MainActivity_kcatsNew<'local>(
        mut jnienv: JNIEnv<'local>,
        _class: JClass<'local>,
        cachepath: JString,
        dbfile: JString,
    ) -> *mut Environment {
        //set panic hook to log panics
        std::panic::set_hook(Box::new(|panic_info| {
            // Log panic info
            log(format!("Rust panic: {:?}", panic_info).as_str());
        }));

        log("creating new kcats env");
        let cacheloc: String = jnienv
            .get_string(&cachepath)
            .expect("Couldn't get java string!")
            .into();
        log("creating new cache");
        let cache = cache::Cache::new(PathBuf::from(cacheloc)).expect("Valid cache location");
        let dbloc: String = jnienv
            .get_string(&dbfile)
            .expect("Couldn't get java string!")
            .into();
        log("setting platform config");

        let result = std::panic::catch_unwind(|| {
            // Your potentially panicking code here

            PlatformConfig::init(PathBuf::from(dbloc), cache).expect("Failed platform init");
            Box::into_raw(Box::new(Environment::default()))
        });

        match result {
            Ok(value) => return value,
            Err(e) => {
                if let Some(s) = e.downcast_ref::<String>() {
                    log(format!("Panic occurred: {}", s).as_str());
                } else if let Some(s) = e.downcast_ref::<&str>() {
                    log(format!("Panic occurred: {}", s).as_str());
                }
                // Handle the panic
            }
        }
        panic!("uh oh");
    }

    #[no_mangle]
    pub extern "C" fn Java_org_skyrod_subverse_MainActivity_katsFree(env: *mut Environment) {
        log("FREE");
        if !env.is_null() {
            unsafe {
                drop(Box::from_raw(env));
            }
        }
    }
}

pub mod config {
    use crate::fit;
    use crate::types::container::error::Error;
    use crate::types::Item;
    use cache::cache;
    use directories::ProjectDirs;
    use std::path::Path;

    use std::path::PathBuf;
    use std::sync::Arc;

    use lazy_static::lazy_static;
    use std::sync::RwLock;

    lazy_static! {
        pub static ref PLATFORM_CONFIG: RwLock<Option<PlatformConfig>> = {
            //println!("Creating PLATFORM_CONFIG at {}:{}", file!(), line!());
            RwLock::new(None)
        };
    }

    /// A configuration struct for the platform we're running on,
    /// specifies where some filesystem resources are located. On some
    /// platforms (like android) we can't guess and will only know at
    /// runtime.
    #[derive(Clone, Debug)]
    pub struct PlatformConfig {
        pub database: Option<Arc<PathBuf>>,
        pub cache: Arc<cache::Cache>,
    }

    impl PlatformConfig {
        pub fn init(database: PathBuf, cache: cache::Cache) -> Result<(), Error> {
            //println!("Initializing with {:?} and {:?}", database, cache);
            let mut config = PLATFORM_CONFIG.write().unwrap();
            *config = Some(PlatformConfig {
                database: Some(Arc::new(database)),
                cache: Arc::new(cache),
            });
            Ok(())
        }

        pub fn get() -> Result<PlatformConfig, Error> {
            let config = PLATFORM_CONFIG.read().unwrap();
            //println!("Getting platform config: {:?}", config);
            config
                .as_ref()
                .ok_or(Error::expected(fit!("initialization"), None::<Item>))
                .cloned()
        }
    }
    /// If we call this function it's because kcats is running as a binary
    /// and we can figure out storage locations without outside input.
    pub fn configure_platform() {
        //println!("Configure platform");
        let project_dirs = ProjectDirs::from("org", "skyrod", "kcats").unwrap();
        let project_dir = project_dirs.data_dir();
        std::fs::create_dir_all(project_dir).unwrap();
        let db_file = project_dir.join("kcats-database.db");

        let cache_dir = ProjectDirs::from("org", "skyrod", "kcats")
            .map(|proj_dirs| proj_dirs.data_dir().join("cache"))
            .unwrap_or_else(|| Path::new(".").join("cache"));

        PlatformConfig::init(db_file, cache::Cache::new(cache_dir).unwrap()).unwrap();
    }
}

#[cfg(test)]
mod tests {

    //! Unit tests, in the form of all the examples of usage of the
    //! different lexicon words. Examples are all in the form of two
    //! programs that should be equivalent, something like `2 3 +` and
    //! `5`. Runs both programs in separate environments, compares the
    //! resulting stack to ensure they are equal.
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    //use super::error::Error;
    use crate::axiom;
    use crate::derivation::{TryDerive, TryFit};
    use crate::serialize::Emit;

    use crate::types::container as coll;
    use crate::types::container::{environment::Environment, error::Error};
    use crate::types::{Item, Word};
    use crate::{fit, list};

    use test_case::test_case;

    //use std::{panic, sync::Once};

    // static INIT: Once = Once::new();

    // fn init_panic_hook() {
    //     //println!("Installing panic hook");
    //     INIT.call_once(|| {
    //         panic::set_hook(Box::new(|info| {
    //             eprintln!("panic in test: {info}");
    //             eprintln!("{}", std::backtrace::Backtrace::capture());
    //         }));
    //     });
    // }

    pub fn get_item(i: &coll::List, index: usize) -> Option<Item> {
        i.get(index).cloned()
    }

    #[tokio::main]
    async fn test_example(
        mut prog_env: Environment,
        program: coll::List,
        expected_prog: coll::List,
        description: Option<String>,
    ) -> Option<Error> {
        let mut exp_env = prog_env.clone();
        prog_env.program.prepend(program.clone());
        exp_env.program.prepend(expected_prog.clone());
        //init_panic_hook();
        let p_fut = tokio::spawn(async move {
            //init_panic_hook();
            axiom::eval(prog_env).await
        });

        let exp_fut = tokio::spawn(async move {
            //init_panic_hook();
            axiom::eval(exp_env).await
        });
        let (prog_env, exp_env) = tokio::join!(p_fut, exp_fut);
        let prog_env = prog_env.unwrap();
        let exp_env = exp_env.unwrap();

        if prog_env.stack == exp_env.stack {
            if let Some(description) = description {
                println!("PASSED: '{}'", description);
            } else {
                println!(
                    "PASSED: expected {} got {}",
                    (exp_env.stack.to_list().iter().emit()),
                    (prog_env.stack.to_list().iter().emit())
                );
            }
            None
        } else {
            println!(
                "\nFAILED: '{}'\nEXPECTED: {}\nACTUAL:   {}\n",
                description.unwrap_or_default(),
                (exp_env.stack.to_list().iter().emit()),
                (prog_env.stack.to_list().iter().emit())
            );
            // println!(
            //     "Debug: expected {:?} got {:?}",
            //     exp_env.stack, prog_env.stack
            // );
            Some(Error::test_assertion(
                program,
                expected_prog,
                prog_env.stack.to_list(),
            ))
        }
    }

    fn test_word(standard_env: Environment, w: Word) -> Vec<Error> {
        fn contains_at_all(p: &coll::List, w: &Item) -> bool {
            for i in p.iter() {
                match coll::List::try_derive(i) {
                    Ok(l) => {
                        if contains_at_all(&l, w) {
                            return true;
                        }
                    }
                    Err(_) => {
                        if i == w {
                            return true;
                        }
                    }
                }
            }
            false
        }
        //println!("Testing! {:?}", w);
        if let Some(d) = standard_env.dictionary.lingo.get(&w) {
            d.examples
                .clone()
                .unwrap()
                .iter()
                .filter_map(|ex| {
                    let l = coll::List::try_derive(ex.clone()).unwrap();
                    let p = coll::List::try_derive(get_item(&l, 0).unwrap());
                    let exp_prog = coll::List::try_derive(get_item(&l, 1).unwrap());
                    let description = get_item(&l, 2).and_then(|i| String::try_derive(i).ok());
                    match (p, exp_prog) {
                        (Ok(p), Ok(exp)) => {
                            if !contains_at_all(&p, &Item::Word(w)) {
                                Some(Error::create(
                                    list!(w, "test",),
                                    "Example program must reference the word being tested",
                                    Some(p),
                                ))
                            } else {
                                test_example(standard_env.clone(), p, exp, description)
                            }
                        }
                        (Err(e), _) => Some(e),
                        (_, Err(e)) => Some(e),
                    }
                })
                .collect::<Vec<Error>>()
        } else {
            //println!("Couldn't find word. Dict is {:?}", )
            vec![Error::create(
                list!("dictionary", list!(w), "lookup"),
                "word is not defined",
                None::<Item>,
            )]
        }
    }

    fn test_lexicon(word: &str, modules: Vec<Item>) {
        crate::config::configure_platform();
        let mut e = Environment::default();
        e = modules
            .iter()
            .fold(e, |env, module| env.load_module(module.clone(), true));
        let r = test_word(e.clone(), word.try_fit().unwrap());
        assert!(r.is_empty(), "{:?}", r);
    }

    #[test_case("*" ; "mult")]
    #[test_case("+" ; "plus")]
    #[test_case("-" ; "minus")]
    #[test_case("/" ; "divide")]
    #[test_case("=" ; "eq")]
    #[test_case("abs")]
    #[test_case("and")]
    #[test_case("assign")]
    #[test_case("association")]
    #[test_case("association?" ; "is_association")]
    #[test_case("autoformat")]
    #[test_case("bytes?" ; "is_bytes")]
    #[test_case("ceiling")]
    #[test_case("character")]
    #[test_case("compare")]
    #[test_case("contains?" ; "contains")]
    #[test_case("cut")]
    #[test_case("dec")]
    #[test_case("decodejson")]
    #[test_case("difference")]
    #[test_case("emit")]
    #[test_case("empty")]
    #[test_case("empty?" ; "is_empty")]
    #[test_case("encodejson")]
    #[test_case("environment")]
    #[test_case("environment?" ; "is_environment")]
    #[test_case("eval-step")]
    #[test_case("evaluate")]
    #[test_case("even?" ; "is_even")]
    #[test_case("exp")]
    #[test_case("finished?" ; "is_finished")]
    #[test_case("floor")]
    #[test_case("format")]
    #[test_case("get")]
    #[test_case("hashbytes")]
    #[test_case("inc")]
    #[test_case("integer?" ; "is_integer")]
    #[test_case("intersection")]
    #[test_case("log")]
    #[test_case("mod")]
    #[test_case("namespace")]
    #[test_case("number")]
    #[test_case("odd?" ; "is_odd")]
    #[test_case("or")]
    #[test_case("parse-edn")]
    #[test_case("parse-utf8")]
    #[test_case("pipe?" ; "is_pipe")]
    #[test_case("pop")]
    #[test_case("quot")]
    #[test_case("range")]
    #[test_case("read")]
    //#[test_case("rem")]
    #[test_case("remove")]
    #[test_case("resolve")]
    #[test_case("reverse")]
    #[test_case("round")]
    #[test_case("set")]
    #[test_case("set?" ; "is_set")]
    #[test_case("slice")]
    #[test_case("sqrt")]
    #[test_case("string")]
    #[test_case("string?" ; "is_string")]
    #[test_case("unnamespace")]
    #[test_case("word")]
    #[test_case("word?" ; "is_word")]
    #[test_case("zero?" ; "is_zero")]
    #[test_case("••🔀" ; "swapdeep")]
    #[test_case("•🔀" ; "swapdown")]
    #[test_case("↔️" ; "branch")]
    #[test_case("▶️"; "execute")]
    #[test_case("☯️" ; "not")]
    #[test_case("⚓" ; "sink")]
    #[test_case("⛏️" ; "unpack")]
    #[test_case("🍫" ; "unwrap")]
    #[test_case("🎒" ; "pack")]
    #[test_case("👥" ; "clone")]
    #[test_case("📏" ; "count")]
    #[test_case("📤" ; "take")]
    #[test_case("📮" ; "put")]
    #[test_case("📸" ; "snapshot")]
    #[test_case("🔀" ; "swap")]
    #[test_case("🔗" ; "join")]
    #[test_case("🗑️" ; "drop")]
    #[test_case("🛟" ; "float")]
    #[test_case("🧦" ; "evert")]
    #[test_case("🪄" ; "dip")]
    #[test_case["last"]]
    fn test_axiom(word: &str) {
        test_lexicon(word, vec![])
    }

    #[test_case("<" ; "lt")]
    #[test_case("<=" ; "lte")]
    #[test_case(">" ; "gt")]
    #[test_case(">=" ; "gte")]
    #[test_case("addmethod")]
    //#[test_case("assemble")]
    #[test_case("assocify")]
    #[test_case("bail")]
    #[test_case("bailer")]
    #[test_case("between?"; "is_between")]
    #[test_case("bits")]
    #[test_case("both")]
    #[test_case("both?" ; "is_both")]
    #[test_case("butlast")]
    #[test_case("catcher")]
    #[test_case("collect")]
    #[test_case("cram")]
    #[test_case("decide")]
    #[test_case("definition")]
    #[test_case("dropper")]
    #[test_case("each")]
    #[test_case("empty?" ; "is_empty")]
    #[test_case("encode")]
    #[test_case("encodeitem")]
    #[test_case("ends?" ; "is_ends")]
    #[test_case("first")]
    #[test_case("flatten")]
    #[test_case("flip")]
    #[test_case("fold")]
    #[test_case("fork")]
    #[test_case("frequencies")]
    #[test_case("future")]
    #[test_case("group")]
    #[test_case("indexed")]
    #[test_case("indexer")]
    #[test_case("indexof")]
    #[test_case("interpose")]
    #[test_case("joiner")]
    #[test_case("keep")]
    #[test_case("label")]
    #[test_case("let")]
    #[test_case("list?" ; "is_list")]
    #[test_case("max")]
    #[test_case("max-by")]
    #[test_case("min")]
    #[test_case("min-by")]
    #[test_case("module")]
    #[test_case("number?" ; "is_number")]
    #[test_case("over")]
    #[test_case("pad")]
    #[test_case("pair?" ; "is_pair")]
    #[test_case("parse")]
    #[test_case("partition")]
    #[test_case("prepend")]
    #[test_case("primrec")]
    #[test_case("produce")]
    #[test_case("radix")]
    #[test_case("reap")]
    #[test_case("repetition")]
    #[test_case("rest")]
    #[test_case("restore")]
    #[test_case("retry")]
    #[test_case("second")]
    #[test_case("skipper")]
    #[test_case("something?" ; "is_something")]
    #[test_case("spawn")]
    #[test_case("splitter")]
    #[test_case("starts?" ; "is_starts")]
    #[test_case("taker")]
    #[test_case("times")]
    #[test_case("top")]
    #[test_case("tos")]
    #[test_case("type")]
    #[test_case("unassign")]
    #[test_case("under")]
    #[test_case("until")]
    #[test_case("update")]
    #[test_case("using")]
    #[test_case("value")]
    #[test_case("walk")]
    #[test_case("when")]
    #[test_case("within?" ; "is_within")]
    #[test_case("xor")]
    #[test_case("zip")]
    #[test_case("••🐋" ; "divedeep")]
    #[test_case("••👥" ; "clonedeep")]
    #[test_case("••📮" ; "putdeep")]
    #[test_case("••🗑️" ; "dropdeep")]
    #[test_case("••🛡️" ; "shielddeep")]
    #[test_case("••🪄" ; "dipdeep")]
    #[test_case("•🐋" ; "divedown")]
    #[test_case("•👥" ; "clonedown")]
    #[test_case("•📮" ; "putdown")]
    #[test_case("•🗑️" ; "dropdown")]
    #[test_case("•🛡️" ; "shielddown")]
    #[test_case("•🪄" ; "dipdown")]
    #[test_case("⏳" ; "_while")]
    #[test_case("⚖️" ; "_if")]
    #[test_case("🌀"; "_loop")]
    #[test_case("🎁" ; "wrap")]
    #[test_case("🎭" ; "complement")]
    #[test_case("🐋" ; "dive")]
    #[test_case("💉" ; "inject")]
    #[test_case("💯" ; "is_every")]
    #[test_case("📣" ; "is_any")]
    #[test_case("🔍")]
    #[test_case("🚜" ; "map")]
    #[test_case("🛡️" ; "shield")]
    #[test_case("🤹" ; "juggle")]
    #[test_case("🧤" ; "_match")]
    #[test_case("🧲" ; "filter")]
    #[test_case("🧹" ; "into")]
    #[test_case("🩹" ; "recovery")]
    #[test_case("🪆" ; "recur")]
    #[test_case("🪗" ; "reduce")]
    #[test_case("🪜" ; "step")]
    fn test_derived(word: &str) {
        test_lexicon(word, vec![])
    }

    #[test_case("key")]
    #[test_case("verify")]
    fn test_crypto(word: &str) {
        test_lexicon(word, vec![fit!("crypto-builtins")])

}
}
