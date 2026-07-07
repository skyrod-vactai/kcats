//! The main kcats module, that executes the kcats interpreter. See [main]
//mod default;

use kcats::axiom;
use kcats::config::configure_platform;
use kcats::serialize::{self, Emit};
pub use kcats::traits::*;
use kcats::types::container::environment::Environment;
use kcats::types::container::List;
use kcats::derivation::Derive;
use std::io::{self, BufRead, Read, Write};

fn print_result(env: Environment) {
    if env.program.is_empty() {
        println!(
            "{}",
            serialize::auto_format(env.stack.to_list().iter().emit().as_str(), 20, 80)
        );
    } else {
        println!(
            "stack: {}\nprogram: {}",
            serialize::auto_format(env.stack.to_list().iter().emit().as_str(), 20, 80),
            serialize::auto_format(List::derive(env.program).iter().emit().as_str(), 20, 80)
        )
    }
}

fn get_stdin() -> String {
    let mut buf = String::new();
    let _ = io::stdin().read_to_string(&mut buf);
    buf
}

/// Evaluates the program in the context of the env, and handles any
/// unhandled errors. Good for interactive programming.
async fn repl_eval(mut env: Environment, mut program: String) -> Environment {
    // to ensure errors are handled by the repl- so that the
    // user can continue with more input.
    program.push_str(" handle");

    match serialize::parse_input(&mut env, program) {
        Ok(_) => axiom::eval(env).await,
        Err(e) => {
            env.push(e);
            env
        }
    }
}
// A function that takes a handle to stdin. It reads a length from
// stdin, then reads that many bytes and returns a string.
async fn read_input() -> Option<String> {
    //spawn a thread to read from stdin
    tokio::spawn(async move {
        let mut stdin = io::stdin().lock();
        let mut buf = String::new();
        if let Err(e) = stdin.read_line(&mut buf) {
            println!("Error reading content length {:?}", e);
            return None;
        }
        // parse an integer from buf
        let read_len = buf.trim();
        let len = read_len.parse::<usize>().unwrap_or_default();
        if len == 0 {
            return None;
        }
        // read len bytes from stdin
        let mut buf = vec![0; len];
        if stdin.read_exact(&mut buf).is_err() {
            return None;
        }

        // convert the bytes to a string
        String::from_utf8(buf).ok()
    })
    .await
    .unwrap_or(None)
}

async fn print_with_length(env: &Environment) {
    let result = serialize::auto_format(env.stack.to_list().iter().emit().as_str(), 20, 80);

    // first print the length of the result
    println!("{}\n{}", result.len(), result);
}

async fn print(env: &Environment) {
    let result = serialize::auto_format(env.stack.to_list().iter().emit().as_str(), 20, 80);
    println!("{}", result);
}

//It converts the bytes to a
// string, and then evaluates that string as a kcats program. It then
// prints the length of the result, and then the result itself.
async fn interactive_mode() {
    let mut env = Environment::default();

    loop {
        if let Some(program) = read_input().await {
            env = repl_eval(env, program).await;
            print_with_length(&env).await;
        }
    }
}

async fn repl() {
    let mut env = Environment::default();

    loop {
        // Print the prompt and flush it to stdout immediately
        print!("kcats> ");
        io::stdout().flush().unwrap();

        // Read a line from stdin
        let mut line = String::new();
        io::stdin().read_line(&mut line).unwrap();

        // Check if the input is empty, if so, continue to the next loop iteration
        if line.trim().is_empty() {
            continue;
        }

        env = repl_eval(env, line).await;
        print(&env).await;
    }
}

async fn read_eval_print(program: String) {
    let mut env = Environment::default();
    match serialize::parse_input(&mut env, program) {
        Ok(_) => {
            print_result(axiom::eval(env).await);
        }
        Err(e) => {
            println!("Error parsing input: {:?}", e);
        }
    }
}

/// The main intepreter entry function that can start the interpreter
/// in several different modes.
#[tokio::main]
async fn main() {
    // Set up process-wide paths and panic if this fails
    configure_platform();
    // read command line options
    let args: Vec<String> = std::env::args().collect();
    
    if args.iter().any(|a| a == "-i") {
        interactive_mode().await;
    } else if args.iter().any(|a| a == "-r") {
        repl().await;
    } else if let Some(idx) = args.iter().position(|a| a == "-f") {
        if let Some(filename) = args.get(idx + 1) {
            if let Ok(mut file) = std::fs::File::open(filename) {
                let mut buf = String::new();
                if file.read_to_string(&mut buf).is_ok() {
                    read_eval_print(buf).await;
                } else {
                    println!("Error reading file: {}", filename);
                }
            } else {
                println!("Error opening file: {}", filename);
            }
        } else {
            println!("Missing filename after -f");
        }
    } else if let Some(idx) = args.iter().position(|a| a == "-p") {
        if let Some(program) = args.get(idx + 1) {
            read_eval_print(program.clone()).await;
        } else {
            println!("Missing program text after -p");
        }
    } else {
        // otherwise, read from stdin
        read_eval_print(get_stdin()).await;
    }
}
