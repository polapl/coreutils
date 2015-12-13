#![crate_name = "expr"]
#![feature(rustc_private)]

/*
	term00 -> term10 ([|, &, =, !=, <, <=, >, >=] term10)*
	
	term10 -> term20 ([+, -] term20)*
	
	term20 -> term30 ([*, /, %] term30)*
	
	term30 -> term40 (: regexp)*

	term40 -> [match term40 regexp, substr term40 term40 term40,
		index term40 term40, length term40, + token, \( term00 \), const]
*/

extern crate getopts;
extern crate regex;

use regex::Regex;
use std::collections::hash_set::HashSet;

enum Result {
	Number(i32),
	Text(String),
}


fn invoke_numeric_expr(arg0: Result, arg1: Result, operation: fn(i32, i32) -> i32) -> i32 {

	let val0: i32;
	let val1: i32;

	match arg0 {
		Result::Number(n) => val0 = n,
		Result::Text(_) => panic!("Integer expected")
	}

	match arg1 {
		Result::Number(n) => val1 = n,
		Result::Text(_) => panic!("Integer expected"),
	}
	
	operation(val0, val1)
}

fn invoke_relations(arg0: Result, arg1: Result, operation_int: fn(i32, i32) -> i32,
	operation_str: fn(String,String) -> i32) -> i32 {

	let val0: String;
	let val1: String;

	match (arg0, arg1) {
		(Result::Number(n0), Result::Number(n1)) => { 
			let n0: i32 = n0;
			let n1: i32 = n1;
			return operation_int(n0, n1) 
		}
		(Result::Number(n), Result::Text(t)) => {
			val0 = n.to_string();
			val1 = t
		}
		(Result::Text(t), Result::Number(n)) => {
			val0 = t;
			val1 = n.to_string()
		}
		(Result::Text(t0), Result::Text(t1)) => {
			val0 = t0.to_string();
			val1 = t1.to_string()
		}
	}

	operation_str(val0, val1)
} 


fn match_fun(tokens: &mut Vec<String>, text: String) -> Result {
	let regex: Regex;
	match tokens.pop() {
		Some(last_token) => {

			let mut last_token = last_token;
			let first = last_token.starts_with("^");
			if first == false { last_token = "^".to_string() + &last_token; }

			regex = match Regex::new(&last_token) { 
				Ok(re) => re,
				Err(err) => panic!("{}", err),
			};

			let caps = regex.captures(&text);
	
			match caps {
				Some(matched_caps) => {

					let brackets: usize = matched_caps.len();
					let mut matched = matched_caps.at(0);
					if matched_caps.len() > 1 {
						matched = matched_caps.at(1);
					}

					match matched {
						Some(s) => {
							if brackets > 1 {
								let retval: String = s.to_string();
								return Result::Text(retval);
							} else {
								let retval: usize = s.to_string().len();
								return Result::Number(retval as i32);
							}
						}
						None => return Result::Text(String::new()),
					}
				}
				None => {

					let brackets_regex = match Regex::new(r"[^\\]\(") {
						Ok(re) => re,
						Err(err) => panic!("{}", err),
					};	
					let brackets_caps = brackets_regex.captures(&last_token);

					match brackets_caps {
						Some(_) => return Result::Text(String::new()),
						None => return Result::Number(0),
					}
	
				}
			}		
		}
		None => panic!("Regex expected, nothing found!")
	}
}


fn parse_expr_term40(tokens: &mut Vec<String>) -> Result {
	// term40 -> [match term40 regexp, substr term40 term40 term40,
	// 		index term40 term40, length term40, + token, \( term00 \), const]

	let mut last_token: String;

	match tokens.pop() {
		Some(s) => last_token = s,
		None => panic!("Syntax error")
	}

	if last_token == "match" {

		let arg: Result = parse_expr_term40(tokens);
		
		let text: String;
		match arg {
			Result::Number(n) => text = n.to_string(),
			Result::Text(t) => text = t,
		}
		return match_fun(tokens, text);
		
	} else if last_token == "substr" {
		
		let arg1: Result = parse_expr_term40(tokens);
		let text: String;
		match arg1 {
			Result::Number(n) => text = n.to_string(),
			Result::Text(t) => text = t,
		}

		let arg2: Result = parse_expr_term40(tokens);
		let mut pos: i32;
		match arg2 {
			Result::Number(n) => pos = n,
			Result::Text(t) => {
				match t.parse::<i32>() {
					Ok(ok) => pos = ok,
					Err(_) => return Result::Text(String::new()),
				}
			}
		}

		let arg3: Result = parse_expr_term40(tokens);
		let mut len: i32;
		match arg3 {
			Result::Number(n) => len = n,
			Result::Text(t) => {
				match t.parse::<i32>() {
					Ok(ok) => len = ok,
					Err(_) => return Result::Text(String::new()),
				}
			}
		}

		if pos <= 0 || len <= 0 {
			return Result::Text(String::new());
		}

		let mut sub: String = String::new();
		for (i, c) in text.chars().enumerate() {
			let i: i32 = i as i32;
			if (pos - 1) <= i && i < (pos + len - 1) {
				sub.push(c);
			}
		}
		return Result::Text(sub);
	} else if last_token == "index" {

		let arg1: Result = parse_expr_term40(tokens);
		let text: String;
		match arg1 {
			Result::Number(n) => text = n.to_string(),
			Result::Text(t) => text = t,
		}

		let arg2: Result = parse_expr_term40(tokens);
		let mut charset = HashSet::<char>::new();
		match arg2 {
			Result::Number(n) => {
				let t = n.to_string();
				for c in t.chars() {
					charset.insert(c);
				}
			}
			Result::Text(t) => {
				for c in t.chars() {
					charset.insert(c);
				}
			}
		}

		for (i, c) in text.chars().enumerate() {
			if charset.contains(&c) {
				return Result::Number(i as i32 + 1);
			}
		}		
		return Result::Number(0);

	} else if last_token == "length" {

		let arg1: Result = parse_expr_term40(tokens);
		let text: String;
		match arg1 {
			Result::Number(n) => text = n.to_string(),
			Result::Text(t) => text = t,
		}

		if text.is_empty() {
			return Result::Number(0);
		}
		return Result::Number(text.len() as i32);

	} else if last_token == "+" {

		let token = tokens.pop();
		match token {
			Some(s) => return Result::Text(s),
			None => panic!("Syntax error"),
		}
		
	} else if last_token == "(" {

		let arg: Result = parse_expr_term00(tokens);
		match tokens.pop() {
			Some(s) => {
				if s != ")" {
					println!("{:?}", s);
					panic!("Syntax error");
				}
			}
			None => panic!("Syntax error"),
		}
		return arg;
	} else {

		let last_to_int = last_token.parse::<i32>();
		match last_to_int {
			Ok(number) => return Result::Number(number),
			Err(_) => {}
		}
		return Result::Text(last_token);
	}
}


fn parse_expr_term30(tokens: &mut Vec<String>) -> Result {
	// term30 -> term40 (: regexp)*

	let arg: Result = parse_expr_term40(tokens);

	let mut last_token: String;
	let mut retval: Result = arg;

	while !tokens.is_empty() {

		match tokens.pop() {
			Some(s) => last_token = s,
			None => last_token = String::new(),
		}
		if last_token != ":" {
			tokens.push(last_token);
			return retval;
		} else {
			match retval {
				Result::Number(n) => retval = match_fun(tokens, n.to_string()),
				Result::Text(t) => retval = match_fun(tokens, t),
			}
		}
	}

	return retval;
}

fn multiply(val0: i32, val1: i32) -> i32 { val0 * val1 }
fn divide(val0: i32, val1: i32) -> i32 { val0 / val1}
fn modulate(val0: i32, val1: i32) -> i32 {val0 % val1}

fn parse_expr_term20(tokens: &mut Vec<String>) -> Result {
	// term20 -> term30 ([*, /, %] term30)*
	
	let mut arg0: Result = parse_expr_term30(tokens);

	loop {
		let operation: fn(i32, i32) -> i32;
		match tokens.last() {
			Some(s) => {
				if s == "*" {
					operation = multiply;
				} else if s == "/" {
					operation = divide;
				} else if s == "%" {
					operation = modulate;
				} else {
					break;
				}
			},
			None => break
		}
		tokens.pop();
		let arg1: Result = parse_expr_term30(tokens);
		arg0 = Result::Number(invoke_numeric_expr(arg0, arg1, operation));
	}

	return arg0;
}


fn add(val0: i32, val1: i32) -> i32 { val0 + val1 }
fn subtract(val0: i32, val1: i32) -> i32 { val0 - val1}

fn parse_expr_term10(tokens: &mut Vec<String>) -> Result {
	// term10 -> term20 ([+, -] term20)*

	let mut arg0: Result = parse_expr_term20(tokens);

	loop {
		let operation: fn(i32, i32) -> i32;
		match tokens.last() {
			Some(s) => {
				if s == "+" {
					operation = add;
				} else if s == "-" {
					operation = subtract;
				} else {
					break;
				}
			},
			None => break
		}
		tokens.pop();
		let arg1: Result = parse_expr_term20(tokens);
		arg0 = Result::Number(invoke_numeric_expr(arg0, arg1, operation));
	}

	return arg0;
}


fn relation_0(val0: Result, val1: Result) -> Result { // relation |		
	match val0 {
		Result::Number(n) => {
			if n != 0 { return Result::Number(n) }
		}
		Result::Text(t) => {
			if t != "" { return Result::Text(t) }
		}
	}
	match val1 {
		Result::Number(n) => {
			if n != 0 { return Result::Number(n) }
		}
		Result::Text(t) => {
			if t != "" { return Result::Text(t) }
		}
	}
	return Result::Number(0);
}
fn relation_1(val0: Result, val1: Result) -> Result { // relation &
	match val1 {
		Result::Number(n) => {
			if n == 0 { return Result::Number(0) }
		}
		Result::Text(t) => {
			if t == "" { return Result::Number(0) }
		}
	}
	match val0 {
		Result::Number(n) => {
			if n != 0 { return Result::Number(n) }
		}
		Result::Text(t) => {
			if t != "" { return Result::Text(t) }
		}
	}
	return Result::Number(0);
}

fn equal_int(val0: i32, val1: i32) -> i32 { if val0 == val1 {1} else {0} }
fn equal_str(val0: String, val1: String) -> i32 { if val0 == val1 {1} else {0} }
fn not_equal_int(val0: i32, val1: i32) -> i32 { if val0 == val1 {0} else {1} }
fn not_equal_str(val0: String, val1: String) -> i32 { if val0 == val1 {0} else {1} }
fn less_int(val0: i32, val1: i32) -> i32 { if val0 < val1 {1} else {0} }
fn less_str(val0: String, val1: String) -> i32 { if val0 < val1 {1} else {0} }
fn less_equal_int(val0: i32, val1: i32) -> i32 { if val0 <= val1 {1} else {0} }
fn less_equal_str(val0: String, val1: String) -> i32 { if val0 <= val1 {1} else {0} }
fn greater_int(val0: i32, val1: i32) -> i32 { if val0 > val1 {1} else {0} }
fn greater_str(val0: String, val1: String) -> i32 { if val0 > val1 {1} else {0} }
fn greater_equal_int(val0: i32, val1: i32) -> i32 { if val0 >= val1 {1} else {0} }
fn greater_equal_str(val0: String, val1: String) -> i32 { if val0 >= val1 {1} else {0} }

fn parse_expr_term00(tokens: &mut Vec<String>) -> Result {
	// term00 -> term10 ([|, &, ==, !=, <, <=, >, >=] term10)*

	let mut arg0: Result = parse_expr_term10(tokens);

	loop {
		let operation_int: fn(i32, i32) -> i32;
		let operation_str: fn(String, String) -> i32;
		
		let last_token: String;

		match tokens.pop() {
			Some(s) => last_token = s,
			None => break
		}

		if last_token == "|" {
			let arg1: Result = parse_expr_term10(tokens);
			arg0 = relation_0(arg0, arg1);
			continue			
		} else if last_token == "&" {
			let arg1: Result = parse_expr_term10(tokens);
			arg0 = relation_1(arg0, arg1);
			continue
		} else if last_token == "==" {
			operation_int = equal_int;
			operation_str = equal_str;
		} else if last_token == "!=" {
			operation_int = not_equal_int;
			operation_str = not_equal_str;
		} else if last_token == "<" {
			operation_int = less_int;
			operation_str = less_str;
		} else if last_token == "<=" {
			operation_int = less_equal_int;
			operation_str = less_equal_str;
		} else if last_token == ">" {
			operation_int = greater_int;
			operation_str = greater_str;
		} else if last_token == ">=" {	
			operation_int = greater_equal_int;
			operation_str = greater_equal_str;
		} else {
			tokens.push(last_token);
			break;
		}
		
		let arg1: Result = parse_expr_term20(tokens);
		arg0 = Result::Number(invoke_relations(arg0, arg1, operation_int, operation_str));
	}

	return arg0
}

pub fn uumain(args: Vec<String>) -> i32 {
	let program = &args[0];
	let opts = [
		getopts::optflag("h", "help", "display this help and exit"),
		getopts::optflag("V", "version", "output version information and exit"),
	];
	let matches = match getopts::getopts(&args[1..], &opts) {
        Ok(m) => m,
        Err(f) => panic!("Invalid options\n{}", f)
    };
    if matches.opt_present("help") {
        println!("expr 1.0.0");
        println!("");
        println!("Usage:");
        println!("  {0} [OPTION]... [ARGS]...", program);
        println!("");
        print!("{}", &getopts::usage("Evaluate expression.", &opts)[..]);
        println!("");
        println!("With no FILE, or when FILE is -, read standard input.");
        return 0;
    }
    if matches.opt_present("version") {
        println!("expr 1.0.0");
        return 0;
    }

	let mut tokens = matches.free;

	tokens.reverse();

	let result = parse_expr_term00(&mut tokens);

	if !tokens.is_empty() {
		panic!("Syntax error");
	}

	match result {
		Result::Number(n) => println!("{:?}", n),
		Result::Text(t) => println!("{:?}", t),
	}
	
	0
}


