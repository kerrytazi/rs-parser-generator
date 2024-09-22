extern crate proc_macro;
extern crate proc_macro2;
extern crate quote;
extern crate syn;

use proc_macro2::{ TokenStream, TokenTree };
use quote::{ format_ident, quote, quote_spanned, TokenStreamExt };

macro_rules! span_err {
	($s: expr, $msg: expr) => {
		Err(quote_spanned!{ $s.span() => compile_error!($msg); })
	};
}

fn snake_to_pascal(s: &str) -> String {
	s
		.split('_')
		.map(|v| {
			let pair = v.split_at(1);
			pair.0.to_ascii_uppercase() + pair.1
		})
		.collect::<Vec<_>>()
		.join("")
}

#[derive(Debug)]
#[derive(PartialEq)]
enum RuleCount {
	One,
	OneOrMore,
	ZeroOrMore,
	ZeroOrOne,
}

#[derive(Debug)]
enum RuleType {
	Ident(String),
	Literal(String),
	Group(RuleDecl),
}

#[derive(Debug)]
struct Rule {
	count: RuleCount,
	rtype: RuleType,
}

impl Rule {
	pub fn new(rule_type: RuleType) -> Rule {
		Rule {
			count: RuleCount::One,
			rtype: rule_type,
		}
	}
}

#[derive(Debug)]
struct RuleDecl {
	name: String,
	name_pascal: String,
	subrules: Vec<Vec<Rule>>,
}

fn parse_lit(lit: &proc_macro2::Literal) -> String {
	use syn::{ parse_macro_input, LitStr };

	let mut result = String::new();
	let mut valid = false;

	let _ = (|| {
		// TODO: parse without `syn` crate
		let inp = quote! { #lit }.into();
		let lit = parse_macro_input!(inp as LitStr);
		result = lit.value();
		valid = true;
		return proc_macro::TokenStream::new();
	})();

	if !valid {
		panic!("TODO");
	}

	return result;
}

fn parse_rule(name: &str, name_pascal: &str, it: &mut proc_macro2::token_stream::IntoIter, last_tt: &mut TokenTree, group: bool) -> Result<(Vec<Vec<Rule>>, TokenStream), TokenStream> {
	let mut subrules = Vec::new();
	subrules.push(Vec::new());

	let mut token_types = TokenStream::new();

	let mut group_index = 0;

	loop {
		let tt = if let Some(v) = it.next() {
			*last_tt = v.clone();
			v
		} else {
			if group {
				break; // group ends without braces
			} else {
				return span_err!(last_tt, "unexpected EOF. missing semicolon");
			}
		};

		// println!("{:?}", tt); // debug

		match &tt {
			TokenTree::Group(v) => {
				if v.delimiter() != proc_macro2::Delimiter::Parenthesis {
					return span_err!(tt, "unexpected brackets");
				}

				let rd = {
					let mut it = v.stream().into_iter();
					let mut last_tt = last_tt.clone();

					let name = format!("{}_g{}", name, group_index);
					let name_pascal = format!("{}G{}", name_pascal, group_index);

					let (subrules, parsed_token_types) = parse_rule(&name, &name_pascal, &mut it, &mut last_tt, true)?;

					token_types.append_all([
						TokenTree::Ident(format_ident!("{}", name_pascal)),
						TokenTree::Punct(proc_macro2::Punct::new(',', proc_macro2::Spacing::Alone)),
					]);

					token_types.append_all(parsed_token_types);

					RuleDecl {
						name,
						name_pascal,
						subrules,
					}
				};

				subrules.last_mut().unwrap().push(Rule::new(RuleType::Group(rd)));
				group_index += 1;
			},
			TokenTree::Ident(v) => {
				subrules.last_mut().unwrap().push(Rule::new(RuleType::Ident(v.to_string())));
			},
			TokenTree::Punct(v) => {
				match v.as_char() {
					';' => {
						if subrules.last().unwrap().is_empty() {
							return span_err!(tt, "expected rule before `;`");
						}

						break;
					},
					'|' => {
						subrules.push(Vec::new());
					},
					'+' => {
						if let Some(subrule) = subrules.last_mut().unwrap().last_mut() {
							if subrule.count != RuleCount::One {
								return span_err!(tt, "multiple count modifiers are not allowed");
							}
							subrule.count = RuleCount::OneOrMore;
						} else {
							return span_err!(tt, "expected rule before `+`");
						}
					},
					'*' => {
						if let Some(subrule) = subrules.last_mut().unwrap().last_mut() {
							if subrule.count != RuleCount::One {
								return span_err!(tt, "multiple count modifiers are not allowed");
							}
							subrule.count = RuleCount::ZeroOrMore;
						} else {
							return span_err!(tt, "expected rule before `*`");
						}
					},
					'?' => {
						if let Some(subrule) = subrules.last_mut().unwrap().last_mut() {
							if subrule.count != RuleCount::One {
								return span_err!(tt, "multiple count modifiers are not allowed");
							}
							subrule.count = RuleCount::ZeroOrOne;
						} else {
							return span_err!(tt, "expected rule before `?`");
						}
					},
					_ => {
						return span_err!(tt, "unexpected symbol");
					}
				}
			},
			TokenTree::Literal(v) => {
				let s = parse_lit(v);
				subrules.last_mut().unwrap().push(Rule::new(RuleType::Literal(s)))
			},
		}
	}

	return Ok((subrules, token_types));
}

struct Headers {
	graphviz: bool,
	debug: bool,
	allocator: bool,
	module: Option<String>,
}

fn parse_headers(it: &mut proc_macro2::token_stream::IntoIter) -> Result<Headers, TokenStream> {
	let mut result = Headers {
		graphviz: false,
		debug: false,
		allocator: false,
		module: None,
	};

	let mut itc = it.clone();

	loop {
		let mut t;

		t = if let Some(v) = itc.next() { v } else { break; };
		let v = if let TokenTree::Punct(v) = &t { v } else { break; };
		if v.as_char() != '@' { return span_err!(t, "unexpected symbol"); }

		t = if let Some(v) = itc.next() { v } else { return span_err!(t, "unexpected EOF. missing identifier"); };
		let v = if let TokenTree::Ident(v) = &t { v } else { return span_err!(t, "unexpected token"); };
		match v.to_string().as_str() {
			"graphviz" => { result.graphviz = true; },
			"debug" => { result.debug = true; },
			"allocator" => { result.allocator = true; },
			"mod" => {
				t = if let Some(v) = itc.next() { v } else { return span_err!(t, "unexpected EOF. missing identifier"); };
				let v = if let TokenTree::Ident(v) = &t { v } else { return span_err!(t, "unexpected token"); };
				result.module = Some(v.to_string());
			},
			_ => { return span_err!(v, "unknown header"); },
		}

		t = if let Some(v) = itc.next() { v } else { return span_err!(t, "unexpected EOF. missing semicolon"); };
		let v = if let TokenTree::Punct(v) = &t { v } else { return span_err!(t, "unexpected token"); };
		if v.as_char() != ';' { return span_err!(t, "unexpected symbol"); }

		*it = itc.clone();
	}

	return Ok(result);
}

fn parse_rules(it: &mut proc_macro2::token_stream::IntoIter) -> Result<(Vec<RuleDecl>, TokenStream), TokenStream> {
	let mut last_tt;
	let mut token_types = TokenStream::new();
	let mut rules = Vec::new();

	loop {
		let name_tt = if let Some(v) = it.next() {
			last_tt = v.clone();
			v
		} else {
			break;
		};

		let name = if let TokenTree::Ident(v) = &name_tt {
			v.to_string()
		} else {
			return span_err!(name_tt, "expected identifier");
		};

		let name_pascal = snake_to_pascal(&name);

		token_types.append_all([
			TokenTree::Ident(format_ident!("{}", name_pascal)),
			TokenTree::Punct(proc_macro2::Punct::new(',', proc_macro2::Spacing::Alone)),
		]);

		if let Some(v) = it.next() {
			last_tt = v.clone();
			if let TokenTree::Punct(p) = &v {
				if p.as_char() != ':' {
					return span_err!(v, "expected colon");
				}
			} else {
				return span_err!(v, "expected colon");
			}
		} else {
			return span_err!(last_tt, "unexpected EOF. missing colon");
		}

		let (subrules, parsed_token_types) = parse_rule(&name, &name_pascal, it, &mut last_tt, false)?;

		rules.push(RuleDecl {
			name,
			name_pascal,
			subrules,
		});

		token_types.append_all(parsed_token_types);
	}

	return Ok((rules, token_types));
}

struct HeadersTokens {
	debug: TokenStream,
	graphviz: TokenStream,
	allocator_decl: TokenStream,
	allocator_def: TokenStream,
	allocator_def_vec: TokenStream,
	allocator_pass_decl: TokenStream,
	allocator_pass_def: TokenStream,
	vector_new: TokenStream,
}

fn make_ands_subrule(mut it: std::slice::Iter<'_, Rule>, name: &str, name_pascal: &str, groups: &mut TokenStream, toks: &HeadersTokens) -> (TokenStream, bool) {
	let toks_allocator_pass_def = &toks.allocator_pass_def;

	if let Some(rule) = it.next() {
		let (inner_and_subrules, last) = make_ands_subrule(it, name, name_pascal, groups, toks);

		let rule_invoke = match &rule.rtype {
			RuleType::Ident(s) => {
				let func_name = format_ident!("parse_{}", s);
				quote! { #func_name(rest #toks_allocator_pass_def) }
			},
			RuleType::Literal(s) => {
				let lit_s = proc_macro2::Literal::string(&s);
				quote! { __parse_literal(rest, #lit_s #toks_allocator_pass_def) }
			},
			RuleType::Group(rd) => {
				groups.append_all(gen_rules(&rd.name, &rd.name_pascal, &rd.subrules, toks));

				let func_name = format_ident!("parse_{}", rd.name);
				quote! { #func_name(rest #toks_allocator_pass_def) }
			},
		};

		let mut propagate_last = false;

		let body = match rule.count {
			RuleCount::One => {
				if last {
					quote! {
						if let Some(ParseResult{ tok, rest }) = #rule_invoke {
							tree.push(tok);
							#inner_and_subrules
						}
					}
				} else {
					quote! {
						if let Some(ParseResult{ tok, rest }) = #rule_invoke {
							let last_size = tree.len();
							tree.push(tok);
							#inner_and_subrules
							tree.truncate(last_size);
						}
					}
				}
			},
			RuleCount::OneOrMore => {
				if last {
					quote! {
						if let Some(ParseResult{ tok, mut rest }) = #rule_invoke {
							tree.push(tok);
							while let Some(ParseResult{ tok, rest: rest_while }) = #rule_invoke {
								tree.push(tok);
								rest = rest_while;
							}
							#inner_and_subrules
						}
					}
				} else {
					quote! {
						if let Some(ParseResult{ tok, rest }) = #rule_invoke {
							let last_size = tree.len();
							tree.push(tok);
							while let Some(ParseResult{ tok, rest: rest_while }) = #rule_invoke {
								tree.push(tok);
								rest = rest_while;
							}
							#inner_and_subrules
							tree.truncate(last_size);
						}
					}
				}
			},
			RuleCount::ZeroOrMore => {
				if last {
					propagate_last = true;
					quote! {
						let mut rest = rest;
						while let Some(ParseResult{ tok, rest: rest_while }) = #rule_invoke {
							tree.push(tok);
							rest = rest_while;
						}
						#inner_and_subrules
					}
				} else {
					quote! {
						let mut rest = rest;
						let last_size = tree.len();
						while let Some(ParseResult{ tok, rest: rest_while }) = #rule_invoke {
							tree.push(tok);
							rest = rest_while;
						}
						#inner_and_subrules
						tree.truncate(last_size);
					}
				}
			},
			RuleCount::ZeroOrOne => {
				if last {
					propagate_last = true;
					quote! {
						let mut rest = rest;
						if let Some(ParseResult{ tok, rest: rest_if }) = #rule_invoke {
							tree.push(tok);
							rest = rest_if;
						}
						#inner_and_subrules
					}
				} else {
					quote! {
						let mut rest = rest;
						let last_size = tree.len();
						if let Some(ParseResult{ tok, rest: rest_if }) = #rule_invoke {
							tree.push(tok);
							rest = rest_if;
						}
						#inner_and_subrules
						tree.truncate(last_size);
					}
				}
			},
		};

		return (body, propagate_last);
	} else {
		let token_type = format_ident!("{}", name_pascal);

		return (quote! {
			return Some(
				ParseResult {
					tok: Token {
						ttype: TokenType::#token_type,
						tree,
						source: s.split_at(s.len() - rest.len()).0,
					},
					rest,
				}
			);
		}, true);
	}
}

fn gen_rules(name: &str, name_pascal: &str, subrules: &Vec<Vec<Rule>>, toks: &HeadersTokens) -> TokenStream {
	let toks_allocator_decl = &toks.allocator_decl;
	let toks_allocator_def = &toks.allocator_def;
	let toks_allocator_pass_decl = &toks.allocator_pass_decl;
	let toks_vector_new = &toks.vector_new;

	let func_name = format_ident!("parse_{}", name);

	let mut ors_tokens = TokenStream::new();
	let mut groups = TokenStream::new();
	let mut last = false;

	for or_subrules in subrules {
		let (and_subrules, _last) = make_ands_subrule(or_subrules.iter(), name, name_pascal, &mut groups, toks);
		ors_tokens.append_all(and_subrules);
		last = _last;
	}

	if last {
		return quote! {
			#groups
			pub fn #func_name<'a #toks_allocator_decl>(s: &'a str #toks_allocator_pass_decl) -> Option<ParseResult<'a #toks_allocator_def>> {
				let mut tree = #toks_vector_new;
				let rest = s;
				#ors_tokens
			}
		};
	} else {
		return quote! {
			#groups
			pub fn #func_name<'a #toks_allocator_decl>(s: &'a str #toks_allocator_pass_decl) -> Option<ParseResult<'a #toks_allocator_def>> {
				let mut tree = #toks_vector_new;
				let rest = s;
				#ors_tokens
				return None;
			}
		};
	}
}

#[proc_macro]
pub fn rules(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
	let input = TokenStream::from(input);
	let mut it = input.into_iter();

	let headers = match parse_headers(&mut it) {
		Ok(v) => v,
		Err(v) => { return v.into(); },
	};

	let (rules, token_types) = match parse_rules(&mut it) {
		Ok(v) => v,
		Err(v) => { return v.into(); },
	};

	let toks = {
		let debug = if headers.debug { quote! { #[derive(Debug)] } } else { TokenStream::new() };

		HeadersTokens {
			debug: debug.clone(),
			graphviz: if headers.graphviz { quote! { #[derive(Debug)] } } else { debug.clone() },
			allocator_decl: if headers.allocator { quote! { , 'b, A: std::alloc::Allocator } } else { TokenStream::new() },
			allocator_def: if headers.allocator { quote! { , 'b, A } } else { TokenStream::new() },
			allocator_def_vec: if headers.allocator { quote! { , &'b A } } else { TokenStream::new() },
			allocator_pass_decl: if headers.allocator { quote! { , allocator: &'b A } } else { TokenStream::new() },
			allocator_pass_def: if headers.allocator { quote! { , allocator } } else { TokenStream::new() },
			vector_new: if headers.allocator { quote! { Vec::new_in(allocator) } } else { quote! { Vec::new() } },
		}
	};

	let toks_debug = &toks.debug;
	let toks_graphviz = &toks.graphviz;
	let toks_allocator_decl = &toks.allocator_decl;
	let toks_allocator_def = &toks.allocator_def;
	let toks_allocator_def_vec = &toks.allocator_def_vec;
	let toks_allocator_pass_decl = &toks.allocator_pass_decl;
	let toks_vector_new = &toks.vector_new;

	let mut result = quote! {
		#toks_graphviz
		pub enum TokenType {
			_Literal,
			#token_types
		}

		#toks_debug
		pub struct Token<'a #toks_allocator_decl> {
			pub ttype: TokenType,
			pub tree: Vec<Token<'a #toks_allocator_def> #toks_allocator_def_vec>,
			pub source: &'a str,
		}

		#toks_debug
		pub struct ParseResult<'a #toks_allocator_decl> {
			pub tok: Token<'a #toks_allocator_def>,
			pub rest: &'a str,
		}

		pub fn __parse_literal<'a #toks_allocator_decl>(s: &'a str, lit: &str #toks_allocator_pass_decl) -> Option<ParseResult<'a #toks_allocator_def>> {
			if s.starts_with(lit) {
				let strs = s.split_at(lit.len());
				return Some(
					ParseResult {
						tok: Token {
							ttype: TokenType::_Literal,
							tree: #toks_vector_new,
							source: strs.0,
						},
						rest: strs.1,
					}
				);
			}
			return None;
		}
	};

	for rule_decl in rules {
		let res = gen_rules(&rule_decl.name, &rule_decl.name_pascal, &rule_decl.subrules, &toks);
		result.append_all(res);
	}

	if headers.graphviz {
		result.append_all(quote! {
			impl<'a #toks_allocator_decl> Token<'a #toks_allocator_def> {
				pub fn graphviz(&self) -> String {
					let mut index = 1;
					let mut connections = self._graphviz(&mut index, 0);
					connections += &format!("a{}[label=\"'{}'\"shape=\"parallelogram\"]", 0, self.source);
					return format!("digraph {{{}}}", connections);
				}

				fn _graphviz(&self, index: &mut usize, parent_index: usize) -> String {
					let my_index = *index;
					*index += 1;

					let mut result = String::new();

					if let TokenType::_Literal = self.ttype {
						result += &format!("a{}[label=\"'{}'\"shape=\"rect\"]", my_index, self.source);
					} else {
						result += &format!("a{}[label=\"{:?}\"]", my_index, self.ttype);
					}
					result += &format!("a{}->a{};", parent_index, my_index);

					for tok in &self.tree {
						result += &tok._graphviz(index, my_index);
					}

					return result;
				}
			}
		});
	}

	if let Some(mod_name) = headers.module {
		let mod_name_tok = format_ident!("{}", mod_name);
		result = quote! {
			mod #mod_name_tok {
				#result
			}
		};
	}

	return result.into();
}
