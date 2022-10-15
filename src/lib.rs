//! # `yaah` – Yet Another AOC Helper for your Advent Of Code adventures
//!
//! This crate handles the boilerplate around fetching, running and benchmarking your input parsing
//! and solution implementations. It is inpired by the excellent
//! [cargo-aoc](https://crates.io/crates/cargo-aoc) cargo extentions.
//!
//! ## Setup
//!
//! This helper will automatically fetch your personnal input file provided you set your AOC
//! session id cookie in the `AOC_SESSION_ID` environment variable. This can be either done through
//! your favorite shell or in a `.cargo/config.toml` file as such:
//! ```toml
//! [env]
//! AOC_SESSION_ID = "your_super_long_session_id_extracted_from_your_browsers_cookie_jar"
//! ```
//!
//! Note that this `.cargo` directory can reside in a parent directory of your project allowing you
//! to have a project specific version controlled cargo config file without leaking your secret
//! session id.
//!
//! If you want to benefit from the auto-generated main function, you will need to add a
//! dependency to clap.
//!
//! ```toml
//! clap = { version = "4.0", features = ["derive"] }
//! ```
//!
//! If you want to benefit from the auto-generated bench marks, your project's manifest will require
//! these extra elements:
//!
//! ```toml
//! [dependencies]
//! criterion = "0.4.0"
//!
//! [[bench]]
//! # Adjust that name to that of your file in the `./benches` directory.
//! name = "aoc-bench"
//! harness = false
//! ```
//!
//! For your convenience a template project is availble. Simply bootstrap your Advent of code with:
//! ```sh
//! cargo generate --git https://github.com/ithinuel/yaah-template.git
//! ```

use std::cell::Cell;
use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::collections::HashSet;
//use std::io::Write;

use itertools::Itertools;
use pm::TokenStream;
use proc_macro as pm;
use proc_macro2 as pm2;
use proc_macro_error::Level;
use proc_macro_error::{proc_macro_error, Diagnostic, ResultExt};
use quote::quote;
use std::collections::HashMap;
use syn::spanned::Spanned;
use syn::ItemFn;
use syn::ReturnType;
use types::Solution;

mod types;
mod utils;

thread_local! {
    static YEAR: Cell<Option<usize>> = Cell::new(None);
    static MAP: RefCell<HashMap<types::DayPartName, types::Solution>> = RefCell::new(HashMap::new());
}

/// # Attribute for the generator functions
///
/// This attribute can accept up to 3 arguments.
/// - Day: mandatory following the syntax `dayN` where N is in [1, 25]
/// - Part: optional following the syntax `partN` where N is in [1, 2]
/// - Name: optional a string typically used for alternate solution implementations.
///
/// See [`aoc`][macro@aoc] for details about how generator are resolved.
///
/// The return type of the generator may be any type with static lifespan.
/// If it is an Option or a Result, then the caller will grace fully stop if `None` or `Err(_)` is
/// returned.
///
/// ## Example
///
/// ```ignore
/// # // block is ignored because this is not build from the root of the test crate.
/// use aoc_helper::aoc_generator;
///
/// #[aoc_generator(day3, part2, named_solution)]
/// fn my_fancy_generator(input: &'static str) -> SomeType {
///     todo!()
/// }
/// ```
#[proc_macro_attribute]
#[proc_macro_error(assert_unwind_safe)]
pub fn aoc_generator(args: pm::TokenStream, mut input: pm::TokenStream) -> pm::TokenStream {
    let dpn: types::DayPartName = syn::parse(args)
        .map_err(|e| {
            Diagnostic::from(e).help(
                "This arguments should follow the following syntax:\n\
                 #[aoc_generator(day1, part2, name)]\n\
                 where `part` and `name` are optional and `name` is any identifier."
                    .to_string(),
            )
        })
        .unwrap_or_abort();

    let gen: ItemFn = syn::parse(input.clone()).unwrap_or_abort();
    let span = gen.span();
    let camelcased = syn::Ident::new(&dpn.to_camelcase(), pm2::Span::call_site());
    let snakecased = syn::Ident::new(&dpn.to_snakecase(), pm2::Span::call_site());
    let snakecased = quote::format_ident!("__{}_generate", snakecased);

    // Register in the map
    MAP.with(|map| match map.borrow_mut().entry(dpn) {
        Entry::Vacant(entry) => {
            entry.insert(Solution {
                generator: Some(pm2::Span::call_site()),
                solver: None,
            });
        }
        Entry::Occupied(mut entry) => {
            if let Some(generator) = entry.get().generator {
                Diagnostic::spanned(span, Level::Error, "Generator already defined".to_string())
                    .span_note(generator, "Previous is located here.".to_string())
                    .abort();
            } else {
                entry.get_mut().generator = Some(gen.span());
            }
        }
    });

    let ident = gen.sig.ident;
    let gen_out = match gen.sig.output {
        ReturnType::Default => {
            Diagnostic::spanned(
                span,
                Level::Error,
                "Generator's return type is missing.".to_string(),
            )
            .abort();
        }
        ReturnType::Type(_, ty) => ty,
    };
    use types::ResultType;
    let gen_out = types::extract_result_type(*gen_out);

    let gen_impl = TokenStream::from(match gen_out {
        ResultType::Plain(gen_out) => {
            quote! {
            mod #snakecased {
                use crate::{ Generator, #camelcased };
                use super::*;

                impl Generator for #camelcased {
                    type GenOutput = #gen_out;
                    fn generate(input: &'static str) -> Result<Self::GenOutput, String> {
                        Ok(#ident(input))
                    }
                }
            }}
        }
        ResultType::Option(gen_out) => {
            quote! {
            mod #snakecased {
                use crate::{ Generator, #camelcased };
                use super::*;

                impl Generator for #camelcased {
                    type GenOutput = #gen_out;
                    fn generate(input: &'static str) -> Result<Self::GenOutput, String> {
                        #ident(input).ok_or_else(|| "Generator returned no result".to_string())
                    }
                }

            }}
        }
        ResultType::Result(gen_out) => {
            quote! {
            mod #snakecased {
                use crate::{ Generator, #camelcased };
                use super::*;

                impl Generator for #camelcased {
                    type GenOutput = #gen_out;
                    fn generate(input: &'static str) -> Result<Self::GenOutput, String> {
                        #ident(input).map_err(|e| format!("Generator failed with: {}", e))
                    }
                }

            }}
        }
    });
    //let _ = std::fs::OpenOptions::new()
    //    .create(true)
    //    .append(true)
    //    .open("output.rs")
    //    .map(|mut file| file.write(format!("{}\n", gen_impl).as_bytes()));
    input.extend([gen_impl]);
    input
}

/// # Attribute for the solver functions
///
/// This attribute can accept up to 3 arguments.
/// - Day: mandatory following the syntax `dayN` where N is in [1, 25]
/// - Part: optional following the syntax `partN` where N is in [1, 2]
/// - Name: optional a string typically used for alternate solution implementations.
///
/// `yaah` will match the solver method to the first matching triple in this order:
/// 1. exact match for Day/Part/Name
/// 1. exact match for Day/Part
/// 1. exact match for Day
/// 1. use the raw `&'static str` input.
///
/// The return type of a solution must implement `Display`. If it is an Option or a Result, then the
/// caller will grace fully stop if `None` or `Err(_)` is returned.
///
/// ## Example
///
/// ```ignore
/// # // block is ignored because this is not build from the root of the test crate.
/// use aoc_helper::aoc;
///
/// #[aoc(day3, part2, named_solution)]
/// pub fn my_fancy_solution(input: &SomeType) -> AnotherType {
///     todo!()
/// }
/// ```
#[proc_macro_attribute]
#[proc_macro_error(assert_unwind_safe)]
pub fn aoc(args: pm::TokenStream, mut input: pm::TokenStream) -> pm::TokenStream {
    let dpn: types::DayPartName = syn::parse(args)
        .map_err(|e| {
            Diagnostic::from(e).help(
                "This arguments should follow the following syntax:\n\
                 #[aoc(day1, part2, name)]\n\
                 where `part` and `name` are optional and `name` is any identifier."
                    .to_string(),
            )
        })
        .unwrap_or_abort();

    let solver: ItemFn = syn::parse(input.clone()).unwrap_or_abort();
    let span = solver.span();
    let camelcased = syn::Ident::new(&dpn.to_camelcase(), pm2::Span::call_site());
    let mut snakecased = dpn.to_snakecase();
    snakecased.push_str("_solver");
    let snakecased = syn::Ident::new(&snakecased, pm2::Span::call_site());

    // Register in the map
    MAP.with(|map| match map.borrow_mut().entry(dpn.clone()) {
        Entry::Vacant(entry) => {
            entry.insert(Solution {
                generator: None,
                solver: Some(pm2::Span::call_site()),
            });
        }
        Entry::Occupied(mut entry) => {
            if let Some(solver) = entry.get().solver {
                Diagnostic::spanned(span, Level::Error, "Generator already defined".to_string())
                    .span_note(solver, "Previous is located here.".to_string())
                    .abort();
            } else {
                entry.get_mut().solver = Some(solver.span());
            }
        }
    });

    let ident = solver.sig.ident;
    let runner_out = match solver.sig.output {
        ReturnType::Default => {
            Diagnostic::spanned(
                span,
                Level::Error,
                "Generator's return type is missing.".to_string(),
            )
            .abort();
        }
        ReturnType::Type(_, ty) => ty,
    };

    // check for special result/option return type
    use types::ResultType;
    let runner_out = types::extract_result_type(*runner_out);

    let solver_impl = TokenStream::from(match runner_out {
        ResultType::Plain(runner_out) => quote! {
            mod #snakecased {
                use crate::{ Solver, #camelcased };
                use super::*;

                impl Solver for #camelcased {
                    type Solution = #runner_out;
                    fn solve(input: &Self::GenOutput) -> Result<Self::Solution, String> {
                        Ok(#ident(input))
                    }
                }

            }
        },
        ResultType::Option(runner_out) => quote! {
            mod #snakecased {
                use crate::{ Solver, #camelcased };
                use super::*;

                impl Solver for #camelcased {
                    type Solution = #runner_out;
                    fn solve(input: &Self::GenOutput) -> Result<Self::Solution, String> {
                        #ident(input).ok_or_else(|| "Solver returned no result".to_string())
                    }
                }

            }
        },
        ResultType::Result(runner_out) => quote! {
            mod #snakecased {
                use crate::{ Solver, #camelcased };
                use super::*;

                impl Solver for #camelcased {
                    type Solution = #runner_out;
                    fn solve(input: &Self::GenOutput) -> Result<Self::Solution, String> {
                        #ident(input).map_err(|e| format!("Solver failed: {}", e))
                    }
                }

            }
        },
    });
    //let _ = std::fs::OpenOptions::new()
    //    .create(true)
    //    .append(true)
    //    .open("output.rs")
    //    .map(|mut file| file.write(format!("{}\n", solver_impl).as_bytes()));
    input.extend([solver_impl]);

    input
}

/// Generates the solver methods used by the main function and the benchmarks.
///
/// _**This must be the last item in your `lib.rs`.**_
///
/// This macro optionally accept `with_benchmarks` as an argument to generate the code required by
/// [`aoc_bench`][macro@aoc_bench]
///
/// ## Example
///
/// ```
/// # aoc_helper::aoc_year!(2021);
/// aoc_helper::aoc_lib!(with_benchmarks);
/// ```
#[proc_macro]
#[proc_macro_error(assert_unwind_safe)]
pub fn aoc_lib(args: pm::TokenStream) -> pm::TokenStream {
    let gen_benches = if args.is_empty() {
        false
    } else {
        match syn::parse::<syn::Ident>(args) {
            Ok(ident) if ident == "with_benchmarks" => true,
            Ok(s) => {
                Diagnostic::spanned(s.span(), Level::Error, "Unexpected identifier".to_string())
                    .help(
                        "`aoc_lib` only accept `with_benchmarks` as an optional argument."
                            .to_string(),
                    )
                    .abort()
            }
            Err(e) => Diagnostic::from(e).abort(),
        }
    };

    let year = YEAR
        .with(|y| y.get())
        .ok_or_else(|| {
            Diagnostic::new(
                proc_macro_error::Level::Error,
                "Year has not been defined yet.".to_string(),
            )
            .help("`aoc_year!(2022)` must be defined at the start of the library.".to_string())
        })
        .unwrap_or_abort();

    let map = MAP.with(|map| map.borrow().clone());

    let mut used_generator = HashSet::new();
    map.iter().for_each(|(dpn, sol)| {
        if sol.solver.is_none() {
            return;
        }

        if let (None, Some(alt)) = (sol.generator, dpn.resolve_alternate(&map)) {
            used_generator.insert(alt);
        } else {
            used_generator.insert(dpn.clone());
        };
    });

    let (impls, solvers, benches): (Vec<_>, Vec<_>, Vec<_>) = map
        .iter()
        .sorted_by_key(|(dpn, _)| dpn.clone())
        .filter_map(|(dpn, v)| {
            let camelcased: syn::Ident = syn::parse_str(&dpn.to_camelcase()).unwrap();
            let snakecased: syn::Ident = syn::parse_str(&dpn.to_snakecase()).unwrap();
            let bench_name = quote::format_ident!("__{}_bench", snakecased);
            let snakecased = quote::format_ident!("__{}_solver", snakecased);
            let Solution { generator, solver } = v;

            let input_file = utils::fetch_input(year, dpn.day).map_err(|e| {
                Diagnostic::new(Level::Error, "Failed to fetch input file".to_string())
                    .help("Check that the session ID is set in `AOC_SESSION_ID` and that your network is working.".to_string())
                    .note(format!("The inner error is: {}", e.to_string()))
            }).expect_or_abort("Failed to fetch input file");

            let mut q = quote! {
                pub struct #camelcased;
                impl Input for #camelcased {
                    const INPUT: &'static str = include_str!(#input_file);
                }

                pub fn #snakecased() -> Result<(String, std::time::Duration, std::time::Duration), String> {
                    let start = std::time::Instant::now();
                    let res = #camelcased::generate(#camelcased::INPUT)?;
                    let generation = std::time::Instant::now();
                    let res = #camelcased::solve(&res);
                    let end = std::time::Instant::now();
                    res.map(|r| (r.to_string(), generation - start, end - generation))
                }
            };

            let bench_name = if gen_benches && v.solver.is_some() {
                let name = match (dpn.part, &dpn.name) {
                    (Some(part), Some(name)) => format!("Day {} part {} {}", dpn.day, part, name),
                    (Some(part), None) => format!("Day {} part {}", dpn.day, part),
                    (None, Some(name)) => format!("Day {} {}", dpn.day, name),
                    (None, None) => format!("Day {}", dpn.day),
                };
                q.extend([quote! {
                    fn #bench_name(c: &mut criterion::Criterion) {
                        c.bench_function(#name, |bencher| {
                            let input = #camelcased::INPUT;
                            let gen = #camelcased::generate(input)
                                .expect("Failed to generate input");
                            bencher.iter(|| #camelcased::solve(&gen).expect("Failed to solve"))
                        });
                    }
                }]);
                Some(quote!(#bench_name))
            } else {
                None
            };

            if generator.is_none() {
                if let Some(alt) = dpn.resolve_alternate(&map) {
                    let alt_camelcased =
                        syn::Ident::new(&alt.to_camelcase(), pm2::Span::call_site());
                    q.extend([quote! {
                        impl Generator for #camelcased {
                            type GenOutput = <#alt_camelcased as Generator>::GenOutput;
                            fn generate(input: &'static str) -> Result<Self::GenOutput, String> {
                                #alt_camelcased ::generate(input)
                            }
                        }
                    }]);
                } else {
                    q.extend([quote! {
                        impl Generator for #camelcased {
                            type GenOutput = &'static str;
                            fn generate(input: &'static str) -> Result<Self::GenOutput, String> {
                                Ok(input)
                            }
                        }
                    }]);
                }
            }
            if solver.is_none() {
                q.extend([quote! {
                    impl Solver for #camelcased {
                        type Solution = &'static str;
                        fn solve(_input: &Self::GenOutput) -> Result<Self::Solution, String> {
                            Ok("Solver not implemented")
                        }
                    }
                }]);
            }

            let day = dpn.day;
            let part = if let Some(part) = dpn.part {
                quote! { Some(#part) }
            } else {
                quote! { None }
            };
            let name = if let Some(name) = dpn.name.as_ref() {
                quote! { Some(#name) }
            } else {
                quote! { None }
            };

            let solver = if solver.is_some() || !used_generator.contains(&dpn) {
                Some(quote! {
                    (DayPartName { day: #day, part: #part, name: #name }, #snakecased)
                })
            } else {
                None
            };

            Some((q, solver, bench_name))
        })
        .multiunzip();
    let solvers: Vec<_> = solvers.into_iter().flatten().collect();
    let benches: Vec<_> = benches.into_iter().flatten().collect();
    let count = solvers.len();
    let bench_count = benches.len();

    let mut res = quote! {
        pub trait Input {
            const INPUT: &'static str;
        }
        pub trait Generator: Input {
            type GenOutput;
            fn generate(input: &'static str) -> Result<Self::GenOutput, String>;
        }
        pub trait Solver: Generator {
            type Solution: std::fmt::Display;
            fn solve(input: &Self::GenOutput) -> Result<Self::Solution, String>;
        }

        #(#impls)*

        pub struct DayPartName {
            pub day: usize,
            pub part: Option<usize>,
            pub name: Option<&'static str>
        }

        pub const YEAR: usize = #year;
        pub const SOLVERS: [ (DayPartName, fn() -> Result<(String, std::time::Duration, std::time::Duration), String> ) ; #count ] = [
            #(#solvers),*
        ];

    };

    if gen_benches {
        res.extend([quote! {
            pub const BENCHES: [ fn(&mut criterion::Criterion); #bench_count ] = [
                #(#benches),*
            ];
        }]);
    }

    //let _ = std::fs::OpenOptions::new()
    //    .create(true)
    //    .append(true)
    //    .open("output.rs")
    //    .map(|mut file| file.write(res.to_string().as_bytes()));

    res.into()
}

/// Sets the year for the solutions
///
/// _**This must appear before any other use of yaah's macros.**_
///
/// ## Example
///
/// ```
/// aoc_helper::aoc_year!(2021);
/// ```
#[proc_macro]
#[proc_macro_error(assert_unwind_safe)]
pub fn aoc_year(input: pm::TokenStream) -> pm::TokenStream {
    let Ok(pm2::TokenTree::Literal(year)) = pm2::TokenStream::from(input)
        .into_iter()
        .exactly_one()
    else {
        proc_macro_error::abort_call_site!("Parsing error."; help = "Use aoc_year!(2018).");
    };

    let year: usize = year
        .to_string()
        .parse()
        .map_err(|_| syn::parse::Error::new(year.span(), "Use aoc_year!(2018)."))
        .expect_or_abort("Invalid year format");
    YEAR.with(|y| {
        y.set(Some(year));
    });

    //let _ = std::fs::File::create("output.rs");

    pm::TokenStream::new()
}

/// Generates the main function
///
/// It integrates with `clap` for extra cli-arguments.
///
/// ## Example
///
/// ```
/// # mod your_libs_name {
/// #    aoc_helper::aoc_year!(2021);
/// #    aoc_helper::aoc_lib!();
/// # }
/// #
/// aoc_helper::aoc_main! { your_libs_name }
/// ```
#[proc_macro]
#[proc_macro_error(assert_unwind_safe)]
pub fn aoc_main(arg: pm::TokenStream) -> pm::TokenStream {
    let lib_name = syn::parse_macro_input!(arg as syn::Ident);
    let main = include_str!("main_tpl.rs").to_string();
    let main = main.replace("#lib", &lib_name.to_string());
    let main: syn::File = syn::parse_str(&main).unwrap_or_abort();
    quote!(#main).into()
}

/// Generates the benchmarks
///
/// ## Example
///
/// In lib.rs
/// ```
/// # aoc_helper::aoc_year(2021);
/// aoc_helper::aoc_lib { with_benchmarks }
/// ```
///
/// In `aoc-bench.rs`
/// ```
/// # mod your_libs_name {
/// #    aoc_helper::aoc_year!(2021);
/// #    aoc_helper::aoc_lib!(with_benchmarks);
/// # }
/// #
/// aoc_helper::aoc_bench! { your_libs_name }
/// ```
#[proc_macro]
#[proc_macro_error(assert_unwind_safe)]
pub fn aoc_bench(arg: pm::TokenStream) -> pm::TokenStream {
    let lib = syn::parse_macro_input!(arg as syn::Ident);

    let r = quote! {
        use #lib::{Generator, Input, Solver};
        use criterion::{criterion_group, criterion_main, Criterion};

        pub fn benches() {
            let mut criterion: Criterion<_> = Criterion::default().configure_from_args();
            #lib::BENCHES.iter().for_each(|bench| {
                bench(&mut criterion);
            });
        }
        criterion_main!(benches);
    };

    //let _ = std::fs::File::create("output.rs")
    //    .unwrap()
    //    .write_all(r.to_string().as_bytes());

    r.into()
}
