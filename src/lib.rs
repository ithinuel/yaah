use std::cell::Cell;
use std::cell::RefCell;
use std::collections::hash_map::Entry;

use itertools::Itertools;
use proc_macro as pm;
use proc_macro2 as pm2;
use proc_macro_error::Level;
use proc_macro_error::{proc_macro_error, Diagnostic, ResultExt};
use quote::quote;
use std::collections::HashMap;
use syn::ItemFn;
use syn::ReturnType;
use syn::Type;
use syn::TypeTuple;
use types::Solution;

mod generator {}
mod runner {}

mod types;

thread_local! {
    static YEAR: Cell<Option<usize>> = Cell::new(None);
    static MAP: RefCell<HashMap<types::DayPartName, types::Solution>> = RefCell::new(HashMap::new());
}

#[proc_macro_attribute]
#[proc_macro_error(assert_unwind_safe)]
pub fn aoc_generator(args: pm::TokenStream, input: pm::TokenStream) -> pm::TokenStream {
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

    let ident = gen.sig.ident.clone();
    let gen_out = match gen.sig.output {
        ReturnType::Default => {
            Diagnostic::new(
                Level::Error,
                "Generator's return type is missing.".to_string(),
            )
            .abort();
        }
        ReturnType::Type(_, ty) => ty,
    };

    println!("{}", gen.sig.ident.to_string());
    let gen = input.to_string();

    let camelcased = dpn.to_camelcase();
    let snakecased = dpn.to_snakecase();

    // Register in the map
    MAP.with(|map| match map.borrow_mut().entry(dpn) {
        Entry::Vacant(entry) => {
            entry.insert(Solution {
                generator: Some(gen),
                runner: None,
            });
        }
        Entry::Occupied(_entry) => {
            //
        }
    });

    quote! {
        mod #snakecased {
            use crate::{ Generator, #camelcased };
            use super::#ident;

            impl Generator for #camelcased {
                type GenOutput = #gen_out;
                fn generate(input: &'static str) -> Self::GenOutput {
                    #ident ( input )
                }
            }

        }
    }
    .into()
}

#[proc_macro_attribute]
#[proc_macro_error(assert_unwind_safe)]
pub fn aoc(args: pm::TokenStream, _input: pm::TokenStream) -> pm::TokenStream {
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

    // captures:
    // - Runner method
    //
    // Register in the map

    pm::TokenStream::new()
}

#[proc_macro]
#[proc_macro_error(assert_unwind_safe)]
pub fn aoc_lib(_: pm::TokenStream) -> pm::TokenStream {
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

    // for each entry in the map, generate a (generator, runner)-name implementer
    // generate an array listing all solution implementers.

    let count = MAP.with(|map| map.borrow().len());
    let mut map = MAP.with(|map| map.borrow().clone());
    let (impls, enum_, instances): (Vec<_>, Vec<_>, Vec<_>) = map
        .drain()
        .sorted_by_key(|(dpn, _)| dpn.clone())
        .map(|(k, v)| {
            let types::DayPartName { day, part, name } = k.clone();
            //let name = if let Some(name) = name {
            //    syn::Ident::new("None", pm2::Span::call_site())
            //} else {
            //    syn::parse2(quote! { Some( #name ) }).expect("failed to generate entity")
            //};

            let camelcased: syn::Ident = syn::parse_str(&k.to_camelcase()).unwrap();
            let input = std::path::Path::new("target")
                .join(year.to_string())
                .join(format!("day{}.txt", k.day))
                .to_string_lossy()
                .to_string();
            let Solution { generator, runner } = v;
            let generator = generator
                .unwrap_or_else(|| "fn generator(input: &str) -> &str { input }".to_string());
            println!("{}", generator);
            let generator: ItemFn = syn::parse_str(&generator).expect("failed to parse generator");

            let gen_out = match &generator.sig.output {
                ReturnType::Default => syn::parse_str("()").unwrap(),
                ReturnType::Type(_, ty) => ty.as_ref().clone(),
            };

            let runner = runner.unwrap_or_else(|| {
                quote! { fn runner(input: &#gen_out) -> &'static str { "Not implemented" } }
                    .to_string()
            });
            println!("{}", runner);
            let runner: ItemFn = syn::parse_str(&runner).expect_or_abort("failed to parse runner");

            let run_out = match &runner.sig.output {
                ReturnType::Default => syn::parse_str("()").unwrap(),
                ReturnType::Type(_, ty) => ty.as_ref().clone(),
            };

            (
                quote! {
                    pub struct #camelcased;
                },
                quote! { #camelcased ( #camelcased ) },
                quote! { (Solutions::#camelcased ( #camelcased )) },
            )
        })
        .multiunzip();

    let res = quote! {
        pub trait Generator {
            type GenOutput;
            const INPUT: &'static str;
            fn generator(input: &'static str) -> Self::GenOutput;
        }
        pub trait Solver: Generator {
            type RunOutput: std::fmt::Display;
            fn solver(input: &Self::GenOutput) -> Self::RunOutput;
        }

        #(#impls)*

        pub enum Solutions {
            #(#enum_),*
        }

        //pub const SOLVERS: [ (usize, usize, Option<&'static str>, Solutions) ; #count ] = [
        //    #(#instances),*
        //];
    };

    let _ = std::fs::write("output.rs", res.to_string());

    res.into()
}

#[proc_macro]
#[proc_macro_error(assert_unwind_safe)]
pub fn aoc_main(_: pm::TokenStream) -> pm::TokenStream {
    quote! {
        fn main() {
            // clap
            //   -d day
            //   -p part
            //   -n name
            //
            // match (day, part, )
            unimplemented!()
        }
    }
    .into()
}

/// Sets the year for the solutions
///
/// This must appear before any other use of aoc-helper's macros.
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
    println!("Year: {:?}", YEAR.with(|y| y.get()));
    pm::TokenStream::new()
}
