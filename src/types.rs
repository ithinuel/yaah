use proc_macro2::Span;
use syn::parse::Parse;
use syn::punctuated::Punctuated;
use syn::{parse, ItemFn};

#[derive(Debug, Clone)]
pub struct Solution {
    pub generator: Option<String>,
    pub runner: Option<String>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub struct DayPartName {
    pub day: usize,
    pub part: Option<usize>,
    pub name: Option<String>,
}

impl DayPartName {
    pub fn to_camelcase(&self) -> String {
        match (self.part, &self.name) {
            (Some(part), Some(name)) => format!("Day{}Part{}{}", self.day, part, name),
            (Some(part), None) => format!("Day{}Part{}", self.day, part),
            (None, None) => format!("Day{}", self.day),
            (None, Some(_)) => unreachable!(),
        }
    }
    pub fn to_snakecase(&self) -> String {
        match (self.part, &self.name) {
            (Some(part), Some(name)) => format!("day{}_part{}_{}", self.day, part, name),
            (Some(part), None) => format!("day{}_part{}", self.day, part),
            (None, None) => format!("day{}", self.day),
            (None, Some(_)) => unreachable!(),
        }
    }
}

impl Parse for DayPartName {
    fn parse(input: parse::ParseStream) -> syn::Result<Self> {
        let mut args = Punctuated::<syn::Ident, syn::Token![,]>::parse_terminated(input)?
            .into_iter()
            .map(|ident| (ident.span(), ident.to_string()));

        let (span, day) = match args.next() {
            Some(day) => day,
            None => return Err(syn::Error::new(Span::call_site(), "Day number missing.")),
        };

        let day: usize = match day
            .starts_with("day")
            .then(|| day.get(3..))
            .flatten()
            .and_then(|d| d.parse().ok())
        {
            Some(day @ 1..=25) => day,
            _ => {
                return Err(syn::Error::new(
                    span,
                    "Day should be specified as `dayN` with N in [1, 25].",
                ))
            }
        };

        let part = args.next();
        let name = args.next();
        let (part, name) = match (&part, &name) {
            (Some(_), Some(_)) => (part, name),
            (Some((_, p)), None) if p.starts_with("part") => (part, None),
            (_, _) => (None, part),
        };

        let part = if let Some((span, part)) = part {
            match part.get(4..).and_then(|p| p.parse().ok()) {
                Some(n @ 1..=2) if part.starts_with("part") => Some(n),
                _ => {
                    return Err(syn::Error::new(
                        span,
                        "Part should be specified as `partN` with N in [1, 2].",
                    ))
                }
            }
        } else {
            None
        };

        let name = name.map(|(_, name)| name);

        Ok(Self { day, part, name })
    }
}
#[cfg(test)]
mod test {
    use super::DayPartName;

    #[test]
    fn parse_day_part_name() {
        let expected = DayPartName {
            day: 25,
            part: Some(1),
            name: Some("a_name".to_string()),
        };
        let res: syn::Result<DayPartName> = syn::parse_str("day25, part1, a_name");
        assert_eq!(res.ok(), Some(expected));
    }

    #[test]
    fn part_day_part_number_to_camelcase() {
        assert_eq!(
            DayPartName {
                day: 25,
                part: Some(1),
                name: Some("a_Name".to_string())
            }
            .to_camelcase(),
            "Day25Part1a_Name"
        );
    }
}
