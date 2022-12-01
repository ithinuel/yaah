use clap::Parser;

/// Runner for all the solution. By default only the last solver is run.
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Show all days
    #[arg(short, long)]
    all: bool,

    /// Filter in only this day
    #[arg(short, long)]
    day: Option<usize>,

    /// Filter in only these parts
    #[arg(short, long)]
    part: Option<usize>,

    /// Filter in only these names
    #[arg(short, long)]
    name: Option<String>,
}

fn filter(dpn: &#lib::DayPartName, args: &Args) -> bool {
    if let Some(day) = args.day {
        if dpn.day != day {
            return false;
        }
    }
    if args.part.is_some() {
        if dpn.part != args.part {
            return false;
        };
    }
    if let Some(name) = &args.name {
        if dpn.name != Some(name.as_str()) {
            return false;
        };
    }
    return true;
}

fn main() {
    let args = Args::parse();

    let runner = |(dpn, solver): &(
        #lib::DayPartName,
        fn() -> Result<(String, std::time::Duration, std::time::Duration), String>,
    )| {
        match (dpn.part, dpn.name) {
            (Some(part), Some(name)) => print!("Day {} - Part {} - {}: ", dpn.day, part, name),
            (Some(part), None) => print!("Day {} - Part {}: ", dpn.day, part),
            (None, Some(name)) => print!("Day {} - {}: ", dpn.day, name),
            (None, None) => print!("Day {}: ", dpn.day),
        }
        match solver() {
            Ok((res, generator, solver)) => {
                println!("{}", res);
                println!("\tgenerator: {:?}", generator);
                if res != "Solver not implemented" {
                    println!("\tsolver: {:?}", solver);
                }
            }
            Err(msg) => {
                println!("FAILED with {}", msg);
            }
        }
        println!()
    };

    println!();
    println!("\tAdvent Of Code {}", #lib::YEAR);
    println!();

    if !(args.all || args.day.is_some() || args.part.is_some() | args.name.is_some()) {
        #lib::SOLVERS.last().map(runner);
    } else {
        #lib::SOLVERS
            .iter()
            .filter(|(dpn, _)| if args.all { true } else { filter(dpn, &args) })
            .for_each(runner);
    }
}
