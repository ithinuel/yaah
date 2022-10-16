use std::io::Write;

pub fn fetch_input(year: usize, day: usize) -> Result<String, anyhow::Error> {
    let base = std::env::var("CARGO_MANIFEST_DIR")?;
    let input_file_path = std::path::Path::new(&base)
        .join("input")
        .join(format!("{year}"));

    let input_file = input_file_path.join(format!("day{day}.txt"));

    if !input_file.exists() {
        use reqwest::{cookie::Jar, Url};

        let session = std::env::var("AOC_SESSION")?;
        let cookie = format!("session={session}; Domain=adventofcode.com");
        let url: Url = "https://adventofcode.com".parse()?;

        let jar = std::sync::Arc::new(Jar::default());
        jar.add_cookie_str(&cookie, &url);

        // TODO: if file does not exist, try to fetch it.
        let client = reqwest::blocking::Client::builder()
            .cookie_provider(jar)
            .build()?;

        let url: Url = format!("https://adventofcode.com/{}/day/{}/input", year, day).parse()?;

        let input = client.get(url).send()?.error_for_status()?.text()?;

        if input
            .starts_with("Puzzle inputs differ by user.  Please log in to get your puzzle input.")
        {
            anyhow::bail!("Failed to fetch input file, check your session secret");
        }

        std::fs::create_dir_all(input_file_path)?;
        std::fs::File::create(input_file.clone())?.write_all(input.as_bytes())?;
    }
    Ok(input_file.to_string_lossy().to_string())
}
