# Yet Another [AOC](https://advent-of-code.com/) Helper

[![crates.io](https://img.shields.io/crates/v/yaah.svg)](https://crates.io/crates/yaah)

## Getting started

- Extract the advent of code's session cookie from your browser.
- Create a new project using the template: `cargo generate --git https://github.com/ithinuel/yaah-template.git`.
- Run `cargo run --release`.
- Enjoy!

## AOC Session

The template stores your session in `.cargo/config.toml`. Alternatively you can define the environment
variable `AOC_SESSION`.

`yaah` will automatically download the input file and store it in `input/{year}/{day}.txt` if it is
not present.
