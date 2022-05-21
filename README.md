# timedot-invoice

WIP: `nix run`-generate your invoice from a [hledger timedot](https://hledger.org/time-planning.html) file.

- Write your own HTML ([heist](https://srid.ca/heist-start) template) and CSS, with a default one generated automatically.
- Make full use of [Ema](https://ema.srid.ca/)'s hot reload, for a live view of invoice.
- Simple. Does one thing, and one thing only. No kitchen sink. "Print to PDF" if you want PDF.

## Contributing

Contributions are welcome, in particular on any of the following aspects:

- Design improvements (HTML, CSS)
- More general parameters (invoice duration, granularity, etc.) without compromising the ability to specialize

Run `bin/run` and hack away in VSCode under Nix shell.
