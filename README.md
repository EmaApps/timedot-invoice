# Ema Template

This repository represents a simple example of [Ema](https://ema.srid.ca/) — it generates a basic site with sidebar from a directory of Markdown files using Pandoc, Blaze HTML & TailwindCSS 3.x — and as such acts as a **template repository** to use for bootstrapping your next static site using Ema.

The generated HTML site can be previewed here: https://srid.github.io/ema-template/

## Getting Started

To develop with full IDE support in Visual Studio Code, follow these steps:

- [Install Nix](https://nixos.org/download.html) & [enable Flakes](https://nixos.wiki/wiki/Flakes)
- Run `nix-shell --run haskell-language-server` to sanity check your environment 
- Open the repository [as single-folder workspace](https://code.visualstudio.com/docs/editor/workspaces#_singlefolder-workspaces) in Visual Studio Code
    - Install the recommended extensions
    - <kbd>Ctrl+Shift+P</kbd> to run command "Nix-Env: Select Environment" and select `shell.nix`. The extension will ask you to reload VSCode at the end.
- Press <kbd>Ctrl+Shift+B</kbd> in VSCode, or run `bin/run` in terminal, to launch the Ema dev server, and navigate to http://localhost:9001/

All but the final step need to be done only once. Check [the Ema tutorial](https://ema.srid.ca/start/tutorial) next.

## Note

- This project uses [relude](https://github.com/kowainik/relude) as its prelude, as well as Tailwind+Blaze as CSS utility and HTML DSL. Even though the author highly recommends them, you are of course free to swap them out for the library of your choice.
- As a first step to using this template, 
  - change the project name in .cabal, flake.nix and hie.yaml files; then commit changes to Git.
      - To automate this, `mv ema-template.cabal myproject.cabal; nix run nixpkgs#sd -- ema-template myproject * */* .github/*/*`
- Configuration:
  - To change the port, see `./.ghcid`
  - To change the CLI arguments used by bin/run, see file .ghcid
  - To update Ema to latest Git revision, run `nix flake lock --update-input ema`
  - To add/remove Haskell dependencies, see the .cabal file. If a dependency is unavailable in nixpkgs, you can override it (to point to say a Git repo) in the `overrides` attribute of flake.nix. You can imitate the manner in which the `ema` (or `lvar`) package itself is overriden.
- To generate static site, run:
  ```sh
  mkdir ../output 
  cd content && nix run .. -- gen ../../output
  ```
  - You might want to change or remove the `<base>` tag in `Main.hs` depending where you will be deploying the site.

## Non-Nix workflow

In order to use this repository without Nix, such as with plain Cabal or Stack, you need to have the following installed manually:

- ghcid (used by `bin/run-haskell` which `./Procfile` invokes)
- [tailwind runner](https://hackage.haskell.org/package/tailwind) along with [tailwind CLI](https://tailwindcss.com/docs/installation)
- [foreman](http://ddollar.github.io/foreman/) (or one of its rewrites)

Once all the above are installed, run `foreman start` to start the Ema live server.
