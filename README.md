# minseo-kim.net

Personal website built with [Hakyll](https://jaspervdj.be/hakyll/), a static site generator written in Haskell. Hosted at [minseo-kim.net](https://minseo-kim.net).

## How to Build

### Prerequisites

- `cabal`: It is recommended to use GHCup for installation.
- `imagemagick`: Required to convert LaTeX to PNG
- `ghostscript`: Required to convert LaTeX to PNG
- `texlive`: Required to convert LaTeX to PNG

### Build Steps

To build this site locally, follow these steps:

```sh
$ cabal configure
$ cabal build all
$ cabal exec site build
```

Please note that the compilation process may take some time.

To preview the website locally, start the server:

```sh
$ cabal exec site watch
```

## Note

Please note that the markdown syntax used for this site may differ from GitHub-flavored markdown. As a result, certain elements like images or LaTeX syntax might not render correctly when viewed on other platforms, such as GitHub markdown preview, rather than pandoc.

