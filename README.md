# minseo-kim.net

Personal website built with [Hakyll](https://jaspervdj.be/hakyll/), a static site generator. Hosted at [minseo-kim.net](https://minseo-kim.net).

## How to Build

### Prerequisites

Make sure you have Haskell and Cabal installed. It is recommended to use GHCup for the installation.

### Build Steps

To build this site locally, follow these steps:

```sh
$ cabal configure
$ cabal build all
$ cabal exec site build
```

Please note that the compilation may take some time.

To preview the website, start the local server:

```sh
$ cabal exec site watch
```

