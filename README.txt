To build WASM version:

Step inside this nix shell:

nix shell 'gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org'

build:

wasm32-wasi-cabal build --allow-newer
