# elm-glsl

The package elm-glsl is a Haskell code for the the parsing of GLSL ES code for the Elm compiler.

It is based on awesome [language-glsl](https://github.com/noteed/language-glsl) by Vo Minh Thu, but uses a parser from [elm-compiler](https://github.com/elm-lang/elm-compiler) instead of Parsec.

## Disclaimer

This is an experiment to replace language-glsl in the Elm compiler code, in order to:

* drop the Parsec dependency from the compiler;
* extend the functionality to support custom requirements.

## Setup

```bash
# Create the following structure
elm-lang
elm-lang/elm-compiler -- the clone of elm-compiler
elm-lang/elm-glsl -- the contents of this repo

# Run the following
cd elm-lang
cabal sandbox init
cabal sandbox add-source elm-compiler
cd elm-compiler
git checkout dev
git apply ../elm-glsl/elm-compiler.patch
cd ../elm-glsl
cabal sandbox init --sandbox ../.cabal-sandbox
cabal install --only-dependencies --enable-tests
cabal configure --enable-tests
cabal test
```

## Creating a patch for compiler

```bash
cd elm-compiler
git checkout dev
# Make changes...
git diff > ../elm-glsl/elm-compiler.patch
```
