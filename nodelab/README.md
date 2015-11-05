[![Build Status](https://cd.newbyteorder.com/api/badges/NewByteOrder/new_byte_order/status.svg)](https://cd.newbyteorder.com/NewByteOrder/new_byte_order)

# NodeLab GUI

## Runing development server

Run `brunch watch --server` and then navigate to [http://localhost:3333](http://localhost:3333). You may also consider using [Pow](http://pow.cx) for running webserver separately.

### Running incremental linker

To use GHCI mode with incremental linker (aka very fast rebuild) please run `brunch watch --server --env interactive`. This environment has major advantage: [Save] to running new code in the browser time is very short: few seconds vs. more than minute. However, it has some limitations: only one browser may be connected do GHCJSi server, when new browser connects the old one will be kicked from the session.

In interactive mode, the page is reloaded upon file save, however new code is injected a couple of seconds later. In the meantime you'll see console of GHCJSi containing build log redirected to the browser.


## Dev-env setup

```
# install protobuf
stack install alex happy hprotoc
git clone git@bitbucket.org:NewByteOrder/new_byte_order.git
cd new_byte_order/
git submodule update --init --recursive
scripts/genproto
scripts/gencabal
cd nodelab/
stack setup
sudo npm install -g brunch bower
bower install
npm install
```

## Building

Follow instructions:
```
brunch build               # development build
brunch build -e production # production build
```

## Protocol buffers

Before building the app for the first time, you need to generate protocol buffers definitions.
1. Install [Protobuf](https://github.com/google/protobuf) and [hprotoc](https://hackage.haskell.org/package/hprotoc). Make sure the `protoc` and `hprotoc` binaries are available on the `$PATH`.
2. Ensure `git submodule init` and `git submodule update` were performed.
3. Run `./scripts/genproto`.
4. Run `./scripts/gencabal` (the order of `genproto` and `gencabal` matters!).

## Building for production

Run `brunch build --production`. `--production` flag enables JS minification (this will take a while). It also disables overriding feature-switches from `localStorage`.

## Feature switches

Feature switches reside in `app/features.default.js`. These values are for production environment. You can override them for development by creating file `app/features.local.js` (listed in `.gitignore`) or by setting `localStorage.[feature_name] = "true|false"` in web browser. Overrides are ignored in production build.

## Tips

* App outputs to browser console build time and Git hash (with prefix "-local" if it had non-commited changes).
* You may want to change `plugins.ghcjs.buildCommand` to `cabal build` to speedup compilation (at least it speeds on my Mac). `plugins.ghcjs.clearScreen` may ba also useful – it clears terminal when building Haskell sources. This is useful in combination with iTerm2 CMD+Click on file path in error messages.
* JS dependencies are managed by Bower (at least one present in bower repos)
* `three-bmfont-text` is designed to be NPM/browserify package. I've built dist package with `cjsify index.js --export THREE_TEXT --ignore-missing > three-bmfont-text-cjs.js`
* GHCJS should be installed with following instructions: https://gist.github.com/kfigiela/b908c44d6203b72e3739#file-install-ghcjs-sh

## Local `brunch-config` overrides

It's possible to override some brunch settings locally by creating file `brunch-config.local.coffee` that exports function `transform` that modifies config. Example:

```coffee
exports.transform = (config) ->
  config.plugins.ghcjs.buildCommand = "stack build"
  config.plugins.ghcjs.clearScreen  = true
```
