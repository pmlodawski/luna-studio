# Flowbox GUI

## Dev-env setup

```
# install GHCJS
sudo npm install -g brunch bower
bower install
npm install
```

## Runing development server

Run `brunch watch --server` and then navigate to [http://localhost:3333](http://localhost:3333). You may also consider using [Pow](http://pow.cx) for running webserver separately.

### Running incremental linker

To use GHCI mode with incremental linker (aka very fast rebuild) please run `brunch watch --server --env interactive`. This environment has major advantage: [Save] to running new code in the browser time is very short: few seconds vs. more than minute. However, it has some limitations: only one browser may be connected do GHCJSi server, when new browser connects the old one will be kicked from the session.

In interactive mode, the page is reloaded upon file save, however new code is injected a couple of seconds later. In the meantime you'll see console of GHCJSi containing build log redirected to the browser.

## Building for production

Run `brunch build --production`. `--production` flag enables JS minification (this will take a while). It also disables overriding feature-switches from `localStorage`.

## Feature switches

Feature switches reside in `app/features.default.js`. These values are for production environment. You can override them for development by creating file `app/features.local.js` (listed in `.gitignore`) or by setting `localStorage.[feature_name] = "true|false"` in web browser. Overrides are ignored in production build.

## Tips

* App outputs to browser console build time and Git hash (with prefix "-local" if it had non-commited changes).
* You may want to change `plugins.ghcjs.buildCommand` to `cabal build` to speedup compilation (at least it speeds on my Mac). `plugins.ghcjs.clearScreen` may ba also useful – it clears terminal when building Haskell sources. This is useful in combination with iTerm2 CMD+Click on file path in error messages.
* JS dependencies are managed by Bower (at least one present in bower repos)
* `three-bmfont-text` is designed to be NPM/browserify package. I've built dist package with `cjsify index.js --export THREE_TEXT --ignore-missing > three-bmfont-text-cjs.js`

## Local `brunch-config` overrides

It's possible to override some brunch settings locally by creating file `brunch-config.local.coffee` that exports function `transform` that modifies config. Example:

```coffee
exports.transform = (config) ->
  config.plugins.ghcjs.buildCommand = "cabal build"
  config.plugins.ghcjs.clearScreen  = true
```