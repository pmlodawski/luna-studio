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

## Building for production

Run `brunch build --production`. `--production` flag enables JS minification (this will take a while). It also disables overriding feature-switches from `localStorage`.

## Feature switches

Feature switches reside in `app/features.default.js`. These values are for production environment. You can override them for development by creating file `app/features.local.js` (listed in `.gitignore`) or by setting `localStorage.[feature_name] = "true|false"` in web browser. Overrides are ignored in production build.

## Tips

* App outputs to browser console build time and Git hash (with prefix "-local" if it had non-commited changes).
* You may want to change `plugins.ghcjs.buildCommand` to `cabal build` to speedup compilation (at least it speeds on my Mac). `plugins.ghcjs.clearScreen` may ba also useful – it clears terminal when building Haskell sources. This is useful in combination with iTerm2 CMD+Click on file path in error messages.
* JS dependencies are managed by Bower (at least one present in bower repos)
* `three-bmfont-text` is designed to be NPM/browserify package. I've built dist package with `cjsify index.js --export THREE_TEXT --ignore-missing > three-bmfont-text-cjs.js`
