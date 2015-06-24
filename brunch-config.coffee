shelljs = require 'shelljs'

cabalProjectName = "gui"

exports.config =
  # See https://github.com/brunch/brunch/blob/master/docs/config.md for documentation.
  paths:
    public: 'www'  
    watched: ['app', 'test', 'vendor', "#{cabalProjectName}.cabal"]
  files:
    javascripts:
      joinTo:
        # 'javascripts/null.js': /^app|^vendor\/libs\.js/
        'javascripts/ghcjs.js': /^app\/.*\.ghcjs$/
        # 'javascripts/vendor.js': /^(vendors|bower_components)/
      order:
        before: []

    stylesheets:
      joinTo:
        'stylesheets/app.css': /^(app|vendor)/
      order:
        before: []
        after: []

  conventions:
    assets: /(assets|vendor\/assets)/
  modules:
    wrapper: false
    definition: false    
  plugins:
    ghcjs:
      placeholder: 'app/env.ghcjs'
      projectName: cabalProjectName
    browserify:
      # A string of extensions that will be used in Brunch and for browserify.
      # Default: js json coffee ts jsx hbs jade.
      extensions: """
      js vert frag
      """
      
      transforms: [require('browserify-shader'), require('envify')]

      bundles:
        'javascripts/libs.js':
          # Passed to browserify.
          entry: 'vendor/libs.js'

          # Anymatch, as used in Brunch.
          matcher: /^vendor/

          onBrowserifyLoad: (bundler) ->
            bundler.require('three', {expose: 'three'})
            bundler.require('jquery', {expose: 'jquery'})
            bundler.require('three-bmfont-text', {expose: 'three-bmfont-text'})
            bundler.require('virtual-dom', {expose: 'virtual-dom'})
            bundler.require('underscore', {expose: 'underscore'})
            console.log("Browserify compiling libs...")

          onAfterBundle: (error, bundleContents) -> console.log 'Browserify is done with libs'

        'javascripts/app.js':
          # Passed to browserify.
          entry: 'app/bootstrap.js'

          # Anymatch, as used in Brunch.
          matcher: /^app/
          
          onBrowserifyLoad: (bundler) ->
            bundler.external('three')
            bundler.external('three-bmfont-text')
            bundler.external('jquery')
            bundler.external('virtual-dom')
            bundler.external('underscore')
            bundler.require('./app/common', {expose: 'common'})
            bundler.require('./app/app', {expose: 'app'})

          
          onBeforeBundle: (bundler) -> 
            local_changes = (shelljs.exec('git diff-index --quiet HEAD --').code == 1)
            git_hash = shelljs.exec('git rev-parse HEAD', {silent:true}).output.trim()
            process.env.GIT_HASH = "#{git_hash}#{if local_changes then "-local" else ""}";
            process.env.BUILD_DATE = new Date
            console.log("Browserify compiling app...")

          onAfterBundle: (error, bundleContents) -> console.log 'Browserify is done with app'
