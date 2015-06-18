shelljs = require 'shelljs'

exports.config =
  # See https://github.com/brunch/brunch/blob/master/docs/config.md for documentation.
  paths:
    public: 'www'  
  files:
    javascripts:
      joinTo:
        'javascripts/ghcjs.js': /^app\/.*\.ghcjs$/
        'javascripts/vendor.js': /^(vendor|bower_components)/
        'test/javascripts/test.js': /^test(\/|\\)(?!vendor)/
        'test/javascripts/test-vendor.js': /^test(\/|\\)(?=vendor)/
      order:
        before: []

    stylesheets:
      joinTo:
        'stylesheets/app.css': /^(app|vendor)/
        'test/stylesheets/test.css': /^test/
      order:
        before: []
        after: []

    templates:
      joinTo: 'javascripts/app.js'

  conventions:
    assets: /(assets|vendor\/assets|font)/
  modules:
    wrapper: false
    definition: false    
  plugins:
    ghcjs:
      placeholder: 'app/env.ghcjs'
      projectName: 'gui'
    browserify:
      # A string of extensions that will be used in Brunch and for browserify.
      # Default: js json coffee ts jsx hbs jade.
      extensions: """
      js coffee
      """
      
      transforms: [require('browserify-shader'), require('envify')]

      bundles:
        'javascripts/app.js':
          # Passed to browserify.
          entry: 'app/bootstrap.js'

          # Anymatch, as used in Brunch.
          matcher: /^app/

          # Any options to pass to `browserify`. 
          # `extensions` will be set to a proper list of
          # `plugins.browserify.extensions`
          instanceOptions: {
          }

          # Any options to pass to `browserify.bundle`.
          # `debug` will be set to `!production` if not already defined.
          bundleOptions: {}
          
          onBrowserifyLoad: (bundler) ->
            bundler.require('three', {expose: 'three'})
            bundler.require('jquery', {expose: 'jquery'})
            bundler.require('./app/app', {expose: 'app'})
            bundler.require('virtual-dom', {expose: 'virtual-dom'})
          
          onBeforeBundle: (bundler) -> 
            local_changes = (shelljs.exec('git diff-index --quiet HEAD --').code == 1)
            git_hash = shelljs.exec('git rev-parse HEAD', {silent:true}).output.trim()
            process.env.GIT_HASH = "#{git_hash}#{if local_changes then "-local" else ""}";

          onAfterBundle: (error, bundleContents) -> console.log 'Browserify is done'
