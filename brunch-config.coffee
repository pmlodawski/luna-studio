exports.config =
  # See https://github.com/brunch/brunch/blob/master/docs/config.md for documentation.
  paths:
    public: 'www'  
  files:
    javascripts:
      joinTo:
        # 'javascripts/shaders.js': /^app/
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
      placeholder: 'app/placeholder.ghcjs'
      projectName: 'gui'
    browserify:
      # A string of extensions that will be used in Brunch and for browserify.
      # Default: js json coffee ts jsx hbs jade.
      extensions: """
      js coffee
      """
      
      transforms: [require('browserify-shader')]

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
            extensions: { exposify: {jquery: "$", three: "THREE"}}
          }

          # Any options to pass to `browserify.bundle`.
          # `debug` will be set to `!production` if not already defined.
          bundleOptions: {}
          