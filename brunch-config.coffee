shelljs = require 'shelljs'

cabalProjectName = "gui"

exports.config =
  paths:
    public: 'www'
    watched: ['app', 'vendor', "#{cabalProjectName}.cabal"]

  files:
    javascripts:
      joinTo:
        'javascripts/ghcjs.js' : /^app\/.*\.ghcjs$/
        'javascripts/app.js'   : /^app\/(js|features\.|shaders|config)/
        'javascripts/vendor.js': /^(vendor|bower_components)/
      order:
        before: []

    stylesheets:
      joinTo:
        'stylesheets/app.css': /^(app|vendor|bower_components)/
      order:
        before: []
        after : []
    templates:
      joinTo:
        'javascripts/app.js': /^(app|vendor|bower_components)/
      order:
        before: []
        after : []

  conventions:
    assets: /(assets|vendor\/assets)/

  modules:
    nameCleaner: (path) ->
      path.replace /^app\/(js\/)?/, ''

  plugins:
    ghcjs:
      placeholder:  'app/env.ghcjs'
      projectName:  cabalProjectName
      buildCommand: 'cabal install'
      clearScreen:  false

    jshint:
      pattern: /^app\/.*\.js$/
      warnOnly: true

  keyword:
    filePattern: /\.(js|css|html)$/
    map:
      git_commit: ->
        local_changes = (shelljs.exec('git diff-index --quiet HEAD --').code == 1)
        git_hash      = shelljs.exec('git rev-parse HEAD', {silent:true}).output.trim()
        "#{git_hash}#{if local_changes then "-local" else ""}";
      env: "development"

  overrides:
    production:
      keyword: map: env: "production"
