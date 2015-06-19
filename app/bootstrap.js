console.log("Current version: " + process.env.GIT_HASH);
console.log("Build at " + process.env.BUILD_DATE);

require('./app').start();

