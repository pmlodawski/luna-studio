# New Byte Order repository

## Requirements

* [Stack](http://haskellstack.org/)
* [NodeJS](http://nodejs.org/)
* [Supervisord](http://supervisord.org/)

## Setup

```shell
$ git clone git@bitbucket.org:NewByteOrder/new_byte_order.git
$ cd new_byte_order
$ REPO_DIR=`pwd`
$ cd $REPO_DIR/build_7.10/backend
$ stack setup # installs ghc
$ cd $REPO_DIR/nodelab
$ stack setup # installs ghcjs
```

## Building

### Code generation & deps installation

```shell
$ cd $REPO_DIR
$ mkdir -p dist/proto
$ scripts/genproto > /dev/null
$ scripts/gencabal
$ cd $REPO_DIR/nodelab
$ npm install
$ bower install --allow-root
```

### After each pull

```shell
$ cd $REPO_DIR
$ scripts/gencabal
```

### Backend & GUI

```shell
$ cd $REPO_DIR/build_7.10/backend
$ stack build --copy-bins
$ cd $REPO_DIR/nodelab
$ brunch build # -e production -- for production build
```

## Running

### Backend

```shell
$ cd $REPO_DIR/supervisor
$ supervisord # will start everyting
$ supervisorctl status # for status
$ supervisorctl restart all # to restart everyting
$ supervisorctl tail -f logger # to tail logger output (see supervisord manual for more)
```


### GUI

```shell
$ cd $REPO_DIR/nodelab
$ brunch watch --server # or serve $REPO_DIR/nodelab/www using any HTTP server
```


## Utility scripts

### `upload_backend.sh`

TODO

### `nodelab/upload_dist.sh`

TODO

### `scripts/*`

TODO
