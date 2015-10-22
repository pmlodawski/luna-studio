#!/bin/bash

echo "Gzipping build..."

set -e

s3_path=s3://nodelab-gui/build/$DRONE_COMMIT/

tmp=$(mktemp -d)
for i in $(find www -type f \( -name '*.html' -o -name '*.css' -o -name '*.woff' -o -name '*.ttf' -o -name '*.eot' -o -name '*.js' \))
do
	file $i
	mkdir -p $tmp/$(dirname $i)
	gzip -c $i > $tmp/$i
done

echo "Uploading gzipped files to S3..."

aws s3 cp                          \
  --recursive                      \
  --acl=public-read                \
  --cache-control='max-age=604800' \
  --content-encoding=gzip          \
  $tmp/www/ $s3_path               \
  --exclude '*.*'                  \
  --include '*.html'               \
  --include '*.js'                 \
  --include '*.css'                \
  --include '*.woff'               \
  --include '*.ttf'                \
  --include '*.eot'                \
  --region  eu-west-1

echo "Uploading non-gzipped files to S3..."

aws s3 cp                          \
  --recursive                      \
  --acl=public-read                \
  --cache-control='max-age=604800' \
  www/     $s3_path                \
  --exclude '*.html'               \
  --exclude '*.js'                 \
  --exclude '*.css'                \
  --exclude '*.woff'               \
  --exclude '*.ttf'                \
  --exclude '*.eot'                \
  --region  eu-west-1

rm -rf $tmp

echo "Upload complete"

exit 0
