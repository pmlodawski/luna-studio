#!/bin/bash

echo "Gzipping build..."

set -e

s3_path=s3://nodelab-gui/backend/$DRONE_COMMIT.tar.gz


cd build_7.10/.bin
tar -czvf artifacts.tar.gz --exclude "*.tar.gz" .
echo "Uploading gzipped files to S3..."

aws s3 cp                          \
  artifacts.tar.gz $s3_path        \
  --region  eu-west-1

echo "Upload complete"

exit 0
