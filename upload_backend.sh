#!/bin/bash

echo "Gzipping build..."

set -e

s3_path=s3://nodelab-gui/backend/$DRONE_COMMIT.tar.gz


cd dist/bin
tar -czvf /tmp/artifacts.tar.gz --exclude "*.tar.gz" .
echo "Uploading gzipped files to S3..."

aws s3 cp                          \
  /tmp/artifacts.tar.gz $s3_path   \
  --region  eu-west-1

echo "Upload complete"

exit 0
