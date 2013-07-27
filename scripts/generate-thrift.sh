FILES=tools/batch/thrift/*.thrift
for f in $FILES
do
 thrift -gen hs  -o tools/batch/ $f
 thrift -gen cpp -o tools/batch-client/ $f
done