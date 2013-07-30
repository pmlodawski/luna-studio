COMMON_FILES=tools/lunac/thrift/*.thrift
BATCH_FILES=tools/batch/thrift/*.thrift

for f in $COMMON_FILES
do
 thrift -gen hs  -o tools/lunac/ $f
 thrift -gen cpp -o tools/batch-client/ $f
done

for f in $BATCH_FILES
do
 thrift -gen hs  -o tools/batch/ $f
 thrift -gen cpp -o tools/batch-client/ $f
done