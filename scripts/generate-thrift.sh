COMMON_FILES_BASE=libs/luna/Flowbox/Luna/Tools/Serialize/Thrift
BATCH_FILES_BASE=tools/batch/Flowbox/Batch/Server/Thrift

COMMON_FILES=$COMMON_FILES_BASE/*.thrift
BATCH_FILES=$BATCH_FILES_BASE/*.thrift

for f in $COMMON_FILES
do
 thrift -gen hs  -out $COMMON_FILES_BASE/Generated $f
 thrift -gen cpp -out tools/batch-clients/cpp/generated $f
done

for f in $BATCH_FILES
do
 thrift -gen hs  -out $BATCH_FILES_BASE/Generated $f
 thrift -gen cpp -out tools/batch-clients/cpp/generated $f
done