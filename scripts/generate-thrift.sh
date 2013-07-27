FILES=tools/batch/thrift/*.thrift
for f in $FILES
do
 thrift -gen cpp -gen hs -o tools/batch/ $f
 # do something on $f
done