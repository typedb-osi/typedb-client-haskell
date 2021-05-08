# create directory for old protofiles
backupDirName="./lib/fixedProtobuf/backup_$(date +"%d-%m-%Y-%H-%M-%S")"

# move old files to backup dir if they exist
[ -f "./lib/fixedProtobuf/session.proto" ]                  \
    && mkdir $backupDirName                                 \
    && mv ./lib/fixedProtobuf/*.proto "$backupDirName/"

# get proto files
protoFiles=$(find ./lib/protocol/ -name "*.proto" \
           | grep -v "cluster")

# first move and patch all files; imports are needed for compilation
for e in $protoFiles; do
    # move file
    cp $e ./lib/fixedProtobuf/

    # patch import statements
    sed -Ei 's%import "[a-z_]+/([a-z_]+.proto)"%import "\1"%g' \
            "./lib/fixedProtobuf/$(basename $e)"

done

# now compile all proto files
protoFiles=$(find ./lib/fixedProtobuf/ -name "*.proto" \
           | grep -v "cluster")

for e in ./lib/fixedProtobuf/*.proto; do
    echo "compiling $e"
    # compile proto file
    compile-proto-file                  \
        --includeDir $(dirname $e)      \
        --proto $(basename $e)          \
        --out ./lib/
done
