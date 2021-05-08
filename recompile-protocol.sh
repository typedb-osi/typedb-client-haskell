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

# now compile and patch all proto files
for e in ./lib/fixedProtobuf/*.proto; do
    echo "compiling $e"
    # compile proto file
    compile-proto-file                  \
        --includeDir $(dirname $e)      \
        --proto $(basename $e)          \
        --out ./lib/
    

    newFile=$(ls -t ./lib/*.hs | head -1)
    protoFileName=$(basename $newFile | cut -f 1 -d '.')
    if [ -f ./lib/patches/$protoFileName.patch ]; then
        echo "found $protoFileName.patch"
        patch $newFile < ./lib/patches/$protoFileName.patch
    fi
done

echo "Finished recompilation and patching"
