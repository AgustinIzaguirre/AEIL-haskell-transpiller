if [ $# -eq 0 ]
then
    echo -e "Error: No files specified should provide one file to compile.\n\nUse as follows:\n\t ./compiler.sh AEILSourceFilePath\n"
else
    make compile FILE=$1
fi