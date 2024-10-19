#!/bin/sh
../obj/sorter D
# we manually delete WORK. because CREATE_FILE on WORK may cause USE_ERRROR on some older implementations.
rm WORK.
../obj/makedict G
../obj/sorter S
rm WORK.
../obj/makestem G
../obj/makeinfl
../obj/makeewds G
../obj/sorter E
rm WORK.
../obj/makeefil
echo "Dictionary build complete."
echo "If successful, the following files constitute a complete set of dictionary data:"
echo "INFLECTS.SEC ADDONS.LAT UNIQUES.LAT DICTFILE.GEN STEMFILE.GEN INDXFILE.GEN EWDSFILE.GEN"
