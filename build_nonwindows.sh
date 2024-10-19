#!/bin/sh
echo "Assumes GNAT and gprbuild are installed and that you have write permission for ./obj/ and ./dictionary/"
gprbuild words_2.gpr -p
gprbuild dictionary.gpr makedict makeefil makeewds makeinfl -p
makestem sorter
cd dictionary
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
echo "Packaging WORDS in obj/words_packaged"
mkdir ../obj/words_packaged
cp INFLECTS.SEC ../obj/words_packaged
cp ADDONS.LAT ../obj/words_packaged
cp UNIQUES.LAT ../obj/words_packaged
cp DICTFILE.GEN ../obj/words_packaged
cp STEMFILE.GEN ../obj/words_packaged
cp INDXFILE.GEN ../obj/words_packaged
cp EWDSFILE.GEN ../obj/words_packaged
cp ../obj/words ../obj/words_packaged
cd ..
echo "If successful, everything you need to run WORDS is now in obj/words_packaged"
echo "namely: words [executable], INFLECTS.SEC, ADDONS.LAT, UNIQUES.LAT, DICTFILE.GEN, STEMFILE.GEN, INDXFILE.GEN, and EWDSFILE.GEN"
