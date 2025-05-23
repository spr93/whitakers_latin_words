#!/bin/sh
echo "Assumes GNAT and gprbuild are installed, and that you have write permission for ./obj/ and ./dictionary/"
echo "Building programs"
echo "***"
gprbuild words_2.gpr -p
echo "***"
echo "Building dictionary files"
echo "***"
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
echo "***"
echo "Packaging WORDS in obj/words_packaged"
echo "***"
mkdir ../obj/words_packaged
cp INFLECTS.SEC ../obj/words_packaged
cp ADDONS.LAT ../obj/words_packaged
cp UNIQUES.LAT ../obj/words_packaged
cp DICTFILE.GEN ../obj/words_packaged
cp STEMFILE.GEN ../obj/words_packaged
cp INDXFILE.GEN ../obj/words_packaged
cp EWDSFILE.GEN ../obj/words_packaged
cp ../obj/words ../obj/words_packaged
echo "Portions copyright (c) 1993-2024 William Armstrong Whitaker and subsequent contributors." > ../obj/words_packaged/license.txt
echo "" >> ../obj/words_packaged/license.txt
cat ../license.txt >> ../obj/words_packaged/license.txt
cd ..
echo "***"
echo "If successful, everything you need to run WORDS is now in obj/words_packaged"
echo "namely: words [executable], INFLECTS.SEC, ADDONS.LAT, UNIQUES.LAT, DICTFILE.GEN, STEMFILE.GEN, INDXFILE.GEN, EWDSFILE.GEN, and license.txt"
