@echo Assumes GNAT and gprbuild are installed, and that you have write permission for .\obj\ and .\dictionary\
@echo Building programs
@echo ***
gprbuild words_2_WINDOWS.gpr -p
@echo ***
@echo Building dictionary files
@echo ***
cd dictionary
..\obj\sorter D
REM manually delete WORK because CREATE_FILE on WORK may cause USE_ERRROR on some older implementations.
del WORK
..\obj\makedict G
..\obj\sorter S
del WORK
..\obj\makestem G
..\obj\makeinfl
..\obj\makeewds G
..\obj\sorter E
del WORK
..\obj\makeefil
@echo ***
@echo Packaging WORDS in obj\words_packaged
@echo ***
mkdir ..\obj\words_packaged
copy INFLECTS.SEC ..\obj\words_packaged
copy ADDONS.LAT ..\obj\words_packaged
copy UNIQUES.LAT ..\obj\words_packaged
copy DICTFILE.GEN ..\obj\words_packaged
copy STEMFILE.GEN ..\obj\words_packaged
copy INDXFILE.GEN ..\obj\words_packaged
copy EWDSFILE.GEN ..\obj\words_packaged
copy ..\obj\words.exe ..\obj\words_packaged
cd ..
@echo ***
@echo If successful, everything you need to run WORDS is now in obj\words_packaged
@echo namely: words.exe, INFLECTS.SEC, ADDONS.LAT, UNIQUES.LAT, DICTFILE.GEN, STEMFILE.GEN, INDXFILE.GEN, and EWDSFILE.GEN

