#!/bin/sh
./sorter D
# CREATE_FILE FOR WORK. may raise USE_ERROR on some implementationss
rm WORK.
./makedict G
./sorter S
rm WORK.
./makestem G
./makeinfl
./makeewds G
./sorter E
rm WORK. 
./makeefil

