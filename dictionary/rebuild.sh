#!/bin/sh
./sorter D
rm WORK.   -- CREATE_FILE FOR WORK. may raise USE_ERROR on some implementations
./makedict G
./sorter S
rm WORK.
./makestem G
./makeinfl
./makeewds G
./sorter E
rm WORK. 
./makeefil

