#!/bin/sh
./sorter D
# we manually delete WORK. because CREATE_FILE on WORK may cause USE_ERRROR on some older implementations.
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
