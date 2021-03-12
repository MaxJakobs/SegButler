#!/bin/sh
if [ ! -d "../testdata" ]; then
  mkdir ../testdata
fi
cd ../testdata                            && \
if [ ! -f "3D_segbutler.tif" ]; then
    curl -L -o 3D_segbutler.tif -k https://www.dropbox.com/s/mipnqgxrelytn2d/3D_segbutler.tif?dl=1
fi