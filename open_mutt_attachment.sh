#!/bin/bash

# Args:
#   $1 filename
#   $2 filetype

DIR="$HOME/.mutt/tmp/attachments"
FILENAME=`basename $1`
FILETYPE=$2

# Make sure our directory exists
mkdir -p $DIR
# Clear out the attachments each time
rm -f $DIR/*

# Try to figure out the filetype
if [ -z $FILETYPE ]; then
    FILETYPE=`file -bi $1 | cut -d"/" -f2`
fi

# Unless we can figure out the type we'll just use the filename
if [ $FILETYPE = "-" ]; then
    DESTINATION="$DIR/$FILENAME"
else
    FILE=`echo $FILENAME | cut -d"." -f1`
    DESTINATION="$DIR/$FILE.$FILETYPE"
fi

# Temporarily save it and then open the file
cp $1 $DESTINATION
open $DESTINATION
