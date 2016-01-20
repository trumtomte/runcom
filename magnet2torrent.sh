#!/bin/bash

# Usage: ./magnet2torrent "<magnet-uri>"

DIR=./watch

echo "Changing directory to $DIR..."
cd $DIR

[[ "$1" =~ xt=urn:btih:([^&/]+) ]] || exit;

HASH=${BASH_REMATCH[1]}

echo "Creating meta torrent file meta-$HASH.torrent..."
echo "d10:magnet-uri${#1}:${1}e" > "meta-$HASH.torrent"
echo "Done!"
