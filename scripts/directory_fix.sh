#!/bin/bash

# Converts the following characters to swedish ones
# ô == Ö, î == ö
# Ü == å, è == Å
# Ñ == ä, TODO: Ä == ?

# If no argument was provided set directory to "."
SEARCHDIR=`echo ${1:-.}`

# If the directory to be searched also contains incorrect characters
if [[ $SEARCHDIR == *ô* ]] || [[ $SEARCHDIR == *î* ]] || [[ $SEARCHDIR == *Ü* ]] || [[ $SEARCHDIR == *è* ]] || [[ $SEARCHDIR == *Ñ* ]]; then
    # Temporary variable of the current directory
    CUR_SEARCHDIR=$SEARCHDIR
    # Search and replace all characters
    SEARCHDIR=`echo $SEARCHDIR | sed -e ' s/ô/Ö/g' -e 's/î/ö/g' -e 's/Ü/å/g' -e 's/è/Å/g' -e 's/Ñ/ä/g'`
    # Move (rename) the directory
    mv $CUR_SEARCHDIR $SEARCHDIR
fi

# Change to the directory to be searched
cd $SEARCHDIR

# Loop through all directories
for DIR in `find . -type d`
do
    # Check if the special characters exists in the current directory
    if [[ $DIR == *ô* ]] || [[ $DIR == *î* ]] || [[ $DIR == *Ü* ]] || [[ $DIR == *è* ]] || [[ $DIR == *Ñ* ]]; then
        # Create temporary variable for the current directory
        CUR=$DIR
        # Search and replace all characters
        NEW=`echo $DIR | sed -e ' s/ô/Ö/g' -e 's/î/ö/g' -e 's/Ü/å/g' -e 's/è/Å/g' -e 's/Ñ/ä/g'`
        # Move (rename) the directory
        mv $CUR $NEW
    fi
done
