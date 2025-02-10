# Set up the environment for FLUKA 2023.3.3
export FLUPRO=/project/xenon/kweerman/Fluka2023_3_3

# clean the path from any previous FLUKA versions
export PATH=$(echo "$PATH" | sed -E 's|:[^:]*Fluka[^:]*||g')

# insert the current FLUKA version
export PATH="$PATH:$FLUPRO:$FLUPRO/flutil"
export FLUFOR=gfortran
