# Set up the environment for FLUKA 2024.1.3
export FLUPRO=/project/xenon/kweerman/Fluka2024_1_3

# clean the path from any previous FLUKA versions
export PATH=$(echo "$PATH" | sed -E 's|:[^:]*Fluka[^:]*||g')

# insert the current FLUKA version
export PATH="$PATH:$FLUPRO:$FLUPRO/flutil"
export FLUFOR=gfortran
