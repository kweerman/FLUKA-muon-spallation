# FLUKA input

Very short summary (detailed explanation will follow in time). MGDRAW (see USERDUMP line in input file) is used to score the isotope creation per event. The tracks of the event can be reconstructed as well. After creating a file with all information, python is used to return the interesting events (isotopes heavier than helium are created). 

## How to compile

The script ldpmqmd is used instead of lfluka in the second line, because the new version of FLUKA requires to link rQMD-2.4 by hand. If this is not done, the PHYSICS model COALESCE will result in an error. 

**Compile MGDRAW:**\
$FLUPRO/flutil/fff mgdraw_vers2.f\
$FLUPRO/flutil/ldpmqmd -o mydraw_vers2 -m fluka mgdraw_vers2.o 

**FLUKA should be called as follow:**\
$FLUPRO/flutil/rfluka -e $FLUPRO/flutil/mydraw_vers2 -N0 -M1 muons_XeLS 
