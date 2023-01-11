# FLUKA input

Simulation of muon spallation in xenon doped liquid scintillator. The isotope production and number of neutrons per event is returned with MGDRAW. MDSTCK is also used to count the creation of neutrons and compare to the results of MGDRAW. Note that the USERDUMP line in the input file calls to MGDRAW, while MDSTCK is automatically called after every interaction. There are two maps containing different versions of MGDRAW: one where the tracks of all events are saved and plotted - "full event tracking" - and one where only the information on heavy elements and neutrons is saved - "neutron and isotope count".

## Map: full event tracking

The tracks of the events are reconstructed with MGDRAW and saved in an unformatted file called SRCEFILE. The python script eventscreator.py is used to loop through SRCEFILE and copy all events where an isotope heavier than helium is created into a new file. In addition the isotopes are counted for every created isotope the A and Z are printed together with the creation count onto the log file. The script qsub_fluka_tmp.py can be used to call to FLUKA multiple times and automatically let eventscreator.py go through the results. Lastly the events can be plotted with plot_creator.py and qsub_plot.py.

## Map: Neutron and isotope count

MGDRAW is used to return a file where only the tracks of neutrons and isotopes heavier than helium are returned (see the if-statements of MGDRAW). Thereafter the script eventscreator.py can be used to count the number of isotopes and neutrons and print them onto the log file. Note that for the neutron count we check if it is not the same neutron as before by checking if the new track coordinates are not the same as before (this might also still need to be done for the isotopes). MDSTCK is used also to count the number of neutrons and returns more than with MGDRAW.

## How to compile

The script ldpmqmd is used instead of lfluka in the second line, because the new version of FLUKA requires to link rQMD-2.4 by hand. If this is not done, the PHYSICS model COALESCE will result in an error. 

**Compile MGDRAW:**\
$FLUPRO/flutil/fff mgdraw_vers2.f\
$FLUPRO/flutil/ldpmqmd -o mydraw_vers2 -m fluka mgdraw_vers2.o 

**Compile MGDRAW and MDSTCK:**\
$FLUPRO/flutil/fff mgdraw_vers4_unform.f\
$FLUPRO/flutil/fff mdstck.f\
$FLUPRO/flutil/ldpmqmd -o mydraw4_unform -m fluka mgdraw_vers4_unform.o mdstck.o

**FLUKA should be called as follow:**\
$FLUPRO/flutil/rfluka -e $FLUPRO/flutil/mydraw_vers2 -N0 -M1 muons_XeLS \
$FLUPRO/flutil/rfluka -e $FLUPRO/flutil/mydraw4_unform -N0 -M1 muons_XeLS 
