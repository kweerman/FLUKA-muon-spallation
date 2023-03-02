## Directory structure
<pre>
<details><summary>data_analyses          # scripts used to read unformatted isotope and neutron files </summary>
<ul>- mdstck_reader.py: reads the unformatted file created with mdstck.f
- residnuc_reader.py: reads the unformatted file created with usrrnc.f
- usdrawisotopes_reader: reads and sums all unformatted isotope files created with eventscreator_usdraw.py</ul></details>
<details><summary>input_files            # fluka input files </summary>
<ul>- muons_XeLS.inp: cylinder of 40m height filled with KamLAND-XeLS
- muons_rock.inp: block of rock, 20 by 20 by 30 cm</ul></details>
<details><summary>python_scripts         # scripts for submitting fluka runs and creating mgdraw output </summary>
<ul>- eventscreator_usdraw.py: loops through mgdraw file and returns isotope spallation products: parents and daughters
- qsub_fluka_usdraw.py: submitting fluka run and creating spallation isotope info with eventscreator</ul>
<ol><details><summary>full_event_plotting    # tracks of all particle creation captured and plotted </summary>
<ul>- eventscreator_event_plotting.py: loops through mgdraw file and returns all particle tracks of events with spallation isotope production
- qsub_fluka_event_plotting.py: submitting fluka run and creating coordinate lists of events with spallation isotope production
- plot_creator_event_plotting.py: creates plots of all events with heavy isotopes</ul></details>
<details><summary>isotope_tracking       # tracks of all spallation isotopes are captured </summary>
<ul>- eventscreator_isotope_tracking.py: loops through mgdraw file and returns all isotope tracks
- qsub_fluka_isotope_tracking.py: submitting fluka run and creating spallation isotope track lists</ul></details>
</ol></details>
<details><summary>user_routines          # user routines for neutron and isotope count in fluka </summary>
<ul>- mdstck.f: neutron capture count and energies
- mgdraw_usdraw.f: entry usdraw called everytime an interaction takes place: events with isotope production returned
- source_muons_kelly.f: $10^5$ muon energies distributed according to muon flux at KamLAND
- usrrnc.f: istope production scored at the end of their paths</ul>
<ol><details><summary>full_event_plotting      # tracks of all particles followed </summary>
<ul>- mgdraw_event_plotting.f: mgdraw called at every step in the simulation and coordinates returned </ul></details>
<details><summary>isotope_tracking         # tracks of all isotopes followed </summary>
<ul>- mgdraw_isotope_tracking.f: mgdraw entry returns all track coordinates of heavy isotopes and neutrons </ul></details></ol></details>
</pre>


<pre>
|- Cylinder                          # fluka source for large LS cylinder 
|- KamLAND                           # fluka source for KamLAND geometry
|- macros                            # macros to calculate production yield
|- script                            # environmental setup and util
</pre>
# FLUKA input

Simulation of muon spallation in xenon doped liquid scintillator. The isotope production and number of neutrons per event is returned with MGDRAW. MDSTCK is also used to count the creation of neutrons and compare to the results of MGDRAW. Note that the USERDUMP line in the input file calls to MGDRAW, while MDSTCK is automatically called after every interaction. There are two folders containing different versions of MGDRAW: one where the tracks of all events are saved and plotted - "full event tracking" - and one where only the information on heavy elements and neutrons is saved - "neutron and isotope count".

### Folder: full event tracking

The tracks of the events are reconstructed with MGDRAW and saved in an unformatted file called SRCEFILE. The python script eventscreator.py is used to loop through SRCEFILE and copy all events where an isotope heavier than helium is created into a new file. In addition the isotopes are counted for every created isotope the A and Z are printed together with the creation count onto the log file. The script qsub_fluka_tmp.py can be used to call to FLUKA multiple times and automatically let eventscreator.py go through the results. Lastly the events can be plotted with plot_creator.py and qsub_plot.py.

### Folder: neutron and isotope count

MGDRAW is used to return a file where only the tracks of neutrons and isotopes heavier than helium are returned (see the if-statements of MGDRAW). Thereafter the script eventscreator.py can be used to count the number of isotopes and neutrons and print them onto the log file. Note that for the neutron count we check if it is not the same neutron as before by checking if the new track coordinates are not the same as before (this might also still need to be done for the isotopes). MDSTCK is used also to count the number of neutrons and returns more than with MGDRAW.

## How to compile

The script ldpmqmd is used instead of lfluka in the second line, because the new version of FLUKA requires to link rQMD-2.4 by hand. If this is not done, the PHYSICS model COALESCE will result in an error. 

**Compile MGDRAW:**\
$FLUPRO/flutil/fff mgdraw_vers2.f\
$FLUPRO/flutil/ldpmqmd -o mydraw_vers2 -m fluka mgdraw_vers2.o 

**Compile MGDRAW and MDSTCK:**\
$FLUPRO/flutil/fff mgdraw_vers4_unform.f\
$FLUPRO/flutil/fff mdstck.f\
$FLUPRO/flutil/fff source_muons.f\
$FLUPRO/flutil/ldpmqmd -o mydraw4_unform -m fluka mgdraw_vers4_unform.o mdstck.o source_muons.o\

NOTE: with the new version of source_muons_test, it is important that the BEAMPOS is defined in the input file since the muon start position is derived from there (XBEAM, YBEAM, ZBEAM)

**FLUKA should be called as follow:**\
$FLUPRO/flutil/rfluka -e $FLUPRO/flutil/mydraw_vers2 -N0 -M1 muons_XeLS \
$FLUPRO/flutil/rfluka -e $FLUPRO/flutil/mydraw4_unform -N0 -M1 muons_XeLS 
