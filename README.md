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

## FLUKA input

Simulation of muon spallation in xenon doped liquid scintillator. The isotope production and tracks per event is returned with mgdraw.f in which the entries MGDRAW and USDRAW can be choosen. MGDRAW is called at every step in the simulation and thus returns the tracks of all particle production. USDRAW is called after each particle interaction, saving the coordinates of the interaction point and the type of interaction as ICODE. After USDRAW is called, the particle can be followed with MGDRAW. The user routine mgdraw.f is called to with the USERDUMP line in the input file. 

Another way to score the isotope production is with usrrnc.f. The user routine is called for stopping residual nuclei. Thus the coordinates that are returned correspond to the end of the isotope track: the last step of the nuclear reaction. To call usrrnc.f the option USERWEIG and RESNUCLEI should be added to the input file.

To score the neutron capture yield, mdstck.f is used which is called after a nuclear interaction takes place. The user routine is used to save the interaction point coordinates and daugther particles created for every neutron capture. In addition the total photon energy is saved. mdstck.f is called automatically after every interaction, thus it is not necessary to add something to the input file.

## mgdraw.f versions

There are three version of mgdraw.f in this project: event_plotting, isotope_tracking and usdraw. Fistly, event_plotting: the entry MGDRAW is called for every point in the simulation and the coordinates are returned. Thus the SRCEFILE contains tracking information of all particles in the simulation. Secondly, isotope_tracking is similar to event_plotting, with the adjustment that only the tracks of heavy isotopes and neutrons are returned. Lastly, usdraw calls to the entry USDRAW and returns the parent and daughter information of all points where a heavy isotope is produced. Coordinates of the interation point and the type of interaction are scored as well.

### Folder: full event plotting

The tracks of the events are reconstructed with MGDRAW and saved in an unformatted file called SRCEFILE. The python script eventscreator.py is used to loop through SRCEFILE and copy all events where an isotope heavier than helium is created into a new file. In addition the isotopes are counted for every created isotope the A and Z are printed together with the creation count onto the log file. The script qsub_fluka.py can be used to call to FLUKA multiple times and automatically let eventscreator.py go through the results. Lastly the events can be plotted with plot_creator.py and qsub_plot.py.

### Folder: isotope tracking

MGDRAW is used to return a file where only the tracks of neutrons and isotopes heavier than helium are returned (see the if-statements of MGDRAW). Thereafter the script eventscreator.py can be used to count the number of isotopes and neutrons and print them onto the log file. Note that for the neutron count we check if it is not the same neutron as before by checking if the new track coordinates are not the same as before (this might also still need to be done for the isotopes). MDSTCK is used also to count the number of neutrons and returns more than with MGDRAW.

## How to compile

The script ldpmqmd is used instead of lfluka in the second line, because the new version of FLUKA requires to link rQMD-2.4 by hand. If this is not done, the PHYSICS model COALESCE will result in an error. 

**Load mgdraw:**\
$FLUPRO/flutil/fff mgdraw_usdraw.f\
$FLUPRO/flutil/fff mgdraw_isotope_tracking.f\
$FLUPRO/flutil/fff mgdraw_event_plotting.f

**Load mdstck, usrrnc and muon flux file:**\
$FLUPRO/flutil/fff mdstck.f\
$FLUPRO/flutil/fff usrrnc.f\
$FLUPRO/flutil/fff source_muons_kelly.f

**Compile different versions of user routines:**\
Version = usdraw, isotope_tracking or event_plotting: depending on the requested output\
$FLUPRO/flutil/ldpmqmd -o XeLS_version -m fluka mdstck.o usrrnc.o source_muons_kelly.o mgdraw_version.o

NOTE: with the new version of source_muons_kelly, it is important that the BEAMPOS is defined in the input file since the muon start position is derived from there (XBEAM, YBEAM, ZBEAM)

**FLUKA should be called as follow:**\
For version = usdraw, isotope_tracking, event_plotting\
$FLUPRO/flutil/rfluka -e $FLUPRO/flutil/XeLS_version -N0 -M1 muons_XeLS
