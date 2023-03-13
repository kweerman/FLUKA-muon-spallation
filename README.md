## Directory structure
<pre>
<details><summary>data_analyses          # scripts used to read unformatted isotope and neutron files </summary>
<ul>- mdstck_reader.py: reads the unformatted file created with mdstck.f
- residnuc_reader.py: reads the unformatted file created with usrrnc.f
- usdrawisotopes_reader.py: reads and sums all unformatted isotope files created with eventscreator_usdraw.py
- createhistograms.ipynb: input txt files with isotope counts
- kamlandcompare.ipynb: compare isotope counts with KamLAND FLUKA data
- neutroncompare.ipynb: read formatted neutron count file (txt)
- watercompareisotopes_beacom.ipynb: read formatted neutron count and compare to Beacom article
- isotope_count_txt.ipynb: read formatted isotope count file (txt)
- usdraw_createdaughterfile.ipynb: read unformatted total run file and convert to daughter and parent information txt file</ul></details>
<details><summary>input_files            # fluka input files </summary>
<ul>- muons_XeLS.inp: cylinder of 40m height filled with KamLAND-XeLS
- muons_rock.inp: block of rock, 20 by 20 by 30 cm</ul></details>
<details><summary>python_scripts         # scripts for submitting fluka runs and creating mgdraw output </summary>
<ul>- eventscreator_usdraw.py: loops through mgdraw file and returns isotope spallation products: parents and daughters
- qsub_fluka_usdraw.py: submitting fluka run and creating spallation isotope info with eventscreator</ul><ol><details><summary>full_event_plotting    # tracks of all particle creation captured and plotted </summary>
<ul>- eventscreator_event_plotting.py: loops through mgdraw file and returns all particle tracks of events with spallation isotope production
- qsub_fluka_event_plotting.py: submitting fluka run and creating coordinate lists of events with spallation isotope production
- plot_creator_event_plotting.py: creates plots of all events with heavy isotopes</ul></details>
<details><summary>isotope_tracking       # tracks of all spallation isotopes are captured </summary>
<ul>- eventscreator_isotope_tracking.py: loops through mgdraw file and returns all isotope tracks
- qsub_fluka_isotope_tracking.py: submitting fluka run and creating spallation isotope track lists</ul></details></ol></details>
<details><summary>user_routines          # user routines for neutron and isotope count in fluka </summary>
<ul>- mdstck.f: neutron capture count and energies
- mgdraw_usdraw.f: entry usdraw called everytime an interaction takes place: events with isotope production returned
- source_muons_kelly.f: $10^5$ muon energies distributed according to muon flux at KamLAND
- usrrnc.f: istope production scored at the end of their paths</ul><ol><details><summary>full_event_plotting      # tracks of all particles followed </summary>
<ul>- mgdraw_event_plotting.f: mgdraw called at every step in the simulation and coordinates returned </ul></details>
<details><summary>isotope_tracking         # tracks of all isotopes followed </summary>
<ul>- mgdraw_isotope_tracking.f: mgdraw entry returns all track coordinates of heavy isotopes and neutrons </ul></details></ol></details>
</pre>

## FLUKA input

Simulation of muon spallation in xenon doped liquid scintillator. The isotope production and tracks per event is returned with mgdraw.f in which the entries MGDRAW and USDRAW can be choosen. MGDRAW is called at every step in the simulation and thus returns the tracks of all particle production. USDRAW is called after each particle interaction, saving the coordinates of the interaction point and the type of interaction as ICODE. After USDRAW is called, the particle can be followed with MGDRAW. The user routine mgdraw.f is called to with the USERDUMP line in the input file. 

Another way to score the isotope production is with usrrnc.f. The user routine is called for stopping residual nuclei. Thus the coordinates that are returned correspond to the end of the isotope track: the last step of the nuclear reaction. To call usrrnc.f the option USERWEIG and RESNUCLEI should be added to the input file.

To score the neutron capture yield, mdstck.f is used which is called after a nuclear interaction takes place. The user routine is used to save the interaction point coordinates and daugther particles created for every neutron capture. In addition the total photon energy is saved. mdstck.f is called automatically after every interaction, thus it is not necessary to add something to the input file. USDRAW can also be used to score the neutron capture yield, as is explained later in this file.

Note source_muons_kelly.f is used to generate muon energies according to the spectrum at KamLAND. The muons are dumped at position (xbeam, ybeam, zbeam) which is defined by BEAMPOS in the input file. The muons are ejected in a straight line and move in the positive z-direction.

### USDRAW routine in mgdraw.f

To get the correct isotope yield with mgdraw.f - in agreement with the results of usrrnc.f - there are a couple of different parameters that have to be included to visualise the complete secondary production. Bear with me, this might seem unneccesary complicated, but when you include all paremeters your will get the desired results!

The secondary production is visualised with three different arrays: KPART, KHEAVY and IBRES/ICRES. Note that heavy ions can be saved in COMMON GENSTK, RESNUC or FHEAVY. Subsequently it is of importance that these cards are included and used.
* **The light secondaries**, which correspond to the particle codes in the fluka manual except for -7 to -12, are saved in COMMON GENSTK to the array KPART (printed with KPART[kp = 1 to NP]).

* **The heavy ions** are saved in four different ways: 
<ol>1) KPART(kp) = -2 which is indicated as heavy ion in the fluka manual. The Z and A of the nucleus can be printed respectively with ICHARGE(KPART(kp)) and IBARCH(KPART(kp)) from COMMON PAPROP.</ol>
<ol>2) KPART(kp) < -12 in which case the ion information can be found in COMMON GENSTK. For the kpth secondary the code is given by KPART(kp) = -(kh + 100A + 100000Z + 1000000 m) with kh the kheavy(ip) index and m=99 indicating an excited state. To unpack the large KPART(kp) value: CALL USRDCI(KPART(I), IONA, IONZ, IONM) and subsequently IONA and IONZ return the isotope A and Z value.</ol>
<ol>3) The heavy ions that correspond to JTRACK = -7 to -12 are saved to COMMON FHEAVY and can be visualised with the array KHEAVY (printed with KHEAVY[ip = 1 to NPHEAV]). Note that in this case KHEAVY(ip) = 7 to 12, thus the absolute value of JTRACK, and these particle codes don't show up in the KPART array. The Z and A of the nucleus can be printed respectively with ICHEAV(KHEAVY(ip)) and IBHEAV(KHEAVY(ip)). </ol>
<ol>4) The residual nucleus information is saved in COMMON RESNUC and can be visualised with IBRES and ICRES (respectively A and Z of the nucleus). Note that the common has to be included by the user in mgdraw.f.</ol>

However, there are conditions that have to be added to the above code to avoid incorrectly counting isotopes double. In the case of KART(kp) < -12, if the isotope is excited it will have a kh value of 7. The isotope is counted double (sometimes triple if m=99 since it will go to m=0 first) since it is printed as a large KPART(kp) number first and after de-excitation it is saved to COMMON FHEAVY, subsequently to be printed with KHEAVY. The overcounting can be avoided by excluding isotopes created with ICODE = 106 "de-excitation of secondaries in flight".  In addition, the residual nucleus information saved in RESNUC should only be returned when ICODE = 101 "inelastic scattering" or ICODE = 300 "low-energy neutron interactions" so that solely new isotope creation is taken into acount, and not for example elastic scattering interactinos.

Lastly, the neutron capture on hydrogen and carbon is scored with USDRAW. The hydrogen capture yield is obtained by counting all secondary gammas with 2.22MeV energy and a neutron as parent. Note you could also look at ICODE = 300 instead of JTRACK = 8 since this former indicates an interaction with low-energy neutrons. Carbon capture is scored by counting interaction where 13-C is created through a neutron interaction. The yield will be the same as with mdstck.f. Note that in mdstck (n,2n) and (n,p) interactions are counted as well.

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
