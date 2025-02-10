# version 6 2023 Kelly Weerman
# submit fluka input file to stoomboot for a certain amount of jobs and returns unformatted files
# SRCEFILE (isotope production and daughter information from usdraw), 
# NEUTRO (neutron capture count from mdstck.f),
# RESIDNUC (isotope production from usrrnc.f).
# in addition eventscreator.py is called to add usdraw isotopes into "isotopes" file
# correct ratio of muon+ and muon-

from subprocess import call 
import os
import math

script_template = """#!/bin/bash
echo "------------------------------------------------------------------------"
echo "Job started on" `date`
echo "Copy made of {inp_file} from {path}"
echo "Data saved to {copy_file}" 
echo "Output saved to {out_folder}"
echo "------------------------------------------------------------------------"
. /project/xenon/kweerman/exercises/FLUKA-muon-spallation/fluka2024.1.3.bash
cd $TMPDIR
{script1}
echo "Python job started on" `date`
{script2}
echo "Python job ended on" `date`
{script3}
{script4}
echo "Number of muons per file is {no_muons}"
echo "------------------------------------------------------------------------"
echo "python pandas_SRCEFILE.py called for {userdump_file} with radius < {max_radius}"
echo "Job ended on" `date`
echo "------------------------------------------------------------------------"
"""

def create_correct_muonratio(cycles):

    muon_min, muon_plus = cycles/2.3, cycles*(1.3/2.3)
    # Get the fractional parts of the numbers
    rounded_muon_min = math.floor(muon_min)
    rounded_muon_plus = math.floor(muon_plus)

    fractional_part1 = muon_min - rounded_muon_min
    fractional_part2 = muon_plus - rounded_muon_plus

    if fractional_part1 > fractional_part2:
        rounded_muon_min = math.floor(muon_min) + 1
    elif fractional_part1 < fractional_part2:
        rounded_muon_plus = math.floor(muon_plus) + 1
    else:
        rounded_muon_min = math.floor(muon_min) + 1
    
    return rounded_muon_min, rounded_muon_plus


# path = start and end with a slash
# job_folder = where to dump the new input files and the output
# out_folder = name and path of where to put the output: /dcache/xenon/kweerman/folder_name
# copy_file = new name without the .inp, cycles = number of runs
# fort_name = name of output file from USERDUMP
# files_folder = for all other files that might be important in the future
# python_filepath = path to the python file that used obtain isotope info from the unformatted usdraw file 
# routine_path = path to where all user routines and XeLS command is found
def submit_flukaruns(path, inp_file, copy_file, job_folder, out_folder, log_folder, files_folder, \
                     python_filepath, routine_path, que='kamland', fort_name='SRCEFILE', \
                     cycle_start=1, cycles=1, no_muons=10**5, max_radius = 192.0):

    # open the file where the random seed has to be changed 
    fin = open(path + inp_file,'r')
    filedate_in = fin.read()
    fin.close()

    muon_min, muon_plus = create_correct_muonratio(cycles)
    print(muon_min, muon_plus)

    # if the folder does not exist, we create one at the given path
    for folder in (job_folder, log_folder, out_folder, files_folder):
        if not os.path.exists(folder):
            os.makedirs(folder)

    # create new files where the number of random seeds is differently
    for cycle in range(cycle_start, cycles + cycle_start):
        filedata_out = filedate_in.replace("100.00","2%i."%(cycle))

        # ensure that the correct ratio of muon+ and muon- has been made
        if cycle > muon_plus:
            filedata_out = filedata_out.replace("MUON+","MUON-")

        # create the dublicate files with different numbers and add new lines
        new_inp = job_folder + copy_file + '{0}.inp'.format(cycle)
        fout = open(new_inp, 'w')
        fout.write(filedata_out)
        fout.close()

        # for every cycle define the userdump filename and 
        # filename for the events with elements heavier than helium
        # in addition the unformatted neutron file is defined for the no neutrons
        userdump_file = '{0}{1}001_{2}'.format(copy_file,cycle,fort_name)
        #event_file = '{0}_importantbatch{1}'.format(copy_file,cycle)
        AZ_file = '{0}_isotopes{1}pandas'.format(copy_file,cycle)
        neutron_file = '{0}{1}001_NEUTRO'.format(copy_file,cycle)
        resid_file = '{0}{1}001_RESIDNUC'.format(copy_file,cycle)

        # create scripts where fluka is called
        inp_script = job_folder + copy_file + 'script%i.sh'%(cycle)
        fscript = open(inp_script, 'w')

        # note this should correspond to the mgdraw fluka name choosen!
        script1 = '$FLUPRO/flutil/rfluka -e {0}XeLSgeometry -N0 -M1 '.format(routine_path) + new_inp

        # from this line the no neutrons and elemnts created will be printed in the log file
        #script2 = 'python {0}eventscreator_usdraw.py {1} {2}'.format(python_filepath, userdump_file, AZ_file)
        script2 = 'python {0}pandas_SRCEFILE.py {1} {2} {3} {4} {5}'.format(python_filepath, userdump_file, cycle, AZ_file, no_muons, max_radius)

        # in this case AZ_file contains the [AZ, particles, particle_count]
        # we move all necesarry output to the dcache folder
        script3 = 'mv {0} {1} {2} {3} {4}'.format(userdump_file, neutron_file, AZ_file, resid_file, out_folder)
        script4 = 'mv {0}{1}001.err {0}{1}001.log {0}{1}001.out \
                        {0}{1}001_fort.19 ran{0}{1}001 ran{0}{1}002 {2}'.format(copy_file,cycle,files_folder)
        script_file_content = script_template.format(inp_file=inp_file, path=job_folder, 
                                                        copy_file=new_inp,out_folder=out_folder,
                                                        script1=script1,script2=script2,script3=script3,
                                                        script4=script4,userdump_file=userdump_file, no_muons=no_muons, max_radius=max_radius)
        fscript.writelines(script_file_content)
        fscript.close()
        
        # submit the job -> check bqueues for the best queue before running
        log_file = log_folder + copy_file + '{0}.log'.format(cycle)
        error_file = log_folder + copy_file + '{0}error.log'.format(cycle)

        qsub_call = 'qsub -d {0} %s -o {1} -e {2} -l "mem=10gb" -q {3}'.format(log_folder, log_file, error_file, que) 
        call(qsub_call % inp_script, shell=True)

        os.remove(inp_script)


# Note: CHANGE CYCLE START FROM 1 TO PREVIOUS CYCLES + 1 IF YOU USE THE SAME FLUKA CODE AS A PREVIOUS BATCH
# Note: to get correct muon energy distribution use: IN BEAMPOS NEED TO ADD -50
# Note: also check that MUON+ is written in the code and 100.00

path0 = '/project/xenon/kweerman/exercises/FLUKA-muon-spallation/'
#out_folder = '/dcache/xenon/kweerman/10Feb2025Fluka_MuonDisk500R500/'
out_folder = '/dcache/xenon/kweerman/test2025/'
que = 'generic7'
cycle_start = 1
cycles = 100
#no_muons = 10**5
#no_muons = 10*10**5
no_muons = 50
#max_radius = 192.0
#max_radius = 650.0
max_radius = 500.0

routine_path = path0 + 'user_routines/geometry/'
path, python_filepath = path0 + 'input_files/', path0 + 'python_scripts/geometry/'
job_folder, log_folder = out_folder + 'input_files/', out_folder + 'log_files_fluka/'
files_folder = out_folder + 'extra_files_fluka'
submit_flukaruns(path, 'muons_KamLAND.inp', 'out_muonsKamLAND', job_folder, 
                 out_folder, log_folder, files_folder, python_filepath, routine_path, que, 'SRCEFILE', 
                 cycle_start, cycles, no_muons, max_radius)
