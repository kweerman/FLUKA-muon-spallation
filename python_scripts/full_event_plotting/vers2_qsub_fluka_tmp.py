# version 2 2022 Kelly Weerman: mgdraw included
# qsub fluka script and return an unformatted file with events where elements
# heavier than helium are created
from subprocess import call 
import os

script_template = """#!/bin/bash
echo "------------------------------------------------------------------------"
echo "Job started on" `date`
echo "Copy made of {inp_file} from {path}"
echo "Data saved to {copy_file}" 
echo "Output saved to {out_folder}"
echo "------------------------------------------------------------------------"
cd $TMPDIR
{script1}
{script2}
{script3}
echo "------------------------------------------------------------------------"
echo "python vers2_eventscreator.py called for {userdump_file}"
echo "Job ended on" `date`
echo "------------------------------------------------------------------------"
"""

# path = start and end with a slash
# job_folder = where to dump the new input files and the output
# out_folder = name and path of where to put the output: /dcache/xenon/kweerman/folder_name
# copy_file = new name without the .inp, cycles = number of runs
# fort_name = name of output file from USERDUMP
def submit_flukaruns(path, inp_file, copy_file, job_folder, out_folder, log_folder, python_filepath, fort_name='SRCEFILE', cycles=1):

    # open the file where the random seed has to be changed 
    fin = open(path + inp_file,'r')
    filedate_in = fin.read()
    fin.close()

    # if the folder does not exist, we create one at the given path
    for folder in (job_folder, log_folder, out_folder):
        if not os.path.exists(folder):
            os.makedirs(folder)

    # create new files where the number of random seeds is differently
    for cycle in range(1, cycles + 1):
        if cycle > 9:
            filedata_out = filedate_in.replace("100.00","1%i.00"%(cycle))
        else:
            filedata_out = filedate_in.replace("100.00","10%i.00"%(cycle))

        # create the dublicate files with different numbers and add new lines
        new_inp = job_folder + copy_file + '{0}.inp'.format(cycle)
        fout = open(new_inp, 'w')
        fout.write(filedata_out)
        fout.close()

        # for every cycle define the userdump filename and 
        # filename for the events with elements heavier than helium
        userdump_file = '{0}{1}001_{2}'.format(copy_file,cycle,fort_name)
        event_file = '{0}_importantbatch{1}'.format(copy_file,cycle)

        # create scripts where fluka is called
        inp_script = job_folder + copy_file + 'script%i.sh'%(cycle)
        fscript = open(inp_script, 'w')

        # note this should correspond to the mgdraw fluka name choosen!
        script1 = '$FLUPRO/flutil/rfluka -e $FLUPRO/flutil/XeLSvers2 -N0 -M1 ' + new_inp
        script2 = 'python {0}vers2_eventscreator.py {1} {2}'.format(python_filepath, userdump_file, event_file)

        # we move all necesarry output to the dcache folder
        move_line = 'mv {0} {1} '.format(userdump_file, event_file)
        script3 = move_line + '{0}{1}001.err {0}{1}001.log {0}{1}001.out \
                        ran{0}{1}001 ran{0}{1}002 {2}'.format(copy_file,cycle,out_folder)
        script_file_content = script_template.format(inp_file=inp_file, path=job_folder, 
                                                        copy_file=new_inp,out_folder=out_folder,
                                                        script1=script1,script2=script2,script3=script3,
                                                        userdump_file=userdump_file)
        fscript.writelines(script_file_content)
        fscript.close()
        
        # send the job to stoomboot immediately
        log_file = log_folder + copy_file + '{0}.log'.format(cycle)
        error_file = log_folder + copy_file + '{0}error.log'.format(cycle)

        # the -d indicates where the log and error file will be dumped! 
        qsub_call = 'qsub -d {0} %s -o {1} -e {2} -l "mem=10gb'.format(log_folder, log_file, error_file) 
        call(qsub_call % inp_script, shell=True)

        os.remove(inp_script)

        
#path = '/project/xenon/kweerman/exercises/mgdraw/test/rock/'
#out_folder = '/dcache/xenon/kweerman/rocktest2/'
#job_folder, log_folder = path + 'input_files1/', out_folder + 'log_files_fluka/'
#submit_flukaruns(path, 'muons_rock.inp', 'out_rockmuons', 
#                    job_folder, out_folder, log_folder,'SRCEFILE',8)

python_filepath = '/project/xenon/kweerman/exercises/'
path = '/project/xenon/kweerman/exercises/KamLAND-XeLS/large_cylinder/'
out_folder = '/dcache/xenon/kweerman/XeLSLargeCylinder/'
job_folder, log_folder = path + 'input_files/', out_folder + 'log_files_fluka/'
submit_flukaruns(path, 'muons_XeLS.inp', 'out_muonsXeLS', 
                    job_folder, out_folder, log_folder, python_filepath, 'SRCEFILE',8)

