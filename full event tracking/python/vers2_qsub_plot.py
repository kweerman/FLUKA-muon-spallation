# version 2 2022 Kelly Weerman
# created plots for elements heavier than helium, input are the unformatted
# files containing lists

from subprocess import call 
import os
import pickle

script_template = """#!/bin/bash
echo "------------------------------------------------------------------------"
echo "Job started on" `date`
echo "Data extracted from {event_filename} from {path}"
echo "Zip files moved to {out_folder} and unzipped"
echo "------------------------------------------------------------------------"
cd $TMPDIR
{script1}
{script2}
cd {out_folder}
{script3}
echo "------------------------------------------------------------------------"
echo "python vers2_plot_creator.py called"
echo "Job ended on" `date`
echo "------------------------------------------------------------------------"
"""

# path = start and end with a slash
# event_filename = the unformatted files with lists of coordinates and particle generations
# out_folder = where the zipfile and figures are dumped\
# cycles = should correspond to the fluka run cycles
def submit_plots(path, event_filename, out_folder, log_folder, cycles=1):
    python_filepath = '/project/xenon/kweerman/exercises/'

    for cycle in range(1, cycles + 1):
        event_file = path + event_filename + str(cycle)

        # only in the event that heavy elements are found do we create zipfiles and figures
        file = open(event_file,'rb')
        try:
            event = pickle.load(file)
        except EOFError:
            pass
        if event != 0:
            for folder in (log_folder, out_folder):
                if not os.path.exists(folder):
                    os.makedirs(folder)
            file.close()

            # create scripts where python is called and the zipfile is generated
            inp_script = path + event_filename + 'script%i.sh'%(cycle)
            fscript = open(inp_script, 'w')
            zip_file = '{0}_zipfile{1}.zip'.format(event_filename,cycle)
            script1 = 'python {0}vers2_plot_creator.py {1} {2} {3}'.format(python_filepath, event_file, zip_file, cycle)

            # move the zip files to the output folder and subsequently unzip there
            script2 = 'mv {0} {1}'.format(zip_file, out_folder)
            script3 = 'unzip {0}'.format(zip_file)
            script_file_content = script_template.format(event_filename=event_filename,path=path,out_folder=out_folder,
                                                            script1=script1,script2=script2,script3=script3)
            fscript.writelines(script_file_content)
            fscript.close()
            
            # send the job to stoomboot immediately
            log_file = log_folder + 'plot_run{0}.log'.format(cycle)
            error_file = log_folder + 'plot_run{0}error.log'.format(cycle)

            # the -d indicates where the log and error file will be dumped! 
            qsub_call = 'qsub -d {0} %s -o {1} -e {2}'.format(log_folder, log_file, error_file) 
            call(qsub_call % inp_script, shell=True)

            os.remove(inp_script)

        
# create new pythonfile to call the function, or call here
#path = '/dcache/xenon/kweerman/rocktest2/'
#out_folder, log_folder = path + 'zip_files/', path + 'log_plots/'
#event_filename = 'out_rockmuons_importantbatch'
#cycles = 8
#submit_plots(path, event_filename, out_folder, log_folder, cycles)

path = '/dcache/xenon/kweerman/XeLSLargeCylinder/'
out_folder, log_folder = path + 'zip_files/', path + 'log_plots/'
event_filename = 'out_muonsXeLS_importantbatch'
cycles = 8
submit_plots(path, event_filename, out_folder, log_folder, cycles)
