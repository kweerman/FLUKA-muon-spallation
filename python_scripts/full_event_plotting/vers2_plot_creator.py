# Version 2 2022 Kelly Weerman
# creates track plots of a file containing the lists with coordinates

import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
import zipfile
import io
import numpy as np
import pickle
import sys

def generation_names():
    # creates a list of particle names corresponding to numbers
    # starting at the 0th place (RAY) until the 62th place (AOMEGAC0)
    file1= open('/project/xenon/kweerman/Fluka/fluka2021.manual','r')
    lines = file1.readlines()

    wordslist = []
    firstline, lastline, chapter5 = False, False, False
    for line in lines:
        if lastline == True:
            break
        words = line.split()
        for word in words:
            # start at chapter 5 for the particle codes
            if word == '5.1}':
                chapter5 = True
            elif word == 'RAY' and chapter5 == True:
                firstline = True
            elif word == 'AOMEGAC0' and chapter5 == True:
                lastline = True

            # continue on the next line for the next particle generation
            if firstline == True:
                wordslist.append(word)
                break
    file1.close()
    return wordslist

def particle_generations(part_genlist):
    # creates a lists with particle numbers and names
    # remove the dublicates from generation list
    gen_list = list(dict.fromkeys(part_genlist))

    # add the particle names to the numbers:
    particle_codes = generation_names()
    particle_list = []
    exception_codes = ['OPTIPHOTON','HEAVY ION','DEUTERON', 'TRITON',\
                         '3-HELIUM', '4-HELIUM']

    for number in gen_list:
        # for particles that are not defined in the FLUKA manual
        if int(number) > 62 or int(number) < -6:
            particle_list.append(str(number))
        elif int(number) < 0:
            particle_list.append(exception_codes[abs(int(number))-1])
        else:
            particle_list.append(particle_codes[int(number)])
    return gen_list, particle_list


def plot_tracks(part_genlist, no_events, x_list, y_list, z_list, resAZ, run, zf):
    # generates a seperate plot for every event 
    gen_list, particle_list = particle_generations(part_genlist)

    # create a list with all the colors for the legend
    colors = dict(mcolors.BASE_COLORS, **mcolors.CSS4_COLORS)
    colors_list = []
    for name,color in colors.items():
        colors_list.append(name)
    colors_list.remove('w')

    # if there are not enough colors, the list is extended
    while len(gen_list) > len(colors_list):
        colors_list.extend(colors_list.copy())

    # appoint the correct color per particle type
    plt.figure()
    for tracks in range(len(x_list)):
        color_index = gen_list.index(part_genlist[tracks])
        plt.plot(z_list[tracks], x_list[tracks],colors_list[color_index])
    plt.legend(particle_list)

    # change the legend colors to match the part generations
    leg =  plt.gca().get_legend()
    for i in range(len(gen_list)):
        leg.legendHandles[i].set_color(colors_list[i])

    plt.axis([-1,4000,-50,50])
    # code necesarry to create a zipped file for all figures
    buf = io.BytesIO()
    plt.savefig(buf)
    plt.close()
    img_name = "run{0}_rock_muons_event{1}.png".format(run,no_events)
    zf.writestr(img_name, buf.getvalue()) 
    
    print("{0} particles heavier than Helium are generated in event {1}".format(len(resAZ),no_events))
    print("The isotope list is {0}".format(resAZ))  

    return gen_list, particle_list


def pickleLoader(pklFile):
    try:
        while True:
            yield pickle.load(pklFile)
    except EOFError:
        pass


def create_figures_zip(event_filename, zipfilename, run):
    # creates a zipfile with figures that contain heavy elements

    events = True
    file = open(event_filename,'rb')
    with zipfile.ZipFile(zipfilename, mode="w") as zf:
        for event in pickleLoader(file):

            # create an empty zipfile if no interesting events are found
            if event == 0:
                events = False
                print("No interesting events are created")
                pass 

            else:
                part_genlist, no_events, x_list, y_list, z_list, resAZ = event
                plot_tracks(part_genlist, no_events, x_list, y_list, z_list, resAZ, run, zf)  

    file.close()
    return events
    
#path = '/dcache/xenon/kweerman/rocktest1/'
#event_filename, zip_filename = '{0}out_rockmuons_importantbatch6'.format(path), 'ziptest6.zip'
event_filename, zip_filename, run = sys.argv[1], sys.argv[2], sys.argv[3]
create_figures_zip(event_filename, zip_filename, run)
