# version 2 Kelly Weerman
# compare the isotope production txt file with KamLAND 

import ast
from itertools import count 
import sys

def AZlist(list_name):
    isotope_list = []
    for item in list_name:
        Z, A, count = item
        isotope_list.append([Z, A])
    return isotope_list


def ktonday(no_muons, eventcount):
    # express count as (kton day)^-1
    # no_muons_simulated should change according to the simulation

    sec24hr = 86400. 
    muonrate = 0.198 # Hz
    frequency = 1. / muonrate # every 5 sec one muon passes through
    time_muons = (no_muons * frequency) / sec24hr # 585 days of simulating, detector livetime

    flukatracklength = 2500. #cm
    meantracklength = 874. #cm
    fluka_livetime = time_muons * (flukatracklength / meantracklength)

    densityXeLS = 0.78013 #g/cm^3
    XeLSvolume = 1150
    XeLSmass = densityXeLS * XeLSvolume * 10**(-3) # to get kton
    
    ktonday = eventcount / (XeLSmass * fluka_livetime) # (kton day)^-1

    return ktonday


def read_kamlandfile(file_name):
    # read the zen800 txt file and return Z, A, count list
    
    kamland_file = open(file_name, 'r')
    kamland_list, error_list = [], []
    for line in kamland_file:
        Z, A, count, error = line.split()
        kamland_list.append([int(Z), int(A), float(count)])
        error_list.append(float(error))
    kamland_file.close()

    return kamland_list, error_list


def compare_KamLAND(file_names, newfile_name, no_muons):
    # compare isotope results with kamland file
    # file_names = ["ownresults.txt, kamlandresults.txt"]

    newfile = open(newfile_name, "w")
    file_kelly = open(file_names[0], 'r')

    # check if the header of the file contains neutron count
    line1 = file_kelly.readline()
    if len(line1) < 100:
        neutrons = line1.split()[5]
        print(neutrons)

        # skip to the next line containing the lists
        isotopes_kelly = file_kelly.readline().strip()
    else:
        isotopes_kelly = line1.strip()

    list_kelly = ast.literal_eval(isotopes_kelly)
    isotopes_kelly = AZlist(list_kelly)

    # the count list of kamland is created here, note count is in events/day/ktonLS
    list_kamland, list_error = read_kamlandfile(file_names[1])
    isotopes_kamland = AZlist(list_kamland)
    
    newfile.write("Comparison {0} - {1} \n".format(file_names[0], file_names[1]))
    newfile.write("Index: Z, A, count {0}, count {1}, difference, error KamLAND, difference / count of {0} \n".format(file_names[0], file_names[1]))

    for isotope in isotopes_kelly:
        index_kelly = isotopes_kelly.index(isotope)
        count_kelly = ktonday(no_muons, list_kelly[index_kelly][2])

        if isotope in isotopes_kamland:
            index_kamland = isotopes_kamland.index(isotope)
            count_kamland = list_kamland[index_kamland][2]
            difference = count_kelly - count_kamland
            percentage = (float(difference)/count_kelly) * 100
            error_kamland = list_error[index_kamland]
            newfile.write("{0} {1} {2:e} {3:e} {4:e}, error: {5:e}, in percentage {6:.2f} \n"\
                .format(isotope[0], isotope[1], count_kelly, count_kamland, difference, error_kamland, abs(percentage)))
        else:
            newfile.write("{0} {1} {2:e} not in file {3}\n"\
                .format(isotope[0], isotope[1], count_kelly, file_names[1]))
        
    for isotope in isotopes_kamland:
        index_kamland = isotopes_kamland.index(isotope)
        count_kamland = list_kamland[index_kamland][2]

        if isotope not in isotopes_kelly:
            newfile.write("{0} {1} {2:e} not in file {3}\n"\
                .format(isotope[0], isotope[1], count_kamland, file_names[0]))
            
    file_kelly.close()
    newfile.close()

# find no_muons from the FlukaReader file, printed that
no_muons = 15000000
file_names = ["AZisotopesKelly.txt", "zen800_210602.txt"]
compare_KamLAND(file_names, "comparison_isotopes_KamLANDKelly.txt", no_muons)
# python comparison_KamLAND.py AZisotopesRESIDNUCtot.txt zen800_210602.txt