# Version 2 2022 Kelly Weerman
# reads binary RESIDNUCs file and adds the counts for every job run
# txt file returned with the total isotope count list ordered
# in addition txt file prints the isotope count per line
# also prints isomeric information

import pickle
import numpy as np
from operator import itemgetter
import csv 
import sys

def pickleLoader(pklFile):
    try:
        while True:
            yield pickle.load(pklFile)
    except EOFError:
        pass

def isotope_list_RESIDNUC(paths, filename, job_range=[1,10]):
    # adds all the RESIDNUC files in the job_range given
    # returns a counting list of the form [[A,Z,count],[...],]

    dataType = np.dtype([
        ('IZ', np.int32),
        ('IA', np.int32),
        ('IS', np.int32),
        ('Xtrack', np.float64),
        ('Ytrack', np.float64),
        ('Ztrack', np.float64),
    ])

    totAZ, totAZ_count = [], []
    muon_tracker = 0

    # get the files from multiple paths
    for path in paths:             
        for job in range(job_range[0], job_range[1] + 1):
            try:
                file = open(path + "{0}{1}001_RESIDNUC".format(filename,job),'rb')
            except FileNotFoundError:
                print("{0}{1}001_RESIDNUC is not found".format(filename,job))
                continue

            print("Opened {0}{1}001_RESIDNUC".format(filename,job))
            # every time a file is opened, we keep track of the amount of muons
            muon_tracker += 1

            # return all the information from the files, and add them
            lines = file.read(4)
            header = np.fromfile(file,dtype='a28',count=1)[0]
            #print(header.decode())

            while lines:
                lines = file.read(8)
                data = np.fromfile(file,dtype=dataType,count=1)
                if len(data) == 0:
                    break

                IZ, IA, IS, X, Y, Z = data[0]

                # not the whole cylinder is tested
                if (Z > 1000 and Z < 3500):
               # if (Z > -1000 and Z < 1500):
                    # create a list with the count per isotope
                    isotope = [IA, IZ, IS]
                    if isotope not in totAZ:
                        totAZ.append(isotope)
                        totAZ_count.append([IA, IZ, IS, 1])
                    else:
                        ind = totAZ.index(isotope)
                        totAZ_count[ind][-1] += 1

            file.close()

    return totAZ_count, muon_tracker


def elements_list(elements_file):
    # returns a list containing the names of the chemical elements
    # the index corresponds to the Z value
    
    elementsFile = open(elements_file, encoding='utf-8')
    elements = list(csv.reader(elementsFile))
    elementsFile.close()

    elements_list = []
    for line in elements:
        elements_list.append(line[1])

    return elements_list


def isotope_names(totAZ_count, elements_file):
    # creates a list containing the names of the isotopes and sorting them 

    sorted_isotopes = sorted(totAZ_count, key=itemgetter(1))
    elements_names = elements_list(elements_file)
    isotope_nameslist, isotope_numberlist = [], []
    last_Z_value, first_count, last_count = 0, 0, 0

    for isotope in sorted_isotopes:
        A, Z, IS, count = isotope

        # every time we have a new element, we sort the A numbers
        if last_Z_value != Z:
            names = isotope_nameslist[first_count:last_count]
            numbers = isotope_numberlist[first_count:last_count]
            isotope_nameslist[first_count:last_count] = sorted(names, key=itemgetter(1))
            isotope_numberlist[first_count:last_count] = sorted(numbers, key=itemgetter(1))
            first_count = last_count

        isotope_nameslist.append([elements_names[Z], A, IS, count])
        isotope_numberlist.append([Z, A, IS, count])
        last_count += 1
        last_Z_value = Z

        # at the last step the A numbers have to be sorted once again
        if last_count == len(sorted_isotopes):
            names = isotope_nameslist[first_count:last_count]
            numbers = isotope_numberlist[first_count:last_count]
            isotope_nameslist[first_count:last_count] = sorted(names, key=itemgetter(1))
            isotope_numberlist[first_count:last_count] = sorted(numbers, key=itemgetter(1))

    return isotope_numberlist, isotope_nameslist

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


def print_isotopes(totAZ_count, no_muons, isotopecount_filename="AZisotopesRESIDNUC2.txt"):
    # print the asked information to a newfile called isotopecount_filename

    newfile = open(isotopecount_filename,"w")
    elements_file = '/project/xenon/kweerman/exercises/PubChemElements_all.csv'
    isotope_numberlist, isotope_nameslist = isotope_names(totAZ_count, elements_file)
    print("The isotope list created is {0}".format(isotope_numberlist))

    newfile.write("{0}\n".format(isotope_numberlist))
    sorted_on_counts = sorted(isotope_nameslist, key=itemgetter(2))

    # uncommand the following line if you want to use the names of the isotopes
    # for line in isotope_nameslist:
    for line in isotope_numberlist:
        A, Z, IS, count = line
        print(A, Z, IS, count, "{:e}".format(ktonday(no_muons,count)))
        newfile.write("{0} {1} {2} {3} {4:e}\n".format(A, Z, IS, count, ktonday(no_muons,count)))

    newfile.close()
    return 


job_range = [1,200]
#paths = ['/dcache/xenon/kweerman/VeryLargeBatch12Mar/']
paths = ['/dcache/xenon/kweerman/18SepMuonEdiffMu+Mu-Fluka2023_2/']
filename = "out_muonsXeLSLong"

#isotopecount_filename="AZisotopesRESIDNUCtot.txt"
isotopecount_filename = sys.argv[1]

totAZ_count, muon_tracker = isotope_list_RESIDNUC(paths, filename, job_range)
# note: no_muons has to be changed according to input!!
#no_muons = muon_tracker * (5*10**5)
no_muons = muon_tracker * (1*10**5)
print("The number of muons is {0}".format(no_muons))
print_isotopes(totAZ_count, no_muons, isotopecount_filename)



