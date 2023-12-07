# Version 3 2023 Kelly Weerman 
# from the "outfile_isotopes" file created with vers5_eventscreator.py the information is substracted
# which is totAZ_count ([A,Z,count]), AZ_jtrack([A,Z],[jtrack],[count]), part_gen_count ([[A,Z],[particles_lists],[counts]])
# and [particle_lists] = [ [jtrack, 6,10,[4,2]] , [...], ] where the last number inbetween brackets, i.e. [4,2], is the A Z of daughters
# return totAZ_count, AZ_jtrack, interactions_list, no_muons in the form [A, Z, count], [[A,Z], [jtracks], [counts]]
# [[A,Z], [particles lists], [counts]] where particle lists [jtrack, 6,10,[4,2]] , [...], ... and no_muons
# additional hydrogen and carbon count returned, as well as a list of muon energies per isotope of the form [isotope, [muon_energy], [count]]
# the created lists (in unformatted "isotope_filename") can be printed with "usdraw_createdaughterfile.ipynb"
# add the isomeric information

import pickle
from operator import itemgetter
import csv
import numpy as np
import sys


def pickleLoader(pklFile):
    try:
        while True:
            yield pickle.load(pklFile)
    except EOFError:
        pass

def extract_first(lijst):
    # returns the first value of each sublist in a list of lists
    return list(map(itemgetter(0), lijst))

def addinfo_tolists(variable, count, list_ind, return_list):
    # return list of the form [[..], [variables], [counts]]
    # checks if the variable is in return_list[ind][1] and adds corresponding count

    if variable not in return_list[list_ind][1]:
        return_list[list_ind][1].append(variable)
        return_list[list_ind][2].append(count)
    else:
        variable_ind = return_list[list_ind][1].index(variable)
        return_list[list_ind][2][variable_ind] += count
    
    return return_list


def isotope_list_count(path, file_name, isotope_filename, job_range=[1,2]):

    interactions_list = []
    no_muons = 0
    totAZ, totAZ_count, totAZ_muonE = [], [], []
    AZ_jtrack = []
    hydrogen_capcount, carbon_capcount = 0, 0
    tot_muondiff, tot_parentscount, tot_energyevents = [], [], []
            
    for job in range(job_range[0], job_range[1] + 1):
        try:
            file = open(path + file_name + "%i"%(job),'rb')
        except FileNotFoundError:
            continue

        no_muons += 1
        print("Opened ", file_name + "%i"%(job))

        # return all the information from the files, and add them
        for event in pickleLoader(file):
            isotope_list, part_gens, part_gen_count, hydrogen_capture, carbon_capture, \
            isotope_muonE, muon_differences, parents_total, energy_list_total = event
        
        file.close()
        hydrogen_capcount += hydrogen_capture
        carbon_capcount += carbon_capture
        tot_muondiff.append(muon_differences)
        tot_parentscount.append(parents_total)
        tot_energyevents.append(energy_list_total)

        # the amount of isotopes created is the length of AZ list
        for i in range(len(isotope_list)):
            A, Z, IS, count = isotope_list[i]
            
            # create a list with the count per isotope
            isotope = [A, Z, IS]
            interactions = part_gens[i][1:]
            jtracks = extract_first(interactions)
            count_interactions = part_gen_count[i][1:]

            muonE_list = isotope_muonE[i][1]
            muonE_counts = isotope_muonE[i][2]

            if isotope not in totAZ:
                totAZ.append(isotope)
                totAZ_count.append([A, Z, IS, count])
                totAZ_muonE.append([isotope, muonE_list, muonE_counts])

                # in the form[[A,Z],[particles_lists],[counts]]
                interactions_list.append([isotope, interactions, count_interactions])
                
                # create the jtrack list of the form [[A,Z],[jtracks],[counts]]
                AZ_jtrack.append([isotope, [], []])
                for i in range(len(interactions)):
                    # index is -1 since we just added the AZ to the list
                    list_ind, jtrack, jtrack_count = -1, jtracks[i], count_interactions[i]
                    AZ_jtrack = addinfo_tolists(jtrack, jtrack_count, list_ind, AZ_jtrack)
                
            else:
                ind = totAZ.index(isotope)
                totAZ_count[ind][-1] += count 

                for k in range(len(muonE_list)):
                    muonE, countE = muonE_list[k], muonE_counts[k]
                    totAZ_muonE = addinfo_tolists(muonE, countE, ind, totAZ_muonE)
                    
                # the particle creation and parent lists are updated
                for j in range(len(interactions)):
                    interaction, count_interaction = interactions[j], count_interactions[j]
                    interactions_list = addinfo_tolists(interaction, count_interaction, ind, interactions_list)

                    jtrack, jtrack_count = jtracks[j], count_interactions[j]
                    AZ_jtrack = addinfo_tolists(jtrack, jtrack_count, ind, AZ_jtrack)
    
    file.close()

    # return the total lists in a file for easier plotting
    isotope_file = open(isotope_filename,'wb')
    pickle.dump([totAZ_count, AZ_jtrack, interactions_list, no_muons, hydrogen_capcount, carbon_capcount, totAZ_muonE], isotope_file)
   # pickle.dump([hydrogen_capcount, carbon_capcount, totAZ_muonE, tot_muondiff, tot_parentscount], isotope_file2)
    print("The isotope list for {0} muons is given by {1}".format(no_muons, totAZ_count))
    print("The number of hydrogen neutron capture is {0} and carbon neutron capture is {1}".format(hydrogen_capcount, carbon_capcount))
    print("The muon energies per isotope is given by {0}".format(totAZ_muonE))
    isotope_file.close()
    
    return totAZ_count, AZ_jtrack, interactions_list, no_muons, hydrogen_capcount, carbon_capcount, totAZ_muonE, tot_muondiff, tot_parentscount

path = "/dcache/xenon/kweerman/18SepMuonEdiffMu+Mu-Fluka2023_2/"
file_name = "out_muonsXeLSLong_isotopes"
job_range = [1,200]
# the isotope file is a file that contains all the added counts from 150 tuns
isotope_filename = sys.argv[1]

isotope_list_count(path, file_name, isotope_filename, job_range)

