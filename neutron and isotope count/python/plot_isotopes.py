# Version 1 2022 Kelly Weerman
# adds all isotope information and neutron count of unformatted isotope files created by vers4_eventscreator.py
# plots a histogram of A as a function of Z and prints the count per isotope and total neutron count
# isotope list in the form [[A1, Z1, count],[A2, Z2, count], ...]
import pickle
import numpy as np
import matplotlib.pyplot as plt

def pickleLoader(pklFile):
    try:
        while True:
            yield pickle.load(pklFile)
    except EOFError:
        pass

def isotope_list(file_names, job_range=[1,10]):
    # create a list of isotopes that can be used for histogram plotting
    # return neutron count and isotope count
    # file_names and job_range are lists 

    A_list, Z_list = [], []
    totAZ, totAZ_count = [], []
    total_neutrons, tot_events = 0, 0
    
    # we loop through a list of the file paths
    for file_name in file_names:                
        for job in range(job_range[0], job_range[1] + 1):
            try:
                file = open(file_name + "%i"%(job),'rb')
            except FileNotFoundError:
                continue

            print("Opened ", file_name + "%i"%(job))

            # return all the information from the files, and add them
            for event in pickleLoader(file):
                no_neutrons, event_count, isotope_list = event
                total_neutrons += no_neutrons
                tot_events += event_count
            file.close()

            for isotope_count in isotope_list:
                A, Z, count = isotope_count

                # create a list with the count per isotope
                isotope = [A, Z]
                if isotope not in totAZ:
                    totAZ.append(isotope)
                    totAZ_count.append([A, Z, count])
                else:
                    ind = totAZ.index(isotope)
                    totAZ_count[ind][-1] += count

                # create lists to plot a histogram
                A_list.extend([A]*count)
                Z_list.extend([Z]*count)
    
    return total_neutrons, tot_events, totAZ_count, Z_list, A_list

def histogram(file_names, job_range=[1,10], figure_name="AZhistogram"):
    # creates a histogram of all isotopes and prints the information
    # file_names and job_range are lists 

    total_neutrons, tot_events, totAZ_count, Z_list, A_list = isotope_list(file_names, job_range)

    print("The number of neutrons is {0}".format(total_neutrons))
    print("For {0} events the isotope list created is {1}".format(tot_events, totAZ_count))
    plt.hist2d(Z_list, A_list, bins=[60,140], cmin=1)
    plt.xlim([0,60])
    plt.ylim([0,140])
    plt.xlabel("Z")
    plt.ylabel("A")
    plt.colorbar(label="count")
    plt.savefig(figure_name)

job_range = [1,20]
file_names = ["/dcache/xenon/kweerman/XeLS100000/out_muonsXeLSLong_isotopes", "/dcache/xenon/kweerman/XeLS50000/out_muonsXeLS50000_isotopes"]
figure_name = "AZhistogram"
histogram(file_names, job_range, figure_name)
