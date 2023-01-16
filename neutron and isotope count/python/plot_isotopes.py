import pickle
import numpy as np
import matplotlib.pyplot as plt

def pickleLoader(pklFile):
    try:
        while True:
            yield pickle.load(pklFile)
    except EOFError:
        pass
""""
file = open("out_muonsXeLS5000_isotopes1",'rb')
for event in pickleLoader(file):
    no_neutrons, event_count, isotope_list = event

file.close()
A_list, Z_list, count_list = [], [], []
for isotope in isotope_list:
    A, Z, count = isotope
    A_list.extend([A]*count)
    Z_list.extend([Z]*count)
    count_list.append(count)

print(no_neutrons, event_count, isotope_list)
plt.hist2d(Z_list, A_list, bins=[60,140], cmin=1)
plt.xlim([0,60])
plt.ylim([0,140])
plt.xlabel("Z")
plt.ylabel("A")
plt.colorbar(label="count")
plt.savefig("AZ.png")

"""

A_list, Z_list = [], []
totAZ, totAZ_count = [], []
total_neutrons, tot_events = 0, 0
path = "/dcache/xenon/kweerman/XeLS100000/"
file_name = "out_muonsXeLSLong_isotopes"
for job in range(1,10):
    try:
        file = open(path + file_name + "%i"%(job),'rb')
    except FileNotFoundError:
        continue
    print("Opened ", file_name + "%i"%(job))
    for event in pickleLoader(file):
        no_neutrons, event_count, isotope_list = event
        total_neutrons += no_neutrons
        tot_events += event_count
    file.close()

    for isotope_count in isotope_list:
        A, Z, count = isotope_count

        # create a list with all the counts
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

print("The number of neutrons is {0}".format(total_neutrons))
print("For {0} events the isotope list created is {1}".format(tot_events, totAZ_count))
plt.hist2d(Z_list, A_list, bins=[60,140], cmin=1)
plt.xlim([0,60])
plt.ylim([0,140])
plt.xlabel("Z")
plt.ylabel("A")
plt.colorbar(label="count")
plt.savefig("AZ2.png")

