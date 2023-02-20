# Version 1 2022 Kelly Weerman
# reads binary mdstck file and counts the number of neutrons created in a 
# certain space window

import numpy as np
import sys

def counting_variable(variable, check_list, counting_list):
    # checks if the variable is already in the list, 
    # and either adds the variable or adds a count

    if variable not in check_list:
        check_list.append(variable)
        counting_list.append([variable,1])
    else:
        ind = check_list.index(variable)
        counting_list[ind][-1] += 1
    return

def ktonday(no_muons, eventcount):
    # express count as (kton day)^-1
    # no_muons_simulated should change according to the simulation

    sec24hr = 86400. 
    muonrate = 0.198 # Hz
    frequency = 1. / muonrate # every 5 sec one muon passes through
    time_muons = (no_muons * frequency) / sec24hr # detector livetime

    flukatracklength = 2500. #cm
    meantracklength = 874. #cm
    fluka_livetime = time_muons * (flukatracklength / meantracklength)

    densityXeLS = 0.78013 #g/cm^3
    XeLSvolume = 1150
    XeLSmass = densityXeLS * XeLSvolume * 10**(-3) # to get kton
    
    ktonday = eventcount / (XeLSmass * fluka_livetime) # (kton day)^-1

    return ktonday

def no_neutrons(paths, filename, job_range=[1,1]):
    # reads the binary neutron file and returns the neutron count,
    # gamma energy and created particle codes

    dataType = np.dtype([
        ('Egamma', np.float64),
        ('Eneutron', np.float64),
        ('Xtrack', np.float64),
        ('Ytrack', np.float64),
        ('Ztrack', np.float64),
        ('Npsecn', np.int32),
    ])

    particle_list, particle_count, gammaE_list, gammaE_count = [], [], [], []
    neutron_count, muon_tracker = 0, 0
    
    for path in paths:
        for job in range(job_range[0], job_range[1] + 1):
            try:
                file = open(path + "{0}{1}001_NEUTRO".format(filename,job),'rb')
            except FileNotFoundError:
                continue

            print("Opened {0}{1}001_NEUTRO".format(filename,job))
            # every time a file is opened, we keep track of the amount of muons
            muon_tracker += 1

            lines = file.read(4)
            header = np.fromfile(file,dtype='a28',count=1)[0]
            #print(header.decode())

            while lines:
                lines = file.read(8)
                data = np.fromfile(file,dtype=dataType,count=1)
                if len(data) == 0:
                    break

                # npsecn is the amount of particles created
                Egamma, Eneutron, X, Y, Z, npsecn = data[0]
                created_part = np.fromfile(file,dtype=np.int32,count=npsecn)

                # not the whole cylinder is tested
                if (Z > 1000 and Z < 3500):
                #if (Z > -1000 and Z < 1500):
                    neutron_count += 1

                    # we create a list that holds the energy count and particle count
                    gammaE_rounded = float("{:.2e}".format(Egamma))
                    counting_variable(gammaE_rounded, gammaE_list, gammaE_count)
                    
                    # for every event the particles are added as a list
                    counting_variable(list(created_part), particle_list, particle_count)
                
            file.close()

    return neutron_count, gammaE_count, particle_count, muon_tracker


def print_neutroninfo(paths, filename, job_range=[1,1], neutron_filename="NeutronInfo.txt"):
    # print the asked information to a newfile called isotopecount_filename

    neutron_count, gammaE_count, particle_count, muon_tracker = no_neutrons(paths, filename, job_range)
    no_muons = muon_tracker * (10**5)
    print("The number of muons is {0}".format(no_muons))

    newfile = open(neutron_filename,"w")
    print("The number of neutrons is {0} or {1} (kton day)^-1"\
                    .format(neutron_count, ktonday(no_muons,neutron_count)))
    newfile.write("The number of neutrons is {0} or {1} (kton day)^-1\n"\
                            .format(neutron_count, ktonday(no_muons,neutron_count)))

    # all particles of the interaction are printed with count
    newfile.write("\nCount or (kton day)^-1 and particles created\n")
    for particle_info in particle_count:
        particles, count = particle_info
        newfile.write("{0} or {1:e} : {2} \n"\
                    .format(count, ktonday(no_muons,count), particles))

    # we print the gamma energy and count
    newfile.write("\nCount or (kton day)^-1 and gamma energy\n")
    for gammaE in gammaE_count:
        energy, count = gammaE
        newfile.write("{0} or {1:e} : {2:e} GeV\n"\
                        .format(count, ktonday(no_muons,count), energy))

    newfile.close()
    return 

job_range = [1,150]
paths = ['/dcache/xenon/kweerman/NewSourceFile17feb/']
filename = "out_muonsXeLSLong"
neutron_filename = sys.argv[1]
#neutron_filename="NeutronInfotot.txt"
print_neutroninfo(paths, filename, job_range, neutron_filename)
