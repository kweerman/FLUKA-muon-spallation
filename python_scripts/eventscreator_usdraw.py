# Version 5 2023 Kelly Weerman
# reads SRCEFILE created with usdraw version 1 and saves the daughter particles
# to a list of the form part_gens = [[A,Z], particles lists] where particle lists [jtrack, 6,10,[4,2]] , [...], ...
# also returns count list corresponding to the particle lists in the form part_gen_count = [[A,Z], count] 
# lastly counts the neutron capture on hydrogen, which is the line: 1, 1, count, X, Y, Z, E
# "isotope_filename" is filled with one line: totAZ_count, part_gens, part_gen_count, hydrogen_capture, carbon_capture

import numpy as np
import sys
import pickle


def events_creator(filename, isotope_filename):
    # reads the SRCEFILE and creates lists of parent and daughter particles 
    # together with the count, for a certain area of space

    dataTypeStart = np.dtype([
        ('Icode', np.int32),
        ('Partcode', np.int32),
        ('Jtrack', np.int32),
    ])

    dataTypeCoord = np.dtype([
        ('Xtrack', np.float64),
        ('Ytrack', np.float64),
        ('Ztrack', np.float64),
    ])

    dataTypeAZ = np.dtype([
        ('A', np.int32),
        ('Z', np.int32),
        ('Np', np.int32),
    ])

    particle_list = []
    totAZ, totAZ_count = [], []
    part_gens, part_gen_count = [], []
    neutron_energylist = []

    try:
        file = open(filename,'rb')
    except IOError:
        raise Exception('Fail to open file')

    # the first line has different bytes than the rest
    lines = file.read(4)
    header = np.fromfile(file,dtype='a28',count=1)[0]
    print(header.decode())
    no_events, hydrogen_capture, carbon_capture = 0, 0, 0

    while lines:
        lines = file.read(8)

        # the last line of the file is empty
        data = np.fromfile(file,dtype=dataTypeStart,count=1)
        if len(data) == 0:
            break
        
        icode, partcode, jtrack = data[0]
        particle_list = [jtrack]

        # a new event line is indicated with the following
        if icode == 0.:
            no_events += 1
            continue

        x0, y0, z0 = np.fromfile(file,dtype=dataTypeCoord,count=1)[0]

        # a neutron capture event is indicated and counted with the following
        # the third variable - jtrack - is the hydrogen neutron count in this case
        if icode == 1:
            capture_energy = np.fromfile(file,dtype=np.float64,count=1)[0]
            
            # not the whole cylinder is tested
            if (z0 > 1000 and z0 < 3500):
                hydrogen_capture += 1
                neutron_energylist.append(capture_energy)
                
            # the next line is read
            continue

        A, Z, n_part = np.fromfile(file,dtype=dataTypeAZ,count=1)[0]

        # the number of KPART particles is indicated with np
        if n_part > 0:
            kpart_parts = np.fromfile(file,dtype=np.int32,count=n_part)
            particle_list.extend(kpart_parts.tolist())

        # then the heavy particles
        # in this case we return the A, Z so twice as many counts
        npheav = np.fromfile(file,dtype=np.int32,count=1)[0]
        if npheav > 0:
            kheavy_part = np.fromfile(file,dtype=np.int32,count=2*npheav)
            particle_list.append(kheavy_part.tolist())

        # we don't count the de-excitation as new isotopes and
        # not the whole cylinder is tested
        if icode != 106 and (z0 > 1000 and z0 < 3500):
            isotope = [A, Z]
            
            # check if we have carbon capture by neutrons
            if isotope == [13, 6] and jtrack == 8:
                carbon_capture += 1
            
            # now we define the isotope counting lists
            in_list = False
            if isotope not in totAZ:
                totAZ.append(isotope)
                totAZ_count.append([A, Z, 1])

                part_gens.append([isotope, particle_list])
                part_gen_count.append([isotope, 1])

            else:
                ind = totAZ.index(isotope)
                totAZ_count[ind][-1] += 1

                no_reactions = 0
                for part_range in part_gens[ind][1:]:
                    no_reactions += 1
                    if particle_list == part_range:
                        part_gen_count[ind][no_reactions] += 1
                        in_list = True
                        break
                if in_list == False:
                    part_gens[ind].append(particle_list)
                    part_gen_count[ind].append(1)

        
    file.close()
    isotope_file = open(isotope_filename,'wb')
    pickle.dump([totAZ_count, part_gens, part_gen_count, hydrogen_capture, carbon_capture], isotope_file)
    isotope_file.close()
    print("The isotope list is given by {0}".format(totAZ_count))
    print("The number of hydrogen neutron capture is {0} and carbon neutron capture is {1}".format(hydrogen_capture, carbon_capture))
    return


file_name, isotope_filename = sys.argv[1], sys.argv[2]
#file_name = "muons_XeLSLong001_SRCEFILE"
#isotope_filename = "important"
events_creator(file_name, isotope_filename)