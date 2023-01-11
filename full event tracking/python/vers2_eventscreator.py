# Version 2 2022 Kelly Weerman
# creates an unformatted file containing track coordinates and information of
# events with elements heavier than helium
import matplotlib.pyplot as plt
import zipfile
import io
import sys
import numpy as np
import pickle


def events_creator(filename, event_filename):
    # read out the values of the fort file and returns a file with events
    # where elements heavier than helium are created: event_file

    dataType = np.dtype([
        ('Jtrack', np.int32),
        ('Xtrack0', np.float32),
        ('Ytrack0', np.float32),
        ('Ztrack0', np.float32),
        ('Xtrack1', np.float32),
        ('Ytrack1', np.float32),
        ('Ztrack1', np.float32),
    ])
    #print(filename)
    try:
        file1 = open(filename, "rb")
    except IOError:
        raise Exception('Fail to open file')

    # the coordinates will be added for every particle generation seperately
    x_list, y_list, z_list = [[]], [[]], [[]]
    part_gen = 10
    part_genlist = [part_gen]
    no_tracks, no_events, no_figs = 0, 0, 0
    resAZ, totAZ, totAZ_count = [], [], []
    event_file = open(event_filename,'wb')
    heavy_element = False

     # the first line has different bytes than the rest
    lines = file1.read(4)
    header = np.fromfile(file1,dtype='a28',count=1)[0]
    print(header.decode())

    while lines:
        lines = file1.read(8)
        data = np.fromfile(file1,dtype=dataType,count=1)
        # note: a list in a list is return, hence the [0]
        new_gen, x0, y0, z0, x1, y1, z1 = data[0]

        # for every event we create coordinate lists and plot in seperate figures
        # plot the event and empty all lists to follow the track of a new muon
        if new_gen == 0:
            no_events += 1

            # return only figures that contain events with many generations
            if heavy_element == True:
                no_figs += 1

                # dump the important event data in another file 
                # note resAZ gives for part_gen = -2 and < -7 the A and Z information
                data_lists = part_genlist, no_events, x_list, y_list, z_list, resAZ
                pickle.dump(data_lists, event_file)

                heavy_element = False
                resAZ = []
                    
            x_list, y_list, z_list = [[]], [[]], [[]]
            part_gen, part_genlist = 10, [10]
            no_tracks = 0
            
            # continue to the next line because the current is empty
            lines = file1.read(8)
            data = np.fromfile(file1,dtype=dataType,count=1)

            # the end of the file is reached, after the last event
            if len(data) < 1:
                break
            new_gen, x0, y0, z0, x1, y1, z1 = data[0]
   
        # if a new particle is found, track the coordinates in a new list
        elif new_gen != part_gen:
            no_tracks += 1

            if part_gen < -6:
                resAZ.append([part_gen, mass_num, charge])

                # every time a new isotope is created, add and start the count
                # the third place in the list is the count of isotopes [A,Z,count]
                isotope = [mass_num, charge]
                if isotope not in totAZ:
                    totAZ.append(isotope)
                    totAZ_count.append([mass_num, charge, 1])
                else:
                    ind = totAZ.index(isotope)
                    totAZ_count[ind][-1] += 1
                    
                heavy_element = True

            elif part_gen == -2:
                resAZ.append([part_gen, mass_num, charge])
                isotope = [mass_num, charge]
                if isotope not in totAZ:
                    totAZ.append(isotope)
                    totAZ_count.append([mass_num, charge, 1])
                else:
                    ind = totAZ.index(isotope)
                    totAZ_count[ind][-1] += 1
                heavy_element = True
                
            part_gen = new_gen
            part_genlist.append(part_gen)

            # create a new list for the next particle to plot seperately
            # we fill the list with random numbers, so we can exchange that for empty lists
            x_list.append(0)
            y_list.append(0)
            z_list.append(0)
            x_list[no_tracks], y_list[no_tracks], z_list[no_tracks] = [], [], []
                        
        # for a new particle generation the next list is filled
        x_list[no_tracks].extend((x0,x1))
        y_list[no_tracks].extend((y0,y1))
        z_list[no_tracks].extend((z0,z1))

        # in the case we encounter an unkown resid AZ, safe the data
        if new_gen < -6:
            lines = file1.read(8)
            data = np.fromfile(file1,dtype=np.int32,count=7)
            mass_num, charge = data[0], data[1]
        
        elif new_gen == -2:
            lines = file1.read(8)
            data = np.fromfile(file1,dtype=np.int32,count=7)
            mass_num, charge = data[0], data[1]

    # in the case that no heavy elements are created 
    if no_figs == 0:
        pickle.dump(no_figs,event_file)

    event_file.close()
    file1.close()
    print("For {0} events the elements are given by {1} ".format(no_figs, totAZ_count))
    return no_figs, resAZ

# when calling this file use the following format in terminal:
# python vers2_eventscreator.py filename event_filename
file_name, event_filename = sys.argv[1], sys.argv[2]
events_creator(file_name, event_filename)


