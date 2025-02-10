# version 2 Kelly Weerman
# Read unformatted SRCEFILE containing isotope creation, neutron capture events 
# and muon energy differences. Fill pandasframe with the data, and return
# as an unformatted (pickle) file

import pandas as pd
import numpy as np
import pickle
from operator import itemgetter
import csv
import sys
import time

def pickleLoader(pklFile):
    try:
        while True:
            yield pickle.load(pklFile)
    except EOFError:
        pass

def fill_file(df_frame, new_filename):
    with open(new_filename, "wb") as new_file:
        pickle.dump(df_frame, new_file)


def events_creator(file_name, file_number, new_filename, no_muons, max_radius=900):
    # reads binary SCREFILE created with FLUKA and returns a pandas dataframe
    # stored in an unformatted file
    
    column_names = ['A', 'Z', 'Xcoord', 'Ycoord', 'Zcoord', 'Count', 'Eventnumber',\
                     'Ediff', 'Parent', 'r']
    all_data, events_Ediffs = [], {}
    #df = pd.DataFrame(columns=column_names, dtype='object')
    
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
    ])

    dataTypeEmuon = np.dtype([
        ('Xtrack', np.float64),
        ('Ytrack', np.float64),
        ('Ztrack', np.float64),
        ('Etrack', np.float64),
        ('Ediff', np.float64),
    ])
    
    try:
        file = open(file_name,'rb')
    except IOError:
        raise Exception('Fail to open file')
        
    # the first line has different bytes than the rest
    lines = file.read(4)
    header = np.fromfile(file,dtype='a28',count=1)[0]
    print(header.decode())

    # add unique identifier to eventnumber to distinguish when adding to other files
    file_identifier = file_number * no_muons
    important_event, Ediff_tmp = False, 1.0
    hydrogen_capture, carbon_capture = 0, 0

    # the first muon entry and energy is read here
    lines = file.read(8)
    no_events = np.fromfile(file, dtype=np.int32,count=1)[0] + file_identifier
    xmu, ymu, zmu, muon_Etrack, muon_Ediff = np.fromfile(file,dtype=dataTypeEmuon,count=1)[0]
    r = np.sqrt(xmu**2 + ymu**2 + zmu**2)
    row_data_entrance = (-1, -1, xmu, ymu, zmu, 1, no_events, Ediff_tmp, 0, r)
    
    while lines:
        lines = file.read(8)
        event_num = np.fromfile(file, dtype=np.int32,count=1)[0] + file_identifier
        
        # a new event line is indicated with the following
        # the initial muon energy is returned here as well
        if event_num != no_events:
            
            # the muon exit coordinates and final muon energy (Etrack) and energy difference (Ediff)
            xmu, ymu, zmu, muon_Etrack, muon_Ediff = np.fromfile(file,dtype=dataTypeEmuon,count=1)[0]
            r = np.sqrt(xmu**2 + ymu**2 + zmu**2)
            row_data_exit = (-2, -2, xmu, ymu, zmu, 1, no_events, Ediff_tmp, 0, r)
            events_Ediffs[no_events] = round(muon_Ediff,2)
            
            # we add the initial and final muon coordinates only if there is neutron capture
            # or heavy isotope production
            if important_event == True:
                all_data.append(row_data_entrance)
                all_data.append(row_data_exit)

            important_event = False

            # check the next event, to see if we are at the end of the file
            lines = file.read(8)
            event_num = np.fromfile(file, dtype=np.int32,count=1)
            if len(event_num) == 0:
                break
            
            # the line contains the muon entry coordinates and energy (Ediff = 0.0)
            xmu, ymu, zmu, muon_Etrack, muon_Ediff = np.fromfile(file,dtype=dataTypeEmuon,count=1)[0]
            no_events = event_num[0] + file_identifier
            r = np.sqrt(xmu**2 + ymu**2 + zmu**2)
            row_data_entrance = (-1, -1, xmu, ymu, zmu, 1, no_events, Ediff_tmp, 0, r)
            
            continue
        
        icode, partcode, jtrack = np.fromfile(file,dtype=dataTypeStart,count=1)[0]
        x0, y0, z0 = np.fromfile(file,dtype=dataTypeCoord,count=1)[0]
        r = np.sqrt(x0**2 + y0**2 + z0**2)
        
        # a neutron capture event is indicated and counted with the following
        # the third variable - jtrack - is the hydrogen neutron count in this case
        if icode == 1:
            capture_energy = np.fromfile(file, dtype=np.float64,count=1)[0]
            
            # check if the hydrogen capture is within the defined radius
            if r**2 <= max_radius**2:
                hydrogen_capture += 1
                important_event = True
                row_data_capture = (0, 0, x0, y0, z0, 1, no_events, Ediff_tmp, 80, r) # indicate hydrogen capture with 80
                all_data.append(row_data_capture)
                
            # the next line is read
            continue
            
        A, Z = np.fromfile(file,dtype=dataTypeAZ,count=1)[0]
  
        # now that we have read the complete line, we can add the isotope to the list if
        # it is not a de-excitation (don't count as a new isotope) and its inside our area
        if icode == 106:
            continue  
            
        if r**2 <= max_radius**2:
            
            important_event = True
            isotope = [A, Z]
            
            # check if we have carbon capture by neutrons
            if isotope == [13,6] and jtrack == 8:
                carbon_capture += 1
                row_data_capture = (0, 0, x0, y0, z0, 1, no_events, Ediff_tmp, 81, r)
                all_data.append(row_data_capture)
                
            # ----------
            #rowdata = 'A', 'Z', 'Xcoord', 'Ycoord', 'Zcoord', 'Count', 'Eventnumber','Ediff', 'Parent' 
            row_data = (A, Z, x0, y0, z0, 1, no_events, Ediff_tmp, jtrack, r)
            all_data.append(row_data)
            # ----------
    
    # fill a pandas dataframe and set the muon energy difference per event
    df = pd.DataFrame(all_data, columns=column_names) 
    for event, Ediff in events_Ediffs.items():
        df.loc[df["Eventnumber"] == event, "Ediff"] = Ediff

    print(f"Hydrogen capture count is {hydrogen_capture} and carbon capture count {carbon_capture}")
    file.close()
    fill_file(df, new_filename)
    return df

start_time = time.time()  
#filename = 'muons_KamLAND001_SRCEFILE'
#filenumber = 1
#new_filename = "test"
#no_muons = 10**5 # number of muons per file
filename, filenumber = sys.argv[1], int(sys.argv[2])
new_filename = sys.argv[3]
no_muons = int(sys.argv[4])
max_radius = float(sys.argv[5])

df = events_creator(filename, filenumber, new_filename, no_muons, max_radius)
print(df)

end_time = time.time()  # record end time
print(f"Execution time: {end_time - start_time} seconds")

#0.05sec