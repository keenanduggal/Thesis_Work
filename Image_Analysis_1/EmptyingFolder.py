#Emptys all of the files in a folder
    # You can adjust the parameters (deletes files that end in .csv for example)

import os
folder = '/Users/keenanduggal/Desktop/Processed/Cell_Line_U/'

filelist = os.listdir(folder) 

for f in filelist:
    os.remove(os.path.join(folder, f))

filelist2 = os.listdir(folder)


