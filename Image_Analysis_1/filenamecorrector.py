import os
os.chdir('/Users/keenanduggal/Desktop/Cropped_Tubule_Pictures')
for file in os.listdir():
    src = file
    dst = file + ".tif"
    os.rename(src, dst)

