import os

reader = open ('/Users/keenanduggal/Desktop/PracticePictureDirectory.txt', 'r')
line = reader.readline()
while line != '':     
       thislist = line.split()
       print(thislist)
       original = thislist[0]
       newname = thislist[1]
       os.rename('/Users/keenanduggal/Desktop/Pictures/%s' % (original), '/Users/keenanduggal/Desktop/Pictures/%s' %  (newname))
       line = reader.readline()  
os.close(reader)
       
       
