dir1 = getDirectory("Segmented");
dir2 = getDirectory("Original");
dir3 = getDirectory("Save");

	filelist = getFileList(dir1);
	segmentedNames = newArray(filelist.length);
	for (j=0; j< filelist.length; j++)
	{
		open(dir1 + filelist[j]);
		id = getTitle();
		segmentedNames[j] = id;
		close("*"); 
	}


	filelist2 = getFileList(dir2);
	originalNames = newArray(filelist2.length);
	for (h=0; h < filelist2.length; h++)
	{
		open(dir2 + filelist2[h]);
		id = getTitle();
		originalNames[h] = id; 
		close("*");
	}


for (i=0; i<filelist.length; i++) 
	{ 
	open(dir1 + segmentedNames[i]);
	chump = getTitle();
	print("imageName:" + chump);
	selectWindow(chump);
	if 
			(endsWith(chump, "b.tif")) 
		    		{
					run("Set Scale...", "distance=190 known=200 pixel=1 unit=nm");
					}
			else if 
				(endsWith(chump, "c.tif")) 
					{
					run("Set Scale...", "distance=205 known=100 pixel=1 unit=nm");
					}
			else if 
				(endsWith(chump, "z.tif")) 
					{
					run("Set Scale...", "distance=132 known=200 pixel=1 unit=nm");
					}
							else 
								{
								print ("ERROR ERROR ERROR")
								}
	run("Set Measurements...", "area mean standard modal min perimeter shape feret's display redirect=None decimal=3");
	// run("Invert"); // For some reason, I had to add this line of code for one of the iterations to correct for a bug.
	run("Analyze Particles...", "size=100-Infinity show=[Overlay Masks] add");
	close("*");
	for(h=0; h<roiManager("Count"); h++)
			{
			roiManager("Select", h);
			roiManager("Rename", "image#:" + (i+1) + "isolatedtubule"+(h+1));
			}
	open(dir2 + originalNames[i]);
	zoop = getTitle();
	print("imageName:" + zoop);
	selectWindow(zoop);
		if 
			(endsWith(zoop, "b.tif")) 
		    		{
					run("Set Scale...", "distance=190 known=200 pixel=1 unit=nm");
					}
			else if 
				(endsWith(zoop, "c.tif")) 
					{
					run("Set Scale...", "distance=205 known=100 pixel=1 unit=nm");
					}
			else if 
				(endsWith(zoop, "z.tif")) 
					{
					run("Set Scale...", "distance=132 known=200 pixel=1 unit=nm");
					}
							else 
								{
								print ("ERROR ERROR ERROR")
								}
	roiManager("Show All");
	roiManager("deselect");
	roiManager("Measure");
	// If you want to see what the overlap is uncomment the next line
		//waitForUser("close all?");
	run("Flatten");
	alpha = getTitle();
	saveAs("tiff", dir3 + alpha);
	roiManager("reset");
	close("*");
	}


selectWindow("Results"); 
saveAs("Measurements", dir3 + "forNormalizationProgram.csv"); 


waitForUser("This will close all of the open images");
	run("Close All");
waitForUser("This will close the Results Window");
	selectWindow("Results");
	run("Close");
waitForUser("This will close ROI Manager")
	selectWindow("ROI Manager");
	run("Close")


