// This program will select the matrix of the pyrenoid (image - cropped background - tubules) and compute the average grey value
// It is designed for the purpose of normalizing intensity measurements 




dir1 = getDirectory("Segmented");
dir2 = getDirectory("Original");
dir3 = getDirectory("Save");
run("Set Measurements...", "mean standard modal min display redirect=None decimal=3");
run("ROI Manager...");

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
	run("Create Selection");
	run("Make Inverse");
	roiManager("Add");
	roiManager("Select", 0);
	roiManager("rename", "pyrenoidArea");
	close();
	open(dir2 + originalNames[i]);
	call("Versatile_Wand_Tool.doWand", 2, 2, 0.0, 0.0, 0.0, "non-contiguous");
	run("Make Inverse");
	roiManager("Add");
	roiManager("Select", 1);
	roiManager("rename", "pyrenoidArea");
	roiManager("Select", newArray(0,1))
	roiManager("AND");
	run("Measure");
	roiManager("reset");
	}


selectWindow("Results"); 
saveAs("Measurements", dir3 + "NormalizationData.txt"); 


waitForUser("close all?");
close("*");





