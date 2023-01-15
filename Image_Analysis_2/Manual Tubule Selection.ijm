directory = getDirectory("raw data");
output = getDirectory("output");
list = getFileList(directory); 

run("Set Measurements...", "area mean standard modal min perimeter shape feret's redirect=None decimal=3");


for (h=0; h<list.length; h++) {
	name = list[h];
	open(directory + name);

// Setting the Scale 

chump = getTitle();
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
								print ("ERROR ERROR ERROR");
								}


// Setting the Scale Completed


	run("Select All");
	//setTool("freehand");
	run("ROI Manager...");
	roiManager("Show All");
	roiManager("Show All with labels");
	waitForUser("Manual Selection", "Everything is Selected?");
	roiManager("measure");
	for(i=0; i<roiManager("Count"); i++)
			{
			roiManager("Select", i);
			roiManager("Rename", chump + ":isolatedtubule"+(i+1));
			}
	selectWindow(chump);
	run("Flatten");
	saveAs("tiff", output + "Outlinesof" + name);
	close("*");
	roiManager("reset");
}


selectWindow("Results"); 
saveAs("Measurements", output + "Measurements.csv"); 
selectWindow("Results");
run("Close");
selectWindow("ROI Manager");
run("Close")




 

  




