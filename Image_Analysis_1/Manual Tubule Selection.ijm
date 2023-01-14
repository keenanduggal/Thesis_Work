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
		    	(startsWith(chump, "(")) 
		    		{
					run("Set Scale...", "distance=137 known=100 pixel=1 unit=nm");
					}
			else if 
				(startsWith(chump, "A")) 
					{
					run("Set Scale...", "distance=214.67 known=100 pixel=1 unit=nm");
					}
				else if 
					(startsWith(chump, "C")) 
						{
						run("Set Scale...", "distance=162.5008 known=100 pixel=1 unit=nm");
						}
					else if 
						(startsWith(chump, "B")) 
							{
							run("Set Scale...", "distance=107 known=100 pixel=1 unit=nm");
							}
						else if 
							(startsWith(chump, "D")) 
								{
								run("Set Scale...", "distance=401.3333 known=500 pixel=1 unit=nm");
								}
							else {
								print ("ERROR ERROR ERROR")
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
			titler = getTitle();
			roiManager("Rename", titler + ":isolatedtubule"+(i+1));
			}
	selectWindow(titler);
	run("Flatten");
	saveAs("tiff", output + "Outlinesof" + name);
	close("*");
	selectWindow("Results"); 
	saveAs("Measurements", output + name + "-Measurements.csv"); 
	waitForUser("Manual Selection", "Proceed to next image?");
	run("Clear Results");
	roiManager("reset");
}






 

  




