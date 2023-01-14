directory = getDirectory("thresholded images");
output = getDirectory("output");
list = getFileList(directory); 

for (h=0; h<list.length; h++) {
	name = list[h];
	open(directory + name);
	setAutoThreshold("Default dark no-reset");
	setThreshold(0, 1);
	setOption("BlackBackground", true);
	run("Convert to Mask");
	saveAs("tiff", output + name);
}


waitForUser("close all?");
close("*");
