directory = getDirectory("Ilastik Segmentation");
output = getDirectory("output");
list = getFileList(directory); 

for (h=0; h<list.length; h++) {
	name = list[h];
	open(directory + name);
	setAutoThreshold("Default dark no-reset");
	setThreshold(0, 1);
	setOption("BlackBackground", true);
	run("Convert to Mask");
	setOption("BlackBackground", true);
	run("Despeckle");
run("Despeckle");
run("Despeckle");
run("Gaussian Blur...", "sigma=20");
setAutoThreshold("Default dark");
run("Threshold...");
setThreshold(71, 255);
run("Convert to Mask");
run("Erode");
run("Erode");
run("Erode");
run("Erode");
run("Erode");
	saveAs("tiff", output + name);
}


waitForUser("close all?");
close("*");
