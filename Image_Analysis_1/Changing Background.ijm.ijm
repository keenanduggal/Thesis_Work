dir1 = getDirectory("Original"); 
list = getFileList(dir1); 
output = getDirectory("output")

for (g=0; g<list.length; g++) { 
	ch1name = list[g];
	open(dir1 + ch1name);
	call("Versatile_Wand_Tool.doWand", 1, 1, 0.0, 0.0, 0.0, "non-contiguous");
	run("Set...", "value=110");
	saveAs("tiff", output + ch1name);
}

waitForUser("close all?");
close("*");