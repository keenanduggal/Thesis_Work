dir1 = getDirectory("Original"); 
list = getFileList(dir1); 
output = getDirectory("output")

for (g=0; g<list.length; g++) { 
	ch1name = list[g];
	open(dir1 + ch1name);
	height = getHeight();
	width = getWidth();
	call("Versatile_Wand_Tool.doWand", 1, 1, 0.0, 0.0, 0.0, "contiguous");
	run("Set...", "value=110");
	call("Versatile_Wand_Tool.doWand", width - 1, height - 1, 0.0, 0.0, 0.0, "contiguous");
	run("Set...", "value=110");
	call("Versatile_Wand_Tool.doWand", 1, height - 1, 0.0, 0.0, 0.0, "contiguous");
	run("Set...", "value=110");
	call("Versatile_Wand_Tool.doWand", width - 1, 1, 0.0, 0.0, 0.0, "contiguous");
	run("Set...", "value=110");
	saveAs("tiff", output + ch1name);
}

waitForUser("close all?");
close("*");