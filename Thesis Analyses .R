# Setting up Environment: 
library(mgcv)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(stringr)
library(broom)
library(ggrepel)
library(cowplot)
library(tidyr)
library(ggpubr)



########### Image Analysis: 1
machine_learning <- read_excel("~/Desktop/ML_Averages.xlsx")
manual_averages <- read_excel("~/Desktop/Manual_Averages.xlsx")


# Size: 
## Area: 
area <- data.frame(names = machine_learning$`Cell Line`, machine_learning = machine_learning$Area, manual = manual_averages$Area)
stacked_area <- cbind(area[1], stack(area[2:3])) %>% arrange(names)

a <- ggplot(stacked_area, aes(x = names, y = values, fill = ind)) +
    geom_bar(stat = "identity", position = position_dodge(width=0.5)) + 
    scale_fill_manual(values=c("#CC9966", "#663339")) + 
    labs(y = "Area (nm^2)", x = " ", title = "Average Tubule Area (nm^2)")  + 
    theme(axis.text.x = element_text(angle = 60, vjust = 0.6)) 
    

# Percent Area: 
data <- data.frame(names = machine_learning$`Cell Line`, machine_learning = machine_learning$`% Area`, manual = manual_averages$`% Area`)
stacked_data <- cbind(data[1], stack(data[2:3])) %>% arrange(names)

b <- ggplot(stacked_data, aes(x = names, y = values, fill = ind)) +
    geom_bar(stat = "identity", position = position_dodge(width=0.5)) + 
    scale_fill_manual(values=c("#CC9966", "#663339")) + 
    labs(y = "Percentage of Area", x = " ", title = "Percent of Pyrenoid Taken up by Tubules")  + 
    theme(axis.text.x = element_text(angle = 60, vjust = 0.6))

## Perimeter: 
data <- data.frame(names = machine_learning$`Cell Line`, machine_learning = machine_learning$Perim., manual = manual_averages$Perim.)
stacked_data <- cbind(data[1], stack(data[2:3])) %>% arrange(names)

c <- ggplot(stacked_data, aes(x = names, y = values, fill = ind)) +
    geom_bar(stat = "identity", position = position_dodge(width=0.5)) + 
    scale_fill_manual(values=c("#CC9966", "#663339")) + 
    labs(y = "Perimeter (nm)", x = " ", title = "Average Tubule Perimeter")  + 
    theme(axis.text.x = element_text(angle = 60, vjust = 0.6))

## Area / Perimeter: 
data <- data.frame(names = machine_learning$`Cell Line`, machine_learning = machine_learning$`Total Area/Perimeter`)

d <- ggplot(data, aes(x = names, y = machine_learning)) +
    geom_bar(stat = "identity",fill = "#CC9966", position = position_dodge(width=0.5)) + 
    labs(y = "Tubule Area / Perimeter (nm)", x = " ", title = "Total Tubule Area / Perimeter (ML)")  + 
    theme(axis.text.x = element_text(angle = 60, vjust = 0.6))

dev.new()
ggarrange(a, b, c, d, ncol = 2, nrow = 2, labels = c("A", "B", "C", "D"))



# Shape: 
## Roundness: 
data <- data.frame(names = machine_learning$`Cell Line`, machine_learning = machine_learning$Round, manual = manual_averages$Round)
stacked_data <- cbind(data[1], stack(data[2:3])) %>% arrange(names)


a <- ggplot(stacked_data, aes(x = names, y = values, fill = ind)) +
    geom_bar(stat = "identity", position = position_dodge(width=0.5)) + 
    scale_fill_manual(values=c("#CC9966", "#663339")) + 
    labs(y = "Roundness (0-1)", x = " ", title = "Average Tubule Roundness")  + 
    theme(axis.text.x = element_text(angle = 60, vjust = 0.6), 
          legend.position="bottom", legend.title=element_blank(), legend.margin=margin(t = 0, unit='cm'))

## Solidity: 
data <- data.frame(names = machine_learning$`Cell Line`, machine_learning = machine_learning$Solidity, manual = manual_averages$Solidity)
stacked_data <- cbind(data[1], stack(data[2:3])) %>% arrange(names)

b <- ggplot(stacked_data, aes(x = names, y = values, fill = ind)) +
    geom_bar(stat = "identity", position = position_dodge(width=0.5)) + 
    scale_fill_manual(values=c("#CC9966", "#663339")) + 
    labs(y = "Solidity (0-1)", x = " ", title = "Average Tubule Solidity")  + 
    theme(axis.text.x = element_text(angle = 60, vjust = 0.6), 
          legend.position="bottom", legend.title=element_blank(), legend.margin=margin(t = 0, unit='cm'))

## Circularity: 
data <- data.frame(names = machine_learning$`Cell Line`, machine_learning = machine_learning$Circ., manual = manual_averages$Circ.)
stacked_data <- cbind(data[1], stack(data[2:3])) %>% arrange(names)

c <- ggplot(stacked_data, aes(x = names, y = values, fill = ind)) +
    geom_bar(stat = "identity", position = position_dodge(width=0.5)) + 
    scale_fill_manual(values=c("#CC9966", "#663339")) + 
    labs(y = "Circularity (0-1)", x = " ", title = "Average Tubule Circularity")  + 
    theme(axis.text.x = element_text(angle = 60, vjust = 0.6), 
          legend.position="bottom", legend.title=element_blank(), legend.margin=margin(t = 0, unit='cm'))

dev.new()
ggarrange(a, b, c, ncol = 3, nrow = 1, labels = c("A", "B", "C"))


# Pixel Intensity: 
## Mean: 
data <- data.frame(names = machine_learning$`Cell Line`, machine_learning = machine_learning$Mean)
a <- ggplot(data, aes(x = names, y = machine_learning)) +
    geom_bar(stat = "identity", fill = "#CC9966", position = position_dodge(width=0.5)) + 
    labs(y = "Mean Normalized Pixel Intensity", x = " ", title = "Mean Tubule Fullness")  + 
    theme(axis.text.x = element_text(angle = 60, vjust = 0.6))

## Min: 
data <- data.frame(names = machine_learning$`Cell Line`, machine_learning = machine_learning$Minimum)

b <- ggplot(data, aes(x = names, y = machine_learning)) +
    geom_bar(stat = "identity", fill = "#CC9966", position = position_dodge(width=0.5)) + 
    labs(y = "Min Normalized Pixel Intensity", x = " ", title = "Minimum Tubule Fullness")  + 
    theme(axis.text.x = element_text(angle = 60, vjust = 0.6))

## Max: 
data <- data.frame(names = machine_learning$`Cell Line`, machine_learning = machine_learning$Max)

c <- ggplot(data, aes(x = names, y = machine_learning)) +
    geom_bar(stat = "identity",fill = "#CC9966", position = position_dodge(width=0.5)) + 
    labs(y = "Max Normalized Pixel Intensity", x = " ", title = "Maximum Tubule Fullness")  + 
    theme(axis.text.x = element_text(angle = 60, vjust = 0.6))

dev.new()
ggarrange(a, b, c, ncol = 3, nrow = 1, labels = c("A", "B", "C"))



########### Image Analysis: 2

cmj030 <- read.csv("~/Desktop/Image_Analysis/CMJ030_output/Measurements.csv")
cmj030$adivp <- cmj030$Area / cmj030$Perim.
ptn1 <- read.csv("~/Desktop/Image_Analysis/W_output/Measurements.csv")
ptn1$adivp <- ptn1$Area / ptn1$Perim.
ptn2 <- read.csv("~/Desktop/Image_Analysis/Y1_output/Measurements.csv")
ptn2$adivp <- ptn2$Area / ptn2$Perim.
ptn3 <- read.csv("~/Desktop/Image_Analysis/Z_output/Measurements.csv")
ptn3$adivp <- ptn3$Area / ptn3$Perim.




#### Size: 
area_df <- data.frame(names = c('CMJ030', 'PTN1', 'PTN2', 'PTN3'), 
                      values = c(mean(cmj030$Area), mean(ptn1$Area), mean(ptn2$Area), mean(ptn3$Area)))

t.test(cmj030$Area, ptn1$Area, alternative = "two.sided")
t.test(cmj030$Area, ptn2$Area, alternative = "two.sided")
t.test(cmj030$Area, ptn3$Area, alternative = "two.sided")  ### Significant!

a <- ggplot(area_df, aes(x = names, y = values)) +
    geom_bar(stat = "identity") + 
    labs(y = "Area (nm^2)", x = " ", title = "Average Tubule Area (nm^2)")  + 
    theme(axis.text.x = element_text(angle = 60, vjust = 0.6)) +
    geom_signif(comparisons=list(c("CMJ030", "PTN3")), annotations="**",
                y_position = 80000, tip_length = 0, vjust=0.4)  


perim_df <- data.frame(names = c('CMJ030', 'PTN1', 'PTN2', 'PTN3'), 
                      values = c(mean(cmj030$Perim.), mean(ptn1$Perim.), mean(ptn2$Perim.), mean(ptn3$Perim.)))
t.test(cmj030$Perim., ptn1$Perim., alternative = "two.sided")
t.test(cmj030$Perim., ptn2$Perim., alternative = "two.sided")
t.test(cmj030$Perim., ptn3$Perim., alternative = "two.sided")

b <- ggplot(perim_df, aes(x = names, y = values)) +
    geom_bar(stat = "identity") + 
    labs(y = "Perimeter (nm)", x = " ", title = "Average Tubule Perimeter (nm)")  + 
    theme(axis.text.x = element_text(angle = 60, vjust = 0.6)) 


area_dvd_perim_df <- data.frame(names = c('CMJ030', 'PTN1', 'PTN2', 'PTN3'), 
                                values = c(mean(cmj030$adivp), mean(ptn1$adivp), mean(ptn2$adivp), mean(ptn3$adivp)))

t.test(cmj030$adivp, ptn1$adivp, alternative = "two.sided")
t.test(cmj030$adivp, ptn2$adivp, alternative = "two.sided")
t.test(cmj030$adivp, ptn3$adivp, alternative = "two.sided")

c <- ggplot(area_dvd_perim_df, aes(x = names, y = values)) +
    geom_bar(stat = "identity") + 
    labs(y = "Area / Perimeter (nm)", x = " ", title = "Average Tubule Area/Perimeter Ratio")  + 
    theme(axis.text.x = element_text(angle = 60, vjust = 0.6)) +
    geom_signif(comparisons=list(c("CMJ030", "PTN2")), annotations="***",
                y_position = 38, tip_length = 0, vjust=0.4)  +
    geom_signif(comparisons=list(c("CMJ030", "PTN3")), annotations="***",
                y_position = 36, tip_length = 0, vjust=0.4)  

dev.new()
ggarrange(a, b, c, ncol = 3, nrow = 1, labels = c("A", "B", "C"))



##### Shape: 

round_df <- data.frame(names = c('CMJ030', 'PTN1', 'PTN2', 'PTN3'), 
                      values = c(mean(cmj030$Round), mean(ptn1$Round), mean(ptn2$Round), mean(ptn3$Round)))

t.test(cmj030$Round, ptn1$Round, alternative = "two.sided")
t.test(cmj030$Round, ptn2$Round, alternative = "two.sided")
t.test(cmj030$Round, ptn3$Round, alternative = "two.sided") 

a <- ggplot(round_df, aes(x = names, y = values)) +
    geom_bar(stat = "identity") + 
    labs(y = "Roundness (0-1)", x = " ", title = "Average Tubule Roundness")  + 
    theme(axis.text.x = element_text(angle = 60, vjust = 0.6)) +
    geom_signif(comparisons=list(c("CMJ030", "PTN3")), annotations="*",
                y_position = 0.75, tip_length = 0, vjust=0.4)  


solid_df <- data.frame(names = c('CMJ030', 'PTN1', 'PTN2', 'PTN3'), 
                       values = c(mean(cmj030$Solidity), mean(ptn1$Solidity), mean(ptn2$Solidity), mean(ptn3$Solidity)))
t.test(cmj030$Solidity, ptn1$Solidity, alternative = "two.sided")
t.test(cmj030$Solidity, ptn2$Solidity, alternative = "two.sided")
t.test(cmj030$Solidity, ptn3$Solidity, alternative = "two.sided")

b <- ggplot(solid_df, aes(x = names, y = values)) +
    geom_bar(stat = "identity") + 
    labs(y = "Solidity (0-1)", x = " ", title = "Average Tubule Solidity")  + 
    theme(axis.text.x = element_text(angle = 60, vjust = 0.6)) 


circ_df <- data.frame(names = c('CMJ030', 'PTN1', 'PTN2', 'PTN3'), 
                                values = c(mean(cmj030$Circ.), mean(ptn1$Circ.), mean(ptn2$Circ.), mean(ptn3$Circ.)))

t.test(cmj030$Circ., ptn1$Circ., alternative = "two.sided")
t.test(cmj030$Circ., ptn2$Circ., alternative = "two.sided")
t.test(cmj030$Circ., ptn3$Circ., alternative = "two.sided")

c <- ggplot(circ_df, aes(x = names, y = values)) +
    geom_bar(stat = "identity") + 
    labs(y = "Circularity (0-1)", x = " ", title = "Average Tubule Circularity")  + 
    theme(axis.text.x = element_text(angle = 60, vjust = 0.6)) 

dev.new()
ggarrange(a, b, c, ncol = 3, nrow = 1, labels = c("A", "B", "C"))





############ Pulldown Optimization: 

# 06/09 trial: 
df <- data.frame(chlorophyll = c(7197,	5487,	5142,	8, 41,	213), 
                 venus = c(194,	623,	632,	135,	1730,	1173), 
                 labels = c('CMJ030 Lysate', 'RBMP1 Lysate', 'Cre01 Lysate', 'Anti-Flag Beads', 'RBMP2 + Beads', 'Cre05 + Beads'))
df %>% slice(4, 1, 2, 5, 3, 6)
stacked_df <- cbind(df[3], stack(df[1:2])) 
ggplot(stacked_df, aes(x = labels, y = values, fill = ind)) +
    geom_bar(stat = "identity", position = position_dodge(width=0.5)) + 
    labs(y = "Spectrofluorometer Intensity", x = " ", title = "Spectrofluorometer Proof Of Concept")  + 
    theme(axis.text.x = element_text(angle = 60, vjust = 0.6))





# Data comes from LANEC_Trial: (Diagnostic SF 1) 
df <- data.frame(chlorophyll = c(1,0,5360, 4908, 10, 23, 17, 76, 11, 3), 
                                 venus = c(153, 46, 815, 1802, 158, 396, 94, 357, 156, 207), 
                 labels = c("Blank", "Beads", "Lysate", "Lysate", "Pre elution", 
                           "Pre-elution", "Post elution", "Post elution",  
                            "Eluate", "Eluate"))

df_cmj030 <- df %>% slice(3,5,9)
df_cmj030$chlorophyll <- df_cmj030$chlorophyll - df$chlorophyll[1]
df_cmj030$venus <- df_cmj030$venus - df$venus[1]

df_cre05 <- df %>% slice(4,6,10)
df_cre05$chlorophyll <- df_cre05$chlorophyll - df$chlorophyll[1]
df_cre05$venus <- df_cre05$venus - df$venus[1]
             

a <- ggplot(df_cmj030, aes(x = 1:length(chlorophyll))) + 
        geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
        geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
        theme(axis.ticks.x=element_blank())  + 
        theme(axis.title.x=element_blank())  + 
        theme(axis.text.x=element_blank())+ 
        geom_label_repel(aes(label = labels, 
                             y = log(chlorophyll), 
                             size = NULL, color = NULL), nudge_y = 0.25)  + 
        labs(y = "Log of Chlorophyll and Venus Expression", title = "CMJ030")
   



z <- ggplot(df_cre05, aes(x = 1:length(chlorophyll))) + 
        geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
        geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
        theme(axis.ticks.x=element_blank())  + 
        theme(axis.title.x=element_blank())  + 
        theme(axis.text.x=element_blank())+ 
        geom_label_repel(aes(label = labels, 
                             y = log(chlorophyll), 
                             size = NULL, color = NULL), nudge_y = 0.25) +  
        labs(y = "Log of Chlorophyll and Venus Expression", title = "Cre05")

dev.new()
plot_grid(a, z, labels = "AUTO")



# Data comes from kd628: (Normal trial of full procedure for three different samples)
df <- data.frame(chlorophyll = c(1, 1, 7246, 4575, 2089, 6688, 5463, 4137, 53, 96, 13, 163, 106, 38, 51, 101, 3), 
                 venus = c(161, 45, 653, 651, 880, 641, 951, 2438, 99, 1935, 232, 89, 1134, 112, 103, 8199, 118), 
                 labels = c('1x Buffer', 'Anti-FLAG Beads', 'Lysate (pre-centrifugation)', 
                'Lysate (pre-centrifugation)', 'Lysate (pre-centrifugation)', 
                'Lysate (post-centrifugation)', 	'Lysate (post-centrifugation)', 	
                'Lysate (post-centrifugation)', 	'Pre-elution beads',	
                'Pre-elution beads',	'Pre-elution beads',	'Post-elution beads	',
                'Post-elution beads',	'Post-elution beads',	'Eluate',	
                'Eluate','Eluate'))

df_cmj030 <- df %>% slice(3,6,9,15)
df_cmj030$chlorophyll <- df_cmj030$chlorophyll - df$chlorophyll[1]
df_cmj030$venus <- df_cmj030$venus - df$venus[1]
df_cmj030 <- df_cmj030 %>% mutate(venus = if_else(venus < 0, 0, venus)) %>% mutate(chlorophyll = if_else(chlorophyll < 0, 0, chlorophyll))

df_cre01 <- df %>% slice(4,7,10,16)
df_cre01$chlorophyll <- df_cre01$chlorophyll - df$chlorophyll[1]
df_cre01$venus <- df_cre01$venus - df$venus[1]
df_cre01 <- df_cre01 %>% mutate(venus = if_else(venus < 0, 0, venus)) %>% mutate(chlorophyll = if_else(chlorophyll < 0, 0, chlorophyll))

df_rbmp1 <- df %>% slice(5,8,11,17)
df_rbmp1$chlorophyll <- df_rbmp1$chlorophyll - df$chlorophyll[1]
df_rbmp1$venus <- df_rbmp1$venus - df$venus[1]
df_rbmp1 <- df_rbmp1 %>% mutate(venus = if_else(venus < 0, 0, venus)) %>% mutate(chlorophyll = if_else(chlorophyll < 0, 0, chlorophyll))


a <- ggplot(df_cmj030, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    geom_label_repel(aes(label = labels, 
                         y = log(chlorophyll), 
                         size = NULL, color = NULL), nudge_y = 0.25)  + 
    labs(y = "Log of Chlorophyll and Venus Expression", title = "CMJ030")




b <- ggplot(df_cre01, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    geom_label_repel(aes(label = labels, 
                         y = log(chlorophyll), 
                         size = NULL, color = NULL), nudge_y = 0.25) +  
    labs(y = "Log of Chlorophyll and Venus Expression", title = "Cre01")


c <- ggplot(df_rbmp1, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    geom_label_repel(aes(label = labels, 
                         y = log(chlorophyll), 
                         size = NULL, color = NULL), nudge_y = 0.25) +  
    labs(y = "Log of Chlorophyll and Venus Expression", title = "RBMP1")

dev.new()
ggarrange(a, b, c, z, ncol = 2, nrow = 2, labels = c("A", "B", "C", "D"))




# Data comes from 0705: Sonication Optimization 
df <- data.frame(chlorophyll = c(1,	0,	4461,	7100,	4550,	4665,	4823,	6479,	7780,	7864,	7747,	7954,	5317,	5443,	5834,	7891,	7726,	7750,	131,	117,	86,	50,	63,	35,	21,	35,	63,	91,	71,	39,	26,	27,	14,	17,	8,	6,	3,	7,	7,	5,	4,	3),
                 venus = c(189,	57,	2970,	979,	1906,	2011,	1973,	3996,	779,	843,	784,	994,	2412,	2802,	2613,	848,	963,	825	,1338,	1523,	1403,	839,	138,	137,	168,	140,	712,	1007,	684,	359,	138,	148,	166,	139,	624,	638,	461,	430,	179,	196,	189,	184), 
                 labels = c('Buffer',	'Beads',	'Cre01/5 Lysate Pre-sonication',	
                           ' CMJ030 Lysate Pre-sonication',	'Cre01/5 Post-sonication #1',	
                           ' Cre01/5 Post-sonication #2',	'Cre01/5 Post-sonication #3',	
                           ' Cre01/5 Post-sonication #4','	CMJ030 Post-sonication #1',	
                           ' CMJ030 Post-sonication #2',	'CMJ030 Post-sonication #3',	
                           ' CMJ030 Post-sonication #4',	'Cre01/5 Post-centrifugation #1',	
                           ' Cre01/5 Post-centrifugation #2',	'Cre01/5 Post-centrifugation #3',	
                           ' CMJ030 Post-centrifugation #1',	'CMJ030 Post-centrifugation #2',
                            'CMJ030 Post-centrifugation #3',	'Cre01/5 Pre-elution Beads #1',	
                            'Cre01/5 Pre-elution Beads #2',	'Cre01/5 Pre-elution Beads #3',	
                           ' Cre01/5 Pre-elution Beads #4',	'CMJ030 Pre-elution Beads #1	',
                            'CMJ030 Pre-elution Beads #2',	'CMJ030 Pre-elution Beads #3	',
                            'CMJ030 Pre-elution Beads #4',	'Cre01/5 Post-elution Beads #1',	
                            'Cre01/5 Post-elution Beads #2',	'Cre01/5 Post-elution Beads #3',	
                            'Cre01/5 Post-elution Beads #4',	'CMJ030 Post-elution Beads #1',	
                            'CMJ030 Post-elution Beads #2',	'CMJ030 Post-elution Beads #3',	
                          '  CMJ030 Post-elution Beads #4',	'Cre01/5 Eluate #1',	'Cre01/5 Eluate #2',	
                            'Cre01/5 Eluate #3',	'Cre01/5 Eluate #4',	'CMJ030 Eluate #1',	
                            'CMJ030 Eluate #2','	CMJ030 Eluate #3',	'CMJ030 Eluate #4'))

# Sonication 1: 
df_cmj030 <- df %>% slice(4, 9, 16, 23, 39)
df_cmj030$chlorophyll <- df_cmj030$chlorophyll - df$chlorophyll[1]
df_cmj030$venus <- df_cmj030$venus - df$venus[1]
df_cmj030 <- df_cmj030 %>% mutate(venus = if_else(venus < 0, 0, venus)) %>% mutate(chlorophyll = if_else(chlorophyll < 0, 0, chlorophyll))


df_cre01 <- df %>% slice(3, 5, 13, 19, 35)
df_cre01$chlorophyll <- df_cre01$chlorophyll - df$chlorophyll[1]
df_cre01$venus <- df_cre01$venus - df$venus[1]
df_cre01 <- df_cre01 %>% mutate(venus = if_else(venus < 0, 0, venus)) %>% mutate(chlorophyll = if_else(chlorophyll < 0, 0, chlorophyll))


a <- ggplot(df_cmj030, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    # geom_label_repel(aes(label = labels, y = log(chlorophyll), size = NULL, color = NULL), nudge_y = 0.25) +  
    labs(y = "Log(Expression)", title = "Sonication 1: CMJ030")




b <- ggplot(df_cre01, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    # geom_label_repel(aes(label = labels, y = log(chlorophyll), size = NULL, color = NULL), nudge_y = 0.25) +  
    labs(y = "Log(Expression)", title = "Sonication 1: Cre01")


# Sonication 2: 
df_cmj030 <- df %>% slice(4, 10, 17, 24, 40)
df_cmj030$chlorophyll <- df_cmj030$chlorophyll - df$chlorophyll[1]
df_cmj030$venus <- df_cmj030$venus - df$venus[1]
df_cmj030 <- df_cmj030 %>% mutate(venus = if_else(venus < 0, 0, venus)) %>% mutate(chlorophyll = if_else(chlorophyll < 0, 0, chlorophyll))


df_cre01 <- df %>% slice(3, 6, 14, 20, 36)
df_cre01$chlorophyll <- df_cre01$chlorophyll - df$chlorophyll[1]
df_cre01$venus <- df_cre01$venus - df$venus[1]
df_cre01 <- df_cre01 %>% mutate(venus = if_else(venus < 0, 0, venus)) %>% mutate(chlorophyll = if_else(chlorophyll < 0, 0, chlorophyll))


c <- ggplot(df_cmj030, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    # geom_label_repel(aes(label = labels, y = log(chlorophyll), size = NULL, color = NULL), nudge_y = 0.25) +  
    labs(y = "Log(Expression)", title = "Sonication 2: CMJ030")




d <- ggplot(df_cre01, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    # geom_label_repel(aes(label = labels, y = log(chlorophyll), size = NULL, color = NULL), nudge_y = 0.25) +  
    labs(y = "Log(Expression)", title = "Sonication 2: Cre01")


# Sonication 3: 
df_cmj030 <- df %>% slice(4, 11, 18, 25, 41)
df_cmj030$chlorophyll <- df_cmj030$chlorophyll - df$chlorophyll[1]
df_cmj030$venus <- df_cmj030$venus - df$venus[1]
df_cmj030 <- df_cmj030 %>% mutate(venus = if_else(venus < 0, 0, venus)) %>% mutate(chlorophyll = if_else(chlorophyll < 0, 0, chlorophyll))


df_cre01 <- df %>% slice(3, 7, 15, 21, 37)
df_cre01$chlorophyll <- df_cre01$chlorophyll - df$chlorophyll[1]
df_cre01$venus <- df_cre01$venus - df$venus[1]
df_cre01 <- df_cre01 %>% mutate(venus = if_else(venus < 0, 0, venus)) %>% mutate(chlorophyll = if_else(chlorophyll < 0, 0, chlorophyll))


e <- ggplot(df_cmj030, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    # geom_label_repel(aes(label = labels, y = log(chlorophyll), size = NULL, color = NULL), nudge_y = 0.25) +  
    labs(y = "Log(Expression)", title = "Sonication 3: CMJ030")


f <- ggplot(df_cre01, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    # geom_label_repel(aes(label = labels, y = log(chlorophyll), size = NULL, color = NULL), nudge_y = 0.25) +  
    labs(y = "Log(Expression)", title = "Sonication 3: Cre01")

# Sonication 4: 
df_cmj030 <- df %>% slice(4, 12, 26, 42)
df_cmj030$chlorophyll <- df_cmj030$chlorophyll - df$chlorophyll[1]
df_cmj030$venus <- df_cmj030$venus - df$venus[1]
df_cmj030 <- df_cmj030 %>% mutate(venus = if_else(venus < 0, 0, venus)) %>% mutate(chlorophyll = if_else(chlorophyll < 0, 0, chlorophyll))


df_cre01 <- df %>% slice(3, 8, 22, 38)
df_cre01$chlorophyll <- df_cre01$chlorophyll - df$chlorophyll[1]
df_cre01$venus <- df_cre01$venus - df$venus[1]
df_cre01 <- df_cre01 %>% mutate(venus = if_else(venus < 0, 0, venus)) %>% mutate(chlorophyll = if_else(chlorophyll < 0, 0, chlorophyll))


g <- ggplot(df_cmj030, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    # geom_label_repel(aes(label = labels, y = log(chlorophyll), size = NULL, color = NULL), nudge_y = 0.25) +  
    labs(y = "Log(Expression)", title = "Sonication 4: CMJ030")


h <- ggplot(df_cre01, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    # geom_label_repel(aes(label = labels, y = log(chlorophyll), size = NULL, color = NULL), nudge_y = 0.25) +  
    labs(y = "Log(Expression)", title = "Sonication 4: Cre01")


dev.new()
ggarrange(a, b, c, d, e, f, g, h, ncol = 4, nrow = 2, labels = c("A", "B", "C", "D", "E", "F", "G", "H"))




## Sonication Optimization Trial 2 (From son2 data) 

df <- data.frame(chlorophyll = c(6594,	5692,	5032,	8142,	22,	4,	7,	33,	14,	47,	45,	18, 9,	7,	1,	3,	10,	4,	11,	17,	6,	3,	13,	8, 5,	27,	16,	30,	41,	15,	6,	1,	1), 
                 venus = c(1025,	1353,	1475,	1107,	165,	1498,	702,	164,	912,	2110,	131,	1315, 484,	206,	1770,	517,	191,	804,	1271,	186,	927,	306,	171,	900, 423,	199,	570,	935,	171,	677,	402,	181	,193), 
                 labels = c('CMJ030 Lysate', "Cre01 Lysate", "RBMP1 Lysate", "Cre01 Post-Sonication", 
                            "CMJ030 Pre-elution Beads 1", "Cre01 Pre-elution Beads 1", "RBMP1 Pre-elution Beads 1", 
                            "CMJ030 Pre-elution Beads 2", "Cre01 Pre-elution Beads 2", "RBMP1 Pre-elution Beads 2",   
                            "CMJ030 Pre-elution Beads 3", "Cre01 Pre-elution Beads 3", "RBMP1 Pre-elution Beads 3",  
                            "CMJ030 Eluate 1", "Cre05 Eluate 1", "RBMP1 Eluate 1", 
                            "CMJ030 Eluate 2", "Cre05 Eluate 2", "RBMP1 Eluate 2",
                            "CMJ030 Eluate 3", "Cre05 Eluate 3", "RBMP1 Eluate 3",
                            "CMJ030 Post Elution Beads 1", "Cre05 Post Elution Beads 1", "RBMP1 Post Elution Beads 1", 
                            "CMJ030 Post Elution Beads 2", "Cre05 Post Elution Beads 2", "RBMP1 Post Elution Beads 2",
                            "CMJ030 Post Elution Beads 3", "Cre05 Post Elution Beads 3", "RBMP1 Post Elution Beads 3", 
                            "Buffer", "Flag Elution"))



# Sonication trial 1 
df_cmj030 <- df %>% slice(1, 5, 14)
df_cmj030$chlorophyll <- df_cmj030$chlorophyll - df$chlorophyll[32]
df_cmj030$venus <- df_cmj030$venus - df$venus[32]
df_cmj030 <- df_cmj030 %>% mutate(venus = if_else(venus < 0, 0, venus)) %>% mutate(chlorophyll = if_else(chlorophyll < 0, 0, chlorophyll))

df_cre01 <- df %>% slice(2, 6, 15)
df_cre01$chlorophyll <- df_cre01$chlorophyll - df$chlorophyll[32]
df_cre01$venus <- df_cre01$venus - df$venus[32]
df_cre01 <- df_cre01 %>% mutate(venus = if_else(venus < 0, 0, venus)) %>% mutate(chlorophyll = if_else(chlorophyll < 0, 0, chlorophyll))

df_rbmp1 <- df %>% slice(3,7,16)
df_rbmp1$chlorophyll <- df_rbmp1$chlorophyll - df$chlorophyll[32]
df_rbmp1$venus <- df_rbmp1$venus - df$venus[32]
df_rbmp1 <- df_rbmp1 %>% mutate(venus = if_else(venus < 0, 0, venus)) %>% mutate(chlorophyll = if_else(chlorophyll < 0, 0, chlorophyll))

a <- ggplot(df_cmj030, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    # geom_label_repel(aes(label = labels, y = log(chlorophyll), size = NULL, color = NULL), nudge_y = 0.25) +  
    labs(y = "Log(Expression)", title = "Sonication 5: CMJ030")


b <- ggplot(df_cre01, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    # geom_label_repel(aes(label = labels, y = log(chlorophyll), size = NULL, color = NULL), nudge_y = 0.25) +  
    labs(y = "Log(Expression)", title = "Sonication 5: Cre01")

c <- ggplot(df_rbmp1, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    # geom_label_repel(aes(label = labels, y = log(chlorophyll), size = NULL, color = NULL), nudge_y = 0.25) +  
    labs(y = "Log(Expression)", title = "Sonication 5: RBMP1")


# Sonication trial 2
df_cmj030 <- df %>% slice(1, 8, 17)
df_cmj030$chlorophyll <- df_cmj030$chlorophyll - df$chlorophyll[32]
df_cmj030$venus <- df_cmj030$venus - df$venus[32]
df_cmj030 <- df_cmj030 %>% mutate(venus = if_else(venus < 0, 0, venus)) %>% mutate(chlorophyll = if_else(chlorophyll < 0, 0, chlorophyll))

df_cre01 <- df %>% slice(2, 9, 18)
df_cre01$chlorophyll <- df_cre01$chlorophyll - df$chlorophyll[32]
df_cre01$venus <- df_cre01$venus - df$venus[32]
df_cre01 <- df_cre01 %>% mutate(venus = if_else(venus < 0, 0, venus)) %>% mutate(chlorophyll = if_else(chlorophyll < 0, 0, chlorophyll))

df_rbmp1 <- df %>% slice(3,10,19)
df_rbmp1$chlorophyll <- df_rbmp1$chlorophyll - df$chlorophyll[32]
df_rbmp1$venus <- df_rbmp1$venus - df$venus[32]
df_rbmp1 <- df_rbmp1 %>% mutate(venus = if_else(venus < 0, 0, venus)) %>% mutate(chlorophyll = if_else(chlorophyll < 0, 0, chlorophyll))

d <- ggplot(df_cmj030, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    # geom_label_repel(aes(label = labels, y = log(chlorophyll), size = NULL, color = NULL), nudge_y = 0.25) +  
    labs(y = "Log(Expression)", title = "Sonication 2: CMJ030")


e <- ggplot(df_cre01, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    # geom_label_repel(aes(label = labels, y = log(chlorophyll), size = NULL, color = NULL), nudge_y = 0.25) +  
    labs(y = "Log(Expression)", title = "Sonication 2: Cre01")

f <- ggplot(df_rbmp1, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    # geom_label_repel(aes(label = labels, y = log(chlorophyll), size = NULL, color = NULL), nudge_y = 0.25) +  
    labs(y = "Log(Expression)", title = "Sonication 2: RBMP1")


# Sonication trial 3
df_cmj030 <- df %>% slice(1, 11, 20)
df_cmj030$chlorophyll <- df_cmj030$chlorophyll - df$chlorophyll[32]
df_cmj030$venus <- df_cmj030$venus - df$venus[32]
df_cmj030 <- df_cmj030 %>% mutate(venus = if_else(venus < 0, 0, venus)) %>% mutate(chlorophyll = if_else(chlorophyll < 0, 0, chlorophyll))

df_cre01 <- df %>% slice(2, 12, 21)
df_cre01$chlorophyll <- df_cre01$chlorophyll - df$chlorophyll[32]
df_cre01$venus <- df_cre01$venus - df$venus[32]
df_cre01 <- df_cre01 %>% mutate(venus = if_else(venus < 0, 0, venus)) %>% mutate(chlorophyll = if_else(chlorophyll < 0, 0, chlorophyll))

df_rbmp1 <- df %>% slice(3,13,22)
df_rbmp1$chlorophyll <- df_rbmp1$chlorophyll - df$chlorophyll[32]
df_rbmp1$venus <- df_rbmp1$venus - df$venus[32]
df_rbmp1 <- df_rbmp1 %>% mutate(venus = if_else(venus < 0, 0, venus)) %>% mutate(chlorophyll = if_else(chlorophyll < 0, 0, chlorophyll))

g <- ggplot(df_cmj030, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    # geom_label_repel(aes(label = labels, y = log(chlorophyll), size = NULL, color = NULL), nudge_y = 0.25) +  
    labs(y = "Log(Expression)", title = "Sonication 6: CMJ030")


h <- ggplot(df_cre01, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    # geom_label_repel(aes(label = labels, y = log(chlorophyll), size = NULL, color = NULL), nudge_y = 0.25) +  
    labs(y = "Log(Expression)", title = "Sonication 6: Cre01")

i <- ggplot(df_rbmp1, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    # geom_label_repel(aes(label = labels, y = log(chlorophyll), size = NULL, color = NULL), nudge_y = 0.25) +  
    labs(y = "Log(Expression)", title = "Sonication 6: RBMP1")


dev.new()
ggarrange(a, b, c, d, e, f, g, h, i, ncol = 3, nrow = 3, labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I"))





### Elution: 

df <- data.frame(chlorophyll = c(1,	7042,	6951,	658,	590,	627,	688,	216,	305,	327,	3738, 9797,	9180,	102,	80,	28),
                 venus = c(180,	2611,	2709,	1304,	1682,	1885,	1749,	918	,1179,	1240,	26083, 962	,851,	82	,104,	193),
                 labels = c('Buffer',	'Cre05 Lysate',	'Cre05 Lysate (post - centrifugation)',
                            'Cre05 pre elution beads',
                            'Cre05 post elution beads (40% Flag solution)',
                            'Cre05 post elution beads (66% Flag solution)',
                            'Cre05 post elution beads (90% Flag solution)',
                            'Cre05 eluate (40% Flag solution)',	
                            'Cre05 eluate (66% Flag solution)	', 'Cre05 eluate (90% Flag solution)',
                            'Cre05 all leftover eluates combined', 'Cmj030 Lysate',	'Cmj030 Lysate (post - centrifugation)	',
                            'Cmj030 pre elution beads',	'Cmj030 post elution beads (90% Flag solution)',	
                            'Cmj030 eluate (90% Flag solution)ed (for DLS))'))



# Sonication trial 1 


df_elution <- df %>% slice(15, 16, 5, 8, 6, 9, 7, 10) 
chlorophyll_ratio <- c() 
venus_ratio <- c() 
j = 1
for (i in c(1, 3, 5, 7)) {
    chlorophyll_ratio[j] = df_elution$chlorophyll[i + 1] / df_elution$chlorophyll[i]
    venus_ratio[j] = df_elution$venus[i + 1] / df_elution$venus[i]
    j = j + 1
}
df_ratios <- data.frame(chlorophyll = chlorophyll_ratio, venus = venus_ratio, FLAG_Buffer = c("CMJ030 Control", "40% FLAG","66% FLAG", "90% FLAG"))
df_plot <- df_ratios %>% pivot_longer(cols = c(chlorophyll, venus))

dev.new()
ggplot(df_plot, aes(x = FLAG_Buffer, y = value, fill = name)) +
    geom_bar(stat = "identity", position = "dodge") + 
    scale_fill_manual(values=c("#336600", "#0000FF")) + 
    labs(y = "Ratio of Expression in Eluate to Post-elution Beads", title = "Elution Optimization") +
    theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"), title = element_text(size = 18))



# Confocal Code: 

data <- read.csv("~/Desktop/Results2.csv")
hist1 <- hist(data$Area, breaks = 100) 
hist1$density <- hist1$counts / sum(hist1$counts)
plot(hist1, freq = F, main = "Distribution of Particle Areas", xlab = "Area (um^2)")    
summary(data$Area)

hist2 <- hist(data$Feret, breaks = 100)
hist2$density <- hist2$counts / sum(hist2$counts)
plot(hist2, freq = F, main = "Distribution of Particle Feret Diameters", xlab = "Diameter (um)", xlim = c(0,8), ylim = c(0, 0.12))    
summary(data$Feret)

data$cut_area <- data$Area %>% quantileCut(10) 
Total_area <- sum(data$Area)
d1 <- data %>% group_by(cut_area) %>% summarize(Percent_TA = sum(Area)/ Total_area)
plot(d1, ylab = "Percent of Total Area", xlab = "Area Levels", main = "Vesicle Area Classes % of Total Area") 





# Data comes from Dec2nd:
df <- data.frame(chlorophyll = c(7085,	4041,	3988,	4288,	734,	133,	296,	81,	1154,	713,	7190,	5686, 159,	169,	3,	4,	2), 
                 venus = c(736,	6506,	500,	7281,	266,	1982,	292,	1111, 326,	370,	743,	2838, 301,	3699,	227,	243,	287), 
                 labels = c("Lysate", "Lysate", "Beads Post-Incubation", "Beads Post-Incubation", 
                            "Post Elution", "Post Elution", "Post BE", "Post BE", 
                            "Post Spin Down", "Post Spin Down", "Post incubation Supernatant", "Post incubation Supernatant", 
                            "Post Spindown Supernatant", "Post SpinDown Supernatant", "BE Flow-Through", "BE Flow-Through", "1x Buffer"))


# Buffer Exchange Trials: 
df_cmj030 <- df %>% slice(1, 3, 5, 7)
df_cmj030$chlorophyll <- df_cmj030$chlorophyll - df$chlorophyll[17]
df_cmj030$venus <- df_cmj030$venus - df$venus[17]
df_cmj030 <- df_cmj030 %>% mutate(venus = if_else(venus < 0, 0, venus)) %>% mutate(chlorophyll = if_else(chlorophyll < 0, 0, chlorophyll))


df_cre05 <- df %>% slice(2, 4, 6, 8)
df_cre05$chlorophyll <- df_cre05$chlorophyll - df$chlorophyll[17]
df_cre05$venus <- df_cre05$venus - df$venus[17]
df_cre05 <- df_cre05 %>% mutate(venus = if_else(venus < 0, 0, venus)) %>% mutate(chlorophyll = if_else(chlorophyll < 0, 0, chlorophyll))


a <- ggplot(df_cmj030, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    geom_label_repel(aes(label = labels, 
                         y = log(chlorophyll), 
                         size = NULL, color = NULL), nudge_y = 0.25)  + 
    labs(y = "Log of Chlorophyll and Venus Expression", title = "CMJ030")



b <- ggplot(df_cre05, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    geom_label_repel(aes(label = labels, 
                         y = log(chlorophyll), 
                         size = NULL, color = NULL), nudge_y = 0.25) +  
    labs(y = "Log of Chlorophyll and Venus Expression", title = "Cre05")

dev.new()
plot_grid(a, b, labels = c('B', 'C'))
          

# Spin down trials 
df_cmj030 <- df %>% slice(1, 3, 5, 9)
df_cmj030$chlorophyll <- df_cmj030$chlorophyll - df$chlorophyll[17]
df_cmj030$venus <- df_cmj030$venus - df$venus[17]
df_cmj030 <- df_cmj030 %>% mutate(venus = if_else(venus < 0, 0, venus)) %>% mutate(chlorophyll = if_else(chlorophyll < 0, 0, chlorophyll))


df_cre05 <- df %>% slice(2, 4, 6, 10)
df_cre05$chlorophyll <- df_cre05$chlorophyll - df$chlorophyll[17]
df_cre05$venus <- df_cre05$venus - df$venus[17]
df_cre05 <- df_cre05 %>% mutate(venus = if_else(venus < 0, 0, venus)) %>% mutate(chlorophyll = if_else(chlorophyll < 0, 0, chlorophyll))

c <- ggplot(df_cmj030, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    geom_label_repel(aes(label = labels, 
                         y = log(chlorophyll), 
                         size = NULL, color = NULL), nudge_y = 0.25)  + 
    labs(y = "Log of Chlorophyll and Venus Expression", title = "CMJ030")



d <- ggplot(df_cre05, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    geom_label_repel(aes(label = labels, 
                         y = log(chlorophyll), 
                         size = NULL, color = NULL), nudge_y = 0.25) +  
    labs(y = "Log of Chlorophyll and Venus Expression", title = "Cre05")

dev.new()
plot_grid(a, b, c, d, labels = c('B','C','D','E'))


# Visualizing the other results from this trial 


df <- data.frame(chlorophyll = c(7085,	4041,	3988,	4288,	734,	133,	296,	81,	1154,	713,	7190,	5686, 159,	169,	3,	4,	2), 
                 venus = c(736,	6506,	500,	7281,	266,	1982,	292,	1111, 326,	370,	743,	2838, 301,	3699,	227,	243,	287), 
                 labels = c("CMJ030 Lysate", "Cre05 Lysate", "a CMJ030 + Beads Post-Incubation", "b Cre05 + Beads Post-Incubation", 
                            "CMJ030 Post elution", "Cre05 Post elution", "e CMJ030 Post Buffer Exchange", "f Cre05 Post Buffer Exchange", 
                            "i CMJ030 Post Spin Down", "j Cre05 Post Spin Down", "c CMJ030 Post incubation Supernatant", "d Cre05 Post incubation Supernatant", 
                            "k CMJ030 Post SpinDown Supernatant", "l Cre05 Post SpinDown Supernatant", "g CMJ030  BE Flow-Through", "h Creo5 BE Flow-Through", "1x Buffer"))


df_cmj030 <- df %>% slice(3, 11, 9, 13, 7, 15)
df_cmj030$chlorophyll <- df_cmj030$chlorophyll - df$chlorophyll[17]
df_cmj030$venus <- df_cmj030$venus - df$venus[17]
df_cmj030 <- df_cmj030 %>% mutate(venus = if_else(venus < 0, 0, venus)) %>% mutate(chlorophyll = if_else(chlorophyll < 0, 0, chlorophyll))
df_cmj030 <- cbind(df_cmj030[3], stack(df_cmj030[1:2])) %>% arrange(labels)
df_cmj030$values <- log(df_cmj030$values)


df_cre05 <- df %>% slice(4, 12, 10, 14, 8, 16)
df_cre05$chlorophyll <- df_cre05$chlorophyll - df$chlorophyll[17]
df_cre05$venus <- df_cre05$venus - df$venus[17]
df_cre05 <- df_cre05 %>% mutate(venus = if_else(venus < 0, 0, venus)) %>% mutate(chlorophyll = if_else(chlorophyll < 0, 0, chlorophyll))
df_cre05 <- cbind(df_cre05[3], stack(df_cre05[1:2])) %>% arrange(labels)
df_cre05$values <- log(df_cre05$values)



a <- ggplot(df_cmj030, aes(x = labels, y = values, fill = ind)) +
    geom_bar(stat = "identity", position = position_dodge(width=0.5)) + 
    scale_fill_manual(values=c("#CC9966", "#663339")) + 
    labs(y = "Log Expression", x = " ")  + 
    theme(axis.text.x = element_text(angle = 60, vjust = 0.6))

z <- ggplot(df_cre05, aes(x = labels, y = values, fill = ind)) +
    geom_bar(stat = "identity", position = position_dodge(width=0.5)) + 
    scale_fill_manual(values=c("#CC9966", "#663339")) + 
    labs(y = "Log Expression", x = " ")  + 
    theme(axis.text.x = element_text(angle = 60, vjust = 0.6))

dev.new()
plot_grid(a, z, labels = "AUTO")


##################################

# Data comes from Dec 15th :
df <- data.frame(chlorophyll = c(2, 5154, 51, 16, 4097, 89, 401, 6244, 1792, 9, 6403, 1999, 1008, 4615, 69, 12, 428, 21), 
                 venus = c(282, 2244, 732, 444, 4296, 1273, 1255, 4778, 5111, 538, 1429, 1522, 539, 442, 299, 314, 1201, 651), 
                 labels = c('1X Buffer', 'Cre01 PC', 'Cre01 PE', 'Cre01 PF-30k', 
                            'Cre05 PC', 'Cre05 PE', 'Cre05 PF-30k',
                            'RBMP1 PC', 'RBMP1 PE', 'RBMP1 PF-30k',
                            'RBMP2 PC', 'RBMP2 PE', 'RBMP2 PF-30k',
                            'SAGA1 PC', 'SAGA1 PE', 'SAGA1 PF-30k',
                            'RBMP1 PF-10k', 'Cre01 PF-10k'))


# 30K Filters : 

cre01 <- df %>% slice(2, 3, 4)
cre01$chlorophyll <- cre01$chlorophyll - df$chlorophyll[1]
cre01$venus <- cre01$venus - df$venus[1]
cre01 <- cre01 %>% mutate(venus = if_else(venus < 0, 0, venus)) %>% mutate(chlorophyll = if_else(chlorophyll < 0, 0, chlorophyll))

cre05 <- df %>% slice(5, 6, 7)
cre05$chlorophyll <- cre05$chlorophyll - df$chlorophyll[1]
cre05$venus <- cre05$venus - df$venus[1]
cre05 <- cre05 %>% mutate(venus = if_else(venus < 0, 0, venus)) %>% mutate(chlorophyll = if_else(chlorophyll < 0, 0, chlorophyll))

rbmp1 <- df %>% slice(8, 9, 10)
rbmp1$chlorophyll <- rbmp1$chlorophyll - df$chlorophyll[1]
rbmp1$venus <- rbmp1$venus - df$venus[1]
rbmp1 <- rbmp1 %>% mutate(venus = if_else(venus < 0, 0, venus)) %>% mutate(chlorophyll = if_else(chlorophyll < 0, 0, chlorophyll))

rbmp2 <- df %>% slice(11, 12, 13)
rbmp2$chlorophyll <- rbmp2$chlorophyll - df$chlorophyll[1]
rbmp2$venus <- rbmp2$venus - df$venus[1]
rbmp2 <- rbmp2 %>% mutate(venus = if_else(venus < 0, 0, venus)) %>% mutate(chlorophyll = if_else(chlorophyll < 0, 0, chlorophyll))

saga1 <- df %>% slice(14, 15, 16)
saga1$chlorophyll <- saga1$chlorophyll - df$chlorophyll[1]
saga1$venus <- saga1$venus - df$venus[1]
saga1 <- saga1 %>% mutate(venus = if_else(venus < 0, 0, venus)) %>% mutate(chlorophyll = if_else(chlorophyll < 0, 0, chlorophyll))


a <- ggplot(cre01, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    labs(y = "Log of Chlorophyll and Venus Expression", title = "Cre01 30k")

b <- ggplot(cre05, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    labs(y = "Log of Chlorophyll and Venus Expression", title = "Cre05 30k")

c <- ggplot(rbmp1, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    labs(y = "Log of Chlorophyll and Venus Expression", title = "RBMP1 30k")

d <- ggplot(rbmp2, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    labs(y = "Log of Chlorophyll and Venus Expression", title = "RBMP2 30k")

e <- ggplot(saga1, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    labs(y = "Log of Chlorophyll and Venus Expression", title = "SAGA1 30k")

dev.new()
plot_grid(a, b, c, d, e, labels = "AUTO")


## 10K Filters 
cre01 <- df %>% slice(2, 3, 18)
cre01$chlorophyll <- cre01$chlorophyll - df$chlorophyll[1]
cre01$venus <- cre01$venus - df$venus[1]
cre01 <- cre01 %>% mutate(venus = if_else(venus < 0, 0, venus)) %>% mutate(chlorophyll = if_else(chlorophyll < 0, 0, chlorophyll))

rbmp1 <- df %>% slice(8, 9, 17)
rbmp1$chlorophyll <- rbmp1$chlorophyll - df$chlorophyll[1]
rbmp1$venus <- rbmp1$venus - df$venus[1]
rbmp1 <- rbmp1 %>% mutate(venus = if_else(venus < 0, 0, venus)) %>% mutate(chlorophyll = if_else(chlorophyll < 0, 0, chlorophyll))



f <- ggplot(cre01, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    labs(y = "Log of Chlorophyll and Venus Expression", title = "Cre01 10K")

g <- ggplot(rbmp1, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    labs(y = "Log of Chlorophyll and Venus Expression", title = "RBMP1 10k")

dev.new()
plot_grid(f, g, labels = "AUTO")



# Total Protein Content of Post-Filtration Samples 
df2 <- data.frame(labels = c('Cre01 10K', 'Cre01 30k', 'RBMP1 10k', "RBMP1 30k", "Cre05 30k", "RBMP2 30k", "SAGA1 30k"),
                  concentration = c(5.026, 0.841, 4.575, 0.995, 1.907, 2.502, 1.028))
df2$protein_amount <- df2$concentration * 50 

h <- ggplot(df2, aes(x = labels, y = protein_amount)) +
    geom_bar(stat = "identity", position = position_dodge(width=0.5)) + 
    labs(y = "Protein amount (ug)", x = " ", title = "Total Amount of Protein \n in Final Outputs")  + 
    theme(axis.text.x = element_text(angle = 60, vjust = 0.6))

dev.new()
plot_grid(a, b, c, d, e, f, g, h, ncol = 4, nrow = 2, labels = "AUTO")


#####################

# results from 1/24 (pg 80) 
library(RColorBrewer)


df <- data.frame(chlorophyll = c(2173, 27, 2239, 1504, 43, 1121, 7), 
                 venus = c(262, 445, 260, 348, 571, 122, 317), 
                 labels = c('_Pre-Filtration', 'Flow-Through', '.Post-Filtration Product', '_Pre-Filtration', 'Flow-Through', '.Post-Filtration Product','Blank'))

df$chlorophyll <- df$chlorophyll - df$chlorophyll[7]
tenK <- df %>% slice(1, 2, 3) 
tenK$legend <- "10K"
thirtyK <- df %>% slice(4, 5, 6)
thirtyK$legend <- "30K"
merged <- merge(x = tenK, y = thirtyK, all = T) %>% arrange((labels))
merged$legend <- factor(merged$legend)
b <- ggplot(merged, aes(x = labels, y = chlorophyll, fill = legend)) +
    geom_bar(stat = "identity", position = position_dodge(width=0.8)) + 
    labs(y = "Chlorophyll Expression", x = " ", title = "Membrane Retention")  + 
    theme(axis.text.x = element_text(angle = 45, vjust = 0.6)) + 
    scale_fill_brewer(palette = "Paired") +
    theme(legend.position= 'top', 
          legend.justification='left',
          legend.direction='horizontal')

# 1/24: Protein amounts:

df <- data.frame(ten = c(103.20, 9, 101.10), 
                 ten_membranes = c(51, 67.30, 306), 
                 thirty = c(148, 108.90, 12.75),
                 thirty_membranes = c(79.88, 389.20, 266.80),
                 labels = c("_Pre-Filtration", "Flow-Through", ".Post-Filtration Filtered Product"))
df_long <- df %>% pivot_longer(cols = ten:thirty_membranes, names_to= "names", values_to= "values")
df_long$names <- factor(df_long$names)
df_long <- df_long %>% arrange(names, desc(labels))
levels(df_long$names) <- list('10kDa' = 'ten', '10kDa + Membranes' = 'ten_membranes', '30kDa' ='thirty', '30kDa + Membranes' = "thirty_membranes")


a <- ggplot(df_long, aes(x = names, y = values, fill = labels)) +
    geom_bar(stat = "identity", position = position_dodge(width=0.8)) + 
    labs(y = "Protein Amount (ug)", x = " ", title = "FLAG Elimination Efficacy")  + 
    scale_fill_brewer(palette = "Dark2") + 
    theme(axis.text.x = element_text(angle = 0, vjust = 0.6)) +
    theme(legend.position= 'bottom', 
          legend.justification='left',
          legend.direction='horizontal')

dev.new() 
plot_grid(a, b, labels = c("A","B"))



# Data from Eric's email (Feb 8th) -- experiment on page 85

data <- data.frame(min = c(2.7, 2.9, 2.5, 1.7, 3.5, 3.4), 
                   max = c(3.2, 3.0, 2.8, 2.1, 4.0, 4.3), 
                   sample = c('CMJ030', 'Cre01', 'Cre05', 'RBMP1', 'RBMP2', 'SAGA1'))
data$min <- data$min * 300 
data$max <- data$max * 300 

ggplot(data = data, aes(x = sample)) +
    geom_linerange(aes(ymin = min, ymax = max, x = sample),
                   size = 2, alpha = 1) + 
    coord_flip() + 
    geom_hline(yintercept = 2.75 * 300, linetype = "longdash") + 
    theme_bw(base_size = 14) + 
    labs(y = "Total Protein Amount (ug)", x = "Sample", title = "Total Protein Quantity in Samples")
    






