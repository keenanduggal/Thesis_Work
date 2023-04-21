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

#### New Approach: 
Cell_Line_1 <- read_csv("~/Desktop/Image_Analysis_Redo/one.csv")
Cell_Line_1$adivp <- Cell_Line_1$Area / Cell_Line_1$Perim.
Cell_Line_D <- read_csv("~/Desktop/Image_Analysis_Redo/d.csv")
Cell_Line_D$adivp <- Cell_Line_D$Area / Cell_Line_D$Perim.
Cell_Line_F <- read_csv("~/Desktop/Image_Analysis_Redo/f.csv")
Cell_Line_F$adivp <- Cell_Line_F$Area / Cell_Line_F$Perim.
Cell_Line_G <- read_csv("~/Desktop/Image_Analysis_Redo/g.csv")
Cell_Line_G$adivp <- Cell_Line_G$Area / Cell_Line_G$Perim.
Cell_Line_H <- read_csv("~/Desktop/Image_Analysis_Redo/h.csv")
Cell_Line_H$adivp <- Cell_Line_H$Area / Cell_Line_H$Perim.
Cell_Line_S <- read_csv("~/Desktop/Image_Analysis_Redo/s.csv")
Cell_Line_S$adivp <- Cell_Line_S$Area / Cell_Line_S$Perim.
Cell_Line_T <- read_csv("~/Desktop/Image_Analysis_Redo/t.csv")
Cell_Line_T$adivp <- Cell_Line_T$Area / Cell_Line_T$Perim.
Cell_Line_U <- read_csv("~/Desktop/Image_Analysis_Redo/u.csv")
Cell_Line_U$adivp <- Cell_Line_U$Area / Cell_Line_U$Perim.


# Area: 
t.test(Cell_Line_1$Area, Cell_Line_D$Area, alternative = "two.sided")
t.test(Cell_Line_1$Area, Cell_Line_F$Area, alternative = "two.sided") ## Significant 
t.test(Cell_Line_1$Area, Cell_Line_G$Area, alternative = "two.sided") ### Significant 
t.test(Cell_Line_1$Area, Cell_Line_H$Area, alternative = "two.sided")
t.test(Cell_Line_1$Area, Cell_Line_S$Area, alternative = "two.sided")
t.test(Cell_Line_1$Area, Cell_Line_T$Area, alternative = "two.sided") ## Significant 
t.test(Cell_Line_1$Area, Cell_Line_U$Area, alternative = "two.sided") # Significant 

machine_area_df <- data.frame('C_1' = mean(Cell_Line_1$Area), 
                      'D' = mean(Cell_Line_D$Area), 
                      'F' = mean(Cell_Line_F$Area), 
                      'G' = mean(Cell_Line_G$Area), 
                      'H' = mean(Cell_Line_H$Area), 
                      'S' = mean(Cell_Line_S$Area), 
                      'T' = mean(Cell_Line_T$Area),
                      'U' = mean(Cell_Line_U$Area))

combined <- gather(machine_area_df, key = "Cell_Line", value = "M_L", C_1, D, F, G, H, S, T, U)
combined$Manual <- manual_averages$Area
stacked_area <- cbind(combined[1], stack(combined[2:3])) %>% arrange(Cell_Line)


a <- ggplot(stacked_area, aes(x = Cell_Line, y = values, fill = ind)) +
    geom_bar(stat = "identity", position = position_dodge(width=0.75)) + 
    scale_fill_manual(values=c("#663339", "#CC996680")) + 
    labs(y = "Area (nm^2)", x = " ", title = "Average Tubule Area")  + 
    theme(axis.text.y=element_text(size=12),
          axis.text.x=element_text(size=15, face = "bold"),
          axis.title=element_text(size=15), 
          plot.title = element_text(size = 20, face="bold"), 
          legend.title=element_blank(), 
          legend.text = element_text(size=12),
          legend.position= 'top', 
          legend.justification='left',
          legend.direction='horizontal', 
          legend.margin=margin(t=2)) + 
    geom_signif(comparisons=list(c("C_1", "F")), annotations="**",
                y_position = 18000, tip_length = 0, size = 0.8, vjust=0.4) + 
    geom_signif(comparisons=list(c("C_1", "U")), annotations="*",
                y_position = 24000, tip_length = 0, size = 0.8, vjust=0.4)  +
    geom_signif(comparisons=list(c("C_1", "G")), annotations="***",
                y_position = 20000, tip_length = 0, size = 0.8, vjust=0.4)+ 
    geom_signif(comparisons=list(c("C_1", "T")), annotations="**",
                y_position = 22000, tip_length = 0, size = 0.8, vjust=0.4)
   
  



# Percent Area: 
percent_area <- read_excel("~/Desktop/Image_Analysis_Redo/percent_area.xlsx")
percent_area$Cell_line <- factor(percent_area$Cell_line)
percent_area %>% group_by(Cell_line) %>% summarize(mean = mean(percent_area) * 100)
t.test(percent_area %>% filter(Cell_line == 'cell_line_1') %>% pull(percent_area), percent_area %>% filter(Cell_line == 'cell_line_d') %>% pull(percent_area), alternative = "two.sided")
t.test(percent_area %>% filter(Cell_line == 'cell_line_1') %>% pull(percent_area), percent_area %>% filter(Cell_line == 'cell_line_f') %>% pull(percent_area), alternative = "two.sided") # Significant 
t.test(percent_area %>% filter(Cell_line == 'cell_line_1') %>% pull(percent_area), percent_area %>% filter(Cell_line == 'cell_line_g') %>% pull(percent_area), alternative = "two.sided") 
t.test(percent_area %>% filter(Cell_line == 'cell_line_1') %>% pull(percent_area), percent_area %>% filter(Cell_line == 'cell_line_h') %>% pull(percent_area), alternative = "two.sided") 
t.test(percent_area %>% filter(Cell_line == 'cell_line_1') %>% pull(percent_area), percent_area %>% filter(Cell_line == 'cell_line_s') %>% pull(percent_area), alternative = "two.sided") 
t.test(percent_area %>% filter(Cell_line == 'cell_line_1') %>% pull(percent_area), percent_area %>% filter(Cell_line == 'cell_line_t') %>% pull(percent_area), alternative = "two.sided") 
t.test(percent_area %>% filter(Cell_line == 'cell_line_1') %>% pull(percent_area), percent_area %>% filter(Cell_line == 'cell_line_u') %>% pull(percent_area), alternative = "two.sided") # Significant 

data <- percent_area %>% group_by(Cell_line) %>% summarize(M_L = mean(percent_area) * 100)
data$Manual <- manual_averages$`% Area`
data$ID <- c('C_1', 'D', 'F', 'G', 'H', 'S', 'T', 'U')
stacked_pa <- cbind(data[4], stack(data[2:3])) %>% arrange(ID)

b <- ggplot(stacked_pa, aes(x = ID, y = values, fill = ind)) +
    geom_bar(stat = "identity", position = position_dodge(width=0.75)) + 
    scale_fill_manual(values=c("#663339", "#CC996680")) + 
    labs(y = "Tubule % of Pyrenoid Area", x = " ", title = "Total Tubule/Pyrenoid Area")  + 
    theme(axis.text.y=element_text(size=12),
          axis.text.x=element_text(size=15, face = "bold"),
          axis.title=element_text(size=15), 
          plot.title = element_text(size = 20, face="bold"), 
          legend.title=element_blank(), 
          legend.text = element_text(size=12),
          legend.position= 'top', 
          legend.justification='left',
          legend.direction='horizontal', 
          legend.margin=margin(t=2)) + 
    geom_signif(comparisons=list(c("C_1", "F")), annotations="*",
                y_position = 22, tip_length = 0, size = 0.8, vjust=0.4) + 
    geom_signif(comparisons=list(c("C_1", "U")), annotations="*",
                y_position = 26, tip_length = 0, size = 0.8, vjust=0.4) 
  


# Perimeter: 
t.test(Cell_Line_1$Perim., Cell_Line_D$Perim., alternative = "two.sided")
t.test(Cell_Line_1$Perim., Cell_Line_F$Perim., alternative = "two.sided") # Significant 
t.test(Cell_Line_1$Perim., Cell_Line_G$Perim., alternative = "two.sided") ### Significant 
t.test(Cell_Line_1$Perim., Cell_Line_H$Perim., alternative = "two.sided")
t.test(Cell_Line_1$Perim., Cell_Line_S$Perim., alternative = "two.sided")
t.test(Cell_Line_1$Perim., Cell_Line_T$Perim., alternative = "two.sided") ## Significant 
t.test(Cell_Line_1$Perim., Cell_Line_U$Perim., alternative = "two.sided") # Significant 

machine_perim_df <- data.frame('C_1' = mean(Cell_Line_1$Perim.), 
                              'D' = mean(Cell_Line_D$Perim.), 
                              'F' = mean(Cell_Line_F$Perim.), 
                              'G' = mean(Cell_Line_G$Perim.), 
                              'H' = mean(Cell_Line_H$Perim.), 
                              'S' = mean(Cell_Line_S$Perim.), 
                              'T' = mean(Cell_Line_T$Perim.),
                              'U' = mean(Cell_Line_U$Perim.))

combined <- gather(machine_perim_df, key = "Cell_Line", value = "M_L", C_1, D, F, G, H, S, T, U)
combined$Manual <- manual_averages$Perim.
stacked_perim <- cbind(combined[1], stack(combined[2:3])) %>% arrange(Cell_Line)


c <- ggplot(stacked_perim, aes(x = Cell_Line, y = values, fill = ind)) +
    geom_bar(stat = "identity", position = position_dodge(width=0.75)) + 
    scale_fill_manual(values=c("#663339", "#CC996680")) + 
    labs(y = "Perim (nm)", x = " ", title = "Average Tubule Perimeter")  + 
    theme(axis.text.y=element_text(size=12),
          axis.text.x=element_text(size=15, face = "bold"),
          axis.title=element_text(size=15), 
          plot.title = element_text(size = 20, face="bold"), 
          legend.title=element_blank(), 
          legend.text = element_text(size=12),
          legend.position= 'top', 
          legend.justification='left',
          legend.direction='horizontal', 
          legend.margin=margin(t=2)) + 
    geom_signif(comparisons=list(c("C_1", "F")), annotations="*",
                y_position = 400, tip_length = 0, size = 0.8, vjust=0.4) + 
    geom_signif(comparisons=list(c("C_1", "G")), annotations="***",
                y_position = 450, tip_length = 0, size = 0.8, vjust=0.4)  +
    geom_signif(comparisons=list(c("C_1", "T")), annotations="**",
                y_position = 500, tip_length = 0, size = 0.8, vjust=0.4)+ 
    geom_signif(comparisons=list(c("C_1", "U")), annotations="*",
                y_position = 555, tip_length = 0, size = 0.8, vjust=0.4)



# Area:Perimeter Ratio: 
t.test(Cell_Line_1$adivp, Cell_Line_D$adivp, alternative = "two.sided") ### Significant
t.test(Cell_Line_1$adivp, Cell_Line_F$adivp, alternative = "two.sided")
t.test(Cell_Line_1$adivp, Cell_Line_G$adivp, alternative = "two.sided") ### Significant 
t.test(Cell_Line_1$adivp, Cell_Line_H$adivp, alternative = "two.sided") ### Significant
t.test(Cell_Line_1$adivp, Cell_Line_S$adivp, alternative = "two.sided") ### Significant
t.test(Cell_Line_1$adivp, Cell_Line_T$adivp, alternative = "two.sided") ### Significant 
t.test(Cell_Line_1$adivp, Cell_Line_U$adivp, alternative = "two.sided") 

df <- data.frame('C_1' = mean(Cell_Line_1$adivp), 
                              'D' = mean(Cell_Line_D$adivp), 
                              'F' = mean(Cell_Line_F$adivp), 
                              'G' = mean(Cell_Line_G$adivp), 
                              'H' = mean(Cell_Line_H$adivp), 
                              'S' = mean(Cell_Line_S$adivp), 
                              'T' = mean(Cell_Line_T$adivp),
                              'U' = mean(Cell_Line_U$adivp))

df_new <- gather(df, key = "Cell_Line", value = "M_L", C_1, D, F, G, H, S, T, U)

d <- ggplot(df_new, aes(x = Cell_Line, y = M_L)) +
    geom_bar(stat = "identity", position = position_dodge(width=0.75), fill = "#663339", color = "black") + 
    labs(y = "Area/Perim (nm)", x = " ", title = "Average Tubule Area:Perimeter")  + 
    theme(axis.text.y=element_text(size=12),
          axis.text.x=element_text(size=15, face = "bold"),
          axis.title=element_text(size=15), 
          plot.title = element_text(size = 20, face="bold"), 
          legend.title=element_blank(), 
          legend.text = element_text(size=12),
          legend.position= 'top', 
          legend.justification='left',
          legend.direction='horizontal', 
          legend.margin=margin(t=2)) + 
    geom_signif(comparisons=list(c("C_1", "D")), annotations="***",
                y_position = 18, tip_length = 0, size = 0.8, vjust=0.4) + 
    geom_signif(comparisons=list(c("C_1", "G")), annotations="***",
                y_position = 20, tip_length = 0, size = 0.8, vjust=0.4)  +
    geom_signif(comparisons=list(c("C_1", "H")), annotations="***",
                y_position = 22, tip_length = 0, size = 0.8, vjust=0.4)+ 
    geom_signif(comparisons=list(c("C_1", "S")), annotations="***",
                y_position = 24, tip_length = 0, size = 0.8, vjust=0.4) + 
    geom_signif(comparisons=list(c("C_1", "T")), annotations="***",
                y_position = 26, tip_length = 0, size = 0.8, vjust=0.4)


dev.new()
ggarrange(a, b, c, d, ncol = 2, nrow = 2, labels = c("A", "B", "C", "D"))


####### Shape: 

# Roundness
t.test(Cell_Line_1$Round, Cell_Line_D$Round, alternative = "two.sided") 
t.test(Cell_Line_1$Round, Cell_Line_F$Round, alternative = "two.sided")
t.test(Cell_Line_1$Round, Cell_Line_G$Round, alternative = "two.sided") 
t.test(Cell_Line_1$Round, Cell_Line_H$Round, alternative = "two.sided") 
t.test(Cell_Line_1$Round, Cell_Line_S$Round, alternative = "two.sided") 
t.test(Cell_Line_1$Round, Cell_Line_T$Round, alternative = "two.sided") 
t.test(Cell_Line_1$Round, Cell_Line_U$Round, alternative = "two.sided") 


ml_round <- data.frame('C_1' = mean(Cell_Line_1$Round), 
                               'D' = mean(Cell_Line_D$Round), 
                               'F' = mean(Cell_Line_F$Round), 
                               'G' = mean(Cell_Line_G$Round), 
                               'H' = mean(Cell_Line_H$Round), 
                               'S' = mean(Cell_Line_S$Round), 
                               'T' = mean(Cell_Line_T$Round),
                               'U' = mean(Cell_Line_U$Round))

combined <- gather(ml_round, key = "Cell_Line", value = "M_L", C_1, D, F, G, H, S, T, U)
combined$Manual <- manual_averages$Round
stacked_round <- cbind(combined[1], stack(combined[2:3])) %>% arrange(Cell_Line)


a <- ggplot(stacked_round, aes(x = Cell_Line, y = values, fill = ind)) +
    geom_bar(stat = "identity", position = position_dodge(width=0.75)) + 
    scale_fill_manual(values=c("#663339", "#CC996680")) + 
    labs(y = "Roundness (0-1)", x = " ", title = "Average Tubule Roundness")  + 
    theme(axis.text.y=element_text(size=10),
          axis.text.x=element_text(size=12, face = "bold"),
          axis.title=element_text(size=12), 
          plot.title = element_text(size = 15, face="bold"), 
          legend.title=element_blank(), 
          legend.text = element_text(size=12),
          legend.position= 'top', 
          legend.justification='left',
          legend.direction='horizontal', 
          legend.margin=margin(t=2)) 



# Solidity
t.test(Cell_Line_1$Solidity, Cell_Line_D$Solidity, alternative = "two.sided") 
t.test(Cell_Line_1$Solidity, Cell_Line_F$Solidity, alternative = "two.sided")
t.test(Cell_Line_1$Solidity, Cell_Line_G$Solidity, alternative = "two.sided") 
t.test(Cell_Line_1$Solidity, Cell_Line_H$Solidity, alternative = "two.sided") 
t.test(Cell_Line_1$Solidity, Cell_Line_S$Solidity, alternative = "two.sided") ## Significant
t.test(Cell_Line_1$Solidity, Cell_Line_T$Solidity, alternative = "two.sided") 
t.test(Cell_Line_1$Solidity, Cell_Line_U$Solidity, alternative = "two.sided") # Significant


ml_solid <- data.frame('C_1' = mean(Cell_Line_1$Solidity), 
                       'D' = mean(Cell_Line_D$Solidity), 
                       'F' = mean(Cell_Line_F$Solidity), 
                       'G' = mean(Cell_Line_G$Solidity), 
                       'H' = mean(Cell_Line_H$Solidity), 
                       'S' = mean(Cell_Line_S$Solidity), 
                       'T' = mean(Cell_Line_T$Solidity),
                       'U' = mean(Cell_Line_U$Solidity))

combined <- gather(ml_solid, key = "Cell_Line", value = "M_L", C_1, D, F, G, H, S, T, U)
combined$Manual <- manual_averages$Solidity
stacked_solid <- cbind(combined[1], stack(combined[2:3])) %>% arrange(Cell_Line)


b <- ggplot(stacked_solid, aes(x = Cell_Line, y = values, fill = ind)) +
    geom_bar(stat = "identity", position = position_dodge(width=0.75)) + 
    scale_fill_manual(values=c("#663339", "#CC996680")) + 
    labs(y = "Solidity (0-1)", x = " ", title = "Average Tubule Solidity")  + 
    theme(axis.text.y=element_text(size=10),
          axis.text.x=element_text(size=12, face = "bold"),
          axis.title=element_text(size=12), 
          plot.title = element_text(size = 15, face="bold"), 
          legend.title=element_blank(), 
          legend.text = element_text(size=12),
          legend.position= 'top', 
          legend.justification='left',
          legend.direction='horizontal', 
          legend.margin=margin(t=2)) + 
    geom_signif(comparisons=list(c("C_1", "S")), annotations="**",
                y_position = 0.95, tip_length = 0, size = 0.8, vjust=0.4) + 
    geom_signif(comparisons=list(c("C_1", "U")), annotations="*",
                y_position = 1, tip_length = 0, size = 0.8, vjust=0.4)


# Circularity
t.test(Cell_Line_1$Circ., Cell_Line_D$Circ., alternative = "two.sided") 
t.test(Cell_Line_1$Circ., Cell_Line_F$Circ., alternative = "two.sided") # Significant
t.test(Cell_Line_1$Circ., Cell_Line_G$Circ., alternative = "two.sided") 
t.test(Cell_Line_1$Circ., Cell_Line_H$Circ., alternative = "two.sided") 
t.test(Cell_Line_1$Circ., Cell_Line_S$Circ., alternative = "two.sided") ## Significant
t.test(Cell_Line_1$Circ., Cell_Line_T$Circ., alternative = "two.sided") 
t.test(Cell_Line_1$Circ., Cell_Line_U$Circ., alternative = "two.sided") ## Significant


ml_circ <- data.frame('C_1' = mean(Cell_Line_1$Circ.), 
                       'D' = mean(Cell_Line_D$Circ.), 
                       'F' = mean(Cell_Line_F$Circ.), 
                       'G' = mean(Cell_Line_G$Circ.), 
                       'H' = mean(Cell_Line_H$Circ.), 
                       'S' = mean(Cell_Line_S$Circ.), 
                       'T' = mean(Cell_Line_T$Circ.),
                       'U' = mean(Cell_Line_U$Circ.))

combined <- gather(ml_circ, key = "Cell_Line", value = "M_L", C_1, D, F, G, H, S, T, U)
combined$Manual <- manual_averages$Circ.
stacked_circ <- cbind(combined[1], stack(combined[2:3])) %>% arrange(Cell_Line)


c <- ggplot(stacked_circ, aes(x = Cell_Line, y = values, fill = ind)) +
    geom_bar(stat = "identity", position = position_dodge(width=0.75)) + 
    scale_fill_manual(values=c("#663339", "#CC996680")) + 
    labs(y = "Circularity (0-1)", x = " ", title = "Average Tubule Circularity")  + 
    theme(axis.text.y=element_text(size=10),
          axis.text.x=element_text(size=12, face = "bold"),
          axis.title=element_text(size=12), 
          plot.title = element_text(size = 15, face="bold"), 
          legend.title=element_blank(), 
          legend.text = element_text(size=12),
          legend.position= 'top', 
          legend.justification='left',
          legend.direction='horizontal', 
          legend.margin=margin(t=2)) + 
    geom_signif(comparisons=list(c("C_1", "F")), annotations="*",
                y_position = 0.76, tip_length = 0, size = 0.8, vjust=0.4) + 
    geom_signif(comparisons=list(c("C_1", "S")), annotations="**",
                y_position = 0.81, tip_length = 0, size = 0.8, vjust=0.4) + 
    geom_signif(comparisons=list(c("C_1", "U")), annotations="**",
                y_position = 0.86, tip_length = 0, size = 0.8, vjust=0.4)


dev.new()
ggarrange(a, b, c, ncol = 3, nrow = 1, labels = c("A", "B", "C"))



### Old Approach 

# Original Data Load: (summary) 
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
    geom_bar(stat = "identity", fill = "#663339", position = position_dodge(width=0.5)) + 
    labs(y = "Mean Normalized Pixel Intensity", x = " ", title = "Mean Tubule Pixel Intensity")  + 
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=12), 
          plot.title = element_text(size = 15, face="bold"), 
          axis.text.x = element_text(angle = 60, vjust = 0.6))

## Min: 
data <- data.frame(names = machine_learning$`Cell Line`, machine_learning = machine_learning$Minimum)

b <- ggplot(data, aes(x = names, y = machine_learning)) +
    geom_bar(stat = "identity", fill = "#663339", position = position_dodge(width=0.5)) + 
    labs(y = "Min Normalized Pixel Intensity", x = " ", title = "Minimum Tubule Pixel Intensity")  + 
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=12), 
          plot.title = element_text(size = 15, face="bold"), 
          axis.text.x = element_text(angle = 60, vjust = 0.6))

## Max: 
data <- data.frame(names = machine_learning$`Cell Line`, machine_learning = machine_learning$Max)

c <- ggplot(data, aes(x = names, y = machine_learning)) +
    geom_bar(stat = "identity",fill = "#663339", position = position_dodge(width=0.5)) + 
    labs(y = "Max Normalized Pixel Intensity", x = " ", title = "Maximum Tubule Pixel Intensity")  + 
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=12), 
          plot.title = element_text(size = 15, face="bold"), 
          axis.text.x = element_text(angle = 60, vjust = 0.6))

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

# Old Plot: 
a <- ggplot(area_df, aes(x = names, y = values)) +
    geom_bar(stat = "identity") + 
    labs(y = "Area (nm^2)", x = " ", title = "Average Tubule Area (nm^2)")  + 
    theme(axis.text.x = element_text(angle = 60, vjust = 0.6)) +
    geom_signif(comparisons=list(c("CMJ030", "PTN3")), annotations="**",
                y_position = 80000, tip_length = 0, vjust=0.4)  

# Used Plot: 
tempdf <- data.frame(PTN3 = ptn3$Area)
tempdf$CMJ030 <- c(cmj030$Area, rep(NA, nrow(tempdf)-length(cmj030$Area)))     
tempdf$PTN1 <- c(ptn1$Area, rep(NA, nrow(tempdf)-length(ptn1$Area)))     
tempdf$PTN2 <- c(ptn2$Area, rep(NA, nrow(tempdf)-length(ptn2$Area)))     
new_area <- pivot_longer(tempdf, cols = c("PTN3","PTN2","PTN1","CMJ030"),  names_to= "Identity", values_to="Area")
d <- ggplot(data = new_area, aes(x = Identity, y = log(Area))) + 
    geom_boxplot() + 
    geom_signif(comparisons=list(c("CMJ030", "PTN3")), annotations="**",
                y_position = 14, tip_length = 0, vjust=0.4, size=1)  + 
    labs(y = "Log[Area (nm^2)]", x = " ", title = "Tubule Area") + 
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=12), 
          plot.title = element_text(size = 15, face="bold")) + 
    geom_jitter(shape=16, position=position_jitter(0.1), alpha = 0.25, color = "darkgreen") + 
    geom_point(aes(x = area_df$names[1], y = log(area_df$values[1])), shape=20, size=5, color="red", fill="red") + 
    geom_text(aes(x = area_df$names[1], y = log(area_df$values[1])), label = round(area_df$values[1], 0), color = "red", vjust = 0, nudge_y = 0.2) + 
    geom_point(aes(x = area_df$names[2], y = log(area_df$values[2])), shape=20, size=5, color="red", fill="red") + 
    geom_text(aes(x = area_df$names[2], y = log(area_df$values[2])), label = round(area_df$values[2], 0), color = "red", vjust = 0, nudge_y = 0.2) + 
    geom_point(aes(x = area_df$names[3], y = log(area_df$values[3])), shape=20, size=5, color="red", fill="red") + 
    geom_text(aes(x = area_df$names[3], y = log(area_df$values[3])), label = round(area_df$values[3], 0), color = "red", vjust = 0, nudge_y = 0.2) + 
    geom_point(aes(x = area_df$names[4], y = log(area_df$values[4])), shape=20, size=5, color="red", fill="red") + 
    geom_text(aes(x = area_df$names[4], y = log(area_df$values[4])), label = round(area_df$values[4], 0), color = "red", vjust = 0, nudge_y = 0.2) 
    



perim_df <- data.frame(names = c('CMJ030', 'PTN1', 'PTN2', 'PTN3'), 
                      values = c(mean(cmj030$Perim.), mean(ptn1$Perim.), mean(ptn2$Perim.), mean(ptn3$Perim.)))
t.test(cmj030$Perim., ptn1$Perim., alternative = "two.sided")
t.test(cmj030$Perim., ptn2$Perim., alternative = "two.sided")
t.test(cmj030$Perim., ptn3$Perim., alternative = "two.sided")

# Old Plot: 
b <- ggplot(perim_df, aes(x = names, y = values)) +
    geom_bar(stat = "identity") + 
    labs(y = "Perimeter (nm)", x = " ", title = "Average Tubule Perimeter (nm)")  + 
    theme(axis.text.x = element_text(angle = 60, vjust = 0.6)) 

#Used Plot: 
tempdf <- data.frame(PTN3 = ptn3$Perim.)
tempdf$CMJ030 <- c(cmj030$Perim., rep(NA, nrow(tempdf)-length(cmj030$Perim.)))     
tempdf$PTN1 <- c(ptn1$Perim., rep(NA, nrow(tempdf)-length(ptn1$Perim.)))     
tempdf$PTN2 <- c(ptn2$Perim., rep(NA, nrow(tempdf)-length(ptn2$Perim.)))     
new_perim <- pivot_longer(tempdf, cols = c("PTN3","PTN2","PTN1","CMJ030"),  names_to= "Identity", values_to="Perim")
e <- ggplot(data = new_perim, aes(x = Identity, y = log(Perim))) + 
    geom_boxplot() + 
    labs(y = "log[Perimeter (nm)]", x = " ", title = "Tubule Perimeter") + 
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=12), 
          plot.title = element_text(size = 15, face="bold")) + 
    geom_jitter(shape=16, position=position_jitter(0.1), alpha = 0.25, color = "darkgreen") + 
    geom_point(aes(x = perim_df$names[1], y = log(perim_df$values[1])), shape=20, size=5, color="red", fill="red") + 
    geom_text(aes(x = perim_df$names[1], y = log(perim_df$values[1])), label = round(perim_df$values[1], 0), color = "red", vjust = 0, nudge_y = 0.2) + 
    geom_point(aes(x = perim_df$names[2], y = log(perim_df$values[2])), shape=20, size=5, color="red", fill="red") + 
    geom_text(aes(x = perim_df$names[2], y = log(perim_df$values[2])), label = round(perim_df$values[2], 0), color = "red", vjust = 0, nudge_y = 0.2) + 
    geom_point(aes(x = perim_df$names[3], y = log(perim_df$values[3])), shape=20, size=5, color="red", fill="red") + 
    geom_text(aes(x = perim_df$names[3], y = log(perim_df$values[3])), label = round(perim_df$values[3], 0), color = "red", vjust = 0, nudge_y = 0.2) + 
    geom_point(aes(x = perim_df$names[4], y = log(perim_df$values[4])), shape=20, size=5, color="red", fill="red") + 
    geom_text(aes(x = perim_df$names[4], y = log(perim_df$values[4])), label = round(perim_df$values[4], 0), color = "red", vjust = 0, nudge_y = 0.2) 



area_dvd_perim_df <- data.frame(names = c('CMJ030', 'PTN1', 'PTN2', 'PTN3'), 
                                values = c(mean(cmj030$adivp), mean(ptn1$adivp), mean(ptn2$adivp), mean(ptn3$adivp)))

t.test(cmj030$adivp, ptn1$adivp, alternative = "two.sided")
t.test(cmj030$adivp, ptn2$adivp, alternative = "two.sided")
t.test(cmj030$adivp, ptn3$adivp, alternative = "two.sided")

# Old Plot 
c <- ggplot(area_dvd_perim_df, aes(x = names, y = values)) +
    geom_bar(stat = "identity") + 
    labs(y = "Area / Perimeter (nm)", x = " ", title = "Average Tubule Area/Perimeter Ratio")  + 
    theme(axis.text.x = element_text(angle = 60, vjust = 0.6)) +
    geom_signif(comparisons=list(c("CMJ030", "PTN2")), annotations="***",
                y_position = 38, tip_length = 0, vjust=0.4)  +
    geom_signif(comparisons=list(c("CMJ030", "PTN3")), annotations="***",
                y_position = 36, tip_length = 0, vjust=0.4)  

#Used Plot: 
tempdf <- data.frame(PTN3 = ptn3$adivp)
tempdf$CMJ030 <- c(cmj030$adivp, rep(NA, nrow(tempdf)-length(cmj030$adivp)))     
tempdf$PTN1 <- c(ptn1$adivp, rep(NA, nrow(tempdf)-length(ptn1$adivp)))     
tempdf$PTN2 <- c(ptn2$adivp, rep(NA, nrow(tempdf)-length(ptn2$adivp)))     
new_ratio <- pivot_longer(tempdf, cols = c("PTN3","PTN2","PTN1","CMJ030"),  names_to= "Identity", values_to="Ratio")
f <- ggplot(data = new_ratio, aes(x = Identity, y = log(Ratio))) + 
    geom_boxplot() + 
    labs(y = "log[Area / Perimeter (nm)]", x = " ", title = "Tubule Area/Perimeter Ratio") + 
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=12), 
          plot.title = element_text(size = 15, face="bold")) + 
    geom_signif(comparisons=list(c("CMJ030", "PTN2")), annotations="***",
                y_position = 5, tip_length = 0, vjust=0.4)  +
    geom_signif(comparisons=list(c("CMJ030", "PTN3")), annotations="***",
                y_position = 5.1, tip_length = 0, vjust=0.4) + 
    geom_jitter(shape=16, position=position_jitter(0.1), alpha = 0.25, color = "darkgreen") + 
    geom_point(aes(x = area_dvd_perim_df$names[1], y = log(area_dvd_perim_df$values[1])), shape=20, size=5, color="red", fill="red") + 
    geom_text(aes(x = area_dvd_perim_df$names[1], y = log(area_dvd_perim_df$values[1])), label = round(area_dvd_perim_df$values[1], 0), color = "red", vjust = 0, nudge_y = 0.2) + 
    geom_point(aes(x = area_dvd_perim_df$names[2], y = log(area_dvd_perim_df$values[2])), shape=20, size=5, color="red", fill="red") + 
    geom_text(aes(x = area_dvd_perim_df$names[2], y = log(area_dvd_perim_df$values[2])), label = round(area_dvd_perim_df$values[2], 0), color = "red", vjust = 0, nudge_y = 0.2) + 
    geom_point(aes(x = area_dvd_perim_df$names[3], y = log(area_dvd_perim_df$values[3])), shape=20, size=5, color="red", fill="red") + 
    geom_text(aes(x = area_dvd_perim_df$names[3], y = log(area_dvd_perim_df$values[3])), label = round(area_dvd_perim_df$values[3], 0), color = "red", vjust = 0, nudge_y = 0.2) + 
    geom_point(aes(x = area_dvd_perim_df$names[4], y = log(area_dvd_perim_df$values[4])), shape=20, size=5, color="red", fill="red") + 
    geom_text(aes(x = area_dvd_perim_df$names[4], y = log(area_dvd_perim_df$values[4])), label = round(area_dvd_perim_df$values[4], 0), color = "red", vjust = 0, nudge_y = 0.2) 


dev.new()
# Old: 
ggarrange(a, b, c, ncol = 3, nrow = 1, labels = c("A", "B", "C"))
# New: 
ggarrange(d, e, f, ncol = 3, nrow = 1, labels = c("A", "B", "C"))



##### Shape: 

round_df <- data.frame(names = c('CMJ030', 'PTN1', 'PTN2', 'PTN3'), 
                      values = c(mean(cmj030$Round), mean(ptn1$Round), mean(ptn2$Round), mean(ptn3$Round)))

t.test(cmj030$Round, ptn1$Round, alternative = "two.sided")
t.test(cmj030$Round, ptn2$Round, alternative = "two.sided")
t.test(cmj030$Round, ptn3$Round, alternative = "two.sided") 

# Old: 
a <- ggplot(round_df, aes(x = names, y = values)) +
    geom_bar(stat = "identity") + 
    labs(y = "Roundness (0-1)", x = " ", title = "Average Tubule Roundness")  + 
    theme(axis.text.x = element_text(angle = 60, vjust = 0.6)) +
    geom_signif(comparisons=list(c("CMJ030", "PTN3")), annotations="*",
                y_position = 0.75, tip_length = 0, vjust=0.4)  

# Used: 
tempdf <- data.frame(PTN3 = ptn3$Round)
tempdf$CMJ030 <- c(cmj030$Round, rep(NA, nrow(tempdf)-length(cmj030$Round)))     
tempdf$PTN1 <- c(ptn1$Round, rep(NA, nrow(tempdf)-length(ptn1$Round)))     
tempdf$PTN2 <- c(ptn2$Round, rep(NA, nrow(tempdf)-length(ptn2$Round)))     
new_round <- pivot_longer(tempdf, cols = c("PTN3","PTN2","PTN1","CMJ030"),  names_to= "Identity", values_to="Round")
d <- ggplot(data = new_round, aes(x = Identity, y = Round)) + 
    geom_boxplot() + 
    labs(y = "Roundness (0-1)", x = " ", title = "Tubule Roundness")  + 
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=12), 
          plot.title = element_text(size = 15, face="bold")) + 
    geom_signif(comparisons=list(c("CMJ030", "PTN3")), annotations="*",
                y_position = 1.05, tip_length = 0, vjust=0.4) + 
    geom_jitter(shape=16, position=position_jitter(0.1), alpha = 0.25, color = "darkgreen") + 
    geom_point(aes(x = round_df$names[1], y = (round_df$values[1])), shape=20, size=5, color="red", fill="red") + 
    geom_text(aes(x = round_df$names[1], y = (round_df$values[1])), label = round(round_df$values[1], 2), color = "red", vjust = 0, nudge_y = 0.05) + 
    geom_point(aes(x = round_df$names[2], y = (round_df$values[2])), shape=20, size=5, color="red", fill="red") + 
    geom_text(aes(x = round_df$names[2], y = (round_df$values[2])), label = round(round_df$values[2], 2), color = "red", vjust = 0, nudge_y = 0.05) + 
    geom_point(aes(x = round_df$names[3], y = (round_df$values[3])), shape=20, size=5, color="red", fill="red") + 
    geom_text(aes(x = round_df$names[3], y = (round_df$values[3])), label = round(round_df$values[3], 2), color = "red", vjust = 0, nudge_y = 0.05) + 
    geom_point(aes(x = round_df$names[4], y = (round_df$values[4])), shape=20, size=5, color="red", fill="red") + 
    geom_text(aes(x = round_df$names[4], y = (round_df$values[4])), label = round(round_df$values[4], 2), color = "red", vjust = 0, nudge_y = 0.05) 



solid_df <- data.frame(names = c('CMJ030', 'PTN1', 'PTN2', 'PTN3'), 
                       values = c(mean(cmj030$Solidity), mean(ptn1$Solidity), mean(ptn2$Solidity), mean(ptn3$Solidity)))
t.test(cmj030$Solidity, ptn1$Solidity, alternative = "two.sided")
t.test(cmj030$Solidity, ptn2$Solidity, alternative = "two.sided")
t.test(cmj030$Solidity, ptn3$Solidity, alternative = "two.sided")

# Old: 
b <- ggplot(solid_df, aes(x = names, y = values)) +
    geom_bar(stat = "identity") + 
    labs(y = "Solidity (0-1)", x = " ", title = "Average Tubule Solidity")  + 
    theme(axis.text.x = element_text(angle = 60, vjust = 0.6)) 


# Used: 
tempdf <- data.frame(PTN3 = ptn3$Solidity)
tempdf$CMJ030 <- c(cmj030$Solidity, rep(NA, nrow(tempdf)-length(cmj030$Solidity)))     
tempdf$PTN1 <- c(ptn1$Solidity, rep(NA, nrow(tempdf)-length(ptn1$Solidity)))     
tempdf$PTN2 <- c(ptn2$Solidity, rep(NA, nrow(tempdf)-length(ptn2$Solidity)))     
new_round <- pivot_longer(tempdf, cols = c("PTN3","PTN2","PTN1","CMJ030"),  names_to= "Identity", values_to="Solidity")
e <- ggplot(data = new_round, aes(x = Identity, y = Solidity)) + 
    geom_boxplot() + 
    labs(y = "Solidity (0-1)", x = " ", title = "Tubule Solidity")  + 
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=12), 
          plot.title = element_text(size = 15, face="bold")) + 
    geom_jitter(shape=16, position=position_jitter(0.1), alpha = 0.25, color = "darkgreen") + 
    geom_point(aes(x = solid_df$names[1], y = (solid_df$values[1])), shape=20, size=5, color="red", fill="red") + 
    geom_text(aes(x = solid_df$names[1], y = (solid_df$values[1])), label = round(solid_df$values[1], 2), color = "red", vjust = 0, nudge_y = 0.05) + 
    geom_point(aes(x = solid_df$names[2], y = (solid_df$values[2])), shape=20, size=5, color="red", fill="red") + 
    geom_text(aes(x = solid_df$names[2], y = (solid_df$values[2])), label = round(solid_df$values[2], 2), color = "red", vjust = 0, nudge_y = 0.05) + 
    geom_point(aes(x = solid_df$names[3], y = (solid_df$values[3])), shape=20, size=5, color="red", fill="red") + 
    geom_text(aes(x = solid_df$names[3], y = (solid_df$values[3])), label = round(solid_df$values[3], 2), color = "red", vjust = 0, nudge_y = 0.05) + 
    geom_point(aes(x = solid_df$names[4], y = (solid_df$values[4])), shape=20, size=5, color="red", fill="red") + 
    geom_text(aes(x = solid_df$names[4], y = (solid_df$values[4])), label = round(solid_df$values[4], 2), color = "red", vjust = 0, nudge_y = 0.05) 



circ_df <- data.frame(names = c('CMJ030', 'PTN1', 'PTN2', 'PTN3'), 
                                values = c(mean(cmj030$Circ.), mean(ptn1$Circ.), mean(ptn2$Circ.), mean(ptn3$Circ.)))

t.test(cmj030$Circ., ptn1$Circ., alternative = "two.sided")
t.test(cmj030$Circ., ptn2$Circ., alternative = "two.sided")
t.test(cmj030$Circ., ptn3$Circ., alternative = "two.sided")

# Old: 
c <- ggplot(circ_df, aes(x = names, y = values)) +
    geom_bar(stat = "identity") + 
    labs(y = "Circularity (0-1)", x = " ", title = "Average Tubule Circularity")  + 
    theme(axis.text.x = element_text(angle = 60, vjust = 0.6)) 

# Used: 
tempdf <- data.frame(PTN3 = ptn3$Circ.)
tempdf$CMJ030 <- c(cmj030$Circ., rep(NA, nrow(tempdf)-length(cmj030$Circ.)))     
tempdf$PTN1 <- c(ptn1$Circ., rep(NA, nrow(tempdf)-length(ptn1$Circ.)))     
tempdf$PTN2 <- c(ptn2$Circ., rep(NA, nrow(tempdf)-length(ptn2$Circ.)))     
new_circ <- pivot_longer(tempdf, cols = c("PTN3","PTN2","PTN1","CMJ030"),  names_to= "Identity", values_to="Circ")
f <- ggplot(data = new_circ, aes(x = Identity, y = Circ)) + 
    geom_boxplot() + 
    labs(y = "Circularity (0-1)", x = " ", title = "Tubule Circularity")  + 
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=12), 
          plot.title = element_text(size = 15, face="bold")) + 
    geom_jitter(shape=16, position=position_jitter(0.1), alpha = 0.25, color = "darkgreen") + 
    geom_point(aes(x = circ_df$names[1], y = (circ_df$values[1])), shape=20, size=5, color="red", fill="red") + 
    geom_text(aes(x = circ_df$names[1], y = (circ_df$values[1])), label = round(circ_df$values[1], 2), color = "red", vjust = 0, nudge_y = 0.05) + 
    geom_point(aes(x = circ_df$names[2], y = (circ_df$values[2])), shape=20, size=5, color="red", fill="red") + 
    geom_text(aes(x = circ_df$names[2], y = (circ_df$values[2])), label = round(circ_df$values[2], 2), color = "red", vjust = 0, nudge_y = 0.05) + 
    geom_point(aes(x = circ_df$names[3], y = (circ_df$values[3])), shape=20, size=5, color="red", fill="red") + 
    geom_text(aes(x = circ_df$names[3], y = (circ_df$values[3])), label = round(circ_df$values[3], 2), color = "red", vjust = 0, nudge_y = 0.05) + 
    geom_point(aes(x = circ_df$names[4], y = (circ_df$values[4])), shape=20, size=5, color="red", fill="red") + 
    geom_text(aes(x = circ_df$names[4], y = (circ_df$values[4])), label = round(circ_df$values[4], 2), color = "red", vjust = 0, nudge_y = 0.05) 


dev.new()
# Old: 
ggarrange(a, b, c, ncol = 3, nrow = 1, labels = c("A", "B", "C"))
# New: 
ggarrange(d, e, f, ncol = 3, nrow = 1, labels = c("A", "B", "C"))




############ Pulldown Optimization: 

# 06/09 trial: 
df <- data.frame(chlorophyll = c(7197,	5487,	5142,	8, 41,	213), 
                 venus = c(194,	623,	632,	135,	1730,	1173), 
                 labels = c('CMJ030 Lysate', 'RBMP1 Lysate', 'Cre01 Lysate', 'Anti-Flag Beads', 'RBMP2 + Beads', 'Cre05 + Beads'))
df %>% slice(4, 1, 2, 5, 3, 6)
stacked_df <- cbind(df[3], stack(df[1:2])) 
ggplot(stacked_df, aes(x = labels, y = values, fill = ind)) +
    geom_bar(stat = "identity", position = position_dodge(width=0.5)) + 
    labs(y = "Fluorescent Intensity (AU)", x = " ", title = "Comparing Fluorescence in Lysates \n  and Pulldown Samples")  + 
    theme(axis.text=element_text(size=10),
          axis.text.x = element_text(angle = 60, vjust = 0.6),
          axis.title=element_text(size=15), 
          plot.title = element_text(size = 20, face="bold")) 





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
        labs(y = "Log(Fluorescence)", title = "Cre05") + 
    ylim(c(0,10))
    

ggplot(df_cre05, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    geom_label_repel(aes(label = labels, 
                         y = log(chlorophyll), 
                         size = NULL, color = NULL), nudge_y = 0.25) +  
    labs(y = "Log of Chlorophyll and Venus Expression", title = "Cre05") + 
    scale_x_discrete(aes(labels=labels))
    

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
    labs(y = "Log(Fluorescence)", title = "CMJ030") + 
    ylim(c(0,10))




b <- ggplot(df_cre01, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    geom_label_repel(aes(label = labels, 
                         y = log(chlorophyll), 
                         size = NULL, color = NULL), nudge_y = 0.25) +  
    labs(y = "Log(Fluorescence)", title = "Cre01") + 
    ylim(c(0,10))


c <- ggplot(df_rbmp1, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    geom_label_repel(aes(label = labels, 
                         y = log(chlorophyll), 
                         size = NULL, color = NULL), nudge_y = 0.25) +  
    labs(y = "Log(Fluorescence)", title = "RBMP1") + 
    ylim(c(0,10))

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
    labs(y = "Log(Fluorescence)", title = "Sonication 1: CMJ030") + 
    ylim(c(0,10))




b <- ggplot(df_cre01, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    # geom_label_repel(aes(label = labels, y = log(chlorophyll), size = NULL, color = NULL), nudge_y = 0.25) +  
    labs(y = "Log(Fluorescence)", title = "Sonication 1: Cre01") + 
    ylim(c(0,10))



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
    labs(y = "Log(Fluorescence)", title = "Sonication 2: CMJ030") + 
    ylim(c(0,10))




d <- ggplot(df_cre01, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    # geom_label_repel(aes(label = labels, y = log(chlorophyll), size = NULL, color = NULL), nudge_y = 0.25) +  
    labs(y = "Log(Fluorescence)", title = "Sonication 2: Cre01")+ 
    ylim(c(0,10))


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
    labs(y = "Log(Fluorescence)", title = "Sonication 3: CMJ030")+ 
    ylim(c(0,10))


f <- ggplot(df_cre01, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    # geom_label_repel(aes(label = labels, y = log(chlorophyll), size = NULL, color = NULL), nudge_y = 0.25) +  
    labs(y = "Log(Fluorescence)", title = "Sonication 3: Cre01")+ 
    ylim(c(0,10))

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
    labs(y = "Log(Fluorescence)", title = "Sonication 4: CMJ030")+ 
    ylim(c(0,10))


h <- ggplot(df_cre01, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    # geom_label_repel(aes(label = labels, y = log(chlorophyll), size = NULL, color = NULL), nudge_y = 0.25) +  
    labs(y = "Log(Fluorescence)", title = "Sonication 4: Cre01")+ 
    ylim(c(0,10))


dev.new()
ggarrange(a, c, e, g, b, d, f, h, ncol = 4, nrow = 2, labels = c("A", "C", "E", "G", "B", "D", "F", "H"))




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
    labs(y = "Log(Fluorescence)", title = "Sonication 5: CMJ030") + 
    ylim(c(0,10))


b <- ggplot(df_cre01, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    # geom_label_repel(aes(label = labels, y = log(chlorophyll), size = NULL, color = NULL), nudge_y = 0.25) +  
    labs(y = "Log(Fluorescence)", title = "Sonication 5: Cre01")+ 
    ylim(c(0,10))

c <- ggplot(df_rbmp1, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    # geom_label_repel(aes(label = labels, y = log(chlorophyll), size = NULL, color = NULL), nudge_y = 0.25) +  
    labs(y = "Log(Fluorescence)", title = "Sonication 5: RBMP1")+ 
    ylim(c(0,10))


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
    labs(y = "Log(Fluorescence)", title = "Sonication 2: CMJ030")+ 
    ylim(c(0,10))


e <- ggplot(df_cre01, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    # geom_label_repel(aes(label = labels, y = log(chlorophyll), size = NULL, color = NULL), nudge_y = 0.25) +  
    labs(y = "Log(Fluorescence)", title = "Sonication 2: Cre01")+ 
    ylim(c(0,10))

f <- ggplot(df_rbmp1, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    # geom_label_repel(aes(label = labels, y = log(chlorophyll), size = NULL, color = NULL), nudge_y = 0.25) +  
    labs(y = "Log(Fluorescence)", title = "Sonication 2: RBMP1")+ 
    ylim(c(0,10))


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
    labs(y = "Log(Fluorescence)", title = "Sonication 6: CMJ030")+ 
    ylim(c(0,10))


h <- ggplot(df_cre01, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    # geom_label_repel(aes(label = labels, y = log(chlorophyll), size = NULL, color = NULL), nudge_y = 0.25) +  
    labs(y = "Log(Fluorescence)", title = "Sonication 6: Cre01")+ 
    ylim(c(0,10))

i <- ggplot(df_rbmp1, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    # geom_label_repel(aes(label = labels, y = log(chlorophyll), size = NULL, color = NULL), nudge_y = 0.25) +  
    labs(y = "Log(Fluorescence)", title = "Sonication 6: RBMP1")+ 
    ylim(c(0,10))


dev.new()
ggarrange(a, d, g, b, e, h, c, f, i, ncol = 3, nrow = 3, labels = c("A", "D", "G", "B", "E", "H", "C", "F", "I"))





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
df_ratios <- data.frame(chlorophyll = chlorophyll_ratio, venus = venus_ratio, FLAG_Buffer = c("90% FLAG w/ \n CMJ030 Control", "40% FLAG","66% FLAG", "90% FLAG"))
df_plot <- df_ratios %>% pivot_longer(cols = c(chlorophyll, venus))

dev.new()
ggplot(df_plot, aes(x = FLAG_Buffer, y = value, fill = name)) +
    geom_bar(stat = "identity", position = "dodge") + 
    scale_fill_manual(values=c("#336600", "#0000FF")) + 
    labs(y = "Eluate : Post-elution Beads Fluorescence Ratio", x = "Buffer Composition") +
    theme(legend.position= 'right', 
          legend.justification='left',
          legend.direction='vertical', 
          legend.title=element_blank(), 
          axis.text=element_text(size=12),
          axis.title=element_text(size=15), 
          legend.text=element_text(size=15)) 
   



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
    labs(y = "Log(Fluorescence)", title = "CMJ030") + 
    ylim(c(0,10))



b <- ggplot(df_cre05, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    geom_label_repel(aes(label = labels, 
                         y = log(chlorophyll), 
                         size = NULL, color = NULL), nudge_y = 0.25) +  
    labs(y = "Log(Fluorescence)", title = "Cre05")+ 
    ylim(c(0,10))


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
    labs(y = "Log(Fluorescence)", title = "CMJ030") + 
    ylim(c(0,10))




d <- ggplot(df_cre05, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    geom_label_repel(aes(label = labels, 
                         y = log(chlorophyll), 
                         size = NULL, color = NULL), nudge_y = 0.25) +  
    labs(y = "Log(Fluorescence)", title = "Cre05") + 
    ylim(c(0,10))


dev.new()
plot_grid(a, c, b, d, labels = c("B", "D", "C", "E"))

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
    labs(y = "Log(Fluorescence)", title = "Cre01 30k") + 
    ylim(c(0, 10)) 


b <- ggplot(cre05, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    labs(y = "Log(Fluorescence)", title = "Cre05 30k") + 
    ylim(c(0, 10)) 

c <- ggplot(rbmp1, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    labs(y = "Log(Fluorescence)", title = "RBMP1 30k")+ 
    ylim(c(0, 10)) 

d <- ggplot(rbmp2, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    labs(y = "Log(Fluorescence)", title = "RBMP2 30k")+ 
    ylim(c(0, 10)) 

e <- ggplot(saga1, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    labs(y = "Log(Fluorescence)", title = "SAGA1 30k")+ 
    ylim(c(0, 10)) 

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
    labs(y = "Log(Fluorescence)", title = "Cre01 10K")+ 
    ylim(c(0, 10)) 

g <- ggplot(rbmp1, aes(x = 1:length(chlorophyll))) + 
    geom_line(aes(y = log(chlorophyll)),color = "Darkgreen", size = 1)+
    geom_line(aes(y = log(venus)), color = "Blue",  size = 1) + 
    theme(axis.ticks.x=element_blank())  + 
    theme(axis.title.x=element_blank())  + 
    theme(axis.text.x=element_blank())+ 
    labs(y = "Log(Fluorescence)", title = "RBMP1 10k")+ 
    ylim(c(0, 10)) 

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
                 labels = c('Pre-Filtration', 'Flow-Through', 'Post-Filtration', 'Pre-Filtration', 'Flow-Through', 'Post-Filtration','Blank'))

df$chlorophyll <- df$chlorophyll - df$chlorophyll[7]
tenK <- df %>% slice(1, 2, 3) 
tenK$legend <- "10K"
thirtyK <- df %>% slice(4, 5, 6)
thirtyK$legend <- "30K"
merged <- merge(x = tenK, y = thirtyK, all = T) %>% arrange((labels))
merged$legend <- factor(merged$legend)
merged$labels <- factor(merged$labels)

b <- ggplot(merged, aes(x = legend, y = chlorophyll, fill = factor(labels, levels = rev(levels(factor(labels)))))) +
    geom_bar(stat = "identity", position = position_dodge(width=0.8)) + 
    labs(y = "Chlorophyll Fluorescence", x = " ", title = "Membrane Retention")  + 
    theme(axis.text.x = element_text(angle = 45, vjust = 0.6)) + 
    scale_fill_brewer(palette = "Set1") +
    theme_classic() + 
    theme(legend.position= 'top', 
          legend.justification='left',
          legend.direction='horizontal', 
          legend.title=element_blank(), 
          axis.text=element_text(size=10),
          axis.title=element_text(size=12), 
          plot.title = element_text(size = 15, face="bold")) 




# 1/24: Protein amounts:

df <- data.frame(ten = c(103.20, 9, 101.10), 
                 ten_membranes = c(51, 67.30, 306), 
                 thirty = c(148, 108.90, 12.75),
                 thirty_membranes = c(79.88, 389.20, 266.80),
                 labels = c("Pre-Filtration", "Flow-Through", "Post-Filtration"))
df_long <- df %>% pivot_longer(cols = ten:thirty_membranes, names_to= "names", values_to= "values")
df_long$names <- factor(df_long$names)
df_long <- df_long %>% arrange(names, desc(labels))
levels(df_long$names) <- list('10kDa' = 'ten', '10kDa + Membranes' = 'ten_membranes', '30kDa' ='thirty', '30kDa + Membranes' = "thirty_membranes")
df$labels <- factor(df$labels)

a <- ggplot(df_long, aes(x = names, y = values, fill = factor(labels, levels = rev(levels(factor(labels)))))) +
    geom_bar(stat = "identity", position = position_dodge(width=0.8)) + 
    labs(y = "Protein Amount (ug)", x = " ", title = "FLAG Elimination Efficacy")  + 
    scale_fill_brewer(palette = "Set1") + 
    theme_classic() + 
    theme(axis.text.x = element_text(angle = 25, vjust = 0.6)) +
    theme(legend.position= 'top', 
          legend.justification='left',
          legend.direction='horizontal', 
          axis.text=element_text(size=10),
          axis.title=element_text(size=12), 
          plot.title = element_text(size = 15, face="bold"), 
          legend.title=element_blank(), )


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
    






######### Re-doing proteomic Analysis: 
dat <- read_excel("~/Desktop/Jonikas Lab /Thesis Data/Keenan_Masterfile.xlsx", sheet = 5)
x = dat$`Graph (controls)`
y = dat$`Graph (RBMP)`

smoothScatter(x=x, y=y, main = "Deeper Proteomic Data",
              xlab = "Thylakoid Relative Abundance", ylab = "Tubule Relative Abundance", cex.main = 1.5, cex.axis = 1, 
              cex.lab = 1, ylim=c(0,0.008), xlim = c(0, 0.008)) 
abline(a=0, b=1, lwd = 2)
par(new = T) 
plot(x = dat$red_control, y = dat$red_rbmp, xlab = '', ylab = '',  col = 'red', ylim=c(0,0.008), xlim = c(0, 0.008), pch = 20, alpha = 0.5, cex = 0.5)


#### Confocal Boxplots: 

probe_data <- read.csv("~/Desktop/Jonikas Lab /Thesis Data/Confocal Readout/Results2.csv")
water_data <- read.csv("~/Desktop/Jonikas Lab /Thesis Data/Confocal Readout/Results.csv")
filtered_probe <- probe_data %>% filter(Area < quantile(probe_data$Area, 0.98)) %>% select(Area)
filtered_water <- water_data %>% filter(Area < quantile(water_data$Area, 0.98)) %>% select(Area)

data <- data.frame(area = c(filtered_probe$Area,filtered_water$Area), identity = c(rep("Sonication Method 2 + Half-Pull-down", times = length(filtered_probe$Area)), rep("Water Bath Sonication + Centrifugation", times = length(filtered_water$Area))))
data$identity <- data$identity %>% factor()

dev.new()
boxplot(data$area ~ data$identity,
        col='steelblue',
        main='Membrane Fragment Size Distributions',
        xlab='Experimental Condition',
        ylab='Size of Membrane Fragments (nm^2)', 
        cex.main = 2,
        cex.axis = 1, cex.lab = 1) 
