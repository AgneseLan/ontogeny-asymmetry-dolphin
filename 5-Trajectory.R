
#===========================================================#
#                                                           #
#     CURVES AND POINTS ANALYSES - ODONTOCETE FAMILIES      #
#                                                           #
#===========================================================#

#CH.5 - Trajectory analysis

#LOAD LIBRARIES ----
#always do this first!!
library(geomorph) 
library(geiger)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(ggrepel)
library(gginnards)
library(ggphylomorpho)
library(ggfortify)
library(RColorBrewer) 
library(borealis)
library(ggthemes)
library(ggpubr)
library(ggplotify)
library(Morpho)
library(rphylopic)
library(png)
library(gridExtra)
library(phytools)
library(evomap)
library(reshape2)
library(scales)
require(grid)

#require(devtools)
#install_github("JeroenSmaers/evomap")
#devtools::install_github("wabarr/ggphylomorpho")
#devtools::install_github("aphanotus/borealis")
#devtools::install_github("kassambara/ggcorrplot")
#apropos("x") lists objects with matching part of name

#TRAJECTORY ANALYSIS ----
#Need multiple observations per group/category and all have to be represented (all categories in each group)

#First perform procD.lm to create linear model that describes what we are trying to test - shape changes at each stage (category) considering the 4 genera
fit_shape_genus_category <- procD.lm(coords ~ genus * category, iter = 999, data = gdf, RRPP = F)

#Check that there is a significant correlation
summary(fit_shape_genus_category)

#Save results to file
sink("Output/fit_shape_genus_category.txt")
summary(fit_shape_genus_category)
sink() 

#Use fit to calculate trajectories
trajectory_genera <- trajectory.analysis(fit_shape_genus_category, groups = gdf$genus, traj.pts = gdf$category, 
                                        pca = TRUE, print.progress = TRUE) 

#View results
#Magnitude differences between trajectories, standard summary - are trajectories different in length?
trajectory_genera_MD <- summary(trajectory_genera, show.trajectories = TRUE, attribute = "MD") 
trajectory_genera_MD

#Trajectory correlations -  are trajectories different in angle/direction?
trajectory_genera_TC <- summary(trajectory_genera, show.trajectories = TRUE, attribute = "TC", angle.type = "deg")
trajectory_genera_TC 

#Trajectory shape differences - are trajectories different in shape?
trajectory_genera_SD <- summary(trajectory_genera, show.trajectories = TRUE, attribute = "SD") 
trajectory_genera_SD 

#Save results to file
sink("Output/trajectory_genera.txt")
print("Magnitude difference (absolute difference between path distances) - length")
trajectory_genera_MD 

print("Correlations (angles) between trajectories - direction")
trajectory_genera_TC

print("Shape differences between trajectory vectors - shape")
trajectory_genera_SD 
sink() 

#Plot results - PCA of fitted values
trajectory_genera_plot <- plot(trajectory_genera, main = "Trajectories of growth by genus",  pch = shapes, #title and type of point to be used
                              col = c("darkgray","lightgray"), bg = c("darkgray","lightgray"), cex = 1, font.main = 2) #improve graphics
#Add line between groups
add.trajectories(trajectory_genera_plot, 
                 traj.pch = shapes, traj.col = 1, traj.lty = 1, traj.lwd = 1, traj.cex = 1.5, traj.bg = 1, 
                 start.bg = "green", end.bg = "red") #trajectory line graphics
#Add legend to see which trajectory belongs to each group
legend(x= -0.32, y = -0.15, legend = c("Delphinapterus", "Globicephala", "Lagenorhynchus", "Phocoena", "Stenella"), 
       pch =  shapes, pt.bg = 1, cex = 0.8)

##Make better PCA plot using ggplot
#Read PC scores as tibble
trajectory_genera_pcscores <- as_tibble(trajectory_genera_plot [["pc.points"]])

#Add group names and other attributes to tibble as columns
trajectory_genera_pcscores <- trajectory_genera_pcscores %>% mutate(specimens = gdf$Id, group = gdf$family, category = gdf$category,  
                                                                    genus = gdf$genus, size = gdf$size, TL = gdf$TL_100, BZW = gdf$BZW_100)
glimpse(trajectory_genera_pcscores)

#Calculate means of PC1 and PC2 at each category per group to draw trajectories
trajectory_genera_pcscores_means <- trajectory_genera_pcscores %>% group_by(genus, category) %>%
  summarise_at(vars(PC1, PC2), list(mean = mean))              #function for means, both columns
glimpse(trajectory_genera_pcscores_means)

#Rename columns so they are easier to use for plot
trajectory_genera_pcscores_means <- trajectory_genera_pcscores_means %>% rename(x = PC1_mean, y = PC2_mean)
#Add group variable for Mysticeti and Odontoceti based on genus
trajectory_genera_pcscores_means <- trajectory_genera_pcscores_means %>% 
                                   mutate(group = if_else(genus == "Delphinapterus", "Monodontidae", if_else(genus == "Phocoena", "Phocoenidae", "Delphinidae"))) %>%
                                            fill(group)
glimpse(trajectory_genera_pcscores_means)

#Nice plot
trajectory_genera_ggplot <- ggplot(trajectory_genera_pcscores, aes(x = PC1, y = PC2, shape = genus, group = genus))+
  geom_point(aes(alpha = category), size = 3, colour = "grey10", fill = "grey10")+
  geom_point(data = trajectory_genera_pcscores_means, aes(x = x, y = y, fill = genus, shape = genus, alpha = category, group = genus), colour = "grey10",
            size = 5, inherit.aes = F)+
  geom_path(data = trajectory_genera_pcscores_means, aes(x = x, y = y, colour = genus, group = genus, size = category), inherit.aes = F,
           linejoin = 'mitre', arrow = arrow(angle = 40, length = unit(0.02, "npc"), ends = "last", type = "open"), show.legend = F)+
  scale_alpha_manual(name = "Growth stage", labels =  c("Early Fetus", "Late Fetus/Neonate", "Juvenile", "Adult"), #to be ordered as they appear in tibble
                     values = c(0.3, 0.4, 0.65, 0.9))+            #legend and color adjustments
  scale_shape_manual(name = "Genus", labels = c("Delphinapterus", "Globicephala", "Lagenorhynchus", "Phocoena", "Stenella"), values = shapes)+
  scale_colour_manual(name = "Genus", labels = c("Delphinapterus", "Globicephala", "Lagenorhynchus", "Phocoena", "Stenella"), #copy from as.factor(genera)
                      values = mypalette_taxa, aesthetics = c("colour", "fill"))+
  scale_size_manual(values = c(0.7, 1, 1.2, 1.5))+
  theme_bw()+
  xlab("PC 1 (62.6%)")+ #copy this from standard trajectory plot
  ylab("PC 2 (16.13%)")+
  ggtitle("Trajectories of growth by genus")+
  guides(shape = guide_legend(label = F, title = NULL, override.aes = list(shape = NA)), 
         colour = guide_legend(label = F, title = NULL), fill = guide_legend(label = F, title = NULL), alpha = guide_legend(override.aes = list(fill = "grey10", colour = "grey10")))+
  theme(plot.title = element_text(face = "bold", hjust = 0.5), legend.text = element_text(size = 11), legend.background = element_blank(),
         legend.key = element_blank(), legend.title = element_text(size = 12, face = "bold"), legend.position = c(0.05,0.05), 
         legend.direction = "vertical", legend.justification = c(0,0))
trajectory_genera_ggplot

#Add silhouettes groups
trajectory_genera_ggplot <-   
  trajectory_genera_ggplot   + 
  add_phylopic(Delph, alpha = 1, x = -0.1, y = -0.1, ysize = 0.05, color = mypalette_taxa[1])+
  add_phylopic(Globi, alpha = 1, x = 0.1, y = -0.06, ysize = 0.035, color = mypalette_taxa[2])+
  add_phylopic(Lage, alpha = 1, x = 0.15, y = -0.01, ysize = 0.06, color = mypalette_taxa[3])+
  add_phylopic(Phoc, alpha = 1, x = -0.05, y = 0, ysize = 0.035, color = mypalette_taxa[4])+
  add_phylopic(Sten, alpha = 1, x = 0.18, y = 0.04, ysize = 0.03, color = mypalette_taxa[5])
#Visualize plot and save as PDF using menu in bar on the right
trajectory_genera_ggplot

###Heatmaps for pairwise comparison trajectories ----

#Create palette for heatmap trajectory plot
mypalette_traj <- brewer.pal(9,"Greens")
image(1:9,1, as.matrix(1:9), col = mypalette_traj,xlab="Greens (sequential)",
      ylab = "", yaxt = "n")

#Save p-values as object
trajectory_genera_length <- trajectory_genera_MD[["pairwise.tables"]][["D"]]
trajectory_genera_length_p <- trajectory_genera_MD[["pairwise.tables"]][["P"]]
trajectory_genera_direction <- trajectory_genera_TC[["pairwise.tables"]][["angle"]]
trajectory_genera_direction_p <- trajectory_genera_TC[["pairwise.tables"]][["P"]]
trajectory_genera_shape <- trajectory_genera_SD[["pairwise.tables"]][["D"]]
trajectory_genera_shape_p <- trajectory_genera_SD[["pairwise.tables"]][["P"]]

#Make list to change tables faster
trajectory_genera_list <- list(trajectory_genera_length, trajectory_genera_length_p, trajectory_genera_direction, trajectory_genera_direction_p, 
                                    trajectory_genera_shape, trajectory_genera_shape_p)

#Set correct row and col names for both
#Loop
for (l in 1:6){   #number of variable is fixed, given by parwise results
  rownames(trajectory_genera_list[[l]]) <- genera_list_short
  colnames(trajectory_genera_list[[l]]) <- genera_list_short
}

#Save only lower triangle for each
trajectory_genera_lower_tri_list <- list()

#Loop
for (l in 1:6){   #number of variable is fixed, given by parwise results
  trajectory_genera_lower_tri_list[[l]] <- get_upper_tri(trajectory_genera_list[[l]])
}

#Melt to make table in the format needed for heatmap
trajectory_genera_melt <- list()

#Loop
for (l in 1:6){   #number of variable is fixed, given by parwise results
  trajectory_genera_melt[[l]] <- melt(trajectory_genera_lower_tri_list[[l]], na.rm = TRUE)
}

#Create single data frames 
trajectory_genera_length_melt <- data.frame(trajectory_genera_melt[[1]], p = trajectory_genera_melt[[2]][[3]])
trajectory_genera_direction_melt <- data.frame(trajectory_genera_melt[[3]], p = trajectory_genera_melt[[4]][[3]])
trajectory_genera_shape_melt <- data.frame(trajectory_genera_melt[[5]], p = trajectory_genera_melt[[6]][[3]])

#Create columns where only significant values are shown
trajectory_genera_length_melt <- trajectory_genera_length_melt %>% mutate(sig_p = ifelse(p < .05, T, F),
                                                                                p_if_sig = ifelse(sig_p, p, NA),
                                                                                value_if_sig = ifelse(sig_p, value, NA)) %>%
  mutate_at(vars(starts_with("value")), list(~ round(., 3)))
trajectory_genera_direction_melt <- trajectory_genera_direction_melt %>% mutate(sig_p = ifelse(p < .05, T, F),
                                                                                  p_if_sig = ifelse(sig_p, p, NA),
                                                                                  value_if_sig = ifelse(sig_p, value, NA)) %>%
  mutate_at(vars(starts_with("value")), list(~ round(., 3)))
trajectory_genera_shape_melt <- trajectory_genera_shape_melt %>% mutate(sig_p = ifelse(p < .05, T, F),
                                                                                    p_if_sig = ifelse(sig_p, p, NA),
                                                                                    value_if_sig = ifelse(sig_p, value, NA)) %>%
  mutate_at(vars(starts_with("value")), list(~ round(., 3)))

#Heatmaps will give error if no variables significant!!
#Nice heatmap plot for each variable
trajectory_genera_length_heatmap_ggplot <- ggplot(data = trajectory_genera_length_melt, aes(Var2, Var1, fill = p_if_sig))+
  geom_tile(colour = "gray80")+
  geom_text(aes(Var2, Var1, label = value_if_sig), color = "white", size = 5) +
  scale_fill_gradient2(low = mypalette_traj[9], high = mypalette_traj[2], mid = mypalette_traj[5], #negative correlations are in blue color and positive correlations in red. 
                       midpoint = 0.03, limit = c(0.001, 0.049), space = "Lab", #scale is from min to max p-values
                       na.value =  mypalette_traj[1], name = "P-values < 0.05") + 
  theme_minimal()+ 
  coord_fixed()+
  ggtitle ("Length difference trajectories")+ 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
        axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x =  element_text(angle = 45, size = 14),
        axis.text.y =  element_text(size = 14, vjust = -0.2, margin = NULL), panel.grid.major = element_blank(),
        legend.justification = c(1, 0), legend.position = c(0.4, 0.7),  legend.direction = "horizontal",
        legend.title = element_text(size = 13), legend.text = element_text(size = 11))+
  guides(fill = guide_colorbar(barwidth = 10, barheight = 1.5,
                               title.position = "top", title.hjust = 0.5))
trajectory_genera_length_heatmap_ggplot

#Nice heatmap plot for each variable
trajectory_genera_direction_heatmap_ggplot <- ggplot(data = trajectory_genera_direction_melt, aes(Var2, Var1, fill = p_if_sig))+
  geom_tile(colour = "gray80")+
  geom_text(aes(Var2, Var1, label = value_if_sig), color = "white", size = 5) +
  scale_fill_gradient2(low = mypalette_traj[9], high = mypalette_traj[2], mid = mypalette_traj[5], #negative correlations are in blue color and positive correlations in red. 
                       midpoint = 0.03, limit = c(0.001, 0.049), space = "Lab", #scale is from min to max p-values
                       na.value =  mypalette_traj[1],  name = "P-values < 0.05") + 
  theme_minimal()+ 
  coord_fixed()+
  ggtitle ("Direction difference trajectories")+ 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
        axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x =  element_text(angle = 45, size = 14),
        axis.text.y =  element_text(size = 14, vjust = -0.2, margin = NULL), panel.grid.major = element_blank(),
        legend.justification = c(1, 0), legend.position = c(0.4, 0.7),  legend.direction = "horizontal",
        legend.title = element_text(size = 13), legend.text = element_text(size = 11))+
  guides(fill = guide_colorbar(barwidth = 10, barheight = 1.5,
                               title.position = "top", title.hjust = 0.5))
trajectory_genera_direction_heatmap_ggplot

#Nice heatmap plot for each variable
trajectory_genera_shape_heatmap_ggplot <- ggplot(data = trajectory_genera_shape_melt, aes(Var2, Var1, fill = p_if_sig))+
  geom_tile(colour = "gray80")+
  geom_text(aes(Var2, Var1, label = value_if_sig), color = "white", size = 5) +
  scale_fill_gradient2(low = mypalette_traj[9], high = mypalette_traj[2], mid = mypalette_traj[5], #negative correlations are in blue color and positive correlations in red. 
                       midpoint = 0.03, limit = c(0.001, 0.049), space = "Lab", #scale is from min to max p-values
                       na.value =  mypalette_traj[1]) + 
  theme_minimal()+ 
  coord_fixed()+
  ggtitle ("Shape difference trajectories")+ 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
        axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x =  element_text(angle = 45, size = 14),
        axis.text.y =  element_text(size = 14, vjust = -0.2, margin = NULL), panel.grid.major = element_blank(),
        legend.position = "none")
trajectory_genera_shape_heatmap_ggplot

###### 
#Next - ch. 6 - Asymmetry