
#===========================================================#
#                                                           #
#     CURVES AND POINTS ANALYSES - ODONTOCETE FAMILIES      #
#                                                           #
#===========================================================#

#CH.6 - Asymmetry

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
library(landvR)
library(paleomorph)
library(abind)
library(rray)
library(reshape2)
library(scales)
library(Rvcg)
set.seed(17)

#require(devtools)
#install_github("JeroenSmaers/evomap")
#devtools::install_github("wabarr/ggphylomorpho")
#devtools::install_github("aphanotus/borealis")
#devtools::install_github("TGuillerme/landvR")


#apropos("x") lists objects with matching part of name

#SYMMETRY ANALYSIS WITH BILAT.SYMMETRY ----
#Save list of landmarks on curves
sink("Output/curves.txt")
print(my_curves$Curves)
sink() 
#Make csv file with landpairs

#Import csv file with landpairs and transform in 2D array - use only single landmarks!
landpairs <- read_csv("Data/landpairs_LM.csv", col_names = F)
landpairs <- array(c(landpairs$X1, landpairs$X2), dim = c(nrow(landpairs),2)) #first is number of rows
landpairs

#Create object with fixed LMs numbers
fixed_LMs <- c(1:64)

#Make new shape array with only LMs before GPA
shape_array_LM <- shape_array[fixed_LMs,,]

#Analysis of bilateral symmetry
skull_symmetry_all <- bilat.symmetry(shape_array_LM, ind = Ids, object.sym = TRUE, land.pairs = landpairs) 

#Check ANOVA results - significance value of analysis - if no significant value for "side" means objects are symmetrical
summary(skull_symmetry_all)

#Plot on mesh to visualize asymmetry
plot(skull_symmetry_all) 

rgl.snapshot(filename = "Output/skull_symmetry_all.png") 

#Plot better spheres for directional asymmetry
#Highlight region where most asymmetry is - select rows
landpairs

#List of points only for different bones - useful for plots
nasals_LM <- c(LMs_nasal_l, LMs_nasal_r)
supraoccipital_LM <- c(LM_table$lm[which(LM_table$bone%in%c("supraoccipital"))])
basioccipital_LM <- c(LM_table$lm[which(LM_table$bone%in%c("basioocipital","basioccipital_lr", "basioccipital_ll"))])
maxilla_LM <- c(LM_table$lm[which(LM_table$bone%in%c("maxilla"))])
premaxilla_LM <- c(LM_table$lm[which(LM_table$bone%in%c("premaxilla"))])
condyles_LM <- c(LM_table$lm[which(LM_table$bone%in%c("condyle"))])
orbit_LM <- c(LM_table$lm[which(LM_table$bone%in%c("frontal", "jugal"))])
squamosal_LM <- c(LM_table$lm[which(LM_table$bone%in%c("squamosal"))])
palatine_LM <- c(LM_table$lm[which(LM_table$bone%in%c("palatine"))])
interparietal_LM <- c(LMs_interparietal)
exoccipital_LM <- c(LM_table$lm[which(LM_table$bone%in%c("exoccipital"))])

#Divide landpairs by side
landpairs_L <- landpairs[,1]
landpairs_R <- landpairs[,2]

#Vector of midline points
midline <- c(53,55,58,61,63,64)


#3D plot
skull_symmetry_all_points <- c(
  #first half of skull, left landmarks
  spheres3d(skull_symmetry_all[["DA.component"]][landpairs_L[landpairs_L %in% maxilla_LM],,1], radius=.005, color = mypalette_paired[8]),
  spheres3d(skull_symmetry_all[["DA.component"]][landpairs_L[landpairs_L %in% premaxilla_LM],,1], radius=.005, color = mypalette_paired[6]),
  spheres3d(skull_symmetry_all[["DA.component"]][landpairs_L[landpairs_L %in% palatine_LM],,1], radius=.005, color = mypalette_paired[7]),
  spheres3d(skull_symmetry_all[["DA.component"]][landpairs_L[landpairs_L %in% nasals_LM],,1], radius=.005, color = mypalette_paired[4]),
  spheres3d(skull_symmetry_all[["DA.component"]][landpairs_L[landpairs_L %in% orbit_LM],,1], radius=.005, color = mypalette_paired[3]),
  spheres3d(skull_symmetry_all[["DA.component"]][landpairs_L[landpairs_L %in% squamosal_LM],,1], radius=.005, color = mypalette_paired[12]),
  spheres3d(skull_symmetry_all[["DA.component"]][landpairs_L[landpairs_L %in% basioccipital_LM],,1], radius=.005, color = mypalette_paired[5]),
  spheres3d(skull_symmetry_all[["DA.component"]][landpairs_L[landpairs_L %in% interparietal_LM],,1], radius=.005, color = mypalette_paired[1]),
  spheres3d(skull_symmetry_all[["DA.component"]][landpairs_L[landpairs_L %in% supraoccipital_LM],,1], radius=.005, color = mypalette_paired[2]),
  spheres3d(skull_symmetry_all[["DA.component"]][landpairs_L[landpairs_L %in% condyles_LM],,1], radius=.005, color = mypalette_paired[10]),
  spheres3d(skull_symmetry_all[["DA.component"]][landpairs_L[landpairs_L %in% exoccipital_LM],,1], radius=.005,  color = mypalette_paired[9]),
 #second half of skull, right landmarks
 spheres3d(skull_symmetry_all[["DA.component"]][landpairs_R[landpairs_R %in% maxilla_LM],,2], radius=.005, color = mypalette_paired[8]),
 spheres3d(skull_symmetry_all[["DA.component"]][landpairs_R[landpairs_R %in% premaxilla_LM],,2], radius=.005, color = mypalette_paired[6]),
 spheres3d(skull_symmetry_all[["DA.component"]][landpairs_R[landpairs_R %in% palatine_LM],,2], radius=.005, color = mypalette_paired[7]),
 spheres3d(skull_symmetry_all[["DA.component"]][landpairs_R[landpairs_R %in% nasals_LM],,2], radius=.005, color = mypalette_paired[4]),
 spheres3d(skull_symmetry_all[["DA.component"]][landpairs_R[landpairs_R %in% orbit_LM],,2], radius=.005, color = mypalette_paired[3]),
 spheres3d(skull_symmetry_all[["DA.component"]][landpairs_R[landpairs_R %in% squamosal_LM],,2], radius=.005, color = mypalette_paired[12]),
 spheres3d(skull_symmetry_all[["DA.component"]][landpairs_R[landpairs_R %in% basioccipital_LM],,2], radius=.005, color = mypalette_paired[5]),
 spheres3d(skull_symmetry_all[["DA.component"]][landpairs_R[landpairs_R %in% interparietal_LM],,2], radius=.005, color = mypalette_paired[1]),
 spheres3d(skull_symmetry_all[["DA.component"]][landpairs_R[landpairs_R %in% supraoccipital_LM],,2], radius=.005, color = mypalette_paired[2]),
 spheres3d(skull_symmetry_all[["DA.component"]][landpairs_R[landpairs_R %in% condyles_LM],,2], radius=.005, color = mypalette_paired[10]),
 spheres3d(skull_symmetry_all[["DA.component"]][landpairs_R[landpairs_R %in% exoccipital_LM],,2], radius=.005,  color = mypalette_paired[9]),
#mean symmetric shape 
spheres3d(mshape(skull_symmetry_all[["symm.shape"]]), radius=.003, color = "black", alpha = 0.7, fastTransparency = T),
#midline points
spheres3d(mshape(skull_symmetry_all[["symm.shape"]][midline,,]), radius=.004, color = "gray80"))

rgl.snapshot(filename = "Output/skull_symmetry_all_DA.png") 

#Create object with symmetric shapes
symmetry_shapes_all <- skull_symmetry_all$symm.shape 
#Create object with asymmetric shapes
asymmetry_shapes_all <- skull_symmetry_all$asymm.shape 

#Save symmetry analysis results to file
sink("Output/skull_symmetry_all.txt")
print("if no significant value for side means objects are symmetrical")
summary(skull_symmetry_all)
sink() 

##Analysis of bilateral symmetry by category ----

#Search for rows for each species
categories_list <- levels(as.factor(classifiers$category))

#Empty object
rows_categories <- list()

#Loop
for (o in 1:length(categories_list)){
  rows_categories[[o]] <- which(gdf$category == categories_list[o])
}
names(rows_categories) <- categories_list #add names to find them more easily

#Symmetry analysis for each category
skull_symmetry_early <- bilat.symmetry(shape_array_LM[,,rows_categories[[1]]], ind = Ids[rows_categories[[1]]], 
                                       object.sym = TRUE, land.pairs = landpairs)
skull_symmetry_late_new <- bilat.symmetry(shape_array_LM[,,rows_categories[[2]]], ind = Ids[rows_categories[[2]]], 
                                          object.sym = TRUE, land.pairs = landpairs) 
skull_symmetry_immature <- bilat.symmetry(shape_array_LM[,,rows_categories[[3]]], ind = Ids[rows_categories[[3]]], 
                                          object.sym = TRUE, land.pairs = landpairs) 
skull_symmetry_adult <- bilat.symmetry(shape_array_LM[,,rows_categories[[4]]], ind = Ids[rows_categories[[4]]], 
                                       object.sym = TRUE, land.pairs = landpairs)

#Check ANOVA results - significance value of analysis - if no significant value for "side" means objects are symmetrical
summary(skull_symmetry_early)
summary(skull_symmetry_late_new)
summary(skull_symmetry_immature)
summary(skull_symmetry_adult)

#Plot
plot(skull_symmetry_early)
rgl.snapshot(filename = "Output/skull_symmetry_early.png") 

plot(skull_symmetry_late_new)
rgl.snapshot(filename = "Output/skull_symmetry_late_new.png") 

plot(skull_symmetry_immature)
rgl.snapshot(filename = "Output/skull_symmetry_immature.png") 

plot(skull_symmetry_adult)
rgl.snapshot(filename = "Output/skull_symmetry_adult.png") 

#Save symmetry analysis results to file
sink("Output/skull_symmetry_category.txt")
print("Early fetus")
summary(skull_symmetry_early)

print("Late fetus/Neonate")
summary(skull_symmetry_late_new)

print("Immature")
summary(skull_symmetry_immature)

print("Adult")
summary(skull_symmetry_adult)
sink() 

#Better plot skull symmetry
#Early
skull_symmetry_early_points <- c(
  #first half of skull, left landmarks
  spheres3d(skull_symmetry_early[["DA.component"]][landpairs_L[landpairs_L %in% maxilla_LM],,1], radius=.005, color = mypalette_paired[8]),
  spheres3d(skull_symmetry_early[["DA.component"]][landpairs_L[landpairs_L %in% premaxilla_LM],,1], radius=.005, color = mypalette_paired[6]),
  spheres3d(skull_symmetry_early[["DA.component"]][landpairs_L[landpairs_L %in% palatine_LM],,1], radius=.005, color = mypalette_paired[7]),
  spheres3d(skull_symmetry_early[["DA.component"]][landpairs_L[landpairs_L %in% nasals_LM],,1], radius=.005, color = mypalette_paired[4]),
  spheres3d(skull_symmetry_early[["DA.component"]][landpairs_L[landpairs_L %in% orbit_LM],,1], radius=.005, color = mypalette_paired[3]),
  spheres3d(skull_symmetry_early[["DA.component"]][landpairs_L[landpairs_L %in% squamosal_LM],,1], radius=.005, color = mypalette_paired[12]),
  spheres3d(skull_symmetry_early[["DA.component"]][landpairs_L[landpairs_L %in% basioccipital_LM],,1], radius=.005, color = mypalette_paired[5]),
  spheres3d(skull_symmetry_early[["DA.component"]][landpairs_L[landpairs_L %in% interparietal_LM],,1], radius=.005, color = mypalette_paired[1]),
  spheres3d(skull_symmetry_early[["DA.component"]][landpairs_L[landpairs_L %in% supraoccipital_LM],,1], radius=.005, color = mypalette_paired[2]),
  spheres3d(skull_symmetry_early[["DA.component"]][landpairs_L[landpairs_L %in% condyles_LM],,1], radius=.005, color = mypalette_paired[10]),
  spheres3d(skull_symmetry_early[["DA.component"]][landpairs_L[landpairs_L %in% exoccipital_LM],,1], radius=.005,  color = mypalette_paired[9]),
  #second half of skull, right landmarks
  spheres3d(skull_symmetry_early[["DA.component"]][landpairs_R[landpairs_R %in% maxilla_LM],,2], radius=.005, color = mypalette_paired[8]),
  spheres3d(skull_symmetry_early[["DA.component"]][landpairs_R[landpairs_R %in% premaxilla_LM],,2], radius=.005, color = mypalette_paired[6]),
  spheres3d(skull_symmetry_early[["DA.component"]][landpairs_R[landpairs_R %in% palatine_LM],,2], radius=.005, color = mypalette_paired[7]),
  spheres3d(skull_symmetry_early[["DA.component"]][landpairs_R[landpairs_R %in% nasals_LM],,2], radius=.005, color = mypalette_paired[4]),
  spheres3d(skull_symmetry_early[["DA.component"]][landpairs_R[landpairs_R %in% orbit_LM],,2], radius=.005, color = mypalette_paired[3]),
  spheres3d(skull_symmetry_early[["DA.component"]][landpairs_R[landpairs_R %in% squamosal_LM],,2], radius=.005, color = mypalette_paired[12]),
  spheres3d(skull_symmetry_early[["DA.component"]][landpairs_R[landpairs_R %in% basioccipital_LM],,2], radius=.005, color = mypalette_paired[5]),
  spheres3d(skull_symmetry_early[["DA.component"]][landpairs_R[landpairs_R %in% interparietal_LM],,2], radius=.005, color = mypalette_paired[1]),
  spheres3d(skull_symmetry_early[["DA.component"]][landpairs_R[landpairs_R %in% supraoccipital_LM],,2], radius=.005, color = mypalette_paired[2]),
  spheres3d(skull_symmetry_early[["DA.component"]][landpairs_R[landpairs_R %in% condyles_LM],,2], radius=.005, color = mypalette_paired[10]),
  spheres3d(skull_symmetry_early[["DA.component"]][landpairs_R[landpairs_R %in% exoccipital_LM],,2], radius=.005,  color = mypalette_paired[9]),
  #mean symmetric shape 
  spheres3d(mshape(skull_symmetry_early[["symm.shape"]]), radius=.003, color = "black", alpha = 0.7, fastTransparency = T),
  #midline points
  spheres3d(mshape(skull_symmetry_early[["symm.shape"]][midline,,]), radius=.004, color = "gray80"))

rgl.snapshot(filename = "Output/skull_symmetry_early_DA.png") 
#Video spin and save gif
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, dir = "Output/")

#Late fetus/newborn
skull_symmetry_late_new_points <- c(
  #first half of skull, left landmarks
  spheres3d(skull_symmetry_late_new[["DA.component"]][landpairs_L[landpairs_L %in% maxilla_LM],,1], radius=.005, color = mypalette_paired[8]),
  spheres3d(skull_symmetry_late_new[["DA.component"]][landpairs_L[landpairs_L %in% premaxilla_LM],,1], radius=.005, color = mypalette_paired[6]),
  spheres3d(skull_symmetry_late_new[["DA.component"]][landpairs_L[landpairs_L %in% palatine_LM],,1], radius=.005, color = mypalette_paired[7]),
  spheres3d(skull_symmetry_late_new[["DA.component"]][landpairs_L[landpairs_L %in% nasals_LM],,1], radius=.005, color = mypalette_paired[4]),
  spheres3d(skull_symmetry_late_new[["DA.component"]][landpairs_L[landpairs_L %in% orbit_LM],,1], radius=.005, color = mypalette_paired[3]),
  spheres3d(skull_symmetry_late_new[["DA.component"]][landpairs_L[landpairs_L %in% squamosal_LM],,1], radius=.005, color = mypalette_paired[12]),
  spheres3d(skull_symmetry_late_new[["DA.component"]][landpairs_L[landpairs_L %in% basioccipital_LM],,1], radius=.005, color = mypalette_paired[5]),
  spheres3d(skull_symmetry_late_new[["DA.component"]][landpairs_L[landpairs_L %in% interparietal_LM],,1], radius=.005, color = mypalette_paired[1]),
  spheres3d(skull_symmetry_late_new[["DA.component"]][landpairs_L[landpairs_L %in% supraoccipital_LM],,1], radius=.005, color = mypalette_paired[2]),
  spheres3d(skull_symmetry_late_new[["DA.component"]][landpairs_L[landpairs_L %in% condyles_LM],,1], radius=.005, color = mypalette_paired[10]),
  spheres3d(skull_symmetry_late_new[["DA.component"]][landpairs_L[landpairs_L %in% exoccipital_LM],,1], radius=.005,  color = mypalette_paired[9]),
  #second half of skull, right landmarks
  spheres3d(skull_symmetry_late_new[["DA.component"]][landpairs_R[landpairs_R %in% maxilla_LM],,2], radius=.005, color = mypalette_paired[8]),
  spheres3d(skull_symmetry_late_new[["DA.component"]][landpairs_R[landpairs_R %in% premaxilla_LM],,2], radius=.005, color = mypalette_paired[6]),
  spheres3d(skull_symmetry_late_new[["DA.component"]][landpairs_R[landpairs_R %in% palatine_LM],,2], radius=.005, color = mypalette_paired[7]),
  spheres3d(skull_symmetry_late_new[["DA.component"]][landpairs_R[landpairs_R %in% nasals_LM],,2], radius=.005, color = mypalette_paired[4]),
  spheres3d(skull_symmetry_late_new[["DA.component"]][landpairs_R[landpairs_R %in% orbit_LM],,2], radius=.005, color = mypalette_paired[3]),
  spheres3d(skull_symmetry_late_new[["DA.component"]][landpairs_R[landpairs_R %in% squamosal_LM],,2], radius=.005, color = mypalette_paired[12]),
  spheres3d(skull_symmetry_late_new[["DA.component"]][landpairs_R[landpairs_R %in% basioccipital_LM],,2], radius=.005, color = mypalette_paired[5]),
  spheres3d(skull_symmetry_late_new[["DA.component"]][landpairs_R[landpairs_R %in% interparietal_LM],,2], radius=.005, color = mypalette_paired[1]),
  spheres3d(skull_symmetry_late_new[["DA.component"]][landpairs_R[landpairs_R %in% supraoccipital_LM],,2], radius=.005, color = mypalette_paired[2]),
  spheres3d(skull_symmetry_late_new[["DA.component"]][landpairs_R[landpairs_R %in% condyles_LM],,2], radius=.005, color = mypalette_paired[10]),
  spheres3d(skull_symmetry_late_new[["DA.component"]][landpairs_R[landpairs_R %in% exoccipital_LM],,2], radius=.005,  color = mypalette_paired[9]),
  #mean symmetric shape 
  spheres3d(mshape(skull_symmetry_late_new[["symm.shape"]]), radius=.003, color = "black", alpha = 0.7, fastTransparency = T),
  #midline points
  spheres3d(mshape(skull_symmetry_late_new[["symm.shape"]][midline,,]), radius=.004, color = "gray80"))

rgl.snapshot(filename = "Output/skull_symmetry_late_new_DA.png") 
#Video spin and save gif
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, dir = "Output/")

#Immature
skull_symmetry_immature_points <- c(
  #first half of skull, left landmarks
  spheres3d(skull_symmetry_immature[["DA.component"]][landpairs_L[landpairs_L %in% maxilla_LM],,1], radius=.005, color = mypalette_paired[8]),
  spheres3d(skull_symmetry_immature[["DA.component"]][landpairs_L[landpairs_L %in% premaxilla_LM],,1], radius=.005, color = mypalette_paired[6]),
  spheres3d(skull_symmetry_immature[["DA.component"]][landpairs_L[landpairs_L %in% palatine_LM],,1], radius=.005, color = mypalette_paired[7]),
  spheres3d(skull_symmetry_immature[["DA.component"]][landpairs_L[landpairs_L %in% nasals_LM],,1], radius=.005, color = mypalette_paired[4]),
  spheres3d(skull_symmetry_immature[["DA.component"]][landpairs_L[landpairs_L %in% orbit_LM],,1], radius=.005, color = mypalette_paired[3]),
  spheres3d(skull_symmetry_immature[["DA.component"]][landpairs_L[landpairs_L %in% squamosal_LM],,1], radius=.005, color = mypalette_paired[12]),
  spheres3d(skull_symmetry_immature[["DA.component"]][landpairs_L[landpairs_L %in% basioccipital_LM],,1], radius=.005, color = mypalette_paired[5]),
  spheres3d(skull_symmetry_immature[["DA.component"]][landpairs_L[landpairs_L %in% interparietal_LM],,1], radius=.005, color = mypalette_paired[1]),
  spheres3d(skull_symmetry_immature[["DA.component"]][landpairs_L[landpairs_L %in% supraoccipital_LM],,1], radius=.005, color = mypalette_paired[2]),
  spheres3d(skull_symmetry_immature[["DA.component"]][landpairs_L[landpairs_L %in% condyles_LM],,1], radius=.005, color = mypalette_paired[10]),
  spheres3d(skull_symmetry_immature[["DA.component"]][landpairs_L[landpairs_L %in% exoccipital_LM],,1], radius=.005,  color = mypalette_paired[9]),
  #second half of skull, right landmarks
  spheres3d(skull_symmetry_immature[["DA.component"]][landpairs_R[landpairs_R %in% maxilla_LM],,2], radius=.005, color = mypalette_paired[8]),
  spheres3d(skull_symmetry_immature[["DA.component"]][landpairs_R[landpairs_R %in% premaxilla_LM],,2], radius=.005, color = mypalette_paired[6]),
  spheres3d(skull_symmetry_immature[["DA.component"]][landpairs_R[landpairs_R %in% palatine_LM],,2], radius=.005, color = mypalette_paired[7]),
  spheres3d(skull_symmetry_immature[["DA.component"]][landpairs_R[landpairs_R %in% nasals_LM],,2], radius=.005, color = mypalette_paired[4]),
  spheres3d(skull_symmetry_immature[["DA.component"]][landpairs_R[landpairs_R %in% orbit_LM],,2], radius=.005, color = mypalette_paired[3]),
  spheres3d(skull_symmetry_immature[["DA.component"]][landpairs_R[landpairs_R %in% squamosal_LM],,2], radius=.005, color = mypalette_paired[12]),
  spheres3d(skull_symmetry_immature[["DA.component"]][landpairs_R[landpairs_R %in% basioccipital_LM],,2], radius=.005, color = mypalette_paired[5]),
  spheres3d(skull_symmetry_immature[["DA.component"]][landpairs_R[landpairs_R %in% interparietal_LM],,2], radius=.005, color = mypalette_paired[1]),
  spheres3d(skull_symmetry_immature[["DA.component"]][landpairs_R[landpairs_R %in% supraoccipital_LM],,2], radius=.005, color = mypalette_paired[2]),
  spheres3d(skull_symmetry_immature[["DA.component"]][landpairs_R[landpairs_R %in% condyles_LM],,2], radius=.005, color = mypalette_paired[10]),
  spheres3d(skull_symmetry_immature[["DA.component"]][landpairs_R[landpairs_R %in% exoccipital_LM],,2], radius=.005,  color = mypalette_paired[9]),
  #mean symmetric shape 
  spheres3d(mshape(skull_symmetry_immature[["symm.shape"]]), radius=.003, color = "black", alpha = 0.7, fastTransparency = T),
  #midline points
  spheres3d(mshape(skull_symmetry_immature[["symm.shape"]][midline,,]), radius=.004, color = "gray80"))

rgl.snapshot(filename = "Output/skull_symmetry_immature_DA.png") 
#Video spin and save gif
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, dir = "Output/")

#Adult
skull_symmetry_adult_points <- c(
  #first half of skull, left landmarks
  spheres3d(skull_symmetry_adult[["DA.component"]][landpairs_L[landpairs_L %in% maxilla_LM],,1], radius=.005, color = mypalette_paired[8]),
  spheres3d(skull_symmetry_adult[["DA.component"]][landpairs_L[landpairs_L %in% premaxilla_LM],,1], radius=.005, color = mypalette_paired[6]),
  spheres3d(skull_symmetry_adult[["DA.component"]][landpairs_L[landpairs_L %in% palatine_LM],,1], radius=.005, color = mypalette_paired[7]),
  spheres3d(skull_symmetry_adult[["DA.component"]][landpairs_L[landpairs_L %in% nasals_LM],,1], radius=.005, color = mypalette_paired[4]),
  spheres3d(skull_symmetry_adult[["DA.component"]][landpairs_L[landpairs_L %in% orbit_LM],,1], radius=.005, color = mypalette_paired[3]),
  spheres3d(skull_symmetry_adult[["DA.component"]][landpairs_L[landpairs_L %in% squamosal_LM],,1], radius=.005, color = mypalette_paired[12]),
  spheres3d(skull_symmetry_adult[["DA.component"]][landpairs_L[landpairs_L %in% basioccipital_LM],,1], radius=.005, color = mypalette_paired[5]),
  spheres3d(skull_symmetry_adult[["DA.component"]][landpairs_L[landpairs_L %in% interparietal_LM],,1], radius=.005, color = mypalette_paired[1]),
  spheres3d(skull_symmetry_adult[["DA.component"]][landpairs_L[landpairs_L %in% supraoccipital_LM],,1], radius=.005, color = mypalette_paired[2]),
  spheres3d(skull_symmetry_adult[["DA.component"]][landpairs_L[landpairs_L %in% condyles_LM],,1], radius=.005, color = mypalette_paired[10]),
  spheres3d(skull_symmetry_adult[["DA.component"]][landpairs_L[landpairs_L %in% exoccipital_LM],,1], radius=.005,  color = mypalette_paired[9]),
  #second half of skull, right landmarks
  spheres3d(skull_symmetry_adult[["DA.component"]][landpairs_R[landpairs_R %in% maxilla_LM],,2], radius=.005, color = mypalette_paired[8]),
  spheres3d(skull_symmetry_adult[["DA.component"]][landpairs_R[landpairs_R %in% premaxilla_LM],,2], radius=.005, color = mypalette_paired[6]),
  spheres3d(skull_symmetry_adult[["DA.component"]][landpairs_R[landpairs_R %in% palatine_LM],,2], radius=.005, color = mypalette_paired[7]),
  spheres3d(skull_symmetry_adult[["DA.component"]][landpairs_R[landpairs_R %in% nasals_LM],,2], radius=.005, color = mypalette_paired[4]),
  spheres3d(skull_symmetry_adult[["DA.component"]][landpairs_R[landpairs_R %in% orbit_LM],,2], radius=.005, color = mypalette_paired[3]),
  spheres3d(skull_symmetry_adult[["DA.component"]][landpairs_R[landpairs_R %in% squamosal_LM],,2], radius=.005, color = mypalette_paired[12]),
  spheres3d(skull_symmetry_adult[["DA.component"]][landpairs_R[landpairs_R %in% basioccipital_LM],,2], radius=.005, color = mypalette_paired[5]),
  spheres3d(skull_symmetry_adult[["DA.component"]][landpairs_R[landpairs_R %in% interparietal_LM],,2], radius=.005, color = mypalette_paired[1]),
  spheres3d(skull_symmetry_adult[["DA.component"]][landpairs_R[landpairs_R %in% supraoccipital_LM],,2], radius=.005, color = mypalette_paired[2]),
  spheres3d(skull_symmetry_adult[["DA.component"]][landpairs_R[landpairs_R %in% condyles_LM],,2], radius=.005, color = mypalette_paired[10]),
  spheres3d(skull_symmetry_adult[["DA.component"]][landpairs_R[landpairs_R %in% exoccipital_LM],,2], radius=.005,  color = mypalette_paired[9]),
  #mean symmetric shape 
  spheres3d(mshape(skull_symmetry_adult[["symm.shape"]]), radius=.003, color = "black", alpha = 0.7, fastTransparency = T),
  #midline points
  spheres3d(mshape(skull_symmetry_adult[["symm.shape"]][midline,,]), radius=.004, color = "gray80"))

rgl.snapshot(filename = "Output/skull_symmetry_adult_DA.png") 
#Video spin and save gif
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, dir = "Output/")


##Analysis of bilateral symmetry by genus ----

#Empty object
rows_genera <- list()

#Loop
for (o in 1:length(genera_list)){
  rows_genera[[o]] <- which(gdf$genus == genera_list[o])
}
names(rows_genera) <- genera_list #add names to find them more easily

#Symmetry analysis for each category
skull_symmetry_delp <- bilat.symmetry(shape_array_LM[,,rows_genera[[1]]], ind = Ids[rows_genera[[1]]], 
                                      object.sym = TRUE, land.pairs = landpairs)
skull_symmetry_glob <- bilat.symmetry(shape_array_LM[,,rows_genera[[2]]], ind = Ids[rows_genera[[2]]], 
                                      object.sym = TRUE, land.pairs = landpairs) 
skull_symmetry_lage <- bilat.symmetry(shape_array_LM[,,rows_genera[[3]]], ind = Ids[rows_genera[[3]]], 
                                      object.sym = TRUE, land.pairs = landpairs) 
skull_symmetry_phoc <- bilat.symmetry(shape_array_LM[,,rows_genera[[4]]], ind = Ids[rows_genera[[4]]], 
                                      object.sym = TRUE, land.pairs = landpairs)
skull_symmetry_sten <- bilat.symmetry(shape_array_LM[,,rows_genera[[5]]], ind = Ids[rows_genera[[5]]], 
                                      object.sym = TRUE, land.pairs = landpairs)

#Check ANOVA results - significance value of analysis - if no significant value for "side" means objects are symmetrical
summary(skull_symmetry_delp)
summary(skull_symmetry_glob)
summary(skull_symmetry_lage)
summary(skull_symmetry_phoc)
summary(skull_symmetry_sten)

#Plot
plot(skull_symmetry_delp)
rgl.snapshot(filename = "Output/skull_symmetry_delp.png") 

plot(skull_symmetry_glob)
rgl.snapshot(filename = "Output/skull_symmetry_glob.png") 

plot(skull_symmetry_lage)
rgl.snapshot(filename = "Output/skull_symmetry_lage.png") 

plot(skull_symmetry_phoc)
rgl.snapshot(filename = "Output/skull_symmetry_phoc.png") 

plot(skull_symmetry_sten)
rgl.snapshot(filename = "Output/skull_symmetry_sten.png") 

#Save symmetry analysis results to file
sink("Output/skull_symmetry_genus.txt")
print(genera_list[1])
summary(skull_symmetry_delp)

print(genera_list[2])
summary(skull_symmetry_glob)

print(genera_list[3])
summary(skull_symmetry_lage)

print(genera_list[4])
summary(skull_symmetry_phoc)

print(genera_list[5])
summary(skull_symmetry_sten)
sink() 

#Plot genera symmetry symm vs asymm
#Phocoena - symm
skull_symmetry_phoc_points <- c(
  #first half of skull, left landmarks
  spheres3d(skull_symmetry_phoc[["DA.component"]][landpairs_L[landpairs_L %in% maxilla_LM],,1], radius=.005, color = mypalette_paired[8]),
  spheres3d(skull_symmetry_phoc[["DA.component"]][landpairs_L[landpairs_L %in% premaxilla_LM],,1], radius=.005, color = mypalette_paired[6]),
  spheres3d(skull_symmetry_phoc[["DA.component"]][landpairs_L[landpairs_L %in% palatine_LM],,1], radius=.005, color = mypalette_paired[7]),
  spheres3d(skull_symmetry_phoc[["DA.component"]][landpairs_L[landpairs_L %in% nasals_LM],,1], radius=.005, color = mypalette_paired[4]),
  spheres3d(skull_symmetry_phoc[["DA.component"]][landpairs_L[landpairs_L %in% orbit_LM],,1], radius=.005, color = mypalette_paired[3]),
  spheres3d(skull_symmetry_phoc[["DA.component"]][landpairs_L[landpairs_L %in% squamosal_LM],,1], radius=.005, color = mypalette_paired[12]),
  spheres3d(skull_symmetry_phoc[["DA.component"]][landpairs_L[landpairs_L %in% basioccipital_LM],,1], radius=.005, color = mypalette_paired[5]),
  spheres3d(skull_symmetry_phoc[["DA.component"]][landpairs_L[landpairs_L %in% interparietal_LM],,1], radius=.005, color = mypalette_paired[1]),
  spheres3d(skull_symmetry_phoc[["DA.component"]][landpairs_L[landpairs_L %in% supraoccipital_LM],,1], radius=.005, color = mypalette_paired[2]),
  spheres3d(skull_symmetry_phoc[["DA.component"]][landpairs_L[landpairs_L %in% condyles_LM],,1], radius=.005, color = mypalette_paired[10]),
  spheres3d(skull_symmetry_phoc[["DA.component"]][landpairs_L[landpairs_L %in% exoccipital_LM],,1], radius=.005,  color = mypalette_paired[9]),
  #second half of skull, right landmarks
  spheres3d(skull_symmetry_phoc[["DA.component"]][landpairs_R[landpairs_R %in% maxilla_LM],,2], radius=.005, color = mypalette_paired[8]),
  spheres3d(skull_symmetry_phoc[["DA.component"]][landpairs_R[landpairs_R %in% premaxilla_LM],,2], radius=.005, color = mypalette_paired[6]),
  spheres3d(skull_symmetry_phoc[["DA.component"]][landpairs_R[landpairs_R %in% palatine_LM],,2], radius=.005, color = mypalette_paired[7]),
  spheres3d(skull_symmetry_phoc[["DA.component"]][landpairs_R[landpairs_R %in% nasals_LM],,2], radius=.005, color = mypalette_paired[4]),
  spheres3d(skull_symmetry_phoc[["DA.component"]][landpairs_R[landpairs_R %in% orbit_LM],,2], radius=.005, color = mypalette_paired[3]),
  spheres3d(skull_symmetry_phoc[["DA.component"]][landpairs_R[landpairs_R %in% squamosal_LM],,2], radius=.005, color = mypalette_paired[12]),
  spheres3d(skull_symmetry_phoc[["DA.component"]][landpairs_R[landpairs_R %in% basioccipital_LM],,2], radius=.005, color = mypalette_paired[5]),
  spheres3d(skull_symmetry_phoc[["DA.component"]][landpairs_R[landpairs_R %in% interparietal_LM],,2], radius=.005, color = mypalette_paired[1]),
  spheres3d(skull_symmetry_phoc[["DA.component"]][landpairs_R[landpairs_R %in% supraoccipital_LM],,2], radius=.005, color = mypalette_paired[2]),
  spheres3d(skull_symmetry_phoc[["DA.component"]][landpairs_R[landpairs_R %in% condyles_LM],,2], radius=.005, color = mypalette_paired[10]),
  spheres3d(skull_symmetry_phoc[["DA.component"]][landpairs_R[landpairs_R %in% exoccipital_LM],,2], radius=.005,  color = mypalette_paired[9]),
  #mean symmetric shape 
  spheres3d(mshape(skull_symmetry_phoc[["symm.shape"]]), radius=.003, color = "black", alpha = 0.7, fastTransparency = T),
  #midline points
  spheres3d(mshape(skull_symmetry_phoc[["symm.shape"]][midline,,]), radius=.004, color = "gray80"))

rgl.snapshot(filename = "Output/skull_symmetry_phoc_DA.png") 
#Video spin and save gif
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, dir = "Output/")

#Lagenorhynchus - asymm
skull_symmetry_lage_points <- c(
  #first half of skull, left landmarks
  spheres3d(skull_symmetry_lage[["DA.component"]][landpairs_L[landpairs_L %in% maxilla_LM],,1], radius=.005, color = mypalette_paired[8]),
  spheres3d(skull_symmetry_lage[["DA.component"]][landpairs_L[landpairs_L %in% premaxilla_LM],,1], radius=.005, color = mypalette_paired[6]),
  spheres3d(skull_symmetry_lage[["DA.component"]][landpairs_L[landpairs_L %in% palatine_LM],,1], radius=.005, color = mypalette_paired[7]),
  spheres3d(skull_symmetry_lage[["DA.component"]][landpairs_L[landpairs_L %in% nasals_LM],,1], radius=.005, color = mypalette_paired[4]),
  spheres3d(skull_symmetry_lage[["DA.component"]][landpairs_L[landpairs_L %in% orbit_LM],,1], radius=.005, color = mypalette_paired[3]),
  spheres3d(skull_symmetry_lage[["DA.component"]][landpairs_L[landpairs_L %in% squamosal_LM],,1], radius=.005, color = mypalette_paired[12]),
  spheres3d(skull_symmetry_lage[["DA.component"]][landpairs_L[landpairs_L %in% basioccipital_LM],,1], radius=.005, color = mypalette_paired[5]),
  spheres3d(skull_symmetry_lage[["DA.component"]][landpairs_L[landpairs_L %in% interparietal_LM],,1], radius=.005, color = mypalette_paired[1]),
  spheres3d(skull_symmetry_lage[["DA.component"]][landpairs_L[landpairs_L %in% supraoccipital_LM],,1], radius=.005, color = mypalette_paired[2]),
  spheres3d(skull_symmetry_lage[["DA.component"]][landpairs_L[landpairs_L %in% condyles_LM],,1], radius=.005, color = mypalette_paired[10]),
  spheres3d(skull_symmetry_lage[["DA.component"]][landpairs_L[landpairs_L %in% exoccipital_LM],,1], radius=.005,  color = mypalette_paired[9]),
  #second half of skull, right landmarks
  spheres3d(skull_symmetry_lage[["DA.component"]][landpairs_R[landpairs_R %in% maxilla_LM],,2], radius=.005, color = mypalette_paired[8]),
  spheres3d(skull_symmetry_lage[["DA.component"]][landpairs_R[landpairs_R %in% premaxilla_LM],,2], radius=.005, color = mypalette_paired[6]),
  spheres3d(skull_symmetry_lage[["DA.component"]][landpairs_R[landpairs_R %in% palatine_LM],,2], radius=.005, color = mypalette_paired[7]),
  spheres3d(skull_symmetry_lage[["DA.component"]][landpairs_R[landpairs_R %in% nasals_LM],,2], radius=.005, color = mypalette_paired[4]),
  spheres3d(skull_symmetry_lage[["DA.component"]][landpairs_R[landpairs_R %in% orbit_LM],,2], radius=.005, color = mypalette_paired[3]),
  spheres3d(skull_symmetry_lage[["DA.component"]][landpairs_R[landpairs_R %in% squamosal_LM],,2], radius=.005, color = mypalette_paired[12]),
  spheres3d(skull_symmetry_lage[["DA.component"]][landpairs_R[landpairs_R %in% basioccipital_LM],,2], radius=.005, color = mypalette_paired[5]),
  spheres3d(skull_symmetry_lage[["DA.component"]][landpairs_R[landpairs_R %in% interparietal_LM],,2], radius=.005, color = mypalette_paired[1]),
  spheres3d(skull_symmetry_lage[["DA.component"]][landpairs_R[landpairs_R %in% supraoccipital_LM],,2], radius=.005, color = mypalette_paired[2]),
  spheres3d(skull_symmetry_lage[["DA.component"]][landpairs_R[landpairs_R %in% condyles_LM],,2], radius=.005, color = mypalette_paired[10]),
  spheres3d(skull_symmetry_lage[["DA.component"]][landpairs_R[landpairs_R %in% exoccipital_LM],,2], radius=.005,  color = mypalette_paired[9]),
  #mean symmetric shape 
  spheres3d(mshape(skull_symmetry_lage[["symm.shape"]]), radius=.003, color = "black", alpha = 0.7, fastTransparency = T),
  #midline points
  spheres3d(mshape(skull_symmetry_lage[["symm.shape"]][midline,,]), radius=.004, color = "gray80"))

rgl.snapshot(filename = "Output/skull_symmetry_lage_DA.png") 
#Video spin and save gif
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, dir = "Output/")

##Analysis of bilateral symmetry by genus and category ----

#These steps are the same for mshape loop - same order fo specimens
#1 - Select coords for each category
#Empty object
category_coords <- list()

#Loop
for (s in 1:length(rows_categories)){
  category_coords[[s]] <- gdf$coords[,,rows_categories[[s]]]
}

#2 - Select genera for each category
#Empty object
category_genera <- list()

#Loop
for (s in 1:s){
  category_genera[[s]] <- gdf$genus[rows_categories[[s]]]
}

#3 - Combine coords and genera in new gdf per category
#Empty object
gdf_category <- list()

#Loop
for (s in 1:s){
  gdf_category[[s]] <- geomorph.data.frame(coords = category_coords[[s]], genus = category_genera[[s]])
}

#4 - Select rows for each genus in each category
#Empty object
category_genus_rows_1 <- list()
category_genus_rows_2 <- list()
category_genus_rows_3 <- list()
category_genus_rows_4 <- list()

#Loop
for (k in 1:length(genera_list)){
  category_genus_rows_1[[k]] <- which(gdf_category[[1]]$genus == genera_list[k])
  category_genus_rows_2[[k]] <- which(gdf_category[[2]]$genus == genera_list[k])
  category_genus_rows_3[[k]] <- which(gdf_category[[3]]$genus == genera_list[k])
  category_genus_rows_4[[k]] <- which(gdf_category[[4]]$genus == genera_list[k])
}

#Create new shape arrays by category
shape_array_LM_1 <- shape_array_LM[,,rows_categories[[1]]]
shape_array_LM_2 <- shape_array_LM[,,rows_categories[[2]]]
shape_array_LM_3 <- shape_array_LM[,,rows_categories[[3]]]
shape_array_LM_4 <- shape_array_LM[,,rows_categories[[4]]]

#Create separate Ids for each category
Ids_1 <- Ids[rows_categories[[1]]]
Ids_2 <- Ids[rows_categories[[2]]]
Ids_3 <- Ids[rows_categories[[3]]]
Ids_4 <- Ids[rows_categories[[4]]]

#Symmetry analysis for each genus by category
skull_symmetry_1 <- list()
skull_symmetry_2 <- list()
skull_symmetry_3 <- list()
skull_symmetry_4 <- list()

#Loop
for (k in 1:length(genera_list)){
  skull_symmetry_1[[k]] <- bilat.symmetry(shape_array_LM_1[,,category_genus_rows_1[[k]]], ind = Ids_1[category_genus_rows_1[[k]]], 
                                          object.sym = TRUE, land.pairs = landpairs)
  skull_symmetry_2[[k]] <- bilat.symmetry(shape_array_LM_2[,,category_genus_rows_2[[k]]], ind = Ids_2[category_genus_rows_2[[k]]], 
                                          object.sym = TRUE, land.pairs = landpairs)
  skull_symmetry_3[[k]] <- bilat.symmetry(shape_array_LM_3[,,category_genus_rows_3[[k]]], ind = Ids_3[category_genus_rows_3[[k]]], 
                                          object.sym = TRUE, land.pairs = landpairs)
  skull_symmetry_4[[k]] <- bilat.symmetry(shape_array_LM_4[,,category_genus_rows_4[[k]]], ind = Ids_4[category_genus_rows_4[[k]]], 
                                          object.sym = TRUE, land.pairs = landpairs)
}  

names(skull_symmetry_1) <- as.list(genera_list)
names(skull_symmetry_2) <- as.list(genera_list)
names(skull_symmetry_3) <- as.list(genera_list)
names(skull_symmetry_4) <- as.list(genera_list)

#Check ANOVA results - significance value of analysis - if no significant value for "side" means objects are symmetrical
skull_symmetry_1
skull_symmetry_2
skull_symmetry_3
skull_symmetry_4

#Save symmetry analysis results to file
sink("Output/skull_symmetry_genus_category.txt")
print("Early fetus")
skull_symmetry_1

print("Late fetus/Neonate")
skull_symmetry_2

print("Immature")
skull_symmetry_3

print("Adult")
skull_symmetry_4
sink() 


#ASYMMETRY MEASURE - SYMMETRIC AND DA COMPONENTS (landVR) ----

#Create palette to represent length differences
colfunc <- colorRampPalette(c("red", "yellow", "white"))
colfunc(10)
plot(rep(1,10),col=colfunc(10),pch=19,cex=3)

colfunc_flip <- colorRampPalette(c("lightyellow3", "yellow", "goldenrod1", "darkgoldenrod1", "orange" , "darkorange", "red", "darkred"))
colfunc_flip(10)
plot(rep(1,20),col=colfunc_flip(20),pch=19,cex=3)

#Create separate Ids for replicates
Ids_reps <-  c("a", "b", "c")

#Create arrays with same specimen repeated three times
specimens_arrays <- list()

#Loop
for (q in 1:length(Ids)){
  specimens_arrays[[q]] <- rray_bind(shape_array_LM[,,q], shape_array_LM[,,q], shape_array_LM[,,q], .axis = 3) 
}  

#Calculate DA shapes for each specimen
skull_symmetry_specimens <- list()

#Loop
for (q in 1:length(Ids)){
  skull_symmetry_specimens[[q]] <- bilat.symmetry(specimens_arrays[[q]], ind = Ids_reps, 
                                                 object.sym = TRUE, land.pairs = landpairs)
} 

#Create list of landmarks per side in order with half midline
landpairs_1 <- c(landpairs_L, midline[1:3])
landpairs_2 <- c(landpairs_R, midline[4:6])

landpairs_1 <- landpairs_1[order(landpairs_1)]
landpairs_2 <- landpairs_2[order(landpairs_2)]

#Separate DA shapes
skull_symmetry_specimens_DA_L <- list()
skull_symmetry_specimens_DA_R <- list()

#Loop
for (q in 1:length(Ids)){
  skull_symmetry_specimens_DA_L[[q]] <- skull_symmetry_specimens[[q]]$DA.component[,,1]
  skull_symmetry_specimens_DA_R[[q]] <- skull_symmetry_specimens[[q]]$DA.component[,,2]
} 

#Create array with DA shapes each side
DA_shapes_L <- array(dim = c(length(fixed_LMs), 3, length(Ids)))
DA_shapes_R <- array(dim = c(length(fixed_LMs), 3, length(Ids)))

#Loop
for (q in 1:length(Ids)){
  DA_shapes_L[,,q] <-  skull_symmetry_specimens_DA_L[[q]]
  DA_shapes_R[,,q] <-  skull_symmetry_specimens_DA_R[[q]]
} 

dimnames(DA_shapes_L)[[3]] <- dimnames(symmetry_shapes_all)[[3]]
dimnames(DA_shapes_R)[[3]] <- dimnames(symmetry_shapes_all)[[3]]

#Create array with symmetric shapes
symm_shapes_specimens <- array(dim = c(length(fixed_LMs), 3, length(Ids)))

#Loop
for (q in 1:length(Ids)){
  symm_shapes_specimens[,,q] <-  skull_symmetry_specimens[[q]]$symm.shape[,,1]
} 

dimnames(symm_shapes_specimens)[[3]] <- dimnames(symmetry_shapes_all)[[3]]

spheres3d(symm_shapes_specimens[,,23], radius = 0.001, col = "black")
spheres3d(DA_shapes_L[,,23], radius = 0.005, col = "red")

rgl.snapshot(filename = "Output/skull_all_DA_L.png") 

spheres3d(DA_shapes_R[,,23], radius = 0.005, col = "blue")

rgl.snapshot(filename = "Output/skull_all_DA_R.png")

spheres3d(symm_shapes_specimens[,,23], radius = 0.005, col = "black")

rgl.snapshot(filename = "Output/skull_all_symm.png")

spheres3d(DA_shapes_L[landpairs_1,,23], radius = 0.005, col = "red")
spheres3d(DA_shapes_R[landpairs_2,,23], radius = 0.005, col = "blue")

rgl.snapshot(filename = "Output/skull_all_DA_comb.png")

#GPA alignment to make sure they all correspond
#Make array to be aligned with left and right DA shapes and symmetric shapes - endures proper alignment
DA_L_symm_shapes <- rray_bind(symm_shapes_specimens, DA_shapes_L, .axis = 1)
DA_R_symm_shapes <- rray_bind(symm_shapes_specimens, DA_shapes_R, .axis = 1)

#GPA
gpa_DA_L_symm_shapes <- gpagen(DA_L_symm_shapes)
gpa_DA_R_symm_shapes <- gpagen(DA_R_symm_shapes)

#Check plots
spheres3d(gpa_DA_L_symm_shapes$consensus[1:64,], radius = 0.003, col = "black")
spheres3d(gpa_DA_L_symm_shapes$consensus[65:128,], radius = 0.003, col = "red")

spheres3d(gpa_DA_R_symm_shapes$consensus[65:128,], radius = 0.003, col = "blue")
spheres3d(gpa_DA_R_symm_shapes$consensus[1:64,], radius = 0.003, col = "gray70")

#Save coords
coords_DA_L_symm <- gpa_DA_L_symm_shapes$coords
coords_DA_R_symm <- gpa_DA_R_symm_shapes$coords

##Use landVR to find differences between DA each side and symmetric landmarks - Linear (Euclidean) distances ----

#Pairs of landmarks, real and symmed
landpairs_DA <- cbind(symm = fixed_LMs, DA = fixed_LMs+64)

#Test on 1 pair
linear.dist(coords_DA_L_symm, landpairs_DA[1,])

#Create matrix for linear distances
distance_matrix_DA_L <-matrix(nrow = length(Ids), ncol = length(fixed_LMs))

for (r in 1:length(landpairs_DA[,1])){
  distance_matrix_DA_L[,r] <- linear.dist(coords_DA_L_symm, landpairs_DA[r,])
  
}

distance_matrix_DA_L <- t(distance_matrix_DA_L)

#Create array to be filled with distances
distance_symm_DA_L <- array(dim = c(nrow(distance_matrix_DA_L), 1, ncol(distance_matrix_DA_L))) #1 is the columns of data we need  - see output of previous line

#Loop linear distances calculations
for (w in 1:ncol(distance_matrix_DA_L)){
  distance_symm_DA_L[,,w] <- distance_matrix_DA_L[,w]
  
}
#Set dimnames as specimens
dimnames(distance_symm_DA_L)[[3]] <- dimnames(coords_DA_L)[[3]]

#Check plot
#Calculate overall mean shapes and distances

all_DA_L_mean <- mshape(coords_DA_L_symm[65:128,,])
all_symm_L_mean <- mshape(coords_DA_L_symm[1:64,,])

distance_symm_DA_L_mean <- apply(distance_symm_DA_L, c(1), mean)

#Looking at the average distances compared to average specimen
distance_DA_L_diff_mean_plot <- procrustes.var.plot(all_DA_L_mean, all_symm_L_mean , 
                                                    col.val = distance_symm_DA_L_mean, col = colfunc,
                                                    magnitude = 1, pt.size = 1, lwd = 1)
spheres3d(mshape(coords_DA_L_symm[1:64,,]), radius = 0.002, col = "gray40")

rgl.snapshot(filename = "Output/distance_diff_DA_L_all.png") 

##Repeat for R side
#Test on 1 pair
linear.dist(coords_DA_R_symm, landpairs_DA[1,])

#Create matrix for linear distances
distance_matrix_DA_R <-matrix(nrow = length(Ids), ncol = length(fixed_LMs))

for (r in 1:length(landpairs_DA[,1])){
  distance_matrix_DA_R[,r] <- linear.dist(coords_DA_R_symm, landpairs_DA[r,])
  
}

distance_matrix_DA_R <- t(distance_matrix_DA_R)

#Create array to be filled with distances
distance_symm_DA_R <- array(dim = c(nrow(distance_matrix_DA_R), 1, ncol(distance_matrix_DA_R))) #1 is the columns of data we need  - see output of previous line

#Loop linear distances calculations
for (w in 1:ncol(distance_matrix_DA_R)){
  distance_symm_DA_R[,,w] <- distance_matrix_DA_R[,w]
  
}
#Set dimnames as specimens
dimnames(distance_symm_DA_R)[[3]] <- dimnames(coords_DA_R)[[3]]

#Check plot
#Calculate overall mean

all_DA_R_mean <- mshape(coords_DA_R_symm[65:128,,])
all_symm_R_mean <- mshape(coords_DA_R_symm[1:64,,])

distance_symm_DA_R_mean <- apply(distance_symm_DA_R, c(1), mean)

#Looking at the average distances compared to average specimen
distance_DA_R_diff_mean_plot <- procrustes.var.plot(all_DA_R_mean, all_symm_R_mean , 
                                                    col.val = distance_symm_DA_R_mean, col = colfunc,
                                                    magnitude = 1, pt.size = 1, lwd = 1)
spheres3d(mshape(coords_DA_R_symm[1:64,,]), radius = 0.002, col = "gray40")


rgl.snapshot(filename = "Output/distance_diff_DA_R_all.png") 

#Test diffs between configurations
t.test(distance_symm_DA_R_mean, distance_symm_DA_L_mean)

#As there in no diff in the means between left and right, use R for analyses

####Check correlation between PC scores and asymmetry####

#Calculate regression for each component for size
distance_DA_means_specs <- apply(distance_matrix_DA_R, c(2), mean)

reg_PC1all_symm <- lm(pcscores_all$Comp1 ~ distance_DA_means_specs)
reg_PC2all_symm <- lm(pcscores_all$Comp2 ~ distance_DA_means_specs)

#View results and p-value
summary(reg_PC1all_symm)
summary(reg_PC2all_symm)
anova(reg_PC1all_symm)
anova(reg_PC2all_symm)

#Save results of significant regression to file
sink("Output/PC1-2all_symm_lm.txt")
print("PC1")
summary(reg_PC1all_symm)
anova(reg_PC1all_symm)
print("PC2")
summary(reg_PC2all_symm)
anova(reg_PC2all_symm)
sink() 

#Allometry res
reg_PC1res_symm <- lm(pcscores_res$Comp1 ~ distance_DA_means_specs)
reg_PC2res_symm <- lm(pcscores_res$Comp2 ~ distance_DA_means_specs)

#View results and p-value
summary(reg_PC1res_symm)
summary(reg_PC2res_symm)
anova(reg_PC1res_symm)
anova(reg_PC2res_symm)

#Save results of significant regression to file
sink("Output/PC1-2res_symm_lm.txt")
print("PC1")
summary(reg_PC1res_symm)
anova(reg_PC1res_symm)
print("PC2")
summary(reg_PC2res_symm)
anova(reg_PC2res_symm)
sink() 

##Plot DA shapes vs symm shapes differences in landVR - tells where variance between sides is concentrated, visual test side symmetry ----
##Look at difference in symmetry by category and genus

###Genus ----
##For each genus, find the mean of the DA data and symmetric data
#Use total range of means for plots, so that they are all compared to the same standard

#Delphinapterus
delp_DA <- coords_DA_R_symm[65:128,,rows_genera[[1]]]
delp_DA_mean <- apply(delp_DA, c(1,2), mean)

delp_symm <- coords_DA_R_symm[1:64,,rows_genera[[1]]]
delp_symm_mean <- apply(delp_symm, c(1,2), mean)

distance_symm_DA_R_delp <- distance_symm_DA_R[,,rows_genera[[1]]]
distance_symm_DA_R_delp_mean <- apply(distance_symm_DA_R_delp, c(1), mean)

#Globicephala
glob_DA <- coords_DA_R_symm[65:128,,rows_genera[[2]]]
glob_DA_mean <- apply(glob_DA, c(1,2), mean)

glob_symm <- coords_DA_R_symm[1:64,,rows_genera[[2]]]
glob_symm_mean <- apply(glob_symm, c(1,2), mean)

distance_symm_DA_R_glob <- distance_symm_DA_R[,,rows_genera[[2]]]
distance_symm_DA_R_glob_mean <- apply(distance_symm_DA_R_glob, c(1), mean)

#Lagenorhynchus
lage_DA <- coords_DA_R_symm[65:128,,rows_genera[[3]]]
lage_DA_mean <- apply(lage_DA, c(1,2), mean)

lage_symm <- coords_DA_R_symm[1:64,,rows_genera[[3]]]
lage_symm_mean <- apply(lage_symm, c(1,2), mean)

distance_symm_DA_R_lage <- distance_symm_DA_R[,,rows_genera[[3]]]
distance_symm_DA_R_lage_mean <- apply(distance_symm_DA_R_lage, c(1), mean)

#Phocoena
phoc_DA <- coords_DA_R_symm[65:128,,rows_genera[[4]]]
phoc_DA_mean <- apply(phoc_DA, c(1,2), mean)

phoc_symm <- coords_DA_R_symm[1:64,,rows_genera[[4]]]
phoc_symm_mean <- apply(phoc_symm, c(1,2), mean)

distance_symm_DA_R_phoc <- distance_symm_DA_R[,,rows_genera[[4]]]
distance_symm_DA_R_phoc_mean <- apply(distance_symm_DA_R_phoc, c(1), mean)

#Stenella
sten_DA <- coords_DA_R_symm[65:128,,rows_genera[[5]]]
sten_DA_mean <- apply(sten_DA, c(1,2), mean)

sten_symm <- coords_DA_R_symm[1:64,,rows_genera[[5]]]
sten_symm_mean <- apply(sten_symm, c(1,2), mean)

distance_symm_DA_R_sten <- distance_symm_DA_R[,,rows_genera[[5]]]
distance_symm_DA_R_sten_mean <- apply(distance_symm_DA_R_sten, c(1), mean)

###Plot landmarks colored by distance on mean shape per genus

#Make matrix with all means by genus and calculate breaks between them based on n of landmarks
genera_breaks <- c(length(fixed_LMs), (length(fixed_LMs)*2), (length(fixed_LMs)*3),
                   (length(fixed_LMs)*4), (length(fixed_LMs)*5))
  
genera_means <- c(distance_symm_DA_R_delp_mean, distance_symm_DA_R_glob_mean, distance_symm_DA_R_lage_mean, distance_symm_DA_R_phoc_mean,
  distance_symm_DA_R_sten_mean)

#4. Create colour gradient of distance values based on colour palette created earlier - all genera to get relative colors
DA_col_genera <- colfunc_flip(50)[as.numeric(cut(genera_means, breaks = 50))]

#Delphinapterus
#5. Check output by plotting points as per-point DA distance
plot(distance_symm_DA_R_delp_mean, col = DA_col_genera[1:genera_breaks[1]], cex = 2, pch = 19)

#DA symm distances on mesh
match(1, str_detect(names(findMeanSpec(gdf$coords[fixed_LMs,,rows_genera[[1]]])), specimens))

#Import mesh of mean specimen - best to reduce mesh size before importing
template_mesh_delp <- vcgImport("Data/refmesh_delp.ply")

shade3d(template_mesh_delp, col = "white", alpha = 0.5, fastTransparency = T)
spheres3d(shape_array[fixed_LMs,,18], col =  DA_col_genera[1:genera_breaks[1]], type = "s",
          radius = 4, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_delp_mesh.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_delp_movie" ,dir = "Output/")

#Globicephala
#5. Check output by plotting points as per-point DA distance
plot(distance_symm_DA_R_glob_mean, col = DA_col_genera[(genera_breaks[1]+1):genera_breaks[2]], cex = 2, pch = 19)

#DA symm distances on mesh
match(1, str_detect(names(findMeanSpec(gdf$coords[fixed_LMs,,rows_genera[[2]]])), specimens))

template_mesh_glob <- vcgImport("Data/refmesh_glob.ply")

shade3d(template_mesh_glob, col = "white", alpha = 0.5, fastTransparency = T)
spheres3d(shape_array[fixed_LMs,,22], col =  DA_col_genera[(genera_breaks[1]+1):genera_breaks[2]], type = "s",
          radius = 2, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_glob_mesh.png") 
play3d(spin3d(axis = c(0, 1, 1), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(0, 1, 1), rpm = 10), duration = 6, movie = "distance_diff_DA_glob_movie" ,dir = "Output/")

#Lagenorhynchus
#5. Check output by plotting points as per-point DA distance
plot(distance_symm_DA_R_lage_mean, col = DA_col_genera[(genera_breaks[2]+1):genera_breaks[3]], cex = 2, pch = 19)

#DA symm distances on mesh
match(1, str_detect(names(findMeanSpec(gdf$coords[fixed_LMs,,rows_genera[[3]]])), specimens))

template_mesh_lage <- vcgImport("Data/refmesh_lage.ply")

shade3d(template_mesh_lage, col = "white", alpha = 0.5, fastTransparency = T)
spheres3d(shape_array[fixed_LMs,,38], col =  DA_col_genera[(genera_breaks[2]+1):genera_breaks[3]], type = "s",
          radius = 3, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_lage_mesh.png") 
play3d(spin3d(axis = c(0, 1, 1), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(0, 1, 1), rpm = 10), duration = 6, movie = "distance_diff_DA_lage_movie" ,dir = "Output/")

#Phocoena
#5. Check output by plotting points as per-point DA distance
plot(distance_symm_DA_R_phoc_mean, col = DA_col_genera[(genera_breaks[3]+1):genera_breaks[4]], cex = 2, pch = 19)

#DA symm distances on mesh
match(1, str_detect(names(findMeanSpec(gdf$coords[fixed_LMs,,rows_genera[[4]]])), specimens))

template_mesh_phoc <- vcgImport("Data/refmesh_phoc.ply")

shade3d(template_mesh_phoc, col = "white", alpha = 0.5, fastTransparency = T)
spheres3d(shape_array[fixed_LMs,,27], col =  DA_col_genera[(genera_breaks[3]+1):genera_breaks[4]], type = "s",
          radius = 1.5, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_phoc_mesh.png") 
play3d(spin3d(axis = c(-1,-1, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(-1,-1, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_phoc_movie" ,dir = "Output/")

#Stenella
#5. Check output by plotting points as per-point DA distance
plot(distance_symm_DA_R_sten_mean, col = DA_col_genera[(genera_breaks[4]+1):genera_breaks[5]], cex = 2, pch = 19)

#DA symm distances on mesh
match(1, str_detect(names(findMeanSpec(gdf$coords[fixed_LMs,,rows_genera[[5]]])), specimens))

template_mesh_sten <- vcgImport("Data/refmesh_sten.ply")

shade3d(template_mesh_sten, col = "white", alpha = 0.5, fastTransparency = T)
spheres3d(shape_array[fixed_LMs,,29], col =  DA_col_genera[(genera_breaks[4]+1):genera_breaks[5]], type = "s",
          radius = 1.5, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_sten_mesh.png") 
play3d(spin3d(axis = c(1, 0, 1), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(0, 1, 1), rpm = 10), duration = 6, movie = "distance_diff_DA_sten_movie" ,dir = "Output/")

###Category ----
##For each category, find the mean of the DA and symmetric data

#Early
early_DA <- coords_DA_R_symm[65:128,,rows_categories[[1]]]
early_DA_mean <- apply(early_DA, c(1,2), mean)

early_symm <- coords_DA_R_symm[1:64,,rows_categories[[1]]]
early_symm_mean <- apply(early_symm, c(1,2), mean)

distance_symm_DA_R_early <- distance_symm_DA_R[,,rows_categories[[1]]]
distance_symm_DA_R_early_mean <- apply(distance_symm_DA_R_early, c(1), mean)

#Late/newborn
late_new_DA <- coords_DA_R_symm[65:128,,rows_categories[[2]]]
late_new_DA_mean <- apply(late_new_DA, c(1,2), mean)

late_new_symm <- coords_DA_R_symm[1:64,,rows_categories[[2]]]
late_new_symm_mean <- apply(late_new_symm, c(1,2), mean)

distance_symm_DA_R_late_new <- distance_symm_DA_R[,,rows_categories[[2]]]
distance_symm_DA_R_late_new_mean <- apply(distance_symm_DA_R_late_new, c(1), mean)

#Immature
immature_DA <- coords_DA_R_symm[65:128,,rows_categories[[3]]]
immature_DA_mean <- apply(immature_DA, c(1,2), mean)

immature_symm <- coords_DA_R_symm[1:64,,rows_categories[[3]]]
immature_symm_mean <- apply(immature_symm, c(1,2), mean)

distance_symm_DA_R_immature <- distance_symm_DA_R[,,rows_categories[[3]]]
distance_symm_DA_R_immature_mean <- apply(distance_symm_DA_R_immature, c(1), mean)

#Adult
adult_DA <- coords_DA_R_symm[65:128,,rows_categories[[4]]]
adult_DA_mean <- apply(adult_DA, c(1,2), mean)

adult_symm <- coords_DA_R_symm[1:64,,rows_categories[[4]]]
adult_symm_mean <- apply(adult_symm, c(1,2), mean)

distance_symm_DA_R_adult <- distance_symm_DA_R[,,rows_categories[[4]]]
distance_symm_DA_R_adult_mean <- apply(distance_symm_DA_R_adult, c(1), mean)

###Plot landmarks colored by distance on mean shape per category

#Make matrix with all means by category and calculate breaks between them based on n of landmarks
category_breaks <- c(length(fixed_LMs), (length(fixed_LMs)*2), (length(fixed_LMs)*3),
                   (length(fixed_LMs)*4))

category_means <- c(distance_symm_DA_R_early_mean, distance_symm_DA_R_late_new_mean, distance_symm_DA_R_immature_mean, 
                  distance_symm_DA_R_adult_mean)

#4. Create colour gradient of distance values based on colour palette created earlier - all categories to get relative colors
DA_col_category <- colfunc_flip(50)[as.numeric(cut(category_means, breaks = 50))]

#Early
#5. Check output by plotting points as per-point DA distance
plot(distance_symm_DA_R_early_mean, col = DA_col_category[1:category_breaks[1]], cex = 2, pch = 19)

#DA symm distances on mesh
specimens[match(1, str_detect(names(findMeanSpec(gdf$coords[fixed_LMs,,rows_categories[[1]]])), specimens))]

template_mesh_early <- vcgImport("Data/refmesh_early.ply")

shade3d(template_mesh_early, col = "white", alpha = 0.5, fastTransparency = T)
spheres3d(shape_array[fixed_LMs,,9], col =  DA_col_category[1:category_breaks[1]], type = "s",
          radius = 0.7, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_early_mesh.png") 
play3d(spin3d(axis = c(0, 0, 1), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(0, 0, 1), rpm = 10), duration = 6, movie = "distance_diff_DA_early_movie" ,dir = "Output/")

#Late/New
#5. Check output by plotting points as per-point DA distance
plot(distance_symm_DA_R_late_new_mean, col = DA_col_category[category_breaks[1]+1:category_breaks[2]], cex = 2, pch = 19)

#DA symm distances on mesh
specimens[match(1, str_detect(names(findMeanSpec(gdf$coords[fixed_LMs,,rows_categories[[2]]])), specimens))]

template_mesh_late_new <- vcgImport("Data/refmesh_late.new.ply")

shade3d(template_mesh_late_new, col = "white", alpha = 0.5, fastTransparency = T)
spheres3d(shape_array[fixed_LMs,,23], col =  DA_col_category[category_breaks[1]+1:category_breaks[2]], type = "s",
          radius = 1.5, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_late_new_mesh.png") 
play3d(spin3d(axis = c(1, 1, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1,1,0), rpm = 10), duration = 6, movie = "distance_diff_DA_late_new_movie" ,dir = "Output/")

#Immature
#5. Check output by plotting points as per-point DA distance
plot(distance_symm_DA_R_immature_mean, col = DA_col_category[category_breaks[2]+1:category_breaks[3]], cex = 2, pch = 19)

#DA symm distances on mesh
specimens[match(1, str_detect(names(findMeanSpec(gdf$coords[fixed_LMs,,rows_categories[[3]]])), specimens))]

template_mesh_immature <- vcgImport("Data/refmesh_immature.ply")

shade3d(template_mesh_immature, col = "white", alpha = 0.5, fastTransparency = T)
spheres3d(shape_array[fixed_LMs,,39], col =  DA_col_category[category_breaks[2]+1:category_breaks[3]], type = "s",
          radius = 3, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_immature_mesh.png") 
play3d(spin3d(axis = c(0, 1, 1), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(0, 1, 1), rpm = 10), duration = 6, movie = "distance_diff_DA_immature_movie" ,dir = "Output/")

#Adult
#5. Check output by plotting points as per-point DA distance
plot(distance_symm_DA_R_adult_mean, col = DA_col_category[category_breaks[3]+1:category_breaks[4]], cex = 2, pch = 19)

#DA symm distances on mesh
specimens[match(1, str_detect(names(findMeanSpec(gdf$coords[fixed_LMs,,rows_categories[[4]]])), specimens))]

template_mesh_adult <- vcgImport("Data/refmesh_adult.ply")

shade3d(template_mesh_adult, col = "white", alpha = 0.5, fastTransparency = T)
spheres3d(shape_array[fixed_LMs,,48], col =  DA_col_category[category_breaks[3]+1:category_breaks[4]], type = "s",
          radius = 4, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_adult_mesh.png") 
play3d(spin3d(axis = c(0, 1, 1), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(0, 1, 1), rpm = 10), duration = 6, movie = "distance_diff_DA_adult_movie" ,dir = "Output/")

###Genus and category ----
##For each genus in each category, find the mean of the DA and symmetric data

#Create new arrays by category
distance_symm_DA_R_1 <- distance_symm_DA_R[,,rows_categories[[1]]]
distance_symm_DA_R_2 <- distance_symm_DA_R[,,rows_categories[[2]]]
distance_symm_DA_R_3 <- distance_symm_DA_R[,,rows_categories[[3]]]
distance_symm_DA_R_4 <- distance_symm_DA_R[,,rows_categories[[4]]]

#Means
distance_symm_DA_R_1_mean <- list()
distance_symm_DA_R_2_mean <- list()
distance_symm_DA_R_3_mean <- list()
distance_symm_DA_R_4_mean <- list()

#Loop
for (k in 1:length(genera_list)){
  distance_symm_DA_R_1_mean[[k]] <- apply(distance_symm_DA_R_1[,category_genus_rows_1[[k]]], 1, mean)
  distance_symm_DA_R_2_mean[[k]] <- apply(distance_symm_DA_R_2[,category_genus_rows_2[[k]]], 1, mean)
  distance_symm_DA_R_3_mean[[k]] <- apply(distance_symm_DA_R_3[,category_genus_rows_3[[k]]], 1, mean)
  distance_symm_DA_R_4_mean[[k]] <- apply(distance_symm_DA_R_4[,category_genus_rows_4[[k]]], 1, mean)
}  

#Create coords array by category
coords_DA_1 <- coords_DA_R_symm[65:128,,rows_categories[[1]]]
coords_DA_2 <- coords_DA_R_symm[65:128,,rows_categories[[2]]]
coords_DA_3 <- coords_DA_R_symm[65:128,,rows_categories[[3]]]
coords_DA_4 <- coords_DA_R_symm[65:128,,rows_categories[[4]]]

coords_symm_1 <- coords_DA_R_symm[1:64,,rows_categories[[1]]]
coords_symm_2 <- coords_DA_R_symm[1:64,,rows_categories[[2]]]
coords_symm_3 <- coords_DA_R_symm[1:64,,rows_categories[[3]]]
coords_symm_4 <- coords_DA_R_symm[1:64,,rows_categories[[4]]]

#Means
coords_DA_mean_1 <- list()
coords_DA_mean_2  <- list()
coords_DA_mean_3  <- list()
coords_DA_mean_4 <- list()

#Loop
for (k in 1:length(genera_list)){
  coords_DA_mean_1[[k]] <- apply(coords_DA_1[,,category_genus_rows_1[[k]]], c(1,2), mean)
  coords_DA_mean_2[[k]] <- apply(coords_DA_2[,,category_genus_rows_2[[k]]], c(1,2), mean)
  coords_DA_mean_3[[k]] <- apply(coords_DA_3[,,category_genus_rows_3[[k]]], c(1,2), mean)
  coords_DA_mean_4[[k]] <- apply(coords_DA_4[,,category_genus_rows_4[[k]]], c(1,2), mean)
}  

coords_symm_mean_1 <- list()
coords_symm_mean_2  <- list()
coords_symm_mean_3  <- list()
coords_symm_mean_4 <- list()

#Loop
for (k in 1:length(genera_list)){
  coords_symm_mean_1[[k]] <- apply(coords_symm_1[,,category_genus_rows_1[[k]]], c(1,2), mean)
  coords_symm_mean_2[[k]] <- apply(coords_symm_2[,,category_genus_rows_2[[k]]], c(1,2), mean)
  coords_symm_mean_3[[k]] <- apply(coords_symm_3[,,category_genus_rows_3[[k]]], c(1,2), mean)
  coords_symm_mean_4[[k]] <- apply(coords_symm_4[,,category_genus_rows_4[[k]]], c(1,2), mean)
}  

###Plot landmarks colored by distance on mean shape per genus per category
##Plots for all categories per genus relative to mean of dataset

#Object with mean growth stages each genus, plus mean all dataset for uniform comparison of values
delp_means_4 <- c(distance_symm_DA_R_1_mean[[1]], distance_symm_DA_R_2_mean[[1]],
                  distance_symm_DA_R_3_mean[[1]], distance_symm_DA_R_4_mean[[1]], distance_symm_DA_R_mean)
glob_means_4 <- c(distance_symm_DA_R_1_mean[[2]], distance_symm_DA_R_2_mean[[2]],
                  distance_symm_DA_R_3_mean[[2]], distance_symm_DA_R_4_mean[[2]], distance_symm_DA_R_mean)
lage_means_4 <- c(distance_symm_DA_R_1_mean[[3]], distance_symm_DA_R_2_mean[[3]],
                  distance_symm_DA_R_3_mean[[3]], distance_symm_DA_R_4_mean[[3]], distance_symm_DA_R_mean)
phoc_means_4 <- c(distance_symm_DA_R_1_mean[[4]], distance_symm_DA_R_2_mean[[4]],
                  distance_symm_DA_R_3_mean[[4]], distance_symm_DA_R_4_mean[[4]], distance_symm_DA_R_mean)
sten_means_4 <- c(distance_symm_DA_R_1_mean[[5]], distance_symm_DA_R_2_mean[[5]],
                  distance_symm_DA_R_3_mean[[5]], distance_symm_DA_R_4_mean[[5]], distance_symm_DA_R_mean)

#4. Create colour gradient of distance values based on colour palette created earlier - each genus to get relative colors
DA_col_genera_delp_4 <- colfunc_flip(50)[as.numeric(cut(delp_means_4, breaks = 50))]
DA_col_genera_glob_4 <- colfunc_flip(50)[as.numeric(cut(glob_means_4, breaks = 50))]
DA_col_genera_lage_4 <- colfunc_flip(50)[as.numeric(cut(lage_means_4, breaks = 50))]
DA_col_genera_phoc_4 <- colfunc_flip(50)[as.numeric(cut(phoc_means_4, breaks = 50))]
DA_col_genera_sten_4 <- colfunc_flip(50)[as.numeric(cut(sten_means_4, breaks = 50))]

#Delphinapterus
#5. Check output by plotting points as per-point DA distance
plot(distance_symm_DA_R_1_mean[[1]], col = DA_col_genera_delp_4[1:genera_breaks[1]], cex = 2, pch = 19)
plot(distance_symm_DA_R_2_mean[[1]], col = DA_col_genera_delp_4[(genera_breaks[1]+1):genera_breaks[2]], cex = 2, pch = 19)
plot(distance_symm_DA_R_3_mean[[1]], col = DA_col_genera_delp_4[(genera_breaks[2]+1):genera_breaks[3]], cex = 2, pch = 19)
plot(distance_symm_DA_R_4_mean[[1]], col = DA_col_genera_delp_4[(genera_breaks[3]+1):genera_breaks[4]], cex = 2, pch = 19)

#DA symm distances on mesh
match(names(findMeanSpec(gdf_category[[1]]$coords[,,category_genus_rows_1[[1]]])), specimens)
match(names(findMeanSpec(gdf_category[[2]]$coords[,,category_genus_rows_2[[1]]])), specimens)
match(names(findMeanSpec(gdf_category[[3]]$coords[,,category_genus_rows_3[[1]]])), specimens) 
match(names(findMeanSpec(gdf_category[[4]]$coords[,,category_genus_rows_4[[1]]])), specimens)

#use this to print out specimens names for mesh: specimens[x]

template_mesh_delp_1 <- vcgImport("Data/refmesh_delp_1.ply")
template_mesh_delp_2 <- vcgImport("Data/refmesh_delp_2.ply")
template_mesh_delp_3 <- vcgImport("Data/refmesh_delp_3.ply")
template_mesh_delp_4 <- vcgImport("Data/refmesh_delp_4.ply")

shade3d(template_mesh_delp_1, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs,,2], col =  DA_col_genera_delp_4[1:genera_breaks[1]], type = "s",
          radius = 1, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_delp_all_mesh_1.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_delp_all_movie_1" ,dir = "Output/")

shade3d(template_mesh_delp_2, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs,,18], col =  DA_col_genera_delp_4[(genera_breaks[1]+1):genera_breaks[2]], type = "s",
          radius =4, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_delp_all_mesh_2.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_delp_all_movie_2" ,dir = "Output/")

shade3d(template_mesh_delp_3, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs,,34], col =  DA_col_genera_delp_4[(genera_breaks[2]+1):genera_breaks[3]], type = "s",
          radius = 5, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_delp_all_mesh_3.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_all_delp_movie_3" ,dir = "Output/")

shade3d(template_mesh_delp_4, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs,,48], col =  DA_col_genera_delp_4[(genera_breaks[3]+1):genera_breaks[4]], type = "s",
          radius = 5, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_delp_all_mesh_4.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_delp_all_movie_4" ,dir = "Output/")

#Globicephala
#5. Check output by plotting points as per-point DA distance
plot(distance_symm_DA_R_1_mean[[2]], col = DA_col_genera_glob_4[1:genera_breaks[1]], cex = 2, pch = 19)
plot(distance_symm_DA_R_2_mean[[2]], col = DA_col_genera_glob_4[(genera_breaks[1]+1):genera_breaks[2]], cex = 2, pch = 19)
plot(distance_symm_DA_R_3_mean[[2]], col = DA_col_genera_glob_4[(genera_breaks[2]+1):genera_breaks[3]], cex = 2, pch = 19)
plot(distance_symm_DA_R_4_mean[[2]], col = DA_col_genera_glob_4[(genera_breaks[3]+1):genera_breaks[4]], cex = 2, pch = 19)

#DA symm distances on mesh
match(names(findMeanSpec(gdf_category[[1]]$coords[,,category_genus_rows_1[[2]]])), specimens)
match(names(findMeanSpec(gdf_category[[2]]$coords[,,category_genus_rows_2[[2]]])), specimens)
match(names(findMeanSpec(gdf_category[[3]]$coords[,,category_genus_rows_3[[2]]])), specimens) 
match(names(findMeanSpec(gdf_category[[4]]$coords[,,category_genus_rows_4[[2]]])), specimens)

#use this to print out specimens names for mesh: specimens[x]

template_mesh_glob_1 <- vcgImport("Data/refmesh_glob_1.ply")
template_mesh_glob_2 <- vcgImport("Data/refmesh_glob_2.ply")
template_mesh_glob_3 <- vcgImport("Data/refmesh_glob_3.ply")
template_mesh_glob_4 <- vcgImport("Data/refmesh_glob_4.ply")

shade3d(template_mesh_glob_1, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs,,5], col =  DA_col_genera_glob_4[1:genera_breaks[1]], type = "s",
          radius = 0.8, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_glob_mesh_all_1.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_glob_all_movie_1" ,dir = "Output/")

shade3d(template_mesh_glob_2, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs,,20], col =  DA_col_genera_glob_4[(genera_breaks[1]+1):genera_breaks[2]], type = "s",
          radius = 4, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_glob_all_mesh_2.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_glob_all_movie_2" ,dir = "Output/")

shade3d(template_mesh_glob_3, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs,,36], col =  DA_col_genera_glob_4[(genera_breaks[2]+1):genera_breaks[3]], type = "s",
          radius = 4, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_glob_all_mesh_3.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_glob_all_movie_3" ,dir = "Output/")

shade3d(template_mesh_glob_4, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs,,51], col =  DA_col_genera_glob_4[(genera_breaks[3]+1):genera_breaks[4]], type = "s",
          radius = 6, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_glob_all_mesh_4.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_glob_all_movie_4" ,dir = "Output/")

#Lagenorhynchus
# 5. Check output by plotting points as per-point DA distance
plot(distance_symm_DA_R_1_mean[[3]], col = DA_col_genera_lage_4[1:genera_breaks[1]], cex = 2, pch = 19)
plot(distance_symm_DA_R_2_mean[[3]], col = DA_col_genera_lage_4[(genera_breaks[1]+1):genera_breaks[2]], cex = 2, pch = 19)
plot(distance_symm_DA_R_3_mean[[3]], col = DA_col_genera_lage_4[(genera_breaks[2]+1):genera_breaks[3]], cex = 2, pch = 19)
plot(distance_symm_DA_R_4_mean[[3]], col = DA_col_genera_lage_4[(genera_breaks[3]+1):genera_breaks[4]], cex = 2, pch = 19)

#DA symm distances on mesh
match(names(findMeanSpec(gdf_category[[1]]$coords[,,category_genus_rows_1[[3]]])), specimens)
match(names(findMeanSpec(gdf_category[[2]]$coords[,,category_genus_rows_2[[3]]])), specimens)
match(names(findMeanSpec(gdf_category[[3]]$coords[,,category_genus_rows_3[[3]]])), specimens) 
match(names(findMeanSpec(gdf_category[[4]]$coords[,,category_genus_rows_4[[3]]])), specimens)

#use this to print out specimens names for mesh: specimens[x]

template_mesh_lage_1 <- vcgImport("Data/refmesh_lage_1.ply")
template_mesh_lage_2 <- vcgImport("Data/refmesh_lage_2.ply")
template_mesh_lage_3 <- vcgImport("Data/refmesh_lage_3.ply")
template_mesh_lage_4 <- vcgImport("Data/refmesh_lage_4.ply")

shade3d(template_mesh_lage_1, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs,,7], col =  DA_col_genera_lage_4[1:genera_breaks[1]], type = "s",
          radius = 1, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_lage_all_mesh_1.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_lage_all_movie_1" ,dir = "Output/")

shade3d(template_mesh_lage_2, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs,,24], col =  DA_col_genera_lage_4[(genera_breaks[1]+1):genera_breaks[2]], type = "s",
          radius = 2, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_lage_all_mesh_2.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_lage_all_movie_2" ,dir = "Output/")

shade3d(template_mesh_lage_3, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs,,38], col =  DA_col_genera_lage_4[(genera_breaks[2]+1):genera_breaks[3]], type = "s",
          radius = 4, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_lage_all_mesh_3.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_lage_all_movie_3" ,dir = "Output/")

shade3d(template_mesh_lage_4, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs,,53], col =  DA_col_genera_lage_4[(genera_breaks[3]+1):genera_breaks[4]], type = "s",
          radius = 4, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_lage_all_mesh_4.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_lage_all_movie_4" ,dir = "Output/")

#Phocoena
#5. Check output by plotting points as per-point DA distance
plot(distance_symm_DA_R_1_mean[[4]], col = DA_col_genera_phoc_4[1:genera_breaks[1]], cex = 2, pch = 19)
plot(distance_symm_DA_R_2_mean[[4]], col = DA_col_genera_phoc_4[(genera_breaks[1]+1):genera_breaks[2]], cex = 2, pch = 19)
plot(distance_symm_DA_R_3_mean[[4]], col = DA_col_genera_phoc_4[(genera_breaks[2]+1):genera_breaks[3]], cex = 2, pch = 19)
plot(distance_symm_DA_R_4_mean[[4]], col = DA_col_genera_phoc_4[(genera_breaks[3]+1):genera_breaks[4]], cex = 2, pch = 19)

#DA symm distances on mesh
match(names(findMeanSpec(gdf_category[[1]]$coords[,,category_genus_rows_1[[4]]])), specimens)
match(names(findMeanSpec(gdf_category[[2]]$coords[,,category_genus_rows_2[[4]]])), specimens)
match(names(findMeanSpec(gdf_category[[3]]$coords[,,category_genus_rows_3[[4]]])), specimens) 
match(names(findMeanSpec(gdf_category[[4]]$coords[,,category_genus_rows_4[[4]]])), specimens)

#use this to print out specimens names for mesh: specimens[x]

template_mesh_phoc_1 <- vcgImport("Data/refmesh_phoc_1.ply")
template_mesh_phoc_2 <- vcgImport("Data/refmesh_phoc_2.ply")
template_mesh_phoc_3 <- vcgImport("Data/refmesh_phoc_3.ply")
template_mesh_phoc_4 <- vcgImport("Data/refmesh_phoc_4.ply")

shade3d(template_mesh_phoc_1, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs,,9], col =  DA_col_genera_phoc_4[1:genera_breaks[1]], type = "s",
          radius = 0.8, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_phoc_all_mesh_1.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_phoc_all_movie_1" ,dir = "Output/")

shade3d(template_mesh_phoc_2, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs,,27], col =  DA_col_genera_phoc_4[(genera_breaks[1]+1):genera_breaks[2]], type = "s",
          radius = 2, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_phoc_all_mesh_2.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_phoc_all_movie_2" ,dir = "Output/")

shade3d(template_mesh_phoc_3, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs,,42], col =  DA_col_genera_phoc_4[(genera_breaks[2]+1):genera_breaks[3]], type = "s",
          radius = 2.5, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_phoc_all_mesh_3.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_phoc_all_movie_3" ,dir = "Output/")

shade3d(template_mesh_phoc_4, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs,,54], col =  DA_col_genera_phoc_4[(genera_breaks[3]+1):genera_breaks[4]], type = "s",
          radius = 3, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_phoc_all_mesh_4.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_phoc_all_movie_4" ,dir = "Output/")

#Stenella
#5. Check output by plotting points as per-point DA distance
plot(distance_symm_DA_R_1_mean[[5]], col = DA_col_genera_sten_4[1:genera_breaks[1]], cex = 2, pch = 19)
plot(distance_symm_DA_R_2_mean[[5]], col = DA_col_genera_sten_4[(genera_breaks[1]+1):genera_breaks[2]], cex = 2, pch = 19)
plot(distance_symm_DA_R_3_mean[[5]], col = DA_col_genera_sten_4[(genera_breaks[2]+1):genera_breaks[3]], cex = 2, pch = 19)
plot(distance_symm_DA_R_4_mean[[5]], col = DA_col_genera_sten_4[(genera_breaks[3]+1):genera_breaks[4]], cex = 2, pch = 19)

#DA symm distances on mesh
match(names(findMeanSpec(gdf_category[[1]]$coords[,,category_genus_rows_1[[5]]])), specimens)
match(names(findMeanSpec(gdf_category[[2]]$coords[,,category_genus_rows_2[[5]]])), specimens)
match(names(findMeanSpec(gdf_category[[3]]$coords[,,category_genus_rows_3[[5]]])), specimens) 
match(names(findMeanSpec(gdf_category[[4]]$coords[,,category_genus_rows_4[[5]]])), specimens)

#use this to print out specimens names for mesh: specimens[x]

template_mesh_sten_1 <- vcgImport("Data/refmesh_sten_1.ply")
template_mesh_sten_2 <- vcgImport("Data/refmesh_sten_2.ply")
template_mesh_sten_3 <- vcgImport("Data/refmesh_sten_3.ply")
template_mesh_sten_4 <- vcgImport("Data/refmesh_sten_4.ply")

shade3d(template_mesh_sten_1, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs,,15], col =  DA_col_genera_sten_4[1:genera_breaks[1]], type = "s",
          radius = 0.8, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_sten_all_mesh_1.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_sten_all_movie_1" ,dir = "Output/")

shade3d(template_mesh_sten_2, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs,,29], col =  DA_col_genera_sten_4[(genera_breaks[1]+1):genera_breaks[2]], type = "s",
          radius = 2, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_sten_all_mesh_2.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_sten_all_movie_2" ,dir = "Output/")

shade3d(template_mesh_sten_3, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs,,46], col =  DA_col_genera_sten_4[(genera_breaks[2]+1):genera_breaks[3]], type = "s",
          radius = 2.5, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_sten_all_mesh_3.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_sten_all_movie_3" ,dir = "Output/")

shade3d(template_mesh_sten_4, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs,,58], col =  DA_col_genera_sten_4[(genera_breaks[3]+1):genera_breaks[4]], type = "s",
          radius = 3, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_sten_all_mesh_4.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_sten_all_movie_4" ,dir = "Output/")

####Save mesh with plotted landmarks####
#Any dult specimen will work
shade3d(template_mesh_lage_4, col = "white")
spheres3d(shape_array[fixed_LMs,,53], col =  "firebrick", type = "s",
          radius = 3.5, aspect = T, main = "mean",axes = F, main = F, fov = 0)
spheres3d(shape_array[-fixed_LMs,,53], col =  "tomato", type = "s",
          radius = 2.5, aspect = T, main = "mean",axes = F, main = F, fov = 0)
text3d(shape_array_LM[,1,53], shape_array_LM[,2,53], shape_array_LM[,3,53], texts = fixed_LMs, pos = 2, offset = 1.5, font = 1, cex = 1.2)
#pop3d() to get rid of text layer and apply another one depending on orientation
text3d(shape_array_LM[,1,53], shape_array_LM[,2,53], shape_array_LM[,3,53], texts = fixed_LMs, pos = 3, offset = 1, font = 1, cex = 1.2)

rgl.snapshot(filename = "Output/landmarks_dorsal.png") 
rgl.snapshot(filename = "Output/landmarks_lateral1.png") 
rgl.snapshot(filename = "Output/landmarks_lateral2.png") 
rgl.snapshot(filename = "Output/landmarks_ventral.png") 
rgl.snapshot(filename = "Output/landmarks_posterior.png") 
rgl.snapshot(filename = "Output/landmarks_anterior.png") 

####Test significance difference between two sides linear distances - entire dataset and by genus/category ----
#Use the means

#Test difference between sides in entire dataset - is the variance on the right higher than on the left+midline ?
symm_DA_test <- t.test(distance_symm_DA_R_mean[landpairs_L], distance_symm_DA_R_mean[landpairs_R])

#Summary with p-value
symm_DA_test

#Compare categories
symm_DA_test_late_new_early <- t.test(distance_symm_DA_R_late_new_mean, distance_symm_DA_R_early_mean)

symm_DA_test_late_new_early

symm_DA_test_late_new_immature <- t.test(distance_symm_DA_R_late_new_mean, distance_symm_DA_R_immature_mean)

symm_DA_test_late_new_immature

symm_DA_test_immature_adult <- t.test(distance_symm_DA_R_immature_mean, distance_symm_DA_R_adult_mean)

symm_DA_test_immature_adult

#Compare between categories of same genus
#Phocoena stages
symm_DA_test_phoc_1_2 <- t.test(distance_symm_DA_R_1_mean[[4]], distance_symm_DA_R_2_mean[[4]])

symm_DA_test_phoc_1_2

symm_DA_test_phoc_2_3 <- t.test(distance_symm_DA_R_2_mean[[4]], distance_symm_DA_R_3_mean[[4]])

symm_DA_test_phoc_2_3

symm_DA_test_phoc_3_4 <- t.test(distance_symm_DA_R_3_mean[[4]], distance_symm_DA_R_4_mean[[4]])

symm_DA_test_phoc_3_4

#Stenella stages
symm_DA_test_sten_1_2 <- t.test(distance_symm_DA_R_1_mean[[5]], distance_symm_DA_R_2_mean[[5]])

symm_DA_test_sten_1_2

symm_DA_test_sten_2_3 <- t.test(distance_symm_DA_R_2_mean[[5]], distance_symm_DA_R_3_mean[[5]])

symm_DA_test_sten_2_3

symm_DA_test_sten_3_4 <- t.test(distance_symm_DA_R_3_mean[[5]], distance_symm_DA_R_4_mean[[5]])

symm_DA_test_sten_3_4

#Delphinapterus stages
symm_DA_test_delp_1_2 <- t.test(distance_symm_DA_R_1_mean[[1]], distance_symm_DA_R_2_mean[[1]])

symm_DA_test_delp_1_2

symm_DA_test_delp_2_3 <- t.test(distance_symm_DA_R_2_mean[[1]], distance_symm_DA_R_3_mean[[1]])

symm_DA_test_delp_2_3

symm_DA_test_delp_3_4 <- t.test(distance_symm_DA_R_3_mean[[1]], distance_symm_DA_R_4_mean[[1]])

symm_DA_test_delp_3_4

#Lagenorhynchus stages
symm_DA_test_lage_1_2 <- t.test(distance_symm_DA_R_1_mean[[3]], distance_symm_DA_R_2_mean[[3]])

symm_DA_test_lage_1_2

symm_DA_test_lage_2_3 <- t.test(distance_symm_DA_R_2_mean[[3]], distance_symm_DA_R_3_mean[[3]])

symm_DA_test_lage_2_3

symm_DA_test_lage_3_4 <- t.test(distance_symm_DA_R_3_mean[[3]], distance_symm_DA_R_4_mean[[3]])

symm_DA_test_lage_3_4

#Globicephala stages
symm_DA_test_glob_1_2 <- t.test(distance_symm_DA_R_1_mean[[2]], distance_symm_DA_R_2_mean[[2]])

symm_DA_test_glob_1_2

symm_DA_test_glob_2_3 <- t.test(distance_symm_DA_R_2_mean[[2]], distance_symm_DA_R_3_mean[[2]])

symm_DA_test_glob_2_3

symm_DA_test_glob_3_4 <- t.test(distance_symm_DA_R_3_mean[[2]], distance_symm_DA_R_4_mean[[2]])
symm_DA_test_glob_3_4

#Save results t-tests distance to file
sink("Output/distance_symm_DA_R_tests.txt")
print("Entire dataset")
symm_DA_test

print("By category")
print(c(categories_list[2], categories_list[1]))
symm_DA_test_late_new_early
print(c(categories_list[2], categories_list[3]))
symm_DA_test_late_new_immature
print(c(categories_list[3], categories_list[4]))
symm_DA_test_immature_adult

print("By genus per category")
print("Delphinapterus stages")
print(c(categories_list[1], categories_list[2]))
symm_DA_test_delp_1_2
print(c(categories_list[2], categories_list[3]))
symm_DA_test_delp_2_3
print(c(categories_list[3], categories_list[4]))
symm_DA_test_delp_3_4
print("Globicephala stages")
print(c(categories_list[1], categories_list[2]))
symm_DA_test_glob_1_2
print(c(categories_list[2], categories_list[3]))
symm_DA_test_glob_2_3
print(c(categories_list[3], categories_list[4]))
symm_DA_test_glob_3_4
print("Lagenorhynchus stages")
print(c(categories_list[1], categories_list[2]))
symm_DA_test_lage_1_2
print(c(categories_list[2], categories_list[3]))
symm_DA_test_lage_2_3
print(c(categories_list[3], categories_list[4]))
symm_DA_test_lage_3_4
print("Phocoena stages")
print(c(categories_list[1], categories_list[2]))
symm_DA_test_phoc_1_2
print(c(categories_list[2], categories_list[3]))
symm_DA_test_phoc_2_3
print(c(categories_list[3], categories_list[4]))
symm_DA_test_phoc_3_4
print("Stenella stages")
print(c(categories_list[1], categories_list[2]))
symm_DA_test_sten_1_2
print(c(categories_list[2], categories_list[3]))
symm_DA_test_sten_2_3
print(c(categories_list[3], categories_list[4]))
symm_DA_test_sten_3_4

sink() 

####Line plot mean distances per genus per stage ----

#Create vector with means per stage per genus
distance_diff_DA_means_delp <- c(symm_DA_test_delp_1_2[["estimate"]][["mean of x"]], symm_DA_test_delp_1_2[["estimate"]][["mean of y"]],
                                 symm_DA_test_delp_3_4[["estimate"]][["mean of x"]], symm_DA_test_delp_3_4[["estimate"]][["mean of y"]])
distance_diff_DA_means_glob <- c(symm_DA_test_glob_1_2[["estimate"]][["mean of x"]], symm_DA_test_glob_1_2[["estimate"]][["mean of y"]],
                                 symm_DA_test_glob_3_4[["estimate"]][["mean of x"]], symm_DA_test_glob_3_4[["estimate"]][["mean of y"]])
distance_diff_DA_means_lage <- c(symm_DA_test_lage_1_2[["estimate"]][["mean of x"]], symm_DA_test_lage_1_2[["estimate"]][["mean of y"]],
                                 symm_DA_test_lage_3_4[["estimate"]][["mean of x"]], symm_DA_test_lage_3_4[["estimate"]][["mean of y"]])
distance_diff_DA_means_phoc <- c(symm_DA_test_phoc_1_2[["estimate"]][["mean of x"]], symm_DA_test_phoc_1_2[["estimate"]][["mean of y"]],
                                 symm_DA_test_phoc_3_4[["estimate"]][["mean of x"]], symm_DA_test_phoc_3_4[["estimate"]][["mean of y"]])
distance_diff_DA_means_sten <- c(symm_DA_test_sten_1_2[["estimate"]][["mean of x"]], symm_DA_test_sten_1_2[["estimate"]][["mean of y"]],
                                 symm_DA_test_sten_3_4[["estimate"]][["mean of x"]], symm_DA_test_sten_3_4[["estimate"]][["mean of y"]])


distance_diff_DA_means_genera <- c(distance_diff_DA_means_delp, distance_diff_DA_means_glob, distance_diff_DA_means_lage,
                                   distance_diff_DA_means_phoc, distance_diff_DA_means_sten)

genera_categories_rep <- list()

#Loop for genera and categories
for (t in 1:length(genera_list)){
  genera_categories_rep[[t]] <- rep(genera_list[t], times = length(categories_list)) 
 }

#Create table for plots and pairwise analysis for heatmap
distance_diff_DA_table <- data.frame(means = distance_diff_DA_means_genera, genus = unlist(genera_categories_rep),
                                     category = rep(categories_list, times = length(genera_list)))
distance_diff_DA_table$family <- if_else(distance_diff_DA_table$genus == genera_list[1], "Monodontidae", 
                                         if_else(distance_diff_DA_table$genus == genera_list[4], "Phocoenidae", "Delphinidae"))

#Order by category
distance_diff_DA_table$category <- factor(distance_diff_DA_table$category, 
                               levels = c("1-early", "2-late/new", "3-immature", "4-adult")) #copy from string printed with the code above
#Order
distance_diff_DA_table <- distance_diff_DA_table[order(distance_diff_DA_table$category),]

#Add confidence intervals for each point
#Means for each specimen per genus and growth stage
distance_symm_DA_R_1_spec_mean <- apply(distance_symm_DA_R[,,rows_categories[[1]]], 2, mean)
distance_symm_DA_R_2_spec_mean <- apply(distance_symm_DA_R[,,rows_categories[[2]]], 2, mean)
distance_symm_DA_R_3_spec_mean <- apply(distance_symm_DA_R[,,rows_categories[[3]]], 2, mean)
distance_symm_DA_R_4_spec_mean <- apply(distance_symm_DA_R[,,rows_categories[[4]]], 2, mean)

distance_diff_DA_R_means_specs <- as.vector(c(distance_symm_DA_R_1_spec_mean, distance_symm_DA_R_2_spec_mean,
                                              distance_symm_DA_R_3_spec_mean, distance_symm_DA_R_4_spec_mean))

#Create dataframe for error bars
distance_diff_DA_R_error <-  data.frame(means = distance_diff_DA_R_means_specs,
                                        genus = genera, family = families, category = categories)

distance_diff_DA_R_error <- distance_diff_DA_R_error %>% group_by(genus,category) %>%
  mutate(lower = min(means), upper = max(means))

#Plot with lines connecting different stages per genus
distance_diff_DA_plot <- 
  ggplot(distance_diff_DA_table, aes(x = category, y = means, color = genus, shape = category, group = genus)) + 
  geom_point(data = distance_diff_DA_R_error, aes(x = category, y = means, color = genus, group = genus), 
             inherit.aes = F, show.legend = F, size = 1.5)+
  geom_line(aes(linetype = family), size = 1) + 
  geom_point(size = 3, fill = "white", stroke = 1.5)+
  scale_shape_manual(values = shapes_cat)+
  scale_colour_manual(values = mypalette_taxa)+ #to be grouped as they appear in tibble    
  scale_linetype_manual(values = c(1, 2, 4))+
  theme_classic(base_size = 12)+
  ylab("Mean distances")+
  xlab("Growth stage")+
  ggtitle ("Mean distances DA and symmetric component")+ 
  scale_x_discrete(labels = c("Early Fetus", "Late Fetus/Neonate", "Juvenile", "Adult"))+
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12), axis.title.y = element_text(vjust = 2), axis.title.x = element_text(vjust = -1),
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 11), legend.position = "bottom", legend.direction = "horizontal")+
  guides(shape = guide_legend(label = F, title = NULL, override.aes = list(shape = NA, linetype = 0, fill = NA, colour = NA)),
         colour = guide_legend(label = F, title = NULL, override.aes = list(shape = NA, linetype = 0, fill = NA)), 
         linetype = guide_legend(keywidth = unit(3, "char"), override.aes = list(color = "black", size = 0.8)))+
  annotate("text", x = 1, y = 0.005, label = "add phylopics")
distance_diff_DA_plot 

#Add significance for each segment
distance_diff_DA_plot  <- 
  distance_diff_DA_plot + # 1 line per taxon, alphabetical order
  annotate("text", x = 1.5, y = 0.0035, label = "***", family = "", fontface = 4, size = 7, colour = mypalette_taxa[1])+
  annotate("text", x = 2.6, y = 0.0038, label = "**", family = "", fontface = 4, size = 7, colour = mypalette_taxa[2])+
  annotate("text", x = 3.2, y = 0.003, label = "**", family = "", fontface = 4, size = 7, colour = mypalette_taxa[3])+
  annotate("text", x = 1.5, y = 0.0021, label = "*", family = "", fontface = 4, size = 7, colour = mypalette_taxa[4])+
  annotate("text", x = 1.5, y = 0.0015, label = "**", family = "", fontface = 4, size = 7, colour = "black")
distance_diff_DA_plot 

####Heatmaps plots for significant differences in distances means ----

#Create palette for heatmap trajectory plot
mypalette_symm <- brewer.pal(9,"Purples")
image(1:9,1, as.matrix(1:9), col = mypalette_symm,xlab="Purples (sequential)",
      ylab = "", yaxt = "n")

#Add genus and category factor for pairwise diffs
distance_diff_DA_table$genus_category <- paste(distance_diff_DA_table$genus, distance_diff_DA_table$category, sep="_")

#Loop replacements categories
for (u in 1:length(categories_list)){
  distance_diff_DA_table$genus_category <- str_replace_all(distance_diff_DA_table$genus_category, categories_list[u], categories_list_short[u])
}

#Loop replacements genera
for (t in 1:length(genera_list)){
  distance_diff_DA_table$genus_category <- str_replace_all(distance_diff_DA_table$genus_category, genera_list[t], genera_list_short[t])
}
distance_diff_DA_table$genus_category

#Unlist means per landmark to calculate pairwise t tests - useful for heatmap
#Change list to matrix
distance_symm_DA_R_1_mean_matrix <- matrix(unlist(distance_symm_DA_R_1_mean), ncol = 1, byrow = T)
distance_symm_DA_R_2_mean_matrix <- matrix(unlist(distance_symm_DA_R_2_mean), ncol = 1, byrow = T)
distance_symm_DA_R_3_mean_matrix <- matrix(unlist(distance_symm_DA_R_3_mean), ncol = 1, byrow = T)
distance_symm_DA_R_4_mean_matrix <- matrix(unlist(distance_symm_DA_R_4_mean), ncol = 1, byrow = T)

distance_symm_DA_R_mean_all <- rbind(distance_symm_DA_R_1_mean_matrix, distance_symm_DA_R_2_mean_matrix, 
                                   distance_symm_DA_R_3_mean_matrix, distance_symm_DA_R_4_mean_matrix)
#Convert to data frame for easier anal
distance_symm_DA_R_mean_all <- as.data.frame(distance_symm_DA_R_mean_all)
#Add groups
distance_symm_DA_R_mean_all$groups <- rep(distance_diff_DA_table$genus_category, each = 64)

#Pairwise t test genus/category to get p values for heatmap plot
pairwise_distance_symm_DA_R_mean_all <- pairwise.t.test(distance_symm_DA_R_mean_all[,1], distance_symm_DA_R_mean_all[,2], 
                                                      var.equal = F, pool.sd = FALSE, paired = F)

stage_breaks <- seq(0,length(distance_symm_DA_R_mean_all[,1]),length(distance_symm_DA_R_1_mean_matrix))

#Pairwise t test to get p values by stage
pairwise_distance_symm_DA_R_mean_1 <- pairwise.t.test(distance_symm_DA_R_mean_all[1:stage_breaks[2],1], distance_symm_DA_R_mean_all[1:stage_breaks[2],2], 
                                                      var.equal = F, pool.sd = FALSE, paired = F)
pairwise_distance_symm_DA_R_mean_2 <- pairwise.t.test(distance_symm_DA_R_mean_all[321:640,1], distance_symm_DA_R_mean_all[321:640,2], 
                                                    var.equal = F, pool.sd = FALSE, paired = F)
pairwise_distance_symm_DA_R_mean_3 <- pairwise.t.test(distance_symm_DA_R_mean_all[641:960,1], distance_symm_DA_R_mean_all[641:960,2], 
                                                    var.equal = F, pool.sd = FALSE, paired = F)
pairwise_distance_symm_DA_R_mean_4 <- pairwise.t.test(distance_symm_DA_R_mean_all[961:1280,1], distance_symm_DA_R_mean_all[961:1280,2], 
                                                    var.equal = F, pool.sd = FALSE, paired = F)

#Save results to file
sink("Output/pairwise_distance_symm_DA_R_mean.txt")
print("all genera/categories")
pairwise_distance_symm_DA_R_mean_all

print("genera by category")
pairwise_distance_symm_DA_R_mean_1
pairwise_distance_symm_DA_R_mean_2
pairwise_distance_symm_DA_R_mean_3
pairwise_distance_symm_DA_R_mean_4
sink()  

####Plot for all data
#Save p-values as object
dist_pvals <- pairwise_distance_symm_DA_R_mean_all[["p.value"]]

#Melt to make table in the format needed for heatmap
dist_pvals_melt <- melt(dist_pvals, value.name = "p", na.rm = TRUE)

#Create columns where only significant values are shown
dist_pvals_melt <- dist_pvals_melt %>% mutate(sig_p = ifelse(p < .05, T, F),
                                              p_if_sig = ifelse(sig_p, p, NA))
dist_pvals_melt

#Nice heatmap plot
distance_diff_DA_genus_category_heatmap_ggplot <- ggplot(data = dist_pvals_melt, aes(Var2, Var1, fill = p_if_sig))+
  geom_tile(colour = "gray80")+
  scale_fill_gradient2(low = mypalette_symm[9], high = mypalette_symm[2], mid = mypalette_symm[5], #negative correlations are in blue color and positive correlations in red. 
                       midpoint = 0.03, limit = c(0, 0.049), space = "Lab", #scale is from min to max p-values
                       na.value =  mypalette_symm[1], name = "P-values < 0.05") + 
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_flip()+
  ggtitle ("Mean distances DA and symmetric component")+ 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
        axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x =  element_text(size = 10),
        axis.text.y =  element_text(size = 10, vjust = -0.2, margin = NULL), panel.grid.major = element_blank(),
        legend.justification = c(1, 0), legend.position = c(0.4, 0.7),  legend.direction = "horizontal",
        legend.title = element_text(size = 11), legend.text = element_text(size = 10))+
  guides(fill = guide_colorbar(barwidth = 8, barheight = 1.3,
                               title.position = "top", title.hjust = 0.5))
distance_diff_DA_genus_category_heatmap_ggplot

####Plots by category
#Save p-values as object
dist_pvals_1 <- pairwise_distance_symm_DA_R_mean_1[["p.value"]]
dist_pvals_2 <- pairwise_distance_symm_DA_R_mean_2[["p.value"]]
dist_pvals_3 <- pairwise_distance_symm_DA_R_mean_3[["p.value"]]
dist_pvals_4 <- pairwise_distance_symm_DA_R_mean_4[["p.value"]]

#Melt to make table in the format needed for heatmap
dist_pvals_melt_1 <- melt(dist_pvals_1, value.name = "p", na.rm = TRUE)
dist_pvals_melt_2 <- melt(dist_pvals_2, value.name = "p", na.rm = TRUE)
dist_pvals_melt_3 <- melt(dist_pvals_3, value.name = "p", na.rm = TRUE)
dist_pvals_melt_4 <- melt(dist_pvals_4, value.name = "p", na.rm = TRUE)

#Create columns where only significant values are shown
dist_pvals_melt_1 <- dist_pvals_melt_1 %>% mutate(sig_p = ifelse(p < .05, T, F),
                                              p_if_sig = ifelse(sig_p, p, NA))
dist_pvals_melt_2 <- dist_pvals_melt_2 %>% mutate(sig_p = ifelse(p < .05, T, F),
                                                  p_if_sig = ifelse(sig_p, p, NA))
dist_pvals_melt_3 <- dist_pvals_melt_3 %>% mutate(sig_p = ifelse(p < .05, T, F),
                                                  p_if_sig = ifelse(sig_p, p, NA))
dist_pvals_melt_4 <- dist_pvals_melt_4 %>% mutate(sig_p = ifelse(p < .05, T, F),
                                                  p_if_sig = ifelse(sig_p, p, NA))

#Nice heatmap plot
distance_diff_DA_genus_category_heatmap_ggplot_1 <- ggplot(data = dist_pvals_melt_1, aes(Var2, Var1, fill = p_if_sig))+
  geom_tile(colour = "gray80")+
  scale_fill_gradient2(low = mypalette_symm[9], high = mypalette_symm[2], mid = mypalette_symm[5], #negative correlations are in blue color and positive correlations in red. 
                       midpoint = 0.03, limit = c(0, 0.049), space = "Lab", #scale is from min to max p-values
                       na.value =  mypalette_symm[1], name = "P-values < 0.05") + 
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_flip()+
  ggtitle ("Early fetus")+ 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
        axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x =  element_text(size = 10),
        axis.text.y =  element_text(size = 10, vjust = -0.2, margin = NULL), panel.grid.major = element_blank(),
        legend.justification = c(1, 0), legend.position = c(0.4, 0.7),  legend.direction = "horizontal",
        legend.title = element_text(size = 11), legend.text = element_text(size = 10))+
  guides(fill = guide_colorbar(barwidth = 8, barheight = 1.3,
                               title.position = "top", title.hjust = 0.5))
distance_diff_DA_genus_category_heatmap_ggplot_1

distance_diff_DA_genus_category_heatmap_ggplot_2 <- ggplot(data = dist_pvals_melt_2, aes(Var2, Var1, fill = p_if_sig))+
  geom_tile(colour = "gray80")+
  scale_fill_gradient2(low = mypalette_symm[9], high = mypalette_symm[2], mid = mypalette_symm[5], #negative correlations are in blue color and positive correlations in red. 
                       midpoint = 0.03, limit = c(0, 0.049), space = "Lab", #scale is from min to max p-values
                       na.value =  mypalette_symm[1], name = "P-values < 0.05") + 
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_flip()+
  ggtitle ("Late fetus/Neonate")+ 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
        axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x =  element_text(size = 10),
        axis.text.y =  element_text(size = 10, vjust = -0.2, margin = NULL), panel.grid.major = element_blank(),
        legend.justification = c(1, 0), legend.position = c(0.4, 0.7),  legend.direction = "horizontal",
        legend.title = element_text(size = 11), legend.text = element_text(size = 10))+
  guides(fill = guide_colorbar(barwidth = 8, barheight = 1.3,
                               title.position = "top", title.hjust = 0.5))
distance_diff_DA_genus_category_heatmap_ggplot_2

distance_diff_DA_genus_category_heatmap_ggplot_3 <- ggplot(data = dist_pvals_melt_3, aes(Var2, Var1, fill = p_if_sig))+
  geom_tile(colour = "gray80")+
  scale_fill_gradient2(low = mypalette_symm[9], high = mypalette_symm[2], mid = mypalette_symm[5], #negative correlations are in blue color and positive correlations in red. 
                       midpoint = 0.03, limit = c(0, 0.049), space = "Lab", #scale is from min to max p-values
                       na.value =  mypalette_symm[1], name = "P-values < 0.05") + 
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_flip()+
  ggtitle ("Juvenile")+ 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
        axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x =  element_text(size = 10),
        axis.text.y =  element_text(size = 10, vjust = -0.2, margin = NULL), panel.grid.major = element_blank(),
        legend.justification = c(1, 0), legend.position = c(0.4, 0.7),  legend.direction = "horizontal",
        legend.title = element_text(size = 11), legend.text = element_text(size = 10))+
  guides(fill = guide_colorbar(barwidth = 8, barheight = 1.3,
                               title.position = "top", title.hjust = 0.5))
distance_diff_DA_genus_category_heatmap_ggplot_3

distance_diff_DA_genus_category_heatmap_ggplot_4 <- ggplot(data = dist_pvals_melt_4, aes(Var2, Var1, fill = p_if_sig))+
  geom_tile(colour = "gray80")+
  scale_fill_gradient2(low = mypalette_symm[9], high = mypalette_symm[2], mid = mypalette_symm[5], #negative correlations are in blue color and positive correlations in red. 
                       midpoint = 0.03, limit = c(0, 0.049), space = "Lab", #scale is from min to max p-values
                       na.value =  mypalette_symm[1], name = "P-values < 0.05") + 
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_flip()+
  ggtitle ("Adult")+ 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
        axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x =  element_text(size = 10),
        axis.text.y =  element_text(size = 10, vjust = -0.2, margin = NULL), panel.grid.major = element_blank(),
        legend.justification = c(1, 0), legend.position = c(0.4, 0.7),  legend.direction = "horizontal",
        legend.title = element_text(size = 11), legend.text = element_text(size = 10))+
  guides(fill = guide_colorbar(barwidth = 8, barheight = 1.3,
                               title.position = "top", title.hjust = 0.5))
distance_diff_DA_genus_category_heatmap_ggplot_4

#Arrange in 1 plot
ggarrange(distance_diff_DA_genus_category_heatmap_ggplot_1, distance_diff_DA_genus_category_heatmap_ggplot_2,
  distance_diff_DA_genus_category_heatmap_ggplot_3, distance_diff_DA_genus_category_heatmap_ggplot_4, 
  ncol = 2, nrow = 2, common.legend = T)

##NO INTERPARIETAL - Use landVR to find differences between DA each side and symmetric landmarks - Linear (Euclidean) distances ----

#Create objects with DA and symm landmarks numbers
landmarks_symm_int <- length(fixed_LMs)-length(interparietal_LM)
landmarks_DA_int <- length(fixed_LMs)*2-length(interparietal_LM)*2

#Subtract landmarks from DA and symm coords
#Perform GPA again to ensure alignment and correct numbering of landmarks
#Right side only as established before for all bones
DA_R_symm_shapes_int <- rray_bind(symm_shapes_specimens[-interparietal_LM,,], DA_shapes_R[-interparietal_LM,,], .axis = 1)

#GPA
gpa_DA_R_symm_shapes_int <- gpagen(DA_R_symm_shapes_int)

#Check plots
spheres3d(gpa_DA_R_symm_shapes_int$consensus[c(1+landmarks_symm_int):landmarks_DA_int,], radius = 0.003, col = "blue")
spheres3d(gpa_DA_R_symm_shapes_int$consensus[1:landmarks_symm_int,], radius = 0.003, col = "gray70")

#Save coords
coords_DA_int_R_symm <- gpa_DA_R_symm_shapes_int$coords

#Create new matrix without interparietal
#Pairs of landmarks, real and symmed
landpairs_DA_int <- cbind(symm = c(1:landmarks_symm_int), DA = c(c(1+landmarks_symm_int):landmarks_DA_int))

#Test on 1 pair
linear.dist(coords_DA_int_R_symm, landpairs_DA_int[1,])

#Create matrix for linear distances
distance_matrix_DA_R_int <-matrix(nrow = length(Ids), ncol = length(landpairs_DA_int[,1]))

for (r in 1:length(landpairs_DA_int[,1])){
  distance_matrix_DA_R_int[,r] <- linear.dist(coords_DA_int_R_symm, landpairs_DA_int[r,])
  }

distance_matrix_DA_R_int <- t(distance_matrix_DA_R_int)

#Create array to be filled with distances
distance_symm_DA_R_int <- array(dim = c(nrow(distance_matrix_DA_R_int), 1, ncol(distance_matrix_DA_R_int))) #1 is the columns of data we need  - see output of previous line

#Loop linear distances calculations
for (w in 1:ncol(distance_matrix_DA_R_int)){
  distance_symm_DA_R_int[,,w] <- distance_matrix_DA_R_int[,w]
  
}

#Set dimnames as specimens
dimnames(distance_symm_DA_R_int)[[3]] <- dimnames(coords_DA_int_R_symm)[[3]]

#Check plot
#Calculate overall mean shapes and distances
all_DA_int_mean <- mshape(coords_DA_int_R_symm[c(landmarks_symm_int+1):landmarks_DA_int,,])
all_symm_int_mean <- mshape(coords_DA_int_R_symm[1:landmarks_symm_int,,])

distance_symm_DA_R_int_mean <- apply(distance_symm_DA_R_int, c(1), mean)

#Looking at the average distances compared to average specimen
distance_DA_int_diff_mean_plot <- procrustes.var.plot(all_DA_int_mean, all_symm_int_mean , 
                                                      col.val = distance_symm_DA_R_int_mean, col = colfunc,
                                                      magnitude = 1, pt.size = 1, lwd = 1)
spheres3d(mshape(coords_DA_int_R_symm[1:landmarks_symm_int,,]), radius = 0.002, col = "gray40")

rgl.snapshot(filename = "Output/distance_diff_DA_R_int_all.png") 

##Plot DA shapes vs symm shapes differences in landVR NO INTERPARIETAL- tells where variance between sides is concentrated, visual test side symmetry ----

###Genus ----
##For each genus, find the mean of the DA data and symmetric data
#Use total range of means for plots, so that they are all compared to the same standard

#Delphinapterus
delp_DA_int <- coords_DA_int_R_symm[62:122,,rows_genera[[1]]]
delp_DA_int_mean <- apply(delp_DA_int, c(1,2), mean)

delp_symm_int <- coords_DA_int_R_symm[1:61,,rows_genera[[1]]]
delp_symm_int_mean <- apply(delp_symm_int, c(1,2), mean)

distance_symm_DA_R_int_delp <- distance_symm_DA_R_int[,,rows_genera[[1]]]
distance_symm_DA_R_int_delp_mean <- apply(distance_symm_DA_R_int_delp, c(1), mean)

#Globicephala
glob_DA_int <- coords_DA_int_R_symm[62:122,,rows_genera[[2]]]
glob_DA_int_mean <- apply(glob_DA_int, c(1,2), mean)

glob_symm_int <- coords_DA_int_R_symm[1:61,,rows_genera[[2]]]
glob_symm_int_mean <- apply(glob_symm_int, c(1,2), mean)

distance_symm_DA_R_int_glob <- distance_symm_DA_R_int[,,rows_genera[[2]]]
distance_symm_DA_R_int_glob_mean <- apply(distance_symm_DA_R_int_glob, c(1), mean)

#Lagenorhynchus
lage_DA_int <- coords_DA_int_R_symm[62:122,,rows_genera[[3]]]
lage_DA_int_mean <- apply(lage_DA_int, c(1,2), mean)

lage_symm_int <- coords_DA_int_R_symm[1:61,,rows_genera[[3]]]
lage_symm_int_mean <- apply(lage_symm_int, c(1,2), mean)

distance_symm_DA_R_int_lage <- distance_symm_DA_R_int[,,rows_genera[[3]]]
distance_symm_DA_R_int_lage_mean <- apply(distance_symm_DA_R_int_lage, c(1), mean)

#Phocoena
phoc_DA_int <- coords_DA_int_R_symm[62:122,,rows_genera[[4]]]
phoc_DA_int_mean <- apply(phoc_DA_int, c(1,2), mean)

phoc_symm_int <- coords_DA_int_R_symm[1:61,,rows_genera[[4]]]
phoc_symm_int_mean <- apply(phoc_symm_int, c(1,2), mean)

distance_symm_DA_R_int_phoc <- distance_symm_DA_R_int[,,rows_genera[[4]]]
distance_symm_DA_R_int_phoc_mean <- apply(distance_symm_DA_R_int_phoc, c(1), mean)

#Stenella
sten_DA_int <- coords_DA_int_R_symm[62:122,,rows_genera[[5]]]
sten_DA_int_mean <- apply(sten_DA_int, c(1,2), mean)

sten_symm_int <- coords_DA_int_R_symm[1:61,,rows_genera[[5]]]
sten_symm_int_mean <- apply(sten_symm_int, c(1,2), mean)

distance_symm_DA_R_int_sten <- distance_symm_DA_R_int[,,rows_genera[[5]]]
distance_symm_DA_R_int_sten_mean <- apply(distance_symm_DA_R_int_sten, c(1), mean)

###Plot landmarks colored by distance on mean shape per genus

#Make matrix with all means by genus and calculate breaks between them based on n of landmarks
fixed_LMs_int <- fixed_LMs[-interparietal_LM]

genera_breaks_int <- c(length(fixed_LMs_int), (length( fixed_LMs_int)*2), 
                       (length( fixed_LMs_int)*3),
                       (length(fixed_LMs_int)*4), (length( fixed_LMs_int)*5))

genera_int_means <- c(distance_symm_DA_R_int_delp_mean, distance_symm_DA_R_int_glob_mean, distance_symm_DA_R_int_lage_mean, distance_symm_DA_R_int_phoc_mean,
                  distance_symm_DA_R_int_sten_mean)

#4. Create colour gradient of distance values based on colour palette created earlier - all genera to get relative colors
DA_col_genera_int <- colfunc_flip(50)[as.numeric(cut(genera_int_means, breaks = 50))]

#Delphinapterus
#5. Check output by plotting points as per-point DA distance
plot(distance_symm_DA_R_int_delp_mean, col = DA_col_genera_int[1:genera_breaks_int[1]], cex = 2, pch = 19)

#DA symm distances on mesh
shade3d(template_mesh_delp, col = "white", alpha = 0.5, fastTransparency = T)
spheres3d(shape_array[fixed_LMs_int,,18], col =  DA_col_genera_int[1:genera_breaks_int[1]], type = "s",
          radius = 4, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_int_delp_mesh.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_int_delp_movie" ,dir = "Output/")

#Globicephala
#5. Check output by plotting points as per-point DA distance
plot(distance_symm_DA_R_int_glob_mean, col = DA_col_genera_int[(genera_breaks_int[1]+1):genera_breaks_int[2]], cex = 2, pch = 19)

#DA symm distances on mesh
shade3d(template_mesh_glob, col = "white", alpha = 0.5, fastTransparency = T)
spheres3d(shape_array[fixed_LMs_int,,22], col =  DA_col_genera_int[(genera_breaks_int[1]+1):genera_breaks_int[2]], type = "s",
          radius = 2, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_int_glob_mesh.png") 
play3d(spin3d(axis = c(0, 1, 1), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(0, 1, 1), rpm = 10), duration = 6, movie = "distance_diff_DA_int_glob_movie" ,dir = "Output/")

#Lagenorhynchus
#5. Check output by plotting points as per-point DA distance
plot(distance_symm_DA_R_int_lage_mean, col = DA_col_genera_int[(genera_breaks_int[2]+1):genera_breaks_int[3]], cex = 2, pch = 19)

#DA symm distances on mesh
shade3d(template_mesh_lage, col = "white", alpha = 0.5, fastTransparency = T)
spheres3d(shape_array[fixed_LMs_int,,38], col =  DA_col_genera_int[(genera_breaks_int[2]+1):genera_breaks_int[3]], type = "s",
          radius = 3, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_int_lage_mesh.png") 
play3d(spin3d(axis = c(0, 1, 1), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(0, 1, 1), rpm = 10), duration = 6, movie = "distance_diff_DA_int_lage_movie" ,dir = "Output/")

#Phocoena
#5. Check output by plotting points as per-point DA distance
plot(distance_symm_DA_R_int_phoc_mean, col = DA_col_genera_int[(genera_breaks_int[3]+1):genera_breaks_int[4]], cex = 2, pch = 19)

#DA symm distances on mesh
shade3d(template_mesh_phoc, col = "white", alpha = 0.5, fastTransparency = T)
spheres3d(shape_array[fixed_LMs_int,,27], col =  DA_col_genera_int[(genera_breaks_int[3]+1):genera_breaks_int[4]], type = "s",
          radius = 1.5, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_int_phoc_mesh.png") 
play3d(spin3d(axis = c(-1,-1, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(-1,-1, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_int_phoc_movie" ,dir = "Output/")

#Stenella
#5. Check output by plotting points as per-point DA distance
plot(distance_symm_DA_R_int_sten_mean, col = DA_col_genera_int[(genera_breaks_int[4]+1):genera_breaks_int[5]], cex = 2, pch = 19)

#DA symm distances on mesh
shade3d(template_mesh_sten, col = "white", alpha = 0.5, fastTransparency = T)
spheres3d(shape_array[fixed_LMs_int,,29], col =  DA_col_genera_int[(genera_breaks_int[4]+1):genera_breaks_int[5]], type = "s",
          radius = 1.5, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_int_sten_mesh.png") 
play3d(spin3d(axis = c(1, 0, 1), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(0, 1, 1), rpm = 10), duration = 6, movie = "distance_diff_DA_int_sten_movie" ,dir = "Output/")

###Category ----
##For each category, find the mean of the DA and symmetric data

#Early
early_DA_int <- coords_DA_int_R_symm[62:122,,rows_categories[[1]]]
early_DA_int_mean <- apply(early_DA_int, c(1,2), mean)

early_symm_int <- coords_DA_int_R_symm[1:61,,rows_categories[[1]]]
early_symm_int_mean <- apply(early_symm_int, c(1,2), mean)

distance_symm_DA_R_int_early <- distance_symm_DA_R_int[,,rows_categories[[1]]]
distance_symm_DA_R_int_early_mean <- apply(distance_symm_DA_R_int_early, c(1), mean)

#Late/newborn
late_new_DA_int <- coords_DA_int_R_symm[62:122,,rows_categories[[2]]]
late_new_DA_int_mean <- apply(late_new_DA_int, c(1,2), mean)

late_new_symm_int <- coords_DA_int_R_symm[1:61,,rows_categories[[2]]]
late_new_symm_int_mean <- apply(late_new_symm_int, c(1,2), mean)

distance_symm_DA_R_int_late_new <- distance_symm_DA_R_int[,,rows_categories[[2]]]
distance_symm_DA_R_int_late_new_mean <- apply(distance_symm_DA_R_int_late_new, c(1), mean)

#Immature
immature_DA_int <- coords_DA_int_R_symm[62:122,,rows_categories[[3]]]
immature_DA_int_mean <- apply(immature_DA_int, c(1,2), mean)

immature_symm_int <- coords_DA_int_R_symm[1:61,,rows_categories[[3]]]
immature_symm_int_mean <- apply(immature_symm_int, c(1,2), mean)

distance_symm_DA_R_int_immature <- distance_symm_DA_R_int[,,rows_categories[[3]]]
distance_symm_DA_R_int_immature_mean <- apply(distance_symm_DA_R_int_immature, c(1), mean)

#Adult
adult_DA_int <- coords_DA_int_R_symm[62:122,,rows_categories[[4]]]
adult_DA_int_mean <- apply(adult_DA_int, c(1,2), mean)

adult_symm_int <- coords_DA_int_R_symm[1:61,,rows_categories[[4]]]
adult_symm_int_mean <- apply(adult_symm_int, c(1,2), mean)

distance_symm_DA_R_int_adult <- distance_symm_DA_R_int[,,rows_categories[[4]]]
distance_symm_DA_R_int_adult_mean <- apply(distance_symm_DA_R_int_adult, c(1), mean)

###Plot landmarks colored by distance on mean shape per category

#Make matrix with all means by category and calculate breaks between them based on n of landmarks
category_breaks_int <- c(length(fixed_LMs_int), (length(fixed_LMs_int)*2), (length(fixed_LMs_int)*3),
                     (length(fixed_LMs_int)*4))

category_means_int <- c(distance_symm_DA_R_int_early_mean, distance_symm_DA_R_int_late_new_mean, distance_symm_DA_R_int_immature_mean, 
                    distance_symm_DA_R_int_adult_mean)

#4. Create colour gradient of distance values based on colour palette created earlier - all categories to get relative colors
DA_col_category_int <- colfunc_flip(50)[as.numeric(cut(category_means_int, breaks = 50))]

#Early
#5. Check output by plotting points as per-point DA distance
plot(distance_symm_DA_R_int_early_mean, col = DA_col_category_int[1:category_breaks_int[1]], cex = 2, pch = 19)

#DA symm distances on mesh
shade3d(template_mesh_early, col = "white", alpha = 0.5, fastTransparency = T)
spheres3d(shape_array[fixed_LMs_int,,9], col =  DA_col_category_int[1:category_breaks_int[1]], type = "s",
          radius = 0.7, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_int_early_mesh.png") 
play3d(spin3d(axis = c(0, 0, 1), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(0, 0, 1), rpm = 10), duration = 6, movie = "distance_diff_DA_int_early_movie" ,dir = "Output/")

#Late/New
#5. Check output by plotting points as per-point DA distance
plot(distance_symm_DA_R_int_late_new_mean, col = DA_col_category_int[category_breaks_int[1]+1:category_breaks_int[2]], cex = 2, pch = 19)

#DA symm distances on mesh
shade3d(template_mesh_late_new, col = "white", alpha = 0.5, fastTransparency = T)
spheres3d(shape_array[fixed_LMs_int,,23], col =  DA_col_category_int[category_breaks_int[1]+1:category_breaks_int[2]], type = "s",
          radius = 1.5, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_int_late_new_mesh.png") 
play3d(spin3d(axis = c(1, 1, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1,1,0), rpm = 10), duration = 6, movie = "distance_diff_DA_int_late_new_movie" ,dir = "Output/")

#Immature
#5. Check output by plotting points as per-point DA distance
plot(distance_symm_DA_R_int_immature_mean, col = DA_col_category_int[category_breaks_int[2]+1:category_breaks_int[3]], cex = 2, pch = 19)

#DA symm distances on mesh
shade3d(template_mesh_immature, col = "white", alpha = 0.5, fastTransparency = T)
spheres3d(shape_array[fixed_LMs_int,,39], col =  DA_col_category_int[category_breaks_int[2]+1:category_breaks_int[3]], type = "s",
          radius = 3, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_int_immature_mesh.png") 
play3d(spin3d(axis = c(0, 1, 1), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(0, 1, 1), rpm = 10), duration = 6, movie = "distance_diff_DA_int_immature_movie" ,dir = "Output/")

#Adult
#5. Check output by plotting points as per-point DA distance
plot(distance_symm_DA_R_int_adult_mean, col = DA_col_category_int[category_breaks_int[3]+1:category_breaks_int[4]], cex = 2, pch = 19)

#DA symm distances on mesh
shade3d(template_mesh_adult, col = "white", alpha = 0.5, fastTransparency = T)
spheres3d(shape_array[fixed_LMs_int,,48], col =  DA_col_category_int[category_breaks_int[3]+1:category_breaks_int[4]], type = "s",
          radius = 4, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_int_adult_mesh.png") 
play3d(spin3d(axis = c(0, 1, 1), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(0, 1, 1), rpm = 10), duration = 6, movie = "distance_diff_DA_int_adult_movie" ,dir = "Output/")

###Genus and category ----
##For each genus in each category, find the mean of the DA and symmetric data

#Create new arrays by category
distance_symm_DA_R_int_1 <- distance_symm_DA_R_int[,,rows_categories[[1]]]
distance_symm_DA_R_int_2 <- distance_symm_DA_R_int[,,rows_categories[[2]]]
distance_symm_DA_R_int_3 <- distance_symm_DA_R_int[,,rows_categories[[3]]]
distance_symm_DA_R_int_4 <- distance_symm_DA_R_int[,,rows_categories[[4]]]

#Means
distance_symm_DA_R_int_1_mean <- list()
distance_symm_DA_R_int_2_mean <- list()
distance_symm_DA_R_int_3_mean <- list()
distance_symm_DA_R_int_4_mean <- list()

#Loop
for (k in 1:length(genera_list)){
  distance_symm_DA_R_int_1_mean[[k]] <- apply(distance_symm_DA_R_int_1[,category_genus_rows_1[[k]]], 1, mean)
  distance_symm_DA_R_int_2_mean[[k]] <- apply(distance_symm_DA_R_int_2[,category_genus_rows_2[[k]]], 1, mean)
  distance_symm_DA_R_int_3_mean[[k]] <- apply(distance_symm_DA_R_int_3[,category_genus_rows_3[[k]]], 1, mean)
  distance_symm_DA_R_int_4_mean[[k]] <- apply(distance_symm_DA_R_int_4[,category_genus_rows_4[[k]]], 1, mean)
}  

#Create coords array by category
coords_DA_int_1 <- coords_DA_int_R_symm[c(landmarks_symm_int+1):landmarks_DA_int,,rows_categories[[1]]]
coords_DA_int_2 <- coords_DA_int_R_symm[c(landmarks_symm_int+1):landmarks_DA_int,,rows_categories[[2]]]
coords_DA_int_3 <- coords_DA_int_R_symm[c(landmarks_symm_int+1):landmarks_DA_int,,rows_categories[[3]]]
coords_DA_int_4 <- coords_DA_int_R_symm[c(landmarks_symm_int+1):landmarks_DA_int,,rows_categories[[4]]]

coords_symm_int_1 <- coords_DA_int_R_symm[1:landmarks_symm_int,,rows_categories[[1]]]
coords_symm_int_2 <- coords_DA_int_R_symm[1:landmarks_symm_int,,rows_categories[[2]]]
coords_symm_int_3 <- coords_DA_int_R_symm[1:landmarks_symm_int,,rows_categories[[3]]]
coords_symm_int_4 <- coords_DA_int_R_symm[1:landmarks_symm_int,,rows_categories[[4]]]

#Means
coords_DA_int_mean_1 <- list()
coords_DA_int_mean_2  <- list()
coords_DA_int_mean_3  <- list()
coords_DA_int_mean_4 <- list()

#Loop
for (k in 1:length(genera_list)){
  coords_DA_int_mean_1[[k]] <- apply(coords_DA_int_1[,,category_genus_rows_1[[k]]], c(1,2), mean)
  coords_DA_int_mean_2[[k]] <- apply(coords_DA_int_2[,,category_genus_rows_2[[k]]], c(1,2), mean)
  coords_DA_int_mean_3[[k]] <- apply(coords_DA_int_3[,,category_genus_rows_3[[k]]], c(1,2), mean)
  coords_DA_int_mean_4[[k]] <- apply(coords_DA_int_4[,,category_genus_rows_4[[k]]], c(1,2), mean)
}  

coords_symm_int_mean_1 <- list()
coords_symm_int_mean_2  <- list()
coords_symm_int_mean_3  <- list()
coords_symm_int_mean_4 <- list()

#Loop
for (k in 1:length(genera_list)){
  coords_symm_int_mean_1[[k]] <- apply(coords_symm_int_1[,,category_genus_rows_1[[k]]], c(1,2), mean)
  coords_symm_int_mean_2[[k]] <- apply(coords_symm_int_2[,,category_genus_rows_2[[k]]], c(1,2), mean)
  coords_symm_int_mean_3[[k]] <- apply(coords_symm_int_3[,,category_genus_rows_3[[k]]], c(1,2), mean)
  coords_symm_int_mean_4[[k]] <- apply(coords_symm_int_4[,,category_genus_rows_4[[k]]], c(1,2), mean)
}  

####Plot landmarks colored by distance on mean shape per genus per category

##Adults only - for line plot

#Mean adults for plot reference
distance_symm_DA_R_int_4_all_mean <- apply(distance_symm_DA_R_int_4, 1, mean)

#Object with adult each species, plus mean adults all dataset for uniform comparison of values
delp_means_2_int <- c(distance_symm_DA_R_int_4_mean[[1]], distance_symm_DA_R_int_4_all_mean)
glob_means_2_int <- c(distance_symm_DA_R_int_4_mean[[2]], distance_symm_DA_R_int_4_all_mean)
lage_means_2_int <- c(distance_symm_DA_R_int_4_mean[[3]], distance_symm_DA_R_int_4_all_mean)
phoc_means_2_int <- c(distance_symm_DA_R_int_4_mean[[4]], distance_symm_DA_R_int_4_all_mean)
sten_means_2_int <- c(distance_symm_DA_R_int_4_mean[[5]], distance_symm_DA_R_int_4_all_mean)

#4. Create colour gradient of distance values based on colour palette created earlier - each genus to get relative colors
DA_col_genera_int_delp <- colfunc_flip(50)[as.numeric(cut(delp_means_2_int, breaks = 50))]
DA_col_genera_int_glob <- colfunc_flip(50)[as.numeric(cut(glob_means_2_int, breaks = 50))]
DA_col_genera_int_lage <- colfunc_flip(50)[as.numeric(cut(lage_means_2_int, breaks = 50))]
DA_col_genera_int_phoc <- colfunc_flip(50)[as.numeric(cut(phoc_means_2_int, breaks = 50))]
DA_col_genera_int_sten <- colfunc_flip(50)[as.numeric(cut(sten_means_2_int, breaks = 50))]

#Delphinapterus
#5. Check output by plotting points as per-point DA distance
plot(distance_symm_DA_R_int_4_mean[[1]], col = DA_col_genera_int_delp[1:genera_breaks_int[1]], cex = 2, pch = 19)

#DA symm distances on mesh
shade3d(template_mesh_delp_4, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs_int,,48], col =  DA_col_genera_int_delp[1:genera_breaks_int[1]], type = "s",
          radius = 5, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_int_delp_mesh_adult.png") 
play3d(spin3d(axis = c(0, 1, 1), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(0,1,1), rpm = 10), duration = 6, movie = "distance_diff_DA_int_delp_movie_4" ,dir = "Output/")

#Globicephala
#5. Check output by plotting points as per-point DA distance
plot(distance_symm_DA_R_int_4_mean[[2]], col = DA_col_genera_int_glob[1:genera_breaks_int[1]], cex = 2, pch = 19)

#DA symm distances on mesh
shade3d(template_mesh_glob_4, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs_int,,51], col =  DA_col_genera_int_glob[1:genera_breaks_int[1]], type = "s",
          radius = 6, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_int_glob_mesh_adult.png") 
play3d(spin3d(axis = c(0,1,0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(0,1, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_int_glob_movie_4" ,dir = "Output/")

#Lagenorhynchus
#5. Check output by plotting points as per-point DA distance
plot(distance_symm_DA_R_int_4_mean[[3]], col = DA_col_genera_int_lage[1:genera_breaks_int[1]], cex = 2, pch = 19)

#DA symm distances on mesh
shade3d(template_mesh_lage_4, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs_int,,53], col =  DA_col_genera_int_lage[1:genera_breaks_int[1]], type = "s",
          radius = 4, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_int_lage_mesh_adult.png") 
play3d(spin3d(axis = c(0, 1,1), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(0,1,1), rpm = 10), duration = 6, movie = "distance_diff_DA_int_lage_movie_4" ,dir = "Output/")

#Phocoena
#5. Check output by plotting points as per-point DA distance
plot(distance_symm_DA_R_int_4_mean[[4]], col = DA_col_genera_int_phoc[1:genera_breaks_int[1]], cex = 2, pch = 19)

#DA symm distances on mesh
shade3d(template_mesh_phoc_4, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs_int,,54], col =  DA_col_genera_int_phoc[1:genera_breaks_int[1]], type = "s",
          radius = 3, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_int_phoc_mesh_adult.png") 
play3d(spin3d(axis = c(0, 1, 1), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(0, 1, 1), rpm = 10), duration = 6, movie = "distance_diff_DA_int_phoc_movie_4" ,dir = "Output/")

#Stenella
#5. Check output by plotting points as per-point DA distance
plot(distance_symm_DA_R_int_4_mean[[5]], col = DA_col_genera_int_sten[1:genera_breaks_int[1]], cex = 2, pch = 19)

#DA symm distances on mesh
shade3d(template_mesh_sten_4, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs_int,,58], col =  DA_col_genera_int_sten[1:genera_breaks_int[1]], type = "s",
          radius = 3, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_int_sten_mesh_adult.png") 
play3d(spin3d(axis = c(0, 0, 1), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(0, 0, 1), rpm = 10), duration = 6, movie = "distance_diff_DA_int_sten_movie_4" ,dir = "Output/")

###All categories
##Plots for all categories per genus relative to mean of dataset

#Object with mean growth stages each species, plus mean all dataset for uniform comparison of values
delp_means_int_4 <- c(distance_symm_DA_R_int_1_mean[[1]], distance_symm_DA_R_int_2_mean[[1]],
                  distance_symm_DA_R_int_3_mean[[1]], distance_symm_DA_R_int_4_mean[[1]], distance_symm_DA_R_int_mean)
glob_means_int_4 <- c(distance_symm_DA_R_int_1_mean[[2]], distance_symm_DA_R_int_2_mean[[2]],
                  distance_symm_DA_R_int_3_mean[[2]], distance_symm_DA_R_int_4_mean[[2]], distance_symm_DA_R_int_mean)
lage_means_int_4 <- c(distance_symm_DA_R_int_1_mean[[3]], distance_symm_DA_R_int_2_mean[[3]],
                  distance_symm_DA_R_int_3_mean[[3]], distance_symm_DA_R_int_4_mean[[3]], distance_symm_DA_R_int_mean)
phoc_means_int_4 <- c(distance_symm_DA_R_int_1_mean[[4]], distance_symm_DA_R_int_2_mean[[4]],
                  distance_symm_DA_R_int_3_mean[[4]], distance_symm_DA_R_int_4_mean[[4]], distance_symm_DA_R_int_mean)
sten_means_int_4 <- c(distance_symm_DA_R_int_1_mean[[5]], distance_symm_DA_R_int_2_mean[[5]],
                  distance_symm_DA_R_int_3_mean[[5]], distance_symm_DA_R_int_4_mean[[5]], distance_symm_DA_R_int_mean)

#4. Create colour gradient of distance values based on colour palette created earlier - each genus to get relative colors
DA_col_genera_int_delp_4 <- colfunc_flip(50)[as.numeric(cut(delp_means_int_4, breaks = 50))]
DA_col_genera_int_glob_4 <- colfunc_flip(50)[as.numeric(cut(glob_means_int_4, breaks = 50))]
DA_col_genera_int_lage_4 <- colfunc_flip(50)[as.numeric(cut(lage_means_int_4, breaks = 50))]
DA_col_genera_int_phoc_4 <- colfunc_flip(50)[as.numeric(cut(phoc_means_int_4, breaks = 50))]
DA_col_genera_int_sten_4 <- colfunc_flip(50)[as.numeric(cut(sten_means_int_4, breaks = 50))]

#Delphinapterus
#5. Check output by plotting points as per-point DA distance
plot(distance_symm_DA_R_int_1_mean[[1]], col = DA_col_genera_int_delp_4[1:genera_breaks_int[1]], cex = 2, pch = 19)
plot(distance_symm_DA_R_int_2_mean[[1]], col = DA_col_genera_int_delp_4[(genera_breaks_int[1]+1):genera_breaks_int[2]], cex = 2, pch = 19)
plot(distance_symm_DA_R_int_3_mean[[1]], col = DA_col_genera_int_delp_4[(genera_breaks_int[2]+1):genera_breaks_int[3]], cex = 2, pch = 19)
plot(distance_symm_DA_R_int_4_mean[[1]], col = DA_col_genera_int_delp_4[(genera_breaks_int[3]+1):genera_breaks_int[4]], cex = 2, pch = 19)

#DA symm distances on mesh
shade3d(template_mesh_delp_1, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs_int,,2], col =  DA_col_genera_int_delp_4[1:genera_breaks_int[1]], type = "s",
          radius = 1, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_delp_int_mesh_1.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_delp_int_movie_1" ,dir = "Output/")

shade3d(template_mesh_delp_2, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs_int,,18], col =  DA_col_genera_int_delp_4[(genera_breaks_int[1]+1):genera_breaks_int[2]], type = "s",
          radius =4, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_delp_int_mesh_2.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_delp_int_movie_2" ,dir = "Output/")

shade3d(template_mesh_delp_3, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs_int,,34], col =  DA_col_genera_int_delp_4[(genera_breaks_int[2]+1):genera_breaks_int[3]], type = "s",
          radius = 5, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_delp_int_mesh_3.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_int_delp_movie_3" ,dir = "Output/")

shade3d(template_mesh_delp_4, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs_int,,48], col =  DA_col_genera_int_delp_4[(genera_breaks_int[3]+1):genera_breaks_int[4]], type = "s",
          radius = 5, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_delp_int_mesh_4.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_delp_int_movie_4" ,dir = "Output/")

#Globicephala
#5. Check output by plotting points as per-point DA distance
plot(distance_symm_DA_R_int_1_mean[[2]], col = DA_col_genera_int_glob_4[1:genera_breaks_int[1]], cex = 2, pch = 19)
plot(distance_symm_DA_R_int_2_mean[[2]], col = DA_col_genera_int_glob_4[(genera_breaks_int[1]+1):genera_breaks_int[2]], cex = 2, pch = 19)
plot(distance_symm_DA_R_int_3_mean[[2]], col = DA_col_genera_int_glob_4[(genera_breaks_int[2]+1):genera_breaks_int[3]], cex = 2, pch = 19)
plot(distance_symm_DA_R_int_4_mean[[2]], col = DA_col_genera_int_glob_4[(genera_breaks_int[3]+1):genera_breaks_int[4]], cex = 2, pch = 19)

#DA symm distances on mesh
shade3d(template_mesh_glob_1, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs_int,,5], col =  DA_col_genera_int_glob_4[1:genera_breaks_int[1]], type = "s",
          radius = 0.8, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_glob_mesh_int_1.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_glob_int_movie_1" ,dir = "Output/")

shade3d(template_mesh_glob_2, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs_int,,20], col =  DA_col_genera_int_glob_4[(genera_breaks_int[1]+1):genera_breaks_int[2]], type = "s",
          radius = 4, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_glob_int_mesh_2.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_glob_int_movie_2" ,dir = "Output/")

shade3d(template_mesh_glob_3, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs_int,,36], col =  DA_col_genera_int_glob_4[(genera_breaks_int[2]+1):genera_breaks_int[3]], type = "s",
          radius = 4, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_glob_int_mesh_3.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_glob_int_movie_3" ,dir = "Output/")

shade3d(template_mesh_glob_4, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs_int,,51], col =  DA_col_genera_int_glob_4[(genera_breaks_int[3]+1):genera_breaks_int[4]], type = "s",
          radius = 6, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_glob_int_mesh_4.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_glob_int_movie_4" ,dir = "Output/")

#Lagenorhynchus
#5. Check output by plotting points as per-point DA distance
plot(distance_symm_DA_R_int_1_mean[[3]], col = DA_col_genera_int_lage_4[1:genera_breaks_int[1]], cex = 2, pch = 19)
plot(distance_symm_DA_R_int_2_mean[[3]], col = DA_col_genera_int_lage_4[(genera_breaks_int[1]+1):genera_breaks_int[2]], cex = 2, pch = 19)
plot(distance_symm_DA_R_int_3_mean[[3]], col = DA_col_genera_int_lage_4[(genera_breaks_int[2]+1):genera_breaks_int[3]], cex = 2, pch = 19)
plot(distance_symm_DA_R_int_4_mean[[3]], col = DA_col_genera_int_lage_4[(genera_breaks_int[3]+1):genera_breaks_int[4]], cex = 2, pch = 19)

#DA symm distances on mesh
shade3d(template_mesh_lage_1, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs_int,,7], col =  DA_col_genera_int_lage_4[1:genera_breaks_int[1]], type = "s",
          radius = 1, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_lage_int_mesh_1.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_lage_int_movie_1" ,dir = "Output/")

shade3d(template_mesh_lage_2, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs_int,,24], col =  DA_col_genera_int_lage_4[(genera_breaks_int[1]+1):genera_breaks_int[2]], type = "s",
          radius = 2, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_lage_int_mesh_2.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_lage_int_movie_2" ,dir = "Output/")

shade3d(template_mesh_lage_3, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs_int,,38], col =  DA_col_genera_int_lage_4[(genera_breaks_int[2]+1):genera_breaks_int[3]], type = "s",
          radius = 4, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_lage_int_mesh_3.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_lage_int_movie_3" ,dir = "Output/")

shade3d(template_mesh_lage_4, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs_int,,53], col =  DA_col_genera_int_lage_4[(genera_breaks_int[3]+1):genera_breaks_int[4]], type = "s",
          radius = 4, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_lage_int_mesh_4.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_lage_int_movie_4" ,dir = "Output/")

#Phocoena
#5. Check output by plotting points as per-point DA distance
plot(distance_symm_DA_R_int_1_mean[[4]], col = DA_col_genera_int_phoc_4[1:genera_breaks_int[1]], cex = 2, pch = 19)
plot(distance_symm_DA_R_int_2_mean[[4]], col = DA_col_genera_int_phoc_4[(genera_breaks_int[1]+1):genera_breaks_int[2]], cex = 2, pch = 19)
plot(distance_symm_DA_R_int_3_mean[[4]], col = DA_col_genera_int_phoc_4[(genera_breaks_int[2]+1):genera_breaks_int[3]], cex = 2, pch = 19)
plot(distance_symm_DA_R_int_4_mean[[4]], col = DA_col_genera_int_phoc_4[(genera_breaks_int[3]+1):genera_breaks_int[4]], cex = 2, pch = 19)

#DA symm distances on mesh
shade3d(template_mesh_phoc_1, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs_int,,9], col =  DA_col_genera_int_phoc_4[1:genera_breaks_int[1]], type = "s",
          radius = 0.8, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_phoc_int_mesh_1.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_phoc_int_movie_1" ,dir = "Output/")

shade3d(template_mesh_phoc_2, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs_int,,27], col =  DA_col_genera_int_phoc_4[(genera_breaks_int[1]+1):genera_breaks_int[2]], type = "s",
          radius = 2, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_phoc_int_mesh_2.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_phoc_int_movie_2" ,dir = "Output/")

shade3d(template_mesh_phoc_3, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs_int,,42], col =  DA_col_genera_int_phoc_4[(genera_breaks_int[2]+1):genera_breaks_int[3]], type = "s",
          radius = 2.5, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_phoc_int_mesh_3.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_phoc_int_movie_3" ,dir = "Output/")

shade3d(template_mesh_phoc_4, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs_int,,54], col =  DA_col_genera_int_phoc_4[(genera_breaks_int[3]+1):genera_breaks_int[4]], type = "s",
          radius = 3, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_phoc_int_mesh_4.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_phoc_int_movie_4" ,dir = "Output/")

#Stenella
#5. Check output by plotting points as per-point DA distance
plot(distance_symm_DA_R_int_1_mean[[5]], col = DA_col_genera_int_sten_4[1:genera_breaks_int[1]], cex = 2, pch = 19)
plot(distance_symm_DA_R_int_2_mean[[5]], col = DA_col_genera_int_sten_4[(genera_breaks_int[1]+1):genera_breaks_int[2]], cex = 2, pch = 19)
plot(distance_symm_DA_R_int_3_mean[[5]], col = DA_col_genera_int_sten_4[(genera_breaks_int[2]+1):genera_breaks_int[3]], cex = 2, pch = 19)
plot(distance_symm_DA_R_int_4_mean[[5]], col = DA_col_genera_int_sten_4[(genera_breaks_int[3]+1):genera_breaks_int[4]], cex = 2, pch = 19)

#DA symm distances on mesh
shade3d(template_mesh_sten_1, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs_int,,15], col =  DA_col_genera_int_sten_4[1:genera_breaks_int[1]], type = "s",
          radius = 0.8, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_sten_int_mesh_1.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_sten_int_movie_1" ,dir = "Output/")

shade3d(template_mesh_sten_2, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs_int,,29], col =  DA_col_genera_int_sten_4[(genera_breaks_int[1]+1):genera_breaks_int[2]], type = "s",
          radius = 2, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_sten_int_mesh_2.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_sten_int_movie_2" ,dir = "Output/")

shade3d(template_mesh_sten_3, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs_int,,46], col =  DA_col_genera_int_sten_4[(genera_breaks_int[2]+1):genera_breaks_int[3]], type = "s",
          radius = 2.5, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_sten_int_mesh_3.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_sten_int_movie_3" ,dir = "Output/")

shade3d(template_mesh_sten_4, col = "white", alpha = 0.3, fastTransparency = T)
spheres3d(shape_array[fixed_LMs_int,,58], col =  DA_col_genera_int_sten_4[(genera_breaks_int[3]+1):genera_breaks_int[4]], type = "s",
          radius = 3, aspect = T, main = "mean",axes = F, main = F, fov = 0)

rgl.snapshot(filename = "Output/distance_diff_DA_sten_int_mesh_4.png") 
play3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6)
movie3d(spin3d(axis = c(1, 0, 0), rpm = 10), duration = 6, movie = "distance_diff_DA_sten_int_movie_4" ,dir = "Output/")


####Test significance difference between two sides linear distances NO INTERPARIETAL - entire dataset and by genus/category ----
#Use the means

#Create landmark pairs for shapes wuth no interparietal
landpairs_L_int <- landpairs[1:c(length(landpairs_L)-1),1]
landpairs_R_int <- landpairs[1:c(length(landpairs_R)-1),2]

#Test difference between sides in entire dataset - is the variance on the right higher than on the left+midline ?
symm_DA_int_test <- t.test(distance_symm_DA_R_int_mean[landpairs_L_int], distance_symm_DA_R_int_mean[landpairs_R_int])

#Summary with p-value
symm_DA_int_test

#Compare categories
symm_DA_int_test_late_new_early <- t.test(distance_symm_DA_R_int_late_new_mean, distance_symm_DA_R_int_early_mean)

symm_DA_int_test_late_new_early

symm_DA_int_test_late_new_immature <- t.test(distance_symm_DA_R_int_late_new_mean, distance_symm_DA_R_int_immature_mean)

symm_DA_int_test_late_new_immature

symm_DA_int_test_immature_adult <- t.test(distance_symm_DA_R_int_immature_mean, distance_symm_DA_R_int_adult_mean)

symm_DA_int_test_immature_adult

#Compare between categories of same genus and between same category in different genera
#Phocoena stages
symm_DA_int_test_phoc_1_2 <- t.test(distance_symm_DA_R_int_1_mean[[4]], distance_symm_DA_R_int_2_mean[[4]])

symm_DA_int_test_phoc_1_2

symm_DA_int_test_phoc_2_3 <- t.test(distance_symm_DA_R_int_2_mean[[4]], distance_symm_DA_R_int_3_mean[[4]])

symm_DA_int_test_phoc_2_3

symm_DA_int_test_phoc_3_4 <- t.test(distance_symm_DA_R_int_3_mean[[4]], distance_symm_DA_R_int_4_mean[[4]])

symm_DA_int_test_phoc_3_4

#Stenella stages
symm_DA_int_test_sten_1_2 <- t.test(distance_symm_DA_R_int_1_mean[[5]], distance_symm_DA_R_int_2_mean[[5]])

symm_DA_int_test_sten_1_2

symm_DA_int_test_sten_2_3 <- t.test(distance_symm_DA_R_int_2_mean[[5]], distance_symm_DA_R_int_3_mean[[5]])

symm_DA_int_test_sten_2_3

symm_DA_int_test_sten_3_4 <- t.test(distance_symm_DA_R_int_3_mean[[5]], distance_symm_DA_R_int_4_mean[[5]])

symm_DA_int_test_sten_3_4

#Delphinapterus stages
symm_DA_int_test_delp_1_2 <- t.test(distance_symm_DA_R_int_1_mean[[1]], distance_symm_DA_R_int_2_mean[[1]])

symm_DA_int_test_delp_1_2

symm_DA_int_test_delp_2_3 <- t.test(distance_symm_DA_R_int_2_mean[[1]], distance_symm_DA_R_int_3_mean[[1]])

symm_DA_int_test_delp_2_3

symm_DA_int_test_delp_3_4 <- t.test(distance_symm_DA_R_int_3_mean[[1]], distance_symm_DA_R_int_4_mean[[1]])

symm_DA_int_test_delp_3_4

#Lagenorhynchus stages
symm_DA_int_test_lage_1_2 <- t.test(distance_symm_DA_R_int_1_mean[[3]], distance_symm_DA_R_int_2_mean[[3]])

symm_DA_int_test_lage_1_2

symm_DA_int_test_lage_2_3 <- t.test(distance_symm_DA_R_int_2_mean[[3]], distance_symm_DA_R_int_3_mean[[3]])

symm_DA_int_test_lage_2_3

symm_DA_int_test_lage_3_4 <- t.test(distance_symm_DA_R_int_3_mean[[3]], distance_symm_DA_R_int_4_mean[[3]])

symm_DA_int_test_lage_3_4

#Globicephala stages
symm_DA_int_test_glob_1_2 <- t.test(distance_symm_DA_R_int_1_mean[[2]], distance_symm_DA_R_int_2_mean[[2]])

symm_DA_int_test_glob_1_2

symm_DA_int_test_glob_2_3 <- t.test(distance_symm_DA_R_int_2_mean[[2]], distance_symm_DA_R_int_3_mean[[2]])

symm_DA_int_test_glob_2_3

symm_DA_int_test_glob_3_4 <- t.test(distance_symm_DA_R_int_3_mean[[2]], distance_symm_DA_R_int_4_mean[[2]])

symm_DA_int_test_glob_3_4

#Save results t-tests distance to file
sink("Output/distance_symm_DA_R_int_tests.txt")
print("Entire dataset")
symm_DA_int_test

print("By category")
print(c(categories_list[2], categories_list[1]))
symm_DA_int_test_late_new_early
print(c(categories_list[2], categories_list[3]))
symm_DA_int_test_late_new_immature
print(c(categories_list[3], categories_list[4]))
symm_DA_int_test_immature_adult

print("By genus per category")
print("Delphinapterus stages")
print(c(categories_list[1], categories_list[2]))
symm_DA_int_test_delp_1_2
print(c(categories_list[2], categories_list[3]))
symm_DA_int_test_delp_2_3
print(c(categories_list[3], categories_list[4]))
symm_DA_int_test_delp_3_4
print("Globicephala stages")
print(c(categories_list[1], categories_list[2]))
symm_DA_int_test_glob_1_2
print(c(categories_list[2], categories_list[3]))
symm_DA_int_test_glob_2_3
print(c(categories_list[3], categories_list[4]))
symm_DA_int_test_glob_3_4
print("Lagenorhynchus stages")
print(c(categories_list[1], categories_list[2]))
symm_DA_int_test_lage_1_2
print(c(categories_list[2], categories_list[3]))
symm_DA_int_test_lage_2_3
print(c(categories_list[3], categories_list[4]))
symm_DA_int_test_lage_3_4
print("Phocoena stages")
print(c(categories_list[1], categories_list[2]))
symm_DA_int_test_phoc_1_2
print(c(categories_list[2], categories_list[3]))
symm_DA_int_test_phoc_2_3
print(c(categories_list[3], categories_list[4]))
symm_DA_int_test_phoc_3_4
print("Stenella stages")
print(c(categories_list[1], categories_list[2]))
symm_DA_int_test_sten_1_2
print(c(categories_list[2], categories_list[3]))
symm_DA_int_test_sten_2_3
print(c(categories_list[3], categories_list[4]))
symm_DA_int_test_sten_3_4

sink() 

####Line plot mean distances per genus per stage ----

#Create vector with means per stage per genus
distance_diff_DA_R_int_means_delp <- c(symm_DA_int_test_delp_1_2[["estimate"]][["mean of x"]], symm_DA_int_test_delp_1_2[["estimate"]][["mean of y"]],
                                     symm_DA_int_test_delp_3_4[["estimate"]][["mean of x"]], symm_DA_int_test_delp_3_4[["estimate"]][["mean of y"]])
distance_diff_DA_R_int_means_glob <- c(symm_DA_int_test_glob_1_2[["estimate"]][["mean of x"]], symm_DA_int_test_glob_1_2[["estimate"]][["mean of y"]],
                                     symm_DA_int_test_glob_3_4[["estimate"]][["mean of x"]], symm_DA_int_test_glob_3_4[["estimate"]][["mean of y"]])
distance_diff_DA_R_int_means_lage <- c(symm_DA_int_test_lage_1_2[["estimate"]][["mean of x"]], symm_DA_int_test_lage_1_2[["estimate"]][["mean of y"]],
                                     symm_DA_int_test_lage_3_4[["estimate"]][["mean of x"]], symm_DA_int_test_lage_3_4[["estimate"]][["mean of y"]])
distance_diff_DA_R_int_means_phoc <- c(symm_DA_int_test_phoc_1_2[["estimate"]][["mean of x"]], symm_DA_int_test_phoc_1_2[["estimate"]][["mean of y"]],
                                     symm_DA_int_test_phoc_3_4[["estimate"]][["mean of x"]], symm_DA_int_test_phoc_3_4[["estimate"]][["mean of y"]])
distance_diff_DA_R_int_means_sten <- c(symm_DA_int_test_sten_1_2[["estimate"]][["mean of x"]], symm_DA_int_test_sten_1_2[["estimate"]][["mean of y"]],
                                     symm_DA_int_test_sten_3_4[["estimate"]][["mean of x"]], symm_DA_int_test_sten_3_4[["estimate"]][["mean of y"]])


distance_diff_DA_R_int_means_genera <- c(distance_diff_DA_R_int_means_delp, distance_diff_DA_R_int_means_glob, distance_diff_DA_R_int_means_lage,
                                       distance_diff_DA_R_int_means_phoc, distance_diff_DA_R_int_means_sten)

distance_diff_DA_R_int_table <- data.frame(means = distance_diff_DA_R_int_means_genera, genus = unlist(genera_categories_rep),
                                         category = rep(categories_list, times = length(genera_list)))
distance_diff_DA_R_int_table$family <- if_else(distance_diff_DA_R_int_table$genus == genera_list[1], "Monodontidae", 
                                             if_else(distance_diff_DA_R_int_table$genus == genera_list[4], "Phocoenidae", "Delphinidae"))
#Order by category
distance_diff_DA_R_int_table$category <- factor(distance_diff_DA_R_int_table$category, 
                                              levels = c("1-early", "2-late/new", "3-immature", "4-adult")) #copy from string printed with the code above
#Order
distance_diff_DA_R_int_table <- distance_diff_DA_R_int_table[order(distance_diff_DA_R_int_table$category),]
#Add confidence intervals for each point
#Means for each specimen per genus and growth stage
distance_symm_DA_R_int_1_spec_mean <- apply(distance_symm_DA_R_int[,,rows_categories[[1]]], 2, mean)
distance_symm_DA_R_int_2_spec_mean <- apply(distance_symm_DA_R_int[,,rows_categories[[2]]], 2, mean)
distance_symm_DA_R_int_3_spec_mean <- apply(distance_symm_DA_R_int[,,rows_categories[[3]]], 2, mean)
distance_symm_DA_R_int_4_spec_mean <- apply(distance_symm_DA_R_int[,,rows_categories[[4]]], 2, mean)

distance_diff_DA_R_int_means_specs <- as.vector(c(distance_symm_DA_R_int_1_spec_mean, distance_symm_DA_R_int_2_spec_mean,
                                                  distance_symm_DA_R_int_3_spec_mean, distance_symm_DA_R_int_4_spec_mean))

#Create dataframe for error bars
distance_diff_DA_R_int_error <-  data.frame(means = distance_diff_DA_R_int_means_specs,
                                            genus = genera, family = families, category = categories)

distance_diff_DA_R_int_error <- distance_diff_DA_R_int_error %>% group_by(genus,category) %>%
  mutate(lower = min(means), upper = max(means))

#Plot with lines connecting different stages per genus
distance_diff_DA_R_int_plot <- 
  ggplot(distance_diff_DA_R_int_table, aes(x = category, y = means, color = genus, shape = category, group = genus)) + 
  geom_point(data = distance_diff_DA_R_int_error, aes(x = category, y = means, color = genus, group = genus), 
             inherit.aes = F, show.legend = F, size = 1.5)+
  geom_line(aes(linetype = family), size = 1) + 
  geom_point(size = 3, fill = "white", stroke = 1.5)+
  scale_shape_manual(values = shapes_cat)+
  scale_colour_manual(values = mypalette_taxa)+ #to be grouped as they appear in tibble    
  scale_linetype_manual(values = c(1, 2, 4))+
  theme_classic(base_size = 12)+
  ylab("Mean distances")+
  xlab("Growth stage")+
  ggtitle ("Mean distances DA and symmetric component - no interparietal")+ 
  scale_x_discrete(labels = c("Early Fetus", "Late Fetus/Neonate", "Juvenile", "Adult"))+
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12), axis.title.y = element_text(vjust = 2), axis.title.x = element_text(vjust = -1),
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 11), legend.position = "bottom", legend.direction = "horizontal")+
  guides(shape = guide_legend(label = F, title = NULL, override.aes = list(shape = NA, linetype = 0, fill = NA, colour = NA)),
         colour = guide_legend(label = F, title = NULL, override.aes = list(shape = NA, linetype = 0, fill = NA)), 
         linetype = guide_legend(keywidth = unit(3, "char"), override.aes = list(color = "black", size = 0.8)))+
  annotate("text", x = 1, y = 0.004, label = "add phylopics")
distance_diff_DA_R_int_plot 

#Add significance for each segment
distance_diff_DA_R_int_plot  <- 
  distance_diff_DA_R_int_plot + # 1 line per taxon, alphabetical order
  annotate("text", x = 1.55, y = 0.0034, label = "***", family = "", fontface = 4, size = 7, colour = mypalette_taxa[1])+
  annotate("text", x = 2.5, y = 0.0036, label = "**", family = "", fontface = 4, size = 7, colour = mypalette_taxa[2])+
  annotate("text", x = 3.25, y = 0.0028, label = "**", family = "", fontface = 4, size = 7, colour = mypalette_taxa[3])+
  annotate("text", x = 1.5, y = 0.0021, label = "*", family = "", fontface = 4, size = 7, colour = mypalette_taxa[4])+
  annotate("text", x = 1.5, y = 0.0015, label = "**", family = "", fontface = 4, size = 7, colour = "black")
distance_diff_DA_R_int_plot 

####Heatmaps plots for significant differences in distances means ----

#Add genus and category factor for pairwise diffs
distance_diff_DA_R_int_table$genus_category <- paste(distance_diff_DA_R_int_table$genus, distance_diff_DA_R_int_table$category, sep="_")

#Loop replacements categories
for (u in 1:length(categories_list)){
  distance_diff_DA_R_int_table$genus_category <- str_replace_all(distance_diff_DA_R_int_table$genus_category, categories_list[u], categories_list_short[u])
}

#Loop replacements genera
for (t in 1:length(genera_list)){
  distance_diff_DA_R_int_table$genus_category <- str_replace_all(distance_diff_DA_R_int_table$genus_category, genera_list[t], genera_list_short[t])
}
distance_diff_DA_R_int_table$genus_category

#Unlist means per landmark to calculate pairwise t tests - useful for heatmap
#Change list to matrix
distance_symm_DA_R_int_1_mean_matrix <- matrix(unlist(distance_symm_DA_R_int_1_mean), ncol = 1, byrow = T)
distance_symm_DA_R_int_2_mean_matrix <- matrix(unlist(distance_symm_DA_R_int_2_mean), ncol = 1, byrow = T)
distance_symm_DA_R_int_3_mean_matrix <- matrix(unlist(distance_symm_DA_R_int_3_mean), ncol = 1, byrow = T)
distance_symm_DA_R_int_4_mean_matrix <- matrix(unlist(distance_symm_DA_R_int_4_mean), ncol = 1, byrow = T)

distance_symm_DA_R_int_mean_all <- rbind(distance_symm_DA_R_int_1_mean_matrix, distance_symm_DA_R_int_2_mean_matrix, 
                                   distance_symm_DA_R_int_3_mean_matrix, distance_symm_DA_R_int_4_mean_matrix)
#Convert to data frame for easier anal
distance_symm_DA_R_int_mean_all <- as.data.frame(distance_symm_DA_R_int_mean_all)
#Add groups
distance_symm_DA_R_int_mean_all$groups <- rep(distance_diff_DA_R_int_table$genus_category, each = length(fixed_LMs_int))

#Pairwise t test to get p values for heatmap plot
pairwise_distance_symm_DA_R_int_mean_all <- pairwise.t.test(distance_symm_DA_R_int_mean_all[,1], distance_symm_DA_R_int_mean_all[,2], 
                                                      var.equal = F, pool.sd = FALSE, paired = F)

stage_breaks_int <- seq(0,length(distance_symm_DA_R_int_mean_all[,1]),length(distance_symm_DA_R_int_1_mean_matrix))

#Pairwise t test to get p values by stage
pairwise_distance_symm_DA_R_int_mean_1 <- pairwise.t.test(distance_symm_DA_R_int_mean_all[1:stage_breaks_int[2],1], distance_symm_DA_R_int_mean_all[1:stage_breaks_int[2],2], 
                                                    var.equal = F, pool.sd = FALSE, paired = F)
pairwise_distance_symm_DA_R_int_mean_2 <- pairwise.t.test(distance_symm_DA_R_int_mean_all[(stage_breaks_int[2]+1):stage_breaks_int[3],1], distance_symm_DA_R_int_mean_all[(stage_breaks_int[2]+1):stage_breaks_int[3],2], 
                                                    var.equal = F, pool.sd = FALSE, paired = F)
pairwise_distance_symm_DA_R_int_mean_3 <- pairwise.t.test(distance_symm_DA_R_int_mean_all[(stage_breaks_int[3]+1):stage_breaks_int[4],1], distance_symm_DA_R_int_mean_all[(stage_breaks_int[3]+1):stage_breaks_int[4],2], 
                                                    var.equal = F, pool.sd = FALSE, paired = F)
pairwise_distance_symm_DA_R_int_mean_4 <- pairwise.t.test(distance_symm_DA_R_int_mean_all[(stage_breaks_int[4]+1):stage_breaks_int[5],1], distance_symm_DA_R_int_mean_all[(stage_breaks_int[4]+1):stage_breaks_int[5],2], 
                                                    var.equal = F, pool.sd = FALSE, paired = F)

#Save results to file
sink("Output/pairwise_distance_symm_DA_R_int_mean.txt")
print("all genera/categories")
pairwise_distance_symm_DA_R_int_mean_all

print("genera by category")
pairwise_distance_symm_DA_R_int_mean_1
pairwise_distance_symm_DA_R_int_mean_2
pairwise_distance_symm_DA_R_int_mean_3
pairwise_distance_symm_DA_R_int_mean_4
sink()  

####Plot for all data
#Save p-values as object
dist_pvals_int <- pairwise_distance_symm_DA_R_int_mean_all[["p.value"]]

#Melt to make table in the format needed for heatmap
dist_pvals_int_melt <- melt(dist_pvals_int, value.name = "p", na.rm = TRUE)

#Create columns where only significant values are shown
dist_pvals_int_melt <- dist_pvals_int_melt %>% mutate(sig_p = ifelse(p < .05, T, F),
                                              p_if_sig = ifelse(sig_p, p, NA))
dist_pvals_int_melt

#Nice heatmap plot
distance_diff_DA_R_int_genus_category_heatmap_ggplot <- ggplot(data = dist_pvals_int_melt, aes(Var2, Var1, fill = p_if_sig))+
  geom_tile(colour = "gray80")+
  scale_fill_gradient2(low = mypalette_symm[9], high = mypalette_symm[2], mid = mypalette_symm[5], #negative correlations are in blue color and positive correlations in red. 
                       midpoint = 0.03, limit = c(0, 0.049), space = "Lab", #scale is from min to max p-values
                       na.value =  mypalette_symm[1], name = "P-values < 0.05") + 
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_flip()+
  ggtitle ("Mean distances DA and symmetric component - no interparietal")+ 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
        axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x =  element_text(size = 10),
        axis.text.y =  element_text(size = 10, vjust = -0.2, margin = NULL), panel.grid.major = element_blank(),
        legend.justification = c(1, 0), legend.position = c(0.4, 0.7),  legend.direction = "horizontal",
        legend.title = element_text(size = 11), legend.text = element_text(size = 10))+
  guides(fill = guide_colorbar(barwidth = 8, barheight = 1.3,
                               title.position = "top", title.hjust = 0.5))
distance_diff_DA_R_int_genus_category_heatmap_ggplot

####Plots by category
#Save p-values as object
dist_pvals_int_1 <- pairwise_distance_symm_DA_R_int_mean_1[["p.value"]]
dist_pvals_int_2 <- pairwise_distance_symm_DA_R_int_mean_2[["p.value"]]
dist_pvals_int_3 <- pairwise_distance_symm_DA_R_int_mean_3[["p.value"]]
dist_pvals_int_4 <- pairwise_distance_symm_DA_R_int_mean_4[["p.value"]]

#Melt to make table in the format needed for heatmap
dist_pvals_int_melt_1 <- melt(dist_pvals_int_1, value.name = "p", na.rm = TRUE)
dist_pvals_int_melt_2 <- melt(dist_pvals_int_2, value.name = "p", na.rm = TRUE)
dist_pvals_int_melt_3 <- melt(dist_pvals_int_3, value.name = "p", na.rm = TRUE)
dist_pvals_int_melt_4 <- melt(dist_pvals_int_4, value.name = "p", na.rm = TRUE)

#Create columns where only significant values are shown
dist_pvals_int_melt_1 <- dist_pvals_int_melt_1 %>% mutate(sig_p = ifelse(p < .05, T, F),
                                                  p_if_sig = ifelse(sig_p, p, NA))
dist_pvals_int_melt_2 <- dist_pvals_int_melt_2 %>% mutate(sig_p = ifelse(p < .05, T, F),
                                                  p_if_sig = ifelse(sig_p, p, NA))
dist_pvals_int_melt_3 <- dist_pvals_int_melt_3 %>% mutate(sig_p = ifelse(p < .05, T, F),
                                                  p_if_sig = ifelse(sig_p, p, NA))
dist_pvals_int_melt_4 <- dist_pvals_int_melt_4 %>% mutate(sig_p = ifelse(p < .05, T, F),
                                                  p_if_sig = ifelse(sig_p, p, NA))

#Nice heatmap plot
distance_diff_DA_R_int_genus_category_heatmap_ggplot_1 <- ggplot(data = dist_pvals_int_melt_1, aes(Var2, Var1, fill = p_if_sig))+
  geom_tile(colour = "gray80")+
  scale_fill_gradient2(low = mypalette_symm[9], high = mypalette_symm[2], mid = mypalette_symm[5], #negative correlations are in blue color and positive correlations in red. 
                       midpoint = 0.03, limit = c(0, 0.049), space = "Lab", #scale is from min to max p-values
                       na.value =  mypalette_symm[1], name = "P-values < 0.05") + 
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_flip()+
  ggtitle ("Early fetus")+ 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
        axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x =  element_text(size = 10),
        axis.text.y =  element_text(size = 10, vjust = -0.2, margin = NULL), panel.grid.major = element_blank(),
        legend.justification = c(1, 0), legend.position = c(0.4, 0.7),  legend.direction = "horizontal",
        legend.title = element_text(size = 11), legend.text = element_text(size = 10))+
  guides(fill = guide_colorbar(barwidth = 8, barheight = 1.3,
                               title.position = "top", title.hjust = 0.5))
distance_diff_DA_R_int_genus_category_heatmap_ggplot_1

distance_diff_DA_R_int_genus_category_heatmap_ggplot_2 <- ggplot(data = dist_pvals_int_melt_2, aes(Var2, Var1, fill = p_if_sig))+
  geom_tile(colour = "gray80")+
  scale_fill_gradient2(low = mypalette_symm[9], high = mypalette_symm[2], mid = mypalette_symm[5], #negative correlations are in blue color and positive correlations in red. 
                       midpoint = 0.03, limit = c(0, 0.049), space = "Lab", #scale is from min to max p-values
                       na.value =  mypalette_symm[1], name = "P-values < 0.05") + 
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_flip()+
  ggtitle ("Late fetus/Neonate")+ 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
        axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x =  element_text(size = 10),
        axis.text.y =  element_text(size = 10, vjust = -0.2, margin = NULL), panel.grid.major = element_blank(),
        legend.justification = c(1, 0), legend.position = c(0.4, 0.7),  legend.direction = "horizontal",
        legend.title = element_text(size = 11), legend.text = element_text(size = 10))+
  guides(fill = guide_colorbar(barwidth = 8, barheight = 1.3,
                               title.position = "top", title.hjust = 0.5))
distance_diff_DA_R_int_genus_category_heatmap_ggplot_2

distance_diff_DA_R_int_genus_category_heatmap_ggplot_3 <- ggplot(data = dist_pvals_int_melt_3, aes(Var2, Var1, fill = p_if_sig))+
  geom_tile(colour = "gray80")+
  scale_fill_gradient2(low = mypalette_symm[9], high = mypalette_symm[2], mid = mypalette_symm[5], #negative correlations are in blue color and positive correlations in red. 
                       midpoint = 0.03, limit = c(0, 0.049), space = "Lab", #scale is from min to max p-values
                       na.value =  mypalette_symm[1], name = "P-values < 0.05") + 
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_flip()+
  ggtitle ("Juvenile")+ 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
        axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x =  element_text(size = 10),
        axis.text.y =  element_text(size = 10, vjust = -0.2, margin = NULL), panel.grid.major = element_blank(),
        legend.justification = c(1, 0), legend.position = c(0.4, 0.7),  legend.direction = "horizontal",
        legend.title = element_text(size = 11), legend.text = element_text(size = 10))+
  guides(fill = guide_colorbar(barwidth = 8, barheight = 1.3,
                               title.position = "top", title.hjust = 0.5))
distance_diff_DA_R_int_genus_category_heatmap_ggplot_3

distance_diff_DA_R_int_genus_category_heatmap_ggplot_4 <- ggplot(data = dist_pvals_int_melt_4, aes(Var2, Var1, fill = p_if_sig))+
  geom_tile(colour = "gray80")+
  scale_fill_gradient2(low = mypalette_symm[9], high = mypalette_symm[2], mid = mypalette_symm[5], #negative correlations are in blue color and positive correlations in red. 
                       midpoint = 0.03, limit = c(0, 0.049), space = "Lab", #scale is from min to max p-values
                       na.value =  mypalette_symm[1], name = "P-values < 0.05") + 
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_flip()+
  ggtitle ("Adult")+ 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
        axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x =  element_text(size = 10),
        axis.text.y =  element_text(size = 10, vjust = -0.2, margin = NULL), panel.grid.major = element_blank(),
        legend.justification = c(1, 0), legend.position = c(0.4, 0.7),  legend.direction = "horizontal",
        legend.title = element_text(size = 11), legend.text = element_text(size = 10))+
  guides(fill = guide_colorbar(barwidth = 8, barheight = 1.3,
                               title.position = "top", title.hjust = 0.5))
distance_diff_DA_R_int_genus_category_heatmap_ggplot_4

#Arrange in 1 plot
ggarrange(distance_diff_DA_R_int_genus_category_heatmap_ggplot_1, distance_diff_DA_R_int_genus_category_heatmap_ggplot_2,
          distance_diff_DA_R_int_genus_category_heatmap_ggplot_3, distance_diff_DA_R_int_genus_category_heatmap_ggplot_4, 
          ncol = 2, nrow = 2, common.legend = T)

#####
#Next - ch. 7 - Ancestral allometries