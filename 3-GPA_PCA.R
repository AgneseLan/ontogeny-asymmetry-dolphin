
#===========================================================#
#                                                           #
#     CURVES AND POINTS ANALYSES - ODONTOCETE FAMILIES      #
#                                                           #
#===========================================================#

#CH.3 - Prepare final dataset for analysis, run GPA and PCA

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
library(car)
library(Rvcg)

#devtools::install_github("JeroenSmaers/evomap")
#devtools::install_github("wabarr/ggphylomorpho")
#devtools::install_github("aphanotus/borealis")

#DATA PREP ----

###SET WD to root folder from console!! -->

#Import classifiers
classifiers <- read_csv("Data/specimens.csv")
glimpse(classifiers)

#Make sure the specimens are in the same order before proceeding!!!
identical(dimnames(final_dataset)[[3]], classifiers$specimen, attrib.as.set = T)

#Create new columns needed for analyses
#Calculate max TL and BZW for each genus
max_TL_BZW <- classifiers %>% group_by(genus) %>% summarize(max_TL = max(TL_m), max_BZW = max(BZW_mm))

#Count number of specimens per genus
count_genus <- classifiers %>% count(genus)

#Create new columns with max TL and BZW repeated for all specimens of each genus
classifiers$max_TL <- c(rep(max_TL_BZW$max_TL[1], times = count_genus$n[1]), rep(max_TL_BZW$max_TL[2], times = count_genus$n[2]), 
                        rep(max_TL_BZW$max_TL[3], times = count_genus$n[3]), rep(max_TL_BZW$max_TL[4], times = count_genus$n[4]), 
                        rep(max_TL_BZW$max_TL[5], times = count_genus$n[5]))
classifiers$max_BZW <- c(rep(max_TL_BZW$max_BZW[1], times = count_genus$n[1]), rep(max_TL_BZW$max_BZW[2], times = count_genus$n[2]), 
                        rep(max_TL_BZW$max_BZW[3], times = count_genus$n[3]), rep(max_TL_BZW$max_BZW[4], times = count_genus$n[4]), 
                        rep(max_TL_BZW$max_BZW[5], times = count_genus$n[5]))

#Calculate % max TL and max BZW for each specimen in each genus
classifiers <- classifiers %>% mutate(TL_100 = (TL_m*100/max_TL), BZW_100 = (BZW_mm*100/max_BZW))   
glimpse(classifiers)

##Order shape data by category, useful for plot legend
#Check levels/category names
as.factor(classifiers$category)

#Order shape data as category
order_dataframe <- geomorph.data.frame(raw_data = final_dataset, category = classifiers$category)
#Check specimens order
dimnames(order_dataframe$raw_data)[[3]]

#Order dataframe
order_dataframe$category <- factor(order_dataframe$category,
                                   levels = c("1-early", "2-late/new", "3-immature", "4-adult"))

#Create new shape data object ordered by category
shape_array <- order_dataframe$raw_data[,,order(order_dataframe$category)]
#Check it worked
dimnames(shape_array)[[3]]

##Order classifiers data by category, useful for plot legend
#Make factor for variable
classifiers$category <- factor(classifiers$category, 
                               levels = c("1-early", "2-late/new", "3-immature", "4-adult")) #copy from string printed with the code above
#Order
classifiers <- classifiers[order(classifiers$category),]
View(classifiers)

#Check specimens and classifiers are in the same order
identical(dimnames(shape_array)[[3]], classifiers$specimen, attrib.as.set = T) 

#Check for outliers in raw data shape array, they would be displayed in red
#Might be due to absent bones, check misstable list
plotOutliers(shape_array)
#Plot outliers landmarks to search for possible problems - check file name to find number in classifiers
checkLM(final_dataset, path="Data/ply/", pt.size = 2, suffix=".ply", render = "s", begin = 17, point = "s")

#Save specimens names as object
specimens <- dimnames(shape_array)[[3]]

#Save Id as object, useful for later analysis
Ids <- classifiers$code

#Save growth stages as factor, useful for later analysis
categories <- classifiers$category
#Check how many colors are needed
as.factor(categories)

#Save genera as factor, useful for later analysis
genera <- classifiers$genus
#Check how many colors are needed
as.factor(genera)

#Save families as factor, useful for later analysis
families <- classifiers$family

##Create project palettes----
mypalette_paired <- brewer.pal(12,"Paired")
image(1:12, 1, as.matrix(1:12), col = mypalette_paired, xlab = "Paired",
      ylab = "", yaxt = "n")

mypalette_blue <- as.matrix(ggthemes_data[["tableau"]][["color-palettes"]][["ordered-sequential"]][["Blue"]][["value"]])
image(1:20, 1, as.matrix(1:20), col = mypalette_blue, xlab = "Blue",
      ylab = "", yaxt = "n")

#Palette for 5 genera - Delphinapterus, Globicephala, Lagenorhynchus, Phocoena, Stenella
mypalette_taxa <- c(mypalette_paired[2], mypalette_paired[5], mypalette_paired[7], mypalette_paired[10], mypalette_paired[3])
image(1:5, 1, as.matrix(1:5), col = mypalette_taxa, xlab = "taxa colors - Delphinapterus, Globicephala, Lagenorhynchus, Phocoena, Stenella",
      ylab = "", yaxt = "n")

#Palette for categories - early, late/new, immature, adult
mypalette_category <- c(mypalette_blue[3,], mypalette_blue[7,], mypalette_blue[13,], mypalette_blue[18,])
image(1:4, 1, as.matrix(1:4), col = mypalette_category, xlab = "categories colors - early, late/new, immature, adult", 
      ylab = "", yaxt = "n")

#Create shape palette 5 genera - Delphinapterus, Globicephala, Lagenorhynchus, Phocoena, Stenella and 4 categories
shapes <- c(23,24,22,25,21)
shapes_cat <- c(23,24,22,21)

##Images for plots
Delph <- readPNG("Data/delphinapterus.png")
Globi <- readPNG("Data/globicephala.png")
Lage <- readPNG("Data/lagenorhynchus.png")
Phoc <- readPNG("Data/phocoena.png")
Sten <- readPNG("Data/stenella.png")

#GPA ALIGNMENT ----

#Procrustes alignment, should also show mean config coordinates
gpa <- gpagen(shape_array) 

#Save Centroid size as object
Csize <- gpa$Csize 
#Log-transform Centroid size as object
logCsize <- log10(Csize) 

#Save mean shape to create links
mean_shape <- gpa$consensus 

#Coordinates of all specimens after GPA alignment
coords <- gpa$coords 

#Plot all specimens with mean to check that all landmarks are ok
plotAllSpecimens(coords, mean = TRUE, label = F, plot.param = list(pt.cex = 0.05, mean.cex = 3, mean.bg = "black"))
#Save screenshot of 3D viewer
rgl.snapshot(filename = "Output/plot_gpa_points.png") 
rgl.snapshot(filename = "Output/plot_gpa_points1.png") 

#Check for outliers, they would be displayed in red - most immature ones are normal as outliers
plotOutliers(coords)
#Plot landmarks from outliers in 3D to check how they look
spheres3d(coords[,,3], r = 0.002)

#checkLM(shape_array, path="", pt.size = 2, suffix=".ply", render="s", begin = 65) 
#to run if needed to check plotting of points on mesh

##Make data frame for analyses in geomorph
gdf <- geomorph.data.frame(coords = coords,
                           Id = classifiers$code, genus = classifiers$genus, family = classifiers$family, 
                           category = classifiers$category, TL_100 = classifiers$TL_100, BZW_100 = classifiers$BZW_100, 
                           Age_100 = classifiers$Age_100, size = logCsize)
glimpse(gdf)


#PCA COMPLETE DATASET ----

#Run PCA on complete dataset
PCA_all <- gm.prcomp(gdf$coords)

#List of PC components and proportion of variation
PCA_all 

#Save PCA results to file
sink("Output/PCA_all_components.txt")
print("PCA complete dataset")
print(PCA_all)
sink() 

#Change row names to codes to make plot readable
row.names(PCA_all$x) <- gdf$Id

##View plot
plot(PCA_all, main = "PCA all data - PC1-PC2",  pch = 21, #title and type of point to be used
     col = "deeppink",   #border of points
     bg = "deeppink",    #fill of points
     cex = 1,            #size of points (1=regular)
     font.main = 2)       #bold font for title
#Add quick labels to plot
text(x = PCA_all$x[,1], y = PCA_all$x[,2], labels = rownames(PCA_all$x), 
     pos = 1,       #position relative to data point
     offset = 0.5,  #distance from data point
     cex = 0.75)    #font size (1=regular)

##View plot
plot(PCA_all, axis1 = 1, axis2 = 3, main = "PCA all data - PC1-PC3",  pch = 21, #title and type of point to be used
     col = "deeppink",   #border of points
     bg = "deeppink",    #fill of points
     cex = 1,            #size of points (1=regular)
     font.main = 2)       #bold font for title
#Add quick labels to plot
text(x = PCA_all$x[,1], y = PCA_all$x[,3], labels = rownames(PCA_all$x), 
     pos = 1,       #position relative to data point
     offset = 0.5,  #distance from data point
     cex = 0.75)    #font size (1=regular)

#Save PC scores as object to use later
pcscores_all <- PCA_all$x 

#Save shapes of extremes for axes used in plot
PC1min_all <- PCA_all[["shapes"]][["shapes.comp1"]][["min"]]
PC1max_all <- PCA_all[["shapes"]][["shapes.comp1"]][["max"]] 
PC2min_all <- PCA_all[["shapes"]][["shapes.comp2"]][["min"]] 
PC2max_all <- PCA_all[["shapes"]][["shapes.comp2"]][["max"]] 

#Show 3D deformation from mean with points overlay, do this for all 4 extremes - using spheres3D for points
#PC1min colors
#spheres3d(mean_shape, radius=.0005, color = "gray60", alpha = 0.5, fastTransparency = T) - plot mean specimens with transparency
PC1min_all_points <- c(
spheres3d(PC1min_all[maxilla,], radius=.001, color = mypalette_paired[8]),
spheres3d(PC1min_all[premaxilla,], radius=.001, color = mypalette_paired[6]),
spheres3d(PC1min_all[palatine,], radius=.001, color = mypalette_paired[7]),
spheres3d(PC1min_all[nasals,], radius=.001, color = mypalette_paired[4]),
spheres3d(PC1min_all[orbit,], radius=.001, color = mypalette_paired[3]),
spheres3d(PC1min_all[squamosal,], radius=.001, color = mypalette_paired[12]),
spheres3d(PC1min_all[basioccipital,], radius=.001, color = mypalette_paired[5]),
spheres3d(PC1min_all[interparietal,], radius=.001, color = mypalette_paired[1]),
spheres3d(PC1min_all[supraoccipital,], radius=.001, color = mypalette_paired[2]),
spheres3d(PC1min_all[condyles,], radius=.001, color = mypalette_paired[10]),
spheres3d(PC1min_all[exoccipital,], radius=.001,  color = mypalette_paired[9]))

rgl.snapshot(filename = "Output/PC1min_all.png") 
#Save 3D window as html file - 3D widget
scene <- scene3d()
widget <- rglwidget()
filename <- tempfile(fileext = ".html")
htmlwidgets::saveWidget(rglwidget(), filename)
browseURL(filename) 

#PC1max colors
PC1max_all_points <- c(
  spheres3d(PC1max_all[maxilla,], radius=.001, color = mypalette_paired[8]),
  spheres3d(PC1max_all[premaxilla,], radius=.001, color = mypalette_paired[6]),
  spheres3d(PC1max_all[palatine,], radius=.001, color = mypalette_paired[7]),
  spheres3d(PC1max_all[nasals,], radius=.001, color = mypalette_paired[4]),
  spheres3d(PC1max_all[orbit,], radius=.001, color = mypalette_paired[3]),
  spheres3d(PC1max_all[squamosal,], radius=.001, color = mypalette_paired[12]),
  spheres3d(PC1max_all[basioccipital,], radius=.001, color = mypalette_paired[5]),
  spheres3d(PC1max_all[interparietal,], radius=.001, color = mypalette_paired[1]),
  spheres3d(PC1max_all[supraoccipital,], radius=.001, color = mypalette_paired[2]),
  spheres3d(PC1max_all[condyles,], radius=.001, color = mypalette_paired[10]),
  spheres3d(PC1max_all[exoccipital,], radius=.001,  color = mypalette_paired[9]))

rgl.snapshot(filename = "Output/PC1max_all.png") 
#Save 3D window as html file - 3D widget
htmlwidgets::saveWidget(rglwidget(), filename)
browseURL(filename) 

#PC2min colors
PC2min_all_points <- c(
  spheres3d(PC2min_all[maxilla,], radius=.001, color = mypalette_paired[8]),
  spheres3d(PC2min_all[premaxilla,], radius=.001, color = mypalette_paired[6]),
  spheres3d(PC2min_all[palatine,], radius=.001, color = mypalette_paired[7]),
  spheres3d(PC2min_all[nasals,], radius=.001, color = mypalette_paired[4]),
  spheres3d(PC2min_all[orbit,], radius=.001, color = mypalette_paired[3]),
  spheres3d(PC2min_all[squamosal,], radius=.001, color = mypalette_paired[12]),
  spheres3d(PC2min_all[basioccipital,], radius=.001, color = mypalette_paired[5]),
  spheres3d(PC2min_all[interparietal,], radius=.001, color = mypalette_paired[1]),
  spheres3d(PC2min_all[supraoccipital,], radius=.001, color = mypalette_paired[2]),
  spheres3d(PC2min_all[condyles,], radius=.001, color = mypalette_paired[10]),
  spheres3d(PC2min_all[exoccipital,], radius=.001,  color = mypalette_paired[9]))

rgl.snapshot(filename = "Output/PC2min_all.png") 
#Save 3D window as html file - 3D widget
htmlwidgets::saveWidget(rglwidget(), filename)
browseURL(filename) 

#PC2max colors
PC2max_all_points <- c(
  spheres3d(PC2max_all[maxilla,], radius=.001, color = mypalette_paired[8]),
  spheres3d(PC2max_all[premaxilla,], radius=.001, color = mypalette_paired[6]),
  spheres3d(PC2max_all[palatine,], radius=.001, color = mypalette_paired[7]),
  spheres3d(PC2max_all[nasals,], radius=.001, color = mypalette_paired[4]),
  spheres3d(PC2max_all[orbit,], radius=.001, color = mypalette_paired[3]),
  spheres3d(PC2max_all[squamosal,], radius=.001, color = mypalette_paired[12]),
  spheres3d(PC2max_all[basioccipital,], radius=.001, color = mypalette_paired[5]),
  spheres3d(PC2max_all[interparietal,], radius=.001, color = mypalette_paired[1]),
  spheres3d(PC2max_all[supraoccipital,], radius=.001, color = mypalette_paired[2]),
  spheres3d(PC2max_all[condyles,], radius=.001, color = mypalette_paired[10]),
  spheres3d(PC2max_all[exoccipital,], radius=.001,  color = mypalette_paired[9]))

rgl.snapshot(filename = "Output/PC2max_all.png") 
#Save 3D window as html file - 3D widget
htmlwidgets::saveWidget(rglwidget(), filename)
browseURL(filename) 

##3D windows save
{#Save screenshot of 3D window, useful for lateral and dorsal views - use screen snip if it fails
rgl.snapshot(filename = "Output/PC1max_all.png") 
#Save 3D window as html file - 3D widget
scene <- scene3d()
widget <- rglwidget()
filename <- tempfile(fileext = ".html")
htmlwidgets::saveWidget(rglwidget(), filename)
browseURL(filename)    #from browser save screenshots as PNG (right click on image-save image) and save HTML (right click on white space-save as->WebPage HTML, only)
}

##Make better PCA plot using ggplot
#Read PC scores as tibble
pcscores_all <- as_tibble(pcscores_all)
#Add labels and other attributes to tibble as columns
pcscores_all <- pcscores_all %>% mutate(specimens = gdf$Id, group = gdf$family, category = gdf$category,
                                        genus = gdf$genus, size = gdf$size, TL = gdf$TL_100, BZW = gdf$BZW_100)
glimpse(pcscores_all)

#Nice PCA plot with stages and groups
PCA_all_ggplot <- ggplot(pcscores_all, aes(x = Comp1, y = Comp2, label = specimens, colour = category, fill = category))+
  geom_point(size = 3, aes(shape = genus))+
  geom_text_repel(colour = "black", size = 4, max.overlaps = 40)+
  scale_colour_manual(name = "Growth stage", labels =  c("Early Fetus", "Late Fetus/Neonate", "Juvenile", "Adult"), #to be ordered as they appear in tibble
                      values = mypalette_category, aesthetics = c("colour","fill"))+            #legend and color adjustments
  scale_shape_manual(name = "Genus", labels = c("Delphinapterus", "Globicephala", "Lagenorhynchus", "Phocoena", "Stenella"), #copy from as.factor(genera)
                     values = shapes)+
  theme_bw()+
  xlab("PC 1 (56.95%)")+ #copy this from standard PCA plot (PCA_all_plot)
  ylab("PC 2 (14.53%)")+
  ggtitle("PCA all data")+
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) 

#Visualize plot and save as PDF using menu in bar on the right
PCA_all_ggplot

#Make hulls for PCA plot with hulls around categories (4 stages: Early Fetus, Late Fetus/Neonate, Juvenile, Adult)
hulls_all_category <- pcscores_all %>%
  group_by(category) %>%
  slice(chull(Comp1, Comp2)) %>%
  rename(x = Comp1, y = Comp2)

#Nice PCA plot with hulls around categories (4 stages: Early Fetus, Late Fetus/Neonate, Juvenile, Adult)
PCA_all_category_ggplot <- ggplot(pcscores_all, aes(x = Comp1, y = Comp2, label = specimens, colour = category, fill = category))+
  geom_point(size = 3, aes(shape = genus))+
  scale_colour_manual(name = "Growth stage", labels =  c("Early Fetus", "Late Fetus/Neonate", "Juvenile", "Adult"), #to be ordered as they appear in tibble
                      values = mypalette_category)+            #legend and color adjustments
  geom_polygon(data = hulls_all_category, aes(x = x, y = y, fill = category), alpha = .5, show.legend = FALSE)+ #colored hulls with transparency
  scale_fill_manual(name = "Growth stage", labels = c("Early Fetus", "Late Fetus/Neonate", "Juvenile", "Adult"),
                    values =  mypalette_category)+ #must match scale_colour_manual
  scale_shape_manual(name = "Genus", labels = c("Delphinapterus", "Globicephala", "Lagenorhynchus", "Phocoena", "Stenella"), #copy from as.factor(genera)
                     values = shapes)+
  theme_bw()+
  xlab("PC 1 (56.95%)")+ #copy this from standard PCA plot (PCA_all_plot)
  ylab("PC 2 (14.53%)")+
  ggtitle("PCA all data")+
  theme(plot.title = element_text(face = "bold", hjust = 0.5))+
  #Remove legend for a scale_ using guide
  guides(fill = guide_legend(label = F, title = NULL, override.aes = list(shape = NA)))

#scale_y_reverse() #reverse y scale to match traj analysis direction if needed

#Visualize plot and save as PDF using menu in bar on the right
PCA_all_category_ggplot

#Make hulls for PCA plot with hulls around genera ("Delphinapterus", "Globicephala", "Lagenorhynchus", "Phocoena", "Stenella")
hulls_all_genus <- pcscores_all %>%
  group_by(genus) %>%
  slice(chull(Comp1, Comp2)) %>%
  rename(x = Comp1, y = Comp2)

#Nice PCA plot with hulls around genera ("Delphinapterus", "Globicephala", "Lagenorhynchus", "Phocoena", "Stenella")
PCA_all_genus_ggplot <- ggplot(pcscores_all, aes(x = Comp1, y = Comp2, label = specimens, fill = genus))+
  geom_polygon(data = hulls_all_genus, aes(x = x, y = y, colour = genus, fill = genus,  linetype = group), alpha = .005, show.legend = FALSE)+ #colored hulls with transparency
  geom_point(size = 4, aes(shape = category, alpha = category), colour = "grey10")+
  scale_colour_manual(name = "Genus", labels = c("Delphinapterus", "Globicephala", "Lagenorhynchus", "Phocoena", "Stenella"), #copy from as.factor(groups)
                      values = mypalette_taxa, aesthetics = c("colour","fill"))+ #to be ordered as they appear in tibble
  scale_alpha_manual(name = "Growth stage", labels =  c("Early Fetus", "Late Fetus/Neonate", "Juvenile", "Adult"), #to be ordered as they appear in tibble
                     values = c(0.3, 0.5, 0.7, 1))+            #legend and color adjustments
  scale_shape_manual(name = "Growth stage", labels =  c("Early Fetus", "Late Fetus/Neonate", "Juvenile", "Adult"), 
                     values = shapes_cat)+
  theme_bw()+
  xlab("PC 1 (56.95%)")+ #copy this from standard PCA plot (PCA_all_plot)
  ylab("PC 2 (14.53%)")+
  ggtitle("PCA all data")+
  theme(plot.title = element_text(face = "bold", hjust = 0.5), legend.text = element_text(size = 11), legend.background = element_blank(),
        legend.key = element_blank(), legend.title = element_text(size = 12, face = "bold"), legend.position = c(0.05,0), 
        legend.direction = "vertical", legend.justification = c(0,0))+
  guides(colour = guide_legend(label = F, title = NULL, override.aes = list(shape = NA, fill = NA, alpha = NA)), 
         fill = guide_legend(label = F, title = NULL), shape = guide_legend(override.aes = list(fill = "grey10", colour = "grey10")))

#Visualize plot and save as PDF using menu in bar on the right
PCA_all_genus_ggplot

#Add phylopics for genera
PCA_all_genus_ggplot <- 
  PCA_all_genus_ggplot +
  add_phylopic(Delph, alpha = 1, x = -0.28, y = -0.08, ysize = 0.06, color = mypalette_taxa[1])+
  add_phylopic(Globi, alpha = 1, x = -0.02, y = -0.15, ysize = 0.04, color = mypalette_taxa[2])+
  add_phylopic(Lage, alpha = 1, x = 0.2, y = -0.06, ysize = 0.075, color = mypalette_taxa[3])+
  add_phylopic(Phoc, alpha = 1, x = -0.25, y = 0.05, ysize = 0.04, color = mypalette_taxa[4])+
  add_phylopic(Sten, alpha = 1, x = -0.12, y = 0.07, ysize = 0.035, color = mypalette_taxa[5])
PCA_all_genus_ggplot

###Regression PC1 and PC2 ----

#Calculate regression for each component for size
reg_PC1all_size <- lm(Comp1 ~ size, data = pcscores_all)
reg_PC2all_size <- lm(Comp2 ~ size, data = pcscores_all)

#View results and p-value
summary(reg_PC1all_size)
summary(reg_PC2all_size)
anova(reg_PC1all_size)
anova(reg_PC2all_size)

#Save results of significant regression to file
sink("Output/PC1-2all_size_lm.txt")
print("PC1")
summary(reg_PC1all_size)
anova(reg_PC1all_size)
print("PC2")
summary(reg_PC2all_size)
anova(reg_PC2all_size)
sink() 

#Calculate regression for each component taking genus into account
reg_PC1all_genus <- lm(Comp1 ~ genus, data = pcscores_all)
reg_PC2all_genus <- lm(Comp2 ~ genus, data = pcscores_all)

#View results and p-value
summary(reg_PC1all_genus)
summary(reg_PC2all_genus)
anova(reg_PC1all_genus)
anova(reg_PC2all_genus)

#Save results of significant regression to file
sink("Output/PC1-2all_genus_lm.txt")
print("PC1")
summary(reg_PC1all_genus)
anova(reg_PC1all_genus)
print("PC2")
summary(reg_PC2all_genus)
anova(reg_PC2all_genus)
sink() 

#Calculate regression for each component taking category into account
reg_PC1all_category <- lm(Comp1 ~ category, data = pcscores_all)
reg_PC2all_category <- lm(Comp2 ~ category, data = pcscores_all)

#View results and p-value
summary(reg_PC1all_category)
summary(reg_PC2all_category)
anova(reg_PC1all_category)
anova(reg_PC2all_category)

#Save results of significant regression to file
sink("Output/PC1-2all_category_lm.txt")
print("PC1")
summary(reg_PC1all_category)
anova(reg_PC1all_category)
print("PC2")
summary(reg_PC2all_category)
anova(reg_PC2all_category)
sink() 

#Save results of all regressions to 1 file
sink("Output/PC1-2_all_lm.txt")
print("PC1")
anova(reg_PC1all_size)
anova(reg_PC1all_genus)
anova(reg_PC1all_category)
print("PC2")
anova(reg_PC2all_size)
anova(reg_PC2all_genus)
anova(reg_PC2all_category)
sink()

###### 
#Next - ch. 4 - Allometry