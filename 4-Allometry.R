
#===========================================================#
#                                                           #
#     CURVES AND POINTS ANALYSES - ODONTOCETE FAMILIES      #
#                                                           #
#===========================================================#


#CH.4 - Allometry correction for common allometry, PCA on residuals

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
library(rray)
library(abind)
library(reshape2)
library(scales)
library(mcp)

#require(devtools)
#install_github("JeroenSmaers/evomap")
#devtools::install_github("wabarr/ggphylomorpho")
#devtools::install_github("aphanotus/borealis")
#devtools::install_github("kassambara/easyGgplot2")

#ALLOMETRY CORRECTION ----
##Evaluate allometry and get the allometry-free shapes using LogCS, use this for analyses

#Regression shape on logCS size
allometry <- procD.lm(gdf$coords ~ gdf$size, iter=999, print.progress = TRUE) 

#Main results of ANOVA analysis of allometry with logCS
summary(allometry) 

#Regression score of shape vs logCS - regression method with "RegScore" plotting
allometry_plot_regscore <- plot(allometry, type = "regression",predictor = gdf$size, reg.type = "RegScore",
                                main = "Shape vs logCS",xlab = "logCS", pch = 21, col = "chartreuse4", bg = "chartreuse4", cex = 1.2, font.main = 2)   #improve graphics
text(x = gdf$size, y = allometry_plot_regscore$RegScore, labels = gdf$Id,
     pos = 3, offset = 0.5, cex = 0.75)    #improve appearance of labels

##Add regression line with confidence intervals to plot
#Create object to use for linear model
allometry_regscores <- allometry_plot_regscore[["RegScore"]] 

#Linear model for line
allometry_regline <- lm(allometry_regscores ~ gdf$size)

##Make better allometry plot with ggplot
#Create data frame object that ggplot can read - use data from plot object you want to improve
allometry_plot <- data.frame(logCS = allometry_plot_regscore[["plot.args"]][["x"]], RegScores = allometry_plot_regscore[["plot.args"]][["y"]])

#Convert data frame to tibble
allometry_plot <- as_tibble(allometry_plot)
#Add labels and other attributes to tibble as columns
allometry_plot <- allometry_plot %>% 
  mutate(specimens = gdf$Id, group = gdf$family, category = gdf$category,
         genus = gdf$genus, size = gdf$size, TL = gdf$TL_100, BZW = gdf$BZW_100)
glimpse(allometry_plot)

##Create residuals array from null to then save as coordinates for analyses
allometry_array <- arrayspecs(allometry$residuals,p = dim(gdf$coords)[1], k = dim(gdf$coords)[2]) 

#New shapes adjusted for allometry with CS to use in analyses
allometry_residuals <- allometry_array + array(mean_shape, dim(allometry_array)) 

#Save mean shape of allometry-adjusted shapes to use later
mean_shape_residuals <- mshape(allometry_residuals)

#PCA ALLOMETRY RESIDUALS ----

#New PCA plot with data corrected for allometry
PCA_residuals <- gm.prcomp(allometry_residuals) 

#List of PC components and proportion of variations
PCA_residuals

#Save PCA results to file
sink("Output/PCA_residuals_components.txt")
print("PCA allometry residuals")
print(PCA_residuals)
sink() 

#Change row names to codes to make plot readable
row.names(PCA_residuals$x) <- gdf$Id

##View plot
plot(PCA_residuals, main = "PCA residuals - PC1-PC2",  pch = 21, #title and type of point to be used
     col = "deeppink",    bg = "deeppink",  cex = 1, font.main = 2)      #improve graphics
#Add quick labels to plot
text(x = PCA_residuals$x[,1], y = PCA_residuals$x[,2], labels = rownames(PCA_residuals$x), 
     pos = 1,   offset = 0.5,  cex = 0.75)    #improve graphics 

##View plot
plot(PCA_residuals, axis1 = 1, axis2 = 3, main = "PCA residuals - PC1-PC3",  pch = 21, #title and type of point to be used
     col = "deeppink",    bg = "deeppink",  cex = 1, font.main = 2)      #improve graphics
#Add quick labels to plot
text(x = PCA_residuals$x[,1], y = PCA_residuals$x[,3], labels = rownames(PCA_residuals$x), 
     pos = 1,   offset = 0.5,  cex = 0.75)    #improve graphics 

#Save PC scores as object to use later
pcscores_res <- PCA_residuals$x

#Min max shapes code
#Save shapes of extremes for axes used in plot
PC1min_res <- PCA_residuals[["shapes"]][["shapes.comp1"]][["min"]]
PC1max_res <- PCA_residuals[["shapes"]][["shapes.comp1"]][["max"]] 
PC2min_res <- PCA_residuals[["shapes"]][["shapes.comp2"]][["min"]] 
PC2max_res <- PCA_residuals[["shapes"]][["shapes.comp2"]][["max"]] 

#Show 3D deformation from mean with points overlay, do this for all 4 extremes - using spheres3D for points
#PC1min colors
#spheres3d(mean_shape, radius=.0005, color = "gray60", alpha = 0.5, fastTransparency = T) - plot mean specimens with transparency
PC1min_res_points <- c(
  spheres3d(PC1min_res[maxilla,], radius=.001, color = mypalette_paired[8]),
  spheres3d(PC1min_res[premaxilla,], radius=.001, color = mypalette_paired[6]),
  spheres3d(PC1min_res[palatine,], radius=.001, color = mypalette_paired[7]),
  spheres3d(PC1min_res[nasals,], radius=.001, color = mypalette_paired[4]),
  spheres3d(PC1min_res[orbit,], radius=.001, color = mypalette_paired[3]),
  spheres3d(PC1min_res[squamosal,], radius=.001, color = mypalette_paired[12]),
  spheres3d(PC1min_res[basioccipital,], radius=.001, color = mypalette_paired[5]),
  spheres3d(PC1min_res[interparietal,], radius=.001, color = mypalette_paired[1]),
  spheres3d(PC1min_res[supraoccipital,], radius=.001, color = mypalette_paired[2]),
  spheres3d(PC1min_res[condyles,], radius=.001, color = mypalette_paired[10]),
  spheres3d(PC1min_res[exoccipital,], radius=.001,  color = mypalette_paired[9]))

rgl.snapshot(filename = "Output/PC1min_res.png") 
#Save 3D window as html file - 3D widget
scene <- scene3d()
widget <- rglwidget()
filename <- tempfile(fileext = ".html")
htmlwidgets::saveWidget(rglwidget(), filename)
browseURL(filename) 

#PC1max colors
PC1max_res_points <- c(
  spheres3d(PC1max_res[maxilla,], radius=.001, color = mypalette_paired[8]),
  spheres3d(PC1max_res[premaxilla,], radius=.001, color = mypalette_paired[6]),
  spheres3d(PC1max_res[palatine,], radius=.001, color = mypalette_paired[7]),
  spheres3d(PC1max_res[nasals,], radius=.001, color = mypalette_paired[4]),
  spheres3d(PC1max_res[orbit,], radius=.001, color = mypalette_paired[3]),
  spheres3d(PC1max_res[squamosal,], radius=.001, color = mypalette_paired[12]),
  spheres3d(PC1max_res[basioccipital,], radius=.001, color = mypalette_paired[5]),
  spheres3d(PC1max_res[interparietal,], radius=.001, color = mypalette_paired[1]),
  spheres3d(PC1max_res[supraoccipital,], radius=.001, color = mypalette_paired[2]),
  spheres3d(PC1max_res[condyles,], radius=.001, color = mypalette_paired[10]),
  spheres3d(PC1max_res[exoccipital,], radius=.001,  color = mypalette_paired[9]))

rgl.snapshot(filename = "Output/PC1max_res.png") 
#Save 3D window as html file - 3D widget
htmlwidgets::saveWidget(rglwidget(), filename)
browseURL(filename) 

#PC2min colors
PC2min_res_points <- c(
  spheres3d(PC2min_res[maxilla,], radius=.001, color = mypalette_paired[8]),
  spheres3d(PC2min_res[premaxilla,], radius=.001, color = mypalette_paired[6]),
  spheres3d(PC2min_res[palatine,], radius=.001, color = mypalette_paired[7]),
  spheres3d(PC2min_res[nasals,], radius=.001, color = mypalette_paired[4]),
  spheres3d(PC2min_res[orbit,], radius=.001, color = mypalette_paired[3]),
  spheres3d(PC2min_res[squamosal,], radius=.001, color = mypalette_paired[12]),
  spheres3d(PC2min_res[basioccipital,], radius=.001, color = mypalette_paired[5]),
  spheres3d(PC2min_res[interparietal,], radius=.001, color = mypalette_paired[1]),
  spheres3d(PC2min_res[supraoccipital,], radius=.001, color = mypalette_paired[2]),
  spheres3d(PC2min_res[condyles,], radius=.001, color = mypalette_paired[10]),
  spheres3d(PC2min_res[exoccipital,], radius=.001,  color = mypalette_paired[9]))

rgl.snapshot(filename = "Output/PC2min_res.png") 
#Save 3D window as html file - 3D widget
htmlwidgets::saveWidget(rglwidget(), filename)
browseURL(filename) 

#PC2max colors
PC2max_res_points <- c(
  spheres3d(PC2max_res[maxilla,], radius=.001, color = mypalette_paired[8]),
  spheres3d(PC2max_res[premaxilla,], radius=.001, color = mypalette_paired[6]),
  spheres3d(PC2max_res[palatine,], radius=.001, color = mypalette_paired[7]),
  spheres3d(PC2max_res[nasals,], radius=.001, color = mypalette_paired[4]),
  spheres3d(PC2max_res[orbit,], radius=.001, color = mypalette_paired[3]),
  spheres3d(PC2max_res[squamosal,], radius=.001, color = mypalette_paired[12]),
  spheres3d(PC2max_res[basioccipital,], radius=.001, color = mypalette_paired[5]),
  spheres3d(PC2max_res[interparietal,], radius=.001, color = mypalette_paired[1]),
  spheres3d(PC2max_res[supraoccipital,], radius=.001, color = mypalette_paired[2]),
  spheres3d(PC2max_res[condyles,], radius=.001, color = mypalette_paired[10]),
  spheres3d(PC2max_res[exoccipital,], radius=.001,  color = mypalette_paired[9]))

rgl.snapshot(filename = "Output/PC2max_res.png") 
#Save 3D window as html file - 3D widget
htmlwidgets::saveWidget(rglwidget(), filename)
browseURL(filename) 

##3D windows save
{#Save screenshot of 3D window, useful for lateral and dorsal views - use screen snip if it fails
  shade3d(PC2min_res_surface)
  rgl.snapshot(filename = "Output/PC2min_res.png") 
  #Save 3D window as html file - 3D widget
  scene <- scene3d()
  widget <- rglwidget()
  filename <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(rglwidget(), filename)
  browseURL(filename)    #from browser save screenshots as PNG (right click on image-save image) and save HTML (right click on white space-save as->WebPage HTML, only)
}

##Make better PCA plot using ggplot
#Read PC scores as tibble
pcscores_res <- as_tibble(pcscores_res)
#Add labels and other attributes to tibble as columns
pcscores_res <- pcscores_res %>% 
  mutate(specimens = gdf$Id, group = gdf$family, category = gdf$category,
         genus = gdf$genus, size = gdf$size, TL = gdf$TL_100, BZW = gdf$BZW_100)
glimpse(pcscores_res)

#Nice PCA plot with stages and groups
PCA_res_ggplot <- ggplot(pcscores_res, aes(x = Comp1, y = Comp2, label = specimens, colour = category, fill = category))+
  geom_point(size = 3, aes(shape = genus))+
  geom_text_repel(colour = "black", size = 4, max.overlaps = 40)+
  scale_colour_manual(name = "Growth stage", labels =  c("Early Fetus", "Late Fetus/Neonate", "Juvenile", "Adult"), #to be ordered as they appear in tibble
                      values = mypalette_category, aesthetics = c("colour","fill"))+            #legend and color adjustments
  scale_shape_manual(name = "Genus", labels = c("Delphinapterus", "Globicephala", "Lagenorhynchus", "Phocoena", "Stenella"), #copy from as.factor(gdf$genus)
                     values = shapes)+
  theme_bw()+
  xlab("PC 1 (43.1%)")+ #copy this from standard PCA plot (PCA_res_plot)
  ylab("PC 2 (12.89%)")+
  ggtitle("PCA residuals")+
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) 

#Visualize plot and save as PDF using menu in bar on the right
PCA_res_ggplot

#Make hulls for PCA plot with hulls around categories (4 stages: Early Fetus, Late Fetus/Neonate, Juvenile, Adult)
hulls_res_category <- pcscores_res %>%
  group_by(category) %>%
  slice(chull(Comp1, Comp2)) %>%
  rename(x = Comp1, y = Comp2)
glimpse(hulls_res_category)

#Nice PCA plot with hulls around categories (4 stages: Early Fetus, Late Fetus/Neonate, Juvenile, Adult)
PCA_res_category_ggplot <- ggplot(pcscores_res, aes(x = Comp1, y = Comp2, colour = category, fill = category))+
  geom_point(size = 3, aes(shape = genus))+
  scale_colour_manual(name = "Growth stage", labels =  c("Early Fetus", "Late Fetus/Neonate", "Juvenile", "Adult"), #to be genused as they appear in tibble
                      values = mypalette_category)+            #legend and color adjustments
  geom_polygon(data = hulls_res_category, aes(x = x, y = y, fill = category), alpha = .5, show.legend = FALSE)+ #colored hulls with transparency
  scale_fill_manual(name = "Growth stage", labels = c("Early Fetus", "Late Fetus/Neonate", "Juvenile", "Adult"),
                    values =  mypalette_category)+ #must match scale_colour_manual
  scale_shape_manual(name = "Genus", labels = c("Delphinapterus", "Globicephala", "Lagenorhynchus", "Phocoena", "Stenella"), #copy from as.factor(gdf$genus)
                     values = shapes)+
  theme_bw()+
  xlab("PC 1 (43.1%)")+ #copy this from standard PCA plot (PCA_res_plot)
  ylab("PC 2 (12.89%)")+
  ggtitle("PCA residuals")+
  theme(plot.title = element_text(face = "bold", hjust = 0.5))+
  #Remove legend for a scale_ using guide
  guides(fill = guide_legend(label = F, title = NULL, override.aes = list(shape = NA)))

#Visualize plot and save as PDF using menu in bar on the right
PCA_res_category_ggplot

#Make hulls for PCA plot with hulls around gdf$genus ("Delphinapterus", "Globicephala", "Lagenorhynchus", "Phocoena", "Stenella")
hulls_res_genus <- pcscores_res %>%
  group_by(genus) %>%
  slice(chull(Comp1, Comp2)) %>%
  rename(x = Comp1, y = Comp2)
glimpse(hulls_res_genus)

#Nice PCA plot with hulls around gdf$genus ("Delphinapterus", "Globicephala", "Lagenorhynchus", "Phocoena", "Stenella")
PCA_res_genus_ggplot <- ggplot(pcscores_res, aes(x = Comp1, y = Comp2, fill = genus))+
  geom_polygon(data = hulls_res_genus, aes(x = x, y = y, colour = genus, fill = genus,  linetype = group), alpha = .005, show.legend = FALSE)+ #colored hulls with transparency
  geom_point(size = 4, aes(shape = category, alpha = category), colour = "grey10")+
  scale_colour_manual(name = "Genus", labels = c("Delphinapterus", "Globicephala", "Lagenorhynchus", "Phocoena", "Stenella"), #copy from as.factor(groups)
                      values = mypalette_taxa, aesthetics = c("colour","fill"))+ #to be ordered as they appear in tibble
  scale_alpha_manual(name = "Growth stage", labels =  c("Early Fetus", "Late Fetus/Neonate", "Juvenile", "Adult"), #to be ordered as they appear in tibble
                     values = c(0.3, 0.5, 0.7, 1))+            #legend and color adjustments
  scale_shape_manual(name = "Growth stage", labels =  c("Early Fetus", "Late Fetus/Neonate", "Juvenile", "Adult"), 
                     values = shapes_cat)+
  theme_bw()+
  xlab("PC 1 (43.1%)")+ #copy this from standard PCA plot (PCA_res_plot)
  ylab("PC 2 (12.89%)")+
  ggtitle("PCA residuals")+
  theme(plot.title = element_text(face = "bold", hjust = 0.5), legend.text = element_text(size = 11), legend.background = element_blank(),
        legend.key = element_blank(), legend.title = element_text(size = 12, face = "bold"), legend.position = c(0.65,0.6), 
        legend.direction = "vertical", legend.justification = c(0,0))+
  guides(colour = guide_legend(label = F, title = NULL, override.aes = list(shape = NA, fill = NA, alpha = NA)), 
         fill = guide_legend(label = F, title = NULL), shape = guide_legend(override.aes = list(fill = "grey10", colour = "grey10")))

#Visualize plot and save as PDF using menu in bar on the right
PCA_res_genus_ggplot

#Add phylopics for genera
PCA_res_genus_ggplot <- 
  PCA_res_genus_ggplot +
  add_phylopic(Delph, alpha = 1, x =  -0.155, y = 0.01, ysize = 0.035, color = mypalette_taxa[1])+
  add_phylopic(Globi, alpha = 1, x = -0.11, y = -0.065, ysize = 0.025, color = mypalette_taxa[2])+
  add_phylopic(Lage, alpha = 1, x = -0.02, y = -0.06, ysize = 0.05, color = mypalette_taxa[3])+
  add_phylopic(Phoc, alpha = 1, x = -0.06, y = 0.07, ysize = 0.025, color = mypalette_taxa[4])+
  add_phylopic(Sten, alpha = 1, x = 0.15, y = 0, ysize = 0.02, color = mypalette_taxa[5])
PCA_res_genus_ggplot

###Regression PC1 and PC2 ----
#Calculate regression for each component taking genus into account
reg_PC1res_genus <- lm(Comp1 ~ genus, data = pcscores_res)
reg_PC2res_genus <- lm(Comp2 ~ genus, data = pcscores_res)

#View results and p-value
summary(reg_PC1res_genus)
summary(reg_PC2res_genus)
anova(reg_PC1res_genus)
anova(reg_PC2res_genus)

#Save results of significant regression to file
sink("Output/PC1-2res_genus_lm.txt")
print("PC1")
summary(reg_PC1res_genus)
anova(reg_PC1res_genus)
print("PC2")
summary(reg_PC2res_genus)
anova(reg_PC2res_genus)
sink()  

#Calculate regression for each component taking category into account
reg_PC1res_category <- lm(Comp1 ~ category, data = pcscores_res)
reg_PC2res_category <- lm(Comp2 ~ category, data = pcscores_res)

#View results and p-value
summary(reg_PC1res_category)
summary(reg_PC2res_category)
anova(reg_PC1res_category)
anova(reg_PC2res_category)

#Save results of significant regression to file
sink("Output/PC1-2res_category_lm.txt")
print("PC1")
summary(reg_PC1res_category)
anova(reg_PC1res_category)
print("PC2")
summary(reg_PC2res_category)
anova(reg_PC2res_category)
sink() 

#Save results of all regressions to 1 file
sink("Output/PC1-2_res_lm.txt")
print("PC1")
anova(reg_PC1res_genus)
anova(reg_PC1res_category)
print("PC2")
anova(reg_PC2res_genus)
anova(reg_PC2res_category)
sink() 

#TEST ALLOMETRY REGRESSION MULTIPLE CHANGE POINTS (Morris et al. 2021) ----
#Test if allometry should be considered separate for pre and postantal in whole skull

#First create model of allometry divided by genus to get RegScores to use in comparison
#Model with genus interaction
allometry_genus_int <- procD.lm(gdf$coords ~ gdf$size * gdf$genus, iter=999, print.progress = TRUE) #null model per genus but no diff in stage

#Check results
summary(allometry_genus_int)

#Regression score of shape vs logCS with genus interaction - regression method with "RegScore" plotting
allometry_genus_plot_regscore <- plot(allometry_genus_int, type = "regression",predictor = gdf$size, reg.type = "RegScore",
                                      main = "Shape vs logCS * genus",xlab = "logCS", pch = 21, col = "chartreuse4", bg = "chartreuse4", cex = 1.2, font.main = 2)   #improve graphics
text(x = gdf$size, y = allometry_genus_plot_regscore$RegScore, labels = Ids,
     pos = 3, offset = 0.5, cex = 0.75)    #improve appearance of labels

#Create data frame object that ggplot can read - use data from plot object you want to improve
allometry_genus_plot <- data.frame(logCS = allometry_genus_plot_regscore[["plot.args"]][["x"]], 
                                   RegScores = allometry_genus_plot_regscore[["plot.args"]][["y"]])
#Convert data frame to tibble
allometry_genus_plot <- as_tibble(allometry_genus_plot)
#Add labels and other attributes to tibble as columns
allometry_genus_plot <- allometry_genus_plot %>% 
  mutate(specimens = gdf$Id, group = gdf$family, category = gdf$category,
         genus = gdf$genus, size = gdf$size, TL = gdf$TL_100, BZW = gdf$BZW_100)
glimpse(allometry_genus_plot)

#Create new column with stage info
gdf$stage <- ifelse(gdf$category == "1-early", "prenatal", ifelse(gdf$category == "2-late/new", "prenatal", "postnatal"))
glimpse(gdf)

gdf$stage <- factor(gdf$stage, 
       levels = c("prenatal", "postnatal"))

allometry_genus_plot$stage <- gdf$stage

#Null model - no break points and single slope/intercept
null_model_allometry_w <- list(RegScores ~ logCS)

null_mcp_w <- mcp(null_model_allometry_w, allometry_genus_plot)
plot(null_mcp_w)
summary(null_mcp_w)

#Null model - no break point slope/intercpet by genus
model_allometry_null_genus_w = list(RegScores ~ 1 + (1|genus) + logCS)

mcp_null_genus_w <- mcp(model_allometry_null_genus_w, allometry_genus_plot)
plot(mcp_null_genus_w)
summary(mcp_null_genus_w)

#Model for single break point with multiple intercepts and slopes
model_allometry_1bp_w = list(RegScores ~ 1 + logCS, ~ 1 + logCS) #common break point, assigned based on data

mcp_1bp_w <- mcp(model_allometry_1bp_w, allometry_genus_plot)
plot(mcp_1bp_w)
plot_pars(mcp_1bp_w, pars = "cp_1", type = "dens") + theme_bw(10)
summary(mcp_1bp_w)

#Model for single break point with variance among genera and multiple intercepts and slopes in both lines
model_allometry_1bp_genus_w = list(RegScores ~ 1 + (1|genus) + logCS, 
                             RegScores ~ 1 + (1|genus) ~  1 + logCS) #break points per genus, assigned based on data

mcp_1bp_genus_w <- mcp(model_allometry_1bp_genus_w, allometry_genus_plot)
plot(mcp_1bp_genus_w, facet_by="genus")
summary(mcp_1bp_genus_w)
plot_pars(mcp_1bp_genus_w, pars = "cp_1", type = "dens_overlay")

#Find best fitting model - loo_compare
null_mcp_w$loo <- loo(null_mcp_w)
mcp_null_genus_w$loo <- loo(mcp_null_genus_w)
mcp_1bp_w$loo <- loo(mcp_1bp_w)
mcp_1bp_genus_w$loo <- loo(mcp_1bp_genus_w)

loo::loo_compare(null_mcp_w$loo, mcp_null_genus_w$loo, mcp_1bp_w$loo, mcp_1bp_genus_w$loo)

#Save results to file
sink("Output/compare_mcps_genus_stage.txt")
print("genus reg scores used")

print("1-null model no bp and 1 slope, 2-null model slope and intercept diffrent per genus, 3-model 1 bp and 1 slope, 3-model 1 bp different slope and intercept in each genus")
loo::loo_compare(null_mcp_w$loo, mcp_null_genus_w$loo, mcp_1bp_w$loo, mcp_1bp_genus_w$loo)

print("summary best model 1bp_genus")
summary(mcp_1bp_genus_w)

sink()

#Make density plot to overlay to regression plot manually
#Use general model with 1 bp for easier plotting
#Prepare data
cp_1_1_w <- as.matrix(mcp_1bp_w[["mcmc_post"]][[1]])
cp_1_2_w <- as.matrix(mcp_1bp_w[["mcmc_post"]][[2]])
cp_1_3_w <- as.matrix(mcp_1bp_w[["mcmc_post"]][[3]])

cp_1_1_w <- cp_1_1_w[,"cp_1"]
cp_1_2_w <- cp_1_2_w[,"cp_1"]
cp_1_3_w <- cp_1_3_w[,"cp_1"]

cp_1_model_w <- data.frame(c1 = cp_1_1_w, c2 = cp_1_2_w, c3 = cp_1_3_w)
cp_1_model_w <- melt(cp_1_model_w)
cp_1_model_w  <- as_tibble(cp_1_model_w)

#Plot to export
cp_1_density_w <- ggplot(data=cp_1_model_w, aes(x = value, fill = variable)) + 
  geom_density(alpha =0.3,  kernel = "gaussian")+
  xlim(min(logCsize), max(logCsize))+
  theme_bw() +
  scale_fill_manual(values = c("black", "grey30", "grey70"))+
theme(legend.position = "none")
cp_1_density_w

#Density plot by genus for comparison
#Prepare data
cp_1_1_w_genus <- as.matrix(mcp_1bp_genus_w[["mcmc_post"]][[1]])
cp_1_2_w_genus <- as.matrix(mcp_1bp_genus_w[["mcmc_post"]][[2]])
cp_1_3_w_genus <- as.matrix(mcp_1bp_genus_w[["mcmc_post"]][[3]])

cp_1_1_w_genus <- cp_1_1_w_genus[,"cp_1"]
cp_1_2_w_genus <- cp_1_2_w_genus[,"cp_1"]
cp_1_3_w_genus <- cp_1_3_w_genus[,"cp_1"]

cp_1_model_w_genus <- data.frame(c1 = cp_1_1_w_genus, c2 = cp_1_2_w_genus, c3 = cp_1_3_w_genus)
cp_1_model_w_genus <- melt(cp_1_model_w_genus)
cp_1_model_w_genus  <- as_tibble(cp_1_model_w_genus)

#Plot to export
cp_1_density_w_genus <- ggplot(data=cp_1_model_w_genus, aes(x = value, fill = variable)) + 
  geom_density(alpha =0.3,  kernel = "gaussian")+
  xlim(min(logCsize), max(logCsize))+
  theme_bw() +
  scale_fill_manual(values = c("black", "grey30", "grey70"))+
  theme(legend.position = "none")
cp_1_density_w_genus

#Add lines with means at birth for each genus - roughly size at birth
#Look for rows with specimens marked as neonates (age = 100%)
rows_neonate <- which(gdf$Age_100 == 100)

#Select rows
genera_neonate <- list()

#Loop
for (k in 1:length(genera_list)){
  genera_neonate [[k]] <- which(gdf$genus[rows_neonate]==genera_list[k])
}

#Extract neonate size data
logCsize_neonate <- gdf$size[which(gdf$Age_100 == 100)]

#Calculate means
logCsize_neonate_means <- list()

#Loop
for (k in 1:length(genera_list)){
  logCsize_neonate_means[[k]] <- mean(logCsize_neonate[genera_neonate[[k]]])
}

#Data for plot
logCsize_neonate_means <-  data.frame(logCS = unlist(logCsize_neonate_means), genus = genera_list)

#Add to plot colored by genus - to add to facet plot manually
cp_1_density_w_genus <- cp_1_density_w_genus + 
  geom_vline(data = logCsize_neonate_means, aes(xintercept = logCS, color = genus), 
           color = mypalette_taxa, linetype = 4, size = 1.2)
cp_1_density_w_genus

#Calculate overall mean to add to 1bp model density plot
means_neonate <- c(mean(logCsize_neonate_means$logCS), "mean")

logCsize_neonate_means <- rbind(logCsize_neonate_means, means_neonate)
logCsize_neonate_means$logCS <- as.numeric(logCsize_neonate_means$logCS) #ensure it's numeric

#Add to plot
cp_1_density_w <- cp_1_density_w + 
  geom_vline(aes(xintercept = logCsize_neonate_means$logCS[6]), 
             color = "firebrick", linetype = 4, size = 1.2)
cp_1_density_w

##Test different allometry between genera ("Delphinapterus", "Globicephala", "Lagenorhynchus", "Phocoena", "Stenella") and stage (pre vs post natal) ----
#Create new column with association between genus and stage - easier computation of model
gdf$stage_genus <- paste(gdf$genus, gdf$stage, sep="_")

allometry_genus_plot$stage_genus <- gdf$stage_genus

#Create models to compare
allometry_genus_stage_comb <- procD.lm(gdf$coords ~ gdf$size + gdf$stage_genus, iter=999, print.progress = TRUE) 
allometry_genus_stage_int <- procD.lm(gdf$coords ~ gdf$size * gdf$stage_genus, iter=999, print.progress = TRUE) 

#Main results of ANOVA analysis of allometry
summary(allometry_genus_stage_comb)
summary(allometry_genus_stage_int) 

#Save results of significant regression to file
sink("Output/allometry_genus_stage_models.txt")
print("Null - common allometry")
summary(allometry) 

print("Null - genus only")
summary(allometry_genus_int)

print("Combination + - genus and stage")
summary(allometry_genus_stage_comb) 

print("Interaction * - genus and stage")
summary(allometry_genus_stage_int)
sink()

#ANOVAs - is a model significantly better than the others?
anova_allometry_genus_stage_models <- anova(allometry, allometry_genus_int, allometry_genus_stage_comb, allometry_genus_stage_int)
anova_allometry_genus_stage_models

#Pairwise comparison for the combination and interaction model
#Helps determine if there is a significant difference in slope (int model) in the allometry trajectory on top of difference in intercept (comb model)
pairwise_allometry_genus_stage <- pairwise(allometry_genus_stage_int, fit.null = allometry_genus_stage_comb,
                                     groups = gdf$stage_genus, 
                                     covariate = gdf$size, print.progress = FALSE) 
pairwise_allometry_genus_stage

#Distances between slope vectors (end-points) - absolute difference between slopes of groups
#if significant means int model better than comb
pairwise_allometry_genus_stage_dist <- summary(pairwise_allometry_genus_stage, confidence = 0.95, test.type = "dist") 
pairwise_allometry_genus_stage_dist

#Correlation between slope vectors (and angles) - similarity of vector orientation or angle,
#if significant means the vectors of the groups are oriented in different ways 
pairwise_allometry_genus_stage_VC <- summary(pairwise_allometry_genus_stage, confidence = 0.95, test.type = "VC",
                                       angle.type = "deg") 
pairwise_allometry_genus_stage_VC

#Absolute difference between slope vector lengths - difference in rate of change per covariate unit (size),
#if significant means there is a significant rate of change difference in shape between groups during growth
pairwise_allometry_genus_stage_DL <-summary(pairwise_allometry_genus_stage, confidence = 0.95, test.type = "DL") 
pairwise_allometry_genus_stage_DL 

#Compare the dispersion around group slopes - fit of the data to the regression
#if significant difference might be problem as it means the groups are not evenly sampled or one of them contains relevant outliers
pairwise_allometry_genus_stage_var <-summary(pairwise_allometry_genus_stage, confidence = 0.95, test.type = "var")
pairwise_allometry_genus_stage_var

#Save results to file
sink("Output/pairwise_allometry_genus_stage.txt")
print("ANOVA models")
print(anova_allometry_genus_stage_models)

print("1-Pairwise absolute distances slopes")
pairwise_allometry_genus_stage_dist

print("2-Distance between angles (slope directions)")
pairwise_allometry_genus_stage_VC

print("3-Difference in slope vector length (difference in rate of change of shape per unit of size)")
pairwise_allometry_genus_stage_DL

print("4-Difference in dispersion around mean slope")
pairwise_allometry_genus_stage_var 
sink()

###Heatmaps plots for significant differences in pairwise ----
#Functions
#Get lower triangle of the correlation matrix
get_lower_tri<-function(x){
  x[upper.tri(x)] <- NA
  return(x)
}
#Get upper triangle of the correlation matrix
get_upper_tri <- function(x){
  x[lower.tri(x)]<- NA
  return(x)
}
#Reorder table
reorder_corr_table <- function(x){
  # Use correlation between variables as distance
  dd <- as.dist((1-x)/2)
  hc <- hclust(dd)
  x <-x[hc$order, hc$order]
}

#Create palette for heatmap plot
mypalette_seq <- brewer.pal(9,"Oranges")
image(1:9,1, as.matrix(1:9), col = mypalette_seq,xlab="Oranges (sequential)",
      ylab = "", yaxt = "n")

#Save p-values as object
pairwise_allometry_stage_dist <- pairwise_allometry_genus_stage_dist[["pairwise.tables"]][["D"]]
pairwise_allometry_stage_dist_p <- pairwise_allometry_genus_stage_dist[["pairwise.tables"]][["P"]]
pairwise_allometry_stage_angle <- pairwise_allometry_genus_stage_VC[["pairwise.tables"]][["angle"]]
pairwise_allometry_stage_angle_p <- pairwise_allometry_genus_stage_VC[["pairwise.tables"]][["P"]]
pairwise_allometry_stage_length <- pairwise_allometry_genus_stage_DL[["pairwise.tables"]][["D"]]
pairwise_allometry_stage_length_p <- pairwise_allometry_genus_stage_DL[["pairwise.tables"]][["P"]]

#Make list to change tables faster
pairwise_allometry_stage_list <- list(pairwise_allometry_stage_dist, pairwise_allometry_stage_dist_p, pairwise_allometry_stage_angle, pairwise_allometry_stage_angle_p, 
                                pairwise_allometry_stage_length, pairwise_allometry_stage_length_p)

#Make new list of variable names including stages and genera
#Save row and col names as variables to change string - colnames = rownames for both
stage_genus_vars <- rownames(pairwise_allometry_stage_dist)

#Make list of shorter stages
stages_list <- levels(gdf$stage)
stages_list_short <- str_sub(stages_list, 1, 4)

#Loop replacements stages
for (s in 1:length(stages_list)){
  stage_genus_vars <- str_replace_all(stage_genus_vars, stages_list[s], stages_list_short[s])
}

#Loop replacements genera
for (t in 1:length(genera_list)){
  stage_genus_vars <- str_replace_all(stage_genus_vars, genera_list[t], genera_list_short[t])
}

#Check it worked
stage_genus_vars

#Set correct row and col names for both
#Loop
for (l in 1:6){   #number of variable is fixed, given by pairwise results
  rownames(pairwise_allometry_stage_list[[l]]) <- stage_genus_vars
  colnames(pairwise_allometry_stage_list[[l]]) <- stage_genus_vars
}

#Save only lower triangle for each
pairwise_allometry_stage_lower_tri_list <- list()

#Loop
for (l in 1:6){   #number of variable is fixed, given by parwise results
  pairwise_allometry_stage_lower_tri_list[[l]] <- get_upper_tri(pairwise_allometry_stage_list[[l]])
}

#Melt to make table in the format needed for heatmap
pairwise_allometry_stage_melt <- list()

#Loop
for (l in 1:6){   #number of variable is fixed, given by parwise results
  pairwise_allometry_stage_melt[[l]] <- melt(pairwise_allometry_stage_lower_tri_list[[l]], na.rm = TRUE)
}

#Create single data frames 
pairwise_allometry_stage_dist_melt <- data.frame(pairwise_allometry_stage_melt[[1]], p = pairwise_allometry_stage_melt[[2]][[3]])
pairwise_allometry_stage_angle_melt <- data.frame(pairwise_allometry_stage_melt[[3]], p = pairwise_allometry_stage_melt[[4]][[3]])
pairwise_allometry_stage_length_melt <- data.frame(pairwise_allometry_stage_melt[[5]], p = pairwise_allometry_stage_melt[[6]][[3]])

#Create columns where only significant values are shown
pairwise_allometry_stage_dist_melt <- pairwise_allometry_stage_dist_melt %>% mutate(sig_p = ifelse(p < .05, T, F),
                                                                        p_if_sig = ifelse(sig_p, p, NA),
                                                                        value_if_sig = ifelse(sig_p, value, NA)) %>%
  mutate_at(vars(starts_with("value")), list(~ round(., 3)))
pairwise_allometry_stage_angle_melt <- pairwise_allometry_stage_angle_melt %>% mutate(sig_p = ifelse(p < .05, T, F),
                                                                          p_if_sig = ifelse(sig_p, p, NA),
                                                                          value_if_sig = ifelse(sig_p, value, NA)) %>%
  mutate_at(vars(starts_with("value")), list(~ round(., 3)))
pairwise_allometry_stage_length_melt <- pairwise_allometry_stage_length_melt %>% mutate(sig_p = ifelse(p < .05, T, F),
                                                                            p_if_sig = ifelse(sig_p, p, NA),
                                                                            value_if_sig = ifelse(sig_p, value, NA)) %>%
  mutate_at(vars(starts_with("value")), list(~ round(., 3)))

#Nice heatmap plot for each variable
pairwise_allometry_stage_dist_heatmap_ggplot <- ggplot(data = pairwise_allometry_stage_dist_melt, aes(Var2, Var1, fill = p_if_sig))+
  geom_tile(colour = "gray80")+
  geom_text(aes(Var2, Var1, label = value_if_sig), color = "white", size = 5) +
  scale_fill_gradient2(low = mypalette_seq[9], high = mypalette_seq[2], mid = mypalette_seq[5], #negative correlations are in blue color and positive correlations in red. 
                       midpoint = 0.03, limit = c(0.001, 0.049), space = "Lab", #scale is from min to max p-values
                       na.value =  mypalette_seq[1], name = "P-values < 0.05") + 
  theme_minimal()+ 
  coord_fixed()+
  ggtitle ("Slope distance")+ 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
        axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x =  element_text(angle = 45, size = 14, 
                                                                                                    hjust = 0.9),
        axis.text.y =  element_text(size = 14, vjust = -0.2, margin = NULL), panel.grid.major = element_blank(),
        legend.justification = c(1, 0), legend.position = c(0.4, 0.7),  legend.direction = "horizontal",
        legend.title = element_text(size = 13), legend.text = element_text(size = 11))+
  guides(fill = guide_colorbar(barwidth = 10, barheight = 1.5,
                               title.position = "top", title.hjust = 0.5))
pairwise_allometry_stage_dist_heatmap_ggplot

#Nice heatmap plot for each variable
pairwise_allometry_stage_angle_heatmap_ggplot <- ggplot(data = pairwise_allometry_stage_angle_melt, aes(Var2, Var1, fill = p_if_sig))+
  geom_tile(colour = "gray80")+
  geom_text(aes(Var2, Var1, label = value_if_sig), color = "white", size = 5) +
  scale_fill_gradient2(low = mypalette_seq[9], high = mypalette_seq[2], mid = mypalette_seq[5], #negative correlations are in blue color and positive correlations in red. 
                       midpoint = 0.03, limit = c(0.001, 0.049), space = "Lab", #scale is from min to max p-values
                       na.value =  mypalette_seq[1], name = "P-values < 0.05") + 
  theme_minimal()+ 
  coord_fixed()+
  ggtitle ("Slope angle difference")+ 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
        axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x =  element_text(angle = 45, size = 14,
                                                                                                    hjust = 0.9),
        axis.text.y =  element_text(size = 14, vjust = -0.2, margin = NULL), panel.grid.major = element_blank(),
        legend.justification = c(1, 0), legend.position = c(0.4, 0.7),  legend.direction = "horizontal",
        legend.title = element_text(size = 13), legend.text = element_text(size = 11))+
  guides(fill = guide_colorbar(barwidth = 10, barheight = 1.5,
                               title.position = "top", title.hjust = 0.5))
pairwise_allometry_stage_angle_heatmap_ggplot

#Nice heatmap plot for each variable
pairwise_allometry_stage_length_heatmap_ggplot <- ggplot(data = pairwise_allometry_stage_length_melt, aes(Var2, Var1, fill = p_if_sig))+
  geom_tile(colour = "gray80")+
  geom_text(aes(Var2, Var1, label = value_if_sig), color = "white", size = 5) +
  scale_fill_gradient2(low = mypalette_seq[9], high = mypalette_seq[2], mid = mypalette_seq[5], #negative correlations are in blue color and positive correlations in red. 
                       midpoint = 0.03, limit = c(0.001, 0.049), space = "Lab", #scale is from min to max p-values
                       na.value =  mypalette_seq[1], name = "P-values < 0.05") + 
  theme_minimal()+ 
  coord_fixed()+
  ggtitle ("Slope length difference")+ 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
        axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x =  element_text(angle = 45, size = 14,
                                                                                                    hjust = 0.9),
        axis.text.y =  element_text(size = 14, vjust = -0.2, margin = NULL), panel.grid.major = element_blank(),
        legend.justification = c(1, 0), legend.position = c(0.4, 0.7),  legend.direction = "horizontal",
        legend.title = element_text(size = 13), legend.text = element_text(size = 11))+
  guides(fill = guide_colorbar(barwidth = 10, barheight = 1.5,
                               title.position = "top", title.hjust = 0.5))
pairwise_allometry_stage_length_heatmap_ggplot


####Plots by growth stage####
#Make data frame for each stage
pairwise_allometry_stage_dist_melt_prenatal <- pairwise_allometry_stage_dist_melt %>% filter(str_detect(Var1, "pren")) %>% 
                                               filter(str_detect(Var2, "pren"))
pairwise_allometry_stage_dist_melt_postnatal <- pairwise_allometry_stage_dist_melt %>% filter(str_detect(Var1, "post")) %>% 
                                               filter(str_detect(Var2, "post"))

pairwise_allometry_stage_angle_melt_prenatal <- pairwise_allometry_stage_angle_melt %>% filter(str_detect(Var1, "pren")) %>% 
  filter(str_detect(Var2, "pren"))
pairwise_allometry_stage_angle_melt_postnatal <- pairwise_allometry_stage_angle_melt %>% filter(str_detect(Var1, "post")) %>% 
  filter(str_detect(Var2, "post"))

pairwise_allometry_stage_length_melt_prenatal <- pairwise_allometry_stage_length_melt %>% filter(str_detect(Var1, "pren")) %>% 
  filter(str_detect(Var2, "pren"))
pairwise_allometry_stage_length_melt_postnatal <- pairwise_allometry_stage_length_melt %>% filter(str_detect(Var1, "post")) %>% 
  filter(str_detect(Var2, "post"))

#Nice heatmap plot
pairwise_allometry_stage_dist_heatmap_ggplot_pren  <- ggplot(data = pairwise_allometry_stage_dist_melt_prenatal, aes(Var2, Var1, fill = p_if_sig))+
  geom_tile(colour = "gray80")+
  scale_fill_gradient2(low = mypalette_seq[9], high = mypalette_seq[2], mid = mypalette_seq[5], #negative correlations are in blue color and positive correlations in red. 
                       midpoint = 0.03, limit = c(0, 0.049), space = "Lab", #scale is from min to max p-values
                       na.value =  mypalette_seq[1], name = "P-values < 0.05") + 
  theme_minimal()+ 
  coord_fixed()+
  scale_x_discrete(labels = genera_list_short)+
  scale_y_discrete(labels = genera_list_short)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1))+
  ggtitle ("Distance - Prenatal")+ 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
        axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x =  element_text(angle = 45, size = 10,hjust = 0.9),
        axis.text.y =  element_text(size = 10, vjust = -0.2, margin = NULL), panel.grid.major = element_blank(),
        legend.position = c(0.18,0.85),  legend.direction = "horizontal",
        legend.title = element_text(size = 11), legend.text = element_text(size = 10))+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1.1,
                               title.position = "top", title.hjust = 0.5))
pairwise_allometry_stage_dist_heatmap_ggplot_pren 

pairwise_allometry_stage_dist_heatmap_ggplot_post  <- ggplot(data = pairwise_allometry_stage_dist_melt_postnatal, aes(Var2, Var1, fill = p_if_sig))+
  geom_tile(colour = "gray80")+
  scale_fill_gradient2(low = mypalette_seq[9], high = mypalette_seq[2], mid = mypalette_seq[5], #negative correlations are in blue color and positive correlations in red. 
                       midpoint = 0.03, limit = c(0, 0.049), space = "Lab", #scale is from min to max p-values
                       na.value =  mypalette_seq[1], name = "P-values < 0.05") + 
  theme_minimal()+ 
   coord_fixed()+   scale_x_discrete(labels = genera_list_short)+   scale_y_discrete(labels = genera_list_short)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1))+
  ggtitle ("Distance - Postnatal")+ 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
        axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x =  element_text(angle = 45, size = 10,hjust = 0.9),
        axis.text.y =  element_text(size = 10, vjust = -0.2, margin = NULL), panel.grid.major = element_blank(),
        legend.position = c(0.18,0.85),  legend.direction = "horizontal",
        legend.title = element_text(size = 11), legend.text = element_text(size = 10))+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1.1,
                               title.position = "top", title.hjust = 0.5))
pairwise_allometry_stage_dist_heatmap_ggplot_post

pairwise_allometry_stage_angle_heatmap_ggplot_pren  <- ggplot(data = pairwise_allometry_stage_angle_melt_prenatal, aes(Var2, Var1, fill = p_if_sig))+
  geom_tile(colour = "gray80")+
  scale_fill_gradient2(low = mypalette_seq[9], high = mypalette_seq[2], mid = mypalette_seq[5], #negative correlations are in blue color and positive correlations in red. 
                       midpoint = 0.03, limit = c(0, 0.049), space = "Lab", #scale is from min to max p-values
                       na.value =  mypalette_seq[1], name = "P-values < 0.05") + 
  theme_minimal()+ 
   coord_fixed()+   scale_x_discrete(labels = genera_list_short)+   scale_y_discrete(labels = genera_list_short)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1))+
  ggtitle ("Angle - Prenatal")+ 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
        axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x =  element_text(angle = 45, size = 10,hjust = 0.9),
        axis.text.y =  element_text(size = 10, vjust = -0.2, margin = NULL), panel.grid.major = element_blank(),
        legend.position = c(0.18,0.85),  legend.direction = "horizontal",
        legend.title = element_text(size = 11), legend.text = element_text(size = 10))+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1.1,
                               title.position = "top", title.hjust = 0.5))
pairwise_allometry_stage_angle_heatmap_ggplot_pren 

pairwise_allometry_stage_angle_heatmap_ggplot_post  <- ggplot(data = pairwise_allometry_stage_angle_melt_postnatal, aes(Var2, Var1, fill = p_if_sig))+
  geom_tile(colour = "gray80")+
  scale_fill_gradient2(low = mypalette_seq[9], high = mypalette_seq[2], mid = mypalette_seq[5], #negative correlations are in blue color and positive correlations in red. 
                       midpoint = 0.03, limit = c(0, 0.049), space = "Lab", #scale is from min to max p-values
                       na.value =  mypalette_seq[1], name = "P-values < 0.05") + 
  theme_minimal()+ 
   coord_fixed()+   scale_x_discrete(labels = genera_list_short)+   scale_y_discrete(labels = genera_list_short)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1))+
  ggtitle ("Angle - Postnatal")+ 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
        axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x =  element_text(angle = 45, size = 10,hjust = 0.9),
        axis.text.y =  element_text(size = 10, vjust = -0.2, margin = NULL), panel.grid.major = element_blank(),
        legend.position = c(0.18,0.85),  legend.direction = "horizontal",
        legend.title = element_text(size = 11), legend.text = element_text(size = 10))+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1.1,
                               title.position = "top", title.hjust = 0.5))
pairwise_allometry_stage_angle_heatmap_ggplot_post

pairwise_allometry_stage_length_heatmap_ggplot_pren  <- ggplot(data = pairwise_allometry_stage_length_melt_prenatal, aes(Var2, Var1, fill = p_if_sig))+
  geom_tile(colour = "gray80")+
  scale_fill_gradient2(low = mypalette_seq[9], high = mypalette_seq[2], mid = mypalette_seq[5], #negative correlations are in blue color and positive correlations in red. 
                       midpoint = 0.03, limit = c(0, 0.049), space = "Lab", #scale is from min to max p-values
                       na.value =  mypalette_seq[1], name = "P-values < 0.05") + 
  theme_minimal()+ 
   coord_fixed()+   scale_x_discrete(labels = genera_list_short)+   scale_y_discrete(labels = genera_list_short)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1))+
  ggtitle ("Length - Prenatal")+ 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
        axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x =  element_text(angle = 45, size = 10,hjust = 0.9),
        axis.text.y =  element_text(size = 10, vjust = -0.2, margin = NULL), panel.grid.major = element_blank(),
        legend.position = c(0.18,0.85),  legend.direction = "horizontal",
        legend.title = element_text(size = 11), legend.text = element_text(size = 10))+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1.1,
                               title.position = "top", title.hjust = 0.5))
pairwise_allometry_stage_length_heatmap_ggplot_pren 

pairwise_allometry_stage_length_heatmap_ggplot_post  <- ggplot(data = pairwise_allometry_stage_length_melt_postnatal, aes(Var2, Var1, fill = p_if_sig))+
  geom_tile(colour = "gray80")+
  scale_fill_gradient2(low = mypalette_seq[9], high = mypalette_seq[2], mid = mypalette_seq[5], #negative correlations are in blue color and positive correlations in red. 
                       midpoint = 0.03, limit = c(0, 0.049), space = "Lab", #scale is from min to max p-values
                       na.value =  mypalette_seq[1], name = "P-values < 0.05") + 
  theme_minimal()+ 
   coord_fixed()+   scale_x_discrete(labels = genera_list_short)+   scale_y_discrete(labels = genera_list_short)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1))+
  ggtitle ("Length - Postnatal")+ 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
        axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x =  element_text(angle = 45, size = 10,hjust = 0.9),
        axis.text.y =  element_text(size = 10, vjust = -0.2, margin = NULL), panel.grid.major = element_blank(),
        legend.position = c(0.18,0.85),  legend.direction = "horizontal",
        legend.title = element_text(size = 11), legend.text = element_text(size = 10))+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1.1,
                               title.position = "top", title.hjust = 0.5))
pairwise_allometry_stage_length_heatmap_ggplot_post

#Arrange together as 1 plot
ggarrange(pairwise_allometry_stage_dist_heatmap_ggplot_pren, pairwise_allometry_stage_angle_heatmap_ggplot_pren,
           pairwise_allometry_stage_length_heatmap_ggplot_pren, pairwise_allometry_stage_dist_heatmap_ggplot_post, 
          pairwise_allometry_stage_angle_heatmap_ggplot_post,
         pairwise_allometry_stage_length_heatmap_ggplot_post,
          ncol = 3, nrow = 2, common.legend = T)

##Plot allometry by genus separate for pre and postnatal stages ----
#Use common regression scores per genus for easier plotting
#Divide prenatal rows
rows_prenatal <- which(allometry_genus_plot$stage%in%c("prenatal"))

#Create data frame object that ggplot can read - use data from plot object you want to improve
allometry_genus_plot_prenatal <- allometry_genus_plot[rows_prenatal,]
allometry_genus_plot_postnatal <- allometry_genus_plot[-rows_prenatal,]

#Plot allometry regression by genus ("Delphinapterus", "Globicephala", "Lagenorhynchus", "Phocoena", "Stenella") and stage (prenatal vs postnatal)
allometry_genus_stage_ggplot <- ggplot(allometry_genus_plot, aes(x = logCS, y = RegScores, colour = genus, fill = genus, linetype = stage))+
  geom_point(size = 3, aes(shape = category), alpha = 0.3)+  
  geom_smooth(data = allometry_genus_plot_prenatal, aes(x = logCS, y = RegScores, colour = genus, fill = genus), method = 'lm',          #confidence intervals and reg line, before points
              size = 1.2, se = F, show.legend = F)+      #put col and other graphics OUTSIDE of aes()!!!
  geom_smooth(data = allometry_genus_plot_postnatal, aes(x = logCS, y = RegScores, colour = genus, fill = genus), method = 'lm',          #confidence intervals and reg line, before points
              size = 1.2, se = F)+      #put col and other graphics OUTSIDE of aes()!!! #put col and other graphics OUTSIDE of aes()!!!
  #points after, so they are on top
  scale_shape_manual(name = "Growth stage", labels =  c("Early Fetus", "Late Fetus/Neonate", "Juvenile", "Adult"), 
                     values = shapes_cat)+
  scale_linetype_manual(name = "Developmental stages", labels = c("prenatal", "postnatal"),  values = c(2, 1))+
  scale_color_manual(values = mypalette_taxa, aesthetics = c("color","fill"))+         
  theme_classic(base_size = 12)+
  ylab("Regression Score")+
  ggtitle ("Allometry by genus prenatal and postnatal - p-value = 0.001**")+ 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 11), legend.text = element_text(size = 12), 
        legend.title = element_text(size = 13), legend.position = "right", legend.direction = "vertical", legend.justification = c(1.2,0))+
  guides(color = guide_legend(label = F, title = NULL, override.aes = list(shape = NA, linetype = 0, fill = NA)),
         fill = guide_legend(label = F, title = NULL), linetype = guide_legend(override.aes = list(color = "black", size = 0.8)))
allometry_genus_stage_ggplot

#Add phylopics
allometry_genus_stage_ggplot  <- 
  allometry_genus_stage_ggplot  + 
  add_phylopic(Delph, alpha = 1, x = 2.85, y = -0.25, ysize = 0.18, color = mypalette_taxa[1])+
  add_phylopic(Globi, alpha = 1, x = 3.4, y = -0.04, ysize = 0.12, color = mypalette_taxa[2])+
  add_phylopic(Lage, alpha = 1, x = 3.55, y = 0.17, ysize = 0.22, color = mypalette_taxa[3])+
  add_phylopic(Phoc, alpha = 1, x = 2.45, y = -0.165, ysize = 0.11, color = mypalette_taxa[4])+
  add_phylopic(Sten, alpha = 1, x = 2.9, y = 0.07, ysize = 0.1, color = mypalette_taxa[5])
allometry_genus_stage_ggplot

####Get coefficients for each genus per stage to use later in anc state####

#Regression score of shape vs logCS per genus and stage - regression method with "RegScore" plotting
allometry_genus_plot_regscore <- plot(allometry_genus_stage_int, type = "regression",predictor = gdf$size, reg.type = "RegScore", 
                                      main = "Shape vs logCS  by genus and stage",xlab = "logCS", pch = 21, cex = 1.2, font.main = 2,
                                      col = c(mypalette_taxa, mypalette_taxa), bg = c(mypalette_taxa, mypalette_taxa))
text(x = gdf$size, y = allometry_genus_plot_regscore$RegScore, labels = gdf$Id,
     pos = 3, offset = 0.5, cex = 0.75)    #improve appearance of labels  

#Create object to use for linear model
allometry_genus_regscores <- allometry_genus_plot_regscore[["RegScore"]] 

#Linear model for line by genus
allometry_genus_regline_df <- data.frame(RegScores = allometry_regscores, logCS = gdf$size, stage_genus = gdf$stage_genus)

allometry_genus_regline <- lm(RegScores ~ logCS * stage_genus, data = allometry_genus_regline_df)

#Get coeffs - the first 2 are reference intercept and slopes, aother values are differences!
allometry_genus_regline_coeffs <- as.matrix(allometry_genus_regline$coefficients)

#Save intercepts and slopes separately
allometry_genus_regline_intercepts <- as.matrix(allometry_genus_regline_coeffs[c(1, 3:(length(stage_genus_vars)+1)),])
allometry_genus_regline_slopes <- as.matrix(allometry_genus_regline_coeffs[c(2, length(stage_genus_vars)+2:(length(stage_genus_vars))),])

#Calculate real intercepts and slopes - add to first line as following lines are difference from reference
allometry_genus_regline_intercepts_ok <- as.matrix(c(allometry_genus_regline_intercepts[1,], allometry_genus_regline_intercepts[1,]+
                                                       allometry_genus_regline_intercepts[2:length(allometry_genus_regline_intercepts),]))

allometry_genus_regline_slopes_ok <- as.matrix(c(allometry_genus_regline_slopes[1,], allometry_genus_regline_slopes[1,]+
                                                   allometry_genus_regline_slopes[2:length(allometry_genus_regline_slopes),]))

#Save as data frame with grouping variables
allometry_genus_coeffs <- data.frame(Slope = allometry_genus_regline_slopes_ok, Intercept = allometry_genus_regline_intercepts_ok, 
                                     row.names = levels(as.factor(gdf$stage_genus)))

allometry_genus_coeffs <- allometry_genus_coeffs %>% mutate(genus = rep(genera_list, each = length(stages_list)),
                                                            stage = rep(c("postnatal","prenatal"), times  =  length(genera_list)),
                                                            group = rep(families_list, each = length(stages_list)))
#Reorder by stage to match other data                                                                        
allometry_genus_coeffs$stage <- factor(allometry_genus_coeffs$stage , levels = stages_list)

allometry_genus_coeffs <- allometry_genus_coeffs[order(allometry_genus_coeffs$stage),]

#Divide stages
allometry_genus_coeffs_prenatal <- allometry_genus_coeffs %>% filter(stage == "prenatal")
allometry_genus_coeffs_postnatal <- allometry_genus_coeffs %>% filter(stage == "postnatal")



###### 
#Next - ch. 5 - Trajectory analysis
