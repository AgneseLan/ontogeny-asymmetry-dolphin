
#===========================================================#
#                                                           #
#     CURVES AND POINTS ANALYSES - ODONTOCETE FAMILIES      #
#                                                           #
#===========================================================#

#CH.7 - Ancestral state reconstruction allometry regression

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
library(rray)
library(abind)
library(mcp)
library(emmeans)
set.seed(17)

#require(devtools)
#install_github("JeroenSmaers/evomap")
#devtools::install_github("wabarr/ggphylomorpho")
#devtools::install_github("aphanotus/borealis")
#remotes::install_github("r-lib/rray")

#apropos("x") lists objects with matching part of name

#ALLOMETRY WITH ANC STATE REC (modified from Morris et al., 2019) ----

#Import trees in Nexus format - branch lengths needed!!
tree <- "Data/tree_odontonly.txt"   #trees with groups

##Read the trees for analysis
tree_genera <- read.nexus(tree) #trees with groups
plot(tree_genera)

#Check names of each tree in object
summary(tree_genera)

##Ancestral slopes calculations and plots ----
#Calculate ancestral states with phytools - slope and intercept for each component
slope_prenatal_anc.ML <- fastAnc(tree_genera, allometry_genus_coeffs_prenatal[,"Slope"])
int_prenatal_anc.ML <- fastAnc(tree_genera, allometry_genus_coeffs_prenatal[,"Intercept"])
slope_postnatal_anc.ML <- fastAnc(tree_genera, allometry_genus_coeffs_postnatal[,"Slope"])
int_postnatal_anc.ML <- fastAnc(tree_genera, allometry_genus_coeffs_postnatal[,"Intercept"])

#Prepare data frame for plot
#Plot tree with node labels to check what node is where
plot(tree_genera, show.node.label = T)
nodelabels() #save tree figure
#Save node numbers
nodes <- as.numeric(names(slope_prenatal_anc.ML$ace))
#Create col names for ancestral data frame
anc_col_names <- c("Slope","Intercept")

#Create data frames with ancestral state values and CI
#Central estimates first
anc_values_traj_prenatal <- matrix(nrow = length(nodes), ncol = 2, dimnames = list(nodes,anc_col_names))
anc_values_traj_prenatal[,"Slope"] <- slope_prenatal_anc.ML
anc_values_traj_prenatal[,"Intercept"] <- int_prenatal_anc.ML
anc_values_traj_prenatal
#Add CI data
anc_values_traj_prenatal <- as.data.frame(anc_values_traj_prenatal)
anc_values_traj_prenatal$genus <- as.character(nodes) 
#Order based on node number
anc_values_traj_prenatal <- anc_values_traj_prenatal[order(anc_values_traj_prenatal$genus),]
anc_values_traj_prenatal

#Central estimates first
anc_values_traj_postnatal <- matrix(nrow = length(nodes), ncol = 2, dimnames = list(nodes,anc_col_names))
anc_values_traj_postnatal[,"Slope"] <- slope_postnatal_anc.ML
anc_values_traj_postnatal[,"Intercept"] <- int_postnatal_anc.ML
anc_values_traj_postnatal
#Add CI data
anc_values_traj_postnatal <- as.data.frame(anc_values_traj_postnatal)
anc_values_traj_postnatal$genus <- as.character(nodes)
#Order based on node number
anc_values_traj_postnatal <- anc_values_traj_postnatal[order(anc_values_traj_postnatal$genus),]
anc_values_traj_postnatal

##Plot allometric trajectories with abline for each group and ancestral node
#Check groups and variables
glimpse(allometry_genus_plot)

#Make tibble and add columns for plotting
anc_values_traj_prenatal <- anc_values_traj_prenatal %>% as_tibble() %>% mutate(stage = "prenatal", group = "anc_node")
#Make tibble and add columns for plotting
anc_values_traj_postnatal <- anc_values_traj_postnatal %>% as_tibble() %>% mutate(stage = "postnatal", group = "anc_node")

values_traj_plot <- bind_rows(anc_values_traj_prenatal, anc_values_traj_postnatal)

#Create palette including anc nodes
mypalette_greys <- brewer.pal(9,"Greys")
image(1:9,1, as.matrix(1:9), col = mypalette_greys,xlab="Greys (sequential)",
      ylab = "", yaxt = "n")

mypalette_nodes <- c(mypalette_taxa, mypalette_greys[9], mypalette_greys[7], mypalette_greys[5], mypalette_greys[3])
image(1:8, 1, as.matrix(1:8), col = mypalette_nodes, 
      xlab = "nodes colors - Delp, Glob, Lage, Phoc, Sten, anc node 6, and node 7, anc node 8, anc node 9",
      ylab = "", yaxt = "n")

mypalette_nodes_only <- c(mypalette_greys[9], mypalette_greys[7], mypalette_greys[5], mypalette_greys[3])

#Plot both stages together - abline just to check it works and line are where expected
allometry_anc_nodes_ggplot <- allometry_genus_stage_ggplot +
  #line on plot
  geom_abline(data = anc_values_traj_prenatal, 
              aes(intercept = Intercept, slope = Slope, group = genus), linetype = 2, size  = 1)+
  geom_abline(data = anc_values_traj_postnatal, 
              aes(intercept = Intercept, slope = Slope, group = genus), linetype = 1, size  = 1)
allometry_anc_nodes_ggplot

#TEST DIFFERENCES BETWEEN ANC STATES AND GROUPS IN SLOPE/INTERCEPT - EMMEANS ----

##Create estimated values for anc and genera
logCsize_prenatal <- gdf$size[which(gdf$stage == "prenatal")]
logCsize_postnatal <- gdf$size[which(gdf$stage == "postnatal")]

#Make x values (size) based on modern
anc_X_prenatal <- expand.grid(logCS = seq(min(logCsize_prenatal), 
                                          max(logCsize_prenatal), #use min and max of x values as limits  
                                          length.out = length(genera_list)*length(nodes)))      #length of sequence must be multiple of taxa for later
anc_X_postnatal <- expand.grid(logCS = seq(min(logCsize_postnatal), 
                                           max(logCsize_postnatal), length.out = length(genera_list)*length(nodes))) 

#Calculate Y values (reg scores) for ancestral nodes using anc.ML intercept and slope values
#List slopes and intercepts to make loop work
#Empty objects
slopes_anc_prenatal <- as.list(slope_prenatal_anc.ML)
intercepts_anc_prenatal <- as.list(int_prenatal_anc.ML)
slopes_anc_postnatal <- as.list(slope_postnatal_anc.ML)
intercepts_anc_postnatal <- as.list(int_postnatal_anc.ML)

#Empty object
anc_Y_list_prenatal <- list()
anc_Y_list_postnatal <- list()

#Loop
for (a in 1:length(nodes)){
  anc_Y_list_prenatal[[a]] <- intercepts_anc_prenatal[[a]] + 
    (anc_X_prenatal * slopes_anc_prenatal[[a]])
  anc_Y_list_postnatal[[a]] <- intercepts_anc_postnatal[[a]] + 
    (anc_X_postnatal * slopes_anc_postnatal[[a]])
}

#Make vector for Y values
anc_Y_prenatal <- as.vector(unlist(anc_Y_list_prenatal))
anc_Y_postnatal <- as.vector(unlist(anc_Y_list_postnatal))

#Create groups and order variables for ancestral states data
anc_nodes <- cbind(rep(nodes, each =  dim(anc_X_postnatal)[[1]]))
anc_groups <- rep("anc_node", times = dim(anc_X_postnatal)[[1]])
#Calculate number of reps for each category based on number of specimens 
anc_category_prenatal <- rep(categories_list[1:2], each = dim(anc_X_postnatal)[[1]]/2)
anc_category_postnatal <- rep(categories_list[3:4], each = dim(anc_X_postnatal)[[1]]/2)

#Create data frame with ReScores, logCS estimated and groups and orders for anc to match allometry tibble
allometry_nodes_prenatal <- data.frame(logCS = anc_X_prenatal, RegScores = anc_Y_prenatal, 
                                       genus = as.character(anc_nodes), group = anc_groups, category = anc_category_prenatal)
allometry_nodes_prenatal <- allometry_nodes_prenatal %>% as_tibble(allometry_nodes_prenatal) %>% mutate(stage = "prenatal")
glimpse(allometry_nodes_prenatal)

allometry_nodes_postnatal <- data.frame(logCS = anc_X_postnatal, RegScores = anc_Y_postnatal, 
                                        genus = as.character(anc_nodes), group = anc_groups, category = anc_category_postnatal)
allometry_nodes_postnatal <- allometry_nodes_postnatal %>% as_tibble(allometry_nodes_postnatal) %>% mutate(stage = "postnatal")
glimpse(allometry_nodes_postnatal)

#Combine all anc data
allometry_nodes_all <- bind_rows(allometry_nodes_prenatal, allometry_nodes_postnatal)

allometry_nodes_all$stage_genus <- paste(allometry_nodes_all$genus, allometry_nodes_all$stage, sep="_")
allometry_nodes_all

#Combine with allometry tibble
colnames(allometry_genus_plot) #check columns
colnames(allometry_nodes_all)
#Get column names to delete
col_diffs <- setdiff(colnames(allometry_genus_plot),colnames(allometry_nodes_all))

#Create new data frame with taxa and anc data
allometry_genus_all <- select(allometry_genus_plot, -col_diffs)

allometry_anc_all <- bind_rows(allometry_genus_all, allometry_nodes_all)

##Order values by genus, useful for plot legend
#Make factor for variable
allometry_anc_all$genus <- factor(allometry_anc_all$genus, 
                                  levels = c(genera_list, as.character(nodes))) #copy from string printed with the code above
#Order
allometry_anc_all <- allometry_anc_all[order(allometry_anc_all$genus),]
#Check
glimpse(allometry_anc_all)

#Save the data frames for stages with ordered nodes 
allometry_anc_all_prenatal <- allometry_anc_all %>% filter(stage == "prenatal") %>% as_tibble()
allometry_anc_all_postnatal <- allometry_anc_all %>% filter(stage == "postnatal") %>% as_tibble()

##Pairwise comparison of regression model between groups
#Create models, with different slopes and int or just int
allometry_anc_all_null <- lm.rrpp(RegScores ~ logCS * genus,   #null model with only differences by genus
                                  data = allometry_anc_all, print.progress = FALSE, iter = 999) 
allometry_anc_all_comb <- lm.rrpp(RegScores ~ logCS + stage_genus,
                                  data = allometry_anc_all, print.progress = FALSE, iter = 999) 
allometry_anc_all_int <- lm.rrpp(RegScores ~ logCS * stage_genus,
                                 data = allometry_anc_all, print.progress = FALSE, iter = 999) 

#Check results
summary(allometry_anc_all_null)
summary(allometry_anc_all_comb)
summary(allometry_anc_all_int)

#Anova for difference between models
anova_allometry_anc_models <- anova(allometry_anc_all_null, allometry_anc_all_comb, allometry_anc_all_int)
anova_allometry_anc_models

#Check slopes different for best model
anova(allometry_anc_all_int)

#Pairwise comparison of slopes using emmeans package
#First recreate model with lm() 
allometry_anc_all_int1 <- lm(RegScores ~ logCS * stage_genus,
                                 data = allometry_anc_all) 
#Check anova still ok
anova(allometry_anc_all_int1)

#Get pairwise comparisons of slopes - p values and differences
allometry_anc_all_emms <- emmeans(allometry_anc_all_int1, "stage_genus")

#to make graph, confusing for lots of groups - pwpp(allometry_anc_all_emms)

#Visualize table with p values only - for heatmaps
allometry_anc_all_ems_table <- pwpm(allometry_anc_all_emms, diffs = F, means = F) #eliminate differences and means in the diagonal, only keep p values
#Transform in table
allometry_anc_all_ems_table <- char(allometry_anc_all_ems_table)

#Save results to file
sink("Output/pairwise_allometry_anc_nodes_genera.txt")
print("ANOVA models")
print(anova_allometry_anc_models)

print("summary best model - lmrpp")
anova(allometry_anc_all_int)

print("summary model used for comparisons - lm")
summary(allometry_anc_all_int1)
anova(allometry_anc_all_int1)

print("Pairwise comparison using emmeans")
summary(allometry_anc_all_emms)

print("Full results table emmeans pairwise comparison")
pwpm(allometry_anc_all_emms)
sink()

##Heatmaps plots for significant differences in slopes ----

#Make already melted table with p-values
allometry_anc_all_ems_pvals <- as.table(allometry_anc_all_ems_table)
allometry_anc_all_ems_pvals <- as.data.frame(allometry_anc_all_ems_pvals)

#Delete extra columns
allometry_anc_all_ems_pvals <- allometry_anc_all_ems_pvals[,-c(3:4)]
#Delete empty rows for p vals
allometry_anc_all_ems_pvals <- allometry_anc_all_ems_pvals[-which(allometry_anc_all_ems_pvals$Freq.Freq == ""), ]

#Change last column name
names(allometry_anc_all_ems_pvals)[3] <- "p"

#Delete "<" so that it can be converted to numeric
allometry_anc_all_ems_pvals$p <- str_replace(allometry_anc_all_ems_pvals$p, "<", "0")
allometry_anc_all_ems_pvals$p <- as.numeric(allometry_anc_all_ems_pvals$p)

#Make vars names shorter
#Loop replacements stages
for (s in 1:length(stages_list)){
  allometry_anc_all_ems_pvals$Var1 <- str_replace_all(allometry_anc_all_ems_pvals$Var1, stages_list[s], stages_list_short[s])
  allometry_anc_all_ems_pvals$Var2 <- str_replace_all(allometry_anc_all_ems_pvals$Var2, stages_list[s], stages_list_short[s])
}

#Loop replacements genera
for (t in 1:length(genera_list)){
  allometry_anc_all_ems_pvals$Var1 <- str_replace_all(allometry_anc_all_ems_pvals$Var1, genera_list[t], genera_list_short[t])
  allometry_anc_all_ems_pvals$Var2 <- str_replace_all(allometry_anc_all_ems_pvals$Var2, genera_list[t], genera_list_short[t])
}

#Loop replacements nodes to match text - simpler numbering
nodes <- as.character(nodes)
nodes_tree <- as.character(c(1:4))

for (h in 1:length(nodes)){
  allometry_anc_all_ems_pvals$Var1 <- str_replace_all(allometry_anc_all_ems_pvals$Var1, nodes[h], nodes_tree[h])
  allometry_anc_all_ems_pvals$Var2 <- str_replace_all(allometry_anc_all_ems_pvals$Var2, nodes[h], nodes_tree[h])
}

#Make list of only anc node labels with stages
anc_list_short <- levels(as.factor(allometry_anc_all_ems_pvals$Var1))
anc_list_short <- anc_list_short[1:c(length(nodes)*2)]

#Subset data frame to keep only anc nodes for plots
allometry_anc_all_ems_pvals <- subset(allometry_anc_all_ems_pvals, Var1 %in% anc_list_short)

#Create columns where only significant values are shown
allometry_anc_all_ems_pvals <-  allometry_anc_all_ems_pvals %>% mutate(sig_p = ifelse(p < .05, T, F),
                                                      p_if_sig = ifelse(sig_p, p, NA))
allometry_anc_all_ems_pvals

#Nice heatmap plot all data
pairwise_allometry_anc_all_heatmap_ggplot  <- ggplot(data = allometry_anc_all_ems_pvals, aes(Var2, Var1, fill = p_if_sig))+
  geom_tile(colour = "gray80")+
  scale_fill_gradient2(low = mypalette_seq[9], high = mypalette_seq[2], mid = mypalette_seq[5], #negative correlations are in blue color and positive correlations in red. 
                       midpoint = 0.03, limit = c(0, 0.049), space = "Lab", #scale is from min to max p-values
                       na.value =  mypalette_seq[1], name = "P-values < 0.05") + 
  theme_minimal()+ 
  coord_fixed()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  ggtitle ("Slope differences anc states and taxa")+ 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x =  element_text(angle = 45, size = 11,hjust = 0.9),
        axis.text.y =  element_text(size = 12, vjust = -0.2, margin = NULL), panel.grid.major = element_blank(),
        legend.position = c(0.18,0.85),  legend.direction = "horizontal",
        legend.title = element_text(size = 10), legend.text = element_text(size = 8))+
  guides(fill = guide_colorbar(barwidth = 6, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
pairwise_allometry_anc_all_heatmap_ggplot

###Plots by growth stage
#Make data frame for each stage
allometry_anc_all_ems_pvals_prenatal <- allometry_anc_all_ems_pvals %>% filter(str_detect(Var1, "pren")) %>% filter(str_detect(Var2, "pren"))
allometry_anc_all_ems_pvals_postnatal <- allometry_anc_all_ems_pvals %>% filter(str_detect(Var1, "post")) %>% filter(str_detect(Var2, "post"))

#Nice heatmap plot
pairwise_allometry_anc_all_heatmap_ggplot_pren  <- ggplot(data = allometry_anc_all_ems_pvals_prenatal, aes(Var2, Var1, fill = p_if_sig))+
  geom_tile(colour = "gray80")+
  scale_fill_gradient2(low = mypalette_seq[9], high = mypalette_seq[2], mid = mypalette_seq[5], #negative correlations are in blue color and positive correlations in red. 
                       midpoint = 0.03, limit = c(0, 0.049), space = "Lab", #scale is from min to max p-values
                       na.value =  mypalette_seq[1], name = "P-values < 0.05") + 
  theme_minimal()+ 
  coord_fixed()+ 
  scale_x_discrete(labels = c(nodes_tree[2:4], genera_list_short))+ #first node not present on x
  scale_y_discrete(labels = nodes_tree)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  ggtitle ("Prenatal")+ 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x =  element_text(angle = 45, size = 12,hjust = 0.9),
        axis.text.y =  element_text(size = 12, vjust = -0.2, margin = NULL), panel.grid.major = element_blank(),
        legend.position = c(0.18,0.85),  legend.direction = "horizontal",
        legend.title = element_text(size = 11), legend.text = element_text(size = 10))+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1.1,
                               title.position = "top", title.hjust = 0.5))
pairwise_allometry_anc_all_heatmap_ggplot_pren

pairwise_allometry_anc_all_heatmap_ggplot_post  <- ggplot(data = allometry_anc_all_ems_pvals_postnatal, aes(Var2, Var1, fill = p_if_sig))+
  geom_tile(colour = "gray80")+
  scale_fill_gradient2(low = mypalette_seq[9], high = mypalette_seq[2], mid = mypalette_seq[5], #negative correlations are in blue color and positive correlations in red. 
                       midpoint = 0.03, limit = c(0, 0.049), space = "Lab", #scale is from min to max p-values
                       na.value =  mypalette_seq[1], name = "P-values < 0.05") + 
  theme_minimal()+ 
  coord_fixed()+ 
  scale_x_discrete(labels = c(nodes_tree[2:4], genera_list_short))+ #first node not present on x
  scale_y_discrete(labels = nodes_tree)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  ggtitle ("Postnatal")+ 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x =  element_text(angle = 45, size = 12, hjust = 0.9),
        axis.text.y =  element_text(size = 12, vjust = -0.2, margin = NULL), panel.grid.major = element_blank(),
        legend.position = c(0.18,0.85),  legend.direction = "horizontal",
        legend.title = element_text(size = 11), legend.text = element_text(size = 10))+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1.1,
                               title.position = "top", title.hjust = 0.5))
pairwise_allometry_anc_all_heatmap_ggplot_post

#Arrange in 1 plot
ggarrange(pairwise_allometry_anc_all_heatmap_ggplot_pren, pairwise_allometry_anc_all_heatmap_ggplot_post,
          ncol = 1, nrow = 2, common.legend = T)

###Plot allometry tips and anc nodes ----

##Plot both stages with fitted slopes
#Improve plot using new dataset
allometry_anc_all_ggplot  <- ggplot(allometry_anc_all, aes(x = logCS, y = RegScores, colour = genus, linetype = stage))+
  geom_smooth(data = allometry_anc_all_prenatal, aes(x = logCS, y = RegScores, colour = genus), method = 'lm',          #confidence intervals and reg line, before points
              se = F, show.legend = F, size = 1.2)+      #put col and other graphics OUTSIDE of aes()!!!
  geom_smooth(data = allometry_anc_all_postnatal, aes(x = logCS, y = RegScores,  colour = genus), method = 'lm',          #confidence intervals and reg line, before points
              se = F, size = 1.2)+      #put col and other graphics OUTSIDE of aes()!!! #put col and other graphics OUTSIDE of aes()!!!
  #points after, so they are on top
  scale_linetype_manual(name = "Developmental stages", labels = c("postnatal","prenatal"), values = c(1,2))+
  scale_color_manual(name = "Nodes", labels = c("Delphinapterus", "Globicephala" ,  "Lagenorhynchus", "Phocoena", "Stenella",
                                "Delphinoidea", "Delp+Phoc", "Delphinidae", "Sten+Glob"), values = mypalette_nodes)+         
  theme_classic(base_size = 12)+
  ylab("Regression Score")+
  ggtitle ("Allometry by genus with ancestral nodes")+ 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 11), legend.text = element_text(size = 10), 
        legend.title = element_text(size = 11), legend.position = c(0.7,0), legend.direction = "vertical", legend.justification = c(0.4,0),
        legend.key = element_blank(), legend.background = element_blank(), legend.box.margin = margin(0,0,0,0 ,unit = "pt"))+
  guides(colour =  guide_legend(ncol=2), linetype = guide_legend(override.aes = list(color = "black", size = 0.8)))
allometry_anc_all_ggplot 

#Add phylopics
allometry_anc_all_ggplot   <- 
  allometry_anc_all_ggplot   + 
  add_phylopic(Delph, alpha = 1, x = 2.85, y = -0.25, ysize = 0.15, color = mypalette_taxa[1])+
  add_phylopic(Globi, alpha = 1, x = 3.4, y = -0.04, ysize = 0.10, color = mypalette_taxa[2])+
  add_phylopic(Lage, alpha = 1, x = 2.9, y = 0.02, ysize = 0.18, color = mypalette_taxa[3])+
  add_phylopic(Phoc, alpha = 1, x = 2.425, y = -0.165, ysize = 0.08, color = mypalette_taxa[4])+
  add_phylopic(Sten, alpha = 1, x = 3.1, y = 0.13, ysize = 0.08, color = mypalette_taxa[5])
allometry_anc_all_ggplot 

##Make plots by stage using ablines (slope and intercept values only) - easier to see trends

#Make new tibbles with only coeffs for genera and anc nodes - 1 per stage
allometry_anc_all_coeffs_prenatal <- bind_rows(allometry_genus_coeffs_prenatal,anc_values_traj_prenatal)

#Make factor for variable
allometry_anc_all_coeffs_prenatal$genus <- factor(allometry_anc_all_coeffs_prenatal$genus, 
                                  levels = c(genera_list, as.character(nodes))) #copy from string printed with the code above
#Order
allometry_anc_all_coeffs_prenatal <- allometry_anc_all_coeffs_prenatal[order(allometry_anc_all_coeffs_prenatal$genus),]
#Check 
allometry_anc_all_coeffs_prenatal$genus

allometry_anc_all_coeffs_postnatal <- bind_rows(allometry_genus_coeffs_postnatal,anc_values_traj_postnatal)

#Make factor for variable
allometry_anc_all_coeffs_postnatal$genus <- factor(allometry_anc_all_coeffs_postnatal$genus, 
                                                  levels = c(genera_list, as.character(nodes))) #copy from string printed with the code above
#Order
allometry_anc_all_coeffs_postnatal <- allometry_anc_all_coeffs_postnatal[order(allometry_anc_all_coeffs_postnatal$genus),]
#Check 
allometry_anc_all_coeffs_postnatal$genus

#Plot prenatal with abline to get full lenght - easier comparison
allometry_anc_all_prenatal_ggplot  <- ggplot(allometry_anc_all_prenatal, aes(x = logCS, y = RegScores))+
  geom_point(size = 0, colour = "white")+
  #line on plot
  geom_abline(data = allometry_anc_all_coeffs_prenatal, 
              aes(intercept = Intercept, slope = Slope,  colour = genus, linetype = group), size  = 1.2)+
  #points after, so they are on top
  scale_linetype_manual(name = "Group", labels = c("ancestral node","Delphinidae", "Monodontidae", "Phocoenidae"), values = c(1:4))+
  scale_color_manual(name = "Nodes", labels = c("Delphinapterus", "Globicephala" ,  "Lagenorhynchus", "Phocoena", "Stenella",
                                                "Delphinoidea", "Delp+Phoc", "Delphinidae", "Sten+Glob"), values = mypalette_nodes)+         
  theme_classic(base_size = 12)+
  ylab("Regression Score")+
  ggtitle ("Allometry by genus with ancestral nodes - prenatal")+ 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 11), legend.text = element_text(size = 10), 
        legend.title = element_text(size = 11), legend.position = c(0.7,0), legend.direction = "vertical", legend.justification = c(0.4,0),
        legend.key = element_blank(), legend.background = element_blank(), legend.box.margin = margin(0,0,0,0 ,unit = "pt"))+
  guides(colour =  guide_legend(ncol=2), linetype = guide_legend(override.aes = list(color = "black", size = 0.8)))
allometry_anc_all_prenatal_ggplot

#Add phylopics
allometry_anc_all_prenatal_ggplot   <- 
  allometry_anc_all_prenatal_ggplot   + 
  add_phylopic(Delph, alpha = 1, x = 2.75, y = -0.25, ysize = 0.12, color = mypalette_taxa[1])+
  add_phylopic(Globi, alpha = 1, x = 3.3, y = -0.04, ysize = 0.08, color = mypalette_taxa[2])+
  add_phylopic(Lage, alpha = 1, x = 2.75, y = -0.05, ysize = 0.15, color = mypalette_taxa[3])+
  add_phylopic(Phoc, alpha = 1, x = 2.45, y = -0.12, ysize = 0.07, color = mypalette_taxa[4])+
  add_phylopic(Sten, alpha = 1, x = 3.1, y = 0.1, ysize = 0.07, color = mypalette_taxa[5])
allometry_anc_all_prenatal_ggplot 

#Plot postnatal with abline to get full lenght - easier comparison
allometry_anc_all_postnatal_ggplot  <- ggplot(allometry_anc_all_postnatal, aes(x = logCS, y = RegScores))+
  geom_point(size = 0, colour = "white")+
  #line on plot
  geom_abline(data = allometry_anc_all_coeffs_postnatal, 
              aes(intercept = Intercept, slope = Slope,  colour = genus, linetype = group), size  = 1.2)+
  #points after, so they are on top
  scale_linetype_manual(name = "Group", labels = c("ancestral node","Delphinidae", "Monodontidae", "Phocoenidae"), values = c(1:4))+
  scale_color_manual(name = "Nodes", labels = c("Delphinapterus", "Globicephala" ,  "Lagenorhynchus", "Phocoena", "Stenella",
                                                "Delphinoidea", "Delp+Phoc", "Delphinidae", "Sten+Glob"), values = mypalette_nodes)+         
  theme_classic(base_size = 12)+
  ylab("Regression Score")+
  ggtitle ("Allometry by genus with ancestral nodes - postnatal")+ 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 11), legend.text = element_text(size = 8), 
        legend.title = element_text(size = 9), legend.position = c(0.2,0.8), 
        legend.key = element_blank(), legend.background = element_blank(), legend.box.margin = margin(0,0,0,0 ,unit = "pt"))+
  guides(colour =  guide_legend(ncol=2), 
         linetype = guide_legend(direction = "horizontal", title.position = "top", ncol = 2, override.aes = list(color = "black", size = 0.8)))
allometry_anc_all_postnatal_ggplot

#Add phylopics
allometry_anc_all_postnatal_ggplot   <- 
  allometry_anc_all_postnatal_ggplot   + 
  add_phylopic(Delph, alpha = 1, x = 3.68, y = 0.15, ysize = 0.05, color = mypalette_taxa[1])+
  add_phylopic(Globi, alpha = 1, x = 3.6, y = 0.075, ysize = 0.04, color = mypalette_taxa[2])+
  add_phylopic(Lage, alpha = 1, x = 3.5, y = 0.18, ysize = 0.08, color = mypalette_taxa[3])+
  add_phylopic(Phoc, alpha = 1, x = 3.6, y = 0.115, ysize = 0.03, color = mypalette_taxa[4])+
  add_phylopic(Sten, alpha = 1, x = 3.51, y = 0.22, ysize = 0.03, color = mypalette_taxa[5])
allometry_anc_all_postnatal_ggplot 

#####
#To get packages citations
#remotes::install_github("Pakillo/grateful")
library(grateful)
cite_packages(style = "pnas", out.format = "docx")

#Session info
sink("sessionInfo.txt")
sessionInfo()
sink()