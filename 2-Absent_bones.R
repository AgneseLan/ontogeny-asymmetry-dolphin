
#===========================================================#
#                                                           #
#     CURVES AND POINTS ANALYSES - ODONTOCETE FAMILIES      #
#                                                           #
#===========================================================#


#CH.2 - Assigning coordinates to landmarks of absent bones
#Code adapted from Ellen Coombs

#LOAD LIBRARIES ----
#always do this first!!
library(tidyverse)
library(Morpho)
library(geomorph)
library(Rvcg)
library(paleomorph)
library(EMMLi)
library(qgraph)
library(ape)
library(geiger)
library(abind)
library("devtools")
library(SURGE)
library(magick)

#ABSENT BONES ----

#Add the data for absent bones for specific species 
#Both curves and fixed LMs

###SET WD to root from console!! -->

#Import LMs list - curves listed in curve_table
LM_table <-  read_csv("Data/LMs.csv")

#Import sets of absent curves and LMs and open file to check what bones have absent data
absent_curves <- read.csv("Data/absent_curves.csv")
absent_LMs <- read.csv("Data/absent_LMs.csv")
View(absent_curves)
View(absent_LMs)

##Absent curves
#Look for bones with absent curves in curve list - column names
# colnames(absent_curves)
curve_nasal_l <- my_curves$Curves[which(curve_table$bone%in%c("nasal_l"))]%>%unlist(.)%>%unique(.)%>%sort(.)
curve_nasal_r <- my_curves$Curves[which(curve_table$bone%in%c("nasal_r"))]%>%unlist(.)%>%unique(.)%>%sort(.)
curve_interparietal <- my_curves$Curves[which(curve_table$bone%in%c("interparietal"))]%>%unlist(.)%>%unique(.)%>%sort(.)
curve_basioccipital_ll <- my_curves$Curves[which(curve_table$bone%in%c("basioccipital_ll"))]%>%unlist(.)%>%unique(.)%>%sort(.)
curve_basioccipital_lr <- my_curves$Curves[which(curve_table$bone%in%c("basioccipital_lr"))]%>%unlist(.)%>%unique(.)%>%sort(.)

#Create new object for absent curves
absentcurve <- slidedlms

absentcurve[curve_nasal_l,,4] #specimen number on the end, test if it worked - check specimen number form absent_curves file

#Loop to substitute coordinates for semilandmarks in absent bone curves
#Put first landmark of curve in matrix for each bone
#Left nasal 
for (i in 1:nrow(absent_curves)){
  if( !is.na(absent_curves$nasal_l[i]))
    absentcurve[curve_nasal_l,c(1:3),i] <- matrix(absentcurve[6,c(1:3),i], nrow = length(curve_nasal_l), ncol=3, byrow=TRUE)
}

#Right nasal
for (i in 1:nrow(absent_curves)){
  if( !is.na(absent_curves$nasal_r[i]))
    absentcurve[curve_nasal_r,c(1:3),i] <- matrix(absentcurve[15,c(1:3),i], nrow = length(curve_nasal_r), ncol=3, byrow=TRUE)
}

#Interparietal
for (i in 1:nrow(absent_curves)){
  if( !is.na(absent_curves$interparietal[i]))
    absentcurve[curve_interparietal,c(1:3),i] <- matrix(absentcurve[57,c(1:3),i], nrow = length(curve_interparietal), ncol=3, byrow=TRUE)
}

#Basioccipital lateral left
for (i in 1:nrow(absent_curves)){
  if( !is.na(absent_curves$basioccipital_ll[i]))
    absentcurve[curve_basioccipital_ll,c(1:3),i] <- matrix(absentcurve[56,c(1:3),i], nrow = length(curve_basioccipital_ll), ncol=3, byrow=TRUE)
}

#Basioccipital lateral right
for (i in 1:nrow(absent_curves)){
  if( !is.na(absent_curves$basioccipital_lr[i]))
    absentcurve[curve_basioccipital_lr,c(1:3),i] <- matrix(absentcurve[54,c(1:3),i], nrow = length(curve_basioccipital_lr), ncol=3, byrow=TRUE)
}


absentcurve[curve_nasal_l,,4] #check if it worked with specimen number with missing curve

##Absent LMs
#Look for absent bones first
# colnames(absent_LMs)
LMs_nasal_l <- LM_table$lm[which(LM_table$bone%in%c("nasal_l"))]
LMs_nasal_r <- LM_table$lm[which(LM_table$bone%in%c("nasal_r"))]
LMs_interparietal <- LM_table$lm[which(LM_table$bone%in%c("interparietal"))]

#Create new object for absent LMs
absentLM <- absentcurve

absentLM[LMs_nasal_l,,4] #specimen number on the end, test if it worked - check specimen number form absent_LMs file

#Loop to substitute coordinates for landmarks in absent bones
#Left nasal
  for (i in 1:nrow(absent_LMs)){
    if( !is.na(absent_LMs$nasal_l[i]))
      absentLM[LMs_nasal_l,c(1:3),i] <- matrix(absentLM[6,c(1:3),i], nrow = length(LMs_nasal_l), ncol=3, byrow=TRUE) #number (40) here is the LM that is missing 
  }

#Right nasal
for (i in 1:nrow(absent_LMs)){
  if( !is.na(absent_LMs$nasal_r[i]))
    absentLM[LMs_nasal_r,c(1:3),i] <- matrix(absentLM[15,c(1:3),i], nrow = length(LMs_nasal_r), ncol=3, byrow=TRUE) 
}

#Interparietal
for (i in 1:nrow(absent_LMs)){
  if( !is.na(absent_LMs$interparietal[i]))
    absentLM[LMs_interparietal,c(1:3),i] <- matrix(absentLM[57,c(1:3),i], nrow = length(LMs_interparietal), ncol=3, byrow=TRUE) 
}

absentLM[LMs_nasal_l,,4] #specimen number on the end, test if it worked - check specimen number form absent_LMs file

absentLM[curve_nasal_l,,4] #check curves still ok

#Create new object for analyses with all missing data, include only shape data
final_dataset <- absentLM

#Check plotting of absent bones
#Look up number for specimens with absent bones in absent_curves and absent_LMs

###SET WD to ply from console!! -->

#Plot
checkLM(final_dataset, path="", pt.size = 15, suffix=".ply", render = "s", begin = 50, point = "p")

#List of points and curves for different bones - useful for plots
nasals <- c(LMs_nasal_l, LMs_nasal_r, curve_nasal_l, curve_nasal_r)
supraoccipital <- c(LM_table$lm[which(LM_table$bone%in%c("supraoccipital"))], 
                    my_curves$Curves[which(curve_table$bone%in%c("supraoccipital"))]) %>% unlist(.)%>%unique(.)%>%sort(.)
basioccipital <- c(LM_table$lm[which(LM_table$bone%in%c("basioccipital", "basioccipital_lr", "basioccipital_ll"))],
                   my_curves$Curves[which(curve_table$bone%in%c("basioccipital", "basioccipital_ll", "basioccipital_lr"))]) %>% 
                  unlist(.)%>%unique(.)%>%sort(.)
maxilla <- c(LM_table$lm[which(LM_table$bone%in%c("maxilla"))], 
             my_curves$Curves[which(curve_table$bone%in%c("maxilla"))]) %>% unlist(.)%>%unique(.)%>%sort(.)
premaxilla <- c(LM_table$lm[which(LM_table$bone%in%c("premaxilla"))], 
             my_curves$Curves[which(curve_table$bone%in%c("premaxilla"))]) %>% unlist(.)%>%unique(.)%>%sort(.)
condyles <- c(LM_table$lm[which(LM_table$bone%in%c("condyle"))],
              my_curves$Curves[which(curve_table$bone%in%c("condyle"))])%>% unlist(.)%>%unique(.)%>%sort(.)
orbit <- c(LM_table$lm[which(LM_table$bone%in%c("frontal", "jugal"))], 
           my_curves$Curves[which(curve_table$bone%in%c("frontal"))]) %>% unlist(.)%>%unique(.)%>%sort(.)
squamosal <- c(LM_table$lm[which(LM_table$bone%in%c("squamosal"))], 
               my_curves$Curves[which(curve_table$bone%in%c("squamosal"))]) %>% unlist(.)%>%unique(.)%>%sort(.)
palatine <- c(LM_table$lm[which(LM_table$bone%in%c("palatine"))], 
              my_curves$Curves[which(curve_table$bone%in%c("palatine"))]) %>% unlist(.)%>%unique(.)%>%sort(.)
interparietal <- c(LMs_interparietal, curve_interparietal)
exoccipital <- c(LM_table$lm[which(LM_table$bone%in%c("exoccipital"))], 
                 my_curves$Curves[which(curve_table$bone%in%c("exoccipital"))]) %>% unlist(.)%>%unique(.)%>%sort(.)

#Check
spheres3d(final_dataset[nasals,,30], radius=1, color = "red") 
spheres3d(final_dataset[-nasals,,30], radius=1, color = "grey")

#Save coordinates to file
save(final_dataset, file = "~/final_dataset.RData")

###### 
#Next - ch. 3 - GPA and PCA
