#######################################################
#### Gender bias occupations (jitter) --> k-means #####
#######################################################

# import some libraries
library(clusterCrit)
library(cluster)
library(factoextra)
library(openxlsx)
library(tidyverse)

rm(list = ls())

###########################################################################

# set the seed
set.seed(48)

# load data
load("occu_jitter_names.RData") #carichiamo i dati con i nomi delle occupazioni

#change occujitt and the file name in the other file
occujitt<- occujitt[ order(row.names(occujitt)), ]  #sort alphabetically
occu_names<-row.names(occujitt)                     #we get the names of the rows

filename='k_means_occu_2_5.xlsx'    #name of the excel file

n_obs<-dim(occujitt)[1] #number of observations
n_decade<-11            #decades

##### USEFUL FUNCTIONS #####
#we need some functions to find the indeces and the max and min values in a cluster

# function finding the indexes
find_indexes<-function(decade, cluster_label){
  
  indexes<-as.vector(which(cluster_labels[,decade] %in% cluster_label))
  return (indexes)
}

# function finding the min value
find_min_value <-function(decade,indexes){
  
  min_value<-occujitt[indexes[1],decade]
  if (length(indexes) < 2) {
    return(min_value)
  }
  
  for (i in 2:length(indexes)){
    
    if(occujitt[indexes[i],decade]<min_value){
      min_value<-occujitt[indexes[i],decade]
    }
    
  }
  return (min_value)
}


# function finding the max value
find_max_value <-function(decade,indexes){
  
  max_value<-occujitt[indexes[1],decade]
  if (length(indexes) < 2) {
    return(max_value)
  }
  
  for (i in 2:length(indexes)){
    
    if(occujitt[indexes[i],decade]>max_value){
      max_value<-occujitt[indexes[i],decade]
    }
    
  }
  return (max_value)
}

# function finding the mean value of a cluster (centroid)
find_mean_value <- function(decade, indexes){
  cluster<-occujitt[indexes, decade]
  return (mean(cluster))
}

# function finding the standard deviation value of a cluster
find_sd_value <- function (decade, indexes){
  cluster<-occujitt[indexes, decade]
  std_value=0
  if(length(indexes)>1){
    std_value=sd(cluster)
  }
  return (std_value)
}


###########
## K = 2 ##
###########

n_clust<-2              #number of clusters

# initialize some useful objects
cluster_labels<- matrix(ncol=n_decade, nrow=n_obs)
centroids<-matrix(ncol=n_decade, nrow=n_clust)
sizes<-matrix(ncol=n_decade, nrow=n_clust)
wss<-sizes<-matrix(ncol=n_decade, nrow=n_clust)
std_deviations<-matrix(ncol=n_decade, nrow=n_clust)



### K-means analysis ###

for (i in 1:n_decade){ #iterate in the decades
  
  data_decade<-occujitt[,i]
  km_res <- kmeans(data_decade, n_clust, nstart = 15)      #k-means
  
  
  cluster_labels[,i]<-as.vector(km_res$cluster)            #clusters
  centroids[,i]<-as.vector(km_res$centers)                 #centroids
  sizes[,i]<-as.vector(km_res$size)                        #size of clusters
  wss[,i]<-as.vector(km_res$withinss)                      #WSS
  
  # for(j in 1:n_clust){
  #   std_deviations[j,i]<-sqrt(wss[j,i]/(sizes[j,i]))       #standard deviation
  # }
}

# copy cluster labels for final evaluations with indices
cluster_labels_2=cluster_labels


# CREATE EXCEL OBJECTS

M=matrix(data=NA,nrow=n_decade*2,ncol = n_clust)   #matrix that will be printed in the excel file
Y=c()                                              #vector with the names of the decades


for (i in 1:n_decade){
  Y[2*i]=sprintf("Year %d", 1890+10*i)   #string such as "Year 1920"
  
  for (j in 1: n_clust){       #iterate up the number of clusters
    
    indexes_decade_i_label_j<-find_indexes(i,j)
    
    min_val<-find_min_value(i,indexes_decade_i_label_j)
    max_val<-find_max_value(i,indexes_decade_i_label_j)
    mean_val<-find_mean_value(i,indexes_decade_i_label_j)
    sd_val<-find_sd_value(i,indexes_decade_i_label_j)
    
    #newvalues and newnames are strings. thew will be the content of the cells 
    newvalues<-sprintf("Cluster %d Mean(%f) Std(%f) Min (%f) Max (%f) Count(%d)",   
                       j,mean_val,sd_val,min_val,max_val,sizes[j,i]) 
    
    newnames=paste(occu_names[indexes_decade_i_label_j], collapse=", ") #components (adj or occu) of a new cluster
      
    #each column represents the j-th cluster (among all the decades)
    M[2*i-1,j]=newvalues   #upper cell: characteristcs of the cluster (or white cell)
    M[2*i,j]=newnames      #lower cell: components (adj or occu) of the cluster (or white cell)
    
  }
  
  #cat ("anno", i, "terminato", "\n")  per vedere se completa il ciclo
}

# merge results
M_final2=cbind(Y,M)   #merge Y and M


###########
## K = 3 ##
###########
rm(list = c('i','j','M','centroids','cluster_labels','km_res','sizes','std_deviations','wss','data_decade','indexes_decade_i_label_j','max_val','mean_val','min_val','n_clust','newnames','newvalues','Y'))

n_clust<-3              #number of clusters


# initialize some useful objects
cluster_labels<- matrix(ncol=n_decade, nrow=n_obs)
centroids<-matrix(ncol=n_decade, nrow=n_clust)
sizes<-matrix(ncol=n_decade, nrow=n_clust)
wss<-sizes<-matrix(ncol=n_decade, nrow=n_clust)
std_deviations<-matrix(ncol=n_decade, nrow=n_clust)

### K-means analysis ###
for (i in 1:n_decade){ #iterate in the decades
  
  data_decade<-occujitt[,i]
  km_res <- kmeans(data_decade, n_clust, nstart = 15)      #k-means
  
  
  cluster_labels[,i]<-as.vector(km_res$cluster)            #clusters
  centroids[,i]<-as.vector(km_res$centers)                 #centroids
  sizes[,i]<-as.vector(km_res$size)                        #size of clusters
  wss[,i]<-as.vector(km_res$withinss)                      #WSS
  
  # for(j in 1:n_clust){
  #   std_deviations[j,i]<-sqrt(wss[j,i]/(sizes[j,i]))       #standard deviation
  # }
}

# copy cluster labels for final evaluations with indices
cluster_labels_3=cluster_labels


# CREATE EXCEL OBJECTS

M=matrix(data=NA,nrow=n_decade*2,ncol = n_clust)           #matrix that will be printed in the excel file
Y=c()                                                      #vector with the names of the decades

for (i in 1:n_decade){
  Y[2*i]=sprintf("Year %d", 1890+10*i)   #string such as "Year 1920"
  
  for (j in 1: n_clust){       #iterate up the number of clusters
    
    indexes_decade_i_label_j<-find_indexes(i,j)
    
    min_val<-find_min_value(i,indexes_decade_i_label_j)
    max_val<-find_max_value(i,indexes_decade_i_label_j)
    mean_val<-find_mean_value(i,indexes_decade_i_label_j)
    sd_val<-find_sd_value(i,indexes_decade_i_label_j)
    
    #newvalues and newnames are strings. thew will be the content of the cells 
    newvalues<-sprintf("Cluster %d Mean(%f) Std(%f) Min (%f) Max (%f) Count(%d)",   
                       j,mean_val,sd_val,min_val,max_val,sizes[j,i])
    
    newnames=paste(occu_names[indexes_decade_i_label_j], collapse=", ") #components (adj or occu) of a new cluster
    
    #each column represents the j-th cluster (among all the decades)
    M[2*i-1,j]=newvalues   #upper cell: characteristcs of the cluster (or white cell)
    M[2*i,j]=newnames      #lower cell: components (adj or occu) of the cluster (or white cell)
    
  }
  
  #cat ("anno", i, "terminato", "\n")  per vedere se completa il ciclo
}

# merge results
M_final3=cbind(Y,M)   #merge Y and M

###########
## K = 4 ##
###########
rm(list = c('i','j','M','centroids','cluster_labels','km_res','sizes','std_deviations','wss','data_decade','indexes_decade_i_label_j','max_val','mean_val','min_val','n_clust','newnames','newvalues','Y'))

n_clust<-4              #number of clusters

# initialize some useful objects
cluster_labels<- matrix(ncol=n_decade, nrow=n_obs)
centroids<-matrix(ncol=n_decade, nrow=n_clust)
sizes<-matrix(ncol=n_decade, nrow=n_clust)
wss<-sizes<-matrix(ncol=n_decade, nrow=n_clust)
std_deviations<-matrix(ncol=n_decade, nrow=n_clust)

### K-means analysis ###
for (i in 1:n_decade){ #iterate in the decades
  
  data_decade<-occujitt[,i]
  km_res <- kmeans(data_decade, n_clust, nstart = 15)      #k-means
  
  
  cluster_labels[,i]<-as.vector(km_res$cluster)            #clusters
  centroids[,i]<-as.vector(km_res$centers)                 #centroids
  sizes[,i]<-as.vector(km_res$size)                        #size of clusters
  wss[,i]<-as.vector(km_res$withinss)                      #WSS
  
  # for(j in 1:n_clust){
  #   std_deviations[j,i]<-sqrt(wss[j,i]/(sizes[j,i]))       #standard deviation
  # }
}

# copy cluster labels for final evaluations with indices
cluster_labels_4=cluster_labels


# CREATE EXCEL OBJECTS
M=matrix(data=NA,nrow=n_decade*2,ncol = n_clust)   #matrix that will be printed in the excel file
Y=c()                                              #vector with the names of the decades
for (i in 1:n_decade){
  Y[2*i]=sprintf("Year %d", 1890+10*i)   #string such as "Year 1920"
  
  for (j in 1: n_clust){       #iterate up the number of clusters
    
    indexes_decade_i_label_j<-find_indexes(i,j)
    
    min_val<-find_min_value(i,indexes_decade_i_label_j)
    max_val<-find_max_value(i,indexes_decade_i_label_j)
    mean_val<-find_mean_value(i,indexes_decade_i_label_j)
    sd_val<-find_sd_value(i,indexes_decade_i_label_j)
    
    #newvalues and newnames are strings. thew will be the content of the cells 
    newvalues<-sprintf("Cluster %d Mean(%f) Std(%f) Min (%f) Max (%f) Count(%d)",   
                       j,mean_val,sd_val,min_val,max_val,sizes[j,i])
    
    newnames=paste(occu_names[indexes_decade_i_label_j], collapse=", ") #components (adj or occu) of a new cluster
    
    #each column represents the j-th cluster (among all the decades)
    M[2*i-1,j]=newvalues   #upper cell: characteristcs of the cluster (or white cell)
    M[2*i,j]=newnames      #lower cell: components (adj or occu) of the cluster (or white cell)
    
  }
  
  #cat ("anno", i, "terminato", "\n")  per vedere se completa il ciclo
}

M_final4=cbind(Y,M)   #merge Y and M


###########
## K = 5 ##
###########
rm(list = c('i','j','M','centroids','cluster_labels','km_res','sizes','std_deviations','wss','data_decade','indexes_decade_i_label_j','max_val','mean_val','min_val','n_clust','newnames','newvalues','Y'))

n_clust<-5              #number of clusters

# initialize some useful objects
cluster_labels<- matrix(ncol=n_decade, nrow=n_obs)
centroids<-matrix(ncol=n_decade, nrow=n_clust)
sizes<-matrix(ncol=n_decade, nrow=n_clust)
wss<-sizes<-matrix(ncol=n_decade, nrow=n_clust)
std_deviations<-matrix(ncol=n_decade, nrow=n_clust)

### K-means analysis ###
for (i in 1:n_decade){ #iterate in the decades
  
  data_decade<-occujitt[,i]
  km_res <- kmeans(data_decade, n_clust, nstart = 15)      #k-means
  
  
  cluster_labels[,i]<-as.vector(km_res$cluster)            #clusters
  centroids[,i]<-as.vector(km_res$centers)                 #centroids
  sizes[,i]<-as.vector(km_res$size)                        #size of clusters
  wss[,i]<-as.vector(km_res$withinss)                      #WSS
  
  # for(j in 1:n_clust){
  #   std_deviations[j,i]<-sqrt(wss[j,i]/(sizes[j,i]))       #standard deviation
  # }
}

# copy cluster labels for final evaluations with indices
cluster_labels_5=cluster_labels


# CREATE EXCEL OBJECTS

M=matrix(data=NA,nrow=n_decade*2,ncol = n_clust)   #matrix that will be printed in the excel file
Y=c()    #vector with the names of the decades
for (i in 1:n_decade){
  Y[2*i]=sprintf("Year %d", 1890+10*i)   #string such as "Year 1920"
  
  for (j in 1: n_clust){       #iterate up the number of clusters
    
    indexes_decade_i_label_j<-find_indexes(i,j)
    
    min_val<-find_min_value(i,indexes_decade_i_label_j)
    max_val<-find_max_value(i,indexes_decade_i_label_j)
    mean_val<-find_mean_value(i,indexes_decade_i_label_j)
    sd_val<-find_sd_value(i,indexes_decade_i_label_j)
    
    #newvalues and newnames are strings. thew will be the content of the cells 
    newvalues<-sprintf("Cluster %d Mean(%f) Std(%f) Min (%f) Max (%f) Count(%d)",   
                       j,mean_val,sd_val,min_val,max_val,sizes[j,i])
    
    newnames=paste(occu_names[indexes_decade_i_label_j], collapse=", ") #components (adj or occu) of a new cluster
    
    #each column represents the j-th cluster (among all the decades)
    M[2*i-1,j]=newvalues   #upper cell: characteristcs of the cluster (or white cell)
    M[2*i,j]=newnames      #lower cell: components (adj or occu) of the cluster (or white cell)
    
  }
  
  #cat ("anno", i, "terminato", "\n")  per vedere se completa il ciclo
}

M_final5=cbind(Y,M)   #merge Y and M


rm(list = c('i','j','M','centroids','cluster_labels','km_res','sizes','std_deviations','wss','data_decade','indexes_decade_i_label_j','max_val','mean_val','min_val','n_clust','newnames','newvalues','Y'))

##### WRITE IN THE EXCEL FILE #####

# ATTENTION:
# one can use different methods to write in the excel file, just choose the one that works for you

##############FIRST METHOD (library xlsx, rJava required)
# library(xlsx)
#
# wb = xlsx::createWorkbook()
# 
# sheet = xlsx::createSheet(wb, "K = 2")
# 
# xlsx::addDataFrame(M_final2, sheet=sheet, startColumn=1, row.names=FALSE, col.names=FALSE)
# 
# sheet = xlsx::createSheet(wb, "K = 3")
# 
# xlsx::addDataFrame(M_final3, sheet=sheet, startColumn=1, row.names=FALSE, col.names=FALSE)
# 
# sheet = xlsx::createSheet(wb, "K = 4")
# 
# xlsx::addDataFrame(M_final4, sheet=sheet, startColumn=1, row.names=FALSE, col.names=FALSE)
# 
# sheet = xlsx::createSheet(wb, "K = 5")
# 
# xlsx::addDataFrame(M_final5, sheet=sheet, startColumn=1, row.names=FALSE, col.names=FALSE)
# 
# xlsx::saveWorkbook(wb, filename)

##############SECOND METHOD (library openxlsx, no rJava required. Be sure that the excel file has actually 4 sheets)
# 
# write.xlsx(
#   M_final2,
#   filename,
#   sheetName = "K=2",
#   col.names = FALSE,
#   row.names = FALSE,
#   append = TRUE,
#   showNA = FALSE,
#   password = NULL
# )
# 
# write.xlsx(
#   M_final3,
#   filename,
#   sheetName = "K=3",
#   col.names = FALSE,
#   row.names = FALSE,
#   append = TRUE,
#   showNA = FALSE,
#   password = NULL
# )
# 
# write.xlsx(
#   M_final4,
#   filename,
#   sheetName = "K=4",
#   col.names = FALSE,
#   row.names = FALSE,
#   append = TRUE,
#   showNA = FALSE,
#   password = NULL
# )
# 
# write.xlsx(
#   M_final5,
#   filename,
#   sheetName = "K=5",
#   col.names = FALSE,
#   row.names = FALSE,
#   append = TRUE,
#   showNA = FALSE,
#   password = NULL
# )


##############THIRD METHOD (library openxlsx, no rJava required)

wb <- createWorkbook()
addWorksheet(wb, "K=2")
addWorksheet(wb, "K=3")
addWorksheet(wb, "K=4")
addWorksheet(wb, "K=5")

writeData(wb, sheet = "K=2", x = M_final2, colNames = FALSE, rowNames = FALSE)
writeData(wb, sheet = "K=3", x = M_final3, colNames = FALSE, rowNames = FALSE)
writeData(wb, sheet = "K=4", x = M_final4, colNames = FALSE, rowNames = FALSE)
writeData(wb, sheet = "K=5", x = M_final5, colNames = FALSE, rowNames = FALSE)

saveWorkbook(wb, file=filename, overwrite = TRUE)

######## Cluster estimates visualization ##################
## (le immagini dei boxplots sono nella cartella "plots")##

#choose k  by changing cluster_labels_k  (k=2,3,4,5) and the title of the plots if you like 


data_plot<-as.data.frame(occujitt[,1])
data_plot$labels<-as.factor(cluster_labels_4[,1])
gg_plot_occu_1900 <- ggplot(data_plot, aes(x = labels, y = occujitt[,1])) + 
  geom_boxplot(width=0.5, color="black", alpha=0.6) + 
  ggtitle("Gender bias occu 1900 (4-Means)") +geom_jitter(color= "red",width=0.15)+geom_hline(yintercept=0, color= "grey")

data_plot<-as.data.frame(occujitt[,6])
data_plot$labels<-as.factor(cluster_labels_4[,6])
gg_plot_occu_1950 <- ggplot(data_plot, aes(x = labels, y = occujitt[,6])) + 
  geom_boxplot(width=0.5, color="black", alpha=0.6) + 
  ggtitle("Gender bias occu 1950 (4-Means)") +geom_jitter(color= "red",width=0.15)+geom_hline(yintercept=0, color= "grey")

data_plot<-as.data.frame(occujitt[,11])
data_plot$labels<-as.factor(cluster_labels_4[,11])
gg_plot_occu_2000 <- ggplot(data_plot, aes(x = labels, y = occujitt[,11])) + 
  geom_boxplot(width=0.5, color="black", alpha=0.6) + 
  ggtitle("Gender bias occu 2000 (4-Means)") +geom_jitter(color= "red",width=0.15)+geom_hline(yintercept=0, color= "grey")

x11()
require(gridExtra)
gg_plot_occu_1900
gg_plot_occu_1950
gg_plot_occu_2000
grid.arrange(gg_plot_occu_1900, gg_plot_occu_1950, gg_plot_occu_2000,  ncol=3)


##### Co-clustering represetations  #######

#### We define a function that takes as input a vector containing the labels that a particular clustering algorithm
#### has assigned to the observations and builds the co-clustering matrix for those labels. If the entry [i,j] of
#### the co-clustering matrix is 1 it means that observation i and observation j have been assigned to the same cluster


co_clust<- function(labels)
{
  
  n=length(labels)
  result= matrix(0,nrow= n, ncol= n)
  
  for(i in 1:n)
  {
    for(j in 1:n)
      
    {
      if (labels[i]== labels[j])
        result[i,j]=1
    }
    
  }
  
  return(result)
}


#### We define a function that takes as input a vector containing the labels that a particular clustering algorithm
#### has assigned to the observations and builds a matrix whose entries are such that if [i,j]=k
#### it means that observation i and observation j have been assigned to cluster k. If [i,j]=0 it means that
#### observations i and j have been assigned to different clusters (we suppose that the labels are {1,2,3,...})

matrix_labels <- function (labels)
{
  
  
  n=length(labels)
  result= matrix(0,nrow= n, ncol= n)
  
  for(i in 1:n)
  {
    for(j in 1:n)
      
    {
      if (labels[i]== labels[j])
        result[i,j]= labels[i]
    }
    
  }
  
  return(result)
}



#### Now we compute those matrix and we plot them


dec = 3 #choose a decade


co_clust_matrix = co_clust(cluster_labels_3[,dec]) #here choose k
mat_labels = matrix_labels(cluster_labels_3[,dec])


#We plot mt_labels

cols <- c(
  '0' = "#FFFFFF",
  '1' = "#CCCCCC",
  '2' = "#99FF33",
  '3' = "#FFF000",
  '4' = "#3300FF",
  '5' = "#CC0099",
  '6' = "#FF9933",
  '7' = "#FF0000",
  '8' = "#000333",
  '9' = "#CC9966",
  '10' = "#CCCC00"
)

image(1:nrow(mat_labels), 1:ncol(mat_labels), t(apply(mat_labels, 2, rev)), col=cols, xlab= "", ylab="")
title(main =paste("Clustering structure of the observations for  3-means (occu) in decade", dec, sep=" "), font.main = 1)

