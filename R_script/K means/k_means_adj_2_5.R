#######################################################
#### Gender bias adjectives (jitter) --> k-means #####
#######################################################

# import some libraries
library(clusterCrit)
library(cluster)
library(factoextra)
library(openxlsx)
library(tidyverse)

rm(list = ls())

###########################################################################


###########################################################################

# set the seed
set.seed(48)

# load data
load("adj_jitter_names.RData") #carichiamo i dati con i nomi delle occupazioni

adjcompjitt<- adjcompjitt[ order(row.names(adjcompjitt)), ]  #sort alphabetically
adj_names<-row.names(adjcompjitt)   #we get the names of the rows

filename='k_means_adj_2_5.xlsx'    #name of the excel file

n_obs<-dim(adjcompjitt)[1]
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
  
  min_value<-adjcompjitt[indexes[1],decade]
  if (length(indexes) < 2) {
    return(min_value)
  }
  
  for (i in 2:length(indexes)){
    
    if(adjcompjitt[indexes[i],decade]<min_value){
      min_value<-adjcompjitt[indexes[i],decade]
    }
    
  }
  return (min_value)
}

# function finding the max value
find_max_value <-function(decade,indexes){
  
  max_value<-adjcompjitt[indexes[1],decade]
  if (length(indexes) < 2) {
    return(max_value)
  }
  
  for (i in 2:length(indexes)){
    
    if(adjcompjitt[indexes[i],decade]>max_value){
      max_value<-adjcompjitt[indexes[i],decade]
    }
    
  }
  return (max_value)
}

# function finding the mean value of a cluster (centroid)
find_mean_value <- function(decade, indexes){
  cluster<-adjcompjitt[indexes, decade]
  return (mean(cluster))
}


# function finding the standard deviation value of a cluster
find_sd_value <- function (decade, indexes){
  cluster<-adjcompjitt[indexes, decade]
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

cluster_labels<- matrix(ncol=n_decade, nrow=n_obs)
centroids<-matrix(ncol=n_decade, nrow=n_clust)
sizes<-matrix(ncol=n_decade, nrow=n_clust)
wss<-sizes<-matrix(ncol=n_decade, nrow=n_clust)
std_deviations<-matrix(ncol=n_decade, nrow=n_clust)


for (i in 1:n_decade){
  
  data_decade<-adjcompjitt[,i]
  km_res <- kmeans(data_decade, n_clust, nstart = 15)
  
  
  cluster_labels[,i]<-as.vector(km_res$cluster)
  centroids[,i]<-as.vector(km_res$centers)
  sizes[,i]<-as.vector(km_res$size)
  wss[,i]<-as.vector(km_res$withinss)
  
  for(j in 1:n_clust){
    std_deviations[j,i]<-sqrt(wss[j,i]/(sizes[j,i]))
  }
}

# copy cluster labels for final evaluations with indices
cluster_labels_2=cluster_labels

#CREATE EXCEL OBJECTS

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
    
    newnames=paste(adj_names[indexes_decade_i_label_j], collapse=", ") #components (adj or occu) of a new cluster
      
    #each column represents the j-th cluster (among all the decades)
    M[2*i-1,j]=newvalues   #upper cell: characteristcs of the cluster (or white cell)
    M[2*i,j]=newnames      #lower cell: components (adj or occu) of the cluster (or white cell)
    
  }
  
  #cat ("anno", i, "terminato", "\n")  per vedere se completa il ciclo
}

M_final2=cbind(Y,M)   #merge Y and M

###########
## K = 3 ##
###########
rm(list = c('i','j','M','centroids','cluster_labels','km_res','sizes','std_deviations','wss','data_decade','indexes_decade_i_label_j','max_val','mean_val','min_val','n_clust','newnames','newvalues','Y'))

n_clust<-3

cluster_labels<- matrix(ncol=n_decade, nrow=n_obs)
centroids<-matrix(ncol=n_decade, nrow=n_clust)
sizes<-matrix(ncol=n_decade, nrow=n_clust)
wss<-sizes<-matrix(ncol=n_decade, nrow=n_clust)
std_deviations<-matrix(ncol=n_decade, nrow=n_clust)

for (i in 1:n_decade){
  
  data_decade<-adjcompjitt[,i]
  km_res <- kmeans(data_decade, n_clust, nstart = 15)
  
  
  cluster_labels[,i]<-as.vector(km_res$cluster)
  centroids[,i]<-as.vector(km_res$centers)
  sizes[,i]<-as.vector(km_res$size)
  wss[,i]<-as.vector(km_res$withinss)
  
  for(j in 1:n_clust){
    std_deviations[j,i]<-sqrt(wss[j,i]/(sizes[j,i]))
  }
}

# copy cluster labels for final evaluations with indices
cluster_labels_3=cluster_labels


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
    
    newnames=paste(adj_names[indexes_decade_i_label_j], collapse=", ") #components (adj or occu) of a new cluster
    
    #each column represents the j-th cluster (among all the decades)
    M[2*i-1,j]=newvalues   #upper cell: characteristcs of the cluster (or white cell)
    M[2*i,j]=newnames      #lower cell: components (adj or occu) of the cluster (or white cell)
    
  }
  
  #cat ("anno", i, "terminato", "\n")  per vedere se completa il ciclo
}

M_final3=cbind(Y,M)   #merge Y and M

###########
## K = 4 ##
###########
rm(list = c('i','j','M','centroids','cluster_labels','km_res','sizes','std_deviations','wss','data_decade','indexes_decade_i_label_j','max_val','mean_val','min_val','n_clust','newnames','newvalues','Y'))

n_clust<-4


cluster_labels<- matrix(ncol=n_decade, nrow=n_obs)
centroids<-matrix(ncol=n_decade, nrow=n_clust)
sizes<-matrix(ncol=n_decade, nrow=n_clust)
wss<-sizes<-matrix(ncol=n_decade, nrow=n_clust)
std_deviations<-matrix(ncol=n_decade, nrow=n_clust)

for (i in 1:n_decade){
  
  data_decade<-adjcompjitt[,i]
  km_res <- kmeans(data_decade, n_clust, nstart = 15)
  
  
  cluster_labels[,i]<-as.vector(km_res$cluster)
  centroids[,i]<-as.vector(km_res$centers)
  sizes[,i]<-as.vector(km_res$size)
  wss[,i]<-as.vector(km_res$withinss)
  
  # for(j in 1:n_clust){
  #   std_deviations[j,i]<-sqrt(wss[j,i]/(sizes[j,i]))
  # }
}


# copy cluster labels for final evaluations with indices
cluster_labels_4=cluster_labels


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
    
    newnames=paste(adj_names[indexes_decade_i_label_j], collapse=", ") #components (adj or occu) of a new cluster
    
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

n_clust<-5


cluster_labels<- matrix(ncol=n_decade, nrow=n_obs)
centroids<-matrix(ncol=n_decade, nrow=n_clust)
sizes<-matrix(ncol=n_decade, nrow=n_clust)
wss<-sizes<-matrix(ncol=n_decade, nrow=n_clust)
std_deviations<-matrix(ncol=n_decade, nrow=n_clust)

for (i in 1:n_decade){
  
  data_decade<-adjcompjitt[,i]
  km_res <- kmeans(data_decade, n_clust, nstart = 15)
  
  
  cluster_labels[,i]<-as.vector(km_res$cluster)
  centroids[,i]<-as.vector(km_res$centers)
  sizes[,i]<-as.vector(km_res$size)
  wss[,i]<-as.vector(km_res$withinss)
  
  # for(j in 1:n_clust){
  #   std_deviations[j,i]<-sqrt(wss[j,i]/(sizes[j,i]))
  # }
}

# copy cluster labels for final evaluations with indices
cluster_labels_5=cluster_labels


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
    
    newnames=paste(adj_names[indexes_decade_i_label_j], collapse=", ") #components (adj or occu) of a new cluster
    
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
#library(xlsx)
#
# wb = xlsx::createWorkbook()
# 
# sheet = xlsx::createSheet(wb, "K=2")
# 
# xlsx::addDataFrame(M_final2, sheet=sheet, startColumn=1, row.names=FALSE, col.names=FALSE)
# 
# sheet = xlsx::createSheet(wb, "K=3")
# 
# xlsx::addDataFrame(M_final3, sheet=sheet, startColumn=1, row.names=FALSE, col.names=FALSE)
# 
# sheet = xlsx::createSheet(wb, "K=4")
# 
# xlsx::addDataFrame(M_final4, sheet=sheet, startColumn=1, row.names=FALSE, col.names=FALSE)
# 
# sheet = xlsx::createSheet(wb, "K=5")
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
#   append = FALSE,
#   showNA = TRUE,
#   password = NULL
# )
# 
# #write in excel file
# write.xlsx(
#   M_final3,
#   filename,
#   sheetName = "K=3",
#   col.names = FALSE,
#   row.names = FALSE,
#   append = TRUE,
#   showNA = TRUE,
#   password = NULL
# )
# 
# #write in excel file
# write.xlsx(
#   M_final4,
#   filename,
#   sheetName = "K=4",
#   col.names = FALSE,
#   row.names = FALSE,
#   append = TRUE,
#   showNA = TRUE,
#   password = NULL
# )
# 
# #write in excel file
# write.xlsx(
#   M_final5,
#   filename,
#   sheetName = "K=5",
#   col.names = FALSE,
#   row.names = FALSE,
#   append = TRUE,
#   showNA = TRUE,
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

data_plot<-as.data.frame(adjcompjitt[,1])
data_plot$labels<-as.factor(cluster_labels_5[,1])
gg_plot_adj_1900 <- ggplot(data_plot, aes(x = labels, y = adjcompjitt[,1])) + 
  geom_boxplot(width=0.5, color="black", alpha=0.6) + 
  ggtitle("Gender bias adj 1900 (5-Means)") +geom_jitter(color= "red",width=0.15)+geom_hline(yintercept=0, color= "grey")

data_plot<-as.data.frame(adjcompjitt[,6])
data_plot$labels<-as.factor(cluster_labels_5[,6])
gg_plot_adj_1950 <- ggplot(data_plot, aes(x = labels, y = adjcompjitt[,6])) + 
  geom_boxplot(width=0.5, color="black", alpha=0.6) + 
  ggtitle("Gender bias adj 1950 (5-Means)") +geom_jitter(color= "red",width=0.15)+geom_hline(yintercept=0, color= "grey")

data_plot<-as.data.frame(adjcompjitt[,11])
data_plot$labels<-as.factor(cluster_labels_5[,11])
gg_plot_adj_2000 <- ggplot(data_plot, aes(x = labels, y = adjcompjitt[,11])) + 
  geom_boxplot(width=0.5, color="black", alpha=0.6) + 
  ggtitle("Gender bias adj 2000 (5-Means)") +geom_jitter(color= "red",width=0.15)+geom_hline(yintercept=0, color= "grey")

x11()
require(gridExtra)
gg_plot_adj_1900
gg_plot_adj_1950
gg_plot_adj_2000
grid.arrange(gg_plot_adj_1900, gg_plot_adj_1950, gg_plot_adj_2000,  ncol=3)


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
title(main =paste("Clustering structure of the observations for  3-means (adj) in decade", dec, sep=" "), font.main = 1)