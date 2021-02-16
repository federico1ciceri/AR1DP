#####################################
### PAM ALGORITHM FOR OCCUPATIONS ###
#####################################
#---------------------------------------------------------------------------------------
# WHY PAM
#---------------------------------------------------------------------------------------
# The k-medoids algorithm is a clustering approach related to k-means clustering for partitioning a data set into k clusters.
# The main advantage with respect to k-means is the partitioning robustness, as a matter of fact the algorithm is less sensitive 
# to noise and outliers. It happens because it uses medoids as cluster centers instead of means (used in k-means).
# The most common k-medoids clustering methods is the PAM algorithm (Partitioning Around Medoids).
# An useful approach to determine the optimal number of clusters is the silhouette method.
# The silhouette value is a measure of how similar an object is to its own cluster (cohesion) compared to other clusters (separation). 
# An high value shows a good partition into clusters.\\
#----------------------------------------------------------------------------------------

#################### Install packages ###############################################

#install.packages('openxlsx')
#install.packages('clusterCrit')
#install.packages('Hmisc')

library(cluster)
library(factoextra)
library(openxlsx)
library(clusterCrit)

################## Load data ############################################
# Set working directory with data!! 

# load data
load("occu_jitter_names.RData") #carichiamo i dati con i nomi delle occupazioni

set.seed(48) 
#change occujitt and the file name in the other file
file="pam_occu.xlsx"
df<- occujitt[ order(row.names(occujitt)), ]  #sort alphabetically

############################### Initialization ######################################
# PAM algorithm with best silhouette k (with k set from 2 to 10) 

Kmax=10 #max number of clusters 
n_decade=11 # decades
n_obs<-dim(df)[1] #dim data

dunn_indeces=c()
avg_sil_indeces=c()
numb_of_clusters=c()
cluster_labels<- matrix(ncol=n_decade, nrow=n_obs)
list_medoids<- list()
list_sizes<-list()
list_av_diss<-list() #here I will store the average dissimilarity in each cluster for each decade

####################### PAM algorithm #############################

for (i in 1:n_decade){      #iterate in the decades
  data_decade<-df[,i] 
  best_msil=0
  best_k=2
  pam=pam(data_decade, 2, metric = "euclidean", stand=FALSE)    #just to initialize
  
  #compute pam clustering with different number of clusters, than choose the one with the best avg_sil_index
  for (k in 2:Kmax){
    pam=pam(data_decade, k, metric = "euclidean", stand=FALSE)       #pam with different k
    sil=silhouette(pam)      #compute silhoutte
    msil=mean(sil[,3])       #compute average silhouette coefficient (quantity to maximize)
    #print(msil)
    if (msil>best_msil){
      best_msil=msil         #update best average silhouette coefficient
      pam_res=pam            #use the model with the best k found
      best_k=k
    }
  }
#--------------------------------------------------------------------------------------------------
### We compute some results and indices for the best clustering per each decade ###
  
  # DUNN index per each decade
  dunn_indeces[i]=intCriteria(matrix(data_decade),as.vector(pam_res$cluster), "Dunn")[[1]]
  
  # average silhouette coefficient per each decade
  avg_sil_indeces[i]=best_msil
  
  # number of clusters per each decade
  numb_of_clusters[i]=best_k      
  
  MM=cbind(numb_of_clusters,avg_sil_indeces,dunn_indeces)  #matrix storing relevant indices
  
  cluster_labels[,i]<-as.vector(pam_res$cluster)
  list_medoids[[i]]<-pam_res$medoids
  list_sizes[[i]]<-as.vector(pam_res$clusinfo[,1])
  list_av_diss[[i]]<-as.vector(pam_res$clusinfo[,3])
}

#---------------------------------------------------------------
### Set names for excel file ###

# we get the names of the rows
df_names<-row.names(df)

find_indexes<-function(decade, cluster_label){
  
  indexes<-as.vector(which(cluster_labels[,decade] %in% cluster_label))
  return (indexes)
}

######################### Functions for computing significant values #####################################

#these functions will compute the relevant values for each single cluster
#such as min value, max value, mean and standard deviation

find_min_value <-function(decade,indexes){
  
  min_value<-df[indexes[1],decade]
  
  if(length(indexes)>1){
  
      for (i in 2:length(indexes)){
    
        if(df[indexes[i],decade]<min_value){
        min_value<-df[indexes[i],decade]
        }
    
      }
  }
  return (min_value)
}

find_max_value <-function(decade,indexes){
  
  max_value<-df[indexes[1],decade]
  
  if(length(indexes)>1){
  
    for (i in 2:length(indexes)){
    
       if(df[indexes[i],decade]>max_value){
       max_value<-df[indexes[i],decade]
       }
    }
  }
  return (max_value)
}

find_mean_value <- function(decade, indexes){
  cluster<-df[indexes, decade]
  return (mean(cluster))
}

find_sd_value <- function (decade, indexes){
  cluster<-df[indexes, decade]
  std_value=0
  if(length(indexes)>1){
    std_value=sd(cluster)
  }
  return (std_value)
}


##################################   We print some results   ############################################

M=matrix(data=NA,nrow=n_decade*2,ncol = Kmax) #matrix that will be printed in the excel file

Y=c()    #vector with the names of the decades

# we build an excel file where if the cluster exists then we compute mean value, min, max and sd, 
# otherwise we put a blank space

for (i in 1:n_decade){
  Y[2*i]=sprintf("Year %d", 1890+10*i)   #string such as "Year 1920"
  
  for (j in 1: Kmax){       #iterate up the max number of clusters (even if I have only few clusters)
    if (j<=length(list_medoids[[i]])){      #print info of the cluster only if it does exist
      indexes_decade_i_label_j<-find_indexes(i,j)
      
      min_val<-find_min_value(i,indexes_decade_i_label_j)
      max_val<-find_max_value(i,indexes_decade_i_label_j)
      mean_val<-find_mean_value(i,indexes_decade_i_label_j)
      sd_val<-find_sd_value(i,indexes_decade_i_label_j)
      
      #newvalues and newnames are strings. thew will be the content of the cells 
      newvalues<-sprintf("Cluster %d Mean(%f) Std(%f) Min (%f) Max (%f) Count(%d)",   
                         j,mean_val,sd_val,min_val,max_val,list_sizes[[i]][j])     #characteristcs of a new cluster
      
      newnames=paste(df_names[indexes_decade_i_label_j], collapse=", ") #components (adj or occu) of a new cluster
      
    }
    else{            #"white" cell if the cluster does not exist
      newvalues=""    
      newnames=""
    }
    
    #each column represents the j-th cluster (among all the decades)
    M[2*i-1,j]=newvalues   #upper cell: characteristics of the cluster (or white cell)
    M[2*i,j]=newnames      #lower cell: components (adj or occu) of the cluster (or white cell)
    
  }
  
  #cat ("anno", i, "terminato", "\n")  per vedere se completa il ciclo
}

M_final=cbind(Y,M)   #merge Y and M


list_to_print <- list(M_final,MM)
names(list_to_print)<-c("Clusters","Results")

#write in excel file
write.xlsx(
  list_to_print,
  file,
  #sheetName = "Clusters",
  col.names = TRUE,
  row.names = FALSE,
  append = TRUE,
  showNA = TRUE,
  password = NULL
)



############# Let's visualize the cluster estimates ##################


data_plot<-as.data.frame(df[,1])
data_plot$labels<-as.factor(cluster_labels[,1])
gg_plot_occu_1900 <- ggplot(data_plot, aes(x = labels, y = df[,1])) + 
  geom_boxplot(width=0.5, color="black", alpha=0.6) + 
  ggtitle("Gender bias occu 1900 (PAM)") +geom_jitter(color= "red",width=0.15)+geom_hline(yintercept=0, color= "grey")

data_plot<-as.data.frame(df[,6])
data_plot$labels<-as.factor(cluster_labels[,6])
gg_plot_occu_1950 <- ggplot(data_plot, aes(x = labels, y = df[,6])) + 
  geom_boxplot(width=0.5, color="black", alpha=0.6) + 
  ggtitle("Gender bias occu 1950 (PAM)") +geom_jitter(color= "red",width=0.15)+geom_hline(yintercept=0, color= "grey")

data_plot<-as.data.frame(df[,11])
data_plot$labels<-as.factor(cluster_labels[,11])
gg_plot_occu_2000 <- ggplot(data_plot, aes(x = labels, y = df[,11])) + 
  geom_boxplot(width=0.5, color="black", alpha=0.6) + 
  ggtitle("Gender bias occu 2000 (PAM)") +geom_jitter(color= "red",width=0.15)+geom_hline(yintercept=0, color= "grey")

x11()
require(gridExtra)
gg_plot_occu_1900
gg_plot_occu_1950
gg_plot_occu_2000
grid.arrange(gg_plot_occu_1900, gg_plot_occu_1950, gg_plot_occu_2000,  ncol=3)


####################### Co-clustering represetations  ###########################################

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



### Now we compute those matrix and we plot them ###

library(fields)

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

# Here we compared decades 1,6,11 (which correspond to 1900, 1950, 2000) but one can choose whatever he/she likes

x11()
par(mfrow=c(1,3))

dec = 1 #choose a decade

co_clust_matrix = co_clust(cluster_labels[,dec])
mat_labels = matrix_labels(cluster_labels[,dec])

i=max(cluster_labels[,dec])

colors=cols[1:(i+1)]


image.plot(1:nrow(mat_labels), 1:ncol(mat_labels), t(apply(mat_labels, 2, rev)), col=colors, xlab= "", ylab="")
title(main =paste("Clustering structure of the observations with PAM in 1900"), font.main = 1)


dec = 6 #choose a decade

co_clust_matrix = co_clust(cluster_labels[,dec])
mat_labels = matrix_labels(cluster_labels[,dec])

i=max(cluster_labels[,dec])

colors=cols[1:(i+1)]

image.plot(1:nrow(mat_labels), 1:ncol(mat_labels), t(apply(mat_labels, 2, rev)), col=colors, xlab= "", ylab="")
title(main =paste("Clustering structure of the observations with PAM in 1950"), font.main = 1)



dec = 11 #choose a decade

co_clust_matrix = co_clust(cluster_labels[,dec])
mat_labels = matrix_labels(cluster_labels[,dec])

i=max(cluster_labels[,dec])

colors=cols[1:(i+1)]

image.plot(1:nrow(mat_labels), 1:ncol(mat_labels), t(apply(mat_labels, 2, rev)), col=colors, xlab= "", ylab="")
title(main =paste("Clustering structure of the observations with PAM in 2000"), font.main = 1)

