########################################################
#################### ARDP1 ANALYSIS ####################
########################################################


################## Install packages ###########################
library(BNPmix)
library(cluster)
library(clusterCrit)
library(factoextra)
library("reshape2")
library("ggplot2")
library('plot.matrix')

################# Load Data ###################################
setwd("C:/Users/Lorenzo/Desktop/Analisi output codice")
load("occu_jitter_names.RData") #carichiamo i dati con i nomi delle occupazioni
data_decades=as.matrix(occujitt)  #rows=occupations, columns=decades

n_obs<-dim(occujitt)[1] #number of observations
n_decades<-11            #decades

# load cluster estimates from the previous version of the paper
load("estclusters_occupation.RData") 
old_cluster_estimates<-as.matrix(estclusters_occu)

col_names<-c(1:n_obs)


#################### Cluster estimates ##############################
# From the output of the ARDP1 code we read "t_i.txt" files for each decade in which we can find the clusters labels for each datapoint
# We do cluster estimate with variation inference and we also tried binder loss for curiosity.
# At the end we considered V.I. cluster estimates per decade

setwd("C:/Users/Lorenzo/Desktop/Analisi output codice/result_prova/result/decades_txt")

decades=list()
decades_matr = list() #import data as list of matrices
decades_output=list()
decades_output_binder=list()
new_cluster_estimates<-matrix(nrow=n_decades, ncol=length(col_names))
decades_commons=list()   #only the cluster with more than two obs (list of vectors of booleans)
decades_commons_binder=list()

take_only_common <- function(vect_with_labels){
  cc=vect_with_labels
  ccu=unique(cc)
  vec=rep(TRUE, n_obs)
  for (x in ccu){
    num=0
    for (i in cc){
      if (x==i){
        num=num+1;
      }
    }
    if (num<3 & num>0){
      for (j in 1:length(cc)){
        if (cc[j]==x){
          vec[j]=FALSE
        }
      }    
    }
  }
  return (vec)
}

for (i in 1:n_decades){
  
  message("decade ",i)
  decades[[i]]<-read.table(paste("t_",i-1,".txt",sep="" ), header=FALSE)
  decades_matr[[i]]<-as.matrix(read.table(paste("t_",i-1,".txt",sep="" ), header=FALSE, ))
  colnames(decades[[i]])<-col_names
  temp <- list(clust = as.matrix(decades[[i]]))
  class(temp) <- "BNPdens"
  
  #### using V.I.
  decades_output[[i]] <- partition(temp)  
  print(decades_output[[i]]$partitions[3,])
  #print(sort(unique(decades_output[[i]]$partitions[3,])))
  print(length(unique(decades_output[[i]]$partitions[3,])))
  new_cluster_estimates[i,] <-decades_output[[i]]$partitions[3,]
  
  #### take only the clusters with more than 2 obs (V.I.)
  decades_commons[[i]]=take_only_common(decades_output[[i]]$partitions[3,])
  #print(decades_output[[i]]$partitions[3,][decades_commons[[i]]])
  #print(sort(unique(decades_output[[i]]$partitions[3,])[decades_commons[[i]]]))
  print(length(unique(decades_output[[i]]$partitions[3,][decades_commons[[i]]])))
  
  #### using binder
  decades_output_binder[[i]] <- partition(temp, dist="Binder")  
  # print(decades_output_binder[[i]]$partitions[3,])
  sort(unique(decades_output_binder[[i]]$partitions[3,]))
  length(unique(decades_output_binder[[i]]$partitions[3,]))
  
  #### take only the clusters with more than 2 obs (binder)
  decades_commons_binder[[i]]=take_only_common(decades_output_binder[[i]]$partitions[3,])
  # print(decades_output_binder[[i]]$partitions[3,][decades_commons_binder[[i]]])
  # print(sort(unique(decades_output_binder[[i]]$partitions[3,])[decades_commons_binder[[i]]]))
  # print(length(unique(decades_output_binder[[i]]$partitions[3,][decades_commons_binder[[i]]])))
  
  message("############ \n")
}

#### BoxPlots ####

# boxplot representations for decades 1900, 1950, 2000
# change data_plot$labels lines if you want to plot binder partitions

#1900
data_plot<-as.data.frame(data_decades[,1])
data_plot$labels<-as.factor(decades_output[[1]]$partitions[3,])       #as.factor(decades_output_binder[[1]]$partitions[3,])
gg_plot_occu_1900 <- ggplot(data_plot, aes(x = labels, y = data_decades[,1])) + 
  geom_boxplot(width=0.5, color="black", alpha=0.6) + 
  ggtitle("Gender bias occu 1900 (AR1DP)") +geom_jitter(color= "red",width=0.15)+geom_hline(yintercept=0, color= "grey")

com1=decades_commons[[1]]  #vector of booleans (TRUE if the cluster has more than 2 obs)
#com1=decades_commons_binder[[1]]
data_plot<-as.data.frame(data_decades[,1][com1])
data_plot$labels<-as.factor(decades_output[[1]]$partitions[3,][com1])       #as.factor(decades_output_binder[[1]]$partitions[3,])
gg_plot_occu_1900_c <- ggplot(data_plot, aes(x = labels, y = data_decades[,1][com1])) + 
  geom_boxplot(width=0.5, color="black", alpha=0.6) + 
  ggtitle("Gender bias occu 1900 (AR1DP) commons") +geom_jitter(color= "red",width=0.15)+geom_hline(yintercept=0, color= "grey")

#1950
data_plot<-as.data.frame(data_decades[,6])   
data_plot$labels<-as.factor(decades_output[[6]]$partitions[3,])       #as.factor(decades_output_binder[[6]]$partitions[3,])
gg_plot_occu_1950 <- ggplot(data_plot, aes(x = labels, y = data_decades[,6])) + 
  geom_boxplot(width=0.5, color="black", alpha=0.6) + 
  ggtitle("Gender bias occu 1950 (AR1DP)") +geom_jitter(color= "red",width=0.15)+geom_hline(yintercept=0, color= "grey")

com6=decades_commons[[6]]     #vector of booleans (TRUE if the cluster has more than 2 obs)
#com6=decades_commons_binder[[6]]
data_plot<-as.data.frame(data_decades[,6][com6])
data_plot$labels<-as.factor(decades_output[[6]]$partitions[3,][com6])       #as.factor(decades_output_binder[[6]]$partitions[3,])
gg_plot_occu_1950_c <- ggplot(data_plot, aes(x = labels, y = data_decades[,6][com6])) + 
  geom_boxplot(width=0.5, color="black", alpha=0.6) + 
  ggtitle("Gender bias occu 1950 (AR1DP) commons") +geom_jitter(color= "red",width=0.15)+geom_hline(yintercept=0, color= "grey")

#2000
data_plot<-as.data.frame(data_decades[,11])
data_plot$labels<-as.factor(decades_output[[11]]$partitions[3,])      ##as.factor(decades_output_binder[[11]]$partitions[3,])
gg_plot_occu_2000 <- ggplot(data_plot, aes(x = labels, y = data_decades[,11])) + 
  geom_boxplot(width=0.5, color="black", alpha=0.6) + 
  ggtitle("Gender bias occu 2000 (AR1DP)") +geom_jitter(color= "red",width=0.15)+geom_hline(yintercept=0, color= "grey")

com11=decades_commons[[11]]    #vector of booleans (TRUE if the cluster has more than 2 obs)
#com11=decades_commons_binder[[11]]
data_plot<-as.data.frame(data_decades[,11][com11])
data_plot$labels<-as.factor(decades_output[[11]]$partitions[3,][com11])       #as.factor(decades_output_binder[[11]]$partitions[3,])
gg_plot_occu_2000_c <- ggplot(data_plot, aes(x = labels, y = data_decades[,11][com11])) + 
  geom_boxplot(width=0.5, color="black", alpha=0.6) + 
  ggtitle("Gender bias occu 2000 (AR1DP) commons") +geom_jitter(color= "red",width=0.15)+geom_hline(yintercept=0, color= "grey")


x11()
require(gridExtra)
#gg_plot_occu_1900
gg_plot_occu_1900_c
#gg_plot_occu_1950
gg_plot_occu_1950_c
#gg_plot_occu_2000
gg_plot_occu_2000_c
#grid.arrange(gg_plot_occu_1900, gg_plot_occu_1950, gg_plot_occu_2000,  ncol=3)
grid.arrange(gg_plot_occu_1900_c, gg_plot_occu_1950_c, gg_plot_occu_2000_c,  ncol=3)


#### Posterior Similarity Matrix #####

## co_clust--> given a set of labels returns a matrix whose entries (i,j) are 1 if observation i and 
#observation j are clustered together


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


count_matrix<-matrix(0, nrow=n_obs, ncol=n_obs)

##choose a decade
dec=7
n_sample = nrow(decades[[dec]])

for(k in 1:n_sample){
  
  count_matrix = count_matrix + co_clust(as.matrix((decades[[dec]][k,])))
  
}

freq_matrix= count_matrix/n_sample


x11()
class(freq_matrix)
par(mar=c(5.1, 4.1, 4.1, 4.1)) # adapt margins
plot(freq_matrix, col=heat.colors(5), main=paste("Posterior similarity matrix for occupations in", 1900+10*(dec-1), sep=" "), xlab="", ylab="")




####################### Traceplot Analysis ######################################
setwd("C:/Users/Lorenzo/Desktop/Analisi output codice/result_prova/result")
m_result <- as.matrix(read.table("m_result.txt", header=FALSE))
psi_result <- as.matrix(read.table("psi_result.txt", header=FALSE))

x11()
plot(ts(t(psi_result)))
title("Traceplot of psi")
x11()
plot(ts(t(m_result)))
title("Traceplot of M")

n_cluster_1900 <-c();
n_cluster_1950 <-c();
n_cluster_2000 <-c();


#number of clusters

for(j in 1:n_sample){
  
  n_cluster_1900[j] <- length(unique(decades_matr[[1]][j,]))
  n_cluster_1950[j] <- length(unique(decades_matr[[6]][j,]))
  n_cluster_2000[j] <- length(unique(decades_matr[[11]][j,]))
}



x11()
plot(ts(n_cluster_1900))
title("Number of clusters in 1900")
x11()
plot(ts(n_cluster_1950))
title("Number of clusters in 1950")
x11()
plot(ts(n_cluster_2000))
title("Number of clusters in 2000")

#posterior means and variances of the number of clusters
post_mean_n_clust_1900<-mean(n_cluster_1900)
post_var_n_clust_1950<-var(n_cluster_1950)
post_mean_n_clust_1900
post_var_n_clust_1950

post_mean_n_clust_1950<-mean(n_cluster_1950)
post_var_n_clust_1950<-var(n_cluster_1950)
post_mean_n_clust_1950
post_var_n_clust_1950

post_mean_n_clust_2000<-mean(n_cluster_2000)
post_var_n_clust_2000<-var(n_cluster_2000)
post_mean_n_clust_2000
post_var_n_clust_2000

#histograms
x11()
par(mfrow=c(1,3))
hist(n_cluster_1900, breaks=8, prob=FALSE)
lines(density(n_cluster_1900), col = "red")
hist(n_cluster_1950, breaks=8, prob=FALSE)
lines(density(n_cluster_1950), col = "red")
hist(n_cluster_2000, breaks=8, prob=FALSE)
lines(density(n_cluster_2000), col = "red")




######################## Autocorrelation #############################################

x11()
acf(psi_result[1,], plot=TRUE)
x11()
acf(m_result[1,], plot=TRUE)





######################## clustering methods used to compare ###############################
#clustering methods
method_names=c("K2","K3","K4","K5","PAM","AR1DP_old","AR1DP_new")

#decades
decade_names=c("t0","t1","t2","t3","t4","t5","t6","t7","t8","t9","t10")


#--------------------------------------------------------------------------------------------------
######################################
### Internal clustering validation ###
######################################

# per ogni epoca calcoliamo alcuni indici significativi per capire
# se il clustering che stiamo facendo è buono

getCriteriaNames(FALSE) #nomi degli indici più importanti per paragonare modelli di clustering

#--------------------SILHOUETTE INDEX -----------------------#

# se s->1 allora l'osservazione i-esima è clusterizzata bene
# se s->0 allora l'osservazione si trova tra due clusters
# se s<0 allora l'osservazione i-esima è clusterizzata male

# to sum up we want to maximize the avarage silhouette index per decade.

#-------------------- DUNN INDEX ----------------------------#

#If the data set contains compact and well-separated clusters, the diameter of the
#clusters is expected to be small and the distance between the clusters is expected to
#be large. 

#Thus, Dunn index should be maximized.

#-----------------------------------------------------------------------
### Initialization

# number of clusters to evaluate
k_min=2
k_max=5
num_clusters=c(k_min:k_max)

#construct the matrices
silhouette_index_matrix<-matrix(ncol=n_decades, nrow=length(method_names))
dunn_index_matrix<-matrix(ncol=n_decades, nrow=length(method_names))
colnames(silhouette_index_matrix)<-colnames(dunn_index_matrix)<-decade_names
rownames(silhouette_index_matrix)<-rownames(dunn_index_matrix)<-method_names


###################### k-means with k=2:5 (first 4 rows) ######################
current_row=1    #iterator for both dunn and silhouette rows

sil_indeces<-c()
dunn_indeces<-c()

for(k in 1:length(num_clusters) ){                      #number of clusters
  for (i in 1:n_decades){                                #number of epoches
    km_res <- eclust(as.matrix(data_decades[,i]), "kmeans", num_clusters[k], nstart = 15,graph = FALSE )
    sil_info <- km_res$silinfo     
    sil_indeces[i]<- sil_info$avg.width                                                                 #Silhouette index
    dunn_indeces[i]<-intCriteria(matrix(data_decades[,i]),as.vector(km_res$cluster),"Dunn")[[1]]        #Dunn index
  }
  
  silhouette_index_matrix[current_row,]<-sil_indeces   #fill silhouette matrix
  dunn_index_matrix[current_row,]<-dunn_indeces        #fill dunn matrix
  current_row=current_row+1
}

###################### PAM (5-th row) ######################
current_row=5
K_max_PAM=10
sil_PAM<-c()
dunn_PAM<-c()

for (i in 1:n_decades){ 
  best_msil=0
  best_k=2
  pam=pam(data_decades[,i], 2, metric = "euclidean", stand=FALSE)    #just to initialize
  
  #compute pam clustering with different number of clusters, than choose the one with the best avg_sil_index
  for (k in 2:K_max_PAM){
    pam=pam(data_decades[,i], k, metric = "euclidean", stand=FALSE)       #pam with different k
    sil=silhouette(pam)      #compute silhoutte
    msil=mean(sil[,3])       #compute average silhouette coefficient (quantity to maximize)
    #print(msil)
    if (msil>best_msil){
      best_msil=msil         #update best average silhouette coefficient
      pam_res=pam            #use the model with the best k found
      best_k=k
    }
  }
  
  dunn_PAM[i]=intCriteria(matrix(data_decades[,i]),as.vector(pam_res$cluster),
                          "Dunn")[[1]]  #dunn index for the best avg_sil_index
  sil_PAM[i]=best_msil                  #silhouette index for the best avg_sil_index
}

dunn_index_matrix[current_row,]<-dunn_PAM        #fill dunn matrix
silhouette_index_matrix[current_row,]<-sil_PAM   #fill silhouette matrix   

###################### AR1DP estimates (6-th row and 7-th row) ######################

sil_AR1DP<-c()
dunn_AR1DP<-c()
markers<-c() #dummy variable indicating if we can perform standard silhouette computation (=1) or not (=0)

# we consider old and new cluster estimates: 
# if estimate_ardp==1 -> old cluster estimates
# if estimate_ardp==2 -> new cluster estimates
for (estimate_ardp in 1:2) {
  if (estimate_ardp==1) {
    cluster_dataset = old_cluster_estimates
  }
  else{
    cluster_dataset = new_cluster_estimates
  }
for (i in 1:n_decades) {
  estimate<-as.vector(cluster_dataset[i,], mode="integer")
  markers[i]<-1
  if (length(unique(estimate))==1) {       #i.e. if all data have been assigned to the same cluster
    markers[i]<-0     
  }
}

for (i in 1:n_decades){                                #number of epoches
  if (markers[i] == 1) {
    estimate<-as.vector(cluster_dataset[i,], mode="integer")
    dunn_AR1DP[i]<-intCriteria(matrix(data_decades[,i]),estimate,"Dunn")[[1]]
    sil_coeffs<- silhouette(estimate, dist(data_decades[,i], "euclidean"))[,3]
    avg_sil<-mean(sil_coeffs)
    sil_AR1DP[i]<-avg_sil
  }
  else {
    sil_AR1DP[i]<-0
    dunn_AR1DP[i]<-0
  }
}

silhouette_index_matrix[current_row+estimate_ardp,] <- sil_AR1DP      #fill silhouette matrix
dunn_index_matrix[current_row+estimate_ardp,] <- dunn_AR1DP           #fill dunn matrix
}


###################### Visualize the results ######################
#print
print(silhouette_index_matrix)
print(dunn_index_matrix)

#plot: silhouette
dsil=data.frame(t(silhouette_index_matrix))
dsil_new=cbind(dsil,decades=0:10)
data_long_sil <- melt(dsil_new, id="decades",value.name = "Silhouette", variable.name = "method")   #format needed by ggplot

x11()
ggplot(data=data_long_sil,
       aes(x=decades, y=Silhouette, shape=method, colour=method)) +
  geom_line()+
  geom_point()+
  scale_x_continuous(breaks=0:10) +
  ggtitle("Silhouette index according to decades")

#plot: dunn
ddunn=data.frame(t(dunn_index_matrix))
ddunn_new=cbind(ddunn,decades=0:10)
data_long_dunn <- melt(ddunn_new, id="decades",value.name = "Dunn", variable.name = "method")   #format needed by ggplot

x11()
ggplot(data=data_long_dunn,
       aes(x=decades, y=Dunn, shape=method, colour=method)) +
  geom_line()+
  geom_point()+
  scale_x_continuous(breaks=0:10)+
  ggtitle("Dunn index according to decades")



######################################
### External clustering validation ###
######################################
# per ogni epoca guardiamo qual è il miglior numero di clusters tra 2 e 5

getCriteriaNames(FALSE) #nomi degli indici più importanti per paragonare modelli di clustering

############################################################

#### JACCARD ####

# The Jaccard coefficient measures similarities between different partitions into clusters.
# Jaccard index supports partitions with smaller number of clusters.

# se jacc->1 allora le due partizioni in clusters che paragono sono molto simili tra loro
# se jacc->0 allora le due partizioni in clusters che paragono sono molto diverse tra loro


#### RAND ####

# The Rand coefficient measures similarities between different partitions into clusters.
# Rand Index tends to be indifferent to the number of clusters.

# se rand->1 allora le due partizioni in clusters che paragono sono molto simili tra loro
# se rand->0 allora le due partizioni in clusters che paragono sono molto diverse tra loro


#### Fowlkes_Mallows ####

# The Folkes_Mallows coefficient measures similarities between different partitions into clusters.
# One basic way to test the validity of this index is to compare two clusterings that are unrelated to each other. 
# The index also showed similarity even when the noisy dataset had a different number of clusters than the clusters 
# of the original dataset. 
# Thus making it a reliable tool for measuring similarity between two clusters.

# se folkes->1 allora le due partizioni in clusters che paragono sono molto simili tra loro
# se folkes->0 allora le due partizioni in clusters che paragono sono molto diverse tra loro

############################################################

# initialize the vector that will contain the indeces for each epoch

n=length(num_clusters)
couples_of_k<-matrix(nrow=n*(n-1)/2,ncol=2)
ind=1
for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    couples_of_k[ind,1]=num_clusters[i]
    couples_of_k[ind,2]=num_clusters[j]
    ind=ind+1
  }
}

jaccard<-c()
rand<-c()
folkes_mallows<-c()
cluster_labels_1<- matrix(ncol=n_decades, nrow=n_obs)
cluster_labels_2<- matrix(ncol=n_decades, nrow=n_obs)
result<-c()



###########Validation Analysis k-means using different k's###

for (j in 1:(dim(couples_of_k)[1])) { #at each iteration I consider a different couple
  for (i in 1:n_decades){ #itero per ogni epoca
    
    km_res_1 <- kmeans(data_decades[,i], couples_of_k[j,1], nstart = 15)       #k-means with the first value of k
    km_res_2 <- kmeans(data_decades[,i], couples_of_k[j,2], nstart = 15)       #k-means  with the second value of k
    
    cluster_labels_1[,i]<-as.vector(km_res_1$cluster)                     #cluster estimate 1
    cluster_labels_2[,i]<-as.vector(km_res_2$cluster)                     #cluster estimate 2
    
    jaccard[i] <- extCriteria(cluster_labels_1[,i],cluster_labels_2[,i],"jaccard") #{couples clustered together by both methods} / #{couples clustered together and not by both methods, without repetitions}
    rand[i] <- extCriteria(cluster_labels_1[,i],cluster_labels_2[,i],"rand") # [ #{couples clustered together by both methods} + #{couples not clustered together by both methods} ] / choose (n, 2)
    folkes_mallows[i] <- extCriteria(cluster_labels_1[,i],cluster_labels_2[,i],"Folkes_Mallows") # sqrt(PPV * TPR)
    # TP = #{couples clustered together by both methods}
    # FP = #{couples clustered together by method 1 but not together by method 2}
    # FN = #{couples clustered not together by method 1 but together by method 2}
    # TN = #{couples clustered not together by both methods}
    # PPV = positive predictive rate = TP / (TP + FP)
    # TPR = true positive rate = TP / (TP + FN)
    
  }
  cat("Comparison between cluster estimates with k=",couples_of_k[j,1], "and k=",couples_of_k[j,2])
  result<-cbind(jaccard,rand,folkes_mallows)
  rownames(result)<-decade_names
  cat("\n")
  print(result)
  cat("\n")
  cat("\n")
  
}

### k-means (with some k) vs PAM ###

K_max_PAM=10
PAM_labels<-matrix(ncol=n_decades, nrow=n_obs)

for (i in 1:n_decades){ 
  best_msil=0
  best_k=2
  pam=pam(data_decades[,i], 2, metric = "euclidean", stand=FALSE)    #just to initialize
  
  #compute pam clustering with different number of clusters, than choose the one with the best avg_sil_index
  for (k in 2:K_max_PAM){
    pam=pam(data_decades[,i], k, metric = "euclidean", stand=FALSE)       #pam with different k
    sil=silhouette(pam)      #compute silhoutte
    msil=mean(sil[,3])       #compute average silhouette coefficient (quantity to maximize)
    
    if (msil>best_msil){
      best_msil=msil         #update best average silhouette coefficient
      pam_res=pam            #use the model with the best k found
      best_k=k
    }
  }
  PAM_labels[,i]<-as.vector(pam_res$cluster)
}


KoI <- c(2,5) #comparison among just a few values of k with PAM
cluster_labels<-matrix(ncol=n_decades, nrow=n_obs)

for (j in 1:length(KoI)) { #k of interest
  for (i in 1:n_decades){ #itero per ogni epoca
    
    km_res <- kmeans(data_decades[,i], KoI[j], nstart = 15)       #k-means with current value of k
    cluster_labels[,i]<-as.vector(km_res$cluster) 
    
    jaccard[i] <- extCriteria(cluster_labels[,i],PAM_labels[,i],"jaccard")
    rand[i] <- extCriteria(cluster_labels[,i],PAM_labels[,i],"rand")
    folkes_mallows[i] <- extCriteria(cluster_labels[,i],PAM_labels[,i],"Folkes_Mallows")
    
  }
  
  cat("Comparison between k-mean with k=",KoI[j], " and Pam")
  result<-cbind(jaccard,rand,folkes_mallows)
  rownames(result)<-decade_names
  cat("\n")
  print(result)
  cat("\n")
  cat("\n")
}

##########################################################################
### Validation analysis with AR1DP estimates

### k-means (with some k) vs AR1DP estimates


KoI <- c(2,5) #comparison among just a few values of k
cluster_labels<-matrix(ncol=n_decades, nrow=n_obs)

for (j in 1:length(KoI)) { #k of interest
  for (i in 1:n_decades){ #itero per ogni epoca
    
    estimate<-as.vector(new_cluster_estimates[i,], mode="integer")
    
    km_res <- kmeans(data_decades[,i], KoI[j], nstart = 15)       #k-means with current value of k
    cluster_labels[,i]<-as.vector(km_res$cluster) 
    
    jaccard[i] <- extCriteria(cluster_labels[,i],estimate,"jaccard")
    rand[i] <- extCriteria(cluster_labels[,i],estimate,"rand")
    folkes_mallows[i] <- extCriteria(cluster_labels[,i],estimate,"Folkes_Mallows")
    
  }
  
  cat("Comparison between k-mean with k=",KoI[j], "and AR1DP estimate")
  result<-cbind(jaccard,rand,folkes_mallows)
  rownames(result)<-decade_names
  cat("\n")
  print(result)
  cat("\n")
  cat("\n")
}

### PAM vs AR1DP estimate
K_max_PAM=10
PAM_labels<-matrix(ncol=n_decades, nrow=n_obs)

for (i in 1:n_decades){ 
  best_msil=0
  best_k=2
  pam=pam(data_decades[,i], 2, metric = "euclidean", stand=FALSE)    #just to initialize
  
  #compute pam clustering with different number of clusters, than choose the one with the best avg_sil_index
  for (k in 2:K_max_PAM){
    pam=pam(data_decades[,i], k, metric = "euclidean", stand=FALSE)       #pam with different k
    sil=silhouette(pam)      #compute silhoutte
    msil=mean(sil[,3])       #compute average silhouette coefficient (quantity to maximize)
    
    if (msil>best_msil){
      best_msil=msil         #update best average silhouette coefficient
      pam_res=pam            #use the model with the best k found
      best_k=k
    }
  }
  PAM_labels[,i]<-as.vector(pam_res$cluster)
  estimate<-as.vector(new_cluster_estimates[i,], mode="integer")
  
  jaccard[i] <- extCriteria(PAM_labels[,i],estimate,"jaccard")
  rand[i] <- extCriteria(PAM_labels[,i],estimate,"rand")
  folkes_mallows[i] <- extCriteria(PAM_labels[,i],estimate,"Folkes_Mallows")
  
}

cat("Comparison between PAM and AR1DP estimate")
result<-cbind(jaccard,rand,folkes_mallows)
rownames(result)<-decade_names
cat("\n")
print(result)
cat("\n")
cat("\n")


################### Comparison between old and new ARDP1 #############################

for (i in 1:n_decades) {
  old_estimate<-as.vector(old_cluster_estimates[i,], mode="integer")
  new_estimate<-as.vector(new_cluster_estimates[i,], mode="integer")
  jaccard[i] <- extCriteria(old_estimate,new_estimate,"jaccard")
  rand[i] <- extCriteria(old_estimate,new_estimate,"rand")
  folkes_mallows[i] <- extCriteria(old_estimate,new_estimate,"Folkes_Mallows")
  
}

cat("Comparison between old and new ARDP1 estimates")
result<-cbind(jaccard,rand,folkes_mallows)
rownames(result)<-decade_names
cat("\n")
print(result)
cat("\n")
cat("\n")

