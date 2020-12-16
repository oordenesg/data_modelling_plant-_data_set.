

#############################################################################################
########################  Data modelling ###########################################
#############################################################################################

# Libraries 
library(dplyr)
library(fpc)
library(cluster)
library(lpSolve)
source("extra_functions.R")
############## 1. Choose an appropriate dataset and use hierarchical, k-means, and PAM.

# 0. Create the appropriate dataset.

data.clustering<-data  # original data 
data.clustering<-data.clustering[,-17] # Removing Leaf Weight variable 
data.clustering<-cbind(data.clustering, empty_values = rowSums(is.na(data.clustering)))  # Auxiliar column
data.clustering<-data.clustering[data.clustering$empty_values <=2,] # Removing all those instances with 3 o more missing values
data.clustering<-data.frame(data.clustering %>% mutate_each(funs(replace(., which(is.na(.)),mean(., na.rm=TRUE))))) # Replacing the missing values by the mean
data.clustering<-data.clustering[,-20] ##20 Removing the auxiliar column
data.clustering<-data.clustering %>% mutate_at(c(2:18), funs(c(scale(.)))) # Scale the data 

# 0. Distance matrix 
data.distance<-dist(data.clustering[,2:18]) #Distance matrix without considering the variable Sample_ID and Class

# 0. number of clusters
number_clusters<-length(unique(data$Class))

# 0. table metrics

metrics<-c("Accuracy","Precision","Recall","F1","Entropy","Diameter")
cluster.methods<-c("HC","PAM","K-Means")
results.3.clusters<-data.frame(matrix(nrow =length(cluster.methods) , ncol=length(metrics)))
colnames(results.3.clusters)<-metrics
rownames(results.3.clusters)<-cluster.methods


###### 1.1 Hierarchical clustering
hc<-hclust(dist(data.clustering[,2:18]))  
hc.clusters<-cutree(hc,number_clusters)

# Metrics
hc.metrics<-matrix.and.metrics(table(data.clustering$Class, hc.clusters))
results.3.clusters[1,1]<-hc.metrics[[2]] #Accuracy
results.3.clusters[1,2]<-hc.metrics[[3]] #Precision
results.3.clusters[1,3]<-hc.metrics[[4]] #Recall
results.3.clusters[1,4]<-hc.metrics[[5]] #F1
results.3.clusters[1,5]<-cluster.stats(data.distance, hc.clusters)$entropy  #Entropy
results.3.clusters[1,6]<-mean(cluster.stats(data.distance, hc.clusters)$diameter)  # Diameter

results.3.clusters


######  1.2 PAM 

PAM<-pam(data.clustering[,2:18], number_clusters, stand = TRUE)
# Metrics
pam.metrics<-matrix.and.metrics(table(data.clustering$Class, PAM$clustering))
results.3.clusters[2,1]<-pam.metrics[[2]] #Accuracy
results.3.clusters[2,2]<-pam.metrics[[3]] #Precision
results.3.clusters[2,3]<-pam.metrics[[4]] #Recall
results.3.clusters[2,4]<-pam.metrics[[5]] #F1
results.3.clusters[2,5]<-cluster.stats(data.distance, PAM$clustering)$entropy  #Entropy
results.3.clusters[2,6]<-mean(cluster.stats(data.distance, PAM$clustering)$diameter)  # Diameter

results.3.clusters


######  1.3 Kmeans
# The kmeans.algorithm function allows to run k-means n times.Parameters
# df: Dataframe
# n: Number of iterations
# from-to: Columns to select
kmeans.algorithm<-function(df,from,to,n){
  set.seed(1)
  results<-data.frame(matrix(nrow=n, ncol=length(metrics)))
  colnames(results)<-metrics
  data.distance<-dist(df[,from:to])
  for(i in 1:n){
    kms<-kmeans(df[,from:to],number_clusters,iter.max=100) #applies k-means with 3 clusters and 100 iterations
    kms.metrics<-matrix.and.metrics(table(df$Class, kms$cluster))
    results[i,1]<-kms.metrics[[2]]
    results[i,2]<-kms.metrics[[3]]
    results[i,3]<-kms.metrics[[4]]
    results[i,4]<-kms.metrics[[5]]
    results[i,5]<-cluster.stats(data.distance, kms$cluster)$entropy  
    results[i,6]<-mean(cluster.stats(data.distance, kms$cluster)$diameter, na.rm = TRUE)
    
  }
  return(colMeans(results))
}
kmeans.results<-kmeans.algorithm(data.clustering,2,18,50)
results.3.clusters[3,]<-kmeans.results

results.3.clusters  # Final table with the results


##############  2. Choose an appropriate dataset. Optimise each clustering method according to two parameters or more.

######  2.1 Optimisation HC clustering
#  The hc.optimization function allows to run the HC optimisation. Parameters
# distance: euclidean or manhattan distance
hc.optimisation<-function(distance){
  methods<-c("single","ward.D2","average","complete")
  results<-data.frame(matrix(nrow=length(methods),ncol = length(metrics)))
  colnames(results)<-metrics
  rownames(results)<-methods
  for(i in 1:length(methods)){
    dis.matrix<-dist(data.clustering[,2:18], method = distance)
    hc<-hclust(dis.matrix, method = methods[i])
    hc.clusters<-cutree(hc,number_clusters)
    hc.metrics<-matrix.and.metrics(table(data.clustering$Class, hc.clusters))
    results[i,1]<-hc.metrics[[2]]
    results[i,2]<-hc.metrics[[3]]
    results[i,3]<-hc.metrics[[4]]
    results[i,4]<-hc.metrics[[5]]
    results[i,5]<-cluster.stats(dis.matrix, hc.clusters)$entropy
    results[i,6]<-mean(cluster.stats(dis.matrix, hc.clusters)$diameter)
  }
  return(results)
}


hc.optimisation.euclidean<-hc.optimisation("euclidean")
hc.otpimisation.manhattan<-hc.optimisation("manhattan")

hc.optimisation.euclidean
hc.otpimisation.manhattan

######  2.2 Optimisation PAM
#  The pam.optimisation function allows to run the PAM optimisation. Parameters
# swap: TRUE or FALSE

pam.optimisation<-function(swap){
  methods<-c("manhattan","euclidean")
  results<-data.frame(matrix(nrow=length(methods),ncol = length(metrics)))
  colnames(results)<-metrics
  rownames(results)<-methods
  for(i in 1:length(methods)){
    dis.matrix<-dist(data.clustering[,2:18], method = methods[i])
    PAM<-pam(data.clustering[,2:18],number_clusters,stand = TRUE, do.swap = swap, metric = methods[i])
    PAM.metrics<-matrix.and.metrics(table(data.clustering$Class, PAM$clustering))
    results[i,1]<-PAM.metrics[[2]]
    results[i,2]<-PAM.metrics[[3]]
    results[i,3]<-PAM.metrics[[4]]
    results[i,4]<-PAM.metrics[[5]]
    results[i,5]<-cluster.stats(dis.matrix, PAM$clustering)$entropy
    results[i,6]<-mean(cluster.stats(dis.matrix, PAM$clustering)$diameter)
  }
  return(results)
}

pam.optimisation.swap<-pam.optimisation(TRUE)
pam.optimisation.no.swap<-pam.optimisation(FALSE)

pam.optimisation.swap
pam.optimisation.no.swap

# kmeans
#  The kmeans.optimisation function allows to run the kmeans optimisation. Parameters
# n: Number of times the algorithm is executed
# from-to-by: Range of number of centroids to evaluate. For example: Evaluate from 5 to 25 initial centroids between 5

kmeans.optimisation<-function(n,from,to,by){
  set.seed(1)
  metrics.2<-c("Centroids","Accuracy","Precision","Recall","F1","Entropy","Diameter")
  methods<-c("Lloyd","MacQueen")   # K-means's methods
  centroids<-rep(seq(from,to,by), each = n)
  results<-data.frame(matrix(nrow=length(centroids),ncol = length(metrics.2))) # number of centroids
  colnames(results)<-metrics.2
  results$Centroids<-centroids #Number of centroids from one to five
  results.list<-lapply(seq_len(length(methods)), function(X) results)# List with the result for each method
  names(results.list)<-methods
  for(i in 1:length(methods)){
    for(j in 1:nrow(results)){
      kms<-kmeans(data.clustering[,2:18],number_clusters,iter.max=100, algorithm = methods[i],nstart = results[j,1])
      kms.metrics<-matrix.and.metrics(table(data.clustering$Class,kms$cluster))
      results.list[[i]][j,2]<-kms.metrics[[2]] #Accuracy
      results.list[[i]][j,3]<-kms.metrics[[3]] #Precision
      results.list[[i]][j,4]<-kms.metrics[[4]] #Recall 
      results.list[[i]][j,5]<-kms.metrics[[5]] #F1
      results.list[[i]][j,6]<-cluster.stats(data.distance, kms$cluster)$entropy    #Entropy
      results.list[[i]][j,7]<-mean(cluster.stats(data.distance, kms$cluster)$diameter, na.rm = TRUE)
    }
  }
  return(lapply(X = results.list,FUN = function(x) {aggregate(x = x[,2:7], by = list(unique.values = x$Centroids), mean)}))
}

kmeans.optimisation.results<-kmeans.optimisation(50,5,25,5)
kmeans.optimisation.results



#1.3. Choose one clustering algorithm of the above and perform this clustering on these alternative datasets that you have produced as a result of Part 1
# Using the kmeans.algorithm function and the datasets created in Part 1

data.without.missing.values.uncorrelated.ST<-data.without.missing.values.uncorrelated %>% mutate_at(c(2:9), funs(c(scale(.))))

kmeans.10pcs<-kmeans.algorithm(input.all.pcs,1,10,50)
kmeans.replace.zero<-kmeans.algorithm(data.mvalues.0.ST,2,19,50)
kmeans.replace.mean<-kmeans.algorithm(data.mvalues.mean.ST,2,19,50) # OK
kmeans.replace.median<-kmeans.algorithm(data.mvalues.median.ST,2,19,50) # OK
kmeans.uncorrelated<-kmeans.algorithm(data.without.missing.values.uncorrelated.ST,2,9,50) # The dataset without missing values and uncorrelated variables
kmeans.after.deletion<-kmeans.algorithm(data.clustering,2,18,50)

kmeans.6sets<-rbind(kmeans.10pcs,
                    kmeans.replace.zero,
                    kmeans.replace.mean,
                    kmeans.replace.median,
                    kmeans.uncorrelated,
                    kmeans.after.deletion)


kmeans.6sets



############# DATA SETS WEKA #############################

getwd() # Find the data sets in the WD
write.csv(data.clustering[,2:19],"data_classification_appropiatedf.csv", row.names = FALSE)
write.csv(input.all.pcs[,c(1:10,18)],"10PCS.csv", row.names = FALSE)
write.csv(data.without.missing.values.uncorrelated.ST[,2:10],"uncorrelated.csv", row.names = FALSE)
write.csv(data.mvalues.0.ST[,2:20],"replaced_by_0.csv", row.names = FALSE)
write.csv(data.mvalues.mean.ST[,2:20],"replaced_by_mean.csv", row.names = FALSE)
write.csv(data.mvalues.median.ST[,2:20],"replaced_by_median.csv", row.names = FALSE)


############### EXTRA: The repeated values ################


dataset.repeated.values<-cbind(data, empty_values = rowSums(is.na(data)))
repeated.values<-dataset.repeated.values%>%
  group_by(Centroid.X,Centroid.Y)%>% # Using Centroid X and Centroid Y like an ID
  mutate(fake_id = paste(Centroid.X, Centroid.Y, sep = "",collapse = NULL))%>%
  arrange(fake_id)%>%
  group_by(fake_id)%>%
  mutate(rank = row_number())%>%
  arrange(rank, .by_group = TRUE)%>%
  filter(rank == 2)%>%
  select(Centroid.X, Centroid.Y)

View(repeated.values)
repeated.values<-data.frame(repeated.values[,2:3])
dataset.repeated.values[dataset.repeated.values$Centroid.X %in% c(repeated.values$Centroid.X), ] # repeated values


