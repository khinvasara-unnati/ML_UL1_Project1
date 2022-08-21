
#Importing required libraries 
library(dplyr)
library(cluster)
library(ggplot2)
library(tidyverse)
library(dendextend)
library(factoextra)
library(tidyr)

#Reading Base Data File 
data <- read.csv('F:\\ISB\\2 - Term 2 - (18-22 June) - H\\ML - Unsupervised 1\\MLUL - Assignment 1 - 9 July\\EastWestAirlinesCluster.csv')

#Checking the data 
dim(data)
head(data)
str(data)
summary(data)


#Data Transformation

## By inspecting the Raw Data, we notice that "Bonus Miles & Bonus Transactions" are related features
  
  ## Individually, high no. of bonus transactions does not mean high bonus miles & vice versa 
  ## So, to understand these variables better together, we create a new variable "Bonus Transaction Ratio" 
  ## This variable will let us know the no. of bonus miles accumulated per bonus transaction
  
    ### Same is done for "Flight Miles & Flight Flight Transactions"

ratio_data <- mutate(data, bonus_trans_ratio = Bonus_miles/Bonus_trans, flight_trans_ratio = Flight_miles_12mo/Flight_trans_12)
head(ratio_data)

#Handling Missing Values 
  ## Replacing NaN values with 0
ratio_data[is.na(ratio_data)]=0
head(ratio_data)

#Checking CC Miles variable
table(data$cc1_miles)
table(data$cc2_miles)
table(data$cc3_miles)
## We note that CCMiles2 and CCMiles3 is a highly skewed data with majority values (99%) falling in category 1.
##Further, similar trend is notices in CCMiles 1 with more than 70% values in category 1


#Feature Deletion
  ##We remove the unnecessary variables which wont be used for clustering 
ratio_data <- subset(ratio_data, select = -c(ID.,Award.,cc1_miles, cc2_miles, cc3_miles)) #Removing these variables because ID and Awards is unique / cateogrical data. Further CC Miles is also ordinal and as seen above, highly skewed. So we remove the same. 
ratio_data <- subset(ratio_data, select = -c(Bonus_miles, Bonus_trans, Flight_miles_12mo, Flight_trans_12)) #Removing these variables since new variable has been created
str(ratio_data)
#We will use these remaining 5 variables for clustering purpose 


#Data Standardization
std_ratio_data <- scale(ratio_data)

##Data Normalisation
##Note - We havent used this scaling method since analysis didnt conclude meaningful segments 
#min_max_norm <- function(x) {(x - min(x)) / (max(x) - min(x))}
#std_ratio_data <- as.data.frame(lapply(ratio_data, min_max_norm))

#---------------------------------------------------------------------------------------------------------------------
  
#HIERARCHIAL CLUSTERING

#Plotting Dendrogram
d <- dist(std_ratio_data, method = "euclidean")
fit <- hclust(d, method = "ward.D2")
plot(fit, main = "Hierarchial Clustering Dendrogram", labels = FALSE, hang=-1)

#Visually inspect the Dendrogram and decide optimal clusters based on vertical distance 
rect.hclust(fit, k=6, border="red")

#Visualizing the Clusters
groups <- cutree(fit,k=6) 
fviz_cluster(list(data=std_ratio_data, cluster = groups), labelsize = 1, ellipse.alpha=0)

#Making the cluster membership and exporting csv
hmembership<-as.matrix(groups)
table(hmembership)
#cluster1 <- subset(std_ratio_data,membership[,1]==1)

colnames(hmembership)[1] <- "Cluster ID"
hierch_data <- cbind(data,hmembership)
write.csv(hierch_data,file='F:\\ISB\\2 - Term 2 - (18-22 June) - H\\ML - Unsupervised 1\\MLUL - Assignment 1 - 9 July\\1 - hierarch_clust_membership.csv')

#Further cluster & segment analysis done in excel and tableau

#------------------------------------------------------------------------------------------------------------------------
  
#RANDOM SAMPLE DATASET 

#Obtaining a sample of 95% (3999*0.95 ~ 3800 observations)
set.seed(50)
random_ratio_data <- sample_n(ratio_data,3800)
head(random_ratio_data)
dim(random_ratio_data)

#Standardizing sample 
std_random_ratio_data <- scale(random_ratio_data)
head(std_random_ratio_data)

#Performing Hierarchial clustering on the random sample
d <- dist(std_random_ratio_data, method = "euclidean")
fit <- hclust(d, method = "ward.D2")
plot(fit, main = "Sample Hierarchial Clustering Dendrogram", labels = FALSE, hang=-1)
rect.hclust(fit, k=6, border="red")

#Visualising sample clusters
groups <- cutree(fit,k=6)
fviz_cluster(list(data=std_random_ratio_data, cluster = groups), labelsize = 1, ellipse.alpha=0)

#Exporting CSV with Cluster Membership
hrmembership<-as.matrix(groups)
colnames(hrmembership)[1] <- "Sample Cluster ID"
hierch_data <- cbind(random_ratio_data,hrmembership)
write.csv(hierch_data,file='F:\\ISB\\2 - Term 2 - (18-22 June) - H\\ML - Unsupervised 1\\MLUL - Assignment 1 - 9 July\\2 - sample_hierarch_clust_membership.csv')

#Further cluster & segment analysis done in excel and tableau

#---------------------------------------------------------------------------------------------------------
  

#K-MEANS CLUSTERING


#Checking optimal number of clusters   
  
##Silhouette Score & Elbow Plot
fviz_nbclust(std_ratio_data, kmeans, method = "silhouette", k.max=10) #Peaks at 2 which is very low cluster number. Next peak is at 6.
fviz_nbclust(std_ratio_data, kmeans, method = "wss") + geom_vline(xintercept = 6, linetype = 2) #Flattening of the curve can be seen at 6

#K-Means Clustering
fit <- kmeans(std_ratio_data, centers = 6)
#fit$size

#Visualising the clusters
fviz_cluster(fit, data = std_ratio_data, geom = c("point"), ellipse.alpha = 0)

kmembership <- cbind(ratio_data, fit$cluster)
aggregate(kmembership, by=list(fit$cluster), FUN=mean)

#Exporting Clusters
kmembership <- cbind(data, fit$cluster)
write.csv(kmembership,file='F:\\ISB\\2 - Term 2 - (18-22 June) - H\\ML - Unsupervised 1\\MLUL - Assignment 1 - 9 July\\3 - kmeans_data.csv')

#Further cluster & segment analysis done in excel and tableau

#=============================================================END============================================================