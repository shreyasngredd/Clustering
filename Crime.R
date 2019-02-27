#####CLUSTERING#####

# Perform Clustering for the crime data and identify the number 
#of clusters formed and draw inferences.

#Load and Attach the data
cri<- read.csv(file.choose())
View(cri)

#Removing State columns before normalizing, 
#as we can't apply Z function to categorical data

library(dplyr)
crime<- select(cri, -X)
View(crime)
attach(crime)

#Summerize the data
summary(crime)
#Murder: Mean= 7.788, Median= 7.250; As Mean>Median,it is skewed to the left.
#Assult: Mean= 170.8, Median= 159.0; As Mean>Median,it is skewed to the left.
#Urbanpop: Mean= 65.54, Median= 66.0; As Mean<Median,it is skewed to the right.
#Rape: Mean= 21.23, Median= 20.10; As Mean>Median,it is skewed to the left.

library(DataExplorer)
plot_str(crime)
str(crime)
plot_missing(crime)

dev.new(width=5, height=4)
plot_histogram(crime)
plot_density(crime)

#Normalizing data
normal_crime<-scale(crime)

#Distance Matrix Computation
d<-dist(normal_crime,method="euclidean") 
d

#Hierarchical cluster
fit <- hclust(d, method="complete")

#Display dendrogram
plot(fit)
plot(fit, hang=-1)

#Cutting tree into five clusters
groups<-cutree(fit,k=5)
groups

rect.hclust(fit,k=5,border="red")

membership<-as.matrix(groups)

final<-data.frame(cri,membership)
View(final)

aggregate(crime,by= list(final$membership),FUN = mean)

#Writing data into a xlsx file
library(xlsx)
write.xlsx(final,file="final_crime.xlsx")

##K-means Clustering

#elbow curve & k ~ sqrt(n/2) to decide the k value

# Determine number of clusters by scree-plot
wss = (nrow(normal_crime)-1)*sum(apply(normal_crime, 2, var))
for (i in 1:4) wss[i] = sum(kmeans(normal_crime, centers=i)$withinss)

# Look for an "elbow" in the scree plot
plot(1:4, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
title(sub = "K-Means Clustering Scree-Plot")

#the number of clusters is 2

# 2 cluster solution
fit_km <- kmeans(normal_crime, 2) 
str(fit_km)

# appending cluster membership
final_crime1<- data.frame(crime, fit_km$cluster) 
View(final_crime1)

final_crime2 <- final_crime1[,c(ncol(final_crime1),1:(ncol(final_crime1)-1))]
View(final_crime2)

#Aggregating based on cluster
aggregate(crime[,1:4], by=list(fit_km$cluster), FUN=mean)

#finding cluster using clara
library(cluster)
xcl <- clara(normal_crime,2)
clusplot(xcl)

#finding cluster using pam 
xpm <- pam(normal_crime,2) 
clusplot(xpm)

table(fit_km$cluster)
