#####CLUSTERING#####

# Perform clustering (Both hierarchical and K means clustering) for the airlines data to obtain optimum number of clusters. 
# Draw the inferences from the clusters obtained.

#Load and Attach the data
air<- read.csv(file.choose())
View(air)

#Removing unecessary columns
library(dplyr)
air<- select(air, -ID.)
View(air)
attach(air)

#Number of columns
ncol(air)

#Summarize the data
summary(air)
#balance: Mean= 73601, Median= 43097; As Mean>Median,it is skewed to the left.
#Qual_miles: Mean= 144.1, Median= 0.0; As Mean>Median,it is skewed to the left. 
#cc1_miles: Mean= 2.06, Median= 1.00; As Mean>Median,it is skewed to the left.
#cc2_miles: Mean= 1.015, Median= 1.000; As Mean>Median,it is skewed to the left. 
#cc3_miles: Mean= 1.012, Median= 1.000; As Mean>Median,it is skewed to the left.
#Bonus_miles: Mean= 17145, Median= 7171; As Mean>Median,it is skewed to the left.
#Bonus_trans: Mean= 11.6, Median= 12.0; As Mean<Median,it is skewed to the right.
#Flight_miles_12mo: Mean= 460.1, Median= 0.0; As Mean>Median,it is skewed to the left.
#Flight_trans_12: Mean= 1.374, Median= 0.000; As Mean>Median,it is skewed to the left.
#Days_since_enroll: Mean= 4119, Median= 4096; As Mean>Median,it is skewed to the left
#Award.: Mean= 0.3703, Median= 0.000; As Mean>Median,it is skewed to the left.

library(DataExplorer)
plot_str(air)
str(air)
plot_missing(air)

dev.new(width=5, height=4)
plot_histogram(air)

plot_density(air)

#Normalizing data
normal_air<- scale(air)
normal_air

##Hierarchical clustering

#Distance Matrix Computation
d<-dist(normal_airl,method="euclidean") 
d

#Hierarchical cluster
fit <- hclust(d, method="complete")

#Display dendrogram
# plot(fit)
# plot(fit, hang=-1)
#The dendogram is unclear due to the huge size of the data

#Cutting tree into five clusters
groups<-cutree(fit,k=5)
groups

#rect.hclust(fit,k=5,border="red")

membership<-as.matrix(groups)

final<-data.frame(air,membership)
View(final)

aggregate(airl,by= list(final$membership),FUN = mean)

#Writing data into a xlsx file
library(xlsx)
write.xlsx(final,file="final_airlines.xlsx")

##K-means Clustering

#elbow curve & k ~ sqrt(n/2) to decide the k value

# Determine number of clusters by scree-plot
wss = (nrow(normal_air)-1)*sum(apply(normal_air, 2, var))
for (i in 1:11) wss[i] = sum(kmeans(normal_air, centers=i)$withinss)

# Look for an "elbow" in the scree plot
plot(1:11, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
title(sub = "K-Means Clustering Scree-Plot")
#the number of clusters is 3

# 3 cluster solution
fit_km <- kmeans(normal_air, 3) 
str(fit_km)

# appending cluster membership
final_air1<- data.frame(air, fit_km$cluster) 
View(final_air1)

final_air2 <- final_air1[,c(ncol(final_air1),1:(ncol(final_air1)-1))]
View(final_air2)

#Aggregating based on cluster
aggregate(air[,1:11], by=list(fit_km$cluster), FUN=mean)

#finding cluster using clara
library(cluster)
xcl <- clara(normal_air,5)
clusplot(xcl)

#finding cluster using pam 
xpm <- pam(normal_airl,5) 
clusplot(xpm)

table(fit_km$cluster)

#Writing data into a xlsx file
library(xlsx)
write.xlsx(final_air2,file="final_airlines_kmc.xlsx")
