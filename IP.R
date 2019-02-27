#####CLUSTERING-KMEANS#####

# Analyze the information given in the following 'Insurance Policy dataset' to
# create clusters of persons falling in the same type (K-means)

##Load and Attach the data
ipd<- read.csv(file.choose())
View(ipd)

attach(ipd)

##Number of rows
nrow(ipd)

##Summarize the data
summary(ipd)

#Premiums.paid: Mean= 12542, Median= 11825; As Mean>Median,it is skewed to the left.
#Age: Mean= 46.11, Median= 45.00; As Mean>Median,it is skewed to the left.
#Days.to.Renew: Mean= 120.4, Median= 89.0; As Mean>Median,it is skewed to the left.
#Claims.made: Mean= 12579, Median= 8386; As Mean>Median,it is skewed to the left.
#Income: Mean= 102250, Median= 102250; As Mean=Median, the distribution will have 0 skewness.

library(DataExplorer)
plot_str(ipd)
str(ipd)
plot_missing(ipd)

plot_histogram(Premiums.Paid)
plot_histogram(Age)
plot_histogram(Days.to.Renew)
plot_histogram(Claims.made)
plot_histogram(Income)

plot_density(Premiums.Paid)
plot_density(Age)
plot_density(Days.to.Renew)
plot_density(Claims.made)
plot_density(Income)

#Normalizing data
normal_ipd<- scale(ipd)
normal_ipd

#K-means clustering
# Determine number of clusters by scree-plot
wss = (nrow(normal_ipd)-1)*sum(apply(normal_ipd, 2, var))
for (i in 1:11) wss[i] = sum(kmeans(normal_ipd, centers=i)$withinss)

# Look for an "elbow" in the scree plot
plot(1:11, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
title(sub = "K-Means Clustering Scree-Plot")
#the number of clusters is 3

library(cluster)
ipd_kmeans <- kmeans(normal_ipd,3)
ipd_clara <- clara(normal_ipd,3)

Insurance_Dataset_clara <- cbind(ipd,ipd_clara$cluster)

clusplot(ipd_clara)

aggregate(Insurance_Dataset_clara, by=
            list(Insurance_Dataset_clara$`ipd_clara$cluster`), FUN = mean)

#K-means animation
library(animation)
km1 <- kmeans.ani(normal_ipd,3)

#finding cluster using pam
ipd_pam <- pam(normal_ipd,3)
Insurance_Dataset_pam <- cbind(ipd,ipd_pam$cluster)
clusplot(ipd_pam)
