# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

# install.packages(c("cluster", "rattle.data","NbClust"))
library(cluster)
library(rattle.data)
library(NbClust)
# Now load the data and look at the first few rows
data(wine, package="rattle.data")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
df <- scale(wine[,-1])

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(df)
# Appears to suggest 3 clusters?


# Exercise 2:
#   * How many clusters does this method suggest? 
      # 3, the graph changes slope at point 3 
#   * Why does this method work? What's the intuition behind it? 
      # As clusters increase, the error decreases to a point of saturation, at the "bend" (here 3)
#   * Look at the code for wssplot() and figure out how it works
      # It calculates the SSE for each amount of clusters 1-15 and then plots error vs # of clusters

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?
      # 3 clusters

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans(df, 3)

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?
table(fit.km$cluster, wine$Type)
      # Appears to have labeled Type 3 as cluster 1, and cluster 3 as Type 1. Cluster 2 is accurately labeld type 2 from original DF.
      # All of the original type 1 and type 3 were accurately grouped. From Type 2, there are 6 data points that are missgrouped
      # (3 in cluster 1 and 3 in cluster 3). This means that out of 178, only 6 were incorrectly grouped (3.3% fail rate). Overall
      # This did a pretty good job


# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?

clusplot(wine, fit.km$cluster, main = '2D representation of the Cluster Solution',
        color = TRUE,
        labels = 2, lines = 0)
  
      # I think it's done a pretty good job. As mentioned before, there are a few errors, which are represented by points
      # (62, 74, 84, 96, 119, 122). Some of these points are very close to the borders between cluster 2 and 1/3 which is makes it
      # clear why there would be a few misslabeled.


# To see about Hierarchical Clustering

distance <- dist(wine, method = "euclidean")
HierarchFit <- hclust(distance, method = "ward")
plot(HierarchFit) # dendrogram
groups <- cutree(HierarchFit, k = 3)
rect.hclust(HierarchFit, k = 3, border = "red")