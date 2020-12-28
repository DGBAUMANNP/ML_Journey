install.packages("rattle.data")
library(rattle.data)

data(wine, package="rattle.data")

wine_subset <- scale(wine[ , c(2:4)])

wine_cluster <- kmeans(wine_subset, centers = 3,
                       iter.max = 10,
                       nstart = 25)


install.packages("factoextra")
library(factoextra)

fviz_cluster(wine_cluster, data = wine_subset)


# Function to compute total within-cluster sum of square
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}
# plotting values for each cluster starting from 1 to 9
wssplot(wine_subset, nc = 9)


