library(tidyverse) 
customers <- read.csv("./Data/bikeshops.csv", header = TRUE)
products <- read.csv("./Data/bikes.csv", header = TRUE) 
orders <- read.csv("./Data/orders.csv", header = TRUE) 

orders %>%
  glimpse()

orders$X <- NULL 

orders %>%
  glimpse()

customers <- customers %>%
  rename(bikeshop.id = ii..bikeshop.id)

products <- products %>%
  rename(product.id = ii..product.id)


write.csv(products, "./Data/producto.csv", row.names = FALSE)


orders.extended <- merge(orders, customers, by.x = "customer.id", by.y="bikeshop.id")
orders.extended <- merge(orders.extended, products, by.x = "product.id", by.y = "bike.id")

orders.extended <- orders.extended %>%
  mutate(price.extended = price * quantity) %>%
  select(order.date, order.id, order.line, bikeshop.name, model,
         quantity, price, price.extended, category1, category2, frame) %>%
  arrange(order.id, order.line)

library(tidyr)  # Needed for spread function
customerTrends <- orders.extended %>%
        group_by(bikeshop.name, model, category1, category2, frame, price) %>%
        summarise(total.qty = sum(quantity)) %>%
        spread(bikeshop.name, total.qty)

customerTrends[is.na(customerTrends)] <- 0  # Remove NA's
write.csv(customerTrends, "./Data/out_customers.csv")







library(Hmisc)  # Needed for cut2 function
customerTrends$price <- cut2(customerTrends$price, g=2)   

# Convert customer purchase quantity to percentage of total quantity -----------
customerTrends.mat <- as.matrix(customerTrends[,-(1:5)])  # Drop first five columns
customerTrends.mat <- prop.table(customerTrends.mat, margin = 2)  # column-wise pct
customerTrends <- bind_cols(customerTrends[,1:5], as.data.frame(customerTrends.mat))


# Running the k-means algorithm -------------------------------------------------
library(cluster) # Needed for silhouette function

kmeansDat <- customerTrends[,-(1:5)]  # Extract only customer columns
kmeansDat.t <- t(kmeansDat)  # Get customers in rows and products in columns

# Setup for k-means loop 
km.out <- list()
sil.out <- list()
x <- vector()
y <- vector()
minClust <- 4      # Hypothesized minimum number of segments
maxClust <- 8      # Hypothesized maximum number of segments

# Compute k-means clustering over various clusters, k, from minClust to maxClust
for (centr in minClust:maxClust) {
        i <- centr-(minClust-1) # relevels start as 1, and increases with centr
        set.seed(11) # For reproducibility
        km.out[i] <- list(kmeans(kmeansDat.t, centers = centr, nstart = 50))
        sil.out[i] <- list(silhouette(km.out[[i]][[1]], dist(kmeansDat.t)))
        # Used for plotting silhouette average widths
        x[i] = centr  # value of k
        y[i] = summary(sil.out[[i]])[[4]]  # Silhouette average width
}

# Plot silhouette results to find best number of clusters; closer to 1 is better
library(ggplot2)
ggplot(data = data.frame(x, y), aes(x, y)) + 
  geom_point(size=3) + 
  geom_line() +
  xlab("Number of Cluster Centers") +
  ylab("Silhouette Average Width") +
  ggtitle("Silhouette Average Width as Cluster Center Varies")

# Get customer names that are in each segment ----------------------------------

# Get attributes of optimal k-means output
maxSilRow <- which.max(y)          # Row number of max silhouette value
optimalClusters <- x[maxSilRow]    # Number of clusters
km.out.best <- km.out[[maxSilRow]] # k-means output of best cluster

# Create list of customer names for each cluster
clusterNames <- list()
clusterList <- list()
for (clustr in 1:optimalClusters) {
  clusterNames[clustr] <- paste0("X", clustr)
  clusterList[clustr] <- list(
    names(
        km.out.best$cluster[km.out.best$cluster == clustr]
        )
    )
}
names(clusterList) <- clusterNames

print(clusterList)

# Combine cluster centroids with bike models for feature inspection ------------
custSegmentCntrs <- t(km.out.best$centers)  # Get centroids for groups
colnames(custSegmentCntrs) <- make.names(colnames(custSegmentCntrs))
customerTrends.clustered <- bind_cols(customerTrends[,1:5], as.data.frame(custSegmentCntrs))


