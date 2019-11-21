# *********************************************************************
# carga librerias ----
library(tidyverse) 
# *********************************************************************

data_tbl <- read.csv("./Data/airline_delay_2016_08.csv", header = TRUE)

data_tbl <- data_tbl 



data_tbl %>% 
  glimpse()

data_tbl <- data_tbl %>% 
  janitor::clean_names()


data_tbl <- data_tbl%>% 
  head(20000)


train_tbl <- data_tbl %>% 
  select(distance, arr_delay, dep_delay) 

train_tbl <- scale(train_tbl)
train_tbl <- as_tibble(train_tbl)

train_tbl %>% 
  DataExplorer::plot_missing()


train_tbl <- train_tbl %>% 
  filter(is.na(dep_delay) == FALSE) %>% 
  filter(is.na(arr_delay) == FALSE) 

#*****************************************
## Initial Cluster analysis (k selection) ----
#*****************************************
set.seed(7777)

#Exploratory for find the best numbers of groups (k) 
# Setup for k-means loop 
km_out <- list()
sil_out <- list()
x <- vector()
y <- vector()
min_clust <- 2      # Hypothesized minimum number of segments
max_clust <- 20    # Hypothesized maximum number of segments

# Compute k-means clustering over various clusters, k, from minClust to maxClust
for (centr in min_clust:max_clust) {
        i <- centr-(min_clust-1) # relevels start as 1, and increases with centr
        #set.seed(777) # For reproducibility
        km_out[i] <- list(kmeans(train_tbl, centers = centr, iter.max=1000, nstart=1))
        sil_out[i] <- list(cluster::silhouette(km_out[[i]][[1]], dist(train_tbl)))
        # Used for plotting silhouette average widths
        x[i] = centr  # value of k
        y[i] = summary(sil_out[[i]])[[4]]  # Silhouette average width
}


# Plot silhouette results to find best number of clusters; closer to 1 is better
ggplot(data = data.frame(x, y), aes(x, y)) + 
  geom_point(size=3) + 
  geom_line() +
  xlab("Número de clústers") +
  ylab("Silhouette Average Width") +
  ggtitle("Silhouette Average Width as Cluster Center Varies")

data.frame(x, y) %>% 
	arrange(desc(y))


base_centers <- 3
k_model <- eclust(train_tbl, "kmeans", k = base_centers,
                 nstart = 2, graph = FALSE)

library(factoextra)
fviz_silhouette(k_model)

# Visualize k-means clusters
fviz_cluster(k_model, geom = "point",  ellipse = FALSE)

# ************************
library(plotly)
train_tbl$cluster <- k_model$cluster
p <- plot_ly(train_tbl, x = ~arr_delay, y = ~dep_delay, z = ~distance,
        marker = list(color = ~cluster, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'array delay'),
                     yaxis = list(title = 'dep_delay'),
                     zaxis = list(title = 'distance')),
         annotations = list(
           x = 1.13,
           y = 1.05,
           text = 'Miles/(US) gallon',
           xref = 'paper',
           yref = 'paper',
           showarrow = FALSE
         ))
p 


# ************************************
# Arbol de decisión

data_tbl$cluster <- k_model$cluster
set.seed(1234) # For reproducibility
tree_model <- C50::C5.0(data_tbl %>% select(arr_delay, dep_delay, distance), 
	as.factor(data_tbl$cluster),
	control = C50::C5.0Control(winnow = FALSE, minCases = 50))
summary(tree_model)




