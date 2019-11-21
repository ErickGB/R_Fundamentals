# *********************************************************************
# carga librerias ----
library(tidyverse) 
library(factoextra)
library(C50)

library(plotly)
# *********************************************************************

# carga datos
data_tbl <- read.csv("./Data/vuelos.csv", header = TRUE) # **** cambiar a otro archivo

# estructura de datos del archivo cargado
data_tbl %>% 
  glimpse()

# limpia los nombres
data_tbl <- data_tbl %>% 
  janitor::clean_names()

# estructura de datos, verificando los cambios de nombres.
data_tbl %>% 
  glimpse()

# selecciona 3 columnas y las asigna a un nuevo data.frame ***
train_tbl <- data_tbl %>% 
  select(distance, arr_delay, dep_delay) 

# estandariza los datos (desviaciones estánderes)
train_tbl <- scale(train_tbl)
train_tbl <- as_tibble(train_tbl)

# revisamos los nulos
train_tbl %>% 
  DataExplorer::plot_missing()

# eliminamos los nulos (son pocos). ****
train_tbl <- train_tbl %>% 
  filter(is.na(dep_delay) == FALSE) %>% 
  filter(is.na(arr_delay) == FALSE) 


# *** estos pasos son los únicos que deben cambiar para generar nuevos análisis



#*****************************************
## Initial Cluster analysis (k selection) ----
#*****************************************
set.seed(7777)

# Explora para establecer el número de "k" potenciales
km_out <- list()
sil_out <- list()
x <- vector()
y <- vector()
min_clust <- 2      # Hypothesized minimum number of segments
max_clust <- 20    # Hypothesized maximum number of segments

# calcula los k means desde min_clust hasta max_clust
for (centr in min_clust:max_clust) {
        i <- centr-(min_clust-1) 
        km_out[i] <- list(kmeans(train_tbl, centers = centr, iter.max=1000, nstart=1))
        sil_out[i] <- list(cluster::silhouette(km_out[[i]][[1]], dist(train_tbl)))
        # Para generar el plot 
        x[i] = centr  # value of k
        y[i] = summary(sil_out[[i]])[[4]]  # Silhouette average width
}


# Plot silhouette 
ggplot(data = data.frame(x, y), aes(x, y)) + 
  geom_point(size=3) + 
  geom_line() +
  xlab("Número de clústers") +
  ylab("Silhouette Average Width") +
  ggtitle("Silhouette Average Width as Cluster Center Varies")

data.frame(x, y) %>% 
	arrange(desc(y))

# ***************************
# Genera el cluster fiinal, 
base_centers <- 3 # ¿Cuántos clústers deseas crear?
k_model <- eclust(train_tbl, "kmeans", k = base_centers,
                 nstart = 2, graph = FALSE)

# visualiza el silhouette de cada clúster
fviz_silhouette(k_model)

# otra visualización
fviz_cluster(k_model, geom = "point",  ellipse = FALSE)

# ************************
# visualización en 3D usando Plotly (otra librería)
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
# Árbol de decisión

# Ya que eliminamos los registros nulos en la tabla "train_tbl" debemos hacer lo mismo con la de datos (lo no estándarizados)
data_tbl <- data_tbl %>% 
  filter(is.na(dep_delay) == FALSE) %>% 
  filter(is.na(arr_delay) == FALSE) 

# Pasamos el identificador del clúster a los datos no estandarizados
data_tbl$cluster <- k_model$cluster
set.seed(1234) # For reproducibility
tree_model <- C50::C5.0(data_tbl %>% select(arr_delay, dep_delay, distance), 
	as.factor(data_tbl$cluster),
	control = C50::C5.0Control(winnow = FALSE, minCases = 50))
summary(tree_model)
plot(tree_model)





