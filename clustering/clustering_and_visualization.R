library(tidyverse)
library(plotly)


# Laden und Skalieren der Daten
preprocessed_dataset <- read.csv("./clustering/preprocessed_dataset.csv")
data <-  preprocessed_dataset %>% select(-Entity, -Code, -World.Region.according.to.the.World.Bank) %>% scale()


# Berechne WCSS für verschiedene k-Werte
wcss <- function(k) {
  kmeans(data, k, nstart = 10 )$tot.withinss
}

k_values <- 1:10
wcss_values <- map_dbl(k_values, wcss)
elbow_df <- data.frame(k = k_values, wcss = wcss_values)

plot_ly(elbow_df, x = ~k, y = ~wcss, type = 'scatter', mode = 'lines+markers') %>%
  layout(title = "Elbow Method for Optimal k",
         xaxis = list(title = "Number of Clusters k"),
         yaxis = list(title = "Total Within-Cluster Sum of Squares (WCSS)"))

# Clustering
data <-  preprocessed_dataset %>% select(-Entity, -Code, -World.Region.according.to.the.World.Bank) %>% scale()
set.seed(123)
kmeans_result <- kmeans(data, centers = 4, iter.max = 100, nstart = 100)
preprocessed_dataset$Cluster <- kmeans_result$cluster
preprocessed_dataset$Cluster <- as.character(preprocessed_dataset$Cluster)



# PCA
X <- subset(preprocessed_dataset, select = -c(Entity, Code, World.Region.according.to.the.World.Bank, Cluster))
prin_comp <- prcomp(X, rank. = 3)
components <- prin_comp[["x"]]
components <- data.frame(components)
components$PC2 <- -components$PC2
components$PC3 <- -components$PC3
components = cbind(components, preprocessed_dataset$Cluster)


clusters <- unique(preprocessed_dataset$Cluster)
colors <- RColorBrewer::brewer.pal(length(clusters), "Set2")


fig <- plot_ly(components, type = "scatter3d",
               x = ~PC1, 
               y = ~PC2, 
               z = ~PC3, 
               color = ~preprocessed_dataset$Cluster,
               colors = colors,
               text = ~preprocessed_dataset$Entity, 
               hoverinfo = "text"
)#, colors = c('#636EFA','#EF553B','#00CC96') )

fig <- fig %>%
  layout(
    title = 'Results of the Cluster Analysis',
    legend = list(title = list(text = "Cluster"))#,
    #scene = list(bgcolor = "#e5ecf6")
    
  )
fig


# Save Dataframe
#file_path <- "./clustering/preprocessed_dataset_with_cluster.csv"
#write.csv(preprocessed_dataset, file = file_path, row.names = FALSE)