library(tidyverse)
library(plotly)

df <- read.csv("./clustering/preprocessed_dataset_with_cluster.csv")
df <- df %>%
  mutate(Cluster = as.character(Cluster))

clusters <- unique(df$Cluster)
colors <- RColorBrewer::brewer.pal(length(clusters), "Set2")


fig <- plot_ly(df, x = ~Cluster, y = ~Combined.total.net.enrolment.rate..primary..both.sexes, type = 'box', 
                  color = ~Cluster,
                  colors = colors)
fig <- fig %>% layout(
  title = "Share of children in primary school age who are in school",
  xaxis = list(title = 'Cluster', zeroline = TRUE, showline = TRUE),
  yaxis = list(title = 'Percentage', zeroline = TRUE, showline = TRUE),
  showlegend = FALSE
)
fig



df <- read.csv("./clustering/preprocessed_dataset_with_cluster.csv")
df <- df %>%
  mutate(Cluster = as.character(Cluster))

clusters <- unique(df$Cluster)
colors <- RColorBrewer::brewer.pal(length(clusters), "Set2")


fig <- plot_ly(df, x = ~Cluster, y = ~Total.net.enrolment.rate..primary..gender.parity.index..GPI., type = 'box', 
               color = ~Cluster,
               colors = colors)
fig <- fig %>% layout(
  title = "Gender parity in net enrolment rates in primary education",
  xaxis = list(title = 'Cluster', zeroline = TRUE, showline = TRUE),
  yaxis = list(title = 'GPI', zeroline = TRUE, showline = TRUE),
  showlegend = FALSE
)
fig


df <- read.csv("./clustering/preprocessed_dataset_with_cluster.csv")
df <- df %>%
  mutate(Cluster = as.character(Cluster))

clusters <- unique(df$Cluster)
colors <- RColorBrewer::brewer.pal(length(clusters), "Set2")


fig <- plot_ly(df, x = ~Cluster, y =~Learning.Adjusted.Years.of.School, type = 'box', 
               color = ~Cluster,
               colors = colors)
fig <- fig %>% layout(
  title = "Average learning-adjusted years of schooling",
  xaxis = list(title = 'Cluster', zeroline = TRUE, showline = TRUE),
  yaxis = list(title = 'Years', zeroline = TRUE, showline = TRUE),
  showlegend = FALSE
)
fig





df <- read.csv("./clustering/preprocessed_dataset_with_cluster.csv")
df <- df %>%
  mutate(Cluster = as.character(Cluster))

clusters <- unique(df$Cluster)
colors <- RColorBrewer::brewer.pal(length(clusters), "Set2")


fig <- plot_ly(df, x = ~Cluster, y =~Harmonized.Test.Scores, type = 'box', 
               color = ~Cluster,
               colors = colors)
fig <- fig %>% layout(
  title = "Average learning outcomes",
  xaxis = list(title = 'Cluster', zeroline = TRUE, showline = TRUE),
  yaxis = list(title = 'Average learning outcomes', zeroline = TRUE, showline = TRUE),
  showlegend = FALSE
)
fig






df <- read.csv("./clustering/preprocessed_dataset_with_cluster.csv")
df <- df %>%
  mutate(Cluster = as.character(Cluster))

clusters <- unique(df$Cluster)
colors <- RColorBrewer::brewer.pal(length(clusters), "Set2")


fig <- plot_ly(df, x = ~Cluster, y =~GDP.per.capita..PPP..constant.2017.international..., type = 'box', 
               color = ~Cluster,
               colors = colors)
fig <- fig %>% layout(
  title = "GDP per capita",
  xaxis = list(title = 'Cluster'),
  yaxis = list(title = 'GDP per capita', zeroline = TRUE, showline = TRUE),
  showlegend = FALSE
)
fig

