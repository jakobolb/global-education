#install.packages("reshape2")
#install.packages("tidymodels")

library(reshape2) 
library(tidyverse)
library(tidymodels)
library(plotly)

df <- read.csv("./data/expected-years-of-schooling-vs-share-in-extreme-poverty.csv")
countrys <- read.csv("./data/world-regions-according-to-the-world-bank.csv")
countrys <- subset(countrys, select = -c(Entity, Year))
df <- merge(df, countrys, by = "Code", all.x = TRUE)

df <- df %>% 
  filter(Code != "") %>%
  filter(Code != "OWID_WRL") %>%
  filter(Year >= 2017) 

columns_to_check <- c(
  #'Expected.years.of.schooling',
  "X.2.15.a.day...Share.of.population.in.poverty",
  "Population..historical.estimates."
)

cleaned_data <- df[complete.cases(df[, columns_to_check]), ]

df <- cleaned_data %>%
  group_by(Entity) %>%
  slice_max(Year, n = 1, with_ties = FALSE) %>%
  ungroup()


X <- df$X.2.15.a.day...Share.of.population.in.poverty
y <- df$Expected.years.of.schooling

lm_model <- linear_reg() %>% 
  set_engine('lm') %>% 
  set_mode('regression') %>%
  fit(Expected.years.of.schooling ~ X.2.15.a.day...Share.of.population.in.poverty, data = df) 

x_range <- seq(min(X), max(X), length.out = 100)
x_range <- matrix(x_range, nrow=100, ncol=1)
xdf <- data.frame(x_range)
colnames(xdf) <- c('X.2.15.a.day...Share.of.population.in.poverty')

ydf <- lm_model %>% predict(xdf) 

colnames(ydf) <- c('Expected.years.of.schooling')
xy <- data.frame(xdf, ydf) 

fig <- plot_ly(df, 
               x = ~X.2.15.a.day...Share.of.population.in.poverty, 
               y = ~Expected.years.of.schooling, 
               #size = ~Population..historical.estimates.,
               type = 'scatter',
               #color = ~World.Region.according.to.the.World.Bank,
               alpha = 0.65, 
               #text = ~Entity, 
               #hoverinfo = "text",
               mode = 'markers', 
               name = 'Countries')

fig <- fig %>% 
  add_trace(
    data = xy, 
    x = ~X.2.15.a.day...Share.of.population.in.poverty, 
    y = ~Expected.years.of.schooling, 
    name = 'Regression Fit', 
    mode = 'lines', 
    alpha = 1)
fig <- fig %>% 
  layout(
    title = "Expected years of schooling vs. share in extreme poverty, 2017-2022",
    xaxis = list(
      title = "Share of population in poverty",
      linewidth = 1
    ),
    yaxis = list(
      title = "Expected years of schooling"
    )
  )
fig

