#install.packages("reshape2")
#install.packages("tidymodels")

library(reshape2) 
library(tidyverse)
library(tidymodels) 
library(plotly)

df <- read.csv("./data/learning-outcomes-vs-gdp-per-capita.csv")

df <- df %>% 
  filter(Code != "") %>%
  filter(Code != "OWID_WRL") %>%
  filter(Year >= 2017) %>% 
  select(-World.Region.according.to.the.World.Bank) %>% 
  mutate(Harmonized.Test.Scores = round(Harmonized.Test.Scores, 0)) %>%
  mutate(GDP.per.capita..PPP..constant.2017.international... = round(GDP.per.capita..PPP..constant.2017.international..., 0))

countrys <- read.csv("./data/world-regions-according-to-the-world-bank.csv")
countrys <- subset(countrys, select = -c(Entity, Year))
df <- merge(df, countrys, by = "Code", all.x = TRUE)

columns_to_check <- c(
  'Harmonized.Test.Scores',
  "GDP.per.capita..PPP..constant.2017.international...",
  "Population..historical.estimates.",
  "World.Region.according.to.the.World.Bank"
)

cleaned_data <- df[complete.cases(df[, columns_to_check]), ]

df <- cleaned_data %>%
  group_by(Entity) %>%
  slice_max(Year, n = 1, with_ties = FALSE) %>%
  ungroup()

y <- df$Harmonized.Test.Scores
X <- df$GDP.per.capita..PPP..constant.2017.international...

lm_model <- linear_reg() %>% 
  set_engine('lm') %>% 
  set_mode('regression') %>%
  fit(Harmonized.Test.Scores ~ GDP.per.capita..PPP..constant.2017.international..., data = df) 

x_range <- seq(min(X), max(X), length.out = 100)
x_range <- matrix(x_range, nrow=100, ncol=1)
xdf <- data.frame(x_range)
colnames(xdf) <- c('GDP.per.capita..PPP..constant.2017.international...')

ydf <- lm_model %>% predict(xdf) 

colnames(ydf) <- c('Harmonized.Test.Scores')
xy <- data.frame(xdf, ydf) 

fig <- plot_ly(df, 
               x = ~GDP.per.capita..PPP..constant.2017.international..., 
               y = ~Harmonized.Test.Scores, 
               type = 'scatter', 
               alpha = 0.65, 
               mode = 'markers',
               name = "Countries")
fig <- fig %>% 
  add_trace(
    data = xy, 
    x = ~GDP.per.capita..PPP..constant.2017.international..., 
    y = ~Harmonized.Test.Scores, 
    name = 'Regression Fit', 
    mode = 'lines', 
    alpha = 1)
fig <- fig %>% 
  layout(
    title = "Average learning outcomes vs. GDP per capita, 2020",
    xaxis = list(
      title = "GDP per capita",
      linewidth = 1  
    ),
    yaxis = list(
      title = "Harmonized test scores",
      showline = FALSE,
      zeorline = FALSE
    )
  )
fig
