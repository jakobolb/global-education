library(tidyverse)
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


fig <- df %>%
  plot_ly(
    x = ~GDP.per.capita..PPP..constant.2017.international..., 
    y = ~Harmonized.Test.Scores, 
    size = ~Population..historical.estimates., 
    color = ~World.Region.according.to.the.World.Bank, 
    text = ~Entity, 
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers',
    hovertemplate = paste(
      ' Country: %{text}<br>',
      'Harmonized test scores: %{y}<br>',
      'GDP per capita: %{x}<br>'
    )
  )
fig <- fig %>% 
  layout(
    title = "Average learning outcomes vs. GDP per capita, 2020",
    xaxis = list(
      title = "GDP per capita",
      linewidth = 2  
    ),
    yaxis = list(
      title = "Harmonized test scores",
      showline = FALSE,
      zeorline = FALSE
    )
  )

fig

#round(cor(df$Harmonized.Test.Scores, df$GDP.per.capita..PPP..constant.2017.international..., use = "complete.obs"),2)
