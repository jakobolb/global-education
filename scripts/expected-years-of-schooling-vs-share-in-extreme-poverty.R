library(tidyverse)
library(plotly)

df <- read.csv("./data/expected-years-of-schooling-vs-share-in-extreme-poverty.csv")
countrys <- read.csv("./data/world-regions-according-to-the-world-bank.csv")
countrys <- subset(countrys, select = -c(Entity, Year))
df <- merge(df, countrys, by = "Code", all.x = TRUE)

df <- df %>% 
  filter(Code != "") %>%
  filter(Code != "OWID_WRL") %>%
  filter(Year >= 2017) %>%
  mutate(Expected.years.of.schooling = round(Expected.years.of.schooling, 2)) %>%
  mutate(X.2.15.a.day...Share.of.population.in.poverty = round(X.2.15.a.day...Share.of.population.in.poverty, 2))

columns_to_check <- c(
  'Expected.years.of.schooling',
  "X.2.15.a.day...Share.of.population.in.poverty",
  "Population..historical.estimates."
)

cleaned_data <- df[complete.cases(df[, columns_to_check]), ]

df <- cleaned_data %>%
  group_by(Entity) %>%
  slice_max(Year, n = 1, with_ties = FALSE) %>%
  ungroup()


fig <- df %>%
  plot_ly(
    x = ~X.2.15.a.day...Share.of.population.in.poverty, 
    y = ~Expected.years.of.schooling, 
    size = ~Population..historical.estimates., 
    color = ~World.Region.according.to.the.World.Bank, 
    text = ~Entity, 
    type = 'scatter',
    mode = 'markers',
    hovertemplate = paste(
      ' Country: %{text}<br>',
      'Share of Population in Poverty: %{x}<br>',
      'Expected Years of Schooling: %{y}<br>'
    )
  )
fig <- fig %>% 
  layout(
    title = "Expected years of schooling vs. share in extreme poverty, 2017-2022",
    xaxis = list(
      title = "Share of population in poverty",
      type = "log",
      linewidth = 1,
      tickvals = c(0.01, 0.1, 1, 10, 100),
      ticktext = c("0.01", "0.1", "1", "10", "100")
    ),
    yaxis = list(
      title = "Expected years of schooling",
      linewidth = 1    
    )
)

fig

#round(cor(df$Expected.years.of.schooling, df$X.2.15.a.day...Share.of.population.in.poverty, use = "complete.obs"),2)

