library(ggplot2)
library(tidyverse)

df <- read_csv("./data/primary-enrollment-selected-countries.csv")
df <- df %>% 
  filter(Code != "" & Code != "OWID_WRL") %>%
  filter(Year != 2019 & Year != 2018) %>%
  rename(Enrolment = `Combined total net enrolment rate, primary, both sexes`)


# black boundaries
l <- list(color = toRGB("black"), width = 0.5)

# specify map projection/options
g <- list(
  showframe = FALSE,
  projection = list(type = 'Mercator')
)

fig <- plot_geo(df, frame = ~Year)
fig <- fig %>% add_trace(
  z = ~Enrolment,
  zmin = 0,
  zmax = 100,
  colors = 'Blues',
  text = ~Entity, 
  locations = ~Code, 
  marker = list(line = l)
)
fig <- fig %>% layout(
  title = 'Share of children in primary school age who are in school',
  geo = g
)

fig

