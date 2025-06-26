library(plotly)
library(tidyverse)

df <- read_csv("./data/net-enrollment-rate-primary-gender-parity-index-gpi.csv")
df <- df %>% 
  filter(Code != "" & Code != "OWID_WRL") %>%
  filter(Year != 2020 & Year != 2019 & Year != 2018) %>%
  rename(gpi = `Total net enrolment rate, primary, gender parity index (GPI)`) %>%
  mutate(gpi = round(gpi, 2))


# black boundaries
l <- list(color = toRGB("black"), width = 0.5)

# specify map projection/options
g <- list(
  showframe = FALSE,
  projection = list(type = 'Mercator')
)

fig <- plot_geo(df, frame = ~Year)
fig <- fig %>% add_trace(
  z = ~gpi,
  text = ~Entity, 
  locations = ~Code, 
  marker = list(line = l),
  colorscale = list(
    c(0, "rgba(106,168,206,255)"), # Werte <= 0.97
    c(0.485, "rgba(106,168,206,255)"),
    c(0.485, "rgba(157,204,140,255)"), # Werte 0.97 bis 1.03
    c(0.515, "rgba(157,204,140,255)"),
    c(0.515, "rgba(244,164,49,255)"), # Werte > 1.03
    c(1, "rgba(244,164,49,255)")
  ),
  colorbar = list(title = "GPI"),
  zmin = 0,
  zmax = 2
  )

fig <- fig %>% layout(
  title = 'Gender parity in net enrolment rates in primary education',
  geo = g
)

fig

