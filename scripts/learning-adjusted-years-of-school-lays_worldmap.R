df <- read_csv("./data/learning-adjusted-years-of-school-lays.csv")
df <- df %>% 
  filter(Code != "" & Code != "OWID_WRL") %>%
  filter(Year == 2020) %>%
  rename(Years = `Learning-Adjusted Years of School`) %>%
  mutate(Years = round(Years, 1))
  
  
# black boundaries
l <- list(color = toRGB("black"), width = 0.5)

# specify map projection/options
g <- list(
  showframe = FALSE,
  projection = list(type = 'Mercator')
)

fig <- plot_geo(df, frame = ~Year)
fig <- fig %>% add_trace(
  z = ~Years,
  zmin = 0,
  zmax = 13,
  colors = 'Blues',
  text = ~Entity, 
  locations = ~Code, 
  marker = list(line = l)
)
fig <- fig %>% layout(
  title = 'Average learning-adjusted years of schooling, 2020',
  geo = g
)

fig

