library(tidyverse)
library(plotly)

df <- read.csv("./data/out-of-school-children-of-primary-school-age-by-world-region.csv")
df <- df %>% 
  filter(Code == "") %>%
  filter(Entity != "Arab World (WB)") %>%
  filter(Entity != "Central Europe and the Baltics (WB)") %>% 
  filter(grepl("\\(WB\\)", Entity)) %>% 
  filter(Year != 1986 & Year != 2021 & Year != 2020)
  

df <- df %>%
  pivot_wider(names_from = Entity, values_from = Children.out.of.school..primary)

df$Summe <- rowSums(df[, c("East Asia and Pacific (WB)", 
                            "Europe and Central Asia (WB)",
                            "Latin America and Caribbean (WB)",
                            "Middle East and North Africa (WB)", 
                            "North America (WB)",
                            "South Asia (WB)",
                            "Sub-Saharan Africa (WB)")])


accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

df <- df %>% accumulate_by(~Year)
colors <- RColorBrewer::brewer.pal(7, "Set2")

fig <- df %>% plot_ly(
  x = ~Year, 
  y = ~`Sub-Saharan Africa (WB)`, 
  frame = ~frame,
  type = 'scatter', 
  mode = 'none', 
  stackgroup = 'one', 
  fillcolor = colors[7],
  name = 'Sub-Saharan Africa',
  text = ~paste("Year: ", Year
                , "<br>Sub-Saharan Africa: ", `Sub-Saharan Africa (WB)`
                , "<br>Total: ", Summe),
  hoverinfo = 'text'
)

fig <- fig %>% add_trace(
  y = ~`South Asia (WB)`, 
  fillcolor = colors[6],
  name = 'South Asia',
  text = ~paste("Year: ", Year
                , "<br>South Asia: ", `South Asia (WB)`
                , "<br>Total: ", Summe),
  hoverinfo = 'text'
)

fig <- fig %>% add_trace(
  y = ~`East Asia and Pacific (WB)`, 
  fillcolor = colors[1],
  name = 'East Asia and Pacific',
  text = ~paste("Year: ", Year
                , "<br>East Asia and Pacific: ", `East Asia and Pacific (WB)`
                , "<br>Total: ", Summe),
  hoverinfo = 'text'
)

fig <- fig %>% add_trace(
  y = ~`Middle East and North Africa (WB)`, 
  #fillcolor = 'rgba(225,96,96,255)',
  fillcolor = colors[4],
  name = 'Middle East and North Africa',
  text = ~paste("Year: ", Year
                , "<br>Middle East and North Africa: ", `Middle East and North Africa (WB)`
                , "<br>Total: ", Summe),
  hoverinfo = 'text'
)

fig <- fig %>% add_trace(
  y = ~`Latin America and Caribbean (WB)`, 
  #fillcolor = 'rgba(99,185,99,255)',
  fillcolor = colors[3],
  name = 'Latin America and Caribbean',
  text = ~paste("Year: ", Year
                , "<br>Latin America and Caribbean: ", `Latin America and Caribbean (WB)`
                , "<br>Total: ", Summe),
  hoverinfo = 'text'
)

fig <- fig %>% add_trace(
  y = ~`Europe and Central Asia (WB)`, 
  #fillcolor = 'rgba(255,161,77,255)',
  fillcolor = colors[2],
  name = 'Europe and Central Asia',
  text = ~paste("Year: ", Year
                , "<br>Europe and Central Asia: ", `Europe and Central Asia (WB)`
                , "<br>Total: ", Summe),
  hoverinfo = 'text'
)

fig <- fig %>% add_trace(
  y = ~`North America (WB)`, 
  #fillcolor = 'rgba(101,161,203,255)', 
  fillcolor = colors[5],
  name = 'North America',
  text = ~paste("Year: ", Year
                , "<br>North America: ", `North America (WB)`
                , "<br>Total: ", Summe),
  hoverinfo = 'text'
)


fig <- fig %>% layout(
  title = "Children out of school worldwide",
  yaxis = list(
    title = "Children not in primary school",
    range = c(0,120000000)
  ),
  xaxis = list(
    title = "Year" 
  )
)

fig <- fig %>% animation_opts(
  frame = 100, 
  transition = 0, 
  redraw = FALSE
)
fig <- fig %>% animation_slider(
  currentvalue = list(
    prefix = "Year "
  )
)

fig <- fig %>% animation_slider(
  hide = T
)
fig <- fig %>% animation_button(
  x = 1, xanchor = "right", y = 0.025, yanchor = "bottom"
)

fig

