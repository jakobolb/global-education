library(plotly)
library(tidyverse)

df <- read.csv("./data/gender-gap-education-levels.csv")
df <- df %>% filter(Code == "OWID_WRL")
df <- pivot_longer(df, cols = starts_with("Combined"), names_to = "who", values_to = "value")
df <- df %>% mutate(value = round(value, 2)) %>%
             mutate(who = recode(who, 
                        "Combined.total.net.enrolment.rate..primary..female" = "Girls in primary education", 
                        "Combined.total.net.enrolment.rate..primary..male" = "Boys in primary education",
                        "Combined.total.net.enrolment.rate..secondary..female" = "Girls in secondary education", 
                        "Combined.total.net.enrolment.rate..secondary..male" = "Boys in secondary education",
                        "Combined.gross.enrolment.ratio.for.tertiary.education..female" = "Girls in tertiary education", 
                        "Combined.gross.enrolment.ratio.for.tertiary.education..male" = "Boys in tertiary education"))


accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

df <- df %>% accumulate_by(~Year)

fig <- df %>%
  plot_ly(
    x = ~Year, 
    y = ~value,
    split = ~who,
    frame = ~frame, 
    type = 'scatter',
    mode = 'lines', 
    text = ~who,
    line = list(simplyfy = F)
  )

fig <- fig %>% layout(
  title = 'Gender gap in primary, secondary and tertiary education, worldwide',
  xaxis = list(
    title = "Year",
    zeroline = F
  ),
  yaxis = list(
    title = "Percentage",
    zeroline = F
  )
) 
fig <- fig %>% animation_opts(
  frame = 100, 
  transition = 0, 
  redraw = FALSE
)
fig <- fig %>% animation_slider(
  hide = T
)
fig <- fig %>% animation_button(
  x = 1, xanchor = "right", y = 0, yanchor = "bottom"
)

fig



