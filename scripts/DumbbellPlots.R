df <- read.csv("./data/primary-enrollment-selected-countries.csv")

df <- df %>% 
  filter(Code != "") 

countries_new <- df %>%
  filter(Year %in% c(2018, 2017, 2016)) %>%
  group_by(Entity) %>%
  slice(which.max(Year)) %>%
  ungroup() %>%
  rename(enrolment_new = Combined.total.net.enrolment.rate..primary..both.sexes) %>%
  rename(Year_new = Year)

countries_old <- df %>%
  filter(Year %in% c(2000, 1999, 1998, 1997, 1996)) %>%
  group_by(Entity) %>%
  slice(which.min(Year)) %>%
  ungroup() %>%
  select(-Entity) %>%
  rename(enrolment_old = Combined.total.net.enrolment.rate..primary..both.sexes) %>%
  rename(Year_old = Year)

df <- inner_join(countries_new, countries_old, by = "Code")

df <- df %>%
  mutate(Gap = abs(enrolment_new - enrolment_old)) %>%
  arrange(desc(Gap))

df <- bind_rows(
  head(df, 10),
  tail(df, 10)
)

df$enrolment_new <- round(df$enrolment_new, digits = 2)
df$enrolment_old <- round(df$enrolment_old, digits = 2)
df$Gap <- round(df$Gap, digits = 2)


df$Entity <- factor(df$Entity, levels = df$Entity[order(df$enrolment_old)])


colors <- RColorBrewer::brewer.pal(3, "Set2")

fig <- plot_ly(df, color = I("gray80")) %>% 
  add_segments(x = ~enrolment_old, xend = ~enrolment_new, y = ~Entity, yend = ~Entity, showlegend = FALSE) %>% 
  add_markers(x = ~enrolment_old, y = ~Entity, name = "Enrolment 20 years ago", color = colors[1], text = ~paste(Entity, "<br>Year: ", Year_old,"<br>Enrolment: ", enrolment_old, "%","<br>Gap: ", Gap, "%"), hoverinfo = "text") %>% 
  add_markers(x = ~enrolment_new, y = ~Entity, name = "Enrolment today", color = colors[3], text = ~paste(Entity, "<br>Year: ", Year_new,"<br>Enrolment: ", enrolment_new, "%","<br>Gap: ", Gap, "%"), hoverinfo = "text")
fig <- fig %>% layout(
  title = "Change in primary enrollment rates over the past 20 years",
  xaxis = list(title = "Percentage change"),
  yaxis = list(title = "Country"),
  margin = list(l = 65)
)
fig
 


