library(tidyverse)
library(ggplot2)
library(plotly)

df <- read.csv("./data/primary-enrollment-selected-countries.csv")
countrys <- read.csv("./data/world-regions-according-to-the-world-bank.csv")
countrys <- subset(countrys, select = -c(Entity, Year))


df <- df %>% 
  filter(Code != "") %>%
  merge(countrys, by = "Code", all.x = TRUE) %>%
  arrange(desc(Year)) %>%
  distinct(Code, .keep_all = TRUE) %>% 
  filter(Year >= 2014) %>%
  mutate(out.of.school = 100 - Combined.total.net.enrolment.rate..primary..both.sexes)


fig <- df %>%
  plot_ly(
    x = ~World.Region.according.to.the.World.Bank,
    y = ~out.of.school,
    split = ~World.Region.according.to.the.World.Bank,
    type = 'violin',
    color = ~World.Region.according.to.the.World.Bank,
    box = list(
      visible = T
    ),
    meanline = list(
      visible = T
    ),
    text = ~paste("Country: ", Entity, "<br>Value: ", out.of.school),  # Text für Hover-Info
    hoverinfo = "text" 
  ) 

fig <- fig %>%
  layout(
    title = "Share of children in primary school age who are out of school",
    xaxis = list(
      title = "Region"
    ),
    yaxis = list(
      title = "Percentage",
      zeroline = F
    )
  )

fig

