library(tidyverse)
library(plotly)


df <- read.csv("C:/Users/jakob/Downloads/number-of-out-of-school-children.csv")

df <- df %>% filter(Code == "OWID_WRL") %>%
  rename(boys_not_in_school = `Out.of.school.children..adolescents.and.youth.of.primary.and.secondary.school.age..male..number.`) %>%
  rename(girls_not_in_school = Out.of.school.children..adolescents.and.youth.of.primary.and.secondary.school.age..female..number.) 

fig1 <- plot_ly(df, type = 'scatter', mode = 'lines', fill = 'tonexty')%>%
  add_trace(x = ~Year, y = ~boys_not_in_school, name = 'Boys', line = list(color = 'blue'), fillcolor = 'rgba(0, 0, 255, 0.2)')%>%
  layout(legend=list(title=list(text='Gender')), xaxis = list(title = 'Year'), yaxis = list(range = c(120000000,220000000), title = 'Children out of school'))


fig2 <- plot_ly(df, type = 'scatter', mode = 'lines', fill = 'tonexty')%>%
  add_trace(x = ~Year, y = ~girls_not_in_school, name = 'Girls')%>%
  layout(legend=list(title=list(text='Gender')), xaxis = list(title = 'Year'), yaxis = list(range = c(120000000,220000000),title = '', showticklabels = FALSE))

fig <- subplot(fig1, fig2,
               nrows = 1, titleY = TRUE, titleX = TRUE) %>% layout(
                 title = 'Number of boys and girls not attending school worldwide',
                 xaxis = list(zerolinecolor = '#ffff',
                              zerolinewidth = 20,
                              gridcolor = 'ffff'),
                 yaxis = list(zerolinecolor = '#ffff',
                              zerolinewidth = 2,
                              gridcolor = 'ffff'),
                 plot_bgcolor='#e5ecf6')

fig

