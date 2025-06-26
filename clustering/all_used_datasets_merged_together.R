library(tidyverse)

filter_to_most_recent_year <- function(df) {
  df %>%
    group_by(Code) %>% # Gruppiere nach 'Code'
    filter(Year == max(Year)) %>% # Filtere nach dem maximalen Jahr innerhalb jeder Gruppe
    ungroup() # Hebe die Gruppierung auf
}

# Laden der Daten
df1 <- read.csv("./data/primary-enrollment-selected-countries.csv") %>% 
  filter(Year >= 2015) %>% 
  filter_to_most_recent_year() %>%
  select(Code, Combined.total.net.enrolment.rate..primary..both.sexes)

df2 <- read.csv("./data/number-of-out-of-school-children.csv") %>% 
  filter(Year >= 2015) %>% 
  filter_to_most_recent_year() %>%
  select(Code, Out.of.school.children..adolescents.and.youth.of.primary.and.secondary.school.age..male..number., Out.of.school.children..adolescents.and.youth.of.primary.and.secondary.school.age..female..number.)

df3 <- read.csv("./data/net-enrollment-rate-primary-gender-parity-index-gpi.csv") %>% 
  filter(Year >= 2015) %>%
  filter_to_most_recent_year() %>%
  select(Code, Total.net.enrolment.rate..primary..gender.parity.index..GPI.)

df4 <- read.csv("./data/gender-gap-education-levels.csv") %>% 
  na.omit() %>%
  filter(Year >= 2015) %>%
  filter_to_most_recent_year() %>%
  select(-Entity, -Year)

df5 <- read.csv("./data/learning-adjusted-years-of-school-lays.csv") %>% 
  filter(Year >= 2015) %>%
  filter_to_most_recent_year() %>%
  select(-Entity, -Year)

df6 <- read.csv("./data/expected-years-of-schooling-vs-share-in-extreme-poverty.csv") %>% 
  na.omit() %>%
  filter(Year >= 2015) %>%
  filter_to_most_recent_year() %>%
  select(-Entity, -Year, -Continent)

df7 <- read.csv("./data/learning-outcomes-vs-gdp-per-capita.csv") %>% 
  na.omit() %>%
  filter(Year >= 2010) %>%
  filter_to_most_recent_year() %>%
  select(-Entity, -Year, -World.Region.according.to.the.World.Bank,-Population..historical.estimates.)

df8 <- read.csv("./data/world-regions-according-to-the-world-bank.csv") %>%
  filter(Code != "") %>%
  select(-Year)


# Datensätze zusammenführen
merged_df <- df8 %>%
  left_join(df1, by = "Code") %>%
  left_join(df2, by = "Code") %>%
  left_join(df3, by = "Code") %>%
  left_join(df4, by = "Code") %>%
  left_join(df5, by = "Code") %>%
  left_join(df6, by = "Code") %>%
  left_join(df7, by = "Code")


# NAs pro Column
na_count <- sapply(merged_df, function(x) sum(is.na(x)))
na_count


# Funktion
remove_columns_with_nas <- function(data, threshold) {
  na_counts <- sapply(data, function(x) sum(is.na(x)))
  data <- data[, na_counts <= threshold]
  return(data)
}

# Entferne Spalten mit mehr als 100 NAs
df_clean <- remove_columns_with_nas(merged_df, threshold = 75)
na_count <- sapply(df_clean, function(x) sum(is.na(x)))
na_count



# Nur Länder mit weniger als 3 NAs
df_clean <- df_clean[rowSums(is.na(df_clean)) <= 2, ]


# Funktion
replace_na_with_mean <- function(df, column_name) {
  column_mean <- mean(df[[column_name]], na.rm = TRUE)
  df[[column_name]][is.na(df[[column_name]])] <- column_mean
  return(df)
}


# NA-Werte in Columns durch den Mittelwert ersetzen
df_clean <- replace_na_with_mean(df_clean, "Combined.total.net.enrolment.rate..primary..both.sexes")
df_clean <- replace_na_with_mean(df_clean, "Total.net.enrolment.rate..primary..gender.parity.index..GPI.")
df_clean <- replace_na_with_mean(df_clean, "Learning.Adjusted.Years.of.School")
df_clean <- replace_na_with_mean(df_clean, "Harmonized.Test.Scores")
df_clean <- replace_na_with_mean(df_clean, "GDP.per.capita..PPP..constant.2017.international...")


# Preprocessing
df_clean <- na.omit(df_clean)


# Save Dataframe
#file_path <- "./clustering/preprocessed_dataset.csv"
#write.csv(df_clean, file = file_path, row.names = FALSE)
