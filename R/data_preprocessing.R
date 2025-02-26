library(tidyverse)
library(ggplot2)
library(dplyr)
library(plotly)
library(htmlwidgets)
library(knitr)

netflix_data <- read.csv("Data/Raw_Data/Netflix TV Shows and Movies.csv")


head(netflix_data)

knitr::kable(summary(netflix_data))

str(netflix_data)

knitr::kable(colSums(is.na(netflix_data)))



# Data cleaning
netflix_data <- netflix_data %>%
  mutate(imdb_votes = ifelse(is.na(imdb_votes), "No Votes", imdb_votes),
         age_certification = ifelse(age_certification == "", "UnRated", age_certification))

View(netflix_data)

write.csv(netflix_data, "Data/Processed_Data/cleaned_netflix_data.csv", row.names = FALSE)