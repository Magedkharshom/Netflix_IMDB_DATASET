
netflix_data <- read.csv("Data/Processed_Data/cleaned_netflix_data.csv")


fig_1a <- ggplot(netflix_data, aes(x = imdb_score, fill = type)) +
  geom_histogram(binwidth = 0.5, position = "dodge", alpha = 0.7) +
  labs(title = "Distribution of IMDb Scores on Netflix",
       x = "IMDb Scores",
       y = "Number of Titles") +
  theme_minimal()
fig_1a

type_counts <- table(netflix_data$type)

fig_1b <- plot_ly(
  labels = names(type_counts),
  values = as.vector(type_counts),
  type = "pie"
) %>%
  layout(title = "Distribution of Types")
fig_1b



fig_1c <- ggplot(netflix_data, aes(x = runtime, y = imdb_score)) +
  geom_point() +
  labs(title = "IMDb Scores vs. Runtime",
       x = "Runtime",
       y = "IMDb Score") +
  theme_minimal()
fig_1c




fig_1d <- netflix_data %>%
  group_by(type) %>%
  summarise(average_score = mean(imdb_score, na.rm = TRUE)) %>%
  ggplot(aes(x = type, y = average_score, fill = type)) +
  geom_bar(stat = "identity") +
  labs(title = "Average IMDb Score by Type", x = "Type", y = "Average IMDb Score")
fig_1d


fig_1e <- plot_ly(netflix_data, x = ~type, y = ~imdb_score, type = 'box'
) %>%
  layout(title = 'Boxplot of IMDb Scores by Type',
         xaxis = list(title = 'Type'),
         yaxis = list(title = 'IMDb Score'))
fig_1e


threshold_score <- 7.1
margin_of_error <- 0.3 

suppressWarnings({
  netflix_data$imdb_votes <- as.numeric(as.character(netflix_data$imdb_votes))
  netflix_data$imdb_score <- as.numeric(as.character(netflix_data$imdb_score))
})

top_scores <- netflix_data %>%
  filter(between(imdb_score, threshold_score - margin_of_error, threshold_score + margin_of_error)) %>%
  arrange(desc(imdb_votes), desc(imdb_score)) %>%
  head(16) %>%
  mutate(hover_text = paste("Year:", release_year, 
                            "<br>Type:", type, 
                            "<br>IMDb Score:", imdb_score, 
                            "<br>Votes:", format(imdb_votes, big.mark = ",")))

# Create the treemap plot
fig_1f <- plot_ly(
  data = top_scores,
  labels = ~title,
  parents = rep("", nrow(top_scores)),
  type = 'treemap',
  hoverinfo = 'text',
  hovertemplate = '%{customdata}<extra></extra>',
  customdata = ~hover_text,
  textfont = list(size = 16)
) %>%
  layout(
    title = "Top Average IMDb Scores by Vote Count within Score Range",
    margin = list(l = 10, r = 10, b = 10, t = 40)
  )

# Display the plot
fig_1f



audience_target_analysis <- netflix_data %>%
  group_by(age_certification, type) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(desc(count))

# Visualize the number of titles targeting each age certification, split by content type
fig_2a <-  ggplot(audience_target_analysis, aes(x = age_certification, y = count, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Target Audience Analysis by Age Certification and Content Type",
       x = "Age Certification", y = "Number of Titles",
       fill = "Content Type") +
  theme_minimal()
fig_2a



fig_3a <- netflix_data %>%
  group_by(release_year) %>%
  summarise(
    average_score = mean(imdb_score, na.rm = TRUE),
    se = sd(imdb_score, na.rm = TRUE) / sqrt(n())  # Standard error
  ) %>%
  ggplot(aes(x = release_year, y = average_score)) +
  geom_ribbon(aes(ymin = average_score - se, ymax = average_score + se), alpha = 0.2) +
  geom_line() +
  labs(title = "Trends of IMDb Scores Over Years", x = "Release Year", y = "Average IMDb Score") +
  theme_minimal()
fig_3a

fig_3b <- plot_ly(netflix_data, x = ~imdb_votes, y = ~imdb_score, type = 'scatter', mode = 'markers',
                  marker = list(size = 5, opacity = 0.5)) %>%
  layout(title = 'IMDb Scores vs. IMDb Votes',
         xaxis = list(title = 'IMDb Votes', type = "log"), # Using log scale for X-axis
         yaxis = list(title = 'IMDb Score'))
fig_3b

fig_3c <- ggplot(netflix_data, aes(x = release_year, y = imdb_score, group = type, color = type)) +
  geom_line(aes(group = interaction(release_year, type)), alpha = 0.3) +  # Use interaction for grouping in geom_line
  geom_smooth(se = FALSE, method = "loess",  formula = y ~ x) +  # Add LOESS smoothed trend line
  labs(title = "IMDb Scores Over the Years by Type", x = "Release Year", y = "IMDb Score") +
  theme_minimal() +
  theme(legend.position = "bottom")
fig_3c




ggsave("Results/Figures/fig_1a.png", fig_1a, width = 10, height = 6)
saveWidget(fig_1b,"Results/Figures/fig_1b.html")
ggsave("Results/Figures/fig_1c.png", fig_1c, width = 10, height = 6)
ggsave("Results/Figures/fig_1d.png", fig_1d, width = 10, height = 6)
saveWidget(fig_1e,"Results/Figures/fig_1e.html")
saveWidget(fig_1f,"Results/Figures/fig_1f.html")



ggsave("Results/Figures/fig_2a.png", fig_2b, width = 10, height = 6)
ggsave("Results/Figures/fig_2b.png", fig_2b, width = 10, height = 6)

ggsave("Results/Figures/fig_3a.png", fig_3a, width = 10, height = 6)
saveWidget(fig_3b,"Results/Figures/fig_3b.html")
ggsave("Results/Figures/fig_3c.png", fig_3c, width = 10, height = 6)
