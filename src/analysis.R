library(tidyverse)
library(tidytext)
library(tm)
library(ggplot2)
library(viridis)
library(wordcloud2)
library(maps)
library(dplyr)

# Import dataset and glimpse
ufo_data <- read_csv("../data/ufo_data.csv")
glimpse(ufo_data)

# Normalize
ufo_data_clean <- drop_na(ufo_data)
ufo_data_clean$latitude <- as.numeric(ufo_data_clean$latitude)
ufo_data_clean$longitude <- as.numeric(ufo_data_clean$longitude)

# Clean comments
ufo_data_clean$comments = gsub("[[:punct:]]|[[:digit:]]|(http[[:alpha:]]*:\\/\\/)", "", ufo_data_clean$comments)
ufo_data_clean$comments = str_squish(ufo_data_clean$comments)
ufo_data_clean$comments = tolower(ufo_data_clean$comments)
ufo_data_clean$comments = removeWords(ufo_data_clean$comments, stopwords("english"))
ufo_data_clean$comments = str_squish(ufo_data_clean$comments)

# Transform
# Add year
ufo_data_clean$year <- substr(ufo_data_clean$`date posted`, start = nchar(ufo_data_clean$`date posted`) - 3, stop = nchar(ufo_data_clean$`date posted`))
ufo_data_clean$year <- as.numeric(ufo_data_clean$year)

# Add month
ufo_data_clean$month <- substr(ufo_data_clean$`date posted`, start = 0, stop = 2)
ufo_data_clean$month <- gsub("/", "", ufo_data_clean$month)
ufo_data_clean$month <- as.numeric(ufo_data_clean$month)
number_to_month <- function(number) {
  if (number < 3)
    number = "winter"
  else if (number < 6)
    number = "spring"
  else if (number < 9)
    number = "summer"
  else if (number < 12)
    number = "fall"
  else
    number = "winter"
}
ufo_data_clean$month <- lapply(ufo_data_clean$month, number_to_month)
ufo_data_clean$month <- as.character(ufo_data_clean$month)

# Make less shapes (since a lot of them are basically the same (example: light = flash))
ufo_data_clean$shape[ufo_data_clean$shape == "flash"] <- "light"
ufo_data_clean$shape[ufo_data_clean$shape == "flare"] <- "light"
ufo_data_clean$shape[ufo_data_clean$shape == "fireball"] <- "light"
ufo_data_clean$shape[ufo_data_clean$shape == "cylinder"] <- "oval-like"
ufo_data_clean$shape[ufo_data_clean$shape == "oval"] <- "oval-like"
ufo_data_clean$shape[ufo_data_clean$shape == "cigar"] <- "oval-like"
ufo_data_clean$shape[ufo_data_clean$shape == "cone"] <- "oval-like"
ufo_data_clean$shape[ufo_data_clean$shape == "teardrop"] <- "oval-like"
ufo_data_clean$shape[ufo_data_clean$shape == "egg"] <- "oval-like"
ufo_data_clean$shape[ufo_data_clean$shape == "triangle"] <- "triangle-like"
ufo_data_clean$shape[ufo_data_clean$shape == "delta"] <- "triangle-like"
ufo_data_clean$shape[ufo_data_clean$shape == "pyramid"] <- "triangle-like"
ufo_data_clean$shape[ufo_data_clean$shape == "diamond"] <- "triangle-like"
ufo_data_clean$shape[ufo_data_clean$shape == "circle"] <- "circle-like"
ufo_data_clean$shape[ufo_data_clean$shape == "sphere"] <- "circle-like"
ufo_data_clean$shape[ufo_data_clean$shape == "disk"] <- "circle-like"
ufo_data_clean$shape[ufo_data_clean$shape == "round"] <- "circle-like"
ufo_data_clean$shape[ufo_data_clean$shape == "changed"] <- "multiple shapes"
ufo_data_clean$shape[ufo_data_clean$shape == "changing"] <- "multiple shapes"
ufo_data_clean$shape[ufo_data_clean$shape == "formation"] <- "multiple shapes"
ufo_data_clean$shape[ufo_data_clean$shape == "rectangle"] <- "other"
ufo_data_clean$shape[ufo_data_clean$shape == "chevron"] <- "other"
ufo_data_clean$shape[ufo_data_clean$shape == "cross"] <- "other"
ufo_data_clean$shape[ufo_data_clean$shape == "hexagon"] <- "other"
ufo_data_clean$shape[ufo_data_clean$shape == "crescent"] <- "other"

                         
# Other helpful dataframes
count_years <- as.data.frame(table(ufo_data_clean$year))
colnames(count_years) <- c("year", "amount")
count_years$year <- as.character(count_years$year)
count_years$year <- as.numeric(count_years$year)

count_years_month <- as.data.frame(table(ufo_data_clean$year, ufo_data_clean$month))
colnames(count_years_month) <- c("year", "month", "amount")
count_years_month$year <- as.character(count_years_month$year)
count_years_month$year <- as.numeric(count_years_month$year)
count_years_month$month <- as.character(count_years_month$month)

# Get summary
summary(ufo_data_clean)

# Sorted plot with sightings by state
ufo_data_clean$state <- fct_rev(fct_infreq(ufo_data_clean$state))
ggplot(ufo_data_clean, aes(x = state, fill = state)) +
  geom_bar() + 
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(
    title = "Amount of UFO Sightings by State",
    x = "State", y = "Amount of Sightings",
    fill = "States"
  ) +
  theme(axis.text.x = element_blank()) +
  scale_fill_viridis_d()

# Get plot with amount of sightings by year
ufo_data_clean$country <- fct_infreq(ufo_data_clean$country)
ggplot(ufo_data_clean, aes(x = year, color = country)) +
  geom_point(stat = "count") +
  geom_smooth(stat = "count") +
  scale_x_continuous(breaks = ufo_data_clean$year) +
  labs(
    title = "Amount of Sightings for Each Year in Recorded Countries",
    x = "Year", y = "Amount of Sightings",
    fill = "Countries"
  ) +
  scale_color_viridis(discrete = TRUE)

# Pie plot of shapes of ufo
ufo_data_clean$shape <- fct_rev(fct_infreq(ufo_data_clean$shape))
ggplot(ufo_data_clean, aes(x = "", fill = shape)) +
  geom_bar() +
  coord_polar("y", start = 0) + 
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(
    title = "Shapes of Reported UFOs",
    fill = "Shapes"
  ) +
  scale_fill_viridis(discrete = TRUE)

# Bar plot for amount of sightings each time of year
ufo_data_clean$month <- fct_rev(fct_infreq(ufo_data_clean$month))
ggplot(ufo_data_clean, aes(x = month, fill = month)) +
  geom_bar() + 
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(
    title = "Amount of UFO Sightings each Time of Year",
    x = "Time of Year", y = "Amount of Sightings",
    fill = "Time of Year"
  ) +
  theme(axis.text.x = element_blank()) +
  scale_fill_viridis_d()

# Heat map of sightings each month each year
ggplot(count_years_month, aes(x = month, y = year, fill = amount)) +
  geom_tile(stat = "identity") +
  scale_y_continuous(breaks = count_years_month$year) +
  labs(
    title = "Amount of UFO Sightings each Time of Year and Year",
    x = "Time of Year", y = "Year",
    fill = "Amount of Sightings"
  ) +
  scale_fill_viridis()


# Plot with coordinates
world <- map_data("world")
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "grey", fill = "black", size = 0.1
  ) +
  geom_point(data = ufo_data_clean, aes(y = latitude, x = longitude), color = "yellow", size = 0.3) +
  labs(
    title = "UFO Reports on the World Map",
    x = "Latitude", y = "Longitude",
    fill = "Report Made"
  )

# Make a word cloud from comments column
ufo_words <-  ufo_data_clean %>%
  select(comments) %>%
  unnest_tokens(word, comments)
words <- ufo_words %>% count(word, sort=TRUE)
figPath <- "alien.png"
wordcloud2(data = words, figPath = figPath, size = 1, color = plasma)

# Show linear regression model
ggplot(count_years, aes(x = year, y = amount, color = "amount")) +
  geom_point(color = "black") +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = count_years$year) +
  labs(
    title = "Linear Regression Model for Predicting Amount of Sightings Given a Year",
    x = "Year", y = "Amount of Sightings",
  ) +
  theme(legend.position = "none") +
  scale_color_viridis(discrete = TRUE)

# Make a model that predicts amount of sightings given a year
model1 <- lm(amount ~ year, data = count_years)
predicting_sightings <- data.frame(year = c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024))
predicting_sightings$amount <- predict(model1, newdata = predicting_sightings)
count_years_before_predicted <- union(count_years, predicting_sightings)
ggplot(count_years_before_predicted, aes(x = year, y = amount, color = "amount")) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = count_years_before_predicted$year) +
  labs(
    title = "Added Predicted Sightings (from 2015 to 2024) Using Linear Regressiong",
    x = "Year", y = "Amount of Sightings",
  ) +
  theme(legend.position = "none") +
  scale_color_viridis(discrete = TRUE)