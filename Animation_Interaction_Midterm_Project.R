## Loading librarys ##

library(readr)
library(tidyverse)
library(gifski)
library(ggplot2)
library(gganimate)
library(dplyr)
#Only used for masking count function which works slightly different
library(plyr)


## Importing Data ##
ImdbTitleBasics <- read_csv("ImdbTitleBasics.csv")
ImdbTitleRatings <- read_csv("ImdbTitleRatings.csv")


## Joining Datasets ##
ImdbTitleBasicsAndRatings = inner_join(ImdbTitleBasics, ImdbTitleRatings, by = c("tconst" = "tconst"))


## Preparing Data for plotting
# Filters for the dataset #
genre_filter = c("Documentary", "Drama", "Fantasy", "Action", "Romance", "Comedy", "Adventure", "Crime", "Sci", "Thriller")
data_entry_filter = "movie" # Use: "short" ||"movie" || "tvShort" || "tvSeries" || "tvMovie" || "tvEpisode" || "tvMiniSeries" || "tvSpecial" || "video" || "videoGame"



# Mutate data for "Average_Rating_By_Genre" and "Average_Rating_Vs_Number_Of_Films_Per_Year"
movies_with_rating = transmute(
  ImdbTitleBasicsAndRatings, 
  year = startYear,
  genre = genres,
  titleType = titleType,
  rating = averageRating,
  votes = numVotes
) %>% filter(titleType == data_entry_filter) %>% 
  separate_rows(genre) %>% filter(genre %in% genre_filter) %>% 
  drop_na() %>%
  count()

movies_with_stats = transmute(
  movies_with_rating,
  year = year,
  genre = genre,
  # Calculate ratios to get relativ propotions of data (normalized to 0.x of 1)
  freq_ratio = ave(freq, year, genre, FUN=sum)/ave(freq, year, FUN=sum),
  vote_ratio = ave(votes, year, genre, FUN=sum)/ave(votes, year, FUN=sum),
  rating_avg = ave(rating, year, genre, FUN=mean)
) %>%
  count()

# Mutate data for "Distribution_Of_Genres"
movies_without_rating = transmute(
  ImdbTitleBasics, 
  year = startYear,
  genre = genres,
  titleType = titleType,
) %>% filter(titleType == data_entry_filter) %>% 
  separate_rows(genre) %>% filter(genre %in% genre_filter) %>% 
  drop_na() %>%
  count()



## Average_Rating_By_Genre -- Plot ##
Average_Rating_By_Genre = ggplot(movies_with_stats, aes(x=genre, y=rating_avg, fill=genre)) + 
  geom_bar(stat='identity') +
  theme_bw() +
  # gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'Genre', y = 'Average rating')



## Average_Rating_Vs_Number_Of_Films_Per_Year -- Plot ##
Average_Rating_Vs_Number_Of_Films_Per_Year = ggplot(movies_with_stats, aes(freq_ratio, rating_avg, size = vote_ratio, color = genre)) +
  #geom_point() +
  geom_point(aes(size = vote_ratio), alpha = 0.7) +
  scale_x_log10() +
  theme_bw() +
  # gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'Number of movies per genre and year', y = 'Average rating')



## Distribution_Of_Genres -- Plot ##
Distribution_Of_Genres = ggplot(movies_without_rating, aes(x=year, y=freq, group=genre, color=genre)) +
  geom_line() +
  geom_point() +
  ggtitle("Distribution of movie genres over the past 100 years") +
  ylab("Number of films in category") + 
  xlab("Year") 



## Extending plot with gganimate-specific elements and smoothing Animation ##
Animation_Average_Rating_By_Genre = Average_Rating_By_Genre + 
  transition_time(year) +
  ease_aes('sine-in-out') +
  enter_fade() + 
  exit_shrink()

Animation_Average_Rating_Vs_Number_Of_Films_Per_Year = Average_Rating_Vs_Number_Of_Films_Per_Year + 
  transition_time(year) +
  ease_aes('linear') +
  enter_fade() + 
  exit_shrink()

Animation_Distribution_Of_Genres = Distribution_Of_Genres +
  transition_reveal(year) +
  enter_fade() + 
  exit_shrink()


# Render animation in RStudio and save it afterwards as gif
animate(Animation_Average_Rating_By_Genre, duration = 20, fps = 20, width = 1000, height = 600, renderer = gifski_renderer())
anim_save("Average_Rating_By_Genre.gif")

animate(Animation_Average_Rating_Vs_Number_Of_Films_Per_Year, duration = 30, fps = 20, width = 1000, height = 600, renderer = gifski_renderer())
anim_save("Average_Rating_Vs_Number_Of_Films_Per_Year.gif")

animate(Animation_Distribution_Of_Genres, duration = 10, fps = 20, width = 1000, height = 600, renderer = gifski_renderer())
anim_save("Distribution_Of_Genres.gif")


