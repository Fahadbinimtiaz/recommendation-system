## 1 Code provided by Edx

##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

###############################
# 2 Code Used in this project #
###############################

# Loading all needed libraries
if(!require(scales)) install.packages("scales") 
library(scales)

# Explore the structure of the data set
str(edx)
head(edx)

# Summary statistics of the ratings
summary(edx$rating)

# Check for any missing values in training data set 
total_missing <- sum(is.na(edx))

# Number of unique users and movies
num_users <- n_distinct(edx$userId)
num_movies <- n_distinct(edx$movieId)

cat("Total missing values in edx dataset: ", total_missing, "\n")
cat("Number of unique users: ", num_users, "\n")
cat("Number of unique movies: ", num_movies, "\n")

# Identify Top 10 movies with the greatest number of ratings
top_movies <- edx %>%
  group_by(title) %>%
  summarize(rating_count = n()) %>%
  arrange(desc(rating_count)) %>%
  slice(1:10)
cat("Top Ten Most Rated Movies are: ", "\n")
print(top_movies)

#Top 5 Movie Ratings with counts
top_ratings <- edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1:5)
cat("Top Five Ratings are: ", "\n")
print(top_ratings)

# Check the prevalence of half-star ratings
half_star_counts <- edx %>%
  filter(rating %% 1 != 0) %>%
  group_by(rating) %>%
  summarize(count = n())
cat("Half-star ratings are less common than whole-star ratings: ",
    nrow(half_star_counts) < n_distinct(edx$rating), "\n")

# Top 10 active users
top_users <- edx %>%
  group_by(userId) %>%
  summarize(rating_count = n()) %>%
  arrange(desc(rating_count)) %>%
  slice(1:10)
cat("Top Ten Active users are: ", "\n")
print(top_users)

# Split genres into separate rows (It may take couple of minutes to process the data)
edx_genres <- edx %>%
  separate_rows(genres, sep = "\\|")

# Count the number of ratings per genre
genre_counts <- edx_genres %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
cat("Genre Counts are: ", "\n")
print(genre_counts)

# Distribution of Ratings
edx %>% group_by(rating) %>% summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_bar(stat = "identity") +
  ggtitle("Distribution of Movie Ratings") +
  xlab("Rating") + ylab("Count") + scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(n.breaks = 10)

# Bar Plot genre distribution
genre_counts %>%
  ggplot(aes(x = reorder(genres, count), y = count)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  ggtitle("Distribution of Movies by Genre") +
  xlab("Genre") +
  ylab("Number of Ratings") +
  scale_y_continuous(labels = comma)

# Bar Plot average rating per genre
genre_avg <- edx_genres %>%
  group_by(genres) %>%
  summarize(avg_rating = mean(rating))

genre_avg %>%
  ggplot(aes(x = reorder(genres, avg_rating), y = avg_rating)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  ggtitle("Average Rating per Genre") +
  xlab("Genre") +
  ylab("Average Rating")

# Convert timestamp into readable format for analysis
edx_year <- edx %>% mutate(year = as.POSIXct(timestamp, origin="1970-01-01") %>% format("%Y") %>% as.integer())

# Line plot of number of ratings per year
yearly_ratings <- edx_year %>%
  group_by(year) %>%
  summarize(count = n())

ggplot(yearly_ratings, aes(x = year, y = count)) +
  geom_line(color = "blue") +
  ggtitle("Number of Ratings per Year") +
  xlab("Year") + ylab("Number of Ratings")

# Plot average ratings over years
ratings_by_year <- edx_year %>%
  group_by(year) %>%
  summarize(average_rating = mean(rating))

ratings_by_year %>%
  ggplot(aes(x = year, y = average_rating)) +
  geom_line(color = "blue") +
  geom_point(color = "green") +
  ggtitle("Average Movie Ratings Over Years") +
  xlab("Year") +
  ylab("Average Rating")

# Define RMSE function 
RMSE <- function(true_ratings, predicted_ratings) 
{ sqrt(mean((true_ratings - predicted_ratings)^2)) } 

# Calculate the global average rating
mu_hat <- mean(edx$rating)

# Calculate RMSE for the baseline model
baseline_rmse <- RMSE(edx$rating, mu_hat)
cat("Baseline RMSE (Global Average): ", baseline_rmse, "\n")

final_results <- data.frame(model="Baseline Model", RMSE=baseline_rmse)

# Compute movie effects (b_i)
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu_hat))

# Predict ratings using movie effect model
predicted_ratings_movie <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  mutate(pred = mu_hat + b_i) %>%
  pull(pred)

# Calculate RMSE for the movie effect model
movie_effect_rmse <- RMSE(edx$rating, predicted_ratings_movie)
cat("RMSE for the movie effect model: ", movie_effect_rmse, "\n")

final_results <- final_results %>% add_row(model="Movie-Effects Model", RMSE=movie_effect_rmse)

# Compute user effects (b_u)
user_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat - b_i))

# Predict ratings using movie + user effects
predicted_ratings_user <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu_hat + b_i + b_u) %>%
  pull(pred)

# Calculate RMSE for movie + user effect model
user_movie_effect_rmse <- RMSE(edx$rating, predicted_ratings_user)
cat("RMSE for the Movie + User effect model: ", user_movie_effect_rmse, "\n")

final_results <- final_results %>% add_row(model="Movie-User-Effects Model", RMSE=user_movie_effect_rmse)

# Compute Genre effect (b_g)
genre_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu_hat - b_i - b_u))

# Predict ratings using the model
predicted_ratings_bug <- edx %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = c("genres" = "genres")) %>%
  mutate(pred = mu_hat + b_i + b_u + b_g) %>%
  pull(pred)

# Calculate RMSE for movie + user + genre effect model
user_movie_genre_effect_rmse <- RMSE(edx$rating, predicted_ratings_bug)
cat("RMSE for the movie + user + genre effect model: ", user_movie_genre_effect_rmse, "\n")

final_results <- final_results %>% add_row(model="Movie-User-Genre-Effects Model", RMSE=user_movie_genre_effect_rmse)

# Define a table of lambdas
lambdas <- seq(0, 5, 0.1)

rmses <- sapply(lambdas, function(lambda) {
  # Calculate the average by user
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu_hat) / (n() + lambda))
  
  # Calculate the average by user
  b_u <- edx %>%
    left_join(b_i, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu_hat) / (n() + lambda))
  
  # Calculate the average by Genres
  b_u_g <- edx %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    group_by(genres) %>%
    summarize(b_u_g = sum(rating - b_i - mu_hat - b_u) / (n() + lambda))
  
  # Compute the predicted ratings on edx dataset
  predicted_ratings <- edx %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(b_u_g, by='genres') %>%
    mutate(pred = mu_hat + b_i + b_u + b_u_g) %>%
    pull(pred)
  
  # Predict the RMSE on the edx set
  return(RMSE(edx$rating, predicted_ratings))
})

# Put all values in a data frame
df <- data.frame(RMSE = rmses, lambdas = lambdas)

# Plotting the graph
ggplot(df, aes(lambdas, rmses)) +
  theme_classic()  +
  geom_point() +
  labs(title = "RMSEs vs Lambdas - Regularized Movie+User+Genre Model",
       y = "RMSEs",
       x = "lambdas")

# Finding the min lambda value and its respective RMSE
min_lambda <- lambdas[which.min(rmses)]

# Calculate RMSE for Regularized movie + user + genre effect model
reg_user_movie_genre_effect_rmse <- min(rmses)
cat("RMSE for the Regularized movie + user + genre effect model: ", reg_user_movie_genre_effect_rmse, "\n")

final_results <- final_results %>% add_row(model="Regularized Movie-User-Genre-Based Model", RMSE=reg_user_movie_genre_effect_rmse)        

# Evaluating Best Model based on Lowest RMSE value
Best_model <- final_results$model[which.min(final_results$RMSE)]
Best_rmse <- min(final_results$RMSE)
Best_lambda <- min_lambda

# Predict Ratings on Final Holdout Test Set
final_b_i <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu_hat) / (n() + Best_lambda))
final_b_u <- edx %>%
  left_join(final_b_i, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu_hat) / (n() + Best_lambda))
final_b_u_g <- edx %>%
  left_join(final_b_i, by='movieId') %>%
  left_join(final_b_u, by='userId') %>%
  group_by(genres) %>%
  summarize(b_u_g = sum(rating - b_i - mu_hat - b_u) / (n() + Best_lambda))

# Compute the predicted ratings on final holdout test dataset

final_predicted_ratings <- final_holdout_test %>%
  left_join(final_b_i, by='movieId') %>%
  left_join(final_b_u, by='userId') %>%
  left_join(final_b_u_g, by='genres') %>%
  mutate(pred = mu_hat + b_i + b_u + b_u_g) %>%
  pull(pred)

# Predict the RMSE on the edx set
final_rmse_final_test_set <- RMSE(final_holdout_test$rating, final_predicted_ratings)
cat("Final RMSE value on Final Holdout Test Set: ", final_rmse_final_test_set, "\n")