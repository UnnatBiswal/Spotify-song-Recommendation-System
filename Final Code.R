library(tidyverse)
library(corrplot)
library(factoextra)
library(funModeling)
library(dplyr)

# To ensure reproducibility of certain results
set.seed(21281078)

# Reading the Dataset
spotify <- read.csv("dataset.csv")

# Remove duplicates based on track_id
spotify <- spotify[!duplicated(spotify$track_id), ]

# Scale a certain attribute
spotify <- spotify %>% mutate(duration_min = duration_ms / 60000)
table(spotify$duration_min)

# Correlation Plot of Different Attributes of music in the dataset
df1 <- select(spotify, popularity, danceability, energy, loudness, speechiness, acousticness, instrumentalness, liveness, valence, tempo)
corrplot(cor(df1))

# Histogram of the different Attributes
spotify_hist <- select(spotify, duration_min, danceability, loudness, energy, speechiness, acousticness, instrumentalness, liveness, valence, tempo)
plot_num(spotify_hist)

# To see the different Genres present in the dataset
Spotify_Genres <- unique(spotify[, "track_genre"])
print(Spotify_Genres)

# Plots of Different Factors/Attributes on which Music Taste depends on

# Filtering the dataset according to personal taste in genre
filtered_spotify <- filter(spotify, track_genre %in% c("pop", "k-pop", "indian", "heavy-metal", "indie", "jazz"))

# Variation of Valence between genres
boxplot(valence ~ track_genre, data = filtered_spotify,
        main = "Variation of Valence between genres",
        xlab = "Valence",
        ylab = "Genre",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)

# Variation of Danceability between genres
boxplot(danceability ~ track_genre, data = filtered_spotify,
        main = "Variation of Danceability between genres",
        xlab = "Danceability",
        ylab = "Genre",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)

# Variation of Energy between genres
boxplot(energy ~ track_genre, data = filtered_spotify,
        main = "Variation of Energy between genres",
        xlab = "Energy",
        ylab = "Genre",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)

# Bar Graphs of some Attributes

# Bar Graph of Energy w.r.t Count
spotify$cut_energy <- cut(spotify$energy, breaks = 10)
ggplot(spotify, aes(x = cut_energy)) +
  geom_bar(width = 0.2) +
  scale_x_discrete(name = "Energy")

# Bar Graph of Danceability w.r.t Count
spotify$cut_danceability <- cut(spotify$danceability, breaks = 10)
ggplot(spotify, aes(x = cut_danceability)) +
  geom_bar(width = 0.2) +
  scale_x_discrete(name = "Danceability")

# Bar Graph of Speechiness w.r.t Count
spotify$cut_speechiness <- cut(spotify$speechiness, breaks = 10)
ggplot(spotify, aes(x = cut_speechiness)) +
  geom_bar(width = 0.2) +
  scale_x_discrete(name = "Speechiness")

# Bar Graph of Valence w.r.t Count
spotify$cut_valence <- cut(spotify$valence, breaks = 10)
ggplot(spotify, aes(x = cut_valence)) +
  geom_bar(width = 0.2) +
  scale_x_discrete(name = "Valence")

# SPOTIFY RECOMMENDATION SYSTEM

# First, scale the different variables in the Dataset for further calculation
spotify_scaled <- scale(select(spotify, popularity, danceability, energy, instrumentalness, liveness, valence))

# Finding The Optimal Number Of Clusters
elbow_method <- function(data, max_clusters = 10) {
  within_ss_dist <- numeric(max_clusters - 1)
  
  for (i in 2:max_clusters) {
    within_ss_dist[i - 1] <- sum(kmeans(data, centers = i)$withinss)
  }
  
  plot(2:max_clusters, within_ss_dist, type = "o", xlab = "Number of Clusters", ylab = "Within groups sum of squares", pch = 18)
}

elbow_method(spotify_scaled)

# Perform k-means clustering
spotify_kmeans <- kmeans(spotify_scaled, centers = 7)

# Display cluster sizes
cluster_sizes <- spotify_kmeans$size
print(cluster_sizes)

# Display cluster centers
cluster_centers <- spotify_kmeans$centers
print(cluster_centers)

# Assign cluster labels to the original dataset
spotify$cluster <- spotify_kmeans$cluster

# Visualize the clusters
fviz_cluster(spotify_kmeans, data = spotify_scaled)

# Between Sum of Squares betweens : signifies the ‘length’ from each centroid from each cluster to the global sample mean.
# Total Sum of Squares totss : signifies the ‘length’ from each observation to global sample mean.
# Down below is the ratio of the factors defined above:
((spotify_kmeans$betweenss) / (spotify_kmeans$totss)) * 100

# Information on Clusters
cluster_info <- group_by(spotify, cluster) %>% summarise_all(mean) %>% select(cluster, acousticness, danceability, energy, instrumentalness, speechiness, valence, liveness)

# Info on Personal Favourite Music of the User (from music inside the Database)
User_Choice<-spotify %>% filter(track_name == "Can't Help Falling In Love", artists == "Kina Grannis")%>% select(cluster, track_name,track_genre)
# Demanding Music with similar attributes as a personal favourite and a genre that the user would like to try
recommendation <- filter(spotify, cluster == 2, track_genre == "acoustic") %>% sample_n(5)%>% select(cluster,artists,track_name,track_genre)

