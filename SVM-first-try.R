
install.packages("e1071")

library(e1071)

data <- read.csv("data/music_genre.csv")

data <- cleanData(data)

good_duration <- data[data$duration_ms != -1, ]

mean(good_duration$duration_ms)

for (value in unique(good_duration$music_genre)) {
  good_duration
  
  
  good_duration[good_duration$music_genre == value, ]
  print(value)
  print(count)
}



data <-subset(data, select = c(acousticness, danceability, energy, instrumentalness, liveness, speechiness, valence, music_genre))

# remove those pesky NAs that caused me strife for about an hour.
# protip: is.numeric(data$acousticness) will return true even if NAs exist,
#   use summary(data$acousticness) to see how many NAs exist.
data <- na.omit(data)

# labels need to be a factor for SVM
data$music_genre <- as.factor(data$music_genre)

n <- nrow(data)

ntrain <- round(n*0.75)

set.seed(420)

train_indexes <- sample(n, ntrain)

train_data <- data[train_indexes,]
test_data <- data[-train_indexes,]

model <- svm(music_genre ~ ., data = train_data,
             method="C-classification", 
             kernel="radial",
             cost=100
)

print(model)
summary(model)

prediction <- predict(model, test_data)

table(test_data$music_genre, prediction)

# these numbers are the diagonal representing the correctly predicted genres
# 
# 41.84% - Not Great, but a good first stab at it.
#
(298+369+432+1057+741+657+805+487+222+162)/nrow(test_data)*100