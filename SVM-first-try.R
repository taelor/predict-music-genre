install.packages("readr")
install.packages("e1071")

library(e1071)

source("dataCleaning.R")

data <- read.csv("data/music_genre.csv")

data <- cleanData(data)

summary(data$speechiness)

#data <-subset(data, select = c(acousticness, danceability, energy, instrumentalness, liveness, speechiness, valence, music_genre))

# labels need to be a factor for SVM
data$music_genre <- as.factor(data$music_genre)

n <- nrow(data)

ntrain <- round(n*0.75)

set.seed(311)

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

print(prediction)
str(prediction)
class(prediction)

levels = levels(prediction)
table = table(test_data$music_genre, prediction)
table
sum(table[,1])

432+25+29+16+172+65+136+72+55+266
432+37+58+20+92+72+67+57+48+110


# these numbers are the diagonal representing the correctly predicted genres
# 
# 41.84% - Not Great, but a good first stab at it.
#
(432+869+629+1005+709+697+573+660+444+813)/nrow(test_data)*100
