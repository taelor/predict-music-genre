install.packages("e1071")

library(e1071)

source("dataCleaning.R")
source("f1_score.R")

data <- read.csv("data/music_genre.csv")

data <- cleanData(data)

summary(data$speechiness)

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

prediction <- predict(model, test_data)

levels = levels(prediction)
table = table(test_data$music_genre, prediction)

f1_scores(levels, table)

f1_score_micro(levels, table)

f1_score_macro(levels, table)

f1_score_weighted(levels, table)