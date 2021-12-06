install.packages("e1071")
install.packages("GGally")

library(e1071)
library(GGally)

source("dataCleaning.R")
source("f1_score.R")

data <- read.csv("data/music_genre.csv")

data <- cleanData(data)

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
             cost=5
)

prediction <- predict(model, test_data)

levels = levels(prediction)
table = table(test_data$music_genre, prediction)

table

f1_scores(levels, table)

f1_score_micro(levels, table)

f1_score_macro(levels, table)

f1_score_weighted(levels, table)

obj <- tune.svm(music_genre ~ ., data = train_data, cost=c(0.001,0.01,0.1, 1,5,10,100) )

#pairs(test_data, main = "Test Data", pch = 21, bg = c("red", "green3", "blue","yellow","purple","deeppink")[unclass(data$music_genre)])

png(filename="pairs_plot.png", width=5000, height=5000)

ggpairs(test_data,
        color="music_genre",
        upper=list(continuous='blank')
        )

dev.off()



