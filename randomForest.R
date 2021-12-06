dataClean$music_genre = as.factor(dataClean$music_genre)

library(randomForest)

n.points = 50000
sampling.rate = 0.8
training = sample(1:n.points, sampling.rate * n.points, replace=F)
train = subset(dataClean[training, ])
testing = setdiff(1:n.points, training)
test = subset(dataClean[testing, ])

rfSimple = randomForest(music_genre~., data=train)

rf = randomForest(music_genre~., data=train, ntree=450, mtry = 4, do.trace = T)

pred = predict(rf, test[-14])

cm = table(test[,14], pred)
