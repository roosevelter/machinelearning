library(caret); library(C50); library(e1070); library(randomForest); library(class); library(ipred)
data = na.omit(data)
set.seed(1993)
dataIndex = createDataPartition(data$terroristattack, p = .75, list = F, times = 1)
dataTrain = data[dataIndex,]
dataTest = data[-dataIndex,]
dataTrain = dataTrain[,-c(1:2,4)]
dataTest = dataTest[,-c(1:2,4)]
dataTrain$terroristattack = as.factor(dataTrain$terroristattack)

dataModel = C5.0(x = dataTrain[,-1], y = dataTrain$terroristattack)
data_pred = predict(dataModel, dataTest)
install.packages("gmodels"); library(gmodels)
CrossTable(dataTest$terroristattack, data_pred, prop.chisq = F, prop.c = F, prop.r = F, dnn = c('actual attack', 'predicted attack'))

dataModelBoost = C5.0(x = dataTrain[,-1], y = dataTrain$terroristattack, trials = 100)
data_pred_boost = predict(dataModelBoost, dataTest)
CrossTable(dataTest$terroristattack, data_pred_boost, prop.chisq = F, prop.c = F, prop.r = F, dnn = c('actual attack', 'predicted attack'))

data = data[,-c(1:2,4)]
data$terroristattack = as.factor(data$terroristattack)
set.seed(1993)
m = train(terroristattack ~ ., data = data, method = "C5.0")

set.seed(1993)
rf1 = randomForest(terroristattack ~ ., data = data)

ctrl = trainControl(method = 'repeatedcv', number = 10, repeats = 10)
grid_rf = expand.grid(.mtry = c(2,4,8,16))
set.seed(1993)
rf2 = train(terroristattack ~ ., data = data, method = 'rf', metric = 'Accuracy', trControl = ctrl, tuneGrid = grid_rf)

grid_c50 <- expand.grid(.model = "tree", .trials = c(10, 20, 30, 40), .winnow = "FALSE")
set.seed(1993)
m50 = train(terroristattack ~ ., data=data, method="C5.0", metric = "Accuracy", trControl = ctrl, tuneGrid = grid_c50)

normalize = function(x) ((x-min(x))/(max(x)- min(x)))
knnData = as.data.frame(lapply(data[,2:ncol(data)], normalize))
knnPredict = knn(train = dataTrain, test = dataTest, cl = dataTrain[,1], k = 9)
CrossTable(x = dataTest[,1], y = knnPredict, prop.chisq = F)

set.seed(1993)
mybag = bagging(terroristattack ~ ., data=data, nbagg=25)
bag_pred = predict(mybag, data)
table(bag_pred, data$terroristattack)
set.seed(1993)
ctrl = trainControl(method = "cv", number = 10)
train(terroristattack ~ ., data = data, method = 'treebag', trControl = ctrl)
