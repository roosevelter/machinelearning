library(caret); library(C50); library(e1070)
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
