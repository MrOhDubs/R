
tp_clean <- fread("C:\\Users\\e085724\\Desktop\\TPCleancsv_TRAIN.csv" , stringsAsFactors = TRUE)
tp_clean$REP_MODEL_YEAR <- as.factor(tp_clean$REP_MODEL_YEAR)
tp_clean$REP_REAR_AXLE_RATIO <- as.factor(tp_clean$REP_REAR_AXLE_RATIO)
#### Start by pulling all numerical columns from tp_clean

tp_nums <- select_if(tp_clean, is.numeric)


### Replace NA's with 0 for now
tp_nums[is.na(tp_nums)] <- 0


### Numerical values need to be scaled from 0 to 1.  Capture max and min values of mpg for unscaling later

maxValue <- apply(tp_nums, 2 , max, na.rm = T)
minValue <- apply(tp_nums, 2 , min, na.rm = T)
maxMPG <- max(tp_nums$MPG)
minMPG <- min(tp_nums$MPG)
tp_nums <- as.data.frame(scale(tp_nums, center = minValue, scale = maxValue-minValue))
max(tp_nums$MPG)


### Splitting the tp_nums df into a training and testing set
### trainTP will have 12,000 rows 
### testTP will contain the remaining 3062 rows
set.seed(8008)
ind <- sample(1:nrow(tp_nums), 12000)
trainTP <- tp_nums[ind,]
testTP <- tp_nums[-ind,]



### This section builds the formula we will use in the ANN
allVars <-colnames(tp_nums)
predictorVars <- allVars[!allVars%in%'MPG']
predictorVars <- paste(predictorVars, collapse = '+')
form=as.formula(paste('MPG ~ ',predictorVars))


### Build the NeuralNet 
library(neuralnet)
neuralModel <- neuralnet(formula = form,
                         hidden = c(  4,2),
                         linear.output = T,
                         data = trainTP,
                         lifesign = 'full',
                         lifesign.step = 500)


par(mfrow = c(1,2))
### plot the neural net
plot(neuralModel)


### Predict for test data set
predictions <- compute(neuralModel, testTP[,2:17])
str(prediction)
range(predictions)

### Unscale both the predictions returned and the actual values


#testTP <- t(apply(testTP, 1, function(r)r*attr(testTP,'scaled:scale') + attr(testTP, 'scaled:center')))


min(tp_clean$MPG,na.rm = T)
predictions <- predictions$net.result*(max(tp_clean$MPG)-min(tp_clean$MPG))+min(tp_clean$MPG)
max(predictions$net.result)

actualValues <- testTP$MPG*(max(tp_clean$MPG)-min(tp_clean$MPG))+min(tp_clean$MPG)
range(actualValues)


### Calculate the Mean Squared Error
MSE <- sum ((predictions - actualValues)^2 / nrow(testTP))


plot(actualValues, 
     predictions, 
     col = 'blue', 
     main = 'Real vs. Predicted Values', 
     pch =1,cex = 0.9,
     type = 'p',
     xlab = 'Actual', 
     ylab = 'Predicted')

abline(0, 1, col = 'black')
residual = actualValues - predictions
plot(residual)
