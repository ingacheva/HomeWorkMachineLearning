source("C://R_home//load_data.R")
source("C://R_home//logistic_regression.R")
library(PerfMeas)
# This sciprt file contains a frame for learning handwritten digitals from the MNIST dataset

# load training data from files
data <- loadMNISTData("C://R_home//dataset//train-images.idx3-ubyte", "C://R_home//dataset//train-labels.idx1-ubyte")
trainLabels <- data$labels
trainData <- data$data

#normalization the data
trainData <- trainData / 255

print(dim(trainData))
print(dim(trainLabels))
# trainingData should be 60000x786,  60000 data and 784 features (28x28), tha matrix trainData has 60000 rows and 784 columns
# trainingLabels should have 60000x1, one class label \in {0,1,...9} for each data.

#n= 10
#print(trainData[n, ])
#print("Class label:"); print(trainLabels[n])
#image(matrix(trainData[n, ], ncol=28, nrow=28), Rowv=28, Colv=28)

# train a model
features <- learn(tdata = trainData, tLabels = trainLabels, epsilon = 0.4)
predictedLabels <- testModel(features, trainData)

#calculate accuracy on training data
print(sum(predictedLabels == trainLabels)/length(trainLabels))

#Recall, precision, specificity, F-measure, FDR and ROC for each class separately. Use a package for ROC.
m = length(predictedLabels);
for (k in 0:9) {
  pred <- matrix(0, m, 1);
  labels <- matrix(0, m, 1);
  for (i in 1:m) {
    if (predictedLabels[i] == k) {
      pred[i] = 1
    }
    if (trainLabels[i] == k) {
      labels[i] = 1
    }
  }
  print(paste("Metrics for class ", toString(k)))
  metrix = F.measure.single(pred, labels)
  print(paste("precision =", toString(metrix["P"])))
  print(paste("recall = ", toString(metrix["R"])))
  print(paste("specificity = ", toString(metrix["S"])))
  print(paste("F measure = ", toString(metrix["F"])))
  print(paste("0/1 loss accuracy = ", toString(metrix["A"])))
  print("")
}

# test the model
data <- loadMNISTData("C:\\R_home\\dataset\\t10k-images.idx3-ubyte", "C:\\R_home\\dataset\\t10k-labels.idx1-ubyte")
testLabels <- data$labels
testData <- data$data

testData <- testData / 255

print(dim(testData))
print(dim(testLabels))

#trainingData should be 10000x786,  10000 data and 784 features (28x28), tha matrix trainData has 10000 rows and 784 columns
#trainingLabels should have 10000x1, one class label \in {0,1,...9} for each data.

predicts <- testModel(features, testData)

#calculate accuracy
print(sum(predicts == testLabels)/length(testLabels))

#calculate the following error metric for each class obtained on the test data:
#Recall, precision, specificity, F-measure, FDR and ROC for each class separately. Use a package for ROC. 
m = length(predicts);
for (k in 0:9) {
  pred <- matrix(0, m, 1);
  labels <- matrix(0, m, 1);
  for (i in 1:m) {
    if (predicts[i] == k) {
      pred[i] = 1;
    }
    if (testLabels[i] == k) {
      labels[i] = 1;
    }
  }
  print(paste("Metrics for class ", toString(k)))
  metrix = F.measure.single(pred, labels)
  print(paste("precision =", toString(metrix["P"])))
  print(paste("recall = ", toString(metrix["R"])))
  print(paste("specificity = ", toString(metrix["S"])))
  print(paste("F measure = ", toString(metrix["F"])))
  print(paste("0/1 loss accuracy = ", toString(metrix["A"])))
  print("")
}
