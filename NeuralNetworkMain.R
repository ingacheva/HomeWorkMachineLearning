source("C://R_home//load_data.R")
source("C://R_home//neural_network.R")
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

# train a model
network <- learnNet(data = trainData, labels = trainLabels, eps = 0.5)
exit();
predictedLabels <- testModel(network, trainData)

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
  sprintf("Metrics for class %d", k)
  metrix = F.measure.single(pred, labels)
  sprintf("precision = %.3f", metrix.P)
  sprintf("recall = %.3f", metrix.R)
  sprintf("specificity = %.3f", metrix.S)
  sprintf("F measure = %.3f", metrix.F)
  sprintf("0/1 loss accuracy = %.3f", metrix.A)
  sprintf("number of positive examples = %.3f", metrix.npos)
  prtint("")
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
network <- learnNet(data = testData, labels = testLabels, eps = 0.5)
exit();
predicts <- testModel(network, testData)

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
    if (trainLabels[i] == k) {
      labels[i] = 1;
    }
  }
  sprintf("Metrics for class %d", k)
  metrix = F.measure.single(pred, labels)
  sprintf("precision = %.3f", metrix.P)
  sprintf("recall = %.3f", metrix.R)
  sprintf("specificity = %.3f", metrix.S)
  sprintf("F measure = %.3f", metrix.F)
  sprintf("0/1 loss accuracy = %.3f", metrix.A)
  sprintf("number of positive examples = %.3f", metrix.npos)
  prtint("")
}
