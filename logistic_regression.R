#learn data for all classes
learn <- function(tdata, tLabels, epsilon) {
  n = length(tdata[1, ]) + 1;
  features <- matrix(0, 10, n);
  #for each class solve binary classification problem
  for (i in 0:9) {
    features[i, ] <- learnModel(data = tdata, y = tLabels, eps = epsilon, class = i); 
    print(i)
  }
  return (features)
}

#learn one class 
learnModel <- function(data, y, eps, class) {
  m = length(data[ ,1]);
  n = length(data[1, ]) + 1;
  #Chek input data
  if(m != length(y)) {
    print("wrong dimension")
    return (-1);
  }
  # Binarization input data
  for (i in 1:m) {
    if (y[i] == class) {
      y[i] = 1;
    }
    else {
      y[i] = 0;
    }
  }
  x <- matrix(1, m, n);
  x[,-1] <- data;
  Q <- matrix(0.1, n, 1);
  g_x <- matrix(0, m, 1);
  delta <- matrix(0, n, 1);
  Err = 1000; Err_old = 10000;
  lambda = 0.00000001;
  mu = 0.000001;
  #gradient desedent
  while (Err >= eps) {
    g_x = 1.0 / (1.0 + exp(-t(Q) %*% t(x)))
    delta <- t(x) %*% (g_x - y);
    delta[-1] = delta[-1] - 2 * lambda * Q[-1];
    Q <- Q - mu * delta;
    Err_old = Err;
    Err <- (1 / m) * sum(-y * log(g_x) - (1 - y) * log(1 - g_x)) + lambda * sum(crossprod(Q[-1]));
    #print(Err)
  }
  return(Q)
}

testModel <- function(Q, data){
  #calculate predictions for each class using obtained parametrs
  m = length(data[ ,1]);
  n = length(data[1, ]) + 1;
  labels <- matrix(0, m, 1);
  g_x <- matrix(0, 10, 1);
  x <- matrix(1, m, n);
  x[,-1] <- data;
  g_x = 1.0 / (1.0 + exp(-t(Q) %*% t(x)))
  labels[i] = which.max(g_x);
  return(labels)
}