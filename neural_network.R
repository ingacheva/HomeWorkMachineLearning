#learn and test data by Neural Network
learnNet <- function(data, labels, eps){
  # only 3 layers
  L_n = 3
  n = length(data[1, ]) + 1;
  m = length(data[ ,1]);
  k = 1
  n_k = n * k
  y <- matrix(data = 0, nrow = m, ncol = k)
  x <- matrix(data = 1, nrow = n_k, ncol = m)
  for (i in 1:k) {
    for (j in 1:m) {
      if (labels[j] == i-1) {
        y[j,i] = 1;
      }
    }
  }
  for (i in 1:k) {
    a = n * (i-1) + 2
    b = n * i
    x[a:b, ] <- data
  }
  Q <- list()
  Q[[1]] <- matrix(0.1, n_k, n_k)
  Q[[2]] <- matrix(0.1, n_k, n_k/2)
  Q[[3]] <- matrix(0.1, n_k/2, 1)
  delta <- list()
  delta[[1]] <- matrix(0, 1, n_k)
  delta[[2]] <- matrix(0, 1, n_k/2)
  delta[[3]] <- matrix(0, 1, 1)
  delta2 <- list()
  delta2[[1]] <- matrix(0, n_k, n_k)
  delta2[[2]] <- matrix(0, n_k, n_k/2)
  delta2[[3]] <- matrix(0, n_k/2, 1)
  g_x <- matrix(0, L_n, n_k)
  J = 1000;
  lambda = 0.001;
  mu = 0.000001;
  
  #gradient desedent
  while (J >= eps) {
    J <- 0
    delta2[[1]] <- matrix(0, n_k, n_k)
    delta2[[2]] <- matrix(0, n_k, n_k/2)
    delta2[[3]] <- matrix(0, n_k/2, 1)
    for (i in 1:m) {
      xx <- x[ ,i]
      yy <- y[i, ]
      g_x <- calculate_g(xx,Q, L_n, n_k)
      h_x <- g_x[L_n,1]
      print(g_x)
      J <- J + yy * log(h_x) - (1 - yy) * log(1 - h_x)
      delta[[L_n]][1,1] <- yy - h_x
      delta2[[L_n]] <- delta2[[L_n]] + delta[[L_n]][1,1] * g_x[L_n - 1,1] 
      for (l in L_n - 1:1) {
        len = length(delta[[l]])
        #This is the backward propagation
        delta[[l]] <- Q[[l+1]] %*% t(delta[[l+1]]) 
        delta[[l]] <- delta[[l]] * g_x[l,1:len] * (1 - g_x[l,1:len])
        #delta[[l]] <- delta[[l]][-1, ]
        len = length(delta[[l - 1]])
        if (l != 1) {
          delta2[[l]] <- delta2[[l]] + t(delta[[l]] %*% t(g_x[l-1,1:len]))
        } 
        else {
          delta2[[l]] <- delta2[[l]] + t(delta[[l]] %*% t(xx))
        }
      }
    }
    J <- J / m;
    print(J)
    #Update weights
    for (i in 1:L_n) {
      delta2[[i]] <- delta2[[i]] / m 
      delta2[[i]][ ,-1] <- delta2[[i]][ ,-1] + lambda * Q[[i]][ ,-1]
      Q[[i]] <- Q[[i]] + mu * delta2[[i]]
    }
  }
  
  return(Q)
}

calculate_g <- function(x,Q, L_n, n) {
  g_x <- matrix(0, L_n, n)
  for (i in 1:n) {
    g_x[1,i] <- 1.0 / (1.0 + exp(-sum(Q[[1]][, i] %*% x)))
  }
  for (i in 1:n/2) {
    g_x[2,i] <- 1.0 / (1.0 + exp(-sum(Q[[2]][ ,i] %*% g_x[1,1:n])))
  }
  g_x[3,i] <- 1.0/ (1.0 + exp(-sum(Q[[3]] %*% g_x[2,1:n/2])))
  return(g_x)
}

testModel <- function(classifier, trainData){
  dataSize = 1
  labels <- matrix(data = 0, nrow = nData, ncol = dataSize)
  return(labels)
}