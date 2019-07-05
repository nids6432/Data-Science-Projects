ht_wt<-read.csv("C:/Users/narif/Documents/Training/Machine Learning Fundamentals/Week4/weight-height/weight-height.csv")
ht_wt<-ht_wt[0:10,2:3]

#How to predict Weight? 

#Method 1, when height is unknown? If height is unknown, then weight can be predicted by just taking the average of 
#height. Sum of Squared Errors tell how good or bad the prediction is. 
x<-seq(1, 10, by=1)
y<-ht_wt$Weight
plot(x, y, main = "Weight",
     xlab = "Person", ylab = "Weight")
y_avg<-mean(ht_wt$Weight)
abline(a=y_avg, b=0, col = 1)
y_cap<-y_avg
text(1,190,"Ankita's: y=0x+187.9",col=1,adj=c(-.1,-.1))
sum((y_cap-y)**2)/(2*dim(ht_wt)[1])

#Method 2: When Height is known. Randomly drawing a line across the points. 
x <- ht_wt$Height
y <- ht_wt$Weight
plot(x, y, main = "Height vs Weight",
     xlab = "Height", ylab = "Weight")
abline(a=-500, b=10, col = 2)
text(71,210,"Nida's Line: y=10x-500",col=2,adj=c(-.1,-.1))
y_cap<-(10*x-500)
sum((y_cap-y)**2)/(2*dim(ht_wt)[1])
#Sum of squared errors gets better. Prediction is getting better. 

#Method 3. Use OLS method to predict line accurately. OLS method takes into consideration, the average/mean
# of values in x and y axis. 
x_avg<-mean(ht_wt$Height)
y_avg<-mean(ht_wt$Weight)
m=sum((x-x_avg)*(y-y_avg))/sum((x-x_avg)**2)
c=y_avg-(m*x_avg)
abline(a=c,b=m, col = 3)
text(71,200,"Mary's Line",col=3,adj=c(-.1,-.1))
y_cap<-(m*x)+c
sum((y_cap-y)**2)/(2*dim(ht_wt)[1])
##OR
lmweight = lm(Weight~Height, data = ht_wt)
lmweight

#Method 4. Gradient Descent
gradientDesc <- function(x, y, learn_rate, conv_threshold, n, max_iter,m,c) {
  plot(x, y, col = "blue", pch = 20)
  m <- runif(1, 0, 20)
  c <- runif(1, -700, 100)
  yhat <- m * x + c
  MSE <- sum((y - yhat) ^ 2) / n
  converged = F
  iterations = 0
  while(converged == F) {
    ## Implement the gradient descent algorithm
    m_new <- m - learn_rate * ((1 / n) * (sum((yhat - y) * x)))
    c_new <- c - learn_rate * ((1 / n) * (sum(yhat - y)))
    m <- m_new
    c <- c_new
    yhat <- m * x + c
    MSE_new <- sum((y - yhat) ^ 2) / n
    #abline(c, m,col="black")
    if(MSE - MSE_new <= conv_threshold) {
      abline(c, m) 
      converged = T
      return(paste("Optimal intercept:", c, "Optimal slope:", m))
    }
    iterations = iterations + 1
    if(iterations > max_iter) { 
      abline(c, m) 
      converged = T
      return(paste("Optimal intercept:", c, "Optimal slope:", m, "MSE: ",MSE))
      }
  }
}


# Run the function 

gradientDesc(ht_wt$Height, ht_wt$Weight, 0.01, 0.001, 10, 200,8.129,-375.5)

x <- ht_wt$Height
y <- ht_wt$Weight
m <- length(y)
X<-cbind(rep(1, 10), x)
X
y
# theta<-rep(0,2)
# Take the last solution from OLS method and plug it as the initial step fro GD
theta<-matrix(c(-375.5,8.129),nrow=2,ncol=1)
theta
compCost<-function(X, y, theta){
  J <- sum(((X%*%theta)-y)^2)/(2*length(y))
  return(J)
}
compCost(X,y,theta)
gradDescent<-function(X, y, theta, alpha, num_iters){
  m <- length(y)
  J_hist <- rep(0, num_iters)
  for(i in 1:num_iters){
    
    # this is a vectorized form for the gradient of the cost function
    # X is a 100x5 matrix, theta is a 5x1 column vector, y is a 100x1 column vector
    # X transpose is a 5x100 matrix. So t(X)%*%(X%*%theta - y) is a 5x1 column vector
    theta <- theta - alpha*(1/m)*(t(X)%*%(X%*%theta - y))
    
    # this for-loop records the cost history for every iterative move of the gradient descent,
    # and it is obtained for plotting number of iterations against cost history.
    J_hist[i]  <- compCost(X, y, theta)
  }
  # for a R function to return two values, we need to use a list to store them:
  results<-list(theta, J_hist)
  return(results)
}

num_iters=200
results <- gradDescent(X, y, theta, alpha=0.0001, num_iters)
theta <- results[[1]]
cost_hist <- results[[2]]
print(theta)
plot(1:num_iters, cost_hist, type = 'l')
cost_hist
# With GD the solution was not much better, but this method works well for  Overall, 
# for extra large problems it's more efficient than linear algebra solution.

