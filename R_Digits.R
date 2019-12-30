# Load Mninst Data
dataDirectory <- "../data"
if (!file.exists(paste(dataDirectory, '/train.csv', sep=""))) {
  link <- 'https://apache-mxnet.s3-accelerate.dualstack.amazonaws.com/R/data/mnist_csv.zip'
  if (!file.exists(paste(dataDirectory, '/mnist_csv.zip', sep=""))) 
    download.file(link, destfile = paste(dataDirectory, '/mnist_csv.zip', sep=""))
  unzip(paste(dataDirectory, '/mnist_csv.zip', sep = ""), exdir=dataDirectory)
}

mnist.train <- read.csv(paste(dataDirectory, '/train.csv', sep=""))
mnist.test <- read.csv(paste(dataDirectory, '/test.csv', sep=""))

head(colnames(mnist.train), 4)
tail(colnames(mnist.train), 4)
head(mnist.train[,1:4])

# show MNIST Image
im<-matrix((mnist.train[4,2:ncol(mnist.train)]), nrow=28, ncol=28)
im_numbers <- apply(im, 2, as.numeric)
image(1:28, 1:28, im_numbers, col=gray((0:255)/255))

#Function to visualize a number
img <- function(d, row_index){
  
  #Obtaining the row as a numeric vector
  r <- as.numeric(d[row_index, 2:785])
  
  #Creating a empty matrix to use
  im <- matrix(nrow = 28, ncol = 28)
  
  #Filling properly the data into the matrix
  j <- 1
  for(i in 28:1){
    im[,i] <- r[j:(j+27)]
    j <- j+28
  }  
  
  #Plotting the image with the label
  image(x = 1:28, 
        y = 1:28, 
        z = im, 
        col=gray((255:0)/255), 
        main = paste("Number:", d[row_index, 1]))
}


img(mnist.train, 4)

# visualize the some digits
par(mfcol=c(6,6))
par(mar=c(1, 0, 3, 0), xaxs='i', yaxs='i')
for (idx in 1:36) { 
  img(mnist.train, idx)
}
par(mfcol=c(1,1))
par(mar=c(5.1, 4.1, 4.1, 2.1), xaxs='r', yaxs='r')

# selectc the rows where the label is either 5 or 6
digits_nn <- mnist.train[(mnist.train$label == 5) | (mnist.train$label == 6),]
digits_nn$y <- digits_nn$label
digits_nn$label <- NULL
table(digits_nn$y)

digits_nn$y <- ifelse(digits_nn$y == 5, 0, 1)
table(digits_nn$y)

set.seed(42)
sample <- sample(nrow(digits_nn), nrow(digits_nn) * 0.8)
test <- setdiff(seq_len(nrow(digits_nn)), sample)

# reduced the number of column data that had the same value for all rows
var(digits_nn[sample, 1], na.rm = TRUE)
digits.X2 <- digits_nn[, apply(digits_nn[sample, 1:(ncol(digits_nn)-1)], 2, var, na.rm=TRUE) != 0]
length(digits.X2)

# performa PCA and plot the cumulative sum of the variances
df.pca <- prcomp(digits.X2[sample,], center = TRUE, scale. = TRUE)
s <- summary(df.pca)
cumprop <- s$importance[3, ]
plot(cumprop, type = "l", main = "Cumulative sum", xlab = "PCA component")
# this plot shows that we can use the first 100 variable (the principal components) and this will account for over 80% of the variance in the original data

# selects out the principal components that account for 50% of our variance
num_cols <- min(which(cumprop > 0.5))
cumprop[num_cols]

# create neural network
#install.packages("neuralnet")
library(neuralnet)
newdat <- data.frame(df.pca$x[, 1:num_cols])
newdat$y <- digits_nn[sample, "y"]
col_names <- names(newdat)
f <- as.formula(paste("y ~", paste(col_names[!col_names %in% "y"], collapse="+")))
nn <- neuralnet(f, data=newdat, hidden=c(4,2), linear.output = FALSE)

plot(nn)

# prediction
test.data <- predict(df.pca, newdata = digits_nn[test, colnames(digits.X2)])
test.data <- as.data.frame(test.data)
preds <- compute(nn, test.data[, 1:num_cols])
preds <- ifelse(preds$net.result > 0.5, "1", "0")
t <- table(digits_nn[test, "y"], preds, dnn = c("Actual", "Predicted"))
acc <- round(100.9 * sum(diag(t))/sum(t), 2)
print(t)
print(sprintf(" accuracy = %1.2f%%", acc))

# without pca
nn.all <- neuralnet( y ~ ., data = digits_nn[sample,], hidden = c(4,2), linear.output = FALSE)
plot(nn.all)
preds.all <- compute(nn.all, digits_nn[test, ])
preds.all <- ifelse(preds.all$net.result > 0.5, "1", "0")
t.all <- table(digits_nn[test, "y"], preds.all, dnn = c("Actual", "Predicted"))
acc.all <- round(100.9 * sum(diag(t.all))/sum(t.all), 2)
print(t.all)
print(sprintf(" accuracy = %1.2f%%", acc.all))


# multi-classification
sample.2 <- sample(nrow(mnist.train), 6000)
train.digits <- sample.2[1:5000]
test.digits <- sample.2[5001:6000]

digits.X <- mnist.train[train.digits, -1]
digits.y_n <- mnist.train[train.digits, 1]
mnist.train$label <- factor(mnist.train$label, levels = 0:9)
digits.y <- mnist.train[train.digits, 1]

digits.test.X <- mnist.train[test.digits, -1]
digits.test.y <- mnist.train[test.digits, 1]

barplot(table(digits.y), main = "Distribution of y values (train)")
barplot(table(digits.test.y), main = "Distribution of y values (test)")

# train by nnet
#install.packages("caret")
#install.packages("e1071")
library(nnet)
library(caret)
set.seed(42)
tic <- proc.time()
digits.m1 <- caret::train(digits.X, digits.y,
                          method = "nnet",
                          tuneGrid = expand.grid(
                            .size = c(5),
                            .decay = c(0.1)),
                          trControl = trainControl(method = "none"),
                          MaxNWts = 10000,
                          maxit = 100)
print(proc.time() - tic)

# pridict
digits.yhat1 <- predict(digits.m1, newdata = digits.test.X)
accuracy <- 100.0 * sum(digits.yhat1 == digits.test.y)/length(digits.test.y)
print(sprintf(" accuracy = %1.2f%%", accuracy))
barplot(table(digits.yhat1), main = "Distribution of y values (model 1)")
table(digits.test.y, digits.yhat1)
caret::confusionMatrix(xtabs(~digits.yhat1 + digits.test.y))


# next model
set.seed(42)
tic <- proc.time()
digits.m2 <- caret::train(digits.X, digits.y,
                          method = "nnet",
                          tuneGrid = expand.grid(
                            .size = c(10),
                            .decay = 0.1
                          ),
                          trControl = trainControl(method = "none"),
                          MaxNWts = 50000,
                          maxit = 100)
print(proc.time() - tic)

# pridict next model
digits.yhat2 <- predict(digits.m2, newdata = digits.test.X)
accuracy <- 100.0 * sum(digits.yhat2 == digits.test.y)/length(digits.test.y)
print(sprintf(" accuracy = %1.2f%%", accuracy))
barplot(table(digits.yhat2), main = "Distribution of y values (model 2)")
table(digits.test.y, digits.yhat2)
caret::confusionMatrix(xtabs(~digits.yhat2 + digits.test.y))


# next model 3
set.seed(42)
tic <- proc.time()
digits.m3 <- caret::train(digits.X, digits.y,
                          method = "nnet",
                          tuneGrid = expand.grid(
                            .size = c(40),
                            .decay = 0.1
                          ),
                          trControl = trainControl(method = "none"),
                          MaxNWts = 50000,
                          maxit = 100)
print(proc.time() - tic)

# pridict next model 3
digits.yhat3 <- predict(digits.m3, newdata = digits.test.X)
accuracy <- 100.0 * sum(digits.yhat3 == digits.test.y)/length(digits.test.y)
print(sprintf(" accuracy = %1.2f%%", accuracy))
barplot(table(digits.yhat2), main = "Distribution of y values (model 3)")
table(digits.test.y, digits.yhat3)
caret::confusionMatrix(xtabs(~digits.yhat3 + digits.test.y))
