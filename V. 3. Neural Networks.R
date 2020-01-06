## MNIST 데이터
dataDirectory <- "../data"
if (!file.exists(paste(dataDirectory, '/train.csv', sep=""))) {
  link <- 'https://apache-mxnet.s3-accelerate.dualstack.amazonaws.com/R/data/mnist_csv.zip'
  if (!file.exists(paste(dataDirectory, '/mnist_csv.zip', sep=""))) 
    download.file(link, destfile = paste(dataDirectory, '/mnist_csv.zip', sep=""))
  unzip(paste(dataDirectory, '/mnist_csv.zip', sep = ""), exdir=dataDirectory)
}

mnist.train <- read.csv(paste(dataDirectory, '/train.csv', sep=""))
mnist.test <- read.csv(paste(dataDirectory, '/test.csv', sep=""))

dim(mnist.train)
dim(mnist.test)

head(colnames(mnist.train), 4)
tail(colnames(mnist.train), 4)
head(mnist.train[,1:4])

## MNIST 데이터 시각화
im <- matrix((mnist.train[4,2:ncol(mnist.train)]), nrow=28, ncol=28)
im_numbers <- apply(im, 2, as.numeric)
image(1:28, 1:28, im_numbers, col=gray((0:255)/255))

## MNIST 데이터 시각화 2
im <- matrix(nrow=28, ncol=28)
j <- 1
for(i in 28:1){
  im[,i] <- (as.numeric(mnist.train[4,2:ncol(mnist.train)]))[j:(j+27)]
  j <- j+28
} 
im_numbers <- apply(im, 2, as.numeric)
image(1:28, 1:28, im_numbers, col=gray((255:0)/255))

## MNIST 데이터 시각화 함수
img <- function(d, row_index){
  # 행을 숫자 벡터로 변경
  r <- as.numeric(d[row_index, 2:785])
  # 행렬 선언 및 데이터 적재
  im <- matrix(nrow = 28, ncol = 28)
  j <- 1
  for(i in 28:1){
    im[,i] <- r[j:(j+27)]
    j <- j+28
  }  
  # 제목이 있는 이미지 출력
  image(x = 1:28, y = 1:28, z = im, col=gray((255:0)/255), main = paste("Number:", d[row_index, 1]))
}

img(mnist.train, 4)

## 여러 이미지 시각화
par(mfcol = c(6, 6))
par(mar = c(1, 0, 3, 0), xaxs='i', yaxs='i')
for (idx in 1:36) { 
  img(mnist.train, idx)
}
par(mfcol = c(1, 1))
par(mar = c(5.1, 4.1, 4.1, 2.1), xaxs='r', yaxs='r')
