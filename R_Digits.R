# Load Mninst Data
mnist.train <- read.csv("C:\\Users\\opc\\Downloads\\mnist_csv\\train.csv")
mnist.test <- read.csv("C:\\Users\\opc\\Downloads\\mnist_csv\\test.csv")

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
