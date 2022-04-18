# Jennifer Luu 
# ID: 012385555
# Assignment 4 
# Due 11/5/19



# 1. Input: a numerical dataset with p variables
# Expected output: a pdf file 

myData <- data.frame(Cars93$Min.Price, Cars93$Price, Cars93$Max.Price, Cars93$MPG.city, Cars93$EngineSize)
myData2 <- data.frame(Cars93$Min.Price, Cars93$Price, Cars93$Max.Price, Cars93$MPG.city, Cars93$EngineSize, Cars93$MPG.highway)

myPlot <- function(x){
  if (length(x) > 5)
  {
    return("Please input a numerical dataset with no more than 5 variables.")
  }
  else
  {
    n <- length(x)
    pdf("Assignment4_Plots.pdf") #Creates the pdf file for the output
    par(oma = c(4, 4, 4, 4), mar = c(2, 2, 2, 2), mfrow = c(n, 2)) #Specifies that there will be x rows and 2 columns for the plot
    colorChoices <- c("cadetblue1", "darkorchid1", "darkseagreen1", "deeppink", "midnightblue")
    
    for (i in 1:n)
    {
      data = as.numeric(x[, i])  #Convert the variable into numerical values 
      hist(data, col = colorChoices[i], border = "black", xlab = FALSE, main = colnames(x)[i])
      boxplot(data, col = colorChoices[i], border = "black", main = colnames(x)[i])
    }
  }
  dev.off()
}

myPlot(myData)
myPlot(myData2)


# 2. Input: A matrix
# Output: The sample variance of each column w/o using any loop (a vector)
# Can only use the functions length, sum, apply, sweep

test1 <- cbind(c(10, 8, 2), c(1, 1, 1,), c(5, 2, 6))

sampleVariance <- function(matrix){
  if (class(matrix) != "matrix")
  {
    return("The input can only be a matrix. Please check the input.")
  }
  else
  {
    #A vector that contains the mean of each column
    allMeans = apply(matrix, MARGIN = 2, mean)
    
    #Subtract each element (xi) in each column by its mean 
    matrix = sweep(matrix, MARGIN = 2, allMeans, "-")
    
    #Square all the elements in the matrix
    matrix = matrix**2
    
    #A vector of the lengths of each column
    allLengths = apply(matrix, MARGIN = 2, length)
    
    #Sums all of the elements in each column
    sampleVar = apply(matrix, MARGIN = 2, sum)
    
    #Divide each column with (length -1)
    sampleVar = sampleVar/(allLengths -1)

    return(sampleVar)
  }
}

sampleVariance(test1)
apply(test1, MARGIN = 2, var)


# 3. Input: A matrix and a vector 
# Output: Matrix multiplication
# Can only use the functions: length, ncol, nrow, apply, sweep

matrix1 <- cbind(c(10, 8, 2), c(4, 2, 1))
vector1 <- c(3,4) # A vector with a length that is equal to ncol of matrix1

vector2<- c(10, 2, 3, 4)  # A vector with a length that is not equal to ncol of matrix1

matrixMult <- function(matrix, vector){
  if (class(matrix) != "matrix")
  {
    return("The first input can be a matrix. Please check the input")
  }
  else if (class(vector) != "numeric")
  {
    return("The second input can only be a numeric vector. Please check the input.")
  }
  else if (ncol(matrix) != length(vector))
  {
    return("The column dimension of the matrix must equal to the length of the vector. Please check the input")
  }
  else if (ncol(matrix) == length(vector))
  {
   
    # Multiply the elements of each column by the vector
    temp <- sweep(matrix, MARGIN = 2, vector, "*") 
    
    # Add all of the elements of each row together
    output<- apply(temp, MARGIN = 1, sum)
  }
  return(output)
}

matrixMult(matrix1, vector1)
matrix1%*%vector1

matrixMult(matrix1, vector2)

