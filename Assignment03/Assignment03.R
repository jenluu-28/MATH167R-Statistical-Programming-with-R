# Jennifer Luu 
# Homework 3

# 1. Input: a numerical vector, Output: the product of the elements 

    # For loop:
    test1 = c("one", "two")
    test2 = vector(mode = "numeric", length = 0)
    test3 = c(1, 2, 3, 4)
    
    productFor <- function(nVector) {
      if (class(nVector) != "numeric") {return("The input must be a numerical vector.")}
      else if (length(nVector) == 0) {return("The numerical vector cannot be empty.")}
      else{
        total = 1
        for (i in 1:length(nVector))
        {
          total = total*nVector[i] 
        }
        return(total)
      }
    }
    
    productFor(test1)
    print("Expected: The input must be a numerical vector.")
    
    productFor(test2)
    print("Expected: The numerical vector cannot be empty.")
    
    productFor(test3)
    print("Expected: 24")
    
    
    # While loop: 
    test1 = c("one", "two")
    test2 = vector(mode = "numeric", length = 0)
    test3 = c(1, 2, 3, 4)
    
    productWhile <- function(nVector){
      if (class(nVector) != "numeric") {return("The input must be a numerical vector.")}
      else if (length(nVector) == 0) {return("The numerical vector cannot be empty.")}
      else{
        total = 1
        count = length(nVector)
        while (count != 0) {
          total = total*nVector[count]
          count = count - 1
        }
        return(total)
      }
    }
    
    productWhile(test1)
    print("Expected: The input must be a numerical vector.")
    
    productWhile(test2)
    print("Expected: The numerical vector cannot be empty.")
    
    productWhile(test3)
    print("Expected: 24")
    
  # Repeat loop:
    test1 = c("one", "two")
    test2 = vector(mode = "numeric", length = 0)
    test3 = c(1, 2, 3, 4)
    
  productRepeat <-function(nVector){
    if(class(nVector) != "numeric") {return("The input must be a numerical vector.")}
    else if (length(nVector) == 0) {return("The numerical vector cannot be empty.")}
    else{
      total = 1
      count = 1
      repeat{
        total = total*nVector[count]
        count = count + 1
        if(count > length(nVector))
          break
      }
      return(total)
    }
  }
  
    productRepeat(test1)
    print("Expected: The input must be a numerical vector.")
    
    productRepeat(test2)
    print("Expected: The numerical vector cannot be empty.")
    
    productRepeat(test3)
    print("Expected: 24")
  
    
# 2. Input: 2 matrices, Output: the matrix multiplication (You can use nrow and ncol)

  # For loop:
    
    m1 = cbind(c(1,2,3), c(4,5,6)) # 2x3 matrix
    m2 = cbind(c(2,3), c(1,1), c(2,3)) # 3x2 matrix
    m3 = cbind(c(1,1,1,1,1)) # 5x1 matrix
    c1 = c("blue", "yellow", "green")
    
  matrixMultFor <- function(m1, m2) {
    if(is.matrix(m1) != "TRUE" || is.matrix(m2) != "TRUE") {return("Both inputs must be valid matrices.")}
    else if (ncol(m1) != nrow(m2))
    {
      return("Incompatiable number of columns and rows for matrix multiplication. Check the input.")
    }
    else
    {
      result = matrix(nrow = nrow(m1), ncol = ncol(m2))
      for (i in 1:nrow(m1))
      {
        for (j in 1:ncol(m2))
        {
          row = m1[i, ]
          column = m2[ ,j]
          temp = row*column
          
          sum = 0
          for (k in 1:length(temp))
          {
            sum = sum + temp[k]
          }
          result[i, j] = sum
        }
      }  
    }
    return(result)
  }
  
  # test 1
  matrixMultFor(c1,m1)
  print("Expected: Both inputs must be valid matrices.")
  
  # test 2
  matrixMultFor(m1,m3)
  print("Expected: Incompatiable number of columns and rows for matrix multiplication. Check the input. ")
  
  # test 3
  matrixMultFor(m1, m2)
  print(m1%*%m2)
  

  # While loop:
  m1 = cbind(c(1,2,3), c(4,5,6)) # 2x3 matrix
  m2 = cbind(c(2,3), c(1,1), c(2,3)) # 3x2 matrix
  m3 = cbind(c(1,1,1,1,1)) # 5x1 matrix
  c1 = c("blue", "yellow", "green")
  
  matrixMultWhile <- function(m1, m2) {
    if(is.matrix(m1) != "TRUE" || is.matrix(m2) != "TRUE") {return("Both inputs must be valid matrices.")}
    else if (ncol(m1) != nrow(m2))
    {
      return("Incompatiable number of columns and rows for matrix multiplication. Check the input.")
    }
    else {
      result = matrix(nrow = nrow(m1), ncol = ncol(m2))
      i = 1
      while (i <= nrow(m1))
      {
        j = 1
        while ( j <= ncol(m2))
        {
          row = m1[i, ]
          column = m2[ ,j]
          temp = row*column
          
          k = 1
          sum = 0
          while (k <= length(temp))
          {
            sum = sum + temp[k]
            k = k + 1
          }
          result[i, j] = sum
          
          j = j + 1
        }
        i = i + 1
      }
    }
    return(result)
  }
  
  # test 1
  matrixMultWhile(c1,m1)
  print("Expected: Both inputs must be valid matrices.")
  
  # test 2
  matrixMultWhile(m1,m3)
  print("Expected: Incompatiable number of columns and rows for matrix multiplication. Check the input. ")
  
  # test 3
  matrixMultWhile(m1, m2)
  print(m1%*%m2)
  
 
  # Repeat loop:   
  
  m1 = cbind(c(1,2,3), c(4,5,6)) # 2x3 matrix
  m2 = cbind(c(2,3), c(1,1), c(2,3)) # 3x2 matrix
  m3 = cbind(c(1,1,1,1,1)) # 5x1 matrix
  c1 = c("blue", "yellow", "green")
  
  matrixMultRepeat <- function(m1, m2){
    if(is.matrix(m1) != "TRUE" || is.matrix(m2) != "TRUE") {return("Both inputs must be valid matrices.")}
    else if (ncol(m1) != nrow(m2))
    {
      return("Incompatiable number of columns and rows for matrix multiplication. Check the input.")
    }
    else
    {
      result = matrix(nrow = nrow(m1), ncol = ncol(m2))
      i = 1
      repeat{
        j = 1
        repeat{
          k = 1
          row = m1[i, ]
          column = m2[ ,j]
          temp = row*column
          
          sum = 0
          repeat{
            sum = sum + temp[k]
            result[i,j] = sum
            
            k = k + 1
            if (k > length(temp))
              break
          }
          j = j + 1
          if (j > ncol(m2))
            break
        }
        i = i + 1
        if ( i > nrow(m1))
          break
      }
    }
    return(result)
  }
  
  # test 1
  matrixMultRepeat(c1,m1)
  print("Expected: Both inputs must be valid matrices.")
  
  # test 2
  matrixMultRepeat(m1,m3)
  print("Expected: Incompatiable number of columns and rows for matrix multiplication. Check the input. ")
  
  # test 3
  matrixMultRepeat(m1, m2)
  print(m1%*%m2)
  

  
# 3. Input: a number and a vector, Output: the (first) position of the number in the vector 

    # For loop:
    num = c(1, 0, 4, 3, 5, 4, 8) # test vector
    words = c("cat", "dog", "cheese")
    
    positionOfFor <-function(number, vector) {
      if (is.integer(number) == "FALSE" | class(vector) != "numeric"){
        return("There is an invalid input. The first input needs to be an integer or the second input needs to be a numeric vector.")
      }
      else 
      {
        for (i in 1:length(vector))
        {
          if (number == vector[i])
          {
            return(i)
          }
        }
      }
    }
    
    # test 1, part 1
    positionOfFor(3.5, num)
    print("Expected: There is an invalid input. The first input needs to be an integer or the second input needs to be a numeric vector.")
    
    # test 1, part 2
    positionOfFor(3, words)
    print("Expected: There is an invalid input. The first input needs to be an integer or the second input needs to be a numeric vector.")
    
    # test 2
    positionOfFor(4L, num)
    print("Expected: 3")
    
    
    # While loop:
    
    num = c(1, 0, 4, 3, 5, 4, 8) # test vector
    words = c("cat", "dog", "cheese")
    
    
    positionOfWhile <- function(number, vector) {
      if (is.integer(number) == "FALSE" | class(vector) != "numeric"){
        return("There is an invalid input. The first input needs to be an integer or the second input needs to be a numeric vector.")
      }
      else
      {
        result = 0 
        i = 0
        while(result == 0)
        {
          i = i + 1
          if (number == vector[i])
          {
            result = i
            return(result)
          }
        }
      }
    }
    
    
    # test 1, part 1
    positionOfWhile(3.5, num)
    print("Expected: There is an invalid input. The first input needs to be an integer or the second input needs to be a numeric vector.")
    
    # test 1, part 2
    positionOfWhile(3, words)
    print("Expected: There is an invalid input. The first input needs to be an integer or the second input needs to be a numeric vector.")
    
    # test 2
    positionOfWhile(4L, num)
    print("Expected: 3")
    
    
  # Repeat loop: 
    
    num = c(1, 0, 4, 3, 5, 4, 8) # test vector
    words = c("cat", "dog", "cheese")
    
    
  positionOfRepeat <- function(number, vector) {
    if (is.integer(number) == "FALSE" | class(vector) != "numeric"){
      return("There is an invalid input. The first input needs to be an integer or the second input needs to be a numeric vector.")
    }
    else{
      result = 0 
      i = 0
      repeat
      {
        i = i + 1
        if(number == vector[i])
        {
          result = i
        }
        if(result != 0 || i > length(vector))
          break
        }
    }
    return(result)
  }
  
  
  # test 1, part 1
  positionOfRepeat(3.5, num)
  print("Expected: There is an invalid input. The first input needs to be an integer or the second input needs to be a numeric vector.")
  
  # test 1, part 2
  positionOfRepeat(3, words)
  print("Expected: There is an invalid input. The first input needs to be an integer or the second input needs to be a numeric vector.")
  
  # test 2
  positionOfRepeat(4L, num)
  print("Expected: 3")

    
# 4. Input: a number x, Output: the first n Natural numbers which the sum is more than x.
    
    # For loop:
    sumGreaterFor <- function(number){
      if (number < 0)
      {
        return("The input is not a valid positive number.")
      }
      else if (number == 0)
      {
        return("There are no natural numbers leading up to 0 whose sum are greater than 0.")
      }
      else if (number == 1)
      {
        return("There are no natural numbers leading up to 1 whose sum are greater than 1.")
      }
      else
      {
        naturals <- vector()
        total = 0
        for (i in 1:number)
        {
          total = total + i
          naturals[i] = i
          if (total > number)
          {
            return(naturals)
          }
        }
      }
    }
    
    # test 1
    sumGreaterFor(-1)
    print("Expected: The input is not a valid positive number.")
    
    # test 2
    sumGreaterFor(0)
    print("Expected: There are no natural numbers leading up to 0 whose sum are greater than 0.")
    
    # test 3
    sumGreaterFor(1)
    print("Expected: There are no natural numbers leading up to 1 whose sum are greater than 1.")
    
    # test 4
    sumGreaterFor(8)
    print("Expected: 1, 2, 3, 4")
    
    # test 5
    sumGreaterFor(19.5)
    print("Expected: 1, 2, 3, 4, 5, 6")

    # While loop:
    sumGreaterWhile <- function(number){
      if (number < 0)
      {
        return("The input is not a valid positive number.")
      }
      else if (number == 0)
      {
        return("There are no natural numbers leading up to 0 whose sum are greater than 0.")
      }
      else if (number == 1)
      {
        return("There are no natural numbers leading up to 1 whose sum are greater than 1.")
      }
      else
      {
         naturals <- vector()
         count = 0
         total = 0 
         while (total <= number)
         {
           count = count + 1
           total = total + count
           naturals[count] = count
         }
         return(naturals)
      }
    }
    
    # test 1
    sumGreaterWhile(-1)
    print("Expected: The input is not a valid positive number.")
    
    # test 2
    sumGreaterWhile(0)
    print("Expected: There are no natural numbers leading up to 0 whose sum are greater than 0.")
    
    # test 3
    sumGreaterWhile(1)
    print("Expected: There are no natural numbers leading up to 1 whose sum are greater than 1.")
    
    # test 4
    sumGreaterWhile(8)
    print("Expected: 1, 2, 3, 4")
    
    # test 5
    sumGreaterWhile(19.5)
    print("Expected: 1, 2, 3, 4, 5, 6")

    
    
    # Repeat loop:
  sumGreaterRepeat <- function(number){
    if (number < 0)
    {
      return("The input is not a valid positive number.")
    }
    else if (number == 0)
    {
      return("There are no natural numbers leading up to 0 whose sum are greater than 0.")
    }
    else if (number == 1)
    {
      return("There are no natural numbers leading up to 1 whose sum are greater than 1.")
    }
    else
    {
      naturals <- vector()
      count = 0 
      total = 0
      repeat {
        count = count + 1
        total = total + count 
        naturals[count] = count
        
        if(total > number)
          break
      }
    }
    return(naturals)
  }
  
  
  # test 1
  sumGreaterRepeat(-1)
  print("Expected: The input is not a valid positive number.")
  
  # test 2
  sumGreaterRepeat(0)
  print("Expected: There are no natural numbers leading up to 0 whose sum are greater than 0.")
  
  # test 3
  sumGreaterRepeat(1)
  print("Expected: There are no natural numbers leading up to 1 whose sum are greater than 1.")
  
  # test 4
  sumGreaterRepeat(8)
  print("Expected: 1, 2, 3, 4")
  
  # test 5
  sumGreaterRepeat(19.5)
  print("Expected: 1, 2, 3, 4, 5, 6")
        
  
# 5. Input: a number x, Output: state if it is a prime number (you can use %%)
    
    #For loop:
    primeFor <- function(number){
      if (number <= 1 | is.integer(number) == FALSE)
      {
        return("The input must be an integer number that is greater than 1.")
      }
      else{
        count = 0
        for (i in 1:number)
        {
          if (number %% i == 0)
          {
            count = count + 1
          }
        }
      }
      # A number is prime if it is divisible only be 1 and itself; count of divisibility = 2
      if (count == 2)
      {
        return("The number is prime.")
      }
      else if (count > 2)
      {
        return("The number is composite.")
      }
    }
    
    # test 1
    primeFor(-1L)
    print("Expected: The input must be an integer that is greater than 1.")
    
    # test 2
    primeFor(11L)
    print("Expected: The number is prime.")
    
    # test 3
    primeFor(24L)
    print("Expected: The number is composite.")
    
    
    
    # While loop:
    primeWhile <- function(number){
      if (number <= 1 | is.integer(number) == FALSE)
      {
        return("The input must be an integer number that is greater than 1.")
      }
      else
      {
        count = 0 
        i = 0 
        while (i != number)
        {
          i = i + 1
          if(number %% i == 0)
          {
            count = count + 1
          }
        }
      }
      # A number is prime if it is divisible only be 1 and itself; count of divisibility = 2
      if(count == 2)
      {
        return("The number is prime.")
      }
      else if (count > 2)
      {
        return("The number is composite.")
      }
    }
    
    # test 1
    primeWhile(-1L)
    print("Expected: The input must be an integer that is greater than 1.")
    
    
    # test 2
    primeWhile(11L)
    print("Expected: The number is prime.")
    
    
    # test 3
    primeWhile(24L)
    print("Expected: The number is composite.")
    
    
    # Repeat loop:
    primeRepeat <- function(number){
      if (number <= 1 | is.integer(number) == FALSE)
      {
        return("The input must be an integer number that is greater than 1.")
      }
      else
      {
        count = 0 
        i = 0 
        repeat
        {
          i = i + 1
          if(number %% i == 0)
          {
            count = count + 1
          }
          if( i > number)
            break
        }
      }
      if(count == 2)
      {
        return("The number is prime.")
      }
      else if (count > 2)
      {
        return("The number is composite.")
      }
    }
    
    
    # test 1
    primeRepeat(-1L)
    print("Expected: The input must be an integer that is greater than 1.")
    
    # test 2
    primeRepeat(11L)
    print("Expected: The number is prime.")
    
    # test 3
    primeRepeat(24L)
    print("Expected: The number is composite.")

    