# Assignment 5
# Jennifer Luu 
# 012385555
# 11/26/19

#########################################################################################

# 1. Yes, the departure of the points from the 45 degree line indicate that the 
# mystery distribution is not a normal distribution. The purpose of a Q-Q plot 
# is to provide a graphical comparison of two probability distributions and to
# see if a dataset is Normally distributed. The majority of the points do not 
# lie on the 45 degree line, which means that the distributions do not approximate 
# each other well. Therefore, the mystery distribution is not a Normal distribution.

#########################################################################################

# 2. Write a function that given a one dimensional data set or a vector, and the
# degrees of freedom, makes a qqplot of the data set vs. a Student-t distribution 
# with dg degrees of freedom. Use appropriate labels and titles. 

stPlot <- function(vector, dg){
  if(class(vector) == "numeric" && dg%%1 == 0)
  {
    studentT = qt(p = vector, df = dg)
    plot(studentT, vector, main = "Student-T Q-Q Plot", xlab = "Quantiles of Student-T", ylab = "Sample Quantiles")
    abline(0,1, col = "green")
  }
  else
  {
    return("Please insert a numerical one-dimensional dataset and a valid positive integer.")
  }
}

points <- seq(0,1, by = 0.02)
stPlot(points, 10)

#########################################################################################

# 3. The file hondas contains information about 22 Honda Civic from Craigslist. 
# Using ggplot, make a relevant plot and make suggestions based on the plot. 

# a. Install the package "readxl" first. 
# b. Make sure the package "ggplot2" is checked. 

library(readxl)
my_hondas <- read_excel("hondas.xlsx")
p <- ggplot(data = my_hondas, aes(x = price, y = Miles, color = Year, shape = type))
p + geom_point(size = 5)

# Comments: 
# We assume that the Honda Civics being sold on Craigslist are used. 

# The circles represent the cars of type EX and the trianlges represent the cars of the type LX. 
# The color of the points indicate when the cars were made. Lighter points convey more recent car models, and
# darker points convey older car models. 

# Based on the graph, there is a negative trend. In general, as the price of the car rises, the lower
# the mileage. The cheapest car, which is less than $10,000, has the highest mileage; it is the only
# car that has over 100,000 miles. 

# From the graph, we can also infer that older car models, regardless
# of whether they are of type EX or LX, tend to have higher mileages. 

# If I were to advise someone who is thinking of buying a car from these listings, 
# I would suggest to pick a car of type LX and from the year 2016 or 2017. These cars have
# the lowest mileages compared to the cars of type EX. 

# If I had to pick only 1 car to buy, I would suggest that the car of type LX between the 
# prices of $10,000 and $12,500 is the best deal. This car has the second lowest mileage and
# it is cheaper than the other LX cars from 2016 or 2017.

#########################################################################################

# 4. Dr Tortora thinks that the number of chocolate chips in a cookie follows a 
# Poisson distribution, she doesn’t know the mean. The file chocolatechips.rdata 
# contains the number of chocolate chips per cookie on a sample of 30 cookies. 
# Use the χ2 test to test the hypothesis that the number of chocolate chips in a 
# cookie follows a Poisson distribution.

# n (size) = 30 cookies
# Null hypothesis: the data from the cookies follow a Poisson distribution 
# Alternative hypothesis: the data from the cookies does not follow a Poisson distribution

# a. Make sure that the file is in the working directory
load("chocolatechips.rdata")
# Shows the number of chocolate chips in a cookie  
table(chocolatechips)

# This vector contains the number of observed values.
# The 0 stands for cookies with 0 chocolate chips. 
# 0, 1, 2, 3, 4, 5, 6, 7, 8, >= 9
feq = seq(0,9)

# Since the mean (lambda) is unknown, we must manually compute it.
chips = c(0, 1*1, 2*2, 3*7, 4*3, 5*5, 6*4, 7*4, 8*2, 9*2)
lambda = sum(chips)/30


d1 = dpois(feq[1:9], lambda)
d2 = 1 - sum(d1)

expectedValue = c(d1,d2)*sum(chips)
chi = sum((chips-expectedValue)^2/expectedValue)

# degrees of freedom is 10 - 1 = 9
# Used both functions to check if the results were equivalent
1-pchisq(q = chi, df = 9)
chisq.test(chips, p = c(d1,d2))

# Comment: 
# Since the p-value < 2.2e-16, it is extremely small and almost 0. Therefore, we reject the null
# hypothesis that the number of chocolate chips in the cookies follows a Poisson 
# distribution. In other words, the number of chocolate chips in a cookie
# does not follow a Poisson distribution.
#########################################################################################




