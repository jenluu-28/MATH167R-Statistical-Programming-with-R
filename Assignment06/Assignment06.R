# Jen Luu
# Assignment 6
# 12/11/19

# 1. 
mycor <- function(m){
  if (class(m) =="matrix" && is.numeric(m)=="TRUE")
  {
    #get sd of all columns 
    xbars = apply(m, 2, mean)
    n = nrow(m)-1
    m2 = sweep(m, 2, xbars, FUN="-")
    m2 = m2^2
    sums = apply(m2, 2, sum)
    sds = sqrt(sums/n)
    
    #main function 
    columns = ncol(m)
    output = matrix(nrow=columns, ncol = columns)
    
    for (i in 1:columns)
    {
      for (j in 1:columns)
      {
        x=m[,i]-xbars[i]
        y=m[,j]-xbars[j]
        sums2=sum(x*y)
        output[i,j] = (sums2/n)/(sds[i]*sds[j])
      }
    }
    return(output)
  }
  else
  {
    return("Please input a nxp numerical matrix.")
  }
}

data = iris[, 1:4]
data = data[1:10,]
data = as.matrix(data)
cor(data)
mycor(data)


# 2.a 
group1 = sleep[sleep$group=="1",]$extra
group2 = sleep[sleep$group=="2",]$extra

# paired = T because the patients in the two groups are the same
t.test(group1, group2, paired = T)

# Null hypothesis: The average increase of hours of sleep is not significantly
# different from 0. 

# P-value is 0.002833, which is < 0.05. Therefore, we reject the null hypothesis. The
# difference in the average increase of hours of sleep between the two groups is not due to 
# chance. 

# 2.b
# Null hypothesis: There is no difference between the variances of the two groups. 
var1 = var(group1)
var2 = var(group2)
var2>var1
f = var2/var1

p = 1-pf(f,length(group1)-1, length(group2)-1)
# p-value is 0.37136 > 0.05 
# Therefore, we fail to reject the null hypothesis. The difference in variances is due to chance. 

# 3. 
# Null hypothesis: The variables Class and Survived for adult male passengers are not 
# independent. 
load("Titanic2.rdata")
data = Titanic2 
data$Age[data$Age=="child"] = "Child" 
data[7, 5] = 0 # Change -17 to 0 for Freq
data = data[-c(2,4), ] # remove rows with NA in Survived 

males = data[which(data$Sex=="Male"),]
adultMales = males[which(males$Age=="Adult"),]

tab = table(adultMales$Class, adultMales$Survived)
tab[c(1,2,3,4), 1] = c(118, 154, 387, 670)
tab[c(1,2,3,4), 2] = c(57, 14, 75, 192)

chisq.test(tab)
# p-value = 2.843e-08 < 0.05
# We reject the null hypothesis. So yes, the variables Class and Survived for
# adult male passengers are independent. 