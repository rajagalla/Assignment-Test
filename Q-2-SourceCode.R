



library(GA)

#library(genalg)
library(ggplot2)


##with given below data The goal of this problem is to put as much #stuff into a container as it will hold while optimizing for #constraints such as item weight, size and value



##Item    Value   Weight(kg)    volume

##Flour   1680      0.265       0.41
##Butter  1440      0.5         0.13
##Sugar   1840      0.441       0.29

##intution/assumptions as below

# we are trying to fit as much as items with maximizing the value and here weight limit should be less than equal to 10 and volume should not exceed 4

##define x1 and x2 and x3 as weight of items in kg

# Here volume is related to weight
# and we can see from data that volume of  one kg flour = 0.41/0.265 is # and we can derive volume for other items in similar way

##and weight limit is less than 10 which is x1+x2+x3 <= 10

# and we create objective function with maximizing the value with given #constraints of weight and volume.
#Here objective function comes as summation of multiplying the weight of item with value divided by its volume 

##define the range of x1 and x2 and x3 as below

x1 <- c(seq(0,10),by = 0.01)
x2 <- c(seq(0,10),by = 0.01)

x3 <- c(seq(0,10),by = 0.01)


##creating the objective function

f <- function(x) {
  f1 <- (1680*x[1])/0.265+(1440*x[2])/0.5+(1840*x[3]/0.441)
  g1 <- x[1]+x[2]+x[3]-10
  g2 <- ((0.41*x[1])/0.265+(0.13*x[2])/0.5+(0.29*x[3])/0.441)-4
  #g3 <- (1680*x[1])/0.265+(1440*x[2])/0.5+(1840*x[3])
  #g3 <- 0.265*x[1]+0.5*x[2]+0.441*x[3]-3
  #g4 <- 0.41*x[4]+0.13*x[5]+0.29*x[6]-5
  #g3 <- 0.265*x[1]+0.41*x[4]-1680
  #g4 <- 0.265*x[2]+0.41*x[5]-1440
  #g5 <- 0.265*x[3]+0.41*x[6]-1840
  
  #penalized constrarint violation
  f = ifelse(g1 <= 0 & g2 <= 0,f1,-1e4)
  #f = ifelse(g3 <= 0 & g4 <= 0  ,f1,-1e4)
  
}



GA <- ga(type = "real-valued",fitness = f,lower = c(0,0,0),upper = c(10,10,10),popSize = 200,maxiter = 300)

#points(GA@solution[1], GA@solution[2],pch = 20,col=6)

summary(GA)

GA@solution



######################################################
###with different constraints of weight and volume as 3 and 5




##creating the objective function

f <- function(x) {
  f1 <- (1680*x[1])/0.265+(1440*x[2])/0.5+(1840*x[3]/0.441)
  g1 <- x[1]+x[2]+x[3]-3
  g2 <- ((0.41*x[1])/0.265+(0.13*x[2])/0.5+(0.29*x[3])/0.441)-5
  #g3 <- (1680*x[1])/0.265+(1440*x[2])/0.5+(1840*x[3])
  #g3 <- 0.265*x[1]+0.5*x[2]+0.441*x[3]-3
  #g4 <- 0.41*x[4]+0.13*x[5]+0.29*x[6]-5
  #g3 <- 0.265*x[1]+0.41*x[4]-1680
  #g4 <- 0.265*x[2]+0.41*x[5]-1440
  #g5 <- 0.265*x[3]+0.41*x[6]-1840
  
  #penalized constrarint violation
  f = ifelse(g1 <= 0 & g2 <= 0,f1,-1e4)
  #f = ifelse(g3 <= 0 & g4 <= 0  ,f1,-1e4)
  
}



GA_2 <- ga(type = "real-valued",fitness = f,lower = c(0,0,0),upper = c(10,10,10),popSize = 200,maxiter = 300)

#points(GA@solution[1], GA@solution[2],pch = 20,col=6)

summary(GA_2)

GA_2@solution











