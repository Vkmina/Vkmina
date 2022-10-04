#In-class exercise (3 minutes)

#Let x <- 3 * seq_len(4). Select the 2nd element of x with (a) positive, (b) negative and (c) logical indices.
x <- 3 * seq_len(4)
x

x[2]
x[-c(1,2,3)]
x[c(F,T,F,F)]

#Sort x in descending order using (a) a sequence of positive indices and (b) the sort function.
#a)
x[length(x):1]
#b)
sort(x, decreasing = T)


for (number in 1:6){
  print(number)
}

for (i in 1:10) {
  if (!i %% 2){
    next
  }
  print(i)
}

i = 1
while (i <= 6){
  print(i)
  i = i-1
}
# this one will never stop


#Load a dataset using data(ToothGrow). 
#Create two vectors of tooth lengths corresponding to OJ and VC factors respectively. 
#Compute the mean of each vector.

rm(list = ls())

data("ToothGrowth")
force(ToothGrowth)
summary(ToothGrowth)

#The data is a datafram

#OJ = Orange Juice
indices_oj <- ToothGrowth$supp == "OJ"
x_oj <- ToothGrowth$len[indices_oj]
x_vc <- ToothGrowth$len[!indices_oj]

mean(x_oj)
mean(x_vc)

#VC= Ascorbic acide
install.packages("bootstrap")
library("bootstrap")

# or 
B = 10000 
B= 1e4
boot_oj <- boot_vc <- rep(NA_real_, B)
n_oj <- length(x_oj)
n_vc <- length(x_vc)

for(b in length(B)) {
  #Orange juice
  set.seed(b)
  x_star <- x_oj[sample(x = n_oj, size = n_oj, replace= TRUE)]
  boot_oj[b] <- mean(x_star)
  
  #VC
  x_star_star <- x_oj[sample(x = n_vc, size = n_vc, replace= TRUE)]
  boot_oj[b] <- mean(x_star_star)
  
  print(boot_oj[b])
}

for (i in x_oj) { {
  mean(x_oj[sample(x = 30, size = 30, replace= TRUE)])
}
  print(i)
}


mean(x_oj[sample(x = 30, size = 30, replace= TRUE)])
# or 
theta <- function(x){mean(x)}
bootstrap(x_oj, 10000, theta)

