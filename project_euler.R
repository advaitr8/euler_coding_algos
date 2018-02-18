#####################
#####Problem 1#######
#####################
N <- 999
three_mults <- vector()
five_mults <- vector()
fifteen_mults <- vector()
for(i in 1:N){
  if(i %% 3 == 0 & i %% 15 != 0){
    three_mults[i] <- i
  }
  if(i %% 5 == 0 & i %% 15 != 0){
    five_mults[i] <- i
  }
  if(i %% 15 == 0){
    fifteen_mults[i] <- i
  }
}

three_mults <- three_mults[complete.cases(three_mults)]
five_mults <- five_mults[complete.cases(five_mults)]
fifteen_mults <- fifteen_mults[complete.cases(fifteen_mults)]
sum(three_mults, five_mults,fifteen_mults)

#####################
#####Problem 2#######
#####################
fib_nums <- vector()
fib_num_gen <- function(n){
  for(i in 3:n){
    fib_nums[1] <- 1
    fib_nums[2] <- 2
    fib_nums[i] <- fib_nums[i-1] + fib_nums[i-2]
    }
  return(fib_nums)
  } 
fib_num_gen(10)

#generate fibonacci numbers until the last number generated is less than 4 million

#this bit can generate Fib numbers until the specified threshold
fibgen <- function(prev_num = 1, last_num = 2, threshold){
  res <- vector() #this creates an empty vector
while((last_num + prev_num) < threshold){
  temp <- last_num
  temp <- last_num + prev_num
  prev_num <- last_num
  last_num <- temp
  res <- c(res, last_num) #this makes a vector res of all the even Fib nums
}
  return(sum(res[res %% 2 == 0]))
}
fibgen(1,2,4000000) + 2 # I add 2 here for the first 2 in the series.
#########################
#Trying an alternate without vector res
fibgen <- function(prev_num = 1, last_num = 2, threshold){
  res = 0 + 2 
  while((last_num + prev_num) < threshold){
    temp <- last_num
    temp <- last_num + prev_num
    prev_num <- last_num
    last_num <- temp
    if(last_num %% 2 == 0)
    res <- res + last_num
  }
  return(res)
}
fibgen(1,2,4000000)



#####################
#####Problem 3#######
#####################

#1. check if number is divisible by 2. If yes, stop. COMPOSITE
#2. check if number divisible by d = 3, if yes stop and print is divisible by 3, COMPOSITE. else change d to 5 and so on.
#3.if d > sqrt(n), STOP and call the number a prime.

#First I try trial division
is_prime <- function(n, d = 3){
  # browser()
  if(n %% 2 == 0)
    print("number is composite")
  else{
    while(d < sqrt(n)){
    if(n %% d == 0){
      cat("composite number is divisible by", d)
      break
      }
    if(n %% d != 0){
        d <- d + 2
      }
   }
  } 
  if(d > sqrt(n) & n %% d != 0){
    cat("prime")
  }
}
is_prime(600851475143)
####
#This program spits the first factor out and ends
#I want to write one that spits all the factors until it hits the sqrt of the number. If it has not found a single factor until then it should say prime.
#####

#1.get a number
#2.divisible by 2, divide the quotient by 2 and continue until the quotient is not divisible by two anymore, if it not divisible by two any more then STOP and print the number that is not divisible by 2. Simultaneously count the number of times you divide by two, or iterations.
times_divide_2 <- function(n){
  browser()
  if(n %% 2 != 0){
    cat(n,"is not divisible by 2")
  }
  else{
    i <- 0
    while(TRUE){
    if(n %% 2 == 0){
      n <- n/2
      i <- i + 1
    }
    if(n %% 2 != 0){
      cat(n, "is not divisible by 2, we've stopped \n")
      cat(i, "is the largest power of 2")
      break
    }
    }
  }
}
times_divide_2(36)

####################################################
####################################################
prime_factorization <- function(n, d = 3){
  # start.time <- Sys.time()
  browser()
  prime_factors <- vector()
  while(TRUE){
  if(n %% 2 == 0){
    n <- n/2
    prime_factors <- c(prime_factors, 2)
  }
  if(n %% 2 != 0){
    break
  }
  }
  while(TRUE){
  if(n %% d == 0){
    n <- n/d
    prime_factors <- c(prime_factors, d)
  }
  if(n %% d != 0){
    d <- d + 2
  }
    if(d > n){
      break
    }
  }
  cat("prime factors", prime_factors,"\n")
  cat("largest prime factor", tail(prime_factors, n=1), "\n")
  # end.time <- Sys.time()
  # time.taken <- end.time - start.time
  # time.taken
}
prime_factorization(36)
  


#####################
#####Problem 4#######
#####################
#1. write a program to test a palindrome
#2. construct the matrix of products
#3. return all palindromes
#4. pick the largest.
prods <- matrix(NA, nrow = 900, ncol = 900)
for(i in 1:900){
  for(j in 1:900){
    prods[i,j] <- (i+99)*(j+99)
  }
}
prods


#a function to reverse a number
rev_nums <- function(n){
  browser()
  pal_vec <- vector()
  rev <- 0
  temp <- n
  while(TRUE){
    rev <- 10*rev + temp%%10
    temp <- floor(temp/10)
    if(temp == 0){
      if(rev == n){
        # pal_vec <- c(pal_vec, n)
        cat(rev, "is a palindrome","\n")
      }
      break
      }
  }
 }
#test example
x <- 121
y <- 124
p <- 345
q <- 323

test_mat <- matrix(c(x,y,p,q), nrow = 2, ncol = 2)
for(i in 1:2){
  for(j in 1:2){
    rev_nums(test_mat[i,j])
  }
}
####

rev_nums(prods[prods > 900000])
lapply(prods[prods > 900000], rev_nums)
big <- prods[prods > 906600 & prods < 906610]

for(i in 1:900){
  lapply(big, rev_nums)
}

#I did not do this one properly. 


#######
# ROSS FUNKY QUESTION
######
rm(list = ls())
#create a 100 open doors (1 = open, 0 = close)
y <- rep(1 ,times = 100)

test <- function(start = 0){
  #for each door
  for(i in 1:100){
    #for each try
    for(j in 2:100){
      #for all multiples of j
      if(i%%j == 0 & y[i] == 0){
        y[i] <- 1 #change state
        next
      }
      #for all multiples of j
      if(i%%j == 0 & y[i] == 1){
        y[i] <- 0 #change state
        next
      }
      print(which(y == 1))
    }
  }
  return(which(y == 1)) #return the open doors
}
test(start = 0) #call the function


############
#FIZZBUZZ
############
test2 <- function(start = 0){
  for(i in 1:100){
    if(i%%3 == 0 & i%%5 != 0){
      print("fizz")
    }
    else if(i%%5 == 0 & i%%3 != 0){
      print("buzz")
    }
    else if(i%%3 == 0 & i%%5 == 0){
      print("fizzbuzz")
    }
    else(print(i))
  }
  
}
test2(start = 0)



