#rm(list = ls())
# Say you’ve made a $5,000 purchase on a credit card with 18% annual interest rate and 2% minimum monthly payment rate. After a year, how much is the remaining balance? Use the following equations.

#1.Minimum monthly payment = Minimum monthly payment rate x Balance (Minimum monthly payment gets split into interest paid and principal paid) 

#2.Interest Paid = Annual interest rate / 12 months x Balance

#3.Principal paid = Minimum monthly payment – Interest paid

#4.Remaining balance = Balance – Principal paid

##############
#Question 1
##############
# Paying the Minimum Problem 1
# Write a program to calculate the credit card balance after one year if a person only pays the minimum monthly payment required by the credit card company each month.
# Use raw_input() to ask for the following three floating point numbers:
# 1. the outstanding balance on the credit card
# 2. annual interest rate
# 3. minimum monthly payment rate
# For each month, print the minimum monthly payment, remaining balance, principle paid in the format shown in the test cases below. All numbers should be rounded to the nearest penny. Finally, print the result, which should include the total amount paid that year and the remaining balance.



calc_bal <- function(outstanding_bal, annual_int_rate, min_pmt_rate){
  monthly_pay <- rep(0,12)
  toward_int <- rep(0,12)
  toward_princ <- rep(0,12)
  rem_bal <- rep(0,12)
  for (i in 1:12){
  monthly_pay[i] <- min_pmt_rate*outstanding_bal 
  toward_int[i] <- (annual_int_rate/12)*outstanding_bal 
  toward_princ[i] <- monthly_pay[i] - toward_int[i] 
  rem_bal[i] <- outstanding_bal - toward_princ[i] 
  outstanding_bal <- outstanding_bal - toward_princ[i]
  cat("Month = ", i, "\n")
  cat("Minimum monthly payment = ", monthly_pay[i], "\n")
  cat("Principle paid = ", toward_princ[i], "\n")
  cat("Remaining balance = ", rem_bal[i], "\n")
  cat("\n")
  }
  cat("RESULT : \n")
  cat("Total amount paid = ", sum(toward_int + toward_princ), "\n")
  cat("Remaining balance = ", rem_bal[12])
  
}
calc_bal(4800,0.2,0.02)

#


##############
#Question 2
##############
#With increments of $10 as requested in the question

months_req_1 <- function(outstanding_bal, int_rate){
  fixed_min_pay <- 10
  current_bal <- outstanding_bal
  new_bal <- vector()
  i <- 0
  while(current_bal > 0){
    i <- i + 1 #month counter
  if(i <= 12){
  new_bal <- current_bal*(1 + int_rate/12) - fixed_min_pay  #update new balance at each step
  current_bal <- new_bal
  }
 else{
    i <- 1
    current_bal <- outstanding_bal
    fixed_min_pay <- fixed_min_pay + 10
    new_bal <- current_bal*(1 + int_rate/12) - fixed_min_pay  #update new balance at each step
    current_bal <- new_bal
 }
    cat("Month = ", i, "\n")
    cat("Fixed monthly = ", fixed_min_pay, "\n")
    cat("Remaining balance = ", new_bal, "\n")
    cat("\n")
  }
  cat("RESULT : \n")
  cat("Final balance = ", new_bal, "\n")
  cat("Number of months it took = ", i,"\n")
  cat("Minimum payment = ", fixed_min_pay)
  }

months_req_1(32000,0.2)

#with decimal increment to show it takes much longer, try $0.1
months_req_2 <- function(outstanding_bal, int_rate){
  fixed_min_pay <- 0.1
  current_bal <- outstanding_bal
  new_bal <- vector()
  i <- 0
  while(current_bal > 0){
    i <- i + 1 #month counter
    if(i <= 12){
      new_bal <- current_bal*(1 + int_rate/12) - fixed_min_pay  #update new balance at each step
      current_bal <- new_bal
    }
    else{
      i <- 1
      current_bal <- outstanding_bal
      fixed_min_pay <- fixed_min_pay + 0.01
      new_bal <- current_bal*(1 + int_rate/12) - fixed_min_pay  #update new balance at each step
      current_bal <- new_bal
    }
    cat("Month = ", i, "\n")
    cat("Fixed monthly = ", fixed_min_pay, "\n")
    cat("Remaining balance = ", new_bal, "\n")
    cat("\n")
  }
  cat("RESULT : \n")
  cat("Final balance = ", new_bal, "\n")
  cat("Number of months it took = ", i,"\n")
  cat("Minimum payment = ", fixed_min_pay)
}

months_req_2(32000,0.2)

#same thing with bisection search
# (upper_bound - lower_bound)/2 >= epsilon
# 
# months_req_3 <- function(outstanding_bal, int_rate, i = 0){
#   browser()
#   lower_bound <- outstanding_bal/12                             # lower bound
#   upper_bound <- (outstanding_bal*(1+(int_rate/12))**12)/12    # upper bound
#   current_bal <- outstanding_bal
#   
#   if(i <= 12){
#   for(i in 1:12){
#     months <- 0 + i
#     fixed_min_pay <- (lower_bound + upper_bound)/2 
#     interest <- current_bal*int_rate/12
#     current_bal <- current_bal + interest - fixed_min_pay
#     if(current_bal <= 0)
#       break
#   }}
#   
#     else if(current_bal < 0){
#       upper_bound <- fixed_min_pay
#       current_bal <- outstanding_bal}
#     
#     else{
#       lower_bound <- fixed_min_pay
#       current_bal <- outstanding_bal}
#   
#   cat("RESULT : \n")
#   cat("Final balance = ", current_bal, "\n")
#   cat("Number of months it took = ", i,"\n")
#   cat("Minimum payment = ", fixed_min_pay)
#   
# }
# months_req_3(32000,0.2)
#   
