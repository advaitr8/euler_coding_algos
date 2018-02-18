#To find the root of the polynomial
#1. Start with an arbitrary x_0
#2. evaluate f(x_0)
#3. evaluate f'(x_0)
#4. if f(x_0) <= epsilon, print x_0 and say that the root is x_0
#5. else if f(x_0) > epsilon, set x_1 = x_0 - (f(x_0)/f'(x_1))
#6. Go back to step 2.
compute_root <- function(poly, x = 0.1, epsilon = 0.0001){
  iter <- 0
  while(TRUE){
    iter <- iter + 1
  if (length(poly) == 1)
    cat("You have entered an integer", poly)
  else{
    x <- x
    f_x <- 0
    deriv_poly <- rep(NA, length(poly))
    d_x <- 0
    for(i in 1:length(poly)){
      f_x <- f_x + poly[i]*(x^(i-1))
      deriv_poly[i] <- (i-1)*poly[i]
      d_x <- d_x + deriv_poly[i]*(x^(i-2))
    }
  }
  if(abs(f_x) <= epsilon){
    cat("value of the function is", f_x, "\n")
    cat("the root is", x, "\n")
    cat("iterations = ", iter)
    break
  }
  if(abs(f_x) > epsilon)
    x <- x - (f_x/d_x)
  }
}
compute_root(c(-13.39, 0.0, 17.5, 3.0, 1.0))
