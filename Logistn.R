Logistn <- function(slope, intercept = 0, E1 = 5, E2 = 20, sigma = 1){
  #   Macro written by Stephen Senn February 2013 and adapted to R by Anna Schritz June 2017
  #   Purpose to produce approximate sample size determination
  #   for logistic regression with a continuous covariate
  #   The macro calculates approximate sample size
  #   required for given type I and type II error rates for
  #   testing that a slope is zero
  #   Argument "slope" must be set in invoking macro.
  #   The other arguments can be set by user but have defaults as follows
  #   Intercept =0. This default level assumes that when the covariate
  #   is zero, the probability of a response will be 50%
  #   type_1=5 the type 1 error rate in percent
  #   type_2=20 the type 2 error rate in percent
  #   sigma=1 the standard deviation of the covariate
  #   slope = 
  #   intercept  = log(Odds)
  #   E1 = Type 1 Error rate (in percentage)
  #   E2 = Type 2 Error rate (in percentage)
  #   T1 = Type 1 Error rate
  #   T2 = Type 2 Error rate
  
  
  a = intercept
  b = slope
  
  # Beginn preliminary calculations
  T1 <- E1/100
  T1_2 <- T1/2
  P1 <- 1- T1_2
  z1 <- qnorm(P1)
  T2 = E2/100
  P2 = 1-T2
  z2 <- qnorm(P2)
  power = 100* P2
  
  # Use approximate formula
  # Note: use of hyperbolic cosine coh()
  n <- 2*ceiling((2*(sigma^2)*(cosh(a) *cosh(b/2)+1)*(z1+z2)^2) / (2*(b^2)))
  
  data = data.frame("Type I error rate" = T1, "Standard deviation of predictor" = sigma, "intercept" = a, "slope" = b, "OR" = exp(slope),
                    "power" = power, "n" = n)
  
  return(data)
}
