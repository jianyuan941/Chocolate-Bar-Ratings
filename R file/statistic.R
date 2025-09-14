
#confidence interval = mean +- (z critical * standard error) 
#formula of standard error = standard deviation/ sqrt(sample size)

#z critical obtained through different method
#normal distribution = qnorm(percentage) ie, 0.95


qnorm(.975)
qbinom(.975, size = 20, prob =0.5)


# Formula Confidence Internval continuous distribution =/= Discrete distribution
# method 1 try to check for whether approximation can be used.

binom_approximate_norm_checking <- function(
  n = NULL, # n = sample size
  x = NULL, # p = number of occurance
  alpha = NULL, #Alpha is accepted risk or significant level
  checking = F
  ){
  
  #probability
  p <- x/n
  
  #1st criteria - probability is within 0.25 to 0.75
  if(p >0.25 & p <0.75){
      
      #2nd criteria - n is large(rule of thumb: np >= 5 and n(1-p)>=5)
      np <- n*p
      n1p <- n*(1-p)
      
      if(np >=5 & n1p >=5){
        calc_method <- "norm approximation"
        criteria_checking<- paste0("passed 2nd criteria: np -", np, " ;n1p -", n1p)
      }else{
        calc_method <- "Clopper–Pearson"
        criteria_checking <- paste0("failed 2nd criteria: np -", np, " ;n1p -", n1p)
      }
    }else{
        calc_method <- "Clopper–Pearson"
        criteria_checking <- paste0("failed 1st criteria: ",p)
    }
  
  #summary for criteria chekcing
  if(checking == T){
    return(paste0(calc_method,intToUtf8(10),criteria_checking))
  }
  
  if(calc_method == "Clopper–Pearson"){
    if(as.character(enquo(alpha)[2]) == "NULL"){
      "Kindly set risk level. ie, accepted risk = 0.05"
    }else{
      lower <- qbeta(alpha/2, x, n-x +1)
      upper <- qbeta(1 - alpha/2, x + 1, n - x)
      
      result_p <- c(lower, upper)
      result_p
    }
  }else{
    "NO"
  }
  

}

