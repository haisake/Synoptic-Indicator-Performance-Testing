#USER DEFINED FUNCTIONS

#create a function to run the tests
test <- function(x, n, target, alt_direction, alpha){
  
  if (alt_direction =="A"){
    alt_direction <- "greater"
  }else if (alt_direction =="B"){
    alt_direction <- "less"
  }else{
    warning("Invalid alt_direction passed to test() in src.r")
    return(NULL)
  }

  temp <- binom.test(x, n, p=target, alternative=alt_direction, conf.level=(1-alpha))
  return(temp$p.value)
}
