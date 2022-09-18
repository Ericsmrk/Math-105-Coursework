fib <- function(n){
  out <- c()
  if(n < 1)
    print("n must be greater than 0")
  if(n == 1)
    out <- c(1)
  else{
    num1 <- 0
    num2 <- 1
    iterator = 2
    out <- c(1)
    while(iterator < n){
      nextNum <- num1 + num2
      out <- c(out,nextNum)
      iterator = iterator + 1
      num1 <- num2
      num2 <- nextNum
    }
  }
  out
}
fib(10)
