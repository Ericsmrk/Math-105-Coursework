# Returns vector of factors of the given input number
# Set the 2nd input to TRUE to print each comparison
# and to view the output vector
getFactor <- function(num,debugger = FALSE){
  foundFactors <- c()
  
  #main loop 
  for (i in 1:num) {
    if(debugger)
      cat("checking if",i,"is a factor of",num,"...\n")
    
    if(num %% i){#if number mod i is not zero do nothing
      if(debugger)
        print("Nope, not a factor!")
    }
    
    else{#if number mod i is zero we found a factor
      if(debugger)
        cat("Yes,",i,"is a factor of",num,"!!!!\n")
      
    foundFactors <- c(foundFactors, i)#add to our list of factors
    }
    if(debugger & i == num)
      cat("FACTORS FOUND:", foundFactors, "\n")
  }#end main loop
  
  return(foundFactors)
}

# Takes two numbers as input and returns the GCF of the two numbers
# Uses a helper function. Set the 3rd input to TRUE to see debugging
findMaxCommonFactor <- function(num1, num2, debugger = FALSE){
  
  #Get factors of each number
  num1Factors <- getFactor(num1, debugger)
  num2Factors <- getFactor(num2, debugger)
  
  #Get length of the first factor
  num1Length <- length(num2Factors)

  if(debugger)
    cat("Now checking for common factors between", num1, "and", num2, ".\n")
  
  #Get the intersection of the two vectors
  common <- intersect(num1Factors,num2Factors)
  
  if(debugger)
    cat("Found these common values:", common, ".\n")
  
  #Return the maximum of this intersection
  return(max(common))
}

findMaxCommonFactor(240,340)

