agecount <- function(age = NULL) {
  ## Check that "age" is non-NULL; else throw error
  if(is.null(age)) {
    stop("age is null")
  }
  
  str <- paste(age, " years old", sep="");
  
  r <- regexec(str, homicides)
  x <- regmatches(homicides, r)
  
  count <- 0
  
  for(i in 1:length(x)) {
    if(x[i] == str) {
      count <- count + 1
    }
  }
  
  
  count
  ## Read "homicides.txt" data file
  ## Extract ages of victims; ignore records where no age is
  ## given
  ## Return integer containing count of homicides for that age
}