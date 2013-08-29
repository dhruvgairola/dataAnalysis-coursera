count <- function(cause = NULL) {
  ## Check that "cause" is non-NULL; else throw error
  if(is.null(cause)) {
    stop("you've entered nothing")
  }
  
  ## Check that specific "cause" is allowed; else throw error
  ## Read "homicides.txt" data file
  
  homicides <- readLines("homicides.txt")
  
  if(cause == "asphyxiation") {
    i <- grep("Cause: [Aa]sphyxiation", homicides)
  } else if(cause == "blunt force") {
    i <- grep("Cause: [Bb]lunt force", homicides)
  } else if(cause == "other") {
    i <- grep("Cause: [Oo]ther", homicides)
  } else if(cause == "shooting") {
    i <- grep("Cause: [Ss]hooting", homicides)
  } else if(cause == "stabbing") {
    i <- grep("Cause: [Ss]tabbing", homicides)
  } else if(cause == "unknown") {
    i <- grep("Cause: [Uu]nknown", homicides)
  } else {
    stop("incorrect cause")
  }
  
  length(i)
  
  ## Extract causes of death
  ## Return integer containing count of homicides for that cause
}