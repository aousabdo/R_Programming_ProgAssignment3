MaxX <- function(Nfails, Nshots, max=0.02, tolerance=1e-3, Threshold=0.0){
  repeat {
    value <- pbinom(Nfails, Nshots, max)
    if(abs(value-Threshold) < tolerance){
      break
    }
    else{
      max <- max + 0.001
    }
  }
  return(max)
}

