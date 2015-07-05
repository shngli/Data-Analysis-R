smoother = function(x, y) {
  n = length(x)  
  a = smooth.spline(x,y)
  
  # Here the default it to have df (lambda) chosen
  # by generalized cross validation

  # Because smooth.spline always returns a vector of 
  # fitted values corresponding to the unique sorted x 
  # values we must make the fit be of the appropriate 
  # length and be in the appropriate order
  X = matrix(rep(a$x,each=n),nrow=n)
  i = max.col(-(X-x)^2,ties="first")
  fit = a$y[i]

  return(fit)
}
