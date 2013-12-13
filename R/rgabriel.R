###rgabriel
###Function for performing the procedure described in Gabriel's 1978 paper
###13 Dec 2013

rgabriel <- function (x = "data vector", f = "factor vector", a = "alpha level") 
{
  if (a == "alpha level") {
    a = 0.05
  }
  if (class(f) != "factor") {
    f = factor(f)
  }
  Level.Name = levels(f)
  k = length(Level.Name)
  input = cbind(f[!sapply(is.na(x), all)], x[!sapply(is.na(x), 
                                                     all)])
  input = input[order(input[, 1]), ]
  f = factor(input[, 1])
  x = input[, 2]
  n = numeric(length = nlevels(f))
  for (i in 1:length(n)) {
    n[i] = length(f[f == i])
  }
  for (i in 1:length(n)) {
    if (i == 1) {
      df = numeric(length = length(n))
    }
    df[i] = n[i] - 1
  }
  s <- tapply(x,f,sd)
  SR = qtukey(p = a, nmeans = k, df = df, lower.tail = FALSE)
  vstar = SR*s/(2*sqrt(n))
  return(vstar)
}
