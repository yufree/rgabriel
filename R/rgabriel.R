###rgabriel
###Function for performing the procedure described in Gabriel's 1978 paper
###27 Dec 2013

rgabriel <- function (x, f, a = "alpha level") 
{
  if (a == "alpha level") {
    a  <-  0.05
  }
  if (class(f) != "factor") {
    f  <-  factor(f)
  }
  Level.Name  <-  levels(f)
  k  <-  length(Level.Name)
  input  <-  cbind(f[!sapply(is.na(x), all)], x[!sapply(is.na(x), all)])
  f  <-  factor(input[, 1])
  x  <-  input[, 2]
  n  <-  numeric(length  <-  nlevels(f))
  for (i in 1:length(n)) {
    n[i]  <-  length(f[f == i])
  }
  for (i in 1:length(n)) {
    if (i == 1) {
      df  <-  numeric(length = length(n))
    }
    df[i]  <-  n[i] - 1
  }
  s <- tapply(x,f,sd)
  kstar <- choose(k,2)
  dfstar <- sum(df)
  psmm = function(x, r, nu) {
    ifelse(x > 0, (2 * pt(x, nu) - 1)^r, 0)
  }
  qsmm = function(q, r, nu) {
    res = uniroot(function(x, r, nu, q) {
      psmm(x, r = r, nu = nu) - q
    }, c(0, 100), r = r, nu = nu, q = q)
    res$root
  }
  SR <- qsmm(1-a/2, kstar, dfstar)
  vstar <- SR*s/sqrt(2*n)
  return(vstar)
}
