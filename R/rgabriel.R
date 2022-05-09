###rgabriel
###Function for performing the procedure described in Gabriel's 1978 paper
###28 Dec 2013

#' the length of bar for Gabriel's barplot
#' 
#' Show the upper or lower confidence interval of Gabriel's barplot.
#' 
#' As shown in Gabriel's paper,use M(alpha,k*,v), the upper alpha point of the
#' Studentized Maximum Modulus of k* normals and v df. And this method is a
#' graphical way for visually mutiple comparision.
#' 
#' @param x data vector
#' @param f factor vector
#' @param a alpha level of mutiple comparison.
#' @return \item{vstar}{the length of the bar for mutiple comparision}
#' @author Yihui XIE
#' 
#' Miao YU
#' @seealso \code{\link{gabriel.plot}}
#' @references Gabriel, K.R., 1978. A Simple Method of Multiple Comparisons of
#' Means. Journal of the American Statistical Association 73, 724.
#' 
#' Stoline, M.R., Ury, H.K., 1979. Tables of the Studentized Maximum Modulus
#' Distribution and an Application to Multiple Comparisons among Means.
#' Technometrics 21, 87.
#' @keywords Gabriel plot
#' @examples
#' 
#' # equal numbers
#' 
#' g <- c(1:50)
#' f <- c(rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(5,10))
#' gabriel.plot(g,f,rgabriel(g,f))
#' 
#' # unequal numbers
#' 
#' g <- c(1:40)
#' f <- c(rep(1,3),rep(2,12),rep(3,15),rep(4,5),rep(5,5))
#' gabriel.plot(g,f,rgabriel(g,f))
#' 
#' 
#' @export rgabriel
rgabriel <- function (x, f, a = 0.05)
{
  f  <- as.factor(f)
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
  s <- tapply(x,f,stats::sd)
  dfstar <- sum(df)
  # the following code is written by Yihui according to Stoline's 1979 paper(equation 2.2). 
  psmm_x = function(x, c, r, nu) {
    snu = sqrt(nu)
    sx  = snu * x  # for the scaled Chi distribution
    lgx = log(snu) - lgamma(nu/2) + (1 - nu/2) * log(2) + (nu - 1) * log(sx) + (-sx^2/2)
    exp(r * log(2 * stats::pnorm(c * x) - 1) + lgx)
  }
  psmm = function(x, r, nu) {
    res = stats::integrate(psmm_x, 0, Inf, c = x, r = r, nu = nu)
    res$value
  }
  qsmm = function(q, r, nu) {
    r = (r * (r - 1)/2)
    if (!is.finite(nu)) return(stats::qnorm(1 - .5 *(1 - q^(1/r))))
    res = stats::uniroot(function(c, r, nu, q) {
      psmm(c, r = r, nu = nu) - q
    }, c(0, 100), r = r, nu = nu, q = q)
    res$root
  }
  SR <- qsmm(1-a, k, dfstar)
  vstar <- SR*s/sqrt(2*n)
  return(vstar)
}
