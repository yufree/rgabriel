#' the Gabriel's barplot (or (l-u)-plot)
#' 
#' Make the Gabriel's barplot, if, and only if, their bar intervals are
#' disjoint, they are differ significantly. This function could also be used to
#' plot error bar when the bar vector is imported as upper or lower margin.
#' 
#' 
#' @param x data vector
#' @param f factor vector
#' @param upper the upper margin of error bar
#' @param lower the upper margin of error bar
#' @param length the length of error bar
#' @param ...  Arguments to be passed to methods, such as graphical parameters.
#' @author Miao YU
#' @seealso \code{\link{rgabriel}}, \code{\link{barplot}}
#' @references Gabriel, K.R., 1978. A Simple Method of Multiple Comparisons of
#' Means. Journal of the American Statistical Association 73, 724.
#' @keywords Gabriel plot error bar
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
#' @export gabriel.plot
gabriel.plot <- function(x, f, upper, lower=upper, length=0.1,...){
  f  <- as.factor(f)
  input  <-  cbind(f[!sapply(is.na(x), all)], x[!sapply(is.na(x), all)])
  f  <-  factor(input[, 1])
  x  <-  input[, 2]
  meang <- tapply(x,f,mean)
  tem <- graphics::barplot(t(meang),ylim=c(0,max(meang)+2*max(upper)),...)
  graphics::arrows(tem,meang+upper, tem, meang-lower, angle=90, code=3, length=length, ...)
}
