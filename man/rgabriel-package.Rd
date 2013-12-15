\name{rgabriel-package}
\alias{rgabriel-package}
\docType{package}
\title{
Gabriel Multiple Comparison Test and Plot the Confidence Interval on Barplot
}
\description{
Functions for conducting and plotting Gabriel's (1978) multiple comparison test accounting for unequal variance and unequal sample sizes.
}
\details{
\tabular{ll}{
Package: \tab rgabriel\cr
Type: \tab Package\cr
Version: \tab 0.4\cr
Date: \tab 2013-12-15\cr
License: \tab GPL (>= 2)\cr
}
This package was created to analyze multi-level one-way experimental designs. It is designed to handle vectorized observation and factor data where there are unequal sample sizes and population variance homogeneity can not be assumed.To conduct the Gabriel test, create two vectors: one for your observations and one for the factor level of each observation. The function, rgabriel, conduct the test and save the output as a vector to input into the gabriel.plot function, which produces a confidence interval plot for Multiple Comparison.  
}
\author{
Miao YU, PhD student in Research Center for Eco-Environmental Sciences(RCEES), Chinese Academy of Sciences

Maintainer: Miao YU <yufreecas@gmail.com>

}
\note{
I would like to acknowledge the invaluable help of the author of http://www.gmw.rug.nl/~boomsma/apstatdata/wilcox20.R who show a table of studentized maximum modulus's distribution.
}
  
\references{
Gabriel, K.R., 1978. A Simple Method of Multiple Comparisons of Means. Journal of the American Statistical Association 73, 724. 
http://www.gmw.rug.nl/~boomsma/apstatdata/wilcox20.R
}
\keyword{ package, plot, Gabriel }

\seealso{
\code{\link{rgabriel}}, \code{\link{gabriel.plot}}
}

\examples{

# equal numbers

g <- c(1:50)
f <- c(rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(5,10))
gabriel.plot(g,f,rgabriel(g,f))

# unequal numbers

g <- c(1:40)
f <- c(rep(1,3),rep(2,12),rep(3,15),rep(4,5),rep(5,5))
gabriel.plot(g,f,rgabriel(g,f))

}
