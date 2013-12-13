\name{rgabriel-package}
\alias{rgabriel-package}
\alias{rgabriel}
\docType{package}
\title{
Gabriel Multiple Comparison Test Adjusted for Unequal Variances and equal Sample Sizes
}
\description{
        This package was created to analyze multi-level one-way
        experimental designs. It is designed to handle vectorized
        observation and factor data where there are equal sample
        sizes and population variance homogeneity can not be assumed.
        To conduct the Gabriel test, create two vectors: one for your 
        observations and one for the factor level of each observation. 
        The function, rgabriel, conduct the test and save the output as
	      a vector to input into the gabriel.plot function, which produces 
	      a confidence interval plot for Multiple Comparison.
}
\details{
\tabular{ll}{
Package: \tab rgabriel\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2013-12-11\cr
License: \tab GPL (>= 2)\cr
}
        This package was created to analyze multi-level one-way
        experimental designs. It is designed to handle vectorized
        observation and factor data where there are equal sample
        sizes and population variance homogeneity can not be assumed.
        To conduct the Gabriel test, create two vectors: one for your 
        observations and one for the factor level of each observation. 
        The function, rgabriel, conduct the test and save the output as
        a vector to input into the gabriel.plot function, which produces 
	      a confidence interval plot for Multiple Comparison. 
}
\author{
Miao YU

Maintainer: Miao YU <yufreecas@gmail.com>

}
\references{
Gabriel, K.R., 1978. A Simple Method of Multiple Comparisons of Means. Journal of the American Statistical Association 73, 724. 
}
~~ Gabriel ~~
~~ plot ~~
\keyword{ package, plot, Gabriel }
\seealso{
SNK
}
\examples{
g <- c(1:50)
f <- c(rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(5,10))
gabriel.plot(g,f,rgabriel(g,f))
}
