\name{rgabriel}
\alias{rgabriel}
\alias{rgabriel}
\docType{package}
\title{
Gabriel Multiple Comparison Test and Plot the Confidence Interval on Barplot
}
\description{
        This package was created to analyze multi-level one-way
        experimental designs. It is designed to handle vectorized
        observation and factor data where there are unequal sample
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
Version: \tab 0.3\cr
Date: \tab 2013-12-14\cr
License: \tab GPL (>= 2)\cr
}
        
        To conduct the Gabriel multiple comparison test, create two vectors: 
        one for your observations and one for the factor level of each observation. 
        The function, rgabriel, conduct the test and save the output as
        a vector to input into the gabriel.plot function, which produces 
	      a confidence interval plot for Multiple Comparison. 
        The function smmcrit could show the Studentized Maximum Modulus at alpha = 0.05 
        when the variate of SMM is less than 8 and smmcrit01 could show the 
        Studentized Maximum Modulus at alpha = 0.05 when the variate of SMM is less than 8
        They were got from a table from wilcox20.R from 
        http://dornsife.usc.edu/assets/sites/239/docs/Rallfun-v20 and included in the
        function rgabriel.
        Also the gabriel.plot could be used to plot an error bar on the barplot
        as long as you create the upper and lower confidence interval of you error bar.
        
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
# equal numbers
g <- c(1:50)
f <- c(rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(5,10))
gabriel.plot(g,f,rgabriel(g,f))
# unequal numbers
g <- c(1:40)
f <- c(rep(1,3),rep(2,12),rep(3,15),rep(4,5),rep(5,5))
gabriel.plot(g,f,rgabriel(g,f))
}
