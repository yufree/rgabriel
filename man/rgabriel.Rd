\name{rgabriel}
\alias{rgabriel}
\title{
the length of bar for Gabriel's barplot
}
\description{
Show the upper or lower confidence interval of Gabriel's barplot.
}
\usage{
rgabriel(x, f, a = 0.05)
}
\arguments{
  \item{x}{
data vector
}
  \item{f}{
factor vector
}
  \item{a}{
alpha level of mutiple comparison. 
}
}
\details{
As shown in Gabriel's paper,use M(alpha,k*,v), the upper alpha point of the Studentized Maximum Modulus of k* normals and v df. And this method is a graphical way for visually mutiple comparision.
}
\value{ 
\item{vstar}{the length of the bar for mutiple comparision}
}
\references{
Gabriel, K.R., 1978. A Simple Method of Multiple Comparisons of Means. Journal of the American Statistical Association 73, 724. 

Stoline, M.R., Ury, H.K., 1979. Tables of the Studentized Maximum Modulus Distribution and an Application to Multiple Comparisons among Means. Technometrics 21, 87.
}
\author{
Yihui XIE

Miao YU
}

\seealso{
\code{\link{gabriel.plot}}
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Gabriel }
\keyword{ plot }
