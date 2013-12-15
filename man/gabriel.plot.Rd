\name{gabriel.plot}
\alias{gabriel.plot}
\title{
the Gabriel's barplot (or (l-u)-plot)
}
\description{
Make the Gabriel's barplot, if, and only if, their bar intervals are disjoint, they are differ significantly. This function could also be used to plot error bar when the bar vector is imported as upper or lower margin.
}
\usage{
gabriel.plot(x, f, upper, lower = upper, length = 0.1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
data vector
}
  \item{f}{
factor vector
}
  \item{upper}{
the upper margin of error bar
}
  \item{lower}{
the upper margin of error bar
}
  \item{length}{
the length of error bar
}
  \item{...}{
Arguments to be passed to methods, such as graphical parameters. 
}
}

\references{
Gabriel, K.R., 1978. A Simple Method of Multiple Comparisons of Means. Journal of the American Statistical Association 73, 724. 
}
\author{
Miao YU
}

\seealso{
\code{\link{rgabriel}}, \code{\link{barplot}}
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

\keyword{ Gabriel }
\keyword{ plot }
\keyword{ error bar }
