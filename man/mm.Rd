\name{mm}
\alias{mm}
\title{Find the minimum and maximum of a vector}
\usage{
  mm(x)
}
\arguments{
  \item{x}{a vector}
}
\description{
  The function \code{mm()} finds the minimum and maximum of
  a vector. It is intended for use with \code{eplot()} to
  properly scale the axes.
}
\examples{
x <- rnorm(100)
y <- rnorm(100)

par(mfrow = c(1,1), mar = c(5,4,4,2), oma = c(0,0,0,0))
eplot(x, y, xlim = mm(x), ylim = mm(y))
}
\author{
  Carlisle Rainey
  (\href{mailto:carlislerainey@gmail.com}{e-mail},
  \href{http://www.carlislerainey.com}{website})
}

