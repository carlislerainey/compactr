\name{aplot}
\alias{aplot}
\title{Create an empty plot with compact axis notation}
\usage{
  aplot(main = NULL)
}
\arguments{
  \item{main}{a label for the subplot. Intended for
  labeling a each plot in a matrix. If you need a title for
  the entire matrix of plots, or a single plot, I recommend
  using a call to the \code{mtext()} function.}
}
\description{
  The \code{aplot()} function simply calls \code{eplot()}
  again, using the same arguments (with the exception of
  \code{main}).
}
\details{
  This function simply draws an empty plot with compact
  axis notation, to which the user can add points, lines,
  text, and so on. Also, if the plot appears as part of a
  matrix, the x-axis notation is suppressed unless the plot
  appears along the bottom row and the y-axis notation is
  suppress unless the plot appears along the left column.
}
\examples{
# run these lines one at a time to see what happens
par(mfrow = c(2,2))
eplot(xlim = c(-1, 1), ylim = c(0, 10))
aplot(main = "Hey Look! No axis labels.")
aplot(main = "But this one has them?!")
aplot(main = "And this one does just what you'd expect!")
# after a call to eplot() or aplot(), I just add
# whatever I want to the plot.
}
\author{
  Carlisle Rainey
  (\href{mailto:carlislerainey@gmail.com}{e-mail},
  \href{http://www.carlislerainey.com}{website})
}

