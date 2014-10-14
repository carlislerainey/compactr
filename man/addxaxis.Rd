\name{addxaxis}
\alias{addxaxis}
\title{Add an x axis to the current plot}
\usage{
  addxaxis()
}
\description{
  This function adds an x-axis to the current plot.
  Intended for use when the plot does not fall along the
  bottom row, but you plan to put no plot beneath it.
}
\examples{
par(mfrow = c(2,2), mar = c(.75,.75,.75,.75), oma = c(3,3,1,1))
eplot(xlim = c(-1, 1), ylim = c(-1, 1), xlab = "X Label")
aplot()
addxaxis()
aplot()
}
\author{
  Carlisle Rainey
  (\href{mailto:carlislerainey@gmail.com}{e-mail},
  \href{http://www.carlislerainey.com}{website})
}

