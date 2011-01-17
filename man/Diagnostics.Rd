\name{Diagnostics}
\alias{Diagnostics}
\title{Diagnostic function for popPK}
\description{Creates concentration-time profiles and goodness-of-fit graphs for popPK.}
\usage{
	 Diagnostics(info)
}
\arguments{\item{info}{See \link{popPK}}}

\value{
  Creates concentration-time profile graphs (conc.vs.time and ind.plots) and goodness-of-fit graphs 
  (dv.vs.pred, dv.vs.ipred, wres.vs.time, iwres.vs.time, wres.vs.pred, iwres.vs.ipred, qqnorm.wres, 
  qqnorm.iwres, histogram.wres, histogram.iwres), and histograms (CovHist, CatHist).
}
\note{
  For examples see \link{popPK}.
  }

\author{Christoffer W. Tornoe & Fang Li \email{pharmacometrics@fda.hhs.gov}}

\keyword{models}
