\name{Estimates}
\alias{Estimates}
\title{Estimates function for popPK}
\description{Creates tables of NONMEM parameter estimates.}
\usage{
	 Estimates(info)
}
\arguments{\item{info}{See \link{popPK}}}

\value{
  Creates data.frame with parameter estimates (ThetaEstimates) for Covariates function, tables of NONMEM parameter estimates (NMestimates), 
  summary of individual parameter estimates (ParameterSummary), 
  scatter plot matrices of individual and random-effects parameters (ParmSplom and RanSplom), and
  histograms of individual and random-effects parameters (ParmHist and RanHist).
}
\note{
  For examples see \link{popPK}.
  }

\author{Christoffer W. Tornoe & Fang Li \email{pharmacometrics@fda.hhs.gov}}

\keyword{models}
