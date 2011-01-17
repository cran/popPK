\name{Covariates}
\alias{Covariates}
\title{Covariates function for popPK}
\description{Creates individual parameter-covariate graphs, quantile summary graphs, and forest plot of covariate effects. Population predicted relationships specified in info$cov.rel are superimposed on the parameter-covariate graphs.}
\usage{
	 Covariates(info)
}
\arguments{\item{info}{See \link{popPK}}}

\value{
  Creates individual parameter-covariate (parm.vs.cov) and quantile summary (parm.vs.cov.quantile) graphs. 
  Graphs with population predicted relationships specified in info$cov.rel are superimposed on the parameter-covariate graphs (plotCov).
  Forest plot with covariate effects (ForestPlot)
}
\note{
  For examples see \link{popPK}.
  }

\author{Christoffer W. Tornoe & Fang Li \email{pharmacometrics@fda.hhs.gov}}

\keyword{models}
