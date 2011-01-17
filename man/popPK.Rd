\name{popPK}
\alias{popPK}
\title{Summary of Population Pharmacokinetic Analysis}
\description{This package creates graphs and tables for NONMEM runs}
\usage{
	popPK(info)
}

\arguments{
\item{info}{A list including the following elements:
	\describe{
		\item{\code{path}}{The path where the NONMEM control stream, output, data, and tables are located (Required).}
		\item{\code{run.no}}{The NONMEM run number (e.g. 1 for run1.mod/ctl) (Required).}

		\item{\code{alpha}}{Alpha level for parameter mean in quantile plot (default is 0.05).}
		\item{\code{bins}}{Cut-off points to group specified covariates by. The minumum and maximum value of the covariate will automatically be added and values outside the range of the covariate will be removed (default is to use quantiles).}
		\item{\code{cat.level}}{Substitute categorical covariates values with text string (e.g. SEX=list(Male='0', Female='1')).}
		\item{\code{cex}}{Size of symbols (default is 1, 1, 1, 1, 1, 1, 1, 1).}
		\item{\code{col}}{Colors for symbols (default is 'black', 'red', 'grey', 'blue', 'orange', 'green', 'yellow', 'brown').}
		\item{\code{cov.rel}}{List of identified parameter-covariate equations to plot (maximum of 1 continuous and 1 categorical covariate per equation) (default is NA).}
		\item{\code{covs}}{List of labels for covariates (default is covariate name in patab/catab).}
		\item{\code{device}}{Graphical device to print to (options: "wmf", "bmp", "jpg", "pdf", "png", or "tiff") (default is "pdf").}		
		\item{\code{digits}}{Number of significant digits to be used in tables.}
		\item{\code{drug.name}}{Drug name.}
		\item{\code{etas}}{List of labels for inter-individual variability parameters (default is omega name in control stream specified by ";").}
		\item{\code{group.by}}{Variable to group symbols in graphs by.}
		\item{\code{ind.plots}}{Plot individual concentration-time profiles (default is FALSE).}
		\item{\code{line.col}}{Line colors for parameter-covariate relationships (default is 'black', 'red', 'grey', 'blue', 'orange', 'green', 'yellow', 'brown').}
		\item{\code{lloq}}{Insert lower limit of quantification in goodness-of-fit graphs.}
		\item{\code{log.covs}}{Log transform covariates in Covariates function (default is FALSE).}
		\item{\code{log.obs}}{Log transform observations in Diagnostics function (default is FALSE).}
		\item{\code{log.parms}}{Log transform parameters in Covariates function (default is FALSE).}
		\item{\code{log.pred}}{Log transform predictions in Diagnostics function (default is FALSE).}
		\item{\code{lty}}{Line types for parameter-covariate equations (default is 1 (solid)).}
		\item{\code{lwd}}{Line width (default is 1).}
		\item{\code{mean}}{Parameter mean for quantile plots (default is "geometric", otherwise "arithmetic").}
		\item{\code{output}}{Output folder for graphs and tables (default is NA which will print to the screen).}
		\item{\code{pch}}{Symbols (default is 20 (closed circle), 1 (open circle), 15 (solid square), 22 (open square), 17 (solid triangle), 2 (open triangle), 215 (x), 3 (+)).}
		\item{\code{prefix}}{Prefix of your NONMEM control stream (default is run).}
		\item{\code{quantiles}}{Number of bins to group the covariates in (default is 4).}
		\item{\code{report}}{Create summary report in Word. This feature only works on Windows and requires R2wd package is installed along with statconnDCOM) (default is FALSE).}
		\item{\code{scale}}{Scale which the parameters were estimated in NONMEM (options='normal' or 'log').}
		\item{\code{smooth}}{Insert smooth (default is FALSE).}
		\item{\code{strat.by}}{Variable to stratify panels by in graphs.}
		\item{\code{suffix}}{NONMEM output table suffix (default is no suffix or .csv).}
		\item{\code{thetas}}{List of labels for fixed-effects parameters (default is theta name in control stream specified by ";").}
		\item{\code{units}}{Units for time and concentration.}
	}
}
}
\value{
  The popPK function creates standard graphs and tables to summarize population pharmacokinetic analysis using NONMEM.
}

\examples{
#Restrictions
	#No time-varying covariates
	#One $THETA, $OMEGA, and $SIGMA for each parameter in the NONMEM control stream
	#Only 1 continuous covariate pr equation in info$cov.rel
	#Only bivariate categorical covariates in info$cov.rel coded as THETA(X)**CAT

info <-list(
    		path=paste(R.home(), "/library/popPK/extdata",sep=""),
		run.no=1,
		output=NA,
		thetas=list(CL="Clearance (L/hr)",V="Volume of distribution (L)",
				KA="Absorption rate constant (1/hr)"),
		etas= list(ETA1="CL",ETA2="V",ETA3="KA"),
		covs = list(BW="Weight (kg)",AGE="Age (years)",ISM="Sex",RACE="Race"),
		cat.level=list(ISM = list(Male=1,Female=0),
				RACE = list(White=1,Black=2,Asian=3,Other=4)),
		group.by ="ISM",
		units = list(time="hr",conc="ng/mL"),
		drug.name = "Drug X",
		quantiles=4,
		log.pred=TRUE,log.obs=TRUE,
		bins=list(AGE=c(25,41)),
		cov.rel=list(CL~THETA(1)*1.01^ISM*(BW/70)^THETA(4),
				"Estimated exponent"=V~THETA(2)*(BW/70)^THETA(5),
				"Allometric"=V~THETA(2)*(BW/70)^1),
		report=TRUE
	)
 
\dontrun{
	xposeData(info)
	Demographics(info)
	Diagnostics(info)
	Estimates(info) 
	Covariates(info)}
}

\author{Christoffer W. Tornoe & Fang Li \email{pharmacometrics@fda.hhs.gov}}

\seealso{\link{xposeData}, \link{Demographics}, \link{Diagnostics}, \link{Estimates}, and \link{Covariates}}

\note{DISCLAIMER: This R package does not reflect the official policy of the FDA. No official endorsement by the FDA is intended or should be inferred. Use of the popPK package is at your own risk.}

\keyword{models}
