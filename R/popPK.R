  require(Hmisc)
  require(lattice)
  require(grid)
  require(xpose4)

  xpdb <- NULL

#=========================================popPK function  =============================================================================#
#Function Name: popPK()                                                                                                                #
#                                                                                                                                      #
#======================================================================================================================================#

popPK <- function(info){
	xposeData(info)
	Demographics(info)
	Diagnostics(info)
	Estimates(info)
	Covariates(info)
}


#=========================================function for printing graphs=================================================================#
#Function Name: printGraph()                                                                                                           #
#                                                                                                                                      #
#======================================================================================================================================#

printGraph <- function(name,device,height=7,width=7){
	if(device=="pdf") pdf(file=paste(name,".pdf",sep=""),height=height,width=width,onefile=FALSE)
	if(device=="wmf") win.metafile(file=paste(name,".wmf",sep=""),height=height,width=width)
	if(device=="bmp") tiff(filename=paste(name,".bmp",sep=""),width=480/7*width,height=480/7*height)
	if(device=="tiff") tiff(filename=paste(name,".tif",sep=""),width=480/7*width,height=480/7*height)
	if(device=="png") png(filename=paste(name,".png",sep=""),width=480/7*width,height=480/7*height)
	if(device=="jpg") jpeg(filename=paste(name,".jpg",sep=""),quality=100,width=480/7*width,height=480/7*height)
}


#=========================================function for adding to info list ============================================================#
#Function Name: CreateInfo()                                                                                                           #
#                                                                                                                                      #
#======================================================================================================================================#

CreateInfo <- function(info){
	if(sum(c("prefix","thetas","etas","covs","output","digits","device",
		          	"cat.level","group.by","strat.by","lloq", "suffix",      
				"scale","smooth","log.pred","log.obs","log.covs","log.parms","ind.plots",        
				"col","line.col","units","drug.name","quantiles","bins",    
				"cov.rel", "eta.plot","cex","lty","lwd","pch","mean","alpha","report")%in%names(info)==FALSE)>0){
		
		cat("\nThe following default settings were added to your info list:\n")

		if(is.null(info$group.by)){
			info$group.by <- NA
			cat(paste("\n   group.by=",info$group.by))
		}

		if(is.null(info$suffix)){
			info$suffix <- ".csv"
			cat(paste("\n   suffix=",info$suffix))
		}

		if(is.null(info$report)){
			info$report <- FALSE
			cat(paste("\n   report=",info$report))
		}

		if(is.null(info$col)){
			info$col <- c("black","red","grey","blue","orange","green","yellow","brown")
			cat(paste("\n   col=",paste(info$col,collapse=",")))
		}else{
			if(!is.na(info$group.by)){	
				if(length(info$col)<length(levels(xpdb@Data[,info$group.by]))){
					info$col <- c(info$col,info$col[1:(length(levels(xpdb@Data[,info$group.by]))-length(info$col))]) 
					cat(paste("\n   col=",paste(info$col,collapse=",")))
				}
			}
		}

		if(is.null(info$line.col)){
			info$line.col <- info$col
			cat(paste("\n   line.col=",paste(info$line.col,collapse=",")))
		}

		if(is.null(info$device)){
			info$device <- "pdf"
			cat(paste("\n   device=",info$device,sep=""))
		}

		if(is.null(info$pch)){
			info$pch<- c(20,1,15,22,17,2,215,3)
			cat(paste("\n   pch=",paste(info$pch,collapse=",")))
		}

		if(is.null(info$cex)){
			info$cex <- c(1,1,1,1,1,1,1,1)
			cat(paste("\n   cex=",paste(info$cex,collapse=",")))
		}
		if(is.null(info$lty)){
			info$lty <- rep(1,length(info$col))
			cat(paste("\n   lty=",paste(info$lty,collapse=",")))
		}
		if(is.null(info$lwd)){
			info$lwd<- rep(1,length(info$col))
			cat(paste("\n   lwd=",paste(info$lwd,collapse=",")))

		}

		if(is.null(info$digits)){
			info$digits <- 4
			cat(paste("\n   digits=",info$digits))
		}

		if(is.null(info$prefix)){
			info$prefix <- "run"
			cat(paste("\n   prefix=",info$prefix))
		}
		if(is.null(info$output)){
			info$output <- NA
			cat(paste("\n   output=",info$output))
		}

		if(is.null(info$thetas)){
			info$thetas <- NA
			cat(paste("\n   thetas=",info$thetas))
		}

		if(is.null(info$etas)){
			info$etas <- NA
			cat(paste("\n   etas=",info$etas))
		}

		if(is.null(info$covs)){
			info$covs <- NA
			cat(paste("\n   covs=",info$covs))
		}

		if(is.null(info$strat.by)){
			info$strat.by <- NA
			cat(paste("\n   strat.by=",info$strat.by))
		}

		if(is.null(info$lloq)){
			info$lloq <- NA
			cat(paste("\n   lloq=",info$lloq))
		}
		if(is.null(info$log.pred)){
			info$log.pred <- FALSE
			cat(paste("\n   log.pred=",info$log.pred))
		}

		if(is.null(info$log.obs)){
			info$log.obs <- FALSE
			cat(paste("\n   log.obs=",info$log.obs))
		}
		if(is.null(info$log.covs)){
			info$log.covs <- FALSE
			cat(paste("\n   log.covs=",info$log.covs))
		}

		if(is.null(info$log.parms)){
			info$log.parms <- FALSE
			cat(paste("\n   log.parms=",info$log.parms))
		}
		if(is.null(info$ind.plots)){
			info$ind.plots <- FALSE
			cat(paste("\n   ind.plots=",info$ind.plots))
		}
		if(is.null(info$scale)){
			info$scale <- "normal"
			cat(paste("\n   scale=",info$scale))
		}
		if(is.null(info$smooth)){
			info$smooth <- FALSE
			cat(paste("\n   smooth=",info$smooth))
		}
		#if(info$smooth!=TRUE) info$smooth <- NULL

		if(is.null(info$cat.level)){
			info$cat.level <- list(nocategory=NA)
			cat(paste("\n   cat.level=",info$cat.level))
		}

		if(is.null(info$units)){
			info$units = list(time="",conc="")
			cat(paste("\n   units=",paste(info$units,collapse=",")))
		}
		if(is.null(info$drug.name)){
			info$drug.name <- ""
			cat(paste("\n   drug.name=",info$drug.name))
		}

		if(is.null(info$quantiles)){
			info$quantiles <- 4
			cat(paste("\n   quantiles=",info$quantiles))
		}
		if(is.null(info$cov.rel)){
			info$cov.rel <- list(nocovrel=NA)
			cat(paste("\n   cov.rel=",info$cov.rel))
		}
		if(is.null(info$eta.plot)){
			info$eta.plot <- list(noetaplot=NA)
			cat(paste("\n   eta.plot=",info$eta.plot))
		}
		if(is.null(info$bins)){
			info$bins <- list(nobins=NA)
			cat(paste("\n   bins=",info$bins))
		}

		if(is.null(info$mean)){
			info$mean <- "geometric"
			cat(paste("\n   mean=",info$mean))
		}

		if(is.null(info$alpha)){
			info$alpha <- 0.05
			cat(paste("\n   alpha=",info$alpha))
		}
		cat("\n\n")
	}
	assign("info",info,envir=.GlobalEnv)
	return(info)

}

#=========================================function for importing xpose data ===========================================================#
#Function Name: xposeData()                                                                                                            #
#                                                                                                                                      #
#======================================================================================================================================#

xposeData <- function(info){
 
	if(sum(c("path","run.no")%in%names(info)==FALSE)>0){

		cat(paste("\nYou forgot the following required items in info: \"",c("path","run.no")[which(c("path","run.no")%in%names(info)==FALSE)],
				"\" to your info list\n",sep=""))

		cat("The script will terminate now")
		stop()
	}

	names(info) <- casefold(names(info),upper=FALSE)

	opath <- getwd()
	setwd(info$path)

	if(is.null(info$suffix)){
		info$suffix <- ".csv"
	}

	if(paste("sdtab",info$run.no,sep="")%in%dir(info$path)){
		xpdb <- xpose.data(info$run.no) 
	}else{
		xpdb <- xpose.data(info$run.no,tab.suffix=info$suffix)
	}

	xpdb@Prefs@Cat.levels <- 10
	xpdb@Prefs@Labels$CL <- "CL"
	xpdb@Prefs@Labels$V <- "V"

	assign("xpdb",xpdb,envir=.GlobalEnv)

	if(sum(c("prefix","thetas","etas","covs","output","digits","device",
		          	"cat.level","group.by","strat.by","lloq","suffix",       
				"scale","smooth","log.pred","log.obs","log.covs","log.parms","ind.plots",         
				"col","line.col","units","drug.name","quantiles","bins",    
				"cov.rel", "eta.plot","cex","lty","lwd","pch","mean","alpha","report")%in%names(info)==FALSE)>0){

		cat("xposeData function: The info list does not include all options\n")
		cat("CreateInfo function is being called from within the xposeData function\n")
		info <- CreateInfo(info)
	}

	if(info$scale=="log"){
		eval(parse(text=paste("xpdb@Data$",xpdb@Prefs@Xvardef$ipred," <- exp(xpdb@Data$",xpdb@Prefs@Xvardef$ipred,")",sep="")))
		eval(parse(text=paste("xpdb@Data$",xpdb@Prefs@Xvardef$pred," <- exp(xpdb@Data$",xpdb@Prefs@Xvardef$pred,")",sep="")))
		eval(parse(text=paste("xpdb@Data$",xpdb@Prefs@Xvardef$dv,"[xpdb@Data$",xpdb@Prefs@Xvardef$dv,"!=0] <- exp(xpdb@Data$",xpdb@Prefs@Xvardef$dv,"[xpdb@Data$",xpdb@Prefs@Xvardef$dv,"!=0])",sep="")))
 		assign("xpdb",xpdb,envir=.GlobalEnv)
	} 

	#xpdb@Estimates <- NULL

	if(length(grep(".1",xpdb@Prefs@Xvardef$dv))>0){
		xpdb@Prefs@Xvardef$dv <- substring(xpdb@Prefs@Xvardef$dv,first=1,last=(regexpr(".1",xpdb@Prefs@Xvardef$dv)[1]-1))
	
		if(sum(xpdb@Prefs@Xvardef$covariates%in%xpdb@Prefs@Xvardef$dv==TRUE)>0){
			xpdb@Prefs@Xvardef$covariates <- xpdb@Prefs@Xvardef$covariates[-which(xpdb@Prefs@Xvardef$dv==xpdb@Prefs@Xvardef$covariates)]
		}
	}

	for(i in 1:length(xpdb@Prefs@Xvardef$covariates)){
		if(xpdb@Prefs@Xvardef$covariates[i]%in%names(info$covs)){
			eval(parse(text=paste("xpdb@Prefs@Labels$",xpdb@Prefs@Xvardef$covariates[i]," <- info$covs[[which(xpdb@Prefs@Xvardef$covariates[",i,"]==names(info$covs))]]",sep="")))
		}
	}

	for(i in 1:length(xpdb@Prefs@Xvardef$parms)){
		if(xpdb@Prefs@Xvardef$parms[i]%in%names(info$thetas)){
			eval(parse(text=paste("xpdb@Prefs@Labels$",xpdb@Prefs@Xvardef$parms[i]," <- info$thetas[[which(xpdb@Prefs@Xvardef$parms[",i,"]==names(info$thetas))]]",sep="")))
		}
		if(xpdb@Prefs@Xvardef$parms[i]%in%names(info$etas)){
			temp <- paste("Inter-individual variability for",info$etas[[which(xpdb@Prefs@Xvardef$parms[i]==names(info$etas))]])
			eval(parse(text=paste("xpdb@Prefs@Labels$",xpdb@Prefs@Xvardef$parms[i]," <- temp",sep="")))
		}
	}

	eval(parse(text=paste("xpdb@Data$",xpdb@Prefs@Xvardef$id," <- as.integer(as.character(xpdb@Data$",xpdb@Prefs@Xvardef$id,"))",sep="")))
		
	assign("xpdb",xpdb,envir=.GlobalEnv)

	if(!is.na(info$output)){
		dir.create(info$output,showWarnings=FALSE)
		setwd(info$output)
	}else{
		setwd(opath)
	}

	#ThetaEstimates <- NULL
 	#assign("ThetaEstimates",ThetaEstimates,envir=.GlobalEnv)

	if(!is.na(info$output)) write.table(xpdb@Data,file="xpdb.csv", sep= ",",col.names = TRUE, row.names=FALSE,quote=FALSE)

	if(info$report==TRUE){

		if(length(grep("R2wd",row.names(installed.packages())))>0){
			require(R2wd)
		}else{
			cat("\nYou need to install the 'R2wd' package to generate the report\n")
			info$report <- FALSE
			assign("info",info,envir=.GlobalEnv)
		}
	}

	if(info$report==TRUE){	
		wdGet()
		wdNewDoc(paste("popPKrun",info$run.no,sep=""))

		wdTitle("Summary of Population PK Analysis",label="popPK")
		wdTitle("",label="blankTitle")

		wdSection("Demographics",label="demographics",newpage=TRUE)
		wdSection("",label="blankDemo")

		wdSection("Population PK Parameter Estimates",label="estimates",newpage=TRUE)
		wdSection("",label="blankEst",newpage=FALSE)

		wdSection("PK Parameter - Covariate Relationships",label="covariates",newpage=TRUE)
		wdSection("",label="blankCov",newpage=FALSE)
		wdSection("",label="blankCov2",newpage=FALSE)

		wdSection("Forest Plot",label="forest",newpage=TRUE)
		wdSection("",label="blankForest",newpage=FALSE)

		wdSection("Quantile Plots",label="quantile",newpage=TRUE)
		wdSection("",label="blankQuant",newpage=FALSE)

		wdSection("PK Parameter - Covariate Plots",label="parm",newpage=TRUE)
		wdSection("",label="blankParm",newpage=FALSE)

		wdSection("Inter-Individual Variability Parameter - Covariate Plots",label="ETA",newpage=TRUE)
		wdSection("",label="blankETA",newpage=FALSE)

		wdSection("Model Diagnostics",label="diagnostics",newpage=TRUE)
		wdSection("",label="blankDiag",newpage=FALSE)
		#wdSectionBreak(continuous=FALSE)

	}

	if(sys.parent()!=0) return(xpdb)
 } #end of Data function
  #-------------------------------------end----end----end---end-----Data------------------------------------------#

#=========================================function of Demographics ==================================================================#
#Function Name: Demographics()                                                                                                       #
#                                                                                                                                    #
#====================================================================================================================================#

Demographics <-function(info){

	names(info) <- casefold(names(info),upper=FALSE)

	if(is.null(xpdb)){
		cat("Demographics function: You forgot to load the data by running xposeData function first\n")
		cat("xposeData function is being called from within the Demographics function\n")
		xpdb <- xposeData(info)
	}

	if(sum(c("prefix","thetas","etas","covs","output","digits","device",
		          	"cat.level","group.by","strat.by","lloq","suffix",       
				"scale","smooth","log.pred","log.obs","log.covs","log.parms","ind.plots",         
				"col","line.col","units","drug.name","quantiles","bins",    
				"cov.rel", "eta.plot","cex","lty","lwd","pch","mean","alpha","report")%in%names(info)==FALSE)>0){
		
		cat("Demographics function: The info list has been changed and does not include all options\n")
		cat("CreateInfo function is being called from within the Demographics function\n")
		info <- CreateInfo(info)
	}

	if(!is.na(info$output)) while(dev.cur()!=1) dev.off()
	if(is.na(info$output)){
		if(length(grep(":",R.home()))>0) windows(rec=TRUE)
	}

	if(info$smooth!=TRUE) smooth <- NULL

	data <- xpdb@Data[match(unique(xpdb@Data[,xpdb@Prefs@Xvardef$id]),xpdb@Data[,xpdb@Prefs@Xvardef$id]),]

	covariatesCat <- rep(NA,length(xpdb@Prefs@Xvardef$covariates))

	for(i in 1:length(xpdb@Prefs@Xvardef$covariates)){
		covariatesCat[i] <- is.factor(eval(parse(text=paste("data$",xpdb@Prefs@Xvardef$covariates[i], sep=""))))
	}

  	ConCovariates <- data.frame(Covariate=rep(NA,sum(covariatesCat==FALSE)),
					N  = rep(NA,sum(covariatesCat==FALSE)),
					Mean=rep(NA,sum(covariatesCat==FALSE)),
					SD = rep(NA,sum(covariatesCat==FALSE)),
					Median=rep(NA,sum(covariatesCat==FALSE)),
                              Min = rep(NA,sum(covariatesCat==FALSE)),
					Max = rep(NA,sum(covariatesCat==FALSE)))

	CatLevels <- 0
	for(i in 1:length(xpdb@Prefs@Xvardef$covariates)){
		if(is.factor(eval(parse(text=paste("data$",xpdb@Prefs@Xvardef$covariates[i], sep=""))))==TRUE){
			CatLevels <- CatLevels + length(levels(eval(parse(text=paste("data$",xpdb@Prefs@Xvardef$covariates[i],sep="")))))
		}
	}

 	CatCovariates <- data.frame(Covariate=rep(NA,CatLevels),
                  Category = rep(NA,CatLevels),
			N  = rep(NA,CatLevels),
			Percentage = rep(NA,CatLevels))

	xpdbcon <- xpdb
	xpdbcon@Prefs@Xvardef$covariates <- xpdb@Prefs@Xvardef$covariates[!covariatesCat]

	xpdbcat <- xpdb
	xpdbcat@Prefs@Xvardef$covariates <- xpdb@Prefs@Xvardef$covariates[covariatesCat]

	panel.cor <- function(x, y, digits=2, prefix="Corr=", cex.cor, ...){
		usr <- par("usr"); on.exit(par(usr))
		par(usr = c(0, 1, 0, 1))
		r <- abs(cor(x, y))
		txt <- format(c(r, 0.123456789), digits=digits)[1]
		txt <- paste(prefix, txt, sep="")
		#txt <- substitute(paste(R^2,"=",txt),list(txt=txt))
		#if(missing(cex.cor)) cex.cor <- 0.5/strwidth(txt)
		#text(0.5, 0.5, txt, cex = cex.cor + r/2 )
		if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
		text(0.5, 0.5, txt, cex = cex.cor ) #*r

	}

	if(!is.na(info$output)){
		if(length(xpdbcon@Prefs@Xvardef$covariates)>1){
			printGraph("CovSplom",info$device)
			if(info$smooth==TRUE){
				pairs(xpdbcon@Data[,xpdbcon@Prefs@Xvardef$covariates],pch=20,cex=1,col="black",
					upper.panel=panel.cor,lower.panel=panel.smooth,gap=0,
					labels=as.character(xpdbcon@Prefs@Labels[names(xpdbcon@Prefs@Labels)%in%xpdbcon@Prefs@Xvardef$covariates]))
			}else{
				pairs(xpdbcon@Data[,xpdbcon@Prefs@Xvardef$covariates],pch=20,cex=1,col="black",
					upper.panel=panel.cor,gap=0,
					labels=as.character(xpdbcon@Prefs@Labels[names(xpdbcon@Prefs@Labels)%in%xpdbcon@Prefs@Xvardef$covariates]))
			}
			dev.off()
	
			printGraph("CovHist",info$device)
			cov.hist(xpdbcon,max.plots.per.page=length(xpdbcon@Prefs@Xvardef$covariates),aspect=1,
				main=NULL,hicol="lightgrey",varname.cex=0.7,axis.text.cex=0.7)
			dev.off()

			printGraph("CovHist%02d",info$device)
			print(cov.hist(xpdbcon,max.plots.per.page=4,#length(xpdbcon@Prefs@Xvardef$covariates),
				type="density",aspect="fill",main=NULL,hicol="lightgrey",x.cex=0.8,y.cex=0.8,scales=list(cex=1))
			)
			dev.off()

			printGraph("CatHist",info$device)
			print(cov.hist(xpdbcat,max.plots.per.page=length(xpdbcat@Prefs@Xvardef$covariates),
				main=NULL,hicol="lightgrey",varname.cex=0.7,axis.text.cex=0.7)
			)
			dev.off()

			printGraph("CatHist%02d",info$device)
			print(cov.hist(xpdbcat,max.plots.per.page=4,#length(xpdbcat@Prefs@Xvardef$covariates),
				type="density",aspect="fill",main=NULL,hicol="lightgrey",x.cex=0.8,y.cex=0.8,scales=list(cex=1))
			)
			dev.off()
		}	
	}else{
		if(length(xpdbcon@Prefs@Xvardef$covariates)>1){
			if(info$smooth){
				print(pairs(xpdbcon@Data[,xpdbcon@Prefs@Xvardef$covariates],pch=20,cex=1,col="black",
					upper.panel=panel.cor,lower.panel=panel.smooth,gap=0,
					labels=as.character(xpdbcon@Prefs@Labels[names(xpdbcon@Prefs@Labels)%in%xpdbcon@Prefs@Xvardef$covariates]))
				)
			}else{
				print(pairs(xpdbcon@Data[,xpdbcon@Prefs@Xvardef$covariates],pch=20,cex=1,col="black",
					upper.panel=panel.cor,gap=0,
					labels=as.character(xpdbcon@Prefs@Labels[names(xpdbcon@Prefs@Labels)%in%xpdbcon@Prefs@Xvardef$covariates]))
				)
			} 


			print(cov.hist(xpdbcon,max.plots.per.page=4,#length(xpdbcon@Prefs@Xvardef$covariates),
				type="density",aspect="fill",main=NULL,hicol="lightgrey",x.cex=0.8,y.cex=0.8,scales=list(cex=1))
			)
			print(cov.hist(xpdbcat,max.plots.per.page=4,#length(xpdbcat@Prefs@Xvardef$covariates),
				type="density",aspect="fill",main=NULL,hicol="lightgrey",x.cex=0.8,y.cex=0.8,scales=list(cex=1))
			)
		}
	}

 	j <- 1
 	k <- 1

 	for(i in 1:length(xpdb@Prefs@Xvardef$covariates)){
		covName <- xpdb@Prefs@Xvardef$covariates[i]
		if(xpdb@Prefs@Xvardef$covariates[i]%in%names(info$covs)) covName <- info$covs[[which(xpdb@Prefs@Xvardef$covariates[i]==names(info$covs))]]

      	if(is.factor(eval(parse(text=paste("data$",xpdb@Prefs@Xvardef$covariates[i], sep=""))))==FALSE){
			ConCovariates$Covariate[j] <- covName
			ConCovariates$Mean[j] <- signif(mean(eval(parse(text=paste("as.numeric(as.character(data$",xpdb@Prefs@Xvardef$covariates[i],"))",sep=""))),na.rm=TRUE),digits=info$digits)
			ConCovariates$SD[j] <- signif(sd(eval(parse(text=paste("as.numeric(as.character(data$",xpdb@Prefs@Xvardef$covariates[i],"))",sep=""))),na.rm=TRUE),digits=info$digits)
			ConCovariates$N[j] <- length(eval(parse(text=paste("as.numeric(as.character(data$",xpdb@Prefs@Xvardef$covariates[i],"))",sep=""))))
			ConCovariates$Median[j] <- signif(median(eval(parse(text=paste("as.numeric(as.character(data$",xpdb@Prefs@Xvardef$covariates[i],"))",sep=""))),na.rm=TRUE),digits=info$digits)
     		 	ConCovariates$Min[j] <- min(eval(parse(text=paste("as.numeric(as.character(data$",xpdb@Prefs@Xvardef$covariates[i],"))",sep=""))),na.rm=TRUE)
			ConCovariates$Max[j] <- max(eval(parse(text=paste("as.numeric(as.character(data$",xpdb@Prefs@Xvardef$covariates[i],"))",sep=""))),na.rm=TRUE)
     			j <- j + 1
     		}else{ 
			CatCovariates$Covariate[k] <- covName
			temp <- character()
			library(popPK)

			for(l in 1:length(levels(eval(parse(text=paste("data$",xpdb@Prefs@Xvardef$covariates[i],sep="")))))){
				if(l!=1) CatCovariates$Covariate[k] <- " "
				CatValue <- levels(eval(parse(text=paste("data$",xpdb@Prefs@Xvardef$covariates[i],sep=""))))[l]

				if(sum(is.na(info$cat.level)==TRUE)==0){
					if(sum(info$cat.level[[xpdb@Prefs@Xvardef$covariates[i]]]%in%CatValue==TRUE)>0){
						CatCovariates$Category[k] <- names(info$cat.level[[xpdb@Prefs@Xvardef$covariates[i]]][info$cat.level[[xpdb@Prefs@Xvardef$covariates[i]]]%in%CatValue])
					}else{
						CatCovariates$Category[k] <- levels(eval(parse(text=paste("data$",xpdb@Prefs@Xvardef$covariates[i],sep=""))))[l]
					}	
				}else{
						CatCovariates$Category[k] <- levels(eval(parse(text=paste("data$",xpdb@Prefs@Xvardef$covariates[i],sep=""))))[l]
				}

				CatCovariates$N[k] <- length(eval(parse(text=paste("data$",xpdb@Prefs@Xvardef$covariates[i],"[data$",xpdb@Prefs@Xvardef$covariates[i],"==levels(data$",xpdb@Prefs@Xvardef$covariates[i],")[l]]",sep=""))))

				CatCovariates$Percentage[k] <- round(100*(length(eval(parse(text=paste("data$",xpdb@Prefs@Xvardef$covariates[i],"[data$",xpdb@Prefs@Xvardef$covariates[i],"==levels(data$",xpdb@Prefs@Xvardef$covariates[i],")[l]]",sep=""))))/length(eval(parse(text=paste("data$",xpdb@Prefs@Xvardef$covariates[i],sep=""))))),1)
				k <- k + 1
			}
	    }#end else
	}

	if(!is.na(info$output)){
		write.table(ConCovariates, file = "ConCovariates.csv",col.names = TRUE, row.names=FALSE,quote=FALSE,sep=",")
		write.table(CatCovariates, file = "CatCovariates.csv",col.names = TRUE, row.name=FALSE,quote=FALSE,sep=",")
	}else{
		cat("\nSummary of Continuous Covariates:\n")
		print(ConCovariates)

		cat("\nSummary of Categorical Covariates:\n")
		print(CatCovariates)
	}

	if(info$report==TRUE){
		wdGoToBookmark("blankDemo")
		wdNormal("")
		wdBody(paste("The summary statistics of continuous and categorical demographic variables for ",info$drug.name," are shown in Table 1 and Table 2.",sep=""))
		
		wdBody("Table 1: Summary statistics of continuous demographics.")
		row.names(ConCovariates) <- NULL
		wdTable(format(ConCovariates))

		#wdSectionBreak(continuous=TRUE)
		wdGoToBookmark("BlankDemo")
		wdNormal("")
		wdBody("Table 2: Summary statistics of categorical demographics.")
		row.names(CatCovariates) <- NULL
		wdTable(format(CatCovariates[,1:4]))

		wdSave(paste("popPKrun",info$run.no,sep=""))
	}


}#---------------------end of Demographics function--------end-------------end-----------------end---------------------------




#=========================================function for diagnosis plots ================================================================#
#Function Name: Diagnostics()                                                                                                          #
#                                                                                                                                      #
#======================================================================================================================================#

Diagnostics <- function(info){

	names(info) <- casefold(names(info),upper=FALSE)

	if(is.null(xpdb)){
		cat("Diagnostics function: You forgot to load the data by running xposeData function first\n")
		cat("xposeData function is being called from within the Diagnostics function\n")
		xpdb <- xposeData(info)
	}

	if(sum(c("prefix","thetas","etas","covs","output","digits","device",
		          	"cat.level","group.by","strat.by","lloq","suffix",       
				"scale","smooth","log.pred","log.obs","log.covs","log.parms","ind.plots",         
				"col","line.col","units","drug.name","quantiles","bins",    
				"cov.rel", "eta.plot","cex","lty","lwd","pch","mean","alpha","report")%in%names(info)==FALSE)>0){
		
		cat("Diagnostics function: The info list has been changed and does not include all options")
		cat("CreateInfo function is being called from within the Diagnostics function")
		info <- CreateInfo(info)
	}
	
	if(!is.na(info$output)) while(dev.cur()!=1) dev.off()
	if(is.na(info$output)){
		if(length(grep(":",R.home()))>0) windows(rec=TRUE)
	}

	if(info$log.obs){
		atLabelsY <- c(seq(0.001,0.01,0.001),seq(0.02,0.1,0.01),seq(0.2,1,0.1),seq(2,10,1),seq(20,100,10),seq(200,1000,100),seq(2000,10000,1000),seq(20000,100000,10000))
		LabelsY <- c("0.001",rep("",8),"0.01",rep("",8),"0.1",rep("",8),"1",rep("",8),"10",rep("",8),"100",rep("",8),"1000",rep("",8),"10000",rep("",8),"100000")
		logY <- 10
	}else{
		atLabelsY <- NA
		LabelsY <- NA
		logY <- FALSE
	}

	if(info$log.pred){
		atLabelsX <- c(seq(0.001,0.01,0.001),seq(0.02,0.1,0.01),seq(0.2,1,0.1),seq(2,10,1),seq(20,100,10),seq(200,1000,100),seq(2000,10000,1000),seq(20000,100000,10000))
		LabelsX <- c("0.001",rep("",8),"0.01",rep("",8),"0.1",rep("",8),"1",rep("",8),"10",rep("",8),"100",rep("",8),"1000",rep("",8),"10000",rep("",8),"100000")
		logX <- 10
	}else{
		atLabelsX <- NA
		LabelsX <- NA
		logX <- FALSE
	}

	if(info$smooth!=TRUE) info$xsmooth <- NULL

	#Individual plots	#series of graphs
	if(info$ind.plots==TRUE){
		if(!is.na(info$output)){
			printGraph("ind.plots%02d",info$device)
			print(ind.plots(xpdb,main=NULL,aspect="fill",layout=c(3,3),smooth=info$xsmooth,
				xlb=list(paste("Time (",info$units$time,")",sep="")),
				ylb=list(paste(info$drug.name," concentration (",info$units$conc,")",sep="")),
				par.strip.text=list(cex=1.5),
				logy=info$log.obs, 
				key=list(x=0,y=1.15,corner=c(0,1),border=FALSE,transparent=TRUE,columns=1,between=1.5,between.columns=5,text.width.multiplier=1,
						text=list(c("Observations","Population predictions","Individual predictions"),cex=0.8),
						line=list(type=c("p","l","l"),lwd=2,pch=16,col=c("grey","blue","red"),lty=c(1,2,1))),
				scales=list(cex=1.5)	
			))
			dev.off() 
		}else{
			print(ind.plots(xpdb,main=NULL,aspect="fill",layout=c(3,3),smooth=info$xsmooth,
				xlb=list(paste("Time (",info$units$time,")",sep="")),
				ylb=list(paste(info$drug.name," concentration (",info$units$conc,")",sep="")),
				par.strip.text=list(cex=1.5),
				logy=info$log.obs, 
				key=list(x=0,y=1.15,corner=c(0,1),border=FALSE,transparent=TRUE,columns=1,between=1.5,between.columns=5,text.width.multiplier=1,
						text=list(c("Observations","Population predictions","Individual predictions"),cex=0.8),
						line=list(type=c("p","l","l"),lwd=2,pch=16,col=c("grey","blue","red"),lty=c(1,2,1))),
				scales=list(cex=1.5)	
			))
		}
	}

	#Obs/Ipred/Pred vs. time plot
      if(!is.na(info$strat.by)){
		plotConcTime <- dv.preds.vs.idv(xpdb,by=info$strat.by,
				subset=paste(xpdb@Prefs@Xvardef$dv,"!=0",sep=""),
				#aspect=1,
				layout=c(3,length(unique(as.character(xpdb@Data[,info$strat.by])))),pch=20,col="black",main=NULL,
				logy=info$log.obs, 
				scales=list(cex=1.5),
				xlb=list(paste("Time (",info$units$time,")",sep=""),cex=1.5),
				ylb=list(paste(info$drug.name," concentration (",info$units$conc,")",sep=""),cex=1.5),
				smooth=info$xsmooth,
				par.strip.text=list(cex=1.5))
	}else{
		plotConcTime <- dv.preds.vs.idv(xpdb,
				subset=paste(xpdb@Prefs@Xvardef$dv,"!=0",sep=""),
				#aspect=1,
				pch=20,col="black",main=NULL,aspect="fill",
				logy=info$log.obs,
				scales=list(cex=1.5),
				xlb=list(paste("Time (",info$units$time,")",sep=""),cex=1.5),
				ylb=list(paste(info$drug.name," concentration (",info$units$conc,")",sep=""),cex=1.5),
				smooth=info$xsmooth,
				par.strip.text=list(cex=1.5))
	}

	if(!is.na(info$output)){
		printGraph("Conc.vs.Time",info$device,6,12)
		print(plotConcTime)
		dev.off()
	}else{
		print(plotConcTime)
	}


	if(!is.na(info$group.by)){
		keyValue <- levels(xpdb@Data[,info$group.by])
		for(cat in 1:length(keyValue)){
			if(sum(is.na(info$cat.level)==TRUE)==0){
				if(sum(info$cat.level[[info$group.by]]%in%keyValue==TRUE)>0){
					keyValue[cat] <- names(info$cat.level[[info$group.by]][info$cat.level[[info$group.by]]%in%keyValue[cat]])
				}
			}
		}

		keyGroup <- info$group.by
		if(sum(is.na(info$covs)==TRUE)==0){
			if(sum(names(info$covs)%in%keyGroup==TRUE)>0){
				keyGroup <- info$covs[[which(names(info$covs)%in%keyGroup)]]
			}
		}

		if(sum(is.na(info$covs)==TRUE)==0){
			if(sum(names(info$covs)%in%keyGroup==TRUE)>0) keyGroup <- info$covs[[info$group.by]]
		}

		keytext <- list(paste(keyGroup,keyValue,sep=": "),cex=1)
		keyline <- list(type="p",pch=info$pch[1:length(levels(xpdb@Data[,info$group.by]))],
					cex=info$cex[1:length(levels(xpdb@Data[,info$group.by]))],
					lwd=info$lwd[1:length(levels(xpdb@Data[,info$group.by]))],
					col=info$col[1:length(levels(xpdb@Data[,info$group.by]))])
	}else{
		keytext <- list("",cex=0.001)
		keyline <- list(type="p",col=0,cex=.001)	
		keyGroup <- ""
	}

	#dv.vs.ipred(xpdb)
	plot1 <- xYplot(
			xpdb@Data[,xpdb@Prefs@Xvardef$dv]~xpdb@Data[,xpdb@Prefs@Xvardef$ipre],
			#eval(parse(text=paste(xpdb@Prefs@Xvardef$dv,"~",xpdb@Prefs@Xvardef$ipre,sep=""))),
			subset=eval(parse(text=paste(xpdb@Prefs@Xvardef$dv,"!=0 & ",xpdb@Prefs@Xvardef$pred,"!=0",sep=""))),
			data=xpdb@Data,
			layout=c(1,1),
			if(!is.na(info$group.by))groups=eval(parse(text=info$group.by)),
			aspect=1/1,
			#xlim=c(min(xpdb@Data$DV)-0.15*xpdb@Data$DV,ceiling(max(xpdb@Data$DV))),
			#ylim=c(min(xpdb@Data$DV)-0.15*xpdb@Data$DV,ceiling(max(xpdb@Data$DV))),
			xlab=list(paste("Individual predicted concentrations (",info$units$conc,")",sep=""),cex=1.5),
			ylab=list(paste("Observed concentrations (",info$units$conc,")",sep=""),cex=1.5),
 			strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
 			#scales=list(cex=1.5),
			key=list(x=0,y=1.01,corner=c(0,0),border=FALSE,transparent=TRUE,
					columns=ifelse(length(keytext[[1]])%%3==0,length(keytext[[1]])%/%3,length(keytext[[1]])%/%3+1),between=1,between.columns=1,text.width.multiplier=1,
					text = keytext, line=keyline),
			scales=list(cex=1.5,x=list(log=logX,at=atLabelsX,labels=LabelsX),y=list(log=logY,at=atLabelsY,labels=LabelsY)),
			panel = function(x,y,...) {
					if(info$log.pred==TRUE & info$log.obs==FALSE){
						panel.curve(expr=10^(x),from=min(x,na.rm=TRUE),to=max(x,na.rm=TRUE),col=1,lwd=2)
						if(!is.na(info$lloq))panel.abline(h=eval(parse(text=info$lloq)),lty=4,col=1,lwd=2)
						if(!is.na(info$lloq))panel.abline(v=log10(eval(parse(text=info$lloq))),lty=4,col=1,lwd=2)
					}
					if(info$log.pred==FALSE & info$log.obs==TRUE){
						panel.curve(expr=log10(x),from=min(x,na.rm=TRUE),to=max(x,na.rm=TRUE),col=1,lwd=2)
						if(!is.na(info$lloq))panel.abline(h=log10(eval(parse(text=info$lloq))),lty=4,col=1,lwd=2)
						if(!is.na(info$lloq))panel.abline(v=eval(parse(text=info$lloq)),lty=4,col=1,lwd=2)
					}
					if(info$log.pred==TRUE & info$log.obs==TRUE){
						panel.abline(c(0,1),col=1,lwd=2)
						if(!is.na(info$lloq))panel.abline(h=log10(eval(parse(text=info$lloq))),lty=4,col=1,lwd=2)
						if(!is.na(info$lloq))panel.abline(v=log10(eval(parse(text=info$lloq))),lty=4,col=1,lwd=2)
					}
					if(info$log.pred==FALSE & info$log.obs==FALSE){
						panel.abline(c(0,1),col=1,lwd=2)
						if(!is.na(info$lloq))panel.abline(h=eval(parse(text=info$lloq)),lty=4,col=1,lwd=2)
						if(!is.na(info$lloq))panel.abline(v=eval(parse(text=info$lloq)),lty=4,col=1,lwd=2)
					}
					#if(!is.na(info$lloq))panel.abline(h=eval(parse(text=info$lloq)),lty=4,col=1,lwd=2)
					#if(!is.na(info$lloq))panel.abline(v=eval(parse(text=info$lloq)),lty=4,col=1,lwd=2)
				      panel.xYplot(x,y,type=c("p"), col=info$col,lty=info$lty,pch=info$pch,cex=info$cex, lwd=info$lwd,...)
				      if(info$smooth==TRUE) panel.loess(x,y,col="blue",lwd=5,lty=4)
			}, par.strip.text=list(cex=1.5))

	#plot1.id <- dv.vs.ipred(xpdb,main=NULL,
	#			smooth=info$xsmooth,
	#			ids=NULL,col=info$col[1],lty=info$lty[1],pch=info$pch[1],
	#			ylb=list(paste("Observed concentrations (",info$units$conc,")",sep=""),cex=1.5),
	#			xlb=list(paste("Individual predicted concentrations (",info$units$conc,")",sep=""),cex=1.5),
	#			scales=list(cex=1.5)
	#		)

	#dv.vs.pred(xpdb)
	plot2 <- xYplot(
			xpdb@Data[,xpdb@Prefs@Xvardef$dv]~xpdb@Data[,xpdb@Prefs@Xvardef$pred],
			#eval(parse(text=paste(xpdb@Prefs@Xvardef$dv,"~",xpdb@Prefs@Xvardef$pred,sep=""))),
			subset=eval(parse(text=paste(xpdb@Prefs@Xvardef$dv,"!=0 & ",xpdb@Prefs@Xvardef$pred,"!=0",sep=""))),
			data=xpdb@Data,
			layout=c(1,1),
			if(!is.na(info$group.by))groups=eval(parse(text=info$group.by)),
			aspect=1/1,
			#xlim=c(min(xpdb@Data$DV)-0.15*xpdb@Data$DV,ceiling(max(xpdb@Data$DV))),
			#ylim=c(min(xpdb@Data$DV)-0.15*xpdb@Data$DV,ceiling(max(xpdb@Data$DV))),
			xlab=list(paste("Population predicted concentrations (",info$units$conc,")",sep=""),cex=1.5),
			ylab=list(paste("Observed concentrations (",info$units$conc,")",sep=""),cex=1.5),
			strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
			scales=list(cex=1.5,x=list(log=logX,at=atLabelsX,labels=LabelsX),y=list(log=logY,at=atLabelsY,labels=LabelsY)),
			key=list(x=0,y=1.01,corner=c(0,0),border=FALSE,transparent=TRUE,
					columns=ifelse(length(keytext[[1]])%%3==0,length(keytext[[1]])%/%3,length(keytext[[1]])%/%3+1),between=1,between.columns=1,text.width.multiplier=1,
					text = keytext, line=keyline),	
			panel = function(x,y,...) {
					if(info$log.pred==TRUE & info$log.obs==FALSE){
						panel.curve(expr=10^(x),from=min(x,na.rm=TRUE),to=max(x,na.rm=TRUE),col=1,lwd=2)
						if(!is.na(info$lloq))panel.abline(h=eval(parse(text=info$lloq)),lty=4,col=1,lwd=2)
						if(!is.na(info$lloq))panel.abline(v=log10(eval(parse(text=info$lloq))),lty=4,col=1,lwd=2)
					}
					if(info$log.pred==FALSE & info$log.obs==TRUE){
						panel.curve(expr=log10(x),from=min(x,na.rm=TRUE),to=max(x,na.rm=TRUE),col=1,lwd=2)
						if(!is.na(info$lloq))panel.abline(h=log10(eval(parse(text=info$lloq))),lty=4,col=1,lwd=2)
						if(!is.na(info$lloq))panel.abline(v=eval(parse(text=info$lloq)),lty=4,col=1,lwd=2)
					}
					if(info$log.pred==TRUE & info$log.obs==TRUE){
						panel.abline(c(0,1),col=1,lwd=2)
						if(!is.na(info$lloq))panel.abline(h=log10(eval(parse(text=info$lloq))),lty=4,col=1,lwd=2)
						if(!is.na(info$lloq))panel.abline(v=log10(eval(parse(text=info$lloq))),lty=4,col=1,lwd=2)
					}
					if(info$log.pred==FALSE & info$log.obs==FALSE){
						panel.abline(c(0,1),col=1,lwd=2)
						if(!is.na(info$lloq))panel.abline(h=eval(parse(text=info$lloq)),lty=4,col=1,lwd=2)
						if(!is.na(info$lloq))panel.abline(v=eval(parse(text=info$lloq)),lty=4,col=1,lwd=2)
					}

				      panel.xYplot(x,y,type=c("p"), col=info$col,lty=info$lty,pch=info$pch,cex=info$cex, lwd=info$lwd,...)
					if(info$smooth==TRUE) panel.loess(x,y,col="blue",lwd=5,lty=4)
			}, par.strip.text=list(cex=1.5))

	#plot2.id <- dv.vs.pred(xpdb,main=NULL,
	#			smooth=info$xsmooth,
	#			ids=NULL,col=info$col[1],lty=info$lty[1],pch=info$pch[1],
	#			ylb=list(paste("Observed concentrations (",info$units$conc,")",sep=""),cex=1.5),
	#			xlb=list(paste("Population predicted concentrations (",info$units$conc,")",sep=""),cex=1.5),
	#			scales=list(cex=1.5)
	#		)

	#wres.vs.idv(xpdb)
	plot3 <- xYplot (
			xpdb@Data[,xpdb@Prefs@Xvardef$wres]~xpdb@Data[,xpdb@Prefs@Xvardef$idv],
			#eval(parse(text=paste(xpdb@Prefs@Xvardef$wres,"~",xpdb@Prefs@Xvardef$idv,sep=""))),
			subset=eval(parse(text=paste(xpdb@Prefs@Xvardef$dv,"!=0 & ",xpdb@Prefs@Xvardef$pred,"!=0",sep=""))),
			data=xpdb@Data,
			layout=c(1,1),
			aspect="fill",
			if(!is.na(info$group.by))groups=eval(parse(text=info$group.by)),
			#xlim=c(min(xpdb@Data$TAD)-0.1*xpdb@Data$TAD,ceiling(max(xpdb@Data$TAD))),
			#ylim=c(min(xpdb@Data$WRES)+0.1*xpdb@Data$WRES,ceiling(max(xpdb@Data$WRES))),
			xlab=list(paste("Time (",info$units$time,")",sep=""),cex=1.5),
 			ylab=list("Weighted residuals",cex=1.5),
			strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
			scales=list(cex=1.5),
			key=list(x=0,y=1.01,corner=c(0,0),border=FALSE,transparent=TRUE,
					columns=ifelse(length(keytext[[1]])%%3==0,length(keytext[[1]])%/%3,length(keytext[[1]])%/%3+1),between=1,between.columns=1,text.width.multiplier=1,
					text = keytext, line=keyline),
			panel = function(x,y,...) {
                              panel.abline(c(0,0),b=0,col=1,lwd=2)
				      panel.xYplot(x,y,type=c("p"), col=info$col,lty=info$lty,pch=info$pch,cex=info$cex, lwd=info$lwd,...)
					if(info$smooth==TRUE) panel.loess(x,y,col="blue",lwd=5,lty=4)
			}, par.strip.text=list(cex=1.5))
	
	#wres.vs.pred(xpdb)
	plot4 <- xYplot (
			xpdb@Data[,xpdb@Prefs@Xvardef$wres]~xpdb@Data[,xpdb@Prefs@Xvardef$pred],
			#eval(parse(text=paste(xpdb@Prefs@Xvardef$wres,"~",xpdb@Prefs@Xvardef$pred,sep=""))),
			subset=eval(parse(text=paste(xpdb@Prefs@Xvardef$dv,"!=0 & ",xpdb@Prefs@Xvardef$pred,"!=0",sep=""))),
			data=xpdb@Data,
			layout=c(1,1),
			if(!is.na(info$group.by))groups=eval(parse(text=info$group.by)),
			aspect="fill",
			#xlim=c(min(xpdb@Data$PRED)-0.1*xpdb@Data$PRED,ceiling(max(xpdb@Data$PRED))),
			#ylim=c(min(xpdb@Data$WRES)+0.1*xpdb@Data$WRES,ceiling(max(xpdb@Data$WRES))),
			xlab=list(paste("Population predicted concentrations (",info$units$conc,")",sep=""),cex=1.5),
			ylab=list("Weighted residuals",cex=1.5),
			strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
			scales=list(cex=1.5,x=list(log=logX,at=atLabelsX,labels=LabelsX)),
			key=list(x=0,y=1.01,corner=c(0,0),border=FALSE,transparent=TRUE,
					columns=ifelse(length(keytext[[1]])%%3==0,length(keytext[[1]])%/%3,length(keytext[[1]])%/%3+1),between=1,between.columns=1,text.width.multiplier=1,
					text = keytext, line=keyline),
			panel = function(x,y,...) {
				panel.abline(c(0,0),b=0,col=1,lwd=2)
			      panel.xYplot(x,y,type=c("p"), col=info$col,lty=info$lty,pch=info$pch,cex=info$cex, lwd=info$lwd,...)
				if(info$smooth==TRUE) panel.loess(x,y,col="blue",lwd=5,lty=4)
			}, par.strip.text=list(cex=1.5))
 
	plot5 <- histogram(xpdb@Data[eval(parse(text=paste("xpdb@Data$",xpdb@Prefs@Xvardef$dv,"!=0",sep=""))),xpdb@Prefs@Xvardef$wres],main="",xlab=list("Weighted residuals",cex=1.5),ylab=list("Frequency (%)",cex=1.5),type="percent",col="grey",lwd=3,scales=list(cex=1.5))
	plot6 <- wres.dist.qq(xpdb,pch=20,main="",xlb=list("Quantiles of Normal",cex=1.5),ylb=list("Quantiles of weighted residuals",cex=1.5),scales=list(cex=1.5),cex=1.5,col="black")

	if(!is.na(info$output)){
		printGraph("dv.vs.ipred",info$device)
		print(plot1)
 		dev.off()

		#win.metafile(file="dv.vs.ipred.by.id.wmf",height=7,width=7) 
		#print(plot1.id)
 		#dev.off()
		printGraph("dv.vs.pred",info$device)
 		print(plot2)
		dev.off()

		#win.metafile(file="dv.vs.pred.by.id.wmf",height=7,width=7)
 		#print(plot2.id)
		#dev.off()

		printGraph("wres.vs.idv",info$device)
		print(plot3)
		dev.off()

		printGraph("wres.vs.pred",info$device)
		print(plot4)
		dev.off()

		printGraph("histogram.wres",info$device)
		print(plot5)
		dev.off()

		printGraph("qqnorm.wres",info$device)
		print(plot6)
		dev.off()
   	}else{
		print(plot1)
		#print(plot1.id)
 		print(plot2)
 		#print(plot2.id)
		print(plot3)
		print(plot4)
		print(plot5)
		print(plot6)
	}


	if(!is.null(xpdb@Prefs@Xvardef$iwres)){
		#iwres.vs.idv(xpdb)
		plot7 <- xYplot (
			xpdb@Data[,xpdb@Prefs@Xvardef$iwres]~xpdb@Data[,xpdb@Prefs@Xvardef$idv],
			#eval(parse(text=paste(xpdb@Prefs@Xvardef$iwres,"~",xpdb@Prefs@Xvardef$idv,sep=""))),
			subset=eval(parse(text=paste(xpdb@Prefs@Xvardef$dv,"!=0 & ",xpdb@Prefs@Xvardef$pred,"!=0",sep=""))),
			data=xpdb@Data,
			layout=c(1,1),
			if(!is.na(info$group.by))groups=eval(parse(text=info$group.by)),
			aspect="fill",
			#xlim=c(min(xpdb@Data$TAD)-0.1*xpdb@Data$TAD,ceiling(max(xpdb@Data$TAD))),
			#ylim=c(min(xpdb@Data$WRES)+0.1*xpdb@Data$WRES,ceiling(max(xpdb@Data$WRES))),
			xlab=list(paste("Time (",info$units$time,")",sep=""),cex=1.5),
			ylab=list("Individual weighted residuals",cex=1.5),
			strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
			scales=list(cex=1.5),
			key=list(x=0,y=1.01,corner=c(0,0),border=FALSE,transparent=TRUE,
					columns=ifelse(length(keytext[[1]])%%3==0,length(keytext[[1]])%/%3,length(keytext[[1]])%/%3+1),between=1,between.columns=1,text.width.multiplier=1,
					text = keytext, line=keyline),
			panel = function(x,y,...) {
                              panel.abline(c(0,0),b=0,col=1,lwd=2)
				      panel.xYplot(x,y,type=c("p"), col=info$col,lty=info$lty,pch=info$pch,cex=info$cex, lwd=info$lwd,...)
					if(info$smooth==TRUE) panel.loess(x,y,col="blue",lwd=5,lty=4)
			}, par.strip.text=list(cex=1.5))

		plot8 <- histogram(xpdb@Data[eval(parse(text=paste("xpdb@Data$",xpdb@Prefs@Xvardef$dv,"!=0",sep=""))),xpdb@Prefs@Xvardef$iwres],main="",xlab=list("Individual weighted residuals",cex=1.5),ylab=list("Frequency (%)",cex=1.5),type="percent",col="grey",lwd=3,scales=list(cex=1.5))
		plot9 <- iwres.dist.qq(xpdb,pch=20,main="",xlb=list("Quantiles of Normal",cex=1.5),ylb=list("Quantiles of individual weighted residuals",cex=1.5),scales=list(cex=1.5),cex=1.5,col="black")

		if(!is.na(info$output)){
			printGraph("iwres.vs.idv",info$device)
			print(plot7)
			dev.off()

			printGraph("histogram.iwres",info$device)
			print(plot8)
			dev.off()

			printGraph("qqnorm.iwres",info$device)
			print(plot9)
			dev.off()
		}else{
			print(plot7)
			print(plot8)
			print(plot9)
		}


	}else{
		printGraph("iwres.vs.time",info$device)
		dev.off()

		printGraph("qqnorm.iwres",info$device)
		dev.off()

		printGraph("histogram.iwres",info$device)
		dev.off()
	}

	if(!is.null(xpdb@Prefs@Xvardef$cwres)){
		plot10 <- xYplot (
			xpdb@Data[,xpdb@Prefs@Xvardef$cwres]~xpdb@Data[,xpdb@Prefs@Xvardef$idv],
			#eval(parse(text=paste(xpdb@Prefs@Xvardef$cwres,"~",xpdb@Prefs@Xvardef$idv,sep=""))),
			subset=eval(parse(text=paste(xpdb@Prefs@Xvardef$dv,"!=0 & ",xpdb@Prefs@Xvardef$pred,"!=0",sep=""))),
			data=xpdb@Data,
			layout=c(1,1),
			aspect="fill",
			if(!is.na(info$group.by))groups=eval(parse(text=info$group.by)),
			#xlim=c(min(xpdb@Data$TAD)-0.1*xpdb@Data$TAD,ceiling(max(xpdb@Data$TAD))),
			#ylim=c(min(xpdb@Data$CWRES)+0.1*xpdb@Data$CWRES,ceiling(max(xpdb@Data$CWRES))),
			xlab=list(paste("Time (",info$units$time,")",sep=""),cex=1.5),
 			ylab=list("Conditional weighted residuals",cex=1.5),
			strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
			scales=list(cex=1.5),
			key=list(x=0,y=1.01,corner=c(0,0),border=FALSE,transparent=TRUE,
					columns=ifelse(length(keytext[[1]])%%3==0,length(keytext[[1]])%/%3,length(keytext[[1]])%/%3+1),between=1,between.columns=1,text.width.multiplier=1,
					text = keytext, line=keyline),
			panel = function(x,y,...) {
                              panel.abline(c(0,0),b=0,col=1,lwd=2)
				      panel.xYplot(x,y,type=c("p"), col=info$col,lty=info$lty,pch=info$pch,cex=info$cex, lwd=info$lwd,...)
					if(info$smooth==TRUE) panel.loess(x,y,col="blue",lwd=5,lty=4)
			}, par.strip.text=list(cex=1.5))
	
		#cwres.vs.pred(xpdb)
		plot11 <- xYplot (
			xpdb@Data[,xpdb@Prefs@Xvardef$cwres]~xpdb@Data[,xpdb@Prefs@Xvardef$pred],
			#eval(parse(text=paste(xpdb@Prefs@Xvardef$cwres,"~",xpdb@Prefs@Xvardef$pred,sep=""))),
			subset=eval(parse(text=paste(xpdb@Prefs@Xvardef$dv,"!=0 & ",xpdb@Prefs@Xvardef$pred,"!=0",sep=""))),
			data=xpdb@Data,
			layout=c(1,1),
			if(!is.na(info$group.by))groups=eval(parse(text=info$group.by)),
			aspect="fill",
			#xlim=c(min(xpdb@Data$PRED)-0.1*xpdb@Data$PRED,ceiling(max(xpdb@Data$PRED))),
			#ylim=c(min(xpdb@Data$CWRES)+0.1*xpdb@Data$CWRES,ceiling(max(xpdb@Data$CWRES))),
			xlab=list(paste("Population predicted concentrations (",info$units$conc,")",sep=""),cex=1.5),
			ylab=list("Conditional weighted residuals",cex=1.5),
			strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
			scales=list(cex=1.5,x=list(log=logX,at=atLabelsX,labels=LabelsX)),
			key=list(x=0,y=1.01,corner=c(0,0),border=FALSE,transparent=TRUE,
					columns=ifelse(length(keytext[[1]])%%3==0,length(keytext[[1]])%/%3,length(keytext[[1]])%/%3+1),between=1,between.columns=1,text.width.multiplier=1,
					text = keytext, line=keyline),
			panel = function(x,y,...) {
				panel.abline(c(0,0),b=0,col=1,lwd=2)
			      panel.xYplot(x,y,type=c("p"), col=info$col,lty=info$lty,pch=info$pch,cex=info$cex, lwd=info$lwd,...)
				if(info$smooth==TRUE) panel.loess(x,y,col="blue",lwd=5,lty=4)
			}, par.strip.text=list(cex=1.5))
 
		plot12 <- histogram(xpdb@Data[xpdb@Prefs@Xvardef$dv!=0,xpdb@Prefs@Xvardef$cwres],main="",xlab=list("Conditional weighted residuals",cex=1.5),ylab=list("Frequency (%)",cex=1.5),type="percent",col="grey",lwd=3,scales=list(cex=1.5))
		plot13 <- cwres.dist.qq(xpdb,pch=20,main="",xlb=list("Quantiles of Normal",cex=1.5),ylb=list("Quantiles of conditional weighted residuals",cex=1.5),scales=list(cex=1.5),cex=1.5,col="black")

		if(!is.na(info$output)){
			printGraph("cwres.vs.idv",info$device)
			print(plot10)
			dev.off()

			printGraph("cwres.vs.pred",info$device)
			print(plot11)
			dev.off()

			printGraph("histogram.cwres",info$device)
			print(plot12)
			dev.off()

			printGraph("qqnorm.cwres",info$device)
			print(plot13)
			dev.off()
		}else{
			print(plot10)
			print(plot11)
			print(plot12)
			print(plot13)
		}
	}


	if(info$report==TRUE){
		wdGoToBookmark("blankDiag")
		wdNormal("")
		wdBody(paste("The goodness-of-fit graphs for ",info$drug.name," population PK model are shown in the following figures.",sep=""))
		wdPlot(plot1,width=6,height=6)
		wdPlot(plot2,width=6,height=6)
		wdPlot(plot3,width=6,height=6)
		wdPlot(plot4,width=6,height=6)
		wdPlot(plot5,width=6,height=6)
		wdPlot(plot6,width=6,height=6)
		#wdSectionBreak(continuous=FALSE)
		wdSave(paste("popPKrun",info$run.no,sep=""))
	}

	#plotAuto <- autocorr.wres(xpdb,main=NULL,smooth=info$xsmooth,pch=info$pch[1],col=info$col[1],cex=info$cex[1])
	#win.metafile(file="auto.corr.wres.wmf",height=7,width=7)
	#print(plotAuto)
	#dev.off()


} #end of Diagnostics function
  #-------------------------------------end----end----end---end-----Diagnostics----Diagnostics----Diagnostics------------------------#




#=========================================function of NONMEM Estimates ==============================================================#
#Function Name: Estimates()                                                                                                          #
#                                                                                                                                    #
#====================================================================================================================================#

Estimates<-function(info){

	names(info) <- casefold(names(info),upper=FALSE)

	if(is.null(xpdb)){
		cat("Estimates Function: You forgot to load the data by running xposeData function first\n")
		cat("xposeData function is being called from within the Estimates function\n")
		xpdb <- xposeData(info)
	}

	if(sum(c("prefix","thetas","etas","covs","output","digits","device",
		          	"cat.level","group.by","strat.by","lloq","suffix",       
				"scale","smooth","log.pred","log.obs","log.covs","log.parms","ind.plots",         
				"col","line.col","units","drug.name","quantiles","bins",    
				"cov.rel", "eta.plot","cex","lty","lwd","pch","mean","alpha","report")%in%names(info)==FALSE)>0){
		
		cat("Estimates function: The info list has been changed and does not include all options\n")
		cat("CreateInfo function is being called from within the Estimates function\n")
		info <- CreateInfo(info)
	}

	if(!is.na(info$output)) while(dev.cur()!=1) dev.off()
	if(is.na(info$output)){
		if(length(grep(":",R.home()))>0) windows(rec=TRUE)
	}

	options(warn = -1)
	if(info$smooth!=TRUE) info$xsmooth <- NULL

	#parm.summary(xpdb,out="ParameterSummary")
      cats <- NULL
      conts <- NULL
	rans <- NULL

	for (parm in xpdb@Prefs@Xvardef$parms) {
		if (is.factor(xpdb@Data[[parm]])) {
			cats <- c(cats, parm)
		}else{
	            if(parm%in%xpdb@Prefs@Xvardef$ranpar){
				rans <- c(rans,parm)	
			}else{
				conts <- c(conts, parm)
			}
		}
	}

	xpdbcon <- xpdb
	xpdbcon@Prefs@Xvardef$parms <- conts

	xpdbran <- xpdb
	xpdbran@Prefs@Xvardef$parms <- rans

	panel.cor <- function(x, y, digits=2, prefix="Corr=", cex.cor, ...){
		usr <- par("usr"); on.exit(par(usr))
		par(usr = c(0, 1, 0, 1))
		r <- abs(cor(x, y))
		txt <- format(c(r, 0.123456789), digits=digits)[1]
		txt <- paste(prefix, txt, sep="")
		#txt <- substitute(paste(R^2,"=",txt),list(txt=txt))
		#if(missing(cex.cor)) cex.cor <- 0.5/strwidth(txt)
		#text(0.5, 0.5, txt, cex = cex.cor + r/2 )
		if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    		text(0.5, 0.5, txt, cex = cex.cor) #*r
	}

	if(!is.na(info$output)){
		#Scatterplots
		printGraph("ParmSplom",info$device)
		if(length(conts)>1){
			if(info$smooth){
				pairs(xpdb@Data[,conts],pch=20,cex=1,col="black",
					upper.panel=panel.cor,lower.panel=panel.smooth,gap=0,
					labels=as.character(xpdb@Prefs@Labels[names(xpdb@Prefs@Labels)%in%conts]))
			}else{
				pairs(xpdb@Data[,conts],pch=20,cex=1,col="black",
					upper.panel=panel.cor,gap=0,
					labels=as.character(xpdb@Prefs@Labels[names(xpdb@Prefs@Labels)%in%conts]))
			}	
		}
		dev.off()

		printGraph("RanSplom",info$device)
		if(length(rans)>1){
			if(info$smooth){
				pairs(xpdb@Data[,rans],pch=20,cex=1,col="black",
					upper.panel=panel.cor,lower.panel=panel.smooth,gap=0,
					labels=as.character(xpdb@Prefs@Labels[names(xpdb@Prefs@Labels)%in%rans]))
			}else{
				pairs(xpdb@Data[,rans],pch=20,cex=1,col="black",
					upper.panel=panel.cor,gap=0,
					labels=as.character(xpdb@Prefs@Labels[names(xpdb@Prefs@Labels)%in%rans]))
			}
		}
		dev.off()

		###Histograms
		printGraph("ParmHist",info$device)
		parm.hist(xpdbcon,max.plots.per.page=length(conts),aspect=1,
			main=NULL,hicol="lightgrey",varname.cex=0.7,axis.text.cex=0.7)
		dev.off()

		printGraph("ParmHist%02d",info$device)
		print(parm.hist(xpdbcon,max.plots.per.page=4,aspect="fill",
			main=NULL,hicol="lightgrey",x.cex=0.8,y.cex=0.8)
		)
		dev.off()

		printGraph("RanHist",info$device)
		parm.hist(xpdbran,max.plots.per.page=length(rans),aspect=1,
			main=NULL,hicol="lightgrey",varname.cex=0.7,axis.text.cex=0.7)
		dev.off()

		printGraph("RanHist%02d",info$device)
		print(parm.hist(xpdbran,max.plots.per.page=4,aspect="fill",
			main=NULL,hicol="lightgrey",x.cex=0.8,y.text.cex=0.8)
		)
		dev.off()
	}else{
		if(length(conts)>1){
			if(info$smooth){
				print(pairs(xpdb@Data[,conts],pch=20,cex=1,col="black",
					upper.panel=panel.cor,lower.panel=panel.smooth,gap=0,
					labels=as.character(xpdb@Prefs@Labels[names(xpdb@Prefs@Labels)%in%conts]))
				)
			}else{
				print(pairs(xpdb@Data[,conts],pch=20,cex=1,col="black",
					upper.panel=panel.cor,gap=0,
					labels=as.character(xpdb@Prefs@Labels[names(xpdb@Prefs@Labels)%in%conts]))
				)
			}
		}

		if(length(rans)>1){
			if(info$smooth){
				print(pairs(xpdb@Data[,rans],pch=20,cex=1,col="black",
					upper.panel=panel.cor,lower.panel=panel.smooth,gap=0,
					labels=as.character(xpdb@Prefs@Labels[names(xpdb@Prefs@Labels)%in%rans]))
				)
			}else{
				print(pairs(xpdb@Data[,rans],pch=20,cex=1,col="black",
					upper.panel=panel.cor,gap=0,
					labels=as.character(xpdb@Prefs@Labels[names(xpdb@Prefs@Labels)%in%rans]))
				)
			}
		}

		###Histograms
		print(parm.hist(xpdbcon,max.plots.per.page=4,aspect="fill",
			main=NULL,hicol="lightgrey",x.cex=0.8,y.cex=0.8))
		print(parm.hist(xpdbran,max.plots.per.page=4,aspect="fill",
			main=NULL,hicol="lightgrey",x.cex=0.8,y.text.cex=0.8))
	}

	#Summary of individual parameter estimates
  	data <- xpdb@Data[match(unique(xpdb@Data[,xpdb@Prefs@Xvardef$id]),xpdb@Data[,xpdb@Prefs@Xvardef$id]),]

	ParmSum <- data.frame(Parameter=rep(NA,length(conts)),
					N  = rep(NA,length(conts)),
					Mean=rep(NA,length(conts)),
					SD = rep(NA,length(conts)),
					Median=rep(NA,length(conts)),
                              Min = rep(NA,length(conts)),
					Max = rep(NA,length(conts)))

	j <- 1
 	for(i in conts){
		Pname <- i
		if(i%in%names(info$thetas)) Pname <- paste(i," (",info$thetas[[which(i==names(info$thetas))]],")",sep="")

		ParmSum$Parameter[j] <- Pname
		ParmSum$Mean[j] <- signif(mean(data[,i],na.rm=TRUE),digits=info$digits)
		ParmSum$SD[j] <- signif(sd(data[,i],na.rm=TRUE),digits=info$digits)
		ParmSum$N[j] <- length(data[,i])
		ParmSum$Median[j] <- signif(median(data[,i],na.rm=TRUE),digits=info$digits)
     		ParmSum$Min[j] <- min(data[,i],na.rm=TRUE)
		ParmSum$Max[j] <- max(data[,i],na.rm=TRUE)
		j <- j + 1
     	}


	RanSum <- data.frame(#Name=rep(NA,length(rans)),
					Parameter=rep(NA,length(rans)),
					ETA=rep(NA,length(rans)),
					N  = rep(NA,length(rans)),
					Mean=rep(NA,length(rans)),
					SD = rep(NA,length(rans)),
					Median=rep(NA,length(rans)),
                              Min = rep(NA,length(rans)),
					Max = rep(NA,length(rans)))

	j <- 1
 	for(i in rans){
		#RanSum$Name[j] <- i
		RanSum$ETA[j] <- i		
		if(i%in%names(info$etas)){
			#RanSum$Name[j] <- paste(i," (",info$etas[[which(i==names(info$etas))]],")",sep="")
			RanSum$Parameter[j] <- info$etas[[which(i==names(info$etas))]]
		}

		RanSum$Mean[j] <- signif(mean(data[,i],na.rm=TRUE),digits=info$digits)
		RanSum$SD[j] <- signif(sd(data[,i],na.rm=TRUE),digits=info$digits)
		RanSum$N[j] <- length(data[,i])
		RanSum$Median[j] <- signif(median(data[,i],na.rm=TRUE),digits=info$digits)
     		RanSum$Min[j] <- min(data[,i],na.rm=TRUE)
		RanSum$Max[j] <- max(data[,i],na.rm=TRUE)
		j <- j + 1
     	}


	#Population parameter estimates
	if(is.null(info$prefix)) info$prefix <- "run"
	if(paste(info$prefix,info$run.no,".mod",sep="")%in%dir(info$path)){
		CS <- scan(file=paste(info$path,"/",info$prefix,info$run.no,".mod",sep=""),sep="\n",what="character")
	}else{
		if(paste(info$prefix,info$run.no,".ctl",sep="")%in%dir(info$path)){
			CS <- scan(file=paste(info$path,"/",info$prefix,info$run.no,".ctl",sep=""),sep="\n",what="character")
		}else{
			cat(paste("Estimates function could not find the NONMEM control stream (",info$prefix,info$run.no,".mod or .ctl) in",info$path,"\n",sep=""))
		}
	}

	#Data
	dataPos <- grep("\\$DATA",CS)

	dataName <- substring(CS[dataPos],first=regexpr("\\$DATA",CS[dataPos])[[1]]+5)
	dataName <- substring(dataName,first=regexpr("[[:alnum:][:punct:]]",dataName)[[1]])
      if(regexpr("[[:space:]]",dataName)>1){
		dataName <- substring(dataName,first=1,last=regexpr("[[:space:]]",dataName)[[1]]-1)
	}
	
	dataName <- gsub("\\\\","/",dataName)
	dataFolder <- dataName

	if(gregexpr("/",dataFolder)[[1]][[1]]>0){
		dataFolder <- substring(dataFolder,first=1,last=gregexpr("/",dataFolder)[[1]][[length(gregexpr("/",dataFolder)[[1]])]]-1)
		dataName <- substring(dataName,first=gregexpr("/",dataName)[[1]][[length(gregexpr("/",dataName)[[1]])]]+1)
	}

	if(length(grep("/",dataFolder))>0){
		if(casefold(dataName,upper=FALSE)%in%casefold(dir(paste(info$path,"/",dataFolder,sep="")),upper=FALSE)){
			inputData <- scan(file=paste(info$path,"/",dataFolder,"/",dataName,sep=""),sep="\n",what="character")	
			if(!is.na(info$output))	write(inputData, file = "inputdata.csv")
		}else{
			cat(paste("Estimates function could not find the NONMEM input data file (",dataName,") specified in the control stream\n",sep=""))
		}
	}else{
		if(casefold(dataName,upper=FALSE)%in%casefold(dir(info$path),upper=FALSE)){
			inputData <- scan(file=paste(info$path,"/",dataName,sep=""),sep="\n",what="character")
			if(!is.na(info$output))	write(inputData, file = "inputdata.csv")
		}else{
			cat(paste("Estimates function could not find the NONMEM input data file (",dataName,") specified in the control stream\n",sep=""))
		}
	}

	#Thetas
	thetaPos <- grep("\\$THETA",CS)
	commentThetaPos <- grep(";\\$THETA",gsub(" ","",CS))

	if(length(commentThetaPos)>0){
		thetaPos <- thetaPos[-which(thetaPos%in%commentThetaPos)]
	}

	thetaNames <- character()
	for(i in 1:length(thetaPos)){
		if(regexpr(";",CS[thetaPos[i]])[1]==-1){
			thetaNames[i] <- paste("Theta(",i,")",sep="")
		}else{
			thetaNames[i] <- substring(CS[thetaPos[i]],first=regexpr(";",CS[thetaPos[i]])+1)
			thetaNames[i] <- gsub(" ","",thetaNames[i])
		}
		
		if(thetaNames[i]%in%names(info$thetas)) thetaNames[i] <- paste(thetaNames[i]," (",info$thetas[[which(thetaNames[i]==names(info$thetas))]],")",sep="")
	
	}

 	ThetaEstimates <- data.frame(Parameter=thetaNames,
					Estimate  = rep(NA,length(thetaNames)),
					SE  = rep(NA,length(thetaNames)),
					RSE  = rep(NA,length(thetaNames)),
					CI95 = rep(NA,length(thetaNames))
			    )
	if(paste(info$prefix,info$run.no,".lst",sep="")%in%dir(info$path)){
		NMoutput <- scan(file=paste(info$path,"/",info$prefix,info$run.no,".lst",sep=""),sep="\n",what="character") #blank.lines.skip=FALSE
	}else{
		if(paste(info$prefix,info$run.no,".out",sep="")%in%dir(info$path)){
			NMoutput <- scan(file=paste(info$path,"/",info$prefix,info$run.no,".out",sep=""),sep="\n",what="character") #blank.lines.skip=FALSE
		}else{
			cat(paste("Estimates function could not find the NONMEM output file (",info$prefix,info$run.no,".lst or .out) in ",info$path,"\n",sep=""))
		}
	}
	
	thetaEstimatePos <- grep("THETA - VECTOR OF FIXED EFFECTS PARAMETERS",NMoutput)+3
	omegaEstimatePos <- grep("OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS",NMoutput)+3
	sigmaEstimatePos <- grep("SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS",NMoutput)+3

	if(length(thetaEstimatePos)==0){
		if(paste(info$prefix,info$run.no,".lst",sep="")%in%dir(info$path)){
			NMoutput <- scan(file=paste(info$path,"/",info$prefix,info$run.no,".lst",sep=""),blank.lines.skip=FALSE,sep="\n",what="character") 
		}else{
			if(paste(info$prefix,info$run.no,".out",sep="")%in%dir(info$path)){
				NMoutput <- scan(file=paste(info$path,"/",info$prefix,info$run.no,".out",sep=""),blank.lines.skip=FALSE,sep="\n",what="character") 
			}
		}
		 cat(paste("\nNo parameter estimates are available in the NONMEM output file (",info$prefix,info$run.no,".lst or .out).\n Please check that there is no special \"return\" character present on line ",length(NMoutput)," of your NONMEM output file.\n",sep="")) 
	}

	for(i in 1:length(thetaEstimatePos)){
		if(length(thetaNames)>12){
			thetaExtraLines <- length(thetaNames)%/%12 - 1
			if(length(thetaNames)%%12!=0) thetaExtraLines <- thetaExtraLines+1

			temp <- strsplit(gsub("  "," ",substring(NMoutput[thetaEstimatePos[i]+thetaExtraLines],first=9))," ")[[1]]
			for(j in 1:thetaExtraLines){
				temp <- c(temp,strsplit(gsub("  "," ",substring(NMoutput[thetaEstimatePos[i]+thetaExtraLines+j],first=9))," ")[[1]])
			}
			temp <- temp[which(temp!="")]

			if(i==1){
				ThetaEstimates$Estimate <- temp
			}else{
				ThetaEstimates$SE <- temp
			}
		}else{
			temp <- strsplit(gsub("  "," ",substring(NMoutput[thetaEstimatePos[i]],first=9))," ")[[1]]
			temp <- temp[which(temp!="")]
			if(i==1){
				ThetaEstimates$Estimate <- temp
				
			}else{
				ThetaEstimates$SE <- temp
			}	
		}
	}
	ThetaEstimates$Estimate <- as.numeric(ThetaEstimates$Estimate)

 	ThetaEstimates$SE <- signif(as.numeric(ThetaEstimates$SE),digits=info$digits)
	ThetaEstimates$RSE <- as.character(signif(100*ThetaEstimates$SE/ThetaEstimates$Estimate,digits=info$digits))
	ThetaEstimates$CI95 <- paste("(",as.character(signif(ThetaEstimates$Estimate-qnorm(0.975)*ThetaEstimates$SE,digits=info$digits)),"-",as.character(signif(ThetaEstimates$Estimate+qnorm(0.975)*ThetaEstimates$SE,digits=info$digits)),")",sep="")

	assign("ThetaEstimates",ThetaEstimates,envir=.GlobalEnv)
	ThetaEstimates$Estimate <- signif(ThetaEstimates$Estimate,digits=info$digits)

	ThetaEstimates$Estimate <- as.character(ThetaEstimates$Estimate)
	ThetaEstimates$SE <- as.character(ThetaEstimates$SE)

	ThetaEstimates$Estimate[ThetaEstimates$Estimate=="NA"| is.na(ThetaEstimates$Estimate)] <- "-"
	ThetaEstimates$SE[ThetaEstimates$SE=="NA" | is.na(ThetaEstimates$SE)] <- "-"
	ThetaEstimates$RSE[ThetaEstimates$RSE=="NA" | is.na(ThetaEstimates$RSE)] <- "-"
	ThetaEstimates$CI95[ThetaEstimates$CI95=="NA" | is.na(ThetaEstimates$CI95)] <- "-"

	ThetaEstimates$CI95[ThetaEstimates$CI95=="(NA-NA)"] <- "-"

	#Omega
	omegaPos <- grep("\\$OMEGA",CS)

	commentOmegaPos <- grep(";\\$OMEGA",gsub(" ","",CS))

	if(length(commentOmegaPos)>0){
		omegaPos <- omegaPos[-which(omegaPos%in%commentOmegaPos)]
	}

	blockNames <- character()
	blockPos <- grep("BLOCK\\(",CS)

	omegaNames <- character()
	cov1Names <- character()
	cov2Names <- character()
	cov1 <- character()
	cov2 <- character()

	etaNumber <- character()

	whichBlock <- NULL
	cumBlock <- NULL
	k <- 1
	pnum <- 1
	
	#Get omega names from info$etas or comments after $OMEGA specified with ";"
	for(i in 1:length(omegaPos)){
		nBlock <- as.integer(substring(CS[omegaPos[i]],first=regexpr("BLOCK\\(",CS[omegaPos[i]])[1]+6,last=regexpr("BLOCK\\(",CS[omegaPos[i]])[1]+6))
		if(omegaPos[i]%in%blockPos & nBlock>1){
			whichBlock <- c(whichBlock,i)
			cumBlock <- c(cumBlock,nBlock)
			N <- 1
			temp <- character()
			tempETA <- character()

			for(l in pnum:(nBlock+pnum-1)){ #for(l in k:(nBlock+k-1)){
				for(m in l:(nBlock+pnum-1)){ #for(m in l:(nBlock+k-1)){
					if(paste("ETA",l,sep="")%in%names(info$etas)){
						lname <- info$etas[[paste("ETA",l,sep="")]]
					}else{
						lname <- paste("ETA",l,sep="")
					}

					if(l==m){
						temp[N] <- lname 
						tempETA[N] <- paste("ETA",l,sep="")
					}else{
						if(paste("ETA",m,sep="")%in%names(info$etas)){
							mname <- info$etas[[paste("ETA",m,sep="")]]
						}else{
							mname <- paste("ETA",m,sep="")
						}
						temp[N] <- paste("Corr(",lname,"-",mname,")",sep="")
						tempETA[N] <- paste("ETA",l,"-ETA",m,sep="")
						cov1[N] <- paste("ETA",l,sep="")
						cov2[N] <- paste("ETA",m,sep="")
					}
					N <- N + 1
				}
			}

			if(regexpr(";",CS[omegaPos[i]])[1]!=-1){
				if(sum(paste("ETA",pnum:(pnum+nBlock-1),sep="")%in%names(info$etas)==FALSE)>0){
					blockNames <- substring(CS[omegaPos[i]],first=regexpr(";",CS[omegaPos[i]])+1)
					blockNames <- gsub(" ","",strsplit(blockNames,split=";")[[1]])

					if(length(blockNames)<nBlock) blockNames <- c(blockNames,paste("ETA",(pnum+length(blockNames)):(pnum+nBlock-1),sep=""))

					#for(n in 1:nBlock){
					for(n in which(!c(paste("ETA",pnum:(pnum+nBlock-1),sep="")%in%names(info$etas)))){
						temp <- gsub(paste("ETA",(pnum:(pnum+nBlock-1))[n],sep=""),blockNames[n],temp)
						cov1 <- gsub(paste("ETA",(pnum:(pnum+nBlock-1))[n],sep=""),blockNames[n],cov1)
						cov2 <- gsub(paste("ETA",(pnum:(pnum+nBlock-1))[n],sep=""),blockNames[n],cov2)
					}
				}
			}

			etaNumber[k:(nBlock*(nBlock+1)/2+k-1)] <- tempETA

			if(nBlock==1){
				omegaNames[k:(nBlock*(nBlock+1)/2+k-1)] <- temp
				cov1Names[k:(nBlock*(nBlock+1)/2+k-1)] <- cov1
				cov2Names[k:(nBlock*(nBlock+1)/2+k-1)] <- cov2
				etaNumber[k:(nBlock*(nBlock+1)/2+k-1)] <- tempETA
			}
			if(nBlock==2){
				omegaNames[k:(nBlock*(nBlock+1)/2+k-1)] <- temp
				cov1Names[k:(nBlock*(nBlock+1)/2+k-1)] <- cov1
				cov2Names[k:(nBlock*(nBlock+1)/2+k-1)] <- cov2
				etaNumber[k:(nBlock*(nBlock+1)/2+k-1)] <- tempETA
			}
			if(nBlock==3){
				omegaNames[k:(nBlock*(nBlock+1)/2+k-1)] <- temp[c(1:2,4,3,5,6)]
				cov1Names[k:(nBlock*(nBlock+1)/2+k-1)] <- cov1[c(1:2,4,3,5,6)]
				cov2Names[k:(nBlock*(nBlock+1)/2+k-1)] <- cov2[c(1:2,4,3,5,6)]
				etaNumber[k:(nBlock*(nBlock+1)/2+k-1)] <- tempETA[c(1:2,4,3,5,6)]
			}
			if(nBlock==4){
				omegaNames[k:(nBlock*(nBlock+1)/2+k-1)] <- temp[c(1:2,5,3,6,8,4,7,9,10)]
				cov1Names[k:(nBlock*(nBlock+1)/2+k-1)] <- cov1[c(1:2,5,3,6,8,4,7,9,10)]
				cov2Names[k:(nBlock*(nBlock+1)/2+k-1)] <- cov2[c(1:2,5,3,6,8,4,7,9,10)]			
				etaNumber[k:(nBlock*(nBlock+1)/2+k-1)] <- tempETA[c(1:2,5,3,6,8,4,7,9,10)]	
			}

			pnum <- pnum + nBlock
			k <- k+nBlock*(nBlock+1)/2

		}else{
			cov1Names[k] <- NA
			cov2Names[k] <- NA
			etaNumber[k] <- paste("ETA",pnum,sep="")

			if(sum(paste("ETA",pnum,sep="")%in%names(info$etas)==FALSE)>0){
				if(regexpr(";",CS[omegaPos[i]])[1]==-1){
					omegaNames[k] <- paste("ETA",pnum,sep="")
				}else{
					omegaNames[k] <- substring(CS[omegaPos[i]],first=regexpr(";",CS[omegaPos[i]])+1)
					omegaNames[k] <- gsub(" ","",omegaNames[k])
					#omegaNames[k] <- paste("ETA",omegaNames[k],sep="")
				}
			}else{
				omegaNames[k] <- info$etas[[paste("ETA",pnum,sep="")]]		
			}
			pnum <- pnum + 1
			k <- k + 1
		}
	}

	OmegaEstimates <- data.frame(Parameter=omegaNames,
					Estimate  = rep(NA,length(omegaNames)),
					SE  = rep(NA,length(omegaNames)),
					RSE  = rep(NA,length(omegaNames)),
					CI95 = rep(NA,length(omegaNames)),
					Shrinkage = rep(NA,length(omegaNames)),
					CV = rep(NA,length(omegaNames)),
					COV1 = cov1Names,
					COV2 = cov2Names,
					ETA = etaNumber
			    )

	#Get omega estimates from NM output file
	for(i in 1:length(omegaEstimatePos)){
		temp <- NMoutput[omegaEstimatePos[i]:sigmaEstimatePos[i]]
		l <- 1
		j <- 1
		pp <- 1
		etaPos <- which(regexpr("ET",temp)==2)
		while(j<=length(etaPos)){
			temp2 <- gsub("\\+        ","",temp[etaPos[j]+1])
			temp2 <- strsplit(gsub("  "," ",temp2)," ")[[1]]

			if(i==1){
				OmegaEstimates$Estimate[l] <- temp2[length(temp2)]
				l <- l + 1

				if(j%in%whichBlock[pp]){
					j <- j + 1 #new
					for(k in 1:(cumBlock[pp]-1)){
						temp2 <- gsub("\\+        ","",temp[etaPos[j]+1]) #temp2 <- gsub("\\+        ","",temp[etaPos[j+k]+1])
						temp2 <- strsplit(gsub("  "," ",temp2)," ")[[1]]

						OmegaEstimates$Estimate[l:(l+length((length(temp2)-k):length(temp2))-1)] <- temp2[(length(temp2)-k):length(temp2)]
						l <- l+length((length(temp2)-k):length(temp2)) #l+length((length(temp2)-k))+1
						j <- j + 1
					}
					pp <- pp + 1
				}else{
					j <- j + 1
				}

			}else{
				OmegaEstimates$SE[l] <- temp2[length(temp2)]
				l <- l + 1
				if(j%in%whichBlock[pp]){
					j <- j + 1 #new
					for(k in 1:(cumBlock[pp]-1)){
						temp2 <- gsub("\\+        ","",temp[etaPos[j]+1]) #temp2 <- gsub("\\+        ","",temp[etaPos[j+k]+1])
						temp2 <- strsplit(gsub("  "," ",temp2)," ")[[1]]
						OmegaEstimates$SE[l:(l+length((length(temp2)-k):length(temp2))-1)] <- temp2[(length(temp2)-k):length(temp2)]
						l <- l+length((length(temp2)-k):length(temp2)) #l+length((length(temp2)-k))+1
						j <- j + 1
					}
					pp <- pp + 1
				}else{
					j <- j + 1
				}
			}
		}
	}

	if(sum(etaNumber%in%RanSum$ETA==TRUE)>0){
		OmegaEstimates$Shrinkage[etaNumber%in%RanSum$ETA] <- 1-RanSum$SD[RanSum$ETA%in%etaNumber]/sqrt(as.numeric(OmegaEstimates$Estimate[etaNumber%in%RanSum$ETA])) 
	}

	OmegaEstimates$CV <- signif(100*sqrt(as.numeric(OmegaEstimates$Estimate)),digits=info$digits)

	if(sum(is.na(OmegaEstimates$COV1)==FALSE)>0){
		for(i in which(!is.na(OmegaEstimates$COV1))){
			OmegaEstimates$CV[i] <- as.numeric(OmegaEstimates$Estimate[i])/(sqrt(as.numeric(OmegaEstimates$Estimate[as.character(OmegaEstimates$ETA)==as.character(OmegaEstimates$COV1[i])]))*sqrt(as.numeric(OmegaEstimates$Estimate[as.character(OmegaEstimates$ETA)==as.character(OmegaEstimates$COV2[i])])))
		}
	}

	OmegaEstimates$Estimate <- signif(as.numeric(OmegaEstimates$Estimate),digits=info$digits)

	OmegaEstimates$SE <- signif(as.numeric(OmegaEstimates$SE),digits=info$digits)
	OmegaEstimates$RSECV <- as.character(signif(0.5*100*OmegaEstimates$SE/OmegaEstimates$Estimate,digits=info$digits))
	OmegaEstimates$RSE <- as.character(signif(100*OmegaEstimates$SE/OmegaEstimates$Estimate,digits=info$digits))
	OmegaEstimates$CI95 <- paste("(",as.character(signif(OmegaEstimates$Estimate-qnorm(0.975)*OmegaEstimates$SE,digits=info$digits)),"-",as.character(signif(OmegaEstimates$Estimate+qnorm(0.975)*OmegaEstimates$SE,digits=info$digits)),")",sep="")
	OmegaEstimates$Shrinkage <- signif(100*as.numeric(OmegaEstimates$Shrinkage),digits=info$digits)
	OmegaEstimates$CV <- signif(as.numeric(OmegaEstimates$CV),digits=info$digits)
	
	OmegaEstimates$Estimate <- as.character(OmegaEstimates$Estimate)
	OmegaEstimates$SE <- as.character(OmegaEstimates$SE)

	OmegaEstimates$Estimate[OmegaEstimates$Estimate=="NA" | is.na(OmegaEstimates$Estimate)] <- "-"
	OmegaEstimates$SE[OmegaEstimates$SE=="NA" | is.na(OmegaEstimates$SE)] <- "-"
	OmegaEstimates$RSE[OmegaEstimates$RSE=="NA" | is.na(OmegaEstimates$RSE)] <- "-"
	OmegaEstimates$RSECV[OmegaEstimates$RSECV=="NA" | is.na(OmegaEstimates$RSECV)] <- "-"
	OmegaEstimates$CI95[OmegaEstimates$CI95=="NA" | is.na(OmegaEstimates$CI95)] <- "-"
	OmegaEstimates$Shrinkage[OmegaEstimates$Shrinkage=="NA" | is.na(OmegaEstimates$Shrinkage)] <- "-"

	OmegaEstimates$CI95[OmegaEstimates$CI95=="(NA-NA)"] <- "-"

	#Sigma
	sigmaPos <- grep("\\$SIGMA",CS)
	
	commentSigmaPos <- grep(";\\$SIGMA",gsub(" ","",CS))

	if(length(commentSigmaPos)>0){
		sigmaPos <- sigmaPos[-which(sigmaPos%in%commentSigmaPos)]
	}

	sigmaNames <- character()
	for(i in 1:length(sigmaPos)){
		if(regexpr(";",CS[sigmaPos[i]])[1]==-1){
			sigmaNames[i] <- paste("Sigma(",i,")",sep="")
		}else{
			sigmaNames[i] <- substring(CS[sigmaPos[i]],first=regexpr(";",CS[sigmaPos[i]])+1)
			sigmaNames[i] <- gsub(" ","",sigmaNames[i])

		}
	}


 	SigmaEstimates <- data.frame(Parameter=sigmaNames,
					Estimate  = rep(NA,length(sigmaNames)),
					SE  = rep(NA,length(sigmaNames)),
					RSE  = rep(NA,length(sigmaNames)),
					CI95 = rep(NA,length(sigmaNames)),
					Shrinkage = rep(NA,length(sigmaNames))
			    )

	for(i in 1:length(sigmaEstimatePos)){
		temp <- NMoutput[sigmaEstimatePos[i]:length(NMoutput)]

		for(j in 1:length(sigmaPos)){
			temp2 <- gsub("\\+        ","",temp[grep(paste("EPS",j,sep=""),temp)[1]+1])
			temp2 <- strsplit(gsub("  "," ",temp2)," ")[[1]]

			if(i==1){
				SigmaEstimates$Estimate[j] <- temp2[length(temp2)]
				if(!is.null(xpdb@Prefs@Xvardef$iwres)){
					SigmaEstimates$Shrinkage <- 1-sd(xpdb@Data[xpdb@Data[,xpdb@Prefs@Xvardef$dv]!=0,xpdb@Prefs@Xvardef$iwres])
				}

			}else{
				SigmaEstimates$SE[j] <- temp2[length(temp2)]
			}
		}
	}

	SigmaEstimates$Estimate <- signif(as.numeric(SigmaEstimates$Estimate),digits=info$digits)
	SigmaEstimates$SE <- signif(as.numeric(SigmaEstimates$SE),digits=info$digits)
	SigmaEstimates$RSE <- as.character(signif(100*SigmaEstimates$SE/SigmaEstimates$Estimate,digits=info$digits))
	SigmaEstimates$CI95 <- paste("(",as.character(signif(SigmaEstimates$Estimate-qnorm(0.975)*SigmaEstimates$SE,digits=info$digits)),"-",as.character(signif(SigmaEstimates$Estimate+qnorm(0.975)*SigmaEstimates$SE,digits=info$digits)),")",sep="")
	SigmaEstimates$CI95 <- as.character(signif(100*SigmaEstimates$Shrinkage,digits=info$digits))

	SigmaEstimates$Estimate <- as.character(SigmaEstimates$Estimate)
	SigmaEstimates$SE <- as.character(SigmaEstimates$SE)

	SigmaEstimates$Estimate[SigmaEstimates$Estimate=="NA" | is.na(SigmaEstimates$Estimate)] <- "-"
	SigmaEstimates$SE[SigmaEstimates$SE=="NA" | is.na(SigmaEstimates$SE)] <- "-"
	SigmaEstimates$RSE[SigmaEstimates$RSE=="NA" | is.na(SigmaEstimates$RSE)] <- "-"
	SigmaEstimates$CI95[SigmaEstimates$CI95=="NA" | is.na(SigmaEstimates$CI95)] <- "-"

	SigmaEstimates$CI95[SigmaEstimates$CI95=="(NA-NA)"] <- "-"

	#Combine estimates
	ThetaHeader <- ThetaEstimates[1,]
	ThetaHeader[1,] <- names(ThetaHeader)
	ThetaHeader$Parameter <- "Fixed-Effects_Parameters"

	OmegaHeader <- OmegaEstimates[1,]
	OmegaHeader[1,] <- names(OmegaHeader)
	OmegaHeader$Parameter <- "Inter-Individual Variability Parameters (CV%)"
	OmegaHeader[4] <- "RSE(%)"
	OmegaHeader[5] <- "Shrinkage(%)"
	OmegaHeader <- OmegaHeader[,1:5]

	OmegaEstimates$Estimate <- OmegaEstimates$CV
	OmegaEstimates$RSE <- OmegaEstimates$RSECV
	OmegaEstimates$CI95 <- OmegaEstimates$Shrinkage

	SigmaHeader <- SigmaEstimates[1,]
	SigmaHeader[1,] <- names(SigmaHeader)
	SigmaHeader$Parameter <- "Intra-Individual Variability Parameters (sigma^2)"
	SigmaHeader[4] <- "RSE(%)"
	SigmaHeader[5] <- "Shrinkage(%)"
	SigmaHeader <- SigmaHeader[,1:5]

	blankHeader <- SigmaHeader
	blankHeader[1:5] <- " "

	NMestimates <- rbind(ThetaEstimates,blankHeader,OmegaHeader,OmegaEstimates[,1:5],blankHeader,SigmaHeader,SigmaEstimates[,1:5])
	names(NMestimates) <- c("Fixed-Effects Parameters","Estimate","SE","RSE(%)","CI95")

	pack <- installed.packages()

	information <- data.frame(Drugname=info$drug.name,Path=info$path,
				Run=info$run.no,
				data=dataName,
				version=paste("v",pack["popPK","Version"],sep="")
				)

	if(!is.na(info$output)){
		write.table(NMestimates[,c(1:2,4:5)], file = "NMestimates.csv", sep= ",",col.names = TRUE, row.names=FALSE,quote=FALSE,na=" ")
		write.table(ParmSum, file = "ParameterSummary.csv", sep= ",",col.names = TRUE, row.names=FALSE,quote=FALSE)
		write.table(RanSum, file = "EtaSummary.csv", sep= ",",col.names = TRUE, row.names=FALSE,quote=FALSE)
      	write(CS,file="NONMEM control stream.txt")
      	write(NMoutput,file="NONMEM output.txt")
		write.table(information,file="info.csv", sep= ",",col.names = TRUE, row.names=FALSE,quote=FALSE)
	}else{
		cat("\nNONMEM Parameter Estimates:\n")
		print(NMestimates[,c(1:2,4:5)])

		cat("\nSummary of individual (POSTHOC) empirical Bayes parameter estimates:\n")
		print(ParmSum)

		cat("\nSummary of inter-individual random parameter estimates:\n")
		print(RanSum)
	}

	if(info$report==TRUE){
		wdGoToBookmark("blankEst")
		wdNormal("")
		wdBody(paste("The population PK parameter estimates for the ",info$drug.name," PK model are shown in Table 3.",sep=""))

		wdBody(paste("Table 3: Summary of ",info$drug.name," population PK model parameter estimates.",sep=""))
		row.names(NMestimates) <- NULL
		wdTable(format(NMestimates[,c(1:2,4:5)]))

		wdGoToBookmark("blankTitle")
		wdNormal("")
		row.names(information) <- NULL
		wdTable(format(information))

		wdSave(paste("popPKrun",info$run.no,sep=""))

	}

	xpdb@Doc <- c("NONMEM control stream:",CS,"NONMEM output:",NMoutput)
	assign("xpdb",xpdb,envir=.GlobalEnv)
	options(warn = 1)



}#--------------------------end of Estimates function-------end------end------end-----end----end----end------------------------------#


#=========================================function of Covariates Plot ===============================================================#
#Function Name: Covariates()                                                                                                         #
#                                                                                                                                    #
#====================================================================================================================================#

Covariates <-function(info){
	names(info) <- casefold(names(info),upper=FALSE)

	if(is.null(xpdb)){
		cat("Covariates Function: You forgot to load the data by running xposeData function first\n")
		cat("xposeData function is being called from within the Covariates function\n")
		xpdb <- xposeData(info)
	}
	
	if(sum(c("prefix","thetas","etas","covs","output","digits","device",
		          	"cat.level","group.by","strat.by","lloq","suffix",       
				"scale","smooth","log.pred","log.obs","log.covs","log.parms","ind.plots",         
				"col","line.col","units","drug.name","quantiles","bins",    
				"cov.rel", "eta.plot","cex","lty","lwd","pch","mean","alpha","report")%in%names(info)==FALSE)>0){
		
		cat("Covariates function: The info list has been changed and does not include all options")
		cat("CreateInfo function is being called from within the Covariates function")
		info <- CreateInfo(info)
	}

	if(!is.na(info$output)) while(dev.cur()!=1) dev.off()
	if(is.na(info$output)){
		if(length(grep(":",R.home()))>0) windows(rec=TRUE)
	}

	#Run Estimates(info) function if ThetaEstimates is not present
	if(sum(is.na(info$cov.rel)==FALSE)>0){
		if(is.null(xpdb@Doc)){
			cat("Covariates function: You forgot to run the Estimates function before running the Covariates function\n")
			cat("Estimates function is being called from within the Covariates function\n")
			Estimates(info)
		}
	}
	
	if(info$smooth!=TRUE) info$xsmooth <- NULL

	if(info$report==TRUE){
		wdGoToBookmark("blankCov")
		wdNormal("")
		wdBody(paste("The identified covariate-PK parameter relationships for ",info$drug.name," are shown in Figure 1.",sep=""))
		wdBody(paste("Figure 1: ",info$drug.name," covariate-PK parameter relationships. The solid lines are the population mean estimates and the box plots and black dots are based on the empirical Bayes individual estimates.",sep=""))
	}

	#win.metafile("Parm.vs.Cov%02d.wmf", pointsize = 10)
	#parm.vs.cov(xpdb,
	#	ids=NULL,
	#	onlyfirst=T,
	#	pch=info$pch[1],col=info$col[1],cex=info$cex[1],lty=info$lty[1],
	#	smooth=smooth,
	#	main=NULL,
	#	aspect="fill",
	#	x.cex=0.8,y.cex=0.8,
	#	scales=list(cex=1)
	#) 
	#dev.off()

	cov.data <- xpdb@Data[match(unique(xpdb@Data[,xpdb@Prefs@Xvardef$id]),xpdb@Data[,xpdb@Prefs@Xvardef$id]),]

 	#assign("cov.data",cov.data,envir=.GlobalEnv)

	parms <- NULL
	for(i in 1:length(info$cov.rel)){
		parms[i] <- as.character(info$cov.rel[[i]])[2]
	}

	cats <- NULL
	for(column in xpdb@Prefs@Xvardef$covariates){
		if (is.factor(xpdb@Data[[column]])){
            	cats <- c(cats, column)
 		}
	}

	nCov <- 1
	nETA <- 1
	plotETA <- 1
	nQuant <- 1
	ETAname <- NULL

	forName <- NULL
	forCov <- NULL
	forParm <- NULL
	refName <- NULL
	refValue <- NULL

	lowParm <- NULL
	highParm <- NULL

	lowCov <- NULL
	highCov <- NULL
	isCat <- NULL

	mCov1 <- NULL
	mCov2 <- NULL
	lCov <- NULL
	lCov <- NULL
	hCov <- NULL
	nForest <- 1

	ER <- list()

	if(info$log.parms){
		atLabelsY <- c(seq(0.001,0.01,0.001),seq(0.02,0.1,0.01),seq(0.2,1,0.1),seq(2,10,1),seq(20,100,10),seq(200,1000,100),seq(2000,10000,1000),seq(20000,100000,10000))
		LabelsY <- c("0.001",rep("",8),"0.01",rep("",8),"0.1",rep("",8),"1",rep("",8),"10",rep("",8),"100",rep("",8),"1000",rep("",8),"10000",rep("",8),"100000")
		logY <- 10
	}else{
		atLabelsY <- NA
		LabelsY <- NA
		logY <- FALSE
	}

	if(info$log.covs){
		atLabelsX <- c(seq(0.001,0.01,0.001),seq(0.02,0.1,0.01),seq(0.2,1,0.1),seq(2,10,1),seq(20,100,10),seq(200,1000,100),seq(2000,10000,1000),seq(20000,100000,10000))
		LabelsX <- c("0.001",rep("",8),"0.01",rep("",8),"0.1",rep("",8),"1",rep("",8),"10",rep("",8),"100",rep("",8),"1000",rep("",8),"10000",rep("",8),"100000")
		logX <- 10
	}else{
		atLabelsX <- NA
		LabelsX <- NA
		logX <- FALSE
	}

	logTicks <- function (lim, loc = c(1, 5)) {
		ii <- floor(log10(range(lim))) + c(-1, 2)
		main <- 10^(ii[1]:ii[2])
		r <- as.numeric(outer(loc, main, "*"))
		r[lim[1] <= r & r <= lim[2]]
	}

	xscale.components.log10 <- function(lim, ...) {
		ans <- xscale.components.default(lim = lim, ...)
		tick.at <- logTicks(10^lim, loc = 1:9)
		tick.at.major <- logTicks(10^lim, loc = 1)
		major <- tick.at %in% tick.at.major
		ans$bottom$ticks$at <- log(tick.at, 10)
		ans$bottom$ticks$tck <- ifelse(major, 1.5, 0.75)
		ans$bottom$labels$at <- log(tick.at, 10)
		ans$bottom$labels$labels <- as.character(tick.at)
		ans$bottom$labels$labels[!major] <- ""
		ans$bottom$labels$check.overlap <- FALSE
		ans
	}

	yscale.components.log10 <- function (lim, ...){
		ans <- yscale.components.default(lim = lim, ...)
		tick.at <- xpose.logTicks(10^lim, loc = 1:9)
		tick.at.major <- xpose.logTicks(10^lim, loc = 1)
		major <- tick.at %in% tick.at.major
		ans$left$ticks$at <- log(tick.at, 10)
		ans$left$ticks$tck <- ifelse(major, 1.5, 0.75)
		ans$left$labels$at <- log(tick.at, 10)
		ans$left$labels$labels <- as.character(tick.at)
		ans$left$labels$labels[!major] <- ""
		ans$left$labels$check.overlap <- FALSE
		ans
	}


	for(i in 1:length(xpdb@Prefs@Xvardef$parms)){
		if(is.factor(cov.data[,xpdb@Prefs@Xvardef$parms[i]])) cov.data[,xpdb@Prefs@Xvardef$parms[i]] <- as.numeric(as.character(cov.data[,xpdb@Prefs@Xvardef$parms[i]]))
		if(length(unique(eval(parse(text=paste("xpdb@Data$",xpdb@Prefs@Xvardef$parms[i],sep="")))))>1){
			for(n in 1:length(xpdb@Prefs@Xvardef$covariates)){
				ylb <- xpdb@Prefs@Xvardef$parms[i]

				if(xpdb@Prefs@Xvardef$parms[i]%in%names(info$etas)) ylb <- paste("Inter-individual variability for ",info$etas[[which(xpdb@Prefs@Xvardef$parms[i]==names(info$etas))]],sep="")
				if(xpdb@Prefs@Xvardef$parms[i]%in%names(info$thetas)) ylb <- info$thetas[[which(xpdb@Prefs@Xvardef$parms[i]==names(info$thetas))]]

				xlb <- xpdb@Prefs@Xvardef$covariates[n]

				if(xpdb@Prefs@Xvardef$covariates[n]%in%names(info$covs)) xlb <- info$covs[[which(xpdb@Prefs@Xvardef$covariates[n]==names(info$covs))]]

				cov.data$yvar <- rep(xpdb@Prefs@Xvardef$parms[i],nrow(cov.data))
				cov.data$xvar <- rep(xpdb@Prefs@Xvardef$covariates[n],nrow(cov.data))

				for(t in cats){
					if(length(levels(cov.data[,t]))>10){
						cov.data[,t] <- as.numeric(as.character(cov.data[,t]))
						cat("\nCategorical covariates can have a maximum of 10 levels\n")
						cat(paste(t," is converted to a continuous covariate\n"))
					}
				}
				if(is.factor(eval(parse(text=paste("cov.data$",xpdb@Prefs@Xvardef$covariates[n],sep=""))))==FALSE){     							
				#if(is.factor(eval(parse(text=paste("xpdb@Data$",xpdb@Prefs@Xvardef$covariates[n],sep=""))))==FALSE){     				
					if(!is.na(info$group.by)){
						keyValue <- levels(cov.data[,info$group.by])
						for(cat in 1:length(keyValue)){
							if(sum(is.na(info$cat.level)==TRUE)==0){
								if(sum(info$cat.level[[info$group.by]]%in%keyValue==TRUE)>0){
									keyValue[cat] <- names(info$cat.level[[info$group.by]][info$cat.level[[info$group.by]]%in%keyValue[cat]])
								}
							}
						}

						keyGroup <- info$group.by
						if(sum(is.na(info$covs)==TRUE)==0){
							if(sum(names(info$covs)%in%keyGroup==TRUE)>0){
								keyGroup <- info$covs[[which(names(info$covs)%in%keyGroup)]]
							}
						}

						if(sum(is.na(info$covs)==TRUE)==0){
							if(sum(names(info$covs)%in%keyGroup==TRUE)>0) keyGroup <- info$covs[[info$group.by]]
						}

						keytext <- list(paste(keyGroup,keyValue,sep=": "),cex=1)
						keyline <- list(type="p",pch=info$pch[1:length(levels(cov.data[,info$group.by]))],
										cex=info$cex[1:length(levels(cov.data[,info$group.by]))],
										lwd=info$lwd[1:length(levels(cov.data[,info$group.by]))],
										col=info$col[1:length(levels(cov.data[,info$group.by]))])
					}else{
						keytext <- list("",cex=0.001)
						keyline <- list(type="p",col=0,cex=.001)	
						keyGroup <- ""
					}

					if(min(cov.data[,xpdb@Prefs@Xvardef$parms[i]],na.rm=TRUE)<=0){

						if(info$log.covs==TRUE){
							plot99<-xYplot(
			     	 			eval(parse(text=paste(xpdb@Prefs@Xvardef$parms[i],"~",xpdb@Prefs@Xvardef$covariates[n],sep=""))),
       						data=cov.data,subset=eval(parse(text=paste(xvar,">0",sep=""))),
							#xlim=ifelse(info$log.covs,c(min(xpdb@Data[xpdb@Data[,xpdb@Prefs@Xvardef$covariates[n]]>0,xpdb@Prefs@Xvardef$covariates[n]],na.rm=TRUE),max(xpdb@Data[xpdb@Data[,xpdb@Prefs@Xvardef$covariates[n]]>0,xpdb@Prefs@Xvardef$covariates[n]],na.rm=TRUE)),NA),
							#ylim=ifelse(info$log.parms,c(min(xpdb@Data[xpdb@Data[,xpdb@Prefs@Xvardef$parms[i]]>0,xpdb@Prefs@Xvardef$parms[i]],na.rm=TRUE),max(xpdb@Data[xpdb@Data[,xpdb@Prefs@Xvardef$parms[i]]>0,xpdb@Prefs@Xvardef$parms[i]],na.rm=TRUE)),NA),
				      		layout=c(1,1),
							aspect=1/1,
							if(!is.na(info$group.by))groups=eval(parse(text=info$group.by)),
							key=list(x=0,y=1.0,corner=c(0,0),border=FALSE,transparent=TRUE,
									columns=ifelse(length(keytext[[1]])%%3==0,length(keytext[[1]])%/%3,length(keytext[[1]])%/%3+1),between=1,between.columns=1,text.width.multiplier=1,
									text = keytext, line=keyline),
							xlab=list(xlb,cex=1.5),
       						ylab=list(ylb,cex=1.5),
							scales=list(cex=1.5,x=list(log=10)),
							xscale.components = xscale.components.log10,
					     		strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
       						panel = function(x,y,...) {
	   							panel.xYplot(x,y,type=c("p"), col=info$col,lty=info$lty,pch=info$pch,cex=info$cex, lwd=info$lwd,...)
 	   							if(info$smooth==TRUE) panel.loess(x,y,col="blue",lty=4,lwd=3)
	   						},
	   						par.strip.text=list(cex=1.5)
         						)#end of xyplot
						}else{
							plot99<-xYplot(
			     	 			eval(parse(text=paste(xpdb@Prefs@Xvardef$parms[i],"~",xpdb@Prefs@Xvardef$covariates[n],sep=""))),
       						data=cov.data,
							#xlim=ifelse(info$log.covs,c(min(xpdb@Data[xpdb@Data[,xpdb@Prefs@Xvardef$covariates[n]]>0,xpdb@Prefs@Xvardef$covariates[n]],na.rm=TRUE),max(xpdb@Data[xpdb@Data[,xpdb@Prefs@Xvardef$covariates[n]]>0,xpdb@Prefs@Xvardef$covariates[n]],na.rm=TRUE)),NA),
							#ylim=ifelse(info$log.parms,c(min(xpdb@Data[xpdb@Data[,xpdb@Prefs@Xvardef$parms[i]]>0,xpdb@Prefs@Xvardef$parms[i]],na.rm=TRUE),max(xpdb@Data[xpdb@Data[,xpdb@Prefs@Xvardef$parms[i]]>0,xpdb@Prefs@Xvardef$parms[i]],na.rm=TRUE)),NA),
				      		layout=c(1,1),
							aspect=1/1,
							if(!is.na(info$group.by))groups=eval(parse(text=info$group.by)),
							key=list(x=0,y=1.0,corner=c(0,0),border=FALSE,transparent=TRUE,
									columns=ifelse(length(keytext[[1]])%%3==0,length(keytext[[1]])%/%3,length(keytext[[1]])%/%3+1),between=1,between.columns=1,text.width.multiplier=1,
									text = keytext, line=keyline),
							xlab=list(xlb,cex=1.5),
       						ylab=list(ylb,cex=1.5),
							scales=list(cex=1.5),
							#scales=list(cex=1.5,x=list(log=ifelse(info$log.covs,10,F)),
							#			y=list(log=ifelse(info$log.parms,10,F))),
					     		strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
       						panel = function(x,y,...) {
	   							panel.xYplot(x,y,type=c("p"), col=info$col,lty=info$lty,pch=info$pch,cex=info$cex, lwd=info$lwd,...)
 	   							if(info$smooth==TRUE) panel.loess(x,y,col="blue",lty=4,lwd=3)
	   						},
	   						par.strip.text=list(cex=1.5)
         						)#end of xyplot
						}
					}else{
						if(info$log.covs==FALSE & info$log.parms==FALSE){
							plot99<-xYplot(
			     	 			eval(parse(text=paste(xpdb@Prefs@Xvardef$parms[i],"~",xpdb@Prefs@Xvardef$covariates[n],sep=""))),
       						data=cov.data,
							#xlim=ifelse(info$log.covs,c(min(xpdb@Data[xpdb@Data[,xpdb@Prefs@Xvardef$covariates[n]]>0,xpdb@Prefs@Xvardef$covariates[n]],na.rm=TRUE),max(xpdb@Data[xpdb@Data[,xpdb@Prefs@Xvardef$covariates[n]]>0,xpdb@Prefs@Xvardef$covariates[n]],na.rm=TRUE)),NA),
							#ylim=ifelse(info$log.parms,c(min(xpdb@Data[xpdb@Data[,xpdb@Prefs@Xvardef$parms[i]]>0,xpdb@Prefs@Xvardef$parms[i]],na.rm=TRUE),max(xpdb@Data[xpdb@Data[,xpdb@Prefs@Xvardef$parms[i]]>0,xpdb@Prefs@Xvardef$parms[i]],na.rm=TRUE)),NA),
				      		layout=c(1,1),
							aspect=1/1,
							if(!is.na(info$group.by))groups=eval(parse(text=info$group.by)),
							key=list(x=0,y=1.0,corner=c(0,0),border=FALSE,transparent=TRUE,
									columns=ifelse(length(keytext[[1]])%%3==0,length(keytext[[1]])%/%3,length(keytext[[1]])%/%3+1),between=1,between.columns=1,text.width.multiplier=1,
									text = keytext, line=keyline),
							xlab=list(xlb,cex=1.5),
       						ylab=list(ylb,cex=1.5),
							scales=list(cex=1.5),
							#scales=list(cex=1.5,x=list(log=ifelse(info$log.covs,10,F)),
							#			y=list(log=ifelse(info$log.parms,10,F))),
					     		strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
       						panel = function(x,y,...) {
	   							panel.xYplot(x,y,type=c("p"), col=info$col,lty=info$lty,pch=info$pch,cex=info$cex, lwd=info$lwd,...)
 	   							if(info$smooth==TRUE) panel.loess(x,y,col="blue",lty=4,lwd=3)
	   						},
	   						par.strip.text=list(cex=1.5)
         						)#end of xyplot
						}

						if(info$log.covs==TRUE & info$log.parms==FALSE){
							plot99<-xYplot(
			     	 			eval(parse(text=paste(xpdb@Prefs@Xvardef$parms[i],"~",xpdb@Prefs@Xvardef$covariates[n],sep=""))),
       						data=cov.data,subset=eval(parse(text=paste(xvar,">0",sep=""))),
							#xlim=ifelse(info$log.covs,c(min(xpdb@Data[xpdb@Data[,xpdb@Prefs@Xvardef$covariates[n]]>0,xpdb@Prefs@Xvardef$covariates[n]],na.rm=TRUE),max(xpdb@Data[xpdb@Data[,xpdb@Prefs@Xvardef$covariates[n]]>0,xpdb@Prefs@Xvardef$covariates[n]],na.rm=TRUE)),NA),
							#ylim=ifelse(info$log.parms,c(min(xpdb@Data[xpdb@Data[,xpdb@Prefs@Xvardef$parms[i]]>0,xpdb@Prefs@Xvardef$parms[i]],na.rm=TRUE),max(xpdb@Data[xpdb@Data[,xpdb@Prefs@Xvardef$parms[i]]>0,xpdb@Prefs@Xvardef$parms[i]],na.rm=TRUE)),NA),
				      		layout=c(1,1),
							aspect=1/1,
							if(!is.na(info$group.by))groups=eval(parse(text=info$group.by)),
							key=list(x=0,y=1.0,corner=c(0,0),border=FALSE,transparent=TRUE,
									columns=ifelse(length(keytext[[1]])%%3==0,length(keytext[[1]])%/%3,length(keytext[[1]])%/%3+1),between=1,between.columns=1,text.width.multiplier=1,
									text = keytext, line=keyline),
							xlab=list(xlb,cex=1.5),
       						ylab=list(ylb,cex=1.5),
							scales=list(cex=1.5,x=list(log=10)),
							xscale.components = xscale.components.log10,
					     		strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
       						panel = function(x,y,...) {
	   							panel.xYplot(x,y,type=c("p"), col=info$col,lty=info$lty,pch=info$pch,cex=info$cex, lwd=info$lwd,...)
 	   							if(info$smooth==TRUE) panel.loess(x,y,col="blue",lty=4,lwd=3)
	   						},
	   						par.strip.text=list(cex=1.5)
         						)#end of xyplot
						}

						if(info$log.covs==FALSE & info$log.parms==TRUE){
							plot99<-xYplot(
			     	 			eval(parse(text=paste(xpdb@Prefs@Xvardef$parms[i],"~",xpdb@Prefs@Xvardef$covariates[n],sep=""))),
       						data=cov.data,subset=eval(parse(text=paste(yvar,">0",sep=""))),
							#xlim=ifelse(info$log.covs,c(min(xpdb@Data[xpdb@Data[,xpdb@Prefs@Xvardef$covariates[n]]>0,xpdb@Prefs@Xvardef$covariates[n]],na.rm=TRUE),max(xpdb@Data[xpdb@Data[,xpdb@Prefs@Xvardef$covariates[n]]>0,xpdb@Prefs@Xvardef$covariates[n]],na.rm=TRUE)),NA),
							#ylim=ifelse(info$log.parms,c(min(xpdb@Data[xpdb@Data[,xpdb@Prefs@Xvardef$parms[i]]>0,xpdb@Prefs@Xvardef$parms[i]],na.rm=TRUE),max(xpdb@Data[xpdb@Data[,xpdb@Prefs@Xvardef$parms[i]]>0,xpdb@Prefs@Xvardef$parms[i]],na.rm=TRUE)),NA),
				      		layout=c(1,1),
							aspect=1/1,
							if(!is.na(info$group.by))groups=eval(parse(text=info$group.by)),
							key=list(x=0,y=1.0,corner=c(0,0),border=FALSE,transparent=TRUE,
									columns=ifelse(length(keytext[[1]])%%3==0,length(keytext[[1]])%/%3,length(keytext[[1]])%/%3+1),between=1,between.columns=1,text.width.multiplier=1,
									text = keytext, line=keyline),
							xlab=list(xlb,cex=1.5),
       						ylab=list(ylb,cex=1.5),
							scales=list(cex=1.5,y=list(log=10)),
							yscale.components = yscale.components.log10,
					     		strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
       						panel = function(x,y,...) {
	   							panel.xYplot(x,y,type=c("p"), col=info$col,lty=info$lty,pch=info$pch,cex=info$cex, lwd=info$lwd,...)
 	   							if(info$smooth==TRUE) panel.loess(x,y,col="blue",lty=4,lwd=3)
	   						},
	   						par.strip.text=list(cex=1.5)
         						)#end of xyplot
						}

						if(info$log.covs==TRUE & info$log.parms==TRUE){					
							plot99<-xYplot(
			     	 			eval(parse(text=paste(xpdb@Prefs@Xvardef$parms[i],"~",xpdb@Prefs@Xvardef$covariates[n],sep=""))),
       						data=cov.data,subset=c(eval(parse(text=paste(xvar,">0 &",yvar,">0",sep="")))),
							#xlim=ifelse(info$log.covs,c(min(xpdb@Data[xpdb@Data[,xpdb@Prefs@Xvardef$covariates[n]]>0,xpdb@Prefs@Xvardef$covariates[n]],na.rm=TRUE),max(xpdb@Data[xpdb@Data[,xpdb@Prefs@Xvardef$covariates[n]]>0,xpdb@Prefs@Xvardef$covariates[n]],na.rm=TRUE)),NA),
							#ylim=ifelse(info$log.parms,c(min(xpdb@Data[xpdb@Data[,xpdb@Prefs@Xvardef$parms[i]]>0,xpdb@Prefs@Xvardef$parms[i]],na.rm=TRUE),max(xpdb@Data[xpdb@Data[,xpdb@Prefs@Xvardef$parms[i]]>0,xpdb@Prefs@Xvardef$parms[i]],na.rm=TRUE)),NA),
				      		layout=c(1,1),
							aspect=1/1,
							if(!is.na(info$group.by))groups=eval(parse(text=info$group.by)),
							key=list(x=0,y=1.0,corner=c(0,0),border=FALSE,transparent=TRUE,
									columns=ifelse(length(keytext[[1]])%%3==0,length(keytext[[1]])%/%3,length(keytext[[1]])%/%3+1),between=1,between.columns=1,text.width.multiplier=1,
									text = keytext, line=keyline),
							xlab=list(xlb,cex=1.5),
       						ylab=list(ylb,cex=1.5),
							scales=list(cex=1.5,x=list(log=10),y=list(log=10)),
							xscale.components = xscale.components.log10,
							yscale.components = yscale.components.log10,
					     		strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
       						panel = function(x,y,...) {
	   							panel.xYplot(x,y,type=c("p"), col=info$col,lty=info$lty,pch=info$pch,cex=info$cex, lwd=info$lwd,...)
 	   							if(info$smooth==TRUE) panel.loess(x,y,col="blue",lty=4,lwd=3)
	   						},
	   						par.strip.text=list(cex=1.5)
         						)#end of xyplot
						}
					}

					####covplot from info$cov.rel
					if(xpdb@Prefs@Xvardef$parms[i]%in%parms){
						covrel <- NULL
						covrelcat <- NULL
						keycat <- NULL
						keycatlevel <- NULL
						numcov <- 1
						numcat <- 1

						for(r in which(parms==xpdb@Prefs@Xvardef$parms[i])){
							catcov <- NULL

							if(regexpr(xpdb@Prefs@Xvardef$covariates[n],as.character(info$cov.rel[[r]])[3])[1]>0){
								
								covrel[numcov] <- as.character(info$cov.rel[[r]])[3]
								for(j in 1:nrow(ThetaEstimates)){
									covrel[numcov] <- gsub(paste("THETA\\(",j,"\\)",sep=""),ThetaEstimates$Estimate[j],covrel[numcov])
								}#end for
								covrel[numcov] <- gsub(xpdb@Prefs@Xvardef$covariates[n],"x",covrel[numcov])

								#Get categorical covariates
								for(p in cats){
									if(regexpr(p,covrel[numcov])[1]>0){
										catcov <- p
									}#end if
								}#end for

								if(!is.null(catcov)){
									for(q in levels(cov.data[,catcov])){
										covrelcat[numcat] <- gsub(catcov,q,covrel[numcov])	
										keycatlevel[numcat] <- q

										if(sum(is.na(info$cat.level)==TRUE)==0){
											if(sum(info$cat.level[[catcov]]%in%q==TRUE)>0){
												keycatlevel[numcat] <- names(info$cat.level[[catcov]][info$cat.level[[catcov]]%in%q])
											}
										}


										if(sum(is.na(info$covs)==TRUE)==0){
											if(sum(names(info$covs)%in%catcov==TRUE)>0){
												keycat[numcat] <- info$covs[[which(names(info$covs)%in%catcov)]]
											}else{
												keycat[numcat] <- catcov											
											}
										}else{
											keycat[numcat] <- catcov
										}

										numcat <- numcat + 1
									}
								}else{
									covrelcat[numcat] <- covrel[numcov]

									keycatlevel[numcat] <- NA
									if(sum(is.na(info$cov.rel)==TRUE)==0){
										if(!is.null(names(info$cov.rel)[r])){
											keycat[numcat] <- names(info$cov.rel)[r]
										}else{
											keycat[numcat] <- NA
										}
									}else{
										keycat[numcat] <- NA
									}

									numcat <- numcat + 1		
								}
							numcov <- numcov + 1	
							}
						}

						if(!is.null(covrelcat)){
							if(length(covrelcat)>1){
								if(sum(!is.na(keycat)==TRUE)>0){
									for(catn in 1:length(keycat)){
										if(!is.na(keycat[catn])){
											if(sum(is.na(info$cat.level)==TRUE)==0){
												if(sum(info$cat.level[[keycat[catn]]]%in%keycatlevel[catn]==TRUE)>0){
													keycatlevel[catn] <- names(info$cat.level[[keycat[catn]]][info$cat.level[[keycat[catn]]]%in%keycatlevel[catn]])
												}
											}
										}	
									}
									keycovrel <- paste(xpdb@Prefs@Xvardef$parms[i],"~",xpdb@Prefs@Xvardef$covariates[n]," (",keycat,"=",keycatlevel,")",sep="")
									keycovrel <- gsub(" \\(NA=NA\\)","",keycovrel)
									keycovrel <- gsub("=NA","",keycovrel)
								}else{
									keycovrel <- paste(xpdb@Prefs@Xvardef$parms[i],"~",xpdb@Prefs@Xvardef$covariates[n]," (Eq",1:length(covrelcat),")",sep="")
								}
							}else{		
								keycovrel <- paste(xpdb@Prefs@Xvardef$parms[i],"~",xpdb@Prefs@Xvardef$covariates[n]," (",keycat,"=",keycatlevel,")",sep="")
								keycovrel <- gsub(" \\(NA=NA\\)","",keycovrel)
								keycovrel <- gsub("\\(=NA\\)","",keycovrel)
								keycovrel <- gsub("=NA","",keycovrel)
							}
		
							if(!is.na(info$group.by)){
								keytext <- list(c(paste(keyGroup,keyValue,sep=": "),keycovrel),cex=1)
								keyline <- list(type=c(rep("p",length(levels(cov.data[,info$group.by]))),rep("l",length(keycovrel))),
										pch=info$pch[1:length(levels(cov.data[,info$group.by]))],
										cex=c(info$cex[1:length(levels(cov.data[,info$group.by]))],rep(5,length(keycovrel))),
										lwd=c(info$lwd[1:length(levels(cov.data[,info$group.by]))],rep(5,length(keycovrel))),
										col=c(info$col[1:length(levels(cov.data[,info$group.by]))],info$line.col[1:length(keycovrel)]))					
							}else{
								keytext <- list(c(keycovrel),cex=1)
								keyline <- list(type=c(rep("l",length(keycovrel))),
										pch=info$pch[1],
										cex=info$cex[1],
										lwd=c(rep(5,length(keycovrel))),
										col=c(info$line.col[1:length(keycovrel)]))					
							}
							
							if(min(cov.data[,xpdb@Prefs@Xvardef$parms[i]],na.rm=TRUE)<=0){
								if(info$log.covs==TRUE){
									plotCov <- xYplot(eval(parse(text=paste(xpdb@Prefs@Xvardef$parms[i],"~",xpdb@Prefs@Xvardef$covariates[n],sep=""))),
								      	data=cov.data,subset=eval(parse(text=paste(xvar,">0",sep=""))),
										layout=c(1,1),
										aspect=1/1,
										if(!is.na(info$group.by))groups=eval(parse(text=info$group.by)),
										xlab=list(xlb,cex=1.5),
										ylab=list(ylb,cex=1.5),
										key=list(x=0,y=1,corner=c(0,0),border=FALSE,transparent=TRUE,
											columns=ifelse(length(keytext[[1]])%%3==0,length(keytext[[1]])%/%3,length(keytext[[1]])%/%3+1),between=1,between.columns=1,text.width.multiplier=1,
											text = keytext, line=keyline),
										scales=list(cex=1.5,x=list(log=10)),
										strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
										#panel.data=covrel,
										xscale.components = xscale.components.log10,
										panel = function(x,y,...) {
											panel.xYplot(x,y,type=c("p"), col=info$col,lty=info$lty,pch=info$pch,cex=info$cex, lwd=info$lwd,...)
											color <- 1
											temp <- gsub("x","10^(x)",covrelcat)
											for(p in 1:length(covrelcat)){																					
												eval(parse(text=paste("panel.curve(expr=",temp[p],",from=",min(x,na.rm=TRUE),",to=",max(x,na.rm=TRUE),",col=info$line.col[color],lty=info$lty[color],lwd=8)",sep="")))
												color <- color+1
												if(color>length(info$col)) color <- 1
											}#end for
										},
										par.strip.text=list(cex=1.5)
								      	)#end of xyplot
								}else{
									plotCov <- xYplot(eval(parse(text=paste(xpdb@Prefs@Xvardef$parms[i],"~",xpdb@Prefs@Xvardef$covariates[n],sep=""))),
								      	data=cov.data,
										layout=c(1,1),
										aspect=1/1,
										if(!is.na(info$group.by))groups=eval(parse(text=info$group.by)),
										xlab=list(xlb,cex=1.5),
										ylab=list(ylb,cex=1.5),
										key=list(x=0,y=1,corner=c(0,0),border=FALSE,transparent=TRUE,
											columns=ifelse(length(keytext[[1]])%%3==0,length(keytext[[1]])%/%3,length(keytext[[1]])%/%3+1),between=1,between.columns=1,text.width.multiplier=1,
											text = keytext, line=keyline),
										scales=list(cex=1.5),
										strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
										#panel.data=covrel,
										panel = function(x,y,...) {
											panel.xYplot(x,y,type=c("p"), col=info$col,lty=info$lty,pch=info$pch,cex=info$cex, lwd=info$lwd,...)
											color <- 1
											for(p in 1:length(covrelcat)){																					
												eval(parse(text=paste("panel.curve(expr=",covrelcat[p],",from=",min(x,na.rm=TRUE),",to=",max(x,na.rm=TRUE),",col=info$line.col[color],lty=info$lty[color],lwd=8)",sep="")))
												color <- color+1
												if(color>length(info$col)) color <- 1
											}#end for
										},
										par.strip.text=list(cex=1.5)
								      	)#end of xyplot
								}
							}else{								
								if(info$log.covs==FALSE & info$log.parms==FALSE){
									plotCov <- xYplot(eval(parse(text=paste(xpdb@Prefs@Xvardef$parms[i],"~",xpdb@Prefs@Xvardef$covariates[n],sep=""))),
								      	data=cov.data,
										layout=c(1,1),
										aspect=1/1,
										if(!is.na(info$group.by))groups=eval(parse(text=info$group.by)),
										xlab=list(xlb,cex=1.5),
										ylab=list(ylb,cex=1.5),
										key=list(x=0,y=1,corner=c(0,0),border=FALSE,transparent=TRUE,
											columns=ifelse(length(keytext[[1]])%%3==0,length(keytext[[1]])%/%3,length(keytext[[1]])%/%3+1),between=1,between.columns=1,text.width.multiplier=1,
											text = keytext, line=keyline),
										scales=list(cex=1.5),
										strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
										#panel.data=covrel,
										panel = function(x,y,...) {
											panel.xYplot(x,y,type=c("p"), col=info$col,lty=info$lty,pch=info$pch,cex=info$cex, lwd=info$lwd,...)
											color <- 1
											for(p in 1:length(covrelcat)){																					
												eval(parse(text=paste("panel.curve(expr=",covrelcat[p],",from=",min(x,na.rm=TRUE),",to=",max(x,na.rm=TRUE),",col=info$line.col[color],lty=info$lty[color],lwd=8)",sep="")))
												color <- color+1
												if(color>length(info$col)) color <- 1
											}#end for
										},
										par.strip.text=list(cex=1.5)
								      	)#end of xyplot
								}

								if(info$log.covs==TRUE & info$log.parms==FALSE){
									plotCov <- xYplot(eval(parse(text=paste(xpdb@Prefs@Xvardef$parms[i],"~",xpdb@Prefs@Xvardef$covariates[n],sep=""))),
								      	data=cov.data,subset=eval(parse(text=paste(xvar,">0",sep=""))),
										layout=c(1,1),
										aspect=1/1,
										if(!is.na(info$group.by))groups=eval(parse(text=info$group.by)),
										xlab=list(xlb,cex=1.5),
										ylab=list(ylb,cex=1.5),
										key=list(x=0,y=1,corner=c(0,0),border=FALSE,transparent=TRUE,
											columns=ifelse(length(keytext[[1]])%%3==0,length(keytext[[1]])%/%3,length(keytext[[1]])%/%3+1),between=1,between.columns=1,text.width.multiplier=1,
											text = keytext, line=keyline),
										scales=list(cex=1.5,x=list(log=10)),
										strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
										#panel.data=covrel,
										xscale.components = xscale.components.log10,
										panel = function(x,y,...) {
											panel.xYplot(x,y,type=c("p"), col=info$col,lty=info$lty,pch=info$pch,cex=info$cex, lwd=info$lwd,...)
											color <- 1
											temp <- gsub("x","10^(x)",covrelcat)
											for(p in 1:length(covrelcat)){																					
												eval(parse(text=paste("panel.curve(expr=",temp[p],",from=",min(x,na.rm=TRUE),",to=",max(x,na.rm=TRUE),",col=info$line.col[color],lty=info$lty[color],lwd=8)",sep="")))
												color <- color+1
												if(color>length(info$col)) color <- 1
											}#end for
										},
										par.strip.text=list(cex=1.5)
								      	)#end of xyplot
								}

								if(info$log.covs==FALSE & info$log.parms==TRUE){
									plotCov <- xYplot(eval(parse(text=paste(xpdb@Prefs@Xvardef$parms[i],"~",xpdb@Prefs@Xvardef$covariates[n],sep=""))),
								      	data=cov.data,subset=eval(parse(text=paste(yvar,">0",sep=""))),
										layout=c(1,1),
										aspect=1/1,
										if(!is.na(info$group.by))groups=eval(parse(text=info$group.by)),
										xlab=list(xlb,cex=1.5),
										ylab=list(ylb,cex=1.5),
										key=list(x=0,y=1,corner=c(0,0),border=FALSE,transparent=TRUE,
											columns=ifelse(length(keytext[[1]])%%3==0,length(keytext[[1]])%/%3,length(keytext[[1]])%/%3+1),between=1,between.columns=1,text.width.multiplier=1,
											text = keytext, line=keyline),
										scales=list(cex=1.5,y=list(log=10)),
										strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
										#panel.data=covrel,
										yscale.components = yscale.components.log10,
										panel = function(x,y,...) {
											panel.xYplot(x,y,type=c("p"), col=info$col,lty=info$lty,pch=info$pch,cex=info$cex, lwd=info$lwd,...)
											color <- 1
											temp <- paste("log10(",covrelcat,")",sep="")
											for(p in 1:length(covrelcat)){																					
												eval(parse(text=paste("panel.curve(expr=",temp[p],",from=",min(x,na.rm=TRUE),",to=",max(x,na.rm=TRUE),",col=info$line.col[color],lty=info$lty[color],lwd=8)",sep="")))
												color <- color+1
												if(color>length(info$col)) color <- 1
											}#end for
										},
										par.strip.text=list(cex=1.5)
								      	)#end of xyplot
								}

								if(info$log.covs==TRUE & info$log.parms==TRUE){
									plotCov <- xYplot(eval(parse(text=paste(xpdb@Prefs@Xvardef$parms[i],"~",xpdb@Prefs@Xvardef$covariates[n],sep=""))),
								      	data=cov.data,subset=c(eval(parse(text=paste(xvar,">0 &", yvar,">0",sep="")))),
										layout=c(1,1),
										aspect=1/1,
										if(!is.na(info$group.by))groups=eval(parse(text=info$group.by)),
										xlab=list(xlb,cex=1.5),
										ylab=list(ylb,cex=1.5),
										key=list(x=0,y=1,corner=c(0,0),border=FALSE,transparent=TRUE,
											columns=ifelse(length(keytext[[1]])%%3==0,length(keytext[[1]])%/%3,length(keytext[[1]])%/%3+1),between=1,between.columns=1,text.width.multiplier=1,
											text = keytext, line=keyline),
										scales=list(cex=1.5,x=list(log=10),y=list(log=10)),
										strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
										#panel.data=covrel,
										xscale.components = xscale.components.log10,
										yscale.components = yscale.components.log10,
										panel = function(x,y,...) {
											panel.xYplot(x,y,type=c("p"), col=info$col,lty=info$lty,pch=info$pch,cex=info$cex, lwd=info$lwd,...)
											color <- 1
											temp <- paste("log10(",covrelcat,")",sep="")
											temp <- gsub("x","10^(x)",temp)
											for(p in 1:length(covrelcat)){																					
												eval(parse(text=paste("panel.curve(expr=",temp[p],",from=",min(x,na.rm=TRUE),",to=",max(x,na.rm=TRUE),",col=info$line.col[color],lty=info$lty[color],lwd=8)",sep="")))
												color <- color+1
												if(color>length(info$col)) color <- 1
											}#end for
										},
										par.strip.text=list(cex=1.5)
								      	)#end of xyplot
								}
							}

      						if(!is.na(info$output)){
								printGraph(paste("plotCov",nCov,sep=""),info$device)
           							print(plotCov)
            						dev.off()

								printGraph(paste(xpdb@Prefs@Xvardef$parms[i],".vs.",xpdb@Prefs@Xvardef$covariates[n],".Cov",sep=""),info$device)
            						print(plotCov)
            						dev.off() 
							}else{
								print(plotCov)
							}

							if(info$report==TRUE){
								wdGoToBookmark("blankCov2")
								wdPlot(plotCov,width=6,height=6)
							}

							nCov <- nCov + 1

							dotcov <- NULL
							nCat <- 0
							if(!is.null(covrel)){

								for(p in 1:length(covrel)){
									noCat <- TRUE
									for(q in cats){
										if(regexpr(q,covrel[p])[1]>0){
											dotcov[p] <- gsub(q,"0",covrel[p])
											noCat <- FALSE
											nCat <- nCat+1
										}							
									}#end for
									if(noCat) dotcov[p] <- covrel[p]

									forName[nForest] <- paste(xpdb@Prefs@Xvardef$parms[i],"~",xpdb@Prefs@Xvardef$covariates[n],sep="")
									if(noCat) forName[nForest] <- keycovrel[p+nCat]
									if(!is.null(names(info$cov.rel)[p])){
										if(names(info$cov.rel)[p]!=""){
											forName[nForest] <- paste(xpdb@Prefs@Xvardef$parms[i],"~",xpdb@Prefs@Xvardef$covariates[n]," (",names(info$cov.rel)[r],")",sep="")
										}
									}

									forCov[nForest] <- xpdb@Prefs@Xvardef$covariates[n]
									forParm[nForest] <- xpdb@Prefs@Xvardef$parms[i]
									isCat[nForest] <- FALSE
									refName[nForest] <- signif(median(cov.data[,xpdb@Prefs@Xvardef$covariates[n]],na.rm=TRUE),digits=info$digits)
									refValue[nForest] <- signif(eval(parse(text=gsub("x",median(cov.data[,xpdb@Prefs@Xvardef$covariates[n]],na.rm=TRUE),dotcov[p]))),digits=info$digits)

									lowParm[nForest] <- signif(eval(parse(text=gsub("x",min(cov.data[,xpdb@Prefs@Xvardef$covariates[n]],na.rm=TRUE),dotcov[p]))),digits=info$digits)
									highParm[nForest] <- signif(eval(parse(text=gsub("x",max(cov.data[,xpdb@Prefs@Xvardef$covariates[n]],na.rm=TRUE),dotcov[p]))),digits=info$digits)

									lCov[nForest] <- eval(parse(text=gsub("x",min(cov.data[,xpdb@Prefs@Xvardef$covariates[n]],na.rm=TRUE),dotcov[p])))
									lowCov[nForest] <- signif(min(cov.data[,xpdb@Prefs@Xvardef$covariates[n]],na.rm=TRUE),digits=info$digits)
									highCov[nForest] <- signif(max(cov.data[,xpdb@Prefs@Xvardef$covariates[n]],na.rm=TRUE),digits=info$digits)

									mCov1[nForest] <- eval(parse(text=gsub("x",median(cov.data[,xpdb@Prefs@Xvardef$covariates[n]],na.rm=TRUE),dotcov[p])))
									mCov2[nForest] <- eval(parse(text=gsub("x",median(cov.data[,xpdb@Prefs@Xvardef$covariates[n]],na.rm=TRUE),dotcov[p])))
									hCov[nForest] <- eval(parse(text=gsub("x",max(cov.data[,xpdb@Prefs@Xvardef$covariates[n]],na.rm=TRUE),dotcov[p])))
									nForest <- nForest + 1
								}#end for
							}#end if
						}#end if
					}#end if
					
					####Quantile plot
					probs <- 0:info$quantiles/info$quantiles

					qCOVLow <- NULL
					qCOVHigh <- NULL
					if(is.null(info$bins)) info$bins <- list(nobins=NA)
					if(!is.na(info$group.by)){
						for(j in sort(levels(cov.data[,info$group.by]))){
							if(sum(names(info$bins)%in%c(xpdb@Prefs@Xvardef$covariates[n])==TRUE)>0){
								CovBin <- sort(c(min(cov.data[,xpdb@Prefs@Xvardef$covariates[n]],na.rm=TRUE),info$bins[[which(names(info$bins)%in%c(xpdb@Prefs@Xvardef$covariates[n]))]],max(cov.data[,xpdb@Prefs@Xvardef$covariates[n]],na.rm=TRUE)))			
								CovBin <- CovBin[CovBin>=min(cov.data[,xpdb@Prefs@Xvardef$covariates[n]],na.rm=TRUE) & CovBin<=max(cov.data[,xpdb@Prefs@Xvardef$covariates[n]],na.rm=TRUE)]
								eval(parse(text=paste("qCOV",j," <- CovBin",sep="")))			
								#eval(parse(text=paste("qCOV",j," <- sort(c(",min(cov.data[,xpdb@Prefs@Xvardef$covariates[n]],na.rm=TRUE),",",paste(info$bins[[which(names(info$bins)%in%c(xpdb@Prefs@Xvardef$covariates[n]))]],collapse=","),",",max(cov.data[,xpdb@Prefs@Xvardef$covariates[n]],na.rm=TRUE),"))",sep="")))			
								probs <- eval(parse(text=paste("qCOV",j,sep="")))						
							}else{
								eval(parse(text=paste("qCOV",j," <- quantile(cov.data[cov.data[,info$group.by]==j,xpdb@Prefs@Xvardef$covariates[n]],probs,na.rm=TRUE)",sep="")))
							}
						}#end for		
						qCOV <- as.numeric(eval(parse(text=paste("c(",paste("qCOV",sort(levels(cov.data[,info$group.by])),sep="",collapse=","),")",sep=""))))

						for(j in 1:length(sort(levels(cov.data[,info$group.by])))){
							qCOVLow <- c(qCOVLow,as.numeric(eval(parse(text=paste("qCOV",sort(levels(cov.data[,info$group.by]))[j],"[-length(qCOV",sort(levels(cov.data[,info$group.by]))[j],")]",sep="")))))
							qCOVHigh <- c(qCOVHigh,as.numeric(eval(parse(text=paste("qCOV",sort(levels(cov.data[,info$group.by]))[j],"[-1]",sep="")))))
						}#end for	
					}else{
						if(sum(names(info$bins)%in%c(xpdb@Prefs@Xvardef$covariates[n])==TRUE)>0){
							CovBin <- sort(c(min(cov.data[,xpdb@Prefs@Xvardef$covariates[n]],na.rm=TRUE),info$bins[[which(names(info$bins)%in%c(xpdb@Prefs@Xvardef$covariates[n]))]],max(cov.data[,xpdb@Prefs@Xvardef$covariates[n]],na.rm=TRUE)))			
							CovBin <- CovBin[CovBin>=min(cov.data[,xpdb@Prefs@Xvardef$covariates[n]],na.rm=TRUE) & CovBin<=max(cov.data[,xpdb@Prefs@Xvardef$covariates[n]],na.rm=TRUE)]

							qCOV <- CovBin
							probs <- qCOV
						}else{
							qCOV <- quantile(cov.data[,xpdb@Prefs@Xvardef$covariates[n]],probs,na.rm=TRUE)
						}
						
						qCOVLow <- as.numeric(qCOV[-length(qCOV)])
						qCOVHigh <- as.numeric(qCOV[-1])
					}#end if	

					if(!is.na(info$group.by)){
						ExposureResponse <- data.frame(CpMid=rep(0,(2*length(sort(levels(cov.data[,info$group.by])))*length(probs)-length(sort(levels(cov.data[,info$group.by]))))))
					}else{
						ExposureResponse <- data.frame(CpMid=rep(0,(2*length(probs)-1)))
					}

					ExposureResponse$Low 		<- ExposureResponse$CpMid
					ExposureResponse$High		<- ExposureResponse$CpMid
					ExposureResponse$Response 	<- ExposureResponse$CpMid
					ExposureResponse$Lower 		<- rep(NA,nrow(ExposureResponse))
					ExposureResponse$Upper 		<- rep(NA,nrow(ExposureResponse))			

					if(!is.na(info$group.by)){
						nGroupby <- length(sort(levels(cov.data[,info$group.by]))) 
						ExposureResponse$Quantile <- 100*c(rep(c(probs[-length(probs)],probs),nGroupby))
						#ExposureResponse$Quantile <- 100*c(rep(c(probs[1:info$quantiles],probs),nGroupby))
						
						ExposureResponse$trt <- c(rep(sort(levels(cov.data[,info$group.by])),each=length(probs)-1),rep(sort(levels(cov.data[,info$group.by])),each=length(probs)))			
						ExposureResponse$type <- c(rep(1:(2*nGroupby),rep(c(length(probs)-1,length(probs)),each=nGroupby)))
					}else{
						nGroupby <- 1
						ExposureResponse$Quantile <- 100*c(probs[-length(probs)],probs)
						#ExposureResponse$Quantile <- 100*c(probs[1:info$quantiles],probs)
						ExposureResponse$trt <- c(rep(1,(length(probs)-1)),rep(1,length(probs)))			
						ExposureResponse$type <- rep(1:2,c(length(probs)-1,length(probs)))	
					}

					ExposureResponse$Low <- c(qCOVLow,qCOV)
					ExposureResponse$High <- c(qCOVHigh,qCOV)
						
					ExposureResponse$CovMid <- (ExposureResponse$High-ExposureResponse$Low)/2 + ExposureResponse$Low
					ExposureResponse$logCovMid <- exp((log(ExposureResponse$High)-log(ExposureResponse$Low))/2 + log(ExposureResponse$Low))
					ExposureResponse$CovMedian <- ExposureResponse$CovMid
					ExposureResponse$logCovMedian <- ExposureResponse$logCovMid

					ExposureResponse$N <- rep(NA,nrow(ExposureResponse))
					ExposureResponse$CI <- rep(NA,nrow(ExposureResponse))

					for(loop in 1:(nGroupby*(length(probs)-1))){
						if(loop%%(length(probs)-1)!=0){
							if(!is.na(info$group.by)){
								temp <- cov.data[as.character(cov.data[,info$group.by])==ExposureResponse$trt[loop] & cov.data[,xpdb@Prefs@Xvardef$covariates[n]]<ExposureResponse$High[loop] & cov.data[,xpdb@Prefs@Xvardef$covariates[n]]>=ExposureResponse$Low[loop] & cov.data[,xpdb@Prefs@Xvardef$covariates[n]]>0 & !is.na(cov.data[,xpdb@Prefs@Xvardef$covariates[n]]),]
							}else{
								temp <- cov.data[cov.data[,xpdb@Prefs@Xvardef$covariates[n]]<ExposureResponse$High[loop] & cov.data[,xpdb@Prefs@Xvardef$covariates[n]]>=ExposureResponse$Low[loop] & cov.data[,xpdb@Prefs@Xvardef$covariates[n]]>0 & !is.na(cov.data[,xpdb@Prefs@Xvardef$covariates[n]]),]					
							}#end if
						}else{
							if(!is.na(info$group.by)){
								temp <- cov.data[as.character(cov.data[,info$group.by])==ExposureResponse$trt[loop] & cov.data[,xpdb@Prefs@Xvardef$covariates[n]]>=ExposureResponse$Low[loop] & cov.data[,xpdb@Prefs@Xvardef$covariates[n]]>0 & !is.na(cov.data[,xpdb@Prefs@Xvardef$covariates[n]]),]
							}else{
								temp <- cov.data[cov.data[,xpdb@Prefs@Xvardef$covariates[n]]>=ExposureResponse$Low[loop] & cov.data[,xpdb@Prefs@Xvardef$covariates[n]]>0 & !is.na(cov.data[,xpdb@Prefs@Xvardef$covariates[n]]),]						
							}
						}#end if 

						if(info$mean!="geometric"){
							ExposureResponse$Response[loop] <- mean(temp[,xpdb@Prefs@Xvardef$parms[i]],na.rm=TRUE)
							ExposureResponse$Upper[loop] <- ExposureResponse$Response[loop] + qnorm(1-info$alpha/2)*sd(temp[,xpdb@Prefs@Xvardef$parms[i]],na.rm=TRUE)/sqrt(length(temp[,xpdb@Prefs@Xvardef$parms[i]]))
							ExposureResponse$Lower[loop] <- ExposureResponse$Response[loop] - qnorm(1-info$alpha/2)*sd(temp[,xpdb@Prefs@Xvardef$parms[i]],na.rm=TRUE)/sqrt(length(temp[,xpdb@Prefs@Xvardef$parms[i]]))
						}else{
							if(sum((cov.data[,xpdb@Prefs@Xvardef$parms[i]]>0)==FALSE)==0){
								ExposureResponse$Response[loop] <- exp(mean(log(temp[,xpdb@Prefs@Xvardef$parms[i]]),na.rm=TRUE))
								ExposureResponse$Upper[loop] <- exp(mean(log(temp[,xpdb@Prefs@Xvardef$parms[i]]),na.rm=TRUE) + qnorm(1-info$alpha/2)*sd(log(temp[,xpdb@Prefs@Xvardef$parms[i]]),na.rm=TRUE)/sqrt(length(temp[,xpdb@Prefs@Xvardef$parms[i]])))
								ExposureResponse$Lower[loop] <- exp(mean(log(temp[,xpdb@Prefs@Xvardef$parms[i]]),na.rm=TRUE) - qnorm(1-info$alpha/2)*sd(log(temp[,xpdb@Prefs@Xvardef$parms[i]]),na.rm=TRUE)/sqrt(length(temp[,xpdb@Prefs@Xvardef$parms[i]])))
							}else{
								ExposureResponse$Response[loop] <- mean(temp[,xpdb@Prefs@Xvardef$parms[i]],na.rm=TRUE)
								ExposureResponse$Upper[loop] <- ExposureResponse$Response[loop] + qnorm(1-info$alpha/2)*sd(temp[,xpdb@Prefs@Xvardef$parms[i]],na.rm=TRUE)/sqrt(length(temp[,xpdb@Prefs@Xvardef$parms[i]]))
								ExposureResponse$Lower[loop] <- ExposureResponse$Response[loop] - qnorm(1-info$alpha/2)*sd(temp[,xpdb@Prefs@Xvardef$parms[i]],na.rm=TRUE)/sqrt(length(temp[,xpdb@Prefs@Xvardef$parms[i]]))
							}
						}
						ExposureResponse$CovMedian[loop] <- median(temp[,xpdb@Prefs@Xvardef$covariates[n]],na.rm=TRUE)
						ExposureResponse$N[loop] <- length(temp[,xpdb@Prefs@Xvardef$covariates[n]])
					}#end for 
	
					ExposureResponse$logCovMedian <- ExposureResponse$CovMedian

					for(j in 1:nGroupby){
						for(jj in (nGroupby*(length(probs)-1)+(j-1)*length(probs)+1):(nGroupby*(length(probs)-1)+(j)*length(probs))){
							if(min(ExposureResponse$Lower,na.rm=TRUE)<0){
								ExposureResponse$Response[jj] <- (1+0.05*j)*min(ExposureResponse$Lower,na.rm=TRUE)			
							}else{
								ExposureResponse$Response[jj] <- (1-0.05*j)*min(ExposureResponse$Lower,na.rm=TRUE)
							}
						}#end for
					}#end for

					ExposureResponse$Cov <- ExposureResponse$CovMedian
					ExposureResponse$logCov <- ExposureResponse$logCovMedian

					assign("ExposureResponse",ExposureResponse,envir=.GlobalEnv)

					ER[[paste(xpdb@Prefs@Xvardef$parms[i],xpdb@Prefs@Xvardef$covariates[n],sep="~")]] <- ExposureResponse
					if(!is.na(info$group.by)){
						yfactor <- length(levels(xpdb@Data[,info$group.by]))
					}else{
						yfactor <- 0
					}
					if(min(cov.data[,xpdb@Prefs@Xvardef$parms[i]],na.rm=TRUE)<=0){
						if(info$log.covs==TRUE){
							plotQuantile <- xYplot(Cbind(Response,Lower,Upper)~Cov,
					 			data=ExposureResponse,subset=c(!is.na(ExposureResponse$Response) & ExposureResponse$Cov>0),
						 		layout=c(1,1),
                       					aspect=1,
								groups=ExposureResponse$type,
								#xlim=c(0.9*min(ExposureResponse$Low,na.rm=TRUE),1.1*max(ExposureResponse$High,na.rm=TRUE)),
								ylim=c((1+0.1+0.05*yfactor)*min(ExposureResponse$Lower,na.rm=TRUE),(1+0.05)*max(ExposureResponse$Upper,na.rm=TRUE)),
								key=list(x=0,y=1.0,corner=c(0,0),border=FALSE,transparent=TRUE,
									columns=ifelse(length(keytext[[1]])%%3==0,length(keytext[[1]])%/%3,length(keytext[[1]])%/%3+1),between=1,between.columns=1,text.width.multiplier=1,
									text = keytext, line=keyline),
								xlab=list(xlb,cex=1.5),
								ylab=list(ylb,cex=1.5),
                       					strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       					scales=list(cex=1.5,x=list(log=10)),
								xscale.components = xscale.components.log10,
                       					panel = function(x,y,subscripts,groups,...) {
									panel.xYplot(x[subscripts<nGroupby*(length(probs)-1)+1],y[subscripts<nGroupby*(length(probs)-1)+1],subscripts[subscripts<nGroupby*(length(probs)-1)+1],groups[subscripts<nGroupby*(length(probs)-1)+1],type=c("p"),methods="bars",label.curves=FALSE,col=info$col[1:nGroupby], lty=1,pch=info$pch[1:nGroupby],cex=info$cex[1:nGroupby], lwd=5,...)
									panel.xYplot(x[subscripts>nGroupby*(length(probs)-1) & subscripts<nGroupby*(2*length(probs)-1)+1],y[subscripts>nGroupby*(length(probs)-1) & subscripts<nGroupby*(2*length(probs)-1)+1],subscripts[subscripts>nGroupby*(length(probs)-1) & subscripts<nGroupby*(2*length(probs)-1)+1],groups,type=c("o"),label.curves=FALSE,col=info$col[1:nGroupby],lty=c(1),pch=c(124),cex=c(1.5), lwd=c(5),...)   	 							
									if(xpdb@Prefs@Xvardef$parms[i]%in%parms){
										if(!is.null(covrelcat)){
											color <- 1
											temp <- gsub("x","10^(x)",covrelcat)
											for(p in 1:length(covrelcat)){																					
												eval(parse(text=paste("panel.curve(expr=",temp[p],",from=",min(x,na.rm=TRUE),",to=",max(x,na.rm=TRUE),",col=info$line.col[color],lwd=8,lty=info$lty[color])",sep="")))
												color <- color+1
												if(color>length(info$col)) color <- 1
											}#end for
										}
									}#end if
								}, 
								par.strip.text=list(cex=1.5))
						}else{
							plotQuantile <- xYplot(Cbind(Response,Lower,Upper)~Cov,subset=!is.na(ExposureResponse$Response),
					 			data=ExposureResponse,
						 		layout=c(1,1),
                       					aspect=1,
								groups=ExposureResponse$type,
								#xlim=c(0.9*min(ExposureResponse$Low,na.rm=TRUE),1.1*max(ExposureResponse$High,na.rm=TRUE)),
								ylim=c((1+0.1+0.05*yfactor)*min(ExposureResponse$Lower,na.rm=TRUE),(1+0.1)*max(ExposureResponse$Upper,na.rm=TRUE)),
								key=list(x=0,y=1.0,corner=c(0,0),border=FALSE,transparent=TRUE,
									columns=ifelse(length(keytext[[1]])%%3==0,length(keytext[[1]])%/%3,length(keytext[[1]])%/%3+1),between=1,between.columns=1,text.width.multiplier=1,
									text = keytext, line=keyline),
								xlab=list(xlb,cex=1.5),
								ylab=list(ylb,cex=1.5),
                       					strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       					scales=list(cex=1.5),
                       					panel = function(x,y,subscripts,groups,...) {
									panel.xYplot(x[subscripts<nGroupby*(length(probs)-1)+1],y[subscripts<nGroupby*(length(probs)-1)+1],subscripts[subscripts<nGroupby*(length(probs)-1)+1],groups[subscripts<nGroupby*(length(probs)-1)+1],type=c("p"),methods="bars",label.curves=FALSE,col=info$col[1:nGroupby], lty=1,pch=info$pch[1:nGroupby],cex=info$cex[1:nGroupby], lwd=5,...)
									panel.xYplot(x[subscripts>nGroupby*(length(probs)-1) & subscripts<nGroupby*(2*length(probs)-1)+1],y[subscripts>nGroupby*(length(probs)-1) & subscripts<nGroupby*(2*length(probs)-1)+1],subscripts[subscripts>nGroupby*(length(probs)-1) & subscripts<nGroupby*(2*length(probs)-1)+1],groups,type=c("o"),label.curves=FALSE,col=info$col[1:nGroupby],lty=c(1),pch=c(124),cex=c(1.5), lwd=c(5),...)   	 							
									if(xpdb@Prefs@Xvardef$parms[i]%in%parms){
										if(!is.null(covrelcat)){
											color <- 1
											for(p in 1:length(covrelcat)){																					
												eval(parse(text=paste("panel.curve(expr=",covrelcat[p],",from=",min(x,na.rm=TRUE),",to=",max(x,na.rm=TRUE),",col=info$line.col[color],lwd=8,lty=info$lty[color])",sep="")))
												color <- color+1
												if(color>length(info$col)) color <- 1
											}#end for
										}
									}#end if
								}, 
								par.strip.text=list(cex=1.5))
						}
					}else{
						if(info$log.covs==FALSE & info$log.parms==FALSE){
							plotQuantile <- xYplot(Cbind(Response,Lower,Upper)~Cov,subset=!is.na(ExposureResponse$Response),
					 			data=ExposureResponse,
						 		layout=c(1,1),
                       					aspect=1,
								groups=ExposureResponse$type,
								#xlim=c(0.9*min(ExposureResponse$Low,na.rm=TRUE),1.1*max(ExposureResponse$High,na.rm=TRUE)),
								ylim=c((1-0.1-0.05*yfactor)*min(ExposureResponse$Lower,na.rm=TRUE),(1+0.1)*max(ExposureResponse$Upper,na.rm=TRUE)),
								key=list(x=0,y=1.0,corner=c(0,0),border=FALSE,transparent=TRUE,
									columns=ifelse(length(keytext[[1]])%%3==0,length(keytext[[1]])%/%3,length(keytext[[1]])%/%3+1),between=1,between.columns=1,text.width.multiplier=1,
									text = keytext, line=keyline),
								xlab=list(xlb,cex=1.5),
								ylab=list(ylb,cex=1.5),
                       					strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       					scales=list(cex=1.5),
                       					panel = function(x,y,subscripts,groups,...) {
									panel.xYplot(x[subscripts<nGroupby*(length(probs)-1)+1],y[subscripts<nGroupby*(length(probs)-1)+1],subscripts[subscripts<nGroupby*(length(probs)-1)+1],groups[subscripts<nGroupby*(length(probs)-1)+1],type=c("p"),methods="bars",label.curves=FALSE,col=info$col[1:nGroupby], lty=1,pch=info$pch[1:nGroupby],cex=info$cex[1:nGroupby], lwd=5,...)
									panel.xYplot(x[subscripts>nGroupby*(length(probs)-1) & subscripts<nGroupby*(2*length(probs)-1)+1],y[subscripts>nGroupby*(length(probs)-1) & subscripts<nGroupby*(2*length(probs)-1)+1],subscripts[subscripts>nGroupby*(length(probs)-1) & subscripts<nGroupby*(2*length(probs)-1)+1],groups,type=c("o"),label.curves=FALSE,col=info$col[1:nGroupby],lty=c(1),pch=c(124),cex=c(1.5), lwd=c(5),...)   	 							
									if(xpdb@Prefs@Xvardef$parms[i]%in%parms){
										if(!is.null(covrelcat)){
											color <- 1
											for(p in 1:length(covrelcat)){																					
												eval(parse(text=paste("panel.curve(expr=",covrelcat[p],",from=",min(x,na.rm=TRUE),",to=",max(x,na.rm=TRUE),",col=info$line.col[color],lwd=8,lty=info$lty[color])",sep="")))
												color <- color+1
												if(color>length(info$col)) color <- 1
											}#end for
										}
									}#end if
								}, 
								par.strip.text=list(cex=1.5))
						}

						if(info$log.covs==TRUE & info$log.parms==FALSE){
							plotQuantile <- xYplot(Cbind(Response,Lower,Upper)~Cov,
					 			data=ExposureResponse,subset=c(!is.na(ExposureResponse$Response) & ExposureResponse$Cov>0),
						 		layout=c(1,1),
                       					aspect=1,
								groups=ExposureResponse$type,
								#xlim=c(0.9*min(ExposureResponse$Low,na.rm=TRUE),1.1*max(ExposureResponse$High,na.rm=TRUE)),
								ylim=c((1-0.1-0.05*yfactor)*min(ExposureResponse$Lower,na.rm=TRUE),(1+0.1)*max(ExposureResponse$Upper,na.rm=TRUE)),
								key=list(x=0,y=1.0,corner=c(0,0),border=FALSE,transparent=TRUE,
									columns=ifelse(length(keytext[[1]])%%3==0,length(keytext[[1]])%/%3,length(keytext[[1]])%/%3+1),between=1,between.columns=1,text.width.multiplier=1,
									text = keytext, line=keyline),
								xlab=list(xlb,cex=1.5),
								ylab=list(ylb,cex=1.5),
                       					strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       					scales=list(cex=1.5,x=list(log=10)),
								xscale.components = xscale.components.log10,
                       					panel = function(x,y,subscripts,groups,...) {
									panel.xYplot(x[subscripts<nGroupby*(length(probs)-1)+1],y[subscripts<nGroupby*(length(probs)-1)+1],subscripts[subscripts<nGroupby*(length(probs)-1)+1],groups[subscripts<nGroupby*(length(probs)-1)+1],type=c("p"),methods="bars",label.curves=FALSE,col=info$col[1:nGroupby], lty=1,pch=info$pch[1:nGroupby],cex=info$cex[1:nGroupby], lwd=5,...)
									panel.xYplot(x[subscripts>nGroupby*(length(probs)-1) & subscripts<nGroupby*(2*length(probs)-1)+1],y[subscripts>nGroupby*(length(probs)-1) & subscripts<nGroupby*(2*length(probs)-1)+1],subscripts[subscripts>nGroupby*(length(probs)-1) & subscripts<nGroupby*(2*length(probs)-1)+1],groups,type=c("o"),label.curves=FALSE,col=info$col[1:nGroupby],lty=c(1),pch=c(124),cex=c(1.5), lwd=c(5),...)   	 							
									if(xpdb@Prefs@Xvardef$parms[i]%in%parms){
										if(!is.null(covrelcat)){
											color <- 1
											temp <- gsub("x","10^(x)",covrelcat)
											for(p in 1:length(covrelcat)){																					
												eval(parse(text=paste("panel.curve(expr=",temp[p],",from=",min(x,na.rm=TRUE),",to=",max(x,na.rm=TRUE),",col=info$line.col[color],lwd=8,lty=info$lty[color])",sep="")))
												color <- color+1
												if(color>length(info$col)) color <- 1
											}#end for
										}
									}#end if
								}, 
								par.strip.text=list(cex=1.5))
						}

						if(info$log.covs==FALSE & info$log.parms==TRUE){
							plotQuantile <- xYplot(Cbind(Response,log10(Lower),log10(Upper))~Cov,
					 			data=ExposureResponse,subset=c(!is.na(ExposureResponse$Response)),
						 		layout=c(1,1),
                       					aspect=1,
								groups=ExposureResponse$type,
								#xlim=c(0.9*min(ExposureResponse$Low,na.rm=TRUE),1.1*max(ExposureResponse$High,na.rm=TRUE)),
								ylim=c((1-0.1-0.05*yfactor)*min(ExposureResponse$Lower,na.rm=TRUE),(1+0.1)*max(ExposureResponse$Upper,na.rm=TRUE)),
								key=list(x=0,y=1.0,corner=c(0,0),border=FALSE,transparent=TRUE,
									columns=ifelse(length(keytext[[1]])%%3==0,length(keytext[[1]])%/%3,length(keytext[[1]])%/%3+1),between=1,between.columns=1,text.width.multiplier=1,
									text = keytext, line=keyline),
								xlab=list(xlb,cex=1.5),
								ylab=list(ylb,cex=1.5),
                       					strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       					scales=list(cex=1.5,y=list(log=10)),
								yscale.components = yscale.components.log10,
                       					panel = function(x,y,subscripts,groups,...) {
									panel.xYplot(x[subscripts<nGroupby*(length(probs)-1)+1],y[subscripts<nGroupby*(length(probs)-1)+1],subscripts[subscripts<nGroupby*(length(probs)-1)+1],groups[subscripts<nGroupby*(length(probs)-1)+1],type=c("p"),methods="bars",label.curves=FALSE,col=info$col[1:nGroupby], lty=1,pch=info$pch[1:nGroupby],cex=info$cex[1:nGroupby], lwd=5,...)
									panel.xYplot(x[subscripts>nGroupby*(length(probs)-1) & subscripts<nGroupby*(2*length(probs)-1)+1],y[subscripts>nGroupby*(length(probs)-1) & subscripts<nGroupby*(2*length(probs)-1)+1],subscripts[subscripts>nGroupby*(length(probs)-1) & subscripts<nGroupby*(2*length(probs)-1)+1],groups,type=c("o"),label.curves=FALSE,col=info$col[1:nGroupby],lty=c(1),pch=c(124),cex=c(1.5), lwd=c(5),...)   	 							
									if(xpdb@Prefs@Xvardef$parms[i]%in%parms){
										if(!is.null(covrelcat)){
											color <- 1
											temp <- paste("log10(",covrelcat,")",sep="")
											for(p in 1:length(covrelcat)){																					
												eval(parse(text=paste("panel.curve(expr=",temp[p],",from=",min(x,na.rm=TRUE),",to=",max(x,na.rm=TRUE),",col=info$line.col[color],lwd=8,lty=info$lty[color])",sep="")))
												color <- color+1
												if(color>length(info$col)) color <- 1
											}#end for
										}
									}#end if
								}, 
								par.strip.text=list(cex=1.5))
						}
						if(info$log.covs==TRUE & info$log.parms==TRUE){
							plotQuantile <- xYplot(Cbind(Response,log10(Lower),log10(Upper))~Cov,
					 			data=ExposureResponse,subset=c(!is.na(ExposureResponse$Response) & ExposureResponse$Cov>0),
						 		layout=c(1,1),
                       					aspect=1,
								groups=ExposureResponse$type,
								#xlim=c(0.9*min(ExposureResponse$Low,na.rm=TRUE),1.1*max(ExposureResponse$High,na.rm=TRUE)),
								ylim=c((1-0.1-0.05*yfactor)*min(ExposureResponse$Lower,na.rm=TRUE),(1+0.1)*max(ExposureResponse$Upper,na.rm=TRUE)),
								key=list(x=0,y=1.0,corner=c(0,0),border=FALSE,transparent=TRUE,
									columns=ifelse(length(keytext[[1]])%%3==0,length(keytext[[1]])%/%3,length(keytext[[1]])%/%3+1),between=1,between.columns=1,text.width.multiplier=1,
									text = keytext, line=keyline),
								xlab=list(xlb,cex=1.5),
								ylab=list(ylb,cex=1.5),
                       					strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       					scales=list(cex=1.5,x=list(log=10),y=list(log=10)),
								xscale.components = xscale.components.log10,
								yscale.components = yscale.components.log10,
                       					panel = function(x,y,subscripts,groups,...) {
									panel.xYplot(x[subscripts<nGroupby*(length(probs)-1)+1],y[subscripts<nGroupby*(length(probs)-1)+1],subscripts[subscripts<nGroupby*(length(probs)-1)+1],groups[subscripts<nGroupby*(length(probs)-1)+1],type=c("p"),methods="bars",label.curves=FALSE,col=info$col[1:nGroupby], lty=1,pch=info$pch[1:nGroupby],cex=info$cex[1:nGroupby], lwd=5,...)
									panel.xYplot(x[subscripts>nGroupby*(length(probs)-1) & subscripts<nGroupby*(2*length(probs)-1)+1],y[subscripts>nGroupby*(length(probs)-1) & subscripts<nGroupby*(2*length(probs)-1)+1],subscripts[subscripts>nGroupby*(length(probs)-1) & subscripts<nGroupby*(2*length(probs)-1)+1],groups,type=c("o"),label.curves=FALSE,col=info$col[1:nGroupby],lty=c(1),pch=c(124),cex=c(1.5), lwd=c(5),...)   	 							
									if(xpdb@Prefs@Xvardef$parms[i]%in%parms){
										if(!is.null(covrelcat)){
											color <- 1
											temp <- paste("log10(",covrelcat,")",sep="")
											temp <- gsub("x","10^(x)",temp)
											for(p in 1:length(covrelcat)){																					
												eval(parse(text=paste("panel.curve(expr=",temp[p],",from=",min(x,na.rm=TRUE),",to=",max(x,na.rm=TRUE),",col=info$line.col[color],lwd=8,lty=info$lty[color])",sep="")))
												color <- color+1
												if(color>length(info$col)) color <- 1
											}#end for
										}
									}#end if
								}, 
								par.strip.text=list(cex=1.5))
						}
					}

			      	if(!is.na(info$output)){
						printGraph(paste(xpdb@Prefs@Xvardef$parms[i],".vs.",xpdb@Prefs@Xvardef$covariates[n],".Quantile",sep=""),info$device)
           					print(plotQuantile)
            				dev.off()
						
						if(xpdb@Prefs@Xvardef$parms[i]%in%parms){
      						if(!is.null(covrelcat)){
								printGraph(paste("plotCovQuantile",nQuant,"",sep=""),info$device)
           							print(plotQuantile)
            						dev.off()
								nQuant <- nQuant + 1 
							}#end if
						}#end if

						####ETA plot from info$eta.plot
						if(names(info$eta.plot)[1]!="noetaplot"){
							if(xpdb@Prefs@Xvardef$parms[i]%in%names(info$eta.plot)){
								rans <- info$eta.plot[xpdb@Prefs@Xvardef$parms[i]==names(info$eta.plot)][[1]]
								if(xpdb@Prefs@Xvardef$covariates[n]%in%rans){
									printGraph(paste("plotETA",nETA,"",sep=""),info$device)
           								print(plot99)
           								dev.off()
									nETA <- nETA + 1 
								}	
							}
						}
					}else{
           					print(plotQuantile)

					}

					if(info$report==TRUE){
						wdGoToBookmark("blankQuant")
						wdPlot(plotQuantile,width=6,height=6)
					}#end if

     				}else{#end if (is.factor)
					eval(parse(text=paste("tempCov <- as.character(cov.data$",xpdb@Prefs@Xvardef$covariates[n],")",sep="")))
					tempCov <- ordered(tempCov,levels=sort(unique(tempCov)))
					CatValue <- levels(tempCov)
					for(cat in 1:length(CatValue)){
						if(sum(is.na(info$cat.level)==TRUE)==0){
							if(sum(info$cat.level[[xpdb@Prefs@Xvardef$covariates[n]]]%in%CatValue==TRUE)>0){
								if(CatValue[cat]%in%info$cat.level[[xpdb@Prefs@Xvardef$covariates[n]]]){
									CatValue[cat] <- names(info$cat.level[[xpdb@Prefs@Xvardef$covariates[n]]][info$cat.level[[xpdb@Prefs@Xvardef$covariates[n]]]%in%CatValue[cat]])
								}
							}
						}
					}
						
					if(min(cov.data[,xpdb@Prefs@Xvardef$parms[i]],na.rm=TRUE)<=0){
	          				plot99 <- bwplot (
      	                 			eval(parse(text=paste(xpdb@Prefs@Xvardef$parms[i],"~",xpdb@Prefs@Xvardef$covariates[n],sep=""))),
            	           			data=cov.data,
                  	     			layout=c(1,1),
                       				aspect=1/1,
							horizontal=FALSE,
                       				xlab=list(xlb,cex=1.5),
	                      			ylab=list(ylb,cex=1.5),
      	                 			strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
							scales=list(cex=1.5,x=list(labels=CatValue,rot=45)),
                  	     			#scales=list(cex=1.5,y=list(at=seq(0,300,100)),x=list(at=c(1,2),labels=c("Male","Female"))),#,x=list(log=10,at=seq(0.01,0.05,0.01))),
                       				panel = function(x,y,...) {	
      							panel.bwplot(x,y,type=c("p"), col=1,lty=c(1),pch=c(20),cex=2, lwd=5,...)
				
								for(q in 1:length(levels(cov.data[,xpdb@Prefs@Xvardef$covariates[n]]))){
									panel.text(x=q,y=min(cov.data[,xpdb@Prefs@Xvardef$parms[i]],na.rm=TRUE),paste("(N=",nrow(cov.data[cov.data[,xpdb@Prefs@Xvardef$covariates[n]]==unique(sort(as.character(cov.data[,xpdb@Prefs@Xvardef$covariates[n]])))[q],]),")",sep=""))
								}
							}, par.strip.text=list(cex=1.5))
   					}else{
	          				if(info$log.parms==TRUE){
							plot99 <- bwplot (
      		                 			eval(parse(text=paste(xpdb@Prefs@Xvardef$parms[i],"~",xpdb@Prefs@Xvardef$covariates[n],sep=""))),
            		           			data=cov.data,
                  		     			layout=c(1,1),
                       					aspect=1/1,
								horizontal=FALSE,
                       					xlab=list(xlb,cex=1.5),
	                      				ylab=list(ylb,cex=1.5),
	      	                 			strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
								scales=list(cex=1.5,y=list(log=10),x=list(labels=CatValue,rot=45)),
								yscale.components = yscale.components.log10,
            	      	     			#scales=list(cex=1.5,y=list(at=seq(0,300,100)),x=list(at=c(1,2),labels=c("Male","Female"))),#,x=list(log=10,at=seq(0.01,0.05,0.01))),
                  	     				panel = function(x,y,...) {	
      								panel.bwplot(x,y,type=c("p"), col=1,lty=c(1),pch=c(20),cex=2, lwd=5,...)
					
									for(q in 1:length(levels(cov.data[,xpdb@Prefs@Xvardef$covariates[n]]))){
										panel.text(x=q,y=log10(min(cov.data[,xpdb@Prefs@Xvardef$parms[i]],na.rm=TRUE)),paste("(N=",nrow(cov.data[cov.data[,xpdb@Prefs@Xvardef$covariates[n]]==unique(sort(as.character(cov.data[,xpdb@Prefs@Xvardef$covariates[n]])))[q],]),")",sep=""))
									}
								}, par.strip.text=list(cex=1.5))
						}else{
							plot99 <- bwplot (
      		                 			eval(parse(text=paste(xpdb@Prefs@Xvardef$parms[i],"~",xpdb@Prefs@Xvardef$covariates[n],sep=""))),
            		           			data=cov.data,
                  		     			layout=c(1,1),
                       					aspect=1/1,
								horizontal=FALSE,
                       					xlab=list(xlb,cex=1.5),
	                      				ylab=list(ylb,cex=1.5),
	      	                 			strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
								scales=list(cex=1.5,x=list(labels=CatValue,rot=45)),
            	      	     			#scales=list(cex=1.5,y=list(at=seq(0,300,100)),x=list(at=c(1,2),labels=c("Male","Female"))),#,x=list(log=10,at=seq(0.01,0.05,0.01))),
                  	     				panel = function(x,y,...) {	
      								panel.bwplot(x,y,type=c("p"), col=1,lty=c(1),pch=c(20),cex=2, lwd=5,...)
					
									for(q in 1:length(levels(cov.data[,xpdb@Prefs@Xvardef$covariates[n]]))){
										panel.text(x=q,y=min(cov.data[,xpdb@Prefs@Xvardef$parms[i]],na.rm=TRUE),paste("(N=",nrow(cov.data[cov.data[,xpdb@Prefs@Xvardef$covariates[n]]==unique(sort(as.character(cov.data[,xpdb@Prefs@Xvardef$covariates[n]])))[q],]),")",sep=""))
									}
								}, par.strip.text=list(cex=1.5))
						}

					} 
					####covplot from info$cov.rel
					if(xpdb@Prefs@Xvardef$parms[i]%in%parms){
						covrel <- NULL
						numcov <- 1

						for(r in which(parms==xpdb@Prefs@Xvardef$parms[i])){
							concov <- NULL
							if(regexpr(xpdb@Prefs@Xvardef$covariates[n],as.character(info$cov.rel[[r]])[3])[1]>0){
								covrel[numcov] <- as.character(info$cov.rel[[r]])[3]
								for(j in 1:nrow(ThetaEstimates)){
									covrel[numcov] <- gsub(paste("THETA\\(",j,"\\)",sep=""),ThetaEstimates$Estimate[j],covrel[numcov])
								}#end for
								covrel[numcov] <- gsub(xpdb@Prefs@Xvardef$covariates[n],"x",covrel[numcov])

							}
						}
						if(!is.null(covrel)){
					
							if(min(cov.data[,xpdb@Prefs@Xvardef$parms[i]],na.rm=TRUE)<=0){
								plotCov <- bwplot(
                       						eval(parse(text=paste(xpdb@Prefs@Xvardef$parms[i],"~",xpdb@Prefs@Xvardef$covariates[n],sep=""))),
		      						data=cov.data,
									horizontal=FALSE,
									layout=c(1,1),
									aspect=1/1,
		      						#groups=eval(parse(text=info$group.by)),
									xlab=list(xlb,cex=1.5),
									ylab=list(ylb,cex=1.5),
									strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
									scales=list(cex=1.5,x=list(labels=CatValue,rot=45)),
									#panel.data=covrel,
									panel = function(x,y,...) {
										panel.bwplot(x,y,type=c("p"), col=1,lty=c(1),pch=c(20),cex=2, lwd=5,...)
										#panel.bwplot(x,y,type=c("p"), col=1,lty=c(1),pch=c(20),cex=2, lwd=5,...)
										for(p in 1:length(covrel)){
											concov<-NULL
											color <- 1
											#Get continuous covariates
											for(q in xpdb@Prefs@Xvardef$covariates){
												if(regexpr(q,covrel[p])[1]>0){
													concov <- q
												}#end if
											}#end for											
											
											for(q in 1:length(levels(cov.data[,xpdb@Prefs@Xvardef$covariates[n]]))){
												covrelcon <- gsub("x",levels(cov.data[,xpdb@Prefs@Xvardef$covariates[n]])[q],covrel[p])	
												covrelcon <- paste(covrelcon,"+0*x",sep="")
												if(!is.null(concov)) covrelcon <- gsub(concov,eval(parse(text="median(cov.data[cov.data[,xpdb@Prefs@Xvardef$covariates[n]]==levels(cov.data[,xpdb@Prefs@Xvardef$covariates[n]])[q],concov],na.rm=TRUE)")),covrelcon)	
												eval(parse(text=paste("panel.curve(expr=",covrelcon,",from=",q-0.3,",to=",q+0.3,",col=info$line.col[color],lty=info$lty[color],lwd=8)",sep="")))
											}
											color <- color + 1
											if(color>length(info$col)) color <- 1
										}
										for(s in 1:length(levels(cov.data[,xpdb@Prefs@Xvardef$covariates[n]]))){
											panel.text(x=s,y=min(cov.data[,xpdb@Prefs@Xvardef$parms[i]],na.rm=TRUE),paste("(N=",nrow(cov.data[cov.data[,xpdb@Prefs@Xvardef$covariates[n]]==unique(sort(as.character(cov.data[,xpdb@Prefs@Xvardef$covariates[n]])))[s],]),")",sep=""))
										}
									},
									par.strip.text=list(cex=1.5)
		      						)#end of xyplot
							}else{
								if(info$log.parms==TRUE){
									plotCov <- bwplot(
                       							eval(parse(text=paste(xpdb@Prefs@Xvardef$parms[i],"~",xpdb@Prefs@Xvardef$covariates[n],sep=""))),
			      						data=cov.data,
										horizontal=FALSE,
										layout=c(1,1),
										aspect=1/1,
		      							#groups=eval(parse(text=info$group.by)),
										xlab=list(xlb,cex=1.5),
										ylab=list(ylb,cex=1.5),
										strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
										scales=list(cex=1.5,y=list(log=10),x=list(labels=CatValue,rot=45)),
										yscale.components = yscale.components.log10,
										#panel.data=covrel,
										panel = function(x,y,...) {
											panel.bwplot(x,y,type=c("p"), col=1,lty=c(1),pch=c(20),cex=2, lwd=5,...)
											#panel.bwplot(x,y,type=c("p"), col=1,lty=c(1),pch=c(20),cex=2, lwd=5,...)
											for(p in 1:length(covrel)){
												concov<-NULL
												color <- 1
												#Get continuous covariates
												for(q in xpdb@Prefs@Xvardef$covariates){
													if(regexpr(q,covrel[p])[1]>0){
														concov <- q
													}#end if
												}#end for											

												for(q in 1:length(levels(cov.data[,xpdb@Prefs@Xvardef$covariates[n]]))){
													covrelcon <- gsub("x",levels(cov.data[,xpdb@Prefs@Xvardef$covariates[n]])[q],covrel[p])	
													covrelcon <- paste(covrelcon,"+0*x",sep="")
													if(!is.null(concov)) covrelcon <- gsub(concov,eval(parse(text="median(cov.data[cov.data[,xpdb@Prefs@Xvardef$covariates[n]]==levels(cov.data[,xpdb@Prefs@Xvardef$covariates[n]])[q],concov],na.rm=TRUE)")),covrelcon)	
													temp <- paste("log10(",covrelcon,")",sep="")
													eval(parse(text=paste("panel.curve(expr=",temp,",from=",q-0.3,",to=",q+0.3,",col=info$line.col[color],lty=info$lty[color],lwd=8)",sep="")))
												}
												color <- color + 1
												if(color>length(info$col)) color <- 1
											}
											for(s in 1:length(levels(cov.data[,xpdb@Prefs@Xvardef$covariates[n]]))){
												panel.text(x=s,y=log10(min(cov.data[,xpdb@Prefs@Xvardef$parms[i]],na.rm=TRUE)),paste("(N=",nrow(cov.data[cov.data[,xpdb@Prefs@Xvardef$covariates[n]]==unique(sort(as.character(cov.data[,xpdb@Prefs@Xvardef$covariates[n]])))[s],]),")",sep=""))
											}
										},
										par.strip.text=list(cex=1.5)
		      							)#end of xyplot
								}else{
									plotCov <- bwplot(
                       							eval(parse(text=paste(xpdb@Prefs@Xvardef$parms[i],"~",xpdb@Prefs@Xvardef$covariates[n],sep=""))),
			      						data=cov.data,
										horizontal=FALSE,
										layout=c(1,1),
										aspect=1/1,
		      							#groups=eval(parse(text=info$group.by)),
										xlab=list(xlb,cex=1.5),
										ylab=list(ylb,cex=1.5),
										strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
										scales=list(cex=1.5,x=list(labels=CatValue,rot=45)),
										#panel.data=covrel,
										panel = function(x,y,...) {
											panel.bwplot(x,y,type=c("p"), col=1,lty=c(1),pch=c(20),cex=2, lwd=5,...)
											#panel.bwplot(x,y,type=c("p"), col=1,lty=c(1),pch=c(20),cex=2, lwd=5,...)
											for(p in 1:length(covrel)){
												concov<-NULL
												color <- 1
												#Get continuous covariates
												for(q in xpdb@Prefs@Xvardef$covariates){
													if(regexpr(q,covrel[p])[1]>0){
														concov <- q
													}#end if
												}#end for											
											
												for(q in 1:length(levels(cov.data[,xpdb@Prefs@Xvardef$covariates[n]]))){
													covrelcon <- gsub("x",levels(cov.data[,xpdb@Prefs@Xvardef$covariates[n]])[q],covrel[p])	
													covrelcon <- paste(covrelcon,"+0*x",sep="")
													if(!is.null(concov)) covrelcon <- gsub(concov,eval(parse(text="median(cov.data[cov.data[,xpdb@Prefs@Xvardef$covariates[n]]==levels(cov.data[,xpdb@Prefs@Xvardef$covariates[n]])[q],concov],na.rm=TRUE)")),covrelcon)	
													eval(parse(text=paste("panel.curve(expr=",covrelcon,",from=",q-0.3,",to=",q+0.3,",col=info$line.col[color],lty=info$lty[color],lwd=8)",sep="")))
												}
												color <- color + 1
												if(color>length(info$col)) color <- 1
											}
											for(s in 1:length(levels(cov.data[,xpdb@Prefs@Xvardef$covariates[n]]))){
												panel.text(x=s,y=min(cov.data[,xpdb@Prefs@Xvardef$parms[i]],na.rm=TRUE),paste("(N=",nrow(cov.data[cov.data[,xpdb@Prefs@Xvardef$covariates[n]]==unique(sort(as.character(cov.data[,xpdb@Prefs@Xvardef$covariates[n]])))[s],]),")",sep=""))
											}
										},
										par.strip.text=list(cex=1.5)
		      							)#end of xyplot
								}
							}

      						if(!is.na(info$output)){
								printGraph(paste("plotCov",nCov,sep=""),info$device)
           							print(plotCov)
            						dev.off()
								nCov <- nCov + 1 

								printGraph(paste(xpdb@Prefs@Xvardef$parms[i],".vs.",xpdb@Prefs@Xvardef$covariates[n],".Cov",sep=""),info$device)
      	      					print(plotCov)
            						dev.off() 
							}else{
								print(plotCov)
							}

							if(info$report==TRUE){
								wdGoToBookmark("blankCov2")
								wdPlot(plotCov,width=6,height=6)
							}

							if(!is.null(covrel)){
								for(p in 1:length(covrel)){
											forName[nForest] <- paste(xpdb@Prefs@Xvardef$parms[i],"~",xpdb@Prefs@Xvardef$covariates[n],if(p>1)paste(" (Eq",p,")",sep=""),sep="")
											forCov[nForest] <- xpdb@Prefs@Xvardef$covariates[n]
											forParm[nForest] <- xpdb@Prefs@Xvardef$parms[i]

											isCat[nForest] <- TRUE
											concov<-NULL
											#Get continuous covariates
											for(q in xpdb@Prefs@Xvardef$covariates){
												if(regexpr(q,covrel[p])[1]>0){
													concov <- q
												}#end if
											}#end for											
											
											for(q in 1:length(levels(cov.data[,xpdb@Prefs@Xvardef$covariates[n]]))){
												covrelcon <- gsub("x",levels(cov.data[,xpdb@Prefs@Xvardef$covariates[n]])[q],covrel[p])	
												if(!is.null(concov)) covrelcon <- gsub(concov,eval(parse(text="median(cov.data[,concov],na.rm=TRUE)")),covrelcon)	
												if(levels(cov.data[,xpdb@Prefs@Xvardef$covariates[n]])[q]=="1"){
													lCov[nForest] <- eval(parse(text=covrelcon))
													hCov[nForest] <- eval(parse(text=covrelcon))
													mCov1[nForest] <- eval(parse(text=covrelcon))
													lowParm[nForest] <- signif(eval(parse(text=covrelcon)),digits=info$digits)
													highParm[nForest] <- signif(eval(parse(text=covrelcon)),digits=info$digits)
													lowCov[nForest] <- CatValue[q]
													highCov[nForest] <- CatValue[q]
												}else{
													mCov2[nForest] <- eval(parse(text=covrelcon))
													refName[nForest] <- CatValue[q]
													refValue[nForest] <- signif(eval(parse(text=covrelcon)),digits=info$digits)
												}
											}

											nForest <- nForest + 1
								}
							}
						}#end if
					}#end if

					####ETA plot from info$eta.plot
					if(names(info$eta.plot)[1]!="noetaplot"){
						if(!is.na(info$output)){
							if(xpdb@Prefs@Xvardef$parms[i]%in%names(info$eta.plot)){
								rans <- info$eta.plot[xpdb@Prefs@Xvardef$parms[i]==names(info$eta.plot)][[1]]
								if(xpdb@Prefs@Xvardef$covariates[n]%in%rans){
									printGraph(paste("plotETA",nETA,sep=""),info$device)
           								print(plot99)
           								dev.off()
									nETA <- nETA + 1 
								}#end if
							}#end if
						}
					}
				
				}#end if factor	

 				if(!is.na(info$output)){
					printGraph(paste(xpdb@Prefs@Xvardef$parms[i],".vs.",xpdb@Prefs@Xvardef$covariates[n],sep=""),info$device)
            			print(plot99)
            			dev.off() 
				}else{
					print(plot99)
				}

				if(info$report==TRUE){
					if(xpdb@Prefs@Xvardef$parms[i]%in%xpdb@Prefs@Xvardef$ranpar){
						wdGoToBookmark("blankETA")
						wdPlot(plot99,width=6,height=6)
					}else{
						wdGoToBookmark("blankParm")
						wdPlot(plot99,width=6,height=6)
					}
				}

			}#end for n loop
  		}#end if length loop
		
		if(names(info$eta.plot)[1]!="noetaplot"){
			if(!is.na(info$output)){
				if(xpdb@Prefs@Xvardef$parms[i]%in%names(info$eta.plot)){
					while(nETA<(plotETA*6+1)){
						printGraph(paste("plotETA",nETA,sep=""),info$device)
	      	    		  	dev.off() 
						nETA <- nETA + 1
					}
					plotETA <- plotETA+1
				}
			}
		}
 	}#end for i loop

	if(names(info$eta.plot)[1]!="noetaplot"){
		ETAnames <- data.frame(ETAnames=rep(NA,4))
		ETAnames$ETAnames[1:length(names(info$eta.plot))] <- names(info$eta.plot)
	}

	if(!is.na(info$output)){
		if(names(info$eta.plot)[1]!="noetaplot"){
			write.table(ETAnames,file="ETAnames.csv", sep= ",",col.names = TRUE, row.names=FALSE,quote=FALSE)
		}
		while(nCov<7){
			printGraph(paste("plotCov",nCov,sep=""),info$device)
			dev.off() 
			nCov <- nCov + 1
		}

		while(nQuant<7){
			printGraph(paste("plotCovQuantile",nQuant,sep=""),info$device)
			dev.off() 
			nQuant <- nQuant + 1
		}

		while(nETA<25){
			printGraph(paste("plotETA",nETA,sep=""),info$device)
          	  	dev.off() 
			nETA <- nETA + 1
		}	
	}

	if(!is.null(forName)){
		forest <- data.frame(name=as.character(forName),
						Cov=as.character(forCov),
						Parm=as.character(forParm),
						isCat=isCat,
						ref=as.character(refName),
						value=refValue,
						lowCov=as.character(lowCov),
						highCov=as.character(highCov),
						lowParm=lowParm,
						highParm=highParm,
						lower=100*lCov/mCov2,
						median=100*mCov1/mCov2,
						upper=100*hCov/mCov2)

		forest$name <- gsub("\\(","\n\\(",as.character(forest$name))
		#order forest plot with increasing covariate effect, continuous then categorical
		forest <- forest[order(forest$upper),]
		forest <- forest[c(which(forest$isCat),which(!forest$isCat)),]

		forest$number <- 1:nrow(forest)

		plotForest <- Dotplot(number~Cbind(median,lower,upper),data=forest,
					xlim=c(ifelse(min(forest$lower,na.rm=TRUE)<min(forest$upper,na.rm=TRUE),min(forest$lower,na.rm=TRUE)-30,min(forest$upper,na.rm=TRUE)-30),ifelse(max(forest$lower,na.rm=TRUE)<max(forest$upper,na.rm=TRUE),max(forest$upper,na.rm=TRUE)+20,max(forest$lower,na.rm=TRUE)+20)),
					ylim=c(0.5,nrow(forest)+0.5),
					ylab="",
					xlab=list("Covariate effect relative to typical value (%)       ",cex=1.5),
					scales=list(cex=1.5,y=list(at=c(1:nrow(forest)),labels=forest$name)),
					panel = function(x,y,...) {
						panel.abline(v=c(80,100,125),col="black",lty=c(4,1,4),lwd=c(1,1,1))
						panel.Dotplot(x,y,lwd=5,col="black",cex=1,...)
						if(nrow(forest)<=2) forcex=1
						if(nrow(forest)>=3) forcex=0.8
						if(nrow(forest)>=5) forcex=0.5

						for(i in 1:nrow(forest)){
							#panel.text(x=min(forest$lower)+5,y=i+0.2,paste("Ref: ",as.character(forest$ref[i]),sep=""),cex=forcex)
							panel.text(x=102,y=i+0.2,as.character(forest$ref[i]),cex=forcex)
							panel.text(x=102,y=i+0.1,as.character(forest$value[i]),cex=forcex)
							panel.text(x=ifelse(min(forest$lower,na.rm=TRUE)<min(forest$upper,na.rm=TRUE),min(forest$lower,na.rm=TRUE)-20,min(forest$upper,na.rm=TRUE)-20),y=i+0.2,paste(as.character(forest$Cov[i]),":",sep=""),cex=forcex)
							panel.text(x=ifelse(min(forest$lower,na.rm=TRUE)<min(forest$upper,na.rm=TRUE),min(forest$lower,na.rm=TRUE)-20,min(forest$upper,na.rm=TRUE)-20),y=i+0.1,paste(as.character(forest$Parm[i]),":",sep=""),cex=forcex)

							if(forest$isCat[i]){
								panel.text(x=forest$median[i],y=i+0.2,as.character(forest$lowCov[i]),cex=forcex)
								panel.text(x=forest$median[i],y=i+0.1,as.character(forest$lowParm[i]),cex=forcex)
							}else{
								if(forest$lower[i]==forest$upper[i]){
									panel.text(x=forest$median[i]-10,y=i+0.2,as.character(forest$lowCov[i]),cex=forcex)
									#panel.text(x=85,y=i+0.1,as.character(forest$lowParm[i]),cex=forcex)
									panel.text(x=forest$median[i]+15,y=i+0.2,as.character(forest$highCov[i]),cex=forcex)
									#panel.text(x=120,y=i+0.1,as.character(forest$highParm[i]),cex=forcex)
								}else{
									panel.text(x=min(forest$lower[i])+2,y=i+0.2,as.character(forest$lowCov[i]),cex=forcex)
									panel.text(x=min(forest$lower[i])+2,y=i+0.1,as.character(forest$lowParm[i]),cex=forcex)
									panel.text(x=max(forest$upper[i]),y=i+0.2,as.character(forest$highCov[i]),cex=forcex)
									panel.text(x=max(forest$upper[i]),y=i+0.1,as.character(forest$highParm[i]),cex=forcex)	
								}
							}
						}
					}
					)

      	if(!is.na(info$output)){
			printGraph("ForestPlot",info$device)
     	 		setTrellis(strip.blank=TRUE, lty.dot.line=0, lwd.dot.line=0)
			temp <- trellis.par.get("plot.line")
			temp$lwd <- 10
			temp$col <- 1
			trellis.par.set("plot.line",temp)
			print(plotForest)
      		dev.off() 

			while(dev.cur()!=1) dev.off()

			write.table(forest,file="forestplot.csv", sep= ",",col.names = TRUE, row.names=FALSE,quote=FALSE)
		}else{
			setTrellis(lty.dot.line=0, lwd.dot.line=0)
			temp <- trellis.par.get("plot.line")
			temp$lwd <- 10
			temp$col <- 1
			trellis.par.set("plot.line",temp)
			print(plotForest)

			cat("\nSummary of Covariate Effects on Parameters (Forest Plot)\n")
			print(forest[,c(2,3,5:13)])
		}
		if(info$report==TRUE){
			wdGoToBookmark("blankForest")
			wdPlot(plotForest,width=6,height=6)
		}
	}
	
	assign("ER",ER,envir=.GlobalEnv)
	rm(ExposureResponse,envir=.GlobalEnv)

	if(info$report==TRUE){
		wdSave(paste("popPKrun",info$run.no,sep=""))
	}

}#end of Covariates function