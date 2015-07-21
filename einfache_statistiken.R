# load library
library(aod)
library(ggplot2)
library(BBmisc)



ACEFScore <- function (df = data.frame())
{
	ACEFScore = c()
	for (x in seq(1, nrow(df))) {
		age = df[x,]$age
		ef = df[x,]$ejectionFraction
		score = age/ef
		if (df[x,]$preOpCreatinine > 2.0) {
			score = score + 1
		}
		ACEFScore = c(ACEFScore, score)
	}
	df = cbind(df, ACEFScore)
	
	return (df)
}


aceftest = NULL
aceftest$age = round(rnorm(20, sd = 20, mean = 30) + 35)
aceftest$ejectionFraction = round(rnorm(20, sd = 20, mean = 30) + 35)
aceftest$preOpCreatinine = abs(rnorm(20, sd = 1, mean = 1))
aceftest = as.data.frame(aceftest)
acef = ACEFScore(aceftest)

exit()



# brkdn is a function that attempts to calculate and display means, 
# variances and valid ns for the variable that appears on the left 
# side of the formula.
# It expects the variables on the right side of the formula to be
# factors, or at worst integers with a very small range.
# It returns a list of "dstat" objects (or a list of lists of "dstat"
# objects if there are two breakdown variables, and so on.)
# A "dstat" object is a matrix that looks like:
#                       vname1  vname2  ...
#       Mean            mean1   mean2   ...
#       Variance        var1    var2    ...
#       Valid n         n1      n2      ...

brkdn<-function(formula,dataframe,maxlevels=10) {
 if(!missing(dataframe) && !missing(formula)) {
  bn<-as.character(attr(terms(formula),"variables")[-1])
  nbn<-length(bn)
  cat("\nBreakdown of",bn[1],"by",bn[nbn],"\n")
  if(nbn > 2) {
   # get the factor for this level
   by.factor<-as.factor(dataframe[[bn[nbn]]])
   factor.levels<-levels(by.factor)
   nlevels<-length(factor.levels)
   if(nlevels > maxlevels) {
    nlevels<-maxlevels
    cat("Too many levels - only using first",maxlevels,"\n")
   }
   brkstats<-as.list(rep(0,nlevels))
   names(brkstats)<-factor.levels
   # calculate the mean for this level
   for(i in 1:nlevels) {
    currentdata<-subset(dataframe,by.factor == factor.levels[i])
    cat(paste("\n",bn[1],sep="",collapse=""),"for",bn[nbn],"- level",factor.levels[i],"\n\n")
    gstat<-desc.stat(currentdata[bn[1]],na.rm=TRUE)
    cat("Mean     ",gstat[1],"\n")
    cat("Variance ",gstat[2],"\n")
    cat("n        ",gstat[3],"\n")
    next.formula<-as.formula(paste(paste(bn[1],"~"),paste(bn[2:(nbn-1)],collapse="+")))
    # and call yourself for the next level down
    brkstats[[i]]<-brkdn(next.formula,currentdata)
   }
   class(brkstats)<-"dstat"
   invisible(brkstats)
  }
  else {
   by.factor<-as.factor(dataframe[[bn[2]]])
   factor.levels<-levels(by.factor)
   nlevels<-length(factor.levels)
   if(nlevels > maxlevels) {
    nlevels<-maxlevels
    cat("Too many levels - only using first",maxlevels,"\n")
   }
   gstats<-matrix(NA,ncol=nlevels,nrow=3)
   colnames(gstats)<-factor.levels[1:nlevels]
   rownames(gstats)<-c("Mean","Variance","n")
   # calculate the basic descriptive stats
   if(is.numeric(dataframe[[bn[1]]])) {
    for(i in 1:nlevels) {
     currentdata<-subset(dataframe[[bn[1]]],by.factor == factor.levels[i])
     if(length(currentdata)) gstats[,i]<-desc.stat(currentdata,na.rm=TRUE)
    }
    class(gstats)<-"dstat"
    print(gstats)
   }
   invisible(gstats)
  }
 }
 else cat("Usage: brkdn(formula, dataframe, maxlevels=10)\n")
}

# desc.stat calculates the mean, variance and valid n for a numeric vector
# needed by brkdn()

desc.stat<-function(datavec,na.rm=TRUE) {
	dstat<-c(0,0,0)
	dstat[1]<-mean(datavec,na.rm=na.rm)
	dstat[2]<-var(datavec,na.rm=na.rm)
	dstat[3]<-sum(!is.na(datavec))
	return(dstat)
}



# clean all variables
	rm(list=ls())

# read things
	doc <- read.csv2("../data/23042015.csv", fileEncoding="latin1")
	print(names(doc))

# make sure factors are factors
	doc$Gender = as.factor(doc$Gender)

	
# create both groups konvAK und TAVIdoc
	oldStyle = FALSE
	if (oldStyle == TRUE) {
		gruppeKonventionell = subset(doc, Gruppe == "konventionelle AK")
		gruppeTAVI = subset(doc, Gruppe == "TAVI_TA" | Gruppe == "TF-TAVI")
	} else {
		gruppeKonventionell = subset(doc, Gruppe == "1")
		gruppeTAVI = subset(doc, Gruppe == "2")
	}
	print(nrow(gruppeKonventionell))
	print(nrow(gruppeTAVI))
	
	
# first: descriptive summary
#		factors = frequency/proportion
#		cont var = mean +/- stddev
#		nyha = median
	if (1 == 1) {
	for (x in colnames(doc)) {
		if (x == "NYHApraeOP")
			messagef("NYHA -- %d vs %d", 
				median(gruppeKonventionell$NYHApraeOP),
				median(gruppeTAVI$NYHApraeOP)
				)
		if (is.factor(doc[[x]])) {
			messagef("%s -- %d (%f)\t\t %d (%f)", x, 
				sum(gruppeKonventionell[[x]] == 1),
				sum(as.numeric(gruppeKonventionell[[x]])-1)/length(gruppeKonventionell[[x]])*100,
				sum(gruppeTAVI[[x]] == 1),
				sum(as.numeric(gruppeTAVI[[x]])-1)/length(gruppeTAVI[[x]])*100)
		} else {
			messagef("%s -- %f (+/- %f)\t\t %f (+/- %f)", x, 
				mean(gruppeKonventionell[[x]]), sd(gruppeKonventionell[[x]]),
				mean(gruppeTAVI[[x]]), sd(gruppeTAVI[[x]]))
		}
	}
	}

	
    
# compute summary statistics
	verbose = FALSE
    for (x in c("Alter", "ACEF", "eurolog", "euroadd", "EUROII", "STS", "mort30", 
        # "Tod", 
        "Survivald", "EF", 
        "Groessecm", "Groessem", "Gewicht", "BMI", "Krea_mgdl", "Krea_mmol", "CC", "NYHApraeOP", 
        "paVKpraeOP", "NeurologpraeOP", "pHTNpraeOP", "COPDpraeOP", "DMpraeOP", "KHK", "aHTN", "VHF", 
        "Niereninsuff", "Dialyse")) 
    {
		print(x)
		if (verbose == TRUE) {
			messagef ("\n-- Variable %s", x)
			print (summary(doc[[x]]))
			messagef ("   Varianz: %f", var(doc[[x]]))
			
			messagef ("   Gruppenspezifisch:")
			messagef ("   Konventionell:")
			print (summary(gruppeKonventionell[[x]]))
			messagef ("   Konventionell Varianz: %f", var(gruppeKonventionell[[x]]))

			messagef ("   TAVI:")
			print (summary(gruppeTAVI[[x]]))
			messagef ("   TAVI Varianz: %f", var(gruppeTAVI[[x]]))
			
			tTest = t.test(gruppeKonventionell[[x]], + gruppeTAVI[[x]])
			pWert = tTest[3]$p.value
			meanKonv = tTest[5]$estimate[1]
			meanTAVI = tTest[5]$estimate[2]
			messagef ("   Durchschnittliches %s in konv Gruppe: %f", x, meanKonv )
			messagef ("   Durchschnittliches %s in TAVI Gruppe: %f", x, meanTAVI )
			messagef ("   pWert: %f", pWert)
		} else {
			tTest = t.test(gruppeKonventionell[[x]], + gruppeTAVI[[x]])
			pWert = tTest[3]$p.value
			if (pWert < 0.1) {
				messagef ("%s --  pWert: %f", x, pWert)
			}
		}
    }  

    
    

# read and prepare data
  doc$mort30 <- factor(doc$mort30)

# what to do with NAs
  #doc[is.na(doc)] <- 0

# loop over all variables
  for (i in c('EUROII', 'eurolog', 'EF', 'Gruppe.1', 'paVKpraeOP', 'NYHApraeOP', 'STS', 'Gender', 'Alter', 'Niereninsuff', 
                'COPDpraeOP', 'pHTNpraeOP', 'DMpraeOP')) 
  {
    print (i)

    currentModel <- glm (doc$mort30 ~ doc[,c(i)], family=binomial(logit))
    print (currentModel)
    print (summary(currentModel))
    print (pchisq(currentModel$deviance, currentModel$df.residual))
  
    #  invisible(readline(prompt="Press [enter] to continue"))
    # print (summary(doclogit))
  }


