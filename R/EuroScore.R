#
# Cardiac
#		(C) 2015, by Aydin Demircioglu
#
#		EuroScore.R
# 
# Cardiac is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Cardiac is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# Please do not use this software to kill, destroy or spy on people, environment or things.
# All negative use is prohibited.
#


EuroScore <- function (df = data.frame(), logistic = TRUE)
{
}


EuroScore2 <- function (df = data.frame(), logistic = TRUE)
{
	# names are more or less 
	betaAge = 0.0666354
	betaGender = 0.3304052
	betaSerum = 0.6521653
	betaExtArteriopathy =  0.6558917
	betaPulmonaryDisease = 0.4931341
	betaNeuroDysfunction = 0.841626
	betaPrevCardSurgory = 1.002625
	betaMyocardialInfarct =0.5460218
	betaLVEF3050 = 0.4191643
	betaLVEFSub30 = 1.094443
	betaPulmonaryPressure = 0.7676924
	betaEndocarditis = 1.101265
	betaUnstableAngina = 0.5677075
	betaEmergencyOp = 0.7127953
	betaCriticalPreopState = 0.9058132
	betaVentricularSeptal = 1.462009
	betaOther = 0.5420364
	betaThoracicSurgery = 1.159787
	betaConstant = -4.789594

	# do some conversion
	
	euroScore = c()
	for (x in seq(1, nrow(df))) {
		X = c(0,1,0,0,1,1)
		beta = c(betaAge, betaGender, betaSerum, betaExtArteriopathy, betaPulmonaryDisease, betaNeuroDysfunction)
		if (logistic == TRUE) {
			predictedMortality = exp (betaConstant + beta%*%X)
			predictedMortality = predictedMortality/(1+predictedMortality)
			euroScore = c(euroScore, predictedMortality)
			print(predictedMortality)
		} else {
			euroScore = c(euroScore, sum(X))
			print("ADDITIVE", predictedMortality)
		}
	}
	df = cbind(df, euroScore)
	
	return (df)
}



aceftest = NULL
aceftest$age = round(rnorm(20, sd = 20, mean = 30) + 35)
aceftest$ejectionFraction = round(rnorm(20, sd = 20, mean = 30) + 35)
aceftest$preOpCreatinine = abs(rnorm(20, sd = 1, mean = 1))
acef = ACEFScore(aceftest)

