#
# Cardiac
#		(C) 2015, by Aydin Demircioglu
#
#		STSScore.R
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


STSScore <- function (df = data.frame(), logistic = TRUE)
{
}


STSScore <- function (df = data.frame())
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
acef = ACEFScore(aceftest)

