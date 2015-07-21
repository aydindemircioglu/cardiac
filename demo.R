#!/usr/bin/Rscript  --vanilla 

#
# cardiac
#
#		(C) 2015, by Aydin Demircioglu
# 
# cardiac is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# cardiac is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# Please do not use this software to destroy or spy on people, environment or things.
# All negative use is prohibited.
#


#library (SVMBridge)
	library(devtools)
	load_all (".")

	age = round(rnorm(20, sd = 20, mean = 30) + 35)
	ejectionFraction = round(rnorm(20, sd = 20, mean = 30) + 35)
	preOpCreatinine = abs(rnorm(20, sd = 1, mean = 1))
	aceftest = data.frame(age, ejectionFraction, preOpCreatinine)
	acef = ACEFScore(aceftest)
	print(acef)


	
