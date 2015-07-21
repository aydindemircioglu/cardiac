#
# Cardiac
#		(C) 2015, by Aydin Demircioglu
#
#		ACEFScore.R
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


#' Compute ACEF Score
#'
#' Compute the ACEF Score by the formula from XY
#'
#' @param	df		data frame, must contain age, ejectionFraction as rows
#'	
#' @export
ACEFScore <- function (df = data.frame(), mapping = mapping() )
{
	# test data frame
	
	# test mapping
	
  # mapping access probably wrong, but for now it works

	ACEFScore = c()
	for (x in seq(1, nrow(df))) {
		age = df[x, as.character(mapping$age)]
		ef = df[x, as.character(mapping$ejectionFraction)]
		score = age/ef
		if (df[x, as.character(mapping$preOpCreatinine)] > 2.0) {
			score = score + 1
		}
		ACEFScore = c(ACEFScore, score)
	}
	return (ACEFScore)
}


#' @export
mapping <- function(...) {
  mapping <- structure(as.list(match.call()[-1]), class="uneval")
}
