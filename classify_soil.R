classify_soil <- function(sand, silt, clay) {
	if (is.na(sand) | is.na(silt) | is.na(clay)) {
		return("NA")
	}

	else if (sand + silt + clay != 100) {
		return("NA")
	}

	# Sands
	else if (sand >= 85 && (silt + 1.5 * clay) <= 15) {
		return("Sand")
	}

	# Loamy Sands
	else if (sand >= 70 && sand < 85 && (silt + 2 * clay) <= 30) {
		return("Loamy Sand")
	}

	# Sandy Loams
	else if (clay <= 20 && sand >= 43 && sand < 85 && (silt + 2 * clay) > 30) {
		return("Sandy Loam")
	}

	# Loam
	else if (clay >= 7 && clay <= 27 && silt >= 28 && silt <= 50 && sand < 52) {
		return("Loam")
	}

	# Silt Loam
	else if ((silt >= 50 && clay >= 12 && clay <= 27) || (silt >= 50 && silt < 80 && clay < 12)) {
		return("Silt Loam")
	}

	# Silt
	else if (silt >= 80 && clay < 12) {
		return("Silt")
	}

	# Sandy Clay Loam
	else if (clay >= 20 && clay < 35 && silt < 28 && sand >= 45) {
		return("Sandy Clay Loam")
	}

	# Clay Loam
	else if (clay >= 27 && clay < 40 && sand >= 20 && sand <= 45) {
		return("Clay Loam")
	}

	# Silty Clay Loam
	else if (clay >= 27 && clay < 40 && sand < 20) {
		return("Silty Clay Loam")
	}

	# Sandy Clay
	else if (clay >= 35 && sand >= 45) {
		return("Sandy Clay")
	}

	# Silty Clay
	else if (clay >= 40 && silt >= 40) {
		return("Silty Clay")
	}

	# Clay
	else if (clay >= 40 && sand < 45 && silt < 40) {
		return("Clay")
	}

	else{
		return("NA")
	}
}

# Apply the function to your data
soil$sc2 <- apply(soil[, c('SAND', 'SILT', 'CLAY')], 1, function(row) classify_soil(row[1], row[2], row[3])) 
