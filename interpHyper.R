# this is based on the source code to e1071::interpolate. 
# Several mods made either because I think they are cleaner or because
# I think they are faster.
#
#  Arguments:
#	x-- a matrix, each row represention coords of  a point. If a single point
#		is submitted as a vector, that's converted to a 1xN matrix
#	a -- the hypercube of data values.  'a' is expected to have its linear
#		dimensions encoded into the dimnames parameter.  If there are no dimnames,
#		a default of 0:(dim(a)[j]) is assigned to each dimension. Note that
#		the dimnames do not need to be linearly spaced, but linear interpolation
#		within the bounding hypercube is performed (when "linear" method selected)
#	method  -- 'linear' is what you'd expect, 'constant' selects the "lower-left"
#		corner of the bounding hypercube; 'spline' is not yet implemented.
#
# E.g. for a 5x5 matrix perhaps the rows are 0.25 cm apart but the 
# cols are 0.5 cm apart, so adims=list( (0:4)*.25, (0:4)*.5)
#
# let's leave the defaults same as for e1071 to 
# reduce panic and horror if people switch from that to this version.

interpHyper <- function(x, a, adims = lapply(dimnames(a), as.numeric), method = c("linear", 
	"constant")) {
# this turns a single point into class 'matrix' to simplify later code
	if (is.vector(x)) 
		x <- matrix(x, ncol = length(x))
	# matrices are arrays, so ok
	if (!is.array(a)) 
		stop(paste(deparse(substitute(a)), " is not an array"))
	ad <- length(dim(a))
# check for existence of dimnames.. this is dev code so far
# but note that if dimnames()==NULL, adims will be an empty list
	if (!length(adims)) {
		#build integer lists. It's up to user to make "x" values fit
		adims <- sapply(1:ad, function(j) 0:dim(a)[j])
	}
#Spline? Some future revision, Consider tying into DiceKriging package,
# at least for rank-3 arrays.
	methodnames <- c("linear", "constant", "spline")
	method <- pmatch(method[1], methodnames)
	if (is.na(method)) 
		stop("invalid interpolation method")
	if (any(unlist(lapply(adims, diff)) < 0)) 
		stop("dimensions of array 'a' not ordered")
	#test for non-extrapolative x values here.
	for (k in 1:ad) if (any(x[, k] < min(adims[[k]])) | any(x[, k] > (max(adims[[k]])))) 
		stop(paste("data in col ", k, " outside dimrange"))

	retval <- rep(0, nrow(x))
#bincount is carbon copy of e1071:bincombinations
# renamed so as not to conflict w/ e1071 func
bincount <- function(p) {
		retval <- matrix(0, nrow = 2^p, ncol = p)
		# possibly vectorizable but not in any simple way
		for (n in 1:p) {
			retval[, n] <- rep(c(rep(0, (2^p/2^n)), rep(1, (2^p/2^n))), length = 2^p)
		}
		retval
}
	bincombi <- bincount(ad)
# another helper function
convexcoeff <- function(x, y) {
		ok <- y > 0
		x[ok] <- y[ok] - x[ok]
		x
}
	for (n in 1:nrow(x)) {
		leftidx <- rep(0, ad)
		xabstand <- rep(0, ad)
		aabstand <- rep(0, ad)
		for (k in 1:ad) {
			leftidx[k] <- max(which(adims[[k]] <= x[n, k]))
			# in my neverending quest to remove "if" 
			leftidx[k] <- leftidx[k] - (leftidx[k] == length(adims[[k]]))
			xabstand[k] <- x[n, k] - adims[[k]][leftidx[k]]
			aabstand[k] <- adims[[k]][leftidx[k] + 1] - adims[[k]][leftidx[k]]
		}
		coefs <- list()
		# goto switch; convert to char to allow default
		switch(as.character(method), 
			`1` = {
				for (k in 1:(2^ad)) {
				retval[n] <- retval[n] + element(a, leftidx + bincombi[k, ]) * prod((aabstand - convexcoeff(xabstand, aabstand * bincombi[k, ]))/aabstand)
				}
			}, 
			`2` = {
			retval[n] <- element(a, leftidx)
			},
		 	stop(paste("method  ", methodnames[as.numeric(method)], " is unimplemented"))
		 	) #end of switch
	} #end of loop over rows of x
# If no input rownames, no output rownames.
	names(retval) <- rownames(x)
	return(invisible(retval))
}


