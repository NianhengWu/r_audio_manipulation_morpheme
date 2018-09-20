fade = function(sound, in_fricative=FALSE, out_fricative=FALSE) {

	# X = rnorm(100) #signal
	# X = base@left
	X = sound@left
	in_samples = round((sound@samp.rate / 100) * 2)
	out_samples = round((sound@samp.rate / 100) * 2)

	if (in_fricative == TRUE) {
		in_samples = round((sound@samp.rate / 100) * 2.5)
	}
	if (out_fricative == TRUE) {
		out_samples = round((sound@samp.rate / 100) * 2.5)
	}

	if (length(X) <= sound@samp.rate){
		in_samples = round(((sound@samp.rate / 100) * 2) / 4)
	}
	fadein = seq(from = 0, to = 1, length = in_samples)
	fadeout = seq(from = 1, to = 0, length = out_samples)

	X[1:in_samples] = X[1:in_samples] * fadein #apply fadein
	X[(length(X)-(out_samples-1)):length(X)] = X[(length(X)-(out_samples-1)):length(X)] * fadeout # apply fade out
	sound@left = X
	sound@stereo = T
	sound@right = sound@left
	return(sound)
	
	# plot(X.or, type = 'l', lwd = 2)
	# lines(X, col = 'red', lty = 2)
	# abline(h = 0, col = 'grey')

}