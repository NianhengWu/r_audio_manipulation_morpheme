zero_x = function(sound) {

	word = sound@left
	start_samp = word[1]
	end_samp = word[length(word)]
	start_idx = 1
	end_idx = length(word)

	# Zero Crossing works
	# If start is postive
	if (start_samp >= 0) {

		while (start_samp >= 0) {
			start_idx = start_idx + 1
			start_samp = word[start_idx]
		}
	} else if (start_samp < 0) { # else it is negative
		while (start_samp < 0) {
			start_idx = start_idx + 1
			start_samp = word[start_idx]
		}
	} 

	# If end is postive
	if (end_samp >= 0) {

		while (end_samp >= 0) {
			end_idx = end_idx - 1
			end_samp = word[end_idx]
		}
	} else if (end_samp < 0) { # else it is negative
		while (end_samp < 0) {
			end_idx = end_idx - 1
			end_samp = word[end_idx]
		}
	} 



	# for(i in 1:200){
	# 	if ((word[i] * -1 > 0 && start_samp > 0) || (word[i] * -1 < 0 && start_samp < 0)) {
	# 		start_idx = i
	# 		break
	# 	}  
	# 	# if (word[i] <= 0 && start_samp >= 0) {
	# 	# 	print("Start samp is Positive")
	# 	# 	start_idx = i
	# 	# 	break
	# 	# }
	# 	# else if (word[i] >= 0 && start_samp <= 0) {
	# 	# 	print("Start samp is Negative")
	# 	# 	start_idx = i
	# 	# 	break
	# 	# }   
	# }


	# for (i in length(word):(length(word) - 200)) {
	# 	if ((word[i] * -1 > 0 && end_samp > 0) || (word[i] * -1 < 0 && end_samp < 0)) {
	# 		end_idx = i
	# 		break
	# 	} 
	# 	# if (word[i] <= 0 && end_samp >= 0) {
	# 	# 	print("End samp is Positive")
	# 	# 	end_idx = i
	# 	# 	break
	# 	# }
	# 	# else if (word[i] >= 0 && end_samp <= 0) {
	# 	# 	print("End samp is Negative")
	# 	# 	end_idx = i
	# 	# 	break
	# 	# }
	# }

	word = word[start_idx:end_idx]

	sound@left = word

	return(sound)

}