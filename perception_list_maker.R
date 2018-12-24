
ROOT = "C:\\Users\\ryanc\\Documents\\RyanCallihan_Fabian\\Recording_Morphology_Experiment"
setwd(ROOT)
load("words_df_all.rda")

stim_lists = c("a", "b", "c", "d")
setwd("stimuli_list")

for (stim in stim_lists) {
	writeLines('file,stimuli,is_original,is_regular,tense', 
		file(paste('perception_stimuli_list_', stim, '.csv', sep='')))
}

lines = array(dim=c(0,3))


unique_words = unique(words_df_all$stem[which(words_df_all$is_regular)])
unique_words = append(unique_words, unique(words_df_all$stem[which(!words_df_all$is_regular)]))

uniq_count = 1
for (word in unique_words) {

	

	# Make tmp DF with all rows of stem

	print(paste("Working on:", word, "- word", uniq_count, "of",
	length(unique_words)))

	# Check for irregular or not
	if (length(which(words_df_all$stem == word & !words_df_all$is_regular)) != 0 ) {

		if (words_df_all$stem == word & words_df_all$tense != "-base") {

			tense = words_df_all$tense[which(words_df_all$stem == word & !words_df_all$is_regular)]
			print(tense)
			list_idx = uniq_count%%length(stim_lists) + 1
			lines = rbind(lines, c(paste("stimuli/", word, "_irregular.wav,", word, ",T,F,", tense,
				sep=''), stim_lists[list_idx], word))
		}
	}
	else { # Regular
		word_df = words_df_all[which(words_df_all$stem == word & 
		(words_df_all$morpheme == 's' | 
		words_df_all$morpheme == 'ed')),]

		names = array(dim=c(0,5))
		if ("ed" %in% word_df$morpheme) {
			names = rbind(names, c(paste(word, 'ed_original.wav', sep=''), word, "T", "T", "past"))
			names = rbind(names, c(paste(word, 'ed_splice.wav', sep=''), word, "F", "T", "past"))
		}

		if ("s" %in% word_df$morpheme) {
			names = rbind(names, c(paste(word, 's_original.wav', sep=''), word, "T", "T", "present"))
			names = rbind(names, c(paste(word, 's_splice.wav', sep=''), word, "F", "T", "present"))
		}


		if (length(word_df$stem) >= 2) {
			list_idx = uniq_count%%length(stim_lists) + 1 # changes start of list index each time
			names_idx = 1
			for (i in list_idx:(list_idx+(length(stim_lists)-1 ))) { # iterates through list index, but is offset
				names_idx = names_idx%%nrow(names) + 1
				list_idx = (i %% length(stim_lists) ) + 1				
				# Adds lines to array. The first element is a string to write, the second, which list to put it in.
				lines = rbind(lines, c(paste("stimuli/", names[names_idx, 1], ",", 
					names[names_idx, 2], ",", names[names_idx, 3], ",", names[names_idx, 4], ",", 
					names[names_idx, 5], sep=''), stim_lists[list_idx], names[names_idx, 2]))
			}

		}

	}
	

	uniq_count = uniq_count + 1
	
}

lines = lines[sample(nrow(lines)),]


for (line_idx in 1:nrow(lines)) {

	word_occurance = length(lines[which(lines[,3] == lines[line_idx,3] & lines[,2] == lines[line_idx,2]),1]) 

	if (word_occurance > 1) {
		print(paste("THE WORD:", lines[line_idx,3], "OCCURS IN LIST:", 
			lines[line_idx,2], word_occurance, "TIMES"))
	} 

	else {
		write(lines[line_idx,1], 
			file=paste('perception_stimuli_list_', lines[line_idx,2], '.csv', sep=''), append= TRUE)
	}
}

setwd("..")
closeAllConnections()

for (i in stim_lists) {
	list_len = nrow(lines[which(lines[,2] == i),])
	ratio_past = nrow(lines[which(lines[,2] == i & grepl(",past",lines[,1], fixed=T)),])
	ratio_pres = nrow(lines[which(lines[,2] == i & grepl(",present",lines[,1], fixed=T)),])
	ratio_pastpres = nrow(lines[which(lines[,2] == i & grepl(",past/present",lines[,1], fixed=T)),])
	
	print(paste("List", i, "contains:"))
	print(paste(list_len, "words"))
	print(paste(ratio_past, "past"))
	print(paste(ratio_pres, "present"))
	print(paste(ratio_pastpres, "past/present"))

	print("")
}

print("Finished")

# ###################################
# # Cycles through list. Start idx++
# for (i in 1:20) {
# 	x = (i%%4)
# 	print(paste("x", x))
# 	for (j in x:(x+3)) {
# 		j = (j %% 4) + 1
# 		print(j)
# 	}
# }
# ####################################
