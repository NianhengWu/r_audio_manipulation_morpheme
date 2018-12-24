
ROOT = "C:\\Users\\ryanc\\Documents\\RyanCallihan_Fabian\\Recording_Morphology_Experiment"
setwd(ROOT)
load("words_df_all.rda")
# base_names = list.files(paste(ROOT, "/bases", sep=""))
# morphemes_names = list.files(paste(ROOT, "/morphemes", sep=""))
# irregulars_names = list.files(paste(ROOT, "/irregulars", sep=""))
# originals_names = list.files(paste(ROOT, "/originals", sep=""))
# splices_names = list.files(paste(ROOT, "/splices", sep=""))
# stems_names = list.files(paste(ROOT, "/stems", sep=""))

stim_lists = c("a", "b", "c", "d")
setwd("stim_list")

for (stim in stim_lists) {
	writeLines('stimuli, type', 
		file(paste('stimuli_list_', stim, '.txt', sep='')))
}

unique_words = unique(words_df_all$stem[which(words_df_all$is_regular)])
uniq_count = 1
for (word in unique_words) {
	print(paste("Working on:", word, "- word", uniq_count, "of",
		length(unique_words)))
	# Make tmp DF with all rows of stem
	word_df = words_df_all[which(words_df_all$stem == word & 
		(words_df_all$morpheme == 's' | 
		words_df_all$morpheme == 'ed')),]

	names = c()
	if ("ed" %in% word_df$morpheme) {
		org = paste(word, 'ed_original.wav', sep='')
		spl = paste(word, 'ed_splice.wav', sep='')
		names = append(names, c(org, spl))
	}

	if ("s" %in% word_df$morpheme) {
		org = paste(word, 's_original.wav', sep='')
		spl = paste(word, 's_splice.wav', sep='')
		names = append(names, c(org, spl))
	}

	if (length(word_df$stem) >= 2) {
		start_idx = 1
		for (i in x:(x+(length(stim_lists)-1 ))) {
			idx = start_idx%%length(names)
			if (idx == 0) {idx = length(names)}
			
			list_idx = (i %% (length(stim_lists) )) + 1

			print(paste("list:", stim_lists[list_idx], "- name:", 
				names[idx]))

			write(paste(names[idx], ",stimuli", sep=''), 
				file=paste('stimuli_list_', stim_lists[list_idx], '.txt', sep=''), 
				append=TRUE)


			start_idx = start_idx + 1
		}
	}
	

	uniq_count = uniq_count + 1
}







# ###################################
## Cycles through list. Start idx++
# for (i in 1:20) {
# 	x = (i%%4)
# 	print(paste("x", x))
# 	for (j in x:(x+3)) {
# 		j = (j %% 4) + 1
# 		print(j)
# 	}
# }
#####################################

