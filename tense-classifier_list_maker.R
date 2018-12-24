
ROOT = "C:\\Users\\ryanc\\Documents\\RyanCallihan_Fabian\\Recording_Morphology_Experiment"
setwd(ROOT)
load("words_df_all.rda")

stim_lists = c("a", "b", "c", "d", "e")
setwd("stimuli_list")

for (stim in stim_lists) {
	writeLines('file,stimuli,tense', 
		file(paste('tense_classifier_stimuli_list_', stim, '.csv', sep='')))
}

lines = array(dim=c(0,3))

unique_words = unique(words_df_all$stem[which(words_df_all$is_regular)])
uniq_count = 1
all_data = array(dim=c(0,3))
for (word in unique_words) {

	if (word != "#") { 
		
		print(paste("Working on:", word, "- word", uniq_count, "of",
			length(unique_words)))

		# Make tmp DF with all rows of stem
		word_df = words_df_all[which(words_df_all$stem == word),]

		names = array(dim=c(0,3))
		if ("ed" %in% word_df$morpheme) {
			names = rbind(names, c(paste(word, '_ed_stem.wav', sep=''), word, "ed"))
		}

		if ("s" %in% word_df$morpheme) {
			names = rbind(names, c(paste(word, '_s_stem.wav', sep=''), word, "s"))
		}

		if ("ing" %in% word_df$morpheme) {
			names = rbind(names, c(paste(word, '_ing_stem.wav', sep=''), word, "ing"))
		}
		if ("base" %in% word_df$morpheme) {
			names = rbind(names, c(paste(word, '_base.wav', sep=''), word, "base"))
		}


		# if (length(word_df$stem) >= 2) {
		# 	list_idx = uniq_count%%length(stim_lists) + 1
		# 	names_idx = 1		
		# 	for (i in list_idx:(list_idx+(length(stim_lists)-1 ))) {
		# 		names_idx = names_idx%%nrow(names) + 1
		# 		list_idx = (i %% length(stim_lists) ) + 1
				

		# 		lines = rbind(lines, c(paste("stimuli/", names[names_idx, 1], ",", 
		# 				names[names_idx, 2], ",", names[names_idx, 3], sep=''), stim_lists[list_idx], word))
		# 		print(names[names_idx,1])

		# 		names_idx = names_idx + 1

			
		# 	}
		# }
		

		uniq_count = uniq_count + 1

		all_data = rbind(all_data, names)
	}
}



# #################### s bullshit #########################
# s_lines = lines[which(grepl("_s_",lines[,1], fixed=T)),]

# s_lines[,2] = NA

# s_lines = unique(s_lines)

# for (i in 1:nrow(s_lines)) {

# 	idx = (i%%length(stim_lists))

# 	for (j in idx:(idx+4))

# 		j = j %% length(stim_lists) + 1

# 		s_lines[i,2] = stim_lists[j]
# }


# lines = lines[-which(grepl("_s_",lines[,1], fixed=T)),]

# lines = rbind(lines, s_lines)

# ############################ base bullshit ##############################


# base_lines = lines[which(grepl("_base.",lines[,1], fixed=T)),]

# base_lines[,2] = NA

# base_lines = unique(base_lines)

# for (i in 1:nrow(base_lines)) {

# 	idx = (i%%length(stim_lists))

# 	for (j in idx:(idx+4))

# 		j = j %% length(stim_lists) + 1

# 		base_lines[i,2] = stim_lists[j]
# }


# lines = lines[-which(grepl("_base.",lines[,1], fixed=T)),]

# lines = rbind(lines, base_lines)

# ############################ ed bullshit #############################

# ed_lines = array(dim=c(0,3))

# ed_lines_uniq = lines[which(grepl("_ed_",lines[,1], fixed=T)),]

# ed_lines_uniq[,2] = NA

# ed_lines_uniq = unique(ed_lines_uniq)

split_data = data.frame(matrix(nrow = 0, ncol = 4))
colnames(split_data) = c("file", "stimuli", "tense", "list")
#################### s bullshit #########################
s_lines = all_data[which(all_data[,3] == "s"),]

s_lines = cbind(s_lines, NA)

s_lines = unique(s_lines)

# for (i in 1:nrow(s_lines)) {

# 	idx = (i%%length(stim_lists))

# 	for (j in idx:(idx+4))

# 		j = j %% length(stim_lists) + 1

# 		s_lines[i,4] = stim_lists[j]
# }

split_data = rbind(split_data, s_lines)

############################ base bullshit ##############################


base_lines = all_data[which(all_data[,3] == "base"),]

base_lines = cbind(base_lines, NA)

base_lines = unique(base_lines)

# for (i in 1:nrow(base_lines)) {

# 	idx = (i%%length(stim_lists))

# 	for (j in idx:(idx+4))

# 		j = j %% length(stim_lists) + 1

# 		base_lines[i,4] = stim_lists[j]
# }

split_data = rbind(split_data, base_lines)

############################ ed bullshit #############################

ed_lines = all_data[which(all_data[,3] == "ed"),]

ed_lines = cbind(ed_lines, NA)

ed_lines = unique(ed_lines)

# for (i in 1:nrow(ed_lines)) {

# 	idx = (i%%length(stim_lists))

# 	for (j in idx:(idx+4))

# 		j = j %% length(stim_lists) + 1

# 		ed_lines[i,4] = stim_lists[j]
# }

split_data = rbind(split_data, ed_lines)


###########################ing bulshit ##########################################

ing_lines = all_data[which(all_data[,3] == "ing"),]

ing_lines = cbind(ing_lines, NA)

ing_lines = unique(ing_lines)

# for (i in 1:nrow(ing_lines)) {

# 	idx = (i%%length(stim_lists))

# 	for (j in idx:(idx+4))

# 		j = j %% length(stim_lists) + 1

# 		ing_lines[i,4] = stim_lists[j]
# }

split_data = rbind(split_data, ing_lines)

##########################################################################
split_data = 
split_data = split_data[sample(nrow(split_data)),]

for (line_idx in 1:nrow(split_data)) {

	word_occurance = length(split_data[which(split_data[,2] == split_data[line_idx,2] & split_data[,4] == split_data[line_idx,4]),1]) 

	if (word_occurance > 1) {
		print(paste("THE WORD:", split_data[line_idx,1], "OCCURS IN LIST:", 
			split_data[line_idx,4], word_occurance, "TIMES"))
	} 

	else {
		write(paste("stimuli/", split_data[line_idx, 1], ",", split_data[names_idx, 2], ",", split_data[names_idx, 3], sep=''), 
			file=paste('tense_classifier_stimuli_list_', split_data[line_idx,4], '.csv', sep=''), append= TRUE)
	}
}

list_len = nrow(all_data)
ratio_ed = nrow(all_data[which(all_data[,3] == "ed"),])
ratio_ing = nrow(all_data[which(all_data[,3] == "ing"),])
ratio_s = nrow(all_data[which(all_data[,3] == "s"),])
ratio_base = nrow(all_data[which(all_data[,3] == "base"),])
print(paste("ALL List", i, "contains:"))
print(paste(list_len, "words ALL"))
print(paste(ratio_ed, "words with ed ALL"))
print(paste(ratio_s, "words with s ALL"))
print(paste(ratio_ing, "words with ing ALL"))
print(paste(ratio_base, "base words ALL"))
print("")

for (i in stim_lists) {
	list_len = nrow(all_data[all_data(all_data[,3] == i),])
	ratio_ed = nrow(all_data[which(all_data[,3] == i & all_data[,3] == "ed"),])
	ratio_ing = nrow(all_data[which(all_data[,3] == i & grepl("_ing_",lines[,1], fixed=T)),])
	ratio_s = nrow(all_data[which(all_data[,3] == i & grepl("_s_",lines[,1], fixed=T)),])
	ratio_base = nrow(all_data[which(liall_datanes[,3] == i & grepl("_base.",lines[,1], fixed=T)),])
	print(paste("List", i, "contains:"))
	print(paste(list_len, "words"))
	print(paste(ratio_ed, "words with ed"))
	print(paste(ratio_s, "words with s"))
	print(paste(ratio_ing, "words with ing"))
	print(paste(ratio_base, "base words"))
	print("")
}

setwd("..")
closeAllConnections()

print("Finished")


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

