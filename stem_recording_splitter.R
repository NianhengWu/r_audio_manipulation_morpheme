library(tuneR)
library(seewave)
ROOT = "C:\\Users\\ryanc\\Documents\\RyanCallihan_Fabian\\Recording_Morphology_Experiment"
setwd(ROOT)
source("zero_x.R")
source("fading.R")

source("../read_praat_boundaries2.R")

freq = 44100

# Textgrids and audio files 
tgs = c("recordings_with_textgrids/regular_mono.TextGrid", "recordings_with_textgrids/regular_2_mono.TextGrid", "recordings_with_textgrids/irregular_1_mono.TextGrid", "recordings_with_textgrids/irregular_2_mono.TextGrid", "recordings_with_textgrids/irregular_3_mono.TextGrid")
# tgs = c("regular_mono.TextGrid")
wavs = c("recordings_with_textgrids/regular_stereo.wav", "recordings_with_textgrids/regular_2_stereo.wav", "recordings_with_textgrids/irregular_1_stereo.wav", "recordings_with_textgrids/irregular_2_stereo.wav", "recordings_with_textgrids/irregular_3_stereo.wav")
# wavs = c("regular_stereo.wav")


ncolnames = c("stem", "morpheme", "is_regular", "is_stem", "start_idx", "end_idx", "tense") # All column names

words_df_all = data.frame(matrix(ncol=length(ncolnames), nrow=0))
colnames(words_df_all) = ncolnames

for(i in 1:length(tgs)) {

	words_df = data.frame(matrix(ncol=length(ncolnames), nrow=0)) # Creates a dataframe to contain all words from single textgrid
	colnames(words_df) = ncolnames

	print(paste("################# Reading", wavs[i], "#######################"))
	
	is_regular = T
	if (i > 2) { is_regular = F} # Checks to see if it is regular or not.

	stems = read_praat_boundaries(tgs[i])
	wav_file = readWave(wavs[i])

	for (word in stems$Word[which(stems$Tier == "word")]) { # Cyles through dataframe, through each word.

		
		if (word != "#") { # If there is a word there

			# tmp vector for single word to append to words_df
			word_tmp = data.frame(matrix(ncol=length(ncolnames), nrow=1)) # Makes a dataframe for a single word stem
			colnames(word_tmp) = ncolnames

			# Adds start time of word
			word_tmp$start_idx = stem_start = stems$s.start[which(stems$Word == 
				word & stems$Tier == "word")]

			# Gets index of the stem alone
			stem_idx = which(stems$s.start == 
				stem_start & stems$Tier == "morpheme")

			# end time of word
			word_tmp$end_idx = stem_end = stems$s.end[stem_idx]
			
			# The actual word
			word_tmp$stem = stem = stems$Word[stem_idx]

			word_tmp$is_regular = is_regular

			word_tmp$is_stem = T # This was working on the stems...

			if (stems$Word[stem_idx+1] != "#") { # Now, if there is a morpheme after the stem!

				# tmp vector for stem to append to words_df
				morph_tmp = data.frame(matrix(ncol=length(ncolnames), nrow=1))
				colnames(morph_tmp) = ncolnames	

				morph_tmp$stem = stem # Adds the same stem as above ^

				morph_tmp$is_stem = F # It is a morpheme, not a stem

				morph_idx = stem_idx+1

				# start time of morph
				morph_tmp$start_idx = morph_start = stems$s.start[morph_idx]

				# end time of morph
				morph_tmp$end_idx = morph_end = stems$s.end[morph_idx]

				# Get tense for words with morphemes

				# Whole morphem
				word_tmp$morpheme = morph_tmp$morpheme = morph = stems$Word[morph_idx]

				# TODO change this
				morph_tmp$is_regular = is_regular

				if (morph_tmp$morpheme == "ed") {
					morph_tmp$tense = word_tmp$tense = "past"
				}
				else {
					morph_tmp$tense = word_tmp$tense = "present"
				}

				words_df = rbind(words_df, morph_tmp)



			} else {
				word_tmp$morpheme = "base"
				if (is_regular == T) {
					word_tmp$tense = "present"
				} else {
					if ((tgs[i] != "recordings_with_textgrids/irregular_3_mono.TextGrid" & stems$Word[which(stems$Tier == "tense" & stems$s.start == stem_start)] != "present") |
					(tgs[i] == "recordings_with_textgrids/irregular_3_mono.TextGrid"))  {
						word_tmp$tense = stems$Word[which(stems$Tier == "tense" & stems$s.start == stem_start)]
					}
					else {
						word_tmp$tense = "present-base"
					}
				}
			}
			
			# appends stem to main df
			words_df = rbind(words_df, word_tmp)


		}

	save(words_df, file=paste("words_df_", i, ".rda", sep=''))
	}
	words_df_all = rbind(words_df_all, words_df)
	print(paste("words_df:", nrow(words_df) ))
	print(paste("words_df_all:", nrow(words_df_all) ))
}



save(words_df_all, file="words_df_all.rda")




