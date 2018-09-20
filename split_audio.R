library(tuneR)
library(seewave)
ROOT = "C:\\Users\\ryanc\\Documents\\RyanCallihan_Fabian\\Recording_Morphology_Experiment"
# ROOT = "/media/ryanc/FE00ED8900ED48EB/Dropbox/RyanCallihan_Fabian/Recording_Morphology_Experiment"
setwd(ROOT)
source("zero_x.R")
source("fading.R")
source("../read_praat_boundaries2.R")


# source("C:/Dropbox/RyanCallihan_Fabian/read_praat_boundaries2.R")
fricatives = c('s', 'z', 'f', 'v', 'h')
freq = 44100
load("words_df_1.rda")
w1 = words_df
load("words_df_2.rda")
w2 = words_df
words_df = rbind(w1, w2)
save(words_df, file=paste("words_df_1.rda", sep=''))

dfs = c("words_df_1.rda", "words_df_3.rda", "words_df_4.rda", "words_df_5.rda")
wavs = c("recordings_with_textgrids/regular_stereo.wav","recordings_with_textgrids/irregular_1_stereo.wav", "recordings_with_textgrids/irregular_2_stereo.wav", "recordings_with_textgrids/irregular_3_stereo.wav")
# wavs = c("regular_stereo.wav")
stupid_regs = c("please", "strip", "charm", "drum", "swinge", "drub", "strum", "jeer", "swill", "ship", "fear", "harm", "fill", "peal")
stupid_wav_file = readWave("recordings_with_textgrids/regular_2_stereo.wav")
for(i in 1:length(dfs)) {

	is_regular = T
	if (i > 1) { is_regular = F} # Checks to see if it is regular or not.

	load(dfs[i])
	wav_file = readWave(wavs[i])

	print(paste("################# Reading", wavs[i], "#######################"))

	unique_words = unique(words_df$stem)

	for (word in unique_words) {
		print(paste("Working on ", word))
		# Make tmp DF with all rows of stem
		word_df = words_df[which(words_df$stem == word),]

		# Check for fricatives
		start_fricative = end_fricative = FALSE
		if (substring(word, 1, 1) %in% fricatives) {
			start_fricative = TRUE
		}
		if (substring(word, nchar(word)) %in% fricatives) {
			end_fricative = TRUE
		}

		if ("base" %in% word_df$morpheme == T) {
			print("BASE IS IN IT!!!")
			base_row = word_df[which(word_df$morpheme == "base"),]
			base = fade(zero_x(cutw(wav_file, f=freq, from=base_row$start_idx, 
				to=base_row$end_idx, output="Wave")), in_fricative=start_fricative, out_fricative=end_fricative)

			if (is_regular == F) {
				savewav(base, filename=paste(ROOT,"/irregulars/",word,"_irregular.wav",sep=''))
			}
			if (is_regular == T) {
				savewav(base, filename=paste(ROOT,"/bases/",word,"_base.wav",sep=''))
			}
		}

		if ("s" %in% word_df$morpheme == T) {
			s_morph_row = word_df[which(word_df$morpheme == "s" & 
				word_df$is_stem == F),]
			s_stem_row = word_df[which(word_df$morpheme == "s" & 
				word_df$is_stem == T),]

			s_morph = fade(zero_x(cutw(wav_file, f=freq, from=s_morph_row$start_idx, 
				to=s_morph_row$end_idx, output="Wave")), in_fricative=TRUE, out_fricative=TRUE)
			s_stem = fade(zero_x(cutw(wav_file, f=freq, from=s_stem_row$start_idx, 
				to=s_stem_row$end_idx, output="Wave")), in_fricative=start_fricative, out_fricative=end_fricative)

			savewav(s_stem, filename=paste(ROOT,"/stems/",word,"_s_stem.wav",sep=''))
			savewav(bind(s_stem,s_morph),filename=paste(ROOT,"/originals/",word,"s_original.wav",sep=''))
			if ("base" %in% word_df$morpheme == T) {
				savewav(bind(base,s_morph),filename=paste(ROOT,"/splices/",word,"s_splice.wav",sep=''))
			}
			print(paste("S morpheme completed:", word))
		} 

		if ("ed" %in% word_df$morpheme == T) {


			ed_morph_row = word_df[which(word_df$morpheme == "ed" & 
					word_df$is_stem == F),]
				ed_stem_row = word_df[which(word_df$morpheme == "ed" & 
					word_df$is_stem == T),]


			if (word %in% stupid_regs) {
				print("ITS A STUPID ONE>>>>")
				ed_morph = fade(zero_x(cutw(stupid_wav_file, f=freq, from=ed_morph_row$start_idx, 
					to=ed_morph_row$end_idx, output="Wave")))
				ed_stem = fade(zero_x(cutw(stupid_wav_file, f=freq, from=ed_stem_row$start_idx, 
					to=ed_stem_row$end_idx, output="Wave")), in_fricative=start_fricative, out_fricative=end_fricative)

			} else {

				ed_morph = fade(zero_x(cutw(wav_file, f=freq, from=ed_morph_row$start_idx, 
					to=ed_morph_row$end_idx, output="Wave")))
				ed_stem = fade(zero_x(cutw(wav_file, f=freq, from=ed_stem_row$start_idx, 
					to=ed_stem_row$end_idx, output="Wave")), in_fricative=start_fricative, out_fricative=end_fricative)

			}


			savewav(ed_stem, filename=paste(ROOT,"/stems/",word,"_ed_stem.wav",sep=''))
			savewav(bind(ed_stem,ed_morph),filename=paste(ROOT,"/originals/", word, "ed_original.wav", sep=''))
			if ("base" %in% word_df$morpheme == T) {
				savewav(bind(base,ed_morph),filename=paste(ROOT,"/splices/",word,"ed_splice.wav",sep=''))
			}
			print(paste("ED morpheme completed:", word))

		}

		if ("ing" %in% word_df$morpheme == T) {

			ing_morph_row = word_df[which(word_df$morpheme == "ing" & 
				word_df$is_stem == F),]
			ing_stem_row = word_df[which(word_df$morpheme == "ing" & 
				word_df$is_stem == T),]

			start = ing_morph_row$start_idx
			end = ing_morph_row$end_idx
			ing_morph = fade(zero_x(cutw(wav_file, f=freq, from=start, to=end, output="Wave")))
			ing_stem = fade(zero_x(cutw(wav_file, f=freq, from=ing_stem_row$start_idx, 
				to=ing_stem_row$end_idx, output="Wave")), in_fricative=start_fricative, out_fricative=end_fricative)

			savewav(ing_stem, filename=paste(ROOT,"/stems/",word,"_ing_stem.wav",sep=""))

			## TODO There is a problem
			savewav(bind(ing_stem,ing_morph),filename=paste(ROOT,"/originals/",word,"ing_original.wav",sep=""))
			if ("base" %in% word_df$morpheme == T) {
				savewav(bind(base,ing_morph),filename=paste(ROOT,"/splices/",word,"ing_splice.wav",sep=""))
			}
			print(paste("ING morpheme completed:", word))

		}
			
		

	}
}

word_df = words_df_all[which(words_df_all$stem == "chart"),]


i = 1

word = "pull"

