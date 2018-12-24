csvs = list.files(pattern="*.csv")
stimuli = list.files(path="stimuli")

for(item in csvs){
	words = read.csv(file=item, header=T, sep=',', stringsAsFactors=F)
	for(word in words[,1]){
		wordSplit = strsplit(word, split="/")[[1]]
		if(!wordSplit[2] %in% stimuli) {
			print(paste(item, "has bad word:", wordSplit[2]))
		}
	}
}
