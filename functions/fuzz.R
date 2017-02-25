
#---------------
# fuzzy matching
#---------------

# fuzz matching

FuzzMatchSp <- function(string, max.dist = 4) {
	channels[amatch(string, channels, maxDist = max.dist)]
}

# FuzzMatchSp('performsancee')


FuzzMatch <- function(string, max.dist = 4) {
	channels[which(adist(string, channels, partial = TRUE, ignore.case = TRUE) < max.dist)]
}

# FuzzMatch('performsancee')

ClipMatch <- function(string) {
	clips <- clip.dict %>% filter(clip == string) %>% as.data.frame
	if(nrow(clips) == 0) {
		NA
	} else {
		clips
	}
}

# ClipMatch('performance')

