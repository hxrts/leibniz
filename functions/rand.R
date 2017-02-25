
#----------------------------------
# random channel from Are.na server
#----------------------------------

RandChan <- function(user = FALSE) {

	seed <- sapply(lapply(strsplit(substr(gsub('\\:|\\.|\\-|\\ ', '', Sys.time()), 12, 20), NULL), rev), paste, collapse = '')

	if(user == TRUE) {

		rand.fetch <-
			str_c('https://secure.are.na/v2/user/sam-hart/search?&per=1&subject=channel&sort=random&seed=', seed) %>% fromJSON %$%
			channels %$%
			slug
	} else {
		rand.fetch <-
			str_c('https://secure.are.na/v2/channels/sort=random', seed) %>%
			fromJSON %$%
			channels %$%
			slug %>%
			head(1)
	}

	rand.fetch
}

# str_c('http://api.are.na/v2/search/channels?sort=random&seed=', seed) %>%
# 	fromJSON %$%
# 	channels %$%
# 	slug

