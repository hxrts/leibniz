
library(wordVectors)
library(stringdist)

source('../spider/_daemon.R', chdir = TRUE)

source('functions/fuzz.R')
source('functions/graph_io.R')
source('functions/mcmc.R')
source('functions/rand.R')
source('functions/utility.R')

options(digits.secs = 6) 

#----------------------
# crawl Are.na channels
#----------------------

LoadGraph()

origin <- RandChan(user = TRUE)
# origin = 'worldmaking'

crawl <- Crawl(origin, depth = 4)

crawl <- Crawl(x$objects$id[sample(1:length(x$objects$id), 100)], depth = 3)

SaveGraph()


#-----------
# clip match
#-----------

channels <- x$objects$id %>% unique %>% sort

# clip numeric tail

channels.clip <-
	channels %>%
	str_split('-') %>%
	map( ~ {

		first <-
			str_c(.x[1:(length(.x) - 1)], collapse = '-') %>%
			gsub('\\-$', '', .)

		last <-
			suppressWarnings(as.numeric(.x[length(.x)])) %>%
			ifelse(is.na(.), 'char', .)

		if(length(.x) == 1 | !is.numeric(last)) {
			NA
		} else {
			first
		}
	}) %>%
	unlist


clip.dict <-
	data_frame(clip = channels.clip, channel = channels) %>%
	filter(!is.na(clip)) %>%
	filter(clip != '')


#-------------
# Markov chain
#-------------

# mcmc dictionary

walk.dict <-
	x$arrows %>%
	select(from, to) %>%
	unique


# Markov walk

walk <- Walk(origin = 'root', walk.dict, walk.length = 100000)

walk.jump <-Jump(walk, walk.dict, walk.length = 1000, jump.num = 100)

corpus <- str_c(c(walk, walk.jump), collapse = ' ')

write_tsv(data_frame(corpus), path = 'corpus/zero.txt')


#---------
# word2vec
#---------

PrepWord2vec('corpus', 'corpus.txt', lowercase = TRUE)

model <- train_word2vec('corpus.txt', output = 'corpus_vectors.bin', threads = 3, vectors = 100, window = 12)

# model <- read.vectors("corpus_vectors.bin")  # read-in previously trained model

nearest_to(model, model[['performance']])

nearest_to(model, model[[c('performance', 'movement', 'event-scores')]], 50)


nearest_to(
	model,
	model[rownames(model) == 'performance',] -
	model[rownames(model) == '',] +
	model[rownames(model) == 'event-scores',],
	50
)


vector_set[['performance']] -
vector_set[['theatre']] +
vector_set[['dance']]


vector_set[['performance']] -
vector_set[[c('dance', 'movement')]] +
vector_set[[c('event-scores', 'dance-notation')]]


cosineSimilarity(
	vector_set[[c('dance', 'movement'), average = FALSE]],
	vector_set[[c('event-scores', 'dance-notation'), average = FALSE]]
)






