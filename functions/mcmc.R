
#-------------
# Markov chain
#-------------

ChooseStep <- function(walk.dict, path) {

	fork <- walk.dict %>% filter(from == tail(path, 1))

	if(nrow(fork) <= 1) {
		choice <- walk.dict %>% slice(sample(1:nrow(walk.dict), 1)) %$% from
	} else {
		rand <- sample(1:nrow(fork), 1)
		choice <- fork %>% slice(rand) %$% to
	}

	c(path, choice)

}


Walk <- function(origin, walk.dict, walk.length) {

	path = origin
	n = 0

	while(n < walk.length) {

		cat('\r', n)

		path <- ChooseStep(walk.dict, path)

		n <- n +1

		path
	}

	path
}


Jump <- function(walk, walk.dict, walk.length, jump.num) {

	n = 1
	jumps <- list()

	while(n <= jump.num) {

		unexplored <- walk[!unlist(walk.dict) %in% walk]

		jump <- unexplored[sample(1:length(unexplored), 1)]

		jumps[[n]] <-  Walk(origin = jump, walk.dict, walk.length = walk.length)

		n = n + 1
	}
	unlist(jumps)
}
