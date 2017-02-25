
#----------
# graph I/O
#----------

# merge and save graph

SaveGraph <- function(x, name = 'saved', new = FALSE) {

	if(new == FALSE) {

		name <- 'master'
		y <- x

		load('/Users/hxrts/projects/arena/spider/graphs/master.Rdata')

		x$objects <-
			bind_rows(x$objects, y$objects) %>%
			unique %>%
			group_by(id) %>%
			arrange(desc(length)) %>%
			slice(1) %>%
			ungroup %>%
			arrange(label)

		x$arrows <- bind_rows(x$arrows, y$arrows) %>% unique

		x$graph <- igraph::union(x$graph, y$graph)

	}

	save(x, file = str_c('/Users/hxrts/projects/arena/spider/graphs/', name, '.Rdata', collapse = ''))
	assign('x', x, envir = .GlobalEnv)
	x
}


# load archived graph

LoadGraph <- function(name = 'master') {
	load(str_c('/Users/hxrts/projects/arena/spider/graphs/', name, '.Rdata', collapse = ''))
	assign('x', x, envir = .GlobalEnv)
	x
}
