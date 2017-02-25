
#---------
# word2vec
#---------

PrepWord2vec <- function (origin, destination, split_characters = '[ ]', lowercase = FALSE, bundle_ngrams = 1, ...) {

	non_choking_strsplit <- function(lines, ...) {

		splitLineIfNecessary <- function(line, limit = 10000) {

			chars = nchar(line)

			if (chars < limit) {
				return(line)
			} else {
				first_half = substr(line, 1, nchar(line) %/% 2)
				second_half = substr(line, 1, nchar(line) %/% 2)
				return(c(splitLineIfNecessary(first_half), splitLineIfNecessary(second_half)))
			}
		}

		lines <- unlist(lapply(lines, splitLineIfNecessary))
		unlist(strsplit(lines, ...))
	}

	message('beginning tokenization to text file at ', destination)

	if (!exists('dir.exists')) {
		dir.exists <- function(x) {
			res <- file.exists(x) & file.info(x)$isdir
			stats::setNames(res, x)
		}
	}

	if (dir.exists(origin)) {
		origin = list.files(origin, recursive = TRUE, full.names = TRUE)
	}

	cat('', file = destination, append = FALSE)

	for (filename in origin) {

		message("\n", filename, appendLF = FALSE)
		con <- file(filename, open = 'r')

		while (length(lines <- readLines(con, n = 1000, warn = FALSE)) > 0) {

			message('.', appendLF = FALSE)

				words <- non_choking_strsplit(lines, split_characters, perl = TRUE)

			if (lowercase) {
				words <- tolower(words)
			}
			cat(c(words, " "), file = destination, append = T)
		}

		close(con)
		cat(c('\n'), file = destination, append = TRUE)
	}

	real_destination_name = destination

	if (bundle_ngrams > 1) {

		while (bundle_ngrams > 1) {

			old_destination = destination
			destination = paste0(destination, '_')
			word2phrase(old_destination, destination, ...)
			file.remove(old_destination)
			bundle_ngrams = bundle_ngrams - 1
		}

		file.rename(destination, real_destination_name)
	}

	silent = real_destination_name
}


