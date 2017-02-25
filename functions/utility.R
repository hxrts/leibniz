
#------------------------
# open channel in browser
#------------------------

Arena <- function(channel) {
	system(str_c('/usr/bin/open -a "/Applications/Google Chrome.app" "http://are.na/', channel, '"', collapse = ''))
}

