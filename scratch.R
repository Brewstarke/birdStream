library(streamgraph)
library(dplyr)
library(ggplot2)

ggplot2::movies %>%
	select(year, Action, Animation, Comedy, Drama, Documentary, Romance, Short) %>%
	tidyr::gather(genre, value, -year) %>%
	group_by(year, genre) %>%
	tally(wt=value) %>%
	streamgraph("genre", "n", "year") %>%
	sg_axis_x(20) %>%
	sg_colors("PuOr") %>%
	sg_legend(show=TRUE, label="Genres: ")


library(shiny)
library(htmltools)
library(shinyapps)
library(streamgraph)
library(dplyr)
library(tidyr)
library(rebird)
library(magrittr)

ebirdregion(region = 'US-NY', back = 10, hotspot = TRUE)->a



ebirdregion(region = 'US-NY', back = 30, hotspot = FALSE)->a



birdstream <- ebirdfreq('hotspots', 'L167247') # Uplands Farm hot spot- L123000
# Jones Beach -- L167247

birdstreamCounty <- ebirdfreq(loctype = 'counties', loc = 'US-NY-103')

birdstream %<>% separate(monthQt, into = c("Month", "Week")) %>% 
	mutate(monthNum = as.numeric(match(.$Month, table = month.name))) %>%
	mutate(weekNo = (monthNum - 1) * 4 + (as.numeric(Week)))

	
birdstream$weekNo <- as.Date(birdstream$weekNo, format = '%U', origin = '0')

birdstream %>% streamgraph(comName, frequency, weekNo) %>%
	sg_fill_brewer("PuOr") %>%
	sg_legend(show=TRUE, label="Species: ")

###


birdStreamPlot <- function(county){
	data <- ebirdfreq('counties', county)
	data %<>% separate(monthQt, into = c("Month", "Week")) %>% 
		mutate(monthNum = as.numeric(match(.$Month, table = month.name))) %>%
		mutate(weekNo = as.Date((monthNum - 1) * 4 + (as.numeric(Week)), format = '%U', origin = '1')) %>% 
		mutate(freqtrans = frequency^3)
	
	data %>% streamgraph(comName, freqtrans, weekNo, offset = 'zero') %>%
		sg_fill_brewer("Spectral") %>%
		sg_axis_x(1, "week", "%U") %>% 
		sg_legend(show=TRUE, label="Species: ")
}

birdStreamPlot('US-NY-103')





