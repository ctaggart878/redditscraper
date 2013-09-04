# A script to scrape the top comments from the top posts of Reddit or a specified 
# subreddit and create a word frequency table from them. It also can plot a 
# word cloud from the most common words on the page. 

# Notes:
#
# This won't work on NSFW subreddits that require you to click that you're 18 
# or older. I haven't looked at how to code that yet.


# Overview of the function:  
# 1. Get the selected page (front or subreddit)
# 2. Build the links to all of the comments
# 3. Scrape each comments page (this step can take a while, 10 to 40 seconds)
# 4. Clean it up
# 5. Get the word frequency 
# 6. Plot a word cloud
# 7. Return a word frequency table


redditScrape <- function(subred = c('nameOfSubred', 'allTop'), time = c('day', 'week', 'month', 'year'), plotCloud = TRUE, saveText = FALSE, myDirectory = "/choose/a/directory") {


	#######################################################
	# 0. Load the required packages.  And check a few items
	
	require(XML)
	require(RCurl)
	require(RColorBrewer) 
	require(wordcloud)
	
	# if more than one time, apply function to each time frame separately
	if (length(time) > 1) { 
	  return(lapply(time, function(i) 
	    redditScrape(subred=subred, time=i, saveText=saveText, myDirectory=myDirectory)))
	}
	

	#######################################################
	# 1. Make the url, get the page. 

	if (subred == 'allTop') {
		url <- paste('http://www.reddit.com/top/?sort=top&t=', time, sep = "")
	} else {	
		url <- paste("http://www.reddit.com/r/", subred, "/top/?sort=top&t=", time, sep = "")
	}
	
	doc <- htmlParse(url)

	#######################################################
	# 2. Get the links that go to comment sections of the posts

	links <- xpathSApply(doc, "//a/@href")
	comments <- grep("comments", links)
	comLinks <- links[comments]
	comments <- grep('reddit.com', comLinks, fixed=TRUE)
	comLinks <- comLinks[comments]


	#######################################################
	#  3. Scrape the pages
	#  This will scrape a page and put it in to 
	#  an R list object 

	textList <- as.list(rep(as.character(""), length(comLinks))) 
	docs <- getURL(comLinks)
	for (i in 1:length(docs)) {
		textList[[i]] <- htmlParse(docs[i], asText = TRUE)
		textList[[i]] <- xpathSApply(textList[[i]], "//p", xmlValue)
	}
		
	#######################################################
	#  4. Clean up the text.

	# Remove the submitted lines and lines at the end of each page
	for (i in 1:length(textList)) {
		submitLine <- grep("submitted [0-9]", textList[[i]]) 
		textList[[i]] <- textList[[i]][{(submitLine[1] + 1):(length(textList[[i]])-10)}]
	}
	
	# Removing lines capturing user and points, etc.
	# Yes, there could be fewer grep calls, but this made it 
	# easier to keep track of what was going on.
	for (i in 1:length(textList)) { 
		grep('points 1 minute ago', textList[[i]]) -> nameLines1
		grep('points [0-9] minutes ago', textList[[i]]) -> nameLines2
		grep('points [0-9][0-9] minutes ago', textList[[i]]) -> nameLines3
		grep("points 1 hour ago", textList[[i]]) -> nameLines4
		grep("points [0-9] hours ago", textList[[i]]) -> nameLines5
		grep("points [0-9][0-9] hours ago", textList[[i]]) -> nameLines6
		grep('points 1 day ago', textList[[i]]) -> nameLines7
		grep('points [0-9] days ago', textList[[i]]) -> nameLines8
		grep('points [0-9][0-9] days ago', textList[[i]]) -> nameLines9
		grep('points 1 month ago', textList[[i]]) -> nameLines10
		grep('points [0-9] months ago', textList[[i]]) -> nameLines11
		grep('points [0-9][0-9] months ago', textList[[i]]) -> nameLines12
		allLines <- c(nameLines1, nameLines2, nameLines3, nameLines4, 
			nameLines5, nameLines6, nameLines7, nameLines8, nameLines9, 
			nameLines10, nameLines11, nameLines12)
		textList[[i]] <- textList[[i]][-allLines]
		textList[[i]] <- textList[[i]][textList[[i]]!=""]
		textList[[i]] <- tolower(textList[[i]])
	}


	# Let's simplify our list. Could have been done earlier, but so it goes. 
	allText <- unlist(textList)

	# Remove the punctuation, links, etc.
	allText <- gsub("https?://[[:alnum:][:punct:]]+", "", allText)
	allText <- gsub("[,.!?\"]", "", allText)
	allText <- strsplit(allText, "\\W+", perl=TRUE)
	allText <- unlist(allText)

	# Remove frequent words and orphans of contractions (that sounds 
	# sadder than it is).
	frequentWords <- c("the", "be", "been", "to", "of", "and", "a", "in", 
	"that", "have", "i", "it", "for", "not", "on", "with", "he", "as", "you", 
	"do", "at", "this", "but", "his", "by", "from", "they", "we", "say", "her", 
	"she", "or", "an", "will", "my", "one", "all", "would", "there", "their", 
	"what", "so", "up", "out", "if", "about", "who", "get", "which", "go", 
	"me", "when", "make", "can", "like", "time", "no", "just", "him", "know", 
	"take", "people", "into", "year", "your", "good", "some", "could", "them", 
	"see", "other", "than", "then", "now", "look", "only", "come", "its", 
	"over", "think", "also", "back", "after", "use", "two", "how", "our", 
	"work", "first", "well", "way", "even", "new", "want", "because", "any", 
	"these", "give", "day", "most", "us", 'is', 'are', 'was', 'were', 'i', 's', 
	'was', 'don', 'aren', 'points1', 'point', 't', 'm', 'points0', '10', '1', 
	're', 'll', 'd', '2', '3', '4', '5', '6', '7', '8', '9', 'doesn','d', 've', 
	'r', 'has', 'had', 'been', 'being', '0', 'more', 'really', 'isn', 'very', 
	'am', 'didn', 'wouldn', '', 'points', 'point', 'months', 'ago', 'deleted', 
	'much')

	for (i in 1:length(frequentWords)) { 
		allText <- allText[allText!=frequentWords[i]]
	}

	# Save the file to your drive. This way you can drop it into
	# Wordle.net or use it other places. 
	if (saveText == TRUE) {
		curWD <- getwd() 
		setwd(myDirectory)
		filename <- paste("Reddit Comments Postscrub ", subred, " ", time, " ", 
			Sys.time(),".txt", sep = "") 
		write.table(allText, file = filename, row.names=F, col.names=F, append=T)
		# save(allText, file = filename)
		textListBackup <- textList
		setwd(curWD)
	}

	#######################################################
	#  5. Word frequency table

	textTable <- table(allText)
	textTable <- sort(textTable, decreasing = TRUE)

	#######################################################
	#  6. Plot word cloud

	if (plotCloud == TRUE) {
		# This is a nice option.  Just use a portion of the 0-1 for color
		rainbow(30,s=.8,v=.6,start=.5,end=1,alpha=1) -> pal
		wordcloud(names(textTable[1:200]), textTable[1:200], scale = c(4,.5), max.words = 200, colors = pal)
	}

	#######################################################
	#  7. Return the text table

	textTable
}


