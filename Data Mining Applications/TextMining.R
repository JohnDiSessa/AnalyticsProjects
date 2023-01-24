# Install
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator
install.packages("RColorBrewer") # color palettes

# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

#text <- readLines(file.choose())
filePath <- "http://www.sthda.com/sthda/RDoc/example-files/martin-luther-
king-i-have-a-dream-speech.txt"
text <- readLines(filePath)
text

# Load the data as a corpus
docs <- Corpus(VectorSource(text))
docs

inspect(docs)

#remove symbols#
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Remove numbers
docs <- tm_map(docs, removeNumbers)

# Remove English common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))

# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2"))

# Remove punctuation
docs <- tm_map(docs, removePunctuation)

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Text stemming
# docs <- tm_map(docs, stemDocument) #reduces words to their root form
#removes suffixes
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(900)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

findFreqTerms(dtm, lowfreq = 4)

#all words at least 30% correlated to freedom#
findAssocs(dtm, terms = "freedom", corlimit = 0.3)

head(d, 80)
nrow(d)

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightskyblue2", main ="Most Frequent Words",
        ylab = "Word Frequencies")


