dest <- "C:\\Modi\\modi3.pdf"
# get txt-file name and open it  
filetxt <- sub(".pdf", ".txt", dest)

library(tm)
library(wordcloud)
library(Rstem)

txt <- readLines(filetxt)
txt <- tolower(txt)
txt <- removeWords(txt, c("\\f", stopwords()))

corpus <- Corpus(VectorSource(txt))
corpus <- tm_map(corpus, removePunctuation)
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
d <- data.frame(freq = sort(rowSums(m), decreasing = TRUE))

d$stem <- wordStem(row.names(d), language = "english")

d$word <- row.names(d)

d <- d[nchar(row.names(d)) < 20, ]

agg_freq <- aggregate(freq ~ stem, data = d, sum)
agg_word <- aggregate(word ~ stem, data = d, function(x) x[1])

d <- cbind(freq = agg_freq[, 2], agg_word)

d <- d[order(d$freq, decreasing = T), ]

wordcloud(d$word, d$freq, scale=c(3,.5),
          min.freq=1,
          max.words=200,
          random.order=FALSE,
          rot.per=0.0,
          colors=brewer.pal(8,"Set2"),
          fixed.asp=FALSE)

file.remove(dir(tempdir(), full.name=T)) # remove files