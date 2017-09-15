require(twitteR)
require(tm)
require(stringr)
require(ggplot2)
require(qdap)
require(stringr)

tweets.df <- read.csv("TwitterData.csv")
tweets.df$created <- as.Date(tweets.df$created, format= "%d-%m-%y")
tweets.df$text <- genX(tweets.df$text, " <", ">")
myCorpus<- Corpus(VectorSource(tweets.df$text)) 
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)  
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)   
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
myStopWords<- c((stopwords('english')),c("rt", "use", "used", "via", "amp"))
myCorpus<- tm_map(myCorpus,removeWords , myStopWords) 
removeSingle <- function(x) gsub(" . ", " ", x)   
myCorpus <- tm_map(myCorpus, content_transformer(removeSingle))
myCorpus<- tm_map(myCorpus, stripWhitespace) 
myCorpusCopy<- myCorpus
writeLines(strwrap(myCorpus[[250]]$content,60))
# myCorpus<- myCorpusCopy
myCorpus<-tm_map(myCorpus, stemDocument)
writeLines(strwrap(myCorpus[[250]]$content,60))

stemCompletion2 <- function(x,dictionary) {
  x <- unlist(strsplit(as.character(x)," "))
  x <- x[x !=""]
  x <- stemCompletion(x, dictionary = dictionary)
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}

myCorpus <- lapply(myCorpus$content, stemCompletion2, dictionary=myCorpusCopy)
temp = character()
for(i in 1:1000){
  temp[i] = myCorpus[[i]]$content
}
final_tweets_df = data.frame(text = temp, stringsAsFactors = F)
myCorpus2 = Corpus(VectorSource(final_tweets_df$text))
wordFreq <- function(corpus,word)
{
  results<- lapply(corpus,
                   function(x){ grep(as.character(x),pattern = paste0("\\<", word))})
  sum(unlist(results))
}
n.tenni<- wordFreq(myCorpusCopy, "tenni")
n.tennis <- wordFreq(myCorpusCopy, "tennis")
cat(n.tenni, n.tennis)

replaceWord <- function(corpus, oldword, newword)
{
  tm_map(corpus, content_transformer(gsub), pattern=oldword, replacement=newword)
}
myCorpus<- replaceWord(myCorpus2, "tenni", "tennis")

tdm<- TermDocumentMatrix(myCorpus2, control= list(wordLengths= c(1, Inf)))
tdm
idx <- which(dimnames(tdm)$Terms %in% c("usopen", "grandslam"))
as.matrix(tdm[idx,21:60])
(freq.terms <- findFreqTerms(tdm, lowfreq = 50))

term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq > 50)
df <- data.frame(term = names(term.freq), freq= term.freq)
ggplot(df, aes(reorder(term, freq),freq)) + theme_bw() + 
  geom_bar(stat = "identity")  + coord_flip() 
+labs(list(title="Term Frequency Chart", x="Terms", y="Term Counts")) 
