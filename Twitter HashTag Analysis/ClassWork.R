if (Sys.getenv("JAVA_HOME")!="") Sys.setenv(JAVA_HOME="")

require(twitteR)
setup_twitter_oauth(
  consumer_key = "MTfL0uv6oBbtVBa3dPuR4PS51",
  consumer_secret = "T480vrOtvSK1XxGcPp20SdRoW99by3EGVvAapzyq8io0uH6lUh",
  access_token = "191958252-uOisKMkvWHWmiTRQnf9d6XMww5wtZVtox1ShcjMm",
  access_secret = "9kTTYtORX4KnXk9SG4R9bQ1PpmQyBYEwmfvDksTh6Wht0")
getCurRateLimitInfo(resources='search')

ThursdayThoughts= searchTwitter(searchString ='#ThursdayThoughts',
                                n=2000 , lang = 'en') 
# last ran at 8:08 PM 14-Sep-2017
ThursdayThoughts_RT_Removed = strip_retweets(ThursdayThoughts,
                                             strip_manual = T,
                                             strip_mt = T)
TT_dataframe = twListToDF(ThursdayThoughts)
write.csv(TT_dataframe,'TT_dataframe.csv')

        TT_dataframe = read.csv(file = 'Data.csv', header = T)
        TT_dataframe = TT_dataframe[-1]
        TT_dataframe = TT_dataframe[1:200,]
        TT_dataframe$created
        TT_dataframe$created <- as.Date(TT_dataframe$created,format = "%m/%d/%y")
        
Tweets = as.character(TT_dataframe$text)
require(tm)
Tweets_Corpus = Corpus(VectorSource(Tweets))

removeUsernameInRT <- function(x) str_replace_all(x,"(?<=@)[^\\s:]+",'')
Tweets_Corpus = tm_map(Tweets_Corpus, content_transformer(removeUsernameInRT))

require(stringr)
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
Tweets_Corpus = tm_map(Tweets_Corpus, content_transformer(removeURL))

removeCompleteNumbers <- function(x) str_replace_all(x,'(?<=[:blank:])[0-9]+(?=[:blank:])', '')
Tweets_Corpus = tm_map(Tweets_Corpus, content_transformer(removeCompleteNumbers))

Tweets_Corpus = tm_map(Tweets_Corpus,removePunctuation)
#Tweets_Corpus = tm_map(Tweets_Corpus, removeWords,c(stopwords('en'),'RT','ThursdayThoughts'))

removeSingle <- function(x) gsub(" . ", " ", x)   
Tweets_Corpus = tm_map(Tweets_Corpus, content_transformer(removeSingle))
Tweets_Corpus = tm_map(Tweets_Corpus,stripWhitespace)
Tweets_Corpus = tm_map(Tweets_Corpus,content_transformer(tolower))
Tweets_Corpus = tm_map(Tweets_Corpus, content_transformer(replace_abbreviation))

Tweets_Corpus_Filtered = Tweets_Corpus
# Tweets_Corpus = Tweets_Corpus_Filtered
Tweets_Corpus<-tm_map(Tweets_Corpus, stemDocument,language = 'english')
stemCompletionuserDefined <- function(x,dictionary) {
  x = unlist(strsplit(as.character(x)," "))
  x = x[x !=""]
  x = stemCompletion(x, dictionary = dictionary)
  x = paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}
Tweets_Corpus = lapply(Tweets_Corpus, stemCompletionuserDefined, dictionary=Tweets_Corpus_Filtered)
temp = character()
for(i in 1: nrow(TT_dataframe)){
  temp[i] = Tweets_Corpus[[i]]$content
}
temp_df = data.frame(text = temp, stringsAsFactors = F)
Tweets_Corpus = Corpus(VectorSource(temp_df$text))
Tweets_Corpus = tm_map(Tweets_Corpus, removeWords,c(stopwords('en'),'rt','thursdaythoughts'))

Tweets_Corpus_TDM = TermDocumentMatrix(Tweets_Corpus)
Tweets_Corpus_TDM_M = as.matrix(Tweets_Corpus_TDM)

## Word Frequency Analysis----
termFrequency = rowSums(Tweets_Corpus_TDM_M)
termFrequency = sort(termFrequency,decreasing =T)

termFrequency[1:25]
barplot(termFrequency[1:25],
        col='tan',las=2)

require(ggplot2)
frequency_df <- data.frame(term = names(termFrequency), freq= termFrequency)
ggplot(frequency_df[1:25,], aes(reorder(term, freq),freq)) +
  theme_bw() + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  labs(list(title="Term Frequency Chart", x="Terms", y="Term Counts")) 

## Word Mention Matrix  ----
index <- which(dimnames(Tweets_Corpus_TDM)$Terms %in% c("life", "dead"))
as.matrix(Tweets_Corpus_TDM[index,21:60])

## Word Cloud ----
pal<- brewer.pal(4, "Dark2")
require(wordcloud)
wordcloud(words = names(termFrequency), 
          freq = termFrequency, 
          min.freq = 2, 
          random.order = F, 
          colors = pal, 
          max.words = 100)
## Word Correlation ----
# WordCorrelation <- apply_as_df(Tweets_Corpus[1:100], word_cor, word = "save", r=.01)
# plot(WordCorrelation)

## Topic Modeling ----
Tweets_Corpus_DTM <- as.DocumentTermMatrix(Tweets_Corpus_TDM)
rowTotals <- apply(Tweets_Corpus_DTM , 1, sum)
NullDocs <- Tweets_Corpus_DTM[rowTotals==0, ]
Tweets_Corpus_DTM   <- Tweets_Corpus_DTM[rowTotals> 0, ]
if (length(NullDocs$dimnames$Docs) > 0) {
  TT_dataframe <- TT_dataframe[-as.numeric(NullDocs$dimnames$Docs),]
}
require(topicmodels)
lda <- LDA(Tweets_Corpus_DTM, k = 5) # find 5 topic
term <- terms(lda, 7) # first 7 terms of every topic
term
topics<- topics(lda)
topics
topics_df<- data.frame(date=(TT_dataframe$created), topic = topics)
qplot(date, ..count.., data=topics_df,
       geom ="density",
       fill= term[topic], position="stack")
