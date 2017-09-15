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

# TT_dataframe = read.csv(file = 'Data.csv', header = T)
# TT_dataframe = TT_dataframe[-1]
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
Tweets_Corpus = tm_map(Tweets_Corpus, removeWords,c(stopwords('en'),'RT','ThursdayThoughts'))

removeSingle <- function(x) gsub(" . ", " ", x)   
Tweets_Corpus = tm_map(Tweets_Corpus, content_transformer(removeSingle))
Tweets_Corpus = tm_map(Tweets_Corpus,stripWhitespace)
Tweets_Corpus = tm_map(Tweets_Corpus,content_transformer(tolower))
Tweets_Corpus = tm_map(Tweets_Corpus, content_transformer(replace_abbreviation))

Tweets_Corpus_Filtered = Tweets_Corpus
Tweets_Corpus<-tm_map(Tweets_Corpus, stemDocument,language = 'english')
# stemCompletionuserDefined <- function(x,dictionary) {
#   x = unlist(strsplit(as.character(x)," "))
#   x = x[x !=""]
#   x = stemCompletion(x, dictionary = dictionary)
#   x = paste(x, sep="", collapse=" ")
#   PlainTextDocument(stripWhitespace(x))
# }
# Tweets_Corpus = lapply(Tweets_Corpus, stemCompletionuserDefined, dictionary=Tweets_Corpus_Filtered)
inspect(Tweets_Corpus[1:5])
Tweets_Corpus <- tm_map(Tweets_Corpus, stemCompletion, dictionary=Tweets_Corpus_Filtered)
inspect(Tweets_Corpus[1:5])

Tweets_Corpus = Corpus(VectorSource(Tweets_Corpus))
backup_Tweets_Corpus = Tweets_Corpus

Tweets_Corpus_TDM = TermDocumentMatrix(Tweets_Corpus)
Tweets_Corpus_TDM_M = as.matrix(Tweets_Corpus_TDM)
termFrequency = rowSums(Tweets_Corpus_TDM_M)
termFrequency = sort(termFrequency,decreasing =T)
termFrequency[1:25]
require(ggplot2)

barplot(termFrequency[1:25],
        col='tan',las=2)

require(qdap)
frequency = freq_terms(TT_dataframe$text,
                       top = 25,
                       at.least = 1000,
                       stopwords =stopwords('english'))
