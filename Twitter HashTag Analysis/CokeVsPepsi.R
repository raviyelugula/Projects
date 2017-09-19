if (Sys.getenv("JAVA_HOME")!="") Sys.setenv(JAVA_HOME="")
require(twitteR)
require(tm)
require(stringr)
require(qdap)
require(ggplot2)
require(wordcloud)
require(topicmodels)
require(tidytext)
require(dplyr)
require(plotrix)
require(ggthemes)
setup_twitter_oauth(
  consumer_key = "MTfL0uv6oBbtVBa3dPuR4PS51",
  consumer_secret = "T480vrOtvSK1XxGcPp20SdRoW99by3EGVvAapzyq8io0uH6lUh",
  access_token = "191958252-uOisKMkvWHWmiTRQnf9d6XMww5wtZVtox1ShcjMm",
  access_secret = "9kTTYtORX4KnXk9SG4R9bQ1PpmQyBYEwmfvDksTh6Wht0")
getCurRateLimitInfo(resources = 'search')

# CocaCola_TweetsList= searchTwitter(searchString ='Coke',n=10000 , lang = 'en') 
# Pepsi_TweetsList= searchTwitter(searchString ='Pepsi',n=10000 , lang = 'en') 
# CocaCola_DF = twListToDF(CocaCola_TweetsList)
# Pepsi_DF = twListToDF(Pepsi_TweetsList)

# write.csv(CocaCola_DF,'CocaCola_DF.csv',row.names = F) # fetched at 18th sep 08:10PM IST
# write.csv(Pepsi_DF,'Pepsi_DF.csv',row.names = F)

CocaCola_DF = read.csv(file ='CocaCola_DF.csv',header = T)
Pepsi_DF = read.csv(file ='Pepsi_DF.csv',header = T)

CocaCola_DF$createdDate = as.Date(CocaCola_DF$created,format = "%d-%m-%Y")
Pepsi_DF$createdDate = as.Date(Pepsi_DF$created,format = "%d-%m-%Y")

CocaCola_Tweets = as.character(CocaCola_DF$text)
CocaCola_Corpus = Corpus(VectorSource(CocaCola_Tweets))

Pepsi_Tweets = as.character(Pepsi_DF$text)
Pepsi_Corpus = Corpus(VectorSource(Pepsi_Tweets))

removeUsernameInRT <- function(x) str_replace_all(x,"(?<=@)[^\\s:]+",'')
CocaCola_Corpus = tm_map(CocaCola_Corpus, content_transformer(removeUsernameInRT))
Pepsi_Corpus = tm_map(Pepsi_Corpus, content_transformer(removeUsernameInRT))

removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
CocaCola_Corpus = tm_map(CocaCola_Corpus, content_transformer(removeURL))
Pepsi_Corpus = tm_map(Pepsi_Corpus, content_transformer(removeURL))

removeCompleteNumbers <- function(x) str_replace_all(x,'(?<=[:blank:])[0-9]+(?=[:blank:])', '')
CocaCola_Corpus = tm_map(CocaCola_Corpus, content_transformer(removeCompleteNumbers))
Pepsi_Corpus = tm_map(Pepsi_Corpus, content_transformer(removeCompleteNumbers))

CocaCola_Corpus = tm_map(CocaCola_Corpus,removePunctuation)
Pepsi_Corpus = tm_map(Pepsi_Corpus,removePunctuation)
removeSingle <- function(x) gsub(" . ", " ", x)   
CocaCola_Corpus = tm_map(CocaCola_Corpus, content_transformer(removeSingle))
CocaCola_Corpus = tm_map(CocaCola_Corpus,stripWhitespace)
CocaCola_Corpus = tm_map(CocaCola_Corpus,content_transformer(tolower))
CocaCola_Corpus = tm_map(CocaCola_Corpus, content_transformer(replace_abbreviation))
Pepsi_Corpus = tm_map(Pepsi_Corpus, content_transformer(removeSingle))
Pepsi_Corpus = tm_map(Pepsi_Corpus,stripWhitespace)
Pepsi_Corpus = tm_map(Pepsi_Corpus,content_transformer(tolower))
Pepsi_Corpus = tm_map(Pepsi_Corpus, content_transformer(replace_abbreviation))

CocaCola_Corpus_Filtered = CocaCola_Corpus
CocaCola_Corpus = tm_map(CocaCola_Corpus, stemDocument,language = 'english')
Pepsi_Corpus_Filtered = Pepsi_Corpus
Pepsi_Corpus = tm_map(Pepsi_Corpus, stemDocument,language = 'english')

stemCompletionuserDefined <- function(x,dictionary) {
  x = unlist(strsplit(as.character(x)," "))
  x = x[x !=""]
  x = stemCompletion(x, dictionary = dictionary)
  x = paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}
CocaCola_Corpus = lapply(CocaCola_Corpus, stemCompletionuserDefined, dictionary=CocaCola_Corpus_Filtered)
Pepsi_Corpus = lapply(Pepsi_Corpus, stemCompletionuserDefined, dictionary=Pepsi_Corpus_Filtered)

temp = character()
for(i in 1: nrow(CocaCola_DF)){
  temp[i] = CocaCola_Corpus[[i]]$content
}
temp_df = data.frame(text = temp, stringsAsFactors = F)
CocaCola_Corpus = Corpus(VectorSource(temp_df$text))
CocaCola_Corpus = tm_map(CocaCola_Corpus, 
                       removeWords,c(stopwords('en'),'rt','cocacola','coke','drink'))
temp1 = character()
for(i in 1: nrow(Pepsi_DF)){
  temp1[i] = Pepsi_Corpus[[i]]$content
}
temp_df1 = data.frame(text = temp1, stringsAsFactors = F)
Pepsi_Corpus = Corpus(VectorSource(temp_df1$text))
Pepsi_Corpus = tm_map(Pepsi_Corpus, 
                         removeWords,c(stopwords('en'),'rt','pepsi','drink'))
rm(list = c('temp1','temp_df1','temp_df','temp'))

CocaCola_Corpus_TDM = TermDocumentMatrix(CocaCola_Corpus)
CocaCola_Corpus_TDM_M = as.matrix(CocaCola_Corpus_TDM)
CocaCola_TermFrequency = rowSums(CocaCola_Corpus_TDM_M)
CocaCola_TermFrequency = sort(CocaCola_TermFrequency,decreasing =T)
Pepsi_Corpus_TDM = TermDocumentMatrix(Pepsi_Corpus)
Pepsi_Corpus_TDM_M = as.matrix(Pepsi_Corpus_TDM)
Pepsi_TermFrequency = rowSums(Pepsi_Corpus_TDM_M)
Pepsi_TermFrequency = sort(Pepsi_TermFrequency,decreasing =T)

Frequency_DF = data.frame(term = names(CocaCola_TermFrequency), 
                           freq= CocaCola_TermFrequency,
                           brand = 'CocaCola')
Frequency_DF = rbind(Frequency_DF,data.frame(term = names(Pepsi_TermFrequency), 
                                             freq= Pepsi_TermFrequency,
                                             brand = 'Pepsi') )
Frequency_DF %>%
  top_n(25,freq) %>%
ggplot(aes(reorder(term, freq),freq,fill= brand)) +
  scale_fill_brewer(palette="Set1")+
  geom_bar(stat = "identity") + 
  coord_flip() +
  labs(list(title="Term Frequency Chart", x="Top 25 Terms", y="Term Counts")) 

index = which(dimnames(CocaCola_Corpus_TDM)$Terms %in% c("like", "diet"))
as.matrix(CocaCola_Corpus_TDM[index,1:50])
index = which(dimnames(Pepsi_Corpus_TDM)$Terms %in% c("like",'coke'))
as.matrix(Pepsi_Corpus_TDM[index,1:50])

pal = brewer.pal(6, "Reds")
pal = pal[2:6]
wordcloud(words = names(CocaCola_TermFrequency), 
          freq = CocaCola_TermFrequency, 
          min.freq = 10, 
          random.order = F, 
          colors = pal, 
          max.words = 500)
pal = brewer.pal(7, "Blues")
pal = pal[4:7]
wordcloud(words = names(Pepsi_TermFrequency), 
          freq = Pepsi_TermFrequency, 
          min.freq = 10, 
          random.order = F, 
          colors = pal, 
          max.words = 500)

Pepsi_Associations = findAssocs(Pepsi_Corpus_TDM,terms ='like',corlimit=0.15)
Pepsi_Associations
Pepsi_Associations_df = list_vect2df(Pepsi_Associations)[, 2:3]

ggplot(Pepsi_Associations_df, aes(y = Pepsi_Associations_df[, 1])) + 
  geom_point(aes(x = Pepsi_Associations_df[, 2],colour = Pepsi_Associations_df[, 2],
                 size = Pepsi_Associations_df[, 2] ), 
             data = Pepsi_Associations_df) + 
  theme_gdocs()+
  scale_color_continuous(high = '#18069A',low = "#8985A3")+
  guides(colour=FALSE,size =F)+
  xlab('Correlation')+ylab('Words')+ggtitle('Pepsi tweets Association with "like" ')
colnames(Pepsi_Associations_df) = c('Words','Corr')
qheat(Pepsi_Associations_df, values=TRUE, high="blue",
      digits=2, plot = FALSE) +
  coord_flip()+guides(fill=F,ylab=F)+
  xlab('Words')+ggtitle('Pepsi tweets HeatMap with "like"')


CocaCola_Associations = findAssocs(CocaCola_Corpus_TDM,terms ='like',corlimit=0.1)
CocaCola_Associations
CocaCola_Associations_df = list_vect2df(CocaCola_Associations)[, 2:3]

ggplot(CocaCola_Associations_df, aes(y = CocaCola_Associations_df[, 1])) + 
  geom_point(aes(x = CocaCola_Associations_df[, 2],colour = CocaCola_Associations_df[, 2],
                 size = CocaCola_Associations_df[, 2] ), 
             data = CocaCola_Associations_df) + 
  theme_gdocs()+
  scale_color_continuous(high = '#C40315',low = "#DE6C76")+
  guides(colour=FALSE,size =F)+
  xlab('Correlation')+ylab('Words')+ggtitle('CocaCola tweets Association with "like" ')
colnames(CocaCola_Associations_df) = c('Words','Corr')
qheat(CocaCola_Associations_df, values=TRUE, high="red",
      digits=2, plot = FALSE) +
  coord_flip()+guides(fill=F,ylab=F)+
  xlab('Words')+ggtitle('CocaCola tweets HeatMap with "like"')


CocaCola_Corpus_DTM <- as.DocumentTermMatrix(CocaCola_Corpus_TDM)
CrowTotals <- apply(CocaCola_Corpus_DTM , 1, sum)
CNullDocs <- CocaCola_Corpus_DTM[CrowTotals==0, ]
CocaCola_Corpus_DTM   <- CocaCola_Corpus_DTM[CrowTotals> 0, ]
if (length(CNullDocs$dimnames$Docs) > 0) {
  CocaCola_DF <- CocaCola_DF[-as.numeric(CNullDocs$dimnames$Docs),]
}
Clda <- LDA(CocaCola_Corpus_DTM, k = 5) 
Cterm <- terms(Clda, 5) 
Cterm
Ctopics<- topics(Clda)
Ctopics_df<- data.frame(date=(CocaCola_DF$created), topic = Ctopics)
X = cut(as.numeric(strftime(Ctopics_df$date, format="%H")),c(0,6,12,18,24))
levels(X) = c('night','morning','noon','evening')
qplot(date,..count.., data=Ctopics_df,
      geom ="density",
      fill= Cterm[topic],position="stack",
      xlab = 'Tweets Time', ylab='Count',main = 'Coca-Cola Topic Density')

Pepsi_Corpus_DTM <- as.DocumentTermMatrix(Pepsi_Corpus_TDM)
ProwTotals <- apply(Pepsi_Corpus_DTM , 1, sum)
PNullDocs <- Pepsi_Corpus_DTM[ProwTotals==0, ]
Pepsi_Corpus_DTM   <- Pepsi_Corpus_DTM[ProwTotals> 0, ]
if (length(PNullDocs$dimnames$Docs) > 0) {
  Pepsi_DF <- Pepsi_DF[-as.numeric(PNullDocs$dimnames$Docs),]
}
Plda <- LDA(Pepsi_Corpus_DTM, k = 5) 
Pterm <- terms(Plda, 5) 
Pterm
Ptopics<- topics(Plda)
Ptopics_df<- data.frame(date=(Pepsi_DF$created), topic = Ptopics)
X = cut(as.numeric(strftime(Ptopics_df$date, format="%H")),c(0,6,12,18,24))
levels(X) = c('night','morning','noon','evening')
qplot(X,..count.., data=Ptopics_df,
      geom ="density",
      fill= Pterm[topic],position="stack",
      xlab = 'Tweets Time', ylab='Count',main = 'Pepsi Topic Density')


colnames(Frequency_DF) = c('word','freq','brand')
sentiment_df = Frequency_DF %>% 
  inner_join(get_sentiments('bing'), by='word')
sentiment_df = sentiment_df %>%
  inner_join(get_sentiments('afinn'),by='word') 

sentiment_df %>%
  top_n(25,freq) %>%
  mutate(x1 = reorder(word,freq),
         y1 = ifelse(sentiment=='positive',freq,-freq)) %>%
  ggplot(aes(x=x1, y= y1, fill = brand)) +
  scale_fill_brewer(palette="Set1")+
  geom_col(stat='identity') +
  coord_flip()+
  facet_wrap(~sentiment, scales = "free_x")+
  ggtitle('Lexicon based word frequency')+
  xlab('Words')+ylab('Frequency')

Emotion_sentiment_df = Frequency_DF %>% 
  inner_join(get_sentiments('nrc'), by='word')
Emotion_sentiment_df %>%
  group_by(sentiment) %>%
  top_n(8,freq) %>%
  ungroup() %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(x = word, y = freq , fill  = brand)) +
  scale_fill_brewer(palette="Set1")+
  geom_col(show.legend = T) +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip()+
  ggtitle('Emotion based word frequency')+
  xlab('Words')+ylab('Frequency')




