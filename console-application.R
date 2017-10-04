setwd("F:\\Mini ProjeCT")
consumer_key <- 'FzbIehCHDCNeTr4067NsFFyeZ'
consumer_secret <- 'ogCVTjN9hdtm4Gm9wYVXryGjDAtxZzHVz2DOcGgFPpyeQsu98c'
access_token <- '4051479629-MRVtAGPKud989ps9kmIPUKhKDcwT62LZ3z94g0u'
access_secret <- 'QfZhK0YR0sSB3OBuMkJiINdDp0GloTg6U6dDJBvzuJgop'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

money.tweets<- searchTwitter("demonetisation", n=1500, lang="en")

df<- do.call("rbind", lapply(money.tweets, as.data.frame))

df$text <- sapply(df$text, function(row) iconv(row, "latin1", "ASCII", sub=""))
df$text = gsub("(f|ht)tp(s?)://(.*)(.)[a-z]+","",df$text)
sample<-df$text

pos.words = scan('positive-words.txt', what='character', comment.char=';')
neg.words = scan('negative-words.txt', what='character', comment.char=';')

os.words=c(pos.words, 'Congrats', 'prizes', 'prize', 'thanks', 'thnx', 'Grt', 'gr8', 'plz', 'trending', 'recovering', 'brainstorm', 'leader')
neg.words = c(neg.words, 'Fight', 'fighting', 'wtf', 'arrest', 'no', 'not')

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  list=lapply(sentences, function(sentence, pos.words, neg.words)
  {
    sentence = gsub('[[:punct:]]',' ',sentence)
    sentence = gsub('[[:cntrl:]]','',sentence)
    sentence = gsub('\\d+','',sentence)  #removes decimal number
    sentence = gsub('\n','',sentence)    #removes new lines
    sentence = tolower(sentence)
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)  #changes a list to character vector
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    pp = sum(pos.matches)
    nn = sum(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    list1 = c(score, pp, nn)
    return (list1)
  }, pos.words, neg.words)
  score_new = lapply(list, `[[`, 1)
  pp1 = lapply(list, `[[`, 2)
  nn1 = lapply(list, `[[`, 3)
  
  scores.df = data.frame(score = score_new, text=sentences)
  positive.df = data.frame(Positive = pp1, text=sentences)
  negative.df = data.frame(Negative = nn1, text=sentences)
  
  list_df = list(scores.df, positive.df, negative.df)
  return(list_df)
}

# Clean the tweets and returns merged data frame
result = score.sentiment(sample, pos.words, neg.words)

library(reshape)
test1=result[[1]]
test2=result[[2]]
test3=result[[3]]

#Creating three different data frames for Score, Positive and Negative
#Removing text column from data frame
test1$text=NULL
test2$text=NULL
test3$text=NULL
#Storing the first row(Containing the sentiment scores) in variable q
q1=test1[1,]
q2=test2[1,]
q3=test3[1,]
qq1=melt(q1, ,var='Score')
qq2=melt(q2, ,var='Positive')
qq3=melt(q3, ,var='Negative') 
qq1['Score'] = NULL
qq2['Positive'] = NULL
qq3['Negative'] = NULL
#Creating data frame
table1 = data.frame(Text=result[[1]]$text, Score=qq1)
table2 = data.frame(Text=result[[2]]$text, Score=qq2)
table3 = data.frame(Text=result[[3]]$text, Score=qq3)

#Merging three data frames into one
table_final=data.frame(Text=table1$Text, Score=table1$value, Positive=table2$value, Negative=table3$value)

head(table_final)

##########################Percentage############################

#Positive Percentage

#Renaming
posSc=table_final$Positive
negSc=table_final$Negative

#Adding column
table_final$PosPercent = posSc/ (posSc+negSc)

#Replacing Nan with zero
pp = table_final$PosPercent
pp[is.nan(pp)] <- 0
table_final$PosPercent = pp
#Negative Percentage

#Adding column
table_final$NegPercent = negSc/ (posSc+negSc)

#Replacing Nan with zero
nn = table_final$NegPercent
nn[is.nan(nn)] <- 0
table_final$NegPercent = nn

###########################Graphs#############################

#Histogram
hist(table_final$Positive, col=rainbow(10))
hist(table_final$Negative, col=rainbow(10))
hist(table_final$Score, col=rainbow(10))

#Pie
slices <- c(sum(table_final$Positive), sum(table_final$Negative))
labels <- c("Positive", "Negative")
library(plotrix)
#pie(slices, labels = labels, col=rainbow(length(labels)), main="Sentiment Analysis")
pie3D(slices, labels = labels, col=rainbow(length(labels)),explode=0.00, main="Sentiment Analysis")

##############Wordcloud##################

money_text = sapply(money.tweets, function(x) x$getText()) #sapply returns a vector 
df <- do.call("rbind", lapply(money.tweets, as.data.frame)) #lapply returns a list
money_text <- sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
str(money_text) #gives the summary/internal structure of an R object

library(tm) #tm: text mining
money_corpus <- Corpus(VectorSource(money_text)) #corpus is a collection of text documents
money_corpus
inspect(money_corpus[1])

#clean text
library(wordcloud)
money_clean <- tm_map(money_corpus, removePunctuation)
money_clean <- tm_map(money_clean, removeWords, stopwords("english"))
money_clean <- tm_map(money_clean, removeNumbers)
money_clean <- tm_map(money_clean, stripWhitespace)
wordcloud(money_clean, random.order=F,max.words=80, col=rainbow(50), scale=c(3.5,1))


a_trends = availableTrendLocations()
woeid = a_trends[which(a_trends$name=="Chennai"),3]
india_trend = getTrends(woeid)
View(india_trend)
trends = india_trend[1:2]

#To clean data and remove Non English words: 
dat <- cbind(trends$name)
dat2 <- unlist(strsplit(dat, split=", "))
dat3 <- grep("dat2", iconv(dat2, "latin1", "ASCII", sub="dat2"))
dat4 <- dat2[-dat3]


#################top 10hashtags #####################

library(twitteR)
tw = userTimeline("BarackObama", n = 3200)
tw = twListToDF(tw)
vec1 = tw$text

#Extract the hashtags:

hash.pattern = "#[[:alpha:]]+"
have.hash = grep(x = vec1, pattern = hash.pattern) #stores the indices of the tweets which have hashes

hash.matches = gregexpr(pattern = hash.pattern,
                        text = vec1[have.hash])
extracted.hash = regmatches(x = vec1[have.hash], m = hash.matches) #the actual hashtags are stored here

df = data.frame(table(tolower(unlist(extracted.hash)))) #dataframe formed with var1(hashtag), freq of hashtag
colnames(df) = c("tag","freq")
df = df[order(df$freq,decreasing = TRUE),]


dat = head(df,50)
dat2 = transform(dat,tag = reorder(tag,freq)) #reorder it so that highest freq is at the top


library(ggplot2)

p = ggplot(dat2, aes(x = tag, y = freq)) + geom_bar(stat="identity", fill = "blue")
p + coord_flip() + labs(title = "Hashtag frequencies in the tweets of the Obama team (@BarackObama)")
