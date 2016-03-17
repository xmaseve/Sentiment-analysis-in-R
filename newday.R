library(tm)
library(NLP)
library(plyr)
library(dplyr)
library(slam)
library(ggplot2)


#read csv
read=function(a){
  tweet=read.csv(a)
  tweet=select(tweet,1,2)
  day=as.Date(tweet$time)
  tweet=data.frame(day,tweet$text)
}  

tweet=read('amazon.csv')

day226=filter(tweet, day=='2016-02-26')
day227=filter(tweet, day=='2016-02-27')
day228=filter(tweet, day=='2016-02-28')
day229=filter(tweet, day=='2016-02-29')
day301=filter(tweet, day=='2016-03-01')
day302=filter(tweet, day=='2016-03-02')
day303=filter(tweet, day=='2016-03-03')
day304=filter(tweet, day=='2016-03-04')
day305=filter(tweet, day=='2016-03-05')
daynew=filter(tweet, day==Sys.Date())

#input positive and negtive words to R
pos = scan('positive-words.txt',
           what='character', comment.char=';')
neg = scan('negative-words.txt',
           what='character', comment.char=';')

#a function to score the corpus
scoreCorpus = function(text, pos, neg) {
  myCorpus = Corpus(VectorSource(text))
  myCorpus=tm_map(myCorpus,tolower)
  myCorpus=tm_map(myCorpus,removePunctuation)
  myCorpus=tm_map(myCorpus,removeNumbers)
  myCorpus=tm_map(myCorpus,removeWords,stopwords("english"))
  myCorpus=tm_map(myCorpus,stemDocument)
  myCorpus=tm_map(myCorpus,stripWhitespace)
  myCorpus=tm_map(myCorpus,PlainTextDocument)
  #termfreq_control = list(removePunctuation = TRUE,
  # stemming=FALSE, stopwords=TRUE, wordLengths=c(2,100))
  dtm = DocumentTermMatrix(myCorpus)
  # term frequency matrix
  tfidf = weightTfIdf(dtm)
  # identify positive terms
  which_pos = Terms(dtm) %in% pos
  
  # identify negative terms
  which_neg <- Terms(dtm) %in% neg
  
  # number of positive terms in each row
  score_pos = row_sums(dtm[, which_pos])
  # number of negative terms in each row
  score_neg = row_sums(dtm[, which_neg])
  # number of rows having positive score makes up the net score
  net_score = sum((score_pos - score_neg)>0)
  # length is the total number of instances in the corpus
  length = length(score_pos - score_neg)
  #score = net_score /length
  score = net_score/length
  return(score)
}

t226=scoreCorpus(day226$tweet.text,pos,neg)
t227=scoreCorpus(day227$tweet.text,pos,neg)
t228=scoreCorpus(day228$tweet.text,pos,neg)
t229=scoreCorpus(day229$tweet.text,pos,neg)
t301=scoreCorpus(day301$tweet.text,pos,neg)
t302=scoreCorpus(day302$tweet.text,pos,neg)
t303=scoreCorpus(day303$tweet.text,pos,neg)
t304=scoreCorpus(day304$tweet.text,pos,neg)
t305=scoreCorpus(day305$tweet.text,pos,neg)
tnew=scoreCorpus(daynew$tweet.text,pos,neg)

date=c('2016-02-26','2016-02-27','2016-02-28','2016-02-29','2016-03-01','2016-03-02',
       '2016-03-03','2016-03-04','2016-03-05')

date=append(date,toString(Sys.Date()))

score=c(t226,t227,t228,t229,t301,t302,t303,t304,t305)

score=append(score,tnew)

result=data.frame(date,score)

write.csv(result,file='output.csv')

result %>%
  ggplot(aes(x=date, y=score,group=1)) + 
  geom_point()+
  geom_line()+
  labs(title="Sentiment Analysis")

