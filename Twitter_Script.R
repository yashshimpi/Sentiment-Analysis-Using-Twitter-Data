library(twitteR)
library(plyr)
library(ggplot2)
library(doBy)
library(caret)
var
install.packages('doBy')
Consumer_Key <-	"########################"
Consumer_Secret <- "#######################"
Access_Token <- "###########################"
Access_Token_Secret <- "#######################"

delta.tweets = searchTwitter('@delta', n=1500)
american.tweets = searchTwitter('@american', n=1500)
southwest.tweets = searchTwitter('@southwest', n=1500)
spirit.tweets = searchTwitter('@spirit' , n=1500)
united.tweets = searchTwitter('@united' , n= 1500)


tweet <- delta.tweets[[1]]
class(tweet)
tweet$getScreenName()
tweet$getText()
hu.liu.pos = scan('F:/R programming/Twitter_Text_Mining/opinion-lexicon-English/positive-words.txt', what='character', comment.char=';')
hu.liu.neg = scan('F:/R programming/Twitter_Text_Mining/opinion-lexicon-English/negative-words.txt', what='character', comment.char=';')

pos.words = c(hu.liu.pos, 'upgrade')
neg.words = c(hu.liu.neg, 'wtf', 'wait',
              'waiting', 'epicfail', 'mechanical')
score.sentiment = function(sentences, pos.words, neg.words, .progress = 'None' )
{
  require(plyr)
  require(stringr)
  
  scores = laply(sentences , function(sentence, pos.words, neg.words)
  {
    sentence = gsub('[[:punct:]]', "", sentence)
    sentence = gsub('[[:cntrl:]]', "", sentence)
    sentence = gsub('\\d+', '', sentence)
    sentence = tolower(sentence)
    word.list = str_split(sentence , "\\s+")
    word = unlist(word.list)
    pos.matches = match(word, pos.words)
    neg.matches = match(word, neg.words)
    
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
    
  
  }, pos.words, neg.words, .progress = .progress)
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

sample = c("You're awesome and I love you",
           "I hate and hate and hate. So angry. Die!",
           "Impressed and amazed: you are peerless in your achievement of
           unparalleled mediocrity.")
result = score.sentiment(sample,pos.words, neg.words)
result$score
delta.text = laply(delta.tweets, function(t) t$getText() )
delta.scores = score.sentiment(delta.text, pos.words,neg.words, .progress='text')
american.text = laply(american.tweets,function(t) t$getText())
american.score = score.sentiment(american.text, pos.words , neg.words, .progress = 'text')
southwest.text = laply(southwest.tweets,function(t) t$getText())
southwest.score = score.sentiment(southwest.text, pos.words , neg.words, .progress = 'text')
united.text = laply(united.tweets,function(t) t$getText())
united.score = score.sentiment(united.text, pos.words , neg.words, .progress = 'text')
spirit.text = laply(spirit.tweets,function(t) t$getText())
spirit.score = score.sentiment(spirit.text, pos.words , neg.words, .progress = 'text')
delta.scores$airline = 'Delta'
delta.scores$code = 'DL’


hist(delta.scores$score)
ggplot2::qplot(delta.scores$score)

all.scores = rbind( american.score, united.score, delta.scores,
southwest.score,spirit.score)
american.score$airline = 'American'
american.score$code = 'AA'
united.score$airline = 'United'
united.score$code = 'UNA'
spirit.score$airline = 'Spirit'
spirit.score$code = 'SP'
southwest.score$airline = 'Southwest'
southwest.score$code = 'SW'
delta.scores$airline = 'Delta'
delta.scores$code = 'DL'

head(delta.scores , n= 5)
head(american.score , n =5)
head(spirit.score , n= 5)

ggplot(data=all.scores) + # ggplot works on data.frames, always
geom_bar(mapping=aes(x=score, fill=airline), binwidth=1) +
facet_grid(airline~.) + # make a separate plot for each airline
theme_bw() + scale_fill_brewer() # plain display, nicer colors

all.scores$very_postive = as.numeric(all.scores$score>= 2)
all.scores$very_negative = as.numeric(all.scores$score <= -2)

twitter.df = ddply(all.scores ,c('airline', 'code'), summarise,pos.count = sum( very_postive ), neg.count = sum( very_negative ) )
twitter.df$all.count = twitter.df$pos.count + twitter.df$neg.count
twitter.df$score = round( 100 * twitter.df$pos.count /twitter.df$all.count )
orderBy(~-score, twitter.df)



library(XML)
acsi.url = 'http://www.theacsi.org/index.php?option=com_content&view=article&id=147&catid=&Itemid=212&i=Airlines'
acsi.df = readHTMLTable(acsi.url, header=T, which=1,stringsAsFactors=F)
acsi.df = acsi.df[,c(1,24)]
head(acsi.df,1)
colnames(acsi.df) = c('airline', 'score')
acsi.df$code = c(NA,'SW',NA, NA, NA,'AA','DL','UA',NA,NA,'SP', NA, NA, NA)
acsi.df$score = as.numeric(acsi.df$score)
compare.df = merge(twitter.df, acsi.df, by='code', suffixes=c('.twitter', '.acsi'))
compare.df = subset(compare.df, all.count > 50)
ggplot( compare.df ) +
geom_point(aes(x=score.twitter,
y=score.acsi,
color=airline.twitter), size=5) +
geom_smooth(aes(x=score.twitter,
y=score.acsi, group=1), se=F,
method="lm") +
theme_bw() +
theme(legend.position=c(0.2,
0.85))

