# Sentiment-Analysis-NA246
#Sentiment analysis of twitter for NA 246 bypoll elections in Karachi


library(twitteR)
library(sentiment)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(httr)
library(ROAuth)

oauth_endpoints("twitter")

api_key <- "*********************************"
api_secret <- "***************************************************"
access_token <- "***************************************************************"
access_token_secret <- "***************************************************************************"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

#PTI
kword1 = searchTwitter("PTI+246",n=1500,since="2015-03-22")
#convert to data frame
kword1 <- twListToDF(kword1)
#get only text
kword1 <- kword1$text
kword2 <- searchTwitter("@PTIofficial",n=1500,since="2015-03-22")
kword2 <- twListToDF(kword2)
#get only text
kword2 <- kword2$text
#remove duplicate tweets
kword2 <- kword2[!kword2 %in% kword1]
kword3 <- searchTwitter("#NA246+pti",n=1500,since="2015-03-22")
kword3 <- twListToDF(kword3)
#get only text
kword3 <- kword3$text
#remove duplicate tweets

kword3 <- kword3[!kword3 %in% kword1]
kword3 <- kword3[!kword3 %in% kword2]


kword4 <- searchTwitter("246+imran",n=1500,since="2015-03-22")
kword4 <- twListToDF(kword4)
#get only text
kword4 <- kword4$text
#remove duplicate tweets
kword4 <- kword4[!kword4 %in% kword1]
kword4 <- kword4[!kword4 %in% kword2]
kword4 <- kword4[!kword4 %in% kword3]
kword5 <- searchTwitter("imran+karachi",n=1500,since="2015-03-22")
kword5 <- twListToDF(kword5)
#get only text
kword5 <- kword5$text
#remove duplicate tweets
kword5 <- kword5[!kword5 %in% kword1]
kword5 <- kword5[!kword5 %in% kword2]
kword5 <- kword5[!kword5 %in% kword3]
kword5 <- kword5[!kword5 %in% kword4]
kword6 <- searchTwitter("@ImranKhanPTI,",n=1500,since="2015-03-22")
kword6 <- twListToDF(kword6)
#get only text
kword6 <- kword6$text
#remove duplicate tweets
kword6 <- kword6[!kword6 %in% kword1]
kword6 <- kword6[!kword6 %in% kword2]
kword6 <- kword6[!kword6 %in% kword3]
kword6 <- kword6[!kword6 %in% kword4]
kword6 <- kword6[!kword6 %in% kword5]
kword7 <- searchTwitter("ik+pti",n=1500,since="2015-03-22")
kword7 <- twListToDF(kword7)
#get only text
kword7 <- kword7$text
#remove duplicate tweets
kword7 <- kword7[!kword7 %in% kword1]
kword7 <- kword7[!kword7 %in% kword2]
kword7 <- kword7[!kword7 %in% kword3]
kword7 <- kword7[!kword7 %in% kword4]
kword7 <- kword7[!kword7 %in% kword5]
kword7 <- kword7[!kword7 %in% kword6]
#concatenate all into one
pti_tweets<-c(kword1,kword2,kword3,kword4,kword5,kword6,kword7)


write.csv(pti_tweets, "pti_tweets.csv", row.names=F)


# remove all unnecessary charaters from the tweets
pti_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", pti_tweets)
# remove '@ people'
pti_txt = gsub("@\\w+", "", pti_txt)
# remove punctuation
pti_txt = gsub("[[:punct:]]", "", pti_txt)
# remove numbers
pti_txt = gsub("[[:digit:]]", "", pti_txt)
# remove html links
pti_txt = gsub("http\\w+", "", pti_txt)
# remove unnecessary spaces
pti_txt = gsub("[ \t]{2,}", "", pti_txt)
pti_txt = gsub("^\\s+|\\s+$", "", pti_txt)


# convert to lowercase
# define "tolower error handling" function
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x),
                       error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}
#lower case using try.error with sapply
pti_txt = sapply(pti_txt, try.error)
#remove stopwords
pti_txt <- pti_txt[!pti_txt %in% stopwords("SMART")]
# remove NAs in pti_txt
pti_txt = pti_txt[!is.na(pti_txt)]
names(pti_txt) = NULL



#you can also change the algorithm here to 'voter'. Try it and see the results
class_pol = classify_polarity(pti_txt, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]
# classify emotion
class_emo = classify_emotion(pti_txt, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"



#put all results into a dataframe
pti_df = data.frame(text=pti_txt,
                    emotion=emotion,
                    polarity=polarity,
                    Candidate=rep("pti",length(pti_txt)),
                    stringsAsFactors=FALSE)
# sort data frame
pti_df = within(pti_df,
                emotion <- factor(emotion,
                                  levels=names(sort(table(emotion),decreasing=TRUE))))



#thanks to Jeffrey Bean for this function!
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores = laply(sentences, function(sentence, pos.words, neg.words)
  {
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

#load lexicons
subjectivity <- read.csv(system.file("data/subjectivity.csv.gz",
                                     package="sentiment"),
                         header=F,stringsAsFactors=F)
names(subjectivity)<-c("word","strength","polarity")
pos.words <- read.table("Z:/Ferguson 13 march/other assignment/DS/NA246-sentiment analysis/Lexicon/Opinion/positive-words.txt", stringsAsFactors=F, skip=35)
pos.words[,2] <- rep("subj", nrow(pos.words))
pos.words[,3] <- rep("positive", nrow(pos.words))
names(pos.words)<-names(subjectivity)
neg.words <- read.table("Z:/Ferguson 13 march/other assignment/DS/NA246-sentiment analysis/Lexicon/Opinion/negative-words.txt", stringsAsFactors=F, skip=35)
neg.words[,2] <- rep("subj", nrow(neg.words))
neg.words[,3] <- rep("negative", nrow(neg.words))
names(neg.words)<-names(subjectivity)
#merge
subjectivity <- rbind(subjectivity,pos.words,neg.words)

# sentiment score
pti = score.sentiment(pti_txt,
                      subjectivity$word[subjectivity$polarity=="positive"],
                      subjectivity$word[subjectivity$polarity=="negative"])
pti$Candidate = rep("pti",nrow(pti))

#******************************************************************************
  #MQM
  kword8 = searchTwitter("mqm+246",n=1500,since="2015-03-22")
  #convert to data frame
  kword8 <- twListToDF(kword8)
  #get only text
  kword8 <- kword8$text
  kword9 <- searchTwitter("#MQM",n=1500,since="2015-03-22")
  kword9 <- twListToDF(kword9)
  #get only text
  kword9 <- kword9$text
  #remove duplicate tweets
  kword9 <- kword9[!kword9 %in% kword8]
  kword10 <- searchTwitter("@OfficialMqm",n=1500,since="2015-03-22")
  kword10 <- twListToDF(kword10)
  #get only text
  kword10 <- kword10$text
  #remove duplicate tweets
  
  kword10 <- kword10[!kword10 %in% kword8]
  kword10 <- kword10[!kword10 %in% kword9]
  
  
  kword11 <- searchTwitter("#WeAreHaqParast",n=1500,since="2015-03-22")
  kword11 <- twListToDF(kword11)
  #get only text
  kword11 <- kword11$text
  #remove duplicate tweets
  kword11 <- kword11[!kword11 %in% kword8]
  kword11 <- kword11[!kword11 %in% kword9]
  kword11 <- kword11[!kword11 %in% kword10]
  kword12 <- searchTwitter("#AltafHussain",n=1500,since="2015-03-22")
  kword12 <- twListToDF(kword12)
  #get only text
  kword12 <- kword12$text
  #remove duplicate tweets
  kword12 <- kword12[!kword12 %in% kword8]
  kword12 <- kword12[!kword12 %in% kword9]
  kword12 <- kword12[!kword12 %in% kword10]
  kword12 <- kword12[!kword12 %in% kword11]
  kword13 <- searchTwitter("Imran+Farooq",n=1500,since="2015-03-22")
  kword13 <- twListToDF(kword13)
  #get only text
  kword13 <- kword13$text
  #remove duplicate tweets
  kword13 <- kword13[!kword13 %in% kword8]
  kword13 <- kword13[!kword13 %in% kword9]
  kword13 <- kword13[!kword13 %in% kword10]
  kword13 <- kword13[!kword13 %in% kword11]
  kword13 <- kword13[!kword13 %in% kword12]
  kword14 <- searchTwitter("altaf+246",n=1500,since="2015-03-22")
  kword14 <- twListToDF(kword14)
  #get only text
  kword14 <- kword14$text
  #remove duplicate tweets
  kword14 <- kword14[!kword14 %in% kword8]
  kword14 <- kword14[!kword14 %in% kword9]
  kword14 <- kword14[!kword14 %in% kword10]
  kword14 <- kword14[!kword14 %in% kword11]
  kword14 <- kword14[!kword14 %in% kword12]
  kword14 <- kword14[!kword14 %in% kword13]
  
  kword15 <- searchTwitter("mqm",n=1500,since="2015-03-22")
  kword15 <- twListToDF(kword15)
  #get only text
  kword15 <- kword15$text
  #remove duplicate tweets
  kword15 <- kword15[!kword15 %in% kword8]
  kword15 <- kword15[!kword15 %in% kword9]
  kword15 <- kword15[!kword15 %in% kword10]
  kword15 <- kword15[!kword15 %in% kword11]
  kword15 <- kword15[!kword15 %in% kword12]
  kword15 <- kword15[!kword15 %in% kword13]
  kword15 <- kword15[!kword15 %in% kword14]
  #concatenate all into one
  #concatenate all into one
  mqm_tweets<-c(kword8,kword9,kword10,kword11,kword12,kword13,kword14,kword15)
  
  write.csv(mqm_tweets, "mqm_tweets.csv", row.names=F)
  
  # remove all unnecessary charaters from the tweets
  mqm_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", mqm_tweets)
  # remove '@ people'
  mqm_txt = gsub("@\\w+", "", mqm_txt)
  # remove punctuation
  mqm_txt = gsub("[[:punct:]]", "", mqm_txt)
  # remove numbers
  mqm_txt = gsub("[[:digit:]]", "", mqm_txt)
  # remove html links
  mqm_txt = gsub("http\\w+", "", mqm_txt)
  # remove unnecessary spaces
  mqm_txt = gsub("[ \t]{2,}", "", mqm_txt)
  mqm_txt = gsub("^\\s+|\\s+$", "", mqm_txt)
  
  
  #lower case using try.error with sapply
  mqm_txt = sapply(mqm_txt, try.error)
  #remove stopwords
  mqm_txt <- mqm_txt[!mqm_txt %in% stopwords("SMART")]
  # remove NAs in mqm_txt
  mqm_txt = mqm_txt[!is.na(mqm_txt)]
  names(mqm_txt) = NULL
  
  #you can also change the algorithm here to 'voter'. Try it and see the results
  class_pol = classify_polarity(mqm_txt, algorithm="bayes")
  # get polarity best fit
  polarity = class_pol[,4]
  # classify emotion
  class_emo = classify_emotion(mqm_txt, algorithm="bayes", prior=1.0)
  # get emotion best fit
  emotion = class_emo[,7]
  # substitute NA's by "unknown"
  emotion[is.na(emotion)] = "unknown"
  
  
  #put all results into a dataframe
  mqm_df = data.frame(text=mqm_txt,
                      emotion=emotion,
                      polarity=polarity,
                      Candidate=rep("mqm",length(mqm_txt)),
                      stringsAsFactors=FALSE)
  # sort data frame
  mqm_df = within(mqm_df,
                  emotion <- factor(emotion,
                                    levels=names(sort(table(emotion),decreasing=TRUE))))
  
  # sentiment score
  mqm = score.sentiment(mqm_txt,
                        subjectivity$word[subjectivity$polarity=="positive"],
                        subjectivity$word[subjectivity$polarity=="negative"])
  mqm$Candidate = rep("mqm",nrow(mqm))
  
  
  
  
******************************************************************************  
sentiments <- rbind(pti_df,mqm_df)
results <- rbind(pti,mqm)

#polarity
ggplot(sentiments, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity), position="dodge") +
  scale_fill_brewer(palette="Dark2") +
  labs(x="Polarity categories", y="Number of Tweets") +
  facet_grid(.~Candidate)
#Emotions
ggplot(sentiments, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion), position="dodge") +
  scale_fill_brewer(palette="Dark2") +
  labs(x="Emotion categories", y="Number of Tweets", title="Emotions Evoked") +
  facet_grid(.~Candidate)
#Total/Cumulative Score
total_pti <- data.frame(score=sum(pti$score),
                        Candidate=rep("pti",length(sum(pti$score))))

total_mqm <- data.frame(score=sum(mqm$score),
                        Candidate=rep("mqm",length(sum(mqm$score))))




#plot!
ggplot() +
  geom_bar(data=total_pti,
           mapping=aes(x=Candidate, y=score),
           binwidth=10, position="dodge",stat="identity", fill="red") +
  geom_bar(data=total_mqm, mapping=aes(x=Candidate, y=score),
           binwidth=10, position="dodge",stat="identity", fill="green") +
  labs(x="Candidate", y="Score", title="Total Sentiment Scores Tallied")

pti_emos = levels(factor(pti_df$emotion))
n_pti_emos = length(pti_emos)
pti.emo.docs = rep("", n_pti_emos)

for (i in 1:n_pti_emos)
{
  tmp = pti_txt[emotion == pti_emos[i]]
  pti.emo.docs[i] = paste(tmp, collapse=" ")
}

pti.emo.docs = removeWords(pti.emo.docs, stopwords("english"))
pti.corpus = Corpus(VectorSource(pti.emo.docs))
pti.tdm = TermDocumentMatrix(pti.corpus)
pti.tdm = as.matrix(pti.tdm)
colnames(pti.tdm) = pti_emos



mqm_emos = levels(factor(mqm_df$emotion))
n_mqm_emos = length(mqm_emos)
mqm.emo.docs = rep("", n_mqm_emos)

for (i in 1:n_mqm_emos)
{
  tmp = mqm_txt[emotion == mqm_emos[i]]
  mqm.emo.docs[i] = paste(tmp, collapse=" ")
}

mqm.emo.docs = removeWords(mqm.emo.docs, stopwords("english"))
mqm.corpus = Corpus(VectorSource(mqm.emo.docs))
mqm.tdm = TermDocumentMatrix(mqm.corpus)
mqm.tdm = as.matrix(mqm.tdm)
colnames(mqm.tdm) = mqm_emos
#word cloud for mqm
comparison.cloud(mqm.tdm, colors = brewer.pal(n_mqm_emos, "Dark2"),
                 scale = c(3,.5), random.order = FALSE, title.size = 1.5)
#word cloud for pti  
comparison.cloud(pti.tdm, colors = brewer.pal(n_pti_emos, "Dark2"),
                   scale = c(3,.5), random.order = FALSE, title.size = 1.5)
  
