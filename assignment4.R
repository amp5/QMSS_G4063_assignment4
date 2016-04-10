library(sp)
library(plyr)
library(RCurl)
library(bitops)
library(rjson)
library(ggplot2)
library(grid)

library(Rstem)
library(tm)
library(NLP)
library(stringr)
library(RColorBrewer)
library(wordcloud)
library(streamR)

gpclibPermit()


setwd("/Users/alexandraplassaras/Desktop/Columbia_Courses/Spring_2016/QMSS_G4063/QMSS_G4063_Data_Visualization/assignment4")
load("ParsedTweets.Rdata")

tweets_all <- rbind(t02092016_df, 
                    t02202016_df, 
                    t02232016_df, 
                    t02272016_df, 
                    t03012016_df, 
                    t03052016_df, 
                    t03062016_df, 
                    t03082016_df, 
                    t03152016_df, 
                    t03212016_df)

tweets_all$text <- sapply(tweets_all$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
#twts <- tweets_all$text
#class(twts)
#View(twts)


#Clinton, Cruz, Rubio, Sanders, Trump
HC_all <- subset (tweets_all, grepl(pattern =  "Clinton | 
                                    clinton | Hillary | 
                                    hillary | Hillaryclinton | 
                                    hillaryclinton | Hillary Clinton | 
                                    hillary clinton" , 
                                    tweets_all$text, ignore.case = TRUE))

BS_all <- subset (tweets_all, grepl(pattern =  "Berniesanders | berniesanders | 
                                    Bernie Sanders  | bernie sanders | Bernie | 
                                    bernie | Sensanders | sensanders" , 
                                    tweets_all$text, ignore.case = TRUE))

TC_all <-  subset (tweets_all, grepl(pattern =  "Cruz | cruz | Ted | ted | 
                                     Tedcruz | tedcruz | Ted Cruz | ted cruz" , 
                                     tweets_all$text, ignore.case = TRUE))

DT_all <- subset (tweets_all, grepl(pattern =  "Donaldtrump  | donaldtrump | 
                                    Donald Trump | donald trump | Trump | trump | 
                                    Donald | donald | Trumpf | trumpf" , 
                                    tweets_all$text, ignore.case = TRUE))



dem_all <- rbind(HC_all, BS_all)
rep_all <- rbind(TC_all, DT_all)

#View(dem_all)

useful_info <- c("text", "created_at", "screen_name")
## First section - separate by political party
all_twts <- tweets_all[useful_info]

dem <-  dem_all[useful_info]
rep <- rep_all[useful_info]

# Second section - separate by political candidate
HC <- HC_all[useful_info]
BS <- BS_all[useful_info]
TC <- TC_all[useful_info]
DT <- DT_all[useful_info]



save (HC, file= 'HC.Rdata')
save (BS, file= 'BS.Rdata')
save (TC, file= 'TC.Rdata')
save (DT, file= 'DT.Rdata')
save (dem, file= 'dem.Rdata')
save (rep, file= 'rep.Rdata')
save (all_twts, file= 'alltwts.Rdata')


########### After R crahses, start here:
# processing all these very large files. Will not use in future will just load old folders
load("alltwts.Rdata")
load("HC.Rdata")
load("BS.Rdata")
load("TC.Rdata")
load("DT.Rdata")
load("dem.Rdata")
load("rep.Rdata")


lexicon <- read.csv("lexicon_ps.csv", stringsAsFactors=F)
econ.words <- lexicon$word[lexicon$polarity=="economy"]
imm.words <- lexicon$word[lexicon$polarity=="immigration"]
health.words <- lexicon$word[lexicon$polarity=="health_care"]
military.words <- lexicon$word[lexicon$polarity=="military"]
gun.words <- lexicon$word[lexicon$polarity=="gun_control"]
china.words <- lexicon$word[lexicon$polarity=="china"]
trade.words <- lexicon$word[lexicon$polarity=="trade"]
race.words <- lexicon$word[lexicon$polarity=="race"]
climate.words <- lexicon$word[lexicon$polarity=="climate_change"]
religion.words <- lexicon$word[lexicon$polarity=="religion"]


### All Tweet Corpus
TweetCorpus <- paste(unlist(tweets_all$text), collapse =" ") #to get all of the tweets together
TweetCorpus <- Corpus(VectorSource(TweetCorpus))
TweetCorpus <- tm_map(TweetCorpus, PlainTextDocument)
TweetCorpus <- tm_map(TweetCorpus, removePunctuation)
TweetCorpus <- tm_map(TweetCorpus, removeWords, stopwords('english'))
TweetCorpus <- tm_map(TweetCorpus, content_transformer(tolower),lazy=TRUE)
TweetCorpus <- tm_map(TweetCorpus, PlainTextDocument)
wordcloud(TweetCorpus, max.words = 100, random.order = FALSE)

### HC Tweet Corpus
TweetCorpusHC <- paste(unlist(HC$text), collapse =" ") #to get all of the tweets together
TweetCorpusHC <- Corpus(VectorSource(TweetCorpusHC))
TweetCorpusHC <- tm_map(TweetCorpusHC, PlainTextDocument)
TweetCorpusHC <- tm_map(TweetCorpusHC, removePunctuation)
TweetCorpusHC <- tm_map(TweetCorpusHC, removeWords, stopwords('english'))
TweetCorpusHC <- tm_map(TweetCorpusHC, content_transformer(tolower),lazy=TRUE)
TweetCorpusHC <- tm_map(TweetCorpusHC, PlainTextDocument)
wordcloud(TweetCorpusHC, max.words = 100, random.order = FALSE)

### BS Tweet Corpus
TweetCorpusBS <- paste(unlist(BS$text), collapse =" ") #to get all of the tweets together
TweetCorpusBS <- Corpus(VectorSource(TweetCorpusBS))
TweetCorpusBS <- tm_map(TweetCorpusBS, PlainTextDocument)
TweetCorpusBS <- tm_map(TweetCorpusBS, removePunctuation)
TweetCorpusBS <- tm_map(TweetCorpusBS, removeWords, stopwords('english'))
TweetCorpusBS <- tm_map(TweetCorpusBS, content_transformer(tolower),lazy=TRUE)
TweetCorpusBS <- tm_map(TweetCorpusBS, PlainTextDocument)
wordcloud(TweetCorpusBS, max.words = 100, random.order = FALSE)


### TC Tweet Corpus
TweetCorpusTC <- paste(unlist(TC$text), collapse =" ") #to get all of the tweets together
TweetCorpusTC <- Corpus(VectorSource(TweetCorpusTC))
TweetCorpusTC <- tm_map(TweetCorpusTC, PlainTextDocument)
TweetCorpusTC <- tm_map(TweetCorpusTC, removePunctuation)
TweetCorpusTC <- tm_map(TweetCorpusTC, removeWords, stopwords('english'))
TweetCorpusTC <- tm_map(TweetCorpusTC, content_transformer(tolower),lazy=TRUE)
TweetCorpusTC <- tm_map(TweetCorpusTC, PlainTextDocument)
wordcloud(TweetCorpusTC, max.words = 100, random.order = FALSE)


### DT Tweet Corpus
TweetCorpusDT <- paste(unlist(DT$text), collapse =" ") #to get all of the tweets together
TweetCorpusDT <- Corpus(VectorSource(TweetCorpusDT))
TweetCorpusDT <- tm_map(TweetCorpusDT, PlainTextDocument)
TweetCorpusDT <- tm_map(TweetCorpusDT, removePunctuation)
TweetCorpusDT <- tm_map(TweetCorpusDT, removeWords, stopwords('english'))
TweetCorpusDT <- tm_map(TweetCorpusDT, content_transformer(tolower),lazy=TRUE)
TweetCorpusDT <- tm_map(TweetCorpusDT, PlainTextDocument)
wordcloud(TweetCorpusDT, max.words = 100, random.order = FALSE)


### DEM Tweet Corpus
TweetCorpusD <- paste(unlist(dem$text), collapse =" ") #to get all of the tweets together
TweetCorpusD <- Corpus(VectorSource(TweetCorpusD))
TweetCorpusD <- tm_map(TweetCorpusD, PlainTextDocument)
TweetCorpusD <- tm_map(TweetCorpusD, removePunctuation)
TweetCorpusD <- tm_map(TweetCorpusD, removeWords, stopwords('english'))
TweetCorpusD <- tm_map(TweetCorpusD, content_transformer(tolower),lazy=TRUE)
TweetCorpusD <- tm_map(TweetCorpusD, PlainTextDocument)
wordcloud(TweetCorpusD, max.words = 100, random.order = FALSE)



### REP Tweet Corpus
TweetCorpusR <- paste(unlist(rep$text), collapse =" ") #to get all of the tweets together
TweetCorpusR <- Corpus(VectorSource(TweetCorpusR))
TweetCorpusR <- tm_map(TweetCorpusR, PlainTextDocument)
TweetCorpusR <- tm_map(TweetCorpusR, removePunctuation)
TweetCorpusR <- tm_map(TweetCorpusR, removeWords, stopwords('english'))
TweetCorpusR <- tm_map(TweetCorpusR, content_transformer(tolower),lazy=TRUE)
TweetCorpusR <- tm_map(TweetCorpusR, PlainTextDocument)
wordcloud(TweetCorpusR, max.words = 100, random.order = FALSE)



all_econ <- sum(str_count(TweetCorpus, econ.words))
all_imm <- sum(str_count(TweetCorpus, imm.words))
all_health <- sum(str_count(TweetCorpus, health.words))
all_military <- sum(str_count(TweetCorpus, military.words))
all_gun <- sum(str_count(TweetCorpus, gun.words))
all_china <- sum(str_count(TweetCorpus, china.words))
all_trade <- sum(str_count(TweetCorpus, trade.words))
all_race <- sum(str_count(TweetCorpus, race.words))
all_climate <- sum(str_count(TweetCorpus, climate.words))
all_religion <- sum(str_count(TweetCorpus, religion.words))



HC_econ <- sum(str_count(TweetCorpusHC, econ.words))
HC_imm <- sum(str_count(TweetCorpusHC, imm.words))
HC_health <- sum(str_count(TweetCorpusHC, health.words))
HC_military <- sum(str_count(TweetCorpusHC, military.words))
HC_gun <- sum(str_count(TweetCorpusHC, gun.words))
HC_china <- sum(str_count(TweetCorpusHC, china.words))
HC_trade <- sum(str_count(TweetCorpusHC, trade.words))
HC_race <- sum(str_count(TweetCorpusHC, race.words))
HC_climate <- sum(str_count(TweetCorpusHC, climate.words))
HC_religion <- sum(str_count(TweetCorpusHC, religion.words))

HC_df = data.frame(HC_econ, 
             HC_imm, 
             HC_health, 
             HC_military, 
             HC_gun, 
             HC_china,
             HC_trade,
             HC_race, 
             HC_climate, 
             HC_religion)
write.csv(HC_df, file = "HC_topics.csv")


BS_econ <- sum(str_count(TweetCorpusBS, econ.words))
BS_imm <- sum(str_count(TweetCorpusBS, imm.words))
BS_health <- sum(str_count(TweetCorpusBS, health.words))
BS_military <- sum(str_count(TweetCorpusBS, military.words))
BS_gun <- sum(str_count(TweetCorpusBS, gun.words))
BS_china <- sum(str_count(TweetCorpusBS, china.words))
BS_trade <- sum(str_count(TweetCorpusBS, trade.words))
BS_race <- sum(str_count(TweetCorpusBS, race.words))
BS_climate <- sum(str_count(TweetCorpusBS, climate.words))
BS_religion <- sum(str_count(TweetCorpusBS, religion.words))

BS_df = data.frame(BS_econ, 
             BS_imm, 
             BS_health, 
             BS_military, 
             BS_gun, 
             BS_china,
             BS_trade,
             BS_race, 
             BS_climate, 
             BS_religion)
write.csv(BS_df, file = "BS_topics.csv")

TC_econ <- sum(str_count(TweetCorpusTC, econ.words))
TC_imm <- sum(str_count(TweetCorpusTC, imm.words))
TC_health <- sum(str_count(TweetCorpusTC, health.words))
TC_military <- sum(str_count(TweetCorpusTC, military.words))
TC_gun <- sum(str_count(TweetCorpusTC, gun.words))
TC_china <- sum(str_count(TweetCorpusTC, china.words))
TC_trade <- sum(str_count(TweetCorpusTC, trade.words))
TC_race <- sum(str_count(TweetCorpusTC, race.words))
TC_climate <- sum(str_count(TweetCorpusTC, climate.words))
TC_religion <- sum(str_count(TweetCorpusTC, religion.words))

TC_df = data.frame(TC_econ, 
             TC_imm, 
             TC_health, 
             TC_military, 
             TC_gun, 
             TC_china,
             TC_trade,
             TC_race, 
             TC_climate, 
             TC_religion)
write.csv(TC_df, file = "TC_topics.csv")


DT_econ <- sum(str_count(TweetCorpusDT, econ.words))
DT_imm <- sum(str_count(TweetCorpusDT, imm.words))
DT_health <- sum(str_count(TweetCorpusDT, health.words))
DT_military <- sum(str_count(TweetCorpusDT, military.words))
DT_gun <- sum(str_count(TweetCorpusDT, gun.words))
DT_china <- sum(str_count(TweetCorpusDT, china.words))
DT_trade <- sum(str_count(TweetCorpusDT, trade.words))
DT_race <- sum(str_count(TweetCorpusDT, race.words))
DT_climate <- sum(str_count(TweetCorpusDT, climate.words))
DT_religion <- sum(str_count(TweetCorpusDT, religion.words))

DT_df = data.frame(DT_econ, 
             DT_imm, 
             DT_health, 
             DT_military, 
             DT_gun, 
             DT_china,
             DT_trade,
             DT_race, 
             DT_climate, 
             DT_religion)
write.csv(DT_df, file = "DT_topics.csv")

D_econ <- sum(str_count(TweetCorpusD, econ.words))
D_imm <- sum(str_count(TweetCorpusD, imm.words))
D_health <- sum(str_count(TweetCorpusD, health.words))
D_military <- sum(str_count(TweetCorpusD, military.words))
D_gun <- sum(str_count(TweetCorpusD, gun.words))
D_china <- sum(str_count(TweetCorpusD, china.words))
D_trade <- sum(str_count(TweetCorpusD, trade.words))
D_race <- sum(str_count(TweetCorpusD, race.words))
D_climate <- sum(str_count(TweetCorpusD, climate.words))
D_religion <- sum(str_count(TweetCorpusD, religion.words))


D_df = data.frame(D_econ, 
                   D_imm, 
                   D_health, 
                   D_military, 
                   D_gun, 
                   D_china,
                   D_trade,
                   D_race, 
                   D_climate, 
                   D_religion)
write.csv(D_df, file = "D_topics.csv")







R_econ <- sum(str_count(TweetCorpusR, econ.words))
R_imm <- sum(str_count(TweetCorpusR, imm.words))
R_health <- sum(str_count(TweetCorpusR, health.words))
R_military <- sum(str_count(TweetCorpusR, military.words))
R_gun <- sum(str_count(TweetCorpusR, gun.words))
R_china <- sum(str_count(TweetCorpusR, china.words))
R_trade <- sum(str_count(TweetCorpusR, trade.words))
R_race <- sum(str_count(TweetCorpusR, race.words))
R_climate <- sum(str_count(TweetCorpusR, climate.words))
R_religion <- sum(str_count(TweetCorpusR, religion.words))

R_df = data.frame(R_econ, 
                  R_imm, 
                  R_health, 
                  R_military, 
                  R_gun, 
                  R_china,
                  R_trade,
                  R_race, 
                  R_climate, 
                  R_religion)
write.csv(R_df, file = "R_topics.csv")


###### After running rediculously

HC_df = read.csv("HC_topics.csv")
BS_df = read.csv("BS_topics.csv")
TC_df = read.csv("TC_topics.csv")
DT_df = read.csv("DT_topics.csv")
dem_df = read.csv("D_topics.csv")
rep_df = read.csv("R_topics.csv")


dem_compared = read.csv("dem_compare.csv") 

ggplot(data=dem_compared, aes(x=term, y=rate, fill=name)) +
  geom_bar(stat="identity", position=position_dodge())  +
  scale_fill_brewer() +
  ggtitle("Rate of Topics Per Candidate")



rep_compared = read.csv("rep_compare.csv") 

ggplot(data=rep_compared, aes(x=term, y=rate, fill=name)) +
  geom_bar(stat="identity", position=position_dodge())  +
  scale_fill_manual(values=c("#FF9999", "#FF6666")) +
  ggtitle("Rate of Topics Per Candidate")


party_compared = read.csv("party_compare.csv") 

ggplot(data=party_compared, aes(x=term, y=rate, fill=party)) +
  geom_bar(stat="identity", position=position_dodge())  +
  scale_fill_manual(values=c("#6699FF", "#FF6666")) +
  ggtitle("Rate of Topics Per Candidate")








load("wide.Rda")


############# Below code does not work. I attempted to create       ####################
############# > classifier(text, pos.words, neg.words)              ####################
############# 42 tweets: 19 % positive, 4 % negative, 77 % neutral  ####################
############# But I was unable to                                   ####################


economy <- sum(TweetCorpus %in% econ.words)
immigration <- sum(TweetCorpus %in% imm.words)
health_care <- sum(TweetCorpus %in% health.words)
military <- sum(TweetCorpus %in% military.words)
gun_control <- sum(TweetCorpus %in% gun.words)
china <- sum(TweetCorpus %in% china.words)
trade <- sum(TweetCorpus %in% trade.words)
race <- sum(TweetCorpus %in% race.words)
climate_change <- sum(TweetCorpus %in% climate.words)
religion <- sum(TweetCorpus %in% religion.words)

clean_tweets <- function(text){
  # loading required packages
  lapply(c("tm", "Rstem", "stringr"), require, c=T, q=T)
  # avoid encoding issues by dropping non-unicode characters
  utf8text <- iconv(text, to='UTF-8-MAC', sub = "byte")
  # remove punctuation and convert to lower case
  words <- removePunctuation(utf8text)
  words <- tolower(words)
  # spliting in words
  words <- str_split(words, " ")
  return(words)
}


classifier <- function(words, 
                       econ.words, 
                       imm.words, 
                       health.words, 
                       military.words, 
                       gun.words, 
                       china.words,
                       trade.words, 
                       race.words,
                       climate.words, 
                       religion.words){
  # classifier
  scores <- unlist(lapply(text, 
                          econ.words, 
                          imm.words, 
                          health.words, 
                          military.words, 
                          gun.words, 
                          china.words,
                          trade.words, 
                          race.words,
                          climate.words, 
                          religion.words))
  n <- length(scores)
  economy <- as.integer(length(which(scores>0))/n*100)
  immigration <- as.integer(length(which(scores<0))/n*100)
  health_care <- as.integer(length(which(scores>0))/n*100)
  military <- as.integer(length(which(scores<0))/n*100)
  gun_control <- as.integer(length(which(scores>0))/n*100)
  china <- as.integer(length(which(scores<0))/n*100)
  trade <- as.integer(length(which(scores>0))/n*100)
  race <- as.integer(length(which(scores<0))/n*100)
  climate_change <- as.integer(length(which(scores>0))/n*100)
  religion <- as.integer(length(which(scores<0))/n*100)
  
  
  cat(n, "tweets:", economy, "% economy,",
      immigration, "% immigration,", health_care, "% health care", 
      military, "% military", gun_control, "% gun control", china, "% china",
      trade, "% trade", race, "% race", climate_change, "% climate change",
      religion, "% religion")
}



text <- clean_tweets(HC)
classifier(text, econ.words, 
           imm.words, 
           health.words, 
           military.words, 
           gun.words, 
           china.words,
           trade.words, 
           race.words,
           climate.words, 
           religion.words)





####### Getting Shiny App to run:


library(shiny)
load("wide.Rda")
runApp("shinyT")
