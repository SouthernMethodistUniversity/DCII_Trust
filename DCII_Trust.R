#################################################################
##################### Sentiment Analysis ########################
#################################################################

library(readtext)
library(tidytext)
library(textdata)
library(lexicon)
library(tidyverse)
library(stopwords)
library(gdata)
library(mice)
library(tseries)
library(forecast)
library(textstem)
library(dynlm)
library(dLagM)
library(vars)
library(fpp2)
library(urca)
library(gridExtra)
library(grid)
library(ARDL)
library(xtable)

###  FUNCTIONS #####

data("stop_words")


assign.dates <- function(document){
  document<- unlist(strsplit(document[[2]], '\\n'))
  doc.tibble<- tibble(text = document)
  doc.tibble$doc_id <- character(length=length(document))
  gg<-grep('^[0-9]?[0-9] (January|February|March|April|May|June|July|August|September|October|November|December) [0-9][0-9][0-9][0-9]$', doc.tibble$text, value=F)
  o<-1
  for (i in gg){
    doc.tibble$doc_id[o:i]<-doc.tibble$text[o]
    o<-i
  }
  doc.tibble$doc_id[0:gg[1]]<-doc.tibble$text[gg[1]]
  doc.tibble$doc_id[gg[length(gg)]:length(doc.tibble$doc_id)]<-doc.tibble$text[gg[length(gg)]]
  return(doc.tibble)
}


drop.numbers <- function(corpus){
  for (i in list(corpus$monogram)){
    myindex<-grep('[0-9]+', i)
    corpus<-  corpus[-myindex,]
    return(corpus)
  }
}

z.score <- function(x){
  (x - mean(x,na.rm = T))/sd(x,na.rm = T)
}

sent_date.to_week<-function(sentiment_data, price_data){
  x <- 1 -(sentiment_data$Date %in% price_data$Date)
  for (i in 1:length(x)){
    if (x[i]){
      tempdate<-sentiment_data$Date[i]
      while (any(tempdate %in% price_data$Date)==F){
        tempdate <- tempdate + 1
      }
      sentiment_data$Date[i]<-tempdate
    }
  }
  return(sentiment_data)
}

read.price_ts<- function(price_ts){
  x<- read_csv(price_ts)
  x<-x[complete.cases(x),]
  colnames(x)<-c('Date', 'Price', 'Volume')
  x$Date<-as.Date(x$Date, "%m/%d/%Y")
  x <-ungroup(x)
  price_change<- x$Price[1:length(x$Price)-1]-x$Price[2:length(x$Price)] 
  price_change<-append(price_change, 0)
  x$Change<-price_change
  return(x)
}


deparser <- function(varname) {
  deparse(substitute(varname))
}

assessment<-function(ff){
  adf_ch <- adf.test(ff$Change)
  adf_sent <- adf.test(ff$sentiment)
  
  if (adf_ch$p.value<0.05 & adf_sent$p.value<0.05){
    myvar<-VARselect(ff[,c('Change', 'sentiment')], lag = 20, type='both')
    bestvar<-myvar$selection[1]
    
    c_s_gtest<-grangertest(ff$Change,ff$sentiment, bestvar)
    s_c_gtest<-grangertest(ff$sentiment,ff$Change, bestvar)
    results_df <- data.frame(c_s_gtest, s_c_gtest, bestvar)
    colnames(results_df)<- c('Rdf CS', 'df CS', 'F CS', 'p CS', 'Rdf SC', 'df SC', 'F SC', 'p SC','best_var')
    results_df[2,]
    return(results_df[2,])
  }
  else{
    print('Flat file is non-stationary')
  }
}

# Brings in all of the tokens and assigns "relevance" based upon term of interest tf 
tokenized<- function(corpus, str_of_interest){
  x <- tolower(str_of_interest)
  init <- ifelse(str_of_interest == 'volkswagen', 'vw', 'wf')
  y <- unlist(strsplit(x, " "))
  z<- toString(substitute(corpus))
  two = 0
  if (length(y)==2){
    two = 1
    corpus_bigrams <- unnest_tokens(corpus, bigram, text, token = "ngrams", n=2) %>%
      count(doc_id, bigram, sort=TRUE) %>%
      mutate(text=NULL, title=z)
    
    total_words_bigrams<-corpus_bigrams %>%
      group_by(doc_id) %>%
      summarise(total=sum(n))
  
    bigram_relevance<-left_join(corpus_bigrams, total_words_bigrams) %>%
      group_by(doc_id) %>%
      mutate(tf=n/total)
    
    bigram_relevance<-bigram_relevance %>%
      group_by(doc_id) %>%
      mutate(relevance=log(sum(tf[which(bigram==paste(as.character(str_of_interest), '\'s', sep='') |
                                    bigram==paste(as.character(str_of_interest), 's', sep='') |
                                    bigram==as.character(str_of_interest) | 
                                    bigram==paste(as.character(str_of_interest), 's', sep='') |
                                    bigram==paste(as.character(init), '\'s', sep='') |
                                    bigram==paste(as.character(init), 's', sep='') |
                                    bigram==as.character(init) | 
                                    bigram==paste(as.character(init), 's', sep=''))])))
    
    bigram_relevance$relevance<-((bigram_relevance$relevance/bigram_relevance$total)-(min(bigram_relevance$relevance)/bigram_relevance$total))/((max(bigram_relevance$relevance)/bigram_relevance$total)-(min(bigram_relevance$relevance)/bigram_relevance$total))

  }
  
  monogram <- unnest_tokens(corpus, monogram, text, token = "ngrams", n=1) %>%
    count(doc_id, monogram, sort=TRUE) %>%
    group_by(doc_id) %>%
    mutate(text=NULL, title=z)
  

  total_words<-monogram %>%
    group_by(doc_id) %>%
    summarise(total=sum(n))
  
  monogram<-left_join(monogram, total_words) %>%
    group_by(doc_id) %>%
    mutate(tf=n/total)
  
  monogram<-monogram %>%
    group_by(doc_id) %>%
    mutate(relevance=log(sum(tf[which(monogram==paste(as.character(str_of_interest), '\'s', sep='') |
                                  monogram==paste(as.character(str_of_interest), 's', sep='') |
                                  monogram==as.character(str_of_interest) | 
                                  monogram==paste(as.character(str_of_interest), 's', sep='') |
                                  monogram==paste(as.character(init), '\'s', sep='') |
                                  monogram==paste(as.character(init), 's', sep='') |
                                  monogram==as.character(init) | 
                                  monogram==paste(as.character(init), 's', sep=''))])))
  
  
  monogram$relevance<-((monogram$relevance/monogram$total)-(min(monogram$relevance)/monogram$total))/((max(monogram$relevance)/monogram$total)-(min(monogram$relevance)/monogram$total))
  
  monogram <- drop.numbers(monogram)
  monogram <- anti_join(monogram, stop_words, by=c('monogram'='word'))
  
  monogram$monogram <- lemmatize_strings(monogram$monogram)
  
  if (two == 1){
    monogram$doc_id<-as.character(monogram$doc_id)
    bigram_relevance$doc_id<-as.character(bigram_relevance$doc_id)
    bigram_relevance[,2:6]<-NULL
    bigram_relevance<- distinct(bigram_relevance)
    result<-inner_join(monogram, bigram_relevance, by="doc_id")
    result$doc_id<-as.Date(result$doc_id, '%Y-%m-%d')
    result[,7]<-NULL
    return(result)
  }
  else{
    return(monogram)
  }
}

x <- tokenized(Barrons, 'wells fargo')

# Need to use wt=tf because otherwise it takes a count of unique words as opposed to sum
# of words that fall under a specific classification. Scaling before or after relvance adjustment?
# Log transform does not help distributions of loughran word bag for WF.
acquire.sent <- function(tokens, lexicon){
  result <- tokens %>%
    inner_join(get_sentiments(lexicon), by=c('monogram'='word')) 
  if ('relevance.y' %in% colnames(result)){
    result<- dplyr::count(result, title, Date = doc_id, sentiment, wt=tf,
                          total=total, relevance = relevance.y) %>%
      spread(sentiment, n, fill=0)
  }
  else{
    result<- dplyr::count(result, title, Date = doc_id, sentiment, wt=tf,
                          total=total, relevance = relevance) %>%
      spread(sentiment, n, fill=0)
  }
  print(head(result))
  if (lexicon =='bing'){
    for (i in 6:7){
      result[,i]<-result[,i]*result[,5]
      result[,i]<-scale(result[,i])
    }
    results <- result %>%
      mutate(sentiment = (positive - negative))
  }
  else if(lexicon =='loughran'){
    for (i in 6:11){
      result[,i]<-result[,i]*result[,5]
      result[,i]<-scale(result[,i])
    }
    results <- result %>%
      mutate(sentiment = (positive - negative))
  }
  else{
    for (i in 6:15){
      result[,i]<-result[,i]*result[,5]
      result[,i]<-scale(result[,i])
    }
    results <- result %>%
      mutate(sentiment = (positive - negative))
  }
  return(results)
}


flatfile<-function(sentiment, price){
  sent <- sent_date.to_week(sentiment, price)

  wfFlatfile<-full_join(sent, price) %>%
    group_by(Date) %>%
    dplyr::select(-Volume, -title) %>%
    summarise_each(funs(mean)) %>%
    ungroup()
  
  set.seed(2)
  wfFlatfile.miceimp<-mice(wfFlatfile)
  wfFlatfile.imp<-complete(wfFlatfile.miceimp, 1)
  
  wfFlatfile.imp<-dplyr::select(wfFlatfile.imp, c(Date, Price, Change, sentiment))

  
  return(wfFlatfile.imp)
}

### Read In Files
##########################################################################
### Set relative directory ###############################################
##########################################################################

setwd('~/Work/Fomby/')

###  WELLS FARGO  ##################################

Barrons <- readtext("WellsFargo/All/Barrons.pdf")
Economist <- readtext("WellsFargo/Economist/*.txt")
FT <- readtext("WellsFargo/All/FinancialTimes.pdf")
Forbes <- readtext("WellsFargo/Forbes/*.pdf")
NYT <- readtext("WellsFargo/All/NewYorkTimes.pdf")
WSJ <- readtext("WellsFargo/All/WallStreetJournal.pdf")
Reuters<-readtext("WellsFargo/Reuters/*.pdf")

###  VOLKS  #########################################

volksBarrons <- readtext("Volks/Barrons/*")
volksBusinessWeek<- readtext("Volks/BusinessWeek/*")
volksEconomist <- readtext("Volks/Economist/*.txt")
volksFT <- readtext("Volks/FinancialTimes/*")
volksForbes <- readtext("Volks/Forbes/*")
volksNYT <- readtext("Volks/NewYorkTimes/*")
volksWSJ <- readtext("Volks/WallStreetJournal/*")
volksFortune <- readtext("Volks/Fortune/*")

### Additional Volks Data ###########################

volksBarrons2<-readtext('Volks/AdditionalData/volksBarrons.pdf')
volksForbes2<-readtext('Volks/AdditionalData/volksForbes.pdf')
volksFT2<-readtext('Volks/AdditionalData/volksFT.pdf')
volksNYT2<-readtext('Volks/AdditionalData/volksNYT.pdf')
volksWSJ2<-readtext('Volks/AdditionalData/volksWSJ.pdf')


###  Assign Dates   #################################

Economist$doc_id<-as.Date(Economist$doc_id, '%Y_%m_%d')
Barrons<-assign.dates(Barrons)
Barrons$doc_id<-as.Date(Barrons$doc_id, '%d %B %Y')
NYT<-assign.dates(NYT)
NYT$doc_id<-as.Date(NYT$doc_id, '%d %B %Y')
WSJ<-assign.dates(WSJ)
WSJ$doc_id<-as.Date(WSJ$doc_id, '%d %B %Y')
Forbes<-assign.dates(Forbes)
Forbes$doc_id<-as.Date(Forbes$doc_id, '%d %B %Y')
FT<-assign.dates(FT)
FT$doc_id<-as.Date(FT$doc_id, '%d %B %Y')
Reuters<-assign.dates(Reuters)
Reuters$doc_id<-as.Date(Reuters$doc_id, '%d %B %Y')

volksBarrons$doc_id<-as.Date(volksBarrons$doc_id, '%Y-%m-%d')
volksBusinessWeek$doc_id<-as.Date(volksBusinessWeek$doc_id, '%Y-%m-%d')
volksEconomist$doc_id<-as.Date(volksEconomist$doc_id, '%Y-%m-%d')
volksFT$doc_id<-as.Date(volksFT$doc_id, '%Y-%m-%d')
volksForbes$doc_id<-as.Date(volksForbes$doc_id, '%Y-%m-%d')
volksNYT$doc_id<-as.Date(volksNYT$doc_id, '%Y-%m-%d')
volksWSJ$doc_id<-as.Date(volksWSJ$doc_id, '%Y-%m-%d')
volksFortune$doc_id<-as.Date(volksFortune$doc_id, '%Y-%m-%d')

volksBarrons2<-assign.dates(volksBarrons2)
volksBarrons2$doc_id<-as.Date(volksBarrons2$doc_id, '%d %B %Y')
volksForbes2<-assign.dates(volksForbes2)
volksForbes2$doc_id<-as.Date(volksForbes2$doc_id, '%d %B %Y')
volksFT2<-assign.dates(volksFT2)
volksFT2$doc_id<-as.Date(volksFT2$doc_id, '%d %B %Y')
volksNYT2<-assign.dates(volksNYT2)
volksNYT2$doc_id<-as.Date(volksNYT2$doc_id, '%d %B %Y')
volksWSJ2<-assign.dates(volksWSJ2)
volksWSJ2$doc_id<-as.Date(volksWSJ2$doc_id, '%d %B %Y')

volksBarrons<-bind_rows(volksBarrons, volksBarrons2)
volksForbes<-bind_rows(volksForbes, volksForbes2)
volksFT<-bind_rows(volksFT, volksFT2)

wftxt<-c(Barrons, Economist, FT, Forbes, NYT, WSJ, Reuters)

vtxt<-c(volksBarrons, volksBusinessWeek, volksEconomist, volksFT, volksForbes, volksNYT, volksWSJ, volksFortune)

detach(plyr)

### Tokenize  ##############################################
EconomistTokens2<-tokenized(Economist, 'wells fargo')
BarronsTokens2<-tokenized(Barrons, 'wells fargo')
NYTTokens2<-tokenized(NYT, 'wells fargo')
WSJTokens2<-tokenized(WSJ, 'wells fargo')
FTTokens2<-tokenized(FT, 'wells fargo')
ForbesTokens2<-tokenized(Forbes, 'wells fargo')
ReutersTokens2<-tokenized(Reuters, 'wells fargo')

c<-rbind(EconomistTokens2, BarronsTokens2, NYTTokens2, WSJTokens2, FTTokens2, ForbesTokens2, ReutersTokens2)

volksEconomistTokens2<-tokenized(volksEconomist, 'volkswagen')
volksBarronsTokens2<-tokenized(volksBarrons, 'volkswagen')
volksNYTTokens2<-tokenized(volksNYT, 'volkswagen') #
volksWSJTokens2<-tokenized(volksWSJ, 'volkswagen') #
volksFTTokens2<-tokenized(volksFT, 'volkswagen')
volksForbesTokens2<-tokenized(volksForbes, 'volkswagen')
volksBusinessWeekTokens2<-tokenized(volksBusinessWeek, 'volkswagen')
volksFortuneTokens2<-tokenized(volksFortune, 'volkswagen')

e<- rbind(volksEconomistTokens2, volksBarronsTokens2, volksNYTTokens2, volksWSJTokens2, volksFTTokens2, volksForbesTokens2, volksBusinessWeekTokens2, volksFortuneTokens2)



c$title<-as.factor(c$title)
e$title<-as.factor(e$title)

### Sentiment Generation  ####################################################


# OOB
wf.loughran <- arrange(acquire.sent(c, "loughran"),doc_id)
wf.nrc <-arrange(acquire.sent(c, "nrc"), doc_id)
wf.bing<-arrange(acquire.sent(c, "bing"), doc_id)

wf.loughran <- dplyr::rename(wf.loughran, negative_l = negative, positive_l = positive)
wf.nrc <-dplyr::rename(wf.nrc, negative_n = negative, positive_n = positive)
wf.bing<-dplyr::rename(wf.bing, negative_b = negative, positive_b = positive)

volks.loughran <- arrange(acquire.sent(e, "loughran"), doc_id)
volks.nrc <- arrange(acquire.sent(e, "nrc"), doc_id)
volks.bing <- arrange(acquire.sent(e, "bing"), doc_id)

volks.loughran <- dplyr::rename(volks.loughran, negative_l = negative, positive_l = positive)
volks.nrc <-dplyr::rename(volks.nrc, negative_n = negative, positive_n = positive)
volks.bing<-dplyr::rename(volks.bing, negative_b = negative, positive_b = positive)

length(unique(volks.loughran$doc_id))
dim(wf.loughran)

# Aggregate to prevent inflating weeks with more observations
wf.loughran <- wf.loughran %>%
  group_by(doc_id) %>%
  summarise_each(funs(mean))
wf.nrc <- wf.nrc %>%
  group_by(doc_id) %>%
  summarise_each(funs(mean))
wf.bing <- wf.bing %>%
  group_by(doc_id) %>%
  summarise_each(funs(mean))

volks.loughran <- volks.loughran %>%
  group_by(doc_id) %>%
  summarise_each(funs(mean))
volks.nrc <- volks.nrc %>%
  group_by(doc_id) %>%
  summarise_each(funs(mean))
volks.bing <- volks.bing%>%
  group_by(doc_id) %>%
  summarise_each(funs(mean))

### LOOB NRC Trust
wf.nrc1<-dplyr::filter(wf.nrc, doc_id != '2018-04-11')
wf.bing1 <- dplyr::filter(wf.bing, doc_id != '2018-04-11')
wf.test<-bind_cols(wf.loughran, dplyr::select(wf.nrc1, trust))
wf.test$sentscore<-wf.test$trust+wf.test$sentiment
wf.test$sentiment<-wf.test$sentscore
wf.test<- dplyr::select(wf.test, -c("superfluous", "uncertainty"))

v_nrc_l_date_diff <- which(volks.nrc$doc_id %in% volks.loughran$doc_id)
volks.nrc[-c(v_nrc_l_date_diff),]

v_nrc1<-dplyr::filter(volks.nrc, doc_id != '2014-11-20')
v_bing1<-dplyr::filter(volks.bing, doc_id != '2014-11-20')
v_LOOBNT<-bind_cols(volks.loughran, dplyr::select(v_nrc1, trust))
v_LOOBNT$sentscore<-v_LOOBNT$trust+v_LOOBNT$sentiment
v_LOOBNT$sentiment<-v_LOOBNT$sentscore

### All negatives summed from NRC and Loughran
### Double counting NRC words as proxy for valence

wfNeg <- bind_cols(wf.loughran, dplyr::select(wf.nrc1, anger, disgust, fear, negative_n, sadness))
wfNeg$sentiment <- apply(wfNeg[,c('constraining','litigious', 'negative_l', 'uncertainty', 'anger', 'fear', 'negative_n', 'sadness')], 1, sum)

volksNeg<- bind_cols(volks.loughran, dplyr::select(v_nrc1, anger, disgust, fear, negative_n, sadness))
volksNeg$sentiment <- apply(volksNeg[,c('constraining','litigious', 'negative_l', 'uncertainty', 'anger', 'fear', 'negative_n', 'sadness')], 1, sum)
volksNeg$sentiment

### Four Combined ####
l_neg <- c('constraining', 'litigious', 'negative_l', 'uncertainty')
n_neg <- c('fear', 'negative_n', 'sadness')
b_neg <- 'negative_b'

wf.loughran1 <- dplyr::select(wf.loughran, -sentiment)
wf.nrc1 <-dplyr::select(wf.nrc, -sentiment)
wf.bing1<-dplyr::select(wf.bing, -sentiment)

wf.loughran[,l_neg] <- -1*wf.loughran[,l_neg]
wf.nrc1[,n_neg] <- -1*wf.nrc1[,n_neg]
wf.bing1[,b_neg] <- -1*wf.bing1[,b_neg]

wf_comb <- left_join(wf.loughran1, wf.nrc1, by = c('doc_id', 'title', 'Date', 'total', 'relevance'))
wf_comb <- left_join(wf_comb, wf.bing1)

wf_comb$sentiment <- apply(wf_comb[,6:length(colnames(wf_comb))], 1, sum)

volks.loughran1 <- dplyr::select(volks.loughran, -sentiment)
volks.nrc1 <-dplyr::select(volks.nrc, -sentiment)
volks.bing1<-dplyr::select(volks.bing, -sentiment)

volks.loughran1[,l_neg] <- -1*volks.loughran[,l_neg]
volks.nrc1[,n_neg] <- -1*volks.nrc1[,n_neg]
volks.bing1[,b_neg] <- -1*volks.bing1[,b_neg]

v_comb <- left_join(volks.loughran1, volks.nrc1, by = c('doc_id', 'title', 'Date', 'total', 'relevance'))
v_comb <- left_join(v_comb, volks.bing1)

v_comb$sentiment <- apply(v_comb[,6:length(colnames(v_comb))], 1, sum)

### Price Processing    ######################################################

wfprice<-read.price_ts('WellsFargo/Bloomberg/WellsFargoPrice.csv')
VolksPrice<-read.price_ts('Volks/Bloomberg/volksPrice10102014_12302016.csv')


### Flat file creation  ########################################################


# OOB Loughran
wfFlatfile.imp<-flatfile(wf.loughran, wfprice)
volksFF1<-flatfile(volks.loughran, VolksPrice)

# OOB Loughran + NRC Trust
wfLoobNRCtrust<- flatfile(wf.test, wfprice)
volksLOOBNT<- flatfile(v_LOOBNT, VolksPrice)

# Negs
wf_neg <- flatfile(wfNeg, wfprice)
v_neg <- flatfile(volksNeg, VolksPrice)

# Comb
wf_comb <- flatfile(wf_comb, wfprice)
v_comb <- flatfile(v_comb, VolksPrice)


#### Assign actual lexicon ####

wfFlatfile.imp <- wfFlatfile.imp
volksFF <- volksLOOBNT
###############################


wfFlatfile.imp$logPrice <- log(wfFlatfile.imp$Price)
volksFF$logPrice <- log(volksFF$Price)

wf_price_change<- wfFlatfile.imp$logPrice[2:length(wfFlatfile.imp$logPrice)] - wfFlatfile.imp$logPrice[1:length(wfFlatfile.imp$logPrice)-1]
wf_price_change<-append(0, wf_price_change)

v_price_change <- volksFF$logPrice[2:length(volksFF$logPrice)]-volksFF$logPrice[1:length(volksFF$logPrice)-1]
v_price_change<-append(0, v_price_change)

v_sent_diff <- volksFF$sentiment[2:length(volksFF$sentiment)]-volksFF$sentiment[1:length(volksFF$sentiment)-1]
v_sent_diff<-append(0, v_sent_diff)


wfFlatfile.imp$log_Change <- wf_price_change
volksFF$log_Change <- v_price_change

#### Difference Sentiment (if necessary)
#volksFF$sentiment <- v_sent_diff





################# Plotting ###########################################
par(mfrow=c(2,2))
plot(wfFlatfile.imp$Date, wfFlatfile.imp$Price, type = 'l', main = 'WF Weekly Price', ylab = 'Price', xlab = 'Date')
plot(wfFlatfile.imp$Date, wfFlatfile.imp$sentiment, type = 'l')


plot(volksFF$Date, volksFF$log_Change, type = 'l', main = 'Volks Weekly Price', ylab = 'Price', xlab = 'Date')
plot(volksFF$Date, volksFF$sentiment, type = 'l')


################# ANALYSIS ###########################################

### Diff Sent ################

### Assumptions  #############

adf.test(wfFlatfile.imp$Change)
adf.test(wfFlatfile.imp$sentiment)
adf.test(wfFlatfile.imp$log_Change)
kpss.test(wfFlatfile.imp$Change)
kpss.test(wfFlatfile.imp$sentiment)
kpss.test(wfFlatfile.imp$log_Change)

adf.test(volksFF$Change)
adf.test(volksFF$sentiment)
adf.test(volksFF$log_Change)
kpss.test(volksFF$Change)
kpss.test(volksFF$sentiment)
kpss.test(volksFF$log_Change)

### VAR Model ####

VARselect(wfFlatfile.imp[,c('Change', 'sentiment')], lag = 20, type='both')
VARselect(wfFlatfile.imp[,c('log_Change', 'sentiment')], lag = 20, type='both')

VARselect(volksFF[,c('sentiment', 'Change')], lag = 20, type='both')
VARselect(volksFF[,c('sentiment', 'log_Change')], lag = 20, type='both')

wf_var<-VAR(wfFlatfile.imp[,c('sentiment', 'log_Change')], p=1, type='both')
serial.test(wf_var)
v_var<-VAR(volksFF[,c('sentiment', 'log_Change')], p=1, type='both')
serial.test(v_var)

# Investigate granger causality across multiple lags
for (i in 1:20){
  cat("LAG =", i)
  print(causality(VAR(wfFlatfile.imp[,c('sentiment', 'log_Change')], p = i, type = "const"), cause = "sentiment")$Granger)
}

### Granger Tests ####


grangertest(wfFlatfile.imp$Change,wfFlatfile.imp$sentiment, )
grangertest(wfFlatfile.imp$sentiment,wfFlatfile.imp$log_Change, 1)

grangertest(volksFF$Change,volksFF$sentiment, 1)
grangertest(volksFF$sentiment,volksFF$Change, 1)




#########################################################################################################
################################      E   D   A      ####################################################
#########################################################################################################

# ts wf data for acf analysis to determine the lag for the ardlDlm
wf_price_ts <- ts(wfFlatfile.imp$Price, start = c(2015,36),  frequency =52)
wf_change_ts <- ts(wfFlatfile.imp$Change, start = c(2015,36),  frequency =52)
wf_logchange_ts <- ts(wfFlatfile.imp$log_Change, start = c(2015,36),  frequency =52)
wf_sent_ts <- ts(wfFlatfile.imp$sentiment,  start = c(2015,36),  frequency =52)

volks_price_ts <- ts(volksFF$Price, start = c(2014,41),  frequency =52)
volks_change_ts <- ts(volksFF$Change, start = c(2014,41),  frequency =52)
volks_logchange_ts <- ts(volksFF$log_Change, start = c(2014,41),  frequency =52)
volks_sent_ts <- ts(volksFF$sentiment,  start = c(2014,41),  frequency =52)

# Half Year trends
for (i in 1:26){
  temp <- wfFlatfile.imp
  pc <- stl(wf_change_ts, s.window = 52, t.window = i) 
  sen <-  stl(wf_sent_ts, s.window = 52, t.window = i) 
  pc_ts <- pc$time.series[,2]
  sen_ts <- sen$time.series[,2]
  temp$pc_ts <- pc_ts
  temp$sen_ts <- sen_ts
  
  gg_t <-  ggplot(temp) +
    geom_line(aes(x= Date, y =pc_ts)) + 
    geom_line(aes(x= Date, y =sen_ts, color='LOESS'))+
    scale_color_discrete(name = 'Sentiment') + 
    theme_bw() +
    labs(title = paste('T Window', as.character(i)))
  print(gg_t)
}


temp <- wfFlatfile.imp
pc <- stl(wf_change_ts, s.window = 52, t.window = 26) 
sen <-  stl(wf_sent_ts, s.window = 52, t.window = 26) 
pc_ts <- pc$time.series[,2]
sen_ts <- sen$time.series[,2]
temp$pc_ts <- pc_ts
temp$sen_ts <- sen_ts


temp$roll_ch <- rollmean(temp$Change, 26, fill=0)
temp$roll_sent <- rollmean(temp$sentiment, 26, fill=0)

ch_front_mean = 1:26
sen_front_mean = 1:26

for (i in 1:26){
  ch_front_mean[i] <- mean(temp$Change[1:i])
  sen_front_mean[i] <- mean(temp$sentiment[1:i])
}

temp$roll_ch[1:26] <- ch_front_mean
temp$roll_sent[1:26] <- sen_front_mean


gg_t <-  ggplot(temp) +
  geom_line(aes(x= Date, y =pc_ts, color = 'Price Change LOESS')) + 
  geom_line(aes(x= Date, y =sen_ts, color='Sent LOESS'))+
  scale_color_discrete(name = 'Key') + 
  theme_bw() +
  labs(title ='Wells Fargo 6 Month Moving Average Plots') +
  ylab('Scaled Averaged Change')
print(gg_t)

gg_t2 <-  ggplot(temp) +
  geom_line(aes(x= Date, y =roll_ch, color = 'Price Change Rolling Avg')) + 
  geom_line(aes(x= Date, y =roll_sent, color='Sent Rolling Avg'))+
  scale_color_discrete(name = 'Key') + 
  theme_bw() +
  labs(title ='Wells Fargo 6 Month Moving Average Plots') +
  ylab('Scaled Averaged Change')
print(gg_t2)

# Wells Fargo Lags look good at T=26 (maybe fit)

for (i in 1:26){
  temp <- volksFF
  pc <- stl(volks_change_ts, s.window = 52, t.window = i) 
  sen <-  stl(volks_sent_ts, s.window = 52, t.window = i) 
  pc_ts <- pc$time.series[,2]/4
  sen_ts <- sen$time.series[,2]
  temp$pc_ts <- pc_ts
  temp$sen_ts <- sen_ts
  
  gg_t <-  ggplot(temp) +
    geom_line(aes(x= Date, y =pc_ts)) + 
    geom_line(aes(x= Date, y =sen_ts, color='LOESS'))+
    scale_color_discrete(name = 'Sentiment') + 
    theme_bw() +
    labs(title = paste('T Window', as.character(i)))
  print(gg_t)
}

temp <- volksFF
pc <- stl(volks_change_ts, s.window = 52, t.window = 26) 
sen <-  stl(volks_sent_ts, s.window = 52, t.window = 26) 
pc_ts <- pc$time.series[,2]/4
sen_ts <- sen$time.series[,2]
temp$pc_ts <- pc_ts
temp$sen_ts <- sen_ts

temp$roll_ch <- rollmean(temp$Change, 26, fill=0)
temp$roll_sent <- rollmean(temp$sentiment, 26, fill=0)

ch_front_mean = 1:26
sen_front_mean = 1:26

for (i in 1:26){
  ch_front_mean[i] <- mean(temp$Change[1:i])
  sen_front_mean[i] <- mean(temp$sentiment[1:i])
}

temp$roll_ch[1:26] <- ch_front_mean
temp$roll_sent[1:26] <- sen_front_mean




gg_t <-  ggplot(temp) +
  geom_line(aes(x= Date, y =pc_ts, color = 'Price Change LOESS')) + 
  geom_line(aes(x= Date, y =sen_ts, color='Sent LOESS'))+
  scale_color_discrete(name = 'Key') + 
  theme_bw() +
  labs(title ='Volkswagen 6 Month Moving Average Plots') +
  ylab('Scaled Averaged Change')
print(gg_t)

gg_t2 <-  ggplot(temp) +
  geom_line(aes(x= Date, y =roll_ch/4, color = 'Price Change Rolling Mean')) + 
  geom_line(aes(x= Date, y =roll_sent, color='Sent Rolling Mean'))+
  scale_color_discrete(name = 'Key') + 
  theme_bw() +
  labs(title ='Volkswagen 6 Month Moving Average Plots') +
  ylab('Scaled Averaged Change')
print(gg_t2)

# Modeling 
# train and test for WF ardlDlm (gonna go 80-20)
test_size<- length(wfFlatfile.imp$Date)%/%5
train_size <- 4*test_size


trainX1 <- wfFlatfile.imp$sentiment[1:train_size]
testX1 <- wfFlatfile.imp$sentiment[(train_size+1):length(wfFlatfile.imp$Date)]

trainY1 <- wfFlatfile.imp$Change[1:train_size]
testY1 <- wfFlatfile.imp$Change[(train_size+1):length(wfFlatfile.imp$Date)]

trainY2 <- wfFlatfile.imp$log_Change[1:train_size]
testY2 <- wfFlatfile.imp$log_Change[(train_size+1):length(wfFlatfile.imp$Date)]

v_test_size<-  length(volksFF$Date)%/%5
v_train_size <- 4*v_test_size


##
v_trainX1 <- volksFF$sentiment[1:v_train_size]
v_testX1 <- volksFF$sentiment[(v_train_size+1):length(volksFF$Date)]

v_trainY1 <- volksFF$Change[1:v_train_size]
v_testY1 <- volksFF$Change[(v_train_size+1):length(volksFF$Date)]

v_trainY2 <- volksFF$log_Change[1:v_train_size]
v_testY2 <- volksFF$log_Change[(v_train_size+1):length(volksFF$Date)]

# auto_ardlm models and predictions
wf_traindf <- data.frame(ts(wfFlatfile.imp$sentiment[1:train_size], start = c(2015,36),  frequency =52),
                         ts(wfFlatfile.imp$Change[1:train_size], start = c(2015,36),  frequency =52),
                         ts(wfFlatfile.imp$log_Change[1:train_size], start = c(2015,36),  frequency =52),
                         wfFlatfile.imp$Date[1:train_size])
colnames(wf_traindf) <- c('sentiment', 'Change', 'logChange', 'Date')

v_traindf <- data.frame(ts(volksFF$sentiment[1:v_train_size], start = c(2014,41),  frequency =52),
                        ts(volksFF$Change[1:v_train_size], start = c(2014,41),  frequency =52),
                        ts(volksFF$log_Change[1:v_train_size], start = c(2014,41),  frequency =52),
                        volksFF$Date[1:v_train_size])
colnames(v_traindf) <- c('sentiment', 'Change', 'logChange', 'Date')


wf_testdf <- data.frame(ts(testX1, start = wfFlatfile.imp$Date[train_size+1],  frequency =52),
                         ts(testY1, start = wfFlatfile.imp$Date[train_size+1],  frequency =52),
                         ts(testY2, start = wfFlatfile.imp$Date[train_size+1],  frequency =52),
                         wfFlatfile.imp$Date[(train_size+1):length(wfFlatfile.imp$Date)])
colnames(wf_testdf) <- c('sentiment', 'Change', 'logChange', 'Date')

v_testdf <- data.frame(ts(v_testX1, start = volksFF$Date[v_train_size+1],  frequency =52),
                        ts(v_testY1, start = volksFF$Date[v_train_size+1],  frequency =52),
                        ts(v_testY2, start = volksFF$Date[v_train_size+1],  frequency =52),
                       volksFF$Date[(v_train_size+1):length(volksFF$Date)])
colnames(v_testdf) <- c('sentiment', 'Change', 'logChange', 'Date')


tfx_model1<- auto_ardl(Change~sentiment, data = wf_traindf, max_order = c(10,4))
tfx_model2<- auto_ardl(logChange~sentiment, data = wf_traindf, max_order = c(10,20))

v_tfx_model1 <- auto_ardl(Change~sentiment, data = v_traindf, max_order = c(10,4))
v_tfx_model2 <- auto_ardl(logChange~sentiment, data = v_traindf, max_order = c(10,20))

tfx_model1$best_order
tfx_model2$best_order
v_tfx_model1$best_order
v_tfx_model2$best_order
tfx_model2$best_model$coefficients
tfx1_pred <- rep(tfx_model1$best_model$coefficients[1],35)
for (i in 5:35){
  tfx1_pred[i] <- tfx_model1$best_model$coefficients[1] +
    tfx1_pred[i-1]*tfx_model1$best_model$coefficients[2] +
    testX1[i]*tfx_model1$best_model$coefficients[3] +
    testX1[i-1]*tfx_model1$best_model$coefficients[4] +
    testX1[i-2]*tfx_model1$best_model$coefficients[5] +
    testX1[i-3]*tfx_model1$best_model$coefficients[6] +
    testX1[i-4]*tfx_model1$best_model$coefficients[7]
}

tfx2_pred <- rep(tfx_model2$best_model$coefficients[1],35)
for (i in 2:35){
  tfx2_pred[i] <- tfx_model2$best_model$coefficients[1] +
    tfx2_pred[i-1]*tfx_model2$best_model$coefficients[2] +
    testX1[i]*tfx_model2$best_model$coefficients[3] 
}

vtfx1_pred <- rep(v_tfx_model1$best_model$coefficients[1],25)
for (i in 5:25){
  vtfx1_pred[i] <- v_tfx_model1$best_model$coefficients[1] +
    vtfx1_pred[i-1]*v_tfx_model1$best_model$coefficients[2] +
    v_testX1[i]*v_tfx_model1$best_model$coefficients[3] +
    v_testX1[i-1]*v_tfx_model1$best_model$coefficients[4] +
    v_testX1[i-2]*v_tfx_model1$best_model$coefficients[5] +
    v_testX1[i-3]*v_tfx_model1$best_model$coefficients[6] +
    v_testX1[i-4]*v_tfx_model1$best_model$coefficients[7]
}

vtfx2_pred <- rep(v_tfx_model2$best_model$coefficients[1],25)
for (i in 3:25){
  vtfx2_pred[i] <- v_tfx_model2$best_model$coefficients[1] +
    vtfx2_pred[i-1]*v_tfx_model2$best_model$coefficients[2] +
    v_testX1[i]*v_tfx_model2$best_model$coefficients[3] +
    v_testX1[i-1]*v_tfx_model2$best_model$coefficients[4] 
}



plot(tfx1_pred, type = 'l')
v_tfx_model2$best_model$coefficients

diff1<- testY1-tfx1_pred
diff2<- testY2-tfx2_pred
v_diff1<- v_testY1-vtfx1_pred
v_diff2<- v_testY2-vtfx2_pred


MSE1 <- sum((diff1)^2)/length(testY1)
MSE2 <- sum((diff2)^2)/length(testY1)
v_MSE1 <- sum((v_diff1)^2)/length(v_testY1)
v_MSE2 <- sum((v_diff2)^2)/length(v_testY1)


# VAR model and predictions 
VARselect(wfFlatfile.imp[,c('Change', 'sentiment')], lag = 20, type='both')
VARselect(wfFlatfile.imp[,c('sentiment', 'log_Change')], lag = 20, type='both')

VARselect(volksFF[,c('Change', 'sentiment')], lag = 20, type='both')
VARselect(volksFF[,c('sentiment', 'log_Change')], lag = 20, type='both')

wf_VARmat1 <- wf_traindf[,c('sentiment', 'Change')]
colnames(wf_VARmat1) <- c('Sentiment', 'Price Change')
wf_VARmat2 <- wf_traindf[,c('sentiment', 'logChange')]
colnames(wf_VARmat2) <- c('Sentiment', 'Log Price Change')
v_VARmat1 <- data.frame(v_trainX1, v_trainY1)
colnames(v_VARmat1) <- c('Sentiment', 'Price Change')
v_VARmat2 <- data.frame(v_trainX1, v_trainY2)
colnames(v_VARmat2) <- c('Sentiment', 'Log Price Change')

wfVAR1 <- VAR(as.ts(wf_VARmat1), p=1, type='both')
wfVAR2 <- VAR(as.ts(wf_VARmat2), p=1, type='both')
volksVAR1 <- VAR(as.ts(v_VARmat1), p=1, type='both') 
volksVAR2 <- VAR(as.ts(v_VARmat2), p=1, type='both') 

wfVAR2$varresult$Log.Price.Change$coefficients
volksVAR2$varresult$Log.Price.Change$coefficients
wfVAR1_pred <- rep(wfVAR1$varresult$Price.Change$coefficients[3], 35)
for (i in 2:35){
  wfVAR1_pred[i] <- wfVAR1$varresult$Price.Change$coefficients[3] +
    i*wfVAR1$varresult$Price.Change$coefficients[4] +
    testX1[i-1]*wfVAR1$varresult$Price.Change$coefficients[1] +
    wfVAR1_pred[i-1]*wfVAR1$varresult$Price.Change$coefficients[2]
}

wfVAR2_pred <- rep(wfVAR2$varresult$Log.Price.Change$coefficients[3], 35)
for (i in 2:35){
  wfVAR2_pred[i] <- wfVAR2$varresult$Log.Price.Change$coefficients[3] +
    i*wfVAR2$varresult$Log.Price.Change$coefficients[4] +
    testX1[i-1]*wfVAR2$varresult$Log.Price.Change$coefficients[1] +
    wfVAR2_pred[i-1]*wfVAR2$varresult$Log.Price.Change$coefficients[2] 
    #testX1[i-2]*wfVAR2$varresult$Log.Price.Change$coefficients[3] +
    #wfVAR2_pred[i-2]*wfVAR2$varresult$Log.Price.Change$coefficients[4] +
    #testX1[i-3]*wfVAR2$varresult$Log.Price.Change$coefficients[5] +
    #wfVAR2_pred[i-3]*wfVAR2$varresult$Log.Price.Change$coefficients[6] 
}

volksVAR1_pred <- rep(volksVAR1$varresult$Price.Change$coefficients[3], 25)
for (i in 2:25){
  volksVAR1_pred[i] <- volksVAR1$varresult$Price.Change$coefficients[3] +
    i*volksVAR1$varresult$Price.Change$coefficients[4] +
    v_testX1[i-1]*volksVAR1$varresult$Price.Change$coefficients[1] +
    volksVAR1_pred[i-1]*volksVAR1$varresult$Price.Change$coefficients[2]
}

volksVAR2_pred <- rep(volksVAR2$varresult$Log.Price.Change$coefficients[3], 25)
for (i in 2:25){
  volksVAR2_pred[i] <- volksVAR2$varresult$Log.Price.Change$coefficients[3] +
    i*volksVAR2$varresult$Log.Price.Change$coefficients[4] +
    v_testX1[i-1]*volksVAR2$varresult$Log.Price.Change$coefficients[1] +
    volksVAR2_pred[i-1]*volksVAR2$varresult$Log.Price.Change$coefficients[2]
}



wf_VAR_MSE1 <- testY1 - wfVAR1_pred
wf_VAR_MSE2 <- testY2 - wfVAR2_pred
v_VAR_MSE1 <- v_testY1 - volksVAR1_pred
v_VAR_MSE2 <- v_testY2 - volksVAR2_pred

wf_VAR_MSE1 <- sum((wf_VAR_MSE1)^2)/length(testY1)
wf_VAR_MSE2 <- sum((wf_VAR_MSE2)^2)/length(testY2)
v_VAR_MSE1 <- sum((v_VAR_MSE1)^2)/length(v_testY1)
v_VAR_MSE2 <- sum((v_VAR_MSE2)^2)/length(v_testY2)


results_df$wf_VAR1 <- wf_VAR_MSE1
results_df$wf_VAR2 <- wf_VAR_MSE2
results_df$v_VAR1 <- v_VAR_MSE1
results_df$v_VAR2 <- v_VAR_MSE2


# Random Walk
wf_rw1 <- rwf(trainY1, h = 35)
wf_rw2 <- rwf(trainY2, h = 35)

v_rw1 <- rwf(v_trainY1, h = 25)
v_rw2 <- rwf(v_trainY2, h = 25)

wf_rw_MSE1 <- testY1 - wf_rw1$mean
wf_rw_MSE2 <- testY2 - wf_rw2$mean
v_rw_MSE1 <- v_testY1 - v_rw1$mean
v_rw_MSE2 <- v_testY2 - v_rw2$mean

wf_rw_MSE1 <- sum((wf_rw_MSE1)^2)/length(testY1)
wf_rw_MSE2 <- sum((wf_rw_MSE2)^2)/length(testY2)
v_rw_MSE1 <- sum((v_rw_MSE1)^2)/length(v_testY1)
v_rw_MSE2 <- sum((v_rw_MSE2)^2)/length(v_testY2)




# Results

wf_results <- data.frame(wf_rw_MSE1, MSE1, wf_VAR_MSE1, wf_rw_MSE2, MSE2, wf_VAR_MSE2)
colnames(wf_results) <- c('RW MSE1', 'ARDL MSE1', 'VAR MSE1',
                             'RW MSE2', 'ARDL MSE2', 'VAR MSE2')
volks_results <- data.frame(v_rw_MSE1, v_MSE1, v_VAR_MSE1, v_rw_MSE2, v_MSE2, v_VAR_MSE2)
colnames(volks_results) <- c('RW MSE1', 'ARDL MSE1', 'VAR MSE1',
                             'RW MSE2', 'ARDL MSE2', 'VAR MSE2')


wf_results
volks_results




### Graphs ####
wf_testdf[,c('rw', 'ln_rw', 'VAR', 'ln_VAR', 'ARDL', 'ln_ARDL')] <- data.frame(wf_rw1$mean, wf_rw2$mean,
                                                                               wfVAR1_pred, wfVAR2_pred,
                                                                               tfx1_pred, tfx2_pred)
final_wfdf <- left_join(wfFlatfile.imp, wf_testdf, by = 'Date')

v_testdf[,c('rw', 'ln_rw', 'VAR', 'ln_VAR', 'ARDL', 'ln_ARDL')] <- data.frame(v_rw1$mean, v_rw2$mean,
                                                                               volksVAR1_pred, volksVAR2_pred,
                                                                               vtfx1_pred, vtfx2_pred)
final_vdf <- left_join(volksFF, v_testdf, by = 'Date')


g_wf_rescomp <- ggplot(final_wfdf) +
  geom_line(aes(x= Date, y =Change.x)) + 
  geom_line(aes(x= Date, y =rw, color='Random Walk'))+
  geom_line(aes(x= Date, y =VAR, color='VAR'))+
  geom_line(aes(x= Date, y =ARDL, color='ARDL'))+
  scale_color_discrete(name = 'Model') + 
  theme_bw() +
  labs(title='Out of Sample Model Comparisons')

g_wf_rescomp_ln <- ggplot(final_wfdf[135:dim(final_wfdf)[1],]) +
  geom_line(aes(x= Date, y =log_Change)) + 
  geom_line(aes(x= Date, y =ln_rw, color='Random Walk'))+
  geom_line(aes(x= Date, y =ln_VAR, color='VAR'))+
  geom_line(aes(x= Date, y = ln_ARDL, color='ARDL'))+
  scale_color_discrete(name = 'Model') + 
  theme_bw() +
  labs(title='Out of Sample Model Comparisons')

g_wf_rescomp_ln

g_v_rescomp <- ggplot(final_vdf) +
  geom_line(aes(x= Date, y =Change.x)) + 
  geom_line(aes(x= Date, y =rw, color='Random Walk'))+
  geom_line(aes(x= Date, y =VAR, color='VAR'))+
  geom_line(aes(x= Date, y =ARDL, color='ARDL'))+
  scale_color_discrete(name = 'Model') + 
  theme_bw() +
  labs(title='Volkswagen Out of Sample Model Comparisons')

g_v_rescomp_ln <- ggplot(final_vdf[90:dim(final_wfdf)[1],]) +
  geom_line(aes(x= Date, y =log_Change)) + 
  geom_line(aes(x= Date, y =ln_rw, color='Random Walk'))+
  geom_line(aes(x= Date, y =ln_VAR, color='VAR'))+
  geom_line(aes(x= Date, y = ln_ARDL, color='ARDL'))+
  scale_color_discrete(name = 'Model') + 
  theme_bw() +
  labs(title='Volkswagen Out of Sample Model Comparisons')

g_v_rescomp_ln


# need to get the mean to be the price mean and the variance to be price variance
wf_sent_trend <- stl(wf_sent_ts, s.window = 52, t.window = 2)
new_sent_series <- (wf_sent_trend$time.series[,2]*sd(wfFlatfile.imp$Price)) + mean(wfFlatfile.imp$Price)
roll_sent <- rollsumr(wfFlatfile.imp$sentiment, 2, 0)
roll_sent <- (roll_sent*(sd(wfFlatfile.imp$Price)/2)) + mean(wfFlatfile.imp$Price)
wfFlatfile.imp$sent_scaled <- new_sent_series
wfFlatfile.imp$roll_sent <- roll_sent

mean(wfFlatfile.imp$Price)

g_wfpriceXsent2 <- ggplot(wfFlatfile.imp) +
  geom_line(aes(x= Date, y =Price)) + 
  geom_line(aes(x= Date, y =sent_scaled, color='LOESS'))+
  geom_line(aes(x= Date, y =roll_sent, color = 'Rolling Avg'))+ 
  scale_color_discrete(name = 'Sentiment') + 
  theme_bw() +
  labs(title='Wells Fargo Price Series By Sentiment Moving Average, 2 weeks')

g_wfpriceXsent2

wf_sent_trend <- stl(wf_sent_ts, s.window = 52, t.window = 8)
new_sent_series <- (wf_sent_trend$time.series[,2]*sd(wfFlatfile.imp$Price)) + mean(wfFlatfile.imp$Price)
roll_sent <- rollsumr(wfFlatfile.imp$sentiment, 8, 0)
roll_sent <- (roll_sent*(sd(wfFlatfile.imp$Price)/8)) + mean(wfFlatfile.imp$Price)
wfFlatfile.imp$sent_scaled <- new_sent_series
wfFlatfile.imp$roll_sent <- roll_sent

mean(wfFlatfile.imp$Price)

g_wfpriceXsent8 <- ggplot(wfFlatfile.imp) +
  geom_line(aes(x= Date, y =Price)) + 
  geom_line(aes(x= Date, y =sent_scaled, color='LOESS'))+
  geom_line(aes(x= Date, y =roll_sent, color = 'Rolling Avg'))+ 
  scale_color_discrete(name = 'Sentiment') + 
  theme_bw() +
  labs(title='Wells Fargo Price Series By Sentiment Moving Average, 8 weeks')

g_wfpriceXsent8

#VW
v_sent_trend <- stl(volks_sent_ts, s.window = 52, t.window = 2)
new_sent_series <- (v_sent_trend$time.series[,2]*sd(volksFF$Price)) + mean(volksFF$Price)
roll_sent <- rollsumr(volksFF$sentiment, 2, 0)
roll_sent <- (roll_sent*(sd(volksFF$Price)/2)) + mean(volksFF$Price)
volksFF$sent_scaled <- new_sent_series
volksFF$roll_sent <- roll_sent

mean(volksFF$Price)

g_vpriceXsent2 <- ggplot(volksFF) +
  geom_line(aes(x= Date, y =Price)) + 
  geom_line(aes(x= Date, y =sent_scaled, color='LOESS'))+
  geom_line(aes(x= Date, y =roll_sent, color = 'Rolling Avg'))+ 
  scale_color_discrete(name = 'Sentiment') + 
  theme_bw() +
  labs(title='Volkswagen Price Series By Sentiment Moving Average, 2 weeks')

g_vpriceXsent2

v_sent_trend <- stl(volks_sent_ts, s.window = 52, t.window = 12)
new_sent_series <- (v_sent_trend$time.series[,2]*sd(volksFF$Price)) + mean(volksFF$Price)
roll_sent <- rollsumr(volksFF$sentiment, 12, 0)
roll_sent <- (roll_sent*(sd(volksFF$Price)/12)) + mean(volksFF$Price)
volksFF$sent_scaled <- new_sent_series
volksFF$roll_sent <- roll_sent

mean(volksFF$Price)

g_vpriceXsent12 <- ggplot(volksFF) +
  geom_line(aes(x= Date, y =Price)) + 
  geom_line(aes(x= Date, y =sent_scaled, color='LOESS'))+
  geom_line(aes(x= Date, y =roll_sent, color = 'Rolling Avg'))+ 
  scale_color_discrete(name = 'Sentiment') + 
  theme_bw() +
  labs(title='Volkswagen Price Series By Sentiment Moving Average, 12 weeks')

g_vpriceXsent12

volks_results <- data.frame(v_rw_MSE1, v_MSE1, v_VAR_MSE1, v_rw_MSE2, v_MSE2, v_VAR_MSE2)
vw_rwpred <- v_rw1$mean
vw_VARpred <- v_VAR_pred1$fcst$v_trainY1[,1] + volksFF$Price[92]

forecast::forecast(wfVAR2) %>%
  autoplot(main = 'Wells Fargo VAR Predictions') + xlab("Date Index") 

wf_traindf
wf_preds <- ggplot(wf_traindf) +
  geom_line(aes(x= Date, y = Change)) + 
  geom_line(aes(x= Date, y = logChange, color='logChange'))+
  geom_line(aes(x= Date, y = roll_sent, color = 'Rolling Avg'))+ 
  scale_color_discrete(name = 'change') + 
  theme_bw() +
  labs(title='Investigate')

wf_pred_df <-  full_join(wfFlatfile.imp, wf_VAR_pred1$fcst$trainY1[,1], by = 'Date')     #$trainY1[,1]

#
g_wfprice<- ggplot(wfFlatfile.imp, aes(Date, Price)) +
  geom_line() + 
  theme_bw() +
  labs(title='Wells Fargo Price Series')

g_wfPriceDiff <- ggplot(wfFlatfile.imp, aes(Date, log_Change)) +
  geom_line() + 
  theme_bw() +
  labs(title='Wells Fargo Log Differenced Price Series')

g_volksprice<- ggplot(volksFF, aes(Date, log_Price)) +
  geom_line() + 
  theme_bw() +
  labs(title='Volkswagen Price Series')

g_volksPriceDiff<- ggplot(volksFF, aes(Date, log_Change)) +
  geom_line() + 
  theme_bw() +
  labs(title='Volkswagen Log Differenced Price Series')

g_wfprice
g_wfPriceDiff
g_volksprice
g_volksPriceDiff

g_wfsent<- ggplot(wfFlatfile.imp, aes(Date, sentiment)) +
  geom_line() + 
  theme_bw() +
  labs(title='Wells Fargo Sentiment Series')

g_wfsent

g_volksent<- ggplot(volksFF, aes(Date, sentiment)) +
  geom_line() + 
  theme_bw() +
  labs(title='Volkswagen Sentiment Series')

g_volksent

par(mfrow=c(2,2))

grid.arrange(g_wfPriceDiff, g_wfsent,g_volksPriceDiff,g_volksent, nrow = 2, ncol=2)


################################
##### SCRATCH ##################
################################


### SVD ##########

# Convert long to wide

c3 <- c[,c(1,2,3)]

length(unique(c$doc_id))


c_wide <- data.frame(matrix(data= 0, nrow=21091, ncol= 453), row.names = unique(c$monogram))
colnames(c_wide) <- unique(c$doc_id)


for (i in 1:194969){
  c_wide[c$monogram[i], as.character(c$doc_id[i])]<-c$n[i]
  if (i%%20000 ==0){
    print(paste((i/20000)*10, '% Done'))
  }
}

length(unique(e$doc_id))
length(unique(e$monogram))


e_wide <- data.frame(matrix(data= 0, nrow=17651, ncol= 408), row.names = unique(e$monogram))
colnames(e_wide) <- unique(e$doc_id)


for (i in 1:228365){
  e_wide[e$monogram[i], as.character(e$doc_id[i])]<-e$n[i]
  if (i%%22000 ==0){
    print(paste((i/20000)*10, '% Done'))
  }
}

# SVD

c_w_SVD <-svd(c_wide)

e_w_SVD <- svd(e_wide)


# Checking out the identity X = U*D*V' , where V' is the transpose of V.
#
U <- c_w_SVD$u
U
V <- c_w_SVD$v
V
d <- c_w_SVD$d
d
D <- diag(d)
D
# Check first identity: X = U*D*V'
(c_w_SVD_check <- U %*% D %*% t(V))
Zero.1 <- c_w_SVD - c_w_SVD_check
Zero.1
# SVD checks out.
#
# Running another check.  D = U'*X*V
(Dcheck <- t(U) %*% X %*% V)
Zero.2 = D - Dcheck

# Essentially checks out.
#
# To approximate X we must choose the number of singular values
# to use in the approximation.  Here we choose to use the 3 largest
# singular values [3.46, 1.73, and 1.73].
#
# Choosing to use the largest 3 singular values (k) to lead the approximation.
# Dk is the D(k) matrix = the subset D matrix.
# Dk <- diag(d[1:3])
#
Dk <- diag(d[1:3])
Dk
Uk <- as.matrix(U[, 1:3])
Uk
Vk <- as.matrix(V[, 1:3])
Vk
# Xk is the SVD approximation of the X matrix using k (=3)
# singular values.
(Xk <- Uk %*% Dk %*% t(Vk))
Xk

# Very close approximation of X given only three singular values were used.
#
# Now we focus on getting the latent semantic scores using k=3.
# This amounts to constructiing the matrix [ginv(Dk)]*Uk'*X.  What you are 
# doing is transformaing the document columns of X into a 3 x n matrix.
# The i-th column of this matrix represents the 3 latent semantic scores
# associated with the i-th document.  In turn these 3 scores per
# document serve as new input variables for classifying each document.
#
Dk_inv <- ginv(Dk)
Dk_inv
Ukt <- t(Uk)
Ukt

dim(Dk_inv %*% Ukt)

(inputs <- Dk_inv %*% Ukt %*% c_w_SVD_check)

c_wide_labelled <- data.frame(data = Dk_inv %*% Ukt)
colnames(c_wide_labelled) <- rownames(c_wide)

### Interesting, looks like this works.
colnames(c_wide_labelled)[order(c_wide_labelled[1,])[1:10]]
colnames(c_wide_labelled)[order(c_wide_labelled[2,])[1:10]]
colnames(c_wide_labelled)[order(c_wide_labelled[3,])[1:10]]

#


c_lsa <-as.textmatrix(c_wide)






# Plot different windows for the volks
plot(volks_change_ts)
plot(volksFF$Price, type='l')
abline(v=min(which(volksFF$scandal==1)), col="blue")
abline(v=26, col="red")

names <- colnames(volksFF[,1:6])
lagsum_price <- c()
lagsum_sent <- c()

for (i in 1:13){
  plot(stl(volks_change_ts, s.window = 52, t.window = i), main=paste('Price Change \n Lag = ', as.character(i), sep=''))
  volksFF[,6+i] <- rollsumr(volksFF$Change, i, 0)
  lagsum_price <- c(lagsum_price, paste('price_lag_', as.character(i), sep =''))
  }

lowest_AIC <-Inf
lowest_HQ <- Inf
lowest_SC <- Inf
lowest_FPE <- Inf

for (i in 2:13){
  sent_t <- stl(volks_sent_ts, s.window = 52, t.window = i)$t
  change_t <- stl(volks_change_ts, s.window = 52, t.window = i)$t
  volksFF[,6+i+(2*(i-1))] <- rollsumr(volksFF$sent, i, 0)
  volksFF[,6+i+1+(2*(i-1))] <- rollsumr(volksFF$Change, i, 0)
  lagsum_sent <- c(lagsum_sent, paste('sent_lag_', as.character(i), sep =''))
  lagsum_price <- c(lagsum_price, paste('price_lag_', as.character(i), sep =''))
  
  minAIC <- min(VARselect(c(change_t, sent_t), lag = 20, type='both')[[2]][1])
  minHQ <- min(VARselect(c(change_t, sent_t), lag = 20, type='both')[[2]][2])
  minSC <- min(VARselect(c(change_t, sent_t), lag = 20, type='both')[[2]][3])
  minFPE <- min(VARselect(c(change_t, sent_t), lag = 20, type='both')[[2]][4])
  
  if (minAIC<lowest_AIC){
    lowest_AIC <- minAIC
    lowestAIC_lag <- paste('Lowest AIC lag: ',
                           as.character(i),
                           ', Lowest AIC VAR lag: ',
                           as.character(min(VARselect(c(price_change, sent_change),
                                                      lag = 20, type='both')[[1]][1])))
  }
  if (minHQ<lowest_HQ){
    lowest_HQ <- minHQ
    lowestHQ_lag <- paste('Lowest HQ lag: ',
                           as.character(i),
                           ', Lowest HQ VAR lag: ',
                           as.character(min(VARselect(c(price_change, sent_change),
                                                      lag = 20, type='both')[[1]][2])))
  }
  if (minSC<lowest_SC){
    lowest_SC <- minSC
    lowestSC_lag <- paste('Lowest SC lag: ',
                           as.character(i),
                           ', Lowest SC VAR lag: ',
                           as.character(min(VARselect(c(price_change, sent_change),
                                                      lag = 20, type='both')[[1]][3])))
  }
  if (minFPE<lowest_FPE){
    lowest_FPE <- minFPE
    lowestFPE_lag <- paste('Lowest FPE lag: ',
                           as.character(i),
                           ', Lowest FPE VAR lag: ',
                           as.character(min(VARselect(c(price_change, sent_change),
                                                      lag = 20, type='both')[[1]][4])))
  }
  
  plot(sent_t, main=paste('Price Change \n Lag = ', as.character(i), sep=''))
  plot(change_t, main=paste('Price Change \n Lag = ', as.character(i), sep=''))
}

colnames(volksFF) <- c(names, lagsum_price, lagsum_sent)

# 

price_change <- stl(volks_change_ts, s.window = 52, t.window = 7)$t
sent_change <- stl(volks_sent_ts, s.window = 52, t.window = 7)$t

str(VARselect(c(price_change, sent_change), lag = 20, type='both'))

min(VARselect(c(price_change, sent_change), lag = 20, type='both')[[2]][1])
