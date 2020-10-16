## WF

lough <- get_sentiments('loughran')
colnames(lough)<- c('monogram', 'sentiment')
sentiment.loughran <- left_join(lough,c, by= 'monogram')
sentiment.loughran <- dplyr::select(sentiment.loughran, c('monogram', 'sentiment', 'n'))

l_word_count <- sentiment.loughran %>%
  count(monogram, sentiment, sort=T)
colnames(l_word_count) <- c('word', 'sentiment', 'n')

wfg1 <- l_word_count %>%
  filter(n > 150) %>%
  mutate(n = ifelse(sentiment %in% c("negative", 'litigious', 'uncertainty', 'constraining'), -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() + 
  labs(y = "Contribution to sentiment") +
  ggtitle("Loughran words") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
wfg1


bing <- get_sentiments('bing')
colnames(bing)<- c('monogram', 'sentiment')
sent.bing <- left_join(bing,c, by= 'monogram')
sent.bing <- dplyr::select(sent.bing, c('monogram', 'sentiment', 'n'))

b_word_count <- sent.bing %>%
  count(monogram, sentiment, sort=T)
colnames(b_word_count) <- c('word', 'sentiment', 'n')

wfg2 <- b_word_count %>%
  filter(n > 150) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() + 
  labs(y = "Contribution to sentiment") +
  ggtitle("Bing words") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
wfg2

nrc <- get_sentiments('nrc')
colnames(nrc)<- c('monogram', 'sentiment')
sent.nrc <- left_join(nrc,c, by= 'monogram')
sent.nrc <- dplyr::select(sent.nrc, c('monogram', 'sentiment', 'n'))

nrc_word_count <- sent.nrc %>%
  count(monogram, sentiment, sort=T)
colnames(nrc_word_count) <- c('word', 'sentiment', 'n')

wfg3 <- nrc_word_count %>%
  filter(n > 250) %>%
  mutate(n = ifelse(sentiment  %in% c("negative", 'fear', 'disgust', 'anger', 'sadness'), -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() + 
  labs(y = "Contribution to sentiment") +
  ggtitle("NRC words") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
wfg3

afinn <- get_sentiments('afinn')
colnames(afinn)<- c('monogram', 'sentiment')
sent.afinn <- left_join(afinn,c, by= 'monogram')
sent.afinn <- dplyr::select(sent.afinn, c('monogram', 'sentiment', 'n'))

afinn_word_count <- sent.afinn %>%
  count(monogram, sentiment, sort=T)
colnames(afinn_word_count) <- c('word', 'sentiment', 'n')

wfg4 <- afinn_word_count %>%
  filter(n > 150) %>%
  mutate(score =  n*sentiment) %>%
  mutate(word = reorder(word, score)) %>%
  ggplot(aes(word, score, fill = sentiment)) +
  geom_col() +
  coord_flip() + 
  labs(y = "Contribution to sentiment") +
  ggtitle("AFINN words") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
wfg4

grid.arrange(wfg1, wfg2, wfg3, wfg4, nrow = 2, ncol=2)

## Volks
lough <- get_sentiments('loughran')
colnames(lough)<- c('monogram', 'sentiment')
sentiment.loughran <- left_join(lough,e, by= 'monogram')
sentiment.loughran <- dplyr::select(sentiment.loughran, c('monogram', 'sentiment', 'n'))

l_word_count <- sentiment.loughran %>%
  count(monogram, sentiment, sort=T)
colnames(l_word_count) <- c('word', 'sentiment', 'n')

vg1 <- l_word_count %>%
  filter(n > 150) %>%
  mutate(n = ifelse(sentiment %in% c("negative", 'litigious', 'uncertainty', 'constraining'), -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() + 
  labs(y = "Contribution to sentiment") +
  ggtitle("Loughran words") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
vg1


bing <- get_sentiments('bing')
colnames(bing)<- c('monogram', 'sentiment')
sent.bing <- left_join(bing,e, by= 'monogram')
sent.bing <- dplyr::select(sent.bing, c('monogram', 'sentiment', 'n'))

b_word_count <- sent.bing %>%
  count(monogram, sentiment, sort=T)
colnames(b_word_count) <- c('word', 'sentiment', 'n')

vg2 <- b_word_count %>%
  filter(n > 150) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() + 
  labs(y = "Contribution to sentiment") +
  ggtitle("Bing words") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
vg2

nrc <- get_sentiments('nrc')
colnames(nrc)<- c('monogram', 'sentiment')
sent.nrc <- left_join(nrc,e, by= 'monogram')
sent.nrc <- dplyr::select(sent.nrc, c('monogram', 'sentiment', 'n'))

nrc_word_count <- sent.nrc %>%
  count(monogram, sentiment, sort=T)
colnames(nrc_word_count) <- c('word', 'sentiment', 'n')

vg3 <- nrc_word_count %>%
  filter(n > 250) %>%
  mutate(n = ifelse(sentiment  %in% c("negative", 'fear', 'disgust', 'anger', 'sadness'), -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() + 
  labs(y = "Contribution to sentiment") +
  ggtitle("NRC words") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
vg3


afinn <- get_sentiments('afinn')
colnames(afinn)<- c('monogram', 'sentiment')
sent.afinn <- left_join(afinn,e, by= 'monogram')
sent.afinn <- dplyr::select(sent.afinn, c('monogram', 'sentiment', 'n'))

afinn_word_count <- sent.afinn %>%
  count(monogram, sentiment, sort=T)
colnames(afinn_word_count) <- c('word', 'sentiment', 'n')

vg4 <- afinn_word_count %>%
  filter(n > 150) %>%
  mutate(score =  n*sentiment) %>%
  mutate(word = reorder(word, score)) %>%
  ggplot(aes(word, score, fill = sentiment)) +
  geom_col() +
  coord_flip() + 
  labs(y = "Contribution to sentiment") +
  ggtitle("AFINN words") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
vg4
