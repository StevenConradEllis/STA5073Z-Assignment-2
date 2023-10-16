rm(list= ls())

library(stringr)
library(tidyverse)
library(tidytext)
library(textstem)
library(textdata)
library(tm)
library(MASS)
library(topicmodels)


set.seed(2023)


# read in text data files and organise these into a data frame
filenames <- c('1994_post_elections_Mandela.txt', 
               '1994_pre_elections_deKlerk.txt', 
               '1995_Mandela.txt', 
               '1996_Mandela.txt', 
               '1997_Mandela.txt',
               '1998_Mandela.txt', 
               '1999_post_elections_Mandela.txt', 
               '1999_pre_elections_Mandela.txt', 
               '2000_Mbeki.txt', 
               '2001_Mbeki.txt', 
               '2002_Mbeki.txt', 
               '2003_Mbeki.txt', 
               '2004_post_elections_Mbeki.txt', 
               '2004_pre_elections_Mbeki.txt', 
               '2005_Mbeki.txt', 
               '2006_Mbeki.txt', 
               '2007_Mbeki.txt', 
               '2008_Mbeki.txt', 
               '2009_post_elections_Zuma.txt', 
               '2009_pre_elections_ Motlanthe.txt', 
               '2010_Zuma.txt', 
               '2011_Zuma.txt', 
               '2012_Zuma.txt', 
               '2013_Zuma.txt', 
               '2014_post_elections_Zuma.txt', 
               '2014_pre_elections_Zuma.txt', 
               '2015_Zuma.txt', 
               '2016_Zuma.txt', 
               '2017_Zuma.txt', 
               '2018_Ramaphosa.txt', 
               '2019_post_elections_Ramaphosa.txt', 
               '2019_pre_elections_Ramaphosa.txt', 
               '2020_Ramaphosa.txt', 
               '2021_Ramaphosa.txt', 
               '2022_Ramaphosa.txt', 
               '2023_Ramaphosa.txt')


this_speech <- c()
this_speech[1] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/1994_post_elections_Mandela.txt', nchars = 27050)
this_speech[2] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/1994_pre_elections_deKlerk.txt', nchars = 12786)
this_speech[3] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/1995_Mandela.txt', nchars = 39019)
this_speech[4] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/1996_Mandela.txt', nchars = 39524)
this_speech[5] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/1997_Mandela.txt', nchars = 37489)
this_speech[6] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/1998_Mandela.txt', nchars = 45247)
this_speech[7] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/1999_post_elections_Mandela.txt', nchars = 34674)
this_speech[8] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/1999_pre_elections_Mandela.txt', nchars = 41225)
this_speech[9] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2000_Mbeki.txt', nchars = 37552)
this_speech[10] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2001_Mbeki.txt', nchars = 41719)
this_speech[11] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2002_Mbeki.txt', nchars = 50544)
this_speech[12] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2003_Mbeki.txt', nchars = 58284)
this_speech[13] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2004_post_elections_Mbeki.txt', nchars = 34590)
this_speech[14] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2004_pre_elections_Mbeki.txt', nchars = 39232)
this_speech[15] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2005_Mbeki.txt', nchars = 54635)
this_speech[16] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2006_Mbeki.txt', nchars = 48643)
this_speech[17] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2007_Mbeki.txt', nchars = 48641)
this_speech[18] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2008_Mbeki.txt', nchars = 44907)
this_speech[19] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2009_post_elections_Zuma.txt', nchars = 31101)
this_speech[20] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2009_pre_elections_Motlanthe.txt', nchars = 47157)
this_speech[21] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2010_Zuma.txt', nchars = 26384)
this_speech[22] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2011_Zuma.txt', nchars = 33281)
this_speech[23] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2012_Zuma.txt', nchars = 33376)
this_speech[24] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2013_Zuma.txt', nchars = 36006)
this_speech[25] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2014_post_elections_Zuma.txt', nchars = 29403)
this_speech[26] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2014_pre_elections_Zuma.txt', nchars = 36233)
this_speech[27] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2015_Zuma.txt', nchars = 32860)
this_speech[28] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2016_Zuma.txt', nchars = 32464)
this_speech[29] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2017_Zuma.txt', nchars = 35981)
this_speech[30] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2018_Ramaphosa.txt', nchars = 33290)
this_speech[31] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2019_post_elections_Ramaphosa.txt', nchars = 42112)
this_speech[32] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2019_pre_elections_Ramaphosa.txt', nchars = 56960)
this_speech[33] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2020_Ramaphosa.txt', nchars = 47910)
this_speech[34] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2021_Ramaphosa.txt', nchars = 43352)
this_speech[35] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2022_Ramaphosa.txt', nchars = 52972)
this_speech[36] <- readChar('https://raw.githubusercontent.com/iandurbach/datasci-fi/master/data/sona/2022_Ramaphosa.txt', nchars = 52972)


sona <- data.frame(filename = filenames, speech = this_speech, stringsAsFactors = FALSE)

# extract year and president for each speech
sona$year <- str_sub(sona$filename, start = 1, end = 4)
sona$pres <- str_remove_all(str_extract(sona$filename, "[dA-Z].*\\."), "\\.")

# clean the sona dataset by adding the date and removing unnecessary text
replace_reg <- '(http.*?(\\s|.$))|(www.*?(\\s|.$))|&amp;|&lt;|&gt;|\n'

sona <-sona %>%
  mutate(speech = lemmatize_strings(stringr::str_replace_all(speech, replace_reg , ' '))
         ,date = str_sub(speech, start=1, end=30)
         ,date = str_replace_all(date, "February", "02")
         ,date = str_replace_all(date, "June", "06")
         ,date = str_replace_all(date, "Feb", "02")
         ,date = str_replace_all(date, "May", "05")
         ,date = str_replace_all(date, "Jun", "06")
         ,date = str_replace_all(date, "Thursday, ","")
         ,date = str_replace_all(date, ' ', '-')        
         ,date = str_replace_all(date, "[A-z]",'')
         ,date = str_replace_all(date, '-----', '')
         ,date = str_replace_all(date, '----', '')
         ,date = str_replace_all(date, '---', '')
         ,date = str_replace_all(date, '--', '')
  )

# Convert to tibble
sona <- as.tibble(sona)

replace_reg <- '(http.*?(\\s|.$))|(www.*?(\\s|.$))|&amp;|&lt;|&gt;'
unnest_reg <- "[^\\w_#@'’]"

# tidy text 
tidy_sona <- sona %>% 
  mutate(speech = str_replace_all(speech, replace_reg, '')) %>% # remove links etx
  unnest_tokens(word, speech, token = 'regex', pattern = unnest_reg) %>%   # tokenize
  filter(!word %in% stop_words$word, str_detect(word, '[A-Za-z]')) %>%  # remove stop words
  dplyr::select(word, pres, date, filename,year)  # select variables needed

# Count the number of times each word in our vocabulary was used by each president, 
# creating a "long" (and tidy) format of the document-term matrix.
sona_tdf <- tidy_sona %>%
  group_by(pres,year,word,date) %>%
  count() %>%  
  ungroup()

# Remove common SONA words that are used very regularly, but which don't convey too much meaning
common_sona_reg <- 'speaker|madame|honourable|chairperson|development|national|ensure|deputy|africa|african|africans|south|southern|government|people|programme|economic|economy|country'
sona_tdf <- sona_tdf %>% filter(!grepl(common_sona_reg, word))


#separate president speeches
mandela_speeches <- sona_tdf %>% filter(pres == "Mandela")
mbeki_speeches <- sona_tdf %>% filter(pres == "Mbeki")
zuma_speeches <- sona_tdf %>% filter(pres == "Zuma")
ramaphosa_speeches <- sona_tdf %>% filter(pres == "Ramaphosa")

# nrc sentiment analysis
#load the nrc dictionary 
nrc <- get_sentiments('nrc') 
save(nrc, file = "dsfi-lexicons.Rdata")

#overall sentiments by all presidents
sona_tdf_nrc <- sona_tdf %>%
  left_join(nrc, by = "word") %>%
  mutate(sentiment = ifelse(is.na(sentiment), 'neutral', sentiment)) %>%
  filter(sentiment != "neutral")%>%
  count(sentiment) %>%
  arrange(desc(n)) %>%
  filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(reorder(sentiment,n),n,fill= -n)) + geom_col() + coord_flip() + ylab("Count")+xlab("")+
  ggtitle("Emotion Sentiment Scores Expressed by All Presidents") +
  theme(plot.title = element_text(size = 11), legend.position = '')+
  scale_fill_gradient(low = "springgreen", high = "darkseagreen1")
sona_tdf_nrc
 
#nrc sentiments by individual presidents  
mandela_nrc <- mandela_speeches %>%
  left_join(nrc, by = "word") %>%
  mutate(sentiment = ifelse(is.na(sentiment), 'neutral', sentiment)) %>%
  filter(sentiment != "neutral") %>%
  count(sentiment) %>%
  arrange(desc(n)) %>%
  filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(reorder(sentiment,n),n,fill= -n)) + geom_col() + coord_flip() + ylab("Count")+xlab("")+
  ggtitle("Emotion Sentiment Scores Expressed by Mandela") +
  theme(plot.title = element_text(size = 8), legend.position = '')+
  scale_fill_gradient(low = "deeppink3", high = "pink")


mbeki_nrc <- mbeki_speeches %>%
  left_join(nrc, by = "word") %>%
  mutate(sentiment = ifelse(is.na(sentiment), 'neutral', sentiment)) %>%
  filter(sentiment != "neutral")%>%
  count(sentiment) %>%
  arrange(desc(n)) %>%
  filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(reorder(sentiment,n),n,fill= -n)) + geom_col() + coord_flip() + ylab("Count")+ xlab("") +
  ggtitle("Emotion Sentiment Scores Expressed by Mbeki") +
  theme(plot.title = element_text(size = 8), legend.position = '')+
  scale_fill_gradient(low = "darkblue", high = "blue")

ramaphosa_nrc <- ramaphosa_speeches %>%
  left_join(nrc, by = "word") %>%
  mutate(sentiment = ifelse(is.na(sentiment), 'neutral', sentiment)) %>%
  filter(sentiment != "neutral")%>%
  count(sentiment) %>%
  arrange(desc(n)) %>%
  filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(reorder(sentiment,n),n,fill= -n)) + geom_col() + coord_flip() + ylab("Count")+xlab("")+
  ggtitle("Emotion Sentiment Scores Expressed by Ramaphosa") +
  theme(plot.title = element_text(size = 8), legend.position = '')+
  scale_fill_gradient(low = "darkgreen", high = "darkseagreen1")


zuma_nrc <- zuma_speeches %>%
  left_join(nrc, by = "word") %>%
  mutate(sentiment = ifelse(is.na(sentiment), 'neutral', sentiment)) %>%
  filter(sentiment != "neutral")%>%
  count(sentiment) %>%
  arrange(desc(n)) %>%
  filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(reorder(sentiment,n),n,fill= -n)) + geom_col() + coord_flip() + ylab("Count")+xlab("")+
  ggtitle("Emotion Sentiment Scores Expressed by Zuma") +
  theme(plot.title = element_text(size = 8), legend.position = '')+
  scale_fill_gradient(low = "magenta4", high = "plum")

# combining above plots in one figure
cowplot::plot_grid(mandela_nrc, mbeki_nrc,
                   zuma_nrc,ramaphosa_nrc,
                   nrow = 2, ncol = 2, labels = "AUTO", label_size = 9, label_x = 0)

#nrc sentiment  over time

# removing deKlerk and Motlanthe from the analysis as they made only 1 speech
sona_tdf  <- sona_tdf  %>%
  filter(!pres %in% c('deKlerk', 'Motlanthe')) # excluding 1 time president
unique(sona_tdf$pres) # dealing with 4 presidents now

#nrc over time 
emotion_change_over_time <- sona_tdf %>%
  left_join(nrc, by = "word") %>%
  mutate(sentiment = ifelse(is.na(sentiment), 'neutral', sentiment)) %>%
  filter(sentiment != "neutral")%>%
  group_by(sentiment,year) %>%
  summarize(n = n()) 

emotion_change_over_time <-emotion_change_over_time%>%
  left_join(emotion_change_over_time %>% 
              group_by(year) %>% 
              summarise(total = sum(n))) %>%
  mutate(freq = n/total)

emotion_change_over_time %>%
  ggplot(aes(x = year, y = freq, colour = sentiment,group=sentiment)) +
  geom_line() +
  geom_smooth(aes(colour = sentiment)) + ylab("frequency") + xlab("year")+
  ggtitle("Emotion Change Over Time") +
  theme(plot.title = element_text(size = 11))
 
#individual president sentiment change  over time
mandela_emotion_change_over_time <- mandela_speeches %>%
  left_join(nrc, by = "word") %>%
  mutate(sentiment = ifelse(is.na(sentiment), 'neutral', sentiment)) %>%
  filter(sentiment != "neutral")%>%
  group_by(sentiment,year) %>%
  summarize(n = n())

mandela_emotion_change_over_time <-mandela_emotion_change_over_time%>%
  left_join(mandela_emotion_change_over_time %>% 
              group_by(year) %>% 
              summarise(total = sum(n))) %>%
  mutate(freq = n/total)

mandela_emotion_change_over_time %>%
  ggplot(aes(x = year, y = freq, colour = sentiment,group=sentiment)) +
  geom_line() + 
  geom_smooth(aes(colour = sentiment))+ ylab("frequency") + xlab("year")+
  ggtitle("Mandela Emotion Change Over Time") +
  theme(plot.title = element_text(size = 11))

mbeki_emotion_change_over_time <- mbeki_speeches %>%
  left_join(nrc, by = "word") %>%
  mutate(sentiment = ifelse(is.na(sentiment), 'neutral', sentiment)) %>%
  filter(sentiment != "neutral")%>%
  group_by(sentiment,year) %>%
  summarize(n = n())

mbeki_emotion_change_over_time <-mbeki_emotion_change_over_time%>%
  left_join(mbeki_emotion_change_over_time %>% 
              group_by(year) %>% 
              summarise(total = sum(n))) %>%
  mutate(freq = n/total)

mbeki_emotion_change_over_time %>%
  ggplot(aes(x = year, y = freq, colour = sentiment,group=sentiment)) +
  geom_line() + 
  geom_smooth(aes(colour = sentiment))+ ylab("frequency") + xlab("year")+
  ggtitle("Mbeki Emotion Change Over Time") +
  theme(plot.title = element_text(size = 11))

zuma_emotion_change_over_time <- zuma_speeches %>%
  left_join(nrc, by = "word") %>%
  mutate(sentiment = ifelse(is.na(sentiment), 'neutral', sentiment)) %>%
  filter(sentiment != "neutral")%>%
  group_by(sentiment,year) %>%
  summarize(n = n())

zuma_emotion_change_over_time <-zuma_emotion_change_over_time%>%
  left_join(zuma_emotion_change_over_time %>% 
              group_by(year) %>% 
              summarise(total = sum(n))) %>%
  mutate(freq = n/total)

zuma_emotion_change_over_time %>%
  ggplot(aes(x = year, y = freq, colour = sentiment,group=sentiment)) +
  geom_line() + 
  geom_smooth(aes(colour = sentiment))+ ylab("frequency") + xlab("year")+
  ggtitle("Zuma Emotion Change Over Time") +
  theme(plot.title = element_text(size = 11))

ramaphosa_emotion_change_over_time <- ramaphosa_speeches %>%
  left_join(nrc, by = "word") %>%
  mutate(sentiment = ifelse(is.na(sentiment), 'neutral', sentiment)) %>%
  filter(sentiment != "neutral")%>%
  group_by(sentiment,year) %>%
  summarize(n = n())

ramaphosa_emotion_change_over_time <-ramaphosa_emotion_change_over_time%>%
  left_join(ramaphosa_emotion_change_over_time %>% 
              group_by(year) %>% 
              summarise(total = sum(n))) %>%
  mutate(freq = n/total)

ramaphosa_emotion_change_over_time %>%
  ggplot(aes(x = year, y = freq, colour = sentiment,group=sentiment)) +
  geom_line() + 
  geom_smooth(aes(colour = sentiment))+ ylab("frequency") + xlab("year")+
  ggtitle("Ramaphosa Emotion Change Over Time") +
  theme(plot.title = element_text(size = 11))

 