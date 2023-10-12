# DS4I GROUP ASSIGNMENT 2
#rm(list= ls())

library(stringr)
library(tidyverse)
library(tidytext)
library(textstem)

# read in text data files and organise these into a data frame
filenames <- c('1994_post_elections_Mandela.txt', '1994_pre_elections_deKlerk.txt', '1995_Mandela.txt', '1996_Mandela.txt', '1997_Mandela.txt', '1998_Mandela.txt', 
               '1999_post_elections_Mandela.txt', '1999_pre_elections_Mandela.txt', '2000_Mbeki.txt', '2001_Mbeki.txt', '2002_Mbeki.txt', '2003_Mbeki.txt', 
               '2004_post_elections_Mbeki.txt', '2004_pre_elections_Mbeki.txt', '2005_Mbeki.txt', '2006_Mbeki.txt', '2007_Mbeki.txt', '2008_Mbeki.txt', 
               '2009_post_elections_Zuma.txt', '2009_pre_elections_ Motlanthe.txt', '2010_Zuma.txt', '2011_Zuma.txt', '2012_Zuma.txt', '2013_Zuma.txt', 
               '2014_post_elections_Zuma.txt', '2014_pre_elections_Zuma.txt', '2015_Zuma.txt', '2016_Zuma.txt', '2017_Zuma.txt', '2018_Ramaphosa.txt', 
               '2019_post_elections_Ramaphosa.txt', '2019_pre_elections_Ramaphosa.txt', '2020_Ramaphosa.txt', '2021_Ramaphosa.txt', '2022_Ramaphosa.txt', '2023_Ramaphosa.txt')


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
sona$president_13 <- str_remove_all(str_extract(sona$filename, "[dA-Z].*\\."), "\\.")

# clean the sona dataset by adding the date and removing unnecessary text
replace_reg <- '(http.*?(\\s|.$))|(www.*?(\\s|.$))|&amp;|&lt;|&gt;|\n'

sona <-sona %>%
  mutate(speech = str_replace_all(speech, replace_reg , ' ')
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

# Set speech to lower case + lemmatize
sona <- sona %>%
  mutate(speech = lemmatize_strings(tolower(speech)))
 
sona <- as_tibble(sona) # convert dataframe to tibble
sona <- sona[,-1] # removing first column
str(sona) # check structure of data
unique(sona$president_13) # dealing with 6 presidents
table(sona$president_13) # the number of speeches given by each president

# removing deKlerk and Motlanthe from the analysis as they made only 1 speech
sona <- sona %>%
  filter(!president_13 %in% c('deKlerk', 'Motlanthe')) # excluding 1 time president
unique(sona$president_13) # dealing with 4 presidents now

#?unnest_tokens
unnest_reg <- "[^\\w_#@']"


# OVERALL COMMON WORDS ANALYSIS -------------------------------------------
#  Most common words used by all presidents -------------------------------
overall_common_words <- sona %>% 
  unnest_tokens(word, speech, token = 'regex', pattern = unnest_reg) %>%
  filter(str_detect(word, '[a-z]')) %>% 
  filter(!word %in% stop_words$word) %>%
  count(word, sort = T) %>%
  slice(1:20) %>%
  #filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = -n)) + geom_col() + coord_flip() + ylab('Count') + xlab('') +
  ggtitle("Top 20 words used by all presidents") +
  theme(plot.title = element_text(size = 11), legend.position = '')+
  scale_fill_gradient(low = "black", high = "grey")



# Most common bigram used by all presidents -------------------------------
overall_common_bigram <- sona %>% 
  unnest_tokens(bigram, speech, token = 'ngrams', n=2) %>%
  separate(bigram, c('word1', 'word2'), sep = ' ') %>%
  filter(str_detect(word1, '[a-z]'), str_detect(word2, '[a-z]')) %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word) %>%
  unite(bigram, word1, word2, sep = ' ') %>%
  count(bigram, sort = T) %>%
  slice(1:20) %>%
  #filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(x = reorder(bigram, n), y = n, fill = -n)) + geom_col() + coord_flip() + ylab('Count') + xlab('') +
  ggtitle("Top 20 bigrams used by all presidents") +
  theme(plot.title = element_text(size = 11), legend.position = '')+
  scale_fill_gradient(low = "black", high = "grey")


# Most common trigram used by all presidents ------------------------------
overall_common_trigram <- sona %>% 
  unnest_tokens(trigram, speech, token = 'ngrams', n=3) %>%
  separate(trigram, c('word1', 'word2', 'word3'), sep = ' ') %>%
  filter(str_detect(word1, '[a-z]'), str_detect(word2, '[a-z]'), str_detect(word3, '[a-z]')) %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word & !word3 %in% stop_words$word) %>%
  unite(trigram, word1, word2, word3, sep = ' ') %>%
  count(trigram, sort = T) %>%
  slice(1:20) %>%
  #filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(x = reorder(trigram, n), y = n, fill = -n)) + geom_col() + coord_flip() + ylab('Count') + xlab('') +
  ggtitle("Top 20 trigrams used by all presidents") +
  theme(plot.title = element_text(size = 11), legend.position = '')+
  scale_fill_gradient(low = "black", high = "grey")

# combining above plots in one figure
cowplot::plot_grid(overall_common_words, overall_common_bigram, overall_common_trigram,
                   nrow = 1, ncol = 3, labels = "AUTO", label_size = 10, label_x = 0)


# PER PRESIDENT COMMON WORDS ANALYSIS -------------------------------------
# Most common words used per president ------------------------------------
common_words_Mandela <- sona %>% 
  filter(president_13 == 'Mandela') %>%
  unnest_tokens(word, speech, token = 'regex', pattern = unnest_reg) %>%
  filter(str_detect(word, '[a-z]')) %>% 
  filter(!word %in% stop_words$word) %>%
  count(word, sort = T) %>%
  slice(1:20) %>%
  #filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = -n)) + geom_col() + coord_flip() + ylab('Count') + xlab('') +
  ggtitle("Top 20 words used by Mandela") +
  theme(plot.title = element_text(size = 11), legend.position = '') +
  scale_fill_gradient(low = "deeppink3", high = "pink")


common_words_Mbeki <- sona %>% 
  filter(president_13 == 'Mbeki') %>%
  unnest_tokens(word, speech, token = 'regex', pattern = unnest_reg) %>%
  filter(str_detect(word, '[a-z]')) %>% 
  filter(!word %in% stop_words$word) %>%
  count(word, sort = T) %>%
  slice(1:20) %>%
  #filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = -n)) + geom_col() + coord_flip() + ylab('Count') + xlab('') +
  ggtitle("Top 20 words used by Mbeki") +
  theme(plot.title = element_text(size = 11), legend.position = '')


common_words_Zuma <- sona %>% 
  filter(president_13 == 'Zuma') %>%
  unnest_tokens(word, speech, token = 'regex', pattern = unnest_reg) %>%
  filter(str_detect(word, '[a-z]')) %>% 
  filter(!word %in% stop_words$word) %>%
  count(word, sort = T) %>%
  slice(1:20) %>%
  #filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = -n)) + geom_col() + coord_flip() + ylab('Count') + xlab('') +
  ggtitle("Top 20 words used by Zuma") +
  theme(plot.title = element_text(size = 11), legend.position = '') + 
  scale_fill_gradient(low = "magenta4", high = "plum")


common_words_Ramaphosa<- sona %>% 
  filter(president_13 == 'Ramaphosa') %>%
  unnest_tokens(word, speech, token = 'regex', pattern = unnest_reg) %>%
  filter(str_detect(word, '[a-z]')) %>% 
  filter(!word %in% stop_words$word) %>%
  count(word, sort = T) %>%
  slice(1:20) %>%
  #filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = -n)) + geom_col() + coord_flip() + ylab('Count') + xlab('') +
  ggtitle("Top 20 words used by Ramaphosa") +
  theme(plot.title = element_text(size = 11), legend.position = '') + 
  scale_fill_gradient(low = "darkgreen", high = "darkseagreen1")

# combining above plots in one figure
cowplot::plot_grid(common_words_Mandela, common_words_Mbeki,
                   common_words_Zuma, common_words_Ramaphosa,
                   nrow = 2, ncol = 2, labels = "AUTO", label_size = 10, label_x = 0)


# Most common bigrams used per president ----------------------------------
common_bigram_Mandela <- sona %>% 
  filter(president_13 == 'Mandela') %>%
  unnest_tokens(bigram, speech, token = 'ngrams', n=2) %>%
  separate(bigram, c('word1', 'word2'), sep = ' ') %>%
  filter(str_detect(word1, '[a-z]'), str_detect(word2, '[a-z]')) %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word) %>%
  unite(bigram, word1, word2, sep = ' ') %>%
  count(bigram, sort = T) %>%
  slice(1:20) %>%
  #filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(x = reorder(bigram, n), y = n, fill = -n)) + geom_col() + coord_flip() + ylab('Count') + xlab('') +
  ggtitle("Top 20 bigrams used by Mandela") +
  theme(plot.title = element_text(size = 11), legend.position = '') +
  scale_fill_gradient(low = "deeppink3", high = "pink")

common_bigram_Mbeki <- sona %>% 
  filter(president_13 == 'Mbeki') %>%
  unnest_tokens(bigram, speech, token = 'ngrams', n=2) %>%
  separate(bigram, c('word1', 'word2'), sep = ' ') %>%
  filter(str_detect(word1, '[a-z]'), str_detect(word2, '[a-z]')) %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word) %>%
  unite(bigram, word1, word2, sep = ' ') %>%
  count(bigram, sort = T) %>%
  slice(1:20) %>%
  #filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(x = reorder(bigram, n), y = n, fill = -n)) + geom_col() + coord_flip() + ylab('Count') + xlab('') +
  ggtitle("Top 20 bigrams used by Mbeki") +
  theme(plot.title = element_text(size = 11), legend.position = '')

common_bigram_Zuma <- sona %>% 
  filter(president_13 == 'Zuma') %>%
  unnest_tokens(bigram, speech, token = 'ngrams', n=2) %>%
  separate(bigram, c('word1', 'word2'), sep = ' ') %>%
  filter(str_detect(word1, '[a-z]'), str_detect(word2, '[a-z]')) %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word) %>%
  unite(bigram, word1, word2, sep = ' ') %>%
  count(bigram, sort = T) %>%
  slice(1:20) %>%
  #filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(x = reorder(bigram, n), y = n, fill = -n)) + geom_col() + coord_flip() + ylab('Count') + xlab('') +
  ggtitle("Top 20 bigrams used by Zuma") +
  theme(plot.title = element_text(size = 11), legend.position = '') +
  scale_fill_gradient(low = "magenta4", high = "plum")

common_bigram_Ramaphosa <- sona %>% 
  filter(president_13 == 'Ramaphosa') %>%
  unnest_tokens(bigram, speech, token = 'ngrams', n=2) %>%
  separate(bigram, c('word1', 'word2'), sep = ' ') %>%
  filter(str_detect(word1, '[a-z]'), str_detect(word2, '[a-z]')) %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word) %>%
  unite(bigram, word1, word2, sep = ' ') %>%
  count(bigram, sort = T) %>%
  slice(1:20) %>%
  #filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(x = reorder(bigram, n), y = n, fill = -n)) + geom_col() + coord_flip() + ylab('Count') + xlab('') +
  ggtitle("Top 20 bigrams used by Ramaphosa") +
  theme(plot.title = element_text(size = 11), legend.position = '') +
  scale_fill_gradient(low = "darkgreen", high = "darkseagreen1")

# combining above plots in one figure
cowplot::plot_grid(common_bigram_Mandela, common_bigram_Mbeki,
                   common_bigram_Zuma, common_bigram_Ramaphosa,
                   nrow = 2, ncol = 2, labels = "AUTO", label_size = 10, label_x = 0)


# Most common trigram used per president ----------------------------------
common_trigram_Mandela <- sona %>% 
  filter(president_13 == 'Mandela') %>%
  unnest_tokens(trigram, speech, token = 'ngrams', n=3) %>%
  separate(trigram, c('word1', 'word2', 'word3'), sep = ' ') %>%
  filter(str_detect(word1, '[a-z]'), str_detect(word2, '[a-z]'), str_detect(word3, '[a-z]')) %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word & !word3 %in% stop_words$word) %>%
  unite(trigram, word1, word2, word3, sep = ' ') %>%
  count(trigram, sort = T) %>%
  slice(1:20) %>%
  #filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(x = reorder(trigram, n), y = n, fill = -n)) + geom_col() + coord_flip() + ylab('Count') + xlab('') +
  ggtitle("Top 20 trigrams used by Mandela") +
  theme(plot.title = element_text(size = 11), legend.position = '') +
  scale_fill_gradient(low = "deeppink3", high = "pink")
  
common_trigram_Mbeki <- sona %>% 
  filter(president_13 == 'Mbeki') %>%
  unnest_tokens(trigram, speech, token = 'ngrams', n=3) %>%
  separate(trigram, c('word1', 'word2', 'word3'), sep = ' ') %>%
  filter(str_detect(word1, '[a-z]'), str_detect(word2, '[a-z]'), str_detect(word3, '[a-z]')) %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word & !word3 %in% stop_words$word) %>%
  unite(trigram, word1, word2, word3, sep = ' ') %>%
  count(trigram, sort = T) %>%
  slice(1:20) %>%
  #filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(x = reorder(trigram, n), y = n, fill = -n)) + geom_col() + coord_flip() + ylab('Count') + xlab('') +
  ggtitle("Top 20 trigrams used by Mbeki") +
  theme(plot.title = element_text(size = 11), legend.position = '') 

common_trigram_Zuma <- sona %>% 
  filter(president_13 == 'Zuma') %>%
  unnest_tokens(trigram, speech, token = 'ngrams', n=3) %>%
  separate(trigram, c('word1', 'word2', 'word3'), sep = ' ') %>%
  filter(str_detect(word1, '[a-z]'), str_detect(word2, '[a-z]'), str_detect(word3, '[a-z]')) %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word & !word3 %in% stop_words$word) %>%
  unite(trigram, word1, word2, word3, sep = ' ') %>%
  count(trigram, sort = T) %>%
  slice(1:20) %>%
  #filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(x = reorder(trigram, n), y = n, fill = -n)) + geom_col() + coord_flip() + ylab('Count') + xlab('') +
  ggtitle("Top 20 trigrams used by Zuma") +
  theme(plot.title = element_text(size = 11), legend.position = '') +
  scale_fill_gradient(low = "magenta4", high = "plum") 

common_trigram_Ramaphosa <- sona %>% 
  filter(president_13 == 'Ramaphosa') %>%
  unnest_tokens(trigram, speech, token = 'ngrams', n=3) %>%
  separate(trigram, c('word1', 'word2', 'word3'), sep = ' ') %>%
  filter(str_detect(word1, '[a-z]'), str_detect(word2, '[a-z]'), str_detect(word3, '[a-z]')) %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word & !word3 %in% stop_words$word) %>%
  unite(trigram, word1, word2, word3, sep = ' ') %>%
  count(trigram, sort = T) %>%
  slice(1:20) %>%
  #filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(x = reorder(trigram, n), y = n, fill = -n)) + geom_col() + coord_flip() + ylab('Count') + xlab('') +
  ggtitle("Top 20 trigrams used by Ramaphosa") +
  theme(plot.title = element_text(size = 11), legend.position = '') +
  scale_fill_gradient(low = "darkgreen", high = "darkseagreen1")

# combining above plots in one figure
cowplot::plot_grid(common_trigram_Mandela, common_trigram_Mbeki,
                   common_trigram_Zuma, common_trigram_Ramaphosa,
                   nrow = 2, ncol = 2, labels = "AUTO", label_size = 10, label_x = 0)

# SENTIMENT ANALYSIS ------------------------------------------------------

# import sentiment dictionaries
afinn <- get_sentiments('afinn') 
unique(afinn$value)

bing <- get_sentiments('bing') 
unique(bing$sentiment)

nrc <- get_sentiments('nrc') 
unique(nrc$sentiment)

save(afinn, bing, nrc, file = "dsfi-lexicons.Rdata")



# Most common positive words used by all presidents -----------------------
overall_common_positive_words <- sona %>% 
  unnest_tokens(word, speech, token = 'words') %>%
  filter(str_detect(word, '[a-z]')) %>% 
  filter(!word %in% stop_words$word) %>%
  left_join(bing, by = 'word') %>%
  rename(bing_sentiment = sentiment) %>%
  mutate(bing_sentiment = ifelse(is.na(bing_sentiment), 'neutral', bing_sentiment)) %>%
  filter(bing_sentiment == 'positive') %>%
  count(word, sort = T) %>%
  slice(1:20) %>%
  #filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = -n)) + geom_col() + coord_flip() + ylab('Count') + xlab('') +
  ggtitle("Top 20 positive words used by all presidents") +
  theme(plot.title = element_text(size = 11), legend.position = '')+
  scale_fill_gradient(low = "springgreen", high = "darkseagreen1")
  
# Most common negative words used by all presidents -----------------------
overall_common_negative_words <- sona %>% 
  unnest_tokens(word, speech, token = 'words') %>%
  filter(str_detect(word, '[a-z]')) %>% 
  filter(!word %in% stop_words$word) %>%
  left_join(bing, by = 'word') %>%
  rename(bing_sentiment = sentiment) %>%
  mutate(bing_sentiment = ifelse(is.na(bing_sentiment), 'neutral', bing_sentiment)) %>%
  filter(bing_sentiment == 'negative') %>%
  count(word, sort = T) %>%
  slice(1:20) %>%
  #filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = -n)) + geom_col() + coord_flip() + ylab('Count') + xlab('') +
  ggtitle("Top 20 negative words used by all presidents") +
  theme(plot.title = element_text(size = 11), legend.position = '')+
  scale_fill_gradient(low = "red", high = "pink")


# combining above plots in one figure
cowplot::plot_grid(overall_common_positive_words, overall_common_negative_words,
                   nrow = 1, ncol = 2, labels = "AUTO", label_size = 10, label_x = 0)

# Most common positive words used per president ---------------------------
common_positive_words_Mandela <- sona %>% 
  filter(president_13 == 'Mandela') %>%
  unnest_tokens(word, speech, token = 'words') %>%
  filter(str_detect(word, '[a-z]')) %>% 
  filter(!word %in% stop_words$word) %>%
  left_join(bing, by = 'word') %>%
  rename(bing_sentiment = sentiment) %>%
  mutate(bing_sentiment = ifelse(is.na(bing_sentiment), 'neutral', bing_sentiment)) %>%
  filter(bing_sentiment == 'positive') %>%
  count(word, sort = T) %>%
  slice(1:20) %>%
  #filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = -n)) + geom_col() + coord_flip() + ylab('Count') + xlab('') +
  ggtitle("Top 20 positive words used by Mandela") +
  theme(plot.title = element_text(size = 11), legend.position = '')+
  scale_fill_gradient(low = "deeppink3", high = "pink")

common_positive_words_Mbeki <- sona %>% 
  filter(president_13 == 'Mbeki') %>%
  unnest_tokens(word, speech, token = 'words') %>%
  filter(str_detect(word, '[a-z]')) %>% 
  filter(!word %in% stop_words$word) %>%
  left_join(bing, by = 'word') %>%
  rename(bing_sentiment = sentiment) %>%
  mutate(bing_sentiment = ifelse(is.na(bing_sentiment), 'neutral', bing_sentiment)) %>%
  filter(bing_sentiment == 'positive') %>%
  count(word, sort = T) %>%
  slice(1:20) %>%
  #filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = -n)) + geom_col() + coord_flip() + ylab('Count') + xlab('') +
  ggtitle("Top 20 positive words used by Mbeki") +
  theme(plot.title = element_text(size = 11), legend.position = '')

common_positive_words_Zuma <- sona %>% 
  filter(president_13 == 'Zuma') %>%
  unnest_tokens(word, speech, token = 'words') %>%
  filter(str_detect(word, '[a-z]')) %>% 
  filter(!word %in% stop_words$word) %>%
  left_join(bing, by = 'word') %>%
  rename(bing_sentiment = sentiment) %>%
  mutate(bing_sentiment = ifelse(is.na(bing_sentiment), 'neutral', bing_sentiment)) %>%
  filter(bing_sentiment == 'positive') %>%
  count(word, sort = T) %>%
  slice(1:20) %>%
  #filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = -n)) + geom_col() + coord_flip() + ylab('Count') + xlab('') +
  ggtitle("Top 20 positive words used by Zuma") +
  theme(plot.title = element_text(size = 11), legend.position = '') +
  scale_fill_gradient(low = "magenta4", high = "plum") 

common_positive_words_Ramaphosa <- sona %>% 
  filter(president_13 == 'Ramaphosa') %>%
  unnest_tokens(word, speech, token = 'words') %>%
  filter(str_detect(word, '[a-z]')) %>% 
  filter(!word %in% stop_words$word) %>%
  left_join(bing, by = 'word') %>%
  rename(bing_sentiment = sentiment) %>%
  mutate(bing_sentiment = ifelse(is.na(bing_sentiment), 'neutral', bing_sentiment)) %>%
  filter(bing_sentiment == 'positive') %>%
  count(word, sort = T) %>%
  slice(1:20) %>%
  #filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = -n)) + geom_col() + coord_flip() + ylab('Count') + xlab('') +
  ggtitle("Top 20 positive words used by Ramaphosa") +
  theme(plot.title = element_text(size = 11), legend.position = '')  +
  scale_fill_gradient(low = "darkgreen", high = "darkseagreen1")

# combining above plots in one figure
cowplot::plot_grid(common_positive_words_Mandela, common_positive_words_Mbeki,
                   common_positive_words_Zuma, common_positive_words_Ramaphosa,
                   nrow = 2, ncol = 2, labels = "AUTO", label_size = 10, label_x = 0)



# Most common negative words used per president ---------------------------

common_negative_words_Mandela <- sona %>% 
  filter(president_13 == 'Mandela') %>%
  unnest_tokens(word, speech, token = 'words') %>%
  filter(str_detect(word, '[a-z]')) %>% 
  filter(!word %in% stop_words$word) %>%
  left_join(bing, by = 'word') %>%
  rename(bing_sentiment = sentiment) %>%
  mutate(bing_sentiment = ifelse(is.na(bing_sentiment), 'neutral', bing_sentiment)) %>%
  filter(bing_sentiment == 'negative') %>%
  count(word, sort = T) %>%
  slice(1:20) %>%
  #filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = -n)) + geom_col() + coord_flip() + ylab('Count') + xlab('') +
  ggtitle("Top 20 negative words used by Mandela") +
  theme(plot.title = element_text(size = 11), legend.position = '')+
  scale_fill_gradient(low = "deeppink3", high = "pink")

common_negative_words_Mbeki <- sona %>% 
  filter(president_13 == 'Mbeki') %>%
  unnest_tokens(word, speech, token = 'words') %>%
  filter(str_detect(word, '[a-z]')) %>% 
  filter(!word %in% stop_words$word) %>%
  left_join(bing, by = 'word') %>%
  rename(bing_sentiment = sentiment) %>%
  mutate(bing_sentiment = ifelse(is.na(bing_sentiment), 'neutral', bing_sentiment)) %>%
  filter(bing_sentiment == 'negative') %>%
  count(word, sort = T) %>%
  slice(1:20) %>%
  #filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = -n)) + geom_col() + coord_flip() + ylab('Count') + xlab('') +
  ggtitle("Top 20 negative words used by Mbeki") +
  theme(plot.title = element_text(size = 11), legend.position = '')

common_negative_words_Zuma <- sona %>% 
  filter(president_13 == 'Zuma') %>%
  unnest_tokens(word, speech, token = 'words') %>%
  filter(str_detect(word, '[a-z]')) %>% 
  filter(!word %in% stop_words$word) %>%
  left_join(bing, by = 'word') %>%
  rename(bing_sentiment = sentiment) %>%
  mutate(bing_sentiment = ifelse(is.na(bing_sentiment), 'neutral', bing_sentiment)) %>%
  filter(bing_sentiment == 'negative') %>%
  count(word, sort = T) %>%
  slice(1:20) %>%
  #filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = -n)) + geom_col() + coord_flip() + ylab('Count') + xlab('') +
  ggtitle("Top 20 negative words used by Zuma") +
  theme(plot.title = element_text(size = 11), legend.position = '') +
  scale_fill_gradient(low = "magenta4", high = "plum") 

common_negative_words_Ramaphosa <- sona %>% 
  filter(president_13 == 'Ramaphosa') %>%
  unnest_tokens(word, speech, token = 'words') %>%
  filter(str_detect(word, '[a-z]')) %>% 
  filter(!word %in% stop_words$word) %>%
  left_join(bing, by = 'word') %>%
  rename(bing_sentiment = sentiment) %>%
  mutate(bing_sentiment = ifelse(is.na(bing_sentiment), 'neutral', bing_sentiment)) %>%
  filter(bing_sentiment == 'negative') %>%
  count(word, sort = T) %>%
  slice(1:20) %>%
  #filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = -n)) + geom_col() + coord_flip() + ylab('Count') + xlab('') +
  ggtitle("Top 20 negative words used by Ramaphosa") +
  theme(plot.title = element_text(size = 11), legend.position = '')  +
  scale_fill_gradient(low = "darkgreen", high = "darkseagreen1")

# combining above plots in one figure
cowplot::plot_grid(common_negative_words_Mandela, common_negative_words_Mbeki,
                   common_negative_words_Zuma, common_negative_words_Ramaphosa,
                   nrow = 2, ncol = 2, labels = "AUTO", label_size = 10, label_x = 0)


# Overall change in word sentiment over time ------------------------------

overall_word_bing_sentiment_change_over_time <- sona %>% 
  unnest_tokens(word, speech, token = 'words') %>%
  filter(str_detect(word, '[a-z]')) %>% 
  filter(!word %in% stop_words$word) %>%
  left_join(bing, by = 'word') %>%
  rename(bing_sentiment = sentiment) %>%
  mutate(bing_sentiment = ifelse(is.na(bing_sentiment), 'neutral', bing_sentiment)) %>%
  group_by(bing_sentiment, year) %>%
  tally() %>%
  pivot_wider(names_from = 'bing_sentiment', values_from = n, values_fill = 0) %>%
  mutate(bing_sentiment_pos_minus_neg = positive - negative) %>%
  ggplot(aes(x = year, y = bing_sentiment_pos_minus_neg)) + 
  geom_col(fill = 'springgreen') + 
  geom_col(data=. %>% filter(bing_sentiment_pos_minus_neg<0), fill = 'red') +
  xlab('Year') + ylab('Sentiment') +
  theme(plot.title = element_text(size = 11)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_rect(aes(xmin = 1 - 0.5, xmax = 6 + 0.5, ymin = -5, ymax = 103),
            fill = "transparent", color = "deeppink3", size = .05, linetype = 'dashed') + annotate(geom="text", x=1.15, y= 5+103, label="Mandela",color="deeppink3", size = 3) +
  geom_rect(aes(xmin = 7 - 0.5, xmax = 15 + 0.5, ymin = 0, ymax = 183),
            fill = "transparent", color = "steelblue", size = .05, linetype = 'dashed') + annotate(geom="text", x=7, y= 5+183, label="Mbeki",color="steelblue", size = 3) +
  geom_rect(aes(xmin = 16 - 0.5, xmax = 24 + 0.5, ymin = 0, ymax = 108),
            fill = "transparent", color = "magenta4", size = .05, linetype = 'dashed') + annotate(geom="text", x=16, y=5+108, label="Zuma",color="magenta4", size = 3) +
  geom_rect(aes(xmin = 25 - 0.5, xmax = 30 + 0.5, ymin = 0, ymax = 215),
            fill = "transparent", color = "darkgreen", size = .05, linetype = 'dashed') + annotate(geom="text", x=25+.3, y=5+215, label="Ramaphosa",color="darkgreen", size = 3) 
  
  
overall_pos_neg_fluctuation <- sona %>% 
  unnest_tokens(word, speech, token = 'words') %>%
  filter(str_detect(word, '[a-z]')) %>% 
  filter(!word %in% stop_words$word) %>%
  left_join(bing, by = 'word') %>%
  rename(bing_sentiment = sentiment) %>%
  mutate(bing_sentiment = ifelse(is.na(bing_sentiment), 'neutral', bing_sentiment)) %>%
  group_by(bing_sentiment, year) %>%
  tally() %>%
  filter(bing_sentiment != 'neutral') %>%
  ggplot(aes(x = year, y = n, colour = bing_sentiment, group = bing_sentiment)) + 
  geom_line() +
  scale_colour_manual(values = c("positive" = "springgreen", "negative" = "red")) +
  geom_smooth(linewidth = 0.2, alpha = 0.2, level = 0.6) +
  ylab('Count') +
  xlab('Year') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = 'bottom') +
  labs(color = '') +
  geom_rect(aes(xmin = 1 , xmax = 6 , ymin = 0, ymax = 460),
            fill = "transparent", color = "deeppink3", size = .05, linetype = 'dashed') + annotate(geom="text", x=1+.5, y=13+460, label="Mandela",color="deeppink3", size = 3) +
  geom_rect(aes(xmin = 7 , xmax = 15 , ymin = 0, ymax = 390),
            fill = "transparent", color = "steelblue", size = .05, linetype = 'dashed') + annotate(geom="text", x=7+.5, y=13+390, label="Mbeki",color="steelblue", size = 3) +
  geom_rect(aes(xmin = 16, xmax = 24, ymin = 0, ymax = 300),
            fill = "transparent", color = "magenta4", size = .05, linetype = 'dashed') + annotate(geom="text", x=16+.5, y=13+300, label="Zuma",color="magenta4", size = 3) +
  geom_rect(aes(xmin = 25, xmax = 30, ymin = 0, ymax = 560),
            fill = "transparent", color = "darkgreen", size = .05, linetype = 'dashed') + annotate(geom="text", x=25+.8, y=13+560, label="Ramaphosa",color="darkgreen", size = 3) 
  

# combining above plots in one figure
cowplot::plot_grid(overall_word_bing_sentiment_change_over_time, overall_pos_neg_fluctuation,
                   nrow = 2, ncol = 1, labels = "AUTO", label_size = 10, label_x = 0)


# Most common positive trigrams used by all presidents --------------------

bing_word1 <- bing %>%
  mutate(word1 = word) %>% select(-1)
bing_word2 <- bing %>%
  mutate(word2 = word) %>% select(-1)
bing_word3 <- bing %>%
  mutate(word3 = word) %>% select(-1)

overall_common_positive_trigrams <- sona %>% 
  unnest_tokens(trigram, speech, token = 'ngrams', n=3) %>%
  separate(trigram, c('word1', 'word2', 'word3'), sep = ' ') %>%
  filter(str_detect(word1, '[a-z]'), str_detect(word2, '[a-z]'), str_detect(word3, '[a-z]')) %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word & !word3 %in% stop_words$word) %>%
  left_join(bing_word1, by = 'word1') %>% rename(bing_sentiment1 = sentiment) %>%
  left_join(bing_word2, by = 'word2') %>% rename(bing_sentiment2 = sentiment) %>%
  left_join(bing_word3, by = 'word3') %>% rename(bing_sentiment3 = sentiment) %>%
  unite(trigram, word1, word2, word3, sep = ' ') %>%
  mutate(bing_sentiment1 = ifelse(is.na(bing_sentiment1), 'neutral', bing_sentiment1)) %>%
  mutate(bing_sentiment2 = ifelse(is.na(bing_sentiment2), 'neutral', bing_sentiment2)) %>%
  mutate(bing_sentiment3 = ifelse(is.na(bing_sentiment3), 'neutral', bing_sentiment3)) %>%
  mutate(bing_sentiment1_score = case_when(bing_sentiment1 == 'neutral' ~ 0,
                                           bing_sentiment1 == 'positive' ~ 1,
                                           bing_sentiment1 == 'negative' ~ -1),
         bing_sentiment2_score = case_when(bing_sentiment2 == 'neutral' ~ 0,
                                           bing_sentiment2 == 'positive' ~ 1,
                                           bing_sentiment2 == 'negative' ~ -1),
         bing_sentiment3_score = case_when(bing_sentiment3 == 'neutral' ~ 0,
                                           bing_sentiment3 == 'positive' ~ 1,
                                           bing_sentiment3 == 'negative' ~ -1),
         bing_sentiment_finalscore = bing_sentiment1_score +bing_sentiment2_score+bing_sentiment3_score,
         bing_sentiment = case_when(bing_sentiment_finalscore == 0 ~ 'neutral',
                                    bing_sentiment_finalscore >= 1 ~ 'positive',
                                    bing_sentiment_finalscore <  1 ~ 'negative')) %>%
  filter(bing_sentiment == 'positive') %>%
  count(trigram, sort = T) %>%
  slice(1:20) %>%
  #filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(x = reorder(trigram, n), y = n, fill = -n)) + geom_col() + coord_flip() + ylab('Count') + xlab('') +
  ggtitle("Top 20 positive trigrams used by all presidents") +
  theme(plot.title = element_text(size = 11), legend.position = '')+
  scale_fill_gradient(low = "springgreen", high = "darkseagreen1")



overall_common_negative_trigrams <- sona %>% 
  unnest_tokens(trigram, speech, token = 'ngrams', n=3) %>%
  separate(trigram, c('word1', 'word2', 'word3'), sep = ' ') %>%
  filter(str_detect(word1, '[a-z]'), str_detect(word2, '[a-z]'), str_detect(word3, '[a-z]')) %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word & !word3 %in% stop_words$word) %>%
  left_join(bing_word1, by = 'word1') %>% rename(bing_sentiment1 = sentiment) %>%
  left_join(bing_word2, by = 'word2') %>% rename(bing_sentiment2 = sentiment) %>%
  left_join(bing_word3, by = 'word3') %>% rename(bing_sentiment3 = sentiment) %>%
  unite(trigram, word1, word2, word3, sep = ' ') %>%
  mutate(bing_sentiment1 = ifelse(is.na(bing_sentiment1), 'neutral', bing_sentiment1)) %>%
  mutate(bing_sentiment2 = ifelse(is.na(bing_sentiment2), 'neutral', bing_sentiment2)) %>%
  mutate(bing_sentiment3 = ifelse(is.na(bing_sentiment3), 'neutral', bing_sentiment3)) %>%
  mutate(bing_sentiment1_score = case_when(bing_sentiment1 == 'neutral' ~ 0,
                                           bing_sentiment1 == 'positive' ~ 1,
                                           bing_sentiment1 == 'negative' ~ -1),
         bing_sentiment2_score = case_when(bing_sentiment2 == 'neutral' ~ 0,
                                           bing_sentiment2 == 'positive' ~ 1,
                                           bing_sentiment2 == 'negative' ~ -1),
         bing_sentiment3_score = case_when(bing_sentiment3 == 'neutral' ~ 0,
                                           bing_sentiment3 == 'positive' ~ 1,
                                           bing_sentiment3 == 'negative' ~ -1),
         bing_sentiment_finalscore = bing_sentiment1_score +bing_sentiment2_score+bing_sentiment3_score,
         bing_sentiment = case_when(bing_sentiment_finalscore == 0 ~ 'neutral',
                                    bing_sentiment_finalscore >= 1 ~ 'positive',
                                    bing_sentiment_finalscore <  1 ~ 'negative')) %>%
  filter(bing_sentiment == 'negative') %>%
  count(trigram, sort = T) %>%
  slice(1:20) %>%
  #filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(x = reorder(trigram, n), y = n, fill = -n)) + geom_col() + coord_flip() + ylab('Count') + xlab('') +
  ggtitle("Top 20 negative trigrams used by all presidents") +
  theme(plot.title = element_text(size = 11), legend.position = '')+
  scale_fill_gradient(low = "red", high = "pink")


# combining above plots in one figure
cowplot::plot_grid(overall_common_positive_trigrams, overall_common_negative_trigrams,
                   nrow = 2, ncol = 1, labels = "AUTO", label_size = 10, label_x = 0)


