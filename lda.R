library(stringr)
library(tidyverse)
library(tidytext)
library(textstem)
library(textdata)
library(tm)
library(MASS)
library(topicmodels)


#Topic modelling is a type of bag-of-words model that aims to summarize a document by describing it in 
# terms of a small number of “topics”. The resulting descriptions can then be used for several purposes, 
# for example, to judge similarity between documents and to cluster together similar documents, 
# or for document classification. This notebook covers topic modelling and 
# in particular one method used to do topic modelling, latent dirichlet allocation.


set.seed(2023)

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
  mutate(speech = lemmatize_strings(tolower(str_replace_all(speech, replace_reg , ' ')))
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

sona <- as_tibble(sona)

mandela_speeches <- sona %>% filter(president_13 == "Mandela")
mbeki_speeches <- sona %>% filter(president_13 == "Mbeki")
ramaphosa_speeches <- sona %>% filter(president_13 == "Ramaphosa")
zuma_speeches <- sona %>% filter(president_13 == "Zuma")

# per president tokenise the document at the word level; filter out stop words
mandela_tidy_speeches <- mandela_speeches %>% unnest_tokens(word, speech, token = 'words', to_lower = T) %>% filter(!word %in% stop_words$word)
mbeki_tidy_speeches <- mbeki_speeches %>% unnest_tokens(word, speech, token = 'words', to_lower = T) %>% filter(!word %in% stop_words$word)
ramaphosa_tidy_speeches <- ramaphosa_speeches %>% unnest_tokens(word, speech, token = 'words', to_lower = T) %>% filter(!word %in% stop_words$word)
zuma_tidy_speeches <- zuma_speeches %>% unnest_tokens(word, speech, token = 'words', to_lower = T) %>% filter(!word %in% stop_words$word)
all_tidy_speeches <- sona %>% unnest_tokens(word, speech, token = 'words', to_lower = T) %>% filter(!word %in% stop_words$word)

# Counts of number of times each word appears in each document
mandela_tdf <- mandela_tidy_speeches %>% group_by(year,word) %>% count() %>% ungroup() 
mbeki_tdf <- mbeki_tidy_speeches %>% group_by(year,word) %>% count() %>% ungroup() 
ramaphosa_tdf <- ramaphosa_tidy_speeches %>% group_by(year,word) %>% count() %>% ungroup() 
zuma_tdf <- zuma_tidy_speeches %>% group_by(year,word) %>% count() %>% ungroup() 
all_tdf <- all_tidy_speeches %>% group_by(year,word) %>% count() %>% ungroup() 

# Reshape long to wide and put into the DocumentTermMatrix class needed by package topicmodels.
dtm_mandela <- mandela_tdf %>% cast_dtm(year, word, n)
dtm_mbeki <- mbeki_tdf %>% cast_dtm(year, word, n)
dtm_ramaphosa <- ramaphosa_tdf %>% cast_dtm(year, word, n)
dtm_zuma <- zuma_tdf %>% cast_dtm(year, word, n)
dtm_all <- all_tdf %>% cast_dtm(year, word, n)

mandela_lda <- LDA(dtm_mandela, k = 3, control = list(seed = 2023))
mbeki_lda <- LDA(dtm_mbeki, k = 3, control = list(seed = 2023))
ramaphosa_lda <- LDA(dtm_ramaphosa, k = 3, control = list(seed = 2023))
zuma_lda <- LDA(dtm_zuma, k = 3, control = list(seed = 2023))
all_lda <- LDA(dtm_all, k = 3, control = list(seed = 2023))


mandela_word_topics <- tidy(mandela_lda, matrix = 'beta')
mbeki_word_topics <- tidy(mbeki_lda, matrix = 'beta')
ramaphosa_word_topics <- tidy(ramaphosa_lda, matrix = 'beta')
zuma_word_topics <- tidy(zuma_lda, matrix = 'beta')
all_word_topics <- tidy(all_lda, matrix = 'beta')


mandela_word_topics %>%
  group_by(topic) %>%
  slice_max(n = 20, order_by = beta) %>% ungroup() %>%
  arrange(topic, -beta) %>%
  ggplot(aes(reorder(term, beta), beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = 'free') + coord_flip()


mbeki_word_topics %>%
  group_by(topic) %>%
  slice_max(n = 20, order_by = beta) %>% ungroup() %>%
  arrange(topic, -beta) %>%
  ggplot(aes(reorder(term, beta), beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = 'free') + coord_flip()

zuma_word_topics %>%
  group_by(topic) %>%
  slice_max(n = 20, order_by = beta) %>% ungroup() %>%
  arrange(topic, -beta) %>%
  ggplot(aes(reorder(term, beta), beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = 'free') + coord_flip()

ramaphosa_word_topics %>%
  group_by(topic) %>%
  slice_max(n = 20, order_by = beta) %>% ungroup() %>%
  arrange(topic, -beta) %>%
  ggplot(aes(reorder(term, beta), beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = 'free') + coord_flip()


all_word_topics %>%
  group_by(topic) %>%
  slice_max(n = 20, order_by = beta) %>% ungroup() %>%
  arrange(topic, -beta) %>%
  ggplot(aes(reorder(term, beta), beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = 'free') + coord_flip()


# Document topic probabilities
gamma <- tidy(all_lda, matrix = 'gamma') 

sona_gamma <- left_join(sona, 
                           gamma,
                           by = c("year" = "document"))


