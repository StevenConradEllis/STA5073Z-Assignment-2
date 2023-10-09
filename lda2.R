library(stringr)
library(tidyverse)
library(tidytext)
library(textstem)
library(textdata)
library(tm)
library(MASS)
library(topicmodels)

set.seed(2023)

(WD <- getwd())
if (!is.null(WD)) setwd(WD)

# read in text data files and organise these into a data frame
filenames <- c('1994_post_elections_Mandela.txt', '1994_pre_elections_deKlerk.txt', '1995_Mandela.txt', '1996_Mandela.txt', '1997_Mandela.txt', '1998_Mandela.txt', 
               '1999_post_elections_Mandela.txt', '1999_pre_elections_Mandela.txt', '2000_Mbeki.txt', '2001_Mbeki.txt', '2002_Mbeki.txt', '2003_Mbeki.txt', 
               '2004_post_elections_Mbeki.txt', '2004_pre_elections_Mbeki.txt', '2005_Mbeki.txt', '2006_Mbeki.txt', '2007_Mbeki.txt', '2008_Mbeki.txt', 
               '2009_post_elections_Zuma.txt', '2009_pre_elections_ Motlanthe.txt', '2010_Zuma.txt', '2011_Zuma.txt', '2012_Zuma.txt', '2013_Zuma.txt', 
               '2014_post_elections_Zuma.txt', '2014_pre_elections_Zuma.txt', '2015_Zuma.txt', '2016_Zuma.txt', '2017_Zuma.txt', '2018_Ramaphosa.txt', 
               '2019_post_elections_Ramaphosa.txt', '2019_pre_elections_Ramaphosa.txt', '2020_Ramaphosa.txt', '2021_Ramaphosa.txt', '2022_Ramaphosa.txt', '2023_Ramaphosa.txt')


this_speech <- c()
this_speech[1] <- readChar('./sona-addresses-1994-2023/1994_post_elections_Mandela.txt', nchars = 27050)
this_speech[2] <- readChar('./sona-addresses-1994-2023/1994_pre_elections_deKlerk.txt', nchars = 12786)
this_speech[3] <- readChar('./sona-addresses-1994-2023/1995_Mandela.txt', nchars = 39019)
this_speech[4] <- readChar('./sona-addresses-1994-2023/1996_Mandela.txt', nchars = 39524)
this_speech[5] <- readChar('./sona-addresses-1994-2023/1997_Mandela.txt', nchars = 37489)
this_speech[6] <- readChar('./sona-addresses-1994-2023/1998_Mandela.txt', nchars = 45247)
this_speech[7] <- readChar('./sona-addresses-1994-2023/1999_post_elections_Mandela.txt', nchars = 34674)
this_speech[8] <- readChar('./sona-addresses-1994-2023/1999_pre_elections_Mandela.txt', nchars = 41225)
this_speech[9] <- readChar('./sona-addresses-1994-2023/2000_Mbeki.txt', nchars = 37552)
this_speech[10] <- readChar('./sona-addresses-1994-2023/2001_Mbeki.txt', nchars = 41719)
this_speech[11] <- readChar('./sona-addresses-1994-2023/2002_Mbeki.txt', nchars = 50544)
this_speech[12] <- readChar('./sona-addresses-1994-2023/2003_Mbeki.txt', nchars = 58284)
this_speech[13] <- readChar('./sona-addresses-1994-2023/2004_post_elections_Mbeki.txt', nchars = 34590)
this_speech[14] <- readChar('./sona-addresses-1994-2023/2004_pre_elections_Mbeki.txt', nchars = 39232)
this_speech[15] <- readChar('./sona-addresses-1994-2023/2005_Mbeki.txt', nchars = 54635)
this_speech[16] <- readChar('./sona-addresses-1994-2023/2006_Mbeki.txt', nchars = 48643)
this_speech[17] <- readChar('./sona-addresses-1994-2023/2007_Mbeki.txt', nchars = 48641)
this_speech[18] <- readChar('./sona-addresses-1994-2023/2008_Mbeki.txt', nchars = 44907)
this_speech[19] <- readChar('./sona-addresses-1994-2023/2009_post_elections_Zuma.txt', nchars = 31101)
this_speech[20] <- readChar('./sona-addresses-1994-2023/2009_pre_elections_Motlanthe.txt', nchars = 47157)
this_speech[21] <- readChar('./sona-addresses-1994-2023/2010_Zuma.txt', nchars = 26384)
this_speech[22] <- readChar('./sona-addresses-1994-2023/2011_Zuma.txt', nchars = 33281)
this_speech[23] <- readChar('./sona-addresses-1994-2023/2012_Zuma.txt', nchars = 33376)
this_speech[24] <- readChar('./sona-addresses-1994-2023/2013_Zuma.txt', nchars = 36006)
this_speech[25] <- readChar('./sona-addresses-1994-2023/2014_post_elections_Zuma.txt', nchars = 29403)
this_speech[26] <- readChar('./sona-addresses-1994-2023/2014_pre_elections_Zuma.txt', nchars = 36233)
this_speech[27] <- readChar('./sona-addresses-1994-2023/2015_Zuma.txt', nchars = 32860)
this_speech[28] <- readChar('./sona-addresses-1994-2023/2016_Zuma.txt', nchars = 32464)
this_speech[29] <- readChar('./sona-addresses-1994-2023/2017_Zuma.txt', nchars = 35981)
this_speech[30] <- readChar('./sona-addresses-1994-2023/2018_Ramaphosa.txt', nchars = 33290)
this_speech[31] <- readChar('./sona-addresses-1994-2023/2019_post_elections_Ramaphosa.txt', nchars = 42112)
this_speech[32] <- readChar('./sona-addresses-1994-2023/2019_pre_elections_Ramaphosa.txt', nchars = 56960)
this_speech[33] <- readChar('./sona-addresses-1994-2023/2020_Ramaphosa.txt', nchars = 47910)
this_speech[34] <- readChar('./sona-addresses-1994-2023/2021_Ramaphosa.txt', nchars = 43352)
this_speech[35] <- readChar('./sona-addresses-1994-2023/2022_Ramaphosa.txt', nchars = 52972)
this_speech[36] <- readChar('./sona-addresses-1994-2023/2023_Ramaphosa.txt', nchars = 52972)

sona <- data.frame(filename = filenames, speech = this_speech, stringsAsFactors = FALSE)
sona$speech_year <- str_sub(sona$filename, start = 1, end = 4)
sona$pres <- str_remove_all(str_extract(sona$filename, "[dA-Z].*\\."), "\\.")

# clean the sona dataset by adding the date and removing unnecessary text
replace_reg <- '(http.*?(\\s|.$))|(www.*?(\\s|.$))|&amp;|&lt;|&gt;|\n'

sona <-sona %>%
  mutate(speech = stringr::str_replace_all(speech, replace_reg , ' ')
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

# turn into tidy text 
tidy_sona <- sona %>% 
  mutate(speech = str_replace_all(speech, replace_reg, '')) %>%              #remove stuff we don't want like links
  unnest_tokens(word, speech, token = 'regex', pattern = unnest_reg) %>%   #tokenize
  filter(!word %in% stop_words$word, str_detect(word, '[A-Za-z]')) %>%  # remove stop words
  dplyr::select(word, pres, date, filename)            #choose the variables we need 


# Count the number of times each word in our vocabulary was used by each president, 
# creating a "long" (and tidy) format of the document-term matrix.
sona_tdf <- tidy_sona %>%
  group_by(pres,word) %>%
  count() %>%  
  ungroup()

# Remove common SONA words that are used very regularly, but which don't convey too much meaning
common_sona_reg <- 'honourable|chairperson|development|national|ensure|deputy|africa|african|africans|south|southern|government|people|programme|economic|economy|country'
sona_tdf <- sona_tdf %>% filter(!grepl(common_sona_reg, word))

# Create a DocumentTermMatrix object using cast_dtm(), required by the topicmodels package 
# which we will use to implement the LDA topic model
dtm_sona <- sona_tdf %>% cast_dtm(pres, word, n)

# Estimate the parameters of the topic model using LDA
sona_lda <- LDA(dtm_sona, k = 4, control = list(seed = 1234))

# Use the tidy() function by the  tidytext package 
# for extracting the per-topic-per-word probabilities, called β (“beta”), from the model
sona_topics <- tidy(sona_lda, matrix = "beta")

# Notice that this has turned the model into a one-topic-per-term-per-row format. 
# For each combination, the model computes the probability of that term 
# being generated from that topic. 

# Use dplyr’s slice_max() to find the 10 terms that are most common within each topic
sona_top_terms <- sona_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

# Visualize using ggplot
sona_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


