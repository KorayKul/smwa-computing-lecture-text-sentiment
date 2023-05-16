#' trump_sentiment.R
#'
#' Analyse Sentiment in Trumps' tweets as President in Early 2020
#'

# --- Library --- #
library(readr)
library(dplyr)
library(tibble)
library(tidyr)
library(stringr)
library(tidytext)
library(vader)
library(lubridate)
library(tokenizers)
library(textdata)
library(yardstick)
library(ggplot2)

# --- read data --- #

tweets <- read_csv('data/trump_early_2020_tweets.csv')

# add calender week to data
tweets <-
    tweets %>%
    mutate(cal_week = week(date_est))

# --- Clean Twitter Junk --- #

tweets <-
    tweets %>%
    mutate(
        # remove links
        text = str_remove_all(text, "https\\S*"),
        text = str_remove_all(text, "http\\S*"),
        text = str_remove_all(text, "t.co*"),
        # remove mentions
        text = str_remove_all(text, "@\\S*"),
        # remove annoying html stuff
        text = str_remove_all(text, "amp"),
        text = str_remove_all(text, "&S*"),
        text = str_replace_all(text, "&#x27;|&quot;|&#x2F;", "'"),
        text = str_replace_all(text, "<a(.*?)>", " "),
        text = str_replace_all(text, "&gt;|&lt;|&amp;", " "),
        text = str_replace_all(text, "&#[:digit:]+;", " "),
        text = str_remove_all(text, "<[^>]*>"),
        # remove numbers
        text = str_remove_all(text, "[:digit:]"),
        # remove excess whitespace
        text = str_squish(text),
        text = str_trim(text),
        # remove RT for retweets -- keeping retweets in the data
        text = str_remove_all(text, "RT")
    ) %>%
    filter(count_words(text) > 1) %>%
    rownames_to_column("id") %>%
    select(-text_id)

# --- Tweets to Tidy Tweets --- #
tidy_tweets <- 
  tweets %>% 
  unnest_tokens(word, text)


# --- Remove Stopwords --- #

tidy_tweets <- 
  tidy_tweets %>% 
  anti_join(stop_words)


# ---  What does Trump tweet about at different points in the day? --- #
# note: probably not covered in lecture due to time constraint

# --- Sentiment Analysis - AFINN --- #
get_sentiments("afinn") # any word has negative sentiment ant number is magnitute for instance -5 is maximum negative
get_sentiments("afinn") %>% filter(value==-5)
get_sentiments("afinn") %>% filter(value==-1)
get_sentiments("afinn") %>% filter(value==5)
get_sentiments("afinn") %>% filter(value==1)

# add sentiment of word back to the data
tidy_afinn <- tidy_tweets %>% 
  left_join(get_sentiments("afinn"))

# go from word level sentiment to tweet level sentiment
sentiment_afinn <- tidy_afinn %>% 
  group_by(id) %>% 
  summarise(score = sum(value, na.rm = TRUE)) %>% 
  # mutate(id = as.numeric(id)) %>% 
  # arrange(id) %>% 
  ungroup() %>% 
  mutate(sentiment_afinn = case_when(
    score > 0 ~ "positive",
    score < 0 ~ "negative",
    TRUE ~ "neutral"
  )) %>% 
  select(-score)

tweets <- tweets %>% 
  left_join(sentiment_afinn)


# --- Sentiment Analysis - BING --- # 
# your sent lexicon have intensities in the previous example then use BING
get_sentiments("bing")
tidy_bing <- 
  tidy_tweets %>% 
  left_join(get_sentiments("bing"))

# from word level to tweet level 
sentiment_bing <- 
  tidy_bing %>% 
  group_by(id,sentiment) %>% 
  count()

# go from long and skinny to wider
sentiment_bing2 <- 
  sentiment_bing %>% 
  filter(!is.na(sentiment)) %>% 
  pivot_wider(names_from = sentiment, 
              values_from = n, 
              values_fill = 0) %>% 
  mutate(sentiment_bing = case_when(
    positive > negative ~ "positive",
    negative > positive ~ "negative",
    TRUE ~ "neutral"
  )) %>% # if number of positive>negative it is positive tweet
  select(-positive, -negative)

tweets <- tweets %>% 
  left_join(sentiment_bing2) %>% 
  mutate(sentiment_bing = if_else(is.na(sentiment_bing),"neutral", sentiment_bing))

# can we compare the two sentiment lexicon outputs?

# conf_mat(tweets,
#         as.factor(sentiment_afinn),
#         as.factor(sentiment_bing))


# --- Sentiment Analysis - NRC --- #
# Note: probably not covered in lecture due to time constraint

# --- Sentiment Analysis - VADER --- #
vader <- tweets %>% 
  select(text) %>% 
  vader_df(.)

# compound score is important
vader2 <- 
  vader %>%  
  mutate(sentiment_vader = case_when (
    compound > 0.05 ~ "positive",
    compound < -0.05 ~ "negative",
    TRUE ~ "neutral"
  )) %>% 
  select(sentiment_vader) %>% 
  rownames_to_column("id") %>% 
  mutate(id = str_remove(id, "[a-z]+"))

tweets <- 
  tweets %>% 
  inner_join(vader2)





# --- Plotting Some Output ---- #
# How does Trump's weekly sentiment evolve over the early stages of 2020?
# I'll use the VADER outputs












