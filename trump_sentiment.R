setwd("C:/Users/allis/OneDrive/Documents/GEOG665/Twitter_Project")
library(tidytext)
library(ggraph)
library(widyr)
library(influential)
library(dplyr)
library(tidyverse)
library(syuzhet)

################################################
###          CLEANING HASHTAG DATA           ###
################################################

pos.tweet <- read.csv("Pos_Trump_Tweets.csv", header = F)
neg.tweet <- read.csv("Neg_Trump_Tweets.csv", header = F)



  

tweet.parsed <- pos.tweet %>%
  mutate(V3 = str_to_lower(V3),
         V3 = str_remove(V3, "<u+0001f44d>"),
         V3 = str_replace_all(V3, "[[:punct:]]", " "),
         case_when(str_detect(V3, "<") ~ " ",
                   TRUE ~ V3))

tweet.parsed %>%
  unnest_tokens(word,)

###################################################################
#THIS SECTION OF CODE WORKS

words <- pos.tweet %>%
  mutate(V3 = str_remove_all(V3, "&amp;|&lt;|&gt;"),
         V3 = str_remove_all(V3, "\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)"),
         V3 = str_remove_all(V3, "[^\x01-\x7F]")) %>% 
  unnest_tokens(word, V3, token = "tweets") %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"),
         !str_detect(word, "^#"),         
         !str_detect(word, "@\\S+")) %>%
  count(word, sort = TRUE)

pos.filtered <- filter(words, n > 25)
pos.filtered <- pos.filtered[-c(18,50,123, 189),]
pos.filtered$n <- as.integer(sqrt(pos.filtered$n))
###############################################################


hashtag.words <- pos.tweet %>%
  unnest_tokens(output = word, input = V2) %>%
  anti_join(stop_words) %>%
  filter(str_detect(word, "[:alpha:]")) %>%
  distinct()

word.metions <- hashtag.words %>%
  count(word, name = "user_n") %>%
  filter(user_n >= 100)


## text column

text.words <- pos.tweet %>%
  unnest_tokens(output = word, input = V3) %>%
  anti_join(stop_words, by = "word") %>%
  filter(str_detect(word, "[:alpha:]")) %>%
  distinct()

text.mentions <- text.words %>%
  count(word, name = "user_n") %>%
  filter(user_n >= 100)

text.mentions <- text.mentions %>%
  rename(
    freq = user_n
  )
text.mentions = as.data.frame(text.mentions)

text.mentions %>%
  count(word, sort = T)

################################################
###          SENTIMENT WITH SYUZHET          ###
################################################

# Positive Tweets
pos.sa.value <- get_nrc_sentiment(pos.tweet$V3)
pos.sa.score <- colSums(pos.sa.value[,])
pos.score.df <- as.data.frame(pos.sa.score)
pos.score.df <- cbind(sentiment = rownames(pos.score.df), pos.score.df, row.names = NULL)
x <- c("positive", "joy", "trust", "anticipation", 
       "surprise", "fear", "sadness", "disgust", 
       "anger", "negative")
pos.score.df <- pos.score.df %>%
  slice(match(x, sentiment))
pos.score.df

# GGPLOT OF POSITIVE
pos.score.df %>%
  ggplot(aes(y= pos.sa.score, x = as.factor(sentiment), 
             fill = sentiment,
             color = sentiment)) +
  labs(title = "NRC Emotion Lexicon Sentiment Analysis \nof Tweets with Positive Hashtags",
       caption = "33,757 total Tweets") +
  theme(
    plot.title = element_text(hjust = 0.5)
    ) +
  xlab("Sentiment") +
  ylab("Score") +
  theme(
    axis.ticks = element_blank(),
    axis.text.x = element_text(color="#2b2d42", size=8),
    axis.text.y = element_text(color="#2b2d42", size=8)
  ) +
  geom_bar(stat = "identity", width = .8) +
  scale_fill_manual(values = alpha(c("#F37226", "#43AA8B", "#90BE6D", "#F9C74F", "#F9844A",
                               "#F94144", "#577590", "#4D908E", "#F8961E", "#277DA1"), 0.7)) +
  scale_color_manual(values = c("#F37226", "#43AA8B", "#90BE6D", "#F9C74F", "#F9844A",
                                "#F94144", "#577590", "#4D908E", "#F8961E", "#277DA1")) +
  theme(
    panel.background = element_rect(fill = "#edf2f4", colour = "#2b2d42",
                                    size = 1, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  ) +
  theme(legend.position="none") +
  coord_flip()

# Negative Tweets 
neg.sa.value <- get_nrc_sentiment(neg.tweet$V3)
neg.sa.score <- colSums(neg.sa.value[,])
neg.score.df <- as.data.frame(neg.sa.score)
neg.score.df <- cbind(sentiment = rownames(neg.score.df), neg.score.df, row.names = NULL)
neg.score.df <- neg.score.df %>%
  slice(match(x, sentiment))

## GGPLOT OF NEGATIVE
neg.score.df %>%
  ggplot(aes(y= neg.sa.score, x = as.factor(sentiment), 
             fill = sentiment,
             color = sentiment)) +
  labs(title = "NRC Emotion Lexicon Sentiment Analysis \nof Tweets with Negative Hashtags",
       caption = "17,619 total Tweets") +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  xlab("Sentiment") +
  ylab("Score") +
  theme(
    axis.ticks = element_blank(),
    axis.text.x = element_text(color="#2b2d42", size=8),
    axis.text.y = element_text(color="#2b2d42", size=8)
  ) +
  geom_bar(stat = "identity", width = .8) +
  scale_fill_manual(values = alpha(c("#F37226", "#43AA8B", "#90BE6D", "#F9C74F", "#F9844A",
                                     "#F94144", "#577590", "#4D908E", "#F8961E", "#277DA1"), 0.7)) +
  scale_color_manual(values = c("#F37226", "#43AA8B", "#90BE6D", "#F9C74F", "#F9844A",
                                "#F94144", "#577590", "#4D908E", "#F8961E", "#277DA1")) +
  theme(
    panel.background = element_rect(fill = "#edf2f4", colour = "#2b2d42",
                                    size = 1, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  ) +
  theme(legend.position="none") +
  coord_flip()

################################################
###                WORD CLOUD                ###
################################################

install.packages("rlang")
library(wordcloud2)
library(htmltools)
library(devtools)
library(rtools)

devtools::install_github("lchiffon/wordcloud2")




url = "https://icon-library.com/images/twitter-icon-eps/twitter-icon-eps-28.jpg"
sun <- "twitter-icon-eps-28.jpg"
download.file(url, sun) 

data("demoFreq")

#THIS ONE WILL WORK

wordcloud2(data = pos.filtered, figPath = system.file("examples/t.png", package = "wordcloud2"),
           size = .9, color = "#5f7d95", backgroundColor = "#edf2f4")

wordcloud2(demoFreq, figPath = system.file("examples/t.png", package = "wordcloud2"),
           color = "skyblue", backgroundColor = "white")


# NEGATIVE TWEET TIMEEEE
words <- neg.tweet %>%
  mutate(V3 = str_remove_all(V3, "&amp;|&lt;|&gt;"),
         V3 = str_remove_all(V3, "\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)"),
         V3 = str_remove_all(V3, "[^\x01-\x7F]")) %>% 
  unnest_tokens(word, V3, token = "tweets") %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"),
         !str_detect(word, "^#"),         
         !str_detect(word, "@\\S+")) %>%
  count(word, sort = TRUE)

neg.filtered <- filter(words, n > 25)
neg.filtered <- neg.filtered[-c(16,18,88,120,172, 211,262,323,429,451,588,628,650),]
neg.filtered$n <- as.integer(sqrt(neg.filtered$n))


wordcloud2(data = neg.filtered, figPath = system.file("examples/flipt.png", package = "wordcloud2"),
           color = "#e06666", backgroundColor = "#edf2f4")

