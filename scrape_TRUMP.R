library(twitteR)
library(rtweet)
library(tidyr)
library(dplyr)
library(readtext)

t.token <- create_token(
  app = "al_research",
  consumer_key = "iCLte0PmNDGwVzqimBZsfcuva",
  consumer_secret = "EKbxRn08iDz31xQGyNq0kOY00926p4qq32YmEnpWEvZKpf5V3G",
  access_token = "1349182237162536960-aT65qhACYaHaHSrTfH7fNhXVUnkjD3",
  access_secret = "drbUX3it6lXTAeCoHVI6i72duqQZXUBIGyyEtF5fWCo4X",
  set_renv = TRUE
)

negative.trump.terms <- c("#WorstPresidentEver", "#WorstPresidentInUSHistory", 
                  "#AmericaOrTrump", "#byetrump", "#ByeTrump","#ByeByeTrump", "#byebyetrump", 
                 "#GoodRiddanceTrump", "#ByeDon", "He's Gone", "He's gone", 
                 "#trumpslastday", "#TrumpsLastDay", "#GTFO")
terms.search.neg <- paste(negative.trump.terms, collapse = " OR ")
#Gathering tweets with negative hashtags
last_status_id <- readtext("C:/Users/allis/OneDrive/Documents/GEOG665/Twitter_Project/neg_last_id.txt")
neg.trump.data <- search_tweets(terms.search.neg, n = 6000, geocode = lookup_coords("usa"),
                            include_rts = FALSE, token = t.token, max_id = last_status_id$text)
new_last_status_id <-  neg.trump.data$status_id[nrow(neg.trump.data)]
write(new_last_status_id, "C:/Users/allis/OneDrive/Documents/GEOG665/Twitter_Project/neg_last_id.txt")
#extracting coordinates for negative and save as csv
neg.trump.geo <-lat_lng(neg.trump.data)
neg.trump.geo <- na.omit(neg.trump.geo[,c("lat","lng")])
write.table(neg.trump.geo, "C:/Users/allis/OneDrive/Documents/GEOG665/Twitter_Project/negativeTrumpGEO.csv", 
            append = T, row.names=F, col.names=F,  sep=",")

neg.trump.text <- neg.trump.data %>%
                  tidyr::unnest(hashtags) %>%
                  group_by(status_id) %>%
                  summarise(hashtags = toString(hashtags))
neg.trump.text <- cbind(neg.trump.text, neg.trump.data$text, neg.trump.data$created_at)
write.table(neg.trump.text,"C:/Users/allis/OneDrive/Documents/GEOG665/Twitter_Project/Neg_Trump_Tweets.csv", 
            append=T, row.names=F, col.names=F,  sep=",")

######Same steps for positive tweets#######

positive.trump.terms <- c("#TeamTrump", "#MAGA", "#MAGAForever","#TeamTrump", "#TeamTrumpForever", 
                  "#ThankYouTrump", "#ThankYouTrump","#Trump2024", "#trump2024", "#TrumpForever", 
                  "#ElectionFraud", "#TrumpTrain", "#BestPresidentEver45","#TrumpWon")
pos.terms.search <- paste(positive.trump.terms, collapse = " OR ")
#Gathering tweets with positive hashtags
pos_last_status_id <- readtext("C:/Users/allis/OneDrive/Documents/GEOG665/Twitter_Project/pos_last_id.txt")
pos.trump.data <- search_tweets(pos.terms.search, n=6000,geocode = lookup_coords("usa"),
                             include_rts = FALSE, token = t.token, max_id = pos_last_status_id$text)
new_pos_last_status_id <-  pos.trump.data$status_id[nrow(pos.trump.data)]
write(new_pos_last_status_id,"C:/Users/allis/OneDrive/Documents/GEOG665/Twitter_Project/pos_last_id.txt")
#extracting coordinates for positive and save as csv
pos.trump.geo <-lat_lng(pos.trump.data)
pos.trump.geo <- na.omit(pos.trump.geo[,c("lat","lng")])
write.table(pos.trump.geo, "C:/Users/allis/OneDrive/Documents/GEOG665/Twitter_Project/positiveTrumpGEO.csv", 
          append = T, row.names = F, col.names = F, sep = ",")

pos.trump.text <- pos.trump.data %>%
                  tidyr::unnest(hashtags) %>%
                  group_by(status_id) %>%
                  summarise(hashtags = toString(hashtags))
pos.trump.text <- cbind(pos.trump.text, pos.trump.data$text, pos.trump.data$created_at)
write.table(pos.trump.text,"C:/Users/allis/OneDrive/Documents/GEOG665/Twitter_Project/Pos_Trump_Tweets.csv", 
            append=T, row.names=F, col.names=F,  sep=",")