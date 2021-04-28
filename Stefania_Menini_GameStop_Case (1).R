#' Title:   WallStreetBets Gamestop Analysis
#' Purpose: Would have been possible to predict the GameStop mad market rise?
#' Author:  Stefania Menini
#' E-mail:  smenini2019@student.hult.edu
#' Date:    Mar 10, 2021


################################################################################
# Analysis Objective ###########################################################
# Given historical data and advanced text mining tools, the first objective of #
# the analysis is to identify any post linked to GameStop to properly          #
# investigate any early indication or signal of GameStop's ground advance,     # 
# early symptom of an imminent move. Achieving this goal will allow us to      #
# estimate how soon the hedge fund could have caught the shift in attitude     #
# towards GameStop, which deeply influenced its share price. Moreover, it will #
# also support the hedge fund in better monitoring public forums to avoid      #
# future losses in case of another significant market impact.                  #
################################################################################


################################################################################
# Initial Set Up ###############################################################
################################################################################

# Setting the working directory
setwd("~/Desktop/NLP/hult_NLP_student/cases/session II/WallStreetBets")

# Loading basic packages required for the analysis
library(ggplot2)
library(ggthemes)
library(stringi)
library(stringr)
library(tm)

# Loading additional packages
library(lubridate)
library(wordcloud)
library(RColorBrewer)
library(plotrix)
library(ggalt)
library(tibble)
library(dplyr)
library(lexicon)
library(tidytext)
library(radarchart)
library(textdata)
library(magrittr)
library(corpus)
library(qdap)
library(igraph)
library(wordcloud2)
library(pbapply)
library(readr)
library(rtweet)
library(mgsub)
library(textclean) 
library(fst)
library(echarts4r)
library(ggwordcloud)
library(tidyr)
library(purrr)
library(reshape)
library(janeaustenr)

# Avoiding strings being counted as factors
options(stringsAsFactors = FALSE)

# Limiting errors by expanding the accepted number of location in character types
Sys.setlocale('LC_ALL','C')

# Loading "wsb_data" dataset and collecting dataset general info
wsb_data <- read.csv('CASE_gme.csv', header = TRUE)
head(wsb_data,5) # Checking the first five rows of "wsb_data" 
names(wsb_data)  # Checking columns names of "wsb_data"
dim(wsb_data)    # Checking "wsb_data" dimension

# Loading "gme_stock_data" dataset and collecting dataset general info
gme_stock_data <- read.csv('gme_HLOC.csv', header = TRUE)
head(gme_stock_data,5) # Checking the first five rows of "gme_stock_data" 
names(gme_stock_data)  # Checking columns names of "gme_stock_data"
dim(gme_stock_data)    # Checking "gme_stock_data" dimension

# Creating stopwords using the 'SMART' 
stops <- c(stopwords('SMART'))


################################################################################
# Customized functions #########################################################
################################################################################

# Defining "tryTolower"
tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)}

###

# Defining "cleanCorpus"
cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url)) 
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)}


################################################################################
# GME Stock Data ###############################################################
################################################################################

# Creating a "gme_stock_data" subset 
gme_stock_subset <- gme_stock_data[403:423, 1:7]

# Plottig GME Volume 
ggplot(data = gme_stock_subset, mapping = aes(x = date, y = GME.Volume,  group=1)) +
  geom_line(color = "darkgoldenrod1") +
  geom_vline(xintercept="2021-01-13", linetype="dashed", alpha = 0.2) +
  geom_vline(xintercept="2021-01-22", linetype="dashed", alpha = 0.2) +
  labs(title = "GameStop Corporation, Volume Chart", x = "", y = "Volume") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_tufte() 

###

# Plotting GME adjusted price
ggplot(data = gme_stock_subset, mapping = aes(x = date, y = GME.Adjusted, group=1)) +
  geom_line(color = "darkgoldenrod1") +
  geom_vline(xintercept="2021-01-27", linetype="dashed", alpha = 0.2) +
  geom_vline(xintercept="2021-01-29", linetype="dashed", alpha = 0.2) +
  labs(title = "GameStop Corporation: Price Adjusted Chart \n", x = "", y = "\nAdjusted Price \n  ") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_tufte() 


################################################################################
# Creating and Organize "wsb_data" Subsets #####################################
################################################################################

# Renaming "wsb_data" first column 
names(wsb_data)[1] <- 'doc_id'

# Removing "wsb_data" unnecessary columns and subsetting for 2021
wsb_data <- subset(wsb_data , select = -c(structure, post_date, comm_date, 
                                          post_text, link, comm_date_weekday,
                                          post_date_weekday))
wsb_data <- subset(wsb_data,  
                   wsb_data$comm_date_yr == 2021)

###

# Creating a "wsb_data" subset for "title"
wsb_title <- subset(wsb_data , select = -c(comment, comm_date_yr, comm_date_month,
                                           comm_date_day))

# Removing duplicates in "wsb_title"
wsb_title <- wsb_title %>% distinct(title, .keep_all = TRUE)
head(wsb_title) # Checking 'wsb_title'

# Subsetting "wsb_title" for highlight comments made before the "short squeeze" (25th January, 2021)
wsb_title_before <- subset(wsb_title,  
                           wsb_title$post_date_month != 2) 
wsb_title_before <- wsb_title_before %>% filter(between(post_date_day, 1, 25))
head(wsb_title_before) # Checking 'wsb_title_before'

###

# Creating a "wsb_data" subset for "comment"
wsb_comment <- subset(wsb_data, select = -c(title, post_date_yr, post_date_month,
                                            post_date_day))

# Subsetting "wsb_comment" for highlight comments made before the "short squeeze" (25th January, 2021)
wsb_comment_before <- subset(wsb_comment,  
                             wsb_comment$comm_date_month != 2)
wsb_comment_before <- wsb_comment_before %>% filter(between(comm_date_day, 1, 25))
head(wsb_comment_before) # Checking 'wsb_comment_before'


################################################################################
# Dealing "wsb_comment_before" emoji ###########################################
################################################################################

# Substituting "emojis" with the lexicon in "wsb_comment_before" and dealing with spaces
st <- Sys.time()
wsb_comment_before_oneMonth_subTxt <- mgsub(wsb_comment_before$comment, 
                                     emojis$code, paste0(' ', emojis$description,' '))
Sys.time() - st # Checking the dime difference

###

# Converting "wsb_comment_before_subTxt" into a tibble
wsb_comment_before_oneMonth_subTxt <- as_tibble(wsb_comment_before_oneMonth_subTxt)

# Merging "wsb_comment_before" and "wsb_comment_before_oneMonth_subTxt"
wsb_comment_before <- data.frame(wsb_comment_before,wsb_comment_before_oneMonth_subTxt)
wsb_comment_before <- subset(wsb_comment_before , select = -c(comment))
names(wsb_comment_before)[7] <- "comment_wrangled"


################################################################################
# Dealing "wsb_title_before" Emoji #############################################
################################################################################

# Substituting "emojis" with the lexicon in "wsb_title_before" and dealing with spaces
st <- Sys.time()
wsb_title_oneMonth_subTxt <- mgsub(wsb_title_before$title, 
                                   emojis$code, paste0(' ', emojis$description,' '))
Sys.time() - st # Checking the dime difference

### 

# Converting "wsb_title_subTxt" into a tibble
wsb_title_subTxt <- as_tibble(wsb_title_oneMonth_subTxt)

# Merging "wsb_title_before" and "wsb_title_oneMonth_subTxt"
wsb_title_before <- data.frame(wsb_title_before,wsb_title_oneMonth_subTxt)
wsb_title_before <- subset(wsb_title_before , select = -c(title))
names(wsb_title_before)[7] <- "title_wrangled"


################################################################################
# Other Cleaning Manipulations #################################################
################################################################################

# Further cleaning manipulations for "wsb_comment_before"
wsb_comment_before$comment_wrangled = gsub("[^\x20-\x7E]", "", wsb_comment_before$comment_wrangled)
head(wsb_comment_before)

# Further cleaning manipulations for "wsb_title_before"
wsb_title_before$title_wrangled = gsub("[^\x20-\x7E]", "", wsb_title_before$title_wrangled)
head(wsb_title)


################################################################################
# Plotting WBS Data ############################################################
################################################################################

# Plotting WallStreetBets' words count within the first 25 days of January
ggplot(data = wsb_comment_before, mapping = aes(x = comm_date_day)) +
  geom_bar(fill = "darkgoldenrod1") +
  labs(title = "WallStreetBets Comments Count before Short Squeeze \n", 
       x = "Day of January \n", y = " ") +
  theme_tufte() 


################################################################################
# More "wsb_comment" subsets ###################################################
################################################################################

# Subsetting "wsb_comment" for highlight comments made between January 1st and 20th
wsb_comment_1_21 <- wsb_comment_before %>% filter(between(comm_date_day, 1, 21))
head(wsb_comment_1_21) # Checking 'wsb_comment_1_21'

###

# Subsetting "wsb_comment" for highlight comments made on January 22nd
wsb_comment_22 <- wsb_comment_before %>% filter(between(comm_date_day, 22,22))
head(wsb_comment_22) # Checking 'wsb_comment_22'

###

# Subsetting "wsb_comment" for highlight comments made on January 23rd
wsb_comment_23 <- wsb_comment_before %>% filter(between(comm_date_day, 23,23))
head(wsb_comment_23) # Checking 'wsb_comment_23'

###

# Subsetting "wsb_comment" for highlight comments made between January 24st and 25th
wsb_comment_24_25 <- wsb_comment_before %>% filter(between(comm_date_day, 24, 25))
head(wsb_comment_24_25) # Checking 'wsb_comment_24_25'


################################################################################
# Searching for Word Patterns in  "wsb_comment_before" #########################
################################################################################

# "Melvin Capital" keywords scanning in "wsb_comment_before"
MelvinCapital_keywordsOR <-"Melvin Capital|Melvin"
comment_MelvinCapital <- grepl(MelvinCapital_keywordsOR, wsb_comment_before$comment_wrangled,
                               ignore.case=TRUE)

# Calculating the % of times "Melvin Capital" keywords have been mentioned
comment_MelvinCapital_frequency <- sum(comment_MelvinCapital) / nrow(wsb_comment_before) 
comment_MelvinCapital_frequency # 0.01139089

###

# "Short Squeeze" keywords scanning in "wsb_comment_before"
ShortSqueeze_keywordOR <-"Short Squeeze|Gamma Squeeze|Short|Shorts|Squeeze|Short attack|Quick Squeeze|
                        little squeeze|little crush|short force"
comment_ShortSqueeze <- grepl(ShortSqueeze_keywordOR, wsb_comment_before$comment_wrangled,
                              ignore.case=TRUE)

# Calculating the % of times "Short Squeeze" keywords have been mentioned
comment_ShortSqueeze_frequency <- sum(comment_ShortSqueeze) / nrow(wsb_comment_before) 
comment_ShortSqueeze_frequency # 0.0881295

###

# "To the moon" keywords scanning in "wsb_comment_before"
moon_keywordsOR <- "Rocket Ships|Rocket|Andromeda|To The Moon|Moon|Shoot the moon
                    |Over the Moon|Let's go|Let go"
comment_moon <- grepl(moon_keywordsOR, wsb_comment_before$comment_wrangled,
                      ignore.case=TRUE)

# Calculating the % of times "To the moon" keywords have been mentioned
comment_moon_frequency <- sum(comment_moon) / nrow(wsb_comment_before) 
comment_moon_frequency # 0.04316547

###

# "Tendies" keywords scanning in "wsb_comment_before"
Tendies_keywordsOR <- "Tendies|Profit"
comment_Tendies<- grepl(Tendies_keywordsOR, wsb_comment_before$comment_wrangled,
                        ignore.case=TRUE)

# Calculating the % of times "Tendies" keywords have been mentioned
comment_Tendies_frequency <- sum(comment_Tendies) / nrow(wsb_comment_before) 
comment_Tendies_frequency # 0.02517986

###

# "GameStop" keywords scanning in "wsb_comment_before"
GME_keywordsOR <- "store manager|business model|gme|GameStop|Game Stop"
comment_GME <- grepl(GME_keywordsOR, wsb_comment_before$comment_wrangled,
                     ignore.case=TRUE)

# Calculating the % of times "GameStop" keywords have been mentioned
comment_GME_frequency <- sum(comment_GME) / nrow(wsb_comment_before) 
comment_GME_frequency # 0.1031175


################################################################################
#  String_count() per subsets (periods) ########################################
################################################################################

# Melvin_Capital
Melvin_Capital <- sum(stri_count(wsb_comment_before$comment_wrangled, 
                                 regex = "Melvin Capital|Melvin"))

Melvin_Capital_1_21 <- sum(stri_count(wsb_comment_1_21$comment_wrangled, 
                                 regex = "Melvin Capital|Melvin"))

Melvin_Capital_22 <- sum(stri_count(wsb_comment_22$comment_wrangled, 
                                      regex = "Melvin Capital|Melvin"))

Melvin_Capital_23 <- sum(stri_count(wsb_comment_23$comment_wrangled, 
                                    regex = "Melvin Capital|Melvin"))

Melvin_Capital_24_25 <- sum(stri_count(wsb_comment_24_25$comment_wrangled, 
                                    regex = "Melvin Capital|Melvin"))

###

# Short_Squeeze
Short_Squeeze <- sum(stri_count(wsb_comment_before$comment_wrangled, 
                                regex ="Short Squeeze|Gamma Squeeze|Short|Shorts|
                                Squeeze|Short attack|Quick Squeeze|
                                little squeeze|little crush|short force"))

Short_Squeeze_1_21 <- sum(stri_count(wsb_comment_1_21$comment_wrangled, 
                                regex ="Short Squeeze|Gamma Squeeze|Short|Shorts|
                                Squeeze|Short attack|Quick Squeeze|
                                little squeeze|little crush|short force"))

Short_Squeeze_22 <- sum(stri_count(wsb_comment_22$comment_wrangled, 
                                     regex ="Short Squeeze|Gamma Squeeze|Short|Shorts|
                                Squeeze|Short attack|Quick Squeeze|
                                little squeeze|little crush|short force"))

Short_Squeeze_23 <- sum(stri_count(wsb_comment_23$comment_wrangled, 
                                   regex ="Short Squeeze|Gamma Squeeze|Short|Shorts|
                                Squeeze|Short attack|Quick Squeeze|
                                little squeeze|little crush|short force"))

Short_Squeeze_24_25 <- sum(stri_count(wsb_comment_24_25$comment_wrangled, 
                                   regex ="Short Squeeze|Gamma Squeeze|Short|Shorts|
                                Squeeze|Short attack|Quick Squeeze|
                                little squeeze|little crush|short force"))
### 

# To_the_Moon
To_the_Moon <- sum(stri_count(wsb_comment_before$comment_wrangled, 
                              regex ="Rocket Ships|Rocket|Andromeda|To The Moon|
                                Moon|Shoot the moon|Over the Moon|Let's go|Let go"))

To_the_Moon_1_21 <- sum(stri_count(wsb_comment_1_21$comment_wrangled, 
                              regex ="Rocket Ships|Rocket|Andromeda|To The Moon|
                                Moon|Shoot the moon|Over the Moon|Let's go|Let go"))

To_the_Moon_22 <- sum(stri_count(wsb_comment_22$comment_wrangled, 
                                   regex ="Rocket Ships|Rocket|Andromeda|To The Moon|
                                Moon|Shoot the moon|Over the Moon|Let's go|Let go"))

To_the_Moon_23 <- sum(stri_count(wsb_comment_23$comment_wrangled, 
                                regex ="Rocket Ships|Rocket|Andromeda|To The Moon|
                                Moon|Shoot the moon|Over the Moon|Let's go|Let go"))

To_the_Moon_24_25 <- sum(stri_count(wsb_comment_24_25$comment_wrangled, 
                                 regex ="Rocket Ships|Rocket|Andromeda|To The Moon|
                                Moon|Shoot the moon|Over the Moon|Let's go|Let go"))

###

# Tendies
Tendies <- sum(stri_count(wsb_comment_before$comment_wrangled, 
                          regex ="Tendies|Profit"))

Tendies_1_21 <- sum(stri_count(wsb_comment_1_21$comment_wrangled, 
                          regex ="Tendies|Profit"))

Tendies_22 <- sum(stri_count(wsb_comment_22$comment_wrangled, 
                               regex ="Tendies|Profit"))

Tendies_23 <- sum(stri_count(wsb_comment_23$comment_wrangled, 
                             regex ="Tendies|Profit"))

Tendies_24_25 <- sum(stri_count(wsb_comment_24_25$comment_wrangled, 
                             regex ="Tendies|Profit"))

###

# GME
GME <- sum(stri_count(wsb_comment_before$comment_wrangled, 
                      regex ="store manager|business model|gme|GameStop|
                               Game Stop"))
GME_1_21 <- sum(stri_count(wsb_comment_1_21$comment_wrangled, 
                      regex ="store manager|business model|gme|GameStop|
                               Game Stop"))

GME_22 <- sum(stri_count(wsb_comment_22$comment_wrangled, 
                           regex ="store manager|business model|gme|GameStop|
                               Game Stop"))

GME_23 <- sum(stri_count(wsb_comment_23$comment_wrangled, 
                         regex ="store manager|business model|gme|GameStop|
                               Game Stop"))

GME_24_25 <- sum(stri_count(wsb_comment_24_25$comment_wrangled, 
                         regex ="store manager|business model|gme|GameStop|
                               Game Stop"))

###

# Organizing term objects into a data frames
all_termFreq <- data.frame(Terms = c('Melvin_Capital','Short_Squeeze', 
                                     'To_the_Moon', 'Tendies','GME'),
                           Freq_tot  = c(Melvin_Capital, Short_Squeeze, 
                                     To_the_Moon,Tendies, GME))

all_termFreq_1_21 <- data.frame(Terms = c('Melvin_Capital','Short_Squeeze', 
                                          'To_the_Moon', 'Tendies','GME'),
                                Freq_1_21  = c( Melvin_Capital_1_21, Short_Squeeze_1_21, 
                                          To_the_Moon_1_21,Tendies_1_21, GME_1_21))

all_termFreq_22 <- data.frame(Terms = c('Melvin_Capital','Short_Squeeze', 
                                          'To_the_Moon', 'Tendies','GME'),
                                Freq_22  = c(Melvin_Capital_22, Short_Squeeze_22, 
                                          To_the_Moon_22,Tendies_22, GME_22))

all_termFreq_23 <- data.frame(Terms = c('Melvin_Capital','Short_Squeeze', 
                                        'To_the_Moon', 'Tendies','GME'),
                               Freq_23  = c(Melvin_Capital_23, Short_Squeeze_23, 
                                        To_the_Moon_23,Tendies_23, GME_23))

all_termFreq_24_25 <- data.frame(Terms = c('Melvin_Capital','Short_Squeeze', 
                                        'To_the_Moon', 'Tendies','GME'),
                               Freq_24_25  = c(Melvin_Capital_24_25, Short_Squeeze_24_25, 
                                        To_the_Moon_24_25 ,Tendies_24_25 , GME_24_25))

###

# Join dataframes to create a new data frame called "termFreq_joined"
termFreq_joined <- list(all_termFreq_1_21,all_termFreq_22,all_termFreq_23,
                        all_termFreq_24_25)%>% reduce(left_join, by = "Terms")
termFreq_joined <-melt(termFreq_joined)

# Renaming "termFreq_joined" columns
names(termFreq_joined)[1] <- 'Terms' 
names(termFreq_joined)[2] <- 'Frequency_Type'
names(termFreq_joined)[3] <- 'Frequency_Value'
head(termFreq_joined) # Checking Results

###

# Plotting "all_termFreq"
ggplot(data = all_termFreq, aes(x = reorder(Terms, Freq_tot), y = Freq_tot)) + 
  geom_bar(stat = "identity", fill = "darkgoldenrod1") + 
  labs(title = "Word Frequency for WallStreetBets Comments", y = "Count", x = " ") +
  coord_flip() + 
  theme_tufte() 

# Plotting "termFreq_joined"
ggplot(data = termFreq_joined, aes(x = reorder(Terms, Frequency_Value), 
                                   y = Frequency_Value, fill = Frequency_Type)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("darkgoldenrod", "darkgoldenrod3", "darkgoldenrod2", "darkgoldenrod1"),
                      name = "January Days Gruped as follow:", 
                      labels = c("Between 1st and 21th of January", "22th of January", 
                                 "23th of January", "Between 24th and 25th of January")) +
  labs(title = "Word Frequency for WallStreetBets Comments, 2021", y = "\n Count \n\n", x = " ") +
  coord_flip() + 
  theme_tufte() 


################################################################################
# Volatile Corpus ##############################################################
################################################################################

# Making and cleaning a volatile corpus for "wsb_comment_before"
wsb_comment_before_corp <- VCorpus(VectorSource(wsb_comment_before$comment_wrangled))
wsb_comment_before_corp <- cleanCorpus(wsb_comment_before_corp, stops)
content(wsb_comment_before_corp[[1]]) # Checking results

###

# Making and cleaning a volatile corpus for "wsb_title_before"
wsb_title_before_corp <- VCorpus(VectorSource(wsb_title_before$title_wrangled))
wsb_title_before_corp <- cleanCorpus(wsb_title_before_corp, stops)
content(wsb_title_before_corp[[1]])  # Checking results

###

# Making and cleaning a volatile corpus for "wsb_comment_1_21"
wsb_comment_1_21_corp <- VCorpus(VectorSource(wsb_comment_1_21$comment_wrangled))
wsb_comment_1_21_corp <- cleanCorpus(wsb_comment_1_21_corp, stops)
content(wsb_comment_1_21_corp[[5]]) # Checking results

###

# Making and cleaning a volatile corpus for "wsb_comment_22"
wsb_comment_22_corp <- VCorpus(VectorSource(wsb_comment_22$comment_wrangled))
wsb_comment_22_corp <- cleanCorpus(wsb_comment_22_corp, stops)
content(wsb_comment_22_corp[[5]]) # Checking results

###

# Making and cleaning a volatile corpus for "wsb_comment_23"
wsb_comment_23_corp <- VCorpus(VectorSource(wsb_comment_23$comment_wrangled))
wsb_comment_23_corp <- cleanCorpus(wsb_comment_23_corp, stops)
content(wsb_comment_23_corp[[5]]) # Checking results

###

# Making and cleaning a volatile corpus for "wsb_comment_24_25"
wsb_comment_24_25_corp <- VCorpus(VectorSource(wsb_comment_24_25$comment_wrangled))
wsb_comment_24_25_corp <- cleanCorpus(wsb_comment_24_25_corp, stops)
content(wsb_comment_24_25_corp[[5]]) # Checking results


################################################################################
# Term Document Matrix #########################################################
################################################################################

# Making a Term Document Matrix for "wsb_comment_before_corp"
comment_Tdm <- TermDocumentMatrix(wsb_comment_before_corp, 
                                  control = list(weighting = weightTf))
comment_TdmM <- as.matrix(comment_Tdm)
dim(comment_TdmM) # Checking matrix dimensions

###

# Making a Term Document Matrix for "wsb_title_before_corp"
title_Tdm <- TermDocumentMatrix(wsb_title_before_corp, 
                                control = list(weighting = weightTf))
title_TdmM <- as.matrix(title_Tdm)
dim(title_TdmM) # Checking matrix dimensions

###

# Making a Term Document Matrix for "wsb_comment_1_21_corp"
comment_21_Tdm <- TermDocumentMatrix(wsb_comment_1_21_corp, 
                                     control = list(weighting = weightTf))
comment_21_TdmM <- as.matrix(comment_21_Tdm)
dim(comment_21_TdmM) # Checking matrix dimensions

### 

# Making a Term Document Matrix for "wsb_comment_22_corp"
comment_22_Tdm <- TermDocumentMatrix(wsb_comment_22_corp, 
                                     control = list(weighting = weightTf))
comment_22_TdmM <- as.matrix(comment_22_Tdm)
dim(comment_22_TdmM) # Checking matrix dimensions

###

# Making a Term Document Matrix for "wsb_comment_23_corp"
comment_23_Tdm <- TermDocumentMatrix(wsb_comment_23_corp, 
                                     control = list(weighting = weightTf))
comment_23_TdmM <- as.matrix(comment_23_Tdm)
dim(comment_23_TdmM) # Checking matrix dimensions

###

# Making a Term Document Matrix for "wsb_comment_24_25_corp"
comment_24_25_Tdm <- TermDocumentMatrix(wsb_comment_24_25_corp, 
                                        control = list(weighting = weightTf))
comment_24_25_TdmM <- as.matrix(comment_24_25_Tdm)
dim(comment_24_25_TdmM) # Checking matrix dimensions


################################################################################
################################################################################

################################################################################
# Most frequent terms from 1st to 25th of January (comment) ####################
################################################################################

# Getting the most frequent terms for "wsb_comment_before"
comment_TopTerms <- rowSums(comment_TdmM)
comment_TopTerms <- data.frame(terms = rownames(comment_TdmM), 
                               freq = comment_TopTerms)
rownames(comment_TopTerms) <- NULL
head(comment_TopTerms)

# Getting the most frequent term
comment_idx <- which.max(comment_TopTerms$freq)
comment_TopTerms[comment_idx, ] # buy


################################################################################
# Plotting the most frequent terms from 1st to 25th of January (comment) #######
################################################################################

# Creating an "comment_TopTerms" subset
comment_TopTerms_subset <- subset(comment_TopTerms, comment_TopTerms$freq > 70) 
comment_TopTerms_subset <- comment_TopTerms_subset[order(comment_TopTerms_subset$freq, 
                                                         decreasing=F),]


# Converting top terms into factors 
comment_TopTerms_subset$terms <- factor(comment_TopTerms_subset$terms, 
                                        levels=unique(as.character(comment_TopTerms_subset$terms))) 

###

# Plotting top terms 
ggplot(data = comment_TopTerms_subset, mapping = aes(x=terms, y=freq)) + 
  geom_bar(stat="identity", fill = "darkgoldenrod1") + 
  labs(title = "Top Terms for WallStreetBets Comments from 1st to 25th of January", 
       x = "", y = "Frequency") +
  geom_text(aes(label = freq), colour = "white", hjust = 1.25, size = 3.0) +
  coord_flip() +
  theme_tufte() 


################################################################################
# Association analysis from 1st to 25th of January (comment) ###################
################################################################################

# Inspecting word associations for "gme" in "wsb_comment_before"
comment_gme_associations <- findAssocs(comment_Tdm, 'gme', 0.21)
comment_gme_associations # Checking results

# Organizing words for "comment_gme_associations"
comment_gme_assocDF <- data.frame(terms=names(comment_gme_associations[[1]]),
                                  value=unlist(comment_gme_associations))
comment_gme_assocDF$terms <- factor(comment_gme_assocDF$terms, 
                                    levels=comment_gme_assocDF$terms)
rownames(comment_gme_assocDF) <- NULL
comment_gme_assocDF

###

# Displaying associations
ggplot(comment_gme_assocDF, aes(y=terms)) +
  geom_point(aes(x=value), data=comment_gme_assocDF, col='darkgoldenrod1') +
  labs(title = "Association Analysis for GME in WallStreetBets Comment from 1st to 25th of January", 
       x = "Value", y = " ") +
  theme_tufte() + 
  geom_text(aes(x=value,label=value), colour="grey",hjust="inward", 
            vjust ="inward" , size=3) 


################################################################################
################################################################################

################################################################################
# Most frequent terms from 1st to 21th of January (comment) ####################
################################################################################

# Getting the most frequent terms for "comment_1_21"
comment_1_21_TopTerms <- rowSums(comment_21_TdmM)
comment_1_21_TopTerms <- data.frame(terms = rownames(comment_21_TdmM), 
                                    freq = comment_1_21_TopTerms)
rownames(comment_1_21_TopTerms) <- NULL
head(comment_1_21_TopTerms)

# Getting the most frequent term
comment_idx <- which.max(comment_1_21_TopTerms$freq)
comment_1_21_TopTerms[comment_idx, ] # shares

###

# Creating an "comment_1_21_TopTerms" subset
comment_1_21_TopTerms_subset <- subset(comment_1_21_TopTerms, comment_1_21_TopTerms$freq > 5) 
comment_1_21_TopTerms_subset <- comment_1_21_TopTerms_subset[order(comment_1_21_TopTerms_subset$freq, 
                                                                   decreasing=F),]


# Converting top terms into factors 
comment_1_21_TopTerms_subset$terms <- factor(comment_1_21_TopTerms_subset$terms, 
                                        levels=unique(as.character(comment_1_21_TopTerms_subset$terms))) 

###

# Plotting top terms 
ggplot(data = comment_1_21_TopTerms_subset, mapping = aes(x=terms, y=freq)) + 
  geom_bar(stat="identity", fill = "darkgoldenrod1") + 
  labs(title = "Top Terms for WallStreetBets Comments from 1st to 21th of January", 
       x = "", y = "Frequency") +
  geom_text(aes(label = freq), colour = "white", hjust = 1.25, size = 3.0) +
  coord_flip() +
  theme_tufte() 


################################################################################
# Association analysis from 1st to 21th of January (comment) ###################
################################################################################

# Inspecting word associations for "squeeze" in "wsb_comment_before"
comment_gme_associations_21 <- findAssocs(comment_21_Tdm, 'gme', 0.44)
comment_gme_associations_21 # Checking results

# Organizing words for "comment_gme_associations_21"
comment_gme_assocDF_21 <- data.frame(terms=names(comment_gme_associations_21 [[1]]),
                                         value=unlist(comment_gme_associations_21))
comment_gme_assocDF_21$terms <- factor(comment_gme_assocDF_21$terms, 
                                           levels=comment_gme_assocDF_21$terms)
rownames(comment_gme_assocDF_21) <- NULL
comment_gme_assocDF_21 

###

# Displaying associations
ggplot(comment_gme_assocDF_21 , aes(y=terms)) +
  geom_point(aes(x=value), data=comment_gme_assocDF_21 , col='darkgoldenrod1') +
  labs(title = "Association Analysis for GME in WallStreetBets Comment from 1st to 21th of January", 
       x = "Value", y = " ") +
  theme_tufte() + 
  geom_text(aes(x=value,label=value), colour="grey",hjust="inward", 
            vjust ="inward" , size=3) 


################################################################################
################################################################################

################################################################################
# Most frequent terms on January 22th (comment) ################################
################################################################################

# Getting the most frequent terms for "comment_22"
comment_22_TopTerms <- rowSums(comment_22_TdmM)
comment_22_TopTerms <- data.frame(terms = rownames(comment_22_TdmM), 
                                  freq = comment_22_TopTerms)
rownames(comment_22_TopTerms) <- NULL
head(comment_22_TopTerms)

# Getting the most frequent term
comment_idx <- which.max(comment_22_TopTerms$freq)
comment_22_TopTerms[comment_idx, ] # buy

###

# Creating an "comment_22_TopTerms" subset
comment_22_TopTerms_subset <- subset(comment_22_TopTerms, comment_22_TopTerms$freq > 15) 
comment_22_TopTerms_subset <- comment_22_TopTerms_subset[order(comment_22_TopTerms_subset$freq, 
                                                               decreasing=F),]


# Converting top terms into factors 
comment_22_TopTerms_subset$terms <- factor(comment_22_TopTerms_subset$terms, 
                                             levels=unique(as.character(comment_22_TopTerms_subset$terms))) 

###

# Plotting top terms 
ggplot(data = comment_22_TopTerms_subset, mapping = aes(x=terms, y=freq)) + 
  geom_bar(stat="identity", fill = "darkgoldenrod1") + 
  labs(title = "Top Terms for WallStreetBets Comments on 22th of January", x = "", 
       y = "Frequency") +
  geom_text(aes(label = freq), colour = "white", hjust = 1.25, size = 3.0) +
  coord_flip() +
  theme_tufte() 


################################################################################
# Association analysis on January 22th (comment) ###############################
################################################################################

# Inspecting word associations for "gme" in "wsb_comment_before"
comment_gme_associations_22 <- findAssocs(comment_22_Tdm, 'gme', 0.26)
comment_gme_associations_22 # Checking results

# Organizing words for "comment_gme_associations_22"
comment_gme_assocDF_22 <- data.frame(terms=names(comment_gme_associations_22 [[1]]),
                                         value=unlist(comment_gme_associations_22))
comment_gme_assocDF_22$terms <- factor(comment_gme_assocDF_22$terms, 
                                           levels=comment_gme_assocDF_22$terms)
rownames(comment_gme_assocDF_22) <- NULL
comment_gme_assocDF_22 

###

# Displaying associations
ggplot(comment_gme_assocDF_22 , aes(y=terms)) +
  geom_point(aes(x=value), data=comment_gme_assocDF_22 , col='darkgoldenrod1') +
  labs(title = "Association Analysis for GME in WallStreetBets Comment on 22th of January", 
       x = "\n Value \n", y = " ") +
  theme_tufte() + 
  geom_text(aes(x=value,label=value), colour="grey",hjust="inward", 
            vjust ="inward" , size=3) 


################################################################################
################################################################################

################################################################################
# Most frequent terms on January 23th (comment) ################################
################################################################################

# Getting the most frequent terms for "comment_23"
comment_23_TopTerms <- rowSums(comment_23_TdmM)
comment_23_TopTerms <- data.frame(terms = rownames(comment_23_TdmM), 
                                  freq = comment_23_TopTerms)
rownames(comment_23_TopTerms) <- NULL
head(comment_23_TopTerms)

# Getting the most frequent term
comment_idx <- which.max(comment_23_TopTerms$freq)
comment_23_TopTerms[comment_idx, ] # money

###

# Creating an "comment_23_TopTerms" subset
comment_23_TopTerms_subset <- subset(comment_23_TopTerms, comment_23_TopTerms$freq > 20) 
comment_23_TopTerms_subset <- comment_23_TopTerms_subset[order(comment_23_TopTerms_subset$freq, 
                                                               decreasing=F),]


# Converting top terms into factors 
comment_23_TopTerms_subset$terms <- factor(comment_23_TopTerms_subset$terms, 
                                           levels=unique(as.character(comment_23_TopTerms_subset$terms))) 

###

# Plotting top terms 
ggplot(data = comment_23_TopTerms_subset, mapping = aes(x=terms, y=freq)) + 
  geom_bar(stat="identity", fill = "darkgoldenrod1") + 
  labs(title = "Top Terms for WallStreetBets Comments on 23th of January", x = "", 
       y = "Frequency") +
  geom_text(aes(label = freq), colour = "white", hjust = 1.25, size = 3.0) +
  coord_flip() +
  theme_tufte() 


################################################################################
# Association analysis on January 23th (comment) ###############################
################################################################################

# Inspecting word associations for "gme" in "comment_23"
comment_gme_associations_23 <- findAssocs(comment_23_Tdm, 'gme', 0.23)
comment_gme_associations_23 # Checking results

# Organizing words for "comment_gme_associations_23"
comment_gme_assocDF_23 <- data.frame(terms=names(comment_gme_associations_23 [[1]]),
                                     value=unlist(comment_gme_associations_23))
comment_gme_assocDF_23$terms <- factor(comment_gme_assocDF_23$terms, 
                                       levels=comment_gme_assocDF_23$terms)
rownames(comment_gme_assocDF_23) <- NULL
comment_gme_assocDF_23 

###

# Displaying associations
ggplot(comment_gme_assocDF_23 , aes(y=terms)) +
  geom_point(aes(x=value), data=comment_gme_assocDF_23 , col='darkgoldenrod1') +
  labs(title = "Association Analysis for GME in WallStreetBets Comment on 23th of January", 
       x = "\n Value \n", y = " ") +
  theme_tufte() + 
  geom_text(aes(x=value,label=value), colour="grey",hjust="inward", 
            vjust ="inward" , size=3) 


################################################################################
################################################################################

################################################################################
# Most frequent terms from 24th to 25th of January (comment) ###################
################################################################################

# Getting the most frequent terms for "comment_24_25"
comment_24_25_TopTerms <- rowSums(comment_24_25_TdmM)
comment_24_25_TopTerms <- data.frame(terms = rownames(comment_24_25_TdmM), 
                                     freq = comment_24_25_TopTerms)
rownames(comment_24_25_TopTerms) <- NULL
head(comment_24_25_TopTerms)

# Getting the most frequent term
comment_idx <- which.max(comment_24_25_TopTerms$freq)
comment_24_25_TopTerms[comment_idx, ] # shares

###

# Creating an "comment_24_25_TopTerms" subset
comment_24_25_TopTerms_subset <- subset(comment_24_25_TopTerms, comment_24_25_TopTerms$freq > 10) 
comment_24_25_TopTerms_subset <- comment_24_25_TopTerms_subset[order(comment_24_25_TopTerms_subset$freq, 
                                                                     decreasing=F),]


# Converting top terms into factors 
comment_24_25_TopTerms_subset$terms <- factor(comment_24_25_TopTerms_subset$terms, 
                                           levels=unique(as.character(comment_24_25_TopTerms_subset$terms))) 

### 

# Plotting top terms 
ggplot(data = comment_24_25_TopTerms_subset, mapping = aes(x=terms, y=freq)) + 
  geom_bar(stat="identity", fill = "darkgoldenrod1") + 
  labs(title = "Top Terms for WallStreetBets Comments from 24th to 25th of January", 
       x = "", y = "Frequency") +
  geom_text(aes(label = freq), colour = "white", hjust = 1.25, size = 3.0) +
  coord_flip() +
  theme_tufte() 


################################################################################
# Association analysis from 24th to 25th of January (comment) ##################
################################################################################

# Inspecting word associations for "gme" in "comment_24_25"
comment_gme_associations_24_25 <- findAssocs(comment_24_25_Tdm, 'gme', 0.42)
comment_gme_associations_24_25 # Checking results

# Organizing words for "comment_gme_associations_24_25"
comment_gme_assocDF_24_25 <- data.frame(terms=names(comment_gme_associations_24_25 [[1]]),
                                     value=unlist(comment_gme_associations_24_25))
comment_gme_assocDF_24_25$terms <- factor(comment_gme_assocDF_24_25$terms, 
                                       levels=comment_gme_assocDF_24_25$terms)
rownames(comment_gme_assocDF_24_25) <- NULL
comment_gme_assocDF_24_25 

###

# Displaying associations
ggplot(comment_gme_assocDF_24_25 , aes(y=terms)) +
  geom_point(aes(x=value), data=comment_gme_assocDF_24_25 , col='darkgoldenrod1') +
  labs(title = "Association Analysis for GME in WallStreetBets Comment from 24th to 25th of January", 
       x = "Value", y = " ") +
  theme_tufte() + 
  geom_text(aes(x=value,label=value), colour="grey",hjust="inward", 
            vjust ="inward" , size=3) 

################################################################################
################################################################################

################################################################################
# Creating a comparison plot of top terms ######################################
################################################################################

# Creating a top terms subsets for the first period (1st - 21st Jan)
comment_1_21_TopTerms_subset_2 <- subset(comment_1_21_TopTerms_subset,  
                                         comment_1_21_TopTerms_subset$terms == "gme" |
                                         comment_1_21_TopTerms_subset$terms == "squeeze" |
                                         comment_1_21_TopTerms_subset$terms == "buy")
names(comment_1_21_TopTerms_subset_2)[2] <- '1st - 21th of January' 
comment_1_21_TopTerms_subset_2 <- melt(comment_1_21_TopTerms_subset_2) 
names(comment_1_21_TopTerms_subset_2)[2] <- 'Frequency_Type'
comment_1_21_TopTerms_subset_2 <- comment_1_21_TopTerms_subset_2 %>% 
  add_row(terms = "squeeze", Frequency_Type = "1st - 21th of January", value = 0)

###

# Creating a top terms subsets for the first period (22nd Jan)
comment_22_TopTerms_subset_2 <- subset(comment_22_TopTerms_subset,  
                                         comment_22_TopTerms_subset$terms == "gme" |
                                         comment_22_TopTerms_subset$terms == "squeeze" |
                                         comment_22_TopTerms_subset$terms == "buy")
names(comment_22_TopTerms_subset_2)[2] <- '22th of January'
comment_22_TopTerms_subset_2 <- melt(comment_22_TopTerms_subset_2)
names(comment_22_TopTerms_subset_2)[2] <- 'Frequency_Type'

###

# Creating a top terms subsets for the first period (23rd Jan)
comment_23_TopTerms_subset_2 <- subset(comment_23_TopTerms_subset,  
                                         comment_23_TopTerms_subset$terms == "gme" |
                                         comment_23_TopTerms_subset$terms == "squeeze" |
                                         comment_23_TopTerms_subset$terms == "buy")
names(comment_23_TopTerms_subset_2)[2] <- '23th of January'
comment_23_TopTerms_subset_2 <- melt(comment_23_TopTerms_subset_2)
names(comment_23_TopTerms_subset_2)[2] <- 'Frequency_Type'

###

# Creating a top terms subsets for the first period (23rd Jan)
comment_24_25_TopTerms_subset_2 <- subset(comment_24_25_TopTerms_subset,  
                                            comment_24_25_TopTerms_subset$terms == "gme" |
                                            comment_24_25_TopTerms_subset$terms == "squeeze" |
                                            comment_24_25_TopTerms_subset$terms == "buy")
names(comment_24_25_TopTerms_subset_2)[2] <- '24th - 25th of January'
comment_24_25_TopTerms_subset_2 <- melt(comment_24_25_TopTerms_subset_2)
names(comment_24_25_TopTerms_subset_2)[2] <- 'Frequency_Type'

###

# Adding datasets vertically 
TopTerms_subset_all <- rbind(comment_1_21_TopTerms_subset_2,comment_22_TopTerms_subset_2,
      comment_23_TopTerms_subset_2, comment_24_25_TopTerms_subset_2)
head(TopTerms_subset_all) # Checking Result

###

# Plotting to compare subsets by topterms 
ggplot(data = TopTerms_subset_all, aes(x = reorder(Frequency_Type, value), 
                                       y = value, fill = terms)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("darkgoldenrod", "darkgoldenrod3", "darkgoldenrod2", "darkgoldenrod1"),
                    name = "Comments Top Terms:") +
  labs(title = "Top Terms within WallStreetBets Comments, 2021", y = "\n Count \n\n", x = " \n Frequency Value \n ") +
  theme_tufte() 


################################################################################
# Echarts4R D3 viz per subset ##################################################
################################################################################

# Plotting an "Echarts4R D3 Viz" for "wsb_comment_before"
comment_TopTerms %>% 
  e_color_range(freq, color, colors = c("black", "darkgoldenrod1")) %>% 
  e_charts() %>% 
  e_cloud(word = terms, 
          freq = freq, 
          color = color,
          rotationRange = c(0, 0),
          sizeRange = c(8, 150)) %>% 
  e_title("WallStreetBets Comment") %>%
  e_tooltip()

###

# Plotting an "Echarts4R D3 Viz" for "comment_1_21_TopTerms"
comment_1_21_TopTerms %>% 
  e_color_range(freq, color, colors = c("black", "darkgoldenrod1")) %>% 
  e_charts() %>% 
  e_cloud(word = terms, 
          freq = freq, 
          color = color,
          rotationRange = c(0, 0),
          sizeRange = c(8, 150)) %>% 
  e_title("WallStreetBets Comment") %>%
  e_tooltip()

###

# Plotting an "Echarts4R D3 Viz" for "comment_22_TopTerms"
comment_22_TopTerms %>% 
  e_color_range(freq, color, colors = c("black", "darkgoldenrod1")) %>% 
  e_charts() %>% 
  e_cloud(word = terms, 
          freq = freq, 
          color = color,
          rotationRange = c(0, 0),
          sizeRange = c(8, 150)) %>% 
  e_title("WallStreetBets Comment") %>%
  e_tooltip()

###

# Plotting an "Echarts4R D3 Viz" for "comment_23_TopTerms"
comment_23_TopTerms %>% 
  e_color_range(freq, color, colors = c("black", "darkgoldenrod1")) %>% 
  e_charts() %>% 
  e_cloud(word = terms, 
          freq = freq, 
          color = color,
          rotationRange = c(0, 0),
          sizeRange = c(8, 150)) %>% 
  e_title("WallStreetBets Comment") %>%
  e_tooltip()

###

# Plotting an "Echarts4R D3 Viz" for "comment_24_25_TopTerms"
comment_24_25_TopTerms %>% 
  e_color_range(freq, color, colors = c("black", "darkgoldenrod1")) %>% 
  e_charts() %>% 
  e_cloud(word = terms, 
          freq = freq, 
          color = color,
          rotationRange = c(0, 0),
          sizeRange = c(8, 150)) %>% 
  e_title("WallStreetBets Comment") %>%
  e_tooltip()


################################################################################
# Creating a new subset for 2020 WSB comments ##################################
################################################################################

# Loading "wsb_data_2020" dataset and creating a subset
wsb_data_2020 <- read.csv('CASE_gme.csv', header = TRUE)
wsb_data_2020 <- subset(wsb_data_2020,  
                        wsb_data_2020$comm_date_yr == 2019 |
                          wsb_data_2020$comm_date_yr == 2020)

###

# Removing unseful columns
wsb_data_2020 <- subset(wsb_data_2020 , select = -c(title))

###

# Substituting "emojis" with the lexicon in "wsb_data_2020" and dealing with spaces
st <- Sys.time()
wsb_data_2020_subTxt <- mgsub(wsb_data_2020$comment, 
                                   emojis$code, paste0(' ', emojis$description,' '))
Sys.time() - st # Checking the dime difference

### 

# Converting "wsb_data_2020_subTxt" into a tibble
wsb_data_2020_subTxt <- as_tibble(wsb_data_2020_subTxt)

# Merging "wsb_data_2020" and "wsb_data_2020_subTxt"
wsb_data_2020 <- data.frame(wsb_data_2020,wsb_data_2020_subTxt)
names(wsb_data_2020)[18] <- "comment_wrangled"

# Making and cleaning a volatile corpus for "wsb_data_2020"
wsb_comment_2020_corp <- VCorpus(VectorSource(wsb_data_2020$comment_wrangled))
wsb_comment_2020_corp <- cleanCorpus(wsb_comment_2020_corp, stops)
content(wsb_comment_2020_corp[[5]]) # Checking results

###

# Making a Term Document Matrix for "wsb_data_2020_corp"
comment_2020_Tdm <- TermDocumentMatrix(wsb_comment_2020_corp, control = list(weighting = weightTf))
comment_2020_TdmM <- as.matrix(comment_2020_Tdm)
dim(comment_2020_TdmM) # Checking matrix dimensions

###

# Top Terms between 2019 and 2020
comment_2020_TopTerms <- rowSums(comment_2020_TdmM)
comment_2020_TopTerms <- data.frame(terms = rownames(comment_2020_TdmM), freq = comment_2020_TopTerms)

###

# lotting an "Echarts4R D3 Viz" for "comment_2020_TopTerms"
comment_2020_TopTerms %>% 
  e_color_range(freq, color, colors = c("black", "darkgoldenrod1")) %>% 
  e_charts() %>% 
  e_cloud(word = terms, 
          freq = freq, 
          color = color,
          rotationRange = c(0, 0),
          sizeRange = c(8, 150)) %>% 
  e_title("WallStreetBets Comment") %>%
  e_tooltip()


################################################################################
# Polarity analysis for "wsb_comment_before" ###################################
################################################################################

# Applying polarity on "comment_wrangled"
comment_doc_pol <- polarity(wsb_comment_before$comment_wrangled)

###

# Specifying supporting functions
source('~/Desktop/NLP/hult_NLP_student/lessons/Z_otherScripts/ZZZ_supportingFunctions.R')

###

# Checking "comment_doc_pol" word count details
comment_doc_pol$all$wc

# Checking "comment_doc_pol" polarity details
comment_doc_pol$all$polarity

# Checking "comment_doc_pol" pos words ID'ed
comment_doc_pol$all$pos.words

# Checking "comment_doc_pol" neg words ID'ed
comment_doc_pol$all$neg.words

# Checking "comment_doc_pol" document view
comment_doc_pol$group

################################################################################
# Polarity analysis for other periods ###########################################
################################################################################

# Applying polarity on "wsb_comment_1_21"
comment_doc_pol_1_21<- polarity(wsb_comment_1_21$comment_wrangled)

# Checking polarity results
comment_doc_pol_1_21$group

###

# Applying polarity on "wsb_comment_22"
comment_doc_pol_22 <- polarity(wsb_comment_22$comment_wrangled)

# Checking polarity results
comment_doc_pol_22$group

###

# Applying polarity on "wsb_comment_23"
comment_doc_pol_23 <- polarity(wsb_comment_23$comment_wrangled)

# Checking polarity results
comment_doc_pol_23$group

###

# Applying polarity on "wsb_comment_24_25"
comment_doc_pol_24_25 <- polarity(wsb_comment_24_25$comment_wrangled)

# Checking polarity results
comment_doc_pol_24_25$group


################################################################################
# Plotting polarity scores for "wsb_comment_before" ############################
################################################################################

# Plotting the distribution of the polarity scores
ggplot(comment_doc_pol$all, aes(x=polarity, y = ..density..))+
  geom_histogram(binwidth = .25, fill= "darkgoldenrod1", color="lightgrey", size=.2) +
  labs(title = "Polarity Distribution Scores for WallStreetBets Comments", x = "Polarity", 
       y = "Density") +
  theme_tufte()

# Appending polarity scores to "wsb_comment_before"
wsb_comment_before$polarity<- scale(comment_doc_pol$all$polarity)

# Subsetting positive and negative comments
POS_comment <- subset(wsb_comment_before$comment_wrangled, wsb_comment_before$polarity>0)
NEG_comment <- subset(wsb_comment_before$comment_wrangled, wsb_comment_before$polarity<0)

# Combining the documents
POS_terms<- paste(POS_comment, collapse = ' ')
NEG_terms<- paste(NEG_comment, collapse= ' ')
ALL_terms<- c(POS_terms, NEG_terms)
ALL_corpus<- VCorpus(VectorSource(ALL_terms))

# Making a TDM transformation
ALLTDM <- TermDocumentMatrix(ALL_corpus, control = list(weighting=weightTfIdf,
                                                        removePunctuation=TRUE,
                                                        removeURL = TRUE,
                                                        removeNumbers = TRUE,
                                                        stopwords=stopwords(kind = 'en')))
# Creating a simple matrix
ALLTDMm <- as.matrix(ALLTDM)
colnames(ALLTDMm)<- c('Positive', 'Negative')

# Comparison.Cloud to analyze visually the polarity of the document
comparison.cloud(ALLTDMm, scale=c(4,1), max.words = 100, 
                 colors = c('black','grey'), use.r.layout=FALSE, title.size=2.5,
                 title.colors="white", match.colors=FALSE,
                 title.bg.colors="darkgoldenrod1")



################################################################################
################################################################################

################################################################################
# Sentiment analysis for "wsb_comment_before" ##################################
################################################################################

# Creating a DTM for "wsb_comment_before"
comment_DTM <- VCorpus(VectorSource(wsb_comment_before$comment_wrangled))
comment_DTM <- cleanCorpus(comment_DTM, stops)
comment_DTM <- DocumentTermMatrix(comment_DTM)
dim(comment_DTM) # Checking "comment_DTM" dimensions

# Tidying "comment_DTM"
tidyCorp <- tidy(comment_DTM)
dim(tidyCorp) # Checking "tidyCorp" dimensions

###

# Getting "bing" lexicon
bing <- get_sentiments(lexicon = c("bing"))

# Performing an inner join between "tidyCorp" and "bing" lexicon
bingSent <- inner_join(tidyCorp, bing, by=c('term' = 'word'))
bingSent

# Analyzing "bingSent" sentiments
aggregate(count~sentiment,bingSent, sum) # avg. polarity  -0.01858667 

###

# Getting "afinn" lexicon
afinn<-get_sentiments(lexicon = c("afinn"))
head(afinn)

# Performing an inner join between "tidyCorp" and "afinn" lexicon
afinnSent <- inner_join(tidyCorp,afinn, by=c('term' = 'word'))
afinnSent

# Examine the quantity
afinnSent$afinnAmt     <- afinnSent$count * afinnSent$value

# Compare w/polarity and bing
mean(afinnSent$afinnAmt)  # avg. polarity  -0.01858667 


################################################################################
# Analysis over time for "wsb_comment_before" ##################################
################################################################################

# Analyzing emotional content over time
afinnTemporal          <- aggregate(afinnAmt~document, afinnSent, sum)
afinnTemporal$document <- as.numeric(afinnTemporal$document)
afinnTemporal          <- afinnTemporal[order(afinnTemporal$document),]

# Plotting "afinnTemporal'
plot(afinnTemporal$afinnAmt, type="l", main="Quick Timeline of Identified Words") 

###

# Getting "ncr" lexicon
nrc <- nrc_emotions

# Tidying "nrc"
nrc <- nrc %>% pivot_longer(-term, names_to = "emotion", values_to = "freq")
nrc <-subset(nrc, nrc$freq>0 )
head(nrc)


# Performing an inner join between "tidyCorp" and "nrc" lexicon
nrcSent <- inner_join(tidyCorp,nrc, by=c('term' = 'term'))
nrcSent

# Plotting a radar chart
table(nrcSent$emotion)
emos <- data.frame(table(nrcSent$emotion))
names(emos) <- c('emotion', 'termsCt')
emos %>% 
  e_charts(emotion) %>% 
  e_radar(termsCt, max = max(emos$termsCt), name = "WallStreetBest Comment Emotions") %>%
  e_tooltip(trigger = "item") %>% e_theme("caravan")

# Other Emotion Lexicons Exist
emotionLex <- affect_wordnet
emotionLex
table(emotionLex$emotion)
table(emotionLex$category)

emotionLex <- subset(emotionLex, 
                     emotionLex$emotion=='Positive'|emotionLex$emotion=='Negative')

# More emotional categories, fewer terms
lexSent <- inner_join(tidyCorp,emotionLex, by=c('term' = 'term'))
lexSent
emotionID <- aggregate(count ~ category, lexSent, sum)
emotionID %>% 
  e_charts(category) %>% e_theme("caravan") %>%
  e_radar(count, max =max(emotionID$count), name = "WallStreetBest Comment Emotions") %>%
  e_tooltip() %>%
  e_theme("caravan") 


################################################################################
################################################################################

################################################################################
# Sentiment analysis from 1st to 21st of January (comment) #####################
################################################################################

# Creating a DTM for "wsb_comment_1_21"
comment_21_DTM <- VCorpus(VectorSource(wsb_comment_1_21$comment_wrangled))
comment_21_DTM <- cleanCorpus(comment_21_DTM, stops)
comment_21_DTM <- DocumentTermMatrix(comment_21_DTM)
dim(comment_21_DTM) # Checking "comment_21_DTM" dimensions

# Tidying "comment_21_DTM"
tidyCorp_21 <- tidy(comment_21_DTM)
dim(tidyCorp_21) # Checking "tidyCorp_21" dimensions

###

# Getting "bing" lexicon
bing <- get_sentiments(lexicon = c("bing"))

# Performing an inner join between "tidyCorp_21" and "bing" lexicon
bingSent_21 <- inner_join(tidyCorp_21, bing, by=c('term' = 'word'))


# Analyzing "bingSent_21" sentiments
aggregate(count~sentiment,bingSent_21, sum) # avg. polarity  -0.01858667 

###

# Getting "afinn" lexicon
afinn<-get_sentiments(lexicon = c("afinn"))

# Performing an inner join between "tidyCorp_21" and "afinn" lexicon
afinnSent_21 <- inner_join(tidyCorp_21,afinn, by=c('term' = 'word'))

# Examine the quantity
afinnSent_21$afinnAmt     <- afinnSent_21$count * afinnSent_21$value

# Compare w/polarity and bing
mean(afinnSent_21$afinnAmt)  # avg. polarity  -0.01858667 


################################################################################
# Analysis over time from 1st to 21st of January (comment) #####################
################################################################################

# Analyzing emotional content over time
afinnTemporal_21          <- aggregate(afinnAmt~document, afinnSent_21, sum)
afinnTemporal_21$document <- as.numeric(afinnTemporal_21$document)
afinnTemporal_21          <- afinnTemporal_21[order(afinnTemporal_21$document),]

# Plotting "afinnTemporal_21'
plot(afinnTemporal_21$afinnAmt, type="l", main="Quick Timeline of Identified Words") 

###

# Performing an inner join between "tidyCorp" and "nrc" lexicon
nrcSent_21 <- inner_join(tidyCorp_21,nrc, by=c('term' = 'term'))
nrcSent_21 

# Plotting a radar chart
table(nrcSent_21$emotion)
emos_21 <- data.frame(table(nrcSent_21$emotion))
names(emos_21) <- c('emotion', 'termsCt')
emos_21 %>% 
  e_charts(emotion) %>% 
  e_radar(termsCt, max = max(emos_21$termsCt), name = "WallStreetBest Comment Emotions") %>%
  e_tooltip(trigger = "item") %>% e_theme("caravan")

# Other Emotion Lexicons Exist
emotionLex_21 <- affect_wordnet
emotionLex_21
table(emotionLex_21$emotion)
table(emotionLex_21$category)

emotionLex_21 <- subset(emotionLex_21, 
                     emotionLex_21$emotion=='Positive'|emotionLex_21$emotion=='Negative')

# More emotional categories, fewer terms
lexSent_21 <- inner_join(tidyCorp_21,emotionLex_21, by=c('term' = 'term'))
lexSent_21
emotionID_21 <- aggregate(count ~ category, lexSent_21, sum)
emotionID_21 %>% 
  e_charts(category) %>% e_theme("caravan") %>%
  e_radar(count, max =max(emotionID_21$count), name = "WallStreetBest Comment Emotions") %>%
  e_tooltip() %>%
  e_theme("caravan") 


################################################################################
################################################################################

################################################################################
# Sentiment analysis on 22nd of January (comment) ##############################
################################################################################

# Creating a DTM for "wsb_comment_22"
comment_22_DTM <- VCorpus(VectorSource(wsb_comment_22$comment_wrangled))
comment_22_DTM <- cleanCorpus(comment_22_DTM, stops)
comment_22_DTM <- DocumentTermMatrix(comment_22_DTM)
dim(comment_22_DTM) # Checking "comment_22_DTM" dimensions

# Tidying "comment_22_DTM"
tidyCorp_22 <- tidy(comment_22_DTM)
dim(tidyCorp_22) # Checking "tidyCorp_22" dimensions

###

# Getting "bing" lexicon
bing <- get_sentiments(lexicon = c("bing"))

# Performing an inner join between "tidyCorp_22" and "bing" lexicon
bingSent_22 <- inner_join(tidyCorp_22, bing, by=c('term' = 'word'))


# Analyzing "bingSent_22" sentiments
aggregate(count~sentiment,bingSent_22, sum) # avg. polarity  -0.01858667 

###

# Getting "afinn" lexicon
afinn<-get_sentiments(lexicon = c("afinn"))

# Performing an inner join between "tidyCorp_22" and "afinn" lexicon
afinnSent_22 <- inner_join(tidyCorp_22,afinn, by=c('term' = 'word'))

# Examine the quantity
afinnSent_22$afinnAmt     <- afinnSent_22$count * afinnSent_22$value

# Compare w/polarity and bing
mean(afinnSent_22$afinnAmt)  # avg. polarity  -0.01858667 


################################################################################
# Analysis over time on 22nd of January (comment) ##############################
################################################################################

# Analyzing emotional content over time
afinnTemporal_22          <- aggregate(afinnAmt~document, afinnSent_22, sum)
afinnTemporal_22$document <- as.numeric(afinnTemporal_22$document)
afinnTemporal_22          <- afinnTemporal_22[order(afinnTemporal_22$document),]

# Plotting "afinnTemporal_22'
plot(afinnTemporal_22$afinnAmt, type="l", main="Quick Timeline of Identified Words") 

###

# Performing an inner join between "tidyCorp" and "nrc" lexicon
nrcSent_22 <- inner_join(tidyCorp_22,nrc, by=c('term' = 'term'))
nrcSent_22 

# Plotting a radar chart
table(nrcSent_22$emotion)
emos_22 <- data.frame(table(nrcSent_22$emotion))
names(emos_22) <- c('emotion', 'termsCt')
emos_22 %>% 
  e_charts(emotion) %>% 
  e_radar(termsCt, max = max(emos_22$termsCt), name = "WallStreetBest Comment Emotions") %>%
  e_tooltip(trigger = "item") %>% e_theme("caravan")

# Other Emotion Lexicons Exist
emotionLex_22 <- affect_wordnet
emotionLex_22
table(emotionLex_22$emotion)
table(emotionLex_22$category)

emotionLex_22 <- subset(emotionLex_22, 
                        emotionLex_22$emotion=='Positive'|emotionLex_22$emotion=='Negative')

# More emotional categories, fewer terms
lexSent_22 <- inner_join(tidyCorp_22,emotionLex_22, by=c('term' = 'term'))
lexSent_22
emotionID_22 <- aggregate(count ~ category, lexSent_22, sum)
emotionID_22 %>% 
  e_charts(category) %>% e_theme("caravan") %>%
  e_radar(count, max =max(emotionID_22$count), name = "WallStreetBest Comment Emotions") %>%
  e_tooltip() %>%
  e_theme("caravan") 


################################################################################
################################################################################

################################################################################
# Sentiment analysis on 23nd of January (comment) ##############################
################################################################################

# Creating a DTM for "wsb_comment_23"
comment_23_DTM <- VCorpus(VectorSource(wsb_comment_23$comment_wrangled))
comment_23_DTM <- cleanCorpus(comment_23_DTM, stops)
comment_23_DTM <- DocumentTermMatrix(comment_23_DTM)
dim(comment_23_DTM) # Checking "comment_23_DTM" dimensions

# Tidying "comment_23_DTM"
tidyCorp_23 <- tidy(comment_23_DTM)
dim(tidyCorp_23) # Checking "tidyCorp_23" dimensions

###

# Getting "bing" lexicon
bing <- get_sentiments(lexicon = c("bing"))

# Performing an inner join between "tidyCorp_23" and "bing" lexicon
bingSent_23 <- inner_join(tidyCorp_23, bing, by=c('term' = 'word'))


# Analyzing "bingSent_23" sentiments
aggregate(count~sentiment,bingSent_23, sum) # avg. polarity  -0.01858667 

###

# Getting "afinn" lexicon
afinn<-get_sentiments(lexicon = c("afinn"))

# Performing an inner join between "tidyCorp_23" and "afinn" lexicon
afinnSent_23 <- inner_join(tidyCorp_23,afinn, by=c('term' = 'word'))

# Examine the quantity
afinnSent_23$afinnAmt     <- afinnSent_23$count * afinnSent_23$value

# Compare w/polarity and bing
mean(afinnSent_23$afinnAmt)  # avg. polarity  -0.01858667 


################################################################################
# Analysis over time on 23nd of January (comment) ##############################
################################################################################

# Analyzing emotional content over time
afinnTemporal_23          <- aggregate(afinnAmt~document, afinnSent_23, sum)
afinnTemporal_23$document <- as.numeric(afinnTemporal_23$document)
afinnTemporal_23          <- afinnTemporal_23[order(afinnTemporal_23$document),]

# Plotting "afinnTemporal_23'
plot(afinnTemporal_23$afinnAmt, type="l", main="Quick Timeline of Identified Words") 

###

# Performing an inner join between "tidyCorp" and "nrc" lexicon
nrcSent_23 <- inner_join(tidyCorp_23,nrc, by=c('term' = 'term'))
nrcSent_23 

# Plotting a radar chart
table(nrcSent_23$emotion)
emos_23 <- data.frame(table(nrcSent_23$emotion))
names(emos_23) <- c('emotion', 'termsCt')
emos_23 %>% 
  e_charts(emotion) %>% 
  e_radar(termsCt, max = max(emos_23$termsCt), name = "WallStreetBest Comment Emotions") %>%
  e_tooltip(trigger = "item") %>% e_theme("caravan")

# Other Emotion Lexicons Exist
emotionLex_23 <- affect_wordnet
emotionLex_23
table(emotionLex_23$emotion)
table(emotionLex_23$category)

emotionLex_23 <- subset(emotionLex_23, 
                        emotionLex_23$emotion=='Positive'|emotionLex_23$emotion=='Negative')

# More emotional categories, fewer terms
lexSent_23 <- inner_join(tidyCorp_23,emotionLex_23, by=c('term' = 'term'))
lexSent_23
emotionID_23 <- aggregate(count ~ category, lexSent_23, sum)
emotionID_23 %>% 
  e_charts(category) %>% e_theme("caravan") %>%
  e_radar(count, max =max(emotionID_23$count), name = "WallStreetBest Comment Emotions") %>%
  e_tooltip() %>%
  e_theme("caravan") 


################################################################################
################################################################################

################################################################################
# Sentiment analysis from 24th to 25th of January (comment) ####################
################################################################################

# Creating a DTM for "wsb_comment_24_25"
comment_24_25_DTM <- VCorpus(VectorSource(wsb_comment_24_25$comment_wrangled))
comment_24_25_DTM <- cleanCorpus(comment_24_25_DTM, stops)
comment_24_25_DTM <- DocumentTermMatrix(comment_24_25_DTM)
dim(comment_24_25_DTM) # Checking "comment_24_25_DTM" dimensions

# Tidying "comment_24_25_DTM"
tidyCorp_24_25 <- tidy(comment_24_25_DTM)
dim(tidyCorp_24_25) # Checking "tidyCorp_24_25" dimensions

###

# Getting "bing" lexicon
bing <- get_sentiments(lexicon = c("bing"))

# Performing an inner join between "tidyCorp_24_25" and "bing" lexicon
bingSent_24_25 <- inner_join(tidyCorp_24_25, bing, by=c('term' = 'word'))


# Analyzing "bingSent_24_25" sentiments
aggregate(count~sentiment,bingSent_24_25, sum) # avg. polarity  -0.01858667 

###

# Getting "afinn" lexicon
afinn<-get_sentiments(lexicon = c("afinn"))

# Performing an inner join between "tidyCorp_24_25" and "afinn" lexicon
afinnSent_24_25 <- inner_join(tidyCorp_24_25,afinn, by=c('term' = 'word'))

# Examine the quantity
afinnSent_24_25$afinnAmt     <- afinnSent_24_25$count * afinnSent_24_25$value

# Compare w/polarity and bing
mean(afinnSent_24_25$afinnAmt)  # avg. polarity  -0.01858667 


################################################################################
# Analysis over time on 24th and 25the of January (comment) ####################
################################################################################

# Analyzing emotional content over time
afinnTemporal_24_25          <- aggregate(afinnAmt~document, afinnSent_24_25, sum)
afinnTemporal_24_25$document <- as.numeric(afinnTemporal_24_25$document)
afinnTemporal_24_25          <- afinnTemporal_24_25[order(afinnTemporal_24_25$document),]

# Plotting "afinnTemporal_24_25'
plot(afinnTemporal_24_25$afinnAmt, type="l", main="Quick Timeline of Identified Words") 

###

# Performing an inner join between "tidyCorp" and "nrc" lexicon
nrcSent_24_25 <- inner_join(tidyCorp_24_25,nrc, by=c('term' = 'term'))
nrcSent_24_25 

# Plotting a radar chart
table(nrcSent_24_25$emotion)
emos_24_25 <- data.frame(table(nrcSent_24_25$emotion))
names(emos_24_25) <- c('emotion', 'termsCt')
emos_24_25 %>% 
  e_charts(emotion) %>% 
  e_radar(termsCt, max = max(emos_24_25$termsCt), name = "WallStreetBest Comment Emotions") %>%
  e_tooltip(trigger = "item") %>% e_theme("caravan")

# Other Emotion Lexicons Exist
emotionLex_24_25 <- affect_wordnet
emotionLex_24_25
table(emotionLex_24_25$emotion)
table(emotionLex_24_25$category)

emotionLex_24_25 <- subset(emotionLex_24_25, 
                        emotionLex_24_25$emotion=='Positive'|emotionLex_24_25$emotion=='Negative')

# More emotional categories, fewer terms
lexSent_24_25 <- inner_join(tidyCorp_24_25,emotionLex_24_25, by=c('term' = 'term'))
lexSent_24_25
emotionID_24_25 <- aggregate(count ~ category, lexSent_24_25, sum)
emotionID_24_25 %>% 
  e_charts(category) %>% e_theme("caravan") %>%
  e_radar(count, max =max(emotionID_24_25$count), name = "WallStreetBest Comment Emotions") %>%
  e_tooltip() %>%
  e_theme("caravan") 


# End ##########################################################################


