#Intallation des packages (une fois)
install.packages("twitteR") #une fois
install.packages("tm") #une fois
install.packages("qdap") #une fois
install.packages("wordcloud")
install.packages("Scale")


#Chargement des bibliothèques
# Load the required Packages
library(twitteR)
library(RCurl)
library(httr)
library(tm)
library(syuzhet)
library(tm) #pour le text mining : Vectorsource(), VCorpus() et le nettoyage removeSparseTerms
library(qdap) #Aussi pour text mining et nettoyage de texte 
library(RCurl)
library(ROAuth)
library(rjson)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(bitops)
library(stringi)
library(wordcloud)
library(lubridate)
library(Scale)
library(reshape2)
library(SnowballC)
library(stringr)




#Connexion à l'API

setup_twitter_oauth(consumer_key = "C9ygYbJtS89oZq8jNH3fnDOlX", 
                    consumer_secret = "rcdBgnRNgv3Pgz5wWRgn2R0xF4KByV4QFieGdZzq4OjrOAXpQD", 
                    access_token = "1272738738-6RSYoXalDyrS93NyAZNXoxdUwO18yagpXBfYVy6", 
                    access_secret = "uy1UoseVOYQlt3HabKQuBOz6mc3B1HiF1GN4o53waeF5x")

#Formatage de dates en chaînes de caractères. Peut être utile selon les cas. 
strlast1095days=as.character(Sys.Date()-1095)
strlast1000days=as.character(Sys.Date()-1000) 
strlastyears=as.character(Sys.Date()-362)
strYesterday = as.character(Sys.Date()-1)  #on a choisi 1 jour en arrière
strToday = as.character(Sys.Date()) 

#Récupération des derniers 1000 tweets max depuis hier pour ebola sans les Retweets

ebola.tweets <- searchTwitter("ebola", n = 150, resultType = "mixed", since = "2010-09-14")
tweets_df <- twListToDF(ebola.tweets) # cette fonction va permettre de transformer la liste de tweets extraite en un "dataframe"
write.csv2(tweets_df, file = "tweets.csv", row.names = FALSE)

malaria.tweets <- searchTwitter("malaria", n = 150, resultType = "mixed", since = "2010-09-14")
tweets1_df <- twListToDF(malaria.tweets) # cette fonction va permettre de transformer la liste de tweets extraite en un "dataframe"
write.csv2(tweets_df, file = "tweets1.csv", row.names = FALSE)

meningitis.tweets <- searchTwitter("meningitis", n = 150, resultType = "mixed", since = "2010-09-14")
tweets2_df <- twListToDF(meningitis.tweets) # cette fonction va permettre de transformer la liste de tweets extraite en un "dataframe"
write.csv2(tweets_df, file = "tweets2.csv", row.names = FALSE)

#Nombres de Tweets récupérés 
length(ebola.tweets)
length(malaria.tweets)
length(meningitis.tweets)

#Date/heure du tweet le plus ancien
ebola.tweets[[length(ebola.tweets)]][["created"]]
malaria.tweets[[length(malaria.tweets)]][["created"]]
meningitis.tweets[[length(meningitis.tweets)]][["created"]]

#On récupère uniquement les textes des tweets 
ebola.text  = lapply(ebola.tweets, function(t) t$getText())
malaria.text = lapply(malaria.tweets, function(t) t$getText())
meningitis.text = lapply(meningitis.tweets, function(t) t$getText())

# CLEANING TWEETS sentiment analysis
 ###############ebola
tweets_df$text=gsub("&amp", "", tweets_df$text)
tweets_df$text = gsub("&amp", "", tweets_df$text)
tweets_df$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets_df$text)
tweets_df$text = gsub("@\\w+", "", tweets_df$text)
tweets_df$text = gsub("[[:punct:]]", "", tweets_df$text)
tweets_df$text = gsub("[[:digit:]]", "", tweets_df$text)
tweets_df$text = gsub("http\\w+", "", tweets_df$text)
tweets_df$text = gsub("[ \t]{2,}", "", tweets_df$text)
tweets_df$text = gsub("^\\s+|\\s+$", "", tweets_df$text)
tweets_df$text <- iconv(tweets_df$text, "UTF-8", "ASCII", sub="")

tweets_df2 <- gsub("http.*","",tweets_df$text)
tweets_df2 <- gsub("https.*","",tweets_df2)
tweets_df2 <- gsub("#.*","",tweets_df2)
tweets_df2 <- gsub("@.*","",tweets_df2)
###################
# CLEANING TWEETS
#malaria
tweets1_df$text=gsub("&amp", "", tweets1_df$text)
tweets1_df$text = gsub("&amp", "", tweets1_df$text)
tweets1_df$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets1_df$text)
tweets1_df$text = gsub("@\\w+", "", tweets1_df$text)
tweets1_df$text = gsub("[[:punct:]]", "", tweets1_df$text)
tweets1_df$text = gsub("[[:digit:]]", "", tweets1_df$text)
tweets1_df$text = gsub("http\\w+", "", tweets1_df$text)
tweets1_df$text = gsub("[ \t]{2,}", "", tweets1_df$text)
tweets1_df$text = gsub("^\\s+|\\s+$", "", tweets1_df$text)
tweets1_df$text <- iconv(tweets1_df$text, "UTF-8", "ASCII", sub="")

tweets1_df2 <- gsub("http.*","",tweets1_df$text)
tweets1_df2 <- gsub("https.*","",tweets1_df2)
tweets1_df2 <- gsub("#.*","",tweets1_df2)
tweets1_df2 <- gsub("@.*","",tweets1_df2)

##########
# CLEANING TWEETS
#####meningitis
tweets2_df$text=gsub("&amp", "", tweets2_df$text)
tweets2_df$text = gsub("&amp", "", tweets2_df$text)
tweets2_df$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets2_df$text)
tweets2_df$text = gsub("@\\w+", "", tweets2_df$text)
tweets2_df$text = gsub("[[:punct:]]", "", tweets2_df$text)
tweets2_df$text = gsub("[[:digit:]]", "", tweets2_df$text)
tweets2_df$text = gsub("http\\w+", "", tweets2_df$text)
tweets2_df$text = gsub("[ \t]{2,}", "", tweets2_df$text)
tweets2_df$text = gsub("^\\s+|\\s+$", "", tweets2_df$text)
tweets2_df$text <- iconv(tweets2_df$text, "UTF-8", "ASCII", sub="")

tweets2_df2 <- gsub("http.*","",tweets2_df$text)
tweets2_df2 <- gsub("https.*","",tweets2_df2)
tweets2_df2 <- gsub("#.*","",tweets2_df2)
tweets2_df2 <- gsub("@.*","",tweets2_df2)
#######cleaning 

#on enlève les URLs  
ebola.text <- gsub("(f|ht)tp(s?)://\\S+", "", ebola.text)
malaria.text <- gsub("(f|ht)tp(s?)://\\S+", "", malaria.text)
meningitis.text <- gsub("(f|ht)tp(s?)://\\S+", "", meningitis.text)

#On remplace les caractères spéciaux par des blancs
ebola.text <- gsub("[][!#$%()*,.:;<=>@^_|~.{}]", " ", ebola.text)
malaria.text <- gsub("[][!#$%()*,.:;<=>@^_|~.{}]", " ", malaria.text)
meningitis.text <- gsub("[][!#$%()*,.:;<=>@^_|~.{}]", " ", meningitis.text)

# Replace abbreviations
ebola.text <- replace_abbreviation(ebola.text)
malaria.text <- replace_abbreviation(malaria.text)
meningitis.text <- replace_abbreviation(meningitis.text)

# Replace contractions
ebola.text <- replace_contraction(ebola.text)
malaria.text <- replace_contraction(malaria.text)
meningitis.text <- replace_contraction(meningitis.text)

# Replace symbols with words
ebola.text <- replace_symbol(ebola.text)
malaria.text <- replace_symbol(malaria.text)
meningitis.text <- replace_symbol(meningitis.text)

#Enlève la ponctuation
ebola.text <- removePunctuation(ebola.text)
malaria.text <- removePunctuation(malaria.text)
meningitis.text <- removePunctuation(meningitis.text)

#Enlève les nombres
ebola.text <- removeNumbers(ebola.text)
malaria.text <- removeNumbers(malaria.text)
meningitis.text <- removeNumbers(meningitis.text)

#bas de casse : en minuscule
ebola.text <- tolower(ebola.text)
malaria.text <- tolower(malaria.text)
meningitis.text <- tolower(meningitis.text)

#Premier nettoyage mots non significatifs avec stopwords
ebola.text  <- removeWords(ebola.text, stopwords("french") )
ebola.text  <- removeWords(ebola.text, stopwords("en") )
ebola.text  <- removeWords(ebola.text, stopwords("SMART") )
ebola.text  <- removeWords(ebola.text, stopwords("german") )

malaria.text  <- removeWords(malaria.text, stopwords("en") )
malaria.text  <- removeWords(malaria.text, stopwords("SMART") )
malaria.text  <- removeWords(malaria.text, stopwords("german") )
malaria.text  <- removeWords(malaria.text, stopwords("french") )

meningitis.text  <- removeWords(meningitis.text, stopwords("french") )
meningitis.text  <- removeWords(meningitis.text, stopwords("en") )
meningitis.text  <- removeWords(meningitis.text, stopwords("SMART") )
meningitis.text  <- removeWords(meningitis.text, stopwords("german") )


#Nettoyage mots non significatifs 3 : liste spécifique fournie par nos soins.
specificWords <- c("cest", "faut", "être", "comme", "non", "alors", "depuis",
                   "fait", "quil","that","ipad","clients", "iphone", "android", "twitter", "flase","client", "web", "you", 
                   "are", "for","fo", "and","alfred")
ebola.text  <- removeWords(ebola.text, specificWords  )
malaria.text  <- removeWords(malaria.text, specificWords  )
meningitis.text   <- removeWords(meningitis.text , specificWords  )

#Nettoyage mots personnalisés en fonction des cas 
ebola.text  <- removeWords(ebola.text, c("that","ipad","clients", "iphone", "android", "twitter", "flase","client", "web", "you", 
                                         "are", "for","fo", "and","alfred" )  )
malaria.text  <- removeWords(malaria.text, c("clients", "iphone", "android", "twitter", "flase","client", "web", "you", 
                                             "are", "for","fo", "and","alfred")  )
meningitis.text  <- removeWords(meningitis.text, c("clients", "iphone", "android", "twitter", "flase","client", "web", "you", 
                                                   "are", "for","fo", "and","alfred")  )

#Nettoyage des blancs
ebola.text <- stripWhitespace(ebola.text)
malaria.text <- stripWhitespace(malaria.text)
meningitis.text <- stripWhitespace(meningitis.text)
#/Nettoyage

#Transformations pour traitements.

#Transformation en vecteurs
Vebola.text <- unlist(ebola.text)
Vmalaria.text <- unlist(malaria.text)
Vmeningitis.text <- unlist(meningitis.text)

#Transformation en vecteurs source
ebola.source <- VectorSource(Vebola.text)
malaria.source <- VectorSource(Vmalaria.text)
meningitis.source <- VectorSource(Vmeningitis.text)

#Transformation en Corpus
ebola.corpus <- VCorpus(ebola.source)
malaria.corpus <- VCorpus(malaria.source)
meningitis.corpus <- VCorpus(meningitis.source)

# creation de Term Document Matrix
ebola.tdm <- TermDocumentMatrix(ebola.corpus)
malaria.tdm <- TermDocumentMatrix(malaria.corpus)
meningitis.tdm <- TermDocumentMatrix(meningitis.corpus)

# Conversion TDM en matrice
ebola.m  <- as.matrix(ebola.tdm)
malaria.m  <- as.matrix(malaria.tdm)
meningitis.m  <- as.matrix(meningitis.tdm)

# Somme des lignes pour fréquences des termes et tri par fréquence.
ebola.term_frequency <- rowSums(ebola.m)
ebola.term_frequency <- sort(ebola.term_frequency, decreasing = TRUE)
malaria.term_frequency <- rowSums(malaria.m)
malaria.term_frequency <- sort(malaria.term_frequency, decreasing = TRUE)
meningitis.term_frequency <- rowSums(meningitis.m)
meningitis.term_frequency <- sort(meningitis.term_frequency, decreasing = TRUE)
####affichage 
head(tweets_df)
head(tweets1_df)
head(tweets2_df)
head(tweets_df$text)
head(tweets1_df$text)
head(tweets2_df$text)
head(tweets_df2)
head(tweets1_df2)
head(tweets2_df2)
#'
#'
#Graphiques en barre pour les 20 premiers termes.
par(mar = c(10,4,4,2))  #pour recadrer le graphiques suite à des labels trop grands (ex : notredamedeslandes)
barplot(ebola.term_frequency[1:20],  col = "tan", las = 2, main="ebola")
barplot(malaria.term_frequency[1:20],  col = "tan", las = 2, main="malaria")
barplot(meningitis.term_frequency[1:20],  col = "tan", las = 2, main="meningitis")

#WordCloud pour ebola
par(mfrow=c(1,1)) #Raz Layout
#Jeu de données des mots et des occurences.

ebola.word_freqs <- data.frame(term = names(ebola.term_frequency), num = ebola.term_frequency)
# Nuage de Mots Clés
wordcloud(ebola.word_freqs$term, ebola.word_freqs$num, min.freq =3,scale=c(5,0.2),max.words = 100,title.size=2.5, colors = c("grey80", "darkgoldenrod1", "tomato"), title = "ebola")

###################

# Emotions for each tweet using NRC dictionary
emotions <- get_nrc_sentiment(tweets_df$text)
emo_bar = colSums(emotions)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])

word.df <- as.vector(tweets_df2)
emotion.df <- get_nrc_sentiment(word.df)
emotion.df2 <- cbind(tweets_df2, emotion.df) 
head(emotion.df2)

sent.value <- get_sentiment(word.df)

most.positive <- word.df[sent.value == max(sent.value)]
most.positive

most.negative <- word.df[sent.value <= min(sent.value)] 
most.negative

sent.value
positive.tweets <- word.df[sent.value > 0]
head(positive.tweets)

negative.tweets <- word.df[sent.value < 0] 
head(negative.tweets)

neutral.tweets <- word.df[sent.value == 0] 
head(neutral.tweets)

category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))
head(category_senti)

table(category_senti)
category_senti
head("neutral.tweets")






#fetch sentiment words from texts
Sentiment <- get_nrc_sentiment(tweets_df2)
head(Sentiment)
text <- cbind(tweets_df2,Sentiment)

#count the sentiment words by category
TotalSentiment <- data.frame(colSums(text[,c(2:11)]))
names(TotalSentiment) <- "count"
TotalSentiment <- cbind("sentiment" = rownames(TotalSentiment), TotalSentiment)
rownames(TotalSentiment) <- NULL

#total sentiment score of all texts
ggplot(data = TotalSentiment, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle(" EBOLA Total Sentiment Score")






#WordCloud pour malaria
par(mfrow=c(1,1))
#Jeu de données des mots et des occurences.
malaria.word_freqs <- data.frame(term = names(malaria.term_frequency), num = malaria.term_frequency)
# Nuage de Mots Clés
wordcloud(malaria.word_freqs$term, malaria.word_freqs$num,min.freq =3,scale=c(5,0.2), max.words = 100,title.size=2.5, colors = c("grey80", "darkgoldenrod1", "tomato"), title = "malaria")
###################

#####malaria
# Emotions for each tweet using NRC dictionary
emotions1 <- get_nrc_sentiment(tweets1_df$text)
emo_bar1 = colSums(emotions1)
emo_sum1 = data.frame(count=emo_bar1, emotion=names(emo_bar))
emo_sum1$emotion = factor(emo_sum1$emotion, levels=emo_sum1$emotion[order(emo_sum1$count, decreasing = TRUE)])


word.df <- as.vector(tweets1_df2)
emotion.df <- get_nrc_sentiment(word.df)
emotion.df2 <- cbind(tweets1_df2, emotion.df) 
head(emotion.df2)

sent.value <- get_sentiment(word.df)

most.positive <- word.df[sent.value == max(sent.value)]
most.positive

most.negative <- word.df[sent.value <= min(sent.value)] 
most.negative

sent.value
positive.tweets <- word.df[sent.value > 0]
head(positive.tweets)

negative.tweets <- word.df[sent.value < 0] 
head(negative.tweets)

neutral.tweets <- word.df[sent.value == 0] 
head(neutral.tweets)

category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))
head(category_senti)

table(category_senti)
category_senti
head("neutral.tweets")







#fetch sentiment words from texts
Sentiment <- get_nrc_sentiment(tweets1_df2)
head(Sentiment)
text <- cbind(tweets1_df2,Sentiment)

#count the sentiment words by category
TotalSentiment <- data.frame(colSums(text[,c(2:11)]))
names(TotalSentiment) <- "count"
TotalSentiment <- cbind("sentiment" = rownames(TotalSentiment), TotalSentiment)
rownames(TotalSentiment) <- NULL

#total sentiment score of all texts
ggplot(data = TotalSentiment, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("MALARIA Total Sentiment Score")





#WordCloud pour meningitis
par(mfrow=c(1,1))
#Jeu de données des mots et des occurences.
meningitis.word_freqs <- data.frame(term = names(meningitis.term_frequency), num = meningitis.term_frequency)
# Nuage de Mots Clés
wordcloud(meningitis.word_freqs$term, meningitis.word_freqs$num,min.freq =3,scale=c(5, .2), max.words = 100, title.size=2.5, colors = c("grey80", "darkgoldenrod1", "tomato"), title = "meningitis")
###################
#########meningitis
# Emotions for each tweet using NRC dictionary
emotions2 <- get_nrc_sentiment(tweets2_df$text)
emo_bar2 = colSums(emotions2)
emo_sum2 = data.frame(count=emo_bar2, emotion2=names(emo_bar2))
emo_sum2$emotion = factor(emo_sum2$emotion, levels=emo_sum2$emotion[order(emo_sum2$count, decreasing = TRUE)])

word.df <- as.vector(tweets2_df2)
emotion.df <- get_nrc_sentiment(word.df)
emotion.df2 <- cbind(tweets2_df2, emotion.df) 
head(emotion.df2)

sent.value <- get_sentiment(word.df)

most.positive <- word.df[sent.value == max(sent.value)]
most.positive

most.negative <- word.df[sent.value <= min(sent.value)] 
most.negative

sent.value
positive.tweets <- word.df[sent.value > 0]
head(positive.tweets)

negative.tweets <- word.df[sent.value < 0] 
head(negative.tweets)

neutral.tweets <- word.df[sent.value == 0] 
head(neutral.tweets)

category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))
head(category_senti)

table(category_senti)
category_senti
head("neutral.tweets")




#fetch sentiment words from texts
Sentiment <- get_nrc_sentiment(tweets2_df2)
head(Sentiment)
text <- cbind(tweets2_df2,Sentiment)

#count the sentiment words by category
TotalSentiment <- data.frame(colSums(text[,c(2:11)]))
names(TotalSentiment) <- "count"
TotalSentiment <- cbind("sentiment" = rownames(TotalSentiment), TotalSentiment)
rownames(TotalSentiment) <- NULL

#total sentiment score of all texts
ggplot(data = TotalSentiment, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("MENINGITIS Total Sentiment Score")





###############################################
# Nuage de Mots clés en commun !
all_ebola <- paste(ebola.text , collapse = "")
all_malaria <- paste(malaria.text , collapse = "")
all_meningitis <- paste(meningitis.text , collapse = "")

all_tweets <- c(all_ebola, all_malaria, all_meningitis)
# transformation
all_source <- VectorSource(all_tweets)
all_corpus <- VCorpus(all_source)
all_tdm <- TermDocumentMatrix(all_corpus)
all_m <- as.matrix(all_tdm)
# Mots clés en commun
commonality.cloud(all_m, colors = "steelblue1",
                  max.words = 100)
commonality.cloud(all_tdm, random.order=FALSE, scale=c(5, .5),colors = brewer.pal(4, "Dark2"), max.words=100,title.size=1.5)

# Ajoute des noms de colonnes.
colnames(all_m) = c("ebola", "malaria", "meningitis")
#Plot le graphique.
comparison.cloud(all_m,
                 random.order=FALSE,
                 colors = brewer.pal(4, "Dark2"),
                 title.size=1.5,
                 max.words = 100,scale=c(5, .5))


#Dendrogramme pour ebola
par(mfrow=c(1,1)) #Raz Layout
# On supprime les termes clairsemées de la Term Document Matrix
ebola.tdm2 <- removeSparseTerms(ebola.tdm , sparse = 0.98)
# Transformation en matrice
ebola.m2 <- as.matrix(ebola.tdm2)
# Transformation en Data Frame
ebola.df2 <- as.data.frame(ebola.m2)
str(ebola.df2)
# Calcul de la distance entre les termes
ebola.dist <- dist(ebola.df2)
# Création de la classification hiérarchique (Hierarchical clustering:hc)
ebola.hc <- hclust(ebola.dist)
# Plot le dendrogram
par(mar = c(8,4,4,2))   #pour cadrage du diagramme
plot(ebola.hc)

#Dendrogramme pour malaria
par(mfrow=c(1,1)) #Raz Layout
# On supprime les termes clairsemées de la Term Document Matrix
malaria.tdm2 <- removeSparseTerms(malaria.tdm , sparse = 0.98)
# Transformation en matrice
malaria.m2 <- as.matrix(malaria.tdm2)
# Transformation en Data Frame
malaria.df2 <- as.data.frame(malaria.m2)
# Calcul de la distance entre les termes
malaria.dist <- dist(malaria.df2)
# Création de la classification hiérarchique (Hierarchical clustering:hc)
malaria.hc <- hclust(malaria.dist)
# Plot le dendrogram
par(mar = c(5,4,4,2))   #pour cadrage du diagramme
plot(malaria.hc)

#Dendrogramme pour meningitis
par(mfrow=c(1,1)) #Raz Layout
# On supprime les termes clairsemées de la Term Document Matrix
meningitis.tdm2 <- removeSparseTerms(meningitis.tdm , sparse = 0.98)
# Transformation en matrice
meningitis.m2 <- as.matrix(meningitis.tdm2)
# Transformation en Data Frame
meningitis.df2 <- as.data.frame(meningitis.m2)
# Calcul de la distance entre les termes
meningitis.dist <- dist(meningitis.df2)
# Création de la classification hiérarchique (Hierarchical clustering:hc)
meningitis.hc <- hclust(meningitis.dist)
# Plot le dendrogram
par(mar = c(5,4,4,2))   #pour cadrage du diagramme
plot(meningitis.hc)

ebola.m3 <- t(ebola.m2) # transpose the matrix to cluster documents (tweets)
set.seed(122) # set a fixed random seed
k <- 6 # number of clusters
kmeansResult <- kmeans(ebola.m3, k)
round(kmeansResult$centers, digits = 3) # cluster centers

malaria.m3 <- t(malaria.m2) # transpose the matrix to cluster documents (tweets)
set.seed(122) # set a fixed random seed
k <- 6 # number of clusters
kmeansResult <- kmeans(malaria.m3, k)
round(kmeansResult$centers, digits = 3) # cluster centers

meningitis.m3 <- t(meningitis.m2) # transpose the matrix to cluster documents (tweets)
set.seed(122) # set a fixed random seed
k <- 4 # number of clusters
kmeansResult <- kmeans(meningitis.m3, k)
round(kmeansResult$centers, digits = 3) # cluster centers

###Top Frequent Terms

(ebola.term_frequency <- findFreqTerms(ebola.tdm, lowfreq = 50))
#barchart(ebola.term_frequency[order.dendrogram = 20])
(malaria.term_frequency <- findFreqTerms(malaria.tdm, lowfreq = 50))
(meningitis.term_frequency <- findFreqTerms(meningitis.tdm, lowfreq = 50))

# which words are associated with 'ebola,malaria,meningitis'?
findAssocs(ebola.tdm, "ebola", 0.1)
ebola.term_frequency <- rowSums(as.matrix(ebola.tdm))
ebola.term_frequency <- subset(ebola.term_frequency, ebola.term_frequency >= 20)
ebola.df2  <- data.frame(ebola.term_frequency  = names(ebola.term_frequency), freq = ebola.term_frequency)

findAssocs(malaria.tdm, "malaria", 0.1)
malaria.term_frequency <- rowSums(as.matrix(malaria.tdm))
malaria.term_frequency <- subset(malaria.term_frequency, malaria.term_frequency >= 20)
malaria.df2  <- data.frame(malaria.term_frequency  = names(malaria.term_frequency), freq = malaria.term_frequency)

findAssocs(meningitis.tdm, "meningitis", 0.1)
meningitis.term_frequency <- rowSums(as.matrix(meningitis.tdm))
meningitis.term_frequency <- subset(meningitis.term_frequency, meningitis.term_frequency >= 20)
meningitis.df2  <- data.frame(meningitis.term_frequency  = names(meningitis.term_frequency), freq = meningitis.term_frequency)
#####################
library(ggplot2)
ggplot(ebola.df2, aes(x=ebola.term_frequency , y=ebola.term_frequency )) + geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=4))

ggplot(malaria.df2, aes(x=malaria.term_frequency , y=malaria.term_frequency )) + geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=4))

ggplot(meningitis.df2, aes(x=meningitis.term_frequency , y=meningitis.term_frequency )) + geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=4))

findAssocs(malaria.tdm2 , "malaria", 0.1)
findAssocs(meningitis.tdm, "meningitis", 0.2)
findAssocs(ebola.tdm2, "ebola",0.1)


##################################
###Network of Terms
library(graph)
library(Rgraphviz)
plot(ebola.term_frequency, vertex.shape = "none", term =ebola.term_frequency , corThreshold = 0.1, weighting = T)
plot(malaria.term_frequency, term =malaria.term_frequency , corThreshold = 0.1, weighting = T,mode="undirected")
plot(meningitis.term_frequency, term =meningitis.term_frequency ,corThreshold = 0.1, weighting = T)


###################################
#Topic Modelling

install.packages("topicmodels")
library(topicmodels)
rowTotals <- apply(ebola.tdm2 , 1, sum) #Find the sum of words in each Document
dtm.new   <- ebola.tdm2[rowTotals> 0, ]           #remove all docs without words

##dtm <- as.DocumentTermMatrix(ebola.tdm)
lda <- LDA(ebola.tdm2, k = 8) # find 8 topics
term <- terms(lda, 7) # first 7 terms of every topic
(term <- apply(term, MARGIN = 2, paste, collapse = ", "))
term

lda <- LDA(malaria.tdm2, k = 8) # find 8 topics
term <- terms(lda, 7) # first 7 terms of every topic
(term <- apply(term, MARGIN = 2, paste, collapse = ", "))
term

lda <- LDA(meningitis.tdm2, k = 8) # find 8 topics
term <- terms(lda, 7) # first 7 terms of every topic
(term <- apply(term, MARGIN = 2, paste, collapse = ", "))
term


