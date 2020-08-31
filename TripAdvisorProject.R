library(bnlearn)
library(dplyr)
library(tidytext)
library(stringr)
library(textstem)
library(plyr)

data("stop_words")

setwd("C:/Users/fabio/Desktop/Università/Modelli Probabilisti per le decisioni/Progetto")

load( file="training.Rdata" )

# prova tokenizers with tidy
dataset_content <- dataset[,c("ID_hotel", "ID_review","Content")]

dataset_content$id <- do.call(paste, c(dataset_content[c("ID_hotel", "ID_review")], sep = "000000")) 
dataset_content <- dataset_content[,c("id","Content")]


text_df <- tibble(id = dataset_content$id, text = dataset_content$Content)
#text_df <- tibble(text = dataset_content$Content)

text_df <- text_df %>%
  unnest_tokens(word, text) 

text_df <- dplyr::summarise(dplyr::group_by(text_df,id,word),count =n())


text_df <- text_df %>% anti_join(stop_words)

######################## 
# cleaning text review #
######################## 

# remove numbers
text_df<-text_df[-grep("\\b\\d+\\b", text_df$word),]
# remove whitespace
text_df$word <- gsub("\\s+","",text_df$word)
# lemming of word
text_df$word <- lemmatize_words(text_df$word)

text_df$word <- iconv(text_df$word, "latin1", "ASCII", sub="")


tf_idf <- text_df %>%
  bind_tf_idf(word, id, count) %>%
  arrange(desc(tf_idf))


affin <- get_sentiments("afinn")

terms_relevant_affin <- tf_idf %>%
  inner_join(affin) %>%
  filter((score >= 3 | score <= -3) & (score < 5 & score > -5)) 


terms_group <- dplyr::summarise(dplyr::group_by(terms_relevant_affin,word),count =n())

temp <- merge(terms_group,terms_relevant_affin,by="word")


# decidere soglia per filtrare termini rilevanti!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
terms_relevant_affin_tresh <- temp %>%
  filter(count.x>500)


terms_without_duplicate<- terms_relevant_affin_tresh %>% arrange (desc(tf_idf))
terms_without_duplicate<-terms_without_duplicate[c(1:4000),]
dataset_terms <- data.frame(matrix(ncol = ncol(terms_without_duplicate), nrow = 0), stringsAsFactors=FALSE)
colnames(dataset_terms) <- colnames(terms_without_duplicate)
duplicate <- list();
for (i in 1:nrow(terms_without_duplicate)){
  if ( terms_without_duplicate[i,"word"] %in% duplicate )
    next
  else{
    duplicate <- c(duplicate, terms_without_duplicate[i,"word"]) 
    dataset_terms <- rbind(dataset_terms, terms_without_duplicate[i,])
  }
}



terms_vector <- dataset_terms$word

terms_for_final_dataset <- data.frame(matrix(ncol = length(terms_vector), nrow = nrow(dataset)), stringsAsFactors=FALSE)
colnames(terms_for_final_dataset) <- terms_vector
terms_for_final_dataset[,] = 0
dataset_finale <- cbind(review=dataset$Content, terms_for_final_dataset, n_help=dataset$`No. Helpful`, values=dataset$Values, rooms=dataset$Rooms,
                        location=dataset$Location, cleanliness=dataset$Cleanliness, check_in=dataset$`Check in`, service=dataset$Service,
                        business=dataset$`Business service`, Overall=dataset$Overall, stringsAsFactors=FALSE)     


for (i in 1:nrow(dataset_finale)) {
  temp = cleaning(dataset_finale[i,1], i)
  dataset_finale[i, 2:(length(terms_vector)+1)] = temp
  print(i)
}

cleaning = function(review, i){
  
  temp = tibble(review = review)
  
  temp = temp %>%
    unnest_tokens(word, review) 
  
  no_stop_words = temp %>%
    anti_join(stop_words)
  
  # remove numbers
  no_stop_words<-no_stop_words[-grep("\\b\\d+\\b", no_stop_words$word),]
  # remove whitespace
  no_stop_words$word <- gsub("\\s+","",no_stop_words$word)
  # lemming of word
  no_stop_words$word <- iconv(no_stop_words$word, "latin1", "ASCII", sub="")
  no_stop_words$word <- lemmatize_words(no_stop_words$word)
  
  hist = terms_vector %in% no_stop_words$word
  temp = as.numeric(hist)
  
  return(temp)
}

save(dataset_finale, file="dataset_finale.RData")

depend = "[Overall]"

for (term in terms_vector) {
  depend = paste(depend, "[", term, "|Overall]", sep = "")
}

for (metadata in c("values","rooms","location","cleanliness","check_in","service","business")) {
  depend = paste(depend, "[", metadata, "|Overall]", sep = "")
}


dataset_finale <- subset(dataset_finale, Overall!="0")
dataset_finale = subset(dataset_finale, values!="-1"|rooms!="-1"|location!="-1"|cleanliness!="-1"|check_in!="-1"|service!="-1"|business!="-1"|Overall!="3")
dataset_finale = subset(dataset_finale, values!="-1"|rooms!="-1"|location!="-1"|cleanliness!="-1"|check_in!="-1"|service!="-1"|business!="-1"|Overall!="4")
dataset_finale = subset(dataset_finale, values!="-1"|rooms!="-1"|location!="-1"|cleanliness!="-1"|check_in!="-1"|service!="-1"|business!="-1"|Overall!="5")
dataset_finale = droplevels(dataset_finale)
dataset_finale[,2:ncol(dataset_finale)] <- lapply(dataset_finale[,2:ncol(dataset_finale)],as.factor)


#dataset_finale <- dataset_finale[,-which(names(dataset_finale) %in% c("n_help"))]

dag <- model2network(depend)


fit <- bn.fit(dag, dataset_finale[,-c(1)])

#poorco zio ora facciamo sto cazzo di testset

load("testing.RData")

testing_content <- testing[,c("Content")]
testing_content <- tibble( text = testing$Content)

terms_for_testing <- data.frame(matrix(ncol = length(terms_vector), nrow = nrow(testing_content)), stringsAsFactors=FALSE)
colnames(terms_for_testing) <- terms_vector
terms_for_testing[,] = 0
dataset_testing <- cbind(review=testing$Content, terms_for_testing, values=testing$Values, rooms=testing$Rooms,
                        location=testing$Location, cleanliness=testing$Cleanliness, check_in=testing$`Check in`, service=testing$Service,
                        business=testing$`Business service`, Overall=testing$Overall, stringsAsFactors=FALSE)     


for (i in 1:nrow(dataset_testing)) {
  temp = cleaning(dataset_testing[i,1], i)
  dataset_testing[i, 2:(length(terms_vector)+1)] = temp
  print(i)
}

dataset_testing <- dataset_testing[,-c(1)]


dataset_testing <- subset(dataset_testing, Overall!="0")
dataset_testing = subset(dataset_testing, values!="-1"|rooms!="-1"|location!="-1"|cleanliness!="-1"|check_in!="-1"|service!="-1"|business!="-1"|Overall!="3")
dataset_testing = subset(dataset_testing, values!="-1"|rooms!="-1"|location!="-1"|cleanliness!="-1"|check_in!="-1"|service!="-1"|business!="-1"|Overall!="4")
dataset_testing = subset(dataset_testing, values!="-1"|rooms!="-1"|location!="-1"|cleanliness!="-1"|check_in!="-1"|service!="-1"|business!="-1"|Overall!="5")
dataset_testing = droplevels(dataset_testing)

dataset_testing[,1:ncol(dataset_testing)] <- lapply(dataset_testing[,1:ncol(dataset_testing)],as.factor)

prediction <- predict (fit,"Overall", dataset_testing, method = "bayes-lw", prob=TRUE)

confronto <- data.frame(Atteso= dataset_testing$Overall, Predetto=prediction[1:length(prediction)])
#confusion matrix
confMat_base <- table(confronto$Atteso,confronto$Predetto)

base_accuracy <- sum(diag(confMat_base))/sum(confMat_base) 

base_precision <- diag(confMat_base) / rowSums(confMat_base) 

base_recall <- diag(confMat_base) / colSums(confMat_base) 

base_F_Measure = 2 * base_precision * base_recall / (base_precision + base_recall)
