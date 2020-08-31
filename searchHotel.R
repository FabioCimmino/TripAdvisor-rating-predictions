testing_hotel <- testing
testing_hotel <- cbind(testing_hotel,hotel= c(""),stringsAsFactors=FALSE)
for (i in 1:nrow(testing)){
  url <- testing_hotel[i,"URL"]
  split <- strsplit(url,"-")
  testing_hotel[i,"hotel"] <- gsub("_"," ",split[[1]][5])
}

searched_hotel <- data.frame(matrix(ncol = ncol(testing_hotel), nrow = 0), stringsAsFactors=FALSE)
colnames(searched_hotel) <- colnames(testing_hotel)
for (i in 1:nrow(testing_hotel)){
  if ( identical( testing_hotel[i,"hotel"],"Holiday Inn Port of Miami Downtown" ))
    searched_hotel <- rbind(searched_hotel, testing_hotel[i,] )
}

dataset_hotel <- searched_hotel[,c("ID_review","Content")]

dataset_hotel$id <- do.call(paste, c(dataset_hotel[c("ID_review")])) 
dataset_hotel <- dataset_hotel[,c("id","Content")]


text_df_hotel <- tibble(id = dataset_hotel$id, text = dataset_hotel$Content)
#text_df <- tibble(text = dataset_content$Content)

text_df_hotel <- text_df_hotel %>%
  unnest_tokens(word, text) 

text_df_hotel <- dplyr::summarise(dplyr::group_by(text_df_hotel,id,word),count =n())


text_df_hotel <- text_df_hotel %>% anti_join(stop_words)

######################## 
# cleaning text review #
######################## 

# remove numbers
text_df_hotel<-text_df_hotel[-grep("\\b\\d+\\b", text_df_hotel$word),]
# remove whitespace
text_df_hotel$word <- gsub("\\s+","",text_df_hotel$word)
# lemming of word
text_df_hotel$word <- lemmatize_words(text_df_hotel$word)

text_df_hotel$word <- iconv(text_df_hotel$word, "latin1", "ASCII", sub="")


tf_idf_hotel <- text_df_hotel %>%
  bind_tf_idf(word,id, count)

affin <- get_sentiments("afinn")

terms_relevant_affin_hotel <- tf_idf_hotel %>%
  inner_join(affin) %>%
  filter((score >= 3 | score <= -3) & (score < 5 & score > -5)) 

terms_group_hotel <- dplyr::summarise(dplyr::group_by(terms_relevant_affin_hotel,word),count =n())

temp_hotel <- merge(terms_group_hotel,terms_relevant_affin_hotel,by="word")

terms_relevant_affin_tresh_hotel <- temp_hotel %>%
  filter(count.x>1)

terms_without_duplicate_hotel<- terms_relevant_affin_tresh_hotel %>% arrange (desc(tf_idf))
terms_without_duplicate_hotel<-terms_without_duplicate_hotel[c(1:100),]
dataset_terms_hotel <- data.frame(matrix(ncol = ncol(terms_without_duplicate_hotel), nrow = 0), stringsAsFactors=FALSE)
colnames(dataset_terms_hotel) <- colnames(terms_without_duplicate_hotel)
duplicate_hotel <- list()
for (i in 1:nrow(terms_without_duplicate_hotel)){
  if ( terms_without_duplicate_hotel[i,"word"] %in% duplicate_hotel )
    next
  else{
    duplicate_hotel <- c(duplicate_hotel, terms_without_duplicate_hotel[i,"word"]) 
    dataset_terms_hotel <- rbind(dataset_terms_hotel, terms_without_duplicate_hotel[i,])
  }
}

dataset_hotel_2 <- dataset_terms_hotel[1:10,"count.x"]
  
