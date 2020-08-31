library(shiny)
library(devtools)
library(ShinyRatingInput)
library(bnlearn)
library(dplyr)
library(tidytext)
library(stringr)
library(textstem)
library(plyr)

# Define the fields we want to save from the form
fields <- c("commento", "values", "rooms", "location", "cleanliness", "check_in", "service", "business", "Overall")

# Save a response
# ---- This is one of the two functions we will change for every storage type ----
saveData <- function(data) {
  data <- as.data.frame(t(data))
    responses <<- data
}

# Load all previous responses
# ---- This is one of the two functions we will change for every storage type ----
loadData <- function() {
  if (exists("responses")) {
    responses
  }
}


# Shiny app with 3 fields that the user can submit data for
shinyApp(
  ui <- fluidPage(
    titlePanel(div(img(height = 70, width = 200, src = "logo.png"),
                   "Review Classification ")),
    
    tags$head(tags$style("#selected_var{color: black;
                                 font-size: 30px;
                        font-style: italic;
                        }"
                         )
    ),
    
    tabsetPanel(
      tabPanel("Review Classification", 
               tags$br(),
               textAreaInput("commento", "Commento", "", placeholder = "Inserire commento"),
               
               # default settings from 1 to 5
               ratingInput("values", label="Values"), tags$br(),
               ratingInput("rooms", label="Rooms"), tags$br(),
               ratingInput("location", label="Location"), tags$br(),
               ratingInput("cleanliness", label="Cleanliness"), tags$br(),
               ratingInput("check_in", label="Check-in"), tags$br(),
               ratingInput("service", label="Service"), tags$br(),
               ratingInput("business", label="Business"), tags$br(),
               #ratingInput("Overall", label="Overall"), tags$br(),
               tags$br(),tags$br(),
               
               actionButton("submit", "Invio"), tags$br(),
               textOutput("selected_var")),
      
      tabPanel("Hotel Classification", 
               tags$br(),
               textInput("hotel", "Hotel", "", placeholder = "Inserire hotel da cercare"),
               actionButton("cerca_hotel", "Invio"), tags$br(),
               
               plotOutput("distPlot"))
    )


  ),
  server = function(input, output, session) {
    
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      data <- sapply(fields, function(x) input[[x]])
      data
    })
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
      
      saveData(formData())
      responses$Overall=3;
      #creo riga per test
      getReview <- data.frame(matrix(ncol = 58, nrow = 1), stringsAsFactors=FALSE)
      colnames(getReview) <- c(terms_vector)
      getReview <- cbind(getReview,responses[1,2:9])
      getReview[,1:ncol(getReview)] <- lapply(getReview[,1:ncol(getReview)],as.factor)
      
      for(i in 1:58) if(is.factor(getReview[,i])) levels(getReview[,i]) <- c("0","1")
      
      for(i in 59:65) if(is.factor(getReview[,i])) levels(getReview[,i]) <- c("-1","1","2","3","4","5")
      
      if(is.factor(getReview[,66])) levels(getReview[,66]) <- c("1","2","3","4","5")
      getReview[1,1:58]=0
      
      getReview[1,59:ncol(getReview)]= responses[1,2:9]
      
      temp = tibble(review = as.character(responses[1,1]))
      
      temp = temp %>%
        unnest_tokens(word,  review) 
      
      no_stop_words = temp %>%
        anti_join(stop_words)
    
      no_stop_words$word <- iconv(no_stop_words$word, "latin1", "ASCII", sub="")
      no_stop_words$word <- lemmatize_words(no_stop_words$word)
      
      hist = terms_vector %in% no_stop_words$word
      temp = as.numeric(hist)
      temp=as.factor(temp)
      
      getReview[1, 1:58] <- temp
      
      pred <- predict (fit,"Overall", getReview, method = "bayes-lw", prob=TRUE)
      
      print(pred)
      
      output$selected_var <- renderText({ 
        paste("La valutazione del commento secondo i parametri selezionati risulta essere ", pred[[1]])
      })
      
    })
    
    observeEvent(input$cerca_hotel, {
      
      searched_hotel <- data.frame(matrix(ncol = ncol(testing_hotel), nrow = 0), stringsAsFactors=FALSE)
      colnames(searched_hotel) <- colnames(testing_hotel)
      for (i in 1:nrow(testing_hotel)){
        if ( identical( testing_hotel[i,"hotel"],input$hotel ))
          searched_hotel <- rbind(searched_hotel, testing_hotel[i,] )
      }
      
      dataset_hotel <- searched_hotel[,c("ID_review","Content")]
      
      dataset_hotel$id <- do.call(paste, c(dataset_hotel[c("ID_review")])) 
      dataset_hotel <- dataset_hotel[,c("id","Content")]
      
      
      text_df_hotel <- tibble(id = dataset_hotel$id, text = dataset_hotel$Content)
      
      
      text_df_hotel <- text_df_hotel %>%
        unnest_tokens(word, text) 
      
      text_df_hotel <- dplyr::summarise(dplyr::group_by(text_df_hotel,id,word),count =n())
      
      
      text_df_hotel <- text_df_hotel %>% anti_join(stop_words)
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
      
      dataset_hotel_2 <- dataset_terms_hotel[1:10,c("word","count.x")]
      
      
      
      
      output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- dataset_hotel_2$count.x
        
        barplot(x, main="Distribuzione delle 10 parole con tf-idf maggiore",
                names.arg=dataset_hotel_2$word, col="dark green")
        
        # draw the histogram with the specified number of bins
        #hist(x, col = 'darkgray', border = 'white')
      })
    
    })
    
  }
)