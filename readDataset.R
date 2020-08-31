
library("readr")

setwd("C:/Users/Sam/Desktop/Modelli prob/Progetto")

training <- data.frame(matrix(ncol = 18, nrow = 0), stringsAsFactors=FALSE)
hotel_colums = c("ID_hotel", "ID_review","Overall Rating", "Avg. Price", "URL", "Author", "Content", "Date", "No. Reader", "No. Helpful",
                 "Overall", "Values", "Rooms", "Location", "Cleanliness", "Check in", "Service", "Business service")
colnames(training) <- hotel_colums

testing <- data.frame(matrix(ncol = 18, nrow = 0), stringsAsFactors=FALSE)
hotel_colums = c("ID_hotel", "ID_review","Overall Rating", "Avg. Price", "URL", "Author", "Content", "Date", "No. Reader", "No. Helpful",
                 "Overall", "Values", "Rooms", "Location", "Cleanliness", "Check in", "Service", "Business service")
colnames(testing) <- hotel_colums

overall_rating = ""
avg_price = ""
url = ""

path_training = "./Dataset/Training/"
path_testing = "./Dataset/Testing/"

file.names.training = dir(path_training, pattern =".dat")
file.name.testing = dir(path_testing, pattern=".dat")

row = vector(length=18)

processFile = function(filepath, id, dataset) {
  row[1] = id
  con = file(filepath, "r")
  while ( TRUE ) {
    line = readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    #print(line)
    split = strsplit(line, ">", )[[1]]
    tag = paste(split[1], ">", sep="")
    content = split[2]
    # controllo tag e inserisco valori nel dataframe
    switch(tag,
           
           "<Overall Rating>"={overall_rating = content
           },
           
           "<Avg. Price>"={avg_price = content
           },
           
           "<URL>" = {url = content},
           "<Author>" = {row[6] = content; 
                         id_review = id_review + 1; 
                         row[2] = id_review},
           "<Content>" = {row[7] = content},
           "<Date>" = {row[8] = content},
           "<No. Reader>" = {row[9] = content},
           "<No. Helpful>" = {row[10] = content},
           "<Overall>" = {row[11] = content},
           "<Value>" = {row[12] = content},
           "<Rooms>" = {row[13] = content},
           "<Location>" = {row[14] = content},
           "<Cleanliness>" = {row[15] = content},
           "<Check in / front desk>" = {row[16] = content},
           "<Service>" = {row[17] = content},
           "<Business service>" = {row[18] = content; 
                                    row[3] = overall_rating;
                                    row[4] = avg_price;
                                    row[5] = url;
                                    #print(row);
                                    dataset = rbind(dataset, row, stringsAsFactors=FALSE);
                                    colnames(dataset) <- hotel_colums
                                    #print(dataset)}
                                    }
           
    )
  }
  
  close(con)
  return(dataset)
}

# ciclare su tutti i file e creare unico dataframe con tutti gli hotel file.names[i]
id_hotel = 1
id_review = 0

for(i in 1:length(file.names.training)){
  
  print(i)
  
  path_file = paste(path_training, file.names.training[i], sep="")
  
  # chiamata a singolo processFile
  training = processFile(path_file, id_hotel, training)
  
  id_review = 0
  id_hotel = id_hotel + 1
}

# ciclare su tutti i file e creare unico dataframe con tutti gli hotel file.names[i]
id_hotel = 1
id_review = 0

for(i in 1:length(file.name.testing)){
  
  print(i)
  
  path_file = paste(path_testing, file.name.testing[i], sep="")
  
  # chiamata a singolo processFile
  testing = processFile(path_file, id_hotel, testing)
  
  #id_review = 0
  id_hotel = id_hotel + 1
}

write.csv(dataset, file = "training.csv")
write.csv(testing, file = "testing.csv")
temp = read.csv("testing.csv")


