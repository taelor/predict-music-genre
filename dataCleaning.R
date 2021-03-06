# This function provides a resuable and consistent way to clean and prepare the music genre data
cleanData <- function(dataRaw){
  library(plyr)
  
  #Drop empty rows
  dataRaw = na.omit(dataRaw)
  
  #Drop unwanted columns
  dataClean = subset(dataRaw,select = -c(obtained_date, artist_name, track_name, instance_id))
  
  #Convert mode and key to numeric values
  dataClean$mode = revalue(dataClean$mode, c("Minor"="0", "Major"="1"))
  dataClean$mode = as.numeric(dataClean$mode)
  dataClean$key = revalue(dataClean$key, c("A#"="0", "D"="1","G#"="2","C#"="3","F#"="4","B"="5","G"="6","F"="7","A"="8","C"="9","E"="10","D#"="11"))
  dataClean$key = as.numeric(dataClean$key)
  
  #Make tempo numeric, replaces all '?' with NA
  dataClean$tempo = as.numeric(dataClean$tempo)
  
  ##Replace all -1 with NA in duration_ms
  dataClean$duration_ms[dataClean$duration_ms==-1] = NA
  
  #Create a subset for a genre, replace NAs in tempo and duration_ms with the average for that column
  electronicSubset = dataClean[dataClean$music_genre == "Electronic",]
  electronicSubset$tempo[is.na(electronicSubset$tempo)] = mean(electronicSubset$tempo, na.rm=T)
  electronicSubset$duration_ms[is.na(electronicSubset$duration_ms)] = mean(electronicSubset$duration_ms, na.rm=T)
  
  animeSubset = dataClean[dataClean$music_genre == "Anime",]
  animeSubset$tempo[is.na(animeSubset$tempo)] = mean(animeSubset$tempo, na.rm=T)
  animeSubset$duration_ms[is.na(animeSubset$duration_ms)] = mean(animeSubset$duration_ms, na.rm=T)
  
  jazzSubset = dataClean[dataClean$music_genre == "Jazz",]
  jazzSubset$tempo[is.na(jazzSubset$tempo)] = mean(jazzSubset$tempo, na.rm=T)
  jazzSubset$duration_ms[is.na(jazzSubset$duration_ms)] = mean(jazzSubset$duration_ms, na.rm=T)
  
  alternativeSubset = dataClean[dataClean$music_genre == "Alternative",]
  alternativeSubset$tempo[is.na(alternativeSubset$tempo)] = mean(alternativeSubset$tempo, na.rm=T)
  alternativeSubset$duration_ms[is.na(alternativeSubset$duration_ms)] = mean(alternativeSubset$duration_ms, na.rm=T)
  
  countrySubset = dataClean[dataClean$music_genre == "Country",]
  countrySubset$tempo[is.na(countrySubset$tempo)] = mean(countrySubset$tempo, na.rm=T)
  countrySubset$duration_ms[is.na(countrySubset$duration_ms)] = mean(countrySubset$duration_ms, na.rm=T)
  
  rapSubset = dataClean[dataClean$music_genre == "Rap",]
  rapSubset$tempo[is.na(rapSubset$tempo)] = mean(rapSubset$tempo, na.rm=T)
  rapSubset$duration_ms[is.na(rapSubset$duration_ms)] = mean(rapSubset$duration_ms, na.rm=T)
  
  bluesSubset = dataClean[dataClean$music_genre == "Blues",]
  bluesSubset$tempo[is.na(bluesSubset$tempo)] = mean(bluesSubset$tempo, na.rm=T)
  bluesSubset$duration_ms[is.na(bluesSubset$duration_ms)] = mean(bluesSubset$duration_ms, na.rm=T)
  
  rockSubset = dataClean[dataClean$music_genre == "Rock",]
  rockSubset$tempo[is.na(rockSubset$tempo)] = mean(rockSubset$tempo, na.rm=T)
  rockSubset$duration_ms[is.na(rockSubset$duration_ms)] = mean(rockSubset$duration_ms, na.rm=T)
  
  classicalSubset = dataClean[dataClean$music_genre == "Classical",]
  classicalSubset$tempo[is.na(classicalSubset$tempo)] = mean(classicalSubset$tempo, na.rm=T)
  classicalSubset$duration_ms[is.na(classicalSubset$duration_ms)] = mean(classicalSubset$duration_ms, na.rm=T)
  
  hipHopSubset = dataClean[dataClean$music_genre == "Hip-Hop",]
  hipHopSubset$tempo[is.na(hipHopSubset$tempo)] = mean(hipHopSubset$tempo, na.rm=T)
  hipHopSubset$duration_ms[is.na(hipHopSubset$duration_ms)] = mean(hipHopSubset$duration_ms, na.rm=T)
  
  #Combine each genre subset
  dataClean = rbind(electronicSubset, animeSubset, jazzSubset, alternativeSubset, countrySubset, rapSubset, bluesSubset, rockSubset, classicalSubset, hipHopSubset)
  
  dataClean
}
