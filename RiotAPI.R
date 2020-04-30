# Blaine Harper
# RiotAPI.R
# Truman State University

# Header when the source is loaded

cat("Thank you for downloading my RiotAPI.R script!
  It's still in alpha, so I don't have proper documentation
  and there will be many changes made as I try to stream-
  line the code prior to attempting to make a production
  Version of this script.
    
    Thank you!
      Blaine Harper
")

# Riot API integration for R

require(httr)
require(jsonlite)
require(tidyverse)
require(forcats)

lol.default.version = "10.4.1"

## Functions
  
lol.api <-function(api.key = "", API = "", subAPI = "", 
                   sum.name = "", accountId = "", summonerId = "", matchId = ""){
  url <- NULL
  if(API == "MATCH-V4") {
    if(subAPI == "matches") {
      url <- paste0("https://na1.api.riotgames.com/lol/match/v4/matches/", matchId,"?api_key=",api.key)
    } else if(subAPI == "lists") {
      url <- paste0("https://na1.api.riotgames.com/lol/match/v4/matchlists/by-account/", accountId,"?api_key=",api.key)
    }
  } else if (API == "SUMMONER-V4") {
    if(subAPI == "by-name") {
      url <- paste0("https://na1.api.riotgames.com/lol/summoner/v4/summoners/by-name/", sum.name,"?api_key=",api.key)
    } else if(subAPI == "by-account") {
      url <- paste0("https://na1.api.riotgames.com/lol/summoner/v4/summoners/by-account/", accountId,"?api_key=",api.key)
    } else {
      cat("That subAPI has not been set up or has been entered incorrectly!")
    }
  } else if(API == "CHAMPION-MASTERY-V4") {
    url <- paste0("https://na1.api.riotgames.com/lol/champion-mastery/v4/champion-masteries/by-summoner/",summonerId,"?api_key=",api.key)
  } else {
    cat("That API has not been set up or has been entered incorrectly!")
  }
  result <- fromJSON(url)
}

## Create the champion data frame
championDF <- function(lol.version)
{
  championList.url <- paste0("http://ddragon.leagueoflegends.com/cdn/",lol.version,"/data/en_US/champion.json")
  championList <- championJSON
  championList <- championList$data
  data.frame(Reduce(rbind, championList), row.names = 1:length(championList))
}

## Do the same thing from the downloaded file
championDF.fromFile <- function(file)
{
  championList.url <- paste0(file)
  championList <- fromJSON(championList.url)
  championList <- championList$data
  data.frame(Reduce(rbind, championList), row.names = 1:length(championList))
}

## ID Conversion Function
convert_champId <- function(champion, lol.version = lol.default.version)
{
  for( i in 1:nrow(championDF)) { 
    champion <- case_when( 
      champion ==  championDF$key[i] ~ as.character(championDF$name[i]),
      TRUE ~ as.character(champion)
    )
  }
  champion
}

lol.setup <- function(sum.name, api.key)
{
  cat(paste0("Loading data for summoner ", sum.name," with api-key ", api.key))
  sum.name <<- sum.name
  api.key <<- api.key
  summoner.data <<- lol.api(sum.name = sum.name, api.key = api.key, API = "SUMMONER-V4", subAPI = "by-name")
  
  # Personal data from the summoner data API
  summonerId <<- summoner.data$id
  accountId <<- summoner.data$accountId
  
  # Match Data stuff
  match.list <<- lol.api(api.key = api.key, API = "MATCH-V4", subAPI = "lists", accountId = accountId)
  match.data <<- match.list$matches
  
  mastery.data <<- lol.api(api.key = api.key, API = "CHAMPION-MASTERY-V4", summonerId = summonerId)
  
  # Add columns with the proper champion names
  match.data$championName <<- convert_champId(match.data$champion)
  mastery.data$championName <<- convert_champId(mastery.data$championId)
  
  # Mastery level is a factor, but it wasn't imported as one
  mastery.data$championLevel <<- factor(mastery.data$championLevel)
  
  # Create match data from different queues
  ranked.data <- match.data %>%
    filter(queue == "420")
  aram.data <- match.data %>%
    filter(queue == "450")
  norms.data <- match.data %>%
    filter(queue == "400")
}

## Plot functions
theme_set(theme_classic())

plot.champ.freq<-function(sum.name, api.key)
{
  # Match Data stuff
  summoner.data <- lol.api(sum.name = sum.name, api.key = api.key, API = "SUMMONER-V4", subAPI = "by-name")
  
  match.list <- lol.api(api.key = api.key, API = "MATCH-V4", subAPI = "lists", accountId = summoner.data$accountId)
  data <- match.list$matches
  data$championName <- convert_champId(data$champion)
  
  ggplot(data = data) +
    geom_bar(mapping = aes(x = fct_infreq(championName), fill = championName),stat="count") +
    geom_hline(yintercept = 0, linetype = "solid") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none") +
    labs(title = paste("Champion frequency for",sum.name),x="Champion",y="Games Played",caption="Data from last 100 Games") +
    scale_fill_viridis_d()
}

plot.masteries<-function(sum.name, api.key)
{  
  summoner.data <- lol.api(sum.name = sum.name, api.key = api.key, API = "SUMMONER-V4", subAPI = "by-name")

  data <- lol.api(api.key = api.key, API = "CHAMPION-MASTERY-V4", summonerId = summoner.data$id)
  data$championName <- convert_champId(data$championId)
  # Mastery level is a factor, but it wasn't imported as one
  data$championLevel <- factor(data$championLevel)
  
  data$championName <- factor(data$championName, levels = data$championName[order(-data$championPoints)])
  peak_mastery <- max(data$championPoints)
  
  ggplot(data = data[1:20,]) +
    geom_bar(mapping = aes(x = championName, y = championPoints, fill = championLevel),stat="identity") +
    geom_hline(yintercept = 0, linetype = "solid") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = c(0.9, 0.6)) +
    scale_fill_viridis_d() +
    labs(fill = "Mastery\nLevel",title = paste("Champion Masteries for",sum.name),
         x="Champion",y="Mastery",caption=paste("Top 20 masteries shown"))
}

# Create a DF populated with champion data

championJSON.url <- paste0("http://ddragon.leagueoflegends.com/cdn/",lol.default.version,"/data/en_US/champion.json")
download.file(championJSON.url, destfile = "champions.json")

championDF <- championDF.fromFile("champions.json")
