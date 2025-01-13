################################################################################
# PARTIE 1 : Scrapping des données
################################################################################


# Chargement des packages


library(tibble)
library(rvest)
library(shiny)
library(dplyr)
library(DT)

# URL du site à scraper
url <- "https://www.programme-tv.net/"

# Lecture du contenu de la page web
webpage <- read_html(url)

conteneurs <- webpage %>% html_nodes(css = ".gridRow-cards")

numeros <- c()
programmes <- c()
durees <- c()
rediffusions <- c()
types <- c()
heures <- c()

for (conteneur in conteneurs) {
  numero <- conteneur %>% 
    html_node(css = ".gridRow-cardsChannelNumber") %>% 
    html_text(trim = TRUE)
  
  titres <- conteneur %>% 
    html_nodes(css = ".mainBroadcastCard-title") %>% 
    html_text(trim = TRUE)
  
  heures_film <- conteneur %>% 
    html_nodes(css = ".mainBroadcastCard-startingHour") %>% 
    html_text(trim = TRUE)
  
  durees_film <- conteneur %>% 
    html_nodes(css = ".mainBroadcastCard-durationContent") %>% 
    html_text(trim = TRUE)
  
  rediffusions_film <- conteneur %>% 
    html_nodes(css = ".mainBroadcastCard-rebroadcast") %>% 
    html_text(trim = TRUE)
  
  types_film <- conteneur %>% 
    html_nodes(css = ".mainBroadcastCard-type") %>% 
    html_text(trim = TRUE)
  
  for (i in seq_along(titres)) {
    numeros <- c(numeros, numero)
    programmes <- c(programmes, titres[i])
    heures <- c(heures, heures_film[i])
    durees <- c(durees, durees_film[i])
    rediffusions <- c(rediffusions, ifelse(length(rediffusions_film) >= i, "Oui", "Non"))
    types <- c(types, types_film[i])
  }
}

data <- tibble(
  Numero_chaine = numeros,
  Nom_du_Programme = programmes,
  Heure_de_départ = heures,
  Duree_du_film = durees,
  Rediffusion = rediffusions,
  Type_de_film = types
)

data <- data %>%
  mutate(Heure_de_départ = gsub("h", ":", Heure_de_départ)) %>%
  mutate(Heure_de_départ = as.POSIXct(Heure_de_départ, format = "%H:%M", tz = "UTC")) %>%
  mutate(Heure_de_départ = format(Heure_de_départ, "%H:%M"))

convertir_duree <- function(duree) {
  if (grepl("h", duree)) {
    duree <- gsub("h", ":", duree)
    duree <- as.POSIXct(duree, format = "%H:%M", tz = "UTC")
    return(format(duree, "%H:%M"))
  } else {
    minutes <- as.numeric(gsub("min", "", duree))
    heures <- floor(minutes / 60)
    minutes_restantes <- minutes %% 60
    return(sprintf("%02d:%02d", heures, minutes_restantes))
  }
}

data <- data %>%
  mutate(Duree_du_film = sapply(Duree_du_film, convertir_duree))