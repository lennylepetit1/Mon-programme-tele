# Mon-programme-télé

```r
# L'objectif de ce code est de scrapper les données du jour du site télé-loisir et d'effectuer des filtres
# pour faciliter le choix du film qu'on souhaite regarder à la maison le soir. Un endroit spécifique
# est dédié aussi pour les films déjà regardés avec la possibilité de mettre un commentaire
# On s'est aidé du livre "R pour l'extraction de données sur le web" de Armel soubeiga pour la partie du scrapping.

################################################################################
# PARTIE 1 : Scrapping des données
################################################################################

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

################################################################################
# PARTIE 2 : Application Shiny avec sauvegarde persistante
################################################################################

# Définir le fichier pour sauvegarder les films regardés
fichier_sauvegarde <- "mes_films_regardes.csv"

# Charger les films regardés si le fichier existe
if (file.exists(fichier_sauvegarde)) {
  mes_films_initial <- read.csv(fichier_sauvegarde, stringsAsFactors = FALSE)
} else {
  mes_films_initial <- data.frame(
    Titre_du_film = character(),
    Duree_du_film = character(),
    Type_de_film = character(),
    stringsAsFactors = FALSE
  )
}

# Interface utilisateur
ui <- fluidPage(
  titlePanel("Filtrer les Films de Programme TV"),
  sidebarLayout(
    sidebarPanel(
      textInput("duree_min", "Durée minimale (HH:MM)", value = "00:00"),
      textInput("duree_max", "Durée maximale (HH:MM)", value = "02:00"),
      selectInput("type_film", "Type de film", 
                  choices = c("Tous", unique(data$Type_de_film)), selected = "Tous"),
      textInput("heure_debut", "Heure de début après (HH:MM)", value = "00:00"),
      textInput("heure_fin_max", "Heure de fin avant (HH:MM)", value = "23:59"),
      checkboxInput("rediffusion", "Rediffusions uniquement", FALSE),
      actionButton("ajouter_film", "Ajouter le film sélectionné à Mes Films Regardés")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Films disponibles", DT::dataTableOutput("table_films")),
        tabPanel("Mes Films Regardés", 
                 DT::dataTableOutput("mes_films"),
                 actionButton("supprimer_film", "Supprimer le film de ma liste"),
                 uiOutput("commentaire_ui")
        )
      )
    )
  )
)

# Serveur
server <- function(input, output, session) {
  data$Heure_de_départ <- as.POSIXct(data$Heure_de_départ, format = "%H:%M", tz = "UTC")
  data$Duree_du_film <- as.POSIXct(data$Duree_du_film, format = "%H:%M", tz = "UTC")
  
  mes_films <- reactiveVal(mes_films_initial)
  
  data_filtrée <- reactive({
    heure_debut_input <- as.POSIXct(input$heure_debut, format = "%H:%M", tz = "UTC")
    heure_fin_max_input <- as.POSIXct(input$heure_fin_max, format = "%H:%M", tz = "UTC")
    duree_min_input <- as.POSIXct(input$duree_min, format = "%H:%M", tz = "UTC")
    duree_max_input <- as.POSIXct(input$duree_max, format = "%H:%M", tz = "UTC")
    
    data %>%
      filter(Duree_du_film >= duree_min_input) %>%
      filter(Duree_du_film <= duree_max_input) %>%
      filter(Heure_de_départ >= heure_debut_input) %>%
      filter(Heure_de_départ <= heure_fin_max_input) %>%
      filter(ifelse(input$rediffusion, Rediffusion == "Oui", TRUE)) %>%
      filter(input$type_film == "Tous" | Type_de_film == input$type_film)
  })
  
  output$table_films <- DT::renderDataTable({
    data_formatée <- data_filtrée() %>%
      mutate(
        Heure_de_départ = format(Heure_de_départ, "%H:%M"),
        Duree_du_film = format(Duree_du_film, "%H:%M")
      )
    DT::datatable(data_formatée, selection = "single")
  })
  
  observeEvent(input$ajouter_film, {
    selected_row <- input$table_films_rows_selected
    if (length(selected_row) > 0) {
      film_selectionne <- data_filtrée()[selected_row, c("Nom_du_Programme", "Duree_du_film", "Type_de_film")]
      film_selectionne$Duree_du_film <- format(film_selectionne$Duree_du_film, "%H:%M")
      film_selectionne$Commentaire <- ""
      mes_films(rbind(mes_films(), film_selectionne))
      write.csv(mes_films(), fichier_sauvegarde, row.names = FALSE)
    }
  })
  
  output$mes_films <- DT::renderDataTable({
    mes_films_formatée <- mes_films() %>%
      mutate(
        Duree_du_film = format(as.POSIXct(Duree_du_film, format = "%H:%M", tz = "UTC"), "%H:%M")
      )
    DT::datatable(mes_films_formatée, selection = "single")
  })
  
  observeEvent(input$supprimer_film, {
    selected_row <- input$mes_films_rows_selected
    if (length(selected_row) > 0) {
      films_restants <- mes_films()[-selected_row, ]
      mes_films(films_restants)
      write.csv(films_restants, fichier_sauvegarde, row.names = FALSE)
    }
  })
  
  # Gestion des commentaires
  observeEvent(input$mes_films_rows_selected, {
    selected_row <- input$mes_films_rows_selected
    if (length(selected_row) > 0) {
      film_selectionne <- mes_films()[selected_row, ]
      output$commentaire_ui <- renderUI({
        textAreaInput(
          inputId = paste("commentaire_", selected_row), 
          label = paste("Commentaire pour", film_selectionne$Nom_du_Programme), 
          value = film_selectionne$Commentaire, 
          rows = 4
        )
      })
    }
  })
  
  observe({
    selected_row <- input$mes_films_rows_selected
    if (length(selected_row) > 0) {
      commentaire_id <- paste("commentaire_", selected_row)
      commentaire_val <- input[[commentaire_id]]
      if (!is.null(commentaire_val)) {
        films <- mes_films()
        films$Commentaire[selected_row] <- commentaire_val
        mes_films(films)
        write.csv(films, fichier_sauvegarde, row.names = FALSE)
      }
    }
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)

