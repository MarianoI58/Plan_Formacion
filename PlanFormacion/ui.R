
#Es una aplicacion para gestionar el plan de formacion de una empresa
# Esta es una aplicacion web construida con shiny. Pude ser la base para muchas aplicaciones en las que haya tablas 
# que se puedan editar, introducir nuevos datos o borrar 
# El ejemplo es la identificacion de necesidades para establecer un plan de formacion
#El ejemplo se encuentra en 
# https://mariano64.shinyapps.io/hoja_referencia/
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)
library(shinyjs)
library(shinyWidgets)

library(shinysky) ## shinysky sirve para personalizar los botones
library(DT) # interface R à la bibliothèque JavaScript DataTables que permite funciones de filtro en tablas HTML

library(data.table) # Extension de data.frame permet l'ajout/modification/suppression rapides de colonnes par groupe
library(lubridate) # Permite la gestion de fechas et intervalos de tiempo. Fonctions pour travailler avec des dates-heures et des intervalles de temps
library(shinyalert) # Créez facilement de jolis messages contextuels (modals) dans « Shiny ». Un modal peut contenir du texte, des images, des boutons OK/Annuler, une entrée pour obtenir une réponse de l'utilisateur et de nombreuses autres options personnalisables.
library(readr) #lire des données rectangulaires (comme « csv », « tsv » et « fwf »). 
library(plan) # se utilizara en el caso de que se quiera hacer una visualizacion del estado del avance de las acciones de formacion
library(ggplot2)
library(plotly)
library(datasets)
library(dplyr) # gramatica para manipulacion de datos
library(timevis)
library(shinylive)
rm(list = ls())


shinyUI(fluidPage(
  
  navbarPage(" ",  
             tabPanel("Identificacion 
                        Necesidades", 
                      # Application title
                      titlePanel("Identificación de necesidades de desarrollo competencial"),
                      ### This is to adjust the width of pop up "showmodal()" for DT modify table 
                      tags$head(tags$style(HTML('
                            .modal-lg {
                            width: 1200px;
                            }
                            '))),
                      helpText("Nota: no olvidar guardar los cambios!"),
                      br(),
                      ### tags$head() is to customize the download button
                      tags$head(tags$style(".butt{background-color:#230682;} .butt{color: #e6ebef;}")),
                      downloadButton("table_csv", "Download in CSV", class="butt"),
                      #useShinyalert(), #Set up shinyalert
                      uiOutput("Esqueleto_Tabla"),
                      actionButton(inputId = "Datos_Tabla",label = "GUARDAR")),
             
             tabPanel("Identificacion de Formaciones",
                      
                      column(3, 
                             
                             # actionButton("action", label = "Actualizar"),
                             fileInput("dnc", "Cargar el documento",
                                       accept = c(
                                         "text/rds",
                                         ".rds")
                             ),
                             pickerInput(
                               inputId = "Pico",
                               label = "Seleccionar necesidad ",
                               choices = 1:25,
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE,
                                 `deselect-all-text` = "Ninguna",
                                 `select-all-text` = "Seleccionar todo",
                                 `none-selected-text` = ""
                               )),
                             tags$hr(),
                             checkboxInput("header", "Header", TRUE)
                      ),
                      
                      hr(),
                      #mainPanel(   
                      column(6,
                             DT::dataTableOutput("Setos")), 
                      hr(),
                      # actionButton(inputId = "Updated",label = "Save")
                      tags$head(tags$style(HTML('
                            .modal-lg {
                            width: 1200px;
                            }
                            '))),
                      helpText("Nota: Guardar los cambios!"),
                      br(),
                      ### tags$head() is to customize the download button
                      tags$head(tags$style(".butt{background-color:#230682;} .butt{color: #e6ebef;}")),
                      column(3,     
                             downloadButton("Tabla1_csv", "Descargar en CSV", class="butt")),
                      #useShinyalert(), # Set up shinyalert
                      
                      uiOutput("MainBody"),actionButton(inputId = "Updated",label = "Guardar 2")
             ),
             
             tabPanel("Programación Formaciones",
                      column(3,
                             # actionButton("action2", label = "Actualizar2"),
                             #sidebarPanel(
                             fileInput("dnf", "Cargar el documento",
                                       accept = c(
                                         "text/rds",
                                         ".rds")
                             ),
                             pickerInput(
                               inputId = "Pico2",
                               label = "Seleccionar la formacion ",
                               choices = 1:25,
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE,
                                 `deselect-all-text` = "Ninguna",
                                 `select-all-text` = "Seleccionar todo",
                                 `none-selected-text` = ""
                               )),
                             tags$hr(),
                             checkboxInput("header", "Header", TRUE)),
                      # hr(),
                      
                      column(6,
                             DT::dataTableOutput('Setos2')),
                      # dataTableOutput("Setos2") )
                      ##########3 UI
                      column(10,
                             tags$head(tags$style(HTML('
                            .modal-lg {
                            width: 1200px;
                            }
                            '))),
                             helpText("Nota: Guardar los cambios!"),
                             br(),
                             ### tags$head() is to customize the download button
                             tags$head(tags$style(".butt{background-color:#230682;} .butt{color: #e6ebef;}")),
                             downloadButton("Trich2_csv", "Download in CSV", class="butt"),
                             #useShinyalert(), # Set up shinyalert
                             uiOutput("Esqueleto2"), 
                             actionButton(inputId = "Updated2",label = "Guardar 3") )
             ),
             
             tabPanel("Grafico simplificado Plan de Formacion",
                      column(6,
                             fileInput("pf", "Cargar el documento",
                                       accept = c(
                                         "text/rds",
                                         ".rds")),
                             
                             actionButton("ok3", "actualizar el grafico")),
                      
                      
                      column(10,
                             h4("Visualizar el grafico"),  
                             timevisOutput("timeline"))
             ),
             
             tabPanel("Grafico de avance",             
                      
                      hr(),
                      plotlyOutput("plot1")
                      
             )
             
             
             
  )
  
)
)


