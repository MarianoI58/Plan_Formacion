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
#useShinyalert()

#Esta aplicacion sirve para la gestion del plan de formacion de una empresa


shinyServer(function(input, output, session){

  ### interactive dataset 
  Valores_tabla<-reactiveValues()
  # Valores_tabla$Data<-as.data.table(dnc)
  
  dnc<-readRDS("dnc.rds",refhook = NULL)
  Valores_tabla$Data <- as.data.table(dnc)
  #### Esqueleto_Tabla is the id of DT table Output de salida final
  output$Esqueleto_Tabla<-renderUI({
    fluidPage(
      tabPanel("Identificacion Necesidades", 
               hr(),
               column(6,offset = 6,
                      HTML('<div class="btn-group" role="group" aria-label="Basic example" style = "padding:10px">'),
                      ### tags$head() This is to change the color of "Add a new row" button
                      tags$head(tags$style(".butt2{background-color:#231651;} .butt2{color: #e6ebef;}")),
                      div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "Agrega_encabezado_linea",label = "Añadir", class="butt2") ),
                      tags$head(tags$style(".butt4{background-color:#4d1566;} .butt4{color: #e6ebef;}")),
                      div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "Modifica_encabezado_linea",label = "Editar", class="butt4") ),
                      tags$head(tags$style(".butt3{background-color:#590b25;} .butt3{color: #e6ebef;}")),
                      div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "Borra_encabezado_linea",label = "Borrar", class="butt3") ),
                      ### Optional: a html button 
                      # HTML('<input type="submit" name="Agrega_encabezado_linea" value="Add">'),
                      HTML('</div>') ),
               
               column(12,DTOutput("Main_table_trich")),
               tags$script("$(document).on('click', '#Main_table_trich button', function () {
                   Shiny.onInputChange('lastClickId',this.id);
                   Shiny.onInputChange('lastClick', Math.random()) });")
               
      ) )
  })
  
  #### render DataTable part ####
  output$Main_table_trich<-renderDT({
    DT=Valores_tabla$Data
    datatable(DT,selection = 'single',
              escape=F) })
  
  
  observeEvent(input$Agrega_encabezado_linea, {
    ### This is the pop up board for input a new row
    showModal(modalDialog(title = "Añadir una nueva necesidad",
                          numericInput(paste0("Id_add", input$Agrega_encabezado_linea), "Id:",0),  
                          textInput(paste0("Sector_add", input$Agrega_encabezado_linea), "Sector"),
                          textInput(paste0("Necesidad_add", input$Agrega_encabezado_linea), "Necesidad"),
                          ###textInput(paste0("Competencia_add", input$Agrega_encabezado_linea), "Competencia"),
                          selectInput(paste0("Competencia_add", input$Agrega_encabezado_linea), "Competencia:",choices=c('Tecnica', 'Organizativa', "Relacional")),
                          selectInput(paste0("Razones_add", input$Agrega_encabezado_linea), "Razones:", choices=c('Nuevos medios','Nuevos métodos', "Sustitucion", "Mejora competencial","Polivalencia")),
                          selectInput(paste0("Criticidad_add", input$Agrega_encabezado_linea), "Criticidad",choices=c("Muy critica","Critica","No critica")),
                          textInput(paste0("RelacionEstrategia_add", input$Agrega_encabezado_linea), "RelacionEstrategia"), 
                          numericInput(paste0("NumeroPersonas_add", input$Agrega_encabezado_linea), "NumeroPersonas:",1),  
                          textInput(paste0("Comentario_add", input$Agrega_encabezado_linea), "Comentario"), 
                          actionButton("gogo", "Añadir item"),
                          easyClose = TRUE, footer = NULL, size= c("s")))
  })
  ### Add a new row to DT  
  observeEvent(input$gogo, {
    NuevaLinea=data.table(
      Id=input[[paste0("Id_add", input$Agrega_encabezado_linea)]],
      Sector=input[[paste0("Sector_add", input$Agrega_encabezado_linea)]],
      Necesidad=input[[paste0("Necesidad_add", input$Agrega_encabezado_linea)]],
      Competencia=input[[paste0("Competencia_add", input$Agrega_encabezado_linea)]],
      Razones=input[[paste0("Razones_add", input$Agrega_encabezado_linea)]],
      Criticidad=input[[paste0("Criticidad_add", input$Agrega_encabezado_linea)]],
      RelacionEstrategia=input[[paste0("RelacionEstrategia_add", input$Agrega_encabezado_linea)]],
      NumeroPersonas=input[[paste0("NumeroPersonas_add", input$Agrega_encabezado_linea)]],
      Comentario=input[[paste0("Comentario_add", input$Agrega_encabezado_linea)]]
    )
    Valores_tabla$Data<-rbind(Valores_tabla$Data,NuevaLinea , fill=TRUE)
    removeModal()
  })
  
  
  
  
  ### save to RDS part 
  observeEvent(input$Datos_Tabla,{
    
    saveRDS(Valores_tabla$Data,"dnc.rds")
    shinyalert(title = "Guardado!", type = "success")
  })
  
  ### Suprime las filas seleccionadas
  ### this is warning messge for deleting
  observeEvent(input$Borra_encabezado_linea,{
    showModal(
      if(length(input$Main_table_trich_rows_selected)>=1 ){
        modalDialog(
          title = "Warning",
          paste("Esta seguro/a de querer borrar?",length(input$Main_table_trich_rows_selected),"rows?" ),
          footer = tagList(
            modalButton("Cancelar"),
            actionButton("ok", "Si")
          ), easyClose = TRUE)
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select row(s) that you want to delect!" ),easyClose = TRUE
        )
      }
      
    )
  })
  
  ### Si el usuario dice OK, elimine las filas seleccionadas
  observeEvent(input$ok, {
    Valores_tabla$Data=Valores_tabla$Data[-input$Main_table_trich_rows_selected]
    removeModal()
  })
  
  ### edit button
  observeEvent(input$Modifica_encabezado_linea,{
    showModal(
      if(length(input$Main_table_trich_rows_selected)>=1 ){
        modalDialog(
          fluidPage(
            h3(strong("Modification"),align="center"),
            hr(),
            dataTableOutput('row_modif'),
            actionButton("save_changes","Guardar los cambios"),
            tags$script(HTML("$(document).on('click', '#save_changes', function () {
                             var list_value=[]
                             for (i = 0; i < $( '.new_input' ).length; i++)
                             {
                             list_value.push($( '.new_input' )[i].value)
                             }
                             Shiny.onInputChange('newValue', list_value) });")) ), size="l" )
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select the row that you want to edit!" ),easyClose = TRUE
        )
      }
      
    )
  })
  
  
  
  
  #### modify part
  output$row_modif<-renderDataTable({
    selected_row=input$Main_table_trich_rows_selected
    old_row=Valores_tabla$Data[selected_row]
    row_change=(list())
    # which(i)
    for (i in colnames(old_row)) 
    {
      if (is.numeric(Valores_tabla$Data[[i]]))
      {
        row_change[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"','  type="number" id=new_',i,' ><br>')
      } 
      else if( is.Date(Valores_tabla$Data[[i]])){
        row_change[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"',' type="date" id=new_  ',i,'  ><br>') 
      }
      else 
        row_change[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"',' type="textarea"  id=new_',i,'><br>')
    }
    row_change=as.data.table(row_change)
    setnames(row_change,colnames(old_row))
    DT=row_change
    DT 
  },escape=F,options=list(dom='t',ordering=F,scrollX = TRUE,selection = "none"))
  
  
  
  ### This is to replace the modified row to existing row
  observeEvent(input$newValue,
               {
                 newValue=lapply(input$newValue, function(col) {
                   if (suppressWarnings(all(!is.na(as.numeric(as.character(col)))))) {
                     as.numeric(as.character(col))
                   } else {
                     col
                   }
                 })
                 DF=data.frame(lapply(newValue, function(x) t(data.frame(x))))
                 colnames(DF)=colnames(Valores_tabla$Data)
                 Valores_tabla$Data[input$Main_table_trich_rows_selected]<-DF
                 
               }
  )
  
  ### Esto es para poder descargar la tabla en csv
  output$table_csv<- downloadHandler(
    filename = function() {
      paste("dnc", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data.frame(Valores_tabla$Data), file, row.names = F)
    }
  )
  #source("global.r")
  ##############################
  ###2° HOJA definicion de necesidades de formacion
  #con llamada al fichro realizado en la pagina anterior "dnc" y trabajo en el fichero "dnf"
  ####################@   
  
  ####++++++++++++++++++++++++++++++++=La parte de llamada a la primera tabla
  
  
  vaya <- reactive({
    
    inFile <- input$dnc
    
    if (is.null(inFile))
      return(NULL)
    
    #dnc <- read.csv(inFile$datapath, header = input$header)
    dnc <- readRDS(inFile$datapath,";")
    
    dnc <- as.data.table(dnc)
    validate(
      need(input$Pico != "", "") 
    )
    dodo <-  filter(dnc,dnc$Id == input$Pico)
  })
  
  output$Setos <- renderDT({   vaya()     })
  
  
  #
  ############################################### La parte de llamada y modificacion de la 2° tabla   DEFINICION DE FORMACIONES   
  
  #removeModal()
  vals<-reactiveValues()
  dnf<-readRDS("dnf.rds",refhook = NULL)
  vals$Data <- as.data.table(dnf)
  #vals$Data<-as.data.table(read.csv2("dnf.csv",sep=";"))
  
  
  #### Esqueleto_Tabla is the id of DT table
  output$MainBody<-renderUI({
    fluidPage(
      tabPanel("Definicion de Formaciones",
               hr(),
               column(6,offset = 6,
                      HTML('<div class="btn-group" role="group" aria-label="Basic example" style = "padding:10px">'),
                      ### tags$head() This is to change the color of "Add a new row" button
                      tags$head(tags$style(".butt2{background-color:#231651;} .butt2{color: #e6ebef;}")),
                      div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "Add1_row_head",label = "Añadir", class="butt2") ),
                      tags$head(tags$style(".butt4{background-color:#4d1566;} .butt4{color: #e6ebef;}")),
                      div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "mod1_row_head",label = "Editar", class="butt4") ),
                      tags$head(tags$style(".butt3{background-color:#590b25;} .butt3{color: #e6ebef;}")),
                      div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "Del1_row_head",label = "Suprimir", class="butt3") ),
                      ### Optional: a html button 
                      # HTML('<input type="submit" name="Agrega_encabezado_linea" value="Add">'),
                      HTML('</div>') ),
               
               column(12,DTOutput("Maintable")),
               tags$script("$(document).on('click', '#Maintable button', function () {
                   Shiny.onInputChange('lastClickId',this.id);
                   Shiny.onInputChange('lastClick', Math.random()) });")
               
      ) )
  })
  
  #### render DataTable part ####
  output$Maintable<-renderDT({
    DT1=vals$Data
    datatable(DT1,selection = 'single',
              escape=F) })
  
  
  observeEvent(input$Add1_row_head, {
    ### This is the pop up board for input a new row
    showModal(modalDialog(title = "Añadir una nueva formacion",
                          numericInput(paste0("Id_add", input$Add1_row_head), "Id:",0),  
                          textInput(paste0("Formacion_add", input$Add1_row_head), "Formacion"),
                          numericInput(paste0("Horas_add", input$Add1_row_head), "Horas:",1), 
                          numericInput(paste0("Coste_add", input$Add1_row_head), "Coste:",100), 
                          textInput(paste0("Organismo_add", input$Add1_row_head), "Organismo"), 
                          numericInput(paste0("N_Personas_add", input$Add1_row_head), "NumeroPersonas:",1),  
                          textInput(paste0("Nombres_add", input$Add1_row_head), "Nombres"), 
                          selectInput(paste0("Estado_add", input$Add1_row_head), "Estado:", choices=c('Sin programar','Programado','En curso', 'Finalizado')),
                          textInput(paste0("Comentario_add", input$Add1_row_head), "Comentario"), 
                          actionButton("go1", "Añadir item"),
                          easyClose = TRUE, footer = NULL, size= c("s") 
    ))
  })
  ### Add a new row to DT  
  observeEvent(input$go1, {
    NuevaLinea=data.table(
      Id=input[[paste0("Id_add", input$Add1_row_head)]],
      Formacion=input[[paste0("Formacion_add", input$Add1_row_head)]],
      Horas=input[[paste0("Horas_add", input$Add1_row_head)]],
      Coste=input[[paste0("Coste_add", input$Add1_row_head)]],
      Organismo=input[[paste0("Organismo_add", input$Add1_row_head)]],
      N_Personas=input[[paste0("N_Personas_add", input$Add1_row_head)]],
      Nombres=input[[paste0("Nombres_add", input$Add1_row_head)]],
      Estado=input[[paste0("Estado_add", input$Add1_row_head)]],
      Comentario=input[[paste0("Comentario_add", input$Add1_row_head)]]
    )
    vals$Data<-rbind(vals$Data,NuevaLinea, fill=TRUE) #si las columnas no coinciden poner: fill=TRUE
    removeModal()
  })
  
  
  
  
  ### save to RDS part 
  observeEvent(input$Updated,{
    #write.csv2(vals$Data, "dnf.csv")
    saveRDS(vals$Data,"dnf.rds")
    shinyalert(title = "Saved!", type = "success")
  })
  
  
  
  ### delete selected rows part
  ### this is warning messge for deleting
  observeEvent(input$Del1_row_head,{
    showModal(
      if(length(input$Maintable_rows_selected)>=1 ){
        modalDialog(
          title = "Warning",
          paste("Esta seguro de querer borrar?",length(input$Maintable_rows_selected),"rows?" ),
          footer = tagList(
            modalButton("Cancelar"),
            actionButton("ok1", "Si")
          ), easyClose = TRUE)
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select row(s) that you want to delect!" ),easyClose = TRUE
        )
      }
      
    )
  })
  
  ### If user say OK, then delete the selected rows
  observeEvent(input$ok1, {
    vals$Data=vals$Data[-input$Maintable_rows_selected]
    removeModal()
  })
  
  ### edit button
  observeEvent(input$mod1_row_head,{
    showModal(
      if(length(input$Maintable_rows_selected)>=1 ){
        modalDialog(
          fluidPage(
            h3(strong("Modification"),align="center"),
            hr(),
            dataTableOutput('row_modif1'),
            actionButton("save_changes","Guardar los cambios"),
            tags$script(HTML("$(document).on('click', '#save_changes', function () {
                             var list_value=[]
                             for (i = 0; i < $( '.new_input' ).length; i++)
                             {
                             list_value.push($( '.new_input' )[i].value)
                             }
                             Shiny.onInputChange('newValue1', list_value) });")) ), size="l" )
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select the row that you want to edit!" ),easyClose = TRUE
        )
      }
      
    )
  })
  
  #### modify part
  output$row_modif1<-renderDataTable({
    selected_row=input$Maintable_rows_selected
    old_row=vals$Data[selected_row]
    row_change1=list()
    
    for (i in colnames(old_row))
    {
      if (is.numeric(vals$Data[[i]]))
      {
        
        row_change1[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"','  type="number" id=new_',i,' ><br>')
      } 
      else if( is.Date(vals$Data[[i]])){
        row_change1[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"',' type="date" id=new_  ',i,'  ><br>') 
      }
      else 
        row_change1[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"',' type="textarea"  id=new_',i,'><br>')
    }
    row_change1=as.data.table(row_change1)
    setnames(row_change1,colnames(old_row))
    DT1=row_change1
    DT1 
  },escape=F,options=list(dom='t',ordering=F,scrollX = TRUE, selection = "none"))
  
  
  
  ### Esto es para reemplazar la fila modificada a la fila existente
  observeEvent(input$newValue1,
               {
                 newValue1=lapply(input$newValue1, function(col) {
                   if (suppressWarnings(all(!is.na(as.numeric(as.character(col)))))) {
                     as.numeric(as.character(col))
                   } else {
                     col
                   }
                 })
                 DF1=data.frame(lapply(newValue1, function(x) t(data.frame(x))))
                 colnames(DF1)=colnames(vals$Data)
                 vals$Data[input$Maintable_rows_selected]<-DF1
                 
               }
  )
  ### This is nothing related to DT Editor but I think it is nice to have a download function in the Shiny so user 
  ### can download the table in csv
  output$Tabla1_csv<- downloadHandler(
    filename = function() {
      paste("dnf", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data.frame(vals$Data), file, row.names = F)
    }
  )      
  
  ############################################################################
  ###3 HOJA
  #### Llamada a la hoja de identificacion de la formacion "dnf"
  
  
  library(dplyr)
  #source("global.r")
  
  vaya2 <- reactive({
    
    inFile2 <- input$dnf
    
    if (is.null(inFile2))
      return(NULL)
    
    #dnc <- read.csv(inFile$datapath, header = input$header)
    dnf <- readRDS(inFile2$datapath,";")
    #dnf <- read_delim(inFile2$datapath,";", escape_double = FALSE, 
    #                  trim_ws = TRUE)
    
    dnf <- as.data.table(dnf)
    validate(
      need(input$Pico2 != "", "") 
    )
    dodo2 <-  filter(dnf,dnf$Id == input$Pico2)
    #dodo2
    #return(dodo2)
  })
  
  output$Setos2 <- renderDT({  vaya2()     })
  
  
  
  
  ##############
  
  ####### 3 HOJA 
  ######Programar la formacion
  
  vals2<-reactiveValues()
  pf<-readRDS("pf.rds",refhook = NULL)
  pf$FechaInicial<- as.Date(pf$FechaInicial,
                            format = "%d/%m/%y")
  pf$FechaFinal <- as.Date(pf$FechaFinal, format="%d/%m/%y")
  vals2$Data <- as.data.table(pf)
  
  
  #### Esqueleto_Tabla is the id of DT table
  output$Esqueleto2<-renderUI({
    fluidPage(
      tabPanel("Gestion de las  Formaciones",
               hr(),
               column(6,offset = 6,
                      HTML('<div class="btn-group" role="group" aria-label="Basic example" style = "padding:10px">'),
                      ### tags$head() This is to change the color of "Add a new row" button
                      tags$head(tags$style(".butt2{background-color:#231651;} .butt2{color: #e6ebef;}")),
                      div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "Add2_row_head",label = "Añadir", class="butt2") ),
                      tags$head(tags$style(".butt4{background-color:#4d1566;} .butt4{color: #e6ebef;}")),
                      div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "mod2_row_head",label = "Editar", class="butt4") ),
                      tags$head(tags$style(".butt3{background-color:#590b25;} .butt3{color: #e6ebef;}")),
                      div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "Del2_row_head",label = "Suprimir", class="butt3") ),
                      ### Optional: a html button 
                      #HTML('<input type="submit" name="Agrega_encabezado_linea" value="Add">'),
                      HTML('</div>') ),
               
               column(12,DTOutput("Maintable2")),
               tags$script("$(document).on('click', '#Maintable2 button', function () {
                   Shiny.onInputChange('lastClickId',this.id);
                   Shiny.onInputChange('lastClick', Math.random()) });")
               
      ) )
  })
  
  #### render DataTable part ####
  output$Maintable2<-renderDT({
    DT2=vals2$Data
    datatable(DT2,selection = 'single',   
              escape=F)
  })
  
  
  observeEvent(input$Add2_row_head, {
    ### This is the pop up board for input a new row
    showModal(modalDialog(title = "Añadir una nueva formacion",
                          numericInput(paste0("Id_add", input$Add2_row_head), "Id:",0),  
                          textInput(paste0("Formacion_add", input$Add2_row_head), "Formacion:"),
                          dateInput(paste0("FechaInicial_add", input$Add2_row_head), "FechaInicial:", value = Sys.Date(), format = "%d/%m/%y"),
                          dateInput(paste0("FechaFinal_add", input$Add2_row_head), "FechaFinal:", value = Sys.Date(),format = "%d/%m/%y"),
                          textInput(paste0("Organismo_add", input$Add2_row_head), "Organismo"), 
                          selectInput(paste0("Realizacion_add", input$Add2_row_head), "Realizacion:", choices=c('Sin programar','Programado','En curso', 'Finalizado')),
                          selectInput(paste0("Evaluacion_add", input$Add2_row_head), "Evaluacion:", choices=c('Sin evaluar','Excelente-10','Buena-8','Aceptable-6', 'Mala-4', 'Muy mala -2')),
                          textInput(paste0("Impacto_add", input$Add2_row_head), "Impacto"), 
                          actionButton("go2", "Añadir"),
                          easyClose = TRUE, footer = NULL, size= c("s") 
    ))
  })
  ### Add a new row to DT  
  observeEvent(input$go2, {
    NuevaLinea2= data.table(
      Id=input[[paste0("Id_add", input$Add2_row_head)]],
      Formacion=input[[paste0("Formacion_add", input$Add2_row_head)]],
      FechaInicial=input[[paste0("FechaInicial_add", input$Add2_row_head)]],
      FechaFinal=input[[paste0("FechaFinal_add", input$Add2_row_head)]],
      Organismo=input[[paste0("Organismo_add", input$Add2_row_head)]],
      Realizacion=input[[paste0("Realizacion_add", input$Add2_row_head)]],
      Evaluacion=input[[paste0("Evaluacion_add", input$Add2_row_head)]],
      Impacto=input[[paste0("Impacto_add", input$Add2_row_head)]]
    )
    vals2$Data<-rbind(vals2$Data,NuevaLinea2, fill=TRUE)
    #fill=TRUE) #si las columnas no coinciden poner: fill=TRUE
    
    removeModal()
  })
  
  ### save to RDS part 
  observeEvent(input$Updated2,{
    saveRDS(vals2$Data,"pf.rds")
    #write.csv2(vals2$Data, "pf.csv")
    shinyalert(title = "Guardado!", type = "success")
    
  })
  
  ### delete selected rows part
  ### this is warning messge for deleting
  observeEvent(input$Del2_row_head,{
    showModal(
      if(length(input$Maintable2_rows_selected)>=1 ){
        modalDialog(
          title = "Warning",
          paste("Esta seguro de querer borrar?",length(input$Maintable2_rows_selected),"rows?" ),
          footer = tagList(
            modalButton("Cancelar"),
            actionButton("ok2", "Si")
          ), easyClose = TRUE)
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select row(s) that you want to delect!" ),easyClose = TRUE
        )
      }
      
    )
  })
  
  ### If user say OK, then delete the selected rows
  observeEvent(input$ok2, {
    vals2$Data=vals2$Data[-input$Maintable2_rows_selected]
    removeModal()
  })
  
  ### edit button
  observeEvent(input$mod2_row_head,{
    showModal(
      if(length(input$Maintable2_rows_selected)>=1 ){
        modalDialog(
          fluidPage(
            h3(strong("Modificacion"),align="center"),
            hr(),
            dataTableOutput('row_modif2'),
            actionButton("save_changes","Guardar los cambios"),
            tags$script(HTML("$(document).on('click', '#save_changes', function () {
                             var list_value=[]
                             for (i = 0; i < $( '.new_input' ).length; i++)
                             {
                             list_value.push($( '.new_input' )[i].value)
                             }
                             Shiny.onInputChange('newValue2', list_value) });")) ), size="l" )
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select the row that you want to edit!" ),easyClose = TRUE
        )
      }
      
    )
  })
  
  #### modify part
  output$row_modif2<-renderDataTable({
    selected_row=input$Maintable2_rows_selected
    old_row=vals2$Data[selected_row]
    row_change2=list()
    
    for (i in colnames(old_row))
    {
      if (is.numeric(vals2$Data[[i]]))
      {
        
        row_change2[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"','  type="number" id=new_',(i),' ><br>')
      } 
      else if( is.Date(vals2$Data[[i]])){
        row_change2[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"',' type="date" format="%d/%m/%y" min="2020-02-01" max="2021-04-03"id=new_',i,'  ><br>')
      }
      else 
        row_change2[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"',' type="textarea" id=new_',i,'><br>')
    }
    row_change2=as.data.table(row_change2)
    setnames(row_change2,colnames(old_row))
    DT2=row_change2
    
    DT2
    
  },escape=F,options=list(dom='t',ordering=F,scrollX = TRUE,selection = "none"))
  
  
  ### Esto  para reemplazar la fila modificada a la fila existente
  observeEvent(input$newValue2,
               {
                 newValue2=lapply(input$newValue2, function(col) {
                   if (suppressWarnings(all(!is.na(as.numeric(as.character(col)))))) {
                     as.numeric(as.character(col))
                   } else  {
                     col
                   }
                 })
                 DF2=data.frame(lapply(newValue2, function(x) t(data.frame(x))))
                 colnames(DF2)=colnames(vals2$Data)
                 DF2$FechaInicial <- as.Date(DF2$FechaInicial)
                 DF2$FechaFinal <- as.Date(DF2$FechaFinal)
                 vals2$Data[input$Maintable2_rows_selected]<-DF2
                 
               }
  )
  ### This is nothing related to DT Editor but I think it is nice to have a download function in the Shiny so user 
  ### can download the table in csv
  
  output$Trich2_csv<- downloadHandler(
    filename = function() {
      paste("pf", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data.frame(vals2$Data), file, row.names = F)
    }
  )
  
  
  ############GRAFICOS
  #####°Grafico
  
  pf <- readRDS("pf.rds")
  # read.csv2("pf.csv",sep=";",header=TRUE)
  pf1 <- pf #as.data.table
  
  
  
  
  #######Grafico simplificado
  
  library(plan)
  library(timevis)
  
  
  observeEvent(input$ok3, 
               {   
                 pf1 
                 data <- data.frame(
                   id      = pf1$Id,
                   content = pf1$Formacion,
                   start   = pf1$FechaInicial,
                   end     = pf1$FechaFinal,
                   progress= pf1$Realizacion
                 )
                 
                 timevis(data,showZoom = TRUE,
                         options = list(editable = TRUE,width ="800px", height = "800px"))
                 
                 output$timeline <- renderTimevis({
                   timevis(data)
                 })
                 
               })
  
  
  ##### Grafico de avance
  
  
  
  
  ggplot_timeline <- ggplot(
    pf1,
    aes(
      x = FechaInicial,
      xend = FechaFinal,
      y = Formacion,
      yend = Formacion,
      color = Realizacion,
      label=Id
    ) 
  ) 
  
  ggplot_timeline
  
  ggplotly(ggplot_timeline + geom_segment(size=3) + xlab("FechaInicial") + ylab("Formacion"), color=Realizacion)
  
  gantt_labeler <- function(start_date = NA, end_date = NA, y_axis = NA, color = NA){
    paste0(
      "Formaciones: ", y_axis, "</br>",
      "Fechas: ", start_date,"<br>", "a ",end_date,
      "</br>",
      "Realizacion: ",color)}
  
  
  output$plot1 <- renderPlotly(
    ggplotly(
      ggplot(
        pf1,
        aes(
          x = FechaInicial,
          #xstart=F,
          xend = FechaFinal,
          y = Formacion,
          yend = Formacion,
          color=Realizacion,
          #colour = eval(as.name(category_column)),
          text = gantt_labeler(start_date = FechaInicial , end_date = FechaFinal, y_axis = Formacion, color = Realizacion)))
      
      + geom_segment(size = 3) + xlab("Fecha") + ylab("") , label = Id,color=Realizacion,
      tooltip = "text")
  )
  

}) 