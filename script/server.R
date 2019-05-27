##################################################################################
# Crea aplicativo para visualizar resultdos preprocesing
#Input: datos agregados  de Uber. Datos del momento y la localización
#       de el evento de recogida de Uber, en la ciudad de Nueva York en los meses  de 
#       julio a septiembre de 2014.(3 trimestre)


# # Libraries

library(ggmap)# Para pintar mapas bajo capas de google
library(dplyr)# Manejo de bases de datos
library(VIM) # para identificar missing
library(lubridate)
library(leaflet) # For maps with ESRI
library(circlize) # for create circle diagramas
library(tidyverse) # for create circle diagramas
#devtools::install_github("mattflor/chorddiag")
library(chorddiag)






library(shiny)
library(leaflet)

shinyServer(function(input, output) {
  
  
  
  #Define colores--------------------------------------------------------------
  colors <- c("blue","red","green","purple", "greenyellow")
  names(colors) <- c("B02512", "B02598", "B02617", "B02682", "B02764")
  colors <- data.frame(colors)
  


    
    output$map <- renderLeaflet({
      
#Leer datos------------------------------------------------------------------------
      
      
      dat <- input$Archivo
      dataResum <- read.delim(dat$datapath, header=T, sep="\t", dec=".")
      
      #dataResum <-read.delim("C:/Users/LENOVO/Documents/ANDES/Tesis1/Aplicativo/Preprocessing/output/aggregate_uber_trip_data.txt", sep="\t", dec=".")
      dataResum["Month"] <- as.character(dataResum[,"Month"])
      dataResum["Day"] <- as.character(dataResum[,"Day"])
      
      mesIN <- input$Mes
      BaseIn <-input$base
      DiaIn <-input$dia
     
      
#Agregar  datos------------------------------------------------------------------------
      
      dataTODO <- group_by(dataResum,Cluster) %>%
        summarise(Lat = min(Lat), Lon = min(Lon),
                  countPick = sum(countPick),
                  sumPass =sum(sumPass))
      
      dataTODOB <- group_by(dataResum,Cluster,Month,Day) %>%
        summarise(Lat = min(Lat), Lon = min(Lon),
                  countPick = sum(countPick),
                  sumPass =sum(sumPass))
      
      dataTODOBD <- group_by(dataResum,Cluster,Month) %>%
        summarise(Lat = min(Lat), Lon = min(Lon),
                  countPick = sum(countPick),
                  sumPass =sum(sumPass))
      
      dataTODOMD <- group_by(dataResum,Cluster,Base) %>%
        summarise(Lat = min(Lat), Lon = min(Lon),
                  countPick = sum(countPick),
                  sumPass =sum(sumPass))
      
      dataTODOD <- group_by(dataResum,Cluster,Base,Month) %>%
        summarise(Lat = min(Lat), Lon = min(Lon),
                  countPick = sum(countPick),
                  sumPass =sum(sumPass))
      
      
 #Filtra datos------------------------------------------------------------------------
      
      if(mesIN =="TODOS" & BaseIn == "TODOS" & DiaIn=="TODOS"){
        data <- dataTODO
        } else {
          if(BaseIn =="TODOS" & mesIN != "TODOS" & DiaIn !="TODOS"){
             data <- subset(dataTODOB, dataTODOB$Month ==mesIN  & dataTODOB$Day==DiaIn)
          } else {
            if(BaseIn =="TODOS" & mesIN != "TODOS" & DiaIn =="TODOS"){
              data <- subset(dataTODOBD, dataTODOBD$Month ==mesIN)
            } else {
              if(BaseIn !="TODOS" & mesIN == "TODOS" & DiaIn =="TODOS"){
                data <- subset(dataTODOMD, dataTODOMD$Base ==BaseIn)
                
              } else {
                if(BaseIn !="TODOS" & mesIN != "TODOS" & DiaIn =="TODOS"){
                  data <- subset(dataTODOD, dataTODOD$Base ==BaseIn & dataTODOD$Month ==mesIN)
                  } else {
                    data <- subset(dataResum, dataResum$Base == BaseIn & dataResum$Month == mesIN & dataResum$Day == DiaIn)
                  }
              }
            }
          }
        }
      
      
#Asigna Colores-----------------------------------------   
      if(input$base != "TODOS") {
        colorB <- as.character(colors[input$base,1])
      } else {
        colorB <- "violet"
      }      
      
#Genera gráfico------------------------------------------------------------------------
      
      if(input$var =="Pickups"){
        maxi<- max(data$countPick)
        leaflet(data = data) %>% addTiles() %>%
          addCircleMarkers(~ Lon, ~Lat, popup = ~as.character(countPick),
                           label = ~as.character(countPick),
                           weight = 1,
                           radius = (data$countPick/maxi) * 20,
                           color = colorB,
                           stroke = TRUE, fillOpacity = 0.6)
      } else {
        
          maxi<- max(data$sumPass)
          leaflet(data = data) %>% addTiles() %>%
            addCircleMarkers(~ Lon, ~Lat, popup = ~as.character(sumPass),
                             label = ~as.character(sumPass),
                             weight = 1,
                             radius = (data$sumPass/maxi) * 20,
                             color = colorB,
                             stroke = TRUE, fillOpacity = 0.6)
          }
      })
    
    output$distPlot <- renderChorddiag({
      dat <- input$matriz
      df <- read.delim(dat$datapath, header=T, sep="\t", dec=".")
      df = as.matrix(df)
      row.names(df) = c(colnames(df))
      df
      
      chorddiag(df, showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 90)
      
    }
    )
      
    
})


    
    
    
    
    
    
#shinyApp(ui = ui, server = shinyServer)
    
    
    
    
    
    
    
    