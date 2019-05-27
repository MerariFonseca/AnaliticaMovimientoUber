
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com

library(shiny)# For page web
library(ggplot2)
library(ggmap)# Para pintar mapas bajo capas de google
library(dplyr)# Manejo de bases de datos
library(VIM) # para identificar missing
library(lubridate)
library(leaflet) # For maps with ESRI
library(circlize) # for create circle diagramas
library(tidyverse) # for create circle diagramas
#devtools::install_github("mattflor/chorddiag")
library(chorddiag)


Bas <- read.delim("Bases.txt", header= F)
Bases <- as.character(levels(Bas[,1]))



Mesi <- read.delim("Meses.txt", header= F)
Meses <- as.character(Mesi[,1])

Variables <- c("Pickups", "Numero de Pasajeros")
Clusters <- c(5,10,15,12)


dias <- seq(from = 1, to = 31, by = 1)
dias[32] ="TODOS"

shinyUI(fluidPage(
  
  
  # Application title
  titlePanel("Análisis de Datos de Movimiento Uber"),
  
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Identificación de lugares importantes",
                         fluidRow(
                           leafletOutput("map"),
                           p(),
                           br(),
                           fileInput("Archivo", label = h3("Archivo de análisis")),
                           div("Seleccione la empresa, el mes y el dia que desea analizar"),
                           selectInput('base', 'Empresa', Bases),
                           selectInput('Mes', 'Meses', Meses),
                           selectInput('dia', 'Dia', dias),
                           selectInput('var', 'Variable', Variables),
                           selectInput('clus', 'Número Cluster', Clusters),
                           code("Universidad de los Andes")
                           )
                         ),
                
                tabPanel("Origen - Destino",
                         fluidRow(
                           chorddiagOutput("distPlot", height = 600),
                           p(),
                           br(),
                           fileInput("matriz", label = h3("Archivo de análisis")),
                           div("Seleccione el periodo  que desea analizar"),
                           selectInput('Mes', 'Meses', Meses),selectInput('dia', 'Dia', dias),
                           selectInput('var', 'Variable', Variables),
                           selectInput('clus', 'Número Cluster', Clusters),
                           code("Universidad de los Andes")
                           )
                         ),
            
                

                tabPanel("Co - Ocurrencia",
                         fluidRow(
                           
                           p()
                         )),
                tabPanel("Identificación de Patrones",
                         fluidRow(
                           
                           p()
                         ))
                )
    )
  )
)

