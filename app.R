library(tidyverse)
library(shinydashboard)
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(flexdashboard)
library(RColorBrewer)
library(crosstalk)
library(kableExtra)
library(RColorBrewer)
library(htmltools)


#setwd("~/1 Especialización/1 Ciencia de los datos aplicada/Trabajo Final/FINAL/AnalisisAccidentalidad")
data2<-read.csv("accidentalidad_enriquecida2.csv",sep=",", dec=".", 
                stringsAsFactors = FALSE)

####################conteo de la Agrupacion Periodo, mes, barrio y gravedad########################
completa_gravedad <- data2 %>%
    select(PERIODO, MES,BARRIO,GRAVEDAD)%>%
    group_by(PERIODO, MES,BARRIO,GRAVEDAD)%>%
    summarise(CONTEO=length(PERIODO))
##################################################################################################

#########################Conteo por periodo, mes y barrio#########################################
completa_barrio <- completa_gravedad %>%
    select(PERIODO, MES,BARRIO, CONTEO)%>%
    group_by(PERIODO, MES,BARRIO)%>%
    summarise(TOTAL=sum(CONTEO))

##################################################################################################

##conteo de la Agrupacion Periodo, mes, barrio y gravedad y el total por periodo y por barrio#####
completa <- left_join(completa_gravedad,completa_barrio, 
                      by = c("PERIODO", "BARRIO", "MES"))

##################################################################################################

###Sobre la completa, se realiza el c?lculo del porcentaje por gravedad###########################
completa$PORCENTAJE <- completa$CONTEO/completa$TOTAL
#ordenamiento_annos<-general[order(-general$TOTAL),]
#ordenamiento_annos<-head(ordenamiento,10)
#ordenamiento_annos$BARRIO<-as.character(ordenamiento$BARRIO)


##################################################################################################

####################Cantidad de accidentes por barrio en todos los tiempos########################
general<- completa[, c("BARRIO", "CONTEO")] %>%
    select(BARRIO, CONTEO) %>%
    group_by(BARRIO) %>%
    summarise(TOTAL =sum(CONTEO))

#################################################################################################

###########Cantidad de accidentes por gravedad en cada barrio en todos los tiempos###############
general_gravedad<-data2[,c("BARRIO", "GRAVEDAD")]%>%
    select(BARRIO,GRAVEDAD)%>%
    group_by(BARRIO, GRAVEDAD)%>%
    summarise(CONTEO=length(BARRIO)) 

#################################################################################################

##################TOP 10 Barrios con m?s accidentalidad en todos los tiempos#####################
ordenamiento<-general[order(-general$TOTAL),]
ordenamiento<-head(ordenamiento,10)
ordenamiento$BARRIO<-as.character(ordenamiento$BARRIO)

################################################################################################

###################Porcentaje por gravedad accidente general###################
completa_porc_anno<- completa[, c("PERIODO","BARRIO","GRAVEDAD", "CONTEO")] %>%
    select(BARRIO,GRAVEDAD,CONTEO) %>%
    group_by(BARRIO,GRAVEDAD) %>%
    summarise(TOTAL= sum(CONTEO))

col_porcentaje_general<- completa[, c("BARRIO", "CONTEO")] %>%
    select(BARRIO,CONTEO) %>%
    group_by(BARRIO) %>%
    summarise(TOTAL= sum(CONTEO))

completa_general <- left_join(completa_porc_anno,col_porcentaje_general, 
                              by = c("BARRIO"))

completa_general$PORCENTAJE <- completa_general$TOTAL.x/completa_general$TOTAL.y

completa_general_10 <- inner_join(completa_general,ordenamiento, 
                                  by = c("BARRIO"))

completa_general_10<- completa_general_10[, c("BARRIO","GRAVEDAD", "TOTAL.x",
                                              "TOTAL.y","PORCENTAJE")]
completa_general_10<-completa_general_10[order(-completa_general_10$TOTAL.y),]


########################################################################


###############Top 10 barrios con mayor accidentalidad por gravedad#############################

general_g<-inner_join(general_gravedad,ordenamiento,by="BARRIO")
general_g<-general_g[order(-general_g$TOTAL),]

################################################################################################


cantidad_barr <- shinydashboard::valueBox(
    value = length(unique(data2$BARRIO)),
    subtitle = "BARRIOS", 
    icon = icon("map-marked-alt"), 
    width = 10,
    color='light-blue'
)

cantidad_accidentes<-nrow(data2)
total_accidentes <- shinydashboard::valueBox(
    value = paste(substr(as.character(cantidad_accidentes),1,3),'K'),
    subtitle = "ACCIDENTES", 
    icon = icon("car-crash"), 
    width = 10,
    color='light-blue'
)

promedio_dia <- shinydashboard::valueBox(
    value = format(round(cantidad_accidentes/length(unique(data2$FECHA_CORREGIDA))),
                   nsmall=0),
    subtitle = "Prom. de Accidentes/dia", 
    icon = icon("exclamation"), 
    width = 10,
    color='light-blue'
)

num_comunas <- shinydashboard::valueBox(
    value = 16,
    subtitle = "COMUNAS", 
    icon = icon("map"), 
    width = 10,
    color='light-blue'
)
#conteo_accidente
#prom_accidente_dia

#################cantidad de accidentes por periodo de tiempo en cada barrio###################
###Ac? se observa la lista de todos los barrios y la cantidad de accidentes en cada a?o########
general_per_barr<- completa[, c("BARRIO", "PERIODO", "CONTEO")] %>%
    select(BARRIO, PERIODO, CONTEO) %>%
    group_by(BARRIO, PERIODO) %>%
    summarise(TOTAL =sum(CONTEO))

###############################################################################################

####################################COORDENADAS POR BARRIO POR GRAVEDAD PARA CADA A?O#########
general_coord<- data2[,c("BARRIO","GRAVEDAD","LATITUD","LONGITUD","PERIODO",
                         "FECHA_CORREGIDA","CLASE","DIRECCION")]

#############################################################################################


completa_gravedad1 <- data2 %>%
    select(FECHA_CORREGIDA,BARRIO,GRAVEDAD)%>%
    group_by(FECHA_CORREGIDA,BARRIO,GRAVEDAD)%>%
    summarise(CONTEO=length(FECHA_CORREGIDA))


completa_barrio1 <- completa_gravedad1 %>%
    select(FECHA_CORREGIDA,BARRIO, CONTEO)%>%
    group_by(FECHA_CORREGIDA,BARRIO)%>%
    summarise(TOTAL=sum(CONTEO))

completa_barrio1$FECHA_CORREGIDA <- as.Date(( completa_barrio1$FECHA_CORREGIDA),"%Y-%m-%d")
completa_barrio1$FECHA_CORREGIDA <- format(as.Date(completa_barrio1$FECHA_CORREGIDA),"%Y-%m")




data_agrupada <- 
    group_by(completa_barrio1,FECHA_CORREGIDA) %>% 
    summarise(SUMA=sum(TOTAL))


data_clasacc <- data2 %>%
    select(CLASE) %>% 
    group_by(CLASE)%>%
    summarise(TOTAL=length(CLASE))

a <- sum(data_clasacc$TOTAL)

result <- cbind(data_clasacc, a)

result$PORCENTAJE <- result$TOTAL/result$a * 100
result <- arrange(result, desc(result$PORCENTAJE))


## Se grafica el porcentaje de clase accidente



result$CLASE <- factor(result$CLASE, levels = c("","INCENDIO", "VOLCAMIENTO","CAIDA DE OCUPANTE", "ATROPELLO", "OTRO","CHOQUE"))
result$PORCENTAJE<-round(result$PORCENTAJE,1)



## Construcción de tabla de clase accidente por año


result_PERIODO<- data2 %>%
    select(PERIODO, CLASE) %>% 
    group_by(PERIODO, CLASE)%>%
    summarise(CANTIDAD_ACCIDENTE=length(PERIODO))



result_CLASE<- data2 %>%
    select(PERIODO) %>% 
    group_by(PERIODO)%>%
    summarise(TOTAL=length(PERIODO))

### Unificar tabla de result_PERIODO, result_CLASE

data_Tot_classacc <-left_join(result_PERIODO,result_CLASE, by ="PERIODO")

### Se saca el porcentaje 

data_Tot_classacc$PORCENTAJE <- data_Tot_classacc$CANTIDAD_ACCIDENTE/data_Tot_classacc$TOTAL * 100
result2 <- arrange(data_Tot_classacc, desc(data_Tot_classacc$PORCENTAJE))

##COMUNA MAYOR ACCIDENTALIDAD
comuna_mayor_acc<- data2 %>% 
    select(COMUNA_V2) %>%
    group_by(COMUNA_V2) %>%
    summarise(TOTAL=length(COMUNA_V2))

comuna_mayor_acc <- arrange(comuna_mayor_acc, desc(comuna_mayor_acc$TOTAL))
comuna_mayor_acc<-head(comuna_mayor_acc,1)

##BARRIO MAYOR ACCIDENTALIDAD
barrio_mayor_acc<- data2 %>% 
    select(BARRIO) %>%
    group_by(BARRIO) %>%
    summarise(TOTAL=length(BARRIO))

barrio_mayor_acc <- arrange(barrio_mayor_acc, desc(barrio_mayor_acc$TOTAL))
barrio_mayor_acc<-head(barrio_mayor_acc,1)

comuna_mayor_a <- shinydashboard::valueBox(
    value = comuna_mayor_acc$COMUNA_V2,
    subtitle = "COMUNA CON MAYOR ACCIDENTALIDAD", 
    icon = icon("map-marked-alt"), 
    width = 10#, color = "red"
)

barrio_mayor_a <- shinydashboard::valueBox(
    value = barrio_mayor_acc$BARRIO,
    subtitle = "BARRIO CON MAYOR ACCIDENTALIDAD", 
    icon = icon("map-marked-alt"), 
    width = 10#, color = "red"
)

######################################### promedio de accidentes por día

N_MAESTRO_FECHAS<-read.csv("N_MAESTRO_FECHAS.csv",sep=",")

completa_conteo_accidentes <- data2 %>%
    select(PERIODO,N_DIA_LABORAL)%>%
    group_by(PERIODO,N_DIA_LABORAL)%>%
    summarise(CONTEO=length(PERIODO))

N_MAESTRO_FECHAS$PERIODO <- as.character(N_MAESTRO_FECHAS$N_FECHAS)
N_MAESTRO_FECHAS$PERIODO <-substr(N_MAESTRO_FECHAS$PERIODO,1,4)
N_MAESTRO_FECHAS$PERIODO <-as.numeric(N_MAESTRO_FECHAS$PERIODO)

completa_conteo_tipo_dia <- N_MAESTRO_FECHAS %>%
    select(PERIODO,N_DIA_LABORAL)%>%
    group_by(PERIODO,N_DIA_LABORAL)%>%
    summarise(CONTEO=length(PERIODO))

completa_promedio_acc_tipo_dia <- left_join(completa_conteo_accidentes,completa_conteo_tipo_dia,by=c("PERIODO","N_DIA_LABORAL"))

completa_promedio_acc_tipo_dia$promedio_acc_dia <-round( completa_promedio_acc_tipo_dia$CONTEO.x/completa_promedio_acc_tipo_dia$CONTEO.y,0)

##########################################

completa_conteo_accidentes_f_impo <- data2 %>%
    select(Fechas.importantes)%>%
    group_by(Fechas.importantes)%>%
    summarise(CONTEO=length(Fechas.importantes))

completa_conteo_accidentes_f_impo<-na.omit(completa_conteo_accidentes_f_impo)

completa_conteo_accidentes_f_impo<-completa_conteo_accidentes_f_impo[order(-completa_conteo_accidentes_f_impo$CONTEO),]

completa_conteo_accidentes_f_impo<-head(completa_conteo_accidentes_f_impo,10)

###///////////////////Análisis por comuna////////////////////////
accidentalidad <- read.csv("accidentalidad_enriquecida2.csv", header=TRUE, sep=",")

accidentalidad_comuna <- subset(accidentalidad, subset=(COD_COMUNA_V2 %in% c(1:16)))

# Variables de entrada
anno <- unique(accidentalidad_comuna$PERIODO)
accidentalidad_comuna$COMUNA_V2<-accidentalidad_comuna$COMUNA_V2<-factor(accidentalidad_comuna$COMUNA_V2)
comuna <- unique(accidentalidad_comuna$COMUNA_V2)

clase <- as.character(accidentalidad_comuna$CLASE)
clase <- unique(clase)

boton_anno_comuna<-selectInput("anno", label="Seleccione un año", choices = anno)

boton_comuna_comuna<-selectInput("comuna", label="Seleccione una comuna", choices = comuna)
boton_clase_comuna<-selectInput("clase", label="Clase de accidente", choices=clase)

botones_comuna<-box(title="Filtros de selección:",
                    boton_anno_comuna,boton_comuna_comuna,
                    solidHeader = TRUE,width = 3)

##############//////////////

gauge_gravedad<-data2[,c("BARRIO","GRAVEDAD")] %>% 
    select(GRAVEDAD)%>%
    group_by(GRAVEDAD)%>%
    summarise(CONTEO=length(GRAVEDAD))

gauge_gravedad$TOTAL<-209426
gauge_gravedad$PORCENTAJE<- round(((gauge_gravedad$CONTEO/gauge_gravedad$TOTAL)*100),1)
g_gravedad_herido<-subset(gauge_gravedad,subset = (gauge_gravedad$GRAVEDAD=="Herido"))
g_gravedad_Muerto<-subset(gauge_gravedad,subset = (gauge_gravedad$GRAVEDAD=="Muerto"))
g_gravedad_SoloDanos<-subset(gauge_gravedad,subset = (gauge_gravedad$GRAVEDAD=="Solo daños"))

porcentaje1<-infoBox("Herido",paste(as.character(g_gravedad_herido$PORCENTAJE),"%"),width = 30,
                     color="yellow")
porcentaje2<-infoBox("Solo daños",paste(as.character(g_gravedad_SoloDanos$PORCENTAJE),"%"),
                     width = 30,color="green")
porcentaje3<-infoBox("Muerto",paste(as.character(g_gravedad_Muerto$PORCENTAJE),"%"),width = 30,
                     color="red")


porcentaje_clase_acc<-flowLayout(h4(align = "center",strong("GRAVEDAD DE ACCIDENTE")),
                                 porcentaje2,porcentaje1,porcentaje3)


header <- dashboardHeader(
    title = HTML(paste(icon("car"), "Accidentalidad"))
)

sidebar <- dashboardSidebar(
    sidebarMenu(id = "tabs", 
                menuItem("Información General",
                         tabName = "general", 
                         icon = icon("info"),selected = TRUE
                ),
                menuItem("Comunas",
                         tabName = "comuna", 
                         icon = icon("map-marker-alt"),startExpanded = TRUE,
                         menuItem("Información por año",
                                  tabName="comuna_poranio",
                                    icon=icon("map-pin"))
                         
                ),
                menuItem("Realizado por",
                         tabName = "realizado", 
                         icon = icon("smile-beam")
                         
                )
    )
)
chart.box.1 <- box(
                   status = "primary", 
                    plotlyOutput("general_stacked",height = 500),width=12)

chart.box.2 <- box(title = "Porcentaje por clase de accidente",
                   status = "primary", 
                   plotlyOutput("clase_acc",height = 300),width=9)

chart.box.3 <- box(title = "Promedio de accidentes por día",
                   status = "primary", 
                   plotlyOutput("prom_acc_dia",height = 300))

chart.box.4 <- box(title = "Promedio de accidentes en días festivos",
                   status = "primary", 
                   plotlyOutput("dia_festivo",height = 300))

chart.box.5 <- column(plotlyOutput("barplot_la"),width=12)

chart.box.6 <- box(
                   status = "primary", 
                   plotlyOutput("barPlot2",height = 450))

chart.box.7 <- box(
                   status = "primary", 
                   plotlyOutput("series_la",height = 375),boton_clase_comuna)

 integrante1<-box( status = "primary","Leidy Milena Castaño Hernández")                 


boton1<-selectInput(inputId = "top10barr",label="Barrio",
                    choices=unique(ordenamiento$BARRIO),multiple = FALSE)
boton2<-selectInput(inputId = "top10grav",label="Comuna",
                    choices=unique(general_coord$GRAVEDAD),multiple = FALSE)
botones<-box(title="Filtros de selección:",boton1,boton2,
             solidHeader = TRUE,width = 3)
mapa<-leafletOutput("map")


boton3<-selectInput(inputId = "top10barr1",label="Seleccione un barrio",
                    choices=unique(ordenamiento$BARRIO),multiple = FALSE)


##organizacion
body <- dashboardBody(
    tabItems(
        tabItem(tabName = "general",
                fluidRow(h1("ANÁLISIS DE ACCIDENTALIDAD EN MEDELLÍN", align="center")),
                splitLayout(num_comunas,cantidad_barr,total_accidentes,promedio_dia),
                fluidRow(h2("INFORMACIÓN POR CLASE Y GRAVEDAD DEL ACCIDENTE", align="center")),
                #(splitLayout(boton1,boton2)),
                fluidRow(chart.box.2,porcentaje_clase_acc),
                fluidRow(h2("TOP 10 BARRIOS CON MAYOR ACCIDENTALIDAD", align="center")),
                fluidRow(chart.box.1),
                fluidRow(h2("UBICACIÓN GEOGRÁFICA - TOP 10 BARRIOS CON MAYOR ACCIDENTALIDAD", align="center")),
                fluidRow(botones,column(wellPanel(mapa),width=9)),
                #fluidRow(gaugeti),
                fluidRow(h2("SERIE TEMPORAL POR CANTIDAD DE ACCIDENTES", align="center")),
                fluidRow(column(plotlyOutput("serie"),width=12)),
                fluidRow(h2("CANTIDAD DE ACCIDENTES EN DÍAS DE FIESTA Y FINES DE SEMANA", 
                            align="center")),
                fluidRow(chart.box.3, chart.box.4)
                ),
        
                tabItem(tabName = "comuna_general"),
                tabItem(tabName = "comuna_poranio",
                splitLayout(num_comunas,cantidad_barr,total_accidentes,promedio_dia),
                fluidRow(h2("COMUNAS DE MEDELLÍN", align="center")),
                fluidRow(column(wellPanel(plotlyOutput("barplot_la")),width=12)),
                fluidRow(h2("ANÁLISIS POR COMUNA", align="center")),
                fluidRow(botones_comuna,column(wellPanel(plotlyOutput("barPlot3")),width=9)),
                fluidRow(h2("INFORMACIÓN SOBRE LA CLASE Y LA CANTIDAD DE ACCIDENTES", align="center")),
                fluidRow(chart.box.6, chart.box.7),
                fluidRow(h2("UBICACIÓN GEOGRÁFICA", align="center")),
                fluidRow(column(wellPanel(leafletOutput("mapa_la")),width=12)),
                fluidRow(h2("GRAVEDAD SEGÚN CLASE DE ACCIDENTE", align="center")),
                fluidRow(column(wellPanel(plotOutput("intensity")),width=12))
                ),
        tabItem(tabName = "realizado",
                fluidRow(h2("CIENCIA DE LOS DATOS", align="center")),
                fluidRow(h4("UNIVERSIDAD NACIONAL DE COLOMBIA - SEDE MEDELLÍN", align="center")),
                fluidRow(h3("  Maria Victoria Valencia Arango")),
                fluidRow(h5("   mvalenciaar@unal.edu.co")),
                fluidRow(h3("  Gustavo Andrés Gómez Higuita")),
                fluidRow(h5("   gagomezh@unal.edu.co")),
                fluidRow(h3("  Luisa Fernanda Ríos Piedrahita")),
                fluidRow(h5("   lfriosp@unal.edu.co")),
                fluidRow(h3("  Janick Alberto Reales Salas")),
                fluidRow(h5("   jarealess@unal.edu.co")),
               fluidRow(h3("  Leidy Milena Castaño Hernández")),
               fluidRow(h5("   lmcastanoh@unal.edu.co"))
                
                )
    )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
    
    output$general_stacked <- renderPlotly({
      
      general_gravedad<-data2[,c("BARRIO", "GRAVEDAD")]%>%
        select(BARRIO,GRAVEDAD)%>%
        group_by(BARRIO, GRAVEDAD)%>%
        summarise(CONTEO=length(BARRIO)) 
      
      ordenamiento<-general[order(-general$TOTAL),]
      ordenamiento<-head(ordenamiento,10)
      ordenamiento$BARRIO<-as.character(ordenamiento$BARRIO)
      
      general_g<-inner_join(general_gravedad,ordenamiento,by="BARRIO")
      general_g<-general_g[order(-general_g$TOTAL),]
      
      
        plot_ly(x = general_g$BARRIO, y = general_g$CONTEO, type = 'bar', 
                name = general_g$GRAVEDAD,text=general_g$TOTAL,
                textposition = 'bottom center'#color = general_g$GRAVEDAD
               
                
        ) %>%
            layout(yaxis = list(title = 'Cantidad'), 
                   xaxis=list(categoryorder='array',
                              categoryarray=general_g$TOTAL),
                   barmode = 'stack',
                   colorway = c('#BF3030', '#800000', '#FE6F5E')
                   ) %>% config(modeBarButtons=list(list(
                                    "resetScale2d")),displaylogo = FALSE)
      
        
    })
    
    output$map <- renderLeaflet({ 
        
        general_coord$LATITUD<-as.numeric(general_coord$LATITUD)
        general_coord$LONGITUD<-as.numeric(general_coord$LONGITUD)
        general_coord1<-subset(general_coord,subset=(BARRIO==input$top10barr & 
                                                         GRAVEDAD==input$top10grav))
        lati<-general_coord1$LATITUD
        longi<-general_coord1$LONGITUD
        mapa<-leaflet()
        mapa<-addProviderTiles(mapa,provider="OpenStreetMap.Mapnik")
        mapa<-fitBounds(mapa,lng1 = min(longi),lng2 =max(longi) ,
                        lat1 = min(lati),lat2 =max(lati) )
        
        icons <- awesomeIcons(icon = 'ios-close', iconColor = 'white', library ='ion')

        mapa<-addAwesomeMarkers(mapa,lat=lati,lng=longi,clusterOptions = markerClusterOptions(),
                                icon=icons)

    }) 
    
   
    
    output$serie<-renderPlotly({
        
        grafico_TS <- plot_ly(
            type = "scatter",  data_agrupada,
            x = ~data_agrupada$FECHA_CORREGIDA,
            y = ~data_agrupada$SUMA,
            name = "Accidentalidad en Medellin",
            mode = "lines+markers",
            line = list(
                color = '#BF3030'
            )) %>%
            
            layout(
                #title = "Serie de tiempo de accidentalidad en Medellin",
                yaxis=list(title="Cantidad de accidentes"),
                xaxis=list(title=""),colorway = c('#000000')
            )%>% config(displayModeBar = FALSE)
    })
    
    
    output$clase_acc<-renderPlotly({
      
       plot_ly (result, x = ~PORCENTAJE, y = ~CLASE, type = 'bar', 
                       orientation = 'h',
                       text=paste(as.character(result$PORCENTAJE),"%"),
                       textposition = 'auto') %>% 
        layout(xaxis=list(title="Porcentaje de clase de accidentes"), 
               #yaxis=list(title="Clase de accidentes"),
               colorway = c('#BF3030', '#800000', '#FE6F5E'))%>% 
        config(modeBarButtons=list(list(
                   "resetScale2d")),displaylogo = FALSE)
    })
    
    
    output$prom_acc_dia<-renderPlotly({   
    plot_ly(completa_promedio_acc_tipo_dia, x = ~PERIODO, 
            y=~promedio_acc_dia, color = ~N_DIA_LABORAL, 
            type = "bar",text=~promedio_acc_dia,textposition = 'auto'
            
            ) %>% 
        layout(title="",yaxis=list(title="Cantidad"))%>% config(modeBarButtons=list(list(
            "resetScale2d")),displaylogo = FALSE)

    
    })
    
    
    output$dia_festivo<-renderPlotly({   
        plot_ly(completa_conteo_accidentes_f_impo, x = ~CONTEO, 
                y=~Fechas.importantes, type = "bar",text=~CONTEO, 
                textposition = 'auto') %>% 
            layout(yaxis = list(title = "", 
                                categoryorder="array",
                                categoryarray = ~CONTEO),
                   xaxis=list(title="Cantidad"),
                   colorway = c('#FE6F5E', '#374278', '#7f1416', 
                                '#424242'))%>% config(modeBarButtons=list(list(
                                    "resetScale2d")),displaylogo = FALSE)
    })
    
    
    #######################análisis comuna
    
    output$barplot_la <- renderPlotly({
        filtro <- subset(accidentalidad_comuna, subset=(PERIODO==input$anno))
        
        contAcc_comuna <- aggregate(ID~COMUNA_V2, data = filtro, FUN =length)
        total <- length(filtro$PERIODO) 
        
        
        contAcc_comuna$value <- (contAcc_comuna$ID/total)*100
        contAcc_comuna$value<-round(contAcc_comuna$value,1)
        
        ordenado <- sort(contAcc_comuna$value, decreasing=TRUE, index.return=TRUE)
        
        contAcc_comuna$value <- sort(contAcc_comuna$value, decreasing=TRUE, index.return=FALSE)
        
        contAcc_comuna$COMUNA_V2 <- contAcc_comuna$COMUNA_V2[ordenado$ix]
        
        plot_ly(x = contAcc_comuna$COMUNA_V2, y = contAcc_comuna$value, type = 'bar',
                text= paste(as.character(contAcc_comuna$value),"%"), textposition = 'outside') %>%
            layout(title="PORCENTAJE DE ACCIDENTALIDAD POR COMUNA",
                   yaxis = list(title = 'Cantidad'), 
                   xaxis=list(categoryorder='array',
                              categoryarray=contAcc_comuna$value),
                   barmode = 'bar',
                   colorway = c('#7f1416', '#7982ce', '#ff975b', 
                                '#a262a9', '#6f4d96', '#3d3b72', '#182844')
            ) %>% config(modeBarButtons=list(list(
                "resetScale2d")),displaylogo = FALSE)
        
        
    })
    
    # CANTIDAD DE ACCIDENTES SEGÚN EL TIPO DE ACCIDENTE Y LA CLASE
    output$intensity <- renderPlot({
        
        filtro <- subset(accidentalidad_comuna, subset=(COMUNA_V2==input$comuna))
        total_comuna <- length(filtro$ID)
        
        # -----> Tener cuidado con la categoría que aparece vacía <-----
        
        comuna_gc <- table(filtro$GRAVEDAD, filtro$CLASE)
        comuna_gc1 <- (round((comuna_gc/total_comuna)*100, 2))
        
        
        color_image <- hsv(h = 1, s = seq(0, 1, length.out = 10), v = 1, alpha = 0.8) # Escala de color
        
        image(t(comuna_gc1[3:1,]), xaxt = 'n', yaxt = 'n', xlab = "Clase accidente", ylab = "Gravedad",  col = color_image)
        axis(1, at = seq(0, 1, by = 1/6), labels = c("Atropello",
                                                     "Caida ocupante",
                                                     "Choque", 
                                                     "Incendio", 
                                                     "Volcamiento", 
                                                     "Otro", ""), cex.axis = 0.6)
        axis(2, at = seq(0, 1, by = 1/2), labels = c("Muerto",
                                                     "Herido",
                                                     "Solo daños"), las = 1, cex.axis = 0.6)
        #title(main = "Gravedad segun clase de accidente", font.main = 3)
        
        
        e <- expand.grid(seq(0,1, length = 7), seq(1,0, length = 3))
        text(e, labels = paste(t(comuna_gc1), "%"), cex = 0.7)
        
    })
    
    
    # PORCENTAJE DE ACCIDENTALIDAD DE ACUERDO AL TIPO DE ACCIDENTE
    
    output$barPlot2 <-renderPlotly({ 
        # Subconjunto con la comuna de interés
        filtro <- subset(accidentalidad_comuna, subset=(COMUNA_V2==input$comuna))
        
        # Número total de accidentes en la comuna
        total_comuna <- length(filtro$ID)
        
        # Número total de accidentes por año en la comuna
        anno_comuna <-  aggregate(ID~CLASE, data = filtro, FUN =length)
        
        # Adicionar variable con la proporción de accidentes calculada por año
        anno_comuna$PROPORCION <- round((anno_comuna$ID/total_comuna*100),2)
        
        anno_comuna$CLASE <- as.factor(anno_comuna$CLASE)
        
        anno_comuna<-anno_comuna[order(-anno_comuna$PROPORCION),]
        
        plot_ly(x = anno_comuna$CLASE, 
                y = anno_comuna$PROPORCION, 
                type = 'bar',text=paste(as.character(anno_comuna$PROPORCION),"%"), 
                textposition = 'outside'
                
                
        ) %>%
            layout(title="Porcentaje por clase de accidente",yaxis = list(title = 'Porcentaje'), 
                   xaxis=list(categoryorder='array',
                              categoryarray=anno_comuna$PROPORCION),
                   barmode = 'bar',
                   colorway = c('#BF3030', '#800000', '#FE6F5E')
            ) %>% config(modeBarButtons=list(list(
                "resetScale2d")),displaylogo = FALSE)
        
        
        
    })
    

    # CÁLCULO DEL PORCENTAJE DE CADA TIPO DE ACCIDENTE PARA CADA MES (SERIE DE TIEMPO) DENTRO DE LA COMUNA
    
    output$series_la <- renderPlotly({
        
        filtro <- subset(accidentalidad_comuna, subset=(PERIODO==input$anno & COMUNA_V2==input$comuna))
        clases <- unique(filtro$CLASE)
        
        #Total de accidentes de la comuna para un mes en un año específico
        total_acc_mes <- aggregate(ID~MES, data=filtro, FUN=length)
        total_acc <- total_acc_mes$ID
        
        # Porcentaje de cada clase de accidente dentro del total de accidentes del mes
        filtro_sub <- subset(filtro, subset=(CLASE==input$clase))
        contAcc_mes <- aggregate(ID ~ MES, data=filtro_sub, FUN=length)
        y1 <- contAcc_mes$ID
        y1 <- y1/total_acc
        y1 <- y1*100          
        
        
        x <- c("Enero", "Febrero", "Marzo", 
               "Abril", "Mayo", "Junio", "Julio",
               "Agosto", "Septiembre", "Octubre",
               "Noviembre", "Diciembre")
        data <- data.frame(x, y1)
        data$x <- factor(data$x, levels = data[["x"]])
        
        if(nrow(data)!=0){
                plot_ly(data, x=~x, y=~y1, type = 'scatter', name=as.factor(input$clase), mode = 'lines+markers')%>%
            layout(title = "Serie de tiempo por tipo de accidente",
                   xaxis = list(title = ""),
                   yaxis = list (title = "Porcentaje"),
                   colorway = c('#800000', '#FE6F5E'))%>% 
                config(modeBarButtons=list(list(
                       "resetScale2d")),displaylogo = FALSE)
            }

        
    })
    
    
    # PARETO PORCENTAJE DE ACCIDENTALIDAD DE CADA BARRIO DENTRO DE LA COMUNA
    
    output$barPlot3 <-renderPlotly({ 
        # Subconjunto con la comuna y año de interés
        filtro <- subset(accidentalidad_comuna, subset=(PERIODO==input$anno & COMUNA_V2==input$comuna))
        
        # Conteo de accidentes para cada barrio de la comuna    
        contAcc_barrio <- aggregate(ID~BARRIO, data = filtro, FUN =length)
        
        # Total de accidentes ocurridos en la comuna
        total <- length(filtro$PERIODO)
        
        # Porcentaje de accidentes por barrio
        contAcc_barrio$value <- (contAcc_barrio$ID/total)*100
        
        # Accidentalidas por barrios ordenada
        ordenado <- sort(contAcc_barrio$value, decreasing=TRUE, index.return=TRUE)
        contAcc_barrio$value <- sort(contAcc_barrio$value, decreasing=TRUE, index.return=FALSE)
        
        contAcc_barrio$BARRIO <- contAcc_barrio$BARRIO[ordenado$ix]
        
        ymax <- max(contAcc_barrio$value)
        contAcc_barrio$value<-round(contAcc_barrio$value,1)
        
        plot_ly(x = contAcc_barrio$BARRIO, 
                y = contAcc_barrio$value, 
                type = 'bar',text=paste(as.character(contAcc_barrio$value),"%"), 
                textposition = 'outside'
                
                
        ) %>%
            layout(title=paste("BARRIOS DE LA COMUNA",toupper(input$comuna)),yaxis = list(title = 'Cantidad'), 
                   xaxis=list(categoryorder='array',
                              categoryarray=contAcc_barrio$value),
                   barmode = 'bar',
                   colorway = c('#abc1df', '#374278', '#7f1416', 
                                '#424242')
            ) %>% config(modeBarButtons=list(list(
                "resetScale2d")),displaylogo = FALSE)
    })
    
    # MAPA DE ACCIDENTALIDAD DENTRO DE LA COMUNA DE ACUERDO AL TIPO DE ACCIDENTE
    
    output$mapa_la <- renderLeaflet({
        
        filtro <- subset(accidentalidad, subset=(PERIODO == input$anno & CLASE==input$clase & COMUNA_V2==input$comuna), select=c("LATITUD", "LONGITUD"))
        
        
        latitud_min_la<- min(filtro$LATITUD)
        latitud_max_la <- max(filtro$LATITUD)
        longitud_min_la <- min(filtro$LONGITUD)
        longitud_max_la <- max(filtro$LONGITUD)
        
        
        icons <- awesomeIcons(icon = 'ios-close', iconColor = 'white', library ='ion')
        # filtro$f_accidente <- as.character(filtro$f_accidente)
        
        map_la <- leaflet()
        map_la<-addProviderTiles(map_la, provider="OpenStreetMap.Mapnik")
        map_la<-fitBounds(map_la,lng1=longitud_min_la, lat1=latitud_min_la, lng2=longitud_min_la, lat2=latitud_max_la)
        map_la<-addAwesomeMarkers(map_la, lat=filtro$LATITUD, lng=filtro$LONGITUD, 
                                  label=filtro$f_accidente, icon=icons, 
                                  clusterOptions=markerClusterOptions())
        
        
    })

    
}

shinyApp(ui = ui, server = server)