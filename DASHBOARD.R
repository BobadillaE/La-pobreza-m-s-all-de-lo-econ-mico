library(rjson)
library(leaflet)
library(leaflet.extras)
library(multinet)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(shiny)
library(shinydashboard)



### DATASETS
datosIngreso <- DistIngreso20
datosDesigualdad <- Libro2
d1 <- Pueblos_AF
d2 <- Pueblos_BF
d3 <- Pueblos_MF
d4 <- PIB_ARRIBA200
d5 <- PIB_ARRIBA100
d6 <- PIB_ABAJO100
estados <- fromJSON(file = "C:/Users/Emiliano Bobadilla/Downloads/formatted4.json")
vertices1 <- actorsLayers
edges1 <- edgesIndicadores
indSoc <- indicadoresSociales
indEco <- indicadoresEco
Relacion_Pib_PoblacionIndigena <- RelacionPibInd
Pib_Per_Capita <- Pibpc
Poblacion_Indigena <- pobInd1
l2 <- l2

misDatasets <- c("Pib_Per_Capita","Poblacion_Indigena","Relacion_Pib_PoblacionIndigena")
net <- ml_empty()
capas <- c("RezagoEdu", "AguaDiaria", "AccesoHospital","SinAfiliacion","Vivienda","SinAlimentos","PibPC")
add_layers_ml(net,capas)
add_vertices_ml(net,vertices1)
net
add_edges_ml(net, edges1)
net
l1 <- layout_multiforce_ml(net, w_inter = 0,gravity = 1)

ui <- dashboardPage(skin="green",
  dashboardHeader(title="Pobreza en México"),
  dashboardSidebar(
      sidebarMenu(
      menuItem("Introducción", tabName = "intro", icon = icon("dashboard")),
      menuItem("Análisis geográfico económico", tabName = "geoEco", icon = icon("dashboard")),
      menuItem("Análisis geográfico social", tabName = "geoSoc", icon = icon("dashboard")),
      menuItem("Conclusiones y propuesta", tabName = "conc", icon = icon("dashboard")),
      menuItem("Referencias", tabName = "ref", icon = icon("dashboard"))
      
    )
  ),
  dashboardBody(
   tabItems(
     tabItem("intro",
               center=TRUE,
               h1("La pobreza en México más allá de lo económico"),
               p("En la actualidad México sufre de un elevado número de carencias, pero ninguna de ellas se compara con la profunda crisis de pobreza que se ha experimentado en la época contemporánea. La medición oficial de la pobreza en el país la realiza el Consejo Nacional de Evaluación de la Política de Desarrollo Social (Coneval), y según su estudio más reciente el 43.9% de la población mexicana vive en pobreza, y el 8.5% de la población, es decir, casi 6.6 millones de personas viven en pobreza extrema.  (Coneval, 2022)"),
              
               h3("Una definición multidimensional"),
               p("El problema de la pobreza va más allá de lo económico, y es por eso que, México, es el primer país en el mundo en tener una medición que utiliza un sistema multidimensional, así, la pobreza que padecen las personas no solo se refleja en el ingreso, sino también en la carencia de derechos sociales que debe otorgar el Estado mexicano a sus ciudadanos. Por una parte, la dimensión del bienestar económico comprende la capacidad adquisitiva de bienes y servicios asociados a las necesidades de la población mediante el ingreso, y, por otra parte, la dimensión social, incluye 6 indicadores de acceso a: servicios de salud, seguridad social, servicios básicos, calidad y espacios de la vivienda, educación y alimentación."),
             p("Según la definición multidimensional anterior, una persona que tiene al menos una carencia de acceso a cualquiera de los 6 derechos sociales, y no cuenta con los ingresos suficientes para cubrir sus necesidades, se encuentra en situación de pobreza. Como ya se mencionó anteriormente, el 43.9% de la población vive en situación de pobreza, esto quiere decir, que se combinan ambos aspectos (económico y social), sin embargo, casi el 70% de la población no tiene garantizado el ejercicio de al menos uno de sus derechos para el desarrollo social. Esta situación es alarmante, pues significa que el Estado y las instituciones fallan en brindar derechos humanos básicos a la gran mayoría del país para lograr tener un nivel de vida digno."),
             p("Las personas que viven en pobreza cuentan con un promedio de 2.4 carencias, y aquellas que se encuentran en situación de pobreza extrema registran 3.6 carencias promedio. Esto es completamente relevante, ya que vemos que las carencias no vienen solas, sino que en la mayoría de los casos son interdependientes. En México el 19.2% de la población califica en la categoría de rezago educativo, el 28.2% tiene carencia por acceso a servicios de salud, el 52% tiene carencia por acceso a la seguridad social, el 9.3% cuenta con carencia por calidad y espacios de vivienda, el 17.9% tiene carencia por acceso a los servicios básicos en la vivienda y el 22.5% tiene carencia por acceso a la alimentación nutritiva y de calidad."),
             h3("Indicadores Sociales"),
             plotOutput("plot1"),
             h3("Indicadores Económicos"),
             plotOutput("plot2"),
             h2("Pobreza significa desigualdad"),
             p("La pobreza viene de la mano con la desigauldad, son interdependientes. Con una simple mirada a la distribución de la riqueza y los porcentajes de la población que reciben dicha distribución, se vuelve claro porque el país se encuentra en tal siruación."),
              sliderInput(inputId = "slider1", label = "Elige un valor para los años mostrados",
                         value = 2010, min = 2000, max = 2020),
             plotOutput("plot3"),
             sliderInput(inputId = "slider2", label = "Elige un valor para los porcentajes de la población mostrados",
                         value = 0, min = 0, max = 100),
             plotOutput("plot4")
    ),
    tabItem("geoEco",
            h1("Análisis geográfico de indicadores económicos"),
            p("Uno de los problemas principales, y que mencionan algunos autores como Hernández Licona en su texto El desarrollo económico en México, es que México es un país con tendencias muy centralizadoras, esto es que la infraestructura del país esta sesgada hacia el centro, y en parte hacia el norte del país. De igual manera Bernardo García menciona: “El dominio indiscutible de la Ciudad de México sobre el resto del país se muestra con su historia. […] La ciudad es el eje de las referencias espaciales, […] y también el punto de confluencia de las principales redes de comunicación”. (2014)"),
            p("Podemos apreciar que en efecto el problema de la distribución de la riqueza en México tiene una relación muy directa con la distribución geográfica del país. Un simple ejemplo de esto es la relación negativa que existe entre el porcentaje de población indígena que vive en las entidades federativas y el ingreso per cápita de las mismas. La relación que se observa es que, a mayor número de población indígena, los estados registran un menor PIB per cápita en los habitantes, tal es el caso de estados como Chiapas, Oaxaca y Guerrero, que registran el porcentaje más alto de población indígena y a su vez el PIB per cápita más bajo respectivamente. Por otra parte, entidades como Campeche, CDMX o Nuevo León, registran el PIB per cápita más alto del país, y a su vez cuentan con un bajo o casi nulo grueso de población indígena"),
            leafletOutput("mapa1"),
            h1("                         "),
            h3("Análisis en gráficos de barras"),
            selectInput("datasets", "Datasets a mostrar", choices = misDatasets),
            plotOutput("plot"),
            h1("     ")
     
    ),
    
    tabItem("geoSoc",
            h1("Análisis geográfico de indicadores sociales"),
            p("Aparte de que las infraestructuras de transportes y comunicaciones estén construidas a partir del centro y norte, que es donde se concentra la mayor parte de la riqueza del país, las instituciones que se supone deberían representar a la mayoría del país, que como ya se mencionó, viven con carencias sociales, realmente tienden a representar a grupos de personas que no experimentan esta realidad de carencias y precariedad, dejando a millones de mexicanos fuera de la protección o cuidado por parte del Estado."),
            h3("Carencia por acceso a servicios básicos en la vivienda"),
            leafletOutput("mapa2"),
            h3("Carencia por rezago educativo"),
            leafletOutput("mapa3"),
            h3("Carencia por acceso a la seguridad social"),
            leafletOutput("mapa4"),
            h3("Carencia por acceso a servicios de salud"),
            leafletOutput("mapa5"),
            h3("Carencia por calidad y espacios de vivienda"),
            leafletOutput("mapa6"),
            h3("Carencia por acceso a la alimentación"),
            leafletOutput("mapa7")
      
    ),
    
    tabItem("conc",
            h1("Conclusiones"),
            p("Al analizar los resultados preliminares de la investigación expuesta en el presente a través de los datos y las visualizaciones, se puede confirmar la hipótesis propuesta al inicio del planteamiento del proyecto: existen poblaciones especificas que son las más afectadas por la configuración del país, en casi todos los aspectos, tal es el caso de Chiapas, Oaxaca y Guerrero, que son el común denominador de los estados con mayores carencias. Es decir, en efecto la localización geográfica es un gran medidor de la pobreza y a su vez una gran herramienta para predecir ciertas zonas que pueden padecer de ciertas carencias de acuerdo con sus alrededores. Esto solo reafirma la necesidad del proyecto para lograr atacar estas carencias especificas en zonas particulares del país."),
            p("En la siguiente red multicapa se reflejan precisamente estos resultados obtenidos de los indicadores sociales y económicos, donde cada capa representa un indicador y cada una cuenta con 10 vertices que son los 10 peores estados en términos de dicho indicador"),
            plotOutput("plot5"),
            h2("Análisis de comunidades entre los estados con mayores carencias"),
            p("Como ya se estableció anteriormente, los vertices representan a los 10 peores estados según cada indicador, y las aristas que conectan dichos vertices representan la conexión que existe entre cada uno de estos estados, es decir, nos muestra aquellos estados que colindan."),
            p("Al realizar los siguientes análisis podemos detectar fácilmente aquellas comunidades o en este caso regiones geográficas específicas que son las más afectadas por la pobreza"),
            plotOutput("plot6"),
            plotOutput("plot7"),
            plotOutput("plot8"),
            h2("Comparación entre los indicadores de la pobreza"),
            p("En la siguiente matriz se presenta una comparación entre las 7 distintas capas que representan cada uno de los indicadores. Al compararse uno a uno tenemos como posibles resultados una escala de 0 a 1, donde 0 significa que las capas no se parecen en nada ya que sus vertices no coinciden, y 1 cuando tenemos una coincidencia perfecta, es decir, entre más actores (en este caso estados) se repitan en las distintas capas, mayor será la cercanía entre las capas."),
            verbatimTextOutput("comparacion"),
            h1("Propuesta a futuro"),
            p("El proyecto que se busca implementar es a grandes rasgos un análisis de la información disponible en México sobre las carencias en términos de derechos sociales alrededor de las entidades federativas, para realizar un mapeo de zonas/poblaciones especificas que sufren de ciertas carencias sociales y poder asignar eficientemente un programa social existente para ayudar a sanar estas deficiencias por parte del sistema. "),
            p("Para hacer este proyecto realidad es necesario trabajar con la Secretaría del Desarrollo Social (SEDESOL) la cual es una de las diecinueve secretarías de Estado que conforman el gabinete legal del presidente de México, encargada de los programas de bienestar para atacar las carencias sociales que vive el país. Algunos programas de la SEDESOL son la red de abasto social DICONSA, el Programa de Inclusión Social PROSPERA, Seguro Popular, el programa Sin Hambre, entre muchos otros programas. Al tener la información detallada de la población podemos lograr una unión de los programas sociales específicos con la población especifica que lo requiere."),
            p("	Muchos de los datos expuestos en el trabajo presente son solo la punta del iceberg, y brindan información muy superficial sobre la situación especifica de las personas que padecen de pobreza. Sin embargo, siendo completamente consciente de la urgencia para revertir la situación a corto plazo se puede implementar esta conexión mencionada entre los programas sociales del gobierno y los focos de atención más graves que existen en el país. Al mismo tiempo se pueden lograr predicciones sobre poblaciones o zonas puntuales con las que no se cuenta información a partir de los datos ya existentes de familias con situaciones similares en zonas cercanas, ya que como se presentó anteriormente la localización geográfica es un gran predictor para la pobreza."),
            p("Al existir esta deficiencia de datos específicos para lograr una mejor distribución de los programas sociales en el país, a mediano plazo se buscaría la recaudación precisamente de esta información para construir una red vasta de datos que permita atacar inmediatamente los focos que se van presentando en el país."),
            p("Finalmente, a largo plazo se busca que el proyecto propuesto a partir de la ciencia de datos logre tener un impacto en la política pública, tanto a nivel local como federal, proponiendo a partir de la recaudación de los datos la creación de cada vez más programas sociales que atiendan las necesidades de la población y eliminando aquellos que demuestran ser ineficientes.")
            
    ),
    tabItem("ref",
            h1("Referencias"),
            p("González, L. (1968). Pueblo en Vilo. El Colegio de México. [Subtexto de: Introducción 2015, ITAM]"),
            p("García, B. (2014). Tiempo y espacio en México: las últimas décadas del siglo XX. El Colegio de México. [Subtexto de: Introducción 2015, ITAM"),
            p("Hernández Licona, G. (2020). El desarrollo económico en México. [Subtexto de: Retos en el siglo XXI, ITAM]"),
            p("Nota técnica sobre el rezago educativo, 2018-2020 (2021). CONEVAL. Recuperado de: "),
            a("https://www.coneval.org.mx/Medicion/MP/Documents/MMP_2018_2020/Notas_pobreza_2020/Nota_tecnica_sobre_el_rezago%20educativo_2018_2020.pdf"),
            p("Seguridad social. (2020). CNDH. Recuperado de: "),
            a("https://desca.cndh.org.mx/indicadores/Seguridad_social"),
            p("Encuesta Nacional de Ingresos y Gastos de los Hogares (ENIGH) (2020). INEGI. Recuperado de: "),
            a("https://www.inegi.org.mx/programas/enigh/nc/2020/"),
            p("Medición de la pobreza (2020). CONEVAL. Recuperado de: "),
            a("https://www.coneval.org.mx/Medicion/MP/Paginas/Pobreza_2020.aspx"),
            h1("         "),
            strong("***Nota"),
            p("Todas las visualizaciones fueron elaboradas a partir de datasets propios creados con información del ENIGH 2021. En la siguiente liga se encuentra el repositorio con los datasets oficiales del gobierno de México, losd datasets propios y el código implementado."),
            a("https://github.com/BobadillaE/Pobreza_Mexico_RStudio")
            
            
    )
    
            
      
    )
  )
  
)


server <- function(input, output){
  datos <- reactive(
    {
      get(input$datasets)
    }
  )

  
  output$plot1 <- renderPlot({
    ggplot(indSoc, aes(fill=variable, y=name, x=value)) + 
      geom_bar(position="dodge", stat="identity")
  })
  
  output$plot2 <- renderPlot({
    ggplot(indEco, aes(fill=variable, y=indicador, x=value)) + 
      geom_bar(position="dodge", stat="identity")
  })
  
  output$plot3 <- renderPlot({
    ggplot(datosIngreso  %>% filter(year >= input$slider1), aes(year, porcentaje, color=deciles)) + 
      geom_line(size =2) +
      geom_point(size=4)
  })
  
  output$plot4 <- renderPlot({
    ggplot(datosDesigualdad%>% filter(value >= input$slider2), aes(fill=variable, y=value, x=clases)) + 
      geom_bar(position="dodge", stat="identity")
  })
  
  output$mapa1 <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data=d4, popup = ~paste(Estado, pib, sep=' $'), group = "Mostrar PIB", color = "green") %>%
      addCircleMarkers(data=d5, popup = ~paste(edo, pib, sep=' $'), group = "Mostrar PIB", color = "yellow") %>%
      addCircleMarkers(data=d6, popup = ~paste(edo, pib, sep=' $'), group = "Mostrar PIB", color = "red") %>%
      addHeatmap(data = d1, group = "Pob. Indigena",blur=60, radius = 35) %>%
      addHeatmap(data = d3, group = "Pob. Indigena", blur=40, radius = 20) %>%
      addHeatmap(data = d2, group = "Pob. Indigena", blur=20, radius = 10) %>%
      addLayersControl(
        overlayGroups = c("Mostrar PIB","Pob. Indigena"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      setView(lng=-99.2, lat=24.34, zoom = 4.8)
  })
  
  
  output$plot <- renderPlot({
    ggplot(datos(),aes(fill=variable,x=value,y=estado)) + 
      geom_bar(position="dodge",stat="identity")
  })
  
  
  output$plot5 <- renderPlot({
    plot(net,layout=l2, grid = c(2,4),legend.x =  "bottomright",legend.inset = c(0.05,0.05))
  })
  
  output$plot6 <- renderPlot({
    plot(net,layout=l1,grid = c(2,4),legend.x =  "bottomright",legend.inset = c(0.05,0.05) ,com = abacus_ml(net))
  })
  
  output$plot7 <- renderPlot({
    plot(net,layout=l1,grid = c(2,4),legend.x =  "bottomright",legend.inset = c(0.05,0.05) ,com = flat_ec_ml(net))
  })
  
  output$plot8 <- renderPlot({
    plot(net,layout=l1,grid = c(2,4),legend.x =  "bottomright",legend.inset = c(0.05,0.05) ,com = glouvain_ml(net))
  })
  
  output$comparacion <- renderPrint({
    layer_comparison_ml(net, method="jaccard.edges")
  })

  output$mapa2 <- renderLeaflet({
    
    leaflet() %>%
      addTiles() %>%
      addLegend(
        position = "bottomleft",
        colors = c('#C4EEF2', '#699EBF', '#16558C', '#052159', '#031740'),
        bins = 5,
        labels = c("50%<", "60%-70%","70%-80%","80%-90%", "90%-100%"), opacity = 1,
        title = "Porcentaje de la población con acceso a agua diaria"
      )%>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      setView(lng = -102.200, lat = 22.5, zoom = 4.5) %>%
      addGeoJSONChoropleth(
        estados,
        valueProperty = "agua",
        scale = c('#C4EEF2', '#699EBF', '#16558C', '#052159', '#031740'),
        fillOpacity = 1,
        labelProperty = "name",
        popupProperty = propstoHTMLTable(
          props = c("id", "name", "agua"),
          table.attrs = list(class = "table table-striped table-bordered"),
        )
      )  
    
  })
  
  output$mapa3 <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addLegend(
        position = "bottomleft",
        colors = c('#96ED89', '#45BF55', '#168039', '#044D29', '#00261C'),
        bins = 5,
        labels = c("15.6%<", "15.6%-16.8%","16.8%-18.3%","18.3%-21.5%", ">21.5%"), opacity = 1,
        title = "Porcentaje de la población con rezago educativo"
      )%>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      setView(lng = -102.200, lat = 22.5, zoom = 4.5) %>%
      addGeoJSONChoropleth(
        estados,
        valueProperty = "edu",
        scale = c('#96ED89', '#45BF55', '#168039', '#044D29', '#00261C'),
        labelProperty = "name",
        fillOpacity = 1,
        popupProperty = propstoHTMLTable(
          props = c("id", "name", "edu"),
          table.attrs = list(class = "table table-striped table-bordered"),
        )
      )
    
  })
  
  output$mapa4 <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addLegend(
        position = "bottomleft",
        colors = c('#ffffe0', '#ffbe84', '#f47361', '#cb2f44', '#8b0000'),
        bins = 5,
        labels = c("13%<", "13%-18%","18%-21%","21%-24%", ">24%"), opacity = 1,
        title = "Porcentaje de la población sin afiliación social"
      )%>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      setView(lng = -102.200, lat = 22.5, zoom = 4.5) %>%
      addGeoJSONChoropleth(
        estados,
        valueProperty = "segsocial",
        colors = c('#ffffe0', '#ffbe84', '#f47361', '#cb2f44', '#8b0000'),
        labelProperty = "name",
        fillOpacity = 1,
        popupProperty = propstoHTMLTable(
          props = c("id", "name", "segsocial"),
          table.attrs = list(class = "table table-striped table-bordered"),
        )
      )
    
  })
  
  output$mapa5 <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addLegend(
        position = "bottomleft",
        colors = c('#fff8ff','#cdb3d3','#9873ab','#5e3886','#120064'),
        bins = 5,
        labels = c("30 minutos<", "30-36 minutos","36-40 minutos","40-43 minutos", ">43 minutos"), opacity = 1,
        title = "Tiempo promedio para acceder a un hospital por vivienda"
      )%>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      setView(lng = -102.200, lat = 22.5, zoom = 4.5) %>%
      addGeoJSONChoropleth(
        estados,
        valueProperty = "salud",
        colors = c('#fff8ff','#cdb3d3','#9873ab','#5e3886','#120064'),
        labelProperty = "name",
        fillOpacity = 1,
        popupProperty = propstoHTMLTable(
          props = c("id", "name", "salud"),
          table.attrs = list(class = "table table-striped table-bordered"),
        ),
        group= "G4"
      )
    
  })
  
  output$mapa6 <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addLegend(
        position = "bottomleft",
        colors = c('#fff3ff','#fccbf6','#fba1e7','#fb71d2','#fb24b8'),
        bins = 5,
        labels = c("58%<", "58%-78.5%","78.5%-83.5%","83.5%-92%", ">92%"), opacity = 1,
        title = "Porcentaje que su vivienda cumple con materiales aceptables"
      )%>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      setView(lng = -102.200, lat = 22.5, zoom = 4.5) %>%
      addGeoJSONChoropleth(
        estados,
        valueProperty = "vivienda",
        colors = c('#fff3ff','#fccbf6','#fba1e7','#fb71d2','#fb24b8'),
        labelProperty = "name",
        fillOpacity = 1,
        popupProperty = propstoHTMLTable(
          props = c("id", "name", "vivienda"),
          table.attrs = list(class = "table table-striped table-bordered"),
        )
      )
    
  })
  
  output$mapa7 <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addLegend(
        position = "bottomleft",
        colors = c('#ffffff','#b9b9b9','#777777','#3b3b3b','#000000'),
        bins = 5,
        labels = c("10%<", "10%-12%","12%-13.4%","13.4%-16%", ">16%"), opacity = 1,
        title = "Población que se quedó sin alimentos(3 meses)"
      )%>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      setView(lng = -102.200, lat = 22.5, zoom = 4.5) %>%
      addGeoJSONChoropleth(
        estados,
        valueProperty = "alimento",
        scale = c('#ffffff','#b9b9b9','#777777','#3b3b3b','#000000'),
        labelProperty = "name",
        fillOpacity = 1,
        popupProperty = propstoHTMLTable(
          props = c("id", "name", "alimento"),
          table.attrs = list(class = "table table-striped table-bordered"),
        )
      )
    
  })

  

}

shinyApp(ui,server)

