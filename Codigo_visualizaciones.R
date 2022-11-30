library(rjson)
library(leaflet)
library(leaflet.extras)
library(multinet)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(forcats)



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

### BAR CHARTS
ggplot(indSoc, aes(fill=variable, y=name, x=value)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(indEco, aes(fill=variable, y=indicador, x=value)) + 
  geom_bar(position="dodge", stat="identity")


ggplot(datosIngreso, aes(year, porcentaje, color=deciles)) + 
  geom_line(size =2) +
  geom_point(size=4)

ggplot(datosDesigualdad, aes(fill=variable, y=value, x=clases)) + 
  geom_bar(position="fill", stat="identity")


ggplot(datosDesigualdad, aes(fill=variable, y=value, x=clases)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(Pib_Per_Capita, aes(fill=variable,x=value,y=estado)) + geom_bar(position="dodge",stat= "identity")

ggplot(Poblacion_Indigena, aes(fill=variable,x=value,y=estado)) + geom_bar(position="dodge",stat= "identity")

ggplot(Relacion_Pib_PoblacionIndigena, aes(fill=variable, x=value, y=estado)) + geom_bar(position="dodge", stat="identity")

### LEAFLET
leaflet() %>%
  addTiles(group = "Mapa 1") %>%
  addCircleMarkers(data=d4, popup = ~paste(Estado, pib, sep=' $'), group = "G1", color = "green") %>%
  addCircleMarkers(data=d5, popup = ~paste(edo, pib, sep=' $'), group = "G1", color = "yellow") %>%
  addCircleMarkers(data=d6, popup = ~paste(edo, pib, sep=' $'), group = "G1", color = "red") %>%
  addHeatmap(data = d1, group = "G2",blur=60, radius = 35) %>%
  addHeatmap(data = d3, group = "G2", blur=40, radius = 20) %>%
  addHeatmap(data = d2, group = "G2", blur=20, radius = 10) %>%
  addProviderTiles(providers$Esri.DeLorme, group = "Mapa 2") %>%
  addLayersControl(
    overlayGroups = c("G1","G2"),
    baseGroups = c("Mapa 1", "Mapa 2"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  setView(lng=-99.2, lat=24.34, zoom = 4.8)




### CHOROPLETHS

leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  setView(lng = -102.200, lat = 22.5, zoom = 4.5) %>%
  addGeoJSONChoropleth(
    estados,
    valueProperty = "agua",
    labelProperty = "name",
    scale=c("viridis"),
    fillOpacity = 1,
    popupProperty = propstoHTMLTable(
      props = c("id", "name", "agua"),
      table.attrs = list(class = "table table-striped table-bordered"),
    ),
    group= "G1"
  ) %>%
  addGeoJSONChoropleth(
    estados,
    valueProperty = "edu",
    scale=c("viridis"),
    labelProperty = "name",
    fillOpacity = 0.5,
    popupProperty = propstoHTMLTable(
      props = c("id", "name", "edu"),
      table.attrs = list(class = "table table-striped table-bordered"),
    ),
    group= "G2"
  ) %>%
  addGeoJSONChoropleth(
    estados,
    valueProperty = "segsocial",
    scale = c("viridis"),
    labelProperty = "name",
    fillOpacity = 1,
    popupProperty = propstoHTMLTable(
      props = c("id", "name", "segsocial"),
      table.attrs = list(class = "table table-striped table-bordered"),
    ),
    group= "G3"
  ) %>%
  addGeoJSONChoropleth(
    estados,
    valueProperty = "salud",
    scale = c("viridis"),
    labelProperty = "name",
    fillOpacity = 1,
    popupProperty = propstoHTMLTable(
      props = c("id", "name", "salud"),
      table.attrs = list(class = "table table-striped table-bordered"),
    ),
    group= "G4"
  ) %>%
  addGeoJSONChoropleth(
    estados,
    valueProperty = "vivienda",
    scale = c("viridis"),
    labelProperty = "name",
    fillOpacity = 1,
    popupProperty = propstoHTMLTable(
      props = c("id", "name", "vivienda"),
      table.attrs = list(class = "table table-striped table-bordered"),
    ),
    group= "G5"
  ) %>%
  addGeoJSONChoropleth(
    estados,
    valueProperty = "alimento",
    scale = c('#ffffe0', '#ffe0a9', '#ffbe84', '#ff986d', '#f47361', '#e35056', '#cb2f44', '#ae112a', '#8b0000'),
    labelProperty = "name",
    fillOpacity = 1,
    popupProperty = propstoHTMLTable(
      props = c("id", "name", "alimento"),
      table.attrs = list(class = "table table-striped table-bordered"),
    ),
    group= "G6"
  ) %>%
  addLayersControl(
    overlayGroups = c("G1","G2","G3","G4","G5","G6")
  )  


### agua

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

### rezago educativo
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

### seguridad social
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

### salud
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

### vivienda
leaflet() %>%
  addTiles() %>%
  addLegend(
    position = "bottomleft",
    colors = c('#8b0000','#cb2f44','#f47361','#ffbe84','#ffffe0'),
    bins = 5,
    labels = c("58%<", "58%-78.5%","78.5%-83.5%","83.5%-92%", ">92%"), opacity = 1,
    title = "Porcentaje que su vivienda cumple con materiales aceptables"
  )%>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  setView(lng = -102.200, lat = 22.5, zoom = 4.5) %>%
  addGeoJSONChoropleth(
    estados,
    valueProperty = "vivienda",
    colors = c('#8b0000','#cb2f44','#f47361','#ffbe84','#ffffe0'),
    labelProperty = "name",
    fillOpacity = 1,
    popupProperty = propstoHTMLTable(
      props = c("id", "name", "vivienda"),
      table.attrs = list(class = "table table-striped table-bordered"),
    )
  )

### alimentación
leaflet() %>%
  addTiles() %>%
  addLegend(
    position = "bottomleft",
    colors = c('#ffffe0', '#ffbe84', '#f47361', '#cb2f44', '#8b0000'),
    bins = 5,
    labels = c("10%<", "10%-12%","12%-13.4%","13.4%-16%", ">16%"), opacity = 1,
    title = "Población que se quedó sin alimentos(3 meses)"
  )%>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  setView(lng = -102.200, lat = 22.5, zoom = 4.5) %>%
  addGeoJSONChoropleth(
    estados,
    valueProperty = "alimento",
    scale = c('#ffffe0', '#ffbe84', '#f47361', '#cb2f44', '#8b0000'),
    labelProperty = "name",
    fillOpacity = 1,
    popupProperty = propstoHTMLTable(
      props = c("id", "name", "alimento"),
      table.attrs = list(class = "table table-striped table-bordered"),
    )
  )


### RED MULTILAYER
net <- ml_empty()
capas <- c("RezagoEdu", "AguaDiaria", "AccesoHospital","SinAfiliacion","Vivienda","SinAlimentos","PibPC")
add_layers_ml(net,capas)
add_vertices_ml(net,vertices1)
net
add_edges_ml(net, edges1)
net
plot(net)

a <- plot(net, com = clique_percolation_ml(net))

b <- plot(net, com = glouvain_ml(net))

layer_comparison_ml(net, method="jaccard.edges")

layer_comparison_ml(net, method="jaccard.actors")

layer_comparison_ml(net, method="jeffrey.degree")

layer_comparison_ml(net, method="pearson.degree")
