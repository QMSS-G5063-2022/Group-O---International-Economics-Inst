suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(rgdal))
suppressPackageStartupMessages(library(WDI))
library(leaflet)

worldmap = readOGR(dsn="Nat_world_map", layer="ne_50m_admin_0_countries")

worldmap@data[worldmap@data$NAME=="United States of America",]$NAME<-"United States"
worldmap@data[worldmap@data$NAME=="Congo",]$NAME<-"Rep. of Congo"
worldmap@data[worldmap@data$NAME=="South Korea",]$NAME<-"Korea"
worldmap@data[worldmap@data$NAME=="North Korea",]$NAME<-"Dem. Rep. Korea"
worldmap@data[worldmap@data$NAME=="Czechia",]$NAME<-"Czech Rep."
worldmap@data[worldmap@data$NAME==unique(worldmap@data$NAME)[grep("Ivoi",unique(worldmap@data$NAME))],]$NAME<-"Cote d'Ivoire"
worldmap@data[worldmap@data$NAME=="Cabo Verde",]$NAME<-"Cape Verde"
worldmap@data[worldmap@data$NAME=="Trinidad and Tobago",]$NAME<-"Trinidad & Tobago"
worldmap@data[worldmap@data$NAME=="Eq. Guinea",]$NAME<-"Equatorial Guinea"
worldmap@data[worldmap@data$NAME=="Bosnia and Herz.",]$NAME<-"Bosnia"
worldmap@data[worldmap@data$NAME=="Antigua and Barb.",]$NAME<-"Antigua and Barbuda"
worldmap@data[worldmap@data$NAME=="Solomon Is.",]$NAME<-"Solomon Islands"
worldmap@data[worldmap@data$NAME==unique(worldmap@data$NAME)[grep("Tom",unique(worldmap@data$NAME))],]$NAME<-"Sao Tome"

ui<-fluidPage(h1="Map Fun",
              sliderInput("year", label="Select Year", min=2005, max=2020, value=2015, sep=""),
              #radioButtons("which","Investment Source",choices=c("China","IMF")),
              checkboxInput("net","Display Network"),
              #actionButton("button","Plot"),
              leafletOutput("leaf_c"),
              leafletOutput("leaf_i")
)

server<-function(input, output){
  suppressPackageStartupMessages(library(leaflet))
  suppressPackageStartupMessages(library(ggplot2))
  suppressPackageStartupMessages(library(ggthemes))
  suppressPackageStartupMessages(library(network))
  suppressPackageStartupMessages(library(maps))
  suppressPackageStartupMessages(library(igraph))
  
  dat_c<-reactive({
      chn_inv<-read.csv('China-Global-Investment-Tracker-2021-Fall-FINAL-2022.2.21-update.up.csv')
      chn_inv<-chn_inv %>%
        group_by(Year, Country) %>%
        mutate(inv_tot = sum(as.numeric(gsub("[[:punct:]]", "", Quantity.in.Millions))))
      chn_inv$Country<-stringr::str_to_lower(chn_inv$Country)
      
      chn_inv[chn_inv$Country=="britain",]$Country<-"uk"
      chn_inv[chn_inv$Country=="myanmar",]$Country<-"burma"
      chn_inv[chn_inv$Country=="russian federation",]$Country<-"russia"
      chn_inv[chn_inv$Country=="trinidad-tobago",]$Country<-"trinidad and tobago"
      chn_inv[chn_inv$Country=="bosnia",]$Country<-"bosnia and herzegovina"
      chn_inv[chn_inv$Country=="sao tome",]$Country<-"uk"
      
      inv_y<-filter(chn_inv, Year==input$year)
      inv_y<-inv_y[!duplicated(inv_y$Country),c("Country","inv_tot")]
      inv_y$inv_tot<-as.numeric(inv_y$inv_tot)
      
      worldmap@data$sov_low<-stringr::str_to_lower(worldmap@data$SOVEREIGNT)
      worldmap@data[worldmap@data$sov_low=="the bahamas",]$sov_low<-"bahamas"
      worldmap@data[worldmap@data$sov_low=="united kingdom",]$sov_low<-"uk"
      worldmap@data[worldmap@data$sov_low=="united states of america",]$sov_low<-"usa"
      worldmap@data[worldmap@data$sov_low=="united arab emirates",]$sov_low<-"uae"
      worldmap@data[worldmap@data$sov_low=="czechia",]$sov_low<-"czech republic"
      worldmap@data[worldmap@data$sov_low=="east timor",]$sov_low<-"timor-leste"
      worldmap@data[worldmap@data$sov_low=="united republic of tanzania",]$sov_low<-"tanzania"
      
      worldmap@data<-left_join(worldmap@data, inv_y, by=c("sov_low"="Country"))
      worldmap}) # End eventReactive making dat_c
    
  dat_i<-reactive({    
     
       imf<-WDI(indicator = "DT.DOD.DIMF.CD", country="all",start=2005, extra=F)
       imf<-filter(imf, !country %in% unique(imf$country)[1:49])
       imf<-rename(imf, Year=year, name=country, IMF_cred=DT.DOD.DIMF.CD)
       imf[imf$name=="Bahamas, The",]$name<-"The Bahamas"
       imf[imf$name=="Cabo Verde",]$name<-"Cape Verde"
       #imf[imf$name=="China",]$name<-"People's Republic of China"
       imf[imf$name=="Congo, Rep.",]$name<-"Republic of the Congo"
       imf[imf$name=="Congo, Dem. Rep.",]$name<-"Democratic Republic of the Congo"
       imf[imf$name=="Cote d'Ivoire",]$name<-"Ivory Coast"
       imf[imf$name=="Egypt, Arab Rep.",]$name<-"Egypt"
       imf[imf$name=="Gambia, The",]$name<-"Gambia"
       imf[imf$name=="Hong Kong SAR, China",]$name<-"Hong Kong"
       imf[imf$name=="Iran, Islamic Rep.",]$name<-"Iran"
       imf[imf$name=="Korea, Rep.",]$name<-"South Korea"
       imf[imf$name=="Korea, Dem. People's Rep.",]$name<-"North Korea"
       imf[imf$name=="Kyrgyz Republic",]$name<-"Kyrgyzstan"
       imf[imf$name=="Lao PDR",]$name<-"Laos"
       imf[imf$name=="Russian Federation",]$name<-"Russia"
       imf[imf$name=="Micronesia, Fed. Sts.",]$name<-"Micronesia"
       imf[imf$name=="Syrian Arab Republic",]$name<-"Syria"
       imf[imf$name=="Slovak Republic",]$name<-"Slovakia"
       imf[imf$name=="Sint Maarten (Dutch part)",]$name<-"Sint Maarten"
       imf[imf$name=="St. Martin (French part)",]$name<-"Saint Martin"
       imf[imf$name=="St. Kitts and Nevis",]$name<-"Saint Kitts and Nevis"
       imf[imf$name=="St. Lucia",]$name<-"Saint Lucia"
       imf[imf$name=="St. Vincent and the Grenadines",]$name<-"Saint Vincent and the Grenadines"
       imf[imf$name=="Timor-Leste",]$name<-"East Timor"
       imf[imf$name=="United States",]$name<-"USA"
       imf[imf$name=="Venezuela, RB",]$name<-"Venezuela"
       imf[imf$name=="Virgin Islands (U.S.)",]$name<-"United States Virgin Islands"
       imf[imf$name=="Yemen, Rep.",]$name<-"Yemen"
       
       inv_y<-filter(imf, Year==input$year)[c("name","IMF_cred")]
      
      # worldmap@data[worldmap@data$NAME_EN=="The Bahamas",]$NAME_EN<-"Bahamas"
       worldmap@data[worldmap@data$NAME_EN=="Brunei",]$NAME_EN<-"Brunei Darussalam"
       worldmap@data[worldmap@data$NAME_EN=="People's Republic of China",]$NAME_EN<-"China"
       worldmap@data[worldmap@data$NAME_EN=="The Gambia",]$NAME_EN<-"Gambia"
       worldmap@data[worldmap@data$NAME_EN=="Federated States of Micronesia",]$NAME_EN<-"Micronesia"
       worldmap@data[worldmap@data$NAME_EN=="United States of America",]$NAME_EN<-"USA"
       worldmap@data[worldmap@data$NAME_EN==unique(worldmap@data$NAME_EN)[grep("Cura",worldmap@data$NAME_EN)],]$NAME_EN<-"Curacao"
      
      worldmap@data<-left_join(worldmap@data, inv_y, by=c("NAME_EN"="name"))
      worldmap
      
     
  }) #These end the eventReactive that makes dat_i
  
  pop_cont1<-reactive({
    f<-dat_c()@data$inv_tot
    f[is.na(f)]<-0
    
    paste0("Country: ", dat_c()@data$SOVEREIGNT, "<br>",
                     "Investment from China (Million USD): $", f)
  }) #end eventReactive defining pop_cont1
  
  pop_cont2<-reactive({
    f<-dat_i()@data$IMF_cred
    f[is.na(f)]<-0
    paste0("Country: ", dat_i()@data$SOVEREIGNT, "<br>",
                     "IMF Investment (Million USD): $", round(f/1000000, 0))
  })
  
  pal1<-reactive({
    j<-colorNumeric(palette="Reds", domain=dat_c()@data$inv_tot)
    j(dat_c()@data$inv_tot)
  })
  
  pal2<-reactive({
    j<-colorNumeric(palette="Blues", domain=dat_i()@data$IMF_cred)
    j(dat_i()@data$IMF_cred)
  })
  
  map_c<-reactive({
    if(input$net==F){
      leaflet() %>%
        setView(lat=40, lng=0, zoom=1.4)%>%
        addPolygons(data=dat_c(), weight=.5, fillOpacity = .75, fillColor = pal1(),
                    highlightOptions = highlightOptions(color="white", weight=2, bringToFront = T, sendToBack = T),
                    popup=pop_cont1(), label=dat_i()@data$SOVEREIGNT) %>%
        addEasyButton(easyButton(icon="fa-globe",title="Home",onClick=JS("function(btn, map){ map.setZoom(1.4); }")))
    } else if(input$net==T){ #ends if(input$net==F)
      rawnodes<-read.csv('http://www.kateto.net/wordpress/wp-content/uploads/2015/06/Country_terms_FREQ.csv')
      rawedges<-read.csv('http://www.kateto.net/wordpress/wp-content/uploads/2015/06/Country_terms_COOC.csv')
      
      chn_inv<-read.csv('China-Global-Investment-Tracker-2021-Fall-FINAL-2022.2.21-update.up.csv')
      chn_inv<-chn_inv %>%
        group_by(Year, Country) %>%
        mutate(inv_tot = sum(as.numeric(gsub("[[:punct:]]", "", Quantity.in.Millions))))
      chn_inv$Country<-stringr::str_to_lower(chn_inv$Country)
      
      chn_inv[chn_inv$Country=="britain",]$Country<-"uk"
      chn_inv[chn_inv$Country=="myanmar",]$Country<-"burma"
      chn_inv[chn_inv$Country=="russian federation",]$Country<-"russia"
      chn_inv[chn_inv$Country=="trinidad-tobago",]$Country<-"trinidad and tobago"
      chn_inv[chn_inv$Country=="bosnia",]$Country<-"bosnia and herzegovina"
      chn_inv[chn_inv$Country=="sao tome",]$Country<-"uk"
      
      net_dat<-filter(chn_inv, Year==min(chn_inv$Year))[!duplicated(filter(chn_inv, Year==min(chn_inv$Year))$Country), c("Country","Year","inv_tot")]
      for(o in unique(chn_inv$Year)[-1]){
        net_dat<-rbind(net_dat, filter(chn_inv, Year==o)[!duplicated(filter(chn_inv, Year==o)$Country), c("Country", "Year", "inv_tot")])
      }
      net_dat["Source"]<-rep("china",nrow(net_dat))
      net_dat<-net_dat[c(4,1,2,3)]
      colnames(net_dat)[2]<-"Target"
      
      net_dat[net_dat$Target=="united arab emirates",]$Target<-"uae"
      net_dat[net_dat$Target=="united states",]$Target<-"usa"
      net_dat[net_dat$Target=="united kingdom",]$Target<-"uk"
      
      nodes<-rename(rawnodes[c("ID","lon","lat")],name=ID)
      h<-dplyr::rename(filter(net_dat,Year==input$year)[c("Source", "Target", "inv_tot")], FROM=Source, TO=Target)
      nodes<-filter(nodes, name %in% c(filter(net_dat, Year==input$year)$Target,"china"))
      nodes$id<-as.integer(rownames(nodes))
      nodes<-nodes[c(4,2,3,1)]
      
      u<-nodes[c(4,3,2)]
      network_<-get.data.frame(graph.data.frame(filter(h, !TO%in%setdiff(h$TO, u$name)), directed=T, vertices=u), "both")
      verts_<-network_$vertices
      
      coordinates(verts_)<-~lon+lat
      
      #edges_<-network_$edges
      edges_1<-filter(network_$edges, network_$edges[,3]<=quantile(network_$edges[,3], .2))
      edges_2<-filter(network_$edges, network_$edges[,3]<=quantile(network_$edges[,3], .4) & network_$edges[,3]>quantile(network_$edges[,3], .2))
      edges_3<-filter(network_$edges, network_$edges[,3]<=quantile(network_$edges[,3], .6) & network_$edges[,3]>quantile(network_$edges[,3], .4))
      edges_4<-filter(network_$edges, network_$edges[,3]<=quantile(network_$edges[,3], .8) & network_$edges[,3]>quantile(network_$edges[,3], .6))
      edges_5<-filter(network_$edges, network_$edges[,3]>quantile(network_$edges[,3], .8))
      
      # edges_<-lapply(1:nrow(edges_),function(i){
      #   as(rbind(verts_[verts_$name==edges_[i, "from"],],
      #            verts_[verts_$name==edges_[i, "to"],]),
      #      "SpatialLines")
      # })
      edges_1<-lapply(1:nrow(edges_1),function(i){
        as(rbind(verts_[verts_$name==edges_1[i, "from"],],
                 verts_[verts_$name==edges_1[i, "to"],]),
           "SpatialLines")
      })
      edges_2<-lapply(1:nrow(edges_2),function(i){
        as(rbind(verts_[verts_$name==edges_2[i, "from"],],
                 verts_[verts_$name==edges_2[i, "to"],]),
           "SpatialLines")
      })
      edges_3<-lapply(1:nrow(edges_3),function(i){
        as(rbind(verts_[verts_$name==edges_3[i, "from"],],
                 verts_[verts_$name==edges_3[i, "to"],]),
           "SpatialLines")
      })
      edges_4<-lapply(1:nrow(edges_4),function(i){
        as(rbind(verts_[verts_$name==edges_4[i, "from"],],
                 verts_[verts_$name==edges_4[i, "to"],]),
           "SpatialLines")
      })
      edges_5<-lapply(1:nrow(edges_5),function(i){
        as(rbind(verts_[verts_$name==edges_5[i, "from"],],
                 verts_[verts_$name==edges_5[i, "to"],]),
           "SpatialLines")
      })
      
      # for(i in seq_along(edges)){
      #   edges_[[i]]<-spChFIDs(edges_[[i]], as.character(i))
      # }
      for(i in seq_along(edges)){
        edges_1[[i]]<-spChFIDs(edges_1[[i]], as.character(i))
      }
      for(i in seq_along(edges)){
        edges_2[[i]]<-spChFIDs(edges_2[[i]], as.character(i))
      }
      for(i in seq_along(edges)){
        edges_3[[i]]<-spChFIDs(edges_3[[i]], as.character(i))
      }
      for(i in seq_along(edges)){
        edges_4[[i]]<-spChFIDs(edges_4[[i]], as.character(i))
      }
      for(i in seq_along(edges)){
        edges_5[[i]]<-spChFIDs(edges_5[[i]], as.character(i))
      }
      #edges_<-do.call(rbind, edges_)
      edges_1<-do.call(rbind, edges_1)
      edges_2<-do.call(rbind, edges_2)
      edges_3<-do.call(rbind, edges_3)
      edges_4<-do.call(rbind, edges_4)
      edges_5<-do.call(rbind, edges_5)
      
      leaflet()%>%
        setView(lat=40, lng=0, zoom=1.4)%>%
        addPolygons(data=dat_c(),weight=.5, color=pal1(), fillOpacity = .75, popup=pop_cont1(), 
                    highlightOptions = highlightOptions(color="white", weight=2, bringToFront = T, sendToBack = T), label=worldmap@data$SOVEREIGNT) %>%
        addEasyButton(easyButton(icon="fa-globe",title="Home",onClick=JS("function(btn, map){ map.setZoom(1.4); }"))) %>%
        addCircles(data=verts_, opacity=.2, fillColor = "red", color="red") %>%
        addPolylines(data=edges_1, color="#EEAECA", opacity=.01, weight=.5)%>%
        addPolylines(data=edges_2, color="#E095A6", opacity=.03, weight=1)%>%
        addPolylines(data=edges_3, color="#D5828A", opacity = .05, weight=1.5) %>%
        addPolylines(data=edges_4, color="#C76966", opacity = .07, weight=2.5) %>%
        addPolylines(data=edges_5, color="#AA381E", opacity=.1, weight=5)
    } #ends if(net==T)
  }) #these end the reactive that makes map_c
  
  map_i<-reactive({
    if(input$net==F){
      leaflet() %>%
        setView(lat=40, lng=0, zoom=1.4)%>%
        addPolygons(data=dat_i(), weight=.5, fillOpacity = .75, fillColor = pal2(),
                    highlightOptions = highlightOptions(color="white", weight=2, bringToFront = T, sendToBack = T),
                                       popup=pop_cont2(), label=dat_i()@data$SOVEREIGNT) %>%
        addEasyButton(easyButton(icon="fa-globe",title="Home",onClick=JS("function(btn, map){ map.setZoom(1.4); }")))
    }  else if(input$net==T){  #ends if(input$net==F)
      rawnodes<-read.csv('http://www.kateto.net/wordpress/wp-content/uploads/2015/06/Country_terms_FREQ.csv')
      rawedges<-read.csv('http://www.kateto.net/wordpress/wp-content/uploads/2015/06/Country_terms_COOC.csv')
      
      imf<-WDI(indicator = "DT.DOD.DIMF.CD", country="all",start=2005, extra=F)
      imf<-filter(imf, !country %in% unique(imf$country)[1:49])
      imf<-rename(imf, Year=year, name=country, IMF_cred=DT.DOD.DIMF.CD)
      imf[imf$name=="Bahamas, The",]$name<-"The Bahamas"
      imf[imf$name=="Cabo Verde",]$name<-"Cape Verde"
      #imf[imf$name=="China",]$name<-"People's Republic of China"
      imf[imf$name=="Congo, Rep.",]$name<-"Republic of the Congo"
      imf[imf$name=="Congo, Dem. Rep.",]$name<-"Democratic Republic of the Congo"
      imf[imf$name=="Cote d'Ivoire",]$name<-"Ivory Coast"
      imf[imf$name=="Egypt, Arab Rep.",]$name<-"Egypt"
      imf[imf$name=="Gambia, The",]$name<-"Gambia"
      imf[imf$name=="Hong Kong SAR, China",]$name<-"Hong Kong"
      imf[imf$name=="Iran, Islamic Rep.",]$name<-"Iran"
      imf[imf$name=="Korea, Rep.",]$name<-"South Korea"
      imf[imf$name=="Korea, Dem. People's Rep.",]$name<-"North Korea"
      imf[imf$name=="Kyrgyz Republic",]$name<-"Kyrgyzstan"
      imf[imf$name=="Lao PDR",]$name<-"Laos"
      imf[imf$name=="Russian Federation",]$name<-"Russia"
      imf[imf$name=="Micronesia, Fed. Sts.",]$name<-"Micronesia"
      imf[imf$name=="Syrian Arab Republic",]$name<-"Syria"
      imf[imf$name=="Slovak Republic",]$name<-"Slovakia"
      imf[imf$name=="Sint Maarten (Dutch part)",]$name<-"Sint Maarten"
      imf[imf$name=="St. Martin (French part)",]$name<-"Saint Martin"
      imf[imf$name=="St. Kitts and Nevis",]$name<-"Saint Kitts and Nevis"
      imf[imf$name=="St. Lucia",]$name<-"Saint Lucia"
      imf[imf$name=="St. Vincent and the Grenadines",]$name<-"Saint Vincent and the Grenadines"
      imf[imf$name=="Timor-Leste",]$name<-"East Timor"
      imf[imf$name=="United States",]$name<-"USA"
      imf[imf$name=="Venezuela, RB",]$name<-"Venezuela"
      imf[imf$name=="Virgin Islands (U.S.)",]$name<-"United States Virgin Islands"
      imf[imf$name=="Yemen, Rep.",]$name<-"Yemen"
      worldmap@data[worldmap@data$NAME_EN=="Brunei",]$NAME_EN<-"Brunei Darussalam"
      worldmap@data[worldmap@data$NAME_EN=="People's Republic of China",]$NAME_EN<-"China"
      worldmap@data[worldmap@data$NAME_EN=="The Gambia",]$NAME_EN<-"Gambia"
      worldmap@data[worldmap@data$NAME_EN=="Federated States of Micronesia",]$NAME_EN<-"Micronesia"
      worldmap@data[worldmap@data$NAME_EN=="United States of America",]$NAME_EN<-"USA"
      worldmap@data[worldmap@data$NAME_EN==unique(worldmap@data$NAME_EN)[grep("Cura",worldmap@data$NAME_EN)],]$NAME_EN<-"Curacao"
      
      net_dat<-filter(imf, Year==min(imf$Year))[!duplicated(filter(imf, Year==min(imf$Year))$name), c("name","Year","IMF_cred")]
      for(o in unique(imf$Year)[-1]){
        net_dat<-rbind(net_dat, filter(imf, Year==o)[!duplicated(filter(imf, Year==o)$name), c("name", "Year", "IMF_cred")])
      }
      net_dat["Source"]<-rep("imf",nrow(net_dat))
      net_dat<-net_dat[c(4,1,2,3)]
      colnames(net_dat)[2]<-"Target"
      
      nodes<-rename(rawnodes[c("ID","lon","lat")],name=ID)
      net_dat$Target<-stringr::str_to_lower(net_dat$Target)
      nodes<-rbind(nodes, c("imf", round(-77.04425201622206,6), round(38.89908346600995,6)))
      nodes<-rbind(nodes, c("American Samoa", round(-170.5794322853497,6), round(-14.26012915103479,6)))
      nodes<-rbind(nodes, c("comoros", round(43.48504853332502,6),round(-11.862176383838756,6)))
      nodes<-rbind(nodes,c("republic of the congo", round(16.052629791985225,6), round(-0.07691295959726356,6)))
      nodes<-rbind(nodes,c("democratic republic of the congo", round(23.84214068136281,6), round(-3.2141630381290125,6)))
      nodes<-rbind(nodes,c("ivory coast", round(-5.529500409752393,6),round(7.253827157140813,6)))
      nodes<-rbind(nodes,c("eswatini", round(31.526906430335316,6), round(-26.50532674658614, 6)))
      nodes<-rbind(nodes,c("guinea-bissau", round(-15.11433094944025,6),round(11.849168222767416,6)))
      nodes<-rbind(nodes, c("sao tome and principe", round(6.637044051405797,6), round(0.3877357338129725, 6)))
      nodes<-rbind(nodes, c("saint lucia", round(-60.981555430152405,6), round(13.890030848573293,6)))
      nodes<-rbind(nodes, c("saint vincent and the grenadines", round(-61.25999737205744,6), round(12.990774451382883,6)))
      
      nodes[nodes$name=="uk",]$name<-"united kingdom"
      nodes[nodes$name=="uae",]$name<-"united arab emirates"
      nodes[nodes$name=="brunei",]$name<-"brunei darussalam"
      nodes[nodes$name=="macedonia",]$name<-"north macedonia"
      nodes[nodes$name=="burma",]$name<-"myanmar"
      
      nodes<-filter(nodes, name %in% c(filter(net_dat, Year==input$year & !is.na(IMF_cred))$Target,"imf"))
      
      nodes$id<-as.integer(rownames(nodes))
      nodes<-nodes[c(4,2,3,1)]
      
      h<-dplyr::rename(filter(net_dat,Year==input$year & !is.na(IMF_cred))[c("Source", "Target", "IMF_cred")], FROM=Source, TO=Target)
      
      u<-nodes[c(4,3,2)]
      network_<-get.data.frame(graph.data.frame(h, directed=T, vertices=u), "both")
      verts_<-network_$vertices
      verts_$lat<-as.numeric(verts_$lat)
      verts_$lon<-as.numeric(verts_$lon)
      
      coordinates(verts_)<-~lon+lat
      
      # edges_<-network_$edges
      edges_1<-filter(network_$edges, network_$edges[,3]<=quantile(network_$edges[,3], .2))
      edges_2<-filter(network_$edges, network_$edges[,3]<=quantile(network_$edges[,3], .4) & network_$edges[,3]>quantile(network_$edges[,3], .2))
      edges_3<-filter(network_$edges, network_$edges[,3]<=quantile(network_$edges[,3], .6) & network_$edges[,3]>quantile(network_$edges[,3], .4))
      edges_4<-filter(network_$edges, network_$edges[,3]<=quantile(network_$edges[,3], .8) & network_$edges[,3]>quantile(network_$edges[,3], .6))
      edges_5<-filter(network_$edges, network_$edges[,3]>quantile(network_$edges[,3], .8))
      
      # edges_<-lapply(1:nrow(edges_),function(i){
      #   as(rbind(verts_[verts_$name==edges_[i, "from"],],
      #            verts_[verts_$name==edges_[i, "to"],]),
      #      "SpatialLines")
      # })
      edges_1<-lapply(1:nrow(edges_1),function(i){
        as(rbind(verts_[verts_$name==edges_1[i, "from"],],
                 verts_[verts_$name==edges_1[i, "to"],]),
           "SpatialLines")
      })
      edges_2<-lapply(1:nrow(edges_2),function(i){
        as(rbind(verts_[verts_$name==edges_2[i, "from"],],
                 verts_[verts_$name==edges_2[i, "to"],]),
           "SpatialLines")
      })
      edges_3<-lapply(1:nrow(edges_3),function(i){
        as(rbind(verts_[verts_$name==edges_3[i, "from"],],
                 verts_[verts_$name==edges_3[i, "to"],]),
           "SpatialLines")
      })
      edges_4<-lapply(1:nrow(edges_4),function(i){
        as(rbind(verts_[verts_$name==edges_4[i, "from"],],
                 verts_[verts_$name==edges_4[i, "to"],]),
           "SpatialLines")
      })
      edges_5<-lapply(1:nrow(edges_5),function(i){
        as(rbind(verts_[verts_$name==edges_5[i, "from"],],
                 verts_[verts_$name==edges_5[i, "to"],]),
           "SpatialLines")
      })
      
      # for(i in seq_along(edges)){
      #   edges_[[i]]<-spChFIDs(edges_[[i]], as.character(i))
      # }
      for(i in seq_along(edges)){
        edges_1[[i]]<-spChFIDs(edges_1[[i]], as.character(i))
      }
      for(i in seq_along(edges)){
        edges_2[[i]]<-spChFIDs(edges_2[[i]], as.character(i))
      }
      for(i in seq_along(edges)){
        edges_3[[i]]<-spChFIDs(edges_3[[i]], as.character(i))
      }
      for(i in seq_along(edges)){
        edges_4[[i]]<-spChFIDs(edges_4[[i]], as.character(i))
      }
      for(i in seq_along(edges)){
        edges_5[[i]]<-spChFIDs(edges_5[[i]], as.character(i))
      }
      
      #edges_<-do.call(rbind, edges_)
      edges_1<-do.call(rbind, edges_1)
      edges_2<-do.call(rbind, edges_2)
      edges_3<-do.call(rbind, edges_3)
      edges_4<-do.call(rbind, edges_4)
      edges_5<-do.call(rbind, edges_5)
      
      leaflet()%>%
        setView(lat=40, lng=0, zoom=1.4)%>%
        addPolygons(data=dat_i(),weight=.5, color=pal2(), fillOpacity = .75, popup=pop_cont2(), 
                    highlightOptions = highlightOptions(color="white", weight=2, bringToFront = T, sendToBack = T), label=worldmap@data$SOVEREIGNT) %>%
        addEasyButton(easyButton(icon="fa-globe",title="Home",onClick=JS("function(btn, map){ map.setZoom(1.4); }"))) %>%
        addCircles(data=verts_, opacity=.2, fillColor = "blue", color="blue") %>%
        addPolylines(data=edges_1, color="#D6E5FF", opacity=.01, weight=.5)%>%
        addPolylines(data=edges_2, color="#B5C9EC", opacity=.03, weight=1)%>%
        addPolylines(data=edges_3, color="#8BA6D5", opacity = .05, weight=1.5) %>%
        addPolylines(data=edges_4, color="#708FC6", opacity = .07, weight=2.5) %>%
        addPolylines(data=edges_5, color="#05358B", opacity=.1, weight=5)
      
    } #end if(input==T)
  }) #these end the reactive that makes map_i
  
  output$leaf_c<-renderLeaflet(map_c())
  
  output$leaf_i<-renderLeaflet(map_i())

} #This one ends server

shinyApp(ui, server)