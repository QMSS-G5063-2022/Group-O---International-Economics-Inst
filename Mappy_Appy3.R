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

worldmap@data[worldmap@data$NAME_EN=="People's Republic of China",]$NAME_EN<-"China"
worldmap@data[worldmap@data$NAME_EN=="Ivory Coast",]$NAME_EN<-"Cote d'Ivoire"
worldmap@data[worldmap@data$NAME_EN=="Democratic Republic of the Congo",]$NAME_EN<-"Congo, Dem. Rep."
worldmap@data[worldmap@data$NAME_EN=="Republic of the Congo",]$NAME_EN<-"Congo, Rep."
worldmap@data[worldmap@data$NAME_EN=="Cape Verde",]$NAME_EN<-"Cabo Verde"
worldmap@data[worldmap@data$NAME_EN=="Egypt",]$NAME_EN<-"Egypt, Arab Rep."
worldmap@data[worldmap@data$NAME_EN=="Iran",]$NAME_EN<-"Iran, Islamic Rep."
worldmap@data[worldmap@data$NAME_EN=="Federated States of Micronesia",]$NAME_EN<-"Micronesia, Fed. Sts."
worldmap@data[worldmap@data$NAME_EN=="The Gambia",]$NAME_EN<-"Gambia, The"
worldmap@data[worldmap@data$NAME_EN=="Kyrgyzstan",]$NAME_EN<-"Kyrgyz Republic"
worldmap@data[worldmap@data$NAME_EN=="Saint Kitts and Nevis",]$NAME_EN<-"St. Kitts and Nevis"
worldmap@data[worldmap@data$NAME_EN=="Laos",]$NAME_EN<-"Lao PDR"
worldmap@data[worldmap@data$NAME_EN=="Russia",]$NAME_EN<-"Russian Federation"
worldmap@data[worldmap@data$NAME_EN=="Venezuela",]$NAME_EN<-"Venezuela, RB"
worldmap@data[worldmap@data$NAME_EN=="Yemen",]$NAME_EN<-"Yemen, Rep."
worldmap@data[worldmap@data$NAME_EN=="Saint Lucia",]$NAME_EN<-"St. Lucia"
worldmap@data[worldmap@data$NAME_EN==unique(worldmap@data$NAME_EN)[grep("Tom", unique(worldmap@data$NAME_EN))],]$NAME_EN<-"Sao Tome and Principe"
worldmap@data[worldmap@data$NAME_EN=="East Timor",]$NAME_EN<-"Timor-Leste"
worldmap@data[worldmap@data$NAME_EN=="Syria",]$NAME_EN<-"Syrian Arab Republic"
worldmap@data[worldmap@data$NAME_EN=="The Bahamas",]$NAME_EN<-"Bahamas, The"
worldmap@data[worldmap@data$NAME_EN=="Hong Kong",]$NAME_EN<-"Hong Kong SAR, China"
worldmap@data[worldmap@data$NAME_EN=="North Korea",]$NAME_EN<-"Korea, Dem. People's Rep."
worldmap@data[worldmap@data$NAME_EN=="South Korea",]$NAME_EN<-"Korea, Rep."
worldmap@data[worldmap@data$NAME_EN=="Saint Vincent and the Grenadines",]$NAME_EN<-"St. Vincent and the Grenadines"
worldmap@data[worldmap@data$NAME_EN=="United States of America",]$NAME_EN<-"United States"
worldmap@data[worldmap@data$NAME_EN=="Sint Maarten",]$NAME_EN<-"Sint Maarten (Dutch Part)"
worldmap@data[worldmap@data$NAME_EN=="Saint Martin",]$NAME_EN<-"Saint Martin (French Part)"
worldmap@data[worldmap@data$NAME_EN=="Slovakia",]$NAME_EN<-"Slovak Republic"
worldmap@data[worldmap@data$NAME_EN=="United States Virgin Islands",]$NAME_EN<-"Virgin Islands (U.S.)"

ui<-fluidPage(h1="Map Fun",
              sliderInput("year", label="Select Year", min=2005, max=2020, value=2015, sep=""),
              #radioButtons("which","Investment Source",choices=c("China","IMF")),
              selectInput("var","Select Variable to Map", choices=c("Investment", "GDP Growth", "Democracy Score")),
              radioButtons("view", "Select", choices=c("World","Africa","Asia","Australiasia","Europe","North America","South America"), selected="World", inline=T),
              #checkboxInput("net","Display Network"),
              #actionButton("button","Plot"),
              splitLayout(leafletOutput("leaf_c"), leafletOutput("leaf_i"))
              # leafletOutput("leaf_c"),
              # leafletOutput("leaf_i")
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
    
    if(input$var=="GDP Growth"){
      gdp<-WDI(country="all",indicator="NY.GDP.MKTP.KD.ZG", start=2005, extra=F)
      gdp$NY.GDP.MKTP.KD.ZG<-round(gdp$NY.GDP.MKTP.KD.ZG,2)
      gdp<-filter(gdp, !country %in% unique(gdp$country)[1:49])
      gdp<-rename(gdp, Year=year, name=country, gdp_growth=`NY.GDP.MKTP.KD.ZG`)
      #gdp$z<-(gdp$gdp_growth-mean(gdp$gdp_growth, na.rm=T))/sd(gdp$gdp_growth,na.rm = T)
      #gdp$mean_scaled<-(gdp$gdp_growth/mean(gdp$gdp_growth,na.rm=T))
      #gdp$min_max<-(gdp$gdp_growth-min(gdp$gdp_growth, na.rm=T))/(max(gdp$gdp_growth,na.rm=T)-min(gdp$gdp_growth,na.rm=T))
      gdp$growth_fac<-ifelse(-10>=gdp$gdp_growth,1, ifelse(gdp$gdp_growth<= -2.5 & gdp$gdp_growth>-10, 2, 
                                                           ifelse(gdp$gdp_growth<= 0 & gdp$gdp_growth>-2.5, 3, 
                                                                  ifelse(gdp$gdp_growth<= 2.5 & gdp$gdp_growth>0, 4, 
                                                                         ifelse(gdp$gdp_growth<= 5 & gdp$gdp_growth>2.5, 5, 
                                                                                ifelse(gdp$gdp_growth<= 10 & gdp$gdp_growth>5, 6, 
                                                                                       ifelse(gdp$gdp_growth>10, 7, 0)))))))
      
      gdp_y<-filter(gdp, Year==input$year)
      worldmap@data<-left_join(worldmap@data, gdp_y, by=c("NAME_EN"="name"))
    }else if(input$var=="Democracy Score"){
      library(vdemdata)
      dem<-filter(vdem[c("country_name","country_text_id","year","v2x_libdem")],year>2004)
      dem[dem$country_text_id=="XKX",]$country_text_id<-"KOS"
      dem[dem$country_text_id=="SSD",]$country_text_id<-"SDS"
      dem[dem$country_text_id=="PSE",]$country_text_id<-"PSX"
      dem[dem$country_text_id=="SML",]$country_text_id<-"SOL"
      dem$dem_fac<-ifelse(dem$v2x_libdem<=.1, 1, ifelse(dem$v2x_libdem<=.25 & dem$v2x_libdem>.1, 2,
                                                        ifelse(dem$v2x_libdem<=.5 & dem$v2x_libdem>.25, 3,
                                                               ifelse(dem$v2x_libdem<=.7 & dem$v2x_libdem>.5, 4, 
                                                                      ifelse(dem$v2x_libdem<=.9 & dem$v2x_libdem>.7, 5, 0)))))
      
      dem<-filter(rename(dem, Year=year, name=country_name, dem_score=v2x_libdem), Year==input$year)
      worldmap@data<-left_join(worldmap@data, dem, by=c("ADM0_A3"="country_text_id"))
    }
    worldmap
    
  }) # End reactive making dat_c
  
  dat_i<-reactive({
    imf<-WDI(indicator = "DT.DOD.DIMF.CD", country="all",start=2005, extra=F)
    imf<-filter(imf, !country %in% unique(imf$country)[1:49])
    imf<-rename(imf, Year=year, name=country, IMF_cred=DT.DOD.DIMF.CD)
    
    inv_y<-filter(imf, Year==input$year)[c("name","IMF_cred")]
    
    worldmap@data[worldmap@data$NAME_EN=="Brunei",]$NAME_EN<-"Brunei Darussalam"
    worldmap@data[worldmap@data$NAME_EN==unique(worldmap@data$NAME_EN)[grep("Cura",worldmap@data$NAME_EN)],]$NAME_EN<-"Curacao"
    
    worldmap@data<-left_join(worldmap@data, inv_y, by=c("NAME_EN"="name"))
    
    if(input$var=="GDP Growth"){
      gdp<-WDI(country="all",indicator="NY.GDP.MKTP.KD.ZG", start=2005, extra=F)
      gdp$NY.GDP.MKTP.KD.ZG<-round(gdp$NY.GDP.MKTP.KD.ZG,2)
      gdp<-filter(gdp, !country %in% unique(gdp$country)[1:49])
      gdp<-rename(gdp, Year=year, name=country, gdp_growth=`NY.GDP.MKTP.KD.ZG`)
      gdp$growth_fac<-ifelse(-10>=gdp$gdp_growth,1, ifelse(gdp$gdp_growth<= -2.5 & gdp$gdp_growth>-10, 2, 
                                                           ifelse(gdp$gdp_growth<= 0 & gdp$gdp_growth>-2.5, 3, 
                                                                  ifelse(gdp$gdp_growth<= 2.5 & gdp$gdp_growth>0, 4, 
                                                                         ifelse(gdp$gdp_growth<= 5 & gdp$gdp_growth>2.5, 5, 
                                                                                ifelse(gdp$gdp_growth<= 10 & gdp$gdp_growth>5, 6, 
                                                                                       ifelse(gdp$gdp_growth>10, 7, 0)))))))
      
      gdp_y<-filter(gdp, Year==input$year)
      worldmap@data<-left_join(worldmap@data, gdp_y, by=c("NAME_EN"="name"))
    } else if(input$var=="Democracy Score"){  #ends if(var=="GDP Growth)
      library(vdemdata)
      dem<-filter(vdem[c("country_name","country_text_id","year","v2x_libdem")],year>2004)
      dem[dem$country_text_id=="XKX",]$country_text_id<-"KOS"
      dem[dem$country_text_id=="SSD",]$country_text_id<-"SDS"
      dem[dem$country_text_id=="PSE",]$country_text_id<-"PSX"
      dem[dem$country_text_id=="SML",]$country_text_id<-"SOL"
      dem$dem_fac<-ifelse(dem$v2x_libdem<=.1, 1, ifelse(dem$v2x_libdem<=.25 & dem$v2x_libdem>.1, 2,
                                                        ifelse(dem$v2x_libdem<=.5 & dem$v2x_libdem>.25, 3,
                                                               ifelse(dem$v2x_libdem<=.7 & dem$v2x_libdem>.5, 4, 
                                                                      ifelse(dem$v2x_libdem<=.9 & dem$v2x_libdem>.7, 5, 0)))))
      
      dem<-filter(rename(dem, Year=year, name=country_name, dem_score=v2x_libdem), Year==input$year)
      worldmap@data<-left_join(worldmap@data, dem, by=c("ADM0_A3"="country_text_id"))
    } #ends if(var==Democracy)  
    worldmap
  }) #These end the reactive that makes dat_i
  
  pop_cont1<-reactive({
    f<-dat_c()@data$inv_tot
    f[is.na(f)]<-0
    if(input$var=="Investment"){
      paste0("Country: ", dat_c()@data$SOVEREIGNT, "<br>",
             "Investment from China (Million USD): $", f)
    }else if(input$var=="GDP Growth"){
      paste0("Country: ", dat_c()@data$SOVEREIGNT, "<br>",
             "Investment from China (Million USD): $", f, "<br>",
             "GDP Growth: ", dat_c()@data$gdp_growth)
    }else if(input$var=="Democracy Score"){
      paste0("Country: ", dat_c()@data$SOVEREIGNT, "<br>",
             "Investment from China (Million USD): $", f, "<br>",
             "Democracy Score: ", dat_c()@data$dem_score)
    }
  }) #end reactive defining pop_cont1
  
  pop_cont2<-reactive({
    f<-dat_i()@data$IMF_cred
    f[is.na(f)]<-0
    if(input$var=="Investment"){
      paste0("Country: ", dat_i()@data$SOVEREIGNT, "<br>",
             "IMF Investment (Million USD): $", round(f/1000000, 0))
    }else if(input$var=="GDP Growth"){
      paste0("Country: ", dat_c()@data$SOVEREIGNT, "<br>",
             "Investment from China (Million USD): $", f, "<br>",
             "GDP Growth: ", dat_c()@data$gdp_growth)
    }else if(input$var=="Democracy Score"){
      paste0("Country: ", dat_c()@data$SOVEREIGNT, "<br>",
             "Investment from China (Million USD): $", f, "<br>",
             "Democracy Score: ", dat_c()@data$dem_score)
    }
  }) # end reactive that makes pop_cont2
  
  pal1<-reactive({
    if(input$var=="Investment"){
      j<-colorNumeric(palette="Reds", domain=dat_c()@data$inv_tot)
      j(dat_c()@data$inv_tot)
    }else if(input$var=="GDP Growth"){
      j<-colorFactor(palette="BrBG", domain=dat_c()@data$growth_fac)
      j(dat_c()@data$growth_fac)
    }else if(input$var=="Democracy Score"){
      j<-colorFactor(palette="BrBG", domain=dat_c()@data$dem_fac)
      j(dat_c()@data$dem_fac)
    }
  }) #end making pal1
  
  pal2<-reactive({
    if(input$var=="Investment"){
      j<-colorNumeric(palette="Blues", domain=dat_i()@data$IMF_cred)
      j(dat_i()@data$IMF_cred)
    }else if(input$var=="GDP Growth"){
      j<-colorFactor(palette="BrBG", domain=dat_i()@data$growth_fac)
      j(dat_c()@data$growth_fac)
    }else if(input$var=="Democracy Score"){
      j<-colorFactor(palette="BrBG", domain=dat_i()@data$dem_fac)
      j(dat_c()@data$dem_fac)
    }
  }) #finish making pal2
  
  lat_re<-reactive({
    if(input$view=="World"){40}
    else if(input$view=="Africa"){5}
    else if(input$view=="Asia"){40}
    else if(input$view=="Australiasia"){-25}
    else if(input$view=="Europe"){50}
    else if(input$view=="North America"){45}
    else if(input$view=="South America"){-15}
  })
  
  lng_re<-reactive({
    if(input$view=="World"){0}
    else if(input$view=="Africa"){25}
    else if(input$view=="Asia"){90}
    else if(input$view=="Australiasia"){135}
    else if(input$view=="Europe"){15}
    else if(input$view=="North America"){-100}
    else if(input$view=="South America"){-65}
  })
  
  zoom_re<-reactive({
    if(input$view=="World"){1.4}
    else if(input$view=="Africa"){2.5}
    else if(input$view=="Asia"){2.5}
    else if(input$view=="Australiasia"){3}
    else if(input$view=="Europe"){4}
    else if(input$view=="North America"){2.5}
    else if(input$view=="South America"){2.5}
  })
  
  map_c<-reactive({
    if(input$var=="Investment"){
      leaflet() %>%
        setView(lat=lat_re(), lng=lng_re(), zoom=zoom_re())%>%
        addPolygons(data=dat_c(), weight=.5, fillOpacity = .75, fillColor = pal1(),
                    highlightOptions = highlightOptions(color="white", weight=2, bringToFront = T, sendToBack = T),
                    popup=pop_cont1(), label=dat_c()@data$SOVEREIGNT) %>%
        #leaflet::addLegend(pal=pal1(), values = dat_c()@data$inv_tot)
        addEasyButton(easyButton(icon="fa-globe",title="Home",onClick=JS("function(btn, map){ map.setZoom(1.4); }")))
    } else { #ends if(input$net==F)
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
      
      verts_<-left_join(verts_, h[c("TO", "inv_tot")], by=c("name"="TO"))
      
      leaflet()%>%
        setView(lat=lat_re(), lng=lng_re(), zoom=zoom_re())%>%
        addPolygons(data=dat_c(),weight=.5, color=pal1(), fillOpacity = .75, popup=pop_cont1(), 
                    highlightOptions = highlightOptions(color="white", weight=2, bringToFront = T, sendToBack = T), label=worldmap@data$SOVEREIGNT) %>%
        #addLegend(pal=pal1(), values=dat_c()@data$growth_fac) %>%
        addEasyButton(easyButton(icon="fa-globe",title="Home",onClick=JS("function(btn, map){ map.setZoom(1.4); }"))) %>%
        addCircles(data=verts_, opacity=.2, fillColor = "red", color="red", weight=2, radius=~inv_tot*10, lat = ~lat, lng = ~lon)

    } #ends if(net==T)
  }) #these end the reactive that makes map_c
  
  map_i<-reactive({
    if(input$var=="Investment"){
      leaflet() %>%
        setView(lat=lat_re(), lng=lng_re(), zoom=zoom_re())%>%
        addPolygons(data=dat_i(), weight=.5, fillOpacity = .75, fillColor = pal2(),
                    highlightOptions = highlightOptions(color="white", weight=2, bringToFront = T, sendToBack = T),
                    popup=pop_cont2(), label=dat_i()@data$SOVEREIGNT) %>%
        addEasyButton(easyButton(icon="fa-globe",title="Home",onClick=JS("function(btn, map){ map.setZoom(1.4); }")))
    }  else{  #ends if(input$net==F)
      rawnodes<-read.csv('http://www.kateto.net/wordpress/wp-content/uploads/2015/06/Country_terms_FREQ.csv')
      rawedges<-read.csv('http://www.kateto.net/wordpress/wp-content/uploads/2015/06/Country_terms_COOC.csv')
      
      imf<-WDI(indicator = "DT.DOD.DIMF.CD", country="all",start=2005, extra=F)
      imf<-filter(imf, !country %in% unique(imf$country)[1:49])
      imf<-rename(imf, Year=year, name=country, IMF_cred=DT.DOD.DIMF.CD)
      
      worldmap@data[worldmap@data$NAME_EN=="Brunei",]$NAME_EN<-"Brunei Darussalam"
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
      nodes<-rbind(nodes, c("american samoa", round(-170.5794322853497,6), round(-14.26012915103479,6)))
      nodes<-rbind(nodes, c("comoros", round(43.48504853332502,6),round(-11.862176383838756,6)))
      nodes<-rbind(nodes,c("congo, rep.", round(16.052629791985225,6), round(-0.07691295959726356,6)))
      nodes<-rbind(nodes,c("congo, dem. rep.", round(23.84214068136281,6), round(-3.2141630381290125,6)))
      nodes<-rbind(nodes,c("cote d'ivoire", round(-5.529500409752393,6),round(7.253827157140813,6)))
      nodes<-rbind(nodes,c("eswatini", round(31.526906430335316,6), round(-26.50532674658614, 6)))
      nodes<-rbind(nodes,c("guinea-bissau", round(-15.11433094944025,6),round(11.849168222767416,6)))
      nodes<-rbind(nodes, c("sao tome and principe", round(6.637044051405797,6), round(0.3877357338129725, 6)))
      nodes<-rbind(nodes, c("st. lucia", round(-60.981555430152405,6), round(13.890030848573293,6)))
      nodes<-rbind(nodes, c("st. vincent and the grenadines", round(-61.25999737205744,6), round(12.990774451382883,6)))
      nodes<-rbind(nodes, c("timor-leste", round(126.04154797375384,6), round(-8.779762146492265,6)))
      nodes<-rbind(nodes, c("st. kitts and nevis", round(-62.65150902029912, 6), round(17.2450959410363,6)))
      
      nodes[nodes$name=="uk",]$name<-"united kingdom"
      nodes[nodes$name=="uae",]$name<-"united arab emirates"
      nodes[nodes$name=="brunei",]$name<-"brunei darussalam"
      nodes[nodes$name=="macedonia",]$name<-"north macedonia"
      nodes[nodes$name=="burma",]$name<-"myanmar"
      nodes[nodes$name=="bahamas",]$name<-"bahamas, the"
      nodes[nodes$name=="cape verde",]$name<-"cabo verde"
      nodes[nodes$name=="egypt",]$name<-"egypt, arab rep."
      nodes[nodes$name=="hong kong",]$name<-"hong kong sar, china"
      nodes[nodes$name=="north korea",]$name<-"korea, dem. people's rep."
      nodes[nodes$name=="south korea",]$name<-"korea, rep."
      nodes[nodes$name=="kyrgyzstan",]$name<-"kyrgyz republic"
      nodes[nodes$name=="laos",]$name<-"lao pdr"
      nodes[nodes$name=="gambia",]$name<-"gambia, the"
      nodes[nodes$name=="micronesia",]$name<-"micronesia, fed. sts."
      nodes[nodes$name=="russia",]$name<-"russian federation"
      nodes[nodes$name=="syria",]$name<-"syrian arab republic"
      nodes[nodes$name=="venezuela",]$name<-"venezuela, rb"
      nodes[nodes$name=="yemen",]$name<-"yemen, rep."
      nodes[nodes$name=="usa",]$name<-"united states"
      nodes[nodes$name=="iran",]$name<-"iran, islamic rep."
      nodes[nodes$name=="slovakia",]$name<-"slovak republic"
      
      nodes<-filter(nodes, name %in% c(filter(net_dat, Year==input$year & !is.na(IMF_cred))$Target,"imf"))
      
      nodes$id<-as.integer(rownames(nodes))
      nodes<-nodes[c(4,2,3,1)]
      
      h<-dplyr::rename(filter(net_dat,Year==input$year & !is.na(IMF_cred))[c("Source", "Target", "IMF_cred")], FROM=Source, TO=Target)
      
      u<-nodes[c("name", "lat","lon")]
      u[u$name=="maldives",]$lat<-(4.222821)
      u[u$name=="maldives",]$lon<-73.153334
      network_<-get.data.frame(graph.data.frame(h, directed=T, vertices=u), "both")
      verts_<-network_$vertices
      verts_$lat<-as.numeric(verts_$lat)
      verts_$lon<-as.numeric(verts_$lon)
      verts_<-left_join(verts_, h[c("TO","IMF_cred")], by=c("name"="TO"))
      
      leaflet()%>%
        setView(lat=lat_re(), lng=lng_re(), zoom=zoom_re())%>%
        addPolygons(data=dat_i(),weight=.5, color=pal2(), fillOpacity = .75, popup=pop_cont2(), 
                    highlightOptions = highlightOptions(color="white", weight=2, bringToFront = T, sendToBack = T), label=worldmap@data$SOVEREIGNT) %>%
        addEasyButton(easyButton(icon="fa-globe",title="Home",onClick=JS("function(btn, map){ map.setZoom(1.4); }"))) %>%
        addCircles(data=verts_, opacity=.2, fillColor = "blue", color="blue", weight=2, radius =~IMF_cred/100000, lat = ~lat, lng = ~lon)
      
    } #end if(input==T)
  }) #these end the reactive that makes map_i
  
  output$leaf_c<-renderLeaflet(map_c())
  
  output$leaf_i<-renderLeaflet(map_i())
  
} #This one ends server

shinyApp(ui, server)