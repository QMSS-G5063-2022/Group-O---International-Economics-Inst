suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(rgdal))
suppressPackageStartupMessages(library(WDI))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(bslib))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggthemes))
suppressPackageStartupMessages(library(network))
suppressPackageStartupMessages(library(maps))
suppressPackageStartupMessages(library(igraph))
suppressPackageStartupMessages(library(plotly))

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

worldmap = readOGR(dsn="Nat_world_map", layer="ne_50m_admin_0_countries")
worldmap@data[worldmap@data$CONTINENT=="Oceania",]$CONTINENT<-"Australiasia"

rawnodes<-read.csv('http://www.kateto.net/wordpress/wp-content/uploads/2015/06/Country_terms_FREQ.csv')
rawedges<-read.csv('http://www.kateto.net/wordpress/wp-content/uploads/2015/06/Country_terms_COOC.csv')

library(vdemdata)
dem<-filter(vdem[c("country_name","country_text_id","year","v2x_libdem", "v2x_freexp")],year>2004)

gdp<-WDI(country="all",indicator="NY.GDP.MKTP.KD.ZG", start=2005, extra=F)
gini<-WDI(country="all",indicator="SI.POV.GINI", start=2005, extra=F)
n<-WDI(indicator = "SN.ITK.DEFC.ZS", start=2005) #I named this stupidly. It's undernourishment data
imf<-WDI(indicator = "DT.DOD.DIMF.CD", country="all",start=2005, extra=F)
unemp<-WDI(indicator="JI.UEM.1564.ZS", country="all", start=2005, extra=F)

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

ui<-navbarPage("", theme = bs_theme(bootswatch = "flatly"),
               tabPanel("Home",
                        titlePanel(h1("Welcome", align = "center")),
                        fluidPage(
                          column(4, offset=4, textOutput("blurb")),
                          column(3, textOutput("placeholder1")),
                          column(3, offset=9, textOutput("placeholder2")))),
               tabPanel("Economics", 
                        fluidPage(
                          tags$h1("Economic Indicators"),
                          sliderInput("year", label="Select Year", min=2005, max=2020, value=2015, sep=""),
                          selectInput("var_e","Select Variable to Map", choices=c("Investment", "GDP Growth", "Gini", "Unemployment")),
                          radioButtons("view", "Select", choices=c("World","Africa","Asia","Australiasia","Europe","North America","South America"), selected="World", inline=T),
                          splitLayout(leafletOutput("leaf_c"), leafletOutput("leaf_i")),
                          splitLayout(plotlyOutput("scatter_c"), plotlyOutput("scatter_i"))
                        )),
               tabPanel("Human Rights",
                        fluidPage(
                          tags$h1("Human Rights Indicators"),
                          sliderInput("year_h", label="Select Year", min=2005, max=2020, value=2015, sep=""),
                          selectInput("var_h","Select Variable to Map", choices=c("Undernourishment Prevelance" ,"Democracy Score", "Freedom of Expression")),
                          radioButtons("view_h", "Select", choices=c("World","Africa","Asia","Australiasia","Europe","North America","South America"), selected="World", inline=T),
                          splitLayout(leafletOutput("leaf_c2"), leafletOutput("leaf_i2")),
                          splitLayout(plotlyOutput("scatter_ch"), plotlyOutput("scatter_ih"))
                        ))
)

server<-function(input, output, session){
  
  observe({
    val<-input$year_h
    updateSliderInput(session, "year", value=val)
  })
  observe({
    val<-input$view_h
    updateSliderInput(session, "view", value=val)
  })
  
  dat_c<-reactive({
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
    
    if(input$var_e=="GDP Growth"){
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
    }else if(input$var_e=="Gini"){
      gini<-filter(gini, !country %in% unique(gini$country)[1:49])
      gini<-rename(gini, Year=year, name=country, gini=`SI.POV.GINI`)
      gini$gini_fac<-ifelse(30>=gini$gini, 1, ifelse(gini$gini>30 & gini$gini<=35, 2,
                                                     ifelse(gini$gini>35 & gini$gini<=40, 3,
                                                            ifelse(gini$gini>40 & gini$gini<=50, 4, 
                                                                   ifelse(gini$gini>50, 5, 0)))))
      gini_y<-filter(gini, Year==input$year)
      worldmap@data<-left_join(worldmap@data, gini_y, by=c("NAME_EN"="name"))
    }else if(input$var_e=="Unemployment"){
      unemp<-rename(unemp, Year=year, name=country, unemp=`JI.UEM.1564.ZS`)
      unemp$unemp_fac<-ifelse(unemp$unemp<=3, 1, ifelse(unemp$unemp>3 & unemp$unemp<=4.5, 2, 
                                                        ifelse(unemp$unemp>4.5 & unemp$unemp<=7, 3,
                                                               ifelse(unemp$unemp>7 & unemp$unemp<=10, 4,
                                                                      ifelse(unemp$unemp>10 & unemp$unemp<=12, 5, 
                                                                             ifelse(unemp$unemp>12 & unemp$unemp<=20, 6, 
                                                                                    ifelse(unemp$unemp>20, 7, 0)))))))
      
      unemp<-filter(unemp, Year==input$year)
      worldmap@data<-left_join(worldmap@data, unemp, by=c("NAME_EN"="name"))
    }
    
    if(input$var_h=="Democracy Score"){
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
   
    }else if(input$var_h=="Undernourishment Prevelance"){
      n<-filter(n, !country %in% unique(n$country)[1:49])
      n<-rename(n, Year=year, name=country, undernourishment=`SN.ITK.DEFC.ZS`)
      n$nourish_fac<-ifelse(n$undernourishment<=3.5, 1, ifelse(n$undernourishment>3.5 & n$undernourishment<=5, 2,
                                                               ifelse(n$undernourishment>5 & n$undernourishment<=7, 3, 
                                                                      ifelse(n$undernourishment>7 & n$undernourishment<=11, 4,
                                                                             ifelse(n$undernourishment>11& n $undernourishment<=15, 5,
                                                                                    ifelse(n$undernourishment>15& n$undernourishment<=20, 6, 
                                                                                           ifelse(n$undernourishment>20 & n$undernourishment<30, 7,
                                                                                                  ifelse(n$undernourishment>30, 8, 0))))))))
      nourish_y<-filter(n, Year==input$year)
      worldmap@data<-left_join(worldmap@data, nourish_y, by=c("NAME_EN"="name"))
    }else if(input$var_h=="Freedom of Expression"){
      dem[dem$country_text_id=="XKX",]$country_text_id<-"KOS"
      dem[dem$country_text_id=="SSD",]$country_text_id<-"SDS"
      dem[dem$country_text_id=="PSE",]$country_text_id<-"PSX"
      dem[dem$country_text_id=="SML",]$country_text_id<-"SOL"
      dem$exp_fac<-ifelse(dem$v2x_freexp<=.05, 1, ifelse(dem$v2x_freexp>.05 & dem$v2x_freexp<=.15, 2,
                                                         ifelse(dem$v2x_freexp>.15 & dem$v2x_freexp<=.25, 3, 
                                                                ifelse(dem$v2x_freexp>.25 & dem$v2x_freexp<=.4, 4,
                                                                       ifelse(dem$v2x_freexp>.4 & dem$v2x_freexp<=.6, 5, 
                                                                              ifelse(dem$v2x_freexp>.6 & dem$v2x_freexp<=.75, 6,
                                                                                     ifelse(dem$v2x_freexp>.75 & dem$v2x_freexp<=.9, 7,
                                                                                            ifelse(dem$v2x_freexp>.9, 8,0))))))))
      
      dem<-filter(rename(dem, Year=year, name=country_name, dem_score=v2x_libdem, exp=v2x_freexp), Year==input$year)
      worldmap@data<-left_join(worldmap@data, dem, by=c("ADM0_A3"="country_text_id"))
    }
    worldmap
    
  }) # End reactive making dat_c
  
  dat_i<-reactive({
    imf<-filter(imf, !country %in% unique(imf$country)[1:49])
    imf<-rename(imf, Year=year, name=country, IMF_cred=DT.DOD.DIMF.CD)
    
    inv_y<-filter(imf, Year==input$year)[c("name","IMF_cred")]
    
    worldmap@data[worldmap@data$NAME_EN=="Brunei",]$NAME_EN<-"Brunei Darussalam"
    worldmap@data[worldmap@data$NAME_EN==unique(worldmap@data$NAME_EN)[grep("Cura",worldmap@data$NAME_EN)],]$NAME_EN<-"Curacao"
    
    worldmap@data<-left_join(worldmap@data, inv_y, by=c("NAME_EN"="name"))
    
    if(input$var_e=="GDP Growth"){
      #gdp<-WDI(country="all",indicator="NY.GDP.MKTP.KD.ZG", start=2005, extra=F)
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
    } else if(input$var_e=="Gini"){
      #gini<-WDI(country="all",indicator="SI.POV.GINI", start=2005, extra=F)
      gini<-filter(gini, !country %in% unique(gini$country)[1:49])
      gini<-rename(gini, Year=year, name=country, gini=`SI.POV.GINI`)
      gini$gini_fac<-ifelse(30>=gini$gini, 1, ifelse(gini$gini>30 & gini$gini<=35, 2,
                                                     ifelse(gini$gini>35 & gini$gini<=40, 3,
                                                            ifelse(gini$gini>40 & gini$gini<=50, 4, 
                                                                   ifelse(gini$gini>50, 5, 0)))))
      gini_y<-filter(gini, Year==input$year)
      worldmap@data<-left_join(worldmap@data, gini_y, by=c("NAME_EN"="name"))
    }else if(input$var_e=="Unemployment"){
      unemp<-rename(unemp, Year=year, name=country, unemp=`JI.UEM.1564.ZS`)
      unemp$unemp_fac<-ifelse(unemp$unemp<=3, 1, ifelse(unemp$unemp>3 & unemp$unemp<=4.5, 2, 
                                                        ifelse(unemp$unemp>4.5 & unemp$unemp<=7, 3,
                                                               ifelse(unemp$unemp>7 & unemp$unemp<=10, 4,
                                                                      ifelse(unemp$unemp>10 & unemp$unemp<=12, 5, 
                                                                             ifelse(unemp$unemp>12 & unemp$unemp<=20, 6, 
                                                                                    ifelse(unemp$unemp>20, 7, 0)))))))
      
      unemp<-filter(unemp, Year==input$year)
      worldmap@data<-left_join(worldmap@data, unemp, by=c("NAME_EN"="name"))
    }
    if(input$var_h=="Undernourishment Prevelance"){
      #n<-WDI(indicator = "SN.ITK.DEFC.ZS", start=2005)
      n<-filter(n, !country %in% unique(n$country)[1:49])
      n<-rename(n, Year=year, name=country, undernourishment=`SN.ITK.DEFC.ZS`)
      n$nourish_fac<-ifelse(n$undernourishment<=3.5, 1, ifelse(n$undernourishment>3.5 & n$undernourishment<=5, 2,
                                                               ifelse(n$undernourishment>5 & n$undernourishment<=7, 3, 
                                                                      ifelse(n$undernourishment>7 & n$undernourishment<=11, 4,
                                                                             ifelse(n$undernourishment>11& n $undernourishment<=15, 5,
                                                                                    ifelse(n$undernourishment>15& n$undernourishment<=20, 6, 
                                                                                           ifelse(n$undernourishment>20 & n$undernourishment<30, 7,
                                                                                                  ifelse(n$undernourishment>30, 8, 0))))))))
      nourish_y<-filter(n, Year==input$year)
      worldmap@data<-left_join(worldmap@data, nourish_y, by=c("NAME_EN"="name"))
    }else if(input$var_h=="Democracy Score"){ 
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
    }else if(input$var_h=="Freedom of Expression"){
      dem[dem$country_text_id=="XKX",]$country_text_id<-"KOS"
      dem[dem$country_text_id=="SSD",]$country_text_id<-"SDS"
      dem[dem$country_text_id=="PSE",]$country_text_id<-"PSX"
      dem[dem$country_text_id=="SML",]$country_text_id<-"SOL"
      dem$exp_fac<-ifelse(dem$v2x_freexp<=.05, 1, ifelse(dem$v2x_freexp>.05 & dem$v2x_freexp<=.15, 2,
                                                         ifelse(dem$v2x_freexp>.15 & dem$v2x_freexp<=.25, 3, 
                                                                ifelse(dem$v2x_freexp>.25 & dem$v2x_freexp<=.4, 4,
                                                                       ifelse(dem$v2x_freexp>.4 & dem$v2x_freexp<=.6, 5, 
                                                                              ifelse(dem$v2x_freexp>.6 & dem$v2x_freexp<=.75, 6,
                                                                                     ifelse(dem$v2x_freexp>.75 & dem$v2x_freexp<=.9, 7,
                                                                                            ifelse(dem$v2x_freexp>.9, 8,0))))))))
      
      dem<-filter(rename(dem, Year=year, name=country_name, dem_score=v2x_libdem, exp=v2x_freexp), Year==input$year)
      worldmap@data<-left_join(worldmap@data, dem, by=c("ADM0_A3"="country_text_id"))
    }
    worldmap
  }) #These end the reactive that makes dat_i
  
  pop_cont1<-reactive({
    f<-dat_c()@data$inv_tot
    f[is.na(f)]<-0
    if(input$var_e=="Investment"){
      paste0("Country: ", dat_c()@data$SOVEREIGNT, "<br>",
             "Investment from China (Million USD): $", f)
    }else if(input$var_e=="GDP Growth"){
      paste0("Country: ", dat_c()@data$SOVEREIGNT, "<br>",
             "Investment from China (Million USD): $", f, "<br>",
             "GDP Growth: ", dat_c()@data$gdp_growth, "%")
    }else if(input$var_e=="Gini"){
      paste0("Country: ", dat_c()@data$SOVEREIGNT, "<br>",
             "Investment from China (Million USD): $", f, "<br>",
             "Gini Coefficient: ", dat_c()@data$gini)
    }else if(input$var_e=="Unemployment"){
      paste0("Country: ", dat_c()@data$SOVEREIGNT, "<br>",
             "Investment from China (Million USD): $", f, "<br>",
             "Unemployment Rate: ", round(dat_c()@data$unemp,2), "%")
    }
    })#end reactive defining pop_cont1
  pop_cont1_h<-reactive({
    f<-dat_c()@data$inv_tot
    f[is.na(f)]<-0
    if(input$var_h=="Undernourishment Prevelance"){
      paste0("Country: ", dat_c()@data$SOVEREIGNT, "<br>",
             "Investment from China (Million USD): $", f, "<br>",
             "Undernourishment Rate: ", dat_c()@data$undernourishment)
    }else if(input$var_h=="Democracy Score"){
       paste0("Country: ", dat_c()@data$SOVEREIGNT, "<br>",
              "Investment from China (Million USD): $", f, "<br>",
              "Democracy Score: ", dat_c()@data$dem_score)
    }else if(input$var_h=="Freedom of Expression"){
      paste0("Country: ", dat_c()@data$SOVEREIGNT, "<br>",
             "Investment from China (Million USD): $", f, "<br>",
             "Freedom of Expression Score: ", dat_c()@data$exp)
    }
  }) #end reactive defining pop_cont1_h
  
  pop_cont2<-reactive({
    f<-dat_i()@data$IMF_cred
    f[is.na(f)]<-0
    if(input$var_e=="Investment"){
      paste0("Country: ", dat_i()@data$SOVEREIGNT, "<br>",
             "IMF Investment (Million USD): $", round(f/1000000, 0))
    }else if(input$var_e=="GDP Growth"){
      paste0("Country: ", dat_i()@data$SOVEREIGNT, "<br>",
             "Investment from IMF (Million USD): $", round(f/1000000, 0), "<br>",
             "GDP Growth: ", dat_c()@data$gdp_growth)
    }else if(input$var_e=="Gini"){
      paste0("Country: ", dat_c()@data$SOVEREIGNT, "<br>",
             "Investment from IMF (Million USD): $", round(f/1000000, 0), "<br>",
             "Gini Coefficient: ", dat_i()@data$gini)
    }else if(input$var_e=="Unemployment"){
      paste0("Country: ", dat_c()@data$SOVEREIGNT, "<br>",
             "Investment from IMF (Million USD): $", round(f/1000000, 0), "<br>",
             "Unemployment Rate: ", round(dat_i()@data$unemp,2), "%")
    }
    })
  pop_cont2_h<-reactive({
    f<-dat_i()@data$IMF_cred
    f[is.na(f)]<-0
    if(input$var_h=="Democracy Score"){
      paste0("Country: ", dat_i()@data$SOVEREIGNT, "<br>",
             "Investment from IMF (Million USD): $", round(f/1000000, 0), "<br>",
             "Democracy Score: ", dat_c()@data$dem_score)
    }else if(input$var_h=="Undernourishment Prevelance"){
      paste0("Country: ", dat_i()@data$SOVEREIGNT, "<br>",
             "Investment from IMF (Million USD): $", round(f/1000000, 0), "<br>",
             "Undernourishment Rate: ", dat_i()@data$undernourishment)
    }else if(input$var_h=="Freedom of Expression"){
      paste0("Country: ", dat_c()@data$SOVEREIGNT, "<br>",
             "Investment from IMF (Million USD): $", f, "<br>",
             "Freedom of Expression Score: ", dat_i()@data$exp)
    }
  }) # end reactive that makes pop_cont2
  
  pal1<-reactive({
    if(input$var_e=="Investment"){
      j<-colorNumeric(palette="Reds", domain=dat_c()@data$inv_tot)
      j(dat_c()@data$inv_tot)
    }else if(input$var_e=="GDP Growth"){
      j<-colorFactor(palette="BrBG", domain=dat_c()@data$growth_fac)
      j(dat_c()@data$growth_fac)
    }else if(input$var_e=="Gini"){
      j<-colorFactor(palette="BrBG", domain=dat_c()@data$gini_fac)
      j(dat_c()@data$gini_fac)
    }else if(input$var_e=="Unemployment"){
      j<-colorFactor(palette="BrBG", domain=dat_c()@data$unemp_fac)
      j(dat_c()@data$unemp_fac)
    }
    })
  pal1_h<-reactive({
    if(input$var_h=="Undernourishment Prevelance"){
      j<-colorFactor(palette="BrBG", domain=dat_c()@data$nourish_fac)
      j(dat_c()@data$nourish_fac)
    }else if(input$var_h=="Democracy Score"){
      j<-colorFactor(palette="BrBG", domain=dat_c()@data$dem_fac)
      j(dat_c()@data$dem_fac)
    }else if(input$var_h=="Freedom of Expression"){
      j<-colorFactor(palette="BrBG", domain=dat_c()@data$exp_fac)
      j(dat_c()@data$exp_fac)
    }
  }) 
  
  pal2<-reactive({
    if(input$var_e=="Investment"){
      j<-colorNumeric(palette="Blues", domain=dat_i()@data$IMF_cred)
      j(dat_i()@data$IMF_cred)
    }else if(input$var_e=="GDP Growth"){
      j<-colorFactor(palette="BrBG", domain=dat_i()@data$growth_fac)
      j(dat_i()@data$growth_fac)
    }else if(input$var_e=="Gini"){
      j<-colorFactor(palette="BrBG", domain=dat_i()@data$gini_fac)
      j(dat_i()@data$gini_fac)
    }else if(input$var_e=="Unemployment"){
      j<-colorFactor(palette="BrBG", domain=dat_i()@data$unemp_fac)
      j(dat_i()@data$unemp_fac)
    }
  })
  pal2_h<-reactive({
    if(input$var_h=="Democracy Score"){
      j<-colorFactor(palette="BrBG", domain=dat_i()@data$dem_fac)
      j(dat_i()@data$dem_fac)
    }else if(input$var_h=="Undernourishment Prevelance"){
      j<-colorFactor(palette="BrBG", domain=dat_i()@data$nourish_fac)
      j(dat_i()@data$nourish_fac)
    }else if(input$var_h=="Freedom of Expression"){
      j<-colorFactor(palette="BrBG", domain=dat_i()@data$exp_fac)
      j(dat_i()@data$exp_fac)
    }
  })
  
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
    if(input$var_e=="Investment"){
      leaflet() %>%
        setView(lat=lat_re(), lng=lng_re(), zoom=zoom_re())%>%
        addPolygons(data=dat_c(), weight=.5, fillOpacity = .75, fillColor = pal1(), color = "black",
                    highlightOptions = highlightOptions(color="white", weight=2, bringToFront = T, sendToBack = T),
                    popup=pop_cont1(), label=dat_c()@data$SOVEREIGNT) %>%
        #leaflet::addLegend(pal=pal1(), values = dat_c()@data$inv_tot)
        addEasyButton(easyButton(icon="fa-globe",title="Home",onClick=JS("function(btn, map){ map.setZoom(1.4); }")))
    } else { 
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
        addPolygons(data=dat_c(),weight=.5, color="black", fillColor=pal1(), fillOpacity = .75, popup=pop_cont1(), 
                    highlightOptions = highlightOptions(color="white", weight=2, bringToFront = T, sendToBack = T), label=worldmap@data$SOVEREIGNT) %>%
        #addLegend(pal=pal1(), values=dat_c()@data$growth_fac) %>%
        addEasyButton(easyButton(icon="fa-globe",title="Home",onClick=JS("function(btn, map){ map.setZoom(1.4); }"))) %>%
        addCircles(data=verts_, opacity=.2, fillColor = "red", color="red", weight=2, radius=~inv_tot*10, lat = ~lat, lng = ~lon)

    } #ends if(net==T)
  }) #these end the reactive that makes map_c
  
  map_c2<-reactive({
    
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
      addPolygons(data=dat_c(),weight=.5, color="black", fillColor=pal1_h(), fillOpacity = .75, popup=pop_cont1_h(), 
                  highlightOptions = highlightOptions(color="white", weight=2, bringToFront = T, sendToBack = T), label=worldmap@data$SOVEREIGNT) %>%
      #addLegend(pal=pal1(), values=dat_c()@data$growth_fac) %>%
      addEasyButton(easyButton(icon="fa-globe",title="Home",onClick=JS("function(btn, map){ map.setZoom(1.4); }"))) %>%
      addCircles(data=verts_, opacity=.2, fillColor = "red", color="red", weight=2, radius=~inv_tot*10, lat = ~lat, lng = ~lon)
    
  })
  
  map_i<-reactive({
    if(input$var_e=="Investment"){
      leaflet() %>%
        setView(lat=lat_re(), lng=lng_re(), zoom=zoom_re())%>%
        addPolygons(data=dat_i(), weight=.5, color="black", fillOpacity = .75, fillColor = pal2(),
                    highlightOptions = highlightOptions(color="white", weight=2, bringToFront = T, sendToBack = T),
                    popup=pop_cont2(), label=dat_i()@data$SOVEREIGNT) %>%
        addEasyButton(easyButton(icon="fa-globe",title="Home",onClick=JS("function(btn, map){ map.setZoom(1.4); }")))
    }  else{  
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
        addPolygons(data=dat_i(),weight=.5, color="black", fillColor=pal2(), fillOpacity = .75, popup=pop_cont2(), 
                    highlightOptions = highlightOptions(color="white", weight=2, bringToFront = T, sendToBack = T), label=worldmap@data$SOVEREIGNT) %>%
        addEasyButton(easyButton(icon="fa-globe",title="Home",onClick=JS("function(btn, map){ map.setZoom(1.4); }"))) %>%
        addCircles(data=verts_, opacity=.2, fillColor = "blue", color="blue", weight=2, radius =~IMF_cred/100000, lat = ~lat, lng = ~lon)
      
    } #end if(input==T)
  }) #these end the reactive that makes map_i
  
  map_i2<-reactive({
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
      addPolygons(data=dat_i(),weight=.5, color="black", fillColor=pal2_h(), fillOpacity = .75, popup=pop_cont2_h(), 
                  highlightOptions = highlightOptions(color="white", weight=2, bringToFront = T, sendToBack = T), label=worldmap@data$SOVEREIGNT) %>%
      addEasyButton(easyButton(icon="fa-globe",title="Home",onClick=JS("function(btn, map){ map.setZoom(1.4); }"))) %>%
      addCircles(data=verts_, opacity=.2, fillColor = "blue", color="blue", weight=2, radius =~IMF_cred/100000, lat = ~lat, lng = ~lon)
    
  })
  
  output$scatter_c<-renderPlotly(
    if(input$var_e=="GDP Growth"){
      if(input$view=="Africa"){
        plot_ly(data=filter(dat_c()@data, CONTINENT=="Africa"), x=~inv_tot, y=~gdp_growth, type="scatter", mode="markers",
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT=="Africa")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="GDP Growth",yaxis=list(title="Annual GDP Growth"),xaxis=list(title="Investment from China"))
      }else if(input$view=="Europe"){
        plot_ly(data=filter(dat_c()@data, CONTINENT=="Europe"), x=~inv_tot, y=~gdp_growth, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT=="Europe")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="GDP Growth",yaxis=list(title="Annual GDP Growth"),xaxis=list(title="Investment from China"))
      }else if(input$view=="Asia"){
        plot_ly(data=filter(dat_c()@data, CONTINENT=="Asia"), x=~inv_tot, y=~gdp_growth, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT=="Asia")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="GDP Growth",yaxis=list(title="Annual GDP Growth"),xaxis=list(title="Investment from China"))
      }else if(input$view=="North America"){
        plot_ly(data=filter(dat_c()@data, CONTINENT=="North America"), x=~inv_tot, y=~gdp_growth, type="scatter", mode="markers",
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT=="North America")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="GDP Growth",yaxis=list(title="Annual GDP Growth"),xaxis=list(title="Investment from China"))
      }else if(input$view=="South America"){
        plot_ly(data=filter(dat_c()@data, CONTINENT=="South America"), x=~inv_tot, y=~gdp_growth, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT=="South America")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="GDP Growth",yaxis=list(title="Annual GDP Growth"),xaxis=list(title="Investment from China"))
      }else if(input$view=="Australiasia"){
        plot_ly(data=filter(dat_c()@data, CONTINENT=="Australiasia"), x=~inv_tot, y=~gdp_growth, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT=="Australiasia")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="GDP Growth",yaxis=list(title="Annual GDP Growth"),xaxis=list(title="Investment from China"))
      }else{
        plot_ly(data=dat_c()@data, x=~inv_tot, y=~gdp_growth, type="scatter", mode="markers", color=~CONTINENT,
                hovertemplate=paste0("Country: ", dat_c()@data$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="GDP Growth",yaxis=list(title="Annual GDP Growth"),xaxis=list(title="Investment from China"))
      }
    }else if(input$var_e=="Gini"){
      if(input$view=="Africa"){
        plot_ly(data=filter(dat_c()@data, CONTINENT=="Africa"), x=~inv_tot, y=~gini, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT=="Africa")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Inequality",yaxis=list(title="Gini Coefficient"),xaxis=list(title="Investment from China"))
      }else if(input$view=="Asia"){
        plot_ly(data=filter(dat_c()@data, CONTINENT=="Asia"), x=~inv_tot, y=~gini, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT=="Asia")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Inequality",yaxis=list(title="Gini Coefficient"),xaxis=list(title="Investment from China"))
      }else if(input$view=="Europe"){
        plot_ly(data=filter(dat_c()@data, CONTINENT=="Europe"), x=~inv_tot, y=~gini, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT=="Europe")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Inequality",yaxis=list(title="Gini Coefficient"),xaxis=list(title="Investment from China"))
      }else if(input$view=="North America"){
        plot_ly(data=filter(dat_c()@data, CONTINENT=="North America"), x=~inv_tot, y=~gini, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT=="North America")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Inequality",yaxis=list(title="Gini Coefficient"),xaxis=list(title="Investment from China"))
      }else if(input$view=="South America"){
        plot_ly(data=filter(dat_c()@data, CONTINENT=="South America"), x=~inv_tot, y=~gini, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT=="South America")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Inequality",yaxis=list(title="Gini Coefficient"),xaxis=list(title="Investment from China"))
      }else if(input$view=="Australiasia"){
        plot_ly(data=filter(dat_c()@data, CONTINENT=="Australiasia"), x=~inv_tot, y=~gini, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT=="Australiasia")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Inequality",yaxis=list(title="Gini Coefficient"),xaxis=list(title="Investment from China"))
      }else{
        plot_ly(data=dat_c()@data, x=~inv_tot, y=~gini, type="scatter", mode="markers", color=~CONTINENT,
                hovertemplate=paste0("Country: ", dat_c()@data$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Inequality",yaxis=list(title="Gini Coefficient"),xaxis=list(title="Investment from China"))
      }
    }else if(input$var_e=="Unemployment"){
      if(input$view=="Africa"){
        plot_ly(data=filter(dat_c()@data, CONTINENT=="Africa"),x=~inv_tot, y=~unemp, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT=="Africa")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Unemployment Rate",yaxis=list(title="Unemployment Rate"),xaxis=list(title="Investment from China"))
      }else if(input$view=="Austrliasia"){
        plot_ly(data=filter(dat_c()@data, CONTINENT=="Australiasia"),x=~inv_tot, y=~unemp, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT=="Australiasia")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Unemployment Rate",yaxis=list(title="Unemployment Rate"),xaxis=list(title="Investment from China"))
      }else if(input$view=="Europe"){
        plot_ly(data=filter(dat_c()@data, CONTINENT=="Europe"),x=~inv_tot, y=~unemp, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT=="Europe")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Unemployment Rate",yaxis=list(title="Unemployment Rate"),xaxis=list(title="Investment from China"))
      }else if(input$view=="North America"){
        plot_ly(data=filter(dat_c()@data, CONTINENT=="North America"),x=~inv_tot, y=~unemp, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT=="North America")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Unemployment Rate",yaxis=list(title="Unemployment Rate"),xaxis=list(title="Investment from China"))
      }else if(input$view=="South America"){
        plot_ly(data=filter(dat_c()@data, CONTINENT=="South America"),x=~inv_tot, y=~unemp, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT=="South America")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Unemployment Rate",yaxis=list(title="Unemployment Rate"),xaxis=list(title="Investment from China"))
      }else if(input$view=="Asia"){
        plot_ly(data=filter(dat_c()@data, CONTINENT=="Asia"),x=~inv_tot, y=~unemp, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT=="Asia")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Unemployment Rate",yaxis=list(title="Unemployment Rate"),xaxis=list(title="Investment from China"))
      }else{
        plot_ly(data=dat_c()@data, x=~inv_tot, y=~unemp, type="scatter", mode="markers", color=~CONTINENT,
                hovertemplate=paste0("Country: ", dat_c()@data$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Unemployment Rate",yaxis=list(title="Unemployment Rate"),xaxis=list(title="Investment from China"))
      }
    }
  )
  
  output$scatter_i<-renderPlotly(
    if(input$var_e=="GDP Growth"){
      if(input$view=="Africa"){
        plot_ly(data=filter(dat_i()@data, CONTINENT=="Africa"), x=~IMF_cred, y=~gdp_growth, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT=="Africa")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="GDP Growth",yaxis=list(title="Annual GDP Growth"),xaxis=list(title="Investment from IMF"))
      }else if(input$view=="Asia"){
        plot_ly(data=filter(dat_i()@data, CONTINENT=="Asia"), x=~IMF_cred, y=~gdp_growth, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT=="Asia")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="GDP Growth",yaxis=list(title="Annual GDP Growth"),xaxis=list(title="Investment from IMF"))
      }else if(input$view=="Australiasia"){
        plot_ly(data=filter(dat_i()@data, CONTINENT=="Australiasia"), x=~IMF_cred, y=~gdp_growth, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT=="Australiasia")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="GDP Growth",yaxis=list(title="Annual GDP Growth"),xaxis=list(title="Investment from IMF"))
      }else if(input$view=="Europe"){
        plot_ly(data=filter(dat_i()@data, CONTINENT=="Europe"), x=~IMF_cred, y=~gdp_growth, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT=="Europe")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="GDP Growth",yaxis=list(title="Annual GDP Growth"),xaxis=list(title="Investment from IMF"))
      }else if(input$view=="North America"){
        plot_ly(data=filter(dat_i()@data, CONTINENT=="North America"), x=~IMF_cred, y=~gdp_growth, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT=="North America")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="GDP Growth",yaxis=list(title="Annual GDP Growth"),xaxis=list(title="Investment from IMF"))
      }else if(input$view=="South America"){
        plot_ly(data=filter(dat_i()@data, CONTINENT=="South America"), x=~IMF_cred, y=~gdp_growth, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT=="Asia")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="GDP Growth",yaxis=list(title="Annual GDP Growth"),xaxis=list(title="Investment from IMF"))
      }else{
        plot_ly(data=dat_i()@data, x=~IMF_cred, y=~gdp_growth, type="scatter", mode="markers", color=~CONTINENT,
                hovertemplate=paste0("Country: ", dat_i()@data$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="GDP Growth",yaxis=list(title="Annual GDP Growth"),xaxis=list(title="Investment from IMF"))
      }
    }else if(input$var_e=="Gini"){
      if(input$view=="Africa"){
        plot_ly(data=filter(dat_i()@data, CONTINENT=="Africa"), x=~IMF_cred, y=~gini, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT=="Africa")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Inequality",yaxis=list(title="Gini Coefficient"),xaxis=list(title="Investment from IMF"))
      }else if(input$view=="Asia"){
        plot_ly(data=filter(dat_i()@data, CONTINENT=="Asia"), x=~IMF_cred, y=~gini, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT=="Asia")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Inequality",yaxis=list(title="Gini Coefficient"),xaxis=list(title="Investment from IMF"))
      }else if(input$view=="Australiasia"){
        plot_ly(data=filter(dat_i()@data, CONTINENT=="Australiasia"), x=~IMF_cred, y=~gini, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT=="Australiasia")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Inequality",yaxis=list(title="Gini Coefficient"),xaxis=list(title="Investment from IMF"))
      }else if(input$view=="Europe"){
        plot_ly(data=filter(dat_i()@data, CONTINENT=="Europe"), x=~IMF_cred, y=~gini, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT=="Europe")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Inequality",yaxis=list(title="Gini Coefficient"),xaxis=list(title="Investment from IMF"))
      }else if(input$view=="North America"){
        plot_ly(data=filter(dat_i()@data, CONTINENT=="North America"), x=~IMF_cred, y=~gini, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT=="North America")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Inequality",yaxis=list(title="Gini Coefficient"),xaxis=list(title="Investment from IMF"))
      }else if(input$view=="South America"){
        plot_ly(data=filter(dat_i()@data, CONTINENT=="South America"), x=~IMF_cred, y=~gini, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT=="Asia")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Inequality",yaxis=list(title="Gini Coefficient"),xaxis=list(title="Investment from IMF"))
      }else{
        plot_ly(data=dat_i()@data, x=~IMF_cred, y=~gini, type="scatter", mode="markers", color=~CONTINENT,
                hovertemplate=paste0("Country: ", dat_i()@data$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Inequality",yaxis=list(title="Gini Coefficient"),xaxis=list(title="Investment from IMF"))
      }
    }else if(input$var_e=="Unemployment"){
      if(input$view=="Africa"){
        plot_ly(data=filter(dat_i()@data, CONTINENT=="Africa"), x=~IMF_cred, y=~unemp, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT=="Africa")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Unemployment Rate",yaxis=list(title="Percent Unemployed"),xaxis=list(title="Investment from IMF"))
      }else if(input$view=="Asia"){
        plot_ly(data=filter(dat_i()@data, CONTINENT=="Asia"), x=~IMF_cred, y=~unemp, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT=="Asia")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Unemployment Rate",yaxis=list(title="Percent Unemployed"),xaxis=list(title="Investment from IMF"))
      }else if(input$view=="Australiasia"){
        plot_ly(data=filter(dat_i()@data, CONTINENT=="Australiasia"), x=~IMF_cred, y=~unemp, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT=="Australiasia")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Unemployment Rate",yaxis=list(title="Percent Unemployed"),xaxis=list(title="Investment from IMF"))
      }else if(input$view=="Europe"){
        plot_ly(data=filter(dat_i()@data, CONTINENT=="Europe"), x=~IMF_cred, y=~unemp, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT=="Europe")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Unemployment Rate",yaxis=list(title="Percent Unemployed"),xaxis=list(title="Investment from IMF"))
      }else if(input$view=="North America"){
        plot_ly(data=filter(dat_i()@data, CONTINENT=="North America"), x=~IMF_cred, y=~unemp, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT=="North America")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Unemployment Rate",yaxis=list(title="Percent Unemployed"),xaxis=list(title="Investment from IMF"))
      }else if(input$view=="South America"){
        plot_ly(data=filter(dat_i()@data, CONTINENT=="South America"), x=~IMF_cred, y=~unemp, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT=="Asia")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Unemployment Rate",yaxis=list(title="Percent Unemployed"),xaxis=list(title="Investment from IMF"))
      }else{
        plot_ly(data=dat_i()@data, x=~IMF_cred, y=~unemp, type="scatter", mode="markers", color=~CONTINENT,
                hovertemplate=paste0("Country: ", dat_i()@data$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Unemployment Rate",yaxis=list(title="Percent Unemployed"),xaxis=list(title="Investment from IMF"))
      }
    }
  )
  
  output$scatter_ch<-renderPlotly(
    if(input$var_h=="Undernourishment Prevelance"){
      if(input$view=="Africa"){
        plot_ly(data=filter(dat_c()@data, CONTINENT=="Africa"), x=~inv_tot, y=~undernourishment, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT=="Africa")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Undernourishment",yaxis=list(title="Percent Undernourished"),xaxis=list(title="Investment from China"))
      }else if(input$view=="Asia"){
        plot_ly(data=filter(dat_c()@data, CONTINENT=="Asia"), x=~inv_tot, y=~undernourishment, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT=="Asia")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Undernourishment",yaxis=list(title="Percent Undernourished"),xaxis=list(title="Investment from China"))
      }else if(input$view=="Australiasia"){
        plot_ly(data=filter(dat_c()@data, CONTINENT=="Australiasia"), x=~inv_tot, y=~undernourishment, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT=="Australiasia")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Undernourishment",yaxis=list(title="Percent Undernourished"),xaxis=list(title="Investment from China"))
      }else if(input$view=="Europe"){
        plot_ly(data=filter(dat_c()@data, CONTINENT=="Europe"), x=~inv_tot, y=~undernourishment, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT=="Europe")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Undernourishment",yaxis=list(title="Percent Undernourished"),xaxis=list(title="Investment from China"))
      }else if(input$view=="North America"){
        plot_ly(data=filter(dat_c()@data, CONTINENT=="North America"), x=~inv_tot, y=~undernourishment, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT=="North America")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Undernourishment",yaxis=list(title="Percent Undernourished"),xaxis=list(title="Investment from China"))
      }else if(input$view=="South America"){
        plot_ly(data=filter(dat_c()@data, CONTINENT=="South America"), x=~inv_tot, y=~undernourishment, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT=="Asia")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Undernourishment",yaxis=list(title="Percent Undernourished"),xaxis=list(title="Investment from China"))
      }else{
        plot_ly(data=dat_c()@data, x=~inv_tot, y=~undernourishment, type="scatter", mode="markers", color=~CONTINENT,
                hovertemplate=paste0("Country: ", dat_c()@data$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Undernourishment",yaxis=list(title="Percent Undernourished"),xaxis=list(title="Investment from China"))
      }
    }else if(input$var_h=="Democracy Score"){
      if(input$view=="Africa"){
        plot_ly(data=filter(dat_c()@data, CONTINENT=="Africa"), x=~inv_tot, y=~dem_score, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT=="Africa")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Liberal Democracy",yaxis=list(title="Liberal Democracy Score"),xaxis=list(title="Investment from China"))
      }else if(input$view=="Asia"){
        plot_ly(data=filter(dat_c()@data, CONTINENT=="Asia"), x=~inv_tot, y=~dem_score, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT=="Asia")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Liberal Democracy",yaxis=list(title="Liberal Democracy Score"),xaxis=list(title="Investment from China"))
      }else if(input$view=="Australiasia"){
        plot_ly(data=filter(dat_c()@data, CONTINENT=="Australiasia"), x=~inv_tot, y=~dem_score, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT=="Australiasia")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Liberal Democracy",yaxis=list(title="Liberal Democracy Score"),xaxis=list(title="Investment from China"))
      }else if(input$view=="Europe"){
        plot_ly(data=filter(dat_c()@data, CONTINENT=="Europe"), x=~inv_tot, y=~dem_score, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT=="Europe")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Liberal Democracy",yaxis=list(title="Liberal Democracy Score"),xaxis=list(title="Investment from China"))
      }else if(input$view=="North America"){
        plot_ly(data=filter(dat_c()@data, CONTINENT=="North America"), x=~inv_tot, y=~dem_score, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT=="North America")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Liberal Democracy",yaxis=list(title="Liberal Democracy Score"),xaxis=list(title="Investment from China"))
      }else if(input$view=="South America"){
        plot_ly(data=filter(dat_c()@data, CONTINENT=="South America"), x=~inv_tot, y=~dem_score, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT=="Asia")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Liberal Democracy",yaxis=list(title="Liberal Democracy Score"),xaxis=list(title="Investment from China"))
      }else{
        plot_ly(data=dat_c()@data, x=~inv_tot, y=~dem_score, type="scatter", mode="markers", color=~CONTINENT,
                hovertemplate=paste0("Country: ", dat_c()@data$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Liberal Democracy",yaxis=list(title="Liberal Democracy Score"),xaxis=list(title="Investment from China"))
      }
    }else if(input$var_h=="Freedom of Expression"){
      if(input$view=="Africa"){
        plot_ly(data=filter(dat_c()@data, CONTINENT=="Africa"), x=~inv_tot, y=~exp, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT=="Africa")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Freedom of Expression",yaxis=list(title="Freedom of Expression Score"),xaxis=list(title="Investment from China"))
      }else if(input$view=="Asia"){
        plot_ly(data=filter(dat_c()@data, CONTINENT=="Asia"), x=~inv_tot, y=~exp, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT=="Asia")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Freedom of Expression",yaxis=list(title="Freedom of Expression Score"),xaxis=list(title="Investment from China"))
      }else if(input$view=="Australiasia"){
        plot_ly(data=filter(dat_c()@data, CONTINENT=="Australiasia"), x=~inv_tot, y=~exp, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT=="Australiasia")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Freedom of Expression",yaxis=list(title="Freedom of Expression Score"),xaxis=list(title="Investment from China"))
      }else if(input$view=="Europe"){
        plot_ly(data=filter(dat_c()@data, CONTINENT=="Europe"), x=~inv_tot, y=~exp, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT=="Europe")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Freedom of Expression",yaxis=list(title="Freedom of Expression Score"),xaxis=list(title="Investment from China"))
      }else if(input$view=="North America"){
        plot_ly(data=filter(dat_c()@data, CONTINENT=="North America"), x=~inv_tot, y=~exp, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT=="North America")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Freedom of Expression",yaxis=list(title="Freedom of Expression Score"),xaxis=list(title="Investment from China"))
      }else if(input$view=="South America"){
        plot_ly(data=filter(dat_c()@data, CONTINENT=="South America"), x=~inv_tot, y=~exp, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT=="Asia")$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Freedom of Expression",yaxis=list(title="Freedom of Expression Score"),xaxis=list(title="Investment from China"))
      }else{
        plot_ly(data=dat_c()@data, x=~inv_tot, y=~exp, type="scatter", mode="markers", color=~CONTINENT,
                hovertemplate=paste0("Country: ", dat_c()@data$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Freedom of Expression",yaxis=list(title="Freedom of Expression Score"),xaxis=list(title="Investment from China"))
      }
    }
  )
  
  output$scatter_ih<-renderPlotly(
    if(input$var_h=="Undernourishment Prevelance"){
      if(input$view=="Africa"){
        plot_ly(data=filter(dat_i()@data, CONTINENT=="Africa"), x=~IMF_cred, y=~undernourishment, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT=="Africa")$SOVEREIgnt, "<extra></extra>"))%>%
          layout(title="Unernourishment",yaxis=list(title="Percent Undernourished"),xaxis=list(title="Investment from IMF"))
      }else if(input$view=="Asia"){
        plot_ly(data=filter(dat_i()@data, CONTINENT=="Asia"), x=~IMF_cred, y=~undernourishment, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT=="Asia")$SOVEREIgnt, "<extra></extra>"))%>%
          layout(title="Unernourishment",yaxis=list(title="Percent Undernourished"),xaxis=list(title="Investment from IMF"))
      }else if(input$view=="Australiasia"){
        plot_ly(data=filter(dat_i()@data, CONTINENT=="Australiasia"), x=~IMF_cred, y=~undernourishment, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT=="Australiasia")$SOVEREIgnt, "<extra></extra>"))%>%
          layout(title="Unernourishment",yaxis=list(title="Percent Undernourished"),xaxis=list(title="Investment from IMF"))
      }else if(input$view=="Europe"){
        plot_ly(data=filter(dat_i()@data, CONTINENT=="Europe"), x=~IMF_cred, y=~undernourishment, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT=="Europe")$SOVEREIgnt, "<extra></extra>"))%>%
          layout(title="Unernourishment",yaxis=list(title="Percent Undernourished"),xaxis=list(title="Investment from IMF"))
      }else if(input$view=="North America"){
        plot_ly(data=filter(dat_i()@data, CONTINENT=="North America"), x=~IMF_cred, y=~undernourishment, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT=="North America")$SOVEREIgnt, "<extra></extra>"))%>%
          layout(title="Unernourishment",yaxis=list(title="Percent Undernourished"),xaxis=list(title="Investment from IMF"))
      }else if(input$view=="South America"){
        plot_ly(data=filter(dat_i()@data, CONTINENT=="South America"), x=~IMF_cred, y=~undernourishment, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT=="Asia")$SOVEREIgnt, "<extra></extra>"))%>%
          layout(title="Unernourishment",yaxis=list(title="Percent Undernourished"),xaxis=list(title="Investment from IMF"))
      }else{
        plot_ly(data=dat_i()@data, x=~IMF_cred, y=~undernourishment, type="scatter", mode="markers", color=~CONTINENT,
                hovertemplate=paste0("Country: ", dat_i()@data$SOVEREIgnt, "<extra></extra>"))%>%
          layout(title="Unernourishment",yaxis=list(title="Percent Undernourished"),xaxis=list(title="Investment from IMF"))
      }
    }else if(input$var_h=="Democracy Score"){
      if(input$view=="Africa"){
        plot_ly(data=filter(dat_i()@data, CONTINENT=="Africa"), x=~IMF_cred, y=~dem_score, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT=="Africa")$SOVEREIgnt, "<extra></extra>"))%>%
          layout(title="Liberal Democracy",yaxis=list(title="Liberal Democracy Score"),xaxis=list(title="Investment from IMF"))
      }else if(input$view=="Asia"){
        plot_ly(data=filter(dat_i()@data, CONTINENT=="Asia"), x=~IMF_cred, y=~dem_score, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT=="Asia")$SOVEREIgnt, "<extra></extra>"))%>%
          layout(title="Liberal Democracy",yaxis=list(title="Liberal Democracy Score"),xaxis=list(title="Investment from IMF"))
      }else if(input$view=="Australiasia"){
        plot_ly(data=filter(dat_i()@data, CONTINENT=="Australiasia"), x=~IMF_cred, y=~dem_score, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT=="Australiasia")$SOVEREIgnt, "<extra></extra>"))%>%
          layout(title="Liberal Democracy",yaxis=list(title="Liberal Democracy Score"),xaxis=list(title="Investment from IMF"))
      }else if(input$view=="Europe"){
        plot_ly(data=filter(dat_i()@data, CONTINENT=="Europe"), x=~IMF_cred, y=~dem_score, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT=="Europe")$SOVEREIgnt, "<extra></extra>"))%>%
          layout(title="Liberal Democracy",yaxis=list(title="Liberal Democracy Score"),xaxis=list(title="Investment from IMF"))
      }else if(input$view=="North America"){
        plot_ly(data=filter(dat_i()@data, CONTINENT=="North America"), x=~IMF_cred, y=~dem_score, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT=="North America")$SOVEREIgnt, "<extra></extra>"))%>%
          layout(title="Liberal Democracy",yaxis=list(title="Liberal Democracy Score"),xaxis=list(title="Investment from IMF"))
      }else if(input$view=="South America"){
        plot_ly(data=filter(dat_i()@data, CONTINENT=="South America"), x=~IMF_cred, y=~dem_score, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT=="Asia")$SOVEREIgnt, "<extra></extra>"))%>%
          layout(title="Liberal Democracy",yaxis=list(title="Liberal Democracy Score"),xaxis=list(title="Investment from IMF"))
      }else{
        plot_ly(data=dat_i()@data, x=~IMF_cred, y=~dem_score, type="scatter", mode="markers", color=~CONTINENT,
                hovertemplate=paste0("Country: ", dat_i()@data$SOVEREIgnt, "<extra></extra>"))%>%
          layout(title="Liberal Democracy",yaxis=list(title="Liberal Democracy Score"),xaxis=list(title="Investment from IMF"))
      }
    }else if(input$var_h=="Freedom of Expression"){
      if(input$view=="Africa"){
        plot_ly(data=filter(dat_i()@data, CONTINENT=="Africa"), x=~IMF_cred, y=~exp, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT=="Africa")$SOVEREIgnt, "<extra></extra>"))%>%
          layout(title="Freedom of Expression",yaxis=list(title="Freedom of Expression Score"),xaxis=list(title="Investment from IMF"))
      }else if(input$view=="Asia"){
        plot_ly(data=filter(dat_i()@data, CONTINENT=="Asia"), x=~IMF_cred, y=~exp, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT=="Asia")$SOVEREIgnt, "<extra></extra>"))%>%
          layout(title="Freedom of Expression",yaxis=list(title="Freedom of Expression Score"),xaxis=list(title="Investment from IMF"))
      }else if(input$view=="Australiasia"){
        plot_ly(data=filter(dat_i()@data, CONTINENT=="Australiasia"), x=~IMF_cred, y=~exp, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT=="Australiasia")$SOVEREIgnt, "<extra></extra>"))%>%
          layout(title="Freedom of Expression",yaxis=list(title="Freedom of Expression Score"),xaxis=list(title="Investment from IMF"))
      }else if(input$view=="Europe"){
        plot_ly(data=filter(dat_i()@data, CONTINENT=="Europe"), x=~IMF_cred, y=~exp, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT=="Europe")$SOVEREIgnt, "<extra></extra>"))%>%
          layout(title="Freedom of Expression",yaxis=list(title="Freedom of Expression Score"),xaxis=list(title="Investment from IMF"))
      }else if(input$view=="North America"){
        plot_ly(data=filter(dat_i()@data, CONTINENT=="North America"), x=~IMF_cred, y=~exp, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT=="North America")$SOVEREIgnt, "<extra></extra>"))%>%
          layout(title="Freedom of Expression",yaxis=list(title="Freedom of Expression Score"),xaxis=list(title="Investment from IMF"))
      }else if(input$view=="South America"){
        plot_ly(data=filter(dat_i()@data, CONTINENT=="South America"), x=~IMF_cred, y=~exp, type="scatter", mode="markers", 
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT=="Asia")$SOVEREIgnt, "<extra></extra>"))%>%
          layout(title="Freedom of Expression",yaxis=list(title="Freedom of Expression Score"),xaxis=list(title="Investment from IMF"))
      }else{
        plot_ly(data=dat_i()@data, x=~IMF_cred, y=~exp, type="scatter", mode="markers", color=~CONTINENT,
                hovertemplate=paste0("Country: ", dat_i()@data$SOVEREIGNT, "<extra></extra>"))%>%
          layout(title="Freedom of Expression",yaxis=list(title="Freedom of Expression Score"),xaxis=list(title="Investment from IMF"))
      }
    }
  )
  
  output$leaf_c<-renderLeaflet(map_c())
  output$leaf_c2<-renderLeaflet(map_c2())
  
  output$leaf_i<-renderLeaflet(map_i())
  output$leaf_i2<-renderLeaflet(map_i2())
  
  output$blurb<-renderText("Here's a blurb with lots and lots of text. I want to see how the allignment is and how the page looks with a much longer blurb than the one i originally did which just said 'here's a blurb' and was a little bit too far to the left but maybe that was just because it was too short")
  output$placeholder1<-renderText("Chart here")
  output$placeholder2<-renderText("and/or here")
  
} #This one ends server

shinyApp(ui, server)