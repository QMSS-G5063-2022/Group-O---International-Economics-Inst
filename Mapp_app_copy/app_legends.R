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
suppressPackageStartupMessages(library(MetBrewer))
suppressPackageStartupMessages(library(DT))
library(vdemdata)


joined_data <- read.csv("alldata_abridged.csv") %>% rename(Year = 1) %>%
  mutate(rights_index = round(rights_index, 2)) %>%
  mutate(egal_dem = round(egal_dem, 2))
hist_data <- joined_data %>% select(1:3, 5:9) %>%
  rename(Country = 2, `Rights Index` = 3, `Dem. Index` = 4,
         Gini = 5, `Poverty Rate` = 6, `IMF Investment` = 7, 
         `China Investment` = 8)
now_data <- hist_data %>% filter(Year == 2018)

chn_inv<-read.csv('China-Global-Investment-Tracker-2021-Fall-FINAL-2022.2.21-update.up.csv')
chn_inv<-chn_inv %>%
  group_by(Year, Country) %>%
  mutate(inv_tot = sum(as.numeric(gsub("[[:punct:]]", "", Quantity.in.Millions))))
chn_inv<-chn_inv %>%
  group_by(Country) %>%
  mutate(projs=n(), country_tot=sum(inv_tot, na.rm = T))
chn_inv$country2<-chn_inv$Country #for use later
chn_inv[chn_inv$Region=="Arab Middle East and North Africa",]$Region<-"MENA"

worldmap = readOGR(dsn="Nat_world_map", layer="ne_50m_admin_0_countries")
worldmap@data[worldmap@data$CONTINENT=="Oceania",]$CONTINENT<-"Australiasia"

rawnodes<-read.csv('http://www.kateto.net/wordpress/wp-content/uploads/2015/06/Country_terms_FREQ.csv')
rawedges<-read.csv('http://www.kateto.net/wordpress/wp-content/uploads/2015/06/Country_terms_COOC.csv')

dem<-filter(vdem[c("country_name","country_text_id","year","v2x_egaldem", "v2x_freexp")],year>2004)

gdp<-WDI(country="all",indicator="NY.GDP.MKTP.KD.ZG", start=2005, extra=F)
gini<-WDI(country="all",indicator="SI.POV.GINI", start=2005, extra=F)
n<-WDI(indicator = "SN.ITK.DEFC.ZS", start=2005) #I named this stupidly. It's undernourishment data
imf<-WDI(indicator = "DT.DOD.DIMF.CD", country="all",start=2005, extra=F)
imf<-filter(imf, !country %in% unique(imf$country)[1:49])
imf<-rename(imf, Year=year, name=country, IMF_cred=DT.DOD.DIMF.CD)
imf<-imf %>%
  group_by(name) %>%
  mutate(country_imf_tot= sum(IMF_cred, na.rm=T))
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

worldmap@data[worldmap@data$NAME=="S. Geo. and the Is.",]$CONTINENT<-"South America"
worldmap@data[worldmap@data$NAME=="Br. Indian Ocean Ter.",]$CONTINENT<-"Africa"
worldmap@data[worldmap@data$NAME=="Saint Helena",]$CONTINENT<-"Africa"
worldmap@data[worldmap@data$NAME=="Seychelles",]$CONTINENT<-"Africa"
worldmap@data[worldmap@data$NAME=="Mauritius",]$CONTINENT<-"Africa"
worldmap@data[worldmap@data$NAME=="Fr. S. Antarctic Lands",]$CONTINENT<-"Africa"
worldmap@data[worldmap@data$NAME=="Heard I. and McDonald Is.",]$CONTINENT<-"Africa"
worldmap@data[worldmap@data$NAME=="Maldives",]$CONTINENT<-"Asia"

ui<-navbarPage("", theme = bs_theme(bootswatch = "flatly"),
               tabPanel("Home",
                        titlePanel(h1("Let's Change This to Something Better", align = "center")),
                        sidebarLayout(
                          sidebarPanel(width= 7, tags$style(".well {background-color:#FFFFFF;}"),
                                                   verticalLayout(textOutput("title1"),
                                                                  tags$head(tags$style("#title1{font-size: 24px}")),
                                                                  leafletOutput("leaf_c_inv"), 
                                                                  br(), 
                                                                  textOutput("title2"),
                                                                  tags$head(tags$style("#title2{font-size: 24px}")),
                                                                  leafletOutput("leaf_i_inv")),
                          tags$style(tableHTML::make_css(list('.well', 'border-width', '0px')))),
                          mainPanel(width=4, br(), textOutput("blurb"),
                                    tags$head(tags$style("#blurb{font-size: 17px}"))))),
               tabPanel("Economics", 
                        fluidPage(
                          tags$h1("Economic Indicators"),
                          sliderInput("year", label="Select Year", min=2005, max=2020, value=2018, sep=""),
                          selectInput("var_e","Select Variable to Map", choices=c("GDP Growth", "Gini", "Unemployment")),
                          radioButtons("view", "Select", choices=c("World","Africa","Asia","Australiasia","Europe","North America","South America"), selected="World", inline=T),
                          splitLayout(leafletOutput("leaf_c"), leafletOutput("leaf_i")),
                          fluidRow(
                            column(3, textOutput("cap1")),
                            column(3, offset=6, textOutput("cap2"))
                          ),
                          br(),
                          br(),
                          br(),
                          splitLayout(plotlyOutput("scatter_c"), plotlyOutput("scatter_i")),
                          br()
                        )),
               tabPanel("Human Rights",
                        fluidPage(
                          tags$h1("Human Rights Indicators"),
                          sliderInput("year_h", label="Select Year", min=2005, max=2020, value=2018, sep=""),
                          selectInput("var_h","Select Variable to Map", choices=c("Undernourishment Prevelance" ,"Democracy Score", "Freedom of Expression")),
                          radioButtons("view_h", "Select", choices=c("World","Africa","Asia","Australiasia","Europe","North America","South America"), selected="World", inline=T),
                          splitLayout(leafletOutput("leaf_c2"), leafletOutput("leaf_i2")),
                          fluidRow(
                            column(3, textOutput("cap3")),
                            column(3, offset=6, textOutput("cap4"))
                          ),
                          br(),
                          br(),
                          br(),
                          splitLayout(plotlyOutput("scatter_ch"), plotlyOutput("scatter_ih")),
                          br()
                        )),
               tabPanel("Country Details",
                        fluidPage(
                          selectizeInput("country_", "Select Country", multiple=F, choices=c("World", unique(filter(chn_inv, projs>=5)$Country)), selected="World",
                                         options=list(create=F, placeholder="", maxitems=1,
                                                      onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                                                      onType = I("function (str) {if (str === \"\") {this.close();}}")
                                         )),
                          plotOutput("sect"),
                          br(),
                          br(),
                          br(),
                          splitLayout(plotlyOutput("line_out"), plotlyOutput("line_out_imf")),
                          br()
                        )),
               tabPanel("Appendix",
                        fluidPage(
                          radioButtons("time", "Selected Data", choices=c("2005-2020", "2018"), selected="2005-2020", inline=T),
                          br(),
                          DTOutput("data_table"),
                          br(),
                          br(),
                          titlePanel("Data Sources"),
                          br(),
                          htmlOutput("sources"),
                          br(),
                          titlePanel("Further Data"),
                          br(),
                          htmlOutput("further_reading"),
                          br()
                        ))
)

server<-function(input, output, session){
  output$title1<-renderText("Total Chinese Investment 2005-2020")
  output$title2<-renderText("Total IMF Investment 2005-2020")
  chn_inv$Country<-stringr::str_to_lower(chn_inv$Country)
  
  chn_inv[chn_inv$Country=="britain",]$Country<-"uk"
  chn_inv[chn_inv$Country=="myanmar",]$Country<-"burma"
  chn_inv[chn_inv$Country=="russian federation",]$Country<-"russia"
  chn_inv[chn_inv$Country=="trinidad-tobago",]$Country<-"trinidad and tobago"
  chn_inv[chn_inv$Country=="bosnia",]$Country<-"bosnia and herzegovina"
  chn_inv[chn_inv$Country=="sao tome",]$Country<-"uk"
  
  observe({
    val<-input$year_h
    updateSliderInput(session, "year", value=val)
  })
  observe({
    val<-input$view_h
    updateSliderInput(session, "view", value=val)
  })
  
  output$cap1<-renderText("Centroid radius corresponds to amount of Chinese investment")
  output$cap2<-renderText("Centroid radius corresponds to amount of IMF Investment")
  output$cap3<-renderText("Centroid radius corresponds to amount of Chinese investment")
  output$cap4<-renderText("Centroid radius corresponds to amount of IMF Investment")
  
  dat_c<-reactive({
    inv_y<-filter(chn_inv, Year==input$year)
    inv_y<-inv_y[!duplicated(inv_y$Country),c("Country","inv_tot", "country_tot")]
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
      dem$dem_fac<-ifelse(dem$v2x_egaldem<=.05, 1, ifelse(dem$v2x_egaldem<=.1 & dem$v2x_egaldem>.05, 2,
                                                          ifelse(dem$v2x_egaldem<=.2 & dem$v2x_egaldem>.1, 3,
                                                                 ifelse(dem$v2x_egaldem<=.3 & dem$v2x_egaldem>.2, 4, 
                                                                        ifelse(dem$v2x_egaldem<=.5 & dem$v2x_egaldem>.3, 5, 
                                                                               ifelse(dem$v2x_egaldem<=.7 & dem$v2x_egaldem>.5, 6,
                                                                                      ifelse(dem$v2x_egaldem<=.9 & dem$v2x_egaldem>.7, 7, 0)))))))
      
      dem<-filter(rename(dem, Year=year, name=country_name, dem_score=v2x_egaldem), Year==input$year)
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
      
      dem<-filter(rename(dem, Year=year, name=country_name, dem_score=v2x_egaldem, exp=v2x_freexp), Year==input$year)
      worldmap@data<-left_join(worldmap@data, dem, by=c("ADM0_A3"="country_text_id"))
    }
    worldmap
    
  }) # End reactive making dat_c
  
  dat_i<-reactive({
    inv_y<-filter(imf, Year==input$year)[c("name","IMF_cred", "country_imf_tot")]
    
    worldmap@data[worldmap@data$NAME_EN=="Brunei",]$NAME_EN<-"Brunei Darussalam"
    worldmap@data[worldmap@data$NAME_EN==unique(worldmap@data$NAME_EN)[grep("Cura",worldmap@data$NAME_EN)],]$NAME_EN<-"Curacao"
    
    worldmap@data<-left_join(worldmap@data, inv_y, by=c("NAME_EN"="name"))
    
    if(input$var_e=="GDP Growth"){
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
      dem$dem_fac<-ifelse(dem$v2x_egaldem<=.05, 1, ifelse(dem$v2x_egaldem<=.1 & dem$v2x_egaldem>.05, 2,
                                                          ifelse(dem$v2x_egaldem<=.2 & dem$v2x_egaldem>.1, 3,
                                                                 ifelse(dem$v2x_egaldem<=.3 & dem$v2x_egaldem>.2, 4, 
                                                                        ifelse(dem$v2x_egaldem<=.5 & dem$v2x_egaldem>.3, 5, 
                                                                               ifelse(dem$v2x_egaldem<=.7 & dem$v2x_egaldem>.5, 6,
                                                                                      ifelse(dem$v2x_egaldem<=.9 & dem$v2x_egaldem>.7, 7, 0)))))))
      
      dem<-filter(rename(dem, Year=year, name=country_name, dem_score=v2x_egaldem), Year==input$year)
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
      
      dem<-filter(rename(dem, Year=year, name=country_name, dem_score=v2x_egaldem, exp=v2x_freexp), Year==input$year)
      worldmap@data<-left_join(worldmap@data, dem, by=c("ADM0_A3"="country_text_id"))
    }
    worldmap
  }) #These end the reactive that makes dat_i
  
  pop_cont1<-reactive({
    f<-dat_c()@data$inv_tot
    f[is.na(f)]<-0
    if(input$var_e=="GDP Growth"){
      paste0("Country: ", dat_c()@data$SOVEREIGNT, "<br>",
             "Investment from China (Million USD): $", f, "<br>",
             "GDP Growth Rate: ", dat_c()@data$gdp_growth, "%")
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
             "Undernourishment Rate: ", dat_c()@data$undernourishment, "%")
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
             "GDP Growth Rate: ", dat_c()@data$gdp_growth, "%")
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
             "Undernourishment Rate: ", dat_i()@data$undernourishment, "%")
    }else if(input$var_h=="Freedom of Expression"){
      paste0("Country: ", dat_c()@data$SOVEREIGNT, "<br>",
             "Investment from IMF (Million USD): $", f, "<br>",
             "Freedom of Expression Score: ", dat_i()@data$exp)
    }
  }) # end reactive that makes pop_cont2
  
  pal1<-reactive({
    if(input$var_e=="GDP Growth"){
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
  
  pal_c_inv<-reactive({
    j<-colorNumeric(palette="Reds", domain=dat_c()@data$country_tot)
    j(dat_c()@data$country_tot)
  })
  pop_cont_c_inv<-reactive({
    f<-dat_c()@data$country_tot
    f[is.na(f)]<-0
    paste0("Country: ", dat_c()@data$SOVEREIGNT, "<br>",
           "Investment from China (Million USD): $", f)
  })
  
  pal_i_inv<-reactive({
    j<-colorNumeric(palette="Blues", domain=dat_i()@data$country_imf_tot)
    j(dat_i()@data$country_imf_tot)
  })
  pop_cont_i_inv<-reactive({
    f<-dat_i()@data$country_imf_tot
    f[is.na(f)]<-0
    paste0("Country: ", dat_i()@data$SOVEREIGNT, "<br>",
           "IMF Investment (Million USD): $", round(f/1000000, 0))
  })
  
  map_c_inv<-reactive({
    leaflet(options=leafletOptions(minZoom = 1, maxZoom = 7)) %>%
      setView(lat=40, lng=0, zoom=1.4) %>%
      addPolygons(data=dat_c(), weight=.5, fillOpacity = .75, fillColor = pal_c_inv(), color="black",
                  highlightOptions=highlightOptions(color="white", weight = 2, bringToFront = T, sendToBack = T),
                  popup=pop_cont_c_inv(), label=dat_c()@data$SOVEREIGNT) %>%
      addEasyButton(easyButton(icon="fa-globe", title="Home", onClick = JS("function(btn, map){ map.setView([40, 0], 1.4); }")))
  })
  
  map_i_inv<-reactive({
    leaflet(options=leafletOptions(minZoom = 1, maxZoom = 7)) %>%
      setView(lat=40, lng=0, zoom=1.4) %>%
      addPolygons(data=dat_i(), weight=.5, fillOpacity = .75, fillColor = pal_i_inv(), color="black",
                  highlightOptions=highlightOptions(color="white", weight = 2, bringToFront = T, sendToBack = T),
                  popup=pop_cont_i_inv(), label=dat_i()@data$SOVEREIGNT) %>%
      addEasyButton(easyButton(icon="fa-globe", title="Home", onClick = JS("function(btn, map){ map.setView([40, 0], 1.4); }")))
  })
  
  map_c<-reactive({
    
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
    
    if(input$var_e=="GDP Growth"){
      leaflet(options = leafletOptions(minZoom = 1, maxZoom = 7))%>%
        setView(lat=lat_re(), lng=lng_re(), zoom=zoom_re())%>%
        addPolygons(data=dat_c(),weight=.5, color="black", fillColor=pal1(), fillOpacity = .75, popup=pop_cont1(), 
                    highlightOptions = highlightOptions(color="white", weight=2, bringToFront = T, sendToBack = T), label=worldmap@data$SOVEREIGNT) %>%
        addLegend(colors = c("#8C510A", "#D8B365", "#F6E8C3","#F5F5F5", "#C7EAE5", "#5AB4AC",  "#01665E", "#808080"), labels=c("<-10", "-10 - -2.5", "-2.5 - 0", "0 - 2.5", "2.5 - 5", "5 - 10", ">10", "NA"), title="Annual GDP Growth Rate (%)") %>%
        addEasyButton(easyButton(icon="fa-globe",title="Home",onClick=JS("function(btn, map){ map.setView([40, 0], 1.4); }"))) %>%
        addCircles(data=verts_, opacity=.2, fillColor = "red", color="red", weight=2, radius=~inv_tot*10, lat = ~lat, lng = ~lon)
    }else if(input$var_e=="Gini"){
      leaflet(options = leafletOptions(minZoom = 1, maxZoom = 7))%>%
        setView(lat=lat_re(), lng=lng_re(), zoom=zoom_re())%>%
        addPolygons(data=dat_c(),weight=.5, color="black", fillColor=pal1(), fillOpacity = .75, popup=pop_cont1(), 
                    highlightOptions = highlightOptions(color="white", weight=2, bringToFront = T, sendToBack = T), label=worldmap@data$SOVEREIGNT) %>%
        addLegend(colors = c("#A6611A","#DFC27D", "#F5F5F5", "#80CDC1",  "#018571", "#808080"), labels=c("<30", "30 - 35", "35 - 40", "40 - 50", ">50", "NA"), title="Gini Coefficient") %>%
        addEasyButton(easyButton(icon="fa-globe",title="Home",onClick=JS("function(btn, map){ map.setView([40, 0], 1.4); }"))) %>%
        addCircles(data=verts_, opacity=.2, fillColor = "red", color="red", weight=2, radius=~inv_tot*10, lat = ~lat, lng = ~lon)
    }else if(input$var_e=="Unemployment"){
      leaflet(options = leafletOptions(minZoom = 1, maxZoom = 7))%>%
        setView(lat=lat_re(), lng=lng_re(), zoom=zoom_re())%>%
        addPolygons(data=dat_c(),weight=.5, color="black", fillColor=pal1(), fillOpacity = .75, popup=pop_cont1(), 
                    highlightOptions = highlightOptions(color="white", weight=2, bringToFront = T, sendToBack = T), label=worldmap@data$SOVEREIGNT) %>%
        addLegend(colors = c("#8C510A", "#D8B365","#F6E8C3","#F5F5F5", "C7EAE5", "#5AB4AC",  "#01665E", "#808080"), labels=c("<3", "3 - 4.5", "4.5 - 7", "7 - 10", "10-12", "12-20", ">20", "NA"), title="Unemployment Rate (%)") %>%
        addEasyButton(easyButton(icon="fa-globe",title="Home",onClick=JS("function(btn, map){ map.setView([40, 0], 1.4); }"))) %>%
        addCircles(data=verts_, opacity=.2, fillColor = "red", color="red", weight=2, radius=~inv_tot*10, lat = ~lat, lng = ~lon)
    }
    
    
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
    
    if(input$var_h=="Democracy Score"){
      leaflet(options = leafletOptions(minZoom = 1, maxZoom = 7))%>%
        setView(lat=lat_re(), lng=lng_re(), zoom=zoom_re())%>%
        addPolygons(data=dat_c(),weight=.5, color="black", fillColor=pal1_h(), fillOpacity = .75, popup=pop_cont1_h(), 
                    highlightOptions = highlightOptions(color="white", weight=2, bringToFront = T, sendToBack = T), label=worldmap@data$SOVEREIGNT) %>%
        addLegend(colors=c("#8C510A","#D8B365", "#F6E8C3", "#F5F5F5", "#C7EAE5", "#5AB4AC", "#01665E", "#808080"), labels=c("<.05", ".05 - .1", ".1 - .2", ".2 - .3", ".3 - .5", ".5 - .7", ".7 - .9", "NA"), title="Egalitarian Democracy Score") %>%
        addEasyButton(easyButton(icon="fa-globe",title="Home",onClick=JS("function(btn, map){ map.setView([40, 0], 1.4); }"))) %>%
        addCircles(data=verts_, opacity=.2, fillColor = "red", color="red", weight=2, radius=~inv_tot*10, lat = ~lat, lng = ~lon)
    }else if(input$var_h=="Undernourishment Prevelance"){
      leaflet(options = leafletOptions(minZoom = 1, maxZoom = 7))%>%
        setView(lat=lat_re(), lng=lng_re(), zoom=zoom_re())%>%
        addPolygons(data=dat_c(),weight=.5, color="black", fillColor=pal1_h(), fillOpacity = .75, popup=pop_cont1_h(), 
                    highlightOptions = highlightOptions(color="white", weight=2, bringToFront = T, sendToBack = T), label=worldmap@data$SOVEREIGNT) %>%
        addLegend(colors=c("#8C510A","#BF812D", "#DFC27D", "#F6E8C3", "#C7EAE5", "#80CDC1", "#35978F", "#01665E", "#808080"), labels=c("<3.5", "3.5 - 5", "5 - 7", "7 - 11", "11 - 15", "15 - 20", "20 - 30",">30", "NA"), title="Undernourishment Rate (%)") %>%
        addEasyButton(easyButton(icon="fa-globe",title="Home",onClick=JS("function(btn, map){ map.setView([40, 0], 1.4); }"))) %>%
        addCircles(data=verts_, opacity=.2, fillColor = "red", color="red", weight=2, radius=~inv_tot*10, lat = ~lat, lng = ~lon)
    }else if(input$var_h=="Freedom of Expression"){
      leaflet(options = leafletOptions(minZoom = 1, maxZoom = 7))%>%
        setView(lat=lat_re(), lng=lng_re(), zoom=zoom_re())%>%
        addPolygons(data=dat_c(),weight=.5, color="black", fillColor=pal1_h(), fillOpacity = .75, popup=pop_cont1_h(), 
                    highlightOptions = highlightOptions(color="white", weight=2, bringToFront = T, sendToBack = T), label=worldmap@data$SOVEREIGNT) %>%
        addLegend(colors=c("#8C510A","#BF812D", "#DFC27D", "#F6E8C3", "#C7EAE5", "#80CDC1", "#35978F", "#01665E", "#808080"), labels=c("<.05", ".05 - .15", ".15 - .25", ".25 - .4", ".4 - .6", ".6 - .75", ".75 - .9",">.9", "NA"), title="Freedom of Expression Score") %>%
        addEasyButton(easyButton(icon="fa-globe",title="Home",onClick=JS("function(btn, map){ map.setView([40, 0], 1.4); }"))) %>%
        addCircles(data=verts_, opacity=.2, fillColor = "red", color="red", weight=2, radius=~inv_tot*10, lat = ~lat, lng = ~lon)
    }
  })
  
  map_i<-reactive({
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
    
    if(input$var_e=="GDP Growth"){
      leaflet(options = leafletOptions(minZoom = 1, maxZoom = 7))%>%
        setView(lat=lat_re(), lng=lng_re(), zoom=zoom_re())%>%
        addPolygons(data=dat_i(),weight=.5, color="black", fillColor=pal2(), fillOpacity = .75, popup=pop_cont2(), 
                    highlightOptions = highlightOptions(color="white", weight=2, bringToFront = T, sendToBack = T), label=worldmap@data$SOVEREIGNT) %>%
        addLegend(colors = c("#8C510A", "#D8B365", "#F6E8C3","#F5F5F5", "#C7EAE5", "#5AB4AC",  "#01665E", "#808080"), labels=c("<-10", "-10 - -2.5", "-2.5 - 0", "0 - 2.5", "2.5 - 5", "5 - 10", ">10", "NA"), title="Annual GDP Growth Rate (%)") %>%
        addEasyButton(easyButton(icon="fa-globe",title="Home",onClick=JS("function(btn, map){ map.setView([40, 0], 1.4); }"))) %>%
        addCircles(data=verts_, opacity=.2, fillColor = "blue", color="blue", weight=2, radius =~IMF_cred/100000, lat = ~lat, lng = ~lon)
    }else if(input$var_e=="Gini"){
      leaflet(options = leafletOptions(minZoom = 1, maxZoom = 7))%>%
        setView(lat=lat_re(), lng=lng_re(), zoom=zoom_re())%>%
        addPolygons(data=dat_i(),weight=.5, color="black", fillColor=pal2(), fillOpacity = .75, popup=pop_cont2(), 
                    highlightOptions = highlightOptions(color="white", weight=2, bringToFront = T, sendToBack = T), label=worldmap@data$SOVEREIGNT) %>%
        addLegend(colors = c("#A6611A","#DFC27D", "#F5F5F5", "#80CDC1",  "#018571", "#808080"), labels=c("<30", "30 - 35", "35 - 40", "40 - 50", ">50", "NA"), title="Gini Coefficient") %>%
        addEasyButton(easyButton(icon="fa-globe",title="Home",onClick=JS("function(btn, map){ map.setView([40, 0], 1.4); }"))) %>%
        addCircles(data=verts_, opacity=.2, fillColor = "blue", color="blue", weight=2, radius =~IMF_cred/100000, lat = ~lat, lng = ~lon)
    }else if(input$var_e=="Unemployment"){
      leaflet(options = leafletOptions(minZoom = 1, maxZoom = 7))%>%
        setView(lat=lat_re(), lng=lng_re(), zoom=zoom_re())%>%
        addPolygons(data=dat_i(),weight=.5, color="black", fillColor=pal2(), fillOpacity = .75, popup=pop_cont2(), 
                    highlightOptions = highlightOptions(color="white", weight=2, bringToFront = T, sendToBack = T), label=worldmap@data$SOVEREIGNT) %>%
        addLegend(colors = c("#8C510A", "#D8B365","#F6E8C3","#F5F5F5", "C7EAE5", "#5AB4AC",  "#01665E", "#808080"), labels=c("<3", "3 - 4.5", "4.5 - 7", "7 - 10", "10-12", "12-20", ">20", "NA"), title="Unemployment Rate (%)") %>%
        addEasyButton(easyButton(icon="fa-globe",title="Home",onClick=JS("function(btn, map){ map.setView([40, 0], 1.4); }"))) %>%
        addCircles(data=verts_, opacity=.2, fillColor = "blue", color="blue", weight=2, radius =~IMF_cred/100000, lat = ~lat, lng = ~lon)
    }
    
  }) #these end the reactive that makes map_i
  
  map_i2<-reactive({
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
    
    if(input$var_h=="Democracy Score"){
      leaflet(options = leafletOptions(minZoom = 1, maxZoom = 7))%>%
        setView(lat=lat_re(), lng=lng_re(), zoom=zoom_re())%>%
        addPolygons(data=dat_i(),weight=.5, color="black", fillColor=pal2_h(), fillOpacity = .75, popup=pop_cont2_h(), 
                    highlightOptions = highlightOptions(color="white", weight=2, bringToFront = T, sendToBack = T), label=worldmap@data$SOVEREIGNT) %>%
        addLegend(colors=c("#8C510A","#D8B365", "#F6E8C3", "#F5F5F5", "#C7EAE5", "#5AB4AC", "#01665E", "#808080"), labels=c("<.05", ".05 - .1", ".1 - .2", ".2 - .3", ".3 - .5", ".5 - .7", ".7 - .9", "NA"), title="Egalitarian Democracy Score") %>%
        addEasyButton(easyButton(icon="fa-globe",title="Home",onClick=JS("function(btn, map){ map.setView([40, 0], 1.4); }"))) %>%
        addCircles(data=verts_, opacity=.2, fillColor = "blue", color="blue", weight=2, radius =~IMF_cred/100000, lat = ~lat, lng = ~lon)
    }else if(input$var_h=="Undernourishment Prevelance"){
      leaflet(options = leafletOptions(minZoom = 1, maxZoom = 7))%>%
        setView(lat=lat_re(), lng=lng_re(), zoom=zoom_re())%>%
        addPolygons(data=dat_i(),weight=.5, color="black", fillColor=pal2_h(), fillOpacity = .75, popup=pop_cont2_h(), 
                    highlightOptions = highlightOptions(color="white", weight=2, bringToFront = T, sendToBack = T), label=worldmap@data$SOVEREIGNT) %>%
        addLegend(colors=c("#8C510A","#BF812D", "#DFC27D", "#F6E8C3", "#C7EAE5", "#80CDC1", "#35978F", "#01665E", "#808080"), labels=c("<3.5", "3.5 - 5", "5 - 7", "7 - 11", "11 - 15", "15 - 20", "20 - 30",">30", "NA"), title="Undernourishment Rate (%)") %>%
        addEasyButton(easyButton(icon="fa-globe",title="Home",onClick=JS("function(btn, map){ map.setView([40, 0], 1.4); }"))) %>%
        addCircles(data=verts_, opacity=.2, fillColor = "blue", color="blue", weight=2, radius =~IMF_cred/100000, lat = ~lat, lng = ~lon)
    }else if(input$var_h=="Freedom of Expression"){
      leaflet(options = leafletOptions(minZoom = 1, maxZoom = 7))%>%
        setView(lat=lat_re(), lng=lng_re(), zoom=zoom_re())%>%
        addPolygons(data=dat_i(),weight=.5, color="black", fillColor=pal2_h(), fillOpacity = .75, popup=pop_cont2_h(), 
                    highlightOptions = highlightOptions(color="white", weight=2, bringToFront = T, sendToBack = T), label=worldmap@data$SOVEREIGNT) %>%
        addLegend(colors=c("#8C510A","#BF812D", "#DFC27D", "#F6E8C3", "#C7EAE5", "#80CDC1", "#35978F", "#01665E", "#808080"), labels=c("<.05", ".05 - .15", ".15 - .25", ".25 - .4", ".4 - .6", ".6 - .75", ".75 - .9",">.9", "NA"), title="Freedom of Expression Score") %>%
        addEasyButton(easyButton(icon="fa-globe",title="Home",onClick=JS("function(btn, map){ map.setView([40, 0], 1.4); }"))) %>%
        addCircles(data=verts_, opacity=.2, fillColor = "blue", color="blue", weight=2, radius =~IMF_cred/100000, lat = ~lat, lng = ~lon)
    }
    
  })
  
  output$scatter_c<-renderPlotly(
    if(input$var_e=="GDP Growth"){
      if(!input$view %in% c("World", "")){
        plot_ly(data=filter(dat_c()@data, CONTINENT==input$view), x=~inv_tot, y=~gdp_growth, type="scatter", mode="markers", 
                marker=list(color="#AA381E"),
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT==input$view)$SOVEREIGNT, "<br>",
                                     "Chinese Investment: $", filter(dat_c()@data, CONTINENT==input$view)$inv_tot, "<br>",
                                     "GDP Growth Rate: ", filter(dat_c()@data, CONTINENT==input$view)$gdp_growth, "%","<extra></extra>"))%>%
          layout(title="GDP Growth",yaxis=list(title="Annual GDP Growth"),xaxis=list(title="Investment from China"))
      }else{
        plot_ly(data=dat_c()@data, x=~inv_tot, y=~gdp_growth, type="scatter", mode="markers", color=~CONTINENT, 
                hovertemplate=paste0("Country: ", dat_c()@data$SOVEREIGNT, "<br>",
                                     "Chinese Investment: $", dat_c()@data$inv_tot, "<br>",
                                     "GDP Growth Rate: ", dat_c()@data$gdp_growth, "%","<extra></extra>"))%>%
          layout(title="GDP Growth",yaxis=list(title="Annual GDP Growth"),xaxis=list(title="Investment from China"))
      }
    }else if(input$var_e=="Gini"){
      if(!input$view %in% c("World", "")){
        plot_ly(data=filter(dat_c()@data, CONTINENT==input$view), x=~inv_tot, y=~gini, type="scatter", mode="markers", marker=list(color="#AA381E"),
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT==input$view)$SOVEREIGNT, "<br>",
                                     "Chinese Investment: $", filter(dat_c()@data, CONTINENT==input$view)$inv_tot, "<br>",
                                     "Gini Coefficient: ", filter(dat_c()@data, CONTINENT==input$view)$gini, "<extra></extra>"))%>%
          layout(title="Inequality",yaxis=list(title="Gini Coefficient"),xaxis=list(title="Investment from China"))
      }else{
        plot_ly(data=dat_c()@data, x=~inv_tot, y=~gini, type="scatter", mode="markers", color=~CONTINENT,
                hovertemplate=paste0("Country: ", dat_c()@data$SOVEREIGNT, "<br>",
                                     "Chinese Investment: $", dat_c()@data$inv_tot, "<br>",
                                     "Gini Coefficient: ", dat_c()@data$gini, "<extra></extra>"))%>%
          layout(title="Inequality",yaxis=list(title="Gini Coefficient"),xaxis=list(title="Investment from China"))
      }
    }else if(input$var_e=="Unemployment"){
      if(!input$view %in% c("World","")){
        plot_ly(data=filter(dat_c()@data, CONTINENT==input$view),x=~inv_tot, y=~unemp, type="scatter", mode="markers", marker=list(color="#AA381E"),
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT==input$view)$SOVEREIGNT, "<br>",
                                     "Chinese Investment: $", filter(dat_c()@data, CONTINENT==input$view)$inv_tot, "<br>",
                                     "Unemployment Rate: ", round(filter(dat_c()@data, CONTINENT==input$view)$unemp,2), "%","<extra></extra>"))%>%
          layout(title="Unemployment Rate",yaxis=list(title="Unemployment Rate"),xaxis=list(title="Investment from China"))
      }else{
        plot_ly(data=dat_c()@data, x=~inv_tot, y=~unemp, type="scatter", mode="markers", color=~CONTINENT,
                hovertemplate=paste0("Country: ", dat_c()@data$SOVEREIGNT, "<br>",
                                     "Chinese Investment: $", dat_c()@data$inv_tot, "<br>",
                                     "Unemployment Rate: ", round(dat_c()@data$unemp,2), "%","<extra></extra>"))%>%
          layout(title="Unemployment Rate",yaxis=list(title="Unemployment Rate"),xaxis=list(title="Investment from China"))
      }
    }
  )
  
  output$scatter_i<-renderPlotly(
    if(input$var_e=="GDP Growth"){
      if(!input$view %in% c("World","")){
        plot_ly(data=filter(dat_i()@data, CONTINENT==input$view), x=~IMF_cred, y=~gdp_growth, type="scatter", mode="markers", marker=list(color="#05358B"),
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT==input$view)$SOVEREIGNT, "<br>",
                                     "IMF Investment: $", filter(dat_i()@data, CONTINENT==input$view)$IMF_cred, "<br>",
                                     "GDP Growth Rate: ", filter(dat_i()@data, CONTINENT==input$view)$gdp_growth, "%","<extra></extra>"))%>%
          layout(title="GDP Growth",yaxis=list(title="Annual GDP Growth"),xaxis=list(title="Investment from IMF"))
      }else{
        plot_ly(data=dat_i()@data, x=~IMF_cred, y=~gdp_growth, type="scatter", mode="markers", color=~CONTINENT,
                hovertemplate=paste0("Country: ", dat_i()@data$SOVEREIGNT, "<br>",
                                     "IMF Investment: $", dat_i()@data$IMF_cred, "<br>",
                                     "GDP Growth Rate: ", dat_i()@data$gdp_growth, "%","<extra></extra>"))%>%
          layout(title="GDP Growth",yaxis=list(title="Annual GDP Growth"),xaxis=list(title="Investment from IMF"))
      }
    }else if(input$var_e=="Gini"){
      if(!input$view %in% c("World", "")){
        plot_ly(data=filter(dat_i()@data, CONTINENT==input$view), x=~IMF_cred, y=~gini, type="scatter", mode="markers", marker=list(color="#05358B"),
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT==input$view)$SOVEREIGNT, "<br>",
                                     "IMF Investment: $", filter(dat_i()@data, CONTINENT==input$view)$IMF_cred, "<br>",
                                     "Gini Coefficient: ", filter(dat_i()@data, CONTINENT==input$view)$gini,"<extra></extra>"))%>%
          layout(title="Inequality",yaxis=list(title="Gini Coefficient"),xaxis=list(title="Investment from IMF"))
      }else{
        plot_ly(data=dat_i()@data, x=~IMF_cred, y=~gini, type="scatter", mode="markers", color=~CONTINENT,
                hovertemplate=paste0("Country: ", dat_i()@data$SOVEREIGNT, "<br>",
                                     "IMF Investment: $", dat_i()@data$IMF_cred, "<br>",
                                     "Gini Coefficient: ", dat_i()@data$gini,"<extra></extra>"))%>%
          layout(title="Inequality",yaxis=list(title="Gini Coefficient"),xaxis=list(title="Investment from IMF"))
      }
    }else if(input$var_e=="Unemployment"){
      if(!input$view %in% c("World", "")){
        plot_ly(data=filter(dat_i()@data, CONTINENT==input$view), x=~IMF_cred, y=~unemp, type="scatter", mode="markers", marker=list(color="#05358B"),
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT==input$view)$SOVEREIGNT, "<br>",
                                     "IMF Investment: $", filter(dat_i()@data, CONTINENT==input$view)$IMF_cred, "<br>",
                                     "Unemployment Rate: ", round(filter(dat_i()@data, CONTINENT==input$view)$unemp,2), "%","<extra></extra>"))%>%
          layout(title="Unemployment Rate",yaxis=list(title="Percent Unemployed"),xaxis=list(title="Investment from IMF"))
      }else{
        plot_ly(data=dat_i()@data, x=~IMF_cred, y=~unemp, type="scatter", mode="markers", color=~CONTINENT,
                hovertemplate=paste0("Country: ", dat_i()@data$SOVEREIGNT, "<br>",
                                     "IMF Investment: $", dat_i()@data$IMF_cred, "<br>",
                                     "Unemployment Rate: ", round(dat_i()@data$unemp,2), "%","<extra></extra>"))%>%
          layout(title="Unemployment Rate",yaxis=list(title="Percent Unemployed"),xaxis=list(title="Investment from IMF"))
      }
    }
  )
  
  output$scatter_ch<-renderPlotly(
    if(input$var_h=="Undernourishment Prevelance"){
      if(!input$view %in% c("World", "")){
        plot_ly(data=filter(dat_c()@data, CONTINENT==input$view), x=~inv_tot, y=~undernourishment, type="scatter", mode="markers", marker=list(color="#AA381E"),
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT==input$view)$SOVEREIGNT, "<br>",
                                     "Chinese Investment: $", filter(dat_c()@data, CONTINENT==input$view)$inv_tot, "<br>",
                                     "Undernourishment Rate: ", filter(dat_c()@data, CONTINENT==input$view)$undernourishment, "%","<extra></extra>"))%>%
          layout(title="Undernourishment",yaxis=list(title="Percent Undernourished"),xaxis=list(title="Investment from China"))
      }else{
        plot_ly(data=dat_c()@data, x=~inv_tot, y=~undernourishment, type="scatter", mode="markers", color=~CONTINENT,
                hovertemplate=paste0("Country: ", dat_c()@data$SOVEREIGNT, "<br>",
                                     "Chinese Investment: $", dat_c()@data$inv_tot, "<br>",
                                     "Unemployment Rate: ", dat_c()@data$undernourishment, "%","<extra></extra>"))%>%
          layout(title="Undernourishment",yaxis=list(title="Percent Undernourished"),xaxis=list(title="Investment from China"))
      }
    }else if(input$var_h=="Democracy Score"){
      if(!input$view %in% c("World", "")){
        plot_ly(data=filter(dat_c()@data, CONTINENT==input$view), x=~inv_tot, y=~dem_score, type="scatter", mode="markers", marker=list(color="#AA381E"),
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT==input$view)$SOVEREIGNT, "<br>",
                                     "Chinese Investment: $", filter(dat_c()@data, CONTINENT==input$view)$inv_tot, "<br>",
                                     "Egalitarian Democracy Score: ", filter(dat_c()@data, CONTINENT==input$view)$dem_score,"<extra></extra>"))%>%
          layout(title="Egalitarian Democracy",yaxis=list(title="Egalitarian Democracy Score"),xaxis=list(title="Investment from China"))
      }else{
        plot_ly(data=dat_c()@data, x=~inv_tot, y=~dem_score, type="scatter", mode="markers", color=~CONTINENT,
                hovertemplate=paste0("Country: ", dat_c()@data$SOVEREIGNT, "<br>",
                                     "Chinese Investment: $", dat_c()@data$inv_tot, "<br>",
                                     "Egalitarian Democracy Score: ", dat_c()@data$dem_score,"<extra></extra>"))%>%
          layout(title="Egalitarian Democracy",yaxis=list(title="Egalitarian Democracy Score"),xaxis=list(title="Investment from China"))
      }
    }else if(input$var_h=="Freedom of Expression"){
      if(!input$view %in% c("World", "")){
        plot_ly(data=filter(dat_c()@data, CONTINENT==input$view), x=~inv_tot, y=~exp, type="scatter", mode="markers", marker=list(color="#AA381E"),
                hovertemplate=paste0("Country: ", filter(dat_c()@data, CONTINENT==input$view)$SOVEREIGNT, "<br>",
                                     "Chinese Investment: $", filter(dat_c()@data, CONTINENT==input$view)$inv_tot, "<br>",
                                     "Freedom of Expression Score: ", filter(dat_c()@data, CONTINENT==input$view)$exp,"<extra></extra>"))%>%
          layout(title="Freedom of Expression",yaxis=list(title="Freedom of Expression Score"),xaxis=list(title="Investment from China"))
      }else{
        plot_ly(data=dat_c()@data, x=~inv_tot, y=~exp, type="scatter", mode="markers", color=~CONTINENT,
                hovertemplate=paste0("Country: ", dat_c()@data$SOVEREIGNT, "<br>",
                                     "Chinese Investment: $", dat_c()$inv_tot, "<br>",
                                     "Freedom of Expression Score: ", dat_c()@data$exp,"<extra></extra>"))%>%
          layout(title="Freedom of Expression",yaxis=list(title="Freedom of Expression Score"),xaxis=list(title="Investment from China"))
      }
    }
  )
  
  output$scatter_ih<-renderPlotly(
    if(input$var_h=="Undernourishment Prevelance"){
      if(!input$view %in% c("World", "")){
        plot_ly(data=filter(dat_i()@data, CONTINENT==input$view), x=~IMF_cred, y=~undernourishment, type="scatter", mode="markers", marker=list(color="#05358B"),
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT==input$view)$SOVEREIGNT, "<br>",
                                     "IMF Investment: $", filter(dat_i()@data, CONTINENT==input$view)$IMF_cred, "<br>",
                                     "Undernourishment Rate: ", filter(dat_i()@data, CONTINENT==input$view)$undernourishment, "%" ,"<extra></extra>"))%>%
          layout(title="Unernourishment",yaxis=list(title="Percent Undernourished"),xaxis=list(title="Investment from IMF"))
      }else{
        plot_ly(data=dat_i()@data, x=~IMF_cred, y=~undernourishment, type="scatter", mode="markers", color=~CONTINENT,
                hovertemplate=paste0("Country: ", dat_i()@data$SOVEREIGNT, "<br>",
                                     "IMF Investment: $", dat_c()@data$IMF_cred, "<br>",
                                     "Undernourishment Rate: ", dat_i()@data$dem_score, "%" ,"<extra></extra>"))%>%
          layout(title="Unernourishment",yaxis=list(title="Percent Undernourished"),xaxis=list(title="Investment from IMF"))
      }
    }else if(input$var_h=="Democracy Score"){
      if(!input$view %in% c("World", "")){
        plot_ly(data=filter(dat_i()@data, CONTINENT==input$view), x=~IMF_cred, y=~dem_score, type="scatter", mode="markers", marker=list(color="#05358B"),
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT==input$view)$SOVEREIGNT, "<br>",
                                     "IMF Investment: $", filter(dat_i()@data, CONTINENT==input$view)$IMF_cred, "<br>",
                                     "Egalitarian Democracy Score: ", filter(dat_i()@data, CONTINENT==input$view)$dem_score,"<extra></extra>"))%>%
          layout(title="Egalitarian Democracy",yaxis=list(title="Egalitarian Democracy Score"),xaxis=list(title="Investment from IMF"))
      }else{
        plot_ly(data=dat_i()@data, x=~IMF_cred, y=~dem_score, type="scatter", mode="markers", color=~CONTINENT,
                hovertemplate=paste0("Country: ", dat_i()@data$SOVEREIGNT, "<br>",
                                     "IMF Investment: $", dat_i()@data$IMF_cred, "<br>",
                                     "Egalitarian Democracy Score: ", dat_i()@data$dem_score,"<extra></extra>"))%>%
          layout(title="Egalitarian Democracy",yaxis=list(title="Egalitarian Democracy Score"),xaxis=list(title="Investment from IMF"))
      }
    }else if(input$var_h=="Freedom of Expression"){
      if(!input$view %in% c("World", "")){
        plot_ly(data=filter(dat_i()@data, CONTINENT==input$view), x=~IMF_cred, y=~exp, type="scatter", mode="markers", marker=list(color="#05358B"),
                hovertemplate=paste0("Country: ", filter(dat_i()@data, CONTINENT==input$view)$SOVEREIGNT, "<br>",
                                     "IMF Investment: $", filter(dat_i()@data, CONTINENT==input$view)$IMF_cred, "<br>",
                                     "Freedom of Expression Score: ", filter(dat_i()@data, CONTINENT==input$view)$exp, "%" ,"<extra></extra>"))%>%
          layout(title="Freedom of Expression",yaxis=list(title="Freedom of Expression Score"),xaxis=list(title="Investment from IMF"))
      }else{
        plot_ly(data=dat_i()@data, x=~IMF_cred, y=~exp, type="scatter", mode="markers", color=~CONTINENT,
                hovertemplate=paste0("Country: ", dat_i()@data$SOVEREIGNT, "<br>",
                                     "IMF Investment: $", dat_i()@data$IMF_cred, "<br>",
                                     "Freedom of Expression Score: ", dat_i()@data$exp,"<extra></extra>"))%>%
          layout(title="Freedom of Expression",yaxis=list(title="Freedom of Expression Score"),xaxis=list(title="Investment from IMF"))
      }
    }
  )
  
  output$leaf_c<-renderLeaflet(map_c())
  output$leaf_c2<-renderLeaflet(map_c2())
  
  output$leaf_c_inv<-renderLeaflet(map_c_inv())
  output$leaf_i_inv<-renderLeaflet(map_i_inv())
  
  output$leaf_i<-renderLeaflet(map_i())
  output$leaf_i2<-renderLeaflet(map_i2())
  
  output$blurb<-renderText("Unlike what many other countries do, China is not known to report its aid projects or and overseas spending to international initiatives voluntarily. Enter Aid Data, a research collaborative at the College of William & Mary. They’ve recently launched a comprehensive dataset tracking Chinese spending to different regions and sectors globally. According to their reports, 300 Chinese governmental organizations contributed a total reported $843 billion dollars worth of spending to over 13,000 projects, tracked in the last twenty years. This Chinese investment, rivaling the United States' aid spending in that time-span has drawn international attention, especially since much of this money goes to countries like Russia and Angola that are oil-rich and hence, potentially valuable allies.  This project seeks to visually compare and contrast the countries receiving aid from China and the IMF across different indices spanning economic development, human rights and political health. We hypothesize that countries receiving their aid from China -- who unlike the IMF has less stringent contingencies -- perform very differently than those getting their aid from other organizations and countries, specifically through the IMF.")
  
  chn_inv<-chn_inv %>%
    group_by(Year) %>%
    mutate(ann_tot = sum(as.numeric(inv_tot)))
  
  # output$line_c<-renderPlot({
  #   ggplot()+
  #     geom_line(data=filter(chn_inv[!duplicated(chn_inv$Year),], Year<2021), aes(x=Year,y=ann_tot/10000), color='darkred', size=1.2)+
  #     labs(x='Year', y='Total Investment in Billions ($)', title='Total Chinese Investment, 2005-2020')+
  #     theme(
  #       panel.border = element_blank(),  
  #       panel.grid.major = element_blank(),
  #       panel.grid.minor = element_blank(),
  #       panel.background = element_blank(),
  #       axis.line = element_line(colour = "black"),
  #       plot.title = element_text(hjust = 0.5), 
  #       legend.position = 'none')
  # }#, width=250, height=250
  # 
  # )
  
  # imf<-imf %>%
  #   group_by(Year) %>%
  #   mutate(ann_imf=sum(as.numeric(IMF_cred),na.rm=T))
  
  # output$line_imf<-renderPlot({
  #   ggplot()+
  #     geom_line(data=filter(imf[!duplicated(imf$Year),], Year<2021), aes(x=Year,y=ann_imf/10000000000, lwd=1), color='dodgerblue4', size=1.2)+
  #     labs(x='Year', y='Total Investment in Billions ($)', title='Total Investment Aid, 1990-2020')+
  #     theme(
  #       panel.border = element_blank(),  
  #       panel.grid.major = element_blank(),
  #       panel.grid.minor = element_blank(),
  #       panel.background = element_blank(),
  #       axis.line = element_line(colour = "black"),
  #       plot.title = element_text(hjust = 0.5), 
  #       legend.position = 'none')
  # }#, width=250, height=250
  # )
  
  sec<-reactive({
    if(input$country_=="World" | input$country_==""){
      chn_inv%>%
        select(Region, Sector, Year)%>%
        group_by(Sector)%>%
        summarize(frequency=n())
    }else{
      chn_inv%>%
        filter(country2==input$country_) %>%
        select(Region, Sector, Year)%>%
        group_by(Sector)%>%
        summarize(frequency=n())
    }
  })
  
  output$sect<-renderPlot({
    ggplot(sec(), aes(x=Sector, y=frequency, fill=Sector))+ 
      geom_bar(stat='identity')+
      coord_flip()+
      theme_bw()+
      guides(fill=F)+
      labs(title='Chinese Investment by Sector', y='Total Number of Projects', x='')+
      scale_fill_manual(values=met.brewer("Renoir", n=14, type='continuous'))+
      theme(
        panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5), 
        legend.position = 'none'
      )
  })
  
  liney_line<-reactive({
    if(input$country_=="World" | input$country_==""){
      tot_reg<-chn_inv%>%
        select(Region, Year, Quantity.in.Millions)%>%
        group_by(Year, Region)%>%
        mutate(`Annual Total Investment`=sum(as.numeric(gsub("[[:punct:]]", "", Quantity.in.Millions)))/100)
      ggplot(tot_reg, aes(x=Year, y=`Annual Total Investment`, color=Region))+
        geom_line()+
        geom_point()+
        labs(x='Year', y='Investment in Billions ($)', title='Chinese Investment by Region, 2005-2021')+
        theme(
          panel.border = element_blank(),  
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          plot.title = element_text(hjust = 0.5), 
          legend.position = 'none'
        )+
        facet_wrap(vars(Region))+
        scale_color_manual(values=met.brewer("Renoir", n=9, type='continuous'))
    }else{
      tot_reg<-chn_inv%>%
        filter(country2==input$country_) %>%
        select(Region, Year, Quantity.in.Millions)%>%
        group_by(Year)%>%
        mutate(`Annual Total Investment`=sum(as.numeric(gsub("[[:punct:]]", "", Quantity.in.Millions)))/100)
      ggplot(tot_reg, aes(x=Year, y=`Annual Total Investment`))+
        geom_line(color="#AA381E")+
        geom_point(color="#AA381E")+
        labs(x='Year', y='Investment in Billions ($)', title='Chinese Investment, 2005-2021')+
        theme(
          panel.border = element_blank(),  
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          plot.title = element_text(hjust = 0.5), 
          legend.position = 'none'
        )+
        scale_color_manual(values=met.brewer("Renoir", n=9, type='continuous'))
    }
  })
  
  output$line_out<-renderPlotly(ggplotly(liney_line()))
  
  imf_liney_line<-reactive({
    gini<-rename(gini, Year=year, name=country, `Gini Coefficient`=`SI.POV.GINI`)
    imf<-left_join(imf, gini, by=c("name", "Year"))
    imf[imf$name=="Venezuela, RB",]$name<-"Venezuela"
    imf[imf$name=="United Arab Emirates",]$name<-"UAE"
    imf[imf$name=="United Kingdom",]$name<-"Britain"
    imf[imf$name=="United States",]$name<-"USA"
    imf[imf$name=="Lao PDR",]$name<-"Laos"
    imf[imf$name=="Iran, Islamic Rep.",]$name<-"Iran"
    imf[imf$name=="Yemen, Rep.",]$name<-"Yemen"
    imf[imf$name=="Congo, Dem. Rep.",]$name<-"Democratic Republic of the Congo"
    imf[imf$name=="Congo, Rep.",]$name<-"Congo"
    imf[imf$name=="Syrian Arab Republic",]$name<-"Syria"
    imf[imf$name=="Egypt, Arab Rep.",]$name<-"Egypt"
    imf[imf$name=="Trinidad and Tobago",]$name<-"Trinidad-Tobago"
    imf[imf$name=="Bahamas, The",]$name<-"Bahamas"
    imf[imf$name=="Brunei Darussalam",]$name<-"Brunei"
    imf[imf$name=="Korea, Dem. People's Rep.",]$name<-"North Korea"
    imf[imf$name=="Korea, Rep.",]$name<-"South Korea"
    imf[imf$name=="Bosnia and Herzegovina",]$name<-"Bosnia"
    imf[imf$name=="Kyrgyz Republic",]$name<-"Kyrgyzstan"
    imf[imf$name=="Cote d'Ivoire",]$name<-"Ivory Coast"
    imf[imf$name=="Sao Tome and Principe",]$name<-"Sao Tome"
    imf[imf$name=="Cabo Verde",]$name<-"Cape Verde"
    imf[imf$name=="North Macedonia",]$name<-"Macedonia"
    
    dem[dem$country_name=="United States of America",]$country_name<-"USA"
    dem[dem$country_name=="United Kingdom",]$country_name<-"Britain"
    dem[dem$country_name=="United Arab Emirates",]$country_name<-"UAE"
    dem[dem$country_name=="Burma/Myanmar",]$country_name<-"Myanmar"
    dem[dem$country_name=="Russia",]$country_name<-"Russian Federation"
    dem[dem$country_name=="Republic of the Congo",]$country_name<-"Congo"
    dem[dem$country_name=="Trinidad and Tobago",]$country_name<-"Trinidad-Tobago"
    dem[dem$country_name=="Bosnia and Herzegovina",]$country_name<-"Bosnia"
    dem[dem$country_name=="Sao Tome and Principe",]$country_name<-"Sao Tome"
    dem[dem$country_name=="North Macedonia",]$country_name<-"Macedonia"
    imf<-left_join(imf, dem, by=c("name"="country_name", "Year"="year"))
    imf<-rename(imf, `Egalitarian Democracy Score`=v2x_egaldem)
    imf<-filter(imf, Year<2021)
    
    if(input$country_ %in% c("World","")){
      imf_<-right_join(imf, chn_inv[c("Region","country2")], by=c("name"="country2"))
      imf_<-imf_ %>%
        group_by(Year, Region)%>%
        mutate(`Annual Total`=round(sum(IMF_cred, na.rm=T)/1000000000,2))
      ggplot(imf_, aes(x=Year, y=`Annual Total`, color=Region))+
        geom_line()+
        geom_point()+
        labs(x='Year', y='Investment in Billions ($)', title='IMF Investment by Region, 2005-2021')+
        theme(
          panel.border = element_blank(),  
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          plot.title = element_text(hjust = 0.5), 
          legend.position = 'none'
        )+
        facet_wrap(vars(Region))+
        scale_color_manual(values=met.brewer("Renoir", n=9, type='continuous'))
    }else{
      imf<-filter(imf, name==input$country_)
      imf$IMF_cred<-round(imf$IMF_cred/1000000000,2)
      ggplot(rename(imf, `Credit from IMF`=IMF_cred), aes(x=Year, y=`Credit from IMF`))+
        geom_line(color="#05358B")+
        #geom_line(aes(y=`Gini Coefficient`), color="orange")+
        #geom_line(aes(y=`Egalitarian Democracy Score`*100), color="green")+
        geom_point(color="#05358B")+
        labs(x='Year', y='Investment in Billions ($)', title='IMF Investment, 2005-2021')+ 
        theme(
          panel.border = element_blank(),  
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          plot.title = element_text(hjust = 0.5), 
          legend.position = 'none'
        )+
        scale_color_manual(values=met.brewer("Renoir", n=9, type='continuous'))
    }
  })
  
  output$line_out_imf<-renderPlotly(ggplotly(imf_liney_line()))
    
  output$data_table <- renderDT(
    if(input$time == "2018"){
      now_data
    }
    else{
      hist_data
    }
  )
    
  output$sources <- renderUI({
    line1 <- "<b>China Global Investment Tracker</b> https://www.aei.org/china-global-investment-tracker/"
    line2 <- "<b>World Development Indicators (World Bank data)</b> https://cran.r-project.org/web/packages/WDI/index.html"
    line3 <- "<b>V-Dem Institute</b> https://github.com/vdeminstitute"
    line4 <- "<b>Maps</b> https://www.naturalearthdata.com/"
    HTML(paste(line1, line2, line3, line4,
               sep = '<br/><br/>'))
  })
    
  output$further_reading <- renderUI({
    line1 <- "<b>AidData Research Lab</b> https://www.aiddata.org/datasets"
    line2 <- "<b>imfr - access IMF's API</b> https://cran.r-project.org/web/packages/imfr/index.html"
    line3 <- "<b>John Hopkins SAIS China-Africa Research Initiative</b> http://www.sais-cari.org/chinese-investment-in-africa"
    line4 <- "<b>Human Rights Scores Dataverse</b> https://dataverse.harvard.edu/dataverse/HumanRightsScores"
    line5 <- "<b>Press Freedom from RSF</b> https://rsf.org/en/index"
    HTML(paste(line1, line2, line3, line4, line5,
               sep = '<br/><br/>'))
  })
  
} #This one ends server

shinyApp(ui, server)
