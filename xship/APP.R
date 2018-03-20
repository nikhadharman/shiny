#....................................
library(shinydashboard,warn.conflicts = FALSE)
library(plotly,warn.conflicts = FALSE)
library(DT,warn.conflicts = FALSE)
library(fmsb)
library (scales)
library(markdown)
library(plyr)
library(zoo)
library(shinyjs)
library(xlsx)
library(xlsxjars)
library(XLConnect,warn.conflicts = FALSE)
library(XLConnectJars,warn.conflicts = FALSE)
library(ggplot2)
library(reshape2)
library(rdrop2)
library(leaflet)
library(shinycssloaders)
options(spinner.color="#0f87f0")
options(spinner.size= 1)
r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()


DATA=data.frame(read.csv("data/Data.csv"))
DATAT=data.frame(read.csv("data/April MTM data csv.csv"))
ADADATA=data.frame(read.csv("data/ADA DATA.csv"))
Shoptrial=data.frame(read.csv("data/Shop Trial Data.csv"))
VESSELDETAILS=data.frame(read.csv("data/Vessel Detail.csv"))
seatrialdata=data.frame(read.csv("data/Sea Trial Data.csv"))
Coeff =data.frame(read.csv("data/Coefficient.csv"))
cp = data.frame(read.csv("data/Datamod.csv"))

admin=div(sidebarMenu(id="mytab",
                      menuItem(strong("Dashboard"), tabName = "Dashboard", icon = icon("dashboard"),selected = T),
                      menuItem(strong("Voyage Calculator"), tabName = "vc", icon = icon("gear")),
                      menuItem(strong("Charter Party Anlaysis"), tabName = "CPA", icon = icon("certificate")),
                      menuItem(strong("Performance Montoring"), tabName = "PER", icon = icon("line-chart"),
                               menuSubItem("Vessel Performance", tabName = "PERFORMANCE", icon = icon("angle-right")),
                               menuSubItem("Fleet Performance", tabName = "FLEETCOMPARISON", icon = icon("angle-right")),
                               menuSubItem("Fleet Operational Overview", tabName = "FOO", icon = icon("angle-right")),
                               menuSubItem("Vessel Operational Overview", tabName = "VOO", icon = icon("angle-right"))),
                      menuItem(strong("Engine Monitoring"),tabName="ENGINE",icon=icon("tachometer")),
                      menuItem(strong("Enviromental Monitoring"),tabName="EM",icon=icon("bar-chart")),
                      menuItem(strong("ISO Method"), tabName = "iso", icon = icon("certificate")),
                      menuItem(strong("Vessel Details"), tabName = "details", icon = icon("ship")),
                      menuItem(strong("Database"), tabName = "database", icon = icon("database")),
                      menuItem(strong("Performance Analysis"), tabName = "analysis", icon = icon("pie-chart"),
                               menuSubItem("Data Analysis", tabName = "Datamonitoring", icon = icon("angle-right")),
                               menuSubItem("Analysis Prediction ", tabName = "PREDICTION", icon = icon("angle-right"))  ,   
                               menuSubItem("Performance Curve", tabName = "PERFORMANCECURVE", icon = icon("angle-right"))),
                      menuItem(strong("Pre/Post Analysis"), tabName = "Prepost", icon = icon("map-signs "),
                               menuSubItem("Data Filtering", tabName = "PreDatamonitoring", icon = icon("angle-right")),
                               menuSubItem("Operating Profile",tabName = "preOperatingprofile",icon = icon("angle-right")),
                               menuSubItem("Prediction", tabName = "prePREDICTION", icon = icon("angle-right"))  ,   
                               menuSubItem("Performance Curve", tabName = "prePERFORMANCECURVE", icon = icon("angle-right"))),
                      menuItem(strong("Info"), tabName = "info", icon = icon("question"))
                      
                      
                      
))
analy=div(sidebarMenu(id="mytab",
                      #menuItem(strong("Dashboard"), tabName = "Dashboard", icon = icon("dashboard"),selected = T),
                      menuItem(strong("Enviromental Monitoring"),tabName="EM",icon=icon("bar-chart"))
                      #menuItem(strong("ISO Method"), tabName = "iso", icon = icon("certificate")),
                      #menuItem(strong("Vessel Details"), tabName = "details", icon = icon("ship")),
                       #   menuItem(strong("Info"), tabName = "info", icon = icon("question"))
                      
))

other= div(
  sidebarMenu(id="mytab",
              menuItem(strong("Dashboard"), tabName = "Dashboard", icon = icon("dashboard"),selected = T),
              menuItem(strong("Voyage Calculator"), tabName = "vc", icon = icon("gear")),
              menuItem(strong("Charter Party Anlaysis"), tabName = "CPA", icon = icon("certificate")),
              menuItem(strong("Performance Montoring"), tabName = "PER", icon = icon("line-chart"),
                       menuSubItem("Vessel Performance", tabName = "PERFORMANCE", icon = icon("angle-right")),
                       menuSubItem("Fleet Performance", tabName = "FLEETCOMPARISON", icon = icon("angle-right")),
                       menuSubItem("Fleet Operational Overview", tabName = "FOO", icon = icon("angle-right")),
                       menuSubItem("Vessel Operational Overview", tabName = "VOO", icon = icon("angle-right"))),
              menuItem(strong("Engine Monitoring"),tabName="ENGINE",icon=icon("tachometer")),
              menuItem(strong("Enviromental Monitoring"),tabName="EM",icon=icon("bar-chart")),
              menuItem(strong("ISO Method"), tabName = "iso", icon = icon("certificate")),
              menuItem(strong("Vessel Details"), tabName = "details", icon = icon("ship")),
              menuItem(strong("Database"), tabName = "database", icon = icon("database")),
              menuItem(strong("Performance Analysis"), tabName = "analysis", icon = icon("pie-chart"),
                       menuSubItem("Data Analysis", tabName = "Datamonitoring", icon = icon("angle-right")),
                       menuSubItem("Analysis Prediction ", tabName = "PREDICTION", icon = icon("angle-right"))  ,   
                       menuSubItem("Performance Curve", tabName = "PERFORMANCECURVE", icon = icon("angle-right"))),
              menuItem(strong("PRE/POST Analysis"), tabName = "Prepost", icon = icon("map-signs "),
                       menuSubItem("Data Filtering", tabName = "PreDatamonitoring", icon = icon("angle-right")),
                       menuSubItem("Operating Profile",tabName = "preOperatingprofile",icon = icon("angle-right")),
                       menuSubItem("Prediction", tabName = "prePREDICTION", icon = icon("angle-right"))  ,   
                       menuSubItem("Performance Curve", tabName = "prePERFORMANCECURVE", icon = icon("angle-right"))),
              menuItem(strong("Info"), tabName = "info", icon = icon("question"))
              
              
  ))



mainBody=div( 
  
  tabItems(
    # First tab content
    
    
    tabItem(tabName = "Dashboard",
            htmlOutput("dash"), htmlOutput("dash1")),
    
    
    
    tabItem(tabName = "vc", 
            box( status="info" ,solidHeader =TRUE,width=NULL,collapsible = T,radioButtons("vctype",label=NULL,choices=c("All Vessel","Fleet wise"), selected = NULL, inline = TRUE, width = NULL),
                 column(width=3,htmlOutput("vcfleet")),
                 column(width=3,htmlOutput("vcvessel")),  column(width=4,htmlOutput("vccdate")),column(width=2,htmlOutput("vccType"))),
            br(),
            tabsetPanel(tabPanel(title="ETA & FOC Calculator",
                                 br(),
                                 box(solidHeader = TRUE,status="info",width = NULL,
                                     br(),fluidRow(column(width= 3,numericInput("DT",label="Draft",value = 10,min=4,max = 30 )),column(width=3,numericInput("SP",label = "Speed",value = 12,min= 2,max=30)),
                                                   column(width = 3,numericInput("DIS",label="Voyage Distance",value = 5000)),column(width=3,valueBoxOutput("DAYS",width=6))),
                                     box(fluidRow(column(width=4,dataTableOutput("Cal")),column(width=4,plotlyOutput("FOCAL")),column(width = 4,plotlyOutput("PCAL")) ),width=NULL,status="info" ,solidHeader =TRUE)
                                 )),
                        tabPanel(title="Speed Calculator",br(),
                                 box(solidHeader = TRUE,status="info",width = NULL,
                                     br(),fluidRow(column(width = 3,numericInput("SPEED_DIS",label="Voyage Distance",value = 5000)),column(width=3,numericInput("SPEED_DAY",label = "Voyage Days",value = 12)),column(width= 3,numericInput("DT",label="Draft",value = 10,min=4,max = 30 )),
                                                   column(width=3,valueBoxOutput("SP",width=6))),
                                     box(fluidRow(column(width=4,dataTableOutput("Cal2")),column(width=4,plotlyOutput("FOCAL2")),column(width = 4,plotlyOutput("PCAL2")) ),width=NULL,status="info" ,solidHeader =TRUE)
                                 ))
                        
                        
            )),
    
    
    
    tabItem(tabName = "details", 
            box(solidHeader = TRUE,status="info",collapsible = T,align = "left",width=NULL,radioButtons("selecttype",label=NULL,choices=c("All Vessel","Fleet Wise"), selected = "All Vessel", inline = TRUE, width = NULL),
                column(width=4,htmlOutput("vfleet")),column(width=4,htmlOutput("selectUI"))),
            
            tabsetPanel( 
              
              tabPanel(title="Vessel Particulars",selected=NULL,
                       box(solidHeader = TRUE,status="info",align = "left",width=NULL,column(width=3,htmlOutput("yob")), column(width=3,offset=1,htmlOutput("loa")),column(width=3,offset=1,htmlOutput("b")),
                           column(width=3,htmlOutput("Draft1UI")),
                           column(width=3,offset=1,htmlOutput("Draft2UI")),          
                           column(width=3,offset=1,htmlOutput("DIS")),
                           column(width=3,htmlOutput("speed2UI")),column(width=3,offset=1,htmlOutput("MCRUI")),
                           column(width=10, htmlOutput("vesselimage")))
              ),
              
              tabPanel(title= "Hydrostatics Data",
                       
                       box(width= NULL,solidHeader = TRUE, status="info", br(),
                           column(width=6,box(solidHeader = TRUE,status="info",width= NULL,dataTableOutput("hydros"))),
                           column(width=6,box(solidHeader = TRUE,status="info",width= NULL,plotlyOutput("Dplot")), br(),
                                  box(width= NULL,solidHeader = TRUE, status="info",plotlyOutput("Hplot") )))
                       
                       
              ),
              
              
              
              tabPanel(title= "Shop Trail",
                       box(width=NULL,solidHeader = TRUE,status="info",
                           column(width=6,box(solidHeader = TRUE,status="info",width= NULL,DT::dataTableOutput("shoptable"))),
                           column(width=6,box(solidHeader = TRUE,status="info",width= NULL,plotlyOutput("Shopplot"))))
              ),
              tabPanel(title= "Sea Trial",br(),
                       
                       box(width= NULL,solidHeader = TRUE,title = "Ballast Condition", status="info", br(),
                           column(width=6,box(solidHeader = TRUE,status="info",width= NULL, dataTableOutput("seatrialtable"))),
                           column(width=6,box(solidHeader = TRUE,status="info",width= NULL,plotlyOutput("seatrialplot"))
                           ))
                       
                       
              )
              
            )), 
    tabItem(tabName = "database", 
            box(solidHeader = TRUE,status="info",collapsible = T,align = "left",width=NULL,radioButtons("databaseselecttype",label=NULL,choices=c("All Vessel","Fleet Wise"), selected = "All Vessel", inline = TRUE, width = NULL),
                column(width=4,htmlOutput("databasefleet")),column(width=4,htmlOutput("databaseselectUI")),column(width=3,br(),htmlOutput("Wdata"))),
            
            tabsetPanel(  tabPanel(title= "Reported Data",
                                   box(width=NULL,solidHeader = TRUE,status="info",
                                       DT::dataTableOutput("table"))
            ),
            
            tabPanel(title= "ADA Data",
                     box(width=NULL,solidHeader = TRUE,status="info",
                         DT::dataTableOutput("ADAtable"))
            )
            )),
    
    
    tabItem(tabName= "Operatingprofile",htmlOutput("myoperatingprofiletabs")
            
    ),
    tabItem(tabName = "Datamonitoring",box(solidHeader = TRUE,status="info",collapsible = T,width=NULL,radioButtons("filttype",label=NULL,choices=c("All vessel","Fleet Wise"), selected = NULL, inline = TRUE, width = NULL),
                                           column(width=4,htmlOutput("filtfleet")),
                                           column(width=4,htmlOutput("filtvessel"))),
            htmlOutput("mytabs")
    ),
    
    
    tabItem(tabName = "PREDICTION",htmlOutput("mypredicationtabs")
    ),
    
    tabItem(tabName = "PERFORMANCECURVE",htmlOutput("myperformancetabs") 
    ),
    
    
    
    tabItem(tabName = "PERFORMANCE",
            box( status="info" ,solidHeader =TRUE,width=NULL,collapsible = T,radioButtons("clienttype",label=NULL,choices=c("All Vessel","Fleet Wise"), selected = NULL, inline = TRUE, width = NULL),
                 column(width=3,htmlOutput("clientfleet")),
                 column(width=3,htmlOutput("clientvessel")),  column(width=4,htmlOutput("cdate")),column(width=2,htmlOutput("cType"))),
            br(),
            
            tabsetPanel(
              tabPanel(title="Performance Curve",selected=NULL,
                       br(),htmlOutput("seastate1"), box(title= strong("FO Calculator"),status="info" ,solidHeader =TRUE,box(column(dataTableOutput("FO_cal1"),width=5),status="info" ,solidHeader =TRUE,width=NULL,
                                                                                                                             column(br(),plotlyOutput("speedFO1"),offset = 1,width = 6)),width=NULL),
                       br(),
                       box(title= strong("Power Calculator"),status="info" ,solidHeader =TRUE,box(column(dataTableOutput("power_cal1"),width = 5),status="info" ,solidHeader =TRUE,width=NULL,column(br(),plotlyOutput("speedpower1"),offset = 1,width=6)),width = NULL)
                       
              ),
              
              
              
              tabPanel(title= "Deviation Observation ",br(),
                       box(width=NULL,status="info" ,solidHeader =TRUE,fluidRow(column(width=3,dateRangeInput("dop", label = "Deviation Period" ,start=as.Date("2016-01-01"),end=Sys.Date())),
                                                                                column(width=3,valueBoxOutput("DPOWER",width="70%")),column(width=3,valueBoxOutput("DFO",width="70%")),
                                                                                column(width=3,downloadButton('Ddata', 'Download'))
                       ),br(),
                       fluidRow(column(width=6,box(title=strong("Power Prediction"),status = "info",solidHeader = TRUE,width=NULL,
                                                   plotlyOutput("Deviatepower"))),
                                column(width=6,box(title=strong("FO Prediction"),status = "info",solidHeader = TRUE,width =NULL,
                                                   plotlyOutput("DeviateFO"))) )),
                       
                       box(align="center",status="info" ,solidHeader =TRUE,dataTableOutput("DDT"),width=NULL)
              ),
              tabPanel(title ="Seatrial Comparision",br(),
                       box(fluidRow(column(width=3,fileInput("Seatrial",label ="DATA"),
                                           tags$style(type="text/css", "#Seatrial_progress { max-width: 100px; }")),
                                    column(width=3,offset=1,htmlOutput("TrialDraft")),
                                    column(width=3,offset=1,numericInput("Seacon",label="Trial Sea State",value=3)))
                           ,br(),
                           fluidRow(box(align="center",status="info" ,solidHeader =TRUE,width=12,column(width=4,dataTableOutput("STT"),br(),
                                                                                                        box(width=7,status="info" ,solidHeader =TRUE,strong(textOutput("text1")),br(),strong(textOutput("text2")))),
                                        column(width=4,offset=2,plotlyOutput("SPOWER"))) ),
                           
                           status="info" ,solidHeader =TRUE,width=12))
              
            )
    ),
    
    
    
    tabItem(tabName= "FLEETCOMPARISON",
            
            box(width=NULL,solidHeader = TRUE,status = "info",collapsible = T,radioButtons("selectiontype",label=NULL,choices=c("All Vessel","Fleet Wise"), selected = NULL, inline = TRUE, width = NULL),
                column(width=4, htmlOutput("compfleet")) ,
                column(width=3, htmlOutput("compvessel"))),
            
            tabsetPanel( 
              
              tabPanel("Vessel Comparison", 
                       
                       htmlOutput("compdraft"),htmlOutput("cSS"),htmlOutput("selvessel"),
                       
                       box(title= strong("Power Comparison"),status="info" ,solidHeader =TRUE,box(column(dataTableOutput("CPOWER"),width = 5),status="info" ,solidHeader =TRUE,width=NULL,column(br(),plotlyOutput("compspeedpower"),offset = 1,width=6)),width = NULL),
                       box(title= strong("FO Comparison"),status="info" ,solidHeader =TRUE,box(column(dataTableOutput("CFO"),width = 5),status="info" ,solidHeader =TRUE,width=NULL,column(br(),plotlyOutput("compspeedfo"),offset = 1,width=6)),width = NULL)
                       
                       
              )
              
              
            ))   ,
    tabItem(tabName="intervention",
            
            box(width=NULL,solidHeader = TRUE,status = "info",collapsible = T,radioButtons("selectype",label=NULL,choices=c("All Vessel","Fleet Wise"), selected = NULL, inline = TRUE, width = NULL),
                column(width=2, htmlOutput("interfleet")) ,
                column(width=3, htmlOutput("interclass") ),
                column(width=3, htmlOutput("intervessel"))),
            tabsetPanel( tabPanel(title="INTERVENTION LIST"), 
                         tabPanel(title="INTERVENTION RESULTS")
            )
    ),
    tabItem(tabName="FOO",
            
            box(solidHeader = TRUE,status="info",width=NULL,collapsible = T,column(width=4,htmlOutput("Dselect")),column(width=4,htmlOutput("Dateselect"))),
            tabsetPanel( 
              tabPanel("Distance GPS (nm)",
                       box(solidHeader = TRUE,status="info",width=NULL,collapsible = F,plotlyOutput("Distance") )),
              tabPanel("Consumption (mt)",
                       box(solidHeader = TRUE,status="info",width=NULL,collapsible = F,plotlyOutput("Tconsumption") )),
              tabPanel("CO2 Emission", 
                       box(solidHeader = TRUE,status="info",width=NULL,collapsible = F,plotlyOutput("Tco2emission")))
            )
    ),
    tabItem(tabName="VOO",
            
            box(solidHeader = TRUE,status="info",width=NULL,collapsible = T,column(width=4,htmlOutput("DClass")),column(width=4,dateRangeInput("Datadate", label = "Monitoring Period" ,start=as.Date("2016-01-01"),end=Sys.Date()))), 
            tabsetPanel( 
              tabPanel("Consumption By Equipments",
                       box(solidHeader = TRUE,status="info",width=NULL,collapsible = F,column(width=6,htmlOutput("ConEqp")),column(width=4,htmlOutput("Aggreg_ConsEqp")),br(),br(),br(),
                           br(),   box(solidHeader = TRUE,status="info",width=NULL, plotlyOutput("conseqp")))),
              tabPanel("Consumption By Fuel ",
                       box(solidHeader = TRUE,status="info",width=NULL,collapsible = F,column(width=4,htmlOutput("ConFuel")),column(width=4,htmlOutput("Aggreg_ConsFuel")),br(),br(),br(),
                           br(),    box(solidHeader = TRUE,status="info",width=NULL, plotlyOutput("consfuel")))),
              tabPanel("Time Vs Parameter",
                       box(solidHeader = TRUE,status="info",width=NULL,collapsible = F,column(width=4,htmlOutput("Timepara")),column(width=4,htmlOutput("Aggreg_TimePara")),br(),br(),br(),
                           br(),   box(solidHeader = TRUE,status="info",width=NULL, plotlyOutput("timepara")) )),
              tabPanel("Relational Parameter",
                       box(solidHeader = TRUE,status="info",width=NULL,collapsible = F,column(width=4,htmlOutput("Relationpara")),br(),br(),br(),
                           br(),    box(solidHeader = TRUE,status="info",width=NULL, plotlyOutput("relationpara")) )) ,
              tabPanel ("Operational Profile",
                        box(solidHeader = TRUE,status="info",width=NULL,collapsible = F,column(width=3,htmlOutput("Operatingprofile")),
                            column(width=3,htmlOutput("OPST")),column(width=3,htmlOutput("OPSS")),column(width=3,htmlOutput("OPWS"))),
                        box(solidHeader = TRUE,status="info",width=NULL, plotlyOutput("Operatingprofilechart")) 
              )
              
            )
    ),
    
    tabItem(tabName = "EM",
            box(width=NULL,solidHeader = TRUE,status = "info",collapsible = T,column(width=2,radioButtons("emtype",label="Type of Voyage",choices=c("All Voyages","EU Voyages"), selected = "All Voyages", inline = TRUE, width = NULL)),
               column(width = 10, radioButtons("method",label="Method Selection",choices=c("Flow Meter Method","Bunker Method"), selected = "Flow Meter Method", inline = TRUE, width = NULL)),
                column(width=2,htmlOutput("vtype")),column(width=3,htmlOutput("vname")),
                column(width=3,dateRangeInput("edate", label = h4(strong("Date Range")) ,start=as.Date("2016-01-01"),end=as.Date(Sys.Date())))),
            htmlOutput("EEOItabs")
            
            ),
    
    tabItem(tabName = "iso",
            box(width=NULL,solidHeader = TRUE,status = "info",collapsible = T,radioButtons("itype",label=NULL,choices=c("All Vessel","Fleet Wise"), selected = "All Vessel", inline = TRUE, width = NULL),
                column(width=4,htmlOutput("ifleet")),column(width=4,htmlOutput("ivesselUI")),
                column(width=4,dateRangeInput("idate1", label = "Reference Period" ,start=as.Date("2016-01-01"),end=as.Date("2016-07-31"))) ,
                column(width=4, dateRangeInput("idate2", label = "Evaluation Period" ,start=as.Date("2016-08-01"),end=Sys.Date()))),
            tabsetPanel(tabPanel("RESULTS",
                                 br(),
                                 box(width=NULL,status="info" ,solidHeader =TRUE,fluidRow(column(width=6,valueBoxOutput("PI"))),br(),
                                     box(title=strong("Performance Indicator"),status = "info",solidHeader = TRUE,width=NULL,
                                         plotlyOutput("isorefpi")),br(),
                                     box(width=NULL,status="info" ,solidHeader =TRUE,fluidRow(column(width=6,valueBoxOutput("REFAVG")),column(width=6,valueBoxOutput("EVAAVG"))
                                     ),br(),
                                     fluidRow(column(width=6,box(title=strong("Reference Speed Change"),status = "info",solidHeader = TRUE,width=NULL,
                                                                 plotlyOutput("isorefcurve"))),
                                              column(width=6,box(title=strong("Evaluation Speed Change"),status = "info",solidHeader = TRUE,width =NULL,
                                                                 plotlyOutput("isoevacurve"))) ))
                                     
                                     
                                 )),
                        tabPanel("DATA",
                                 br(),
                                 box(width=NULL,solidHeader= TRUE,status="info",title=strong("Data For The Period"),dataTableOutput("isoDataTable1"))),
                        tabPanel("DESCRIPTION",br(),
                                 box(width=NULL,status="info" ,solidHeader =TRUE,includeMarkdown("data/ISO DESCRIPTION.Rmd"))
                        )
                        
                        
            )
    ),
    
    
    tabItem(tabName = "info",
            tabsetPanel(tabPanel("Help",
                                 box( width = NULL, status = "info", solidHeader = TRUE, title="Help",                
                                      includeMarkdown("data/pro.Rmd")
                                 )),
                        tabPanel( "Contact Us",
                                  box( width = NULL, status = "info", solidHeader = TRUE, title=strong("ABOUT"),                
                                       includeMarkdown("data/about.RMd")
                                  )))),
    tabItem(tabName = "PreDatamonitoring",
            box(solidHeader = TRUE,status="info",width=NULL,
                box(solidHeader = TRUE,status="info",title =strong("Data Filering"),fluidRow(solidHeader = TRUE,status="info",column(width=6,htmlOutput("prefleet")),column(width=5,htmlOutput("prevessel"))),
                    column(width=6,dateRangeInput("predates", label = "Pre-Analysis Period" ,start=as.Date("2016-01-01"),end=Sys.Date())) ,
                    column(width=6, dateRangeInput("postdates", label = "Post-Analysis Period" ,start=as.Date("2016-01-01"),end=Sys.Date()))  ,
                    column(width=6, htmlOutput("predraftlimit")),column(width=6, htmlOutput("prepowerlimit")),
                    column(width=6, htmlOutput("prespeedlimit")),column(width=6, htmlOutput("presliplimit")),
                    column(width=6, sliderInput("preSFOC",label = "SFOC Range",min=100,max=350,value = c(150,300),step=10,width = "75%")),
                    column(width=6, sliderInput("preSEASTATE",label = "Sea State",min=0,max=9,value = c(0,8),step=1,width = "75%")),
                    column(width=6,htmlOutput("presteaminglimit")),
                    column(width=6,br(),actionButton("ppgobutton", "Go!"),tags$head(tags$style("#ppgobutton{background-color:#212F3D;
                                                                                               color:#ECF0F1;}") )),
                    br(),br(),br(),
                    dataTableOutput("predata")  ),
                
                box(solidHeader = TRUE,status="info",title=strong("Charts"),radioButtons("preFaxis", 
                                                                                         label = ("Y-Axis"), 
                                                                                         choices = list("Power"= 1,"FO" = 2,"SFOC"=3,"Speed"=4,"Draft"=5,"Sea State"=6,"RPM"=7,"Slip"=8),inline = TRUE,selected = 1),
                    plotlyOutput("preFdd",height = "200%"),br(),
                    br(),
                    radioButtons("preP1axis",label=("Y-Axis"),choice =  list("Power"= 1,"FO" = 2),inline = TRUE,selected = 1),
                    plotlyOutput("preFplots")
                    
                ))),
    tabItem(tabName = "prePREDICTION",
            fluidRow(column(6,valueBoxOutput("preEPOWER"),valueBoxOutput("preEFO")) 
            ),
            
            box(solidHeader = TRUE,status="info",title=strong("Analysis"),width= NULL, fluidRow(column(width=6,dataTableOutput("prePPtable")),
                                                                                                column(5,offset=1,br(),plotlyOutput("prePowerplot"),br(),plotlyOutput("preFOplot"))))),
    
    tabItem(tabName= "preOperatingprofile",fluidRow(box(solidHeader= TRUE,status="info",title=strong("Draft"),plotlyOutput("preOPDraft")),
                                                    box(solidHeader= TRUE,status="info",title=strong("Speed"),plotlyOutput("preOPSpeed"))),
            fluidRow(box(solidHeader= TRUE,status="info",title=strong("Seastate"),plotlyOutput("preOPSEAS")),        
                     box(solidHeader= TRUE,status="info",title=strong("Power"),plotlyOutput("preOPPower"))
            )),
    
    tabItem(tabName = "prePERFORMANCECURVE" ,
            
            htmlOutput("predraft"),
            fluidRow(column(width=6,htmlOutput("preSS"))),
            
            box(title= strong("Power Comparison"),status="info" ,solidHeader =TRUE,box(column(dataTableOutput("prePOWER"),width = 5),status="info" ,solidHeader =TRUE,width=NULL,column(br(),plotlyOutput("prespeedpower"),offset = 1,width=6,br(),valueBoxOutput("ppreEPOWER"))),width = NULL),
            box(title= strong("FO Comparison"),status="info" ,solidHeader =TRUE,box(column(dataTableOutput("preFO"),width = 5),status="info" ,solidHeader =TRUE,width=NULL,column(br(),plotlyOutput("prespeedfo"),offset = 1,width=6,br(),valueBoxOutput("ppreEFO"))),width = NULL)
            
            
    ),
    
    tabItem(tabName="ENGINE",box(width=NULL,solidHeader = TRUE,status = "info",collapsible = TRUE,
                                 column(width=4, htmlOutput("enginevessel")),column(width=4, htmlOutput("enginedate")),column(width = 4,htmlOutput("enginemonth"))),
            
            tabsetPanel(tabPanel("Results",  box(width=NULL,solidHeader = TRUE,status = "info",
                                                 column(width=6, withSpinner(dataTableOutput("mainparti")),br(),br(),box(width = NULL,withSpinner(plotOutput("spider_chart",height = 500)))),column(width=6, withSpinner(dataTableOutput("STD"))))),#,box(width = NULL,solidheader = TRUE,status = "info",column(width = 12,withSpinner(plotOutput("spider_chart",height = 750))))),
                        
                        tabPanel("Engine Performance",
                                 box(width=NULL,solidHeader = TRUE,status = "info",htmlOutput("enginechart"),htmlOutput("Chart"))),
                        tabPanel("Power Curve",
                                 box(title ="Engine Load vs Parameters" , width=NULL,solidHeader = TRUE,status = "info",fluidRow(column(offset=2,width=6,plotlyOutput("powerplot")),column(width = 4,htmlOutput("datetext"))),height = 900)),
                        #tags$style(type="text/css", "#datetext { height: 50px; width: 100%; text-align:left; vertical-align:top; font-size: 15px; display: block;}")),
                        
                        tabPanel("Cylinder Comparison",box(width = NULL,solidHeader = TRUE,status  = "info",htmlOutput("cylpara"),htmlOutput("cylchart"))))),
    
    tabItem(tabName = "CPA",
            box(width=NULL,solidHeader = TRUE,status = "warning",
                column(width=4,htmlOutput("CPAVessel")),column(width=4,htmlOutput("CPAVoyageno")),br(),
                #column(width=3,textOutput("DEPART"),textOutput("ARRIVAL")),
                #column(width=3,textOutput("DEPARTDATE"),textOutput("ARRIVALDATE")),
                #column(width=3,offset =8,textOutput("SHIPTYPE"))
                br(),column(width = 12, dataTableOutput("CPAtable"))
                
            ),
            tabsetPanel(tabPanel("Voyage Audit Report",br(),
                                 box(width=NULL,solidHeader = TRUE,status = "warning",title = strong("Speed and Consumption Warranty & Good Weather Definition"),
                                     column(width=4,htmlOutput("Warrentyspeed")),column(width=4,htmlOutput("WarrentyFO")),
                                     column(width=4,htmlOutput("WarrentyDO")),column(width=4,htmlOutput("WarrentyWindforce")),
                                     column(width=4,htmlOutput("Warrentyseastate"))
                                 ),
                                 box(width=NULL,solidHeader = TRUE,status = "warning",title = strong("Time & Consumption values"),column(width=4,valueBoxOutput("Timeloss",width = "75%")),column(width=4,valueBoxOutput("Fueloil",width = "75%")),column(width=4,valueBoxOutput("Dieseloil",width = "75%"))
                                     
                                     
                                 )),tabPanel("Calculations",
                                             br(),
                                             box(width=NULL,solidHeader= TRUE,status="info",title=strong("Time,Speed & Consumption Calculations"),
                                                 box(width=NULL,solidheader= T,title=strong("Good Weather Analysis "),dataTableOutput("CPAspeeddt")),br(),
                                                 box(width=NULL,solidheader=T,title=strong("Time Calculations"),dataTableOutput("CPAtimedt")),br(),
                                                 box(width=NULL,solidheader=T,title=strong("Consumption Calculations"),dataTableOutput("CPAconsdt")))),
                        tabPanel("Description",br(),
                                 box(width=NULL,status="info" ,solidHeader =TRUE,includeMarkdown("data/CPA description.Rmd"))
                        ))
    )
                ))

header = dashboardHeader(title = tags$a(tags$img(src='Xshipo.png'),"XShip Performance"),
                         dropdownMenu(type="tasks",icon = icon("user"),
                                      menuItem("LogOut",
                                               icon = icon("sign-out"),
                                               href = "https://xship.shinyapps.io/XShip_APP/__logout__/" ,newtab = FALSE)
                         ))


sidebar <- dashboardSidebar(width=250,
                            uiOutput("sidebarpanel"))

body <- dashboardBody( 
  shinyjs::useShinyjs(),
  tags$head(
    tags$script(
      HTML("
           window.onload = function() {
           resize();
           }
           window.onresize = function() {
           resize();
           }
           Shiny.addCustomMessageHandler ('triggerResize',function (val) {
           window.dispatchEvent(new Event('resize'));
           });
           function resize(){
           var h = window.innerHeight - $('.navbar').height() - 150; // Get dashboardBody height
           $('#Dashboard','#box').height(h); 
           }"
        )
      )
      ),

  
  tags$head(tags$script(src='https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js')),
  tags$head(tags$script(src='loader.js')),
  tags$head(tags$script("$.noConflict(true);")),
  tags$div(id = "loader",
           tags$div(id = "loaderInner")
  ),
  
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "responsive.css")),
  mainBody)

jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output,session) { 
  
  
  userid =  div(sidebarUserPanel(session$user,subtitle = a(href = "", icon("circle", class = "text-success"), "Online"),
                                 image = "XshipF.png"),heigth=500)                               
  
  user <- reactive({"xshipadmin@navgathi.com"})
  
  isAdmin <- reactive({
    
    if (user() == "xshipadmin@navgathi.com" ){
      
      return(TRUE)
      
    }else if (user() == "xshipdemo@navgathi.com" ){
      
      return(2)
      
    } else {
      
      return(FALSE)
      
    }
    
  })
  
  output$sidebarpanel <- renderUI({
    
    if (isAdmin()==TRUE){
      # If a manager, show everything.
      div(userid,admin)
    }
    else if (isAdmin()==2){
      div(userid,analy)
    }
    else{
      
      div(userid,other)
    }
    
  })
  

  
  shinyjs::delay(1, { 
    
    s <- list(
      family ="'Lato', sans-serif",
      size = 15,
      weight= "bold",
      color = "#212F3D")
    
    l <- list(
      font = s,
      bgcolor = "#FFFFFF"
     
    )
    
    #Graph Axis....................
    f <- list(
      family = "'Lato', sans-serif",
      size = 16,
      weight= "bold",
      color = "#212F3D"
    )
    
    c <- list(
      family = "'Lato', sans-serif",
      size = 18,
      weight= "bold",
      color = "#000")

    Speed_axis <- list(
      title = "Speed (knots)",
      titlefont = f,
      zeroline=FALSE,tickfont =f,gridcolor = "#FFFFFF",tickangle=0
    )
    Power_axis <- list(
      title = "Power (kW)",
      titlefont = f,
      zeroline=FALSE,tickfont =f,gridcolor = "#ABB2B9",tickangle=0
    )
    FO_axis <- list(
      title = "FO/24Hrs (tonne)",
      titlefont = f,
      zeroline=FALSE,tickfont =f,gridcolor = "#ABB2B9",tickangle=0
    )
    
    Sea_Axis<- list(
      title = "Sea State",
      titlefont = f,
      zeroline=FALSE,tickfont =f,gridcolor = "#FFFFFF",tickangle=0
    )
    

    
    
    colors <- c('#00B1B0','#c1cc99','#6cae75','#4bd9bc','#026956','#5299d3')
    
    
    source(file.path("server", "tab-dashboards.R"),  local = TRUE)$value
    source(file.path("server", "tab-voyagecalculator.R"),  local = TRUE)$value
    source(file.path("server", "tab-vesselperformance.R"),  local = TRUE)$value
    source(file.path("server", "tab-fleetcomparison.R"),  local = TRUE)$value
    source(file.path("server", "tab-datamonitoring1.R"),  local = TRUE)$value
    source(file.path("server", "tab-datamonitoring2.R"),  local = TRUE)$value
    source(file.path("server", "tab-enginemonitoring.R"),  local = TRUE)$value
    source(file.path("server", "tab-EEOI&MRV.R"),  local = TRUE)$value
    source(file.path("server", "tab-vesseldetails.R"),  local = TRUE)$value
    source(file.path("server", "tab-database.R"),  local = TRUE)$value
    source(file.path("server", "tab-performanceanalysis.R"),  local = TRUE)$value
    source(file.path("server", "tab-prepostanalysis.R"),  local = TRUE)$value
    source(file.path("server", "tab-isoanalysis.R"),  local = TRUE)$value
    source(file.path("server", "tab-charterpartyanalysis.R"),  local = TRUE)$value
    
    
    
    
    
    #INTERVENTION.......................
    
    
    output$intervessel <- renderUI({ 
      r=DATA
      s=input$selectype
      if(s=="FLEET WISE"){
        r=subset(r,Fleet == input$interfleet)
        r=subset(r,Class == input$interclass)
        Vessel_List = unique(as.character(r[,3]), incomparables = FALSE)
        selectInput("intervessel", label=strong("VESSEL"), choices = Vessel_List, selected = c("Strategic Alliance"), multiple = F, selectize = TRUE, width = "75%", size = NULL)
      }else{
        Vessel_List = unique(as.character(r[,3]), incomparables = FALSE)
        selectInput("intervessel", label=strong("VESSEL"), choices = Vessel_List, selected = c("Strategic Alliance"), multiple = F, selectize = TRUE, width = "75%", size = NULL)
      }
    })
    
    output$interfleet <- renderUI({ 
      r=DATA
      s=input$selectype
      if(s=="FLEET WISE"){
        Vessel_List = unique(as.character(r[,1]), incomparables = FALSE)
        selectInput("interfleet", label=strong("FLEET"), choices = Vessel_List, selected = "1", multiple = FALSE, selectize = TRUE, width = "30%", size = NULL)
      }
      else{
        return(NULL)
      }
    })
    
    output$interclass <- renderUI({ 
      r=DATA
      s=input$selectype
      if(s=="FLEET WISE"){
        r=subset(r,Fleet == input$interfleet)
        Vessel_List = unique(as.character(r[,2]), incomparables = FALSE)
        selectInput("interclass", label=strong("CLASS"), choices = Vessel_List, selected = "Strategic Alliance", multiple = FALSE, selectize = TRUE, width = "50%", size = NULL)
      }else
      {return(NULL)}
    })
    
    #Reports......
    VD<- reactive({
      y=data.frame(read.csv("data/Vessel Detail.csv"))
      validate(
        need(try(input$filtVessel),"Please Wait or Select the vessel")
      )
      ff=input$filtVessel
      y=subset(y,y$Vessel == ff)
    })
    
    output$Report <- downloadHandler(
      
      filename = function() { 'Report.xlsx' }, content = function(file) {
        require (XLConnect)
        exc2 <- loadWorkbook("data/VESSEL PERFORMANCE.xlsx", create = TRUE)
        setStyleAction(exc2,XLC$"STYLE_ACTION.NONE")
        r=VD()
        P=coeffdata()
        FO=FOcoeffdata()
        yob=unique(as.numeric(r[,8]), incomparables = FALSE)
        loa=unique(as.numeric(r[,9]), incomparables = FALSE)
        b=unique(as.numeric(r[,10]), incomparables = FALSE)
        DIS=unique(as.numeric(r[,11]), incomparables = FALSE)
        MCR=unique(as.numeric(r[,7]), incomparables = FALSE)
        # Fo coeefficient 
        writeWorksheet(exc2,FO$n1, sheet = c(1,2,3), startRow = 41, startCol = 3,header=FALSE)
        writeWorksheet(exc2,FO$n2, sheet = c(1,2,3), startRow = 42, startCol = 3,header=FALSE)
        writeWorksheet(exc2,FO$n3, sheet = c(1,2,3), startRow = 43, startCol = 3,header=FALSE)
        writeWorksheet(exc2,FO$k, sheet = c(1,2,3), startRow = 44, startCol = 3,header=FALSE)
        #power Coefficient
        writeWorksheet(exc2, P$n1, sheet = c(1,2,3), startRow = 41, startCol = 7,header=FALSE)
        writeWorksheet(exc2, P$n2, sheet = c(1,2,3), startRow = 42, startCol = 7,header=FALSE)
        writeWorksheet(exc2, P$n3, sheet = c(1,2,3), startRow = 43, startCol = 7,header=FALSE)
        writeWorksheet(exc2, P$k, sheet = c(1,2,3), startRow = 44, startCol = 7,header=FALSE)
        #Vessel details 
        writeWorksheet(exc2, input$filtVessel , sheet = c(1,2,3), startRow = 9, startCol = 2,header=FALSE)
        writeWorksheet(exc2, input$dates[1] , sheet = 1, startRow = 24, startCol = 15,header=FALSE)
        writeWorksheet(exc2, input$dates[2] , sheet = 1 , startRow = 24, startCol = 16,header=FALSE)
        writeWorksheet(exc2, input$draft1 , sheet = 1 , startRow = 26, startCol = 15,header=FALSE)
        writeWorksheet(exc2, input$draft2 , sheet = 1 , startRow = c(27,25), startCol =c(15,6),header=FALSE)
        writeWorksheet(exc2, input$speed2 ,  sheet = 1 , startRow = 29, startCol = 16,header=FALSE)
        writeWorksheet(exc2, MCR ,  sheet = 1 , startRow = 25, startCol = 9,header=FALSE)
        writeWorksheet(exc2, input$filtVessel , sheet = 1 , startRow = 25, startCol = 2,header=FALSE)
        writeWorksheet(exc2, yob , sheet = 1 , startRow = 25, startCol = 3,header=FALSE)
        writeWorksheet(exc2, loa , sheet = 1 , startRow = 25, startCol = 4,header=FALSE)
        writeWorksheet(exc2, b , sheet = 1 , startRow = 25, startCol = 5,header=FALSE)
        writeWorksheet(exc2, DIS , sheet = 1 , startRow = 25, startCol = 7,header=FALSE)
        
        setForceFormulaRecalculation(exc2,sheet = "*",TRUE)
        saveWorkbook(exc2,file)
      }
    )
    #ADA Reports...........
    ADAVD<- reactive({
      y=data.frame(read.csv("data/Vessel Detail.csv"))
      validate(
        need(try(input$filtVessel),"Please Wait or Select the vessel")
      )
      ff=input$filtVessel
      y=subset(y,y$Vessel == ff)
    })
    
    output$ADAReport <- downloadHandler(
      
      filename = function() { 'ADA Report.xlsx' }, content = function(file) {
        require (XLConnect)
        exc2 <- loadWorkbook("data/VESSEL PERFORMANCE.xlsx", create = TRUE)
        setStyleAction(exc2,XLC$"STYLE_ACTION.NONE")
        r=VD()
        P=ADAcoeffdata()
        FO=ADAFOcoeffdata()
        yob=unique(as.numeric(r[,8]), incomparables = FALSE)
        loa=unique(as.numeric(r[,9]), incomparables = FALSE)
        b=unique(as.numeric(r[,10]), incomparables = FALSE)
        DIS=unique(as.numeric(r[,11]), incomparables = FALSE)
        MCR=unique(as.numeric(r[,7]), incomparables = FALSE)
        # Fo coeefficient 
        writeWorksheet(exc2,FO$n1, sheet = c(1,2,3), startRow = 41, startCol = 3,header=FALSE)
        writeWorksheet(exc2,FO$n2, sheet = c(1,2,3), startRow = 42, startCol = 3,header=FALSE)
        writeWorksheet(exc2,FO$n3, sheet = c(1,2,3), startRow = 43, startCol = 3,header=FALSE)
        writeWorksheet(exc2,FO$k, sheet = c(1,2,3), startRow = 44, startCol = 3,header=FALSE)
        #power Coefficient
        writeWorksheet(exc2, P$n1, sheet = c(1,2,3), startRow = 41, startCol = 7,header=FALSE)
        writeWorksheet(exc2, P$n2, sheet = c(1,2,3), startRow = 42, startCol = 7,header=FALSE)
        writeWorksheet(exc2, P$n3, sheet = c(1,2,3), startRow = 43, startCol = 7,header=FALSE)
        writeWorksheet(exc2, P$k, sheet = c(1,2,3), startRow = 44, startCol = 7,header=FALSE)
        #Vessel details 
        writeWorksheet(exc2, input$filtVessel , sheet = c(1,2,3), startRow = 9, startCol = 2,header=FALSE)
        writeWorksheet(exc2, input$ADAdates[1] , sheet = 1, startRow = 24, startCol = 15,header=FALSE)
        writeWorksheet(exc2, input$ADAdates[2] , sheet = 1 , startRow = 24, startCol = 16,header=FALSE)
        writeWorksheet(exc2, input$ADAdraft1 , sheet = 1 , startRow = 26, startCol = 15,header=FALSE)
        writeWorksheet(exc2, input$ADAdraft2 , sheet = 1 , startRow = c(27,25), startCol =c(15,6),header=FALSE)
        writeWorksheet(exc2, input$ADAspeed2 ,  sheet = 1 , startRow = 29, startCol = 16,header=FALSE)
        writeWorksheet(exc2, MCR ,  sheet = 1 , startRow = 25, startCol = 9,header=FALSE)
        writeWorksheet(exc2, input$filtVessel , sheet = 1 , startRow = 25, startCol = 2,header=FALSE)
        writeWorksheet(exc2, yob , sheet = 1 , startRow = 25, startCol = 3,header=FALSE)
        writeWorksheet(exc2, loa , sheet = 1 , startRow = 25, startCol = 4,header=FALSE)
        writeWorksheet(exc2, b , sheet = 1 , startRow = 25, startCol = 5,header=FALSE)
        writeWorksheet(exc2, DIS , sheet = 1 , startRow = 25, startCol = 7,header=FALSE)
        
        setForceFormulaRecalculation(exc2,sheet = "*",TRUE)
        saveWorkbook(exc2,file)
      }
    )
    #Reports INTERVENTION ......
    
    output$PREPOSTReport <- downloadHandler(
      
      filename = function() { 'PRE/POST Anlaysis Report.xlsx' }, content = function(file) {
        require (XLConnect)
        exc2 <- loadWorkbook("data/INTERVENTION.xlsx", create = TRUE)
        setStyleAction(exc2,XLC$"STYLE_ACTION.NONE")
        P=ppcoeffdata()
        FO=ppcoeffdatafo()
        # Fo coefficient 
        writeWorksheet(exc2,FO$n1, sheet = 1, startRow = 41, startCol = 4,header=FALSE)
        writeWorksheet(exc2,FO$n2, sheet = 1, startRow = 42, startCol = 4,header=FALSE)
        writeWorksheet(exc2,FO$n3, sheet = 1, startRow = 43, startCol = 4,header=FALSE)
        writeWorksheet(exc2,FO$DD, sheet = 1, startRow = 44, startCol = 4,header=FALSE)
        writeWorksheet(exc2,FO$k, sheet = 1, startRow = 45, startCol = 4,header=FALSE)
        #power Coefficient
        writeWorksheet(exc2, P$n1, sheet = 2, startRow = 41, startCol = 4,header=FALSE)
        writeWorksheet(exc2, P$n2, sheet = 2, startRow = 42, startCol = 4,header=FALSE)
        writeWorksheet(exc2, P$n3, sheet = 2, startRow = 43, startCol =4,header=FALSE)
        writeWorksheet(exc2, P$DD, sheet = 2, startRow = 44, startCol = 4,header=FALSE)
        writeWorksheet(exc2, P$k, sheet = 2, startRow = 45, startCol = 4,header=FALSE)
        #Vessel details 
        writeWorksheet(exc2, input$preVessel , sheet = c(1,2), startRow = 8, startCol = 5,header=FALSE)
        writeWorksheet(exc2, input$preDraft , sheet = c(1,2), startRow = 13, startCol = 3,header=FALSE)
        writeWorksheet(exc2, input$preSS , sheet = c(1,2), startRow = 14, startCol = 3,header=FALSE)
        
        setForceFormulaRecalculation(exc2,sheet = "*",TRUE)
        saveWorkbook(exc2,file)
      }
    )#Reports......
    SUMEE1_rep=reactive({
      y=Mdata()
      
      m=matrix(0,nrow=7,ncol=5,byrow = TRUE)
      colnames(m)=c("Fuel(HS)",	"Fuel (LS)","Fuel (MDO)","Fuel (MGO)","Fuel (MGO LS)")
      rownames(m)=c("ECA Zone","Non-ECA Zone","Total Fuel","CF(t-CO2/t-Fuel)","CO2 ECA Zone(Tonnes)",
                    "CO2 Non ECA Zone(Tonnes)","CO2(Tonnes)")
      eca = subset(y,y$ECA == "Y")
      neca= subset(y,y$ECA == "N")
      m[1,]=c(sum(as.numeric(as.character(eca$FUEL.HS))),sum(as.numeric(as.character(eca$FUEL.LS))),sum(as.numeric(as.character(eca$FUEL.MDO))),sum(as.numeric(as.character(eca$FUEL.MGO))),sum(as.numeric(as.character(eca$FUEL.MGO.LS))))
      m[2,]=c(sum(as.numeric(as.character(neca$FUEL.HS))),sum(as.numeric(as.character(neca$FUEL.LS))),sum(as.numeric(as.character(neca$FUEL.MDO))),sum(as.numeric(as.character(neca$FUEL.MGO))),sum(as.numeric(as.character(neca$FUEL.MGO.LS))))
      m[3,]=c(sum(as.numeric(as.character(y$FUEL.HS))),sum(as.numeric(as.character(y$FUEL.LS))),sum(as.numeric(as.character(y$FUEL.MDO))),sum(as.numeric(as.character(y$FUEL.MGO))),sum(as.numeric(as.character(y$FUEL.MGO.LS))))
      sea=subset(y,Status=="AT SEA")
      m[4,]=c(3.114, 3.114, 3.206, 3.206,3.206)
      m[5,]=c(m[1,]*m[4,])
      port=subset(y,Status=="IN PORT")
      
      m[6,]=c(m[2,]*m[4,])
      m[7,]=c(sum(m[2,],m[4,]))
      m
      
      
    })
    
    SUMEE2_rep=reactive({
      y=Mdata()
     
      m=SUMEE1_rep()
      t = subset(y,Status == "AT SEA")
      cargo = subset(t,Report.Type == "NOON")
      tcargo = unique(as.numeric(as.character(cargo$CARGO.TOTAL)), incomparables = FALSE)
      
      cargoteu = subset(t,Report.Type == "NOON")
      tcargoe = unique(as.numeric(as.character(cargoteu$TEU.FULL)), incomparables = FALSE)
      eca = subset(y,y$ECA == "Y")
      neca= subset(y,y$ECA == "N")
      n=data.frame(round(sum(as.numeric(m[7,])),2),
                   round(sum(as.numeric(m[5,])),2),
                   round(sum(as.numeric(m[6,])),2), 
                   round(sum(as.numeric(tcargo)),2),
                   round(sum(as.numeric(tcargoe)),2),
                   round(sum(as.numeric(m[1,1])+(1.05*(as.numeric(m[1,4])+as.numeric(m[1,3])+as.numeric(m[1,5])))+(1.025*as.numeric(m[1,2]))),2),
                   round(sum(as.numeric(t$Sea.Time)),2),
                   round(sum(as.numeric(y$MILES.BY.GPS)),2),
                   round(((round(sum(as.numeric(m[7,])),2)*1000)/round(sum(as.numeric(y$MILES.BY.GPS)),2)),2),
                   (((round(sum(as.numeric(m[7,])),2)*1000)/round(sum(as.numeric(tcargoe)),2))), 
                   ((round(sum(as.numeric(t$Sea.Time)),2)*1000)/round(sum(as.numeric(y$MILES.BY.GPS)),2)),
                   ((round(sum(as.numeric(t$Sea.Time)),2)*1000)/round(sum(as.numeric(t$CARGO.TOTAL.TEU),2))),
                   round(sum(as.numeric(y$Cargo.Transport.Work)),2),
                   round(sum(as.numeric(y$TEU.Transport.Work)),2),
                   round(((round(sum(as.numeric(m[7,])),2)*10^6)/round(sum(as.numeric(y$Cargo.Transport.Work)),2)),2),
                   round(((round(sum(as.numeric(m[7,])),2)*10^6)/round(sum(as.numeric(y$TEU.Transport.Work)),2)),2))
      colnames(n)=c("Total CO2 (Tonnes)","Total CO2 In ECA Zone(Tonnes)","Total CO2 Non-ECA Zone(Tonnes)","Total Cargo(Tonnes)","Total Cargo(TEU)","Total Fuel HS Equivalent",'Total time spent at sea Hrs',"Total distance travelled Miles(GPS)","CO2 per mile(Kg/NM)",
                    "CO2 per TEU(Kg/TEU)","Equivalent Fuel per mile(Kg/NM)","Equivalent Fuel per TEU(Kg/TEU)","Total Transportation Work(Tonne-Mile)","Total Transportation Work(TEU-Mile)","Average energy efficiency(gm/tonne-mile)","Average energy efficiency(gm/TEU-mile)")					
      n
    })
    
    SUMEE3_rep=reactive({
      y=Mdata()
      
      m=SUMEE1_rep()
      n=data.frame(m[3,],m[4,],m[5,],m[6,],m[7,])
      n
    })
    
    EECHARTDATA_rep=reactive({
      y=Mdata()
    
      y$CO2= as.numeric(y$TOTAL.CO2)
      y$EEOI= (y$CO2*10^6)/(as.numeric(y$CARGO.TOTAL)* as.numeric(y$MILES.BY.GPS))
      #y$EEOI= sub("#N/A","0",y$EEOI)
      x=data.frame(y$Corrected.Date,y$EEOI,y$CO2)
      colnames(x)=c("DATE","EEOI(gm/Ton-miles)","Total CO2(Tonnes)")
      x[is.na(x)] <- 0
      x
      
    })
    
    
    output$Report <- downloadHandler(
      
      filename = function() { 'Report.xlsx' }, content = function(file) {
        require (XLConnect)
        exc2 <- XLConnect::loadWorkbook("data/EEOI_TEMPLATES.xlsx")
        setStyleAction(exc2,XLC$"STYLE_ACTION.NONE")
        r=SUMEE2_rep()
        s=SUMEE3_rep()
        t=EECHARTDATA_rep()
        #........................Summary table 2............................................
        writeWorksheet(exc2,r[1,1], sheet = 1, startRow = 12, startCol = 8,header=FALSE)
        writeWorksheet(exc2,r[1,2], sheet = 1, startRow = 13, startCol = 8,header=FALSE)
        writeWorksheet(exc2,r[1,3], sheet = 1, startRow = 14, startCol = 8,header=FALSE)
        writeWorksheet(exc2,r[1,4], sheet = 1, startRow = 15, startCol = 8,header=FALSE)
        
        writeWorksheet(exc2,r[1,5], sheet = 1, startRow = 16, startCol = 8,header=FALSE)
        writeWorksheet(exc2,r[1,6], sheet = 1, startRow = 17, startCol = 8,header=FALSE)
        writeWorksheet(exc2,r[1,7], sheet = 1, startRow = 18, startCol = 8,header=FALSE)
        writeWorksheet(exc2,r[1,8], sheet = 1, startRow = 19, startCol = 8,header=FALSE)
        
        writeWorksheet(exc2,r[1,9] , sheet = 1, startRow = 20, startCol = 8,header=FALSE)
        writeWorksheet(exc2,r[1,10] , sheet = 1, startRow = 21, startCol = 8,header=FALSE)
        writeWorksheet(exc2,r[1,11] , sheet = 1 , startRow = 22, startCol = 8,header=FALSE)
        writeWorksheet(exc2,r[1,12], sheet = 1 , startRow = 23, startCol = 8,header=FALSE)
        writeWorksheet(exc2,r[1,13] , sheet = 1 , startRow = 24, startCol =8,header=FALSE)
        writeWorksheet(exc2,r[1,14] ,  sheet = 1 , startRow = 25, startCol = 8,header=FALSE)
        writeWorksheet(exc2,r[1,15] ,  sheet = 1 , startRow = 26, startCol = 8,header=FALSE)
        writeWorksheet(exc2,r[1,16] ,  sheet = 1 , startRow = 27, startCol = 8,header=FALSE)
        #.......................Summary table 1..............................................
        writeWorksheet(exc2,s[1,1] , sheet = 1, startRow = 14, startCol = 2,header=FALSE)
        writeWorksheet(exc2,s[1,2] , sheet = 1, startRow = 15, startCol = 2,header=FALSE)
        writeWorksheet(exc2,s[1,3] , sheet = 1 , startRow = 16, startCol = 2,header=FALSE)
        writeWorksheet(exc2,s[1,4], sheet = 1 , startRow = 17, startCol = 2,header=FALSE)
        writeWorksheet(exc2,s[1,5] , sheet = 1 , startRow = 18, startCol =2,header=FALSE)
        
        
        writeWorksheet(exc2,s[2,1] , sheet = 1, startRow = 14, startCol = 3,header=FALSE)
        writeWorksheet(exc2,s[2,2] , sheet = 1, startRow = 15, startCol = 3,header=FALSE)
        writeWorksheet(exc2,s[2,3] , sheet = 1 , startRow = 16, startCol = 3,header=FALSE)
        writeWorksheet(exc2,s[2,4], sheet = 1 , startRow = 17, startCol = 3,header=FALSE)
        writeWorksheet(exc2,s[2,5] , sheet = 1 , startRow = 18, startCol =3,header=FALSE)
        
        writeWorksheet(exc2,s[3,1] , sheet = 1, startRow = 14, startCol = 4,header=FALSE)
        writeWorksheet(exc2,s[3,2] , sheet = 1, startRow = 15, startCol = 4,header=FALSE)
        writeWorksheet(exc2,s[3,3] , sheet = 1 , startRow = 16, startCol = 4,header=FALSE)
        writeWorksheet(exc2,s[3,4], sheet = 1 , startRow = 17, startCol = 4,header=FALSE)
        writeWorksheet(exc2,s[3,5] , sheet = 1 , startRow = 18, startCol =4,header=FALSE)
        
        writeWorksheet(exc2,s[4,1] , sheet = 1, startRow = 14, startCol = 5,header=FALSE)
        writeWorksheet(exc2,s[4,2] , sheet = 1, startRow = 15, startCol = 5,header=FALSE)
        writeWorksheet(exc2,s[4,3] , sheet = 1 , startRow = 16, startCol = 5,header=FALSE)
        writeWorksheet(exc2,s[4,4], sheet = 1 , startRow = 17, startCol = 5,header=FALSE)
        writeWorksheet(exc2,s[4,5] , sheet = 1 , startRow = 18, startCol =5,header=FALSE)
        
        writeWorksheet(exc2,s[5,1] , sheet = 1, startRow = 14, startCol = 6,header=FALSE)
        writeWorksheet(exc2,s[5,2] , sheet = 1, startRow = 15, startCol = 6,header=FALSE)
        writeWorksheet(exc2,s[5,3] , sheet = 1 , startRow = 16, startCol = 6,header=FALSE)
        writeWorksheet(exc2,s[5,4], sheet = 1 , startRow = 17, startCol = 6,header=FALSE)
        writeWorksheet(exc2,s[5,5] , sheet = 1 , startRow = 18, startCol =6,header=FALSE)
        #......................Voyage details................................................
        writeWorksheet(exc2,input$Vesselname , sheet = 1, startRow = 4, startCol = 2,header=FALSE)
        
        
        writeWorksheet(exc2,as.Date(input$edate[1],"%y-%m-%d"), sheet = 1 , startRow = 6, startCol = 7,header=FALSE)
        writeWorksheet(exc2,as.Date(input$edate[2],"%y-%m-%d"), sheet = 1 , startRow = 6, startCol = 8,header=FALSE)
        writeWorksheet(exc2,t, sheet = 2 , startRow = 2, startCol = 1,header=FALSE)
        # write.xlsx(t,file,sheetName="Sheet2")
        
        setForceFormulaRecalculation(exc2,sheet = "*",TRUE)
        
        saveWorkbook(exc2,file)
      }
    )
    
    
    
    
    
    
  })
  

  
}

shinyApp(ui, server)


