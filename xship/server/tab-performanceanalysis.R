

#TABSETPANEL FOR DATA MONITORING ...............................................
output$mytabs = renderUI({
  y=VESSELDETAILS
  ff=input$filtVessel
  y=subset(y,y$Vessel==ff)
  ADA = unique(as.character(y[,15]), incomparables = FALSE)
  validate(need(try(ADA),"please wait..."))
  if(ADA =="N"){
    
    myTabs = tabsetPanel( tabPanel( "Noon Data Analysis",
                                    box(solidHeader = TRUE,status="info",width=NULL,
                                        box(solidHeader = TRUE,status="info",title =strong("Data Filtering"),
                                            dateRangeInput("dates", label = "Analysis Period" ,start=as.Date("2016-01-01"),end=Sys.Date())  ,
                                            column(width=6,htmlOutput("draftlimit")),column(width=6,htmlOutput("powerlimit")),
                                            column(width=6,htmlOutput("speedlimit")),column(width=6,htmlOutput("sliplimit")),
                                            column(width=6,sliderInput("SFOC",label = "SFOC Range",min=100,max=350,value = c(100,250),step=10,width = "75%")),
                                            column(width=6,sliderInput("SEASTATE",label = "Sea State",min=0,max=9,value = c(0,8),step=1,width = "75%")),
                                            column(width=6,htmlOutput("streaminglimit")),
                                            column(width=6,br(),actionButton("goButton", "Go!")),
                                            
                                            br(),br(),
                                            dataTableOutput("Fldata")  ),
                                        
                                        box(solidHeader = TRUE,status="info",title=strong("Charts"),radioButtons("Faxis", 
                                                                                                                 label = ("Y-Axis"), 
                                                                                                                 choices = list("Power"= 1,"FO" = 2,"SFOC"=3,"Speed"=4,"Draft"=5,"Sea State"=6,"Slip"=7,"RPM"= 8),inline = TRUE,selected = 1),
                                            plotlyOutput("Fdd",height = "200%"),br(),
                                            br(),
                                            radioButtons("P1axis",label=("Y-Axis"),choice =  list("Power"= 1,"FO" = 2),inline = TRUE,selected = 1),
                                            plotlyOutput("Fplots"),
                                            br(),br(),
                                            plotlyOutput("SF1")
                                        ))
    ))
  }
  else{
    
    myTabs =  tabsetPanel(tabPanel( "Noon Data Analysis",
                                    box(solidHeader = TRUE,status="info",width=NULL,
                                        box(solidHeader = TRUE,status="info",title =strong("Data Filtering"),
                                            dateRangeInput("dates", label = "Analysis Period" ,start=as.Date("2016-01-01"),end=Sys.Date())  ,
                                            column(width=6,htmlOutput("draftlimit")),column(width=6,htmlOutput("powerlimit")),
                                            column(width=6,htmlOutput("speedlimit")),column(width=6,htmlOutput("sliplimit")),
                                            column(width=6,sliderInput("SFOC",label = "SFOC Range",min=100,max=350,value = c(100,250),step=10,width = "75%")),
                                            column(width=6,sliderInput("SEASTATE",label = "Sea State",min=0,max=9,value = c(0,8),step=1,width = "75%")),
                                            column(width=6,htmlOutput("streaminglimit")),
                                            column(width=6,br(),actionButton("goButton", "Go!")),
                                            
                                            br(),
                                            dataTableOutput("Fldata")  ),
                                        
                                        box(solidHeader = TRUE,status="info",title=strong("Charts"),radioButtons("Faxis", 
                                                                                                                 label = ("Y-Axis"), 
                                                                                                                 choices = list("Power"= 1,"FO" = 2,"SFOC"=3,"Speed"=4,"Draft"=5,"Sea State"=6,"Slip"=7,"RPM"= 8),inline = TRUE,selected = 1),
                                            plotlyOutput("Fdd",height = "200%"),br(),
                                            br(),
                                            radioButtons("P1axis",label=("Y-Axis"),choice =  list("Power"= 1,"FO" = 2),inline = TRUE,selected = 1),
                                            plotlyOutput("Fplots"),
                                            br(),br(),
                                            plotlyOutput("SF1")
                                        ))
    ),
    tabPanel("ADA Data Analysis",
             box(solidHeader = TRUE,status="info",width=NULL,
                 box(solidHeader = TRUE,status="info",title =strong("ADA Data Filtering"),
                     dateRangeInput("ADAdates", label = "Analysis Period" ,start=as.Date("2016-01-01"),end=Sys.Date())  ,
                     column(width=6,htmlOutput("ADAdraftlimit")),column(width=6,htmlOutput("ADApowerlimit")),
                     column(width=6,htmlOutput("ADAspeedlimit")),column(width=6,htmlOutput("ADAsliplimit")),
                     column(width=6,sliderInput("ADASFOC",label = "SFOC Range",min=100,max=350,value = c(100,250),step=10,width = "75%")),
                     column(width=6,sliderInput("ADASEASTATE",label = "Sea State",min=0,max=9,value = c(0,8),step=1,width = "75%")),
                     column(offset=5,width=4,actionButton("ADAgoButton", "Go!") ,tags$head(tags$style("ADAgoButton{background-color:#212F3D;
                                                                                                      color:#F4F6F7;}") )),
                     
                     br(),br(),
                     dataTableOutput("ADAFldata")  ),
                 
                 box(solidHeader = TRUE,status="info",title=strong("Charts"),radioButtons("ADAFaxis", 
                                                                                          label = ("Y-Axis"), 
                                                                                          choices = list("Power"= 1,"FO" = 2,"SFOC"=3,"Speed"=4,"Draft"=5,"Sea State"=6,"Slip"=7,"RPM"= 8),inline = TRUE,selected = 1),
                     plotlyOutput("ADAFdd",height = "200%"),br(),
                     br(),
                     radioButtons("ADAP1axis",label=("Y-Axis"),choice =  list("Power"= 1,"FO" = 2),inline = TRUE,selected = 1),
                     plotlyOutput("ADAFplots"),
                     br(),br(),
                     plotlyOutput("ADASF1")
                 )))
                 )
  }
  
  myTabs
})
#predication Tab .......................................
output$mypredicationtabs = renderUI({
  y=VESSELDETAILS
  ff=input$filtVessel
  y=subset(y,y$Vessel==ff)
  ADA = unique(as.character(y[,15]), incomparables = FALSE)
  
  if(ADA =="N"){ 
    
    mytabs = tabsetPanel(tabPanel( "Noon Data Prediction",br(),
                                   fluidRow(column(4,valueBoxOutput("EPOWER",width="40%")),column(width=4,valueBoxOutput("EFO",width="40%")), 
                                                   column(width=4,box(width=NULL,solidHeader = TRUE,status="info", textOutput("Vessel1"),br(),textOutput("Date1"),
                                                       tags$head(tags$style("#Vessel1{color: green; font-size: 30px; font-style: bold; font-family: Lato; }"  )),
                                                       tags$head(tags$style("#Date1{color: green; font-size: 20px; font-style: bold; font-family: Lato;}"  ))
                                                   ))
                                   ),
                                   box(solidHeader = TRUE,status="info",title=strong("Noon Analysis"),width= NULL, fluidRow(column(width=6,dataTableOutput("PPtable")),
                                                                                                                            column(5,offset=1,br(),plotlyOutput("Powerplot"),br(),plotlyOutput("FOplot"),br(),plotlyOutput("compareShopplot"))))))
  }
  else {
    mytabs = tabsetPanel(tabPanel( "Noon Data Prediction",br(),
                                   fluidRow(column(4,valueBoxOutput("EPOWER",width="40%")),column(width=4,valueBoxOutput("EFO",width="40%")), 
                                            column(width=4,box(width=NULL,solidHeader = TRUE,status="info", textOutput("Vessel1"),br(),textOutput("Date1"),
                                                               tags$head(tags$style("#Vessel1{color: green; font-size: 30px; font-style: bold; font-family: Lato; }"  )),
                                                               tags$head(tags$style("#Date1{color: green; font-size: 20px; font-style: bold; font-family: Lato;}"  ))
                                            ))
                                   ),
                                   box(solidHeader = TRUE,status="info",title=strong("Analysis"),width= NULL, 
                                       fluidRow(column(width=6,dataTableOutput("PPtable")),column(5,offset=1,br(),plotlyOutput("Powerplot"),br(),
                                                                                                  plotlyOutput("FOplot"),br(),plotlyOutput("compareShopplot"))))),
                         tabPanel( "ADA Data Prediction",br(),
                                   fluidRow(column(6,valueBoxOutput("ADAEPOWER"),valueBoxOutput("ADAEFO"))
                                   ),
                                   box(solidHeader = TRUE,status="info",title=strong(" ADA Analysis"),width= NULL, 
                                       fluidRow(column(width=6,dataTableOutput("ADAPPtable")),column(5,offset=1,br(),plotlyOutput("ADAPowerplot"),br(),
                                                                                                     plotlyOutput("ADAFOplot"),br(),plotlyOutput("ADAcompareShopplot")))))
    )
    
  }
})
#Performances Curve TAb ............................................
output$myperformancetabs = renderUI({
  y=VESSELDETAILS
  ff=input$filtVessel
  y=subset(y,y$Vessel==ff)
  ADA = unique(as.character(y[,15]), incomparables = FALSE)
  
  if(ADA =="N"){ 
    mytabs = tabsetPanel(tabPanel( "Noon Performance Curve",br(),
                                  box(width=NULL,solidHeader = TRUE,status="info",
                                      fluidRow(column(width=4,selectInput("seastate",label = "Sea State",choices = c(0,1,2,3,4,5,6,7,8),selected = 3)),
                                            column(width=3, box(width=NULL,solidHeader = TRUE,status="info", textOutput("Vessel2"),br(),textOutput("Date2"),
                                                                tags$head(tags$style("#Vessel2{color: green; font-size: 30px; font-style: bold; font-family: Lato;}"  )),
                                                                tags$head(tags$style("#Date2{color: green; font-size: 20px; font-style: bold; font-family: Lato;}"  ))
                                            )),                                   
                                            column(width=3,offset=2,actionButton("SAVE", "SAVE",width="50%")) )),
                                   br(),
                                   
                                   box(title= strong("FO Calculator"),status="info" ,solidHeader =TRUE,box(column(dataTableOutput("FO_cal"),width=5),status="info" ,solidHeader =TRUE,width=NULL,
                                                                                                           column(br(),plotlyOutput("speedFO"),offset = 1,width = 6)),width=NULL),
                                   br(),
                                   box(title= strong("Power Calculator"),status="info" ,solidHeader =TRUE,box(column(dataTableOutput("power_cal"),width = 5),status="info" ,solidHeader =TRUE,width=NULL,column(br(),plotlyOutput("speedpower"),offset = 1,width=6)),width = NULL),
                                   br(),
                                   box(title= strong("FO Calculator For Sea State"),status="info" ,solidHeader =TRUE,width=NULL,
                                       column(width=3,numericInput("Draftss",label = "Draft",value =  10)),br(),br(),br(),br(),
                                       box(column(dataTableOutput("SSFO_cal"),width = 4),status="info" ,solidHeader =TRUE,width=NULL,
                                           column(br(),plotlyOutput("SSFO"),offset = 1,width=6)))
    ))
    
  }
  else
  {
    
    mytabs = tabsetPanel(tabPanel( "Noon Performance Curve",br(),
                                  box(width=NULL,solidHeader = TRUE,status="info",
                                    fluidRow(column(width=4,selectInput("seastate",label = "Sea State",choices = c(0,1,2,3,4,5,6,7,8),selected = 3)),
                                            column(width=3, box(width=6,solidHeader = TRUE,status="info", textOutput("Vessel2"),br(),textOutput("Date2"),
                                                                tags$head(tags$style("#Vessel2{color: green; font-size: 30px; font-style: bold;  font-family: Lato;}"  )),
                                                                tags$head(tags$style("#Date2{color: green; font-size: 20px; font-style: bold; font-family: Lato;}"  ))
                                            )),                                   
                                            column(width=3,offset=2,actionButton("SAVE", "SAVE",width="50%")) )),
                                   br(),
                                   
                                   box(title= strong("FO Calculator"),status="info" ,solidHeader =TRUE,box(column(dataTableOutput("FO_cal"),width=5),status="info" ,solidHeader =TRUE,width=NULL,
                                                                                                           column(br(),plotlyOutput("speedFO"),offset = 1,width = 6)),width=NULL),
                                   br(),
                                   box(title= strong("POWER Calculator"),status="info" ,solidHeader =TRUE,box(column(dataTableOutput("power_cal"),width = 5),status="info" ,solidHeader =TRUE,width=NULL,column(br(),plotlyOutput("speedpower"),offset = 1,width=6)),width = NULL),
                                   br(),
                                   box(title= strong("FO Calculator For Sea State"),status="info" ,solidHeader =TRUE,width=NULL,
                                       column(width=3,numericInput("Draftss",label = "Draft",value =  10)),br(),br(),br(),br(),
                                       box(column(dataTableOutput("SSFO_cal"),width = 4),status="info" ,solidHeader =TRUE,width=NULL,
                                           column(br(),plotlyOutput("SSFO"),offset = 1,width=6)))
    ),
    
    tabPanel( "ADA Performance Curve",
              fluidRow(column(width=5,selectInput("ADAseastate",label = "Sea State",choices = c(0,1,2,3,4,5,6,7,8),selected = 3)),
                       column(offset=2,width=5, downloadButton('ADAReport' ,strong('Download Report')),                                      
                              actionButton("ADASAVE", "SAVE",width="25%") )),
              br(),
              
              box(title= strong("FO Calculator"),status="info" ,solidHeader =TRUE,box(column(dataTableOutput("ADAFO_cal"),width=5),status="info" ,solidHeader =TRUE,width=NULL,
                                                                                      column(br(),plotlyOutput("ADAspeedFO"),offset = 1,width = 6)),width=NULL),
              br(),
              box(title= strong("POWER Calculator"),status="info" ,solidHeader =TRUE,box(column(dataTableOutput("ADApower_cal"),width = 5),status="info" ,solidHeader =TRUE,width=NULL,column(br(),plotlyOutput("ADAspeedpower"),offset = 1,width=6)),width = NULL))
    )
  }
  mytabs
})
#operating profile...............................................
output$myoperatingprofiletabs = renderUI({
  y=VESSELDETAILS
  ff=input$filtVessel
  y=subset(y,y$Vessel==ff)
  ADA = unique(as.character(y[,15]), incomparables = FALSE)
  
  if(ADA =="N"){
    
    mytabs = tabsetPanel(tabPanel( "Noon Operating Profile",br(),  fluidRow(box(solidHeader= TRUE,status="info",title=strong("DRAFT"),plotlyOutput("OPDraft")),
                                                                            box(solidHeader= TRUE,status="info",title=strong("SPEED"),plotlyOutput("OPSpeed"))),
                                   fluidRow(box(solidHeader= TRUE,status="info",title=strong("Sea State"),plotlyOutput("OPSEAS")),        
                                            box(solidHeader= TRUE,status="info",title=strong("Power"),plotlyOutput("OPPower")),
                                            box(solidHeader= TRUE,status="info",title=strong("FO/24Hrs"),plotlyOutput("OPFO")))))
  }
  else{
    mytabs = tabsetPanel(tabPanel( "Noon Operating Profile",br(),  fluidRow(box(solidHeader= TRUE,status="info",title=strong("DRAFT"),plotlyOutput("OPDraft")),
                                                                            box(solidHeader= TRUE,status="info",title=strong("SPEED"),plotlyOutput("OPSpeed"))),
                                   fluidRow(box(solidHeader= TRUE,status="info",title=strong("Sea State"),plotlyOutput("OPSEAS")),        
                                            box(solidHeader= TRUE,status="info",title=strong("Power"),plotlyOutput("OPPower")),
                                            box(solidHeader= TRUE,status="info",title=strong("FO/24Hrs"),plotlyOutput("OPFO")))),
                         tabPanel( "ADA Operating Profile",  fluidRow(box(solidHeader= TRUE,status="info",title=strong("DRAFT"),plotlyOutput("ADAOPDraft")),
                                                                      box(solidHeader= TRUE,status="info",title=strong("SPEED"),plotlyOutput("ADAOPSpeed"))),
                                   fluidRow(box(solidHeader= TRUE,status="info",title=strong("Sea State"),plotlyOutput("ADAOPSEAS")),        
                                            box(solidHeader= TRUE,status="info",title=strong("Power"),plotlyOutput("ADAOPPower")),
                                            box(solidHeader= TRUE,status="info",title=strong("FO/24Hrs"),plotlyOutput("ADAOPFO"))))
    )
  }
  mytabs
})



#filtered data......
vesselpart <- reactive({
  y=data.frame(read.csv("data/Vessel Detail.csv"))
  validate(
    need(try(input$filtVessel),"Please Wait or Select the vessel")
  )
  ff=input$filtVessel
  y=subset(y,y$Vessel == ff)
})


mcr <- reactive({
  y=data.frame(read.csv("data/Vessel Detail.csv"))
  ff=input$filtVessel
  y=subset(y,y$Vessel == ff)
  y = unique(as.numeric(y[,7]), incomparables = FALSE)
  mcr = as.numeric (y)
})

shopdataxx = reactive ({
  y=Shoptrial
  ff=input$filtVessel
  y=subset(y,y$VESSEL.NAME==ff)
  y$VESSEL.NAME = NULL
  y$FLEET=NULL
  y$CLASS=NULL
  colnames(y)=c("Engine Load %","Power(kW)","SFOC Measured(g/kW-Hr)","SFOC Corrected(g/kW-Hr)")
  y
})

output$filtvessel <- renderUI({ 
  r=DATA
  s=input$filttype
  if(s=="Fleet Wise"){
    r=subset(r,Fleet==input$filtfleet)
    
    Vessel_List = unique(as.character(r[,3]), incomparables = FALSE)
    selectInput("filtVessel", label=h4(strong("Vessel")), choices = Vessel_List, selected = "Strategic Alliance", multiple = FALSE, selectize = TRUE, width = "75%", size = NULL)
  }
  else{
    Vessel_List = unique(as.character(r[,3]), incomparables = FALSE)
    selectInput("filtVessel", label=h4(strong("Vessel")), choices = Vessel_List, selected = "Strategic Alliance", multiple = FALSE, selectize = TRUE, width = "75%", size = NULL)
  }
})

output$Vessel1 <- renderText({ 
  
  paste0(input$filtVessel)
})

output$Date1 <- renderText({ 
  
  paste0("Analysis period:",input$dates[1] ,"  &  " ,input$dates[2] )
})


output$Vessel2 <- renderText({ 
  
  paste0(input$filtVessel)
})

output$Date2 <- renderText({ 
  
  paste0("Analysis period:",input$dates[1] ,"  &  " ,input$dates[2] )
})




output$filtfleet <- renderUI({ 
  r=DATA
  s=input$filttype
  if(s=="Fleet Wise"){
    Vessel_List = unique(as.character(r[,1]), incomparables = FALSE)
    selectInput("filtfleet", label=h4(strong("Fleet")), choices = Vessel_List, selected = 1, multiple = FALSE, selectize = TRUE, width = "25%", size = NULL)
  }
  else{return(NULL)}
})

output$draftlimit <- renderUI({
  r= vesselpart()
  minD = unique(as.numeric(as.character(r[,4])), incomparables = FALSE)
  maxD = unique(as.numeric(r[,5]), incomparables = FALSE)
  sliderInput("draft",label = "Draft Range",min = minD,max = maxD,value=c(minD,maxD),step = 0.5,width = "75%" )
})
output$speedlimit <- renderUI({
  r= vesselpart()
  maxS = round(unique(as.numeric(r[,6]), incomparables = FALSE),0)
  sliderInput("speed",label = "Speed Range",min = 9,max = maxS,value = c(10,maxS), step = 0.5,width = "75%")
})

output$sliplimit <- renderUI({
  sliderInput("slip",label = "Slip Range",min = -20,max = 20,value = c(-20,20), step = 0.5,width = "75%")
})
output$powerlimit <- renderUI({
  y=round(mcr(),0)
  x=round((0.15*mcr()),0)
  sliderInput("Power",label = "Power Range",min = x ,max = y,value = c(x,y),step=0.5,width = "75%")
})
output$streaminglimit <- renderUI({
  sliderInput("steam",label = "Steaming time ",min = 0,max = 26,value = c(5,24), step = 0.5,width = "75%")
})


xx=reactive({
  input$goButton
  if(input$goButton==0){return()}
  y=DATA
  isolate({
    dataf=subset(y,y$Vessel.Name==input$filtVessel)
    dataf=subset(dataf,Report.Type == "NOON")
    VESSEL= dataf$Vessel.Name;DATE=dataf$Corrected.Date;DRAFT=round(dataf$DRAFT,2);SPEED=round(dataf$SOG,2);STW=round(dataf$SOW,2);RPM=as.numeric(dataf$RPM);SLIP=as.numeric(dataf$SLIP);
    POWER=as.numeric(as.character(dataf$POWER.kW));FO=dataf$FO.per.24Hrs;SEASTATE=as.numeric(as.character(dataf$SEA.STATE));SFOC=as.numeric(as.character(dataf$SFOC));STEAMINGTIME=as.numeric(dataf$TOTAL.STEAMING.TIME)
    Noon=data.frame(VESSEL,DATE,SEASTATE,DRAFT,SPEED,STW,RPM,SLIP,POWER,FO,SFOC,STEAMINGTIME )
    Noon=subset(Noon,as.Date(DATE,"%d-%m-%y")>=as.Date(input$dates[1],"%d-%m-%y")& as.Date(DATE,"%d-%m-%y")<=as.Date(input$dates[2],"%d-%m-%y"))
    Noon=subset(Noon,SPEED > as.numeric(input$speed[1]) & SPEED < as.numeric(input$speed[2]) & SEASTATE >=as.numeric(input$SEASTATE[1]) & SEASTATE <= as.numeric(input$SEASTATE[2]))
    Noon=subset(Noon, POWER > as.numeric(input$Power[1]) & POWER < as.numeric(input$Power[2]))
    Noon=subset(Noon,DRAFT > as.numeric(input$draft[1]) & DRAFT < as.numeric (input$draft[2]))
    Noon=subset(Noon,SFOC > as.numeric(input$SFOC[1]) & SFOC < as.numeric(input$SFOC[2]))
    Noon=subset(Noon,SLIP > as.numeric(input$slip[1]) & SLIP < as.numeric(input$slip[2]))
    mcr=as.numeric(input$Power[2])
    Noon=subset(Noon,STEAMINGTIME >= as.numeric(input$steam[1]) & STEAMINGTIME <= as.numeric(input$steam[2]))
    Noon$LOAD =((Noon$POWER/mcr)*100)
    
  })
  
  Noon
}
)

output$Fplots=renderPlotly({
  dataf=xx()
  if(is.null(dataf)){return()}
  i=input$P1axis
  if(i==1){ y=plot_ly(data = dataf, x = ~SPEED , y = ~POWER , type='scatter',mode = "markers",marker=list(color="#7E57C2",size = 8),name="Observed Power")}
  if(i==2){ y=plot_ly(data = dataf, x = ~SPEED , y = ~FO , type='scatter',mode = "markers",marker=list(color="#4DD0E1",size = 8),name="Observed FO")}
  
  y%>%layout(
    xaxis = list(title = "Speed (knots)", titlefont =f, tickfont =f,gridcolor = "#FFFFFF"),
    yaxis = list(titlefont = f, tickfont = f,gridcolor = "#ABB2B9"), 
    plot_bgcolor = "#FFFFFF",
    paper_bgcolor = "#FFFFFF"
  )
})

output$Fdd=renderPlotly({
  
  dataf=xx()
  if(is.null(dataf)){return()}
  dataf$DATE=as.Date(dataf$DATE,"%d-%m-%y")
  i=input$Faxis
  if(i==1){ y=plot_ly(data = dataf, x = ~DATE , y = ~POWER , type='scatter',mode = "markers",marker=list(color="#EC7063",size = 8),showlegend = FALSE)}
  if(i==2){ y=plot_ly(data = dataf, x = ~DATE, y = ~FO, type='scatter',mode = "markers",marker=list(color="#DC7633",size = 8),showlegend = FALSE)}
  if(i==3){ y=plot_ly(data = dataf,x = ~DATE , y = ~SFOC , type='scatter',mode = "markers",marker=list(color="#AF7AC5",size = 8),showlegend = FALSE)}
  if(i==4){y= plot_ly(data = dataf,x = ~DATE  , y = ~SPEED ,type='scatter',name = "SOG", mode = "markers",marker=list(color="#2E86C1",size = 8),showlegend = T)
  y= y%>%add_trace(y = ~STW ,name = "STW", type='scatter',mode = "markers",marker=list(color="#F1C40F",size = 8),showlegend = T)}
  if(i==5){y= plot_ly(data = dataf, x = ~DATE , y = ~DRAFT , type='scatter',mode = "markers",marker=list(color="#3498DB",size = 8),showlegend = FALSE)}
  if(i==6){y= plot_ly(data = dataf, x = ~DATE , y = ~SEASTATE , type='scatter',mode = "markers",marker=list(color="#8E44AD",size = 8),showlegend = FALSE)}
  if(i==7){y= plot_ly(data = dataf, x = ~DATE , y = ~SLIP , type='scatter',mode = "markers",marker=list(color="#C00CF0",size = 8),showlegend = FALSE)}
  if(i==8){y= plot_ly(data = dataf, x = ~DATE , y = ~RPM ,type='scatter', mode = "markers",marker=list(color="#F00C7B",size = 8),showlegend = FALSE)}
  
  

  
  y%>%layout(
    xaxis = list(title = "Date", titlefont =f, tickfont =f,gridcolor = "#FFFFFF",tickangle=0),
    yaxis = list(titlefont = f, tickfont = f,gridcolor = "#ABB2B9"), 
    plot_bgcolor = "#FFFFFF",
    paper_bgcolor = "#FFFFFF",legend=l
    
  )
})


output$SF1 = renderPlotly({
  dataf=xx()
  
  if(is.null(dataf)){return()}
  plot_ly(data=dataf,x=~LOAD,y= ~SFOC,type='scatter',mode = "markers",color=~LOAD, marker=list(size = 8,colorscale='Viridis', colorbar = list(title = "Load",titlefont=s,tickfont=s)),name="SFOC" )%>%
    layout(title= "SFOC Vs Load",titlefont=c,xaxis = list(title = " Engine Load", titlefont =f, tickfont =f,gridcolor = "#FFFFFF",tickangle=0),
           yaxis = list(title= "SFOC",titlefont = f, tickfont = f,gridcolor = "#ABB2B9"), 
           plot_bgcolor = "#FFFFFF",
           paper_bgcolor = "#FFFFFF",showlegend = FALSE
           
    )
})


output$Fldata=renderDataTable({
  y=xx()
  if(is.null(y)){return()}
  y$LOAD=NULL
  
  datatable(y,class = 'cell-border stripe',options = list(pageLength = 150,searching = FALSE,paging = T,audowidth=T,dom = "tip",
                                                          scrollX=TRUE, scrollY =1200,
                                                          scroller = T ),colnames = c("Vessel","Date","Sea State","Draft","Speed","STW","RPM","Slip","Power","FO","SFOC","Steaming Time"),rownames = FALSE)%>%
    formatStyle(names(y),backgroundColor = "#ECF0F1",color="#000", className = 'dt-center')
})

output$filterdata <- downloadHandler(
  filename = function() { 'filtered_data.csv' }, content = function(file) {
    write.csv(xx(), file, row.names = FALSE)
  }
)



#ADA FILTERDATA..........................................
output$ADAdraftlimit <- renderUI({
  r= vesselpart()
  minD = unique(as.numeric(as.character(r[,4])), incomparables = FALSE)
  maxD = unique(as.numeric(r[,5]), incomparables = FALSE)
  sliderInput("ADAdraft",label = "Draft Range",min = minD,max = maxD,value=c(minD,maxD),step = 0.5,width = "75%" )
})
output$ADAspeedlimit <- renderUI({
  r= vesselpart()
  maxS = round(unique(as.numeric(r[,6]), incomparables = FALSE),0)
  sliderInput("ADAspeed",label = "Speed Range",min = 9,max = maxS,value = c(10,maxS), step = 0.5,width = "75%")
})

output$ADAsliplimit <- renderUI({
  sliderInput("ADAslip",label = "Slip Range",min = -20,max = 20,value = c(-20,20), step = 0.5,width = "75%")
})
output$ADApowerlimit <- renderUI({
  y=mcr()
  x=0.15*mcr()
  sliderInput("ADAPower",label = "Power Range",min = x ,max = y,value = c(x, y), width = "75%")
})

ADAxx=reactive({
  input$ADAgoButton
  if(input$ADAgoButton==0){return()}
  y=ADADATA
  isolate({ 
    dataf=subset(y,y$Vessel==input$filtVessel)
    dataf=subset(dataf,Report.Type == "SEA")
    VESSEL= dataf$Vessel;DATE=as.Date(dataf$Corrected.Date,"%d-%m-%y");DRAFT=suppressWarnings(round(as.numeric(dataf$DRAFT),2));SPEED=suppressWarnings(round(as.numeric(as.character(dataf$SOG)),2));RPM=dataf$RPM;SLIP=as.numeric(dataf$Apparent.Slip);
    POWER=suppressWarnings(as.numeric(as.character(dataf$Power.KW)));FO=dataf$FO.per.24.Hrs;SEASTATE=suppressWarnings(as.numeric(as.character(dataf$SEA.STATE)));SFOC=suppressWarnings(as.numeric(as.character(dataf$SFOC)))
    Noon=data.frame(VESSEL,DATE,SEASTATE,DRAFT,SPEED,RPM,SLIP,POWER,FO,SFOC)
    Noon=subset(Noon,DATE >= as.Date(input$ADAdates[1])& DATE <=as.Date(input$ADAdates[2]))
    Noon=subset(Noon,SPEED > as.numeric(input$ADAspeed[1]) & SPEED < as.numeric(input$ADAspeed[2]))
    Noon=subset(Noon, POWER > as.numeric(input$ADAPower[1]) & POWER < as.numeric(input$ADAPower[2]))
    Noon=subset(Noon,DRAFT > as.numeric(input$ADAdraft[1]) & DRAFT < as.numeric (input$ADAdraft[2]))
    Noon=subset(Noon,SFOC > as.numeric(input$ADASFOC[1]) & SFOC < as.numeric(input$ADASFOC[2]))
    mcr=as.numeric(input$Power[2])
    Noon=subset(Noon,SLIP > as.numeric(input$ADAslip[1]) & SLIP < as.numeric(input$ADAslip[2]))
    Noon=subset(Noon,SEASTATE >=as.numeric(input$ADASEASTATE[1]) & SEASTATE<= as.numeric(input$ADASEASTATE[2]))
    Noon$LOAD = ((Noon$POWER/mcr)*100)
  })
  Noon
}
)

output$ADAFplots=renderPlotly({
  
  validate(
    need(try(ADAxx()),"Press Go Button to load Data..........")
  )
  dataf=ADAxx()
  if(is.null(dataf)){return()}
  i=input$ADAP1axis
  if(i==1){ y=plot_ly(data = dataf, x = ~SPEED , y = ~POWER , type='scatter',mode = "markers",marker=list(color="#7E57C2",size = 8),name="Observed Power")}
  if(i==2){ y=plot_ly(data = dataf, x = ~SPEED , y = ~FO ,type='scatter', mode = "markers",marker=list(color="#4DD0E1",size = 8),name="Observed Power")}
  
  
  y=y%>%layout(
    xaxis = list(title = "Speed (knots)", titlefont =f, tickfont =f,gridcolor = "#FFFFFF"),
    yaxis = list(titlefont = f, tickfont = f,gridcolor = "#ABB2B9"), 
    plot_bgcolor = "#FFFFFF",
    paper_bgcolor = "#FFFFFF"
  )
})

output$ADAFdd=renderPlotly({
  
  dataf=ADAxx()
  if(is.null(dataf)){return()}
  dataf$DATE=as.Date(dataf$DATE,"%d-%m-%y")
  i=input$ADAFaxis
  if(i==1){ y=plot_ly(data = dataf, x = ~DATE , y = ~POWER ,type='scatter', mode = "markers",marker=list(color="#EC7063",size = 8))}
  if(i==2){ y=plot_ly(data = dataf, x = ~DATE, y = ~FO, type='scatter',mode = "markers",marker=list(color="#DC7633",size = 8))}
  if(i==3){ y=plot_ly(data = dataf,x = ~DATE , y = ~SFOC , type='scatter',mode = "markers",marker=list(color="#AF7AC5",size = 8))}
  if(i==4){y= plot_ly(data = dataf,x = ~DATE  , y = ~SPEED , type='scatter',mode = "markers",marker=list(color="#F9E79F",size = 8))}
  if(i==5){y= plot_ly(data = dataf, x = ~DATE , y = ~DRAFT , type='scatter',mode = "markers",marker=list(color="#3498DB",size = 8))}
  if(i==6){y= plot_ly(data = dataf, x = ~DATE , y = ~SEASTATE ,type='scatter', mode = "markers",marker=list(color="#DCE775",size = 8))}
  if(i==7){y= plot_ly(data = dataf, x =~DATE , y = ~SLIP , type='scatter',mode = "markers",marker=list(color="#470CF0",size = 8))}
  if(i==8){y= plot_ly(data = dataf, x = ~DATE , y = ~RPM , type='scatter',mode = "markers",marker=list(color="#F00C7B",size = 8))}

  
 y= y%>%layout(
    xaxis = list(title = "Date", titlefont =f, tickfont =f,gridcolor = "#FFFFFF",tickangle=0),
    yaxis = list(titlefont = f, tickfont = f,gridcolor = "#ABB2B9"), 
    plot_bgcolor = "#FFFFFF",
    paper_bgcolor = "#FFFFFF",showlegend = FALSE
    
  )
})




output$ADASF1 = renderPlotly({
  d=ADAxx()
  if(is.null(d)){return()}
  
  plot_ly(data=d,x=~LOAD,y= ~SFOC,type='scatter',mode = "markers",color= ~LOAD, marker=list(size = 8,colorscale='Viridis', colorbar = list(title = "Load",titlefont=s,tickfont=s)),name="SFOC" )%>%
    
    layout(title= "SFOC Vs Load",titlefont=f,
           xaxis = list(title = " Engine Load", titlefont =f, tickfont =f,gridcolor = "#FFFFFF",tickangle=0),
           yaxis = list(title= "SFOC",titlefont = f, tickfont = f,gridcolor = "#ABB2B9"), 
           plot_bgcolor = "#FFFFFF",
           paper_bgcolor = "#FFFFFF",showlegend = FALSE
           
    )
})

output$ADAFldata=renderDataTable({
  y=ADAxx()
  if(is.null(y)){return()}
  y$LOAD=NULL
  datatable(y,class = 'cell-border stripe',options = list(pageLength = 150,searching = FALSE,paging = T,dom = "tip",
                                                          scrollX=TRUE, scrollY =1200,
                                                          scroller = T ),colnames = c("Vessel","Date","Sea State","Draft","Speed","RPM","Slip","Power","FO","SFOC"),rownames = FALSE)%>%
    formatStyle(names(y),backgroundColor = "#ECF0F1",color="#000", className = 'dt-center')
})

output$ADAfilterdata <- downloadHandler(
  filename = function() { 'ADAfiltered_data.csv' }, content = function(file) {
    write.csv(ADAxx(), file, row.names = FALSE)
  }
)




#Analysis Procedure......

fit=reactive({
  y=xx()
  if(is.null(y)){return()}
  P=y$POWER
  ss=log(10-y$SEASTATE)
  D=y$DRAFT
  S=log(y$SPEED)
  Pd=log(P/D^(as.numeric(n1())))
  fit=lm(Pd~ss+S)
})

fitss=reactive({
  y=xx()
  if(is.null(y)){return()}
  P=y$POWER
  ss=log(10-y$SEASTATE)
  D=y$DRAFT
  S=log(y$SPEED)
  Pd=log(P/(D^(as.numeric(n1()))*ss^(-0.3)))
  fit=lm(Pd~S)
})


n1=reactive({
  y=data.frame(read.csv("data/Hydros Data.csv"))
  ff=input$filtVessel
  if(is.null(ff))
    return(NULL)
  y=subset(y,y$Vessel == ff)
  y=subset(y,Draft >= input$draft[1] & Draft <= input$draft[2])
  n1=lm((log(y$WSA))~(log(y$Draft)))$coeff[2]
})

# coefficients
coeffdata = reactive({
  fit=fit()
  fitss=fitss()
  n1=n1()
  if(fit$coefficients[2] >= 0) {
    n2=fitss$coefficients[2]
    n3= -0.3
    k=fitss$coefficients[1]
  } else {
    n2=fit$coefficients[3]
    n3=fit$coefficients[2]
    k=fit$coefficients[1]
  }
  x=data.frame(n1,n2,n3,k)
  x
})

#FO Predicition .......................
fit1=reactive({
  y=xx()
  P=y$FO
  ss=log(10-y$SEASTATE)
  D=y$DRAFT
  S=log(y$SPEED)
  Pd=log(P/D^(as.numeric(n1())))
  fit=lm(Pd~ss+S)
})


fit1ss=reactive({
  y=xx()
  if(is.null(y)){return()}
  P=y$FO
  ss=log(10-y$SEASTATE)
  D=y$DRAFT
  S=log(y$SPEED)
  Pd=log(P/(D^(as.numeric(n1()))*ss^(-0.3)))
  fit=lm(Pd~S)
})

# FO coefficients

FOcoeffdata = reactive({
  fit=fit1()
  fitss=fit1ss()
  n1=n1()
  if(fit$coefficients[2] >= 0) {
    n2=fitss$coefficients[2]
    n3= -0.3
    k=fitss$coefficients[1]
  } else {
    n2=fit$coefficients[3]
    n3=fit$coefficients[2]
    k=fit$coefficients[1]
  }
  x=data.frame(n1,n2,n3,k)
  x
})



PP=reactive({
  y=xx()
  fo=FOcoeffdata()
  P= coeffdata()
  if(is.null(y)){return()}
  y$Pred_POWER=round((y$DRAFT^P$n1*y$SPEED^P$n2*(10-y$SEASTATE)^P$n3*exp(P$k)),2)
  y$Pred_FO=round((y$DRAFT^fo$n1*y$SPEED^fo$n2*(10-y$SEASTATE)^fo$n3*exp(fo$k)),2)
  y$Power_Error=((y$Pred_POWER-y$POWER)/(y$POWER))
  y$FO_Error=((y$Pred_FO-y$FO)/(y$FO))
  y=y
})

output$EPOWER <- renderValueBox({
  y=PP()
  if(is.null(y)){return()}
  DP = (y$POWER-y$Pred_POWER)/y$POWER
  Perror = abs(DP)
  DP=round(mean(Perror)*100,digits = 1 )
  DPower= 100-DP
  valueBox(
    paste0(DPower, "%"), strong("Accuracy in Power Prediction"), 
    color = "navy",width = NULL
  )}
)

output$EFO <- renderValueBox({
  y=PP()
  if(is.null(y)){return()}
  DF = (y$FO-y$Pred_FO)/y$FO
  
  Ferror = abs(DF)
  DF=round((mean(Ferror)*100),digits = 1 ) 
  DFO=100-DF
  valueBox(
    paste0(DFO, "%"), strong("Accuracy in FO/24Hrs Prediction"), 
    color = "navy",width = NULL
  )
})



output$PPtable=renderDataTable({
  y=PP()
  if (is.null(y))
    return(NULL)
  
  datatable(y,class = 'cell-border stripe',options = list(pageLength = 150,searching = FALSE,paging = T,dom = "tip",
                                                          scrollX=TRUE, scrollY =1200,
                                                          scroller = T ),colnames = c("Vessel","Date","Sea State","Draft","Speed","STW","RPM","Slip","Power","FO","SFOC","Steaming Time","Load","Predicted Power","Predicted FO","Power Error","FO Error"),rownames = FALSE)%>%
    formatPercentage("Power_Error",1)%>%formatPercentage("FO_Error",1)%>%
    formatStyle(names(y),backgroundColor = "#ECF0F1",color="#000")
})

output$Powerplot=renderPlotly({
  dataf=PP()
  if (is.null(dataf))
    return(NULL)
  
  p=plot_ly(data =PP(), x = ~SPEED, y = ~POWER, type='scatter',mode = "markers",marker=list(color="green",size=12),name="Observed Power")%>%
   add_trace(data= PP(),x = ~SPEED,y = ~Pred_POWER,type='scatter',mode= "markers",marker=list(color="#BF1BE7",size=12),name="Predicted Power")%>% 
    
    
    layout(title="Power Vs Speed",titlefont=c,legend = l, xaxis = list(title = " Speed (knots)", titlefont =f,tickfont = f, gridcolor = "#FFFFFF",tickangle=0),
                               yaxis = list(title= "Power (kW)",titlefont = f, tickfont = f,gridcolor = "#ABB2B9"), 
                                     plot_bgcolor = "#FFFFFF", paper_bgcolor = "#FFFFFF",showlegend = TRUE
                                                                       
    )
})
output$FOplot=renderPlotly({
  dataf=PP()
  if (is.null(dataf))
    return(NULL)
  
  p = plot_ly(data =PP(), x = ~SPEED, y = ~FO,type='scatter', mode = "markers",marker=list(color="green",size=12),name="Observed FO")%>%
    add_trace(data= PP(),x = ~SPEED,y = ~Pred_FO,type='scatter',mode= "markers",marker=list(color="#BF1BE7",size=12),name="Predicted FO")%>%
    
    layout(title="FO Vs Speed",titlefont =c,legend = l, xaxis = list(title = " Speed (knots)", titlefont =f, tickfont =f,gridcolor = "#FFFFFF",tickangle=0),
                                            yaxis = list(title= "FO (tonne)",titlefont = f, tickfont = f,gridcolor = "#ABB2B9"), 
                                      plot_bgcolor = "#FFFFFF",  paper_bgcolor = "#FFFFFF",showlegend = TRUE
                                                                     
    )
})
output$compareShopplot=renderPlotly({
  y=shopdataxx()
  m = PP()
  mcr=mcr()
  if (is.null(y))
    return(NULL)
  
  x1=y[,1]
  y1= y[,3]
  
  #x1 = c(1,2,3,4,5)
  #y1 = c(1,4,9,16,25)
  
  fit2aa <- lm(y1 ~ poly(x1, 2, raw=TRUE))
  n1=as.numeric(fit2aa$coefficients[3])
  n2=as.numeric(fit2aa$coefficients[2])
  k=as.numeric(fit2aa$coefficients[1])
  
  
  power = m$POWER
  SFOC = m$SFOC
  powratio = power*100/mcr
  thSFOC = round(powratio^2*n1+powratio*n2+k,2)
  SFOCratio = SFOC/thSFOC
  avg = mean(SFOCratio)
  
  testx = seq(min(x1,na.rm = T),max(x1,na.rm = T),length.out = 30)
  testy = testx^2*n1+testx*n2+k
  testy1 = testy*avg
  
  r=seq(150,300,by=10)
  
  p = plot_ly(x= testx, y= testy,name="Shop Trial SFOC",type='scatter',mode="lines+markers",line = list(shape = "spline",color="#15972B"),marker=list(color="#15972B",opacity=0))%>%
    add_trace(x = testx,y = testy1,name = "Actual SFOC",type='scatter',mode="lines+markers",line = list(shape = "spline",color="#BF1BE7"),marker=list(color="#BF1BE7",opacity=0))%>%
    layout(title="Load(%) Vs SFOC",titlefont=c,
         xaxis = list(title = "Load(%)", titlefont =f, tickfont =f,gridcolor = "#FFFFFF"),
         yaxis = list(title = "SFOC(g/kW-Hr)",   titlefont = f, tickfont = f,gridcolor = "#ABB2B9",range=c(150,250)), 
         plot_bgcolor = "#FFFFFF",
         paper_bgcolor = "#FFFFFF",legend = l,showlegend=T
  )
  
})




# ADA Analysis Procedure......
#ADA Power PREDICTION.............
ADAfit=reactive({
  y=ADAxx()
  if(is.null(y)){return()}
  P=y$POWER
  ss=log(10-y$SEASTATE)
  D=y$DRAFT
  S=log(y$SPEED)
  Pd=log(P/D^(as.numeric(n1())))
  fit=lm(Pd~ss+S)
})

ADAfitss=reactive({
  y=ADAxx()
  if(is.null(y)){return()}
  P=y$POWER
  ss=log(10-y$SEASTATE)
  D=y$DRAFT
  S=log(y$SPEED)
  Pd=log(P/(D^(as.numeric(n1()))*ss^(-0.3)))
  fit=lm(Pd~S)
})


ADAn1=reactive({
  y=data.frame(read.csv("data/Hydros Data.csv"))
  ff=input$filtVessel
  if(is.null(ff))
    return(NULL)
  y=subset(y,y$Vessel == ff)
  y=subset(y,Draft >= input$ADAdraft[1] & Draft <= input$ADAdraft[2])
  n1=lm((log(y$WSA))~(log(y$Draft)))$coeff[2]
})

# coefficients
ADAcoeffdata = reactive({
  fit=ADAfit()
  fitss=ADAfitss()
  n1=ADAn1()
  if(fit$coefficients[2] >= 0) {
    n2=fitss$coefficients[2]
    n3= -0.3
    k=fitss$coefficients[1]
  } else {
    n2=fit$coefficients[3]
    n3=fit$coefficients[2]
    k=fit$coefficients[1]
  }
  x=data.frame(n1,n2,n3,k)
  x
})


#FO Predicition .......................
ADAfit1=reactive({
  y=ADAxx()
  if(is.null(y)){return()}
  P=y$FO
  ss=log(10-y$SEASTATE)
  D=y$DRAFT
  S=log(y$SPEED)
  Pd=log(P/D^(as.numeric(n1())))
  fit=lm(Pd~ss+S)
})

ADAfit1ss=reactive({
  y=ADAxx()
  if(is.null(y)){return()}
  P=y$FO
  ss=log(10-y$SEASTATE)
  D=y$DRAFT
  S=log(y$SPEED)
  Pd=log(P/(D^(as.numeric(n1()))*ss^(-0.3)))
  fit=lm(Pd~S)
})

# FO coefficients

ADAFOcoeffdata = reactive({
  fit=ADAfit1()
  fitss=ADAfit1ss()
  n1=ADAn1()
  if(fit$coefficients[2] >= 0) {
    n2=fitss$coefficients[2]
    n3= -0.3
    k=fitss$coefficients[1]
  } else {
    n2=fit$coefficients[3]
    n3=fit$coefficients[2]
    k=fit$coefficients[1]
  }
  x=data.frame(n1,n2,n3,k)
  x
})

ADAPP=reactive({
  y=ADAxx()
  P= ADAcoeffdata ()
  fo = ADAFOcoeffdata()
  if(is.null(y)){return()}
  y$Pred_POWER=round((y$DRAFT^P$n1*y$SPEED^P$n2*(10-y$SEASTATE)^P$n3*exp(P$k)),2)
  y$Pred_FO=round((y$DRAFT^fo$n1*y$SPEED^fo$n2*(10-y$SEASTATE)^fo$n3*exp(fo$k)),2)
  y$Power_Error=((y$Pred_POWER-y$POWER)/(y$POWER))
  y$FO_Error=((y$Pred_FO-y$FO)/(y$FO))
  y=y
})

output$ADAEPOWER <- renderValueBox({
  y=ADAPP()
  if(is.null(y)){return()}
  DP = (y$POWER-y$Pred_POWER)/y$POWER
  Perror = abs(DP)
  DP=round(mean(Perror)*100,digits = 1 )
  DPower= 100-DP
  valueBox(
    paste0(DPower, "%"), strong("Accuracy in Power Prediction"), 
    color = "green",width = NULL
  )}
)

output$ADAEFO <- renderValueBox({
  y=ADAPP()
  if(is.null(y)){return()}
  DF = (y$FO-y$Pred_FO)/y$FO
  
  Ferror = abs(DF)
  DF=round((mean(Ferror)*100),digits = 1 ) 
  DFO=100-DF
  valueBox(
    paste0(DFO, "%"), strong("Accuracy in FO/24Hrs Prediction"), 
    color = "green",width = NULL
  )
})


output$ADAPPtable=renderDataTable({
  y=ADAPP()
  if (is.null(y))
    return(NULL)
  
  datatable(y,class = 'cell-border stripe',options = list(pageLength = 150,searching = FALSE,paging = T,audowidth=T,dom = "tip",
                                                          scrollX=TRUE, scrollY =1200,
                                                          scroller = T ),colnames = c("Vessel","Date","Sea State","Draft","Speed","RPM","Slip","Power","FO","SFOC","Load","Predicted Power","Predicted FO","Power Error","FO Error"),rownames = FALSE)%>%formatPercentage("Power_Error",1)%>%formatPercentage("FO_Error",1)%>%
    formatStyle(names(y),backgroundColor = "#ECF0F1",color="#000")
})

output$ADAPowerplot=renderPlotly({
  dataf=ADAPP()
  if (is.null(dataf))
    return(NULL)
  
  p = plot_ly(data =ADAPP(), x = ~SPEED, y = ~POWER,type='scatter', mode = "markers",marker=list(color="green",size=12),name="Observed Power")%>%
    add_trace(data= ADAPP(),x = ~SPEED,y = ~Pred_POWER,mode= "markers",marker=list(color="#BF1BE7",size=12),name="Predicted Power") %>%
  layout(title="Power Vs Speed",titlefont=c,legend = l,
                                                                     xaxis = list(title = " Speed (knots)", titlefont =f,tickfont = f, gridcolor = "#FFFFFF",tickangle=0),
                                                                     yaxis = list(title= "Power (kW)",titlefont = f, tickfont = f,gridcolor = "#ABB2B9"), 
                                                                     plot_bgcolor = "#FFFFFF",
                                                                     paper_bgcolor = "#FFFFFF",showlegend = TRUE
                                                                     
  )
})
output$ADAFOplot=renderPlotly({
  dataf=ADAPP()
  if (is.null(dataf))
    return(NULL)
  
  p=plot_ly(data =ADAPP(), x = ~SPEED, y = ~FO, type='scatter',mode = "markers",marker=list(color="green",size=12),name="Observed FO")%>%
    add_trace(data= ADAPP(),x = ~SPEED,y = ~Pred_FO,mode= "markers",marker=list(color="#BF1BE7",size=12),name="Predicted FO")%>%
  layout(title="FO Vs SPEED",titlefont =c,legend = l,
                                                                   xaxis = list(title = " SPEED (knots)", titlefont =f, tickfont =f,gridcolor = "#FFFFFF",tickangle=0),
                                                                   yaxis = list(title= "FO (tonne)",titlefont = f, tickfont = f,gridcolor = "#ABB2B9"), 
                                                                   plot_bgcolor = "#FFFFFF",
                                                                   paper_bgcolor = "#FFFFFF",showlegend = TRUE
                                                                   
  )
})
output$ADAcompareShopplot=renderPlotly({
  y=shopdataxx()
  m = ADAPP()
  mcr=mcr()
  if (is.null(y))
    return(NULL)
  
  x1=y[,1]
  y1= y[,3]
  
  #x1 = c(1,2,3,4,5)
  #y1 = c(1,4,9,16,25)
  
  fit2aa <- lm(y1 ~ poly(x1, 2, raw=TRUE))
  n1=as.numeric(fit2aa$coefficients[3])
  n2=as.numeric(fit2aa$coefficients[2])
  k=as.numeric(fit2aa$coefficients[1])
  
  power = m$POWER
  SFOC = m$SFOC
  powratio = power*100/mcr
  thSFOC = round(powratio^2*n1+powratio*n2+k,2)
  SFOCratio = SFOC/thSFOC
  avg = mean(SFOCratio)
  
  testx = seq(min(x1,na.rm = T),max(x1,na.rm = T),length.out = 30)
  testy = testx^2*n1+testx*n2+k
  testy1 = testy*avg
  
  
  r=seq(150,300,by=10)
  p= plot_ly(x= testx, y= testy,name="Shop Trial SFOC",type='scatter',mode="lines+markers",line = list(shape = "spline",color="#15972B"), marker=list(color="#15972B",opacity=0))%>%
    add_trace(x = testx,y = testy1,name = "Actual SFOC",type='scatter',mode="lines+markers",line = list(shape = "spline",color="#BF1BE7"),marker=list(color="#BF1BE7",opacity=0))%>%
    
  layout(title="Load(%) Vs SFOC",titlefont=c,
         xaxis = list(title = "Load(%)", titlefont =f, tickfont =f,gridcolor = "#FFFFFF"),
         yaxis = list(title = "SFOC(g/kW-Hr)",   titlefont = f, tickfont = f,gridcolor = "#ABB2B9",range=c(150,250)), 
         plot_bgcolor = "#FFFFFF",
         paper_bgcolor = "#FFFFFF",legend = l,showlegend=T
  )
  
})
#Performances Sheet
#Graph Axis....................

Powerdata<-reactive({
  vpq=vesselpart()
  Pcoeff =coeffdata()
  minD=unique(as.numeric(as.character(vpq[,4])), incomparables = FALSE)
  maxD = unique(as.numeric(vpq[,5]), incomparables = FALSE)
  maxs= unique(as.numeric(vpq[,6]), incomparables = FALSE)
  speed <- seq(10, maxs, by=1) 
  draft <- c(minD:maxD)
  ss<-as.numeric(input$seastate)
  m= matrix(NA,length(speed),length(draft))
  colnames(m)=draft
  row.names(m)=speed
  for(i in 1:length(draft) )
  {
    
    m[,i]=round(draft[i]^as.numeric(Pcoeff$n1)*speed^as.numeric(Pcoeff$n2)*(10-ss)^as.numeric(Pcoeff$n3)*exp(as.numeric(Pcoeff$k)),0)
  }
  
  m=as.data.frame(m)    
  
})

speed_powerplot= function(){
 }
output$power_cal =renderDataTable({
  y=Powerdata()
  if(is.null(y)){return()}
  datatable(y,class = 'cell-border stripe', options = list(autoWidth = TRUE,searching = FALSE,paging = FALSE),colnames = c('Speed/Draft' = 1))%>%
    formatStyle(names(y),color="#212F3D",backgroundColor = "#ECF0F1")%>%formatStyle('Speed/Draft',  color = '#ECF0F1', backgroundColor = '#515A5A', fontWeight = 'bold')
})

output$speedpower=renderPlotly({
  vpq=vesselpart()
  minD=unique(as.numeric(as.character(vpq[,4])), incomparables = FALSE)
  maxD = unique(as.numeric(vpq[,5]), incomparables = FALSE)
  maxs= unique(as.numeric(vpq[,6]), incomparables = FALSE)
  speed <- seq(10, maxs, by=1)
  draft <- c(minD:maxD)
  mydata=Powerdata()
  if(is.null(mydata)){return()}
  c=length(draft)
  p=plot_ly(x=rownames(mydata),y=mydata[,1],name=draft[1],type='scatter',mode="lines+markers",line = list(shape = "linear"))
  for(i in 2: length(draft)){
    p = p%>%add_trace(x=rownames(mydata),y=mydata[,i],name=draft[i],type='scatter',mode="lines+markers",line = list(shape = "linear"))
    
    
    
  } 
  p = p%>%layout(title = "Speed Power Curve",titlefont=c,xaxis=Speed_axis,yaxis=Power_axis,
         legend = l, plot_bgcolor = "#FFFFFF", paper_bgcolor = "#FFFFFF",showlegend = TRUE
           
    )
})



#FO CALCULATOR IN ANLAYSIS  PERFORMANCE.............
fodata<-reactive({
  vpq=vesselpart()
  Focoeff = FOcoeffdata()
  minD=unique(as.numeric(as.character(vpq[,4])), incomparables = FALSE)
  maxD = unique(as.numeric(vpq[,5]), incomparables = FALSE)
  maxs= unique(as.numeric(vpq[,6]), incomparables = FALSE)
  speed <- seq(10, maxs, by=1)
  draft <- c(minD:maxD)
  ss<-as.numeric(input$seastate)
  m= matrix(NA,length(speed),length(draft))
  colnames(m)=draft
  row.names(m)=speed
  for(i in 1:length(draft) )
  {
    
    m[,i]=round(draft[i]^as.numeric(Focoeff$n1)*speed^as.numeric(Focoeff$n2)*(10-ss)^as.numeric(Focoeff$n3)*exp(as.numeric(Focoeff$k)),1)
  }
  m= as.data.frame(m)    
  
})


output$FO_cal <- renderDataTable({
  x=fodata()
  if(is.null(x)){return()}
  datatable(x,class = 'cell-border stripe', options = list(autoWidth = TRUE,searching = FALSE,paging = FALSE),colnames = c('Speed/Draft' = 1))%>%
    formatStyle(names(x),color="#212F3D",backgroundColor = "#ECF0F1")%>%formatStyle('Speed/Draft',  color = '#ECF0F1', backgroundColor = '#515A5A', fontWeight = 'bold')
})

output$speedFO=renderPlotly({
  vpq=vesselpart()
  if(is.null(vpq)){return()}
  minD=unique(as.numeric(as.character(vpq[,4])), incomparables = FALSE)
  maxD = unique(as.numeric(vpq[,5]), incomparables = FALSE)
  maxs= unique(as.numeric(vpq[,6]), incomparables = FALSE)
  speed <- speed <- seq(10, maxs, by=1)
  draft <- c(minD:maxD)
  mydata=fodata()
  c=length(draft)
  p=plot_ly(x=rownames(mydata),y=mydata[,1],name=draft[1],type='scatter',mode="lines+markers",line = list(shape = "linear"))
  for(i in 2: length(draft)){
    p=p%>%add_trace(x=rownames(mydata),y=mydata[,i],name=draft[i],type='scatter',mode="lines+markers",line = list(shape = "linear"))
    
    
  }
  sp=p%>%layout(title = "Speed FO/24Hrs Curve",titlefont=c,xaxis=Speed_axis,yaxis=FO_axis,showlegend=TRUE,legend = l,
                                                                                                                    plot_bgcolor = "#FFFFFF",
                                                                                                                    paper_bgcolor = "#FFFFFF"
                                                                                                                    
  )
})
#FO SEASTATE PERFORMANCE CURVE ..............................................
#FO CALCULATOR IN ANLAYSIS  PERFORMANCE.............

SSfodata<-reactive({
  vpq=vesselpart()
  Focoeff = FOcoeffdata()
  maxs= unique(as.numeric(vpq[,6]), incomparables = FALSE)
  speed <- seq(10, maxs, by=1)
  seastate <- c(0:8)
  Draft<-as.numeric(input$Draftss)
  m= matrix(NA,length(speed),length(seastate))
  colnames(m)=seastate
  row.names(m)=speed
  for(i in 1:length(seastate) )
  {
    
    m[,i]=round(Draft^as.numeric(Focoeff$n1)*speed^as.numeric(Focoeff$n2)*(10-seastate[i])^as.numeric(Focoeff$n3)*exp(as.numeric(Focoeff$k)),1)
  }
  m= as.data.frame(m)    
  
})

output$SSFO_cal <- renderDataTable({
  x=SSfodata()
  if(is.null(x)){return()}
  datatable(x,class = 'cell-border stripe', options = list(autoWidth = TRUE,searching = FALSE,paging = FALSE),colnames = c('Speed/Sea State' = 1))%>%
    formatStyle(names(x),color="#212F3D",backgroundColor = "#ECF0F1")%>%formatStyle('Speed/Sea State',  color = '#ECF0F1', backgroundColor = '#515A5A', fontWeight = 'bold')
})

output$SSFO=renderPlotly({
  vpq=vesselpart()
  maxs= unique(as.numeric(vpq[,6]), incomparables = FALSE)
  speed <- seq(10, maxs, by=1)
  draft <- c(0:8)
  mydata=SSfodata()
  c=length(draft)
  p=plot_ly(x=rownames(mydata),y=mydata[,1],name=draft[1],type='scatter',mode="lines+markers",line = list(shape = "linear"))
  for(i in 2: length(draft)){
    p=p%>%add_trace(x=rownames(mydata),y=mydata[,i],name=draft[i],type='scatter',mode="lines+markers",line = list(shape = "linear"))
  }
  sp=p%>%layout(title = "Speed FO/24Hrs Curve For Sea State",titlefont=c,xaxis=Speed_axis,yaxis=FO_axis,showlegend=TRUE)%>%layout(legend = l,
                                                                                                                                 plot_bgcolor = "#FFFFFF",
                                                                                                                                 paper_bgcolor = "#FFFFFF",showlegend = TRUE
                                                                                                                                 
  )
})


#ADA PERFORMANCE CURVE.....................................................


ADAPowerdata<-reactive({
  vpq=vesselpart()
  Pcoeff = ADAcoeffdata()
  minD=unique(as.numeric(as.character(vpq[,4])), incomparables = FALSE)
  maxD = unique(as.numeric(vpq[,5]), incomparables = FALSE)
  maxs= unique(as.numeric(vpq[,6]), incomparables = FALSE)
  speed <- seq(10, maxs, by=1)
  draft <- c(minD:maxD)
  ss<-as.numeric(input$ADAseastate)
  m= matrix(NA,length(speed),length(draft))
  colnames(m)=draft
  row.names(m)=speed
  for(i in 1:length(draft) )
  {
    
    m[,i]=round(draft[i]^as.numeric(Pcoeff$n1)*speed^as.numeric(Pcoeff$n2)*(10-ss)^as.numeric(Pcoeff$n3)*exp(as.numeric(Pcoeff$k)),0)
  }
  
  m=as.data.frame(m)    
  
})

ADAspeed_powerplot= function(){
  
  vpq=vesselpart()
  Pcoeff = ADAcoeffdata()
  if(is.null(vpq)){return()}
  minD=unique(as.numeric(as.character(vpq[,4])), incomparables = FALSE)
  maxD = unique(as.numeric(vpq[,5]), incomparables = FALSE)
  maxs= unique(as.numeric(vpq[,6]), incomparables = FALSE)
  speed <- seq(10, maxs, by=1)
  draft <- c(minD:maxD)
  mydata=ADAPowerdata()
  c=length(draft)
  p=plot_ly(x=rownames(mydata),y=mydata[,1],name=draft[1],type='scatter',mode="lines+markers",line = list(shape = "linear"))
  for(i in 2: length(draft)){
    p=p%>%add_trace(x=rownames(mydata),y=mydata[,i],name=draft[i],type='scatter',mode="lines+markers",line = list(shape = "linear"))
    
    
    
  } }
output$ADApower_cal =renderDataTable({
  y=ADAPowerdata()
  datatable(y, class = 'cell-border stripe',options = list(autoWidth = TRUE,searching = FALSE,paging = FALSE),colnames = c('Speed/Draft' = 1))%>%
    formatStyle(names(y),color="#212F3D",backgroundColor = "#ECF0F1")%>%formatStyle('Speed/Draft',  color = '#ECF0F1', backgroundColor = '#515A5A', fontWeight = 'bold')
})

output$ADAspeedpower=renderPlotly({
  vpq=vesselpart()
  
  if(is.null(vpq)){return()}
  minD=unique(as.numeric(as.character(vpq[,4])), incomparables = FALSE)
  maxD = unique(as.numeric(vpq[,5]), incomparables = FALSE)
  maxs= unique(as.numeric(vpq[,6]), incomparables = FALSE)
  speed <- seq(10, maxs, by=1)
  draft <- c(minD:maxD)
  mydata=ADAPowerdata()
  c=length(draft)
  p=plot_ly(x=rownames(mydata),y=mydata[,1],name=draft[1],type='scatter',mode="lines+markers",line = list(shape = "linear"))
  for(i in 2: length(draft)){
    p=p%>%add_trace(x=rownames(mydata),y=mydata[,i],name=draft[i],type='scatter',mode="lines+markers",line = list(shape = "linear"))
    
    
    
  } 
  sp=p%>%layout(title = "Speed Power Curve",titlefont=c,xaxis=Speed_axis,yaxis=Power_axis,showlegend=TRUE,legend = l,
                plot_bgcolor = "#FFFFFF",
                paper_bgcolor = "#FFFFFF",showlegend = TRUE
                
                
  )
})



#FO CALCULATOR IN ANLAYSIS  PERFORMANCE..............
ADAfodata<-reactive({
  vpq=vesselpart()
  Focoeff = ADAFOcoeffdata()
  minD=unique(as.numeric(as.character(vpq[,4])), incomparables = FALSE)
  maxD = unique(as.numeric(vpq[,5]), incomparables = FALSE)
  maxs= unique(as.numeric(vpq[,6]), incomparables = FALSE)
  speed <- seq(10, maxs, by=1)
  draft <- c(minD:maxD)
  ss<-as.numeric(input$ADAseastate)
  m= matrix(NA,length(speed),length(draft))
  colnames(m)=draft
  row.names(m)=speed
  for(i in 1:length(draft) )
  {
    
    m[,i]=round(draft[i]^as.numeric(Focoeff$n1)*speed^as.numeric(Focoeff$n2)*(10-ss)^as.numeric(Focoeff$n3)*exp(as.numeric(Focoeff$k)),1)
  }
  m= as.data.frame(m)    
  
})


output$ADAFO_cal <- renderDataTable({
  x=ADAfodata()
  datatable(x,class = 'cell-border stripe', options = list(autoWidth = TRUE,searching = FALSE,paging = FALSE),colnames = c('Speed/Draft' = 1))%>%
    formatStyle(names(x),color="#212F3D",backgroundColor = "#ECF0F1")%>%formatStyle('Speed/Draft',  color = '#ECF0F1', backgroundColor = '#515A5A', fontWeight = 'bold')
})

output$ADAspeedFO=renderPlotly({
  vpq=vesselpart()
  if(is.null(vpq)){return()}
  minD=unique(as.numeric(as.character(vpq[,4])), incomparables = FALSE)
  maxD = unique(as.numeric(vpq[,5]), incomparables = FALSE)
  maxs= unique(as.numeric(vpq[,6]), incomparables = FALSE)
  speed <- seq(10, maxs, by=1)
  draft <- c(minD:maxD)
  mydata=ADAfodata()
  c=length(draft)
  p=plot_ly(x=rownames(mydata),y=mydata[,1],name=draft[1],type='scatter',mode="lines+markers",line = list(shape = "linear"))
  for(i in 2: length(draft)){
    p=p%>%add_trace(x=rownames(mydata),y=mydata[,i],name=draft[i],type='scatter',mode="lines+markers",line = list(shape = "linear"))
    
  } 
  sp=p%>%layout(title = "Speed FO/24Hrs Curve",titlefont=c,xaxis=Speed_axis,yaxis=FO_axis,showlegend=TRUE)%>%layout(legend = l,
                                                                                                                    plot_bgcolor = "#FFFFFF",
                                                                                                                    paper_bgcolor = "#FFFFFF",showlegend = TRUE
                                                                                                                    
  )
})


#SAVE ANALYSIS ........................................................

coeff=reactive({
  xfile= data.frame(read.csv("data/Coefficient.csv"))
  vpq=vesselpart()
  vpq=subset(vpq,Vessel==input$filtVessel)
  fleet=unique(as.numeric(vpq[,1]), incomparables = FALSE)
  class = fleet
  row.names(xfile) <- NULL
  values <- reactiveValues()
  values$df <- xfile
  x1=format(as.Date(input$dates[1]),"%d %b %Y")
  x2=format(as.Date(input$dates[2]),"%d %b %Y")
  daterange= paste("From","",x1 ,"","To" ,"",x2)
  Type=as.character("Noon")
  P=coeffdata()
  FO=FOcoeffdata()
  newLine <- c(fleet,class,input$filtVessel,daterange,P$n1,P$n2,P$n3,P$k,FO$n2,FO$n3,FO$k,x1,x2,Type)
  values$df <- rbind(as.matrix(values$df), unlist(newLine))
  values$df
})

observeEvent(input$SAVE, {
  y=coeff()
  save_data <<- y
  write.csv(
    x = y,
    file = "data/Coefficient.csv" , 
    row.names = FALSE, quote = TRUE
  )
  
})
#ADA ANALYSIS SAVE ...............................

ADAcoeff=reactive({
  xfile= data.frame(read.csv("data/Coefficient.csv"))
  vpq=vesselpart()
  vpq=subset(vpq,Vessel==input$filtVessel)
  fleet=unique(as.numeric(vpq[,1]), incomparables = FALSE)
  class = fleet
  row.names(xfile) <- NULL
  values <- reactiveValues()
  values$df <- xfile
  x1=format(as.Date(input$ADAdates[1]),"%d %b %Y")
  x2=format(as.Date(input$ADAdates[2]),"%d %b %Y")
  daterange= paste("From","",x1 ,"","To" ,"",x2)
  Type=as.character("ADA")
  P=ADAcoeffdata()
  FO=ADAFOcoeffdata()
  newLine <- c(fleet,class,input$filtVessel,daterange,P$n1,P$n2,P$n3,P$k,FO$n2,FO$n3,FO$k,x1,x2,Type)
  values$df <- rbind(as.matrix(values$df), unlist(newLine))
  values$df
})

observeEvent(input$ADASAVE, {
  y=ADAcoeff()
  save_data <<- y
  write.csv(
    x = y,
    file = "data/Coefficient.csv" , 
    row.names = FALSE, quote = TRUE
  )
  
})