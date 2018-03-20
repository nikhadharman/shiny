
####EEOI/MRv
Eeoidata= data.frame(read.csv("data/EEOI Data1.csv"))





Mdata=reactive({
  
  if(input$emtype=="All Voyages"){
    y=Eeoidata 
    y=subset(y,y$Vessel.Name == input$Vname )
    y=subset(y,as.Date(as.character(y$Corrected.Date),"%d-%m-%y")>=as.Date(input$edate[1],"%y-%m-%d")& as.Date(as.character(y$Corrected.Date),"%d-%m-%y")<=as.Date(input$edate[2],"%y-%m-%d"))
  }else {
    y=Eeoidata 
    y=subset(y,y$Vessel.Name == input$Vname )
    y=subset(y,y$EU=="Y")
    y=subset(y,as.Date(as.character(y$Corrected.Date),"%d-%m-%y")>=as.Date(input$edate[1],"%y-%m-%d")& as.Date(as.character(y$Corrected.Date),"%d-%m-%y")<=as.Date(input$edate[2],"%y-%m-%d"))
  }
  y
})

output$EMVoyageno <- renderUI({ 
  r = Mdata()
  s=input$Vname 
  r=subset(r,Vessel.Name==s)
  Vessel_List = unique(sort(as.numeric(as.character(r$VoyageNo)),decreasing = T), incomparables = FALSE)
  selectInput("EMvoyageno", label=h4(strong("Passage ID")), choices = Vessel_List, selected = NULL, multiple = FALSE, selectize = TRUE, width = "50%", size = NULL)
})

output$pno <- renderUI({ 
  r = Mdata()
  s=input$Vname 
  r=subset(r,Vessel.Name==s)
  r=subset(r,as.numeric(as.character(r$VoyageNo)) == input$EMvoyageno)
  Vessel_List = unique(r$Depart_Arrival, incomparables = FALSE)
  selectInput("passage", label=h4(strong("Voyage Code")), choices = Vessel_List, selected = NULL, multiple = FALSE, selectize = TRUE, width = "50%", size = NULL)
})



#VOYAGE REPORT.............................................................................................
ROBFUEL <- reactive({
  y = Mdata()
  len = length(y[,6])
  y$ROBFUELHS = rep(0,len)
  y$ROBFUELLS = rep(0,len)
  y$ROBFUELMDO = rep(0,len)
  y$ROBFUELMGO = rep(0,len)
  y$ROBFUELMGOLS = rep(0,len)
  
  #y$ROBFUELHS[len] = y[1,42] - y[len,42] + sum(y[,37])
  #y$ROBFUELLS[len] = y[1,43] - y[len,43] + sum(y[,38])
  #y$ROBFUELMDO[len] = y[1,44] - y[len,44] + sum(y[,39])
  #y$ROBFUELMGO[len] = y[1,45] - y[len,45] + sum(y[,40])
  #y$ROBFUELMGOLS[len] = y[1,46] - y[len,46] + sum(y[,41])
  for(i in 2 : len){
    y$ROBFUELHS[i] = y[i-1,42] - y[i,42] + y[i,37] 
    y$ROBFUELLS[i] = y[i-1,43] - y[i,43] + y[i,38]
    y$ROBFUELMDO[i] = y[i-1,44] - y[i,44] + y[i,39]
    y$ROBFUELMGO[i] = y[i-1,45] - y[i,45] + y[i,40]
    y$ROBFUELMGOLS[i] = y[i-1,46] - y[i,46] + y[i,41]
  }
  y
})

SUMEE1=reactive({
  
  if(input$method == "Bunker Method"){
    y=ROBFUEL()
    y = subset(y,VoyageNo == input$EMvoyageno)
    y = subset(y,Depart_Arrival == input$passage)
    m=matrix(0,nrow=7,ncol=5,byrow = TRUE)
    colnames(m)=c("Fuel(HS)",	"Fuel (LS)","Fuel (MDO)","Fuel (MGO)","Fuel (MGO LS)")
    rownames(m)=c("Emission factor (Ton-CO2/Ton-Fuel)",
                  "Sulphur content[%]",
                  "Total Fuel (Ton)",
                  "Fuel at Berth (Ton)",
                  "Fuel at Sea (Ton)",
                  "Total CO2 (Ton)",
                  "Total SOx (Ton)")
    m[1,]=c(3.114,3.114,3.206,3.206,3.206)
    m[2,]=c(4.5,3.5,1.5,1.5,1.5)
    m[3,]=c(sum(as.numeric(as.character(y$ROBFUELHS))),sum(as.numeric(as.character(y$ROBFUELLS))),sum(as.numeric(as.character(y$ROBFUELMDO))),sum(as.numeric(as.character(y$ROBFUELMGO))),sum(as.numeric(as.character(y$ROBFUELMGOLS))))
    sea=subset(y,Status=="At Sea")
    m[5,]=c(sum(as.numeric(as.character(sea$ROBFUELHS))),sum(as.numeric(as.character(sea$ROBFUELLS))),sum(as.numeric(as.character(sea$ROBFUELMDO))),sum(as.numeric(as.character(sea$ROBFUELMGO))),sum(as.numeric(as.character(sea$ROBFUELMGOLS))))
    port=subset(y,Status=="In Port")
    m[4,]=c(sum(as.numeric(as.character(port$ROBFUELHS))),sum(as.numeric(as.character(port$ROBFUELLS))),sum(as.numeric(as.character(port$ROBFUELMDO))),sum(as.numeric(as.character(port$ROBFUELMGO))),sum(as.numeric(as.character(port$ROBFUELMGOLS))))
    m[6,]=c(m[1,]*m[3,])
    m[7,]=c(m[2,]*m[3,]*20/1000)
    m[3,]=round(m[3,],2)
    m[5,]=round(m[5,],2)
    m[4,]=round(m[4,],2)
    m[6,]=round(m[6,],2)
    m[7,]=round(m[7,],2)
    m 
  } else {
  y=Mdata()
  y=subset(y,VoyageNo == input$EMvoyageno)
  y=subset(y,Depart_Arrival == input$passage)
  
    m=matrix(0,nrow=7,ncol=5,byrow = TRUE)
    colnames(m)=c("Fuel(HS)",	"Fuel (LS)","Fuel (MDO)","Fuel (MGO)","Fuel (MGO LS)")
    rownames(m)=c("Emission factor (Ton-CO2/Ton-Fuel)",
                  "Sulphur content[%]",
                  "Total Fuel (Ton)",
                  "Fuel at Berth (Ton)",
                  "Fuel at Sea (Ton)",
                  "Total CO2 (Ton)",
                  "Total SOx (Ton)")
    m[1,]=c(3.1144,3.151,3.206,3.206,3.206)
    m[2,]=c(4.5,3.5,1.5,1.5,1.5)
    m[3,]=c(sum(as.numeric(as.character(y$FUEL.HS))),sum(as.numeric(as.character(y$FUEL.LS))),sum(as.numeric(as.character(y$FUEL.MDO))),sum(as.numeric(as.character(y$FUEL.MGO.HS))),sum(as.numeric(as.character(y$FUEL.MGO.LS))))
    sea=subset(y,Status=="At Sea")
    m[5,]=c(sum(as.numeric(as.character(sea$FUEL.HS))),sum(as.numeric(as.character(sea$FUEL.LS))),sum(as.numeric(as.character(sea$FUEL.MDO))),sum(as.numeric(as.character(sea$FUEL.MGO.HS))),sum(as.numeric(as.character(sea$FUEL.MGO.LS))))
    port=subset(y,Status=="In Port")
    m[4,]=c(sum(as.numeric(as.character(port$FUEL.HS))),sum(as.numeric(as.character(port$FUEL.LS))),sum(as.numeric(as.character(port$FUEL.MDO))),sum(as.numeric(as.character(port$FUEL.MGO.HS))),sum(as.numeric(as.character(port$FUEL.MGO.LS))))
    m[6,]=c(m[1,]*m[3,])
    m[7,]=c(m[2,]*m[3,]*20/1000)
    m[3,]=round(m[3,],2)
    m[5,]=round(m[5,],2)
    m[4,]=round(m[4,],2)
    m[6,]=round(m[6,],2)
    m[7,]=round(m[7,],2)
    
    m 
  }
})

output$SEE1Table=DT::renderDataTable({
  m=data.frame(SUMEE1())
  colnames(m) = c("Fuel(HS)",	"Fuel (LS)","Fuel (MDO)","Fuel (MGO)","Fuel MGO LS")
  datatable(m,options = list(pageLength = 15,searching = FALSE,paging = FALSE),rownames = T,colnames=c("TITLE"=1),class = "display")%>%formatStyle(names(m),backgroundColor = "white",color="#000")%>%formatStyle("TITLE",  color = '#FFF', backgroundColor = '#515A5A', fontWeight = 'bold')
  
})

SUMEE2=reactive({
  y=Mdata()
  m=SUMEE1()
  y=subset(y,VoyageNo == input$EMvoyageno)
  y=subset(y,Depart_Arrival == input$passage)
  
  if(input$Vtype=="Container"){
  n=matrix(0,nrow=17,ncol=1,byrow = TRUE)
  rownames(n)=c("Total Fuel Consumption",
                "Total Fuel Consumption at Sea(Ton)",
                "Total Fuel Consumption at Berth(Ton)",
                "Total CO2 Emission (Ton)",
                "CO2 Emissions at sea (Ton)",
                "CO2 Emission at Berth (Ton)",
                'Total Distance Sailed (Nm)',
                "Total time at sea (days)",
                "Total Cargo (Ton)",
                "Total Cargo (TEU)",
                "Total Transport Work (Ton-Nm)",
                "Total Transport Work (TEU-Nm)",
                "Fuel consumption per distance (Ton/Nm)",
                "Fuel consumption per tranport work (freight transport) (g/Ton*Nm)",
                "CO2 Emission per Distance (g/Nm)",
                "CO2 Emission per tranport work (g/Ton*Nm)",
                "CO2 Emission per tranport work (g/TEU*Nm)")					
  n[1]=round(sum(as.numeric(m[3,])),2)
  n[2]=round(sum(as.numeric(m[5,])),2)
  n[3]=round(sum(as.numeric(m[4,])),2)
  n[4]=round(sum(as.numeric(m[6,])),2)
  n[5]=round(sum(as.numeric((m[5,]*m[1]))),2)
  n[6]=round(sum(as.numeric((m[4,]*m[1]))),2)
  n[7]=round(sum(as.numeric(y$MILES.BY.GPS)),2)
  t = subset(y,Status == "At Sea")
  cargo = subset(t,Report.Type == "Noon at sea" || Report.Type == "EOSP" )
  tcargo = unique(as.numeric(as.character(cargo$CARGO.TOTAL)), incomparables = FALSE)
  teucargo = unique(as.numeric(as.character(cargo$CARGO.TOTAL.TEU)), incomparables = FALSE)
  n[8]=round(sum(as.numeric(t$TOTAL.STEAMING.TIME/24)),2)
  n[9]=round(sum(as.numeric(tcargo)),2)
  n[10]=round(sum(as.numeric(teucargo)),2)
  n[11]=round((n[9]*n[7]),2)
  n[12]=round((n[10]*n[7]),2)
  n[13]=round((n[2]/n[7]),2)
  n[14]=round((n[2]*10^6/n[11]),2)
  n[15]=round((n[4]*10^6/n[7]),2)
  n[16]=round((n[4]*10^6/n[11]),2)
  n[17]=round((n[4]*10^6/n[12]),2)
  n
  }
  else{
    n=matrix(0,nrow=14,ncol=1,byrow = TRUE)
    rownames(n)=c("Total Fuel Consumption",
                  "Total Fuel Consumption at Sea(Ton)",
                  "Total Fuel Consumption at Berth(Ton)",
                  "Total CO2 Emission (Ton)",
                  "CO2 Emissions at sea (Ton)",
                  "CO2 Emission at Berth (Ton)",
                  'Total Distance Sailed (Nm)',
                  "Total time at sea (days)",
                  "Total Cargo (Ton)",
                  "Total Transport Work (Ton-Nm)",				
                  "Fuel consumption per distance (Ton/Nm)",
                  "Fuel consumption per tranport work (freight transport) (g/Ton*Nm)",
                  "CO2 Emission per Distance (g/Nm)",
                  "CO2 Emission per tranport work (g/Ton*Nm)")					
    n[1]=round(sum(as.numeric(m[3,])),2)
    n[2]=round(sum(as.numeric(m[5,])),2)
    n[3]=round(sum(as.numeric(m[4,])),2)
    n[4]=round(sum(as.numeric(m[6,])),2)
    n[5]=round(sum(as.numeric((m[5,]*m[1]))),2)
    n[6]=round(sum(as.numeric((m[4,]*m[1]))),2)
    n[7]=round(sum(as.numeric(y$MILES.BY.GPS)),2)
    t = subset(y,Status == "At Sea")
    cargo = subset(t,Status == "At Sea")
    tcargo = unique(as.numeric(as.character(cargo$CARGO.TOTAL)), incomparables = FALSE)
    n[8]=round(sum(as.numeric(t$TOTAL.STEAMING.TIME/24)),2)
    n[9]=round(sum(as.numeric(tcargo)),2)
    n[10]=round((n[9]*n[7]),2)
    n[11]=round((n[2]/n[7]),2)
    n[12]=round((n[2]*10^6/n[10]),2)
    n[13]=round((n[4]*10^6/n[7]),2)
    n[14]=round((n[4]*10^6/n[10]),2)
    n
  }
})



output$SEE2Table=DT::renderDataTable({
  n=data.frame(SUMEE2())
  colnames(n)=c("Value")
  datatable(n, options = list(pageLength = 20,searching = FALSE,paging = FALSE,audowidth=T),
            rownames = T,colnames=c("Parameter"=1),class = 'display')%>%formatStyle(names(n),
                                                                                    backgroundColor = "white",color="#000")%>%
    formatStyle("Parameter",  color = '#FFF', backgroundColor = '#515A5A', fontWeight = 'bold')
  
  
})


output$EEOIBox <- renderValueBox({
  x=SUMEE2()
  if(input$Vtype=="Container"){
  if(is.na(x[16])){return(NULL)}
  y = as.character(x[16])
  if(y == "Inf" ){
    subtitle = " No Cargo Available  "
    x= 0 
    color = "red"
    icon = icon("")
  }
  else {
    x= x[16]
    subtitle = strong("EEOI (g/Ton*Nmi)")
    icon = icon("percent")
    color = "green"
  }}
  else{
  if(is.na(x[14])){return(NULL)}
    y = as.character(x[14])
    if(y == "Inf" ){
    subtitle = " No Cargo Available  "
    x= 0 
    color = "red"
    icon = icon("")
  }
  else {
    x= x[14]
    subtitle = strong("EEOI (g/Ton*Nmi)")
    icon = icon("percent")
    color = "green"
  }
  }
  valueBox(
    x, subtitle, icon = icon,
    color = color
  )
})

output$EEOIBox2 <- renderValueBox({
  x=SUMEE2()
  if(is.na(x[17])){return(NULL)}
  y = as.character(x[17])
  if(y == "Inf" ){
    subtitle = " No Cargo Available  "
    x= 0 
    color = "red"
    icon = icon("")
  }
  else {
    x= x[17]
    subtitle = strong("EEOI (g/TEU*Nmi)")
    icon = icon("percent")
    color = "green"
  }
  valueBox(
    x, subtitle, icon = icon,
    color = color
  )
})

#output$EEOIBox <- renderValueBox({
#  x=SUMEE2()
#  if(is.na(x[15])){return(NULL)}
#  y = as.character(x[15])
#  if(y == "Inf" ){
#    subtitle = " No Cargo Available  "
#    x= 0 
#    color = "red"
#    icon = icon("")
#  }
#  else {
#    x= x[15]
#    subtitle = strong("EEOI (g/Ton*Nmi)")
#    icon = icon("percent")
#    color = "green"
#  }
#  valueBox(
#    x, subtitle, icon = icon,
#    color = color
#  )
#})

output$FuelBox <- renderValueBox({
  x=SUMEE2()
  valueBox(
    x[1], strong("Total Fuel (Tonne)"), icon = icon("fire"),
    color = "yellow"
  )
})

# EEOI ...........

EEOIladen=reactive({
  y=Mdata()
  y=subset(y,VoyCondition=="LADEN")
  
  
})

#.......................

output$DataTable=DT::renderDataTable({
  y=Mdata()
  sortdf <- y[order(as.Date(y$Corrected.Date,format = "%d-%m-%y"),decreasing = T),]
  datatable(sortdf,class = 'cell-border stripe', options = list(pageLength = 150,searching = FALSE,paging = T,audowidth=T,dom = "tip",
                                                                scrollX=TRUE, scrollY = 600,
                                                                scroller = TRUE,
                                                                scrollCollapse=TRUE),rownames = FALSE)%>%formatStyle(names(y),backgroundColor = "white")
  
})

#.........................EEOI chart.............................

EECHARTDATA=reactive({
  y=Mdata()
  y=subset(y,VoyageNo == input$EMvoyageno)
  y=subset(y,Depart_Arrival == input$passage)
  y$CO2= as.numeric(as.character(y$TOTAL.CO2))
  y$FOC=as.numeric(as.character(y$TOTALFO))
  y$EEOI= (y$CO2*10^6)/(as.numeric(as.character(y$CARGO.TOTAL))* as.numeric(as.character(y$MILES.BY.GPS)))
  y[is.na(y)] <- 0
  y
  
})

EECHARTDATAT=reactive({
  y=Mdata()
  y=subset(y,VoyageNo == input$EMvoyageno)
  y=subset(y,Depart_Arrival == input$passage)
  y$CO2= as.numeric(as.character(y$TOTAL.CO2))
  y$FOC=as.numeric(as.character(y$TOTALFO))
  y$EEOI= (y$CO2*10^6)/(as.numeric(as.character(y$CARGO.TOTAL.TEU))* as.numeric(as.character(y$MILES.BY.GPS)))
  y$EEOIC= (y$CO2*10^6)/(as.numeric(as.character(y$CARGO.TOTAL))* as.numeric(as.character(y$MILES.BY.GPS)))
  y[is.na(y)] <- 0
  y
  
})

EECHARTDATA1=reactive({
  y=Mdata()
  y$CO2= as.numeric(as.character(y$TOTAL.CO2))
  ycargo = subset(y,Status == "At Sea")
  uc=ddply(ycargo, .(VoyageNo),summarize,cargo = sum(unique(as.numeric(as.character(CARGO.TOTAL)))),cargot = sum(unique(as.numeric(as.character(CARGO.TOTAL.TEU)))))
  #y$Transpwork = as.numeric(as.character(y$CARGO.TOTAL)) * as.numeric(as.character(y$MILES.BY.GPS))
  u=ddply(y, .(VoyageNo),summarize,co=sum(as.numeric(as.character(CO2))),mil = sum(as.numeric(as.character(MILES.BY.GPS))),fo = sum(as.numeric(as.character(TOTALFO))) )
  ux = merge(uc,u,by.x = "VoyageNo",by.y = "VoyageNo")
  ux$EEOI= (as.numeric(as.character(ux$co))*10^6)/(as.numeric(as.character(ux$mil))*as.numeric(as.character(ux$cargo)))
  ux$EEOIt= (as.numeric(as.character(ux$co))*10^6)/(as.numeric(as.character(ux$mil))*as.numeric(as.character(ux$cargot)))
  ux
  
})

output$EECHART = renderPlotly({
  validate(
    need(try(EECHARTDATA()),"Please Wait or Select the vessel")
  )
  u=EECHARTDATA()
  #u=ddply(y,.(Corrected.Date),summarize,ee=mean(EEOI))
  u =subset(u,EEOI != Inf)
  p <- plot_ly(
    x = as.Date(u$Corrected.Date,format="%d-%m-%y"),
    y =u$EEOI,
    name = "EEOI",
    type = "bar",marker=list(color="#00CC66"))
  p %>% layout(title="Energy efficiency (grams/tonne-mile)",titlefont=c,margin = list(b = 85),
               xaxis = list(title = "Date", titlefont =s, tickfont =f,gridcolor = "white",type = "category"),
               yaxis = list(title = "Energy Efficiency(gm/tonne-mile)",   titlefont = s, tickfont = f,gridcolor = "#ABB2B9"), 
               plot_bgcolor = "white",
               paper_bgcolor = "white")
  
})

output$EECHARTFO = renderPlotly({
  validate(
    need(try(EECHARTDATA()),"Please Wait or Select the vessel")
  )
  u=EECHARTDATA()
  y=ddply(u,.(Corrected.Date),summarize,fo=sum(FOC))
  #u =subset(u,ee != Inf)
  p <- plot_ly(
    x = as.Date(y$Corrected.Date,format="%d-%m-%y"),
    y =y$fo,
    name = "EEOI",
    type = "bar",marker=list(color="#990000"))
  p %>% layout(title="FO Consumption",titlefont=c,margin = list(b = 85),
               xaxis = list(ticks = "outside",
                            tickmode = "array",
                            tickvals = as.Date(y$Corrected.Date,format="%d-%m-%y"),
                            ticktext = labels,
                            tickformat = "%d-%b-%y",
                            tickangle = 0,
                           title = "Date",  tickfont =f,gridcolor = "white"),
               yaxis = list(title = "FO Tonnes",   titlefont = s, tickfont = f,gridcolor = "#ABB2B9"), 
               plot_bgcolor = "white",
               paper_bgcolor = "white")
  
})

output$EECHARTT = renderPlotly({
  validate(
    need(try(EECHARTDATAT()),"Please Wait or Select the vessel")
  )
  u=EECHARTDATAT()
  #u=ddply(y,.(Corrected.Date),summarize,ee=sum(EEOI))
  u =subset(u,EEOI != Inf)
  p <- plot_ly(
    x = as.Date(u$Corrected.Date,format="%d-%m-%y"),
    y =u$EEOI,
    name = "EEOI",
    type = "bar",marker=list(color="#238aff"))
  p %>% layout(title="Energy efficiency (grams/TEU-mile)",titlefont=c,margin = list(b = 85),
               xaxis = list(title = "Date", titlefont =s, tickfont =f,gridcolor = "white",type = "category"),
               yaxis = list(title = "Energy Efficiency(gm/TEU-mile)",   titlefont = s, tickfont = f,gridcolor = "#ABB2B9"), 
               plot_bgcolor = "white",
               paper_bgcolor = "white")
  
})

output$EECHART2 = renderPlotly({
  validate(
    need(try(EECHARTDATA()),"Please Wait or Select the vessel")
  )
  u=EECHARTDATA()
  y=ddply(u,.(Corrected.Date),summarize,ee=sum(CO2))
  p <- plot_ly(
    x = as.Date(y$Corrected.Date,format="%d-%m-%y"),
    y = y$ee,
    name = "EEOI",
    type = "bar",marker=list(color="#841b51"))
  p %>% layout(title="CO2 (tonnes)",titlefont=c,margin = list(b = 85),
               
               xaxis = list(ticks = "outside",
                            tickmode = "array",
                            tickvals = as.Date(y$Corrected.Date,format="%d-%m-%y"),
                            ticktext = labels,
                            tickformat = "%d-%b-%y",
                            tickangle = 0,
                            title = "Date", titlefont =s, tickfont =f,gridcolor = "white"),
               yaxis = list(title = "CO2 (tonnes)",   titlefont = s, tickfont = s,gridcolor = "#ABB2B9"), 
               plot_bgcolor = "white",
               paper_bgcolor = "white")
  
})

output$EECHART11 = renderPlotly({
  validate(
    need(try(EECHARTDATA1()),"Please Wait or Select the vessel")
  )
  y=EECHARTDATA1()
  u = ddply(y,.(VoyageNo),summarize,ee = mean(EEOI))
  p <- plot_ly(u,
               x = ~as.character(VoyageNo),
               y = ~ee,
               name = "EEOI",
               type = "bar",marker=list(color="#00CC66"))
  p %>% layout(title="Energy efficiency (grams/tonne-mile)",titlefont=c,margin = list(b = 85),
               xaxis = list(title = "Voyage No", titlefont =s, tickfont =f,gridcolor = "white",type = "category"),
               yaxis = list(title = "Energy Efficiency(gm/tonne-mile)",   titlefont = s, tickfont = s,gridcolor = "#ABB2B9"), 
               plot_bgcolor = "white",
               paper_bgcolor = "white")
  
})
#year wise /......................................
output$EECHART11T = renderPlotly({
  validate(
    need(try(EECHARTDATA1()),"Please Wait or Select the vessel")
  )
  y=EECHARTDATA1()
  u = ddply(y,.(VoyageNo),summarize,ee = mean(EEOIt))
  p <- plot_ly(u,
               x = ~as.character(VoyageNo),
               y = ~ee,
               name = "EEOI",
               type = "bar",marker=list(color="#238aff"))
  p %>% layout(title="Energy efficiency (grams/TEU-mile)",titlefont=c,margin = list(b = 85),
               xaxis = list(title = "Voyage No", titlefont =s, tickfont =f,gridcolor = "white",type = "category"),
               yaxis = list(title = "Energy Efficiency(gm/TEU-mile)",   titlefont = s, tickfont = s,gridcolor = "#ABB2B9"), 
               plot_bgcolor = "white",
               paper_bgcolor = "white")
  
})

output$EECHART22 = renderPlotly({
  validate(
    need(try(EECHARTDATA1()),"Please Wait or Select the vessel")
  )
  y=EECHARTDATA1()
  p <- plot_ly(y,
               x =~as.character(VoyageNo),
               y = ~co,
               name = "EEOI",
               type = "bar",marker=list(color="#841b51"))
  p %>% layout(title="CO2 (tonnes)",titlefont=c,margin = list(b = 85),
               
               xaxis = list(title = "Voyage No", titlefont =s, tickfont =f,gridcolor = "white",type = "category"),
               yaxis = list(title = "CO2 (tonnes)",   titlefont = s, tickfont = s,gridcolor = "#ABB2B9"), 
               plot_bgcolor = "white",
               paper_bgcolor = "white")
  
})
output$EECHART22FO = renderPlotly({
  validate(
    need(try(EECHARTDATA1()),"Please Wait or Select the vessel")
  )
  y=EECHARTDATA1()
  p <- plot_ly(y,
               x =~as.character(VoyageNo),
               y = ~fo,
               name = "EEOI",
               type = "bar",marker=list(color="#990000"))
  p %>% layout(title="FO (tonnes)",titlefont=c,margin = list(b = 85),
               
               xaxis = list(title = "Voyage No", titlefont =s, tickfont =f,gridcolor = "white",type = "category"),
               yaxis = list(title = "FO (tonnes)",   titlefont = s, tickfont = s,gridcolor = "#ABB2B9"), 
               plot_bgcolor = "white",
               paper_bgcolor = "white")
  
})

s1 <- list(
  family ="'Lato', sans-serif",
  size = 12,
  color = "black")
l1 <- list(
  font = s1,
  bgcolor = "#FFFFFF"
  
)

output$ladbal=renderPlotly({
  validate(
    need(try(Mdata(),input$EMvoyageno),"Please Wait or Select the vessel")
  )
  y=Mdata()
  y=subset(y,VoyageNo == input$EMvoyageno)
  y=y=subset(y,Depart_Arrival == input$passage)
  u=ddply(y, .(VoyCondition),summarize,x=length((VoyageNo)))
  v=as.numeric(sum(u$x))
  u$VoyCondition = paste(u$VoyCondition)
  plot_ly(u, labels = ~VoyCondition, values =~x, type = "pie", hole =0.5,pull=0.01,name = "Total Vessel", domain=list(x=c(0,1),y=c(0,1)),
          showlegend = F,textposition='outside',textinfo='label',outsidetextfont=s1,
          marker=list(colors= c("#97ebff","#7b6bad")) ,hoverinfo="none")%>%
    layout(margin=list(b=75),legend = l1,  plot_bgcolor = "white", paper_bgcolor = "white",
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
           
    )
})
#...........................Year Wise....................................................................................

allSUMEE1=reactive({
  y=Mdata()
  m=matrix(0,nrow=7,ncol=5,byrow = TRUE)
  colnames(m)=c("Fuel(HS)",	"Fuel (LS)","Fuel (MDO)","Fuel (MGO)","Fuel (MGO LS)")
  rownames(m)=c("Emission factor (Ton-CO2/Ton-Fuel)",
                "Sulphur content[%]",
                "Total Fuel (Ton)",
                "Fuel at Berth (Ton)",
                "Fuel at Sea (Ton)",
                "Total CO2 (Ton)",
                "Total SOx (Ton)")
  m[1,]=c(3.1144,3.151,3.206,3.206,3.206)
  m[2,]=c(4.5,3.5,1.5,1.5,1.5)
  m[3,]=c(sum(as.numeric(as.character(y$FUEL.HS))),sum(as.numeric(as.character(y$FUEL.LS))),sum(as.numeric(as.character(y$FUEL.MDO))),sum(as.numeric(as.character(y$FUEL.MGO.HS))),sum(as.numeric(as.character(y$FUEL.MGO.LS))))
  sea=subset(y,Status=="At Sea")
  m[5,]=c(sum(as.numeric(as.character(sea$FUEL.HS))),sum(as.numeric(as.character(sea$FUEL.LS))),sum(as.numeric(as.character(sea$FUEL.MDO))),sum(as.numeric(as.character(sea$FUEL.MGO.HS))),sum(as.numeric(as.character(sea$FUEL.MGO.LS))))
  port=subset(y,Status=="In Port")
  m[4,]=c(sum(as.numeric(as.character(port$FUEL.HS))),sum(as.numeric(as.character(port$FUEL.LS))),sum(as.numeric(as.character(port$FUEL.MDO))),sum(as.numeric(as.character(port$FUEL.MGO.HS))),sum(as.numeric(as.character(port$FUEL.MGO.LS))))
  m[6,]=c(m[1,]*m[3,])
  m[7,]=c(m[2,]*m[3,]*20/1000)
  m
})

eufuel=reactive({
  y=Mdata()
  m=matrix(0,nrow=4,ncol=5,byrow = TRUE)
  colnames(m)=c("Fuel(HS)",	"Fuel (LS)","Fuel (MDO)","Fuel (MGO)","Fuel (MGO LS)")
  rownames(m)=c("From EU Port to EU Port",
                "From EU Port to Non EU Port",
                "From Non EU Port to EU Port",
                "From Non EU Port to Non EU Port")
  eutoeu = subset(y,y$DepEU == "Y" & y$ArrivalEU == "Y")
  eutononeu = subset(y,y$DepEU == "Y" & y$ArrivalEU == "N")
  noneutoeu = subset(y,y$DepEU == "N" & y$ArrivalEU == "Y")
  noneutononeu = subset(y,y$DepEU == "N" & y$ArrivalEU == "N")
  m[1,]= c(sum(as.numeric(as.character(eutoeu$FUEL.HS))),sum(as.numeric(as.character(eutoeu$FUEL.LS))),sum(as.numeric(as.character(eutoeu$FUEL.MDO))),sum(as.numeric(as.character(eutoeu$FUEL.MGO.HS))),sum(as.numeric(as.character(eutoeu$FUEL.MGO.LS))))
  m[2,]=c(sum(as.numeric(as.character(eutononeu$FUEL.HS))),sum(as.numeric(as.character(eutononeu$FUEL.LS))),sum(as.numeric(as.character(eutononeu$FUEL.MDO))),sum(as.numeric(as.character(eutononeu$FUEL.MGO.HS))),sum(as.numeric(as.character(eutononeu$FUEL.MGO.LS))))
  m[3,]=c(sum(as.numeric(as.character(noneutoeu$FUEL.HS))),sum(as.numeric(as.character(noneutoeu$FUEL.LS))),sum(as.numeric(as.character(noneutoeu$FUEL.MDO))),sum(as.numeric(as.character(noneutoeu$FUEL.MGO.HS))),sum(as.numeric(as.character(noneutoeu$FUEL.MGO.LS))))
  m[4,]=c(sum(as.numeric(as.character(noneutononeu$FUEL.HS))),sum(as.numeric(as.character(noneutononeu$FUEL.LS))),sum(as.numeric(as.character(noneutononeu$FUEL.MDO))),sum(as.numeric(as.character(noneutononeu$FUEL.MGO.HS))),sum(as.numeric(as.character(noneutononeu$FUEL.MGO.LS))))
  m
})

output$allSEE1Table=DT::renderDataTable({
  m=data.frame(allSUMEE1())
  colnames(m) = c("Fuel(HS)",	"Fuel (LS)","Fuel (MDO)","Fuel (MGO)","Fuel MGO LS")
  datatable(m,options = list(pageLength = 15,searching = FALSE,paging = FALSE),rownames = T,colnames=c("TITLE"=1),class = "display")%>%formatStyle(names(m),backgroundColor = "white",color="#000")%>%formatStyle("TITLE",  color = '#FFF', backgroundColor = '#515A5A', fontWeight = 'bold')
  
})

output$EUfueldetails=DT::renderDataTable({
  m=data.frame(eufuel())
  colnames(m) = c("Fuel(HS)",	"Fuel (LS)","Fuel (MDO)","Fuel (MGO)","Fuel MGO LS")
  datatable(m,options = list(pageLength = 15,searching = FALSE,paging = FALSE),rownames = T,colnames=c("TITLE"=1),class = "display")%>%formatStyle(names(m),backgroundColor = "white",color="#000")%>%formatStyle("TITLE",  color = '#FFF', backgroundColor = '#515A5A', fontWeight = 'bold')
  
})



allSUMEE2=reactive({
  y=Mdata()
  m=allSUMEE1()
  if(input$Vtype=="Container"){
    n=matrix(0,nrow=17,ncol=1,byrow = TRUE)
    rownames(n)=c("Total Fuel Consumption",
                  "Total Fuel Consumption at Sea(Ton)",
                  "Total Fuel Consumption at Berth(Ton)",
                  "Total CO2 Emission (Ton)",
                  "CO2 Emissions at sea (Ton)",
                  "CO2 Emission at Berth (Ton)",
                  'Total Distance Sailed (Nm)',
                  "Total time at sea (days)",
                  "Total Cargo (Ton)",
                  "Total Cargo (TEU)",
                  "Total Transport Work (Ton-Nm)",
                  "Total Transport Work (TEU-Nm)",
                  "Fuel consumption per distance (Ton/Nm)",
                  "Fuel consumption per tranport work (freight transport) (g/Ton*Nm)",
                  "CO2 Emission per Distance (g/Nm)",
                  "CO2 Emission per tranport work (g/Ton*Nm)",
                  "CO2 Emission per tranport work (g/TEU*Nm)")					
    n[1]=round(sum(as.numeric(m[3,])),2)
    n[2]=round(sum(as.numeric(m[5,])),2)
    n[3]=round(sum(as.numeric(m[4,])),2)
    n[4]=round(sum(as.numeric(m[6,])),2)
    n[5]=round(sum(as.numeric((m[5,]*m[1]))),2)
    n[6]=round(sum(as.numeric((m[4,]*m[1]))),2)
    n[7]=round(sum(as.numeric(y$MILES.BY.GPS)),2)
    t = subset(y,Status == "At Sea")
    cargo = subset(t,Report.Type == "Noon at sea" || Report.Type == "EOSP" )
    tcargo = unique(as.numeric(as.character(cargo$CARGO.TOTAL)), incomparables = FALSE)
    teucargo = unique(as.numeric(as.character(cargo$CARGO.TOTAL.TEU)), incomparables = FALSE)
    n[8]=round(sum(as.numeric(t$TOTAL.STEAMING.TIME/24)),2)
    n[9]=round(sum(as.numeric(tcargo)),2)
    n[10]=round(sum(as.numeric(teucargo)),2)
    n[11]=round((n[9]*n[7]),2)
    n[12]=round((n[10]*n[7]),2)
    n[13]=round((n[2]/n[7]),2)
    n[14]=round((n[2]*10^6/n[11]),2)
    n[15]=round((n[4]*10^6/n[7]),2)
    n[16]=round((n[4]*10^6/n[11]),2)
    n[17]=round((n[4]*10^6/n[12]),2)
    n
  }
  else{
    n=matrix(0,nrow=14,ncol=1,byrow = TRUE)
    rownames(n)=c("Total Fuel Consumption",
                  "Total Fuel Consumption at Sea(Ton)",
                  "Total Fuel Consumption at Berth(Ton)",
                  "Total CO2 Emission (Ton)",
                  "CO2 Emissions at sea (Ton)",
                  "CO2 Emission at Berth (Ton)",
                  'Total Distance Sailed (Nm)',
                  "Total time at sea (days)",
                  "Total Cargo (Ton)",
                  "Total Transport Work (Ton-Nm)",				
                  "Fuel consumption per distance (Ton/Nm)",
                  "Fuel consumption per tranport work (freight transport) (g/Ton*Nm)",
                  "CO2 Emission per Distance (g/Nm)",
                  "CO2 Emission per tranport work (g/Ton*Nm)")					
    n[1]=round(sum(as.numeric(m[3,])),2)
    n[2]=round(sum(as.numeric(m[5,])),2)
    n[3]=round(sum(as.numeric(m[4,])),2)
    n[4]=round(sum(as.numeric(m[6,])),2)
    n[5]=round(sum(as.numeric((m[5,]*m[1]))),2)
    n[6]=round(sum(as.numeric((m[4,]*m[1]))),2)
    n[7]=round(sum(as.numeric(y$MILES.BY.GPS)),2)
    t = subset(y,Status == "At Sea")
    cargo = subset(t,Status == "At Sea")
    tcargo = unique(as.numeric(as.character(cargo$CARGO.TOTAL)), incomparables = FALSE)
    n[8]=round(sum(as.numeric(t$TOTAL.STEAMING.TIME/24)),2)
    n[9]=round(sum(as.numeric(tcargo)),2)
    n[10]=round((n[9]*n[7]),2)
    n[11]=round((n[2]/n[7]),2)
    n[12]=round((n[2]*10^6/n[10]),2)
    n[13]=round((n[4]*10^6/n[7]),2)
    n[14]=round((n[4]*10^6/n[10]),2)
    n
  }
})



output$allSEE2Table=DT::renderDataTable({
  n=data.frame(allSUMEE2())
  colnames(n)=c("Value")
  datatable(n, options = list(pageLength = 20,searching = FALSE,paging = FALSE,audowidth=T),rownames = T,colnames=c("Parameter"=1),class = 'display')%>%formatStyle(names(n),backgroundColor = "white",color="#000")%>%formatStyle("Parameter",  color = '#FFF', backgroundColor = '#515A5A', fontWeight = 'bold')
  
  
})


#...................................for verifavia modification.....................................................................................#
output$EEOItabs <- renderUI({
  
  
    myTabs = tabsetPanel(tabPanel("Voyage Wise Report",
                                  br(),
                                  if(input$Vtype=="Container"){
                                  box(width=NULL,solidHeader= TRUE,status="info",
                                      box(width=NULL,solidHeader= TRUE,status="info",box(width=6,solidHeader= TRUE,status="info",column(width=4,htmlOutput("EMVoyageno")),column(width=6,htmlOutput("pno")),column(width = 2,downloadButton('Voyagewisereportcont' ,strong('Download Report'))),column(width=12,valueBoxOutput("EEOIBox"),valueBoxOutput("EEOIBox2"),valueBoxOutput("FuelBox"))),
                                          box(width=6,height="340px",solidHeader= TRUE,status="info",title="Voyage Condition",column(width=12,withSpinner(plotlyOutput("ladbal",height="285px")))),
                                          column(width=6,withSpinner(dataTableOutput("SEE1Table"))),br(),column(width=6,withSpinner(dataTableOutput("SEE2Table")))),
                                      box(width=NULL,solidHeader= TRUE,status="info",title=strong("EEOI"),column(width=12,withSpinner(plotlyOutput("EECHART"))),column(width=12,withSpinner(plotlyOutput("EECHARTT"))),column(width=6,withSpinner(plotlyOutput("EECHART2"))),column(width=6,withSpinner(plotlyOutput("EECHARTFO")))))
                                  }else{
                                    box(width=NULL,solidHeader= TRUE,status="info",
                                        box(width=NULL,solidHeader= TRUE,status="info",box(width=6,solidHeader= TRUE,status="info",column(width=4,htmlOutput("EMVoyageno")),column(width=6,htmlOutput("pno")),column(width = 2,downloadButton('Voyagewisereportoth' ,strong('Download Report'))),column(width=12,valueBoxOutput("EEOIBox"),valueBoxOutput("FuelBox"))),
                                            box(width=6,height="340px",solidHeader= TRUE,status="info",title="Voyage Condition",column(width=12,withSpinner(plotlyOutput("ladbal",height="285px")))),
                                            column(width=6,withSpinner(dataTableOutput("SEE1Table"))),br(),column(width=6,withSpinner(dataTableOutput("SEE2Table")))),
                                        box(width=NULL,solidHeader= TRUE,status="info",title=strong("EEOI"),column(width=12,withSpinner(plotlyOutput("EECHART"))),column(width=6,withSpinner(plotlyOutput("EECHART2"))),column(width=6,withSpinner(plotlyOutput("EECHARTFO")))))
                                  }
                                    ),
                         tabPanel("Year Wise Emissions Report",
                                  br(),
                                  if(input$Vtype=="Container"){
                                  box(width=NULL,solidHeader= TRUE,status="info",
                                     box(width=NULL,solidHeader= TRUE,status="info",column(width = 12,downloadButton('allVoyagewisereportcont' ,strong('Download Report'))),column(width=6,dataTableOutput("allSEE1Table"),dataTableOutput("EUfueldetails")),br(),column(width=6,dataTableOutput("allSEE2Table"))),column(width=12,plotlyOutput("EECHART11"),plotlyOutput("EECHART11T")),column(width=6,plotlyOutput("EECHART22")),column(width=6,plotlyOutput("EECHART22FO")))
                                   } else {
                                    box(width=NULL,solidHeader= TRUE,status="info",
                                        box(width=NULL,solidHeader= TRUE,status="info",column(width = 12,downloadButton('allVoyagewisereportoth' ,strong('Download Report'))),column(width=6,dataTableOutput("allSEE1Table"),dataTableOutput("EUfueldetails")),br(),column(width=6,dataTableOutput("allSEE2Table"))),column(width=12,plotlyOutput("EECHART11")),column(width=6,plotlyOutput("EECHART22")),column(width=6,plotlyOutput("EECHART22FO")))
                                    
                                    
                                 } ),
                          tabPanel("Data",
                                  br(),
                                  box(width=NULL,solidHeader= TRUE,status="info",title=strong("EEOI/MRV Data"),column(width=12,downloadButton('datadown' ,strong('Download Data'))),br(),column(width=12,dataTableOutput("DataTable"))))
                         #tabPanel("Description",
                         #         br(),
                         #         box(width=NULL,solidHeader= TRUE,status="info",includeMarkdown("data/Description_ac.Rmd")))
                         
    )
 
 
  myTabs
})

output$vtype <- renderUI({ 
  if(input$method == "Bunker Method"){
    selectInput("Vtype", label=h4(strong("Vessel Type")),choices = "Bulker", selected = "Bulker", multiple = FALSE, selectize = TRUE, width = "75%", size = NULL) 
  } else {
  if(input$emtype == "All Voyages"){
  r=Eeoidata
  Vessel_List = unique(as.character(r[,1]), incomparables = FALSE)
  selectInput("Vtype", label=h4(strong("Vessel Type")), choices = Vessel_List, selected = "Bulker", multiple = FALSE, selectize = TRUE, width = "75%", size = NULL)
  } else {
    r=Eeoidata
    r=subset(r,r$EU == "Y")
    Vessel_List = unique(as.character(r[,1]), incomparables = FALSE)
    selectInput("Vtype", label=h4(strong("Vessel Type")), choices = Vessel_List, selected = NULL, multiple = FALSE, selectize = TRUE, width = "75%", size = NULL)
    
  }
  }
})
output$vname<- renderUI({
  if(input$method == "Bunker Method"){
    selectInput("Vname", label=h4(strong("Vessel Name")),choices = "B2", selected = "B2", multiple = FALSE, selectize = TRUE, width = "75%", size = NULL) 
  } else{
  if(input$emtype == "All Voyages"){
  r=Eeoidata
  r = subset(r,r[,1] == input$Vtype)
  Vessel_List = unique(as.character(r[,4]), incomparables = FALSE)
  selectInput("Vname", label=h4(strong("Vessel Name")), choices = Vessel_List, selected = NULL, multiple = FALSE, selectize = TRUE, width = "75%", size = NULL)
  }else {
    r=Eeoidata
    r=subset(r,r$EU == "Y")
    r = subset(r,r[,1] == input$Vtype)
    Vessel_List = unique(as.character(r[,4]), incomparables = FALSE)
    selectInput("Vname", label=h4(strong("Vessel Name")), choices = Vessel_List, selected = NULL, multiple = FALSE, selectize = TRUE, width = "75%", size = NULL)
    
  }
  }
})

#..........................................................Download Option for Containers............................................................................................................#
voysummary1=reactive({
  
  m=SUMEE1()
  n=data.frame(m[1,],m[2,],m[3,],m[4,],m[5,],m[6,],m[7,])
  n
})
voysummary2=reactive({
  
  
  m=SUMEE2()
  
  n=data.frame(m[1,],m[2,],m[3,],m[4,],m[5,],m[6,],m[7,],m[8,],m[9,],m[10,],m[11,],m[12,],m[13,],m[14,],m[15,],m[16,],m[17,])
  
  
  
  n
})

output$Voyagewisereportcont <- downloadHandler(
  
  filename = function() { 'Voyagewisereport.xlsx' }, content = function(file) {
    require (XLConnect)
    exc2 <- XLConnect::loadWorkbook("data/EEOI_TEMPLATES.xlsx")
    setStyleAction(exc2,XLC$"STYLE_ACTION.NONE")
    y=Mdata()
    y=subset(y,VoyageNo == input$EMvoyageno)
    y=subset(y,Depart_Arrival == input$passage)
    tot = length(y[,1])
    z= subset(y,y$VoyCondition == "BALLAST")
    ball = length(z[,1])
    lad = tot - ball
   
    
    r=voysummary2()
    s=voysummary1()
    t=EECHARTDATAT()
    table1 = data.frame(t$Corrected.Date,t$EEOIC,t$EEOI)
    table2 = data.frame(t$Corrected.Date,t$CO2)
    table3 = data.frame(t$Corrected.Date,t$FOC)
    
   
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
    writeWorksheet(exc2,r[1,17] ,  sheet = 1 , startRow = 28, startCol = 8,header=FALSE)
    #.......................Summary table 1..............................................
    writeWorksheet(exc2,s[1,1] , sheet = 1, startRow = 12, startCol = 2,header=FALSE)
    writeWorksheet(exc2,s[1,2] , sheet = 1, startRow = 13, startCol = 2,header=FALSE)
    writeWorksheet(exc2,s[1,3] , sheet = 1 , startRow = 14, startCol = 2,header=FALSE)
    writeWorksheet(exc2,s[1,4], sheet = 1 , startRow = 15, startCol = 2,header=FALSE)
    writeWorksheet(exc2,s[1,5] , sheet = 1 , startRow = 16, startCol =2,header=FALSE)
    writeWorksheet(exc2,s[1,6] , sheet = 1 , startRow = 17, startCol =2,header=FALSE)
    writeWorksheet(exc2,s[1,7] , sheet = 1 , startRow = 18, startCol =2,header=FALSE)
    
    
    writeWorksheet(exc2,s[2,1] , sheet = 1, startRow = 12, startCol = 3,header=FALSE)
    writeWorksheet(exc2,s[2,2] , sheet = 1, startRow = 13, startCol = 3,header=FALSE)
    writeWorksheet(exc2,s[2,3] , sheet = 1 , startRow = 14, startCol = 3,header=FALSE)
    writeWorksheet(exc2,s[2,4], sheet = 1 , startRow = 15, startCol = 3,header=FALSE)
    writeWorksheet(exc2,s[2,5] , sheet = 1 , startRow = 16, startCol =3,header=FALSE)
    writeWorksheet(exc2,s[2,6] , sheet = 1 , startRow = 17, startCol =3,header=FALSE)
    writeWorksheet(exc2,s[2,7] , sheet = 1 , startRow = 18, startCol =3,header=FALSE)
    
    writeWorksheet(exc2,s[3,1] , sheet = 1, startRow = 12, startCol = 4,header=FALSE)
    writeWorksheet(exc2,s[3,2] , sheet = 1, startRow = 13, startCol = 4,header=FALSE)
    writeWorksheet(exc2,s[3,3] , sheet = 1 , startRow = 14, startCol = 4,header=FALSE)
    writeWorksheet(exc2,s[3,4], sheet = 1 , startRow = 15, startCol = 4,header=FALSE)
    writeWorksheet(exc2,s[3,5] , sheet = 1 , startRow = 16, startCol =4,header=FALSE)
    writeWorksheet(exc2,s[3,6] , sheet = 1 , startRow = 17, startCol =4,header=FALSE)
    writeWorksheet(exc2,s[3,7] , sheet = 1 , startRow = 18, startCol =4,header=FALSE)
    
    writeWorksheet(exc2,s[4,1] , sheet = 1, startRow = 12, startCol = 5,header=FALSE)
    writeWorksheet(exc2,s[4,2] , sheet = 1, startRow = 13, startCol = 5,header=FALSE)
    writeWorksheet(exc2,s[4,3] , sheet = 1 , startRow = 14, startCol = 5,header=FALSE)
    writeWorksheet(exc2,s[4,4], sheet = 1 , startRow = 15, startCol = 5,header=FALSE)
    writeWorksheet(exc2,s[4,5] , sheet = 1 , startRow = 16, startCol =5,header=FALSE)
    writeWorksheet(exc2,s[4,6] , sheet = 1 , startRow = 17, startCol =5,header=FALSE)
    writeWorksheet(exc2,s[4,7] , sheet = 1 , startRow = 18, startCol =5,header=FALSE)
    
    writeWorksheet(exc2,s[5,1] , sheet = 1, startRow = 12, startCol = 6,header=FALSE)
    writeWorksheet(exc2,s[5,2] , sheet = 1, startRow = 13, startCol = 6,header=FALSE)
    writeWorksheet(exc2,s[5,3] , sheet = 1 , startRow = 14, startCol = 6,header=FALSE)
    writeWorksheet(exc2,s[5,4], sheet = 1 , startRow = 15, startCol = 6,header=FALSE)
    writeWorksheet(exc2,s[5,5] , sheet = 1 , startRow = 16, startCol =6,header=FALSE)
    writeWorksheet(exc2,s[5,6] , sheet = 1 , startRow = 17, startCol =6,header=FALSE)
    writeWorksheet(exc2,s[5,7] , sheet = 1 , startRow = 18, startCol =6,header=FALSE)
    #......................Voyage details................................................
    writeWorksheet(exc2,input$Vname , sheet = 1, startRow = 4, startCol = 2,header=FALSE)
    writeWorksheet(exc2, input$EMvoyageno , sheet = 1 , startRow = 5, startCol =2,header=FALSE)
    writeWorksheet(exc2, input$passage , sheet = 1 , startRow = 6, startCol =2,header=FALSE)
    
    writeWorksheet(exc2,as.Date(input$edate[1],"%y-%m-%d"), sheet = 1 , startRow = 6, startCol = 7,header=FALSE)
    writeWorksheet(exc2,as.Date(input$edate[2],"%y-%m-%d"), sheet = 1 , startRow = 6, startCol = 8,header=FALSE)
    writeWorksheet(exc2, ball , sheet = 2 , startRow = 2, startCol =20,header=FALSE)
    writeWorksheet(exc2, lad , sheet = 2 , startRow = 2, startCol =21,header=FALSE)
    writeWorksheet(exc2,table1, sheet = 2 , startRow = 2, startCol = 1,header=FALSE)
    writeWorksheet(exc2,table2, sheet = 2 , startRow = 2, startCol = 5,header=FALSE)
    writeWorksheet(exc2,table3, sheet = 2 , startRow = 2, startCol = 8,header=FALSE)
    
    
    #write.xlsx(t,file,sheetName="Sheet2")
    
    setForceFormulaRecalculation(exc2,sheet = "*",TRUE)
    
    saveWorkbook(exc2,file = file)
    
  }
)
voysummary11=reactive({
  
  m=allSUMEE1()
  n=data.frame(m[1,],m[2,],m[3,],m[4,],m[5,],m[6,],m[7,])
  n
})
voysummary22=reactive({
  
  
  m=allSUMEE2()
  
  n=data.frame(m[1,],m[2,],m[3,],m[4,],m[5,],m[6,],m[7,],m[8,],m[9,],m[10,],m[11,],m[12,],m[13,],m[14,],m[15,],m[16,],m[17,])
  
  
  
  n
})
output$allVoyagewisereportcont <- downloadHandler(
  
  filename = function() { 'yearwisereport.xlsx' }, content = function(file) {
    require (XLConnect)
    exc2 <- XLConnect::loadWorkbook("data/EEOI_TEMPLATESyw.xlsx")
    setStyleAction(exc2,XLC$"STYLE_ACTION.NONE")
    y=Mdata()
    tot = length(y[,1])
    z= subset(y,y$VoyCondition == "BALLAST")
    ball = length(z[,1])
    lad = tot - ball
    
    
    r=voysummary22()
    s=voysummary11()
    t=EECHARTDATA1()
    table1 = data.frame(t$VoyageNo,t$EEOI,t$EEOIt)
    table2 = data.frame(t$VoyageNo,t$co)
    table3 = data.frame(t$VoyageNo,t$fo)
    
    
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
    writeWorksheet(exc2,r[1,17] ,  sheet = 1 , startRow = 28, startCol = 8,header=FALSE)
    #.......................Summary table 1..............................................
    writeWorksheet(exc2,s[1,1] , sheet = 1, startRow = 12, startCol = 2,header=FALSE)
    writeWorksheet(exc2,s[1,2] , sheet = 1, startRow = 13, startCol = 2,header=FALSE)
    writeWorksheet(exc2,s[1,3] , sheet = 1 , startRow = 14, startCol = 2,header=FALSE)
    writeWorksheet(exc2,s[1,4], sheet = 1 , startRow = 15, startCol = 2,header=FALSE)
    writeWorksheet(exc2,s[1,5] , sheet = 1 , startRow = 16, startCol =2,header=FALSE)
    writeWorksheet(exc2,s[1,6] , sheet = 1 , startRow = 17, startCol =2,header=FALSE)
    writeWorksheet(exc2,s[1,7] , sheet = 1 , startRow = 18, startCol =2,header=FALSE)
    
    
    writeWorksheet(exc2,s[2,1] , sheet = 1, startRow = 12, startCol = 3,header=FALSE)
    writeWorksheet(exc2,s[2,2] , sheet = 1, startRow = 13, startCol = 3,header=FALSE)
    writeWorksheet(exc2,s[2,3] , sheet = 1 , startRow = 14, startCol = 3,header=FALSE)
    writeWorksheet(exc2,s[2,4], sheet = 1 , startRow = 15, startCol = 3,header=FALSE)
    writeWorksheet(exc2,s[2,5] , sheet = 1 , startRow = 16, startCol =3,header=FALSE)
    writeWorksheet(exc2,s[2,6] , sheet = 1 , startRow = 17, startCol =3,header=FALSE)
    writeWorksheet(exc2,s[2,7] , sheet = 1 , startRow = 18, startCol =3,header=FALSE)
    
    writeWorksheet(exc2,s[3,1] , sheet = 1, startRow = 12, startCol = 4,header=FALSE)
    writeWorksheet(exc2,s[3,2] , sheet = 1, startRow = 13, startCol = 4,header=FALSE)
    writeWorksheet(exc2,s[3,3] , sheet = 1 , startRow = 14, startCol = 4,header=FALSE)
    writeWorksheet(exc2,s[3,4], sheet = 1 , startRow = 15, startCol = 4,header=FALSE)
    writeWorksheet(exc2,s[3,5] , sheet = 1 , startRow = 16, startCol =4,header=FALSE)
    writeWorksheet(exc2,s[3,6] , sheet = 1 , startRow = 17, startCol =4,header=FALSE)
    writeWorksheet(exc2,s[3,7] , sheet = 1 , startRow = 18, startCol =4,header=FALSE)
    
    writeWorksheet(exc2,s[4,1] , sheet = 1, startRow = 12, startCol = 5,header=FALSE)
    writeWorksheet(exc2,s[4,2] , sheet = 1, startRow = 13, startCol = 5,header=FALSE)
    writeWorksheet(exc2,s[4,3] , sheet = 1 , startRow = 14, startCol = 5,header=FALSE)
    writeWorksheet(exc2,s[4,4], sheet = 1 , startRow = 15, startCol = 5,header=FALSE)
    writeWorksheet(exc2,s[4,5] , sheet = 1 , startRow = 16, startCol =5,header=FALSE)
    writeWorksheet(exc2,s[4,6] , sheet = 1 , startRow = 17, startCol =5,header=FALSE)
    writeWorksheet(exc2,s[4,7] , sheet = 1 , startRow = 18, startCol =5,header=FALSE)
    
    writeWorksheet(exc2,s[5,1] , sheet = 1, startRow = 12, startCol = 6,header=FALSE)
    writeWorksheet(exc2,s[5,2] , sheet = 1, startRow = 13, startCol = 6,header=FALSE)
    writeWorksheet(exc2,s[5,3] , sheet = 1 , startRow = 14, startCol = 6,header=FALSE)
    writeWorksheet(exc2,s[5,4], sheet = 1 , startRow = 15, startCol = 6,header=FALSE)
    writeWorksheet(exc2,s[5,5] , sheet = 1 , startRow = 16, startCol =6,header=FALSE)
    writeWorksheet(exc2,s[5,6] , sheet = 1 , startRow = 17, startCol =6,header=FALSE)
    writeWorksheet(exc2,s[5,7] , sheet = 1 , startRow = 18, startCol =6,header=FALSE)
    #......................Voyage details................................................
    writeWorksheet(exc2,input$Vname , sheet = 1, startRow = 4, startCol = 2,header=FALSE)
    
    
    writeWorksheet(exc2,as.Date(input$edate[1],"%y-%m-%d"), sheet = 1 , startRow = 6, startCol = 7,header=FALSE)
    writeWorksheet(exc2,as.Date(input$edate[2],"%y-%m-%d"), sheet = 1 , startRow = 6, startCol = 8,header=FALSE)
    writeWorksheet(exc2, ball , sheet = 2 , startRow = 2, startCol =20,header=FALSE)
    writeWorksheet(exc2, lad , sheet = 2 , startRow = 2, startCol =21,header=FALSE)
    writeWorksheet(exc2,table1, sheet = 2 , startRow = 2, startCol = 1,header=FALSE)
    writeWorksheet(exc2,table2, sheet = 2 , startRow = 2, startCol = 5,header=FALSE)
    writeWorksheet(exc2,table3, sheet = 2 , startRow = 2, startCol = 8,header=FALSE)
    
    
    #write.xlsx(t,file,sheetName="Sheet2")
    
    setForceFormulaRecalculation(exc2,sheet = "*",TRUE)
    
    saveWorkbook(exc2,file = file)
    
  }
)
#...................................................Download report for Bulkers etc.....................................................................................................#
voysummary1b=reactive({
  
  m=SUMEE1()
  n=data.frame(m[1,],m[2,],m[3,],m[4,],m[5,],m[6,],m[7,])
  n
})
voysummary2b=reactive({
  
  
  m=SUMEE2()
  
  n=data.frame(m[1,],m[2,],m[3,],m[4,],m[5,],m[6,],m[7,],m[8,],m[9,],m[10,],m[11,],m[12,],m[13,],m[14,])
  
  
  
  n
})
output$Voyagewisereportoth <- downloadHandler(
  
  filename = function() { 'Voyagewisereport.xlsx' }, content = function(file) {
    require (XLConnect)
    exc2 <- XLConnect::loadWorkbook("data/EEOI_TEMPLATES2.xlsx")
    setStyleAction(exc2,XLC$"STYLE_ACTION.NONE")
    y=Mdata()
    y=subset(y,VoyageNo == input$EMvoyageno)
    y=subset(y,Depart_Arrival == input$passage)
    tot = length(y[,1])
    z= subset(y,y$VoyCondition == "BALLAST")
    ball = length(z[,1])
    lad = tot - ball
    
    
    r=voysummary2b()
    s=voysummary1b()
    t=EECHARTDATA()
    table1 = data.frame(t$Corrected.Date,t$EEOI)
    table2 = data.frame(t$Corrected.Date,t$CO2)
    table3 = data.frame(t$Corrected.Date,t$FOC)
    
    
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
    
    #.......................Summary table 1..............................................
    writeWorksheet(exc2,s[1,1] , sheet = 1, startRow = 12, startCol = 2,header=FALSE)
    writeWorksheet(exc2,s[1,2] , sheet = 1, startRow = 13, startCol = 2,header=FALSE)
    writeWorksheet(exc2,s[1,3] , sheet = 1 , startRow = 14, startCol = 2,header=FALSE)
    writeWorksheet(exc2,s[1,4], sheet = 1 , startRow = 15, startCol = 2,header=FALSE)
    writeWorksheet(exc2,s[1,5] , sheet = 1 , startRow = 16, startCol =2,header=FALSE)
    writeWorksheet(exc2,s[1,6] , sheet = 1 , startRow = 17, startCol =2,header=FALSE)
    writeWorksheet(exc2,s[1,7] , sheet = 1 , startRow = 18, startCol =2,header=FALSE)
    
    
    writeWorksheet(exc2,s[2,1] , sheet = 1, startRow = 12, startCol = 3,header=FALSE)
    writeWorksheet(exc2,s[2,2] , sheet = 1, startRow = 13, startCol = 3,header=FALSE)
    writeWorksheet(exc2,s[2,3] , sheet = 1 , startRow = 14, startCol = 3,header=FALSE)
    writeWorksheet(exc2,s[2,4], sheet = 1 , startRow = 15, startCol = 3,header=FALSE)
    writeWorksheet(exc2,s[2,5] , sheet = 1 , startRow = 16, startCol =3,header=FALSE)
    writeWorksheet(exc2,s[2,6] , sheet = 1 , startRow = 17, startCol =3,header=FALSE)
    writeWorksheet(exc2,s[2,7] , sheet = 1 , startRow = 18, startCol =3,header=FALSE)
    
    writeWorksheet(exc2,s[3,1] , sheet = 1, startRow = 12, startCol = 4,header=FALSE)
    writeWorksheet(exc2,s[3,2] , sheet = 1, startRow = 13, startCol = 4,header=FALSE)
    writeWorksheet(exc2,s[3,3] , sheet = 1 , startRow = 14, startCol = 4,header=FALSE)
    writeWorksheet(exc2,s[3,4], sheet = 1 , startRow = 15, startCol = 4,header=FALSE)
    writeWorksheet(exc2,s[3,5] , sheet = 1 , startRow = 16, startCol =4,header=FALSE)
    writeWorksheet(exc2,s[3,6] , sheet = 1 , startRow = 17, startCol =4,header=FALSE)
    writeWorksheet(exc2,s[3,7] , sheet = 1 , startRow = 18, startCol =4,header=FALSE)
    
    writeWorksheet(exc2,s[4,1] , sheet = 1, startRow = 12, startCol = 5,header=FALSE)
    writeWorksheet(exc2,s[4,2] , sheet = 1, startRow = 13, startCol = 5,header=FALSE)
    writeWorksheet(exc2,s[4,3] , sheet = 1 , startRow = 14, startCol = 5,header=FALSE)
    writeWorksheet(exc2,s[4,4], sheet = 1 , startRow = 15, startCol = 5,header=FALSE)
    writeWorksheet(exc2,s[4,5] , sheet = 1 , startRow = 16, startCol =5,header=FALSE)
    writeWorksheet(exc2,s[4,6] , sheet = 1 , startRow = 17, startCol =5,header=FALSE)
    writeWorksheet(exc2,s[4,7] , sheet = 1 , startRow = 18, startCol =5,header=FALSE)
    
    writeWorksheet(exc2,s[5,1] , sheet = 1, startRow = 12, startCol = 6,header=FALSE)
    writeWorksheet(exc2,s[5,2] , sheet = 1, startRow = 13, startCol = 6,header=FALSE)
    writeWorksheet(exc2,s[5,3] , sheet = 1 , startRow = 14, startCol = 6,header=FALSE)
    writeWorksheet(exc2,s[5,4], sheet = 1 , startRow = 15, startCol = 6,header=FALSE)
    writeWorksheet(exc2,s[5,5] , sheet = 1 , startRow = 16, startCol =6,header=FALSE)
    writeWorksheet(exc2,s[5,6] , sheet = 1 , startRow = 17, startCol =6,header=FALSE)
    writeWorksheet(exc2,s[5,7] , sheet = 1 , startRow = 18, startCol =6,header=FALSE)
    #......................Voyage details................................................
    writeWorksheet(exc2,input$Vname , sheet = 1, startRow = 4, startCol = 2,header=FALSE)
    writeWorksheet(exc2, input$EMvoyageno , sheet = 1 , startRow = 5, startCol =2,header=FALSE)
    writeWorksheet(exc2, input$passage , sheet = 1 , startRow = 6, startCol =2,header=FALSE)
    
    writeWorksheet(exc2,as.Date(input$edate[1],"%y-%m-%d"), sheet = 1 , startRow = 6, startCol = 7,header=FALSE)
    writeWorksheet(exc2,as.Date(input$edate[2],"%y-%m-%d"), sheet = 1 , startRow = 6, startCol = 8,header=FALSE)
    writeWorksheet(exc2,ball, sheet = 2 , startRow = 2, startCol =20,header=FALSE)
    writeWorksheet(exc2,lad, sheet = 2 , startRow = 2, startCol =21,header=FALSE)
    writeWorksheet(exc2,table1, sheet = 2 , startRow = 2, startCol = 1,header=FALSE)
    writeWorksheet(exc2,table2, sheet = 2 , startRow = 2, startCol = 5,header=FALSE)
    writeWorksheet(exc2,table3, sheet = 2 , startRow = 2, startCol = 8,header=FALSE)
    
    
    #write.xlsx(t,file,sheetName="Sheet2")
    
    setForceFormulaRecalculation(exc2,sheet = "*",TRUE)
    
    saveWorkbook(exc2,file = file)
    
  }
)

allvoysummary1b=reactive({
  
  m=allSUMEE1()
  n=data.frame(m[1,],m[2,],m[3,],m[4,],m[5,],m[6,],m[7,])
  n
})
allvoysummary2b=reactive({
  
  
  m=allSUMEE2()
  
  n=data.frame(m[1,],m[2,],m[3,],m[4,],m[5,],m[6,],m[7,],m[8,],m[9,],m[10,],m[11,],m[12,],m[13,],m[14,])
  
  
  
  n
})

output$allVoyagewisereportoth <- downloadHandler(
  
  filename = function() { 'yearwisereport.xlsx' }, content = function(file) {
    require (XLConnect)
    exc2 <- XLConnect::loadWorkbook("data/EEOI_TEMPLATES2yw.xlsx")
    setStyleAction(exc2,XLC$"STYLE_ACTION.NONE")
    y=Mdata()
    tot = length(y[,1])
    z= subset(y,y$VoyCondition == "BALLAST")
    ball = length(z[,1])
    lad = tot - ball
    
    
    r=allvoysummary2b()
    s=allvoysummary1b()
    t=EECHARTDATA1()
    table1 = data.frame(t$VoyageNo,t$EEOI)
    table2 = data.frame(t$VoyageNo,t$co)
    table3 = data.frame(t$VoyageNo,t$fo)
    
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
    
    #.......................Summary table 1..............................................
    writeWorksheet(exc2,s[1,1] , sheet = 1, startRow = 12, startCol = 2,header=FALSE)
    writeWorksheet(exc2,s[1,2] , sheet = 1, startRow = 13, startCol = 2,header=FALSE)
    writeWorksheet(exc2,s[1,3] , sheet = 1 , startRow = 14, startCol = 2,header=FALSE)
    writeWorksheet(exc2,s[1,4], sheet = 1 , startRow = 15, startCol = 2,header=FALSE)
    writeWorksheet(exc2,s[1,5] , sheet = 1 , startRow = 16, startCol =2,header=FALSE)
    writeWorksheet(exc2,s[1,6] , sheet = 1 , startRow = 17, startCol =2,header=FALSE)
    writeWorksheet(exc2,s[1,7] , sheet = 1 , startRow = 18, startCol =2,header=FALSE)
    
    
    writeWorksheet(exc2,s[2,1] , sheet = 1, startRow = 12, startCol = 3,header=FALSE)
    writeWorksheet(exc2,s[2,2] , sheet = 1, startRow = 13, startCol = 3,header=FALSE)
    writeWorksheet(exc2,s[2,3] , sheet = 1 , startRow = 14, startCol = 3,header=FALSE)
    writeWorksheet(exc2,s[2,4], sheet = 1 , startRow = 15, startCol = 3,header=FALSE)
    writeWorksheet(exc2,s[2,5] , sheet = 1 , startRow = 16, startCol =3,header=FALSE)
    writeWorksheet(exc2,s[2,6] , sheet = 1 , startRow = 17, startCol =3,header=FALSE)
    writeWorksheet(exc2,s[2,7] , sheet = 1 , startRow = 18, startCol =3,header=FALSE)
    
    writeWorksheet(exc2,s[3,1] , sheet = 1, startRow = 12, startCol = 4,header=FALSE)
    writeWorksheet(exc2,s[3,2] , sheet = 1, startRow = 13, startCol = 4,header=FALSE)
    writeWorksheet(exc2,s[3,3] , sheet = 1 , startRow = 14, startCol = 4,header=FALSE)
    writeWorksheet(exc2,s[3,4], sheet = 1 , startRow = 15, startCol = 4,header=FALSE)
    writeWorksheet(exc2,s[3,5] , sheet = 1 , startRow = 16, startCol =4,header=FALSE)
    writeWorksheet(exc2,s[3,6] , sheet = 1 , startRow = 17, startCol =4,header=FALSE)
    writeWorksheet(exc2,s[3,7] , sheet = 1 , startRow = 18, startCol =4,header=FALSE)
    
    writeWorksheet(exc2,s[4,1] , sheet = 1, startRow = 12, startCol = 5,header=FALSE)
    writeWorksheet(exc2,s[4,2] , sheet = 1, startRow = 13, startCol = 5,header=FALSE)
    writeWorksheet(exc2,s[4,3] , sheet = 1 , startRow = 14, startCol = 5,header=FALSE)
    writeWorksheet(exc2,s[4,4], sheet = 1 , startRow = 15, startCol = 5,header=FALSE)
    writeWorksheet(exc2,s[4,5] , sheet = 1 , startRow = 16, startCol =5,header=FALSE)
    writeWorksheet(exc2,s[4,6] , sheet = 1 , startRow = 17, startCol =5,header=FALSE)
    writeWorksheet(exc2,s[4,7] , sheet = 1 , startRow = 18, startCol =5,header=FALSE)
    
    writeWorksheet(exc2,s[5,1] , sheet = 1, startRow = 12, startCol = 6,header=FALSE)
    writeWorksheet(exc2,s[5,2] , sheet = 1, startRow = 13, startCol = 6,header=FALSE)
    writeWorksheet(exc2,s[5,3] , sheet = 1 , startRow = 14, startCol = 6,header=FALSE)
    writeWorksheet(exc2,s[5,4], sheet = 1 , startRow = 15, startCol = 6,header=FALSE)
    writeWorksheet(exc2,s[5,5] , sheet = 1 , startRow = 16, startCol =6,header=FALSE)
    writeWorksheet(exc2,s[5,6] , sheet = 1 , startRow = 17, startCol =6,header=FALSE)
    writeWorksheet(exc2,s[5,7] , sheet = 1 , startRow = 18, startCol =6,header=FALSE)
    #......................Voyage details................................................
    writeWorksheet(exc2,input$Vname , sheet = 1, startRow = 4, startCol = 2,header=FALSE)
    
    
    writeWorksheet(exc2,as.Date(input$edate[1],"%y-%m-%d"), sheet = 1 , startRow = 6, startCol = 7,header=FALSE)
    writeWorksheet(exc2,as.Date(input$edate[2],"%y-%m-%d"), sheet = 1 , startRow = 6, startCol = 8,header=FALSE)
    writeWorksheet(exc2,ball, sheet = 2 , startRow = 2, startCol =20,header=FALSE)
    writeWorksheet(exc2,lad, sheet = 2 , startRow = 2, startCol =21,header=FALSE)
    writeWorksheet(exc2,table1, sheet = 2 , startRow = 2, startCol = 1,header=FALSE)
    writeWorksheet(exc2,table2, sheet = 2 , startRow = 2, startCol = 5,header=FALSE)
    writeWorksheet(exc2,table3, sheet = 2 , startRow = 2, startCol = 8,header=FALSE)
    
    
    #write.xlsx(t,file,sheetName="Sheet2")
    
    setForceFormulaRecalculation(exc2,sheet = "*",TRUE)
    
    saveWorkbook(exc2,file = file)
    
  }
)

#.................................Data download.......................................................................................................................#

output$datadown <- downloadHandler(
  
  filename = function() { 'EEOIData.xlsx' }, content = function(file) {
    require (XLConnect)
    exc2 <- XLConnect::loadWorkbook("data/datadown.xlsx")
    setStyleAction(exc2,XLC$"STYLE_ACTION.NONE")
    y=Mdata()

    
    #........................Summary table 2............................................
    writeWorksheet(exc2,y, sheet = 1, startRow = 1, startCol = 1,header=TRUE)

    
    
    #write.xlsx(t,file,sheetName="Sheet2")
    
    setForceFormulaRecalculation(exc2,sheet = "*",TRUE)
    
    saveWorkbook(exc2,file = file)
    
  }
)










