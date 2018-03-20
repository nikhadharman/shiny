
#voyage calculator .......
vcvesselpart <- reactive({
  y=data.frame(read.csv("data/Vessel Detail.csv"))
  ff=input$vcvessel
  y=subset(y,y$Vessel == ff)
})
vcxfile= data.frame(read.csv("data/Coefficient.csv"))

output$vcvessel <- renderUI({ 
  r=vcxfile
  s=input$vctype
  if(s=="Fleet wise"){
    r=subset(r,Fleet == input$vcfleet)
    
    Vessel_List = unique(as.character(r[,3]), incomparables = FALSE)
    selectInput("vcvessel", label=strong("Vessel"), choices = Vessel_List, selected = "MTM Penang", multiple = FALSE, selectize = TRUE, width = "50%", size = NULL)
  }else
  {
    Vessel_List = unique(as.character(r[,3]), incomparables = FALSE)
    selectInput("vcvessel", label=strong("Vessel"), choices = Vessel_List, selected = "MTM Penang", multiple = FALSE, selectize = TRUE, width = "50%", size = NULL)
  }
})
output$vcfleet <- renderUI({ 
  r=vcxfile
  s=input$vctype
  if(s=="Fleet wise"){
    Vessel_List = unique(as.character(r[,1]), incomparables = FALSE)
    selectInput("vcfleet", label=strong("Fleet"), choices = Vessel_List, selected = Vessel_List[1], multiple = FALSE, selectize = TRUE, width = "50%", size = NULL)
  }else{return(NULL)}
})



output$vccdate <- renderUI({ 
  r=vcxfile
  r =subset(r,r$vessel==input$vcvessel)
  daterange = unique(as.character(r[,4]), incomparables = FALSE)
  selectInput("vccdate", label=strong("Date Range"), choices = daterange, selected = daterange[1], multiple = FALSE, selectize = TRUE, width = "50%", size = NULL)
})

output$vccType <- renderUI({ 
  r=vcxfile
  r =subset(r,r$vessel==input$vcvessel)
  r =subset(r,r$daterange == input$vccdate)
  Type = unique(as.character(r[,14]), incomparables = FALSE)
  selectInput("vccType", label=strong("Type"), choices = Type, selected =  Type[1], multiple = FALSE, selectize = TRUE, width = "50%", size = NULL)
})

vcd=reactive({
  y=vcxfile
  y =subset(y,y$vessel==input$vcvessel)
  y =subset(y,y$daterange == input$vccdate)
  y =subset(y,y$Type == input$vccType)
  y
})



vccal= reactive({
  y=vcd()
  SP=input$SP
  DT=input$DT
  D=(input$DIS)/(SP*24)
  SeaState=c(0,1,2,3,4,5,6,7,8)
  FO_Cons_Per_Day=c(NA)
  
  POWER=c(NA)
  
  M=data.frame(SeaState,POWER,FO_Cons_Per_Day)
  for(i in 1:9){
    
    M[i,3]= round(DT^as.numeric(y$n1)*SP^as.numeric(y$f2)*(10-SeaState[i])^as.numeric(y$f3)*exp(as.numeric(y$k1)),2)
    M[i,2]=round(DT^as.numeric(y$n1)*SP^as.numeric(y$n2)*(10-SeaState[i])^as.numeric(y$n3)*exp(as.numeric(y$k)),1)
  }
  M
  M$FO_cons_voyage=round(M$FO_Cons_Per_Day*D,2)
  colnames(M)=c("Sea State","Power (kW)","FO/24 Hrs (T)","FO Voyage (T)")
  M
})

output$Cal=renderDataTable({
  validate(
    need(try(vccal()),"Please Wait....")
  )
  y=vccal()
  datatable(y,class = 'cell-border stripe', options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%
    formatStyle(names(y),color="#000")
})

output$Cal2=renderDataTable({
  validate(
    need(try(vccal()),"Please Wait....")
  )
  y=vccal()
  datatable(y,class = 'cell-border stripe', options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%
    formatStyle(names(y),color="#000")
})


output$FOCAL=renderPlotly({
  validate(
    need(try(vccal()),"Please Wait....")
  )
  xD=vccal()
  s=plot_ly(xD,x=~xD[,1],y=~xD[,3],type="scatter",mode="lines+markers",line = list(shape = "linear",color= "#009DCC"))
  s = s%>%layout(title = "FO/24 Hrs",titlefont=c,xaxis=Sea_Axis,yaxis=FO_axis,showlegend=FALSE)%>%
    layout(plot_bgcolor = "#FFFFFF", paper_bgcolor = "#FFFFFF",showlegend = FALSE )
})

output$FOCAL2=renderPlotly({
  validate(
    need(try(vccal()),"Please Wait....")
  )
  xD=vccal()
  s=plot_ly(xD,x=~xD[,1],y=~xD[,3],type="scatter",mode="lines+markers",line = list(shape = "linear",color= "#009DCC"))
  s = s%>%layout(title = "FO/24 Hrs",titlefont=c,xaxis=Sea_Axis,yaxis=FO_axis,showlegend=FALSE)%>%
    layout(plot_bgcolor = "#FFFFFF", paper_bgcolor = "#FFFFFF",showlegend = FALSE )
})

output$PCAL=renderPlotly({
  validate(
    need(try(vccal()),"Please Wait....")
  )
  xD=vccal()
  s=plot_ly(xD,x=~xD[,1],y=~xD[,2],type="scatter",mode="lines+markers",line = list(shape = "linear",color= "#009DCC"))
  s = s%>%layout(title = "Power",titlefont=c,xaxis=Sea_Axis,yaxis=Power_axis,showlegend=FALSE)%>%
    layout(plot_bgcolor = "#FFFFFF", paper_bgcolor = "#FFFFFF",showlegend = FALSE)
})

output$PCAL2=renderPlotly({
  validate(
    need(try(vccal()),"Please Wait....")
  )
  xD=vccal()
  s=plot_ly(xD,x=~xD[,1],y=~xD[,2],type="scatter",mode="lines+markers",line = list(shape = "linear",color= "#009DCC"))
  s = s%>%layout(title = "Power",titlefont=c,xaxis=Sea_Axis,yaxis=Power_axis,showlegend=FALSE)%>%
    layout(plot_bgcolor = "#FFFFFF", paper_bgcolor = "#FFFFFF",showlegend = FALSE)
})

output$DAYS=renderValueBox({
  D=round((input$DIS)/(input$SP*24),0)
  valueBox(
    paste(D), h2("Voyage Days"),icon = icon("ship"),
    color = "blue"
  )
})
output$SP=renderValueBox({
  D=round((input$SPEED_DIS)/(input$SPEED_DAY*24),0)
  valueBox(
    paste(D,"Knots"), h2("Speed"),icon = icon("ship"),
    color = "green"
  )
})


