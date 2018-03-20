

#performances curve for client purpose ..........................
clientvesselpart <- reactive({
  y=data.frame(read.csv("data/Vessel Detail.csv"))
  validate(
    need(try(input$clientvessel),"Please Wait or Select the vessel")
  )
  ff=input$clientvessel
  y=subset(y,y$Vessel == ff)
})
xfile= data.frame(read.csv("data/Coefficient.csv"))

output$clientvessel <- renderUI({ 
  r=xfile
  s=input$clienttype
  if(s=="Fleet Wise"){
    r=subset(r,Fleet == input$clientfleet)
    
    Vessel_List = unique(as.character(r[,3]), incomparables = FALSE)
    selectInput("clientvessel", label=strong("Vessel"), choices = Vessel_List, selected =Vessel_List[1], multiple = FALSE, selectize = TRUE, width = "50%", size = NULL)
  }else
  {
    Vessel_List = unique(as.character(r[,3]), incomparables = FALSE)
    selectInput("clientvessel", label=strong("Vesel"), choices = Vessel_List, selected = Vessel_List[1], multiple = FALSE, selectize = TRUE, width = "50%", size = NULL)
  }
})
output$clientfleet <- renderUI({ 
  r=xfile
  s=input$clienttype
  if(s=="Fleet Wise"){
    Vessel_List = unique(as.character(r[,1]), incomparables = FALSE)
    selectInput("clientfleet", label=strong("Fleet"), choices = Vessel_List, selected = Vessel_List[1], multiple = FALSE, selectize = TRUE, width = "50%", size = NULL)
  }else{return(NULL)}
})



output$cdate <- renderUI({ 
  r=xfile
  r =subset(r,r$vessel==input$clientvessel)
  daterange = unique(as.character(r[,4]), incomparables = FALSE)
  selectInput("cdate", label=strong("Date Range"), choices = daterange, selected = daterange[1], multiple = FALSE, selectize = TRUE, width = "50%", size = NULL)
})

output$cType <- renderUI({ 
  r=xfile
  r =subset(r,r$vessel==input$clientvessel)
  r =subset(r,r$daterange == input$cdate)
  Type = unique(as.character(r[,14]), incomparables = FALSE)
  selectInput("cType", label=strong("Type"), choices = Type, selected = Type[1], multiple = FALSE, selectize = TRUE, width = "50%", size = NULL)
})

output$seastate1=renderUI({selectInput("seastate1",label = strong("Sea State"),choices = c(0,1,2,3,4,5,6,7,8),selected = 3,width = "15%", size = NULL)})

cd=reactive({
  validate(
    need(try(xfile),"Please Wait....")
  )
  y=xfile
  y =subset(y,y$vessel==input$clientvessel)
  y =subset(y,y$daterange == input$cdate)
  y =subset(y,y$Type == input$cType)
  y
})


Powerdata1<-reactive({
  y=cd()
  vpq=clientvesselpart()
  minD=unique(as.numeric(as.character(vpq[,4])), incomparables = FALSE)
  maxD = unique(as.numeric(vpq[,5]), incomparables = FALSE)
  maxs= unique(as.numeric(vpq[,6]), incomparables = FALSE)
  speed <- seq(10, maxs, by=1)
  draft <- c(minD:maxD)
  ss<-as.numeric(input$seastate1)
  m= matrix(NA,length(speed),length(draft))
  colnames(m)=draft
  row.names(m)=speed
  for(i in 1:length(draft) )
  {
    
    m[,i]=round(draft[i]^as.numeric(y$n1)*speed^as.numeric(y$n2)*(10-ss)^as.numeric(y$n3)*exp(as.numeric(y$k)),0)
  }
  
  m=as.data.frame(m)    
  
})


output$power_cal1 =renderDataTable({
  validate(
    need(try(Powerdata1()),"Please Wait....")
  )
  y=Powerdata1()
  datatable(y,class = 'cell-border stripe', options = list(searching = FALSE,paging = FALSE),colnames = c('Speed/Draft' = 1))%>%
    formatStyle(names(y),color="#212F3D",backgroundColor = "#ECF0F1")%>%formatStyle('Speed/Draft',  color = '#ECF0F1', backgroundColor = '#515A5A', fontWeight = 'bold')
})

output$speedpower1=renderPlotly({
  
  validate(
    need(try(Powerdata1()),"Please Wait....")
  )
  vpq=clientvesselpart()
  minD=unique(as.numeric(as.character(vpq[,4])), incomparables = FALSE)
  maxD = unique(as.numeric(vpq[,5]), incomparables = FALSE)
  maxs= unique(as.numeric(vpq[,6]), incomparables = FALSE)
  speed <- seq(10, maxs, by=1)
  draft <- c(minD:maxD)
  mydata=Powerdata1()
  c=length(draft)
  p=plot_ly(x=rownames(mydata),y=mydata[,1],name=draft[1],type='scatter',mode="lines+markers",line = list(shape = "linear"))
  for(i in 2: length(draft)){
    p=p%>%add_trace(x=rownames(mydata),y=mydata[,i],name=draft[i],type='scatter',mode="lines+markers",line = list(shape = "linear"))
    
  }
  p = p%>%layout(title = "Speed Power Curve",titlefont=c,xaxis=Speed_axis,yaxis=Power_axis,
                 legend = l,plot_bgcolor = "#FFFFFF",paper_bgcolor = "#FFFFFF",showlegend = TRUE)
})



# fo Calualtor ....
fodata2<-reactive({
  y=cd()
  vpq=clientvesselpart()
  minD=unique(as.numeric(as.character(vpq[,4])), incomparables = FALSE)
  maxD = unique(as.numeric(vpq[,5]), incomparables = FALSE)
  maxs= unique(as.numeric(vpq[,6]), incomparables = FALSE)
  speed <- seq(10, maxs, by=1)
  draft <- c(minD:maxD)
  ss<-as.numeric(input$seastate1)
  m= matrix(NA,length(speed),length(draft))
  colnames(m)=draft
  row.names(m)=speed
  for(i in 1:length(draft) )
  {
    
    m[,i]=round(draft[i]^as.numeric(y$n1)*speed^as.numeric(y$f2)*(10-ss)^as.numeric(y$f3)*exp(as.numeric(y$k1)),1)
  }
  m= as.data.frame(m)    
  
})


output$FO_cal1 <- renderDataTable({
  validate(
    need(try(fodata2()),"Please Wait....")
  )
  x=fodata2()
  datatable(x,class = 'cell-border stripe', options = list(searching = FALSE,paging = FALSE),colnames = c('Speed/Draft' = 1))%>%
    formatStyle(names(x),color="#212F3D",backgroundColor = "#ECF0F1")%>%formatStyle('Speed/Draft',  color = '#ECF0F1', backgroundColor = '#515A5A', fontWeight = 'bold')
})

output$speedFO1=renderPlotly({
  validate(
    need(try(fodata2()),"Please Wait....")
  )
  vpq=clientvesselpart()
  minD=unique(as.numeric(as.character(vpq[,4])), incomparables = FALSE)
  maxD = unique(as.numeric(vpq[,5]), incomparables = FALSE)
  maxs= unique(as.numeric(vpq[,6]), incomparables = FALSE)
  speed <- seq(10, maxs, by=1)
  draft <- c(minD:maxD)
  mydata=fodata2()
  c=length(draft)
  
  p=plot_ly(x=rownames(mydata),y=mydata[,1],name=draft[1],type='scatter',mode="lines+markers",line = list(shape = "linear"))
  for(i in 2: length(draft)){
    p=p%>%add_trace(x=rownames(mydata),y=mydata[,i],name=draft[i],type='scatter',mode="lines+markers",line = list(shape = "linear"))
    
  }
  sp=p%>%layout(title = "Speed FO/24 Hrs Curve",titlefont=c,xaxis=Speed_axis,yaxis=FO_axis,
                legend = l, plot_bgcolor = "#FFFFFF", paper_bgcolor = "#FFFFFF",showlegend = TRUE)
})

#DEViation sheet....

Ddata=reactive({
  y=DATA
  dataf=subset(y,y$Vessel.Name==input$clientvessel)
  dataf=subset(dataf,Report.Type == "NOON")
  VESSEL= dataf$Vessel.Name;DATE=dataf$Corrected.Date;DRAFT=round(dataf$DRAFT,2);SPEED=round(dataf$SOG,2);RPM=dataf$RPM;SLIP=dataf$SLIP;
  POWER=as.numeric(as.character(dataf$POWER.kW));FO=dataf$FO.per.24Hrs;SEASTATE=as.numeric(as.character(dataf$SEA.STATE));SFOC=as.numeric(as.character(dataf$SFOC))
  Noon=data.frame(VESSEL,DATE,SEASTATE,DRAFT,SPEED,RPM,SLIP,POWER,FO,SFOC)
  Noon=subset(Noon,as.Date(DATE,"%d-%m-%y")>=as.Date(input$dop[1],"%d-%m-%y") & as.Date(DATE,"%d-%m-%y")<=as.Date(input$dop[2],"%d-%m-%y"))
  Noon = subset(Noon,Noon$POWER > 0 & Noon$FO > 0 & Noon$SEASTATE >= 0  & Noon$SEASTATE < 9)
  Noon = subset(Noon,Noon$DRAFT > 6 & Noon$SPEED > 10 & Noon$SFOC > 150)
  Noon
}
)

DELDATA = reactive({
  DATAF=Ddata()
  x=cd()
  x$x2=as.character(x$x2)
  x$x2=as.Date(x$x2,"%d-%b-%Y")
  DATAF = subset(DATAF,as.Date(DATE,"%d-%m-%y") >= as.Date(x$x2,"%d-%m-%y"))
  DATAF = subset(DATAF,DATAF$POWER > 0 & DATAF$FO > 0)
  DATAF = subset(DATAF,DATAF$DRAFT > 6 & DATAF$SPEED > 10 & DATAF$SFOC > 150)
  
})

Devdata = reactive({  
  inFile <- input$file2
  
  if (is.null(inFile)){
    DELDATA()
  }
  else{     
    dataf=read.csv(inFile$datapath)
  }
})
DT=reactive({
  x=cd()
  y=Devdata()
  y$Pred_power=round((y$DRAFT^x$n1*y$SPEED^x$n2*(10-y$SEASTATE)^x$n3*exp(x$k)),2)
  y$Pred_FO=round((y$DRAFT^x$n1*y$SPEED^x$f2*(10-y$SEASTATE)^x$f3*exp(x$k1)),2)
  y$Power_DEV=  ((y$POWER-y$Pred_power)/y$POWER)
  y$FO_DEV = ((y$FO-y$Pred_FO)/y$FO)
  y$SFOC <- NULL
  y=y 
})

output$DDT=DT::renderDataTable({
  if(is.null(DT()))
    return(NULL)
  DT::datatable(DT(),class = 'cell-border stripe', options = list(pageLength = 150,searching = FALSE,paging = T,dom = "tip",
                                                                  scrollX=TRUE, scrollY =1200,
                                                                  scroller = T ),rownames = FALSE)%>%
    formatPercentage('FO_DEV', 1)%>%
    formatPercentage('Power_DEV', 1)%>%formatStyle(names(DT()),backgroundColor = "#ECF0F1",color="#212F3D")
  
})

output$Deviatepower = renderPlotly({
  dataf=DT()
  dataf$DATE=as.Date(dataf$DATE,"%d-%m-%y")
  p=plot_ly(data =dataf ,x = ~DATE,y= ~POWER,name="Observed Power",type="scatter",mode = "markers",marker=list(color= "#2C3E50",size=12))
  p=p%>%add_trace(data= dataf,x = ~DATE,y = ~Pred_power,name="Predicted Power",type="scatter",mode = "markers",marker=list(color= "#1FAD9F",size=12))
  
  p=p%>%layout(
    xaxis = list(title = "Date", titlefont =f, tickfont =f,gridcolor = "#FFFFFF",tickangle=0),
    yaxis = list(title="Power (kW)",titlefont = f, tickfont = f,gridcolor = "#ABB2B9"), 
    plot_bgcolor = "#FFFFFF",
    paper_bgcolor = "#FFFFFF",showlegend = TRUE,legend=l)
  
  
})
output$DeviateFO = renderPlotly({
  dataf=DT()
  dataf$DATE=as.Date(dataf$DATE,"%d-%m-%y")
  p=plot_ly(data = dataf,x = ~DATE,y= ~FO,name="Observed FO",type="scatter",mode = "markers",marker=list(color= "#2C3E50",size=12))
  p=p%>%add_trace(data= dataf,x = ~DATE,y = ~Pred_FO,name="Predicted FO",type="scatter",mode = "markers",marker=list(color= "#1FAD9F",size=12))
  
  
  p=p%>%layout(
    xaxis = list(title = "DATE", titlefont =f, tickfont =f,gridcolor = "#FFFFFF",tickangle=0),
    yaxis = list(title="FO/24 Hr(tonne)",titlefont = f, tickfont = f,gridcolor = "#ABB2B9"), 
    plot_bgcolor = "#FFFFFF",
    paper_bgcolor = "#FFFFFF",showlegend = TRUE,legend=l
  )
  
})


output$DPOWER <- renderValueBox({
  y=DT()
  DP = (y$POWER-y$Pred_power)/y$POWER
  Perror = abs(DP)
  DPower=round(mean(Perror)*100,digits = 1 )  
  valueBox(
    paste0(DPower, "%"), strong("Devition Power"), icon = icon("line-chart"),
    color = "teal"
  )}
)
output$DFO <- renderValueBox({
  y=DT()
  DF = (y$FO-y$Pred_FO)/y$FO
  Ferror = abs(DF)
  DFO=round((mean(Ferror)*100),digits = 1 )  
  valueBox(
    paste0(DFO, "%"), strong(" Deviation FO/24 Hr"), icon = icon("line-chart"),
    color = "teal"
  )
})
output$Ddata <- downloadHandler(
  filename = function() { 'DEVIATION.csv' }, content = function(file) {
    write.csv(DT(), file, row.names = FALSE)
  }
)

#Sea Trial .....

seavpq <- reactive({
  y=data.frame(read.csv("data/Vessel Detail.csv"))
  ff=input$clientvessel
  if(is.null(ff))
    return(NULL)
  y=subset(y,y$Vessel == ff)
  y
})


seatrial<- reactive({
  seatrial=data.frame(read.csv("data/Sea Trial Data.csv"))
  ff=input$clientvessel
  seatrial=subset(seatrial,seatrial$Vessel == ff)
})

ST=reactive({
  ST =input$Seatrial
  if (is.null(ST))
  {Seatrial=seatrial()}
  else{
    Seatrial=data.frame(read.csv(ST$datapath))}
})
output$TrialDraft <- renderUI({
  r=seavpq()
  if(is.null(r))
    return(NULL)
  TD=unique(as.numeric(as.character(r[,12])), incomparables = FALSE)
  numericInput("TD",label="Trial Draft (m)",value=TD)
})

SP= reactive({
  x=cd()
  SP=input$Seacon
  DT=input$TD
  y=ST()
  y$Prediction_Power= c(NA)
  M=y
  for(i in 1:length(y$Speed)){
    M[i,6]=round(DT^as.numeric(x$n1)*M$Speed[i]^as.numeric(x$n2)*(10-SP)^as.numeric(x$n3)*exp(as.numeric(x$k)),1)
  }
  M$Vessel=NULL
  M$Fleet=NULL
  M$Class=NULL
  M
  
})

output$STT=DT::renderDataTable({
  validate(
    need(try(SP()),"Please Wait....")
  )
  y=SP()
  colnames(y)=c("Speed (knots)","Seatrial Power (kW)","Estimated Power (kW)")
  DT::datatable(y,class = 'cell-border stripe', options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%formatStyle(names(y),backgroundColor = "#ECF0F1",color="#212F3D")
  
})
output$SPOWER=renderPlotly({
  validate(
    need(try(SP()),"Please Wait....")
  )
  p=plot_ly(SP(),x=~Speed,y=~Sea.Trial.Power,name ="Sea Trial power",type="scatter",mode = "lines+markers",line = list(shape = "linear",color="#2C3E50"),marker = list(color="#2C3E50"))
  p= p%>%add_trace(x=~Speed,y=~Prediction_Power,name="Pred_Power",type="scatter",mode = "lines+markers",line = list(shape = "linear",color="#1FAD9F"),marker = list(color="#1FAD9F"))
  p= p%>%layout(title = "Power Comparison",titlefont = c,
                xaxis = Speed_axis, yaxis= Power_axis ,showlegend=TRUE, legend=l,
           plot_bgcolor = "#FFFFFF", paper_bgcolor = "#FFFFFF")
  
})

output$text1 <- renderText({ 
  r = seavpq()
  
  paste0(input$clientvessel," is Built On : ",r$YOB)
})

output$text2 <- renderText({ 
  
  paste0("Analysis period:",input$cdate)
})

output$DError <- renderValueBox({
  y=SP()
  DP = (y$Sea.Trial.Power-y$Prediction_Power)/y$Sea.Trial.Power
  Perror = abs(DP)
  DError=round(mean(Perror)*100,digits = 1 )  
  valueBox(
    paste0(DError, "%"), strong("% Deviation "), icon = icon("line-chart"),
    color = "purple"
  )
})