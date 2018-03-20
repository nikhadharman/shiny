#ENGINE ANALYSIS ...................................

#enginedata =readWorksheetFromFile( "data/ENGINE ANALYSIS.xlsx", sheet = 1, header = FALSE ) 
enginedata =readWorksheetFromFile( "data/Engine Analysis_test.xlsx", sheet = 1, header = TRUE ) 
shoptrial = readWorksheetFromFile( "data/SHOPTRIAL DATA.xlsx", sheet = 1, header = TRUE ) # read excel file 
#ENGINE TEMP @ CYLINDER......................



output$enginedate <- renderUI({ 
  r=enginedata
  vessel = input$engineVessel
  r=subset(r,r[,1]== vessel)
  r <- r[order(as.Date(r$Date,"%d-%m-%y"),decreasing = T),]
  
  date_List = unique(as.Date(r[,6],"%d-%m-%y"), incomparables = FALSE)
  
  selectInput("enginedate", label="Test Date Selection", choices = date_List, selected = as.Date(date_List[1],"%d-%m-%y"), multiple = FALSE, selectize = TRUE, width = "50%", size = NULL)
  
})

output$enginevessel <- renderUI({ 
  r=enginedata
  Vessel_List = unique(as.character(r[,1]), incomparables = FALSE)
  selectInput("engineVessel", label="Vessel", choices = Vessel_List, selected = "STRATEGIC ALLIANCE", multiple = FALSE, selectize = TRUE, width = "50%", size = NULL)
  
})

output$enginemonth <- renderUI({
  
  numericInput("monthno",label = "Enter Number of Months",value = 4,width = "50%",min = 1,max = 12)
  
  
})

enginedatatable = reactive({
  r=enginedata
  vessel = input$engineVessel
  r=subset(r,r[,1]== vessel)
  date = input$enginedate
  r$Date =  as.Date(r$Date,"%d-%m-%y")
  r = subset(r,Date <= date)
  r <- r[order(r$Date,decreasing = T),] 
  r = head(r,input$monthno)
  r
  
})


shoptrialdata = reactive({
  r=shoptrial
  vess = input$engineVessel
  r=subset(r,r[,3]== vess)
  r
  
})

output$vessel_name = renderText({
  paste("Vessel Selection :",input$engineVessel)
})
output$Engine_Date=renderText({
  paste("Date:",input$enginedate)
})
output$vessel_name1=renderText({
  paste("Vessel Selection :",input$engineVessel)
})
output$Engine_Date1=renderText({
  paste("Date:",input$enginedate)
})
output$vessel_name11=renderText({
  paste("Vessel Name :",input$engineVessel)
})
output$Engine_Date11=renderText({
  paste("Date:",input$enginedate)
})
#Texh---------------------------------------------------------------------------------------------------------------------------------------------

exhaustdata= reactive({
  r = enginedatatable()
  Exhaust.Temp = c(r$ExTemp1[1],r$ExTemp2[1],r$ExTemp3[1],r$ExTemp4[1],r$ExTemp5[1],r$ExTemp6[1])
  Cylinder = c(01:6)
  
  tableET = data.frame(Cylinder,Exhaust.Temp)
  tableET=subset(tableET, !is.na(tableET$Exhaust.Temp))
  tableET
})

output$exhausttemp = renderPlotly({
  validate(
    need(try(exhaustdata()),"Press Wait or NO DATA AVAILABLE..........")
  )
  
  m = exhaustdata()
  m=subset(m, !is.na(m$Exhaust.Temp))
  x = enginedatatable()
  avg = x$ExTempAvg[1]
  p <- plot_ly(m,
               x =~Cylinder ,
               y = ~Exhaust.Temp,
               name = "Cyl Exhaust Temp",
               type = "bar",
               marker=list(color="#9B59B6"))%>%
    add_trace(x=c(head(m$Cylinder,1),tail(m$Cylinder,1)),y=c(avg,avg),name="Average Line",type="scatter",mode="lines+markers",marker= list(color="#FE0707",size=8,opacity = 0),showlegend = FALSE)
  p=p%>%layout(title="Texh", titlefont = s,xaxis=list(title="Cylinder No", titlefont = s, tickfont = s,gridcolor = "#FFFFFF"),yaxis=list(title="Exh. Temp at Cyl. out(deg.C)", titlefont = s, tickfont = s,gridcolor = "#E5E7E9"),showlegend=F,plot_bgcolor = "#FFFFFF",
               paper_bgcolor = "#FFFFFF")
  p
})

output$ETtable1 = renderDataTable({
  validate(
    need(try(exhaustdata()),"Press Wait or NO DATA AVAILABLE..........")
  )
  m = exhaustdata()
  
  normal = 400
  r=subset(m, !is.na(m$Exhaust.Temp))
  mean = NA
  m$mean_value <- mean
  m$diff <- mean-m$Exhaust.Temp
  m$result <- ifelse(m$Exhaust.Temp<normal, "Normal", "High Value")
  
  
  m[1,3] = round(mean(as.numeric(m$Exhaust.Temp ),na.rm = TRUE),digits = 1)
  m[,4] = round((m[1,3]-m$Exhaust.Temp ),digits = 1)
  
  names(m)<-c("Cylinder Number","Measured Value (deg.C)","Average Value","(Measured)-(Average)","Result")
  
  
  
  datatable(m, options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%
    formatStyle(names(m),color="#000", backgroundColor = "white")
  
  
  
})
output$ETtable2 = renderDataTable({
  validate(
    need(try(exhaustdata()),"Press Wait or NO DATA AVAILABLE..........")
  )
  m=exhaustdata()
  mean = NA
  devtableET = data.frame(mean,abs(mean-m$Exhaust.Temp))
  names(devtableET)<-c("Average Value","(Measured)-(Average)")
  devtableET
  devtableET[1,1] = round(mean(as.numeric(m$Exhaust.Temp),na.rm = TRUE),digits = 1)
  devtableET[,2] = round((devtableET[1,1]-m$Exhaust.Temp),digits = 1)
  
  datatable(devtableET, options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%
    formatStyle(names(devtableET),color="#000",backgroundColor = "white")
  
  
  
  
})


output$ETtable3 = renderDataTable({
  validate(
    need(try(exhaustdata()),"Press Wait or NO DATA AVAILABLE..........")
  )
  m=exhaustdata()
  normal = 400
  r=subset(m, !is.na(m$Exhaust.Temp))
  y=nrow(r)
  result = NA *m$Exhaust.Temp
  resulttableET = data.frame(m$Cylinder,result)
  names(resulttableET)<-c("Cylinder Number","Result")
  for(i in 1:y) {
    x = m[i,2]
    if (x< normal){ resulttableET[i,2] = "Normal"}
    else{resulttableET[i,2] = "High Value!"}
    resulttableET
  }
  
  datatable(resulttableET, options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%
    formatStyle(names(resulttableET),color="#000",backgroundColor = "white")
  
  
}) 

#PUMP MARK

pumpmark= reactive({
  r = enginedatatable()
  Pump.Mark = c(r$PumpMark1[1],r$PumpMark2[1],r$PumpMark3[1],r$PumpMark4[1],r$PumpMark5[1],r$PumpMark6[1])
  Cylinder = c(01:6)
  
  
  tablePM = data.frame(Cylinder,Pump.Mark)
  tablePM=subset(tablePM, !is.na(tablePM$Pump.Mark))
  tablePM
  
})

output$pumpmark = renderPlotly({
  validate(
    need(try(pumpmark(),enginedatatable()),"Press Wait or NO DATA AVAILABLE..........")
  )
  m=pumpmark()
  m=subset(m, !is.na(m$Pump.Mark))
  x = enginedatatable()
  avg = x$PumpMarkAvg[1]
  p <- plot_ly(m,
               x =~Cylinder ,
               y = ~Pump.Mark,
               name = "Pump Mark",
               type = "bar",
               marker=list(color="#4DD0E1"))   
  p=p%>%layout(title="Pump Mark", titlefont = s,xaxis=list(title="Cylinder No", titlefont = s, tickfont = s,gridcolor = "#FFFFFF"),yaxis=list(title="Pump Mark", titlefont = s, tickfont = s,gridcolor = "#E5E7E9"),showlegend=F,plot_bgcolor = "#FFFFFF",
               paper_bgcolor = "#FFFFFF")%>%
    add_trace(x=c(head(m$Cylinder,1),tail(m$Cylinder,1)),y=c(avg,avg),name="Average Line",type="scatter",mode="lines+markers",marker= list(color="#FE0707",size=8,opacity = 0),showlegend = FALSE)
  
  p
})

output$PMtable1 = renderDataTable({
  validate(
    need(try(pumpmark()),"Press Wait or NO DATA AVAILABLE..........")
  )
  m=pumpmark()
  
  normal = 55
  r=subset(m, !is.na(m$Pump.Mark))
  mean = NA
  m$mean_value <- mean
  m$diff <- mean-m$Pump.Mark
  m$result <- ifelse(m$Pump.Mark<normal, "Normal", "High Value")
  
  
  m[1,3] = round(mean(as.numeric(m$Pump.Mark ),na.rm = TRUE),digits = 1)
  m[,4] = round((m[1,3]-m$Pump.Mark ),digits = 1)
  
  names(m)<-c("Cylinder Number","Measured Value","Average Value","(Measured)-(Average)","Result")
  
  
  
  datatable(m, options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%
    formatStyle(names(m),color="#000",backgroundColor = "white")
  
  
})
output$PMtable2 = renderDataTable({
  validate(
    need(try(pumpmark()),"Press Wait or NO DATA AVAILABLE..........")
  )
  m=pumpmark()
  mean = NA
  devtablePM = data.frame(mean,abs(mean-m$Pump.Mark))
  names(devtablePM)<-c("Average Value","(Measured)-(Average)")
  devtablePM
  devtablePM[1,1] = round(mean(as.numeric(m$Pump.Mark),na.rm = TRUE),digits = 1)
  devtablePM[,2] = round((devtablePM[1,1]-m$Pump.Mark),digits = 1)
  
  datatable(devtablePM, options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%
    formatStyle(names(devtablePM),color="#000",backgroundColor = "white")
}) 

output$PMtable3 = renderDataTable({
  validate(
    need(try(pumpmark()),"Press Wait or NO DATA AVAILABLE..........")
  )
  m=pumpmark()
  normal = 55
  r=subset(m, !is.na(m$Pump.Mark))
  y=nrow(r)
  result = NA * m$Pump.Mark
  resulttablePM = data.frame(m$Cylinder,result)
  names(resulttablePM)<-c("Cylinder Number","Result")
  for(i in 1:y) {
    x = m[i,2]
    if (x< normal){ resulttablePM[i,2] = "Normal"}
    else{resulttablePM[i,2] = "High Value!"}
    resulttablePM
  }
  datatable(resulttablePM, options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%
    formatStyle(names(resulttablePM),color="#000",backgroundColor = "white")
  
})

# COMPRESSION PRESSURE
pcomp= reactive({
  r = enginedatatable()
  PComp = c(r$PressComp1[1],r$PressComp2[1],r$PressComp3[1],r$PressComp4[1],r$PressComp5[1],r$PressComp6[1])
  Cylinder = c(01:6)
  
  table = data.frame(Cylinder,PComp)
  table=subset(table, !is.na(table$PComp))
  table
}) 

output$PComp = renderPlotly({
  validate(
    need(try(pcomp(),enginedatatable()),"Press Wait or NO DATA AVAILABLE..........")
  )
  m= pcomp()
  m=subset(m, !is.na(m$PComp))
  x = enginedatatable()
  avg = x$PressCompAvg[1]
  
  p <- plot_ly(m,
               x =~Cylinder ,
               y = ~PComp ,
               type = "bar",
               marker=list(color="#9334E6"))%>%
    add_trace(x=c(head(m$Cylinder,1),tail(m$Cylinder,1)),y=c(avg,avg),name="Average Line",type="scatter",mode="lines+markers",marker= list(color="#FE0707",size=8,opacity = 0),showlegend = FALSE)
  p=p%>%layout(title="PComp", titlefont = s,xaxis=list(title="Cylinder No", titlefont = s, tickfont = s,gridcolor = "#FFFFFF"),yaxis=list(title="Compression Pressure(bar)", titlefont = s, tickfont = s,gridcolor = "#E5E7E9"),showlegend=F,plot_bgcolor = "#FFFFFF",
               paper_bgcolor = "#FFFFFF")
  
  p
})

output$PCtable1 = renderDataTable({
  m= pcomp()
  
  normal = 75
  r=subset(m, !is.na(m$PComp))
  mean = NA
  m$mean_value <- mean
  m$diff <- mean-m$PComp
  m$result <- ifelse(m$PComp<normal, "Normal", "High Value")
  
  
  m[1,3] = round(mean(as.numeric(m$PComp ),na.rm = TRUE),digits = 1)
  m[,4] = round((m[1,3]-m$PComp ),digits = 1)
  
  names(m)<-c("Cylinder Number","Measured Value (bar)","Average Value","(Measured)-(Average)","Result")
  
  
  
  datatable(m, options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%
    formatStyle(names(m),color="#000",backgroundColor = "white")
  
})
output$PCtable2 = renderDataTable({
  m= pcomp()
  x = NA
  devtablePC = data.frame(x,abs(x-m$PComp ))
  names(devtablePC)<-c("Average Value","(Measured)-(Average)")
  devtablePC
  devtablePC[1,1] = round(mean(as.numeric(m$PComp ),na.rm = TRUE),digits = 1)
  devtablePC[,2] = round((devtablePC[1,1]-m$PComp ),digits = 1)
  devtablePC
  
  datatable(devtablePC, options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%
    formatStyle(names(devtablePC),color="#000",backgroundColor = "white")
})

output$PCtable3 = renderDataTable({
  m= pcomp()
  normal = 75
  r=subset(m, !is.na(m$PComp))
  y=nrow(r)
  result = NA * m$PComp
  resulttablePC = data.frame(m$Cylinder,result)
  names(resulttablePC)<-c("Cylinder Number","Result")
  for(i in 1:y) {
    x = m[i,2]
    if (x< normal){ resulttablePC[i,2] = "Normal"}
    else{resulttablePC[i,2] = "High Value!"}
    resulttablePC
  }
  
  
  datatable(resulttablePC, options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%
    formatStyle(names(resulttablePC),color="#000",backgroundColor = "white")
})

# MAXIMUM PRESSURE
pmax= reactive({
  r = enginedatatable()
  
  MaxPress = c(r$MaxPress1[1],r$MaxPress2[1],r$MaxPress3[1],r$MaxPress4[1],r$MaxPress5[1],r$MaxPress6[1])
  Cylinder = c(01:6)
  
  table = data.frame(Cylinder,MaxPress)
  table=subset(table, !is.na(table$MaxPress))
  table
}) 

output$Pmax = renderPlotly({
  m= pmax()
  m=subset(m, !is.na(m$MaxPress))
  x = enginedatatable()
  avg = x$MaxPressAvg[1]
  p <- plot_ly(m,
               x =~Cylinder ,
               y = ~MaxPress ,
               type = "bar",
               marker=list(color="#27E474"))%>%
    add_trace(x=c(head(m$Cylinder,1),tail(m$Cylinder,1)),y=c(avg,avg),name="Average Line",type="scatter",mode="lines+markers",marker= list(color="#FE0707",size=8,opacity = 0),lines = list(color = "#085EA2"),showlegend = FALSE)
  p=p%>%layout(title="Pmax", titlefont = s,xaxis=list(title="Cylinder No", titlefont = s, tickfont = s,gridcolor = "#FFFFFF"),yaxis=list(title="Maximum Pressure(bar)", titlefont = s, tickfont = s,gridcolor = "#E5E7E9"),showlegend=F,plot_bgcolor = "#FFFFFF",
               paper_bgcolor = "#FFFFFF")
  
  p
})

output$MPtable1 = renderDataTable({
  m= pmax()
  
  normal = 110
  r=subset(m, !is.na(m$MaxPress))
  mean = NA
  m$mean_value <- mean
  m$diff <- mean-m$MaxPress
  m$result <- ifelse(m$MaxPress<normal, "Normal", "High Value")
  
  
  m[1,3] = round(mean(as.numeric(m$MaxPress ),na.rm = TRUE),digits = 1)
  m[,4] = round((m[1,3]-m$MaxPress ),digits = 1)
  
  names(m)<-c("Cylinder Number","Measured Value (bar)","Average Value","(Measured)-(Average)","Result")
  
  
  
  datatable(m, options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%
    formatStyle(names(m),color="#000",backgroundColor = "white")
  
  
})
output$MPtable2 = renderDataTable({
  m= pmax()
  mean = NA
  devtableMP = data.frame(mean,abs(mean-m$MaxPress))
  names(devtableMP)<-c("Average Value","(Measured)-(Average)")
  devtableMP
  devtableMP[1,1] = round(mean(as.numeric(m$MaxPress ),na.rm = TRUE),digits = 1)
  devtableMP[,2] = round((devtableMP[1,1]-m$MaxPress ),digits = 1)
  
  
  datatable(devtableMP, options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%
    formatStyle(names(devtableMP),color="#000",backgroundColor = "white")
})

output$MPtable3 = renderDataTable({
  m= pmax()
  normal = 110
  r=subset(m, !is.na(m$MaxPress))
  y=nrow(r)
  result = NA * m$MaxPress
  resulttableMP = data.frame(m$Cylinder,result)
  names(resulttableMP)<-c("Cylinder Number","Result")
  for(i in 1:y) {
    x = m[i,2]
    if (x< normal){ resulttableMP[i,2] = "Normal"}
    else{resulttableMP[i,2] = "High Value!"}
    resulttableMP
  }
  
  
  
  datatable(resulttableMP, options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%
    formatStyle(names(resulttableMP),color="#000",backgroundColor = "white")
})


#TC SPEED VS ENGINE SPEED------------------------------------------------------------------------------------------------------------------------



output$graphTCEn = renderPlotly({ 
  
  
  mydata = enginedatatable()
  STdata = shoptrialdata()
  
  
  x= STdata$engine.speed
  y=STdata$TCspeed
  testx = seq(from=min(STdata$engine.speed,na.rm = T), to=max(STdata$engine.speed,na.rm=T), length.out= 30)
  
  reg = lm(y ~ poly(x, 2, raw=TRUE))
  
  n1=as.numeric(reg$coefficients[3])
  n2=as.numeric(reg$coefficients[2])
  k=as.numeric(reg$coefficients[1])
  
  testy = n1*testx*testx + n2*testx + k
  
  p <- plot_ly()#x = as.numeric(mydata$RPM[1]), y = as.numeric(mydata$TCrpm[1]), type='scatter' ,
  # mode = "markers",marker=list(size=12),name = mydata$Date[1], showlegend = TRUE)
  
  #add_trace(x = Cylinder,
  #  y = Avg, mode = "lines")
  
  
  for(i in 1 :input$monthno){
    p=p%>%add_trace(mydata,x = as.numeric(mydata$RPM[i]), y = as.numeric(mydata$TCrpm[i]), mode = "markers",marker=list(size=12),
                    name = mydata$Date[i], showlegend = TRUE)
  }
  #p=p%>%add_trace(mydata,x = as.numeric(mydata$RPM[3]), y = as.numeric(mydata$TCrpm[3]), mode = "markers",marker=list(size=12),
  #               name = mydata$Date[3], showlegend = TRUE)
  #p=p%>%add_trace(mydata,x = as.numeric(mydata$RPM[4]), y = as.numeric(mydata$TCrpm[4]),  mode = "markers",marker=list(size=12),
  #                name = mydata$Date[4], showlegend = TRUE)
  p = p%>%add_trace(x =  x, y =  y,  mode = "markers",marker=list(size=8,color = "#640017"), name = "Shoptrial data", showlegend = TRUE)
  
  p = p%>%add_trace(p,x=testx,y = testy, type='scatter' , mode = "lines+markers", line=list(shape="spline",color = "#640017"),marker=list(opacity=0),name = "reg",showlegend = F)
  
  p <-p%>%layout(title="T/C Speed Vs Engine Speed", titlefont = s, xaxis = list(title="Engine Speed (rpm)", titlefont = s, tickfont = s,gridcolor = "#FFFFFF"),yaxis=list(title="Corrected T/C Speed (rpm)", titlefont = s, tickfont = s,gridcolor = "#E5E7E9"),
                 showlegend=T,plot_bgcolor = "#FFFFFF",
                 paper_bgcolor = "#FFFFFF",legend=l)  
  p
  
})

output$graphTCEn1 = renderPlotly({ 
  
  
  mydata = enginedatatable()
  STdata = shoptrialdata()
  c = input$monthno
  
  x= STdata$engine.speed
  y=STdata$TCspeed
  reg = lm(y ~ poly(x, 2, raw=TRUE))
  
  n1=as.numeric(reg$coefficients[3])
  n2=as.numeric(reg$coefficients[2])
  k=as.numeric(reg$coefficients[1])
  
  tdate = rep(0,c)
  testx = rep(0,c)
  testy = rep(0,c)
  TCspeed = rep(0,c)
  deviation = rep(0,c)
  for(i in 1 : input$monthno){
    
    
    tdate[i] = mydata$Date[i]
    testx[i] = mydata$RPM[i]
    testy[i] = n1*testx[i]*testx[i] + n2*testx[i] + k
    TCspeed[i] = mydata$TCrpm[i]
    deviation[i] = round((TCspeed[i]-testy[i])*100/ testy[i],2)
  }
  
  df = data.frame(tdate,deviation)
  
  
  p <- plot_ly(df,x = as.Date(tdate) , y = deviation, type='scatter' ,
               mode = "lines+markers",marker=list(size=12), showlegend = TRUE)
  
  #add_trace(x = Cylinder,
  #  y = Avg, mode = "lines")
  
  
  
  p <-p%>%layout(title="Deviation %", titlefont = s, xaxis = list(title="Month", titlefont = s, tickfont = s,gridcolor = "#FFFFFF"),yaxis=list(title="% deviation", titlefont = s,ticksuffix = "%", tickfont = s,gridcolor = "#E5E7E9"),
                 showlegend=F,plot_bgcolor = "#FFFFFF",
                 paper_bgcolor = "#FFFFFF",legend=l)  
  p
  
})

output$shoptrialTCEn = renderDataTable({ 
  
  STdata = shoptrialdata()
  
  x= STdata$engine.speed
  y=STdata$TCspeed
  
  
  table = data.frame(x,y)
  names(table)<-c("Engine Speed","T/C Speed")
  
  datatable(table, options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%
    formatStyle(names(table),color="#000",backgroundColor = "white")
  
})

output$historicTCEn = renderDataTable({ 
  mydata = enginedatatable()
  
  dates = mydata$Date
  #names = c("Latest Data","Others1","Others2","Others3")
  historicx = mydata$RPM
  historicy = round(mydata$TCrpm,1)
  
  table = data.frame(dates,historicx,historicy)
  names(table) = c("Date of Measurement","Engine Speed","T/C Speed")
  
  datatable(table, options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%
    formatStyle(names(table),color="#000",backgroundColor = "white")
  
})

# Load Diagram------------------

output$graph_Load_diagram = renderPlotly({ 
  
  
  mydata = enginedatatable()
  STdata = shoptrialdata()
  
  
  ddd= STdata$ld_rpm
  yyy1=STdata$ld_load1
  yyy2=STdata$ld_load2
  
  
  
  p = plot_ly( )#x = as.numeric(mydata$RPM[1]), y = as.numeric(mydata$Engine.Load[1]), type='scatter' ,
  #mode = "markers",marker=list(size=12),name = mydata$Date[1], showlegend = TRUE)
  for(i in 1:input$monthno){
    
    p = p%>%add_trace(x = as.numeric(mydata$RPM[i]), y = as.numeric(mydata$Engine.Load[i]),  mode = "markers",marker=list(size=12),
                      name =  mydata$Date[i], showlegend = TRUE)
  }
  # p = p%>%add_trace(x = as.numeric(mydata$RPM[3]), y = as.numeric(mydata$Engine.Load[3]),  mode = "markers",marker=list(size=12),
  #                   name =  mydata$Date[3], showlegend = TRUE)
  # p = p%>%add_trace(x = as.numeric(mydata$RPM[4]), y = as.numeric(mydata$Engine.Load[4]), mode = "markers",marker=list(size=12),
  #                   name =  mydata$Date[4], showlegend = TRUE)
  p = p%>%add_trace(x = as.numeric(STdata$ld_rpm), y = as.numeric(STdata$ld_load1), type='scatter',  mode = "lines+markers",line=list(shape="spline",smoothing = 0.5,color = "#640017") ,marker=list(opacity=0),name = "reg",showlegend = F)
  
  p = p%>%add_trace(x = as.numeric(STdata$ld_rpm2),y = as.numeric(STdata$ld_load2), type='scatter',  mode = "lines+markers",line=list(shape="spline",color = "#640017") ,marker=list(opacity=0),name = "reg",showlegend = F)
  
  p = p%>%add_trace(x = as.numeric(STdata$ld_rpm3),y = as.numeric(STdata$ld_load3), type='scatter',  mode = "lines+markers",line=list(shape="spline",smoothing = 1.3,color = "#640017") ,marker=list(opacity=0),name = "reg",showlegend = F)
  
  p =p%>%layout(title="Load Diagram", titlefont = s, xaxis = list(title="Engine Speed (rpm)", titlefont = s, tickfont = s,gridcolor = "#FFFFFF",range = seq(70,135,by=5),linecolor='#636363',
                                                                  linewidth=2),yaxis=list(title="Engine Load (%)", titlefont = s, tickfont = s,gridcolor = "#E5E7E9",range = seq(40,110,by=10),linecolor='#636363',
                                                                                          linewidth=2),
                showlegend=T,plot_bgcolor = "#FFFFFF",
                paper_bgcolor = "#FFFFFF",legend=l)  
  
  
})


output$historical_Daigram = renderDataTable({ 
  mydata = enginedatatable()
  
  dates = mydata$Date
  #names = c("Latest Data","Others1","Others2","Others3")
  historicx = mydata$RPM
  historicy = round(mydata$Engine.Load,0)
  
  table = data.frame(dates,historicx,historicy)
  names(table) = c("Date of Measurement","Engine RPM","Engine Load")
  
  datatable(table, options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%
    formatStyle(names(table),color="#000",backgroundColor = "white")
  
})
#Torque rich index....................................

output$graph_TorqueRichIndex_diagram = renderPlotly({ 
  
  mydata = enginedatatable()
  daterange = c(as.Date(as.character(mydata$Date[1])),as.Date(as.character(mydata$Date[4])))
  referenceline = 1.00
  Cautionline = 1.10
  alarmline = 1.20
  Cautionline1 = 0.82
  alarmline1 = 0.80
  
  
  
  p = plot_ly( x = as.Date(as.character(mydata$Date[1])), y = as.numeric(mydata$TorqueRichIndex[1]),  type='scatter',
               mode = "markers",marker=list(size=12),name = mydata$Date[1], showlegend = TRUE)
  for(i in 2 : input$monthno){
    p = p%>%add_trace(x = as.Date(as.character(mydata$Date[i])), y = as.numeric(mydata$TorqueRichIndex[i]),  type='scatter',
                      mode = "markers",marker=list(size=12),name = mydata$Date[i], showlegend = TRUE)
  }
  #p = p%>%add_trace(x = as.Date(as.character(mydata$Date[3])), y = as.numeric(mydata$TorqueRichIndex[3]),  type='scatter',
  #                  mode = "markers",marker=list(size=12),name = mydata$Date[3], showlegend = TRUE)
  #p = p%>%add_trace(x = as.Date(as.character(mydata$Date[4])), y = as.numeric(mydata$TorqueRichIndex[4]),  type='scatter',
  #                  mode = "markers",marker=list(size=12),name = mydata$Date[4], showlegend =TRUE)
  
  p = p%>%add_trace(x = daterange, y = c(referenceline,referenceline), type='scatter', 
                    mode="lines+markers",marker= list(color="	#4B0082",size=2),name = "Reference Line", showlegend =TRUE)
  p =p%>% add_trace(x = daterange, y = c(Cautionline, Cautionline), type='scatter',
                    mode="lines+markers",marker= list(color="#7FFF00",size=2),name = "Caution Line", showlegend =TRUE)
  p = p%>%add_trace(x = daterange, y = c(alarmline, alarmline), type='scatter',
                    mode="lines+markers",marker= list(color="#DC143C",size=2),name = "Alarm Line", showlegend =TRUE)
  p = p%>%add_trace(x = daterange, y = c(Cautionline1, Cautionline1), type='scatter',
                    mode="lines+markers",marker= list(color="#7FFF00",size=2),name = "Caution Line", showlegend =FALSE)
  p = p%>%add_trace(x = daterange, y = c(alarmline1, alarmline1), type='scatter',
                    mode="lines+markers",marker= list(color="#DC143C",size=2),name = "Alarm Line", showlegend =FALSE)
  
  p =p%>%layout(title="Trend of Torque Rich", titlefont = s, xaxis = list(title="Date", titlefont = s, tickfont = s,gridcolor = "#FFFFFF",linecolor='#636363',
                                                                          linewidth=2),yaxis=list(title="Torque Rich", titlefont = s, tickfont = s,gridcolor = "#E5E7E9",range = seq(0,1.5,by=0.2),linecolor='#636363',
                                                                                                  linewidth=2),
                showlegend=T,plot_bgcolor = "#FFFFFF",
                paper_bgcolor = "#FFFFFF",legend=l)  
  
})


output$Torquerich_Daigram = renderDataTable({ 
  mydata = enginedatatable()
  dates = mydata$Date
  #names = c("Latest Data","Others1","Others2","Others3")
  historicx =round( mydata$TorqueRichIndex,4)
  historicy = mydata$Condition
  result = NA
  
  for(i in 1:length (dates)){
    if(historicx [i] >= 0.9000  )
    {
      results="NORMAL"
      
    } else if(historicx [i] >= 0.8000 & historicx [i] <= 0.9000)
      
    { 
      results="Light Condition"
      
    } else 
      
    { 
      results="Very Light Condition"
      
    } 
    result[i]=results
  }
  table = data.frame(dates,historicx,historicy,result)
  names(table) = c("Date Of Measurement","Torque Rich Index","Condition","Result")
  
  datatable(table, options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%
    formatStyle(names(table),color="#000",backgroundColor = "white")
  
})

#PUMP MARK VS ENGINE SPEED.....................................

output$graphPMEn = renderPlotly({ 
  
  
  mydata=enginedatatable()
  STdata = shoptrialdata()
  
  
  x= STdata$engine.speed
  y=STdata$pumpmark
  testx = seq(from=min(STdata$engine.speed,na.rm = T), to=max(STdata$engine.speed,na.rm=T), length.out= 30)
  
  reg = lm(y ~ poly(x, 2, raw=TRUE))
  
  n1=as.numeric(reg$coefficients[3])
  n2=as.numeric(reg$coefficients[2])
  k=as.numeric(reg$coefficients[1])
  
  testy = round(testx^2*n1+testx*n2+k,2)
  
  
  p <- plot_ly( )#x = as.numeric(mydata$RPM[1]), y = as.numeric(mydata$PumpMarkAvg[1]), type='scatter' ,
  # mode = "markers",marker=list(size=12),name = mydata$Date[1], showlegend = TRUE)
  
  #add_trace(x = Cylinder,
  #  y = Avg, mode = "lines")
  
  for(i in 1 : input$monthno){
    p=p%>%add_trace(x =  as.numeric(as.character(mydata$RPM[i])), y =  as.numeric(as.character(mydata$PumpMarkAvg[i])), mode = "markers",marker=list(size=12),
                    name = mydata$Date[i], showlegend = TRUE)
  }
  #p=p%>%add_trace(x =  as.numeric(as.character(mydata$RPM[3])), y =  as.numeric(as.character(mydata$PumpMarkAvg[3])), mode = "markers",marker=list(size=12),
  #                name = mydata$Date[3], showlegend = TRUE)
  #p=p%>%add_trace(x =  as.numeric(as.character(mydata$RPM[4])), y =  as.numeric(as.character(mydata$PumpMarkAvg[4])), mode = "markers",marker=list(size=12),
  #                name =mydata$Date[4], showlegend = TRUE)
  p =p%>% add_trace(x =  x, y =  y,  mode = "markers",marker=list(size=8,color = "#640017"),
                    name = "Shoptrial data", showlegend = TRUE)
  p= p%>%add_trace(x=testx,y = testy, type="scatter" ,mode = "lines+markers", line=list(shape="spline",color = "#640017"),marker=list(opacity=0),name = "reg",showlegend = F)
  
  p <-p%>%layout(title="Pump Mark Vs Engine Speed", titlefont = s, xaxis = list(title="Engine Speed (rpm)", titlefont = s, tickfont = s,gridcolor = "#FFFFFF"),yaxis=list(title="Corrected Pump Mark", titlefont = s, tickfont = s,gridcolor = "#E5E7E9"),
                 showlegend=T,plot_bgcolor = "#FFFFFF",
                 paper_bgcolor = "#FFFFFF",legend=l)  
  p
  
})

output$graphPMEn1 = renderPlotly({ 
  
  
  mydata = enginedatatable()
  STdata = shoptrialdata()
  
  
  x= STdata$engine.speed
  y=STdata$pumpmark
  reg = lm(y ~ poly(x, 2, raw=TRUE))
  
  n1=as.numeric(reg$coefficients[3])
  n2=as.numeric(reg$coefficients[2])
  k=as.numeric(reg$coefficients[1])
  c = input$monthno
  tdate = rep(0,c)
  testx = rep(0,c)
  testy = rep(0,c)
  Pmark = rep(0,c)
  deviation = rep(0,c)
  for(i in 1 : input$monthno){
    
    
    tdate[i] = mydata$Date[i]
    testx[i] = mydata$RPM[i]
    testy[i] = n1*testx[i]*testx[i] + n2*testx[i] + k
    Pmark[i] = mydata$PumpMarkAvg[i]
    deviation[i] = round((Pmark[i] - testy[i])*100/ testy[i],2)
  }
  
  df = data.frame(tdate,deviation)
  
  
  p <- plot_ly(df,x = as.Date(tdate) , y = deviation, type='scatter' ,
               mode = "lines+markers",marker=list(size=12), showlegend = TRUE)
  
  #add_trace(x = Cylinder,
  #  y = Avg, mode = "lines")
  
  
  
  p <-p%>%layout(title="Deviation %", titlefont = s, xaxis = list(title="Month", titlefont = s, tickfont = s,gridcolor = "#FFFFFF"),yaxis=list(title="% deviation", titlefont = s,ticksuffix = "%", tickfont = s,gridcolor = "#E5E7E9"),
                 showlegend=F,plot_bgcolor = "#FFFFFF",
                 paper_bgcolor = "#FFFFFF",legend=l)  
  p
  
})

output$shoptrialPMEn = renderDataTable({ 
  
  STdata = shoptrialdata()
  
  x= STdata$engine.speed
  y=STdata$pumpmark
  
  
  table = data.frame(x,y)
  names(table)<-c("Engine Speed","Pump Mark")
  
  datatable(table, options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%
    formatStyle(names(table),color="#000",backgroundColor = "white")
  
})

output$historicPMEn = renderDataTable({ 
  mydata=enginedatatable()
  
  dates = mydata$Date
  names = c("Latest Data","Others1","Others2","Others3")
  historicx = mydata$RPM
  historicy = round(mydata$PumpMarkAvg,1)
  
  table = data.frame(dates,historicx,historicy)
  names(table) = c("Date of Measurement","Engine Speed","Pump Mark")
  
  datatable(table, options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%
    formatStyle(names(table),color="#000",backgroundColor = "white")
  
})

#Pscav VS T/C SPEED

output$graphPsTC = renderPlotly({ 
  
  
  mydata=enginedatatable() 
  STdata = shoptrialdata()
  
  
  x= STdata$TCspeed
  y=STdata$pscav
  testx = seq(from=min(STdata$TCspeed,na.rm = T), to=max(STdata$TCspeed,na.rm=T), length.out= 30)
  
  reg = lm(y ~ poly(x, 2, raw=TRUE))
  
  n1=as.numeric(reg$coefficients[3])
  n2=as.numeric(reg$coefficients[2])
  k=as.numeric(reg$coefficients[1])
  
  testy = testx^2*n1+testx*n2+k
  
  
  p <- plot_ly()# x = as.numeric(mydata$TCrpm[1]), y = as.numeric(mydata$Pscav[1]), type='scatter' ,
  #mode = "markers",marker=list(size=12),name = mydata$Date[1], showlegend = TRUE)
  
  #add_trace(x = Cylinder,
  #  y = Avg, mode = "lines")
  
  for(i in 1 : input$monthno){
    p=p%>%add_trace(x =  as.numeric(as.character(mydata$TCrpm[i])), y =  as.numeric(as.character(mydata$Pscav[i])), mode = "markers",marker=list(size=12),
                    name = mydata$Date[i], showlegend = TRUE)
  }
  #p=p%>%add_trace(x =  as.numeric(as.character(mydata$TCrpm[3])), y =  as.numeric(as.character(mydata$Pscav[3])), mode = "markers",marker=list(size=12),
  #                name = mydata$Date[3], showlegend = TRUE)
  #p=p%>%add_trace(x =  as.numeric(as.character(mydata$TCrpm[4])), y =  as.numeric(as.character(mydata$Pscav[4])), mode = "markers",marker=list(size=12),
  #                name = mydata$Date[4], showlegend = TRUE)
  p = p%>%add_trace(x =  x, y =  y,  mode = "markers",marker=list(size=8,color = "#640017"),
                    name = "Shoptrial data", showlegend = TRUE)
  p= p%>% add_trace(x=testx,y = testy,  type="scatter" ,mode = "lines+markers", line=list(shape="spline",color = "#640017"),marker=list(opacity=0), name = "reg",showlegend = F)
  
  p <-p%>%layout(title="Pscav Vs T/C Speed", titlefont = s, xaxis = list(title="Corrected T/C Speed (rpm)", titlefont = s, tickfont = s,gridcolor = "#FFFFFF"),yaxis=list(title="Corrected Pscav (MPa.abs)", titlefont = s, tickfont = s,gridcolor = "#E5E7E9"),
                 showlegend=T,plot_bgcolor = "#FFFFFF",
                 paper_bgcolor = "#FFFFFF",legend=l)  
  p
  
})

output$graphPsTC1 = renderPlotly({ 
  
  
  mydata = enginedatatable()
  STdata = shoptrialdata()
  
  
  x= STdata$TCspeed
  y= STdata$pscav
  reg = lm(y ~ poly(x, 2, raw=TRUE))
  
  n1=as.numeric(reg$coefficients[3])
  n2=as.numeric(reg$coefficients[2])
  k=as.numeric(reg$coefficients[1])
  c = input$monthno
  tdate = rep(0,c)
  testx = rep(0,c)
  testy = rep(0,c)
  pscav = rep(0,c)
  deviation = rep(0,c)
  for(i in 1 : input$monthno){
    
    
    tdate[i] = mydata$Date[i]
    testx[i] = mydata$TCrpm[i]
    testy[i] = n1*testx[i]*testx[i] + n2*testx[i] + k
    pscav[i] = mydata$Pscav[i]
    deviation[i] = round(( pscav[i] - testy[i])*100/ testy[i],2)
  }
  
  df = data.frame(tdate,deviation)
  
  
  p <- plot_ly(df,x = as.Date(tdate) , y = deviation, type='scatter' ,
               mode = "lines+markers",marker=list(size=12), showlegend = TRUE)
  
  #add_trace(x = Cylinder,
  #  y = Avg, mode = "lines")
  
  
  
  p <-p%>%layout(title="Deviation %", titlefont = s, xaxis = list(title="Month", titlefont = s, tickfont = s,gridcolor = "#FFFFFF"),yaxis=list(title="% deviation", titlefont = s,ticksuffix = "%", tickfont = s,gridcolor = "#E5E7E9"),
                 showlegend=F,plot_bgcolor = "#FFFFFF",
                 paper_bgcolor = "#FFFFFF",legend=l)  
  p
  
})

output$shoptrialPsTC = renderDataTable({ 
  
  STdata = shoptrialdata()
  
  x= STdata$TCspeed
  y=STdata$pscav
  
  
  table = data.frame(x,y)
  names(table)<-c("T/C Speed","Pscav(MPa.abs)")
  
  datatable(table, options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%
    formatStyle(names(table),color="#000",backgroundColor = "white")
  
})

output$historicPsTC = renderDataTable({ 
  mydata=enginedatatable()
  
  dates = mydata$Date
  names = c("Latest Data","Others1","Others2","Others3")
  historicx = mydata$RPM
  historicy = round(mydata$Pscav,2)
  
  table = data.frame(dates,historicx,historicy)
  names(table) = c("Date of Measurement","T/C Speed","Pscav (MPa.abs)")
  
  datatable(table, options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%
    formatStyle(names(table),color="#000",backgroundColor = "white")
  
})

#Press. drop at A/C VS Pscav

output$graphPdPs = renderPlotly({ 
  
  
  mydata=enginedatatable()
  STdata = shoptrialdata()
  
  
  x= STdata$pscav
  y=STdata$pressdrop
  testx = seq(from=min(STdata$pscav,na.rm = T), to=max(STdata$pscav,na.rm=T), length.out= 30)
  
  reg = lm(y ~ poly(x, 2, raw=TRUE))
  
  n1=as.numeric(reg$coefficients[3])
  n2=as.numeric(reg$coefficients[2])
  k=as.numeric(reg$coefficients[1])
  
  testy = round(testx^2*n1+testx*n2+k,2)
  
  
  p <- plot_ly()# x = as.numeric(mydata$Pscav[1]), y = as.numeric(mydata$PressdropAC[1]), type='scatter' ,
  #mode = "markers",marker=list(size=12),name = mydata$Date[1], showlegend = TRUE)
  
  #add_trace(x = Cylinder,
  #  y = Avg, mode = "lines")
  
  for(i in 1 : input$monthno){
    p=p%>%add_trace(x =  as.numeric(as.character(mydata$Pscav[i])), y =  as.numeric(as.character(mydata$PressdropAC[i])), mode = "markers",marker=list(size=12),
                    name = mydata$Date[i], showlegend = TRUE)
  }
  # p=p%>%add_trace(x =  as.numeric(as.character(mydata$Pscav[3])), y =  as.numeric(as.character(mydata$PressdropAC[3])), mode = "markers",marker=list(size=12),
  #                name =mydata$Date[3], showlegend = TRUE)
  #p=p%>%add_trace(x =  as.numeric(as.character(mydata$Pscav[4])), y =  as.numeric(as.character(mydata$PressdropAC[4])), mode = "markers",marker=list(size=12),
  #                name = mydata$Date[4], showlegend = TRUE)
  p = p%>%add_trace(x =  x, y =  y,  mode = "markers",marker=list(size=8,color = "#640017"),
                    name = "Shoptrial data", showlegend = TRUE)
  p=p%>% add_trace(x=testx,y = testy,  type="scatter" ,mode = "lines+markers", line=list(shape="spline",color = "#640017"),marker=list(opacity=0), name = "reg",showlegend = F)
  
  p <-p%>%layout(title="Press Drop at A/C Vs Pscav", titlefont = s, xaxis = list(title="Corrected Pscav (MPa.abs)", titlefont = s, tickfont = s,gridcolor = "#FFFFFF"),yaxis=list(title="Press Drop at A/C (kPa)", titlefont = s, tickfont = s,gridcolor = "#E5E7E9"),
                 showlegend=T,plot_bgcolor = "#FFFFFF",
                 paper_bgcolor = "#FFFFFF",legend=l)  
  p
  
})

output$graphPdPs1 = renderPlotly({ 
  
  
  mydata = enginedatatable()
  STdata = shoptrialdata()
  
  
  x= STdata$pscav
  y=STdata$pressdrop
  reg = lm(y ~ poly(x, 2, raw=TRUE))
  
  n1=as.numeric(reg$coefficients[3])
  n2=as.numeric(reg$coefficients[2])
  k=as.numeric(reg$coefficients[1])
  c = input$monthno
  tdate = rep(0,c)
  testx = rep(0,c)
  testy = rep(0,c)
  pd = rep(0,c)
  deviation = rep(0,c)
  for(i in 1 : input$monthno){
    
    
    tdate[i] = mydata$Date[i]
    testx[i] = mydata$Pscav[i]
    testy[i] = n1*testx[i]*testx[i] + n2*testx[i] + k
    pd[i] = mydata$PressdropAC[i]
    deviation[i] = round(( pd[i] - testy[i])*100/ testy[i],2)
  }
  
  df = data.frame(tdate,deviation)
  
  
  p <- plot_ly(df,x = as.Date(tdate) , y = deviation, type='scatter' ,
               mode = "lines+markers",marker=list(size=12), showlegend = TRUE)
  
  #add_trace(x = Cylinder,
  #  y = Avg, mode = "lines")
  
  
  
  p <-p%>%layout(title="Deviation %", titlefont = s, xaxis = list(title="Month", titlefont = s, tickfont = s,gridcolor = "#FFFFFF"),yaxis=list(title="% deviation", titlefont = s,ticksuffix = "%", tickfont = s,gridcolor = "#E5E7E9"),
                 showlegend=F,plot_bgcolor = "#FFFFFF",
                 paper_bgcolor = "#FFFFFF",legend=l)  
  p
  
})

output$shoptrialPdPs = renderDataTable({ 
  
  STdata = shoptrialdata()
  
  x= STdata$pscav
  y=STdata$pressdrop
  
  
  table = data.frame(x,y)
  names(table)<-c("Pscav(MPa.abs)","Pr. Drop at A/C")
  
  datatable(table, options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%
    formatStyle(names(table),color="#000",backgroundColor = "white")
  
})

output$historicPdPs = renderDataTable({ 
  mydata=enginedatatable()
  
  dates = mydata$Date
  #names = c("Latest Data","Others1","Others2","Others3")
  historicx = mydata$Pscav
  historicy = round(mydata$PressdropAC,1)
  
  table = data.frame(dates,historicx,historicy)
  names(table) = c("Date of Measurement","Pscav (MPa.abs)","Pr. Drop at A/C")
  
  datatable(table, options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%
    formatStyle(names(table),color="#000",backgroundColor = "white")
  
})

#Pcomp VS Pscav

output$graphPcPs = renderPlotly({ 
  
  
  mydata=enginedatatable()
  STdata = shoptrialdata()
  
  
  x= STdata$pscav
  y=STdata$pcomp
  testx = seq(from=min(STdata$pscav,na.rm = T), to=max(STdata$pscav,na.rm=T), length.out= 30)
  
  reg = lm(y ~ poly(x, 2, raw=TRUE))
  
  n1=as.numeric(reg$coefficients[3])
  n2=as.numeric(reg$coefficients[2])
  k=as.numeric(reg$coefficients[1])
  
  testy = round(testx^2*n1+testx*n2+k,2)
  
  
  p <- plot_ly()# x = as.numeric(mydata$Pscav[1]), y = as.numeric(mydata$PressCompAvg[1]), type='scatter' ,
  #mode = "markers",marker=list(size=12),name = mydata$Date[1], showlegend = TRUE)
  
  #add_trace(x = Cylinder,
  #  y = Avg, mode = "lines")
  
  for(i in 1 : input$monthno){
    p=p%>%add_trace(x =  as.numeric(as.character(mydata$Pscav[i])), y =  as.numeric(as.character(mydata$PressCompAvg[i])), mode = "markers",marker=list(size=12),
                    name = mydata$Date[i], showlegend = TRUE)
  }
  #p=p%>%add_trace(x =  as.numeric(as.character(mydata$Pscav[3])), y =  as.numeric(as.character(mydata$PressCompAvg[3])), mode = "markers",marker=list(size=12),
  #                name = mydata$Date[3], showlegend = TRUE)
  #p=p%>%add_trace(x =  as.numeric(as.character(mydata$Pscav[4])), y =  as.numeric(as.character(mydata$PressCompAvg[4])), mode = "markers",marker=list(size=12),
  #               name = mydata$Date[4], showlegend = TRUE)
  p = p%>%add_trace(x =  x, y =  y,  mode = "markers",marker=list(size=8,color = "#640017"),
                    name = "Shoptrial data", showlegend = TRUE)
  p= p%>% add_trace(p,x=testx,y = testy, type="scatter" ,mode = "lines+markers", line=list(shape="spline",color = "#640017"),marker=list(opacity=0), name = "reg",showlegend = F)
  
  p <-p%>%layout(title="Pcomp Vs Pscav", titlefont = s, xaxis = list(title="Corrected Pscav (MPa.abs)", titlefont = s, tickfont = s,gridcolor = "#FFFFFF"),yaxis=list(title="Corrected Pcomp (bar)", titlefont = s, tickfont = s,gridcolor = "#E5E7E9"),
                 showlegend=T,plot_bgcolor = "#FFFFFF",
                 paper_bgcolor = "#FFFFFF",legend=l)  
  p
  
})

output$graphPcPs1 = renderPlotly({ 
  
  
  mydata = enginedatatable()
  STdata = shoptrialdata()
  
  
  x= STdata$pscav
  y=STdata$pcomp
  reg = lm(y ~ poly(x, 2, raw=TRUE))
  
  n1=as.numeric(reg$coefficients[3])
  n2=as.numeric(reg$coefficients[2])
  k=as.numeric(reg$coefficients[1])
  c = input$monthno
  tdate = rep(0,c)
  testx = rep(0,c)
  testy = rep(0,c)
  pc = rep(0,c)
  deviation = rep(0,c)
  for(i in 1 : input$monthno){
    
    
    tdate[i] = mydata$Date[i]
    testx[i] = mydata$Pscav[i]
    testy[i] = n1*testx[i]*testx[i] + n2*testx[i] + k
    pc[i] = mydata$PressCompAvg[i]
    deviation[i] = round(( pc[i] - testy[i])*100/ testy[i],2)
  }
  
  df = data.frame(tdate,deviation)
  
  
  p <- plot_ly(df,x = as.Date(tdate) , y = deviation, type='scatter' ,
               mode = "lines+markers",marker=list(size=12), showlegend = TRUE)
  
  #add_trace(x = Cylinder,
  #  y = Avg, mode = "lines")
  
  
  
  p <-p%>%layout(title="Deviation %", titlefont = s, xaxis = list(title="Month", titlefont = s, tickfont = s,gridcolor = "#FFFFFF"),yaxis=list(title="% deviation", titlefont = s,ticksuffix = "%", tickfont = s,gridcolor = "#E5E7E9"),
                 showlegend=F,plot_bgcolor = "#FFFFFF",
                 paper_bgcolor = "#FFFFFF",legend=l)  
  p
  
})

output$shoptrialPcPs = renderDataTable({ 
  
  STdata = shoptrialdata()
  
  x= STdata$pscav
  y=STdata$pcomp
  
  
  table = data.frame(x,y)
  names(table)<-c("Pscav(MPa.abs)","Pcomp (bar)")
  
  datatable(table, options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%
    formatStyle(names(table),color="#000",backgroundColor = "white")
  
})

output$historicPcPs = renderDataTable({ 
  mydata=enginedatatable()
  
  dates = mydata$Date
  names = c("Latest Data","Others1","Others2","Others3")
  historicx = mydata$Pscav
  historicy = round(mydata$PressCompAvg,0)
  
  table = data.frame(dates,historicx,historicy)
  names(table) = c("Date of Measurement","Pscav (MPa.abs)","Pcomp (bar)")
  
  datatable(table, options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%
    formatStyle(names(table),color="#000",backgroundColor = "white")
  
})

#Texh VS Engine Load

output$graphETEL = renderPlotly({ 
  
  
  mydata=enginedatatable()
  STdata = shoptrialdata()
  
  
  x= STdata$Load
  y=STdata$Texh
  testx = seq(from=min(STdata$Load,na.rm = T), to=max(STdata$Load,na.rm=T), length.out= 30)
  
  reg = lm(y ~ poly(x, 2, raw=TRUE))
  
  n1=as.numeric(reg$coefficients[3])
  n2=as.numeric(reg$coefficients[2])
  k=as.numeric(reg$coefficients[1])
  
  testy = round(testx^2*n1+testx*n2+k,2)
  
  
  p <- plot_ly()# x = as.numeric(mydata$Engine.Load[1]), y = as.numeric(mydata$ExTempAvg[1]), type='scatter' ,
  #mode = "markers",marker=list(size=12),name = mydata$Date[1], showlegend = TRUE)
  
  #add_trace(x = Cylinder,
  #  y = Avg, mode = "lines")
  
  for(i in 1 :input$monthno){
    p=p%>%add_trace(x =  as.numeric(as.character(mydata$Engine.Load[i])), y =  as.numeric(as.character(mydata$ExTempAvg[i])), mode = "markers",marker=list(size=12),
                    name = mydata$Date[i], showlegend = TRUE)
  }
  #p=p%>%add_trace(x =  as.numeric(as.character(mydata$Engine.Load[3])), y =  as.numeric(as.character(mydata$ExTempAvg[3])), mode = "markers",marker=list(size=12),
  #                name = mydata$Date[3], showlegend = TRUE)
  #p=p%>%add_trace(x =  as.numeric(as.character(mydata$Engine.Load[4])), y =  as.numeric(as.character(mydata$ExTempAvg[4])), mode = "markers",marker=list(size=12),
  #               name = mydata$Date[4], showlegend = TRUE)
  p = p%>%add_trace(x =  x, y =  y,  mode = "markers",marker=list(size=8,color = "#640017"),
                    name = "Shoptrial data", showlegend = TRUE)
  p=p%>%add_trace(x=testx,y = testy,type="scatter" ,mode = "lines+markers", line=list(shape="spline",color = "#640017"),marker=list(opacity=0), name = "reg",showlegend = F)
  
  p <-p%>%layout(title="Texh Vs Load", titlefont = s, xaxis = list(title="Engine Load(%)", titlefont = s, tickfont = s,gridcolor = "#FFFFFF"),yaxis=list(title="Corrected Texh Cyl. Out. (deg.C)", titlefont = s, tickfont = s,gridcolor = "#E5E7E9"),
                 showlegend=T,plot_bgcolor = "#FFFFFF",
                 paper_bgcolor = "#FFFFFF",legend=l)  
  p
  
})

output$graphETEL1 = renderPlotly({ 
  
  
  mydata = enginedatatable()
  STdata = shoptrialdata()
  
  
  x= STdata$Load
  y=STdata$Texh
  reg = lm(y ~ poly(x, 2, raw=TRUE))
  
  n1=as.numeric(reg$coefficients[3])
  n2=as.numeric(reg$coefficients[2])
  k=as.numeric(reg$coefficients[1])
  c = input$monthno
  tdate = rep(0,c)
  testx = rep(0,c)
  testy = rep(0,c)
  pc = rep(0,c)
  deviation = rep(0,c)
  for(i in 1 : input$monthno){
    
    
    tdate[i] = mydata$Date[i]
    testx[i] = mydata$Engine.Load[i]
    testy[i] = n1*testx[i]*testx[i] + n2*testx[i] + k
    pc[i] = mydata$ExTempAvg[i]
    deviation[i] = round(( pc[i] - testy[i])*100/ testy[i],2)
  }
  
  df = data.frame(tdate,deviation)
  
  
  p <- plot_ly(df,x = as.Date(tdate) , y = deviation, type='scatter' ,
               mode = "lines+markers",marker=list(size=12), showlegend = TRUE)
  
  #add_trace(x = Cylinder,
  #  y = Avg, mode = "lines")
  
  
  
  p <-p%>%layout(title="Deviation %", titlefont = s, xaxis = list(title="Month", titlefont = s, tickfont = s,gridcolor = "#FFFFFF"),yaxis=list(title="% deviation", titlefont = s,ticksuffix = "%", tickfont = s,gridcolor = "#E5E7E9"),
                 showlegend=F,plot_bgcolor = "#FFFFFF",
                 paper_bgcolor = "#FFFFFF",legend=l)  
  p
  
})

output$shoptrialETEL = renderDataTable({ 
  
  STdata = shoptrialdata()
  
  x= STdata$Load
  y=STdata$Texh
  
  
  table = data.frame(x,y)
  names(table)<-c("Engine Load (%)","Texh (deg.C)")
  
  datatable(table, options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%
    formatStyle(names(table),color="#000",backgroundColor = "white")
  
})

output$historicETEL = renderDataTable({ 
  mydata=enginedatatable()
  
  dates = mydata$Date
  names = c("Latest Data","Others1","Others2","Others3")
  historicx = mydata$Engine.Load
  historicy = round(mydata$ExTempAvg,0)
  
  table = data.frame(dates,historicx,historicy)
  names(table) = c("Date of Measurement","Engine Load (%)","Texh (deg.C)")
  
  datatable(table, options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%
    formatStyle(names(table),color="#000",backgroundColor = "white")
  
})

#PMAX-PCOMP VS PUMP MARK

output$graphPM = renderPlotly({ 
  
  mydata=enginedatatable()                                                                                                                    
  STdata = shoptrialdata()
  
  
  x= STdata$pumpmark
  y=STdata$pmax.pcomp
  testx = seq(from=min(STdata$pumpmark,na.rm = T), to=max(STdata$pumpmark,na.rm=T), length.out= 30)
  
  reg = lm(y ~ poly(x, 2, raw=TRUE))
  
  n1=as.numeric(reg$coefficients[3])
  n2=as.numeric(reg$coefficients[2])
  k=as.numeric(reg$coefficients[1])
  
  testy = round(testx^2*n1+testx*n2+k,2)
  
  
  p <- plot_ly()# x = mydata$PumpMarkAvg[1], y = mydata$MaxPressAvg[1]-mydata$PressCompAvg[1], type='scatter' ,
  #mode = "markers",marker=list(size=12),name =mydata$Date[1], showlegend = TRUE)
  
  #add_trace(x = Cylinder,
  #  y = Avg, mode = "lines")
  
  for(i in 1 :input$monthno){
    p=p%>%add_trace(x =  as.numeric(as.character(mydata$PumpMarkAvg[i])), y =  as.numeric(as.character(mydata$MaxPressAvg[i]-mydata$PressCompAvg[i])), mode = "markers",marker=list(size=12),
                    name = mydata$Date[i], showlegend = TRUE)
  }
  #p=p%>%add_trace(x =  as.numeric(as.character(mydata$PumpMarkAvg[3])), y =  as.numeric(as.character(mydata$MaxPressAvg[3]-mydata$PressCompAvg[3])), mode = "markers",marker=list(size=12),
  #               name = mydata$Date[3], showlegend = TRUE)
  #p=p%>%add_trace(x =  as.numeric(as.character(mydata$PumpMarkAvg[4])), y =  as.numeric(as.character(mydata$MaxPressAvg[4]-mydata$PressCompAvg[4])), mode = "markers",marker=list(size=12),
  #                name = mydata$Date[4], showlegend = TRUE)
  p = p%>%add_trace(x =  x, y =  y,  mode = "markers",marker=list(size=8,color = "#640017"),
                    name = "Shoptrial data", showlegend = TRUE)
  p=p%>%add_trace(x=testx,y = testy, type="scatter" ,mode = "lines+markers", line=list(shape="spline",color = "#640017"),marker=list(opacity=0), name = "reg",showlegend = F)
  
  p <-p%>%layout(title="(PMAX-PCOMP) Vs Pump Mark", titlefont = s, xaxis = list(title="Pump Mark(measured)", titlefont = s, tickfont = s,gridcolor = "#FFFFFF"),yaxis=list(title="Pmax-Pcomp(bar)", titlefont = s, tickfont = s,gridcolor = "#E5E7E9"),
                 showlegend=T,plot_bgcolor = "#FFFFFF",
                 paper_bgcolor = "#FFFFFF",legend=l)  
  p
  
})

output$graphPM1 = renderPlotly({ 
  
  
  mydata = enginedatatable()
  STdata = shoptrialdata()
  
  
  x= STdata$pumpmark
  y= STdata$pmax.pcomp
  reg = lm(y ~ poly(x, 2, raw=TRUE))
  
  n1=as.numeric(reg$coefficients[3])
  n2=as.numeric(reg$coefficients[2])
  k=as.numeric(reg$coefficients[1])
  c = input$monthno
  tdate = rep(0,c)
  testx = rep(0,c)
  testy = rep(0,c)
  pc = rep(0,c)
  deviation = rep(0,c)
  for(i in 1 : input$monthno){
    
    
    tdate[i] = mydata$Date[i]
    testx[i] = mydata$PumpMarkAvg[i]
    testy[i] = n1*testx[i]*testx[i] + n2*testx[i] + k
    pc[i] =  mydata$MaxPressAvg[i]-mydata$PressCompAvg[i]
    deviation[i] = round(( pc[i] - testy[i])*100/ testy[i],2)
  }
  
  df = data.frame(tdate,deviation)
  
  
  p <- plot_ly(df,x = as.Date(tdate) , y = deviation, type='scatter' ,
               mode = "lines+markers",marker=list(size=12), showlegend = TRUE)
  
  #add_trace(x = Cylinder,
  #  y = Avg, mode = "lines")
  
  
  
  p <-p%>%layout(title="Deviation %", titlefont = s, xaxis = list(title="Month", titlefont = s, tickfont = s,gridcolor = "#FFFFFF"),yaxis=list(title="% deviation", titlefont = s,ticksuffix = "%", tickfont = s,gridcolor = "#E5E7E9"),
                 showlegend=F,plot_bgcolor = "#FFFFFF",
                 paper_bgcolor = "#FFFFFF",legend=l)  
  p
  
})

output$shoptrialPM = renderDataTable({ 
  
  STdata = shoptrialdata()
  
  
  x= STdata$pumpmark
  y=STdata$pmax.pcomp
  
  
  table = data.frame(x,y)
  names(table)<-c("Pump Mark","Pmax - Pcomp")
  
  datatable(table, options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%
    formatStyle(names(table),color="#000",backgroundColor = "white")
  
})

output$historicPM = renderDataTable({ 
  mydata=enginedatatable()
  
  dates = mydata$Date
  names = c("Latest Data","Others1","Others2","Others3")
  historicx = round(mydata$PumpMarkAvg,1)
  historicy = round(mydata$MaxPressAvg-mydata$PressCompAvg,2)
  
  table = data.frame(dates,historicx,historicy)
  names(table) = c("Date of Measurement","Pump Mark","Pmax - Pcomp")
  
  datatable(table, options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%
    formatStyle(names(table),color="#000",backgroundColor = "white")
  
})

#SFOC VS ENGINE LOAD

output$graphSFOC = renderPlotly({ 
  
  mydata=enginedatatable()
  STdata = shoptrialdata()
  
  
  x= STdata$Load
  y=STdata$SFOC
  testx = seq(from=min(STdata$Load,na.rm = T), to=max(STdata$Load,na.rm=T), length.out= 30)
  
  reg = lm(y ~ poly(x, 2, raw=TRUE))
  
  n1=as.numeric(reg$coefficients[3])
  n2=as.numeric(reg$coefficients[2])
  k=as.numeric(reg$coefficients[1])
  
  testy = round(testx^2*n1+testx*n2+k,2)
  
  
  
  
  p <- plot_ly()# x = as.numeric(as.character(mydata$Engine.Load[1])), y = as.numeric(as.character(mydata$SFOC[1])), type='scatter' ,
  #mode = "markers",marker=list(size=12),name = mydata$Date[1], showlegend = TRUE)
  
  
  for(i in 1 :input$monthno){
    
    p=p%>%add_trace(x =  as.numeric(as.character(mydata$Engine.Load[i])), y =  as.numeric(as.character(mydata$SFOC[i])), mode = "markers",marker=list(size=12),
                    name = mydata$Date[i], showlegend = TRUE)
  }
  #p=p%>%add_trace(x =  as.numeric(as.character(mydata$Engine.Load[3])), y =  as.numeric(as.character(mydata$SFOC[3])), mode = "markers",marker=list(size=12),
  #                name = mydata$Date[3], showlegend = TRUE)
  #p=p%>%add_trace(x =  as.numeric(as.character(mydata$Engine.Load[4])), y =  as.numeric(as.character(mydata$SFOC[4])), mode = "markers",marker=list(size=12),
  #                 name = mydata$Date[4], showlegend = TRUE)
  p=p%>%add_trace(x =  x, y =  y, mode = "markers",marker=list(size=8,color = "#640017"),
                  name = "Shoptrial data", showlegend = TRUE)
  p=p%>%add_trace(x=testx,y = testy,  type="scatter" ,mode = "lines+markers", line=list(shape="spline",color = "#640017"),marker=list(opacity=0), name = "reg",showlegend = F)
  
  
  p <-p%>%layout(title="Fuel Oil Consumption Rate Vs Engine Load", titlefont = s,xaxis=list(title="Engine Load (%)", titlefont = s, tickfont = s,gridcolor = "#FFFFFF"),yaxis=list(title="Fuel Oil Consumption(g/kWhr)", titlefont = s, tickfont = s,gridcolor = "#E5E7E9"),showlegend=T,plot_bgcolor = "#FFFFFF",
                 paper_bgcolor = "#FFFFFF",legend=l)  
  p
})

output$graphSFOC1 = renderPlotly({ 
  
  
  mydata = enginedatatable()
  STdata = shoptrialdata()
  
  
  x= STdata$Load
  y=STdata$SFOC
  reg = lm(y ~ poly(x, 2, raw=TRUE))
  
  n1=as.numeric(reg$coefficients[3])
  n2=as.numeric(reg$coefficients[2])
  k=as.numeric(reg$coefficients[1])
  c = input$monthno
  tdate = rep(0,c)
  testx = rep(0,c)
  testy = rep(0,c)
  pc = rep(0,c)
  deviation = rep(0,c)
  for(i in 1 : input$monthno){
    
    
    tdate[i] = mydata$Date[i]
    testx[i] = mydata$Engine.Load[i]
    testy[i] = n1*testx[i]*testx[i] + n2*testx[i] + k
    pc[i] =   mydata$SFOC[i]
    deviation[i] = round(( pc[i] - testy[i])*100/ testy[i],2)
  }
  
  df = data.frame(tdate,deviation)
  
  
  p <- plot_ly(df,x = as.Date(tdate) , y = deviation, type='scatter' ,
               mode = "lines+markers",marker=list(size=12), showlegend = TRUE)
  
  #add_trace(x = Cylinder,
  #  y = Avg, mode = "lines")
  
  
  
  p <-p%>%layout(title="Deviation %", titlefont = s, xaxis = list(title="Month", titlefont = s, tickfont = s,gridcolor = "#FFFFFF"),yaxis=list(title="% deviation", titlefont = s,ticksuffix = "%", tickfont = s,gridcolor = "#E5E7E9"),
                 showlegend=F,plot_bgcolor = "#FFFFFF",
                 paper_bgcolor = "#FFFFFF",legend=l)  
  p
  
})

output$shoptrialSFOC = renderDataTable({ 
  
  STdata = shoptrialdata()
  
  x= STdata$Load
  y=STdata$SFOC
  
  table = data.frame(x,y)
  names(table)<-c("Engine Load","SFOC(g/kW-hr)")
  
  datatable(table, options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%
    formatStyle(names(table),color="#000",backgroundColor = "white")
  
})

output$historicSFOC = renderDataTable({ 
  
  mydata=enginedatatable()
  
  dates = as.Date(mydata$Date,"%d%m%y")
  names = c("Latest Data","Others1","Others2","Others3")
  historicx = round(mydata$Engine.Load,2)
  historicy = round(mydata$SFOC,0)
  
  table = data.frame(dates,historicx,historicy)
  
  names(table)<-c("Date of Measurement","Engine Load","SFOC(g/kW-hr)")
  
  datatable(table, options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%
    formatStyle(names(table),color="#000",backgroundColor = "white")
  
})

#T/C SPEED VS ENGINE LOAD---------------------------------------------------------------------------------------------------------

output$graphTCspeed = renderPlotly({ 
  
  
  mydata=enginedatatable()
  STdata = shoptrialdata()
  
  
  x= STdata$Load
  y=STdata$TCspeed
  
  
  
  testx = seq(from=min(STdata$Load,na.rm = T), to=max(STdata$Load,na.rm=T), length.out= 30)
  reg = lm(y ~ poly(x, 2, raw=TRUE))
  
  n1=as.numeric(reg$coefficients[3])
  n2=as.numeric(reg$coefficients[2])
  k=as.numeric(reg$coefficients[1])
  
  testy = round(testx^2*n1+testx*n2+k,2)
  
  p <- plot_ly()# x = as.numeric(as.character(mydata$Engine.Load[1])), y = as.numeric(as.character(mydata$TCrpm[1])), type='scatter' ,
  #mode = "markers",marker=list(size=12),name = mydata$Date[1], showlegend = TRUE)
  
  #add_trace(x = Cylinder,
  #  y = Avg, mode = "lines")
  
  
  for(i in 1 : input$monthno){
    
    p=p%>%add_trace(x =  as.numeric(as.character(mydata$Engine.Load[i])), y =  as.numeric(as.character(mydata$TCrpm[i])), mode = "markers",marker=list(size=12),
                    name =  mydata$Date[i], showlegend = TRUE)
  }
  #p=p%>%add_trace(x =  as.numeric(as.character(mydata$Engine.Load[3])), y =  as.numeric(as.character(mydata$TCrpm[3])), mode = "markers",marker=list(size=12),
  #                name =  mydata$Date[3], showlegend = TRUE)
  #p=p%>%add_trace(x =  as.numeric(as.character(mydata$Engine.Load[4])), y =  as.numeric(as.character(mydata$TCrpm[4])), mode = "markers",marker=list(size=12),
  #                name =  mydata$Date[4], showlegend = TRUE)
  p=p%>%add_trace(x =  x, y =  y, mode = "markers",marker=list(size=8,color = "#640017"),
                  name = "Shoptrial data", showlegend = TRUE)
  p=p%>%add_trace(x=testx,y = testy,  type="scatter" ,mode = "lines+markers", line=list(shape="spline",color = "#640017"),marker=list(opacity=0), name = "reg",showlegend = F)
  
  p <-p%>%layout(title="T/C Speed Vs Engine Load", titlefont = s,xaxis=list(title="Engine Load (%)", titlefont = s, tickfont = s,gridcolor = "#FFFFFF"),yaxis=list(title="Corrected T/C Speed (rpm)", titlefont = s, tickfont = s,gridcolor = "#E5E7E9"),showlegend=T,plot_bgcolor = "#FFFFFF",
                 paper_bgcolor = "#FFFFFF",legend=l)  
  p
  
})
output$graphTCspeed1 = renderPlotly({ 
  
  
  mydata = enginedatatable()
  STdata = shoptrialdata()
  
  
  x= STdata$Load
  y=STdata$TCspeed
  reg = lm(y ~ poly(x, 2, raw=TRUE))
  
  n1=as.numeric(reg$coefficients[3])
  n2=as.numeric(reg$coefficients[2])
  k=as.numeric(reg$coefficients[1])
  c = input$monthno
  tdate = rep(0,c)
  testx = rep(0,c)
  testy = rep(0,c)
  pc = rep(0,c)
  deviation = rep(0,c)
  for(i in 1 : input$monthno){
    
    
    tdate[i] = mydata$Date[i]
    testx[i] = mydata$Engine.Load[i]
    testy[i] = n1*testx[i]*testx[i] + n2*testx[i] + k
    pc[i] = mydata$TCrpm[i]
    deviation[i] = round(( pc[i] - testy[i])*100/ testy[i],2)
  }
  
  df = data.frame(tdate,deviation)
  
  
  p <- plot_ly(df,x = as.Date(tdate) , y = deviation, type='scatter' ,
               mode = "lines+markers",marker=list(size=12), showlegend = TRUE)
  
  #add_trace(x = Cylinder,
  #  y = Avg, mode = "lines")
  
  
  
  p <-p%>%layout(title="Deviation %", titlefont = s, xaxis = list(title="Month", titlefont = s, tickfont = s,gridcolor = "#FFFFFF"),yaxis=list(title="% deviation", titlefont = s,ticksuffix = "%", tickfont = s,gridcolor = "#E5E7E9"),
                 showlegend=F,plot_bgcolor = "#FFFFFF",
                 paper_bgcolor = "#FFFFFF",legend=l)  
  p
  
})

output$shoptrialTCspeed = renderDataTable({ 
  
  STdata = shoptrialdata()
  
  
  x= STdata$Load
  y=STdata$TCspeed
  
  table = data.frame(x,y)
  names(table)<-c("Engine Load","T/C Speed (rpm)")
  
  datatable(table, options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%
    formatStyle(names(table),color="#000",backgroundColor = "white")
  
})

output$historicTCspeed = renderDataTable({ 
  validate(
    need(try(enginedatatable()),"Please Wait or Select the vessel")
  ) 
  mydata=enginedatatable()
  
  dates = mydata$Date
  names = c("Latest Data","Others1","Others2","Others3")
  historicx = round(mydata$Engine.Load,2)
  historicy = round(mydata$TCrpm,0)
  
  table = data.frame(dates,historicx,historicy)
  
  names(table)<-c("Date of Measurement","Engine Load","SFOC(g/kW-hr)")
  
  datatable(table, options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%
    formatStyle(names(table),color="#000",backgroundColor = "white")
  
})

output$mainparti = renderDataTable({ 
  validate(
    need(try(enginedatatable()),"Please Wait or Select the vessel")
  ) 
  
  mydata=enginedatatable()
  
  
  
  title = c("Ship Name","Main Engine Type")
  parti = c(as.character(mydata$Ship.Name[1]),as.character(mydata$Main.Engine.Type[1]))
  
  table = data.frame(title,parti)
  
  names(table)<-c("Title","Particulars ")
  
  datatable(table, options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%
    formatStyle(names(table),color="#000",backgroundColor = "white")
  
})

result_table=reactive({
  
  r = shoptrialdata()
  STD = data.frame(r, header = TRUE)
  mydata=enginedatatable()
  
  fit2aa <- lm(STD$pmax.pcomp ~ poly(STD$pumpmark, 2, raw=TRUE))
  n1=as.numeric(fit2aa$coefficients[3])
  n2=as.numeric(fit2aa$coefficients[2])
  k=as.numeric(fit2aa$coefficients[1])
  m = as.numeric(as.character(mydata$PumpMarkAvg[1]))
  pmaxpm = round(m^2*n1+m*n2+k,2)
  
  fit2a <- lm(STD$SFOC ~ poly(STD$Load, 2, raw=TRUE))
  n1=as.numeric(fit2a$coefficients[3])
  n2=as.numeric(fit2a$coefficients[2])
  k=as.numeric(fit2a$coefficients[1])
  m = as.numeric(as.character(mydata$Engine.Load[1]))
  SFOC = round(m^2*n1+m*n2+k,2)
  
  fit2b <- lm(STD$TCspeed ~ poly(STD$Load, 2, raw=TRUE))
  n1=as.numeric(fit2b$coefficients[3])
  n2=as.numeric(fit2b$coefficients[2])
  k=as.numeric(fit2b$coefficients[1])
  m = as.numeric(as.character(mydata$Engine.Load[1]))
  TCspeed = round(m^2*n1+m*n2+k,2)
  
  fit2c <- lm(STD$Texh ~ poly(STD$Load, 2, raw=TRUE))
  n1=as.numeric(fit2c$coefficients[3])
  n2=as.numeric(fit2c$coefficients[2])
  k=as.numeric(fit2c$coefficients[1])
  m = as.numeric(as.character(mydata$Engine.Load[1]))
  Texh = round(m^2*n1+m*n2+k,2)
  
  fit2d <- lm(STD$pcomp ~ poly(STD$pscav, 2, raw=TRUE))
  n1=as.numeric(fit2d$coefficients[3])
  n2=as.numeric(fit2d$coefficients[2])
  k=as.numeric(fit2d$coefficients[1])
  m = as.numeric(as.character(mydata$Pscav[1]))
  pcs = round(m^2*n1+m*n2+k,2)
  
  fit2e <- lm(STD$pressdrop ~ poly(STD$pscav, 2, raw=TRUE))
  n1=as.numeric(fit2e$coefficients[3])
  n2=as.numeric(fit2e$coefficients[2])
  k=as.numeric(fit2e$coefficients[1])
  m = as.numeric(as.character(mydata$Pscav[1]))
  pdpscav = round(m^2*n1+m*n2+k,2)
  
  fit2f <- lm(STD$pscav ~ poly(STD$TCspeed, 2, raw=TRUE))
  n1=as.numeric(fit2f$coefficients[3])
  n2=as.numeric(fit2f$coefficients[2])
  k=as.numeric(fit2f$coefficients[1])
  m = as.numeric(as.character(mydata$TCrpm[1]))
  pscavspeed = round(m^2*n1+m*n2+k,2)
  
  fit2g <- lm(STD$pumpmark ~ poly(STD$engine.speed, 2, raw=TRUE))
  n1=as.numeric(fit2g$coefficients[3])
  n2=as.numeric(fit2g$coefficients[2])
  k=as.numeric(fit2g$coefficients[1])
  m = as.numeric(as.character(mydata$RPM[1]))
  pmenspeed = round(m^2*n1+m*n2+k,2)
  
  fit2h <- lm(STD$TCspeed ~ poly(STD$engine.speed, 2, raw=TRUE))
  n1=as.numeric(fit2h$coefficients[3])
  n2=as.numeric(fit2h$coefficients[2])
  k=as.numeric(fit2h$coefficients[1])
  m = as.numeric(as.character(mydata$RPM[1]))
  TCenspeed = round(m^2*n1+m*n2+k,2)
  
  cc = c(TCenspeed,pmenspeed,pscavspeed,pdpscav,pcs,Texh,TCspeed,SFOC,pmaxpm,3,3,35,3)
  
  cc
  
  
}) 


output$STD = renderDataTable({ 
  
  mydata=enginedatatable()
  
  
  m_pmax = pmax()
  mean_pmax = round(mean(as.numeric(m_pmax$MaxPress ),na.rm = TRUE),digits = 1)
  max_dev_pmax =  round(max(abs(mean_pmax-m_pmax$MaxPress),na.rm = TRUE),digits = 1)
  
  m_pcomp = pcomp()
  mean_pcomp = round(mean(as.numeric(m_pcomp$PComp ),na.rm = TRUE),digits = 1)
  max_dev_pcomp = round(max(abs(mean_pcomp-m_pcomp$PComp),na.rm = TRUE),digits = 1)
  
  m_temp=exhaustdata()
  mean_temp = round(mean(as.numeric(m_temp$Exhaust.Temp),na.rm = TRUE),digits = 1)
  max_dev_temp = round(max(abs(mean_temp-m_temp$Exhaust.Temp),na.rm = TRUE),digits = 1)
  
  m_pumpmark =pumpmark()
  mean_pumpmark =  round(mean(as.numeric(m_pumpmark$Pump.Mark),na.rm = TRUE),digits = 1)
  max_dev_pumpmark = round(max(abs(mean_pumpmark-m_pumpmark$MaxPress),na.rm = TRUE),digits = 1)
  
  x = VESSELDETAILS
  x = subset(x,Vessel == input$engineVessel)
  
  if(x$PM == "N"){
    
    
    a = "Engine Performance"
    b = "Comparison of Each Cylinder"
    aa = c(a,a,a,a,a,a,a,b,b,b)
    bb = c("T/C Speed Vs Engine Speed","Pscav Vs T/C Speed","Press. drop at A/C Vs Pscav","Pcomp vs Pscav","Texh Vs Engine Load","T/C Speed Vs Engine Load","SFOC Vs Engine Load","Pmax Deviation","Pcomp Deviation","Texh Deviation")
    cc1 = result_table()
    cc = c(cc1[1],cc1[3],cc1[4],cc1[5],cc1[6],cc1[7],cc1[8],cc1[10],cc1[11],cc1[12])
    dd = c(as.numeric(mydata$TCrpm[1]),as.numeric(mydata$Pscav[1]),as.numeric(mydata$PressdropAC[1]),as.numeric(mydata$PressCompAvg[1]),as.numeric(mydata$ExTempAvg[1]),as.numeric(mydata$TCrpm[1]),as.numeric(mydata$SFOC[1]),max_dev_pmax,max_dev_pcomp,max_dev_temp)
    dd = round(dd,2)
    
    
    
    summary_table = data.frame(aa,bb,cc,dd)
    
    names(summary_table)<-c("Title","Kind of Graph","Standard Value","Analysis Result")
    summary_table$Status <- ifelse(abs(cc-dd)<0.25*cc, "Normal", ifelse(cc>dd,"Lowvalue","High Value"))
    summary_table$Status[7] <- ifelse(dd[7]<cc[7], "Normal", "High Value")
    summary_table$Status[8] <- ifelse(dd[8]<cc[8], "Normal", "High Value")
    summary_table$Status[9] <- ifelse(dd[9]<cc[9], "Normal", "High Value")
    summary_table$Status[10] <- ifelse(dd[10]<cc[10], "Normal", "High Value")
    
    datatable(summary_table, options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%
      formatStyle(names(summary_table),color="#000",backgroundColor = "white")
  }
  else{
    a = "Engine Performance"
    b = "Comparison of Each Cylinder"
    aa = c(a,a,a,a,a,a,a,a,a,b,b,b,b)
    bb = c("T/C Speed Vs Engine Speed","Pump Mark Vs Engine Speed","Pscav Vs T/C Speed","Press. drop at A/C Vs Pscav","Pcomp vs Pscav","Texh Vs Engine Load","T/C Speed Vs Engine Load","SFOC Vs Engine Load","(Pmax-Pcomp) Vs Pump Mark","Pmax Deviation","Pcomp Deviation","Texh Deviation","Pump Mark Deviation")
    cc = result_table()
    dd = c(as.numeric(mydata$TCrpm[1]),as.numeric(mydata$PumpMarkAvg[1]),as.numeric(mydata$Pscav[1]),as.numeric(mydata$PressdropAC[1]),as.numeric(mydata$PressCompAvg[1]),as.numeric(mydata$ExTempAvg[1]),as.numeric(mydata$TCrpm[1]),as.numeric(mydata$SFOC[1]),as.numeric(mydata$MaxPressAvg[1])-as.numeric(mydata$PressCompAvg[1]),max_dev_pmax,max_dev_pcomp,max_dev_temp,max_dev_pumpmark)
    dd = round(dd,2)
    
    
    
    summary_table = data.frame(aa,bb,cc,dd)
    
    names(summary_table)<-c("Title","Kind of Graph","Standard Value","Analysis Result")
    summary_table$Status <- ifelse(abs(cc-dd)<0.25*cc, "Normal", ifelse(cc>dd,"Lowvalue","High Value"))
    summary_table$Status[10] <- ifelse(dd[10]<cc[10], "Normal", "High Value")
    summary_table$Status[11] <- ifelse(dd[11]<cc[11], "Normal", "High Value")
    summary_table$Status[12] <- ifelse(dd[12]<cc[12], "Normal", "High Value")
    summary_table$Status[13] <- ifelse(dd[13]<cc[13], "Normal", "High Value")
    
    datatable(summary_table, options = list(searching = FALSE,paging = FALSE),rownames = FALSE)%>%
      formatStyle(names(summary_table),color="#000",backgroundColor = "white")
    
  }
  
})

output$spider_chart = renderPlot({
  x = VESSELDETAILS
  x = subset(x,Vessel == input$engineVessel)
  table = result_table()
  mydata=enginedatatable()
  if(x$PM == "Y"){
    dev_1 = (as.numeric(mydata$SFOC[1])-table[8])*100/table[8]
    dev_2 = (as.numeric(mydata$PumpMarkAvg[1])-table[2])*100/table[2]
    dev_3 = (as.numeric(mydata$Pscav[1])-table[3])*100/table[3]
    dev_4 = (as.numeric(mydata$PressCompAvg[1])-table[5])*100/table[5]
    dev_5 = (as.numeric(mydata$TCrpm[1])-table[1])*100/table[1]
    dev_6 = (as.numeric(mydata$TCrpm[1])-table[7])*100/table[7]
    dev_7 =  (as.numeric(mydata$ExTempAvg[1])-table[6])*100/table[6]
    
    m= matrix(NA,2,7 )
    colnames(m)=c("SFOC Vs BHP","P.M' Vs EngSpeed","PScav' Vs T/CSpeed'","PComp' Vs PScav'","T/CSpeed' Vs EngSpeed ","T/CSpeed' Vs BHP","ExhTemp' Vs BHP")
    rownames(m)=c("std","actual")
    m[1,]=c(rep(0,7))
    m[2,]=c(dev_1,dev_2,dev_3,dev_4,dev_5,dev_6,dev_7)
    m = as.data.frame(m)
    m=rbind(rep(20,7) , rep(-20,7),m)
    par(bg='grey90')
    radarchart(m)
    colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) )
    colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4)  )
    radarchart( m  , axistype=1 , 
                #custom polygon
                pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
                #custom the grid
                cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(-20,20,10), cglwd=0.8,
                #custom labels
                vlcex=0.8
                
    )
  }
  
  else{
    dev_1 = (as.numeric(mydata$SFOC[1])-table[8])*100/table[8]
    
    dev_3 = (as.numeric(mydata$Pscav[1])-table[3])*100/table[3]
    dev_4 = (as.numeric(mydata$PressCompAvg[1])-table[5])*100/table[5]
    dev_5 = (as.numeric(mydata$TCrpm[1])-table[1])*100/table[1]
    dev_6 = (as.numeric(mydata$TCrpm[1])-table[7])*100/table[7]
    dev_7 =  (as.numeric(mydata$ExTempAvg[1])-table[6])*100/table[6]
    
    m= matrix(NA,2,6 )
    colnames(m)=c("SFOC Vs BHP","PScav' Vs T/CSpeed'","PComp' Vs PScav'","T/CSpeed' Vs EngSpeed ","T/CSpeed' Vs BHP","ExhTemp' Vs BHP")
    rownames(m)=c("std","actual")
    m[1,]=c(rep(0,6))
    m[2,]=c(dev_1,dev_3,dev_4,dev_5,dev_6,dev_7)
    m = as.data.frame(m)
    m=rbind(rep(20,6) , rep(-20,6),m)
    par(bg='grey90')
    radarchart(m)
    colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) )
    colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4)  )
    radarchart( m  , axistype=1 , 
                #custom polygon
                pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
                #custom the grid
                cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(-20,20,10), cglwd=0.8,
                #custom labels
                vlcex=0.8
                
    )
    
  }
  
  
})

output$enginechart <- renderUI({
  x = VESSELDETAILS
  x = subset(x,Vessel == input$engineVessel)
  
  if(x$PM =="Y"){
    selectInput("FLEET1","Select the Graph",choices = c("Load Diagram & Torque Rich "=1,"T/C Speed Vs Engine Speed"=2,
                                                        "Pscav Vs T/C Speed"=4,"Press. Drop at A/C Vs Pscav"=5,
                                                        "Pcomp Vs Pscav"=6,"Texh Vs Engine Load"=7,"T/C Speed Vs Engine Load"=8,"SFOC Vs Engine Load"=9,
                                                        "Pump Mark Vs Engine Speed"=3,"Pmax-Pcomp Vs Pump mark"=10),selected = 1,width="25%")
  }
  else{
    selectInput("FLEET1","Select the Graph",choices = c("Load Diagram & Torque Rich "=1,"T/C Speed Vs Engine Speed"=2,
                                                        "Pscav Vs T/C Speed"=3,"Press. Drop at A/C Vs Pscav"=4,
                                                        "Pcomp Vs Pscav"=5,"Texh Vs Engine Load"=6,"T/C Speed Vs Engine Load"=7,"SFOC Vs Engine Load"=8),
                selected = 1,width="25%")
  }
})

output$Chart = renderUI({
  
  x = VESSELDETAILS
  x = subset(x,Vessel == input$engineVessel)
  
  if(x$PM =="Y"){
    i=input$FLEET1
    if(i==1){
      tagList(column(width=6,br(),box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("graph_Load_diagram"))),
              
              column(width=6,br(),box(width=NULL,solidHeader = T,title = "Test Data",status = "info",dataTableOutput("historical_Daigram")))
              
      ) }
    else if(i==2){tagList(
      column(width=6,br(),box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("graphTCEn")),box(width = NULL,solidHeader = TRUE,title = "Test Data",status = "info",dataTableOutput("historicTCEn"))),
      column(width=6,br(),box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("graphTCEn1")),box(width=NULL,solidHeader = TRUE,title = "Shop Trial Data",status = "info",dataTableOutput("shoptrialTCEn")))
      
    )}
    else if(i==3){tagList(
      column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("graphPMEn")),box(width = NULL,solidHeader = TRUE,title = "Test Data",status = "info",dataTableOutput("historicPMEn"))),
      column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("graphPMEn1")),box(width=NULL,solidHeader = TRUE,title = "Shop Trial Data",status = "info",dataTableOutput("shoptrialPMEn")))
      
    )}
    
    else if(i==4){tagList(
      column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("graphPsTC")),box(width = NULL,solidHeader = TRUE,title = "Test Data",status = "info",dataTableOutput("historicPsTC"))),
      column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("graphPsTC1")),box(width=NULL,solidHeader = TRUE,title = "Shop Trial Data",status = "info",dataTableOutput("shoptrialPsTC")))
      
    )}
    
    else if(i==5){tagList(
      column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("graphPdPs")),box(width = NULL,solidHeader = TRUE,title = "Test Data",status = "info",dataTableOutput("historicPdPs"))),
      column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("graphPdPs1")),box(width=NULL,solidHeader = TRUE,title = "Shop Trial Data",status = "info",dataTableOutput("shoptrialPdPs")))
      
    )}
    
    else if(i==6){tagList(
      column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("graphPcPs")),box(width = NULL,solidHeader = TRUE,title = "Test Data",status = "info",dataTableOutput("historicPcPs"))),
      column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("graphPcPs1")),box(width=NULL,solidHeader = TRUE,title = "Shop Trial Data",status = "info",dataTableOutput("shoptrialPcPs")))
      
    )}
    
    else if(i==7){tagList(
      column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("graphETEL")),box(width = NULL,solidHeader = TRUE,title = "Test Data",status = "info",dataTableOutput("historicETEL"))),
      column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("graphETEL1")),box(width=NULL,solidHeader = TRUE,title = "Shop Trial Data",status = "info",dataTableOutput("shoptrialETEL")))
      
    )}
    
    else if(i==8){tagList(
      column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("graphTCspeed")),box(width = NULL,solidHeader = TRUE,title = "Test Data",status = "info",dataTableOutput("historicTCspeed"))),
      column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("graphTCspeed1")),box(width=NULL,solidHeader = TRUE,title = "Shop Trial Data",status = "info",dataTableOutput("shoptrialTCspeed")))
      
    )}
    
    else if(i==9){tagList(
      column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("graphSFOC")),box(width = NULL,solidHeader = TRUE,title = "Test Data",status = "info",dataTableOutput("historicSFOC"))),
      column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("graphSFOC1")),box(width=NULL,solidHeader = TRUE,title = "Shop Trial Data",status = "info",dataTableOutput("shoptrialSFOC")))
      
    )}
    
    else if(i==10){tagList(
      column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("graphPM")),box(width = NULL,solidHeader = TRUE,title = "Test Data",status = "info",dataTableOutput("historicPM"))),
      column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("graphPM1")),box(width=NULL,solidHeader = TRUE,title = "Shop Trial Data",status = "info",dataTableOutput("shoptrialPM")))
      
    )}
    
    else{return()}
  }
  else {
    i=input$FLEET1
    if(i==1){
      tagList(column(width=6,br(),box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("graph_Load_diagram"))),
              
              column(width=6,br(),box(width=NULL,solidHeader = T,title = "Test Data",status = "info",dataTableOutput("historical_Daigram")))
              
      ) }
    else if(i==2){tagList(
      column(width=6,br(),box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("graphTCEn")),box(width = NULL,solidHeader = TRUE,title = "Test Data",status = "info",dataTableOutput("historicTCEn"))),
      column(width=6,br(),box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("graphTCEn1")),box(width=NULL,solidHeader = TRUE,title = "Shop Trial Data",status = "info",dataTableOutput("shoptrialTCEn")))
      
    )}
    
    
    else if(i==3){tagList(
      column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("graphPsTC")),box(width = NULL,solidHeader = TRUE,title = "Test Data",status = "info",dataTableOutput("historicPsTC"))),
      column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("graphPsTC1")),box(width=NULL,solidHeader = TRUE,title = "Shop Trial Data",status = "info",dataTableOutput("shoptrialPsTC")))
      
    )}
    
    else if(i==4){tagList(
      column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("graphPdPs")),box(width = NULL,solidHeader = TRUE,title = "Test Data",status = "info",dataTableOutput("historicPdPs"))),
      column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("graphPdPs1")),box(width=NULL,solidHeader = TRUE,title = "Shop Trial Data",status = "info",dataTableOutput("shoptrialPdPs")))
      
    )}
    
    else if(i==5){tagList(
      column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("graphPcPs")),box(width = NULL,solidHeader = TRUE,title = "Test Data",status = "info",dataTableOutput("historicPcPs"))),
      column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("graphPcPs1")),box(width=NULL,solidHeader = TRUE,title = "Shop Trial Data",status = "info",dataTableOutput("shoptrialPcPs")))
      
    )}
    
    else if(i==6){tagList(
      column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("graphETEL")),box(width = NULL,solidHeader = TRUE,title = "Test Data",status = "info",dataTableOutput("historicETEL"))),
      column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("graphETEL1")),box(width=NULL,solidHeader = TRUE,title = "Shop Trial Data",status = "info",dataTableOutput("shoptrialETEL")))
      
    )}
    
    else if(i==7){tagList(
      column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("graphTCspeed")),box(width = NULL,solidHeader = TRUE,title = "Test Data",status = "info",dataTableOutput("historicTCspeed"))),
      column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("graphTCspeed1")),box(width=NULL,solidHeader = TRUE,title = "Shop Trial Data",status = "info",dataTableOutput("shoptrialTCspeed")))
      
    )}
    
    else if(i==8){tagList(
      column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("graphSFOC")),box(width = NULL,solidHeader = TRUE,title = "Test Data",status = "info",dataTableOutput("historicSFOC"))),
      column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("graphSFOC1")),box(width=NULL,solidHeader = TRUE,title = "Shop Trial Data",status = "info",dataTableOutput("shoptrialSFOC")))
      
    )}
    
    
    
    else{return()}
  }
  
})

#.................................For power curve.........................................................................................

powerplotdata <- reactive({
  
  
  r = shoptrialdata()
  Engine.Load = r$Load
  SFOC = r$SFOC
  PSCAV = r$pscav
  PMAX = r$pmax
  PCOMP = r$pcomp
  Engine.RPM = r$engine.speed
  TC.RPM = r$TCspeed
  Date1 = r$Date
  
  
  m = data.frame(Engine.Load,PSCAV,SFOC,PMAX,TC.RPM,PCOMP,Engine.RPM)
  m
  
  
})

powerplotdata1 <- reactive({
  
  r = enginedatatable()
  Engine.Load = r$Engine.Load
  SFOC = r$SFOC
  PSCAV = r$Pscav
  PMAX = r$MaxPressAvg
  PCOMP = r$PressCompAvg
  Engine.RPM = r$RPM
  TC.RPM = r$TCrpm
  Date1 = r$Date
  
  
  m = data.frame(Engine.Load,PSCAV,SFOC,PMAX,TC.RPM,PCOMP,Engine.RPM)
  m
  
  
})

powerplotdata11 <- reactive({
  
  r = enginedatatable()
  
  Date = r$Date
  
  
  m = data.frame(Date)
  m
  
  
})

output$ooopowerplot <- renderPlotly({
  
  pp = data.frame(powerplotdata())
  p <- plot_ly(pp, x = ~Engine.Load, y = ~PSCAV,type ="scatter", mode= "line+markers",height = 200)
  q <- plot_ly(pp, x = ~Engine.Load, y = ~SFOC,type ="scatter", mode= "line+markers",height = 200)
  r <- plot_ly(pp, x = ~Engine.Load, y = ~PMAX,type ="scatter", mode= "line+markers",height = 200)
  s <- plot_ly(pp, x = ~Engine.Load, y = ~TC.RPM,type ="scatter", mode= "line+markers",height = 200)
  t <- plot_ly(pp, x = ~Engine.Load, y = ~PCOMP,type ="scatter", mode= "line+markers",height = 200)
  u <- plot_ly(pp, x = ~Engine.Load, y = ~Engine.RPM,type ="scatter", mode= "line+markers",height = 200)
  
  subplot(p,q,r,s,t,u, nrows = 6, shareX = TRUE,titleY = TRUE) %>% layout(yaxis = list(domain = c(0, 0.16)), 
                                                                          yaxis2 = list(domain = c(0.16, 0.32)))
  
  
})








output$powerplot <- renderPlotly({
  
  r = powerplotdata()
  b = powerplotdata1()
  c = powerplotdata11()
  mydata = enginedatatable()
  
  
  pt <- r%>%
    tidyr::gather(variable,value, -Engine.Load) %>%
    transform(id = as.integer(factor(variable))) 
  
  p=plot_ly(pt,x = ~Engine.Load, y = ~value , color = ~variable, colors = "Dark2", type ="scatter", mode= "lines+markers",line=list(shape="spline"), width = 800, height = 800,
            yaxis = ~paste0("y", id), legendgroup =  ~ variable ,showlegend = FALSE)
  
  clr = c("blue","red","#884EA0","green","#ccb400","#800000","#CD5C5C","#FFA07A","teal","#E6B0AA","#082336","#F4D03F")
  
  for(i in 1:input$monthno){
    bt1 <-  b[i,]%>%
      tidyr::gather(variable, value, -Engine.Load) %>%
      transform(id = as.integer(factor(variable))) 
    
    p = p%>%add_markers(x = bt1$Engine.Load, y = bt1$value, color = bt1$variable,name = c$Date[i],text=c$Date[i], type ="scatter",mode = "markers",marker=list(size=8,color=clr[i]),
                        yaxis = paste0("y", bt1$id),showlegend = F)
  }
  
  p = p%>%subplot(nrows = 6, shareX = TRUE,titleY = TRUE ) %>%  
    layout(autosize = F,yaxis =list(title = "TC RPM"),yaxis6 =list(title = "SFOC"),yaxis2 =list(title = "Engine RPM"),yaxis3 =list(title = "PCOMP"),yaxis4 =list(title = "PMAX"),yaxis5 =list(title = "PSCAV"))
  
  p
  
  
})

output$datetext <- renderText({
  mydata = enginedatatable()
  Date = mydata$Date
  Date1 = as.data.frame(Date)
  clr = c("blue","red","#884EA0","green","#ccb400","#800000","#CD5C5C","#FFA07A","teal","#E6B0AA","#082336","#F4D03F")
  
  HTML(sprintf("<li  style='color:%s;font-size:20px'> <text style='color:#000000;font-size:12px'>  %s <br/> </text></li>",clr[1:input$monthno],t(Date1[1:input$monthno,1]))) 
  
})

#..............................................cylinder comparison.....................................................................#

output$cylpara <- renderUI({
  x = VESSELDETAILS
  x = subset(x,Vessel == input$engineVessel)
  if(x$PM == "Y"){
    selectInput("cylcomp","Select the Parameter",
                choices = c("Maximum Pressure"=1,"Compressor Pressure"=2,"M/E Cyl. Exhaust Temperature"=3,"Pump Mark"=4),
                selected = 1,width ="25%")
  }
  else{
    selectInput("cylcomp","Select the Parameter",
                choices = c("Maximum Pressure"=1,"Compressor Pressure"=2,"M/E Cyl. Exhaust Temperature"=3),
                selected = 1,width ="25%")
  }
})

output$cylchart <- renderUI({
  
  
  x = VESSELDETAILS
  x = subset(x,Vessel == input$engineVessel)
  if(x$PM == "Y"){
    i = input$cylcomp
    if(i == 1){
      box(width=NULL,solidHeader = TRUE,status = "info",column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("Pmax"))),column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",dataTableOutput("MPtable1"))))
    }
    
    else if(i == 2){
      box(width=NULL,solidHeader = TRUE,status = "info",column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("PComp"))),column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",dataTableOutput("PCtable1"))))
    }
    
    else if(i == 3){
      box(width=NULL,solidHeader = TRUE,status = "info",column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("exhausttemp"))),column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",dataTableOutput("ETtable1"))))
    }
    
    else if(i == 4){
      box(width=NULL,solidHeader = TRUE,status = "info",column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("pumpmark"))),column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",dataTableOutput("PMtable1"))))
    }
    else {return(NULL)}
  }
  
  else if(x$PM == "N"){
    i = input$cylcomp
    if(i == 1){
      box(width=NULL,solidHeader = TRUE,status = "info",column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("Pmax"))),column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",dataTableOutput("MPtable1"))))
    }
    
    else if(i == 2){
      box(width=NULL,solidHeader = TRUE,status = "info",column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("PComp"))),column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",dataTableOutput("PCtable1"))))
    }
    
    else if(i == 3){
      box(width=NULL,solidHeader = TRUE,status = "info",column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",plotlyOutput("exhausttemp"))),column(width=6,box(width=NULL,solidHeader = TRUE,status = "info",dataTableOutput("ETtable1"))))
    }
    
    
    else {return(NULL)}
    
    
    
    
    
  }
}
)


