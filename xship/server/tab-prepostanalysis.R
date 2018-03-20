

# PREPOST ANALYSIS..................................
premcr <- reactive({
  y=data.frame(read.csv("data/Vessel Detail.csv"))
  validate(
    need(try(input$preVessel),"Please Wait or Select the vessel")
  )
  ff=input$preVessel
  y=subset(y,y$Vessel == ff)
  y = unique(as.numeric(y[,7]), incomparables = FALSE)
  mcr = as.numeric (y)
})


PPvesselpart <- reactive({
  y=data.frame(read.csv("data/Vessel Detail.csv"))
  validate(
    need(try(input$preVessel),"Please Wait or Select the vessel")
  )
  ff=input$preVessel
  y=subset(y,y$Vessel == ff)
})


output$prevessel <- renderUI({ 
  r=DATA
  r=subset(r,Fleet==input$prefleet)
  Vessel_List = unique(as.character(r[,3]), incomparables = FALSE)
  selectInput("preVessel", label=h4(strong("Vessel")), choices = Vessel_List, selected = NULL, multiple = FALSE, selectize = TRUE, width = "75%", size = NULL)
})
output$prefleet <- renderUI({ 
  r=DATA
  Vessel_List = unique(as.character(r[,1]), incomparables = FALSE)
  selectInput("prefleet", label=h4(strong("Fleet")), choices = Vessel_List, selected = NULL, multiple = FALSE, selectize = TRUE, width = "75%", size = NULL)
})

output$predraftlimit <- renderUI({
  r= PPvesselpart()
  minD = unique(as.numeric(as.character(r[,4])), incomparables = FALSE)
  maxD = unique(as.numeric(r[,5]), incomparables = FALSE)
  sliderInput("predraft",label = "Draft Range",min = minD,max = maxD,value=c(minD,maxD),step = 0.5,width = "75%" )
})
output$prespeedlimit <- renderUI({
  r= PPvesselpart()
  maxS = round(unique(as.numeric(r[,6]), incomparables = FALSE),0)
  sliderInput("prespeed",label = "Speed Range",min = 9,max = maxS,value = c(10,maxS), step = 0.5,width = "75%")
})
output$presliplimit <- renderUI({
  sliderInput("preslip",label = "Slip Range",min = -20,max = 20,value = c(-20,20), step = 0.5,width = "75%")
})
output$prepowerlimit <- renderUI({
  y=premcr()
  x=0.15*premcr()
  sliderInput("prePower",label = "Power Range",min = x ,max = y,value = c(x, y), width = "75%")
})
output$presteaminglimit <- renderUI({
  sliderInput("presteam",label = "Steaming Time",min = 0,max = 26,value = c(5,24), step = 0.5,width = "75%")
})
prexx=reactive({
  y=DATA
  dataf=subset(y,y$Vessel.Name==input$preVessel)
  validate(
    need(try(y),"Please Wait....")
  )
  dataf=subset(dataf,Report.Type == "NOON")
  VESSEL= dataf$Vessel.Name;DATE=dataf$Corrected.Date;DRAFT=round(dataf$DRAFT,2);SPEED=round(dataf$SOG,2);STW=round(dataf$SOW,2);RPM=as.numeric(dataf$RPM);SLIP=as.numeric(dataf$SLIP);
  POWER=as.numeric(as.character(dataf$POWER.kW));FO=dataf$FO.per.24Hrs;SEASTATE=as.numeric(as.character(dataf$SEA.STATE));SFOC=as.numeric(as.character(dataf$SFOC));STEAMINGTIME=as.numeric(dataf$TOTAL.STEAMING.TIME)
  Noon=data.frame(VESSEL,DATE,SEASTATE,DRAFT,SPEED,STW,RPM,SLIP,POWER,FO,SFOC,STEAMINGTIME)
})
predata=reactive({
  input$ppgobutton
  if(input$ppgobutton==0){return(NULL)}
  Noon=prexx()
  isolate({
    Noon=subset(Noon,as.Date(DATE,"%d-%m-%y")>=as.Date(input$predates[1],"%d-%m-%y")& as.Date(DATE,"%d-%m-%y")<=as.Date(input$predates[2],"%d-%m-%y"))
    Noon=subset(Noon,SPEED> as.numeric(input$prespeed[1]) & SPEED < as.numeric(input$prespeed[2]) & SEASTATE>=as.numeric(input$preSEASTATE[1]) & SEASTATE<= as.numeric(input$preSEASTATE[2]))
    Noon=subset(Noon, POWER> as.numeric(input$prePower[1]) & POWER <  as.numeric(input$prePower[2]))
    Noon=subset(Noon,DRAFT > as.numeric(input$predraft[1]) & DRAFT < as.numeric (input$predraft[2]))
    Noon=subset(Noon,SFOC > as.numeric(input$preSFOC[1]) & SFOC < as.numeric(input$preSFOC[2]))
    Noon=subset(Noon,SLIP > as.numeric(input$preslip[1]) & SLIP < as.numeric(input$preslip[2]))
    Noon$DD=as.numeric(1)
  })
  Noon
}
)

postdata=reactive({
  input$ppgobutton
  if(input$ppgobutton==0){return(NULL)}
  Noon=prexx()
  isolate({
    Noon=subset(Noon,as.Date(DATE,"%d-%m-%y")>=as.Date(input$postdates[1],"%d-%m-%y")& as.Date(DATE,"%d-%m-%y")<=as.Date(input$postdates[2],"%d-%m-%y"))
    Noon=subset(Noon,SPEED> as.numeric(input$prespeed[1]) & SPEED < as.numeric(input$prespeed[2]) & SEASTATE>=as.numeric(input$preSEASTATE[1]) & SEASTATE<= as.numeric(input$preSEASTATE[2]))
    Noon=subset(Noon, POWER> as.numeric(input$prePower[1]) & POWER <  as.numeric(input$prePower[2]))
    Noon=subset(Noon,DRAFT > as.numeric(input$predraft[1]) & DRAFT < as.numeric (input$predraft[2]))
    Noon=subset(Noon,SFOC > as.numeric(input$preSFOC[1]) & SFOC < as.numeric(input$preSFOC[2]))
    Noon=subset(Noon,SLIP > as.numeric(input$preslip[1]) & SLIP < as.numeric(input$preslip[2]))
    Noon$DD=as.numeric(2)
  })
  Noon
})
output$preFplots=renderPlotly({
  predata=predata()
  postdata=postdata()
  if(is.null(predata)){return(NULL)}
  if(is.null(postdata)){return(NULL)}
  validate(
    need(try(predata,postdata),"Please Wait....")
  )
  i=input$preP1axis
  if(i==1){ y=plot_ly(data = predata, x = ~SPEED , y = ~POWER , type="scatter",mode = "markers",marker=list(color="#27AE60",size = 9),name="Pre Data")
  y=y%>%add_trace( data = postdata,x=~SPEED,y=~POWER,type="scatter", mode = "markers",marker=list(color="#5C96F5",size = 9),name="Post Data")}
  if(i==2){ y=plot_ly(data = predata, x = ~SPEED , y = ~FO ,  type="scatter",mode = "markers",marker=list(color="#FB00FF",size = 9),name="Pre Data")
  y=y%>%add_trace(data = postdata,x=~SPEED,y=~FO,type="scatter", mode = "markers",marker=list(color="#5C96F5",size = 9),name="Post Data")}
  
  y%>%layout(
    xaxis = list(title = "Speed (knots)", titlefont =f, tickfont =f,gridcolor = "#FFFFFF"),
    yaxis = list(titlefont = f, tickfont =f,gridcolor = "#ABB2B9"), 
    plot_bgcolor = "#FFFFFF",
    paper_bgcolor = "#FFFFFF",legend=l
  )
  
})

output$preFdd=renderPlotly({
  
  predata=predata()
  postdata=postdata()
  if(is.null(predata)){return(NULL)}
  if(is.null(postdata)){return(NULL)}
  i=input$preFaxis
  validate(
    need(try(predata,postdata),"Please Wait or Select the vessel")
  )
  
  predata$DATE=as.Date(predata$DATE,"%d-%m-%y")
  postdata$DATE=as.Date( postdata$DATE,"%d-%m-%y")
  
  if(i==1){ y=plot_ly(data = predata, x = ~DATE , y = ~POWER ,type="scatter", mode = "markers",marker=list(color="#27AE60",size = 9),name="Pre Data")
  y=y%>%add_trace( data = postdata,x =~DATE  , y = ~POWER, type="scatter",mode = "markers",marker=list(color="#5C96F5",size = 9),name="Post Data")}
  if(i==2){ y=plot_ly(data = predata, x =~DATE , y = ~FO, type="scatter",mode = "markers",marker=list(color="#27AE60",size = 9),name="Pre Data")
  y=y%>%add_trace( data = postdata,x = ~DATE  , y = ~FO, type="scatter",mode = "markers",marker=list(color="#5C96F5",size = 9),name="Post Data")}
  if(i==3){ y=plot_ly(data = predata,x = ~DATE  , y = ~SFOC ,type="scatter", mode = "markers",marker=list(color="#27AE60",size = 9),name="Pre Data")
  y=y%>%add_trace( data = postdata,x = ~DATE , y = ~SFOC, type="scatter",mode = "markers",marker=list(color="#5C96F5",size = 9),name="Post Data") }
  if(i==4){y= plot_ly(data = predata,x = ~DATE   , y = ~SPEED , type="scatter",mode = "markers",marker=list(color="#27AE60",size = 9),name="Pre Data")
  y=y%>%add_trace( data = postdata,x =~DATE , y = ~SPEED,type="scatter", mode = "markers",marker=list(color="#5C96F5",size = 9),name="Post Data")  }
  if(i==5){y= plot_ly(data =predata, x = ~DATE  ,   y = ~DRAFT  ,type="scatter", mode = "markers",marker=list(color="#27AE60",size = 9),name="Pre Data")
  y=y%>%add_trace( data = postdata,x = ~DATE , y = ~DRAFT ,type="scatter", mode = "markers",marker=list(color="#5C96F5",size = 9),name="Post Data") }
  if(i==6){y= plot_ly(data = predata, x = ~DATE  , y = ~SEASTATE ,type="scatter", mode = "markers",marker=list(color="#27AE60",size = 9),name="Pre Data")
  y=y%>%add_trace( data = postdata,x =~DATE , y = ~SEASTATE , type="scatter",mode = "markers",marker=list(color="#5C96F5",size = 9),name="Post Data")  }
  if(i==7){y= plot_ly(data = predata, x =~DATE  , y = ~RPM ,type="scatter", mode = "markers",marker=list(color="#27AE60",size = 9),name="Pre Data")
  y=y%>%add_trace( data = postdata,x =~DATE , y = ~RPM , type="scatter",mode = "markers",marker=list(color="#5C96F5",size = 9),name="Post Data")  }
  if(i==8){y= plot_ly(data = predata, x = ~DATE  , y = ~SLIP , type="scatter",mode = "markers",marker=list(color="#27AE60",size = 9),name="Pre Data")
  y=y%>%add_trace( data = postdata,x =~DATE , y = ~SLIP ,type="scatter", mode = "markers",marker=list(color="#5C96F5",size = 9),name="Post Data")  }
  

  y%>%layout(
    xaxis = list(title = "Date", titlefont =f, tickfont =f,gridcolor = "#FFFFFF",tickangle=0),
    yaxis = list(titlefont = f, tickfont = f,gridcolor = "#ABB2B9"), 
    plot_bgcolor ="#FFFFFF",
    paper_bgcolor = "#FFFFFF",showlegend = TRUE,legend=l
    
  )
})

prepostdata=reactive({
  y=predata()
  x=postdata()
  if(is.null(y)){return(NULL)}
  if(is.null(x)){return(NULL)}
  validate(
    need(try(y,x),"Please Wait or Select the vessel")
  )
  z=rbind(y,x)
  z
})



output$predata=renderDataTable({
  y=prepostdata()
  if(is.null(y)){return(NULL)}
  validate(
    need(try(y),"Please Wait or Select the vessel")
  )
  datatable(y,class = 'cell-border stripe',options = list(pageLength = 150,searching = FALSE,paging = T,audowidth=T,dom = "tip",
                                                          scrollX=TRUE, scrollY = 1200,
                                                          scroller = T),colnames = c("Vessel","Date","Sea State","Draft","Speed","STW","RPM","Slip","Power","FO","SFOC","Steaming Time","DD"),rownames = FALSE)%>%
    formatStyle(names(y),backgroundColor = "#ECF0F1",color="#000", className = 'dt-center')
  
})

output$prepostdata <- downloadHandler(
  filename = function() { 'filtered_data.csv' }, content = function(file) {
    write.csv(prexx(), file, row.names = FALSE)
  }
)

# PROCEDURE CALCULATION>>>>
#Analysis Procedure......
#Power PREDICTION.............

ppfit=reactive({
  y=prepostdata()
  x=ppn1()
  P=y$POWER
  ss=log(10-y$SEASTATE)
  dd= y$DD
  DD=log(dd)
  D=y$DRAFT
  Speed=as.numeric(y$SPEED)
  S=log(Speed)
  Pd=log(P/D^(as.numeric(ppn1())))
  fit=lm(Pd ~ DD+ss+S)
  
})
ppfitss=reactive({
  y=prepostdata()
  x=ppn1()
  P=y$POWER
  ss=log(10-y$SEASTATE)
  dd= y$DD
  DD=log(dd)
  D=y$DRAFT
  Speed=as.numeric(y$SPEED)
  S=log(Speed)
  Pd=log(P/(D^(as.numeric(ppn1()))*ss^(-0.3)))
  fit=lm(Pd ~ DD+S)
  
})

ppn1=reactive({
  vpq=PPvesselpart()
  minD=unique(as.numeric(as.character(vpq[,4])), incomparables = FALSE)
  maxD = unique(as.numeric(vpq[,5]), incomparables = FALSE)
  y=data.frame(read.csv("data/Hydros Data.csv"))
  ff=input$preVessel
  if(is.null(ff))
    return(NULL)
  y=subset(y,y$Vessel == ff)
  y=subset(y,Draft >= minD & Draft <= maxD)
  n1=lm((log(y$WSA))~(log(y$Draft)))$coeff[2]
})

# coefficients

ppcoeffdata=reactive({
  fit=ppfit()
  fitss=ppfitss()
  n1=ppn1()
  if(fit$coefficients[3] >=0){
    n2=fitss$coefficients[3]
    n3=-0.3
    DD=fitss$coefficients[2]
    k=fitss$coefficients[1]
  } else {
    n2=fit$coefficients[4]
    n3=fit$coefficients[3]
    DD=fit$coefficients[2]
    k=fit$coefficients[1]
  }
  x=data.frame(n1,n2,n3,DD,k)
})

#FO Predicition .......................
ppfit1=reactive({
  y=prepostdata()
  x=ppn1()
  P=y$FO
  ss=log(10-y$SEASTATE)
  dd= y$DD
  DD=log(dd)
  D=y$DRAFT
  Speed=as.numeric(y$SPEED)
  S=log(Speed)
  Pd=log(P/D^(as.numeric(ppn1())))
  fit=lm(Pd ~ DD+ss+S)
  
})
ppfit1ss=reactive({
  y=prepostdata()
  x=ppn1()
  P=y$FO
  ss=log(10-y$SEASTATE)
  dd= y$DD
  DD=log(dd)
  D=y$DRAFT
  Speed=as.numeric(y$SPEED)
  S=log(Speed)
  Pd=log(P/(D^(as.numeric(ppn1()))*ss^(-0.3)))
  fit=lm(Pd ~ DD+S)
  
})
# FO coefficients
ppcoeffdatafo=reactive({
  fit=ppfit1()
  fitss=ppfit1ss()
  n1=ppn1()
  if(fit$coefficients[3] >=0){
    n2=fitss$coefficients[3]
    n3=-0.3
    DD=fitss$coefficients[2]
    k=fitss$coefficients[1]
  } else {
    n2=fit$coefficients[4]
    n3=fit$coefficients[3]
    DD=fit$coefficients[2]
    k=fit$coefficients[1]
  }
  x=data.frame(n1,n2,n3,DD,k)
})

prepostPP=reactive({
  y=prepostdata()
  P=ppcoeffdata()
  FO=ppcoeffdatafo()
  y$Pred_POWER=round((y$DD^P$DD*y$DRAFT^P$n1*y$SPEED^P$n2*(10-y$SEASTATE)^P$n3*exp(P$k)),2)
  y$Pred_FO=round((y$DD^FO$DD*y$DRAFT^FO$n1*y$SPEED^FO$n2*(10-y$SEASTATE)^FO$n3*exp(FO$k)),2)
  y$Power_Error=((y$Pred_POWER-y$POWER)/(y$POWER))
  y$FO_Error=((y$Pred_FO-y$FO)/(y$FO))
  y=y
})

output$preEPOWER <- renderValueBox({
  P=ppcoeffdata()
  DD=P$DD
  if(is.null(DD)){return()}
  DPower=round(((1-2^DD)*100),2)  
  if(DPower>=0){
    valueBox(
      paste0(DPower, "%"), strong("Gain in Power %"), 
      color = "green",width = NULL) }
  else {
    valueBox(
      paste0(DPower, "%"), strong("Loss in Power %"), 
      color = "red",width = NULL) 
  }
  
})

output$preEFO <- renderValueBox({
  P=ppcoeffdatafo()
  DD=P$DD
  if(is.null(DD)){return()}
  DF = round(((1-2^DD)*100),2)
  if(DF>=0){
    valueBox(
      paste0(DF, "%"), strong("Gain in FO %"), 
      color = "green",width = NULL) }
  else {
    valueBox(
      paste0(DF, "%"), strong("Loss in FO %"), 
      color = "red",width = NULL) 
  }
})


output$prePPtable=renderDataTable({
  if (is.null(prepostPP()))
    return(NULL)
  y=prepostPP()
  validate(
    need(try(y),"Please Wait or Select the vessel")
  )
  datatable(y,class = 'cell-border stripe',options = list(pageLength = 150,searching = FALSE,paging = T,audowidth=T,dom = "tip",
                                                          scrollX=TRUE, scrollY =1200,
                                                          scroller = T ),rownames = FALSE)%>%formatPercentage('FO_Error', 1)%>% formatPercentage('Power_Error', 1)%>%
    formatStyle(names(y),backgroundColor = "#ECF0F1",color="#212F3D")
})

output$prePowerplot=renderPlotly({
  dataf=prepostPP()
  validate(
    need(try(dataf),"Please Wait or Select the vessel")
  )
  if (is.null(dataf))
    return(NULL)
  
  p=plot_ly(data =dataf, x = ~SPEED, y = ~POWER,type="scatter", mode = "markers",marker=list(color="green",size=12),name="Observed Power")
  p=p%>%add_trace(data= dataf,x = ~SPEED,y = ~Pred_POWER,type="scatter",mode= "markers",marker=list(color="yellow",size=12),name="Predicted Power") 
  p=p%>%layout(title="Power Vs Speed",titlefont=c)%>%layout(legend = l,
                                                                           xaxis = list(title = " Speed (knots)", titlefont =f,tickfont = f, gridcolor = "#FFFFFF",tickangle=0),
                                                                           yaxis = list(title= "Power (kW)",titlefont = f, tickfont = f,gridcolor = "#ABB2B9"), 
                                                                           plot_bgcolor = "#FFFFFF",
                                                                           paper_bgcolor = "#FFFFFF",showlegend = TRUE
                                                                           
  )
  
})

output$preFOplot=renderPlotly({
  dataf=prepostPP()
  validate(
    need(try(dataf),"Please Wait or Select the vessel")
  )
  if (is.null(dataf))
    return(NULL)
  p=plot_ly(data =prepostPP(), x = ~SPEED, y = ~FO, type="scatter",mode = "markers",marker=list(color="green",size=12),name="Obsserved FO")
  p=p%>%add_trace(data= prepostPP(),x = ~SPEED,y = ~Pred_FO,type="scatter",mode= "markers",marker=list(color="yellow",size=12),name="Predicted FO")
  p=p%>%layout(title="FO Vs Speed",titlefont =c)%>%layout(legend = l,
                                                                         xaxis = list(title = " Speed (knots)", titlefont =f, tickfont =f,gridcolor = "#FFFFFF",tickangle=0),
                                                                         yaxis = list(title= "FO (tonne)",titlefont = f, tickfont = f,gridcolor = "#ABB2B9"), 
                                                                         plot_bgcolor = "#FFFFFF",
                                                                         paper_bgcolor = "#FFFFFF",showlegend = TRUE
                                                                         
  )
})

#PREPOSToperating Profile......................
#DRAFT

PREOPD= reactive({
  y=prepostPP()
  y$DRAFT=round(y$DRAFT,0)
  PREOPD=ddply(y,.(DRAFT,DD),nrow)
  t=sum(PREOPD$V1)
  PREOPD$V1=(round((PREOPD$V1/t)*100,2))
  PREOPD
})
output$preOPDraft = renderPlotly({
  yy=PREOPD()
  validate(
    need(try(yy),"Please Select the vessel ....")
  )
  x=dplyr::filter(yy,DD==1)
  y=dplyr::filter(yy,DD==2)
  p = plot_ly(
              x =x$DRAFT,
              y =x$V1,
              name = "Pre Draft",
              type = "bar",marker=list(color="#06BDF7"))
  p = p%>% add_trace(
                     x = y$DRAFT,
                     y = y$V1,
                     name = "Post Draft",
                     type = "bar",marker=list(color="#EEC815"))
  p = p %>% layout(
    xaxis = list(title = "Draft", titlefont =f, tickfont =f,gridcolor = "#FFFFFF"),
    yaxis = list(title = "Percentage (%)",   titlefont = f, tickfont = f,gridcolor = "#ABB2B9"), 
    plot_bgcolor = "#FFFFFF",
    paper_bgcolor = "#FFFFFF",legend=l,barmode = 'group', bargap = 0.15
    
  )
  
})


#SPEED

PREOPS= reactive({
  y=prepostPP()
  y$SPEED=round(y$SPEED,0)
  PREOPD=ddply(y,.(SPEED,DD),nrow)
  t=sum(PREOPD$V1)
  PREOPD$V1=(round((PREOPD$V1/t)*100,2))
  PREOPD
})
output$preOPSpeed = renderPlotly({ 
  yy=PREOPS()
  validate(
    need(try(yy),"Please Select the vessel ....")
  )
  x=dplyr::filter(yy,DD==1)
  y=dplyr::filter(yy,DD==2)
  p = plot_ly(
              x = x$SPEED,
              y =x$V1,
              name = "Pre Speed",
              type = "bar",marker=list(color="#EA83E2"))
  p = p%>% add_trace(
                     x = y$SPEED,
                     y = y$V1,
                     name = "Post Speed",
                     type = "bar",marker=list(color="#6A5CF5"))
  p = p %>% layout(
    xaxis = list(title = "Speed", titlefont =f, tickfont =f,gridcolor = "#FFFFFF"),
    yaxis = list(title = "Percentage (%)",   titlefont = f, tickfont = f,gridcolor = "#ABB2B9"), 
    plot_bgcolor = "#FFFFFF",
    paper_bgcolor = "#FFFFFF",legend=l,barmode = 'group', bargap = 0.15
    
  )
  
})

#SEASTATE

PREOPSS= reactive({
  y=prepostPP()
  y$SEASTATE=round(y$SEASTATE,0)
  PREOPD=ddply(y,.(SEASTATE,DD),nrow)
  t=sum(PREOPD$V1)
  PREOPD$V1=(round((PREOPD$V1/t)*100,2))
  PREOPD
})
output$preOPSEAS = renderPlotly({
  yy=PREOPSS()
  validate(
    need(try(yy),"Please Select the vessel ....")
  )
  x=dplyr::filter(yy,DD==1)
  y=dplyr::filter(yy,DD==2)
  p <- plot_ly(
               x = x$SEASTATE,
               y = x$V1,
               name = "Pre SS",
               type = "bar",marker=list(color="#94EA83"))
  p =p%>% add_trace(
                    x = y$SEASTATE,
                    y = y$V1,
                    name = "Post SS",
                    type = "bar",marker=list(color="#EEC815"))
  p = p %>% layout(
    xaxis = list(title = "Sea State", titlefont =f, tickfont =f,gridcolor = "#FFFFFF"),
    yaxis = list(title = "Percentage (%)",   titlefont =f, tickfont = f,gridcolor = "#ABB2B9"), 
    plot_bgcolor = "#FFFFFF",
    paper_bgcolor = "#FFFFFF",legend=l,barmode = 'group', bargap = 0.15
  )
  
})
#Power

PREOPP= reactive({
  y=prepostPP()
  y$POWER=round((y$POWER/1000),0)*1000
  PREOPD=ddply(y,.(POWER,DD),nrow)
  t=sum(PREOPD$V1)
  PREOPD$V1=(round((PREOPD$V1/t)*100,2))
  PREOPD
})

output$preOPPower = renderPlotly({
  yy=PREOPP()
  validate(
    need(try(PREOPP()),"Please Select the vessel ....")
  )
  x=dplyr::filter(yy,DD==1)
  y=dplyr::filter(yy,DD==2)
  p = plot_ly(
              x = x$POWER,
              y = x$V1,
              name = "Pre Power",
              type = "bar",marker=list(color="#F55CAF"))
  p = p %>%  add_trace(
                       
                       x = y$POWER,
                       y = y$V1,
                       name = "Post Power",
                       type = "bar",marker=list(color="#5CF55E"))
  p = p %>% layout(
    xaxis = list(title = "Power", titlefont =s, tickfont =s,gridcolor = "#FFFFFF"),
    yaxis = list(title = "Percentage (%)",   titlefont = s, tickfont = s,gridcolor = "#ABB2B9"), 
    plot_bgcolor = "#FFFFFF",
    paper_bgcolor = "#FFFFFF",legend=l,barmode = 'group', bargap = 0.15
  )
  
})


#performance curve for pre post analaysis ............
output$predraft <- renderUI({  
  numericInput("preDraft", label=strong("Draft (In meter)"), value = 6, width = "25%")
})
output$preSS <- renderUI({ 
  selectInput("preSS", label=strong("Sea State"), choices = c(0,1,2,3,4,5,6,7,8), selected = NULL, multiple = FALSE, selectize = TRUE,width = "25%", size = NULL)
})

preCdata<-reactive({
  vpq=PPvesselpart()
  P=ppcoeffdata()
  maxS=unique(as.numeric(vpq[,6]), incomparables = FALSE)
  x=c("Before","After")
  speed <- seq(10, maxS)
  draft <- input$preDraft
  ss<-as.numeric(input$preSS)
  m= matrix(NA,length(speed),length(x))
  colnames(m)=x
  row.names(m)=speed
  
  for(i in 1:length(x))
  {
    m[,i]=round((i)^P$DD*draft^P$n1*speed^P$n2*(10-ss)^P$n3*exp(P$k),2)
    
  }
  m=as.data.frame(m)    
  m
})
output$prePOWER <- renderDataTable({
  validate(
    need(try(preCdata()),"Please select the vessels....")
  )
  x=preCdata()
  if(is.null(x)){return()}
  datatable(x,class = 'cell-border stripe', options = list(autoWidth = TRUE,searching = FALSE,paging = FALSE),colnames = c('Speed' = 1))%>%
    formatStyle(names(x),color="#212F3D",backgroundColor = "#ECF0F1")%>%formatStyle('Speed',  color = '#ECF0F1', backgroundColor = '#515A5A', fontWeight = 'bold')
})


output$prespeedpower=renderPlotly({
  
  validate(
    need(try(preCdata()),"Please select the vessels....")
  )
  
  mydata=preCdata()
  
  p=plot_ly(x=rownames(mydata),y=mydata[,1],name="Pre Analysis",type="scatter",mode="lines+markers")
  
  p= p%>%add_trace(x=rownames(mydata),y=mydata[,2],name="Post Analysis",type="scatter",mode="lines+markers")
  
  
  sp=p%>%layout(title = "Power Comparison",titlefont=c,xaxis=Speed_axis,yaxis=Power_axis,showlegend=TRUE)%>%layout(legend = l,  plot_bgcolor = "#FFFFFF",
                                                                                                                   paper_bgcolor = "#FFFFFF",showlegend = TRUE
                                                                                                                   
  )
})

preCFdata<-reactive({
  vpq=PPvesselpart()
  FO=ppcoeffdatafo()
  maxS=unique(as.numeric(vpq[,6]), incomparables = FALSE)
  speed <- seq(10, maxS)
  x=c("Before","After")
  draft <- input$preDraft
  ss<-as.numeric(input$preSS)
  m= matrix(NA,length(speed),length(x))
  colnames(m)=x
  row.names(m)=speed
  
  for(i in 1:length(x))
  {
    
    m[,i]=round((i)^FO$DD*draft^FO$n1*speed^FO$n2*(10-ss)^FO$n3*exp(FO$k),2)
  }
  m=as.data.frame(m)    
  m
})
output$preFO <- renderDataTable({
  validate(
    need(try(preCFdata()),"Please Wait or Select the Vessel....")
  )
  x=preCFdata()
  if(is.null(x)){return()}
  datatable(x, class = 'cell-border stripe',options = list(autoWidth = TRUE,searching = FALSE,paging = FALSE),colnames = c('Speed' = 1))%>%
    formatStyle(names(x),color="#212F3D",backgroundColor = "#ECF0F1")%>%formatStyle('Speed',  color = '#ECF0F1', backgroundColor = '#515A5A', fontWeight = 'bold')
})

output$prespeedfo=renderPlotly({
  validate(
    need(try(preCFdata()),"Please select the vessels....")
  )
  my=preCFdata()
  p=plot_ly(x=rownames(my),y=my[,1],name="Pre Analysis",type="scatter",mode="lines+markers")
  p=p%>%add_trace(x=rownames(my),y=my[,2],name="Post Analysis",type="scatter",mode="lines+markers")
  
  sp=p%>%layout(title = "FO Comparison",titlefont=c,xaxis=Speed_axis,yaxis=FO_axis,showlegend=TRUE)%>%layout(legend = l,  plot_bgcolor = "#FFFFFF",
                                                                                                             paper_bgcolor = "#FFFFFF",showlegend = TRUE
                                                                                                             
  )
})


output$ppreEPOWER <- renderValueBox({
  P=ppcoeffdata()
  DD=P$DD
  if(is.null(DD)){return()}
  DPower=round(((1-2^DD)*100),2)  
  if(DPower>=0){
    valueBox(
      paste0(DPower, "%"), strong("Gain in Power %"), 
      color = "green",width = NULL) }
  else {
    valueBox(
      paste0(DPower, "%"), strong("Loss in Power %"), 
      color = "red",width = NULL) 
  }
})


output$ppreEFO <- renderValueBox({
  P=ppcoeffdatafo()
  DD=P$DD
  if(is.null(DD)){return()}
  DF = round(((1-2^DD)*100),2)
  if(DF>=0){
    valueBox(
      paste0(DF, "%"), strong("Gain in FO %"), 
      color = "green",width = NULL) }
  else {
    valueBox(
      paste0(DF, "%"), strong("Loss in FO %"), 
      color = "red",width = NULL) 
  }
})