

# Fleet comparison .....................


cfile= Coeff

compvesselpart <- reactive({
  y=VESSELDETAILS
  validate(
    need(try(input$compvessel),"Please Wait or Select the vessel")
  )
  ff=input$compvessel[1]
  y=subset(y,y$Vessel == ff)
})

output$compvessel <- renderUI({ 
  r=cfile
  s=input$selectiontype
  if(s=="Fleet Wise"){
    r=subset(r,Fleet == input$compfleet)
    Vessel_List = unique(as.character(r[,3]), incomparables = FALSE)
    selectInput("compvessel", label=strong("Vessel"), choices = Vessel_List, selected =Vessel_List[1], multiple = TRUE, selectize = TRUE, width = "75%", size = NULL)
  }else{
    Vessel_List = unique(as.character(r[,3]), incomparables = FALSE)
    selectInput("compvessel", label=strong("Vessel"), choices = Vessel_List, selected =Vessel_List[1], multiple = TRUE, selectize = TRUE, width = "75%", size = NULL)
  }
})

output$compfleet <- renderUI({ 
  r=cfile
  s=input$selectiontype
  if(s=="Fleet Wise"){
    Vessel_List = unique(as.character(r[,1]), incomparables = FALSE)
    selectInput("compfleet", label=strong("Fleet"), choices = Vessel_List, selected = Vessel_List[1], multiple = FALSE, selectize = TRUE, width = "30%", size = NULL)
  }
  else{
    return(NULL)
  }
})

output$compdraft <- renderUI({  
  numericInput("compDraft", label=strong("Draft (In meter)"), value = 6, width = "25%")
})
output$cSS <- renderUI({ 
  selectInput("cSS", label=strong("Sea state"), choices = c(0,1,2,3,4,5,6,7,8), selected = NULL, multiple = FALSE, selectize = TRUE,width = "25%", size = NULL)
})




Cdata<-reactive({
  
  vpq=compvesselpart()
  if(is.null(vpq)){return()}
  spd= unique(as.numeric(vpq[,6]), incomparables = FALSE)
  validate(
    need(try(spd),"Please Wait or Select the Vessel....")
  )
  speed <- c(10:spd)
  Vsl <- c(input$compvessel)
  draft <- input$compDraft
  ss<-as.numeric(input$cSS)
  m= matrix(NA,length(speed),length(Vsl))
  colnames(m)=Vsl
  row.names(m)=speed
  
  for(i in 1:length(Vsl))
  {
    y = cfile
    y = subset(y,y$vessel==Vsl[i])
    v=y$daterange[order(format(as.character(y$daterange)))[1]]
    v=as.character(v)
    y=subset(y,y$daterange==v)
    m[,i]=round(draft^as.numeric(y$n1)*speed^as.numeric(y$n2)*(10-ss)^as.numeric(y$n3)*exp(as.numeric(y$k)),2)
    
  }
  
  m=as.data.frame(m) 
  
  
})

output$CPOWER <- renderDataTable({
  x=Cdata()
  validate(
    need(try(x),"Please Wait or Select the Vessel....")
  )
  datatable(x,class = 'cell-border stripe', options = list(searching = FALSE,paging = FALSE),colnames = c('Speed' = 1))%>%
    formatStyle(names(x),color="#212F3D",backgroundColor = "#ECF0F1")%>%formatStyle('Speed',  color = '#ECF0F1', backgroundColor = '#515A5A', fontWeight = 'bold')
})


output$compspeedpower=renderPlotly({
  
  vpq=compvesselpart()
  spd= unique(as.numeric(vpq[,6]), incomparables = F)
  validate(
    need(try(spd),"Please Wait or Select the Vessel....")
  )
  speed <- c(10:spd)
  Vsl <- c(input$compvessel)
  mydata=Cdata()
  c=length(Vsl)
  p=plot_ly(x=rownames(mydata),y=mydata[,1],name=Vsl[1],type="scatter",mode="lines+markers")
  if(c>=2){
    for(i in 2: length(Vsl)){
      
      p=p%>%add_trace(x=rownames(mydata),y=mydata[,i],name=Vsl[i],type="scatter",mode="lines+markers")
      
    } 
  }
  
  p=p%>%layout(title = "Power Comparison",titlefont= c,xaxis=Speed_axis,yaxis=Power_axis,
               legend = l,  plot_bgcolor = "#FFFFFF", paper_bgcolor = "#FFFFFF",showlegend = TRUE)
})

CFdata<-reactive({
  vpq=compvesselpart()
  spd= unique(as.numeric(vpq[,6]), incomparables = FALSE)
  validate(
    need(try(spd),"Please Wait or Select the Vessel....")
  )
  speed <- c(10:spd)
  Vsl <- c(input$compvessel)
  draft <- input$compDraft
  ss<-as.numeric(input$cSS)
  m= matrix(NA,length(speed),length(Vsl))
  colnames(m)=Vsl
  row.names(m)=speed
  
  for(i in 1:length(Vsl))
  {
    y=cfile
    y =subset(y,y$vessel==Vsl[i])
    v=y$daterange[order(format(as.character(y$daterange)))[1]]
    v=as.character(v)
    y=subset(y,y$daterange==v)
    m[,i]=round(draft^as.numeric(y$n1)*speed^as.numeric(y$f2)*(10-ss)^as.numeric(y$f3)*exp(as.numeric(y$k1)),1)
  }
  m=as.data.frame(m)
  
})
output$CFO <- renderDataTable({
  x=CFdata()
  validate(
    need(try(x),"Please Wait or Select the Vessel....")
  )
  datatable(x,class = 'cell-border stripe' ,options = list(searching = FALSE,paging = FALSE),colnames = c('Speed' = 1))%>%
    formatStyle(names(x),color="#212F3D",backgroundColor = "#ECF0F1")%>%formatStyle('Speed',  color = '#ECF0F1', backgroundColor = '#515A5A', fontWeight = 'bold')
})



output$compspeedfo=renderPlotly({
  
  vpq=compvesselpart()
  if(is.null(vpq)){return()}
  spd= unique(as.numeric(vpq[,6]), incomparables = FALSE)
  validate(
    need(try(spd),"Please Wait or Select the Vessel....")
  )
  speed <- c(10:spd)
  Vsl <- c(input$compvessel)
  my=CFdata()
  if(is.null(my)){return()}
  c=length(Vsl)
  p=plot_ly(x=rownames(my),y=my[,1],name= Vsl[1],type="scatter",mode="lines+markers")
  if(c>=2){
    for(i in 2: length(Vsl)){
      p=p%>%add_trace(x=rownames(my),y=my[,i],name= Vsl[i],type="scatter",mode="lines+markers")
      
    }
  }
  p=p%>%layout(title = "FO Comparison",titlefont=c,xaxis=Speed_axis,yaxis=FO_axis,
               legend = l,  plot_bgcolor = "#FFFFFF", paper_bgcolor = "#FFFFFF",showlegend = TRUE)
})

#Op Comparison...
OPC=reactive({
  dataf=DATA
  dataf=subset(dataf,Report.Type == "NOON")
  VESSEL= dataf$Vessel.Name;DATE=dataf$Corrected.Date;DRAFT=round(dataf$DRAFT,2);SPEED=round(dataf$SOG,2);RPM=dataf$RPM;SLIP=dataf$SLIP;
  POWER=dataf$POWER.kW;FO=as.numeric(dataf$FO.per.24Hrs);SEASTATE=as.numeric(as.character(dataf$SEA.STATE));SFOC=as.numeric(dataf$SFOC)
  Noon=data.frame(VESSEL,DATE,SEASTATE,DRAFT,SPEED,RPM,SLIP,POWER,FO,SFOC)
  
})


CSS<-reactive({
  SEASTATE=c(0:8)
  vessel <- c(input$compvessel)
  m= matrix(NA,length(SEASTATE),length(vessel))
  colnames(m)=vessel
  row.names(m)=SEASTATE
  for (j in 1:length(vessel))
  {
    x=0
    y= OPC()
    y= subset(y,VESSEL == vessel[j] )
    y$SEASTATE=round(y$SEASTATE,0)
    t=nrow(y)
    
    for(i in 1:length(SEASTATE))
    {  
      y
      h=SEASTATE[i]  
      k=h+1
      d = subset(y,SEASTATE >= h & SEASTATE < k)
      a=(nrow(d))
      m[i,j]=percent(a/t)
    }
    
  }
  m=as.data.frame(m)    
  
})


output$CSEAS=renderPlotly({
  
  validate(
    need(try(CSS()),"Please Wait or Select the Vessel....")
  )
  vessel <- c(input$compvessel)
  mydata=CSS()
  c=length(vessel)
  p=plot_ly(y=vessel[1],x=mydata[,1],type="bar",color=rownames(mydata), marker = list(line=list(color="#FFFFFF",width=1)),orientation = 'h',showlegend = T)
  
  if(c>=2){
    for(i in 2:c){
      
      p=p%>%add_trace(y=vessel[i],x=mydata[,i],type="bar",color=rownames(mydata), evaluate = TRUE, marker = list(line=list(color="#FFFFFF",width=1)),orientation = 'h',showlegend = F)
      
      
    } 
    
  }
  
  p=p%>%layout( bargap=0.4,margin=list(l=110,r=30),barmode="stack",title = "SS COMPARISON",titlefont=s,domain=c(0.5,1),xaxis = list(title = "PERCENTAGE (%)", showgrid=F,showline=F,titlefont =s, tickfont =s,gridcolor = "#ABB2B9"),
                yaxis = list(title = "VESSEL",   titlefont = s, tickfont = s,tickangle=325,showgrid=F,showline=F,gridcolor = "#ABB2B9"))%>%layout(legend = l,  plot_bgcolor = "#FFFFFF",
                                                                                                                                                  paper_bgcolor = "#FFFFFF"
                                                                                                                                                  
                )
})

# DRAFT COMPARISON ...............

CD<-reactive({
  
  Draft=c(4:16)
  vessel <- c(input$compvessel)
  m= matrix(NA,length(Draft),length(vessel))
  colnames(m)=vessel
  row.names(m)=Draft
  for (j in 1:length(vessel))
  {
    
    y= OPC()
    y= subset(y,VESSEL == vessel[j] )
    y$DRAFT=round(y$DRAFT,0)
    t=nrow(y)
    
    for(i in 1:length(Draft))
    {  
      y
      h=Draft[i]
      k=h+1
      d = subset(y,DRAFT >= h & DRAFT < k)
      a=(nrow(d))
      m[i,j]=percent(a/t)
    }
  }
  m=as.data.frame(m)
})


output$CDP=renderPlotly({
  validate(
    need(try(CD()),"Please Wait or Select the Vessel....")
  )
  vessel <- c(input$compvessel)
  mydata=CD()
  c=length(vessel)
  
  p=plot_ly(y=vessel[1],x=mydata[,1],type="bar", color=row.names(mydata),marker = list(line=list(color="#F3g8FD",width=1)),orientation = 'h',showlegend = T)
  
  if(c>=2){
    for(i in 2:c){
      
      p=p%>%add_trace(y=vessel[i],x=mydata[,i],type="bar", evaluate = TRUE,color=row.names(mydata) ,marker = list(line=list(color="#FFFFFF",width=1)),orientation = 'h',showlegend = F)
      
    } 
    p=p%>%layout(bargap=0.4, margin=list(l=110),barmode="stack",title = "DRAFT COMPARISON",titlefont=s,xaxis = list(title = "PERCENTAGE (%)", showgrid=F,showline=F,titlefont =s, tickfont =s,gridcolor = "#ABB2B9"),
                                     yaxis = list(title = "VESSEL",   titlefont = s, tickfont = s,tickangle=325,showgrid=F,showline=F,gridcolor = "#ABB2B9"))%>%layout(legend = l,  plot_bgcolor = "#FFFFFF",
                                                                                                                                                                       paper_bgcolor = "#FFFFFF"
                                                                                                                                                                       
                                     )
  }
})


# Speed comparison ..............

CS<-reactive({
  
  vessel <- c(input$compvessel)
  m= matrix(NA,length(vessel))
  #colnames(m)=AVG.SPEED
  row.names(m)=vessel
  for (j in 1:length(vessel))
  {
    
    y= OPC()
    y= subset(y,VESSEL == vessel[j] )
    y$SPEED=round(y$SPEED,0)
    d = mean(y$SPEED)
    m[j,]=round(d,1)
  }
  
  m=as.data.frame(m)
  
})


output$CSPEED <- renderDataTable({
  x=CS()
  datatable(x,class = 'cell-border stripe', options = list(autoWidth = TRUE,searching = FALSE,paging = FALSE),colnames = c('DRAFT' = 1))%>%
    formatStyle(names(x),color="#212F3D",backgroundColor = "#FFFFFF")%>%formatStyle('DRAFT',  color = '#FFFFFF', backgroundColor = '#FFFFFF', fontWeight = 'bold')
})



csop= function(vessel){
  validate(
    need(try(CS()),"Please Wait or Select the Vessel....")
  )
  vessel <- c(input$compvessel)
  mydata=CS()
  c=length(vessel)
  
  p=plot_ly(y=vessel[1],x=mydata[1,1],type="bar",name="AVG.SPEED", marker = list(color="#BB8FCE",line=list(color="#FFFFFF",width=1)),orientation = 'h',showlegend = F)
  
  if(c>=2){
    for(i in 2:c){
      
      add_trace(y=vessel[i],x=mydata[i,1],type="bar",name="AVG.SPEED", evaluate = TRUE,marker = list(color="#BB8FCE",line=list(color="#FFFFFF",width=1)),orientation = 'h',showlegend = FALSE)
      
    } 
    
  }else{return(NULL)}
  
}

output$CSP=renderPlotly({
  
  sp=csop(input$compvessel)%>%layout( bargap=0.4,margin=list(l=110,r=50),barmode="stack",title = "AVG.SPEED COMPARISON",titlefont=s,xaxis = list(title = "SPEED", showgrid=F,showline=F,titlefont =s, tickfont =s,gridcolor = "#ABB2B9"),
                                      yaxis = list(title = "VESSEL",   titlefont = s, tickfont = s,tickangle=325,showgrid=F,showline=F,gridcolor = "#ABB2B9"))%>%
    layout(legend = l,  plot_bgcolor = "#FFFFFF", paper_bgcolor = "#FFFFFF")
  
  
})
