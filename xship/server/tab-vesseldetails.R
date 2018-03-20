
#VESSEL DETAIL....

Vpq <- reactive({
  y= VESSELDETAILS
  if(is.null(y))
    return(NULL)
  validate(
    need(try(input$Vessel),"Please Wait or Select the vessel")
  )
  ff=input$Vessel
  y=suppressWarnings(subset(y,y$Vessel == ff))
})

output$vfleet <- renderUI({ 
  r=DATA
  s=input$selecttype
  if(is.null(s))
    return(NULL)
  if(s=="Fleet Wise")
  {
    Fleet_List =suppressWarnings(unique(as.character(r[,1]), incomparables = FALSE))
    selectInput("Fleet", label=h4(strong("Fleet")), choices = Fleet_List, selected = 1, multiple = FALSE, selectize = TRUE, width = "50%", size = NULL)
  }else {return(NULL)}
})


output$selectUI <- renderUI({ 
  r=DATA
  s=input$selecttype
  if(is.null(s))
    return(NULL)
  if(s=="Fleet Wise"){
    r=subset(r,Fleet == input$Fleet)
    Vessel_List = suppressWarnings(unique(as.character(r[,3]), incomparables = FALSE))
    selectInput("Vessel", label=h4(strong("Vessel")), choices = Vessel_List, selected = "Strategic Alliance", multiple = FALSE, selectize = TRUE, width = "50%", size = NULL)
  }
  else{
    
    Vessel_List = suppressWarnings( unique(as.character(r[,3]), incomparables = FALSE))
    selectInput("Vessel", label=h4(strong("Vessel")), choices = Vessel_List, selected = "Strategic Alliance", multiple = FALSE, selectize = TRUE, width = "50%", size = NULL)
  }
})
output$yob <- renderUI({
  r=Vpq()
  if(is.null(r))
    return(NULL)
  yob= suppressWarnings(unique(as.numeric(r[,8]), incomparables = FALSE))
  numericInput("yob",label="YOB",value=yob)
})


output$loa <- renderUI({
  r=Vpq()
  if(is.null(r))
    return(NULL)
  loa=suppressWarnings(unique(as.numeric(r[,9]), incomparables = FALSE))
  numericInput("loa",label="LOA (m)",value=loa)
})

output$b <- renderUI({
  r=Vpq()
  if(is.null(r))
    return(NULL)
  b=suppressWarnings(unique(as.numeric(r[,10]), incomparables = FALSE))
  numericInput("b",label="Moulded Breadth (m)",value=b)
})

output$DIS <- renderUI({
  r=Vpq()
  if(is.null(r))
    return(NULL)
  DIS=suppressWarnings(unique(as.numeric(r[,11]), incomparables = FALSE))
  numericInput("DIS",label="Displacement(Scantling/Design Draft) (T)",value=DIS)
})

output$Draft1UI <- renderUI({
  r=Vpq()
  if(is.null(r))
    return(NULL)
  draft1=suppressWarnings(unique(as.numeric(as.character(r[,4])), incomparables = FALSE))
  numericInput("draft1",label="Ballast Draft (m)",value=draft1)
})

output$Draft2UI <- renderUI({
  r=Vpq()
  if(is.null(r))
    return(NULL)
  draft2=suppressWarnings(unique(as.numeric(r[,5]), incomparables = FALSE))
  numericInput("draft2",label="Scantling/Design Draft (m)",value=draft2)
})

output$speed2UI <- renderUI({
  r=Vpq()
  if(is.null(r))
    return(NULL)
  speed2 = suppressWarnings(unique(as.numeric(r[,6]), incomparables = FALSE))
  numericInput("speed2",label="Max Service Speed (Knots)",value=speed2)
})
output$MCRUI <- renderUI({
  r=Vpq()
  if(is.null(r))
    return(NULL)
  mcr = suppressWarnings(unique(as.numeric(r[,7]), incomparables = FALSE))
  numericInput("MCR",label="Max Continuous Rating (kW)",value=mcr)
})


# image vessel ...

output$vesselimage=renderUI({
  vessel=input$Vessel
  if(is.null(vessel))
    return(NULL)
  vessel=str_replace_all(vessel, fixed(" "), "")
  filename <- paste(vessel,".jpg",sep="")
  validate(
    need(try(filename),"NO IMAGE AVAILABLE .....")
  )
  s=tags$img(src = filename, width=1600, height=500)
  
})




#hydros data.......


Hydros<- reactive({
  y=data.frame(read.csv("data/Hydros Data.csv"))
  ff=input$Vessel
  y=subset(y,y$Vessel == ff)
  
})


H=reactive({
  inhydros =input$hydros
  if (is.null(inhydros))
  {y=Hydros()}
  else{
    hydros=read.csv(inhydros$datapath)}
})


output$Hplot=renderPlotly({
  if (is.null(H()))
    return(NULL)
  p= plot_ly(data = H(), x= ~Draft, y= ~WSA,name="WSA",type = 'scatter',mode = 'lines+markers',line=list(color= "#CD3131") ,marker=list(color= "#CD3131") )%>%
      layout(title="WSA vs DRAFT ",titlefont=c,
           xaxis = list(title = "Draft (meter)", titlefont =f, tickfont =f,gridcolor = "#FFFFFF"),
           yaxis = list(title = "Wetted Surface Area (Sq.meter)",   titlefont =f, tickfont = f,gridcolor = "#ABB2B9"), 
           plot_bgcolor = "#FFFFFF",
           paper_bgcolor = "#FFFFFF"
           
    )
})

output$Dplot=renderPlotly({
  if (is.null(H()))
    return(NULL)
  plot_ly(data = H(), x= ~Draft, y= ~Displ,name="Displ",type = 'scatter',mode = 'lines+markers',line=list(color= "#74B49B") ,marker=list(color= "#74B49B") )%>%
    
    layout(title="DISPLACEMENT Vs DRAFT",titlefont=c,
           xaxis = list(title = "Draft (meter)", titlefont =f, tickfont =f,gridcolor = "#FFFFFF"),
           yaxis = list(title = "DIisplacement (Tonne)",   titlefont = f, tickfont = f,gridcolor = "#ABB2B9"), 
           plot_bgcolor = "#FFFFFF",
           paper_bgcolor = "#FFFFFF"
    )
  
  
})
output$hydros= renderDataTable({
  
  y=H()
  y$Vessel = NULL
  y$Fleet=NULL
  
  
  datatable(y,class = 'cell-border stripe', rownames = FALSE,options = list(autoWidth = TRUE,searching = FALSE,paging = FALSE))%>%
    formatStyle(names(y),color="#000")
})

output$N1=renderText({
  paste("Draft coefficient(Draft & WSA Relation) n1 :",n1())
})


#shoptrial data .....................
shopdata = reactive ({
  y=Shoptrial
  ff=input$Vessel
  y=subset(y,y$VESSEL.NAME==ff)
  y$VESSEL.NAME = NULL
  y$FLEET=NULL
  y$CLASS=NULL
  colnames(y)=c("ENGINE LOAD %","POWER(kW)","SFOC Measured(g/kW-Hr)","SFOC Corrected(g/kW-Hr)")
  y
})
output$shoptable <- DT::renderDataTable({ 
  y=shopdata()
  
  datatable(y,class = 'cell-border stripe',options = list(autoWidth = TRUE,searching = FALSE,paging = FALSE),rownames = FALSE)%>%
    formatStyle(names(y),color="#000")
  
})

testfit=reactive({
  y=shopdata()
  if (is.null(y))
    return(NULL)
  x1 = y[, 1]
  y1 = y[, 3]
  y2 = y[, 4]
  
  fit2aa = lm(y1 ~ poly(x1, 2, raw = TRUE))
  n1 = as.numeric(fit2aa$coefficients[3])
  n2 = as.numeric(fit2aa$coefficients[2])
  k = as.numeric(fit2aa$coefficients[1])
  
  fit2bb = lm(y2 ~ poly(x1, 2, raw = TRUE))
  n1b = as.numeric(fit2bb$coefficients[3])
  n2b = as.numeric(fit2bb$coefficients[2])
  kb = as.numeric(fit2bb$coefficients[1])
  
  testx = seq(min(x1), max(x1), length.out = 30)
  testy1 = testx ^ 2 * n1 + testx * n2 + k
  testy2 = testx ^ 2 * n1b + testx * n2b + kb
  test=data.frame(testx,testy1,testy2)
  test
})

output$Shopplot=renderPlotly({
  
  y=shopdata()
  if (is.null(y))
    return(NULL)
  test=testfit()
  p  = plot_ly(
    y,
    x = ~y[, 1],
    y = ~y[, 3],
    name = "SFOC Measured", type = 'scatter',mode='markers',
    marker = list(size = 8, color = "#74B49B"),
    showlegend = T) %>%
    add_trace(y,
              x = ~y[, 1],
              y = ~y[, 4],
              name = "SFOC Measured", type = 'scatter',mode='markers',
              marker = list(size = 8, color = "#CD3131"),
              showlegend = T)%>%
    add_trace(test,x= test$testx,y = test$testy1, type = 'scatter',mode='lines+markers',
              marker=list(opacity=0,color = "#74B49B"), line = list(shape = "spline",color = "#74B49B"),showlegend = FALSE)%>%
    
    add_trace(test,x= test$testx,y = test$testy2, type = 'scatter',mode='lines+markers', 
              marker=list(opacity=0,color = "#CD3131"), line = list(shape = "spline",color = "#CD3131"),showlegend = FALSE)
  
  p = p%>% layout(title="LOAD(%) Vs SFOC",titlefont=c,
                  xaxis = list(title = "Load(%)", titlefont =f, tickfont =f,gridcolor = "#FFFFFF"),
                  yaxis = list(title = "SFOC(g/kW-Hr)",   titlefont = f, tickfont =f,gridcolor = "#ABB2B9"), 
                  plot_bgcolor = "#FFFFFF",
                  paper_bgcolor = "#FFFFFF",legend = l
  )
  p
  
})


#Sea trial Data............................................................
seatrialdb = reactive({
  y=seatrialdata
  y=subset(y,y$Vessel==input$Vessel)
  y$Vessel=NULL
  y$Fleet= NULL
  y$Class =NULL
  y
  
})
output$seatrialtable<- DT::renderDataTable({ 
  y=seatrialdb()
  colnames(y)=c("Speed (kn)","Power (kW)")
  datatable(y,options = list(autoWidth = TRUE,searching = FALSE,paging = FALSE),rownames = FALSE)
  
})

output$seatrialplot=renderPlotly({
  plot_ly(data = seatrialdb(), x= ~Speed, y= ~Sea.Trial.Power,name="Ballast Draft",type = 'scatter',mode = 'lines+markers',line = list(shape = "spline") )%>%
    layout(title="Sea Trial Curve",titlefont=s,
           xaxis = list(title = "Speed (knots)", titlefont =s, tickfont =s,gridcolor = "white"),
           yaxis = list(title = "Power (kW)",   titlefont = s, tickfont = s,gridcolor="#ABB2B9"), 
           plot_bgcolor = "white",
           paper_bgcolor = "white",legend = l
    )
  
})


