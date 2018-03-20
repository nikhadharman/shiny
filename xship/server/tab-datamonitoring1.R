#FLEET OPERATION OVERVIEW........... 
output$Dmeasure <- renderUI({ 
  selectInput("Dmeasure",label="Parameter Selection",choices = c("TOTAL M/E FOC(Tons)","TOTAL CARGO(MT)","TOTAL CYL OIL(Tons)","TOTAL A/E FOC(Tons)","TOTAL TEU","TOTAL Blr FOC(Tons)","ROB BILGE","ROB SLUDGE"), selected = "TOTAL M/E FOC(Tons)", multiple = F, selectize = TRUE, width = "50%", size = NULL)
})

output$Dselect <- renderUI({ 
  
  y=DATA
  Fleet_List = suppressWarnings(unique(as.character(y[,1]), incomparables = FALSE))
  selectInput("DFleet",label="Fleet Selection",choices = Fleet_List , selected = 1, multiple = F, selectize = TRUE, width = "75%", size = NULL)
})
output$DClass <- renderUI({ 
  
  y=DATA
  Class_List = suppressWarnings(unique(as.character(y[,2]), incomparables = FALSE))
  selectInput("DDVessel",label="Class Selection",choices = Class_List , selected = "ASTRID", multiple = F, selectize = TRUE, width = "75%", size = NULL)
})


output$Aggreg_ConsEqp<- renderUI({ 
  radioButtons("Aggreg_conseqp",label="Grouping",choices = c("Daily","Monthly","Quarterly","Yearly") , selected = "Daily", inline = T)
})

output$Aggreg_ConsFuel<- renderUI({ 
  radioButtons("Aggreg_consfuel",label="Grouping",choices = c("Daily","Monthly","Quarterly","Yearly") , selected = "Daily", inline = T)
})

output$Aggreg_TimePara<- renderUI({ 
  radioButtons("Aggreg_timepara",label="Grouping",choices = c("Daily","Monthly","Quarterly","Yearly") , selected = "Daily", inline = T)
})

output$ConEqp <- renderUI({ 
  selectInput("ConEqp",label="Selection",choices = c("Main Engine(mt)","AE System(mt)","Boilers(mt)","All Equipments(mt)") , selected = "All Equipments(mt)", multiple = F, selectize = TRUE, width = "75%", size = NULL)
})
output$ConFuel <- renderUI({ 
  selectInput("ConFuel",label="Selection",choices = c("Fuel HS","Fuel LS","Fuel MDO","Fuel MGO","Fuel MGO LS","All Type Fuel") , selected = "ALL", multiple = F, selectize = TRUE, width = "75%", size = NULL)
})
output$Timepara <- renderUI({ 
  selectInput("Timepara",label="Selection",choices = c("Avg Speed GPS (kn)","Avg Speed Log (kn)","Fwd Draft (m)","Aft Draft (m)","Mean Draft (m)","Trim (m)","Seastate","M/E Avg Power (kW)","M/E Avg SFOC (gr/kWh)","M/E Avg Rpm (rpm)","M/E Avg Slip","M/E Avg SCOC (gr/kWh)","ROB FO SLUDGE") , selected = "M/E Avg SFOC (gr/kWh)", multiple = F, selectize = TRUE, width = "75%", size = NULL)
})
output$Relationpara <- renderUI({ 
  selectInput("Relationpara",label="Selection",choices = c("Power(kW) Vs SFOC (g/kWh)" = 1,"Power(kW) Vs SCOC (g/kWh)"=2, "Speed(kn) Vs Fo Cons(t)"=3,"A/E Running Time (Hr) Vs FO Cons (T)"=4,"Engine RPM Vs Power"=5) , selected = 1, multiple = F, selectize = TRUE, width = "75%", size = NULL)
})

output$Dateselect <- renderUI({ 
  y=DATA
  y = subset(y,Fleet==input$DFleet)
  y$tmp <-  as.Date(y$Corrected.Date,'%d-%m-%y')
  y$s=format(y$tmp,"%b'%Y")
  Date_List = suppressWarnings(unique(as.character(y$s), incomparables = FALSE))
  selectInput("Dateselect",label="Date",choices = Date_List , selected = 1, multiple = F, selectize = TRUE, width = "75%", size = NULL)
})


#FLEET DIATANCE OVERVIEW............................
output$Distance=renderPlotly({
  validate(
    need(try(input$DFleet),"Please Wait ...")
  )
  y=DATA 
  y = subset(y,Fleet==input$DFleet)
  y$tmp <-  as.Date(y$Corrected.Date,'%d-%m-%y')
  y$s=format(y$tmp,"%b'%Y")
  y = subset(y,s==input$Dateselect)
  u=ddply(y, .(Vessel.Name,s),summarize,tot=sum(MILES.BY.GPS))
  p= plot_ly(x=as.character(u$Vessel.Name),y=u$tot,type="bar",marker = list(color= "#5299d3 "))
  p=p%>%layout(title = "Distance (GPS)",titlefont=c,width=NULL,xaxis=list(title = paste("Fleet:",input$DFleet), titlefont =f, tickfont =f,gridcolor = "#FFFFFF",rangemode = "nonzero"),
           yaxis=list(title = "Distance GPS (nm)", titlefont =f, tickfont =f,gridcolor = "#E5E7E9"),showlegend=F,legend = l, margin = list(l=70,b = 85,r=70), plot_bgcolor = "#FFFFFF",paper_bgcolor = "#FFFFFF",showlegend = T,width=NULL
  )
  
})

output$Tconsumption=renderPlotly({
  validate(
    need(try(input$DFleet),"Please Wait ...")
  )
  y=DATA 
  y = subset(y,Fleet==input$DFleet)
  y$tmp <-  as.Date(y$Corrected.Date,'%d-%m-%y')
  y$MEcon = as.numeric(as.character(y$FUEL.M.E.HS))+as.numeric(as.character(y$FUEL.M.E.LS))+as.numeric(as.character(y$FUEL.M.E.MDO))+as.numeric(as.character(y$FUEL.M.E.MGO))+as.numeric(as.character(y$FUEL.M.E.MGO.LS))
  y$AEcon = as.numeric(as.character(y$FUEL.AUX.HS))+as.numeric(as.character(y$FUEL.AUX.LS))+as.numeric(as.character(y$FUEL.AUX.MDO))+as.numeric(as.character(y$FUEL.AUX.MGO))+as.numeric(as.character(y$FUEL.AUX.MGO.LS))
  y$BLcon = as.numeric(as.character(y$FUEL.BOILER.HS))+as.numeric(as.character(y$FUEL.BOILER.LS))+as.numeric(as.character(y$FUEL.BOILER.MDO))+as.numeric(as.character(y$FUEL.BOILER.MGO))+as.numeric(as.character(y$FUEL.BOILER.MGO.LS))
  y$Tcon= y$MEcon + y$AEcon + y$BLcon 
  y$s=format(y$tmp,"%b'%Y")
  y = subset(y,s==input$Dateselect)
  u=ddply(y, .(Vessel.Name,s),summarize,tot=sum(Tcon))
  p= plot_ly(x=as.character(u$Vessel.Name),y=u$tot,type="bar",marker = list(color="#68d8d6"))
  p=p%>%layout(title = "Consumption (mt)",titlefont=c,width=NULL,xaxis=list(title = paste("Fleet:",input$DFleet), titlefont =f, tickfont =f,gridcolor = "#FFFFFF",rangemode = "nonzero"),
           yaxis=list(title = "Consumption (mt)", titlefont =f, tickfont =f,gridcolor = "#E5E7E9"),showlegend=F,legend = l, margin = list(l=70,b = 85,r=70), plot_bgcolor = "#FFFFFF",paper_bgcolor = "#FFFFFF",showlegend = T,width=NULL
  )
  
})

output$Tco2emission=renderPlotly({
  validate(                                                              
    need(try(input$DFleet),"Please Wait ...")
  )
  y=DATA 
  y = subset(y,Fleet==input$DFleet)
  y$tmp <-  as.Date(y$Corrected.Date,'%d-%m-%y')
  y$HS = as.numeric(as.character(y$FUEL.M.E.HS))+as.numeric(as.character(y$FUEL.AUX.HS))+as.numeric(as.character(y$FUEL.BOILER.HS))
  y$LS = as.numeric(as.character(y$FUEL.M.E.LS))+as.numeric(as.character(y$FUEL.AUX.LS))+as.numeric(as.character(y$FUEL.BOILER.LS))
  y$MDO =as.numeric(as.character(y$FUEL.M.E.MDO)) +as.numeric(as.character(y$FUEL.AUX.MDO))+as.numeric(as.character(y$FUEL.BOILER.MDO))
  y$MGO =as.numeric(as.character(y$FUEL.M.E.MGO))+as.numeric(as.character(y$FUEL.AUX.MGO))+as.numeric(as.character(y$FUEL.BOILER.MGO))
  y$MGOLS=as.numeric(as.character(y$FUEL.M.E.MGO.LS))+as.numeric(as.character(y$FUEL.BOILER.MGO.LS))+as.numeric(as.character(y$FUEL.AUX.MGO.LS))
  y$TCO2=((y$HS*3.114400) + (y$LS*3.151040) + ((y$MDO + y$MGO+y$MGOLS)*3.206000))
  y$s=format(y$tmp,"%b'%Y")
  y = subset(y,s==input$Dateselect)
  u=ddply(y, .(Vessel.Name,s),summarize,tot=sum(TCO2))
  p= plot_ly(x=as.character(u$Vessel.Name),y=u$tot,type="bar",marker = list(color="#84F99E"))
  p=p%>%layout(title = "CO2 Emission (t-co2)",titlefont=c,width=NULL,xaxis=list(title = paste("Fleet:",input$DFleet), titlefont =f, tickfont =f,gridcolor = "#FFFFFF",rangemode = "nonzero"),
           yaxis=list(title = "CO2 Emission (t-co2)", titlefont =f, tickfont =f,gridcolor = "#E5E7E9"),showlegend=F,legend = l, margin = list(l=70,b = 85,r=70), plot_bgcolor = "#FFFFFF",paper_bgcolor = "#FFFFFF",showlegend = T,width=NULL
  )
  
})