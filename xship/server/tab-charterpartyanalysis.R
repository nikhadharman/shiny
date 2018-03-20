

  
#Charter party analysis .................
output$CPAVessel<- renderUI({ 
y=cp
Class_List = suppressWarnings(unique(as.character(y$Vessel.Name), incomparables = FALSE))
selectInput("CPAvessel",label="Vessel Name",choices = Class_List , selected = "Strategic Alliance", multiple = F, selectize = TRUE, width = "75%", size = NULL)
})

output$CPAVoyageno <- renderUI({ 
  y=cp
  y=subset(y,Vessel.Name == input$CPAvessel)
  Class_List1 = sort(suppressWarnings(unique(as.character(y$VoyageNo), incomparables = FALSE)), decreasing = TRUE)
  selectInput("CPAvoyageno",label="Voyage No",choices = Class_List1 , selected = Class_List1 [1], multiple = F, selectize = TRUE, width = "75%", size = NULL)
})


CPADetail=reactive({
  y=cp
  validate(
    need(try(y),"Please Wait or Select the Date")
  )
  y = subset(y,Vessel.Name == input$CPAvessel )
  y = subset(y,VoyageNo == input$CPAvoyageno)
  y$Corrected.Date = sort(as.Date(y$Corrected.Date,"%d-%m-%y"),decresing = FALSE)
  Departure = as.character(head(y$DepPort,1))
  Arrival = as.character(tail(y$ArrivalPort,1))
  Departure.Date =  format(head(y$Corrected.Date,1),"%d - %b - %Y")
  Arrival.Date =  format(tail(y$Corrected.Date,1),"%d - %b - %Y")
  Ship.Type = as.character(head(y$Fleet,1))
  y = subset (y,Report.Type == "Noon at Sea") 
  m = data.frame(Departure,Arrival,Departure.Date,Arrival.Date,Ship.Type)
  m
})

output$CPAtable=DT::renderDataTable({
  m=data.frame(CPADetail())
  datatable(m,options = list(pageLength = F,searching = FALSE,paging = FALSE,audowidth=T),rownames = F,colnames = c("Departure","Arrival","Departure Date","Arrival Date","Ship Type") ,class = "display")%>%formatStyle(names(m),backgroundColor = "white",color="black")
})


#output$DEPART <- renderText({ 
#  y=CPADetail()
 #   paste0("DEPARTURE:   "  ,as.character(y$DEPART),sep = " " )
#})
#output$ARRIVAL <- renderText({ 
#  y=CPADetail()
#  paste0("ARRIVAL:  "   ,as.character(y$ARRIVAL),sep = " " )
#})
#output$DEPARTDATE <- renderText({ 
#  y=CPADetail()
#  paste0("DEPARTURE DATE:  "  ,as.character(y$DEPARTDATE))
#})
#output$ARRIVALDATE <- renderText({ 
#  y=CPADetail()
#  paste0("ARRIVAL DATE: "   ,as.character(y$ARRIVALDATE))
#})
#output$SHIPTYPE <- renderText({ 
# y=CPADetail()
#  paste0("SHIP TYPE:   "   ,as.character(y$SHIPTYPE))
#})



#WARRENTY SPEED AND CONSUMPTION AND GOOD WEATHER DEFINITION...............................................................
output$Warrentyspeed <- renderUI({
    numericInput("WSPEED",label="Warrented Speed About (Knots) ", value=13.5,width = "75%")
})
output$WarrentyFO <- renderUI({
  numericInput("WFOC",label="Warrented FO Consumption About (MT/day) ", value=22.00,width = "75%")
})
output$WarrentyDO <- renderUI({
  numericInput("WDOC",label="Warrented DO Consumption About (MT/day) ", value=0.10,width = "75%")
})
output$WarrentyWindforce <- renderUI({
  numericInput("Wforce",label="Good Weather Wind Force (Beufort Force)  ", value= 4,width = "75%")
})
output$Warrentyseastate <- renderUI({
  numericInput("WSS",label="Good Weather Douglass Sea State  ", value= 3 ,width = "75%")
})


#SPEED AND CONSUMPTION CALCUALTION..................................................................................

CPAgoodweather=reactive({
  
  y=cp
  y = subset(y,y$Vessel.Name == input$CPAvessel)
  y = subset(y,y$VoyageNo == input$CPAvoyageno)
  y = subset(y,y$SEA.STATE <= input$WSS )
  y = subset(y,y$WindForce <= input$Wforce )
  y = subset (y,y$Report.Type == "Noon at Sea")
  Distancesailed = sum(y$MILES.BY.GPS)
  TimeenRoute = sum(y$TOTAL.STEAMING.TIME)
  AverageSpeed = mean(y$SOG)
  Fuelcons = sum(y$FUEL.M.E.HS, y$FUEL.M.E.LS )
  Dieselcons = sum(y$FUEL.M.E.MDO)
  m = data.frame(Distancesailed,TimeenRoute,AverageSpeed,Fuelcons,Dieselcons)
  m[is.na(m)] <- 0
  m
}
)

CPAallweather=reactive(
  {
  
  y=cp
  y = subset(y,y$Vessel.Name == input$CPAvessel)
  y = subset(y,y$VoyageNo == input$CPAvoyageno)
  y = subset (y,y$Report.Type == "Noon at Sea")
  Distancesailedall = sum(y$MILES.BY.GPS)
  TimeenRouteall = sum(y$TOTAL.STEAMING.TIME)
  AverageSpeedall = mean(y$SOG)
  Fuelconsall = sum(y$FUEL.M.E.HS, y$FUEL.M.E.LS )
  Dieselconsall = sum(y$FUEL.M.E.MDO)
  m = data.frame(Distancesailedall,TimeenRouteall,AverageSpeedall,Fuelconsall,Dieselconsall)
  m[is.na(m)] <- 0
  m
}
)

CPAtime = reactive(
  {
  y=CPAallweather()
  z=CPAgoodweather()
  TTatgoodweathersp = y$Distancesailedall / z$AverageSpeed
  TTatwarrantedsp = y$Distancesailedall / (input$WSPEED - 0.5)
  Timelost = TTatgoodweathersp - TTatwarrantedsp
  m = data.frame(TTatgoodweathersp,TTatwarrantedsp,Timelost)
  m[is.na(m)] <- 0
  m
}
)

CPAcons = reactive(
  {
  y=CPAallweather()
  z=CPAgoodweather()
  
  FO_c = (y$Distancesailedall /  z$AverageSpeed) * (z$Fuelcons / z$TimeenRoute)
  FO_d = (y$Distancesailedall /  (input$WSPEED - 0.5)) * ((1.05 * input$WFOC) / 24)
  FO_e = (y$Distancesailedall /  (input$WSPEED - 0.5)) * ((0.95 * input$WFOC) / 24)
  DO_c = (y$Distancesailedall /  z$AverageSpeed) * (z$Dieselcons / z$TimeenRoute)
  DO_d = (y$Distancesailedall /  (input$WSPEED - 0.5)) * ((1.05 * input$WDOC) / 24)
  DO_e = (y$Distancesailedall /  (input$WSPEED - 0.5)) * ((0.95 * input$WDOC) / 24)
  
  FOovercons = FO_c - FO_d
  DOsav = DO_e - DO_c
  m = data.frame(FO_c,FO_d,FO_e,DO_c,DO_d,DO_e,FOovercons,DOsav)
  m[is.na(m)] <- 0
  m
 
})

output$Timeloss <- renderValueBox({
  x=CPAtime() 
  validate(
    need(try(x),"Press Wait or NO DATA AVAILABLE..........")
  )
  if(x$Timelost == Inf ){
    valueBox("No Data Available",strong(" "),
             color = "teal",width=14
    )
  }
  else if(x$Timelost > 0){
    valueBox(
      round(x$Timelost,2), strong("Time Lost Hrs"),icon = icon("clock-o"),
      color = "red",width=14
    )
  }else if(x$Timelost < 0){
    valueBox(
      round(-1*x$Timelost,2), strong("Time Gained in Hrs"),icon = icon("clock-o"),
      color = "green",width=14
    )
  } 
})

output$Fueloil <- renderValueBox(
  {
    x=CPAcons()
    if(x$FOovercons == 0 ){
      valueBox("No Data Available",strong(" "),
               color = "teal",width=14
      )
    }
    else if(x$FOovercons > 0){
      valueBox(
        round(x$FOovercons,2), strong("Fuel oil MT Over consumption"),icon = icon("fire"),
        color = "red",width=14
      )
    }else {
      valueBox(
        round(-1*x$FOovercons,2), strong("Fuel oil MT Under consumption"),icon = icon("fire"),
        color = "green",width=14
      )
      
    }
  })

output$Dieseloil <- renderValueBox(
  {
    x=CPAcons()
    if(x$DOsav == 0 ){
      valueBox("No Data Available",strong(" "),
               color = "teal",width=14
      )
    }
    else if(x$DOsav > 0){
      valueBox(
        round(x$DOsav,2), strong("Diesel oil MT Saving"),icon = icon("fire"),
        color = "green",width=14
      )
    }else {
      valueBox(
        round(-1*x$DOsav,2), strong("Diesel oil MT Over consumption"),icon = icon("fire"),
        color = "red",width=14
      )
      
    }
  })

CPAdfspeed = reactive({
  y=CPAgoodweather()
  z=CPAallweather()
  m=matrix(0,nrow=5,ncol=2,byrow = TRUE)
  colnames(m) = c("Good Weather","Entire Voyage")
  rownames(m)=c("Distance Sailed ","Time en Route ","Average Speed ","Fuel Consumption ","Diesel Consumption ")
  m[1,1] = paste0(y$Distancesailed, " Miles")
  m[2,1] = paste0(y$TimeenRoute, " Hrs")
  m[3,1] = paste0(round(y$AverageSpeed,2), " knots")
  m[4,1] = paste0(round(y$Fuelcons,2), " MT")
  m[5,1] = paste0(round(y$Dieselcons,2), " MT")
  m[1,2] = paste0(z$Distancesailedall, " Miles")
  m[2,2] = paste0(z$TimeenRouteall, " Hrs")
  m[3,2] = paste0(round(z$AverageSpeedall,2), " knots")
  m[4,2] = paste0(round(z$Fuelconsall,2), " MT")
  m[5,2] = paste0(round(z$Dieselconsall,2), " MT")
  m
  
}
)

CPAdftime = reactive({
  y=CPAtime()
  m=matrix(0,nrow=2,ncol=1,byrow = TRUE)
  colnames(m) = c("Time in Hrs")
  rownames(m) = c("Total time at good weather speed","Total time at warranted speed")
  m[1] = paste0(round(y$TTatgoodweathersp,2), " Hrs")
  m[2] = paste0(round(y$TTatwarrantedsp,2), " Hrs")
  m
}
)

CPAdfcons = reactive({
  y = CPAcons()
  m=matrix(0,nrow=6,ncol=1,byrow = TRUE)
  colnames(m) = c("Consumption in MT")
  rownames(m) = c("FO-Entire voyage Consumption using vessel Good weather Consumption","FO-Maximum Warranted Consumption for over-consumption","FO-Minimum Warranted Consumption for Fuel-Saving",
                  "DO-Entire voyage Consumption using vessel Good weather Consumption","DO-Maximum Warranted Consumption for over-consumption","DO-Minimum Warranted Consumption for Fuel-Saving")
  m[1] = paste0(round(y$FO_c,2), " MT")
  m[2] = paste0(round(y$FO_d,2), " MT")
  m[3] = paste0(round(y$FO_e,2), " MT")
  m[4] = paste0(round(y$DO_c,2), " MT")
  m[5] = paste0(round(y$DO_d,2), " MT")
  m[6] = paste0(round(y$DO_e,2), " MT")
  m
}
)

output$CPAspeeddt=DT::renderDataTable({
  m=data.frame(CPAdfspeed())
      datatable(m,options = list(pageLength = 15,searching = FALSE,paging = FALSE,audowidth=T),rownames = T,colnames = colnames(m) ,class = "display")%>%formatStyle(names(m),backgroundColor = "#ECF0F1",color="blue")
})


output$CPAtimedt=DT::renderDataTable({
  m=data.frame(CPAdftime())
  datatable(m,options = list(pageLength = 15,searching = FALSE,paging = FALSE,audowidth=T),rownames = T,colnames = colnames(m) ,class = "display")%>%formatStyle(names(m),backgroundColor = "#ECF0F1",color="blue")
})

output$CPAconsdt=DT::renderDataTable({
  m=data.frame(CPAdfcons())
  datatable(m,options = list(pageLength = 15,searching = FALSE,paging = FALSE,audowidth=T),rownames = T,colnames = colnames(m) ,class = "display")%>%formatStyle(names(m),backgroundColor = "#ECF0F1",color="blue")
})




