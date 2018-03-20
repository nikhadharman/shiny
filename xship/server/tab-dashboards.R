# vessel info................
output$vesselselect <- renderUI({ 
  y=DATAT
  Class_List = suppressWarnings(unique(as.character(y$Vessel.Name), incomparables = FALSE))
  selectInput("vesselselect",label="Vessel Name:",choices = Class_List , selected = "A1", multiple = F, selectize = TRUE, width = "75%", size = NULL)
})
output$voynodb <- renderUI({
  y=DATAT
  y=subset(y,Vessel.Name == input$vesselselect)
  Class_List = sort(unique((as.character(y$Voyage)), incomparables = FALSE),decreasing = TRUE)
  selectInput("voyageno1",label="Voyage No:",choices = Class_List , selected = Class_List[1] ,multiple = F, selectize = TRUE, width = "75%", size = NULL)
})



output$dbdate <- renderUI({ 
  y=DATAT
  y=subset(y,Vessel.Name == input$vesselselect)
  y=subset(y,y$Voyage == input$voyageno1)
  Class_List1 = suppressWarnings(unique(as.Date(y$Corrected.Date,"%d-%m-%y"), incomparables = FALSE))
  latest = sort(Class_List1,decreasing = TRUE)
  lasst = sort(Class_List1,decreasing = FALSE)
  dateInput("dbdate", label = "Date Input" , value = latest[1] , min = lasst[1] , max = latest[1],
            format = "dd-M-yyyy", startview = "month", weekstart = 0,div(style = "height:50px;"),width = "75%")
})

output$dbdate4 <- renderUI({ 
  y=DATAT
  y=subset(y,Vessel.Name == input$vesselselect)
  y=subset(y,y$Voyage == input$voyageno1)
  Class_List1 = suppressWarnings(unique(as.Date(y$Corrected.Date,"%d-%m-%y"), incomparables = FALSE))
  latest = sort(Class_List1,decreasing = TRUE)
  lasst = sort(Class_List1,decreasing = FALSE)
  dateInput("dbdate1", label = "Date Input" , value = input$dbdate , min = lasst[1] , max = latest[1],
            format = "dd-M-yyyy", startview = "month", weekstart = 0,div(style = "height:50px;"),width = "75%")
  
})

cpdatabase=reactive({
  validate(
    need(try(input$vesselselect),"Please Wait or Select the Vessel")
  )
  
  y=DATAT
  y=subset(y,Vessel.Name == input$vesselselect)
  y=subset(y,y$Voyage == input$voyageno1)
  y=subset(y,as.Date(y$Corrected.Date,"%d-%m-%y") == input$dbdate[1])
  y
  
})

cpdatabase1=reactive({
  y=DATAT
  validate(
    need(try(input$dbdate),"Please Wait or Select the Date")
  )
  y=subset(y,Vessel.Name == input$vesselselect)
  y=subset(y,y$Voyage == input$voyageno1)
  y
  
})

xcpdatabase=reactive({
  # validate(
  #   need(try(input$vesselselect1),"Please Wait or Select the Vessel")
  # )
  
  y=DATA
  y=subset(y,Vessel.Name == input$vesselselect)
  y=subset(y,y$Voyage == input$voyageno1)
  y=subset(y,as.Date(y$Corrected.Date,"%d-%m-%y") == input$dbdate1[1])
  y
  
})

cpdata=reactive({
  y=cpdatabase()
  validate(
    need(try(y),"Please Wait or Select the Date")
  )
  cps = as.numeric(as.character(y$CharterPartySpeed[1]))
  As = as.numeric(y$SOG[1])
  cpc =  as.numeric(as.character(y$CharterPartyCons[1]))
  Ac = as.numeric(y$FO.per.24Hrs[1])
  m= matrix(NA,2,3 )
  colnames(m)=c(" ","ACTUAL","CHARTER PARTY")
  m[1,]=c("Speed",As,cps)
  m[2,]=c("FO Consumption",Ac,cpc)
  m = as.data.frame(m)
})




output$Ac <- renderText({ 
  y=cpdatabase()
  
  paste0("Reported FOC:   ",round(as.numeric(sum(as.numeric(y$FUEL.M.E.HS),as.numeric(y$FUEL.M.E.LS),as.numeric(y$FUEL.AUX.HS),
                                                 as.numeric(y$FUEL.AUX.LS),as.numeric(y$FUEL.BOILER.HS),as.numeric(y$FUEL.BOILER.LS),as.numeric(y$FUEL.MGO.HS))),2)," MT")
})



output$As <- renderText({ 
  y=cpdatabase()
  
  paste0("Reported  Speed:   ",round(as.numeric(y$SOG[1]),2)," knots")
  
})

output$Apow <- renderText({ 
  y=cpdatabase()
  
  paste0("Power:   ",round(as.numeric(y$POWER.kW[1]),0)," kW")
})

output$Adraft <- renderText({ 
  y=cpdatabase()
  
  paste0("Draft FWD:   ",round(as.numeric(as.character(y$DRAFT.FWD[1])),1)," m")
})

output$Bdraft <- renderText({ 
  y=cpdatabase()
  
  paste0("Draft AFT:   ",round(as.numeric(as.character(y$DRAFT.AFT[1])),1)," m")
})





output$Ass <- renderText({ 
  y=cpdatabase()
  
  paste0("Sea State:   ",as.numeric(as.character(y$SEA.STATE[1])) )
})

output$slp <- renderText({ 
  y=cpdatabase()
  
  paste0("Slip:   ",as.numeric(as.character(y$SLIP[1]))," %" )
})

output$rpm <- renderText({ 
  y=cpdatabase()
  
  paste0("RPM:   ",as.numeric(as.character(y$RPM[1])) )
})



output$cpc1 <- renderText({ 
  y=xcpdatabase()
  
  paste0("CP FOC:   ",round(as.numeric(as.character(y$CharterPartyCons[1])),2)," MT")
})

output$Ac1 <- renderText({ 
  y=xcpdatabase()
  
  paste0("Reported FOC:   ",round(as.numeric(sum(as.numeric(y$FUEL.M.E.HS),as.numeric(y$FUEL.M.E.LS),as.numeric(y$FUEL.AUX.HS),
                                                 as.numeric(y$FUEL.AUX.LS),as.numeric(y$FUEL.BOILER.HS),as.numeric(y$FUEL.BOILER.LS),as.numeric(y$FUEL.MGO.HS))),2)," MT")
})
output$cps1 <- renderText({ 
  y=xcpdatabase()
  
  paste0("CP Speed:   ",round(as.numeric(as.character(y$CharterPartySpeed[1])),2)," knots")
})

output$As1 <- renderText({ 
  y=xcpdatabase()
  
  paste0("Reported  Speed:   ",round(as.numeric(y$SOG[1]),2)," knots")
  
})

output$Apow1 <- renderText({ 
  y=xcpdatabase()
  
  paste0("Power:   ",round(as.numeric(y$POWER.kW[1]),0)," kW")
})

output$Adraft1 <- renderText({ 
  y=xcpdatabase()
  
  paste0("Draft FWD:   ",round(as.numeric(y$DRAFT.FWD[1]),1)," m")
})

output$Bdraft1 <- renderText({ 
  y=xcpdatabase()
  
  paste0("Draft AFT:   ",round(as.numeric(y$DRAFT.AFT[1]),1)," m")
})

output$Aws1 <- renderText({ 
  y=xcpdatabase()
  
  paste0("Wind Direction:   ",round(as.numeric(as.character(y$WindDirection[1])),1)," Deg")
})

output$Awf1 <- renderText({ 
  y=xcpdatabase()
  
  paste0("Wind Force:   ",round(as.numeric(as.character(y$WindForce[1])),1)," BF")
})

output$Ass1 <- renderText({ 
  y=xcpdatabase()
  
  paste0("Sea State:   ",as.numeric(as.character(y$SEA.STATE[1])) )
})

output$slp1 <- renderText({ 
  y=xcpdatabase()
  
  paste0("Slip:   ",as.numeric(as.character(y$SLIP[1]))," %" )
})

output$rpm1 <- renderText({ 
  y=xcpdatabase()
  
  paste0("RPM:   ",as.numeric(as.character(y$RPM[1])) )
})


voyagedetails = reactive({
  y = cpdatabase()
  m=matrix(NA,nrow=5,ncol=1,byrow = TRUE)
  colnames(m) = c("Reported Data")
  rownames(m) = c("Status","Port Name","Next Port","Total Cargo","Displacement")
  m[1] = as.character(y$Status[1])
  m[2] = as.character(y$DepPort[1])
  m[3] = as.character(y$ArrivalPort[1])
  m[4] = as.character(y$CARGO.TOTAL[1])
  m[5] = as.character(y$Displacement[1])
  
  m = data.frame(m)
} 
)

output$voyagetable=DT::renderDataTable({
  m=voyagedetails()
  validate(
    need(try(m),"Please Wait or Select the Date")
  )
  
  datatable(m,options = list(pageLength = 15,searching = FALSE,paging = FALSE,audowidth=T),rownames = T, colnames=c("TITLE"=1) ,class = "display")%>%formatStyle(names(m),backgroundColor = "white",color="#000")%>%formatStyle("TITLE",  color = '#FFF', backgroundColor = '#515A5A', fontWeight = 'bold')
})


output$dbgsel <- renderUI({
  
  selectInput("DBgsel",label="Select Y axis:",choices = c("Speed","ME FO","Boiler FO","AUX FO","HSFO Cons","LSMGO Cons","Slip","Power","RPM","Wind Force","Sea State") , selected = "Speed", multiple = F, selectize = TRUE, width = "75%", size = NULL)
  
  
})
output$dbgsel1 <- renderUI({
  
  selectInput("DBgsel1",label="Select Y axis:",choices = c("Speed","ME FO","Boiler FO","AUX FO","HSFO Cons","LSMGO Cons","Slip","Power","RPM","Wind Force","Sea State") , selected = input$DBgsel, multiple = F, selectize = TRUE, width = "75%", size = NULL)
  
  
})

output$dashbplot =renderPlotly({
  y = cpdatabase() #with date and vessel......................................
  y1 = cpdatabase1() #with vessel name only...................................
  vn= as.numeric(y$Voyage)
  pc= as.character(y$Depart_Arrival)
  #cp= as.numeric(as.character(y$CharterPartyCons))
  #cp1= as.numeric(as.character(y$CharterPartySpeed))
  y1 = subset(y1, as.numeric(y1$Voyage) == vn )
  y1 = subset(y1, as.character(y1$Depart_Arrival) == pc )
  VD = VESSELDETAILS
  VD = subset(VD,Vessel == input$vesselselect)
  xx = input$DBgsel
  
  
  
  if(xx=="Speed"){
    p=plot_ly(y1,x=as.Date(y1$Corrected.Date,"%d-%m-%y"),y=y1$SOG,name="Speed",type = "scatter",mode="lines+markers",line=list(color="#8B0000"))
    #add_trace(x=c(head(as.Date(y1$Corrected.Date,"%d-%m-%y"),1),tail(as.Date(y1$Corrected.Date,"%d-%m-%y"),1)), y= cp1[1],  type="scatter",mode="lines+markers",line = list(color="	#8B0000",dash='dash'),name = "CP Speed",showlegend=F)
    
  }else if(xx=="ME FO"){
    p=plot_ly(y1,x=as.Date(y1$Corrected.Date,"%d-%m-%y"),y=as.numeric(as.numeric(y1$FUEL.M.E.HS)+as.numeric(y1$FUEL.M.E.LS)+
                                                                        as.numeric(y1$FUEL.M.E.MGO)+as.numeric(y1$FUEL.M.E.MDO)),name="ME FO",type = "scatter",mode="lines+markers",line=list(color="#8B0000"))
    #add_trace(x=c(head(as.Date(y1$Corrected.Date,"%d-%m-%y"),1),tail(as.Date(y1$Corrected.Date,"%d-%m-%y"),1)), y= cp[1], type="scatter",mode="lines+markers",line = list(color="#8B0000",dash='dash'),name = "CP FOC",showlegend=F)
  }else if(xx=="Boiler FO"){
    p=plot_ly(y1,x=as.Date(y1$Corrected.Date,"%d-%m-%y"),y=as.numeric(as.numeric(y1$FUEL.BOILER.HS)+as.numeric(y1$FUEL.BOILER.LS)+
                                                                        as.numeric(y1$FUEL.BOILER.MGO)+as.numeric(y1$FUEL.BOILER.MDO)),name="BOILER FO",type = "scatter",mode="lines+markers",line=list(color="#8B0000"))
    #add_trace(x=c(head(as.Date(y1$Corrected.Date,"%d-%m-%y"),1),tail(as.Date(y1$Corrected.Date,"%d-%m-%y"),1)), y= cp[1], type="scatter",mode="lines+markers",line = list(color="#8B0000",dash='dash'),name = "CP FOC",showlegend=F)
  }else if(xx=="AUX FO"){
    p=plot_ly(y1,x=as.Date(y1$Corrected.Date,"%d-%m-%y"),y=as.numeric(as.numeric(y1$FUEL.AUX.HS)+as.numeric(y1$FUEL.AUX.LS)+
                                                                        as.numeric(y1$FUEL.AUX.MGO)+as.numeric(y1$FUEL.AUX.MDO)),name="AUX FO",type = "scatter",mode="lines+markers",line=list(color="#8B0000"))
    #add_trace(x=c(head(as.Date(y1$Corrected.Date,"%d-%m-%y"),1),tail(as.Date(y1$Corrected.Date,"%d-%m-%y"),1)), y= cp[1], type="scatter",mode="lines+markers",line = list(color="#8B0000",dash='dash'),name = "CP FOC",showlegend=F)
  }else if(xx=="HSFO Cons"){
    p=plot_ly(y1,x=as.Date(y1$Corrected.Date,"%d-%m-%y"),y=as.numeric(as.numeric(y1$FUEL.M.E.HS)+as.numeric(y1$FUEL.AUX.HS)+
                                                                        as.numeric(y1$FUEL.BOILER.HS)),name="HS FO",type = "scatter",mode="lines+markers",line=list(color="#8B0000"))
    #add_trace(x=c(head(as.Date(y1$Corrected.Date,"%d-%m-%y"),1),tail(as.Date(y1$Corrected.Date,"%d-%m-%y"),1)), y= cp[1], type="scatter",mode="lines+markers",line = list(color="#8B0000",dash='dash'),name = "CP FOC",showlegend=F)
  }else if(xx=="LSMGO Cons"){
    p=plot_ly(y1,x=as.Date(y1$Corrected.Date,"%d-%m-%y"),y=as.numeric(y1$FUEL.M.E.MGO),name="LSMGO",type = "scatter",mode="lines+markers",line=list(color="#8B0000"))
    #add_trace(x=c(head(as.Date(y1$Corrected.Date,"%d-%m-%y"),1),tail(as.Date(y1$Corrected.Date,"%d-%m-%y"),1)), y= cp[1], type="scatter",mode="lines+markers",line = list(color="#8B0000",dash='dash'),name = "CP FOC",showlegend=F)
  }else if(xx=="Slip"){
    p=plot_ly(y1,x=as.Date(y1$Corrected.Date,"%d-%m-%y") ,y=as.numeric(as.character(y1$SLIP)),name="Slip",type = "scatter",mode="lines+markers",line=list(color="#8B0000"))
    #add_trace(x=c(head(as.Date(y1$Corrected.Date,"%d-%m-%y"),1),tail(as.Date(y1$Corrected.Date,"%d-%m-%y"),1)), y= VD$Slip, type="scatter",mode="lines+markers",line = list(color="#8B0000",dash='dash'),name = "Benchmark Slip",showlegend=F)
  }else if(xx=="Power"){
    p=plot_ly(y1,x=as.Date(y1$Corrected.Date,"%d-%m-%y") ,y=as.numeric(as.character(y1$POWER.kW)),name="Power",type = "scatter",mode="lines+markers",line=list(color="#8B0000"))
    #add_trace(x=c(head(as.Date(y1$Corrected.Date,"%d-%m-%y"),1),tail(as.Date(y1$Corrected.Date,"%d-%m-%y"),1)), y= VD$NCR, type="scatter",mode="lines+markers",line = list(color="#8B0000",dash='dash'),name = "Benchmark Power",showlegend=F)
  }else if(xx=="RPM"){
    p=plot_ly(y1,x=as.Date(y1$Corrected.Date,"%d-%m-%y") ,y=as.numeric(as.character(y1$RPM)),name="RPM",type = "scatter",mode="lines+markers",line=list(color="#8B0000"))
    #add_trace(x=c(head(as.Date(y1$Corrected.Date,"%d-%m-%y"),1),tail(as.Date(y1$Corrected.Date,"%d-%m-%y"),1)), y= VD$RPM, type="scatter",mode="lines+markers",line = list(color="#8B0000",dash='dash'),name = "Benchmark RPM",showlegend=F)
  }else if(xx=="Wind Force"){
    p=plot_ly(y1,x=as.Date(y1$Corrected.Date,"%d-%m-%y") ,y=as.numeric(as.character(y1$WindForce)),name="Wind Force",type = "scatter",mode="lines+markers",line=list(color="#8B0000"))
    #add_trace(x=c(head(as.Date(y1$Corrected.Date,"%d-%m-%y"),1),tail(as.Date(y1$Corrected.Date,"%d-%m-%y"),1)), y= VD$WS, type="scatter",mode="lines+markers",line = list(color="#8B0000",dash='dash'),name = "Benchmark BF",showlegend=F)
  }else if(xx=="Sea State"){
    p=plot_ly(y1,x=as.Date(y1$Corrected.Date,"%d-%m-%y") ,y=as.numeric(as.character(y1$SEA.STATE)),name="Sea State",type = "scatter",mode="lines+markers",line=list(color="#8B0000"))
    # add_trace(x=c(head(as.Date(y1$Corrected.Date,"%d-%m-%y"),1),tail(as.Date(y1$Corrected.Date,"%d-%m-%y"),1)), y= VD$SS, type="scatter",mode="lines+markers",line = list(color="#8B0000",dash='dash'),name = "Benchmark Sea State",showlegend=F)
  }
  p=p%>%layout(titlefont=list(family = "'Lato', sans-serif",size = 15,color = "black"),
               xaxis=list(title="",gridcolor = "white",color = "black",tickvals = as.Date(y1$Corrected.Date,"%d-%m-%y"),tickformat = "%d'%b'%y",
                          showline=TRUE),
               yaxis=list(title =xx,gridcolor = "white",tickfont=list(color = "black"),showline = TRUE,
                          linecolor = "black"),
               legend = list(
                 x = 1.1 ,y = 1,
                 font = s,
                 bgcolor = "white",
                 bordercolor = "white",
                 borderwidth = 2),
               plot_bgcolor = "white",paper_bgcolor = "white",showlegend = TRUE,margin = list(b = 50)
               
               
  )
  p
  
  
  
})
output$dashbplot1 =renderPlotly({
  y = cpdatabase() #with date and vessel......................................
  y1 = cpdatabase1() #with vessel name only...................................
  vn= as.numeric(y$VoyageNo)
  pc= as.character(y$Depart_Arrival)
  cp= as.numeric(as.character(y$CharterPartyCons))
  cp1= as.numeric(as.character(y$CharterPartySpeed))
  y1 = subset(y1, as.numeric(y1$VoyageNo) == vn )
  y1 = subset(y1, as.character(y1$Depart_Arrival) == pc )
  VD = VESSELDETAILS
  VD = subset(VD,Vessel == input$vesselselect)
  xx = input$DBgsel1
  color = c("#8B0000","blue","purple","green")
  
  p=plot_ly(y1)
  for(i in 1 : length(xx)){
    
    if(xx[i]=="Speed"){
      p=p%>%add_trace(y1,x=as.Date(y1$Corrected.Date,"%d-%m-%y"),y=y1$SOG,name="Speed",type = "scatter",mode="lines+markers",line=list(color=color[i]),yaxis = paste0("y", i))
       # add_trace(x=c(head(as.Date(y1$Corrected.Date,"%d-%m-%y"),1),tail(as.Date(y1$Corrected.Date,"%d-%m-%y"),1)), y= cp1[1],  type="scatter",mode="lines+markers",line = list(color=color[i],dash='dash'),name = "CP Speed",showlegend=F,yaxis = paste0("y", i))
      
    }else if(xx[i]=="ME FO"){
      p=p%>%add_trace(y1,x=as.Date(y1$Corrected.Date,"%d-%m-%y"),y=as.numeric(as.numeric(y1$FUEL.M.E.HS)+as.numeric(y1$FUEL.M.E.LS)+
                                                                                as.numeric(y1$FUEL.M.E.MGO.HS)+as.numeric(y1$FUEL.M.E.MDO)),name="ME FO",type = "scatter",mode="lines+markers",line=list(color=color[i]),yaxis = paste0("y", i))
      #add_trace(x=c(head(as.Date(y1$Corrected.Date,"%d-%m-%y"),1),tail(as.Date(y1$Corrected.Date,"%d-%m-%y"),1)), y= cp[1], type="scatter",mode="lines+markers",line = list(color="#8B0000",dash='dash'),name = "CP FOC",showlegend=F)
    }else if(xx[i]=="Boiler FO"){
      p=p%>%add_trace(y1,x=as.Date(y1$Corrected.Date,"%d-%m-%y"),y=as.numeric(as.numeric(y1$FUEL.BOILER.HS)+as.numeric(y1$FUEL.BOILER.LS)+
                                                                                as.numeric(y1$FUEL.BOILER.MGO.HS)+as.numeric(y1$FUEL.BOILER.MDO)),name="BOILER FO",type = "scatter",mode="lines+markers",line=list(color=color[i]),yaxis = paste0("y", i))
      #add_trace(x=c(head(as.Date(y1$Corrected.Date,"%d-%m-%y"),1),tail(as.Date(y1$Corrected.Date,"%d-%m-%y"),1)), y= cp[1], type="scatter",mode="lines+markers",line = list(color="#8B0000",dash='dash'),name = "CP FOC",showlegend=F)
    }else if(xx[i]=="AUX FO"){
      p=p%>%add_trace(y1,x=as.Date(y1$Corrected.Date,"%d-%m-%y"),y=as.numeric(as.numeric(y1$FUEL.AUX.HS)+as.numeric(y1$FUEL.AUX.LS)+
                                                                                as.numeric(y1$FUEL.AUX.MGO.HS)+as.numeric(y1$FUEL.AUX.MDO)),name="AUX FO",type = "scatter",mode="lines+markers",line=list(color=color[i]),yaxis = paste0("y", i))
      #add_trace(x=c(head(as.Date(y1$Corrected.Date,"%d-%m-%y"),1),tail(as.Date(y1$Corrected.Date,"%d-%m-%y"),1)), y= cp[1], type="scatter",mode="lines+markers",line = list(color="#8B0000",dash='dash'),name = "CP FOC",showlegend=F)
    }else if(xx[i]=="HSFO Cons"){
      p=p%>%add_trace(y1,x=as.Date(y1$Corrected.Date,"%d-%m-%y"),y=as.numeric(y1$FUEL.HS),name="HS FO",type = "scatter",mode="lines+markers",line=list(color=color[i]),yaxis = paste0("y", i))
      #add_trace(x=c(head(as.Date(y1$Corrected.Date,"%d-%m-%y"),1),tail(as.Date(y1$Corrected.Date,"%d-%m-%y"),1)), y= cp[1], type="scatter",mode="lines+markers",line = list(color="#8B0000",dash='dash'),name = "CP FOC",showlegend=F)
    }else if(xx[i]=="LSMGO Cons"){
      p=p%>%add_trace(y1,x=as.Date(y1$Corrected.Date,"%d-%m-%y"),y=as.numeric(y1$FUEL.MGO.HS),name="LSMGO",type = "scatter",mode="lines+markers",line=list(color=color[i]),yaxis = paste0("y", i))
      #add_trace(x=c(head(as.Date(y1$Corrected.Date,"%d-%m-%y"),1),tail(as.Date(y1$Corrected.Date,"%d-%m-%y"),1)), y= cp[1], type="scatter",mode="lines+markers",line = list(color="#8B0000",dash='dash'),name = "CP FOC",showlegend=F)
    }else if(xx[i]=="Slip"){
      p=p%>%add_trace(y1,x=as.Date(y1$Corrected.Date,"%d-%m-%y") ,y=as.numeric(as.character(y1$SLIP)),name="Slip",type = "scatter",mode="lines+markers",line=list(color=color[i]),yaxis = paste0("y", i))
        #add_trace(x=c(head(as.Date(y1$Corrected.Date,"%d-%m-%y"),1),tail(as.Date(y1$Corrected.Date,"%d-%m-%y"),1)), y= VD$Slip, type="scatter",mode="lines+markers",line = list(color=color[i],dash='dash'),name = "Benchmark Slip",showlegend=F,yaxis = paste0("y", i))
    }else if(xx[i]=="Power"){
      p=p%>%add_trace(y1,x=as.Date(y1$Corrected.Date,"%d-%m-%y") ,y=as.numeric(as.character(y1$POWER.kW)),name="Power",type = "scatter",mode="lines+markers",line=list(color=color[i]),yaxis = paste0("y", i))
        #add_trace(x=c(head(as.Date(y1$Corrected.Date,"%d-%m-%y"),1),tail(as.Date(y1$Corrected.Date,"%d-%m-%y"),1)), y= VD$NCR, type="scatter",mode="lines+markers",line = list(color=color[i],dash='dash'),name = "Benchmark Power",showlegend=F,yaxis = paste0("y", i))
    }else if(xx[i]=="RPM"){
      p=p%>%add_trace(y1,x=as.Date(y1$Corrected.Date,"%d-%m-%y") ,y=as.numeric(as.character(y1$RPM)),name="RPM",type = "scatter",mode="lines+markers",line=list(color=color[i]),yaxis = paste0("y", i))
        #add_trace(x=c(head(as.Date(y1$Corrected.Date,"%d-%m-%y"),1),tail(as.Date(y1$Corrected.Date,"%d-%m-%y"),1)), y= VD$RPM, type="scatter",mode="lines+markers",line = list(color=color[i],dash='dash'),name = "Benchmark RPM",showlegend=F,yaxis = paste0("y", i))
    }else if(xx[i]=="Wind Force"){
      p=p%>%add_trace(y1,x=as.Date(y1$Corrected.Date,"%d-%m-%y") ,y=as.numeric(as.character(y1$WindForce)),name="Wind Force",type = "scatter",mode="lines+markers",line=list(color=color[i]),yaxis = paste0("y", i))
        #add_trace(x=c(head(as.Date(y1$Corrected.Date,"%d-%m-%y"),1),tail(as.Date(y1$Corrected.Date,"%d-%m-%y"),1)), y= VD$WS, type="scatter",mode="lines+markers",line = list(color=color[i],dash='dash'),name = "Benchmark BF",showlegend=F,yaxis = paste0("y", i))
    }else if(xx[i]=="Sea State"){
      p=p%>%add_trace(y1,x=as.Date(y1$Corrected.Date,"%d-%m-%y") ,y=as.numeric(as.character(y1$SEA.STATE)),name="Sea State",type = "scatter",mode="lines+markers",line=list(color=color[i]),yaxis = paste0("y", i))
       # add_trace(x=c(head(as.Date(y1$Corrected.Date,"%d-%m-%y"),1),tail(as.Date(y1$Corrected.Date,"%d-%m-%y"),1)), y= VD$SS, type="scatter",mode="lines+markers",line = list(color=color[i],dash='dash'),name = "Benchmark Sea State",showlegend=F,yaxis = paste0("y", i))
    }
    
    p=p%>%layout(titlefont=list(family = "'Lato', sans-serif",size = 15,color = "black"),
                 xaxis=list(title="",gridcolor = "white",color = "black",tickvals = as.Date(y1$Corrected.Date,"%d-%m-%y"),tickformat = "%d'%b'%y",
                            showline=TRUE))
    
    if(i == 1) {
      
      p=p%>%layout(yaxis=list(title =xx[i],gridcolor = "white",tickfont=list(color=color[i]),color=color[i],zeroline = FALSE,showline = T
      ))
    }
    
    if(i == 2) {
      
      p=p%>%layout(yaxis2 = list(title =xx[i],zeroline = FALSE,showline = T
                                 
                                 ,overlaying = "y"
                                 , anchor = "free"
                                 ,color=color[i], position=0.03
      ))
    }  else if( i == 3) {
      
      p=p%>%layout( yaxis3 = list(title =xx[i],zeroline = FALSE,showline = T,
                                  side = "right", overlaying = "y"
                                  ,color=color[i]
      ))
      
    } else if( i == 4) {
      
      
      p=p%>%layout( yaxis4 = list(title =xx[i],zeroline = FALSE,showline = T,
                                  side = "right", overlaying = "y",position=0.98
                                  ,anchor = "free"
                                  ,color=color[i]
      ))
    }
    p=p%>%layout( legend = list(
      x = 1.1 ,y = 1,
      font = s,
      bgcolor = "white",
      bordercolor = "white",
      borderwidth = 2),
      plot_bgcolor = "white",paper_bgcolor = "white",showlegend = TRUE, margin = list(
        pad = 30, b = 90, l = 150, r = 150
      )
    )
    
    
    
  }
  p
  
  
  
})
points <- reactive({
  y = cpdatabase() #with date and vessel......................................
  y1 = cpdatabase1() #with vessel name only...................................
  vn= as.numeric(y$Voyage)
  pc= as.character(y$Depart_Arrival)
  cp= as.numeric(as.character(y$CharterPartyCons))
  cp1= as.numeric(as.character(y$CharterPartySpeed))
  y1 = subset(y1, as.numeric(y1$Voyage) == vn )
  y1 = subset(y1, as.character(y1$Depart_Arrival) == pc )
  ylat = as.numeric(as.character(y1$lat))
  ylong = as.numeric(as.character(y1$long))
  x = length(ylat)
  point = cbind (ylat,ylong)
  point
  df1 <- structure(list(lat = ylat, lng = ylong), .Names = c("lat", "lng"), row.names = c(NA, paste("-",x,"L")), class = "data.frame")
  df1
  
  
})

output$mymap <- renderLeaflet({
  point = points()
  point1 = cpdatabase()
  latt = as.numeric(as.character(point1$lat))
  longg = as.numeric(as.character(point1$long))
  
  map <- leaflet() %>% addProviderTiles(providers$MtbMap) %>%
    addProviderTiles(providers$Stamen.TonerLines,
                     options = providerTileOptions(opacity = 0.35)) %>%
    addProviderTiles(providers$Stamen.TonerLabels)
  map <- map %>% 
    addCircleMarkers(data=point, radius = 8, color = 'red', fill = TRUE,  labelOptions=c(noHide=TRUE)) %>%
    addMarkers(lng = longg, lat = latt)%>%
    addPolylines(data=point, lng = ~lng, lat = ~lat)
  map
})

output$mymap1 <- renderLeaflet({
  point = points()
  point1 = cpdatabase()
  latt = as.numeric(as.character(point1$lat))
  longg = as.numeric(as.character(point1$long))
  
  
  
  map <- leaflet() %>% addProviderTiles(providers$MtbMap) %>%
    addProviderTiles(providers$Stamen.TonerLines,
                     options = providerTileOptions(opacity = 0.35)) %>%
    addProviderTiles(providers$Stamen.TonerLabels)
  map <- map %>% 
    addCircleMarkers(data=point, radius = 8, color = 'red', fill = TRUE , labelOptions=c(noHide=TRUE)) %>%
    addMarkers(lng = longg, lat = latt)%>%
    addPolylines(data=point, lng = ~lng, lat = ~lat,stroke = TRUE)
  map
})




isolate({
  updateTabItems(session, "mytab", "Dashboard")
})

observeEvent(input$expand,{
  
  showModal(popup1)
  defaultdate = input$dbdate
}
)
observeEvent(input$expand1,{
  
  showModal(popup2)
}
)
observeEvent(input$expand2,{
  
  showModal(popup3)
}
)

popup1<-modalDialog(
  fluidPage(box(solidHeader = TRUE,status="primary",width=NULL, height="500px",title=strong("Vessel Info"),
                fluidRow(column(width=4,htmlOutput("dbdate4"),div(style = "height:50px;")),
                         column(width=4,div(style = "height:50px;")),column(width = 4,div(style = "height:50px;"))),
                fluidRow(column(width=4,offset = 4,textOutput("Awf1")),column(width=4,textOutput("Aws1")),div(style = "height:30px;")),
                fluidRow(column(width=4,offset=2,textOutput("cps1"),br(),textOutput("As1"),div(style = "height:10px;")),
                         column(width=4,offset=2,textOutput("cpc1"),br(),textOutput("Ac1"),div(style = "height:10px;"))),
                fluidRow(column(width=2,offset=1,textOutput("Apow1"),textOutput("slp1"),textOutput("rpm1"),div(style = "height:10px;")),
                         column(width=2,textOutput("Bdraft1"),div(style = "height:10px;")),column(width=4,offset=1,textOutput("Ass1"),div(style = "height:10px;")),
                         column(width=2,textOutput("Adraft1"),div(style = "height:10px;"))),
                
                tags$head(tags$style("#Awf1,#Aws1,#As1,#Ac1,#cps1,#cpc1,#Apow1,#Ass1,#Adraft1,#Bdraft1,#slp1,#rpm1{color:Blue;}"  ))     
  )),
  size="l",easyClose = TRUE
)
popup2<-modalDialog(
  fluidPage(box(width=NULL,solidHeader= TRUE,status="info",title=strong("Vessel Position"),
                column(width = 12,leafletOutput("mymap1")
                )),tags$style(type = "text/css", "#mymap1 {height: calc(100vh - 80px) !important;}")),
  size="l",easyClose = TRUE
)
popup3<-modalDialog(
  fluidPage(    box(width=NULL,height="600px",solidHeader= TRUE,status="info",title=strong("Voyage Parameters"),
                    fluidRow(column(width=2,htmlOutput("dbgsel1"))),
                    fluidRow(column(width=12,withSpinner(plotlyOutput("dashbplot1")))))
  ),
  size="l",easyClose = TRUE
)


output$dash <- renderUI({
  fluidPage(box(solidHeader = TRUE,status="primary",width=6, height="550px",title=p(strong("Vessel Info")),
                #actionButton("expand","",icon = icon("expand"),class = "btn-xl"),
                #tags$head(tags$style("#expand{position:relative; margin-left:830px;}" ))),
                fluidRow(column(width=4,htmlOutput("vesselselect"),div(style = "height:50px;")),
                         column(width=2,htmlOutput("voynodb"),div(style = "height:50px;")),
                         column(width=3,htmlOutput("dbdate")),column(width = 3,div(actionLink("expand","",icon = icon("expand","fa-2x"),class = "btn-xl"),#tags$head(tags$style("#expand{ height:40px;}" )),
                                                                                   style="float:right"))),
                fluidRow(column(width=4,offset = 4,textOutput("Awf")),column(width=4,textOutput("Aws")),div(style = "height:30px;")),
                fluidRow(column(width=4,offset=2,textOutput("cps"),br(),textOutput("As"),div(style = "height:10px;")),
                         column(width=4,offset=2,textOutput("cpc"),br(),textOutput("Ac"),div(style = "height:10px;"))),
                fluidRow(column(width=2,offset=1,textOutput("Apow"),textOutput("slp"),textOutput("rpm"),div(style = "height:10px;")),column(width=2,textOutput("Bdraft"),div(style = "height:10px;")),column(width=4,offset=1,textOutput("Ass"),div(style = "height:10px;")),column(width=2,textOutput("Adraft"),div(style = "height:10px;"))),
                
                tags$head(tags$style("#Awf,#Aws,#As,#Ac,#cps,#cpc,#Apow,#Ass,#Adraft,#Bdraft,#slp,#rpm{color:Blue;}"  ))     
  ),box(width=6,solidHeader= TRUE,status="info",height="550px",title=strong("Vessel Position"),column(width = 12,div(actionLink("expand1","",icon=icon("expand","fa-2x")),style="float:right")),br(),
        box(width=12,leafletOutput("mymap")))
  
  
  
  )
})

output$dash1 <- renderUI({
  
  fluidPage(box(width=6,solidHeader= TRUE,status="info",height="600px",title=strong("Voyage Table"),
                column(width=12,withSpinner(dataTableOutput("voyagetable")))),
            box(width=6,height="600px",solidHeader= TRUE,status="info",title=strong("Voyage Parameters"),
                fluidRow(column(width=4,htmlOutput("dbgsel")),column(width = 8,div(actionLink("expand2","",icon=icon("expand","fa-2x")),style="float:right"))),
                fluidRow(column(width=12,withSpinner(plotlyOutput("dashbplot")))))
  )
})





