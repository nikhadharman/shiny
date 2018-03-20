
# VESSEL OPERATION OVERVIEW......................  

agregg_conseqp = reactive({
  if(input$Aggreg_conseqp=="Yearly"){
    f=paste("%Y")
  }else {
    f=paste("%b-%y")
  }
  f
})
agregg_consfuel = reactive({
  if(input$Aggreg_consfuel=="Yearly"){
    f=paste("%Y")
  }else {
    f=paste("%b-%y")
  }
  f
})
agregg_timepara = reactive({
  if(input$Aggreg_timepara=="Yearly"){
    f=paste("%Y")
  }else {
    f=paste("%b-%y")
  }
  f
})


output$conseqp=renderPlotly({
  validate(
    need(try(input$DDVessel),"Please Wait ...")
  )
  validate(
    need(try(input$ConEqp),"Please Wait ...")
  )
  ff=agregg_conseqp()
  y=DATA 
  y = subset(y,Class == input$DDVessel)
  y$ss <-  sort(as.Date(y$Corrected.Date,'%d-%m-%y'),decreasing = F)
  y=subset(y,as.Date(ss,"%d-%m-%y")>=as.Date(input$Datadate[1],"%d-%m-%y") & as.Date(ss,"%d-%m-%y")<=as.Date(input$Datadate[2],"%d-%m-%y"))
  y$MEcon =as.numeric(y$FUEL.M.E.HS)+as.numeric(y$FUEL.M.E.LS)+as.numeric(y$FUEL.M.E.MDO)+as.numeric(y$FUEL.M.E.MGO)+as.numeric(y$FUEL.M.E.MGO.LS)
  y$AEcon = as.numeric(y$FUEL.AUX.HS)+as.numeric(y$FUEL.AUX.LS)+as.numeric(y$FUEL.AUX.MDO)+as.numeric(y$FUEL.AUX.MGO)+as.numeric(y$FUEL.AUX.MGO.LS)
  y$BLcon = as.numeric(y$FUEL.BOILER.HS)+as.numeric(y$FUEL.BOILER.LS)+as.numeric(y$FUEL.BOILER.MDO)+as.numeric(y$FUEL.BOILER.MGO)+as.numeric(y$FUEL.BOILER.MGO.LS)
  y$Tcon= y$MEcon + y$AEcon + y$BLcon 
  
  if(input$Aggreg_conseqp=="Yearly"){
    y$ss=format(y$ss,ff)
    y$ss=as.Date(y$s, format=ff)
    
  }else if(input$Aggreg_conseqp=="Monthly")
  {
    y$s=format(y$ss,ff)
    y$ss=as.yearmon(y$ss, format=ff)
    
  } else if(input$Aggreg_conseqp=="Quarterly"){
    y$ss=format(y$ss,'%d-%m-%y')
    y$ss=as.yearqtr(y$ss, format='%d-%m-%y')
    y$ss=as.Date(y$ss, format=ff)
  }
  else if(input$Aggreg_conseqp=="Daily"){
    y$ss <-  as.Date(y$Corrected.Date,'%d-%m-%y')
  }
  
  if(input$ConEqp == "Main Engine(mt)"){
    u=ddply(y, .(Vessel.Name,ss),summarize,tot=sum(MEcon))
  } else if(input$ConEqp == "AE System(mt)"){
    u=ddply(y, .(Vessel.Name,ss),summarize,tot=sum(AEcon))
  } else if(input$ConEqp == "Boilers(mt)"){
    u=ddply(y, .(Vessel.Name,ss),summarize,tot=sum(BLcon))
  }  else {
    u=ddply(y, .(Vessel.Name,ss),summarize,tot=sum(Tcon))
  }
  if(input$Aggreg_conseqp=="Daily"){
    cc=unique(as.character(u[,1]), incomparables = FALSE)
    uu=subset(u,Vessel.Name==cc[1])
    p= plot_ly(x=as.Date.character(uu$ss),y=uu$tot,name= cc[1],type="scatter",mode='lines+markers',line = list(shape = "linear",width=3),marker=list(width=5))
    if(length(cc)>1)
      
    {
      for(i in 2:length(cc))
      {
        xx=u
        xx=subset(xx,Vessel.Name==cc[i])
        p=p%>%add_trace(x=as.Date.character(xx$ss),y=xx$tot,type="scatter",name= cc[i],mode='lines+markers',line = list(shape = "linear",width=3),marker=list(width=5))
      }
    }
    p=p%>%layout(title = input$ConEqp,titlefont=c,width=NULL,xaxis=list(title = input$Aggreg_conseqp, titlefont =f, tickfont =f,gridcolor = "#FFFFFF",rangemode = "nonzero"),
             yaxis=list(title = "Consumption (mt)", titlefont =f, tickfont =f,gridcolor = "#E5E7E9"),showlegend=T,legend = l, margin = list(l=70,b = 85,r=70), plot_bgcolor = "#FFFFFF",paper_bgcolor = "#FFFFFF",showlegend = T,width=NULL
    )
  } else {
    cc=unique(as.character(u[,1]), incomparables = FALSE)
    uu=subset(u,Vessel.Name==cc[1])
    p= plot_ly(x=format(uu$ss,ff),y=uu$tot,name= cc[1],type="bar")
    if(length(cc)>1)
      
    {
      for(i in 2:length(cc))
      {
        xx=u
        xx=subset(xx,Vessel.Name==cc[i])
        p=p%>%add_trace(x=format(xx$ss,ff),y=xx$tot,name= cc[i],type="bar")
      }
    }
    p=p%>%layout(title = input$ConEqp,titlefont=s,width=NULL,xaxis=list(type = "category",
                                                                        categoryorder = "array",
                                                                        categoryarray = sort(uu$ss),
                                                                        title = input$Aggreg_conseqp, titlefont =s, tickfont =s,gridcolor = "white",rangemode = "nonzero"),
                 yaxis=list(title = "Consumption (MT)", titlefont =s, tickfont =s,gridcolor = "#E5E7E9"),showlegend=T,legend = l, margin = list(l=70,b = 85,r=70), plot_bgcolor = "white",paper_bgcolor = "white",showlegend = T,width=NULL
    )
  }
  
})
output$consfuel=renderPlotly({
  validate(
    need(try(input$DDVessel),"Please Wait ...")
  )
  validate(
    need(try(input$ConFuel),"Please Wait ...")
  )
  ff=agregg_consfuel()
  y=DATA 
  y = subset(y,Class == input$DDVessel)
  y$ss <-  as.Date(y$Corrected.Date,'%d-%m-%y')
  y=subset(y,as.Date(ss,"%d-%m-%y")>=as.Date(input$Datadate[1],"%d-%m-%y") & as.Date(ss,"%d-%m-%y")<=as.Date(input$Datadate[2],"%d-%m-%y"))
  y$HS = as.numeric(as.character(y$FUEL.M.E.HS))+as.numeric(as.character(y$FUEL.AUX.HS))+as.numeric(y$FUEL.BOILER.HS)
  y$LS = as.numeric(as.character(y$FUEL.M.E.LS))+as.numeric(as.character(y$FUEL.AUX.LS))+as.numeric(y$FUEL.BOILER.LS)
  y$MDO =as.numeric(as.character(y$FUEL.M.E.MDO)) +as.numeric(as.character(y$FUEL.AUX.MDO))+as.numeric(y$FUEL.BOILER.MDO)
  y$MGO =as.numeric(as.character(y$FUEL.M.E.MGO))+as.numeric(as.character(y$FUEL.AUX.MGO))+as.numeric(y$FUEL.BOILER.MGO)
  y$MGOLS=as.numeric(as.character(y$FUEL.M.E.MGO.LS))+as.numeric(y$FUEL.BOILER.MGO.LS)+as.numeric(as.character(y$FUEL.AUX.MGO.LS))
  y$TCO2=y$HS + y$LS + y$MDO + y$MGO+y$MGOLS
  
  if(input$Aggreg_consfuel=="Yearly"){
    y$ss=format(y$ss,ff)
    y$ss=as.Date(y$s, format=ff)
    
  }else if(input$Aggreg_consfuel=="Monthly")
  {
    y$s=format(y$ss,ff)
    y$ss=as.yearmon(y$ss, format=ff)
    
  } else if(input$Aggreg_consfuel=="Quarterly"){
    y$ss=format(y$ss,'%d-%m-%y')
    y$ss=as.yearqtr(y$ss, format='%d-%m-%y')
    y$ss=as.Date(y$ss, format=ff)
  }
  else if(input$Aggreg_consfuel=="Daily"){
    y$ss <-  as.Date(y$Corrected.Date,'%d-%m-%y')
  }
  
  
  
  if(input$ConFuel == "Fuel HS"){
    u=ddply(y, .(Vessel.Name,ss),summarize,tot=sum(HS))
  } else if(input$ConFuel == "Fuel LS"){
    u=ddply(y, .(Vessel.Name,ss),summarize,tot=sum(LS)) 
  } else if(input$ConFuel == "Fuel MDO"){
    u=ddply(y, .(Vessel.Name,ss),summarize,tot=sum(MDO))
  } else if(input$ConFuel == "Fuel MGO"){
    u=ddply(y, .(Vessel.Name,ss),summarize,tot=sum(MGO))
  } else if(input$ConFuel == "Fuel MGO LS"){
    u=ddply(y, .(Vessel.Name,ss),summarize,tot=sum(MGOLS))
  } else {
    u=ddply(y, .(Vessel.Name,ss),summarize,tot=sum(TCO2))
  }
  
  if(input$Aggreg_consfuel=="Daily"){
    cc=unique(as.character(u[,1]), incomparables = FALSE)
    uu=subset(u,Vessel.Name==cc[1])
    p= plot_ly(x=as.Date.character(uu$ss),y=uu$tot,name= cc[1],type="scatter",mode='lines+markers',line = list(shape = "linear",width=3),marker=list(width=5))
    if(length(cc)>1)
      
    {
      for(i in 2:length(cc))
      {
        xx=u
        xx=subset(xx,Vessel.Name==cc[i])
        p=p%>%add_trace(x=as.Date.character(xx$ss),y=xx$tot,type="scatter",name= cc[i],mode='lines+markers',line = list(shape = "linear",width=3),marker=list(width=5))
      }
    }
    p=p%>%layout(title = input$ConFuel,titlefont=c,width=NULL,xaxis=list(title =input$Aggreg_consfuel, titlefont =f, tickfont =f,gridcolor = "#FFFFFF",rangemode = "nonzero"),
             yaxis=list(title = "Consumption (mt)", titlefont =f, tickfont =f,gridcolor = "#E5E7E9"),showlegend=T,legend = l, margin = list(l=70,b = 85,r=70), plot_bgcolor = "#FFFFFF",paper_bgcolor = "#FFFFFF",showlegend = T,width=NULL
    )
  } else {
    cc=unique(as.character(u[,1]), incomparables = FALSE)
    uu=subset(u,Vessel.Name==cc[1])
    p= plot_ly(x=format(uu$ss,ff),y=uu$tot,name= cc[1],type="bar")
    if(length(cc)>1)
      
    {
      for(i in 2:length(cc))
      {
        xx=u
        xx=subset(xx,Vessel.Name==cc[i])
        p=p%>%add_trace(x=format(xx$ss,ff),y=xx$tot,name= cc[i],type="bar")
      }
    }
    p=p%>%layout(title = input$ConFuel,titlefont=s,width=NULL,xaxis=list(type = "category",
                                                                         categoryorder = "array",
                                                                         categoryarray = sort(uu$ss),title =input$Aggreg_consfuel, titlefont =s, tickfont =s,gridcolor = "white",rangemode = "nonzero"),
                 yaxis=list(title = "Consumption (MT)", titlefont =s, tickfont =s,gridcolor = "#E5E7E9"),showlegend=T,legend = l, margin = list(l=70,b = 85,r=70), plot_bgcolor = "white",paper_bgcolor = "white",showlegend = T,width=NULL
    )
  }
  
  
})
output$timepara=renderPlotly({ 
  validate(
    need(try(input$DDVessel),"Please Wait ...")
  )
  validate(
    need(try(input$Timepara),"Please Wait ...")
  )
  ff=agregg_timepara()
  y=DATA 
  y = subset(y,Class==input$DDVessel)
  y=subset(y,Report.Type=="NOON")
  y$ss <-  as.Date(y$Corrected.Date,'%d-%m-%y')
  y=subset(y,as.Date(ss,"%d-%m-%y")>=as.Date(input$Datadate[1],"%d-%m-%y") & as.Date(ss,"%d-%m-%y")<=as.Date(input$Datadate[2],"%d-%m-%y"))
  
  if(input$Aggreg_timepara=="Yearly"){
    y$ss=format(y$ss,ff)
    y$ss=as.Date(y$s, format=ff)
    
  }else if(input$Aggreg_timepara=="Monthly")
  {
    y$s=format(y$ss,ff)
    y$ss=as.yearmon(y$ss, format=ff)
    
  } else if(input$Aggreg_timepara=="Quarterly"){
    y$ss=format(y$ss,'%d-%m-%y')
    y$ss=as.yearqtr(y$ss, format='%d-%m-%y')
    y$ss=as.Date(y$ss, format=ff)
  }
  else if(input$Aggreg_timepara=="Daily"){
    y$ss <-  as.Date(y$Corrected.Date,'%d-%m-%y')
  }
  
  if(input$Timepara == "M/E Avg SFOC (gr/kWh)"){
    u=ddply(y, .(Vessel.Name,ss),summarize,tot=mean(as.numeric(as.character(SFOC))))
    ya="SFOC (gr/kWh)"
  } else  if(input$Timepara == "M/E Avg Power (kW)"){
    u=ddply(y, .(Vessel.Name,ss),summarize,tot=mean(as.numeric(POWER.kW)))
    ya="Power(kW)"
  } else if(input$Timepara == "M/E Avg Slip"){
    u=ddply(y, .(Vessel.Name,ss),summarize,tot=mean(as.numeric(SLIP)))
    ya="Slip"
  } else if(input$Timepara == "M/E Avg SCOC (gr/kWh)"){
    u=ddply(y, .(Vessel.Name,ss),summarize,tot=mean(as.numeric(as.character(SCOC))))
    ya="SCOC (gr/kWh)"
  } else if(input$Timepara == "Avg Speed GPS (kn)"){
    u=ddply(y, .(Vessel.Name,ss),summarize,tot=mean(as.numeric(as.character(SOG))))
    ya="Speed(kn)"
  } else if(input$Timepara == "Avg Speed Log (kn)"){
    u=ddply(y, .(Vessel.Name,ss),summarize,tot=mean(as.numeric(as.character(SOW))))
    ya="Speed(kn)"
  } else if(input$Timepara == "Fwd Draft (m)"){
    u=ddply(y, .(Vessel.Name,ss),summarize,tot=mean(as.numeric(as.character(DRAFT.FWD))))
    ya="Draft(m)"
  }  else if(input$Timepara == "Aft Draft (m)"){
    u=ddply(y, .(Vessel.Name,ss),summarize,tot=mean(as.numeric(as.character(DRAFT.AFT))))
    ya="Draft(m)"
  } else if(input$Timepara == "Mean Draft (m)"){
    u=ddply(y, .(Vessel.Name,ss),summarize,tot=mean(as.numeric(as.character(DRAFT))))
    ya="Draft(m)"
  } else if(input$Timepara == "Trim (m)"){
    y$TRIM=(y$DRAFT.AFT-y$DRAFT.FWD)
    u=ddply(y, .(Vessel.Name,ss),summarize,tot=mean(as.numeric(as.character(TRIM))))
    ya="Trim (Aft - Fwd)(m)"
  } else if(input$Timepara == "Seastate"){
    u=ddply(y, .(Vessel.Name,ss),summarize,tot=mean(as.numeric(as.character(SEA.STATE))))
    ya="Beaufort Number"
  } else if(input$Timepara == "M/E Avg Rpm (rpm)"){
    u=ddply(y, .(Vessel.Name,ss),summarize,tot=mean(as.numeric(as.character(RPM))))
    ya=" Engine RPM (rpm)"
  } else if(input$Timepara == "ROB FO SLUDGE"){
    u=ddply(y, .(Vessel.Name,ss),summarize,tot=max(as.numeric(as.character(ROB.FO.SLUDGE))))
    ya=" FO Sludge (T)"
  }
  
  
  
  if(input$Aggreg_timepara=="Daily"){
    cc=unique(as.character(u[,1]), incomparables = FALSE)
    uu=subset(u,Vessel.Name==cc[1])
    p= plot_ly(x=as.Date.character(uu$ss),y=uu$tot,name= cc[1],type="scatter",mode='lines+markers',line = list(shape = "linear",width=3),marker=list(width=6))
    if(length(cc)>1)
      
    {
      for(i in 2:length(cc))
      {
        xx=u
        xx=subset(xx,Vessel.Name==cc[i])
        p=p%>%add_trace(x=as.Date.character(xx$ss),y=xx$tot,name= cc[i],type="scatter",mode='lines+markers',line = list(shape = "linear",width=3),marker=list(width=6))
      }
    }
    p=p%>%layout(title =input$Timepara,titlefont=c,width=NULL,xaxis=list(title =input$Aggreg_timepara, titlefont =f, tickfont =f,gridcolor = "#FFFFFF",rangemode = "nonzero"),
             yaxis=list(title = ya, titlefont =f, tickfont =f,gridcolor = "#E5E7E9"),showlegend=T,legend = l, margin = list(l=70,b = 85,r=50), plot_bgcolor = "#FFFFFF",paper_bgcolor = "#FFFFFF",showlegend = T,width=NULL
    )
    
  }
  else {
    
    cc=unique(as.character(u[,1]), incomparables = FALSE)
    uu=subset(u,Vessel.Name==cc[1])
    p= plot_ly(x=format(uu$ss,ff),y=uu$tot,name= cc[1],type="bar")
    if(length(cc)>1)
      
    {
      for(i in 2:length(cc))
      {
        xx=u
        xx=subset(xx,Vessel.Name==cc[i])
        p=p%>%add_trace(x=format(xx$ss,ff),y=xx$tot,name= cc[i],type="bar")
        
      }
    }
    p=p%>%layout(title = input$Timepara,titlefont=s,width=NULL,xaxis=list(type = "category",
                                                                          categoryorder = "array",
                                                                          categoryarray = sort(uu$ss),title =input$Aggreg_timepara, titlefont =s, tickfont =s,gridcolor = "white",rangemode = "nonzero"),
                 yaxis=list(title = ya, titlefont =s, tickfont =s,gridcolor = "#E5E7E9"),showlegend=T,legend = l, margin = list(l=70,b = 85,r=50), plot_bgcolor = "white",paper_bgcolor = "white",showlegend = T,width=NULL
    )
  }
  
})

output$relationpara=renderPlotly({ 
  validate(
    need(try(input$DDVessel),"Please Wait ...")
  )
  validate(
    need(try(input$Relationpara),"Please Wait ...")
  )
  
  y=DATA 
  y$Tsteaming = as.numeric(y$STEAMING.TIME.AUX.1)+as.numeric(y$STEAMING.TIME.AUX.2)+as.numeric(y$STEAMING.TIME.AUX.3)+as.numeric(y$STEAMING.TIME.AUX.4)+as.numeric(y$STEAMING.TIME.AUX.5)
  y$AEcon = as.numeric(y$FUEL.AUX.HS)+as.numeric(y$FUEL.AUX.LS)+as.numeric(y$FUEL.AUX.MDO)+as.numeric(y$FUEL.AUX.MGO)+as.numeric(y$FUEL.AUX.MGO.LS)
  y = subset(y,Class==input$DDVessel)
  y=subset(y,Report.Type=="NOON")
  y$ss <-  as.Date(y$Corrected.Date,'%d-%m-%y')
  y=subset(y,as.Date(ss,"%d-%m-%y")>=as.Date(input$Datadate[1],"%d-%m-%y") & as.Date(ss,"%d-%m-%y")<=as.Date(input$Datadate[2],"%d-%m-%y"))
  cc=unique(as.character(y[,3]), incomparables = FALSE)
  y=subset(y,Report.Type=="NOON")
  u=subset(y,Vessel.Name==cc[1])
  if(input$Relationpara == 1)
  {
    
    p= plot_ly(x=as.numeric(as.character(u$POWER.kW)),y=as.numeric(as.character(u$SFOC)),name= cc[1],type="scatter",mode = "markers",marker= list(size=8))
    if(length(cc)>1)
      
    {
      for(i in 2:length(cc))
      {
        xx=y
        xx=subset(xx,Vessel.Name==cc[i])
        p=p%>%add_trace(x=as.numeric(as.character(xx$POWER.kW)),y=as.numeric(as.character(xx$SFOC)),type="scatter",mode = "markers",marker= list(size=8),name= cc[i])
      }
    }
    p=p%>%layout(title = "Power Vs SFOC",titlefont=c,width=NULL,xaxis=list(title ="POWER(kW)", titlefont =f, tickfont =f,gridcolor = "#FFFFFF",rangemode = "nonzero"),
             yaxis=list(title = "SFOC (gr/kWh)", titlefont =f, tickfont =f,gridcolor = "#E5E7E9"),showlegend=T,legend = l, margin = list(l=70,b = 85,r=50), plot_bgcolor = "#FFFFFF",paper_bgcolor = "#FFFFFF",showlegend = T,width=NULL
    )
  } else if(input$Relationpara == 2)
  {
    p= plot_ly(x=as.numeric(as.character(u$POWER.kW)),y=as.numeric(as.character(u$SCOC)),name= cc[1],type="scatter",mode = "markers",marker= list(size=8))
    if(length(cc)>1)
      
    {
      for(i in 2:length(cc))
      {
        xx=y
        xx=subset(xx,Vessel.Name==cc[i])
        p=p%>%add_trace(x=as.numeric(as.character(xx$POWER.kW)),y=as.numeric(as.character(xx$SCOC)),type="scatter",mode = "markers",marker= list(size=8),name= cc[i])
      }
    }
    p=p%>%layout(title = "Power Vs SCOC",titlefont=c,width=NULL,xaxis=list(title ="POWER(kW)", titlefont =f, tickfont =f,gridcolor = "#FFFFFF",rangemode = "nonzero"),
             yaxis=list(title = "SCOC (gr/kWh)", titlefont =f, tickfont =f,gridcolor = "#E5E7E9"),showlegend=T,legend = l, margin = list(l=70,b = 85,r=50), plot_bgcolor = "#FFFFFF",paper_bgcolor = "#FFFFFF",showlegend = T,width=NULL
    )
  } else if(input$Relationpara == 3)
  {
    p= plot_ly(x=as.numeric(as.character(u$SOG)),y=as.numeric(as.character(u$M.E.FOC)),name= cc[1],type="scatter",mode = "markers",marker= list(size=8))
    if(length(cc)>1)
      
    {
      for(i in 2:length(cc))
      {
        xx=y
        xx=subset(xx,Vessel.Name==cc[i])
        p=p%>%add_trace(x=as.numeric(as.character(xx$SOG)),y=as.numeric(as.character(xx$M.E.FOC)),type="scatter",mode = "markers",marker= list(size=8),name= cc[i])
      }
    }
    p=p%>%layout(title = "Speed Vs ME FO Cons",titlefont=c,width=NULL,xaxis=list(title ="Speed (kn)", titlefont =f, tickfont =f,gridcolor = "#FFFFFF",rangemode = "nonzero"),
             yaxis=list(title = "ME FO Consumption(t)", titlefont =f, tickfont =f,gridcolor = "#E5E7E9"),showlegend=T,legend = l, margin = list(l=70,b = 85,r=50), plot_bgcolor = "#FFFFFF",paper_bgcolor = "#FFFFFF",showlegend = T,width=NULL
    )
  }  else if(input$Relationpara == 4)
  {
    
    p= plot_ly(x=u$Tsteaming,y=as.numeric(as.character(u$AEcon)),name= cc[1],type="scatter",mode = "markers",marker= list(size=8))
    if(length(cc)>1)
      
    {
      for(i in 2:length(cc))
      {
        xx=y
        xx=subset(xx,Vessel.Name==cc[i])
        p=p%>%add_trace(x=as.numeric(as.character(xx$Tsteaming)),y=as.numeric(as.character(xx$AEcon)),type="scatter",mode = "markers",marker= list(size=8),name= cc[i])
      }
    }
    p=p%>%layout(title = "AE Running Time(Hr) Vs FO Cons(t)",titlefont=c,width=NULL,xaxis=list(title ="Running Hours", titlefont =f, tickfont =f,gridcolor = "#FFFFFF",rangemode = "nonzero"),
             yaxis=list(title = "FO Consumption(t)", titlefont =f, tickfont =f,gridcolor = "#E5E7E9"),showlegend=T,legend = l, margin = list(l=70,b = 85,r=50), plot_bgcolor = "#FFFFFF",paper_bgcolor = "#FFFFFF",showlegend = T,width=NULL
    )
  }  else if(input$Relationpara == 5)
  {
    
    p= plot_ly(x=as.numeric(as.character(u$RPM)),y=as.numeric(as.character(u$POWER.kW)),name= cc[1],type="scatter",mode = "markers",marker= list(size=8))
    if(length(cc)>1)
      
    {
      for(i in 2:length(cc))
      {
        xx=y
        xx=subset(xx,Vessel.Name==cc[i])
        p=p%>%add_trace(x=as.numeric(as.character(xx$RPM)),y=as.numeric(as.character(xx$POWER.kW)),type="scatter",mode = "markers",marker= list(size=8),name= cc[i])
      }
    }
    p=p%>%layout(title = "Engine RPM Vs POWER ",titlefont=c,width=NULL,xaxis=list(title ="RPM", titlefont =f, tickfont =f,gridcolor = "#FFFFFF",rangemode = "nonzero"),
             yaxis=list(title = "Power(kW)", titlefont =f, tickfont =f,gridcolor = "#E5E7E9"),showlegend=T,legend = l, margin = list(l=70,b = 85,r=50), plot_bgcolor = "#FFFFFF",paper_bgcolor = "#FFFFFF",showlegend = T,width=NULL
    )
  }
  
})
# Operating Profile .....................
output$OPST <- renderUI({
  sliderInput("steamingtime",label = "Steaming time ",min = 20,max = 26,value = c(23,25), step = 1 ,width = "75%")
})
output$OPSS <- renderUI({
  sliderInput("SeaState",label = "Sea State",min = 0,max = 9,value = c(0,3), step = 1 ,width = "75%")
})
output$OPWS <- renderUI({
  sliderInput("Windspeed",label = "Wind Speed",min = 0,max = 70,value = c(0,15), step = 5 ,width = "75%")
})
output$Sel<- renderUI({ 
  radioButtons("Selection",label=NULL,choices = c("Average","Range") , selected = "Average", inline = T)
})

output$Operatingprofile<- renderUI({ 
  selectInput("operatingprofile",label="OP",choices = c("Draft(m)","Speed(kn)","Power(kW)","FO/24 Hrs(t)","Sea state","Slip","RPM") , selected = "Draft(m)", multiple = F, selectize = TRUE, width = "75%", size = NULL)
})
output$Operatingprofilechart=renderPlotly({ 
  validate(
    need(try(input$DDVessel),"Please Wait ...")
  )
  validate(
    need(try(input$operatingprofile),"Please Wait ...")
  )
  y=DATA 
  y = subset(y,Class==input$DDVessel)
  y=subset(y,as.numeric(as.character(RPM)) > 0)
  y$ss <-  as.Date(y$Corrected.Date,'%d-%m-%y')
  y =subset(y,as.Date(ss,"%d-%m-%y")>=as.Date(input$Datadate[1],"%d-%m-%y") & as.Date(ss,"%d-%m-%y")<=as.Date(input$Datadate[2],"%d-%m-%y"))
  y=subset(y,as.numeric(as.character(SEA.STATE)) >= as.numeric (input$SeaState[1]) & as.numeric(as.character(SEA.STATE)) <= as.numeric (input$SeaState[2]) )
  y=subset(y,as.numeric(as.character(WIND.SPEED)) >= as.numeric (input$Windspeed[1]) & as.numeric(as.character(WIND.SPEED)) <= as.numeric (input$Windspeed[2]) )
  y=subset(y,as.numeric(as.character(TOTAL.STEAMING.TIME)) >= as.numeric (input$steamingtime[1]) & as.numeric(as.character(TOTAL.STEAMING.TIME)) <= as.numeric (input$steamingtime[2]) )
  cc=unique(as.character(y[,3]), incomparables = FALSE)
  
  y$month= y$ss 
  
  if(input$operatingprofile == "Draft(m)" )
  {
    h = ddply(y, .(Vessel.Name,month),summarize,tot=mean(as.numeric(as.character(DRAFT))))
  } else if(input$operatingprofile == "Speed(kn)" )   {
    h = ddply(y, .(Vessel.Name,month),summarize,tot=mean(as.numeric(as.character(SOG))))
  } else if(input$operatingprofile == "Power(kW)" ) {
    h = ddply(y, .(Vessel.Name,month),summarize,tot=mean(as.numeric(as.character(POWER.kW))))
  } else if(input$operatingprofile == "Sea state" ) {
    h = ddply(y, .(Vessel.Name,month),summarize,tot=mean(as.numeric(as.character(SEA.STATE))))
  } else if(input$operatingprofile == "FO/24Hrs(t)" ) {
    h = ddply(y, .(Vessel.Name,month),summarize,tot=mean(as.numeric(as.character(FO.per.24Hrs))))
  } else if(input$operatingprofile == "RPM" ) {
    h = ddply(y, .(Vessel.Name,month),summarize,tot=mean(as.numeric(as.character(RPM))))
  } else  {
    h = ddply(y, .(Vessel.Name,month),summarize,tot=mean(as.numeric(as.character(SLIP))))
  }
  hh=subset(h,Vessel.Name==cc[1])
  p =plot_ly(x=as.Date(hh$month,"%b-%y"),y = hh$tot,name= cc[1],mode='lines+markers',type="scatter",line = list(shape = "linear",width=3),marker=list(width=6))
  if(length(cc)>1)
    
  {
    for(i in 2:length(cc))
    {
      xx=h
      xx=subset(xx,Vessel.Name==cc[i])
      p=p%>%add_trace(x=as.Date(xx$month,"%b-%y"),y=xx$tot,mode='lines+markers',type="scatter",line = list(shape = "linear",width=3),marker=list(width=6),name= cc[i])
    }
  }
  p=p%>%layout(title = input$operatingprofile,titlefont=c,width=NULL,xaxis=list(title ="Monthly Average", titlefont =f, tickfont =f,gridcolor = "#FFFFFF"),
           yaxis=list(title = input$operatingprofile, titlefont =f, tickfont =f,gridcolor = "#E5E7E9"),showlegend=T,legend = l, margin = list(l=70,b = 85,r=50), plot_bgcolor = "#FFFFFF",paper_bgcolor = "#FFFFFF",showlegend = T,width=NULL
  )
  
})