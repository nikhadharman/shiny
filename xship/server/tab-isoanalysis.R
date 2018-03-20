
# ISO Method.................................................................
isofile=readWorksheetFromFile( "data/test.xlsx", sheet = 1, header = TRUE )
ir= data.frame(isofile)
# vessel data..........................
design_draft = 12.5
breadth = 48.2
ref_area = 1344
#......................................
wind_coeff_heading = 0.68 #..................from 15016
vessel_name = ir$VESSEL
Fleet = ir$FLEET
date = ir$Corrected.Date
mean_draft = ir$Tm
ambient_temp = ir$AMBIENT.TEMP
ir$draft_ch = design_draft - mean_draft
draft_change = ir$draft_ch
del_power = ir$POWER.KW
disp = ir$DISPLACEMENT
stw = ir$SPEED.BY.LOG * .51444
sog = ir$SPEED.BY.GPS * .51444
wind_dir = ir$WIND.DIRECTION
wind_speed = ir$WIND.SPEED * .51444
proj_area = ref_area + breadth * draft_change
air_density = 101325/(287.058 * (ambient_temp + 273.15))
wind_coeff = ir$CAA #...............................................need input from 15016
res_resistance = (.5 * air_density * proj_area * wind_speed^2 * wind_coeff) - (.5 * air_density * proj_area * sog^2 * wind_coeff_heading)
corr_power = del_power - (res_resistance * sog /700)
refcurv_speed = ir$Speed.from.reference.curve.KN.for.corrected.power * .51444 #..............need input for a displacement and corrected power from s/p trial
perf_value = (stw-refcurv_speed) * 100/refcurv_speed

isodf= data.frame(Fleet,vessel_name,date,ambient_temp,mean_draft,del_power,disp,stw,sog,refcurv_speed,wind_dir,wind_speed,draft_change,proj_area
                  ,air_density,res_resistance,corr_power,perf_value)


output$ivesselUI <- renderUI({ 
  r=isodf
  s=input$itype
  if(s=="Fleet Wise"){
    r= subset(r,r$Fleet ==input$iefleet)
    Vessel_List = unique(as.character(r[,2]), incomparables = FALSE)
    selectInput("iVesselname", label=h4(strong("Vessel")), choices = Vessel_List, selected = NULL, multiple = FALSE, selectize = TRUE, width ="75%" , size = NULL)
  }else{
    
    Vessel_List = unique(as.character(r[,2]), incomparables = FALSE)
    selectInput("iVesselname", label=h4(strong("Vessel")), choices = Vessel_List, selected = NULL, multiple = FALSE, selectize = TRUE, width ="75%" , size = NULL)
    
  }
})

output$ifleet <- renderUI({ 
  r=isodf
  s=input$itype
  if(s=="Fleet Wise"){
    Vessel_List = unique(as.character(r[,1]), incomparables = FALSE)
    selectInput("iefleet", label=h4(strong("Fleet")), choices = Vessel_List, selected = NULL, multiple = FALSE, selectize = TRUE, width = "30%", size = NULL)
  }else{return(NULL)}
})


isofiltdata=reactive({
  y=isodf 
  ff=input$iVesselname
  y=subset(y,y$vessel_name == ff)
  y
})

refdata=reactive({
  y = isofiltdata()
  ref = subset(y,as.Date(date) >= as.Date(input$idate1[1]) & as.Date(date) <= as.Date(input$idate1[2]))
  ref
  
}
)

filtdata=reactive({
  y= isofiltdata()
  
  
  y=subset(y,as.Date(date)>=as.Date(input$idate1[1]) & as.Date(date)<=as.Date(input$idate2[2]))
  y
  
}
)

evadata=reactive({
  
  y=isofiltdata()
  
  y=subset(y,as.Date(date)>=as.Date(input$idate2[1]) & as.Date(date)<=as.Date(input$idate2[2]))
  
  
  y
}
)



output$isoDataTable1 = renderDataTable({
  
  datatable(filtdata(),class = 'cell-border stripe', options = list(pageLength = 150,searching = FALSE,paging = T,audowidth=T,dom = "tip",
                                                                    scrollX=TRUE, scrollY = 600,
                                                                    scroller = TRUE,
                                                                    scrollCollapse=TRUE),rownames = FALSE)%>%formatStyle(names(refdata()),backgroundColor = "#ECF0F1")
  
})

output$isospcurve = renderPlotly({
  data = isofiltdata()
  y=plot_ly(x =data$stw ,y=data$del_power ,name="STW",type="scatter",mode= 'markers',marker=list(color= "#5E05F0",size=10))%>%
    add_trace(x =data$refcurv_speed,y=data$del_power,name="Corrected STW",type="scatter",mode= 'markers',marker=list(color= "#FE0707",size=10))

  y%>%layout(
    xaxis = list(title = "Speed (m/s)", titlefont =f, tickfont =f,gridcolor = "#FFFFFF",tickangle=0),
    yaxis = list(title="Power (KW)",titlefont = f, tickfont = f,gridcolor = "#ABB2B9"), 
    plot_bgcolor = "#FFFFFF",
    paper_bgcolor = "#FFFFFF",showlegend = TRUE,legend=l
    
  )
  
})

output$isorefcurve = renderPlotly({ 
  y = isofiltdata()
  ref = subset(y,as.Date(date) >= as.Date(input$idate1[1]) & as.Date(date) <= as.Date(input$idate1[2]))
  y=plot_ly(x = ref$stw,y=ref$del_power ,name="STW",type="scatter",mode= 'markers',marker=list(color= "#5E05F0",size=10))
   y=y%>% add_trace(x =ref$refcurv_speed,y=ref$del_power,name="Corrected STW",mode= 'markers',marker=list(color= "#FE0707",size=10))

  y%>%layout(
    xaxis = list(title = "Speed (m/s)", titlefont =f, tickfont =f,gridcolor = "#FFFFFF",tickangle=0),
    yaxis = list(title="Power (KW)",titlefont = f, tickfont = f,gridcolor = "#ABB2B9"), 
    plot_bgcolor = "#FFFFFF",
    paper_bgcolor = "#FFFFFF",showlegend = TRUE,legend=l
    
  )
  
})


output$isoevacurve = renderPlotly({
  data = evadata()
  y=plot_ly(x = data$stw ,y=data$del_power ,name="STW",type="scatter",mode= 'markers',marker=list(color= "#5E05F0",size=10))
   y =y%>% add_trace(x = data$refcurv_speed,y=data$del_power,name="Corrected STW",type="scatter",mode= 'markers',marker=list(color= "#FE0707",size=10))

  
  y%>%layout(
    xaxis = list(title = "Speed (m/s)", titlefont =f, tickfont =f,gridcolor = "#FFFFFF",tickangle=0),
    yaxis = list(title="Power (KW)",titlefont = f, tickfont = f,gridcolor = "#ABB2B9"), 
    plot_bgcolor = "#FFFFFF",
    paper_bgcolor = "#FFFFFF",showlegend = TRUE,legend=l
    
  )
  
})

output$isorefpi = renderPlotly({
  z=refdata()
  avg= mean(z$perf_value)
  data = refdata()
  z1=evadata()
  avg1= mean(z1$perf_value)
  data1 = evadata()
  p=plot_ly(x =data$date ,y=data$perf_value ,name="Reference STW",type="scatter",mode= 'markers',marker=list(color= "#5E05F0",size=10),showlegend = TRUE)
  p=p%>% add_trace(x =c(head(data$date,1),tail(data$date,1)),y=c(avg,avg),name="Average PV",type="scatter",mode="lines+markers",marker= list(color="#FE0707",size=2),showlegend = FALSE)
  p=p%>%add_trace(x =data1$date ,y=data1$perf_value ,name="Evaluation STW",mode= 'markers',type="scatter",marker=list(color= "green",size=10),showlegend = TRUE)
  p=p%>%add_trace(x =c(head(data1$date,1),tail(data1$date,1)) ,y=c(avg1,avg1),name="Average  PV",type="scatter",mode="lines+markers",marker= list(color="#FE0707",size=2),showlegend = FALSE)
  
  

  p=p%>%layout(
    xaxis = list(title = "Period", titlefont =f, tickfont =f,gridcolor = "#FFFFFF",tickangle=0),
    yaxis = list(title="PVs",titlefont = f, tickfont = f,gridcolor = "#ABB2B9"), 
    plot_bgcolor = "#FFFFFF",
    paper_bgcolor = "#FFFFFF",legend=l
    
  )
  
})



output$REFAVG <- renderValueBox({
  y=refdata()
  if(is.null(y)){return()}
  #avg1= mean(y$stw)
  #avg2= mean(y$refcurv_speed)
  #speedloss_ref= round((avg1 - avg2)*100/mean(y$refcurv_speed), digits = 3)
  valueBox(
    paste0(round(mean(y$perf_value),2), "%"), strong("Performance Value (PV)"), 
    color = "navy",width = NULL
  )}
)

output$EVAAVG <- renderValueBox({
  y=evadata()
  if(is.null(y)){return()}
  #avg1= mean(y$stw)
  #avg2= mean(y$refcurv_speed)
  #speedloss_ref= round((avg1 - avg2)*100/mean(y$refcurv_speed), digits = 3)
  valueBox(
    paste0(round(mean(y$perf_value),2), "%"), strong("Performance Value (PV)"), 
    color = "navy",width = NULL
  )}
)

output$PI <- renderValueBox({
  z=evadata()
  y=refdata()
  if(is.null(y)){return()}
  avg1= mean(y$perf_value)
  avg2= mean(z$perf_value)
  pi= round((avg2 - avg1), digits = 3)
  valueBox(
    paste0(pi, "%"), strong("Performance Indicator (PI)"), 
    color = "navy",width = NULL
  )
})


