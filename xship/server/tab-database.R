
#RAW datas...... 
output$databasefleet <- renderUI({ 
  r=DATA
  s=input$databaseselecttype
  if(is.null(s))
    return(NULL)
  if(s=="Fleet Wise")
  {
    Fleet_List =suppressWarnings(unique(as.character(r[,1]), incomparables = FALSE))
    selectInput("databaseFleet", label=h4(strong("Fleet")), choices = Fleet_List, selected = 1, multiple = FALSE, selectize = TRUE, width = "50%", size = NULL)
  }else {return(NULL)}
})


output$databaseselectUI <- renderUI({ 
  r=DATA
  s=input$databaseselecttype
  if(is.null(s))
    return(NULL)
  if(s=="Fleet Wise"){
    r=subset(r,Fleet == input$databaseFleet)
    
    Vessel_List = suppressWarnings(unique(as.character(r[,3]), incomparables = FALSE))
    selectInput("databaseVessel", label=h4(strong("Vessel")), choices = Vessel_List, selected = "Strategic Alliance", multiple = FALSE, selectize = TRUE, width = "50%", size = NULL)
  }
  else{
    
    Vessel_List = suppressWarnings( unique(as.character(r[,3]), incomparables = FALSE))
    selectInput("databaseVessel", label=h4(strong("Vessel")), choices = Vessel_List, selected = "Strategic Alliance", multiple = FALSE, selectize = TRUE, width = "50%", size = NULL)
  }
})

Rawdata = reactive ({
  y=DATA
  ff=input$databaseVessel
  y=subset(y,y$Vessel.Name==ff)
  y
})

DD=reactive(
  {
    inFile <- input$file
    if (is.null(inFile)){
      Rawdata()
    }
    else{
      dataf=data.frame(read.csv(inFile$datapath))
    }
  } 
)
#Table DATA.....
output$table <- DT::renderDataTable({ 
  y=DD()
  datatable(y,class = 'cell-border stripe',options = list(pageLength = 50,dom = "tip",
                                                          scrollX=TRUE,
                                                          scrollCollapse=TRUE),rownames = FALSE)%>%
    formatStyle(names(y),backgroundColor = "#ECF0F1",color="#000")
  
})

#ADA datas...... 

output$Wdata=renderUI({
  y= VESSELDETAILS
  if(is.null(y))
    return(NULL)
  validate(
    need(try(input$databaseVessel),"Please Wait or Select the vessel")
  )
  ff=input$databaseVessel
  y=suppressWarnings(subset(y,y$Vessel == ff))
  r=y
  ADA = unique(as.character(r[,15]), incomparables = FALSE)
  if(ADA =="Y"){
    paste("ADA DATA AVAILABLE")
  }
  else{paste("NO ADA DATA AVAILABLE")}
})

ADAdata = reactive ({
  y=ADADATA
  ff=input$databaseVessel
  y=subset(y,y$Vessel==ff)
  x=nrow(y)
  if (x==0){Y=NULL} else{
    y}
  
})


#ADA  Table DATA.....
output$ADAtable <- DT::renderDataTable({ 
  y=ADAdata()
  if(is.null(y)){return()}
  
  datatable(y,class = 'cell-border stripe',options = list(pageLength = 150,dom = "tip",
                                                          scrollX=TRUE,
                                                          scrollCollapse=TRUE),rownames = FALSE)%>%
    formatStyle(names(y),backgroundColor = "#ECF0F1",color="#000")
  
})
