#### Log in module ###
USER <- reactiveValues(Logged = Logged,PASSWORD = PASSWORD)

output$pass <- renderText({  
  if (USER$Logged == FALSE) {
    if (!is.null(input$Login)) {
      if (input$Login > 0) {
        Username <- isolate(input$userName)
        Password <- isolate(input$passwd)
        Id.username <- which(PASSWORD$username == Username)
        Id.password <- which(PASSWORD$Passord    == Password)
        if (length(Id.username) > 0 & length(Id.password) > 0) {
          if (Id.username == Id.password) {
            USER$Logged <- TRUE
          } 
        } else  {
          "User name or password failed!"
        }
      } 
    }
  }
})