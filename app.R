#INFO201 Final App

library(shiny)

source("our_ui.R")
source("our_server.R")

shinyApp(ui = ui, server = server)