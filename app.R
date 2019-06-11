library(shiny)
library(shinythemes)
library(shinyWidgets)
library(hisse)
library(utilhisse)
library(viridis)
source("module_h_scatterplot.R")
source("module_h_dotplot.R")
source("module_h_ridgelines.R")
source("module_h_trait_recon.R")
source("module_h_rate_recon.R")
data("diatoms")

ui <-
  navbarPage(
    theme = shinytheme("flatly"),
    title = "Hidden State Speciation and Extinction (HiSSE)",
    fluid = TRUE,
    inverse = TRUE,
    
    # navbar for type of model
    tabPanel(
      "BINARY",
      # file load
      column(3, 
      wellPanel(
        fileInput(
          width = "100%",
          'h_recon_input',
          'Choose a HiSSE marginal ancestral reconstruction file:',
          accept = c('.Rsave', "RSave", '.Rdata', 'RData')
        ),
        helpText('This should be an object output from `hisse::MarginRecon` or a list of such objects where each element contains the AIC score of the model. Make sure the object was saved to a file with the extension "Rsave" or "Rdata".')
      )
      ),
      column(9,
      h_scatterplot_ui(id = "1"),
      h_dotplot_ui(id = "2"),
      h_ridgelines_ui(id = "3"),
      h_trait_recon_ui(id = "4"),
      h_rate_recon_ui(id = "5")
      )
      
    ),
    tabPanel("MULTISTATE")#,
    # tabPanel("GEOGRAPHIC")
    # sidebar to load file
    
    # tabsets for different visualizations
    # tab 1 scatter
    # tab 2 trait recon
    # tab 3 rate recon
    # each tab has a download widget
    # tabs have their own additional inputs (colors, parameters, ... )
  )

server <- function(input, output) {
  h_input_name <- reactive({
    validate(
      need(
        input$h_recon_input != "" ,
        "Please select a HiSSE ancestral reconstruction file"
      )
    )
    in_file <- input$h_recon_input
    return(in_file)
  })
  
  h_recon_load <- reactive({
    H <- get(load(h_input_name()$datapath))
    validate(
      need(
        class(H) == "hisse.states" || class(H[[1]]) == "hisse.states",
        "Looks like this is not a HiSSE ancestral reconstruction file (makes sure `class(obj)` or if list, `class(obj[[1]])`, returns 'hisse.states')"
      )
    )
    return(H)
  })
  
  callModule(module = h_scatterplot_srv,
             id = "1",
             h_obj = h_recon_load)
  
  callModule(module = h_dotplot_srv,
             id = "2",
             h_obj = h_recon_load)
  
  callModule(module = h_ridgelines_srv,
             id = "3",
             h_obj = h_recon_load)
  
  callModule(module = h_trait_recon_srv,
             id = "4",
             h_obj = h_recon_load)
  
  callModule(module = h_rate_recon_srv,
             id = "5",
             h_obj = h_recon_load)
}

# Run the application
shinyApp(ui = ui, server = server)
