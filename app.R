library(shiny)
library(shinythemes)
library(shinyWidgets)
library(hisse)
library(utilhisse)
library(viridis)
library(colorplaner)

refs_text <- tags$div(
  tags$p("This web application is based on the following papers and R packages:"),
  tags$p("Papers:"),
  tags$p(
      "Beaulieu, J. M., & O’Meara, B. C. (2016). Detecting hidden diversification shifts in models of trait-dependent speciation and extinction. Systematic biology, 65(4), 583-601.", tags$a(href="https://academic.oup.com/sysbio/article/65/4/583/1753616", "link")
    ),
  tags$p(
      "Caetano, D. S., O'Meara, B. C., & Beaulieu, J. M. (2018). Hidden state models improve state‐dependent diversification approaches, including biogeographical models. Evolution, 72(11), 2308-2324.", tags$a(href="https://onlinelibrary.wiley.com/doi/abs/10.1111/evo.13602", "link")
    ),
  tags$p(
      "Nakov, T., Beaulieu, J. M., & Alverson, A. J. (2019). Diatoms diversify and turn over faster in freshwater than marine environments. bioRxiv, 406165.", tags$a(href="https://doi.org/10.1101/406165", "link")
    ),
  tags$p("R packages:"),
  tags$p("Beaulieu, J., O'Meara, B., & Caetano, D. (2019). Package ‘hisse’", tags$a(href="https://cran.r-project.org/web/packages/hisse/index.html", 'CRAN')),
  tags$p("Nakov, T. (2019). Package ’utilhisse’", tags$a(href="https://github.com/teofiln/utilhisse", 'Guthub'))
  )

# modules
mods <-
  list.files(path = "modules/",
             pattern = "*.R",
             full.names = TRUE)
lapply(mods, source)

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
               helpText(
                 'This should be an object output from `hisse::MarginRecon` or a list of such objects where each element contains the AIC score of the model. Make sure the object was saved to a file with the extension "Rsave" or "Rdata".'
               ),
               checkboxInput("h_demo", label = "Use demo file", value = FALSE),
               conditionalPanel(condition = "input.h_demo", 
                                HTML("This is a model fit for a CID4 model for plankton-benthos trait in diatoms. For more information see our paper: <a href=https://doi.org/10.1101/406165>'Biorxiv'</a>.")),
               tags$hr(),
               checkboxGroupButtons(
                 inputId = "references",
                 choiceNames = "References",
                 choiceValues = 1,
                 status = "success",
                 selected = "References"
               ),
               conditionalPanel(condition = "input.references == 1",
                                wellPanel(refs_text))
             )),
      column(
        9,
        h_scatterplot_ui(id = "1"),
        h_dotplot_ui(id = "2"),
        h_ridgelines_ui(id = "3"),
        h_trait_recon_ui(id = "4"),
        h_rate_recon_ui(id = "5")
      )
      
    ),
    tabPanel(
      "MULTISTATE",
      # file load
      column(3,
             wellPanel(
               fileInput(
                 width = "100%",
                 'm_recon_input',
                 'Choose a MuHiSSE marginal ancestral reconstruction file:',
                 accept = c('.Rsave', "RSave", '.Rdata', 'RData')
               ),
               helpText(
                 'This should be an object output from `hisse::MarginReconMuHiSSE` or a list of such objects where each element contains the AIC score of the model. Make sure the object was saved to a file with the extension "Rsave" or "Rdata".'
               ),
               checkboxInput("m_demo", label = "Use demo file", value = FALSE),
               conditionalPanel(condition = "input.m_demo", 
                                HTML("This is a model fit for a MuHiSSE model for marine-freshwater + plankton-benthos interaction in diatoms. For more information see our paper: <a href=https://doi.org/10.1101/406165>'Biorxiv'</a>.")),
      tags$hr(),
      checkboxGroupButtons(
        inputId = "references2",
        choiceNames = "References",
        choiceValues = 1,
        status = "success",
        selected = "References"
      ),
      conditionalPanel(condition = "input.references2 == 1",
                       wellPanel(refs_text))
    )),
      column(
        9,
        m_scatterplot_ui(id = "11"),
        m_dotplot_ui(id = "12"),
        m_ridgelines_ui(id = "13"),
        m_scatterplot_cp_ui(id = "14"),
        m_trait_recon_ui(id = "15"),
        m_trait_recon_cp_ui(id = "16"),
        m_rate_recon_ui(id = "17")
      )
    )#,
    # tabPanel("GEOGRAPHIC")
    
  )

server <- function(input, output) {
  ##### ---- server logic for binary ----------------------- #####
  h_recon_load <- reactive({
    if (input$h_demo) {
      data("diatoms")
      H <- diatoms$cid4_recon
      return(H)
    } else {
      validate(
        need(
          input$h_recon_input != "" ,
          "Please select a HiSSE ancestral reconstruction file"
        )
      )
      in_file <- input$h_recon_input
      
      H <- get(load(in_file$datapath))
      validate(
        need(
          class(H) == "hisse.states" || class(H[[1]]) == "hisse.states",
          "Looks like this is not a MuHiSSE ancestral reconstruction file (makes sure `class(obj)` or if list, `class(obj[[1]])`, returns 'muhisse.states')"
        )
      )
      return(H)
    }
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
  
  
  ##### ---- server logic for multistate ----------------------- #####
  m_recon_load <- reactive({
    if (input$m_demo) {
      data("diatoms")
      H <- diatoms$muhisse_recon
      return(H)
    } else {
      validate(
        need(
          input$m_recon_input != "" ,
          "Please select a MuHiSSE ancestral reconstruction file"
        )
      )
      in_file <- input$m_recon_input
      H <- get(load(in_file$datapath))
      validate(
        need(
          class(H) == "muhisse.states" || class(H[[1]]) == "muhisse.states",
          "Looks like this is not a MuHiSSE ancestral reconstruction file (makes sure `class(obj)` or if list, `class(obj[[1]])`, returns 'muhisse.states')"
        )
      )
      return(H)
    }
  })
  
  callModule(module = m_scatterplot_srv,
             id = "11",
             h_obj = m_recon_load)
  
  callModule(module = m_dotplot_srv,
             id = "12",
             h_obj = m_recon_load)
  
  callModule(module = m_ridgelines_srv,
             id = "13",
             h_obj = m_recon_load)
  
  callModule(module = m_scatterplot_cp_srv,
             id = "14",
             h_obj = m_recon_load)
  
  callModule(module = m_trait_recon_srv,
             id = "15",
             h_obj = m_recon_load)
  
  callModule(module = m_trait_recon_cp_srv,
             id = "16",
             h_obj = m_recon_load)

  callModule(module = m_rate_recon_srv,
             id = "17",
             h_obj = m_recon_load)
}

# Run the application
shinyApp(ui = ui, server = server)
