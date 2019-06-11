params <- c(
  "Net turnover",
  "Extinction fraction",
  "Speciation",
  "Extinction",
  "Net diversification"
)

theme_h_ridge <- theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.background = element_rect(color="black", size = 1))

##### --- h_dotplot ui  ---------------------- #####

h_ridgelines_ui <- function(id) {
  ns <- NS(id)
  tagList(
    checkboxGroupButtons(
      inputId = ns("h_ridge"),
      choiceNames = "Ridgelines of the distribution of model averaged diversification rates",
      choiceValues = 1,
      status = "primary",
      selected = "Ridgelines of the distribution of model averaged diversification rates"
    ),
    
    conditionalPanel(
      condition = paste0("input['", ns("h_ridge"), "'] == 1"),
      wellPanel(fluidRow(
        column(
          3,
          # parameter
          selectInput(
            inputId = ns("parameter"),
            label = "Parameter:",
            choices = params,
            selected = "Net turnover"
          ),
          # # x_label
          # textInput(
          #   inputId = ns("x_label"),
          #   label = "X axis label:",
          #   placeholder = "The binary trait in your model"
          # ),
          # states_names
          textInput(
            inputId = ns("states_names1"),
            label = "State 0:",
            placeholder = "Character state 0",
            value = 0
          ),
          textInput(
            inputId = ns("states_names2"),
            label = "State 1:",
            placeholder = "Character state 1",
            value = 1
          ),
          # plot_as_waiting_time
          checkboxInput(
            inputId = ns("plot_as_waiting_time"),
            label = "Plot as waiting time",
            value = FALSE
          ),
          actionButton(inputId = ns("plot"), label = "Plot"),
          tags$hr()
        ),
        column(width = 9, 
               plotOutput(ns("plt")))
      )
      )))
}

##### --- h_dotplot srv ---------------------- #####

h_ridgelines_srv <-
  function(input,
           output,
           session,
           h_obj) {
    h_proc <- reactive({
      x <- h_process_recon(h_obj ())
      return(x)
    })
    
    param <- reactive({
      x <- case_when(
        input$parameter == "Net turnover" ~ "turnover",
        input$parameter == "Extinction fraction" ~ "extinct.frac",
        input$parameter == "Speciation" ~ "speciation",
        input$parameter == "Extinction" ~ "extinction",
        input$parameter == "Net diversification" ~ "net.div"
      )
      return(x)
    })
    
    plt <- eventReactive(input$plot, {
      p <- h_ridgelines(
        processed_hisse_recon = h_proc(),
        parameter = param(),
        # x_label = input$x_label,
        states_names = c(input$states_names1, input$states_names2),
        plot_as_waiting_time = input$plot_as_waiting_time,
        colors= viridis(end = 0.6, n=2)
      ) +
        theme_h_ridge
      return(p)
    })
    
    output$plt <- renderPlot({
      plt()
    })
  }