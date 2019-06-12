params <- c(
  "Net turnover",
  "Extinction fraction",
  "Speciation",
  "Extinction",
  "Net diversification"
)

theme_h_scatter <- theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.background = element_rect(color="black", size = 1))

##### --- m_scatterplot ui  ---------------------- #####

m_scatterplot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    checkboxGroupButtons(
      inputId = ns("m_scatter"),
      choiceNames = "Scatterplot of model averaged diversification rates",
      choiceValues = 1,
      status = "primary",
      selected = "Scatterplot of model averaged diversification rates"
    ),
    
    conditionalPanel(
      condition = paste0("input['", ns("m_scatter"), "'] == 1"),
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
            label = "State 00:",
            placeholder = "Character state 00",
            value = "00"
          ),
          textInput(
            inputId = ns("states_names2"),
            label = "State 01:",
            placeholder = "Character state 01",
            value = "01"
          ),
          textInput(
            inputId = ns("states_names3"),
            label = "State 10:",
            placeholder = "Character state 10",
            value = "10"
          ),
          textInput(
            inputId = ns("states_names4"),
            label = "State 11:",
            placeholder = "Character state 11",
            value = "11"
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
        column(width = 7, 
               plotOutput(ns("plt")))
      )
      )))
}

##### --- m_scatterplot srv ---------------------- #####

m_scatterplot_srv <-
  function(input,
           output,
           session,
           h_obj) {
    h_proc <- reactive({
      x <- m_process_recon(h_obj ())
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
      p <- m_scatterplot(
        processed_muhisse_recon = h_proc(),
        parameter = param(),
        # x_label = input$x_label,
        states_names = c(input$states_names1, input$states_names2, input$states_names3, input$states_names4),
        plot_as_waiting_time = input$plot_as_waiting_time,
        colors= viridis(end = 0.6, n=4)
      ) +
        theme_h_scatter
      return(p)
    })
    
    output$plt <- renderPlot({
      plt()
    })
  }