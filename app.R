require(shiny)
require(PhysioSpaceMethods)
require(HumanPhysioSpace)
require(PlantPhysioSpace)
require(parallel)
# Allow upload of 1GB tables
options(shiny.maxRequestSize = 1024 ^ 3)
# Allow for maximum parallel computation - use all available cores
options(mc.cores = detectCores())

phys.spaces <-
  c('', unlist(lapply(c('HumanPhysioSpace', 'PlantPhysioSpace'), function(x)
    data(package = x)$results[, "Item"])))

isValid <-
  function(x) {
    !is.null(x) && !all(is.na(x)) && length(x) > 0
  }

ui <- fluidPage(titlePanel("Physiospace Web Interface"),
                
                # Sidebar layout with input and output definitions
                sidebarLayout(
                  # Sidebar panel for inputs
                  sidebarPanel(
                    # Input: Slider for the number of bins
                    selectInput("physSpace", "Select a Physio-Space for your analysis", choices = phys.spaces),
                    
                    conditionalPanel(
                      condition = "typeof input.physSpace !== 'undefined' && input.physSpace !== ''",
                      selectInput(
                        "geneListOrExpressionMatrix",
                        "Please select a mode of input",
                        choices = c('',
                                    "Gene-List",
                                    "Expression-Matrix")
                      )
                    ),
                    conditionalPanel(
                      condition = paste(
                        "typeof input.physSpace !== 'undefined' && input.physSpace !== '' &&",
                        "typeof input.geneListOrExpressionMatrix !== 'undefined' &&",
                        "input.geneListOrExpressionMatrix === 'Expression-Matrix'"
                      ),
                      fileInput(
                        "file1",
                        paste(
                          "Please upload your table (max. 1GB and CSV-Format). For details on how to prepare it,",
                          "please consult the Vignette of the R package `PhysioSpaceMethods`."
                        ),
                        multiple = TRUE,
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")
                      )
                    ),
                    
                    conditionalPanel(
                      condition = paste(
                        "typeof input.physSpace !== 'undefined' && input.physSpace !== '' &&",
                        "typeof input.geneListOrExpressionMatrix !== 'undefined' &&",
                        "input.geneListOrExpressionMatrix === 'Gene-List'"
                      ),
                      selectizeInput(
                        "upGenes",
                        "Please select the significantly up-regulated Genes",
                        c(),
                        multiple = TRUE
                      ),
                      selectizeInput(
                        "downGenes",
                        "Please select the significantly down-regulated Genes",
                        c(),
                        multiple = TRUE
                      )
                    ),
                    conditionalPanel(
                      condition = "output.receivedAllInput",
                      sliderInput(
                        "reducedPlotting",
                        paste(
                          "If non-zero this is the number of significant rows plotted per gene.",
                          "Note, that you can change the value and the plot without clicking 'Compute' again."
                        ),
                        min = 0,
                        max = 20,
                        value = 0,
                        step = 1
                      ),
                      actionButton("compute", "Compute")
                    )
                  )
                  ,
                  
                  # Main panel for displaying outputs:
                  mainPanel(plotOutput(outputId = "physSpacePlot"))
                ))

# Define the server:
server <- function(input, output, session) {
  # Generate the choices of gene identifiers depending on the selected Physio-Space:
  physSpaceGeneIds <- reactive({
    if (!is.null(input$physSpace) && nchar(input$physSpace) > 0) {
      rownames(get(input$physSpace))
    } else
      c()
  })
  
  # Enable selection of up- and down-regulated genes:
  observe({
    if (!is.null(input$geneListOrExpressionMatrix) &&
        input$geneListOrExpressionMatrix == 'Gene-List') {
      updateSelectizeInput(session,
                           "upGenes",
                           choices = physSpaceGeneIds(),
                           server = TRUE)
      updateSelectizeInput(session,
                           "downGenes",
                           choices = physSpaceGeneIds(),
                           server = TRUE)
    }
  })
  
  # Calculate the Physiospace Mapping:
  calc.physio.map <- observeEvent(input$compute,
                                  if (isValid(input$upGenes) &&
                                      isValid(input$downGenes) ||
                                      isValid(input$file1$datapath)) {
                                    # Generate input for the function calculatePhysioMap:
                                    calc.phys.map.inp <- reactive({
                                      if (!is.null(input$file1$datapath)) {
                                        # Read uploaded data as matrix
                                        counts.df <- as.matrix(
                                          read.table(
                                            input$file1$datapath,
                                            sep = ",",
                                            quote = '"',
                                            comment.char = '',
                                            header = TRUE,
                                            stringsAsFactors = FALSE
                                          )
                                        )
                                        # row-names must be gene names in the first column
                                        rownames(counts.df) <-
                                          counts.df[, 1]
                                        # Normalize read counts as deviations from mean
                                        counts.df - apply(counts.df, 1, mean)
                                      } else if (isValid(input$upGenes) &&
                                                 isValid(input$downGenes)) {
                                        # Just use a list of significantly up- and down-regulated genes:
                                        list(input$upGenes, input$downGenes)
                                      }
                                    })
                                    # Calculate the Physiospace Map
                                    physio.map <- reactive({
                                      # Create a Progress object
                                      progress <-
                                        shiny::Progress$new()
                                      # Make sure it closes when we exit this reactive, even if there's an error
                                      on.exit(progress$close())
                                      # Show progress message
                                      progress$set(message = "Analyzing data. This can take a while..", value = 0)
                                      tryCatch({
                                        PhysioSpaceMethods::calculatePhysioMap(
                                          InputData = calc.phys.map.inp()[, 1:5],
                                          Space = HS_LUKK_Space,
                                          #PARALLEL = TRUE,
                                          NumbrOfCores = getOption("mc.cores", 1)
                                        )
                                      }, error = function(e) {
                                        showNotification(
                                          paste("An unexpected error has occurred. Please try again.", e),
                                          duration = 6,
                                          type = 'error'
                                        )
                                      }, finally = {
                                        # Do nothing. This is needed to prevent the shiny App from dying.
                                      })
                                    })
                                    # Plot the results
                                    output$physSpacePlot <-
                                      renderPlot({
                                        if (!is.null(physio.map()) && is.matrix(physio.map())) {
                                          PhysioHeatmap(
                                            PhysioResults = physio.map(),
                                            main = "RNA-seq vs Microarray",
                                            SymmetricColoring = TRUE,
                                            SpaceClustering = TRUE,
                                            Space = HS_LUKK_Space,
                                            ReducedPlotting = (if (!is.null(input$reducedPlotting) &&
                                                                   as.integer(input$reducedPlotting) > 0) {
                                              as.integer(input$reducedPlotting)
                                            } else
                                              FALSE)
                                          )
                                        }
                                      })
                                    
                                  })
  
  # Enable the user to start the computation, if all required input has been provided:
  output$receivedAllInput <- reactive({
    isValid(input$upGenes) &&
      isValid(input$downGenes) || isValid(input$file1$datapath)
  })
  outputOptions(output, 'receivedAllInput', suspendWhenHidden = FALSE)
}

# Start the shiny server
shinyApp(ui = ui, server = server)
