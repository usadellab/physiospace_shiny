require(shiny)
require(PhysioSpaceMethods)
require(HumanPhysioSpace)
require(PlantPhysioSpace)
require(parallel)
# Allow upload of 1GB tables
options(shiny.maxRequestSize = 1024 ^ 3)
# Show all errors in detail:
options(shiny.sanitize.errors = FALSE)
# Allow for maximum parallel computation - use all available cores
options(mc.cores = detectCores())

phys.spaces <-
  c('', unlist(lapply(c('HumanPhysioSpace', 'PlantPhysioSpace'), function(x)
    data(package = x)$results[, "Item"])))

isValid <-
  function(x) {
    !is.null(x) && !all(is.na(x)) && length(x) > 0
  }

allOrSelection <-
  function(sub.set, compl.set) {
    if (isValid(sub.set) && isValid(compl.set)) {
      intersect(sub.set, compl.set)
    } else if (!isValid(sub.set) && isValid(compl.set))
      compl.set
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
                    conditionalPanel(condition = "output.receivedAllInput",
                                     actionButton("compute", "Compute")),
                    conditionalPanel(
                      condition = "output.done === 'true'",
                      tags$hr(),
                      tags$h3('Plot settings'),
                      sliderInput(
                        "reducedPlotting",
                        "If non-zero this is the number of significant rows plotted per gene.",
                        min = 0,
                        max = 20,
                        value = 0,
                        step = 1
                      ),
                      selectizeInput(
                        "physSpaceTissues",
                        "Select subset of Physio-Space tissues to display in plot. Leave empty to display all.",
                        c(),
                        multiple = TRUE
                      ),
                      selectizeInput(
                        "experimentTissues",
                        "Select subset of your experiment's tissues to display in plot. Leave empty to display all.",
                        c(),
                        multiple = TRUE
                      ),
                      tags$hr(),
                      tags$h3('Results'),
                      downloadButton("downloadPhysSpaceMap", "Download results table (Physio-Map)"),
                      tags$br(),
                      downloadButton("downloadPhysioHeatMap", "Download plot (PDF)")
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
  phys.map <- reactive({
    if (as.logical(input$compute) && (
      isValid(input$upGenes) &&
      isValid(input$downGenes) ||
      isValid(input$file1$datapath)
    )) {
      # Create a Progress object
      progress <-
        shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      # Show progress message
      progress$set(message = "Analyzing data. This can take a while..", value = 0)
      tryCatch({
        PhysioSpaceMethods::calculatePhysioMap(
          InputData = calc.phys.map.inp(),
          Space = get(input$physSpace),
          NumbrOfCores = getOption("mc.cores", 1)
        )
      }, error = function(e) {
        showNotification(
          paste("An unexpected error has occurred. Please try again.", e),
          duration = 6,
          type = 'error'
        )
      }, finally = {
        # Do nothing. This is needed to prevent the shiny App from dying in case of an error.
      })
    }
  })
  
  # Enable selection of tissues to reduce large plots:
  observe({
    if (!is.null(phys.map())) {
      updateSelectizeInput(
        session,
        "physSpaceTissues",
        choices = rownames(phys.map()),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "experimentTissues",
        choices = colnames(phys.map()),
        server = TRUE
      )
    }
  })
  
  # Plot the results
  plotPhysioHeatMap <- reactive({
    if (!is.null(phys.map()) &&
        is.matrix(phys.map())) {
      pm.cols <-
        allOrSelection(input$experimentTissues, colnames(phys.map()))
      pm.rows <-
        allOrSelection(input$physSpaceTissues, rownames(phys.map()))
      
      p.h <- PhysioHeatmap(
        PhysioResults = phys.map()[pm.rows, pm.cols, drop = FALSE],
        main = "PhysioSpace Heatmap",
        SymmetricColoring = TRUE,
        SpaceClustering = TRUE,
        # The PhysioMap's rows are the columns of the PhysioSpace:
        Space = get(input$physSpace)[, pm.rows, drop = FALSE],
        ReducedPlotting = (if (!is.null(input$reducedPlotting) &&
                               as.integer(input$reducedPlotting) > 0) {
          as.integer(input$reducedPlotting)
        } else
          FALSE)
      )
      # See stackoverflow.com/questions/27276994/outputting-shiny-non-ggplot-plot-to-pdf
      dev.copy2pdf(file = "/home/shiny/plot.pdf")
      p.h
    }
  })
  # Make plot available in output:
  output$physSpacePlot <-
    renderPlot({
      plotPhysioHeatMap()
    })
  
  # Enable downloading of results-table:
  output$downloadPhysSpaceMap <- downloadHandler(
    filename = function() {
      "physioSpace_Results.csv"
    },
    content = function(file) {
      write.table(
        phys.map(),
        file,
        row.names = TRUE,
        sep = ",",
        quote = FALSE
      )
    }
  )
  
  # Enable downloading of the Plot in PDF:
  output$downloadPhysioHeatMap <- downloadHandler(
    filename =
      'physioHeatMap.pdf'
    ,
    content = function(file) {
      # See stackoverflow.com/questions/27276994/outputting-shiny-non-ggplot-plot-to-pdf
      file.copy("/home/shiny/plot.pdf", file)
    },
    contentType = 'application/pdf'
  )
  
  # Enable the user to start the computation, if all required input has been provided:
  output$receivedAllInput <- reactive({
    isValid(input$upGenes) &&
      isValid(input$downGenes) || isValid(input$file1$datapath)
  })
  outputOptions(output, 'receivedAllInput', suspendWhenHidden = FALSE)
  
  # Signal success in computation:
  output$done <- reactive({
    if (!is.null(phys.map()))
      'true'
    else
      'false'
  })
  outputOptions(output, 'done', suspendWhenHidden = FALSE)
}

# Start the shiny server
shinyApp(ui = ui, server = server)
