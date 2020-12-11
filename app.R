# Developed with R version 3.3.2 (64-bit)
library(dplyr)
library(stringr)
library(png)
library(shinyjs)
library(DT)
library(visNetwork)
library(rintrojs)

#For shinyapp.io deployment:
library(BiocManager)
options(repos = BiocManager::repositories())

library(shinydashboard)
phys.spaces <-
  c('', unlist(lapply(c('PlantPhysioSpace'), function(x)
    data(package = x)$results[, "Item"])))
source("carouselPanel.R")

# Panel div for visualization
# override the currently broken definition in shinyLP version 1.1.0
panel_div <- function(class_type, content) {
  div(class = sprintf("panel panel-%s", class_type),
      div(class = "panel-body", content)
  )
}

require(shiny)
require(PhysioSpaceMethods)
require(PlantPhysioSpace)
require(parallel)
# Allow upload of 1GB tables
options(shiny.maxRequestSize = 1024 ^ 3)
# Show all errors in detail:
options(shiny.sanitize.errors = FALSE)
# Allow for maximum parallel computation - use all available cores
options(mc.cores = detectCores())


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



ui <- shinyUI(navbarPage(title = img(src="logo.png", height = "60px", style = "margin-top: -15px"), 
                         id = "navBar",
                         theme = "paper.css",
                         collapsible = TRUE,
                         inverse = TRUE,
                         windowTitle = "Plant.PhysioSpace",
                         position = "fixed-top",
                         footer = includeHTML("./www/include_footer.html"),
                         header = tags$style(
                           ".navbar-right {
                           float: right !important;
                           }",
                       "body {padding-top: 75px;}"),
                       
                       tabPanel("HOME", value = "home",
                                
                                shinyjs::useShinyjs(),
                                introjsUI(),
                                
                                tags$head(tags$script(HTML('
                                                           var fakeClick = function(tabName) {
                                                           var dropdownList = document.getElementsByTagName("a");
                                                           for (var i = 0; i < dropdownList.length; i++) {
                                                           var link = dropdownList[i];
                                                           if(link.getAttribute("data-value") == tabName) {
                                                           link.click();
                                                           };
                                                           }
                                                           };
                                                           '))),
                                fluidRow(
                                  HTML("
                                       <section class='banner'>
                                       <h1 class='parallax'>Analyze
                                       <br>
                                       Your Stressed Plants
                                       </h1>
                                       <p class='parallax_description'>Plant.PhysioSpace is an 
                                       analytical tool that measures stress response in plants using gene 
                                       expression data.</p>
                                       </section>
                                       ")
                                  ),
                                
                                
                                # PAGE BREAK
                                tags$hr(),
                                
                                # INSTRUCTIONAL SECTION
                                fluidRow(
                                  shiny::HTML("<br><br><center> <h1>What Plant.PhysioSpace Provides</h1> </center>
                                              <br>")
                                  ),
                                
                                fluidRow(
                                  # column(3),
                                  
                                  column(4,
                                         div(class="panel panel-default", 
                                             div(class="panel-body",  width = "600px",
                                                 align = "center",
                                                 div(
                                                   h5("Multiple Species Analysis")
                                                 ),
                                                 div(align = "center",
                                                     h6(
                                                       "Because of the inter-species capability 
                                                       'source', our method can process virtually 
                                                       any plant species!"
                                                     )
                                                     ),
                                                 div(
                                                   tags$div(align = "center",
                                                            tags$a("Read more",
                                                                   href="https://doi.org/10.3389/fpls.2021.577789",
                                                                   class="btn btn-success btn-lg"))
                                                 )
                                                     )
                                         )
                                ),
                                column(4,
                                       div(class="panel panel-default",
                                           div(class="panel-body",  width = "600px", 
                                               align = "center",
                                               div(
                                                 h5("Stress Response Quantification")
                                               ),
                                               div(
                                                 h6(
                                                   "Our tool computes the plant stress response type 
                                                   and magnitute from gene expression!"
                                                 )
                                                 ),
                                               div(
                                                 tags$div(align = "center",
                                                          tags$a("Try it out",
                                                                 onclick="fakeClick('dash');window.scrollTo(0, 0)",
                                                                 class="btn btn-success btn-lg"))
                                               )
                                               )
                                )
                                ),
                                column(4,
                                       div(class="panel panel-default",
                                           div(class="panel-body",  width = "600px", 
                                               align = "center",
                                               div(
                                                 h5("Freedom and Convenience")
                                               ),
                                               div(
                                                 h6(
                                                   "Programming knowhow not needed; as a free webtool
                                                   our method is designed to be easy-to-use and accessible!"
                                                 )
                                                 ),
                                               div(
                                                 tags$div(align = "center",
                                                          tags$a("Check out tutorials",
                                                                 onclick="fakeClick('tutorial');window.scrollTo(0, 0)",
                                                                 class="btn btn-success btn-lg"))
                                               )
                                               )
                                )
                                )
                                # column(3)
                                
                                  ),
                                
                                fluidRow(
                                  
                                  style = "height:50px;"),
                                
                                # PAGE BREAK
                                tags$hr(),
                                
                                # AFTERWARD
                                fluidRow(
                                  column(6,
                                         img(src="about_hero.jpg", width = "100%", 
                                             style="margin-left: -50px")),
                                  column(5,
                                         shiny::HTML("<br><br><left> <h1>Plant<span style='color: #00cc00;'>.</span>PhysioSpace is...</h1> </center><br>"),
                                         shiny::HTML("<h5>a derivitive of 
                                                     <a href='https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0077627'>
                                                     <i>PhysioSpace</i> method by Lenz et al.</a>, which can measure stress response in plants.
                                                     The tool is trained on more than 4000 data sets from publicly available plant stressed gene 
                                                     expression data. And it using an optimized mapping method to compare you data to 
                                                     its database and provide an in-depth stress analysis of your plants.</h5>"),
                                         br(),
                                         tags$a(class="btn btn-primary", 
                                                href="https://doi.org/10.3389/fpls.2021.577789", 
                                                "Read more about it on our paper")
                                         ),
                                  column(1)
                                  ),
                                
                                fluidRow(
                                  
                                  style = "height:50px;"),
                                
                                # PAGE BREAK
                                tags$hr(),
                                
                                fluidRow(shiny::HTML("<br><br><center> <h1>Ready to Get Started?</h1> </center>
                                                     <br>")
                                ),
                                fluidRow(
                                  column(3),
                                  column(6,
                                         tags$div(align = "center", 
                                                  tags$a("Start", 
                                                         onclick="fakeClick('dash');window.scrollTo(0, 0)", 
                                                         class="btn btn-success btn-lg")
                                         )
                                  ),
                                  column(3)
                                ),
                                fluidRow(style = "height:25px;"
                                )
                                
                                ), # Closes the first tabPanel called "Home"
                       
                       tabPanel("DASHBOARD", value = "dash",
                                
                                dashboardPage(skin = "green",
                                              dashboardHeader(title = "Physio-dashboard"),
                                              dashboardSidebar(
                                                # tags$div(
                                                #   actionButton("help", "Need help?"),
                                                #   style = "margin-left:20%;width:40%;"
                                                # ),
                                                introBox(
                                                  selectInput("physSpace", "Select a reference Physio-Space for your analysis:", 
                                                              choices = phys.spaces),
                                                  data.step = 1,
                                                  data.position = "left",
                                                  data.intro = "First you have to choose a reference for your analysis. 
                                                  Explanations about different spaces are provided in the tutorial section."
                                                ),
                                                introBox(
                                                  conditionalPanel(
                                                    condition = "typeof input.physSpace !== 'undefined' && input.physSpace !== ''",
                                                    radioButtons(
                                                      "geneListOrExpressionMatrix",
                                                      "Select a mode of input:",
                                                      choices = c("Gene-List",
                                                                  "(RELATIVE)Expression-Matrix")
                                                    )
                                                  ),
                                                  data.step = 2,
                                                  data.intro = "Then, you have to choose if you want a ..."
                                                ),
                                                conditionalPanel(
                                                  condition = paste(
                                                    "typeof input.physSpace !== 'undefined' && input.physSpace !== '' &&",
                                                    "typeof input.geneListOrExpressionMatrix !== 'undefined' &&",
                                                    "input.geneListOrExpressionMatrix === '(RELATIVE)Expression-Matrix'"
                                                  ),
                                                  fileInput(
                                                    "file1",
                                                    paste(
                                                      "Upload your table: a max. 1GB CSV file",
                                                      "(For details on how to prepare your data,",
                                                      "check the tutorial section)"
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
                                                    "Select the significantly up-regulated Genes:",
                                                    c(),
                                                    multiple = TRUE
                                                  ),
                                                  selectizeInput(
                                                    "downGenes",
                                                    "Select the significantly down-regulated Genes:",
                                                    c(),
                                                    multiple = TRUE
                                                  )
                                                ),
                                                conditionalPanel(condition = "output.receivedAllInput",
                                                                 actionButton("compute", "Compute"))
                                                ),
                                              dashboardBody(
                                                tags$style(HTML("
                                                                .box.box-solid.box-primary>.box-header {
                                                                color:#ECF0F5;
                                                                background:#ECF0F5
                                                                }
                                                                
                                                                .box.box-solid.box-primary{
                                                                border-bottom-color:#ECF0F5;
                                                                border-left-color:#ECF0F5;
                                                                border-right-color:#ECF0F5;
                                                                border-top-color:#ECF0F5;
                                                                }
                                                                ")),
                                                fluidRow(column(width=8,
                                                                box(width = 12,solidHeader = TRUE,
                                                                    status = "primary",
                                                                    collapsible = TRUE, collapsed = FALSE,
                                                                    conditionalPanel(condition = "output.done === 'true'",
                                                                                     tags$h3('Results Plot')
                                                                    ),
                                                                    plotOutput(outputId = "physSpacePlot")
                                                                ),
                                                                box(width = 12,solidHeader = TRUE,
                                                                    status = "primary",
                                                                    collapsible = TRUE, collapsed = FALSE, 
                                                                    conditionalPanel(condition = "output.done === 'true'",
                                                                                     tags$h3('Download'),
                                                                                     downloadButton("downloadPhysSpaceMap", 
                                                                                                    "Download results table (Physio-Map)"),
                                                                                     downloadButton("downloadPhysioHeatMap", 
                                                                                                    "Download plot (PDF)")
                                                                    )
                                                                )
                                                ),
                                                box(width = 4,solidHeader = TRUE,
                                                    status = "primary",
                                                    collapsible = TRUE, collapsed = FALSE, 
                                                    conditionalPanel(condition = "output.done === 'true'",
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
                                                                     )
                                                    )
                                                )
                                                )
                                                
                                                )
                       )
                                  ),
                       
                       tabPanel("TUTORIAL", value = "tutorial",
                                fluidRow(
                                  shiny::HTML("<br><br><center> 
                                              <h1>Tutorial</h1> 
                                              </center>
                                              <br>
                                              <br>"),
                                  style = "height:250px;"),
                                fluidRow(
                                  column(12,
                                         h6("PlantPhysioSpace is a shiny webapp, built upon 
                                            the R packages PhysioSpaceMethods and 
                                            PlantPhysioSpace 
                                            (git.rwth-aachen.de/jrc-combine), 
                                            for in-depth analysis 
                                            of plant response to different types of 
                                            stress.", style="text-align: justify;"),
                                         h6("Before jumping into some examples of how to use 
                                            this webapp, let's have a tour of this app's
                                            dashboard:", style="text-align: justify;"),
                                         div(
                                           tags$img(src = "DashScreenWithSteps.png", 
                                                    width = "100%")
                                         ),
                                         h6("And here's a run down of steps marked in 
                                            the figure above:", style="text-align: justify;"),
                                         h6("Step 0: PlantPhysioSpace is run using its 
                                            'dashboard', which can be found under the 
                                            'DASHBOARD' tab. If you are somewhere else in
                                            the app, such as home or about pages, you can
                                            go to dashboard by clicking 'DASHBOARD' in 
                                            the navbar.", style="text-align: justify;"),
                                         h6("Step 1: In first step, you have to choose a
                                            reference for you analysis. This reference
                                            space is used in mapping and deciphering
                                            your input data.
                                            At the moment, there are six references
                                            available for users: three references based
                                            on Arabidopsis thaliana (with names starting
                                            with 'AT'), one Glycine max 
                                            ('GM_Stress_Space'), one Oryza sativa space
                                            ('OS_Stress_Space'), and one 
                                            Triticum aestivum space ('TA_Stress_Space').
                                            Remember that your data and the reference
                                            space doesn't have to be from the same 
                                            species, since PlantPhysioSpace can map 
                                            between different species.
                                            For more details about how each reference
                                            space is made, you can check 
                                            PlantPhysioSpace R package documentation.", 
                                            style="text-align: justify;"),
                                         h6("Step 2 & 3: In this step, you have to choose 
                                            what kind of input data you intend to use:
                                            in case you have a list of up- and 
                                            down-regulated genes, 'Gene-list' is the 
                                            option for you. After choosing 'Gene-list', 
                                            two input fields appear, one for up-regulated 
                                            and the other for down-regulated genes, 
                                            that you can enter your gene lists in. 
                                            Remember that the PlantPhysioSpace expects 
                                            'ENTREZ IDs of Arabidopsis thalian' genes as 
                                            input. So if you have any other type of 
                                            identifier for your gene list, you need to 
                                            convert them first. 
                                            If you have genome-wide fold-change values 
                                            (or any other 'relative' value comparing 
                                            samples to their corresponding controls), 
                                            you can choose '(RELATIVE)Expression-Matrix'.
                                            In this case, you need to upload a csv file
                                            of fold-changes, with genes in rows and
                                            samples in columns, and 
                                            'Arabidopsis thalian ENTREZ IDs' in row names 
                                            and sample names as column names. A 
                                            screenshot of an example acceptable csv file
                                            follows:", 
                                            style="text-align: justify;"),
                                         div(
                                           tags$img(src = "InputMatrixSample.png", 
                                                    width = "100%")
                                         ),
                                         h6("Step 4: Hit 'COMPUTE' to start the analysis.", 
                                            style="text-align: justify;"),
                                         h6("Step 5, 6 & 7: After the calculations are
                                            finished, the results will be plotted in the
                                            middle of the dashboard, in the 
                                            'Results Plot' section, as a heatmap.
                                            The plot can be modified by the controls in 
                                            the 'Plot Setting' section. And also the 
                                            result (similarity score values and/or the 
                                            plot) can be downloaded using the buttons in 
                                            the 'Download' section.", 
                                            style="text-align: justify;")
                                         )
                                         )
                                         ),
                       
                       tabPanel("ABOUT", value = "about",
                                
                                fluidRow(
                                  shiny::HTML("<br><br><center> 
                                              <h1>About Plant<span style='color: #00cc00;'>.</span>PhysioSpace</h1> 
                                              <h4>What's behind the data</h4>
                                              </center>
                                              <br>
                                              <br>"),
                                  style = "height:250px;"),
                                fluidRow(
                                  column(12,
                                         h6("For achieving adequate robustness, our 
                                            method requires extensive training data. We curated more than 4000 plant 
                                            stress response gene expression samples from GEO and SRA, and used them in 
                                            our 'space generation', i.e. model training.", style="text-align: justify;"),
                                         
                                         h6("Samples corresponding to each species 
                                            are normalized in bulk to remove the batch
                                            effect. We used robust multi‐array average or RMA (Irizarry et al., 2003) 
                                            for normalizing microarray RAW data files and a pipeline consisting of Fastq-dump, 
                                            Trimmomatic (Bolger et al., 2014), Star aligner (Dobin et al., 2013) and 
                                            featureCounts (Liao et al., 2014) to derive counts from SRA records.", style="text-align: justify;"),
                                         
                                         h6("Each sample is annotated with a label 
                                            from a stress set. In this study, 
                                            all samples are divided into Aluminum, Magnesium, Biotic, Cold, Drought, FarRed,
                                            FeDeficiency, Genotoxic, Heat, Herbicide, Hormone, Hypoxia, Light, LowPH, 
                                            Metabolic, Mutant, Nitrogen, Osmotic, Radiation, Salt, Submergence, UV and 
                                            Wounding stress groups. For samples which underwent more than one stress, 
                                            new labels were generated by concatenating existing labels from the stress set. 
                                            For example, ‘Biotic.Drought’ designates a sample which sustained both Biotic 
                                            and Drought stresses.", style="text-align: justify;"),
                                         
                                         h6("Any stress from the aforementioned 
                                            stress set is extensively trained and can be
                                            detected and quantitized.", style="text-align: justify;")
                                         )
                                         ),
                                fluidRow(
                                  div(align = "center",
                                      tags$span(h4("What's behind the method"), 
                                                style = "font-weight:bold"
                                      ))
                                ),
                                fluidRow(
                                  column(12,
                                         h6("The PhysioSpace method is a 
                                            supervised dimension reduction method,
                                            which aims to extract 'relevant 
                                            physiological information' from big 
                                            data sets and store it in a 
                                            mathematical matrix called 'space' 
                                            (Lenz et al, 2013). The method can be 
                                            divided into two main steps: 
                                            Space generation and Physio-mapping."),
                                         h6("On this webapp, the model training, 
                                            i.e. the 'Space generation', has already 
                                            been done and models of stress response 
                                            for different plants are ready 
                                            to use. Users can map their own data 
                                            onto the pre-calculated models. The 
                                            webapp quantifies the stress response 
                                            of the input data using the 
                                            'Physio-mapping' algorithm."),
                                         h6("More information about the algorithm
                                            can be found in the paper.")
                                         )
                                         ),
                                # TEAM BIO
                                fluidRow(
                                  column(3),
                                  column(6,
                                         shiny::HTML("<br><br><center> <h5>About the team</h5> </center><br>")
                                         # shiny::HTML("<h6></h6>")
                                  ),
                                  column(3)
                                ),
                                
                                fluidRow(
                                  
                                  style = "height:50px;"),
                                
                                fluidRow(
                                  column(3),
                                  column(2,
                                         div(class="panel panel-default",
                                             div(class="panel-body",  width = "600px", 
                                                 align = "center",
                                                 div(
                                                   tags$img(src = "asis.jpg", 
                                                            width = "100px", height = "100px")
                                                 ),
                                                 div(
                                                   tags$h5("Dr. Asis Hallab"),
                                                   tags$p(tags$i("Design and development"))
                                                 ),
                                                 div(
                                                   ""
                                                 )
                                             )
                                         )
                                  ),
                                  column(2),
                                  column(2,
                                         div(class="panel panel-default",
                                             div(class="panel-body",  width = "600px", 
                                                 align = "center",
                                                 div(
                                                   tags$img(src = "ali.png", 
                                                            width = "100px", height = "100px")
                                                 ),
                                                 div(
                                                   tags$h5("Ali H. Esfahani"),
                                                   tags$p(tags$i("Overall research and design"))
                                                 ),
                                                 div(
                                                   "ali.hadizadeh at rwth dash aachen dot de"
                                                 )
                                             )
                                         )
                                  ),
                                  column(3)
                                  
                                ),
                                # Terms of use
                                fluidRow(
                                  shiny::HTML("<hr><br><br><center> 
                                              <h1 id='terms'>Terms and Conditions of Use</h1> 
                                              </center>
                                              <br>
                                              <br>"),
                                  style = "height:250px;"),
                                
                                fluidRow(
                                  div(align = "center",
                                      tags$span(h4("Terms"), 
                                                style = "font-weight:bold"
                                      ))
                                ),
                                fluidRow(
                                  column(12,
                                         h6("By accessing this web site, you are 
                                            agreeing to be bound by these web site 
                                            Terms and Conditions of Use, all 
                                            applicable laws and regulations, 
                                            and agree that you are responsible 
                                            for compliance with any applicable 
                                            local laws. If you do not agree with 
                                            any of these terms, you are prohibited 
                                            from using or accessing this site. 
                                            The materials contained in this web site 
                                            are protected by applicable copyright 
                                            and trade mark law.", style="text-align: justify;")
                                         )
                                         ),
                                
                                fluidRow(
                                  div(align = "center",
                                      tags$span(h4("Use License"), 
                                                style = "font-weight:bold"
                                      ))
                                ),
                                fluidRow(
                                  column(12,
                                         h6("Permission is granted to temporarily 
                                            download one copy of the materials (information or software) on 
                                            PlantPhysioSpace's web site for personal, non-commercial transitory viewing only. 
                                            This is the grant of a license, not a transfer of title, 
                                            and under this license you may not: modify or copy the materials, use the materials 
                                            for any commercial purpose, or for any public display (commercial or non-commercial),
                                            attempt to decompile or reverse engineer any software contained on 
                                            PlantPhysioSpace's web site, remove any copyright or other proprietary notations 
                                            from the materials, or transfer the materials to another person or 'mirror' 
                                            the materials on any other server. This license shall automatically terminate 
                                            if you violate any of these restrictions 
                                            and may be terminated by PlantPhysioSpace 
                                            at any time. Upon terminating your 
                                            viewing of these materials or upon the 
                                            termination of this license, you must 
                                            destroy any downloaded materials in your 
                                            possession whether in electronic or 
                                            printed format.", style="text-align: justify;")
                                         )
                                         ),
                                
                                fluidRow(
                                  div(align = "center",
                                      tags$span(h4("Disclaimer"), 
                                                style = "font-weight:bold"
                                      ))
                                ),
                                fluidRow(
                                  column(12,
                                         h6("The materials on PlantPhysioSpace's 
                                            web site are provided 'as is'. PlantPhysioSpace makes no warranties, 
                                            expressed or implied, and hereby disclaims and negates all other warranties, 
                                            including without limitation, implied warranties or conditions of 
                                            merchantability, fitness for a particular purpose, 
                                            or non-infringement of intellectual property or other violation of rights. 
                                            Further, PlantPhysioSpace does not warrant or make any representations 
                                            concerning the accuracy, likely results, or reliability of the use of the 
                                            materials on its Internet web site or otherwise relating to such materials or 
                                            on any sites linked to this site.", style="text-align: justify;")
                                         )
                                         ),
                                
                                fluidRow(
                                  div(align = "center",
                                      tags$span(h4("Limitations"), 
                                                style = "font-weight:bold"
                                      ))
                                ),
                                fluidRow(
                                  column(12,
                                         h6("In no event shall PlantPhysioSpace 
                                            or its suppliers be liable for any 
                                            damages (including, without limitation, 
                                            damages for loss of data or profit, 
                                            or due to business interruption,) 
                                            arising out of the use or inability to 
                                            use the materials on PlantPhysioSpace's 
                                            Internet site, even if PlantPhysioSpace 
                                            or a PlantPhysioSpace authorized 
                                            representative has been notified 
                                            orally or in writing of the possibility 
                                            of such damage. Because some jurisdictions 
                                            do not allow limitations on 
                                            implied warranties, or limitations of 
                                            liability for consequential or 
                                            incidental damages, these limitations 
                                            may not apply to you.", style="text-align: justify;")
                                         )
                                         ),
                                
                                fluidRow(
                                  div(align = "center",
                                      tags$span(h4("Revisions and Errata"), 
                                                style = "font-weight:bold"
                                      ))
                                ),
                                fluidRow(
                                  column(12,
                                         h6("The materials appearing on 
                                            PlantPhysioSpace's web site 
                                            could include technical, 
                                            typographical, or 
                                            photographic errors. PlantPhysioSpace 
                                            does not warrant that any of the 
                                            materials on its web site are accurate, 
                                            complete, or current. PlantPhysioSpace 
                                            may make changes to the materials 
                                            contained on its web site at any time 
                                            without notice. PlantPhysioSpace 
                                            does not, however, make any commitment 
                                            to update the materials.", style="text-align: justify;")
                                         )
                                         ),
                                
                                fluidRow(
                                  div(align = "center",
                                      tags$span(h4("Links"), 
                                                style = "font-weight:bold"
                                      ))
                                ),
                                fluidRow(
                                  column(12,
                                         h6("PlantPhysioSpace has not reviewed 
                                            all of the sites linked to its Internet 
                                            web site and is not responsible for the 
                                            contents of any such linked site. 
                                            The inclusion of any link does not 
                                            imply endorsement by PlantPhysioSpace 
                                            of the site. Use of any such linked web 
                                            site is at the user's own risk.", style="text-align: justify;")
                                         )
                                         ),
                                
                                fluidRow(
                                  div(align = "center",
                                      tags$span(h4("Site Terms of Use Modifications"), 
                                                style = "font-weight:bold"
                                      ))
                                ),
                                fluidRow(
                                  column(12,
                                         h6("PlantPhysioSpace may revise these 
                                            terms of use for its web site at any 
                                            time without notice. By using this web 
                                            site you are agreeing to be bound by 
                                            the then current version of these Terms 
                                            and Conditions of Use.", style="text-align: justify;")
                                         )
                                         ),
                                
                                fluidRow(
                                  div(align = "center",
                                      tags$span(h4("Governing Law"), 
                                                style = "font-weight:bold"
                                      ))
                                ),
                                fluidRow(
                                  column(12,
                                         h6("Any claim relating to PlantPhysioSpace's 
                                            web site shall be governed by the laws 
                                            of Germany without regard to its 
                                            conflict of law provisions.", style="text-align: justify;")
                                         )
                                         ),
                                
                                fluidRow(style = "height:150px;")
                                         )  # Closes About tab
                       
                                )
              
)

# Define the server:
server <- shinyServer(function(input, output, session) {
  
  # Navbar ------------------------------------------------------------------
  shinyjs::addClass(id = "navBar", class = "navbar-right")
  
  # Intro JS ----------------------------------------------------------------
  observeEvent(input$help,
               introjs(session, options = list("nextLabel"="Next",
                                               "prevLabel"="Back",
                                               "skipLabel"="Exit"))
  )  
  
  # DT Options --------------------------------------------------------------
  options(DT.options = list( lengthMenu = c(10, 20),
                             dom = 'tl'
  ))  # table and lengthMenu options
  
  
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
      counts.df[,-1,drop=FALSE]
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
        physMap <- PhysioSpaceMethods::calculatePhysioMap(
          InputData = calc.phys.map.inp(),
          Space = get(input$physSpace)
          # NumbrOfCores = getOption("mc.cores", 1) #Shinyapp.io has a problem with parallel!
        )
        if(ncol(physMap)==1) colnames(physMap) <- "User_input" #To fix a plotting bug when user input gene lists
        physMap
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
      
      PhysioScores = phys.map()[pm.rows, pm.cols, drop = FALSE]
      if(any(is.infinite(range(PhysioScores)))){
        Mx <- max(PhysioScores[is.finite(PhysioScores)])
        Mn <- min(PhysioScores[is.finite(PhysioScores)])
        PhysioScores[PhysioScores > Mx] <- 2*Mx
        PhysioScores[PhysioScores < Mn] <- -2*abs(Mn)
      }
      
      p.h <- PhysioHeatmap(
        PhysioResults = PhysioScores,
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
      dev.copy2pdf(file = "./plot.pdf")
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
      file.copy("./plot.pdf", file)
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
  
})

# Start the shiny server
shinyApp(ui = ui, server = server)
