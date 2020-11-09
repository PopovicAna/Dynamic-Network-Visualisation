library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)
library(shinyWidgets)
library(shinyBS)
library(formattable)
library(plotly)
library(colourpicker)
library(dendextend)
library(tuple)
library(leaflet)
library(rgdal)
library(DT)
library(openxlsx)
library(tidyverse)
library(ggplot2)
library(ggcorrplot)
library(pROC)
library(ggh4x)
library(lubridate)
library(igraph)
library(DescTools)


# Reading in the data and other code: ----------------------------------
source("Files/Prioritisation-of-analytical-techniques_1.R")

# Importing Australian postcode shape data
PC <- readOGR(dsn = "Data/PC_Shapes/V2/POA_2016_AUST_V2.shp")


# UI ----------------------------------------------------------------------
ui <- dashboardPagePlus(
  skin = "blue",
  dashboardHeaderPlus(
    title = "DNV"
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem(
        "Data",
        tabName = "Data",
        icon = icon("database"),
        menuItem(
          "View Data", 
          tabName = "ViewData"),
        menuItem(
          "Target Variables (GCMS Only)",
          tabName = "TarVar"),
        menuItem(
          "Optimise Measures", 
          tabName = "Optimise")
      ),
      menuItem(
        "Networks",
        tabName = "dashboard",
        icon = icon("dashboard"),
        menuItem(
          "Plots", 
          tabName = "NP"),
        menuItem(
          "Summary", 
          tabName = "Summary")
      ),
      menuItem(
        "Analysis", 
        tabName = "Analysis",
        icon = icon("chart-area"),
        menuItem(
          "Relational", 
          tabName = "Rel"),
        menuItem(
          "Temporal", 
          tabName = "Temp"),
        menuItem(
          "Spatial", 
          tabName = "Spa"),
        menuItem(
          "Quantitative", 
          tabName = "Quant")
      )
    )
  ),
  dashboardBody(
    tags$style(".small-box.bg-light-blue {-webkit-box-shadow: none; -moz-box-shadow: none;box-shadow: none;}"),
    tabItems(
      tabItem(
        tabName = "ViewData",
        fluidRow(
          column(
            width = 2,
            fluidRow(
              box(
                width = 12,
                pickerInput(
                  inputId = "sheet",
                  label = "View Sheets:",
                  choices = c("Lookup","GCMS","IRMS","CE"),
                  selected = "Lookup"),
                tags$hr(),
                radioButtons("disp", "Display",
                             choices = c(Head = "head",
                                         All = "all"),
                             selected = "head"),
                tags$hr(),
                downloadButton(
                  "downloadData",
                  "Download example data"
                )
              )
            )
          ),
          column(
            width = 10,
            box(
              width = 12,
              title = "Example Data",
              formattableOutput("contents")
            )
          )
        )
      ),
      tabItem(
        tabName = "TarVar",
        fluidRow(
          column(
            width = 6,
            fluidRow(
              box(
                status = "primary",
                solidHeader = TRUE,
                title = "Choosing optimal target varaibles",
                tags$div(
                  tags$p(
                    paste("Not all variables contribute equally to the GCMS specimen profiles and should be removed if they are deemed to be redundant.",
                          "For this reason, the target variable criteria needs to be applied to remove any redundant variables.",  
                          "The criteria that need to be satisfied when choosing target variables includes: ")
                  ),
                  tags$ol(
                    tags$li("they should be uncorrelated to each other;"),
                    tags$li("they must be present in most specimens at a sufficient concentration;"),
                    tags$li("they must have a small intra-variability and large inter-variability in order to be able to group specimens possessing similar variables."), 
                  ),
                  style = "font-size: 14px; text-align: justify; margin-left:20px; margin-right:20px"
                ),
                height = "350px"
              ),
              box(
                title = "Rho Spearman correlation between varaibles",
                plotOutput("TV3", height = "290px"),
                bsPopover("TV3", "Consider the following:",
                          content = paste0("Optimal variables should not be strongly correlated to each other (i.e. should have an absolute value less than 0.7). ",
                                           "Correlations which are not statistically significant (i.e. p-value less than 0.05) have been excluded from the plot."), 
                          trigger = "hover")
              )
            ),
            box(
              title = "Presence of variables in specimens",
              width = "100%",
              plotOutput("TV1", height = "280px"),
              bsPopover("TV1", "Consider the following:", 
                        content = "Variables to be used for further analysis should be present in most specimens.", 
                        trigger = "hover")
            )
          ),
          column(width = 6,
                 box(
                   title = "Intra- and inter-variability of variables",
                   width = "100%",
                   plotOutput("TV2", height = "660px"),
                   bsPopover("TV2", "Consider the following:", 
                             content = "Optimal variables should have a low intra-variability and a high inter-variability.",
                             trigger = "hover")
                 )
          )
        ),
        fluidRow(
          box(
            width = 12,
            background = "light-blue",
            column(
              width = 10,
              checkboxGroupButtons(
                inputId = "TVs",
                label = "Choice of optimal target varaibles:",
                choices = c(paste0("V0",1:9),"V10"),
                selected = c(paste0("V0",1:9),"V10"),
                justified = TRUE,
                checkIcon = list(
                  yes = icon("ok", lib = "glyphicon"))
              )
            ),
            column(
              width = 2,
              actionBttn(
                inputId = "CalcOM",
                label = "Calculate Optimal Measures",
                style = "pill",
                color = "default"
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "Optimise",
        fluidRow(
          valueBoxOutput(
            "OPT_AT",
            width = 3
          ),
          valueBoxOutput(
            "OPT_PT",
            width = 3
          ) %>% withSpinner(color="#3C8DBC", proxy.height = "160px"),
          valueBoxOutput(
            "OPT_CM",
            width = 3
          ),
          valueBoxOutput(
            "OPT_PR",
            width = 3
          )
        ),
        fluidRow(
          column(
            width = 9,
            box(
              width = 12,
              plotOutput("THV_FPFN")
            )
          ),
          column(
            width = 3,
            box(
              title = "Choose a False Positive Rate: ",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              numericInputIcon(
                inputId = "FPR",
                label = NULL,
                value = 2.5,
                icon = icon("percentage")
              )
            ),
            box(
              width = 12,
              uiOutput(
                "THV"
              ),
              uiOutput(
                "FNR"
              )
            ),
            box(
              background = "light-blue",
              width = 12,
              actionButton(
                width = "100%",
                inputId = "Set_FPR",
                label = "Go to Plots",
                icon = icon("arrow-circle-right"),
                style = "font-size:16px; color: #fff; background-color: #3C8DBC; border-color: #3C8DBC"
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "NP",
        fluidRow(
          column(
            width = 2,
            
            box(width = "100%",
                title = "Network Properties",
                solidHeader = TRUE,
                status = "primary",
                fluidRow(
                  style = "margin-top:20px; margin-bottom:20px; margin-left:5px; margin-right:5px",
                  dateRangeInput(
                    "dates",
                    label="Overall date range",
                    start="2015-01-01",
                    end="2019-12-31",
                    width="100%")
                ),
                fluidRow(
                  style = "margin-top:20px; margin-bottom:20px; margin-left:5px; margin-right:5px",
                  pickerInput(
                    inputId = "Region",
                    label = "Region",
                    choices =
                      c("ACT","NSW","NT","QLD","SA","TAS","VIC","WA"),
                    selected =
                      c("ACT","NSW","NT","QLD","SA","TAS","VIC","WA"),
                    options = list(
                      `actions-box` = TRUE),
                    multiple = T,
                    width="100%")
                ),
                tags$hr(),
                fluidRow(
                  style = "margin-top:20px; margin-bottom:20px; margin-left:5px; margin-right:5px",
                  actionButton(
                    inputId = "do",
                    label = "Create Network",
                    icon = icon("project-diagram"),
                    width="100%",
                    style = "font-size:16px; color: #fff; background-color: #3C8DBC; border-color: #3C8DBC")
                ),
                bsPopover(
                  id = "do",
                  title = "Edit the parameters",
                  content = paste("In the network properties section choose a desired optimal date range and desired regions."),
                  trigger = "hover"
                )
            )
          ),
          column(
            width = 10,
            boxPlus(
              width = "100%",
              title = "Newtork Plots",
              closable = FALSE, 
              solidHeader = FALSE, 
              collapsible = TRUE,
              enable_sidebar = TRUE,
              sidebar_width = 25,
              sidebar_background = "#ffffff",
              sidebar_start_open = FALSE,
              sidebar_content = tagList(
                tabBox(
                  width = 12,
                  tabPanel(
                    title = "",
                    icon = icon("info"),
                    fluidRow(
                      style = "color: #000; margin-left:5px; margin-right:5px",
                      checkboxGroupButtons(
                        inputId = "AnTech",
                        label = h3("Overlay cheimcal data"),
                        choices = c("IRMS", "Chiral"),
                        status = "danger",
                        individual = T,
                        size = "normal",
                        justified = F,
                        selected = NULL
                      )
                    )
                  ),
                  tabPanel(
                    title = "",
                    icon = icon("sitemap"),                            
                    fluidRow(
                      h3("Layout"),
                      style = "color: #000; margin-left:0px;margin-right:0px",
                      selectizeInput(
                        inputId = "netlay",
                        label = "Algorithms",
                        choices = c("Fruchterman-Reingold",
                                    "Reingold-Tilford",
                                    "Graphopt",
                                    "Kamada-Kawai"))
                    ),
                    fluidRow(
                      style = "color: #000; margin-top:-20px",
                      column(
                        width = 12,
                        tags$h5(tags$strong("Keep seizure leaves")),
                        switchInput(
                          inputId = "leaf",
                          label = icon("leaf"),
                          onLabel = "Yes",
                          offLabel = "No",
                          size = "large",
                          value = T,
                          width="100%"))
                    )
                  ),
                  tabPanel(
                    title = "",
                    icon = icon("palette"),                            
                    fluidRow(
                      h3("Apperance"),
                      style = "color: #000;",
                      column(
                        width = 6,
                        colourInput(
                          inputId = "CCcol",
                          label = "CC Colour",
                          value = "#000000",
                          showColour = "background"
                        )),
                      column(
                        width = 6,
                        colourInput(
                          inputId = "Groupcol",
                          label = "Seizure Colour",
                          value = "#D3D3D3",
                          showColour = "background"
                        ))
                    ),
                    fluidRow(
                      style = "color: #000;margin-left:0px;margin-right:0px",
                      sliderInput(
                        inputId = "mrkrsize",
                        label = "Marker Size",
                        min = 1,
                        max = 20,
                        value = 10,
                        ticks = F,
                        width="100%")
                    )
                  )
                )
              ),
              fluidRow(
                fluidRow(
                  style = "margin-left:40px;margin-right:40px",
                  plotlyOutput("net") %>% withSpinner(color="#3C8DBC", hide.ui = FALSE)
                ),
                fluidRow(
                  style = "margin-left:40px;margin-right:40px",
                  uiOutput("date_selector")
                )
              )
            )
          )
        ),
        fluidRow(
          box(
            width = 3,
            background = "light-blue",
            solidHeader = TRUE,
            fluidRow(
              valueBox(
                tags$p("Relational Analysis", style = "font-size: 70%;"), 
                tags$strong("Identification of clusters within networks"), 
                color = "light-blue",
                icon = icon("project-diagram"),
                width = 12
              )
            ),
            fluidRow(
              actionBttn(
                inputId = "Go2Rel",
                label = "Go to relational analysis",
                icon = icon("arrow-circle-right"),
                size = "md",
                block = TRUE,
                style = "simple"
              )
              
            ),
            style = 'padding-bottom: 0px;'
          ),
          box(
            width = 3,
            background = "light-blue",
            solidHeader = TRUE,
            fluidRow(
              valueBox(
                tags$p("Temporal Analysis", style = "font-size: 70%;"),
                tags$strong("Monitor success of policing strategies"),
                color = "light-blue",
                icon = icon("calendar"),
                width = 12
              )
            ),
            fluidRow(
              actionBttn(
                inputId = "Go2Temp",
                label = "Go to temporal analysis",
                icon = icon("arrow-circle-right"),
                size = "md",
                block = TRUE,
                style = "simple"
              )
              
            ),
            style = 'padding-bottom: 0px;'
          ),
          box(
            width = 3,
            background = "light-blue",
            solidHeader = TRUE,
            fluidRow(
              valueBox(
                tags$p("Spatial Analysis", style = "font-size: 70%;"),
                tags$strong("Suggest collaboration between jurisdictions"), 
                color = "light-blue", 
                icon = icon("map-marker-alt"),
                width = 12
              )
            ),
            fluidRow(
              actionBttn(
                inputId = "Go2Spa",
                label = "Go to spatial analysis",
                icon = icon("arrow-circle-right"),
                size = "md",
                block = TRUE,
                style = "simple"
              )
              
            ),
            style = 'padding-bottom: 0px;'
          ),
          box(
            width = 3,
            background = "light-blue",
            solidHeader = TRUE,
            fluidRow(
              valueBox(
                tags$p("Quantitative Analysis", style = "font-size: 70%;"), 
                tags$strong("Inform policy and resource allocation"),
                color = "light-blue",
                icon = icon("chart-line"),
                width = 12
              )
            ),
            fluidRow(
              actionBttn(
                inputId = "Go2Quant",
                label = "Go to quantitative analysis",
                icon = icon("arrow-circle-right"),
                size = "md",
                block = TRUE,
                style = "simple"
              )
              
            ),
            style = 'padding-bottom: 0px;'
          )
          
        )
      ),
      tabItem(tabName = "Summary",
              fluidRow(
                column(
                  width = 6,
                  h2("Summary")
                ),
                column(
                  width = 3,
                  offset = 3,
                  actionButton(
                    inputId = "back2net",
                    label = "Back to network",
                    icon = icon("arrow-circle-left"),
                    width = "100%",
                    style = "font-size:16px; color: #fff; background-color: #3C8DBC; border-color: #3C8DBC")
                )
              ),
              fluidRow(
                column(
                  width = 2,
                  offset = .5,
                  textInput("Cluster",
                            "Cluster:",
                            "")
                )
              ),
              fluidRow(
                tabBox(
                  width = 12,
                  tabPanel("Lookup Table",
                           DTOutput("CLT")),
                  tabPanel("Variable Table", 
                           DTOutput("CVT")),
                  tabPanel("Correlation Matrix", 
                           DTOutput("CCM"))
                )
              )
      ),
      tabItem(
        tabName = "Rel",
        fluidRow(
          column(
            width = 6,
            h2("Relational Analysis")
          ),
          column(
            width = 3,
            offset = 3,
            actionButton(
              inputId = "back2netRel",
              label = "Back to network",
              icon = icon("arrow-circle-left"),
              width = "100%",
              style = "font-size:16px; color: #fff; background-color: #3C8DBC; border-color: #3C8DBC"
            )
          )
        ),
        fluidRow(
          valueBoxOutput("NetVal1", width = 2),
          valueBoxOutput("NetVal2", width = 2),
          valueBoxOutput("NetVal3", width = 2),
          valueBoxOutput("NetVal4", width = 2),
          valueBoxOutput("NetVal5", width = 2),
          valueBoxOutput("NetVal6", width = 2)
        ),
        fluidRow(
          column(
            width = 2, 
            offset = .5,
            numericInput(
              "No_LC",
              "Number of components to view:",
              value = 6,
              min = 1,
              max = 20,
              step = 1
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            plotlyOutput(
              "LCs"
            )
          )
        ),
        fluidRow(
          style = "margin-left:40px;margin-right:40px",
          uiOutput("date_selectorRel")
        )
      ),
      tabItem(
        tabName = "Temp",
        fluidRow(
          column(
            width = 6,
            h2("Temporal Analysis")
          ),
          column(
            width = 3,
            offset = 3,
            actionButton(
              inputId = "back2netTemp",
              label = "Back to network",
              icon = icon("arrow-circle-left"),
              width = "100%",
              style = "font-size:16px; color: #fff; background-color: #3C8DBC; border-color: #3C8DBC"
            )
          )
        ),
        fluidRow(
          box(
            width = 6,
            formattableOutput("TD")
          ),
          # box(
          #   width = 4,
          #   title = "Venn"
          # ),
          box(
            width = 6,
            plotOutput("CC_Over_Time", height = "270px"),
          )
        ),
        fluidRow(
          box(
            width = 9,
            plotOutput("Quarters")
          ),
          column(
            width = 3,
            valueBoxOutput(
              width = 12,
              "CC_Mean_Day"
            ),
            valueBoxOutput(
              width = 12,
              "CC_Median_Day"
            )
          )
        )
      ),
      tabItem(
        tabName = "Spa",
        fluidRow(
          column(
            width = 6,
            h2("Spatial Analysis")
          ),
          column(
            width = 3,
            offset = 3,
            actionButton(
              inputId = "back2netSpa",
              label = "Back to network",
              icon = icon("arrow-circle-left"),
              width = "100%",
              style = "font-size:16px; color: #fff; background-color: #3C8DBC; border-color: #3C8DBC"
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            splitLayout(
              valueBoxOutput("No_Reg_VB1", width = 12),
              valueBoxOutput("No_Reg_VB2", width = 12),
              valueBoxOutput("No_Reg_VB3", width = 12),
              valueBoxOutput("No_Reg_VB4", width = 12),
              valueBoxOutput("No_Reg_VB5", width = 12),
              valueBoxOutput("No_Reg_VB6", width = 12),
              valueBoxOutput("No_Reg_VB7", width = 12),
              valueBoxOutput("No_Reg_VB8", width = 12)
            )
          )
        ),
        fluidRow(
          box(
            width = 9,
            leafletOutput("Map") %>% withSpinner(color="#3C8DBC"),
            actionButton(
              inputId = "do_map",
              label = "Generate Map",
              icon = icon("map-marker-alt"),
              width = "100%",
              style = "font-size:16px; color: #fff; background-color: #3C8DBC; border-color: #3C8DBC")
          ),
          box(
            width = 3,
            DTOutput("MapTab")
          )
        ),
        fluidRow(
          style = "margin-left:40px;margin-right:40px",
          uiOutput("date_selectorSpa")
        )
      ),
      tabItem(
        tabName = "Quant",
        fluidRow(
          column(
            width = 6,
            h2("Quantitative Analysis")
          ),
          column(
            width = 3,
            offset = 3,
            actionButton(
              inputId = "back2netQuant",
              label = "Back to network",
              icon = icon("arrow-circle-left"),
              width = "100%",
              style = "font-size:16px; color: #fff; background-color: #3C8DBC; border-color: #3C8DBC"
            )
          )
        ),
        fluidRow(
          
          box(
            width = 6,
            plotOutput("Purity", height = "700px") %>% withSpinner(color="#3C8DBC")
          ),
          box(
            width = 6,
            plotOutput("Precursor", height = "700px") %>% withSpinner(color="#3C8DBC")
          )
        )
      )
    )
  )
)


# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  # VIEW DATA ---------------------------------------------------------------
  output$downloadData <- downloadHandler(
    filename = "Example_Data.xlsx",
    content = function(file){
      write.xlsx(list(Lookup = Lookup,
                      GCMS = GCMS,
                      IRMS = IRMS, 
                      CE = CE), 
                 file)
    }
  )
  
  output$contents <- renderFormattable({
    req(input$disp)
    if(input$disp == "head") {
      head(formattable(get(input$sheet)), n = 20)
    }
    else {
      formattable(get(input$sheet))
    }
  })
  
  # CHOICE OF TARGET VARIBLES (GCMS ONLY)--------------------------------
  
  # Plots to help determine whcih variables are redundant and which to keep
  output$TV1 <- renderPlot(Bar)
  output$TV2 <- renderPlot(psplit)
  output$TV3 <- renderPlot(corrplot)
  
  # Removing variables from GCMS which are deemed to be redundant
  GCMS_TV <- eventReactive(input$CalcOM,{
    GCMS_T <<- GCMS %>% select(input$TVs)
  })
  
  observeEvent(input$CalcOM,{
    req(GCMS_TV())
    #source("Files/Prioritisation-of-analytical-techniques_2.R")  #Run with more memory
    source("Files/Prioritisation-of-analytical-techniques_2_V2.R")
  })
  
  # OPTIMISE MEASURES ---------------------------------------------------
  
  # Optimal measures value boxes
  output$OPT_AT <- renderValueBox({
    req(input$CalcOM)
    valueBox(
      "Optimal Analytical Technique",
      value = "GCMS",
      color = "light-blue",
      icon = icon("database")
    )
  })
  
  output$OPT_PT <- renderValueBox({
    req(input$CalcOM)
    req(exists("OPT_GCMS_PT")==TRUE)
    valueBox(
      "Optimal Pre-treatment",
      value = OPT_GCMS_PT,
      color = "light-blue",
      icon = icon("ruler")
    )
  })
  
  output$OPT_CM <- renderValueBox({
    req(input$CalcOM)
    req(exists("OPT_GCMS_CM")==TRUE)
    valueBox(
      "Optimal Comparison Metric",
      value = OPT_GCMS_CM,      
      color = "light-blue",
      icon = icon("ruler")
    )
  })
  
  output$OPT_PR <- renderValueBox({
    req(input$CalcOM)
    req(exists("OPT_GCMS_PR")==TRUE)
    valueBox(
      "Optimal Population Rule",
      value = OPT_GCMS_PR,
      color = "light-blue",
      icon = icon("ruler")
    )
  })
  
  # Defining TP, TN, FP and FN rates at each threshold values (THV - aka comparison metric score)
  CUTOFFS <- reactive({
    req(exists("OPT_GCMS_PT_CM_R"))
    data.frame(
      THV = RCM(OPT_GCMS_PT_CM_R)$thresholds,
      TPR = RCM(OPT_GCMS_PT_CM_R)$sensitivities,
      FPR = 1-RCM(OPT_GCMS_PT_CM_R)$specificities,
      FNR = 1-RCM(OPT_GCMS_PT_CM_R)$sensitivities,
      TNR = RCM(OPT_GCMS_PT_CM_R)$specificities)
  })
  
  CUTOFFS_2 <- reactive({
    CUTOFFS() %>% 
      select(THV,FNR,FPR) %>% 
      pivot_longer(FPR:FNR, names_to = "Rate")})
  
  # FP rate and FN rate vs THV (Comparison metric score)
  output$THV_FPFN <- renderPlot({
    req(input$CalcOM)
    ggplot(CUTOFFS_2(),aes(x = THV, y = value*100, group = Rate)) +
      geom_line(aes(linetype = Rate), size = 1.5) +
      scale_linetype_manual(values=c("dashed","solid")) +
      labs(title = "Use the lines to choose an appropriate Threshold Value",
           x = "Comparison Metric Score",
           y = "FP and FN rates (%)") + 
      theme_minimal()
  })
  
  # Setting an acceptable FPR to define whether specimen pairs are linked or not
  # Note: For the purpose of this research an acceptable FPR is 0.025 (i.e. 2.5%)
  output$THV <- renderUI({
    req(input$CalcOM)
    numericInputIcon(
      inputId = "THV",
      label = "Threshold Value: ",
      value = round(as.numeric(tail(subset(CUTOFFS(), FPR > (input$FPR)/100, select = THV), n = 1)),1),
      icon = icon("ruler-vertical")
    )
  })
  
  output$FNR <- renderUI({
    req(input$CalcOM)
    numericInputIcon(
      inputId = "FPR",
      label = "False Negative Rate: ",
      value = round((as.numeric(tail(subset(CUTOFFS(), FPR > (input$FPR)/100, select = FNR), n = 1)))*100,1),
      icon = icon("percentage")
    )
  })
  
  # NETWORK PLOTS -----------------------------------------------------------
  
  # Creating chemical classes based on the THV and making network plots
  G <- eventReactive(input$do,{
    source("Files/Analysing-illicit-drug-networks.R", local = T)
    
    g <- graph_from_data_frame(d = Links[,1:5],vertices = Nodes[,1:3], directed = F)
    g <- delete_edges(g, which(E(g)$onset<as.numeric(input$dates[1]) |   
                                 E(g)$terminus>as.numeric(input$dates[2])))
    g <- delete_vertices(g,degree(g)==0)
  })
  
  # Reactive slider for changing network plot date range
  output$date_selector = renderUI({
    sliderInput("slider",
                label="",
                min=as.Date(input$dates[1]),
                max=as.Date(input$dates[2]),
                value=as.Date(c(input$dates[1],input$dates[1]+365)),
                width="100%")
  })
  
  # Reactively removing and restoring seizure leaves (i.e. seizures/groups onlu connected to one CC)
  GT =  eventReactive(c(input$do,input$leaf,input$slider),{
    if(input$leaf==T){
      gt = delete.edges(G(), which(E(G())$onset<as.numeric(input$slider[1]) |
                                     E(G())$onset>as.numeric(input$slider[2])))
      gt = delete.vertices(simplify(gt), degree(gt)==0) #comment to avoid layout error
    } else {
      gt = delete.edges(G(), which(E(G())$onset<as.numeric(input$slider[1]) |
                                     E(G())$onset>as.numeric(input$slider[2])))
      gt = delete.vertices(gt,which(substr(names(V(gt)),1,2)!="CC" & degree(gt)==1))
      gt = delete.vertices(simplify(gt), degree(gt)==0) #comment to avoid layout error
    }
    g2 <<- gt
  })
  
  # Setting coordinates for the different network plot layout
  LN = eventReactive(input$do,{
    Ln <- as.data.frame(
      if(input$netlay=="Fruchterman-Reingold"){
        layout_with_fr(G(),grid="nogrid",
                       coords=norm_coords(
                         layout_with_fr(G(), grid = "nogrid"), 
                         xmin = -1, xmax = 1, ymin = -1, ymax = 1),  
                       niter=10,start.temp=0.05)
      } else if (input$netlay=="Reingold-Tilford"){
        layout_as_tree(G())
      } else if (input$netlay=="Graphopt"){
        layout_with_graphopt(G())
      } else if (input$netlay=="Kamada-Kawai"){
        layout_with_kk(G(),
                       coords=norm_coords(
                         layout_with_kk(G()), 
                         xmin = -1, xmax = 1, ymin = -1, ymax = 1))
      } else {
        return(NULL)
      }
    )
    rownames(Ln) <- names(V(G()))
    Ln <<- Ln
  })
  
  # Creating an edgelist to plot lines between nodes (i.e. links)
  es <- eventReactive(
    c(input$do,input$slider,input$leaf,input$AnTech,
      input$netlay,input$mrkrsize,input$Groupcol,input$CCcol),{
        as.data.frame(get.edgelist(GT()))
      })
  
  # Extracting relevant nodes to plot the network and allow for overlay of additional data
  RN <- eventReactive(
    c(input$do,input$slider,input$leaf,input$AnTech,
      input$netlay,input$mrkrsize,input$Groupcol,input$CCcol),{
        
        # Creating a reactive dataframe (RN) containing the nodes present in the current plot
        RN <- as.data.frame(LN()[rownames(LN()) %in% names(V(GT())),])
        RN$Region = Lookup$Region[match(rownames(RN),Lookup$Group)]
        RN$Date = Lookup$Date[match(rownames(RN),Lookup$Group)]
        
        # Adding IRMS data to RN based on the data in lookup
        tempIRMS <<- aggregate(PreR~Group,Lookup[match(rownames(RN),Lookup$Group),],
                               function(x) {temp <- table(x); names(temp)[which.max(temp)]})
        RN$IRMS = tempIRMS$PreR[match(rownames(RN),tempIRMS$Group)]
        RN$IRMS = ifelse(substr(rownames(RN),1,2)=="CC","CC",RN$IRMS)
        
        # Adding CE (Chiral) data to RN based on the data in lookup
        tempCH = aggregate(Chiral~Group,Lookup[match(rownames(RN),Lookup$Group),],
                           function(x) {temp <- table(x); names(temp)[which.max(temp)]})
        RN$Chiral = tempCH$Chiral[match(rownames(RN),tempCH$Group)]
        RN$Chiral = ifelse(substr(rownames(RN),1,2)=="CC","CC",RN$Chiral)
        
        # Ordering the dataframe
        RN = RN[order(if(is.null(input$AnTech)==T){
          rownames(RN)
        } else if(input$AnTech=="IRMS") {
          RN$IRMS
        } else if(input$AnTech=="Chiral"){
          RN$Chiral}),]
        
        #Setting the node colours depending on the data shown (e.g. GCMS only, GCMS + IRMS, GCMS + CE)
        RN$colour = if(is.null(input$AnTech)==T){
          ifelse(substr(rownames(RN),1,2)=="CC",input$CCcol, input$Groupcol)
        } else if(input$AnTech=="IRMS") {
          cols = data.frame(Type = c("CC",unique(Lookup$PreR)))
          cols$Colours = ifelse(cols$Type=="CC",as.character(input$CCcol),
                                ifelse(cols$Type=="Inconclusive",as.character(input$Groupcol),""))
          cols = cols[order(cols$Colours),]
          cols$Colours = ifelse(cols$Colours=="",
                                RColorBrewer::brewer.pal(
                                  length(unique(cols$Type[cols$Type!="CC"&cols$Type!="Inconclusive"])),"Dark2"),
                                cols$Colours)
          cols$Colours[match(RN$IRMS,cols$Type)]
        } else if(input$AnTech=="Chiral") {
          cols = data.frame(Type = c("CC",unique(Lookup$Chiral)))
          cols$Colours = ifelse(cols$Type=="CC",as.character(input$CCcol),
                                ifelse(cols$Type=="Inconclusive",as.character(input$Groupcol),""))
          cols = cols[order(cols$Colours),]
          cols$Colours = ifelse(cols$Colours=="",
                                RColorBrewer::brewer.pal(
                                  length(unique(cols$Type[cols$Type!="CC"&cols$Type!="Inconclusive"])),"Dark2"),
                                cols$Colours)
          cols$Colours[match(RN$Chiral,cols$Type)]
        }
        
        RN$names <- ifelse(RN$IRMS=="CC",rownames(RN),NA)
        L <<- RN
        return(RN)
      })
  
  # Creating the network plot
  output$net <- renderPlotly({
    req(RN())
    req(es())
    plot_ly(
      x = RN()[,1], y = RN()[,2], type = "scatter", mode = "markers", source="subset",
      name=if(is.null(input$AnTech)==T){
        rownames(RN())
      } else if(input$AnTech=="IRMS") {
        RN()$IRMS
      } else if(input$AnTech=="Chiral"){
        RN()$Chiral},
      text = rownames(RN()), hoverinfo = rownames(RN()),  
      marker=list(color = RN()$colour, size = input$mrkrsize
      )) %>%
      layout(
        legend = list(orientation = ifelse(is.null(input$AnTech)==T,'v','h')),
        shapes = lapply(c(1:length(es()[1]$V1)),function(i){
          list(
            type = "line",
            line = list(color = "#030303", width = 0.3),
            x0 = RN()[rownames(RN())==as.character(es()[i,]$V1),1],
            y0 = RN()[rownames(RN())==as.character(es()[i,]$V1),2],
            x1 = RN()[rownames(RN())==as.character(es()[i,]$V2),1],
            y1 = RN()[rownames(RN())==as.character(es()[i,]$V2),2]
          )
        }),
        xaxis = list(title = "", showgrid = F, showticklabels = F, zeroline = F),
        yaxis = list(title = "", showgrid = F, showticklabels = F, zeroline = F))
    
  })
  
  
  
  # NETWORK SUMMARY ---------------------------------------------------------
  output$CLT = renderDT({
    Lkp = Lookup[Lookup$Date>=input$slider[1]&Lookup$Date<=input$slider[2]&Lookup$cluster==input$Cluster,]
    Lkp[rowSums(is.na(Lkp)) != ncol(Lkp),]
    return(as.datatable(formattable(Lkp), rownames= FALSE) %>% 
             formatStyle(
               columns = c(1:ncol(Lkp)),
               backgroundColor = "#FFFFFF")
    )
  })
  
  output$CVT = renderDT({
    req(GCMS_TV)
    round(GCMS_T[rownames(GCMS_T) %in% Lookup$Specimen[Lookup$Date>=input$slider[1]&Lookup$Date<=input$slider[2]&Lookup$cluster==input$Cluster],], 2)
  })
  
  output$CCM = renderDT({
    Filt_CCM <- CCM[rownames(CCM) %in% Lookup$Specimen[Lookup$Date>=input$slider[1]&Lookup$Date<=input$slider[2]&Lookup$cluster==input$Cluster],
                    rownames(CCM) %in% Lookup$Specimen[Lookup$Date>=input$slider[1]&Lookup$Date<=input$slider[2]&Lookup$cluster==input$Cluster]]
    return(as.datatable(formattable(Filt_CCM[-1,-ncol(Filt_CCM)], 
                                    list(area(col = c(1:ncol(Filt_CCM)-1)) ~ color_tile("#DeF7E9", "#71CA97"))
    ), 
    options=list(scrollX=TRUE)))
  })
  
  
  
  # RELATIONAL ANALYSIS -------------------------------------------------
  
  # Network values
  Net_Val <- eventReactive(c(input$do,input$slider),{
    req(RN())
    req(es())
    data.frame(
      No_SG = sum(RN()$IRMS!="CC"),
      No_CC = sum(RN()$IRMS=="CC"),
      Net_Dens = round(ecount(GT())/(
        length(names(V(GT()))[substr(names(V(GT())),1,2)=="CC"])*
          (length(names(V(GT())))-length(names(V(GT()))[substr(names(V(GT())),1,2)=="CC"]))
      ),3),
      CC_2_SG = es() %>% group_by(V1) %>% summarise(G = n_distinct(V2)) %>% filter(G >= 2) %>% count() %>% pull(n),
      MLP = es() %>% group_by(V2) %>% summarise(G = n_distinct(V1)) %>% filter(G >= 2) %>% count() %>% pull(n),
      No_Reg = length(unique(RN()$Region))
    )
  })
  
  output$NetVal1 <- renderValueBox({
    valueBox(
      value = Net_Val()$No_SG,
      subtitle = "Number of Seizures", 
      color = "light-blue")
  })
  
  output$NetVal2 <- renderValueBox({
    valueBox(
      value = Net_Val()$No_CC,
      subtitle = "Number of CCs", 
      color = "light-blue")
  })
  
  output$NetVal3 <- renderValueBox({
    valueBox(
      value = paste0(Net_Val()$Net_Dens," %"),
      subtitle = "Network Desnity", 
      color = "light-blue")
  })
  
  output$NetVal4 <- renderValueBox({
    valueBox(
      value = Net_Val()$CC_2_SG,
      subtitle = "CCs with 2 or more Seizures", 
      color = "light-blue")
  })
  
  output$NetVal5 <- renderValueBox({
    valueBox(
      value = Net_Val()$MLP,
      subtitle = "Multi-profile Seizures", 
      color = "light-blue")
  })
  
  output$NetVal6 <- renderValueBox({
    valueBox(
      value = Net_Val()$No_Reg,
      subtitle = "Number of Regions", 
      color = "light-blue")
  })
  
  # Creating subplots for the n largest compoents in the main plot
  output$LCs <- renderPlotly({
    req(GT)
    
    Net_Comps <- components(GT())$csize
    Net_Comps_Order <- order(Net_Comps, decreasing = T)
    Net_Dec <- decompose(GT())
    
    Y <- lapply(1:input$No_LC, function(x)Net_Dec[[Net_Comps_Order[x]]])
    
    LN_Sub <- lapply(1:input$No_LC,function(x)
      data.frame(layout_with_fr(Y[[x]]), 
                 L[match(names(V(Y[[x]])),rownames(L)),3:8],
                 row.names = names(V(Y[[x]]))))
    es_Sub <- lapply(1:input$No_LC,function(x)as.data.frame(get.edgelist(Y[[x]])))
    
    
    LC_Plots <- lapply( 
      1:input$No_LC,
      function(x){
        plot_ly(
          x = LN_Sub[[x]][,1], y = LN_Sub[[x]][,2], type = "scatter", mode = "markers", source="subset",
          name=if(is.null(input$AnTech)==T){
            rownames(LN_Sub[[x]])
          } else if(input$AnTech=="IRMS") {
            LN_Sub[[x]]$IRMS
          } else if(input$AnTech=="Chiral"){
            LN_Sub[[x]]$Chiral},
          text = rownames(LN_Sub[[x]]), hoverinfo = rownames(LN_Sub[[x]]),  
          marker=list(color = LN_Sub[[x]]$colour, size = input$mrkrsize
          )) %>%
          layout(#paper_bgcolor='#5875D5',
            plot_bgcolor='#FAFBFD',
            legend = list(orientation = ifelse(is.null(input$AnTech)==T,'v','h')),
            shapes = lapply(c(1:length(es_Sub[[x]][1]$V1)),function(i){
              list(
                type = "line",
                line = list(color = "#030303", width = 0.3),
                x0 = LN_Sub[[x]][rownames(LN_Sub[[x]])==as.character(es_Sub[[x]][i,]$V1),1],
                y0 = LN_Sub[[x]][rownames(LN_Sub[[x]])==as.character(es_Sub[[x]][i,]$V1),2],
                x1 = LN_Sub[[x]][rownames(LN_Sub[[x]])==as.character(es_Sub[[x]][i,]$V2),1],
                y1 = LN_Sub[[x]][rownames(LN_Sub[[x]])==as.character(es_Sub[[x]][i,]$V2),2]
              )
            }),
            xaxis = list(title = "", showgrid = F, showticklabels = F, zeroline = F),
            yaxis = list(title = "", showgrid = F, showticklabels = F, zeroline = F))
      })
    
    subplot(LC_Plots, nrows = as.integer(sqrt(input$No_LC)))
  })
  
  # Reactive slider for changing network plot date range
  output$date_selectorRel = renderUI({
    sliderInput("sliderRel",
                label="",
                min=as.Date(input$dates[1]),
                max=as.Date(input$dates[2]),
                value=as.Date(c(input$dates[1],input$dates[1]+365)),
                width="100%")
  })
  
  
  # TEMPORAL ANALYSIS ---------------------------------------------------
  
  # Table of temporal analysis values (not reactive - precalculated data)
  Temp_Data <- as.data.frame(read.xlsx("Data/Temporal_data.xlsx", sheet = "Sheet1", rowNames = T, colNames = T))
  
  output$TD <- renderFormattable({
    formattable(Temp_Data,
                digits = 0,
                list(area(row = c(2,4)) ~ percent
                )
    )
  })
  
  # # Venn diagram -pending
  # output$Venn <- renderPlot({})
  
  # Creating a line plot (with a cumulative reference) showing the number of months CCs are active
  output$CC_Over_Time <- renderPlot({
    # Calculating the number of CCs observed over time
    A <- aggregate(Date~cluster, Lookup, function(x){c(min(x), max(x))})
    A <- as.data.frame(do.call(cbind,A))
    A$V2 <- as.Date(A$V2, origin = "1970-01-01")
    A$V3 <- as.Date(A$V3, origin = "1970-01-01")
    A$days <- difftime(A$V3 ,A$V2 , units = c("days")) + 1
    A$days <- as.numeric(A$days)
    A$mo <- round(A$days/30,0)
    A <<- A
    
    B <- aggregate(cluster~mo, A, function(x){length(unique(x))})
    B$cumu <- cumsum(B$cluster)
    B$cumu <- round(B$cumu/max(B$cumu)*100,0)
    
    # visualising the no of CC observed over time 
    ggplot(B,aes(x = mo)) + 
      geom_line(aes(y=cluster),size=1.5)+
      geom_line(aes(y=cumu/2),size=1.5, color="grey60")+
      scale_x_continuous(name="Number of months",expand = c(0,0), limits = c(0,60))+
      scale_y_continuous(name="Number of CCs",breaks=seq(0,50,5),
                         sec.axis=sec_axis(~.*2,name="Cumulative number of CCs (%)",breaks=seq(0,100,20)))+
      theme_light()+
      theme(legend.position="bottom",
            axis.title=element_text(face="bold", size = 12), 
            axis.text = element_text(size = 10),
            strip.text=element_text(face="bold"),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank())
  })
  
  # Valueboxes for the mean and median CCs life in days\
  output$CC_Mean_Day <- renderValueBox(
    valueBox(
      round(mean(A$days),0),
      "Mean life of CCs in days", 
      color = "light-blue"
    )
  )
  output$CC_Median_Day <- renderValueBox(
    valueBox(
      round(median(A$days),0),
      "Median life of CCs in days", 
      color = "light-blue"
    )
  )
  
  # Plotting Cc 'life' over all quarters
  output$Quarters <- renderPlot({  
    # Number of CC gained, retained and lost per quarter
    CC_Life <- Lookup %>% group_by(cluster) %>% summarise(min = min(Date), max = max(Date))
    CC_Life$days <- difftime(CC_Life$max ,CC_Life$min , units = c("days")) + 1
    CC_Life$month <- round(CC_Life$days/30,0)
    CC_Life$int <- interval(CC_Life$min, CC_Life$max)
    
    Quarters <- data.frame(Start = seq(as.Date("2015-01-01"), as.Date("2019-10-01"), by = "quarter"),
                           End = seq(as.Date("2015-04-01"), as.Date("2020-01-10"), by = "quarter")-1)
    Quarters$int <- interval(Quarters$Start, Quarters$End)
    Quarters$CC <- unlist(lapply(c(1:nrow(Quarters)),function(x){length(na.omit(intersect(CC_Life$int, Quarters$int[x])))}))
    Quarters$gain <- unlist(lapply(c(1:nrow(Quarters)),function(x){sum(as.Date(CC_Life$min) %within% Quarters$int[x], na.rm = TRUE)}))
    Quarters$const <- Quarters$CC-Quarters$gain
    Quarters$lost <- unlist(lapply(c(1:nrow(Quarters)),function(x){sum(as.Date(CC_Life$max) %within% Quarters$int[x-1], na.rm = TRUE)}))
    Quarters$Q <- quarters(Quarters$Start)
    Quarters <- pivot_longer(Quarters, gain:lost)
    
    # Visualising the number of CC gained, retained and lost per quarter 
    Quarters %>% 
      mutate(
        value = case_when(
          name == "lost" ~ -1 * value,
          TRUE ~ as.numeric(value)),
        YQ = paste0(year(Start),"-",Q)
      ) %>% 
      ggplot(aes(YQ, value)) +
      geom_bar(aes(fill = name), stat = "identity") +
      scale_fill_manual(values = c("grey60","coral2", "cadetblue")) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 90, size = 10),
        axis.text.y = element_text(size = 10)
      )
  })
  
  
  # SPATIAL ANALYSIS ----------------------------------------------------
  # Valueboxes showing number of CCs that include seizures from each region
  No_Reg_Data <- reactive({
    req(RN())
    lapply(c("ACT","NSW","NT","QLD","SA","TAS","VIC","WA"),
           function(x){
             sum(na.omit(RN()$Region)==x)
           })
  })
  
  output$No_Reg_VB1 <- renderValueBox({
    valueBox(
      value = No_Reg_Data()[[1]],
      subtitle = "ACT", 
      color = "light-blue")
  })
  output$No_Reg_VB2 <- renderValueBox({
    valueBox(
      value = No_Reg_Data()[[2]],
      subtitle = "NSW", 
      color = "light-blue")
  })
  output$No_Reg_VB3 <- renderValueBox({
    valueBox(
      value = No_Reg_Data()[[3]],
      subtitle = "NT", 
      color = "light-blue")
  })
  output$No_Reg_VB4 <- renderValueBox({
    valueBox(
      value = No_Reg_Data()[[4]],
      subtitle = "QLD", 
      color = "light-blue")
  })
  output$No_Reg_VB5 <- renderValueBox({
    valueBox(
      value = No_Reg_Data()[[5]],
      subtitle = "SA", 
      color = "light-blue")
  })
  output$No_Reg_VB6 <- renderValueBox({
    valueBox(
      value = No_Reg_Data()[[6]],
      subtitle = "TAS", 
      color = "light-blue")
  })
  output$No_Reg_VB7 <- renderValueBox({
    valueBox(
      value = No_Reg_Data()[[7]],
      subtitle = "VIC", 
      color = "light-blue")
  })
  output$No_Reg_VB8 <- renderValueBox({
    valueBox(
      value = No_Reg_Data()[[8]],
      subtitle = "WA", 
      color = "light-blue")
  })
  
  # creating a map to display where seizures have been made (respective to the date slider)
  # NOTE: CURRENTLY THIS ONLY EXTENDS TO NSW DAATA
  countPC <- eventReactive(input$do_map,{
    Lookup %>% 
      filter(!is.na(Postcode)) %>%
      filter(!is.na(cluster)) %>%
      select(Group,Postcode,Date) %>%
      distinct()
  })
  
  data_input <- reactive({
    countPC() %>%
      filter(Date >= input$slider[1]
      ) %>%
      filter(Date <= input$slider[2]
      ) %>%
      group_by(Postcode) %>%
      summarise(Seizures = dplyr::n())
  })
  
  PCs <- reactive({subset(PC, is.element(PC$POA_NAME16,data_input()$Postcode))})
  
  data_input_ordered <- reactive({
    data_input()[order(match(data_input()$Postcode,PCs()$POA_NAME16)),]
  })
  
  labels <- reactive({
    labels <- paste("<p>", "Postcode: ", data_input_ordered()$Postcode, "<p>",
                    "<p>", "No. of seizures: ", data_input_ordered()$Seizures, "<p>", sep = "")
  })
  
  pal <- colorBin("YlOrRd", domain = c(0,1), bins = seq(0,8,2))
  
  output$Map <- renderLeaflet({
    leaflet() %>% 
      setView(lat = -33, lng = 147, zoom = 6) %>%
      addProviderTiles(providers$Stamen.TonerLite) %>%    # Map Themes
      addPolygons(data = PCs(), 
                  color = "#666666", 
                  weight = 1, 
                  fillOpacity = 0.8, 
                  fillColor = pal(data_input_ordered()$Seizures),
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = lapply(labels(), HTML)) %>%
      addLegend(title = "No. of seizures",
                pal = pal,
                values = data_input_ordered()$Seizures, 
                opacity = 0.7,
                position = "topright")
  })
  
  output$MapTab <- renderDT(data_input(), filter = 'top', server = FALSE, 
                            options = list(pageLength = 5, autoWidth = F),
                            rownames= FALSE)
  
  # Reactive slider for changing network plot date range
  output$date_selectorSpa = renderUI({
    sliderInput("sliderSpa",
                label="",
                min=as.Date(input$dates[1]),
                max=as.Date(input$dates[2]),
                value=as.Date(c(input$dates[1],input$dates[1]+365)),
                width="100%")
  })
  
  # QUANTITATIVE ANALYSIS -----------------------------------------------
  
  # Calculating purities for each Region and all regions (domestic) per quarter
  output$Purity <- renderPlot({
    Lkp_Regional <- Lookup
    Lkp_Domestic <- Lookup
    Lkp_Domestic$Region = "Domestic"
    
    Lkp_All <-  rbind(Lkp_Regional[,c("Specimen","Date","Year","Purity","Region")],
                      Lkp_Domestic[,c("Specimen","Date","Year","Purity","Region")])
    Lkp_All$Quarter <- quarters(Lkp_All$Date)
    Lkp_All$Region_f <- factor(Lkp_All$Region, 
                               levels = c("ACT","NSW","NT","QLD","SA","TAS","VIC","WA","Domestic"))
    
    
    ggplot(Lkp_All, aes(x=Quarter, y=Purity)) +
      geom_boxplot(outlier.shape = 21) + 
      facet_grid(Region_f~Year, scales="free_x", drop=T) +
      labs(y="Purity (%)")+
      theme_light()+
      theme(axis.title=element_text(face="bold",size = 11),
            axis.text = element_text(size = 10),
            strip.text=element_text(face="bold",size = 12),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    
  })
  
  # Determining precursors used for each Region and all regions (domestic)
  output$Precursor <- renderPlot({
    Lkp_Regional <- Lookup
    Lkp_Domestic <- Lookup
    Lkp_Domestic$Region = "Domestic"
    
    Lkp_All <-  rbind(Lkp_Regional[,c("Specimen","Date","Year","Pre","Region")],
                      Lkp_Domestic[,c("Specimen","Date","Year","Pre","Region")])
    
    Lkp_All$Pre <- ifelse(is.na(Lkp_All$Pre),"Unclassified",
                          ifelse(Lkp_All$Pre%in%c("Pred. EPH/PSE","PSE","EPH"),"EPH/PSE",
                                 ifelse(Lkp_All$Pre=="Pred. P2P","P2P",Lkp_All$Pre)))
    
    Lkp_Pre <- Lkp_All %>% 
      group_by(Region,Year) %>% 
      count(Pre) %>% 
      group_by(Region,Year) %>% 
      mutate(countT = sum(n)) %>% 
      group_by(Pre, add=TRUE) %>%
      mutate(per=round(100*n/countT,2))
    Lkp_Pre$Region_f <- factor(Lkp_Pre$Region, 
                               levels = c("ACT","NSW","NT","SA","TAS","VIC","WA","Domestic","National"))
    Lkp_Pre$Pre_f <- factor(Lkp_Pre$Pre, 
                            levels = c("EPH/PSE","P2P","Mixed","Unclassified"))
    
    ggplot(Lkp_Pre,aes(x=Pre_f,y=per)) +
      geom_bar(stat="identity")+
      facet_grid(Region_f~Year, scales="fixed", drop=T) +
      labs(y="Percentage", x="Precursor Type")+
      scale_fill_grey()+
      theme_light()+
      theme(axis.title=element_text(face="bold",size = 11),
            axis.text = element_text(size = 10),
            axis.text.x = element_text(angle = 45,hjust=1),
            strip.text=element_text(face="bold",size = 12),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
  })
  
  
  # RELAYOUT ------------------------------------------------------------
  
  # The "Calculate optimal measures" button takes you to the "Optimise Measures" page 
  observeEvent(input$CalcOM,{
    updateTabItems(session, "tabs", selected = "Optimise")
  })
  
  # The "Go to plots" button takes you to the "plot" page
  observeEvent(input$Set_FPR,{
    updateTabItems(session, "tabs", selected = "NP")
  })
  
  # Clicking on a CC in the network plot takes you to its "Summary" page
  observeEvent(event_data("plotly_click", source="subset"),{
    s = event_data("plotly_click", source="subset")
    if(substr(rownames(RN())[round(RN()$V1,6)==as.numeric(round(s[["x"]],6)) &
                             round(RN()$V2,6)==as.numeric(round(s[["y"]],6))],1,2)=="CC"){
      updateTabItems(session, "tabs", selected = "Summary")
      updateTextInput(session, "Cluster", value =
                        substring(rownames(RN())[round(RN()$V1,6)==as.numeric(round(s[["x"]],6)) &
                                                   round(RN()$V2,6)==as.numeric(round(s[["y"]],6))],4))
    } else {
      plotly_empty()
    }
  })
  
  # Move from Summary page back to Plot page
  observeEvent(input$back2net,{
    updateTabItems(session, "tabs", selected = "NP")
  })
  
  # Several buttons on "Plot" page; each take you to the respective "Analysis" page and back to the "Plot" page
  observeEvent(input$Go2Rel,{
    updateTabItems(session, "tabs", selected = "Rel")
  })
  observeEvent(input$back2netRel,{
    updateTabItems(session, "tabs", selected = "NP")
  })
  
  observeEvent(input$Go2Temp,{
    updateTabItems(session, "tabs", selected = "Temp")
  })
  observeEvent(input$back2netTemp,{
    updateTabItems(session, "tabs", selected = "NP")
  })
  
  observeEvent(input$Go2Spa,{
    updateTabItems(session, "tabs", selected = "Spa")
  })
  observeEvent(input$back2netSpa,{
    updateTabItems(session, "tabs", selected = "NP")
  })
  
  observeEvent(input$Go2Quant,{
    updateTabItems(session, "tabs", selected = "Quant")
  })
  observeEvent(input$back2netQuant,{
    updateTabItems(session, "tabs", selected = "NP")
  })
  
  # Making all the date sliders dependent on each other
  observeEvent(input$slider,{
    updateSliderInput(session, "sliderRel", value = c(input$slider[1],input$slider[2]))
  })
  observeEvent(input$sliderRel,{
    updateSliderInput(session, "slider", value = c(input$sliderRel[1],input$sliderRel[2]))
  })
  
  observeEvent(input$slider,{
    updateSliderInput(session, "sliderSpa", value = c(input$slider[1],input$slider[2]))
  })
  observeEvent(input$sliderSpa,{
    updateSliderInput(session, "slider", value = c(input$sliderSpa[1],input$sliderSpa[2]))
  })
  
  observeEvent(input$sliderRel,{
    updateSliderInput(session, "sliderSpa", value = c(input$sliderRel[1],input$sliderRel[2]))
  })
  observeEvent(input$sliderSpa,{
    updateSliderInput(session, "sliderRel", value = c(input$sliderSpa[1],input$sliderSpa[2]))
  })
  
}

shinyApp(ui, server)