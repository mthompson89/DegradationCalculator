# import libraries ####
library(shiny)
library(rsconnect)
library(shinyWidgets)
library(tidyverse)
library(dplyr)
library(readxl)
library(data.table)
library(vroom)
library(shinythemes)
library(ggplot2)
library(plotly)
library(sortable)
library(shinyjs)
library(shinyBS)
library(shinyalert)

options(app_idle_timeout = 0, shiny.maxRequestSize = 3000*1024^2)
# Load Data ####
# 
# sg_ref <- as.data.frame(read_csv('sg_ref.csv')) %>%
#   filter(desirability > 4) %>%
#   filter(Northeast == 1)
# 
# primesp <- sg_ref %>%
#   filter(desirability == 1) %>%
#   pull(cname,Gen_sp)
# 
# secsp <- sg_ref %>%
#   filter(desirability == 2) %>%
#   pull(cname,Gen_sp)
# 
# tertsp <- sg_ref %>%
#   filter(desirability == 3) %>%
#   pull(cname,Gen_sp)
# 
# hwdf <- sg_ref %>%
#   filter(HW_SW == "HW")
# 
# swdf <- sg_ref %>%
#   filter(HW_SW == 'SW')
# function builder ####
  ## functions for calculating stocking percentages ####

C_lineBA <- function(DBH, SG){
  
  a = 0.00015# constants from Ducey&knapp 2010 relative density measure
  b = 0.00218 # constants from Ducey&knapp 2010 relative density measure
  c = 0.4 #c-line stocking
  k = pi/40000 #metric constant for area
  
  BA <- (((25^1.6*c*k)/(a+b*SG))*(DBH*2.54)^0.4)*4.356
  
}
B_lineBA <- function(DBH, SG){
  
  a = 0.00015
  b = 0.00218
  c = 0.6 #c-line stocking
  k = pi/40000 #metric constant for area
  
  BA <- (((25^1.6*c*k)/(a+b*SG))*(DBH*2.54)^0.4)*4.356
  
}
A_lineBA <- function(DBH, SG){
  
  a = 0.00015
  b = 0.00218
  c = 1 #c-line stocking
  k = pi/40000 #metric constant for area
  
  BA <- (((25^1.6*c*k)/(a+b*SG))*(DBH*2.54)^0.4)*4.356
  
}

  ## function to read in and calculate deg for a FVS output ####
fvs_calc <- function(data, prime,sec,tert){
  # SG_ref <- vroom("SG_ref.csv", col_types = "ffnn")
  
  removedSp <- c("ACSA3","FAGR","TSCA","PIRU","BEAL2","ACRU","ABBA","ACPE")
  
  sg_ref()$splist <- sub("^0+", "", sg_ref()$splist)
  
  #remove rows that have plant ID instead of fia sp codes
  data<- data[!grepl(paste(removedSp, collapse="|"), data$Species),]
  
  #remove leading zeros in fia species cods
  data$Species <- sub("^0+", "", data$Species)
  
  #join fvs data SG ref so we have specific gravity for all species
  df1 <- data%>%
    full_join(sg_ref(), by = c("Species" = "splist"))
  
  #create coloumns for tree factor, relative density, and counting trees by desirability.
  df1.2 <- df1%>%
    mutate(TPH = TPA*2.47105,
           TF = TPH/`Total Plots`,
           RD = TF*(0.00015+0.00218*sg)*((DBH*2.54)/25)^1.6,
           prime_AGS = Gen_sp %in% prime  & `Ags Ugs` == "AGS",  # next four lines create boolians for if a tree is a primary AGS or secondary/tertiary
           sec_AGS = Gen_sp %in% sec & `Ags Ugs` == "AGS",
           tert_AGS = Gen_sp %in% tert & `Ags Ugs` == "AGS",
           UGS = `Ags Ugs` == "UGS")
  
  standRD <- df1.2%>%
    group_by(StandID)%>%
    summarise(PrimeRD = sum(if_else(prime_AGS == TRUE,RD,0)),
              SecRD = sum(if_else(sec_AGS == TRUE,RD,0)),
              TertRD = sum(if_else(tert_AGS == TRUE,RD,0)),
              UGSRD = sum(if_else(UGS == TRUE,RD,0))
    )
  standRD.Table <- as.data.table(standRD) 
  
  standRD.Table <- standRD.Table[, `:=` (Deg = case_when(PrimeRD >= 0.4 ~ 1,
                                                         PrimeRD + SecRD >= 0.4~ 2,
                                                         PrimeRD + SecRD + TertRD >= 0.4 ~ 3,
                                                         PrimeRD + SecRD + TertRD + UGSRD >= 0.4 ~4,
                                                         TRUE ~ 5)), by = .(StandID)]
  
  dfStandRD <- as.data.frame(standRD.Table)
  dfStandRD_v2 <- dfStandRD%>%
    distinct()%>%
    na.omit()
  return(dfStandRD_v2)
}

createhw_input_pair_ui <- function(hwspecies_name, hwsci_name) {
  fluidRow(style = "overflow-y:scroll; max-height: 400px; position:relative;",
    column(12, align = "center",
           h5(strong(hwspecies_name)),
           h6(hwsci_name),
           column(6, align = "center", numericInput(
             inputId = hwsci_name,
             label = "AGS",
             value = 0,
             min = 0,
             width = validateCssUnit(200)
           )),
           column(6, align = "center", numericInput(
             inputId = paste0(hwsci_name, "UGS",sep = ''),
             label = "UGS",
             value = 0,
             min = 0,
             width = validateCssUnit(200)
           ))
    )
  )
}

createsw_input_pair_ui <- function(swspecies_name, swsci_name) {
  fluidRow(style = "overflow-y:scroll; max-height: 400px; position:relative;",
    column(12, align = "center",
           h5(strong(swspecies_name)),
           h6(swsci_name),
           column(6, align = "center", numericInput(
             inputId = swsci_name,
             label = "AGS",
             value = 0,
             min = 0,
             width = validateCssUnit(200)
           )),
           column(6, align = "center", numericInput(
             inputId = paste0(swsci_name, "UGS", sep = ''),
             label = "UGS",
             value = 0,
             min = 0,
             width = validateCssUnit(200)
           ))
    )
  )
}

createhwslider_input_pair_ui <- function(hwspecies_name, hwsci_name) {
  fluidRow(style = "overflow-y:scroll; max-height: 400px; position:relative;",
    column(12, align = "center",
           h5(strong(hwspecies_name)),
           h6(hwsci_name),
           column(6, align = "center", sliderInput(
             inputId = hwsci_name,
             label = "BA Proportion",
             value = 0,
             min = 0,
             max = 1,
             width = validateCssUnit(200)
           )),
           column(6, align = "center", sliderInput(
             inputId = paste0(hwsci_name, "AGS", sep = ''),
             label = "Proportion AGS",
             value = 0,
             min = 0,
             max = 1,
             width = validateCssUnit(200)
           ))
    )
  )
}

createswslider_input_pair_ui <- function(swspecies_name, swsci_name) {
  fluidRow(style = "overflow-y:scroll; max-height: 400px; position:relative;",
    column(12, align = "center",
           h5(strong(swspecies_name)),
           h6(swsci_name),
           column(6, align = "center", sliderInput(
             inputId = swsci_name,
             label = "BA Proportion",
             value = 0,
             min = 0,
             max = 1,
             width = validateCssUnit(200)
           )),
           column(6, align = "center", sliderInput(
             inputId = paste0(swsci_name, "AGS", sep = ''),
             label = "Proportion AGS",
             value = 0,
             min = 0,
             max = 1,
             width = validateCssUnit(200)
           ))
    )
  )
}
# Define UI #####
shinyApp(
  ui = tagList(
    useShinyjs(),
    tags$head(
      tags$link(rel="stylesheet", type="text/css",
                href="slick-1.8.1/slick/slick-theme.css"),
      tags$link(rel="stylesheet", type="text/css",
                href="slick-1.8.1/slick/slick.css"),
      tags$script(type="text/javascript", 
                  src="slick-1.8.1/slick/slick.js"),
      tags$script(HTML(
        "$(document).ready(function(){
                          $('#images').slick({
                            arrows: true,
                            dots:true
                          });
                        });")),
      tags$style(HTML(
        "#images .slick-prev {
                          position:default;

                      }
                      #images .slick-next {
                        position:default;
                      }
                      .slick-prev:before, .slick-next:before { 
                          color:#2c3e50 ;
                          font-size: 20px;
                      }
                      .content {
                          margin: auto;
                          padding: auto;
                          width: auto;
                      }"))
    ),
    navbarPage(
      theme = shinytheme("flatly"),  
      "Degradation Calculator",
  ## How To ####
      tabPanel("How To",
               fluidRow(align = "center",
                        tags$div(
                          class = "content",
                          tags$div(
                            id = "images", 
                            tags$img(
                              src = "Welcome.jpg",
                              width = "auto",
                              height = "auto"
                            ),
                            tags$img(
                              src = "HowitWorks1.jpg",
                              width = "auto",
                              height = "auto"
                            ),
                            tags$img(
                              src = "HowitWorks2.jpg",
                              width = "auto",
                              height = "auto"
                            ),
                            tags$img(
                              src = "PlotInput.jpg",
                              width = "auto",
                              height = "auto"
                            ),
                            tags$img(
                              src = "TreeListInput.jpg",
                              width = "auto",
                              height = "auto"
                            ),
                            tags$img(
                              src = "TreeCatSelector.jpg",
                              width = "auto",
                              height = "auto"
                            )
                          )
                        )
               )
      ),
  ## Prism Plots #####
      tabPanel("Plot Input",
    ### Sidebar Panel ####
               sidebarPanel(
                 #input if a Basal area prism is going to be used or known basal area by species
                 selectInput('region','Plese Select Region', choices = c("Northeast",'Mid-Atlantic'),selected = 'Northeast'),
                 selectInput('BAInput','1. Prism Plot Data or Known Basal Area (sqft/acre) by Species?',
                             choices =  c('Prism Plot Data','Known Basal Area by Species','Basal Area percentage'),selected = "Prism Plot Data"),
                 bsTooltip("BAInput", "Choose if you are entering 1. Data directly from a prism plot2. The known basal area (sqft/acre) by species in the stand3. The percent basal area of each species.",
                           "right", options = list(container = "body")),
                 
                 uiOutput('basalArea'),
                 bsTooltip("basalArea", "Enter the basal area factor prism you are using to conduct the prism plot",
                           "right", options = list(container = "body")),
                 
                 uiOutput('basalArea2'),
                 bsTooltip("basalArea2", "Enter the number of prism plots you conducted in the stand",
                           "right", options = list(container = "body")),
                 
                 uiOutput('totalBA'),
                 bsTooltip("totalBA", "Enter the total basal area of the stand",
                           "right", options = list(container = "body")),
                 
                 #input wether stand avg dbh or stand class size will be used
                 selectInput("dbhInput", "2. Select Stand Class Size or Mean DBH of Stand",
                             c("Stand Class Size","Mean DBH of Stand")),
                 bsTooltip("dbhInput", "Choose whether you are going to use the average stand size class to calculate DBH or if you have calculated the mean DBH of the stand",
                           "right", options = list(container = "body")),
                 uiOutput("ui"),
                 uiOutput('KnownBANote'),
                 fluidPage(
                   column(6,uiOutput("HWTrDtInputs")),
                   column(6,uiOutput('SWTrDtInputs'))
                 ),
                 # fluidPage(
                 #   uiOutput('SWTrDtInputs')
                 # ),
                 width = 6
               ),
    ### Main panel#####
               mainPanel(
                 fluidRow(style = "padding-left: 200px",align = "center",
                           plotlyOutput("plot",width = 600, height = 700),
                           #tableOutput("Testdf"),
                           # verbatimTextOutput("allInputs")
                 
                 ),
                 fluidRow(style = "padding: 50px,50px,50px,50px", align = "center",
                          htmlOutput("degValue")
                          
                 ),
                 width = 6
                 
               )
      ),
  ## FVS Outputs #####
      tabPanel("Tree List Input", 
               sidebarPanel(
                 tags$head(tags$style(type="text/css", "
                                         .loading {
                                              display: inline-block;
                                              overflow: hidden;
                                              height: 1.3em;
                                              margin-top: -0.3em;
                                              line-height: 1.5em;
                                              vertical-align: text-bottom;
                                              box-sizing: border-box;
                                              }
                                         .loading.dots::after {
                                            text-rendering: geometricPrecision;
                                             content: '.\\A..\\A...\\A....';
                                            animation: spin10 2s steps(10) infinite;
                                            animation-duration: 2s;
                                            animation-timing-function: steps(10);
                                            animation-delay: 0s;
                                            animation-iteration-count: infinite;
                                            animation-direction: normal;
                                            animation-fill-mode: none;
                                            animation-play-state: running;
                                            animation-name: spin10;
                                        }
                                        .loading::after {
                                            display: inline-table;
                                            white-space: pre;
                                            text-align: left;
                                        }
                                        @keyframes spin10 { to { transform: translateY(-15.0em); } }
                                        '
                                      ")),
                 # paste("Select FVS file to upload. File must be no larger than 3Gb and must be a csv"),
                 fileInput("FVS_Sheet", "Upload Tree List",
                           accept = ".csv",
                           multiple = FALSE,
                           width = 750),
                 downloadButton("downloadData", "Download")
                 
               ),
               mainPanel(
                 conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                  tags$div("Please Wait While App is Working", id = "UpdateAnimate", class = "loading dots")),
                 
                 # dataTableOutput("treelist_checker")
                 dataTableOutput("FVS")
                 
               )
      ),
  ## Tree Category Selector#####
      tabPanel("Tree Category Selector",
               fluidRow(align = "center",
                        h3("Change species desirablity categories"),
                        h5("Click and drag species into prefered desirability boxes. To reset species, simply refresh app")
               ),
               uiOutput('list'),
               fluidRow(align = "center",
                        h6("Default Species Categories"),
                        tags$div(
                          class = "content",
                          tags$div(
                            id = "images", 
                            tags$img(
                              src = "SpeciesCat.jpg",
                              width = "auto"
                            )
                          )
                        )
               )
      )
    )
  ),
  
# SERVER #####
  server = function(input, output) {
    
  ## reactive objects ####
    SpDataframe <- reactive({
      as.data.frame(read_csv('sg_ref.csv')) %>%
        filter(desirability < 4)
    })
    sg_ref <- reactive({
      
      sg_ref <- SpDataframe() %>%
        filter(!!sym(input$region) == 1)
      
    })
    
      primesp <- reactive({
        sg_ref() %>%
        filter(desirability == 1) %>%
        pull(cname,Gen_sp)
      })
      secsp <- reactive({sg_ref() %>%
        filter(desirability == 2) %>%
        pull(cname,Gen_sp)
      })
      tertsp <- reactive({
        sg_ref() %>%
        filter(desirability == 3) %>%
        pull(cname,Gen_sp)
      })
      hwdf <-reactive({
        sg_ref() %>%
        filter(HW_SW == "HW")
      })
      swdf <- reactive({
        sg_ref() %>%
        filter(HW_SW == 'SW')
      })
    datasetInput <- reactive({
      file <- input$FVS_Sheet
      
      ext <- tools::file_ext(file$datapath)
      validate(need(ext == "csv", 
                    "Please Upload a csv File,  Results Will Appear Here"))
      
      vroom(file$datapath, col_types = c("MgmtID" = "f","StandID" = "f", "Species" = "f"))
      
    })
    primaryInput <- reactive({
      primarySp <- input$prime
    })
    SecondaryInput <- reactive({
      secondarySp <- input$sec
    })
    TertiaryInput <- reactive({
      tertiarySp <- input$tert
    })
    
    hw_inputs <- reactive({
      # Apply the function to each row of the dataframe to create input pairs
      inputhw_pairs_ui <- lapply(1:nrow(hwdf()), function(i) {
        createhw_input_pair_ui(hwdf()$cname[i], hwdf()$Gen_sp[i])
      })
      
      # Combine input pairs into a single UI element
      do.call(tagList, inputhw_pairs_ui)
    })
    
    sw_inputs <- reactive({
      # Apply the function to each row of the dataframe to create input pairs
      inputsw_pairs_ui <- lapply(1:nrow(swdf()), function(i) {
        createsw_input_pair_ui(swdf()$cname[i], swdf()$Gen_sp[i])
      })
      
      # Combine input pairs into a single UI element
      do.call(tagList, inputsw_pairs_ui)
    })
    
    perhw_inputs <- reactive({
      # Apply the function to each row of the dataframe to create input pairs
      inputhw_pairs_ui <- lapply(1:nrow(hwdf()), function(i) {
        createhwslider_input_pair_ui(hwdf()$cname[i], hwdf()$Gen_sp[i])
      })
      
      # Combine input pairs into a single UI element
      do.call(tagList, inputhw_pairs_ui)
    })
    
    persw_inputs <- reactive({
      # Apply the function to each row of the dataframe to create input pairs
      inputsw_pairs_ui <- lapply(1:nrow(swdf()), function(i) {
        createswslider_input_pair_ui(swdf()$cname[i], swdf()$Gen_sp[i])
      })
      
      # Combine input pairs into a single UI element
      do.call(tagList, inputsw_pairs_ui)
    })
    
    SpeciesDataInput <- reactive({
      
      # sg_ref <- read_xlsx('SG_ref.xlsx',sheet = 2)
      # data.frame(sp = sg_ref %>% pull(Gen_sp))
      splist <- sg_ref() %>% pull(Gen_sp)
      # Use lapply to iterate over the input IDs
      data_list <- lapply(splist, function(id) {
        # Check if the input has a non-null value
        if (!is.null(input[[id]])){
          # If value is not null, return a dataframe row
          data.frame(Gen_sp = id, AGSValue = input[[id]],
                     UGSValue = ifelse(input$BAInput != 'Basal Area percentage',
                                       input[[paste(id,'UGS',sep = "")]],
                                       (1-input[[paste(id,'AGS',sep = '')]])*input[[id]]),
                     AGSProp = ifelse(input$BAInput == 'Basal Area percentage',
                                      input[[id]]*input[[paste(id,"AGS",sep = '')]],
                                      0))
        } else {
          # If value is null, return NULL
          NULL
        }
      })
      
      # Use Filter to remove NULL entries
      data <- do.call(rbind, Filter(function(x) !is.null(x), data_list)) 
      
      data <- data %>% 
        mutate(AGSBA = case_when(input$BAInput == 'Prism Plot Data'~ (AGSValue*input$BAF)/input$Nsweeps,
                                 input$BAInput == 'Basal Area percentage'~ AGSProp*input$totBA,
                                 TRUE~AGSValue),
               UGSBA = case_when(input$BAInput == 'Prism Plot Data'~ (UGSValue*input$BAF)/input$Nsweeps,
                                 input$BAInput == 'Basal Area percentage'~ UGSValue*input$totBA,
                                 TRUE~UGSValue),
               TOTBA = AGSBA+UGSBA) %>% 
        filter(AGSValue!=0) %>% 
        left_join(sg_ref(),by = join_by(Gen_sp))
      
      return(data)
    })
  ## renderUI inputs ####
    output$basalArea <- renderUI({
      if (is.null(input$BAInput))
        return()
      
      switch(input$BAInput,
             "Prism Plot Data" = numericInput("BAF","1a. Basal Area Factor",value = 5), 
             
             "Known Basal Area by Species" = NULL,
             
             "Basal Area percentage" = NULL
             
      )
    })
    output$basalArea2 <- renderUI({
      if (is.null(input$BAInput))
        return()
      
      switch(input$BAInput,
             "Prism Plot Data" = numericInput("Nsweeps","1b. Number of prism points", value = 1),
             
             "Known Basal Area by Species" = NULL,
             
             "Basal Area percentage" = NULL
             
      )
    })
    
    output$totalBA <- renderUI({
      if (is.null(input$BAInput))
        return()
      
      switch(input$BAInput,
             "Prism Plot Data" = NULL,
             
             "Known Basal Area by Species" = numericInput("totBA","1a. Total Basal Area of Stand", value = 0),
             
             "Basal Area percentage" = numericInput("totBA","1a. Total Basal Area of Stand", value = 0)
             
      )
    })
    
    output$KnownBANote <- renderUI({
      if (is.null(input$BAInput))
        return()
      
      switch(input$BAInput,
             "Prism Plot Data" = fluidRow(
               h5(strong("3. Input Tallied Trees by species")),
               column(6, align = "center", h3(strong("Hardwood"))),
               column(6, align = "center", h3(strong("Softwood")))
             ),
             
             "Known Basal Area by Species" = fluidRow(
               h5(strong("3. Input Basal Area by Species")),
               column(6, align = "center", h3(strong("Hardwood"))),
               column(6, align = "center", h3(strong("Softwood")))
             ),
             "Basal Area percentage" = fluidRow(
               h5(strong("3. Input Percent (in decimal) Basal Area by Species")),
               column(6, align = "center", h3(strong("Hardwood"))),
               column(6, align = "center", h3(strong("Softwood")))
             )
             
      )
    })
    
    output$ui <- renderUI({
      if (is.null(input$dbhInput))
        return()
      
      switch(input$dbhInput,
             "Stand Class Size" = selectInput("DBH","2a. Average Size Class in Plot",
                                              choices = c('Sapling (1-5")',
                                                          'Pole (5-9")',
                                                          'Small Sawtimber (9-13")',
                                                          'Large Sawtimber (>13")')),
             
             "Mean DBH of Stand" =  numericInput("exactDBH","2a. Mean DBH of Stand", 0)
             
      )
    })
    

    output$HWTrDtInputs <- renderUI({
      if (is.null(input$BAInput))
        return()
      
      switch(input$BAInput,
             "Prism Plot Data" = hw_inputs(),
             "Known Basal Area by Species" = hw_inputs(),
             'Basal Area percentage' = perhw_inputs()
      )
    })
    output$SWTrDtInputs <- renderUI({
      if (is.null(input$BAInput))
        return()
      
      switch(input$BAInput,
             "Prism Plot Data" = sw_inputs(),
             "Known Basal Area by Species" = sw_inputs(),
             'Basal Area percentage' = persw_inputs()
      )
    })
  
  output$list <- 
    renderUI({
      fluidRow(align = "center",
               bucket_list(
                 header = c(""),
                 add_rank_list(
                   text = "Primary",
                   labels = primesp(),
                   input_id = "prime"
                 ),
                 add_rank_list(
                   text = "Secondary",
                   labels = secsp(),
                   input_id = "sec"
                 ),
                 add_rank_list(
                   text = "Tertiary",
                   labels = tertsp(),
                   input_id = "tert"
                 ),
                 
                 group_name = "bucket_list_group"
               )
      )
    })
  
  ### prism Tree sp. Inputs ######
  # "Prism Plot Data" = fluidRow(style = "overflow-y:scroll; max-height: 400px; position:relative;",
  #                              column(6, align = "center",
  #                                     h5(strong("American Beech")),
  #                                     h6("(Fagus grandifolia)"),
  #                                     column(6,align = "right",numericInput("Fagus_grandifolia", "AGS", 0, min = 0,width = validateCssUnit(100))),
  #                                     column(6,align = "left",numericInput("Fagus_grandifoliaUGS", "UGS", 0, min = 0,width = validateCssUnit(100))),
  #                                     
  #                                     h5(strong("American Elm")),
  #                                     h6("(Ulmus americana)"),
  #                                     column(6,align = "right",numericInput("Ulmus_americana", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     column(6,align = "left",numericInput("Ulmus_americanaUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     
  #                                     h5(strong("Balsam Poplar")),
  #                                     h6("(Populus balsamifera)"),
  #                                     column(6,align = "right", numericInput("Populus_balsamifera", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     column(6,align = "left",numericInput("Populus_balsamiferaUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     
  #                                     h5(strong("Bigtooth Aspen")),
  #                                     h6("(Populus grandidentata)"),
  #                                     column(6,align = "right",numericInput("Populus_grandidentata", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     column(6,align = "left",numericInput("Populus_grandidentataUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     
  #                                     h5(strong("Black Birch")),
  #                                     h6("(Betula lenta)"),
  #                                     column(6,align = "right", numericInput("Betula_lenta", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     column(6,align = "left",numericInput("Betula_lentaUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     
  #                                     h5(strong("Black Cherry")),
  #                                     h6("(Prunus serotina)"),
  #                                     column(6,align = "right",numericInput("Prunus_serotina", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     column(6,align = "left",numericInput("Prunus_serotinaUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     
  #                                     h5(strong("Grey Birch")),
  #                                     h6("(Betual populifolia)"),
  #                                     column(6,align = "right",numericInput("Betula_populifolia", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     column(6,align = "left",numericInput("Betula_populifoliaUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     
  #                                     h5(strong("Quaking Aspen")),
  #                                     h6("(Populaus tremuloides)"),
  #                                     column(6,align = "right",numericInput("Populus_tremuloides", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     column(6,align = "left",numericInput("Populus_tremuloidesUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     
  #                                     h5(strong("Red Maple")),
  #                                     h6("(Acer rubrum)"),
  #                                     column(6,align = "right",numericInput("Acer_rubrum", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     column(6,align = "left",numericInput("Acer_rubrumUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     
  #                                     h5(strong("Red Oak")),
  #                                     h6("(Quercus rubra)"),
  #                                     column(6,align = "right",numericInput("Quercus_rubra", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     column(6,align = "left",numericInput("Quercus_rubraUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     
  #                                     h5(strong("Sugar Maple")),
  #                                     h6("(Acer saccharum)"),
  #                                     column(6,align = "right",numericInput("Acer_saccharum", "AGS", 0, min = 0, width =  validateCssUnit(100))),
  #                                     column(6,align = "left",numericInput("Acer_saccharumUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     
  #                                     h5(strong("White Ash")),
  #                                     h6("(Fraxinus americana)"),
  #                                     column(6,align = "right",numericInput("Fraxinus_americana", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     column(6,align = "left",numericInput("Fraxinus_americanaUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     
  #                                     h5(strong("White Birch")),
  #                                     h6("(Betula papyrifera)"),
  #                                     column(6,align = "right",numericInput("Betula_papyrifera", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     column(6,align = "left",numericInput("Betula_papyriferaUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     
  #                                     h5(strong("White Oak")),
  #                                     h6("(Quercus alba)"),
  #                                     column(6,align = "right",numericInput("Quercus_alba", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     column(6,align = "left",numericInput("Quercus_albaUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     
  #                                     h5(strong("Yellow Birch")),
  #                                     h6("(Betula alleghaniensis)"),
  #                                     column(6,align = "right",numericInput("Betula_alleghaniensis", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     column(6,align = "left",numericInput("Betula_alleghaniensisUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                              ),
  #                              column(6, align = "center",
  #                                     h5(strong("Arborvitae")),
  #                                     h6("(Thuja occidentailis)"),
  #                                     column(6,align = "right",numericInput("Thuja_occidentalis", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     column(6,align = "left",numericInput("Thuja_occidentalisUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     
  #                                     h5(strong("Balsam Fir")),
  #                                     h6("(Abies balsamea)"),
  #                                     column(6,align = "right",numericInput("Abies_balsamea", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     column(6,align = "left",numericInput("Abies_balsameaUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     
  #                                     h5(strong("Black Spruce")),
  #                                     h6("(Picea Mariana)"),
  #                                     column(6,align = "right",numericInput("Picea_mariana", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     column(6,align = "left",numericInput("Picea_marianaUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     
  #                                     h5(strong("Eastern Hemlock")),
  #                                     h6("(Tsuga canadensis)"),
  #                                     column(6,align = "right",numericInput("Tsuga_canadensis", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     column(6,align = "left",numericInput("Tsuga_canadensisUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     
  #                                     h5(strong("Eastern Tamarack")),
  #                                     h6("(Larix laricina)"),
  #                                     column(6,align = "right",numericInput("Larix_laricina", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     column(6,align = "left",numericInput("Larix_laricinaUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     
  #                                     h5(strong("Red Pine")),
  #                                     h6("(Pinus resinosa)"),
  #                                     column(6,align = "right",numericInput("Pinus_resinosa", "AGS", 0, min = 0, width = validateCssUnit(150))),
  #                                     column(6,align = "left",numericInput("Pinus_resinosaUGS", "UGS", 0, min = 0, width =  validateCssUnit(150))),
  #                                     
  #                                     h5(strong("Red Spruce")),
  #                                     h6("(Picea rubens)"),
  #                                     column(6,align = "right",numericInput("Picea_rubens", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     column(6,align = "left",numericInput("Picea_rubensUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     
  #                                     h5(strong("White Pine")),
  #                                     h6("(Pinus strobus)"),
  #                                     column(6,align = "right",numericInput("Pinus_strobus", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     column(6,align = "left",numericInput("Pinus_strobusUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     
  #                                     h5(strong("White Spruce")),
  #                                     h6("(Picea glauca)"),
  #                                     column(6,align = "right",numericInput("Picea_glauca", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     column(6,align = "left",numericInput("Picea_glaucaUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                     
  #                                     
  #                              )
  # ),
  ### Known BATree sp. Inputs ######
  # "Known Basal Area by Species" =  fluidRow(style = "overflow-y:scroll; max-height: 400px; position:relative;",
  #                                           column(6, align = "center",
  #                                                  h5(strong("American Beech")),
  #                                                  h6("(Fagus grandifolia)"),
  #                                                  column(6,align = "right",numericInput("Fagus_grandifolia", "AGS", 0, min = 0,width = validateCssUnit(100))),
  #                                                  column(6,align = "left",numericInput("Fagus_grandifoliaUGS", "UGS", 0, min = 0,width = validateCssUnit(100))),
  #                                                  
  #                                                  h5(strong("American Elm")),
  #                                                  h6("(Ulmus americana)"),
  #                                                  column(6,align = "right",numericInput("Ulmus_americana", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  column(6,align = "left",numericInput("Ulmus_americanaUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  
  #                                                  h5(strong("Balsam Poplar")),
  #                                                  h6("(Populus balsamifera)"),
  #                                                  column(6,align = "right", numericInput("Populus_balsamifera", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  column(6,align = "left",numericInput("Populus_balsamiferaUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  
  #                                                  h5(strong("Bigtooth Aspen")),
  #                                                  h6("(Populus grandidentata)"),
  #                                                  column(6,align = "right",numericInput("Populus_grandidentata", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  column(6,align = "left",numericInput("Populus_grandidentataUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  
  #                                                  h5(strong("Black Birch")),
  #                                                  h6("(Betula lenta)"),
  #                                                  column(6,align = "right", numericInput("Betula_lenta", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  column(6,align = "left",numericInput("Betula_lentaUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  
  #                                                  h5(strong("Black Cherry")),
  #                                                  h6("(Prunus serotina)"),
  #                                                  column(6,align = "right",numericInput("Prunus_serotina", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  column(6,align = "left",numericInput("Prunus_serotinaUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  
  #                                                  h5(strong("Grey Birch")),
  #                                                  h6("(Betual populifolia)"),
  #                                                  column(6,align = "right",numericInput("Betula_populifolia", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  column(6,align = "left",numericInput("Betula_populifoliaUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  
  #                                                  h5(strong("Quaking Aspen")),
  #                                                  h6("(Populaus tremuloides)"),
  #                                                  column(6,align = "right",numericInput("Populus_tremuloides", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  column(6,align = "left",numericInput("Populus_tremuloidesUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  
  #                                                  h5(strong("Red Maple")),
  #                                                  h6("(Acer rubrum)"),
  #                                                  column(6,align = "right",numericInput("Acer_rubrum", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  column(6,align = "left",numericInput("Acer_rubrumUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  
  #                                                  h5(strong("Red Oak")),
  #                                                  h6("(Quercus rubra)"),
  #                                                  column(6,align = "right",numericInput("Quercus_rubra", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  column(6,align = "left",numericInput("Quercus_rubraUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  
  #                                                  h5(strong("Sugar Maple")),
  #                                                  h6("(Acer saccharum)"),
  #                                                  column(6,align = "right",numericInput("Acer_saccharum", "AGS", 0, min = 0, width =  validateCssUnit(100))),
  #                                                  column(6,align = "left",numericInput("Acer_saccharumUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  
  #                                                  h5(strong("White Ash")),
  #                                                  h6("(Fraxinus americana)"),
  #                                                  column(6,align = "right",numericInput("Fraxinus_americana", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  column(6,align = "left",numericInput("Fraxinus_americanaUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  
  #                                                  h5(strong("White Birch")),
  #                                                  h6("(Betula papyrifera)"),
  #                                                  column(6,align = "right",numericInput("Betula_papyrifera", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  column(6,align = "left",numericInput("Betula_papyriferaUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  
  #                                                  h5(strong("White Oak")),
  #                                                  h6("(Quercus alba)"),
  #                                                  column(6,align = "right",numericInput("Quercus_alba", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  column(6,align = "left",numericInput("Quercus_albaUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  
  #                                                  h5(strong("Yellow Birch")),
  #                                                  h6("(Betula alleghaniensis)"),
  #                                                  column(6,align = "right",numericInput("Betula_alleghaniensis", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  column(6,align = "left",numericInput("Betula_alleghaniensisUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                           ),
  #                                           column(6, align = "center",
  #                                                  h5(strong("Arborvitae")),
  #                                                  h6("(Thuja occidentailis)"),
  #                                                  column(6,align = "right",numericInput("Thuja_occidentalis", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  column(6,align = "left",numericInput("Thuja_occidentalisUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  
  #                                                  h5(strong("Balsam Fir")),
  #                                                  h6("(Abies balsamea)"),
  #                                                  column(6,align = "right",numericInput("Abies_balsamea", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  column(6,align = "left",numericInput("Abies_balsameaUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  
  #                                                  h5(strong("Black Spruce")),
  #                                                  h6("(Picea Mariana)"),
  #                                                  column(6,align = "right",numericInput("Picea_mariana", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  column(6,align = "left",numericInput("Picea_marianaUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  
  #                                                  h5(strong("Eastern Hemlock")),
  #                                                  h6("(Tsuga canadensis)"),
  #                                                  column(6,align = "right",numericInput("Tsuga_canadensis", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  column(6,align = "left",numericInput("Tsuga_canadensisUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  
  #                                                  h5(strong("Eastern Tamarack")),
  #                                                  h6("(Larix laricina)"),
  #                                                  column(6,align = "right",numericInput("Larix_laricina", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  column(6,align = "left",numericInput("Larix_laricinaUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  
  #                                                  h5(strong("Red Pine")),
  #                                                  h6("(Pinus resinosa)"),
  #                                                  column(6,align = "right",numericInput("Pinus_resinosa", "AGS", 0, min = 0, width = validateCssUnit(150))),
  #                                                  column(6,align = "left",numericInput("Pinus_resinosaUGS", "UGS", 0, min = 0, width =  validateCssUnit(150))),
  #                                                  
  #                                                  h5(strong("Red Spruce")),
  #                                                  h6("(Picea rubens)"),
  #                                                  column(6,align = "right",numericInput("Picea_rubens", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  column(6,align = "left",numericInput("Picea_rubensUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  
  #                                                  h5(strong("White Pine")),
  #                                                  h6("(Pinus strobus)"),
  #                                                  column(6,align = "right",numericInput("Pinus_strobus", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  column(6,align = "left",numericInput("Pinus_strobusUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  
  #                                                  h5(strong("White Spruce")),
  #                                                  h6("(Picea glauca)"),
  #                                                  column(6,align = "right",numericInput("Picea_glauca", "AGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  column(6,align = "left",numericInput("Picea_glaucaUGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
  #                                                  
  #                                                  
  #                                           )
  # ),
  ### BA perc Tree sp. Inputs ######
  # 'Basal Area percentage' =  fluidRow(style = "overflow-y:scroll; max-height: 400px; position:relative;",
  #                                     column(6, align = "center",
  #                                            h5(strong("American Beech")),
  #                                            h6("(Fagus grandifolia)"),
  #                                            column(6,align = "right",numericInput("Fagus_grandifolia", "BA percentage", 0, min = 0, max = 1)),
  #                                            column(6,align = "left", sliderInput("Fagus_grandifoliaAGS", "percentAGS", 0, min = 0, max = 1, step = 0.1)),
  #                                            
  #                                            h5(strong("American Elm")),
  #                                            h6("(Ulmus americana)"),
  #                                            column(6,align = "right",numericInput("Ulmus_americana", "BA percentage", 0, min = 0, max = 1)),
  #                                            column(6,align = "left",sliderInput("Ulmus_americanaAGS", "percentAGS", 0, min = 0, max = 1, step = 0.1)),
  #                                            
  #                                            h5(strong("Balsam Poplar")),
  #                                            h6("(Populus balsamifera)"),
  #                                            column(6,align = "right", numericInput("Populus_balsamifera", "BA percentage", 0, min = 0, max = 1)),
  #                                            column(6,align = "left",sliderInput("Populus_balsamiferaAGS", "percentAGS", 0, min = 0, max = 1, step = 0.1)),
  #                                            
  #                                            h5(strong("Bigtooth Aspen")),
  #                                            h6("(Populus grandidentata)"),
  #                                            column(6,align = "right",numericInput("Populus_grandidentata", "BA percentage", 0, min = 0, max = 1)),
  #                                            column(6,align = "left",sliderInput("Populus_grandidentataAGS", "percentAGS", 0, min = 0, max = 1, step = 0.1)),
  #                                            
  #                                            h5(strong("Black Birch")),
  #                                            h6("(Betula lenta)"),
  #                                            column(6,align = "right", numericInput("Betula_lenta", "BA percentage", 0, min = 0, max = 1)),
  #                                            column(6,align = "left",sliderInput("Betula_lentaAGS", "percentAGS", 0, min = 0, max = 1, step = 0.1)),
  #                                            
  #                                            h5(strong("Black Cherry")),
  #                                            h6("(Prunus serotina)"),
  #                                            column(6,align = "right",numericInput("Prunus_serotina", "BA percentage", 0, min = 0, max = 1)),
  #                                            column(6,align = "left",sliderInput("Prunus_serotinaAGS", "percentAGS", 0, min = 0, max = 1, step = 0.1)),
  #                                            
  #                                            h5(strong("Grey Birch")),
  #                                            h6("(Betual populifolia)"),
  #                                            column(6,align = "right",numericInput("Betula_populifolia", "BA percentage", 0, min = 0, max = 1)),
  #                                            column(6,align = "left",sliderInput("Betula_populifoliaAGS", "percentAGS", 0, min = 0, max = 1, step = 0.1)),
  #                                            
  #                                            h5(strong("Quaking Aspen")),
  #                                            h6("(Populaus tremuloides)"),
  #                                            column(6,align = "right",numericInput("Populus_tremuloides", "BA percentage", 0, min = 0, max = 1)),
  #                                            column(6,align = "left",sliderInput("Populus_tremuloidesAGS", "percentAGS", 0, min = 0, max = 1, step = 0.1)),
  #                                            
  #                                            h5(strong("Red Maple")),
  #                                            h6("(Acer rubrum)"),
  #                                            column(6,align = "right",numericInput("Acer_rubrum", "BA percentage", 0, min = 0, max = 1)),
  #                                            column(6,align = "left",sliderInput("Acer_rubrumAGS", "percentAGS", 0, min = 0, max = 1, step = 0.1)),
  #                                            
  #                                            h5(strong("Red Oak")),
  #                                            h6("(Quercus rubra)"),
  #                                            column(6,align = "right",numericInput("Quercus_rubra", "BA percentage", 0, min = 0, max = 1)),
  #                                            column(6,align = "left",sliderInput("Quercus_rubraAGS", "percentAGS", 0, min = 0, max = 1, step = 0.1)),
  #                                            
  #                                            h5(strong("Sugar Maple")),
  #                                            h6("(Acer saccharum)"),
  #                                            column(6,align = "right",numericInput("Acer_saccharum", "BA percentage", 0, min = 0, max = 1)),
  #                                            column(6,align = "left",sliderInput("Acer_saccharumAGS", "percentAGS", 0, min = 0, max = 1, step = 0.1)),
  #                                            
  #                                            h5(strong("White Ash")),
  #                                            h6("(Fraxinus americana)"),
  #                                            column(6,align = "right",numericInput("Fraxinus_americana", "BA percentage", 0, min = 0, max = 1)),
  #                                            column(6,align = "left",sliderInput("Fraxinus_americanaAGS", "percentAGS", 0, min = 0, max = 1, step = 0.1)),
  #                                            
  #                                            h5(strong("White Birch")),
  #                                            h6("(Betula papyrifera)"),
  #                                            column(6,align = "right",numericInput("Betula_papyrifera", "BA percentage", 0, min = 0, max = 1)),
  #                                            column(6,align = "left",sliderInput("Betula_papyriferaAGS", "percentAGS", 0, min = 0, max = 1, step = 0.1)),
  #                                            
  #                                            h5(strong("White Oak")),
  #                                            h6("(Quercus alba)"),
  #                                            column(6,align = "right",numericInput("Quercus_alba", "BA percentage", 0, min = 0, max = 1)),
  #                                            column(6,align = "left",sliderInput("Quercus_albaAGS", "percentAGS", 0, min = 0, max = 1, step = 0.1)),
  #                                            
  #                                            h5(strong("Yellow Birch")),
  #                                            h6("(Betula alleghaniensis)"),
  #                                            column(6,align = "right",numericInput("Betula_alleghaniensis", "BA percentage", 0, min = 0, max = 1)),
  #                                            column(6,align = "left",sliderInput("Betula_alleghaniensisAGS", "percentAGS", 0, min = 0, max = 1, step = 0.1)),
  #                                     ),
  #                                     column(6, align = "center",
  #                                            h5(strong("Arborvitae")),
  #                                            h6("(Thuja occidentailis)"),
  #                                            column(6,align = "right",numericInput("Thuja_occidentalis", "BA percentage", 0, min = 0, max = 1)),
  #                                            column(6,align = "left",sliderInput("Thuja_occidentalisAGS", "percentAGS", 0, min = 0, max = 1, step = 0.1)),
  #                                            
  #                                            h5(strong("Balsam Fir")),
  #                                            h6("(Abies balsamea)"),
  #                                            column(6,align = "right",numericInput("Abies_balsamea", "BA percentage", 0, min = 0, max = 1)),
  #                                            column(6,align = "left",sliderInput("Abies_balsameaAGS", "percentAGS", 0, min = 0, max = 1, step = 0.1)),
  #                                            
  #                                            h5(strong("Black Spruce")),
  #                                            h6("(Picea Mariana)"),
  #                                            column(6,align = "right",numericInput("Picea_mariana", "BA percentage", 0, min = 0, max = 1)),
  #                                            column(6,align = "left",sliderInput("Picea_marianaAGS", "percentAGS", 0, min = 0, max = 1, step = 0.1)),
  #                                            
  #                                            h5(strong("Eastern Hemlock")),
  #                                            h6("(Tsuga canadensis)"),
  #                                            column(6,align = "right",numericInput("Tsuga_canadensis", "BA percentage", 0, min = 0, max = 1)),
  #                                            column(6,align = "left",sliderInput("Tsuga_canadensisAGS", "percentAGS", 0, min = 0, max = 1, step = 0.1)),
  #                                            
  #                                            h5(strong("Eastern Tamarack")),
  #                                            h6("(Larix laricina)"),
  #                                            column(6,align = "right",numericInput("Larix_laricina", "BA percentage", 0, min = 0, max = 1)),
  #                                            column(6,align = "left",sliderInput("Larix_laricinaAGS", "percentAGS", 0, min = 0, max = 1, step = 0.1)),
  #                                            
  #                                            h5(strong("Red Pine")),
  #                                            h6("(Pinus resinosa)"),
  #                                            column(6,align = "right",numericInput("Pinus_resinosa", "BA percentage", 0, min = 0, max = 1)),
  #                                            column(6,align = "left",sliderInput("Pinus_resinosaAGS", "percentAGS", 0, min = 0, max = 1, step = 0.1))),
  #                                            
  #                                            h5(strong("Red Spruce")),
  #                                            h6("(Picea rubens)"),
  #                                            column(6,align = "right",numericInput("Picea_rubens", "BA percentage", 0, min = 0, max = 1)),
  #                                            column(6,align = "left",sliderInput("Picea_rubensAGS", "percentAGS", 0, min = 0, max = 1, step = 0.1)),
  #                                            
  #                                            h5(strong("White Pine")),
  #                                            h6("(Pinus strobus)"),
  #                                            column(6,align = "right",numericInput("Pinus_strobus", "BA percentage", 0, min = 0, max = 1)),
  #                                            column(6,align = "left",sliderInput("Pinus_strobusAGS", "percentAGS", 0, min = 0, max = 1, step = 0.1)),
  #                                            
  #                                            h5(strong("White Spruce")),
  #                                            h6("(Picea glauca)"),
  #                                            column(6,align = "right",numericInput("Picea_glauca", "BA percentage", 0, min = 0,max = 1)),
  #                                            column(6,align = "left",sliderInput("Picea_glaucaAGS", "percentAGS", 0, min = 0,max = 1, step = 0.1))
  #                                     )
  # )
  
 
  ## Render Plot #####      
    output$plot  <- renderPlotly({
      validate(
        need(sum(SpeciesDataInput()$AGSValue)>0, 
             "Please Input Tree Species To Calculate Degradation Class
                     Results Will Appear Here")
      )

      
      # if(input$BAInput == 'Basal Area Percentage'){
        
        # PlotSp <- c(input$'Pinus_resinosa'*input$totBA,         input$'Acer_saccharum'*input$totBA,        
        #             input$'Betula_alleghaniensis'*input$totBA,  input$'Betula_papyrifera'*input$totBA,     
        #             input$'Pinus_strobus'*input$totBA,          input$'Quercus_rubra'*input$totBA,         
        #             input$'Prunus_serotina'*input$totBA,        input$'Quercus_alba'*input$totBA,          
        #             input$'Acer_rubrum'*input$totBA,            input$'Tsuga_canadensis'*input$totBA,      
        #             input$'Thuja_occidentalis'*input$totBA,     input$'Fraxinus_americana'*input$totBA,    
        #             input$'Picea_mariana'*input$totBA,          input$'Picea_glauca'*input$totBA,          
        #             input$'Betula_lenta'*input$totBA,           input$'Abies_balsamea'*input$totBA,        
        #             input$'Fagus_grandifolia'*input$totBA,      input$'Populus_tremuloides'*input$totBA,   
        #             input$'Betula_populifolia'*input$totBA,     input$'Populus_grandidentata'*input$totBA, 
        #             input$'Populus_balsamifera'*input$totBA,    input$'Ulmus_americana'*input$totBA,       
        #             input$'Picea_rubens'*input$totBA,           input$'Larix_laricina'*input$totBA,        
        #             )
        # 
        # c(input$'Pinus_resinosa'*input$totalBA,         (1-input$PinusResinosa_UGS)*input$'Pinus_resinosa'*input$totalBA,               input$'Acer_saccharum'*input$totalBA,        (1-input$AcerSaccharum_UGS)*input$'Acer_saccharum'*input$totalBA,
        #   input$'Betula_alleghaniensis'*input$totalBA,  (1-input$Betulaalleghaniensis_UGS)*input$'Betula_alleghaniensis'*input$totalBA, input$'Betula_papyrifera'*input$totalBA,     (1-input$BetulaPapyrifera_UGS)*input$'Betula_papyrifera'*input$totalBA,
        #   input$'Pinus_strobus'*input$totalBA,          (1-input$PinusStrobus_UGS)*input$'Pinus_strobus'*input$totalBA,                 input$'Quercus_rubra'*input$totalBA,         (1-input$QuercusRubra_UGS)*input$'Quercus_rubra'*input$totalBA,
        #   input$'Prunus_serotina'*input$totalBA,        (1-input$PrunusSerotina_UGS)*input$'Prunus_serotina'*input$totalBA,             input$'Quercus_alba'*input$totalBA,          (1-input$QuercusAlba_UGS)*input$'Quercus_alba'*input$totalBA,
        #   input$'Acer_rubrum'*input$totalBA,            (1-input$AcerRubrum_UGS)*input$'Acer_rubrum'*input$totalBA,                     input$'Tsuga_canadensis'*input$totalBA,      (1-input$TsugaCanadensis_UGS)*input$'Tsuga_canadensis'*input$totalBA,
        #   input$'Thuja_occidentalis'*input$totalBA,     (1-input$ThujaOccidentalis_UGS)*input$'Thuja_occidentalis'*input$totalBA,       input$'Fraxinus_americana'*input$totalBA,    (1-input$FraxinusAmericana_UGS)*input$'Fraxinus_americana'*input$totalBA,
        #   input$'Picea_mariana'*input$totalBA,          (1-input$PiceaMariana_UGS)*input$'Picea_mariana'*input$totalBA,                 input$'Picea_glauca'*input$totalBA,          (1-input$PiceaGlauca_UGS)*input$'Picea_glauca'*input$totalBA,
        #   input$'Betula_lenta'*input$totalBA,           (1-input$BetulaLenta_UGS)*input$'Betula_lenta'*input$totalBA,                   input$'Abies_balsamea'*input$totalBA,        (1-input$AbiesBalsamea_UGS)*input$'Abies_balsamea'*input$totalBA,
        #   input$'Fagus_grandifolia'*input$totalBA,      (1-input$FagusGrandifolia_UGS)*input$'Fagus_grandifolia'*input$totalBA,         input$'Populus_tremuloides'*input$totalBA,   (1-input$PopulusTremuloides_UGS)*input$'Populus_tremuloides'*input$totalBA,
        #   input$'Betula_populifolia'*input$totalBA,     (1-input$BetulaPopulifolia_UGS)*input$'Betula_populifolia'*input$totalBA,       input$'Populus_grandidentata'*input$totalBA, (1-input$PopulusGrandidentata_UGS)*input$'Populus_grandidentata'*input$totalBA,
        #   input$'Populus_balsamifera'*input$totalBA,    (1-input$PopulusBalsamifera_UGS)*input$'Populus_balsamifera'*input$totalBA,     input$'Ulmus_americana'*input$totalBA,       (1-input$UlmusAmericana_UGS)*input$'Ulmus_americana'*input$totalBA,
        #   input$'Picea_rubens'*input$totalBA,           (1-input$PiceaRubens_UGS)*input$'Picea_rubens'*input$totalBA,                   input$'Larix_laricina'*input$totalBA,        (1-input$LarixLaricina_UGS)*input$'Larix_laricina'*input$totalBA,
        # )
        # }else{
        #   PlotSp <- c(input$'Pinus_resinosa',         input$PinusResinosa_UGS,        input$'Acer_saccharum',        input$AcerSaccharum_UGS,
        #               input$'Betula_alleghaniensis',  input$Betulaalleghaniensis_UGS, input$'Betula_papyrifera',     input$BetulaPapyrifera_UGS,
        #               input$'Pinus_strobus',          input$PinusStrobus_UGS,         input$'Quercus_rubra',         input$QuercusRubra_UGS,
        #               input$'Prunus_serotina',        input$PrunusSerotina_UGS,       input$'Quercus_alba',          input$QuercusAlba_UGS,
        #               input$'Acer_rubrum',            input$AcerRubrum_UGS,           input$'Tsuga_canadensis',      input$TsugaCanadensis_UGS,
        #               input$'Thuja_occidentalis',     input$ThujaOccidentalis_UGS,    input$'Fraxinus_americana',    input$FraxinusAmericana_UGS,
        #               input$'Picea_mariana',          input$PiceaMariana_UGS,         input$'Picea_glauca',          input$PiceaGlauca_UGS,
        #               input$'Betula_lenta',           input$BetulaLenta_UGS,          input$'Abies_balsamea',        input$AbiesBalsamea_UGS,
        #               input$'Fagus_grandifolia',      input$FagusGrandifolia_UGS,     input$'Populus_tremuloides',   input$PopulusTremuloides_UGS,
        #               input$'Betula_populifolia',     input$BetulaPopulifolia_UGS,    input$'Populus_grandidentata', input$PopulusGrandidentata_UGS,
        #               input$'Populus_balsamifera',    input$PopulusBalsamifera_UGS,   input$'Ulmus_americana',       input$UlmusAmericana_UGS,
        #               input$'Picea_rubens',           input$PiceaRubens_UGS,          input$'Larix_laricina',        input$LarixLaricina_UGS)
        # }
      
      
      # names(PlotSp) <- sp_Names
      Avg_SG <- SpeciesDataInput() %>% 
        mutate(wtSG = sg*TOTBA) %>% 
        reframe(Avg_SG = sum(wtSG)/sum(TOTBA)) %>% 
        pull(Avg_SG)
      
      # Avg_SG <- sum(weight_SG)/sum(SpeciesDataInput()$TOTBA)
      
      if(input$DBH == 'Sapling (1-5")'){
        DBHin <- 3.5
      }else if(input$DBH == 'Pole (5-9")'){
        DBHin <- 7
      }else if(input$DBH == 'Small Sawtimber (9-13")'){
        DBHin <- 11
      }else if(input$DBH == 'Large Sawtimber (>13")'){
        DBHin <- 14
      }else {
        NA
      }
      
      if(input$dbhInput == "Stand Class Size"){
        c_line <- round(C_lineBA(DBHin, Avg_SG),2)
        b_line <- round(B_lineBA(DBHin, Avg_SG),2)
        a_line<- round(A_lineBA(DBHin, Avg_SG),2)
      }else{
        c_line <- round(C_lineBA(input$exactDBH, Avg_SG),2)
        b_line <- round(B_lineBA(input$exactDBH, Avg_SG),2)
        a_line<- round(A_lineBA(input$exactDBH, Avg_SG),2)
      }
      
      # if(input$BAInput == 'Prism Plot Data'){
      #   
      #   plot_data <- sapply(input$bucket_list_group, function(category){
      #     ifelse(length(category) == 0, 0,
      #            sum(sapply(category, function(item) input[[item]]))*(input$BAF/input$Nsweeps))
      #   })
      #   
      #   UGS <- sum(input$PinusResinosa_UGS,input$AcerSaccharum_UGS,input$Betulaalleghaniensis_UGS,input$BetulaPapyrifera_UGS,input$PinusStrobus_UGS,
      #              input$QuercusRubra_UGS,input$PrunusSerotina_UGS,input$QuercusAlba_UGS,input$AcerRubrum_UGS,input$TsugaCanadensis_UGS,
      #              input$ThujaOccidentalis_UGS,input$FraxinusAmericana_UGS,input$PiceaMariana_UGS,input$PiceaGlauca_UGS,input$BetulaLenta_UGS,input$AbiesBalsamea_UGS,
      #              input$FagusGrandifolia_UGS,input$PopulusTremuloides_UGS,input$BetulaPopulifolia_UGS,input$PopulusGrandidentata_UGS,input$PopulusBalsamifera_UGS,
      #              input$UlmusAmericana_UGS,input$PiceaRubens_UGS, input$LarixLaricina_UGS)*(input$BAF/input$Nsweeps)
      #   
      # }
      # else if(input$BAInput == 'Known Basal Area by Species'){
      #   plot_data <- sapply(input$bucket_list_group, function(category){
      #     ifelse(length(category) == 0, 0,
      #            sum(sapply(category, function(item) input[[item]])))
      #   })
      #   
      #   UGS <- c(input$PinusResinosa_UGS,
      #            input$AcerSaccharum_UGS,
      #            input$Betulaalleghaniensis_UGS,
      #            input$BetulaPapyrifera_UGS,
      #            input$PinusStrobus_UGS,
      #            input$QuercusRubra_UGS,
      #            input$PrunusSerotina_UGS,
      #            input$QuercusAlba_UGS,
      #            input$AcerRubrum_UGS,
      #            input$TsugaCanadensis_UGS,
      #            input$ThujaOccidentalis_UGS,
      #            input$FraxinusAmericana_UGS,
      #            input$PiceaMariana_UGS,
      #            input$PiceaGlauca_UGS,
      #            input$BetulaLenta_UGS,
      #            input$AbiesBalsamea_UGS,
      #            input$FagusGrandifolia_UGS,
      #            input$PopulusTremuloides_UGS,
      #            input$BetulaPopulifolia_UGS,
      #            input$PopulusGrandidentata_UGS,
      #            input$PopulusBalsamifera_UGS,
      #            input$UlmusAmericana_UGS,
      #            input$PiceaRubens_UGS, 
      #            input$LarixLaricina_UGS)
      # }else{
      #   plot_data <- sapply(input$bucket_list_group, function(category){
      #     ifelse(length(category) == 0, 0,
      #            sum(unlist(sapply(category, function(item) {(input[[item]]*input$totBA)*input[[paste(item,'AGS',sep = '')]]}))))
      #   })
      #   
      #   UGS <- c((1-input$'Pinus_resinosaAGS')*(input$'Pinus_resinosa'*input$totBA),
      #            (1-input$'Acer_saccharumAGS')*(input$'Acer_saccharum'*input$totBA),
      #            (1-input$'Betula_alleghaniensisAGS')*(input$'Betula_alleghaniensis'*input$totBA),
      #            (1-input$'Betula_papyriferaAGS')*(input$'Betula_papyrifera'*input$totBA),
      #            (1-input$'Pinus_strobusAGS')*(input$'Pinus_strobus'*input$totBA),
      #            (1-input$'Quercus_rubraAGS')*(input$'Quercus_rubra'*input$totBA),
      #            (1-input$'Prunus_serotinaAGS')*(input$'Prunus_serotina'*input$totBA),
      #            (1-input$'Quercus_albaAGS')*(input$'Quercus_alba'*input$totBA),
      #            (1-input$'Acer_rubrumAGS')*(input$'Acer_rubrum'*input$totBA),
      #            (1-input$'Tsuga_canadensisAGS')*(input$'Tsuga_canadensis'*input$totBA),
      #            (1-input$'Thuja_occidentalisdAGS')*(input$'Thuja_occidentalis'*input$totBA),
      #            (1-input$'Fraxinus_americanaAGS')*(input$'Fraxinus_americana'*input$totBA),
      #            (1-input$'Picea_marianaAGS')*(input$'Picea_mariana'*input$totBA),
      #            (1-input$'Picea_glaucaAGS')*(input$'Picea_glauca'*input$totBA),
      #            (1-input$'Betula_lentaAGS')*(input$'Betula_lenta'*input$totBA),
      #            (1-input$'Abies_balsameaAGS')*(input$'Abies_balsamea'*input$totBA),
      #            (1-input$'Fagus_grandifoliaAGS')*(input$'Fagus_grandifolia'*input$totBA),
      #            (1-input$'Populus_tremuloidesAGS')*(input$'Populus_tremuloides'*input$totBA),
      #            (1-input$'Betula_populifoliaAGS')*(input$'Betula_populifolia'*input$totBA),
      #            (1-input$'Populus_grandidentataAGS')*(input$'Populus_grandidentata'*input$totBA),
      #            (1-input$'Populus_balsamiferAGS')*(input$'Populus_balsamifer'*input$totBA),
      #            (1-input$'Ulmus_americanaAGS')*(input$'Ulmus_americana'*input$totBA),
      #            (1-input$'Picea_rubensAGS')*(input$'Picea_rubens'*input$totBA), 
      #            (1-input$'Larix_laricinaAGS')*(input$'Larix_laricina'*input$totBA))
      # }
      
      
      # totalBA <- sum(plot_data[1],plot_data[2],plot_data[3],UGS)
      PrimeBA <- sum(SpeciesDataInput() %>% filter(desirability == 1) %>% pull(AGSBA))
      secBA <- sum(SpeciesDataInput() %>% filter(desirability == 2) %>% pull(AGSBA))
      tertBA <- sum(SpeciesDataInput() %>% filter(desirability == 3) %>% pull(AGSBA))
      UgsBA <- sum(SpeciesDataInput() %>% pull(UGSBA))
      totalBA <- sum(SpeciesDataInput() %>% pull(TOTBA))
      
      hline <- function(y = 0, color = "black") {
        list(
          type = "line", 
          x0 = 0, 
          x1 = 1, 
          xref = "paper",
          y0 = y, 
          y1 = y, 
          line = list(color = color)
        )
      }
      
      plot <- plot_ly(x = "Stand", y = PrimeBA, type = "bar", name = "Primary", marker = list(color = 'rgb(110, 235, 131)')) %>%
        add_trace(y = secBA, name = "Secondary", marker = list(color = 'rgb(40, 83, 107)'))%>%
        add_trace(y = tertBA, name = "Tertiary",marker = list(color = 'rgb(217, 131, 36)'))%>%
        add_trace(y = UgsBA, name = "Unacceptable Growing Stock", marker = list(color = 'rgb(140, 28, 19)') )%>%
        layout(yaxis = list(title = "Basal Area"),xaxis = list(title = ""), barmode = "stack")%>%
        layout(shapes = list(hline(c_line,  color = "black"),hline(b_line, color = "black"),hline(a_line, color = "black")))%>%
        add_annotations(x = 0, y = c_line,  xref = "x", yref = "y", text = paste("C-Line ",c_line,"Sq Ft/acre"), showarrow = F, yshift = 10, xshift = "right")%>%
        add_annotations(x = 0, y = b_line,  xref = "x", yref = "y", text = paste("B-Line ",b_line,"Sq Ft/acre"), showarrow = F, yshift = 10)%>%
        add_annotations(x = 0, y = a_line,  xref = "x", yref = "y", text = paste("A-Line ",a_line,"Sq Ft/acre"), showarrow = F, yshift = 10)%>%
        add_annotations(x = 0, y = totalBA, xref = "x", yref = "y", text = paste("Total Basal Area",totalBA,"Sq Ft/acre"), showarrow = F, yshift = 10)%>%
        layout(paper_bgcolor= 'transparent')%>%
        layout(plot_bgcolor = 'transparent')
      
      # plot <- plot_ly(SpeciesDataInput(),x = "Stand", y = plot_data[1], type = "bar", name = "Primary", marker = list(color = 'rgb(110, 235, 131)')) %>%
      #   add_trace(y = plot_data[2], name = "Secondary", marker = list(color = 'rgb(40, 83, 107)'))%>%
      #   add_trace(y = plot_data[3], name = "Tertiary",marker = list(color = 'rgb(217, 131, 36)'))%>%
      #   add_trace(y = ~UGS, name = "Unacceptable Growing Stock", marker = list(color = 'rgb(140, 28, 19)') )%>%
      #   layout(yaxis = list(title = "Basal Area"),xaxis = list(title = ""), barmode = "stack")%>%
      #   layout(shapes = list(hline(c_line,  color = "black"),hline(b_line, color = "black"),hline(a_line, color = "black")))%>%
      #   add_annotations(x = 0, y = c_line,  xref = "x", yref = "y", text = paste("C-Line ",c_line,"Sq Ft/acre"), showarrow = F, yshift = 10, xshift = "right")%>%
      #   add_annotations(x = 0, y = b_line,  xref = "x", yref = "y", text = paste("B-Line ",b_line,"Sq Ft/acre"), showarrow = F, yshift = 10)%>%
      #   add_annotations(x = 0, y = a_line,  xref = "x", yref = "y", text = paste("A-Line ",a_line,"Sq Ft/acre"), showarrow = F, yshift = 10)%>%
      #   add_annotations(x = 0, y = totalBA, xref = "x", yref = "y", text = paste("Total Basal Area",totalBA,"Sq Ft/acre"), showarrow = F, yshift = 10)%>%
      #   layout(paper_bgcolor= 'transparent')%>%
      #   layout(plot_bgcolor = 'transparent')
      
      plot
      
    })
  ## Render Degradation Value ####   
    output$degValue <- renderText({
      validate(
        need(sum(SpeciesDataInput()$AGSValue)>0, 
          # 
          # sum(input$'Pinus_resinosa',input$PinusResinosa_UGS,input$'Acer_saccharum',input$AcerSaccharum_UGS,
          #        input$'Betula_alleghaniensis',input$Betulaalleghaniensis_UGS, input$'Betula_papyrifera',input$BetulaPapyrifera_UGS,
          #        input$'Pinus_strobus',input$PinusStrobus_UGS, input$'Quercus_rubra',input$QuercusRubra_UGS,
          #        input$'Prunus_serotina',input$PrunusSerotina_UGS, input$'Quercus_alba',input$QuercusAlba_UGS,
          #        input$'Acer_rubrum',input$AcerRubrum_UGS, input$'Tsuga_canadensis',input$TsugaCanadensis_UGS,
          #        input$'Thuja_occidentalis',input$ThujaOccidentalis_UGS, input$'Fraxinus_americana',input$FraxinusAmericana_UGS,
          #        input$'Picea_mariana',input$PiceaMariana_UGS, input$'Picea_glauca',input$PiceaGlauca_UGS,
          #        input$'Betula_lenta',input$BetulaLenta_UGS, input$'Abies_balsamea',input$AbiesBalsamea_UGS,
          #        input$'Fagus_grandifolia',input$FagusGrandifolia_UGS, input$'Populus_tremuloides',input$PopulusTremuloides_UGS,
          #        input$'Betula_populifolia',input$BetulaPopulifolia_UGS, input$'Populus_grandidentata',input$PopulusGrandidentata_UGS,
          #        input$'Populus_balsamifera',input$PopulusBalsamifera_UGS, input$'Ulmus_americana',input$UlmusAmericana_UGS,
          #        input$'Picea_rubens', input$PiceaRubens_UGS, input$'Larix_laricina', input$LarixLaricina_UGS) > 0, 
             "Please Input Tree Species To Calculate Degradation Class
                     Results Will Appear Here")
      )
      
    
      # PlotSp <- c(input$'Pinus_resinosa',         input$PinusResinosa_UGS,        input$'Acer_saccharum',        input$AcerSaccharum_UGS,
      #             input$'Betula_alleghaniensis',  input$Betulaalleghaniensis_UGS, input$'Betula_papyrifera',     input$BetulaPapyrifera_UGS,
      #             input$'Pinus_strobus',          input$PinusStrobus_UGS,         input$'Quercus_rubra',         input$QuercusRubra_UGS,
      #             input$'Prunus_serotina',        input$PrunusSerotina_UGS,       input$'Quercus_alba',          input$QuercusAlba_UGS,
      #             input$'Acer_rubrum',            input$AcerRubrum_UGS,           input$'Tsuga_canadensis',      input$TsugaCanadensis_UGS,
      #             input$'Thuja_occidentalis',     input$ThujaOccidentalis_UGS,    input$'Fraxinus_americana',    input$FraxinusAmericana_UGS,
      #             input$'Picea_mariana',          input$PiceaMariana_UGS,         input$'Picea_glauca',          input$PiceaGlauca_UGS,
      #             input$'Betula_lenta',           input$BetulaLenta_UGS,          input$'Abies_balsamea',        input$AbiesBalsamea_UGS,
      #             input$'Fagus_grandifolia',      input$FagusGrandifolia_UGS,     input$'Populus_tremuloides',   input$PopulusTremuloides_UGS,
      #             input$'Betula_populifolia',     input$BetulaPopulifolia_UGS,    input$'Populus_grandidentata', input$PopulusGrandidentata_UGS,
      #             input$'Populus_balsamifera',    input$PopulusBalsamifera_UGS,   input$'Ulmus_americana',       input$UlmusAmericana_UGS,
      #             input$'Picea_rubens',           input$PiceaRubens_UGS,          input$'Larix_laricina',        input$LarixLaricina_UGS)

      ######
      # THIS ONLY WORKS IF WE ARE USING A SINGLE AVERAGE DBH OF ENTIRE STAND!!!
      ######
      Avg_SG <- SpeciesDataInput() %>% 
        mutate(wtSG = sg*TOTBA) %>% 
        reframe(Avg_SG = sum(wtSG)/sum(TOTBA)) %>% 
        pull(Avg_SG)
      
      # Avg_SG <- sum(weight_SG)/sum(SpeciesDataInput()$TOTBA)
      ##############
      if(input$DBH == 'Sapling (1-5")'){
        DBHin <- 3.5
      }else if(input$DBH == 'Pole (5-9")'){
        DBHin <- 7
      }else if(input$DBH == 'Small Sawtimber (9-13")'){
        DBHin <- 11
      }else if(input$DBH == 'Large Sawtimber (>13")'){
        DBHin <- 14
      }else {
        NA
      }
      
      if(input$dbhInput == "Stand Class Size") {
        
        cline <- round(C_lineBA(DBHin, Avg_SG),2)
        
      }
      else{
        
        cline <- round(C_lineBA(input$exactDBH, Avg_SG),2)
        
      }
      
      # if(input$BAInput == 'Prism Plot Data'){
      #   plot_data <- sapply(input$bucket_list_group, function(category){
      #     ifelse(length(category) == 0, 0,
      #            sum(sapply(category, function(item) input[[item]]))*(input$BAF/input$Nsweeps))
      #   
      #   })
      #   
      #   UGS <- sum(input$PinusResinosa_UGS,input$AcerSaccharum_UGS,input$Betulaalleghaniensis_UGS,input$BetulaPapyrifera_UGS,input$PinusStrobus_UGS,
      #              input$QuercusRubra_UGS,input$PrunusSerotina_UGS,input$QuercusAlba_UGS,input$AcerRubrum_UGS,input$TsugaCanadensis_UGS,
      #              input$ThujaOccidentalis_UGS,input$FraxinusAmericana_UGS,input$PiceaMariana_UGS,input$PiceaGlauca_UGS,input$BetulaLenta_UGS,input$AbiesBalsamea_UGS,
      #              input$FagusGrandifolia_UGS,input$PopulusTremuloides_UGS,input$BetulaPopulifolia_UGS,input$PopulusGrandidentata_UGS,input$PopulusBalsamifera_UGS,
      #              input$UlmusAmericana_UGS,input$PiceaRubens_UGS, input$LarixLaricina_UGS)*(input$BAF/input$Nsweeps)
      # 
      # }
      # else if(input$BAInput == 'Known Basal Area by Species'){
      #   plot_data <- sapply(input$bucket_list_group, function(category){
      #     ifelse(length(category) == 0, 0,
      #            sum(sapply(category, function(item) input[[item]])))
      #   })
      #   
      #   UGS <- c(input$PinusResinosa_UGS,input$AcerSaccharum_UGS,input$Betulaalleghaniensis_UGS,input$BetulaPapyrifera_UGS,input$PinusStrobus_UGS,
      #            input$QuercusRubra_UGS,input$PrunusSerotina_UGS,input$QuercusAlba_UGS,input$AcerRubrum_UGS,input$TsugaCanadensis_UGS,
      #            input$ThujaOccidentalis_UGS,input$FraxinusAmericana_UGS,input$PiceaMariana_UGS,input$PiceaGlauca_UGS,input$BetulaLenta_UGS,input$AbiesBalsamea_UGS,
      #            input$FagusGrandifolia_UGS,input$PopulusTremuloides_UGS,input$BetulaPopulifolia_UGS,input$PopulusGrandidentata_UGS,input$PopulusBalsamifera_UGS,
      #            input$UlmusAmericana_UGS,input$PiceaRubens_UGS, input$LarixLaricina_UGS)
      # }else{
      #   plot_data <- sapply(input$bucket_list_group, function(category){
      #     ifelse(length(category) == 0, 0,
      #            sum(unlist(sapply(category, function(item){
      #              (input[[item]]*input$totBA)*input[[paste(item,'AGS',sep = '')]]
      #              }))))
      #   })
      #   
      #   
      #   UGS <- c((1-input$'Pinus_resinosaAGS')*(input$'Pinus_resinosa'*input$totBA),
      #            (1-input$'Acer_saccharumAGS')*(input$'Acer_saccharum'*input$totBA),
      #            (1-input$'Betula_alleghaniensisAGS')*(input$'Betula_alleghaniensis'*input$totBA),
      #            (1-input$'Betula_papyriferaAGS')*(input$'Betula_papyrifera'*input$totBA),
      #            (1-input$'Pinus_strobusAGS')*(input$'Pinus_strobus'*input$totBA),
      #            (1-input$'Quercus_rubraAGS')*(input$'Quercus_rubra'*input$totBA),
      #            (1-input$'Prunus_serotinaAGS')*(input$'Prunus_serotina'*input$totBA),
      #            (1-input$'Quercus_albaAGS')*(input$'Quercus_alba'*input$totBA),
      #            (1-input$'Acer_rubrumAGS')*(input$'Acer_rubrum'*input$totBA),
      #            (1-input$'Tsuga_canadensisAGS')*(input$'Tsuga_canadensis'*input$totBA),
      #            (1-input$'Thuja_occidentalisdAGS')*(input$'Thuja_occidentalis'*input$totBA),
      #            (1-input$'Fraxinus_americanaAGS')*(input$'Fraxinus_americana'*input$totBA),
      #            (1-input$'Picea_marianaAGS')*(input$'Picea_mariana'*input$totBA),
      #            (1-input$'Picea_glaucaAGS')*(input$'Picea_glauca'*input$totBA),
      #            (1-input$'Betula_lentaAGS')*(input$'Betula_lenta'*input$totBA),
      #            (1-input$'Abies_balsameaAGS')*(input$'Abies_balsamea'*input$totBA),
      #            (1-input$'Fagus_grandifoliaAGS')*(input$'Fagus_grandifolia'*input$totBA),
      #            (1-input$'Populus_tremuloidesAGS')*(input$'Populus_tremuloides'*input$totBA),
      #            (1-input$'Betula_populifoliaAGS')*(input$'Betula_populifolia'*input$totBA),
      #            (1-input$'Populus_grandidentataAGS')*(input$'Populus_grandidentata'*input$totBA),
      #            (1-input$'Populus_balsamiferAGS')*(input$'Populus_balsamifer'*input$totBA),
      #            (1-input$'Ulmus_americanaAGS')*(input$'Ulmus_americana'*input$totBA),
      #            (1-input$'Picea_rubensAGS')*(input$'Picea_rubens'*input$totBA), 
      #            (1-input$'Larix_laricinaAGS')*(input$'Larix_laricina'*input$totBA))
      #   
      # }
      
      
      PAG <- sum(SpeciesDataInput() %>% 
                   filter(desirability == 1) %>% 
                   pull(AGSBA))
      PSAG <- sum(SpeciesDataInput() %>% 
                    filter(desirability <= 2) %>% 
                    pull(AGSBA))
      PSTAG <- sum(SpeciesDataInput() %>% 
                     filter(desirability <= 3) %>% 
                     pull(AGSBA))
      totalBA <- sum(SpeciesDataInput()$TOTBA)

        if(is.null(input$totBA) == FALSE){
            if(totalBA != input$totBA){
              paste("<font size= '+3'>Oops! it looks like your total basal area entered for 1a. (",input$totBA,") doesn't match the sum of your entered trees!(",totalBA,")")
            }else{
              if(PAG >= cline){
                paste("<font size= '+3'>Stand degradation level is ","<p><font color=\"#FF0000\"><font size= '+3'><b>",1)
              }else if(PSAG >= cline){
                paste("<font size= '+3'>Stand degradation level is ","<p><font color=\"#FF0000\"><font size= '+3'><b>",2)
              }else if(PSTAG >= cline){
                paste("<font size= '+3'>Stand degradation level is ","<p><font color=\"#FF0000\"><font size= '+3'><b>",3)
              }else if(totalBA >= cline){
                paste("<font size= '+3'>Stand degradation level is ","<p><font color=\"#FF0000\"><font size= '+3'><b>",4)
              }else {
                paste("<font size= '+3'>Stand degradation level is ","<p><font color=\"#FF0000\"><font size= '+3'><b>",5)
              }
            }
          }else{
            if(PAG >= cline){
              paste("<font size= '+3'>Stand degradation level is ","<p><font color=\"#FF0000\"><font size= '+3'><b>",1)
            }else if(PSAG >= cline){
              paste("<font size= '+3'>Stand degradation level is ","<p><font color=\"#FF0000\"><font size= '+3'><b>",2)
            }else if(PSTAG >= cline){
              paste("<font size= '+3'>Stand degradation level is ","<p><font color=\"#FF0000\"><font size= '+3'><b>",3)
            }else if(totalBA >= cline){
              paste("<font size= '+3'>Stand degradation level is ","<p><font color=\"#FF0000\"><font size= '+3'><b>",4)
            }else {
              paste("<font size= '+3'>Stand degradation level is ","<p><font color=\"#FF0000\"><font size= '+3'><b>",5)
            }
          }

      
    })
    
    # output$Testdf <- renderTable({
    # 
    # })
      # weight_SG <- SpeciesDataInput() %>% 
      #   mutate(wtSG = sg*TOTBA) %>% 
      #   reframe(Avg_SG = sum(wtSG)/sum(TOTBA))
      # 
      # 
      # # data.frame(sp = sg_ref %>% pull(Gen_sp))
      # splist <- sg_ref %>% pull(Gen_sp)
      # # Use lapply to iterate over the input IDs
      # data_list <- lapply(splist, function(id) {
      #   # Check if the input has a non-null value
      #   if (!is.null(input[[id]])){
      #     # If value is not null, return a dataframe row
      #     data.frame(ID = id, AGSValue = input[[id]],
      #                UGSValue = ifelse(input$BAInput != 'Basal Area percentage',
      #                                  input[[paste(id,'UGS',sep = "")]],
      #                                  (1-input[[paste(id,'AGS',sep = '')]])*input[[id]]),
      #                AGSProp = ifelse(input$BAInput == 'Basal Area percentage',
      #                                 input[[id]]*input[[paste(id,"AGS",sep = '')]],
      #                                 0))
      #   } else {
      #     # If value is null, return NULL
      #     NULL
      #   }
      # })
      # 
      # # Use Filter to remove NULL entries
      # data <- do.call(rbind, Filter(function(x) !is.null(x), data_list)) 
      # 
      # data <- data %>% 
      #   mutate(AGSBA = case_when(input$BAInput == 'Prism Plot Data'~ (AGSValue*input$BAF)/input$Nsweeps,
      #                            input$BAInput == 'Basal Area percentage'~ AGSProp*input$totBA,
      #                            TRUE~AGSValue),
      #          UGSBA = case_when(input$BAInput == 'Prism Plot Data'~ (UGSValue*input$BAF)/input$Nsweeps,
      #                            input$BAInput == 'Basal Area percentage'~ UGSValue*input$totBA,
      #                            TRUE~UGSValue),
      #          TOTBA = sum(AGSBA,UGSBA,na.rm = T)) %>% 
      #   filter(AGSValue!=0)
      # 
      # # Return the dataframe
      # data

      # })
    
    # Display all inputs in the console
    # observe({
    #   print(reactiveValuesToList(input))
    # })
    output$allInputs  <- renderPrint({
     print(hw_inputs())
   })
      # weight_SG <- SpeciesDataInput() %>%
      #   mutate(wtSG = sg*TOTBA) %>%
      #   reframe(Avg_SG = sum(wtSG)/sum(TOTBA)) %>%
      #   pull(Avg_SG)

      # Avg_SG <- sum(weight_SG)/sum(AGSBA+UGSBA)

  ## Render Display Table (treelist input) ####
    output$FVS <- renderDataTable({
      
      fvs_calc(datasetInput(),primaryInput(),SecondaryInput(),TertiaryInput())
      # data <- datasetInput()
      # SG_ref <- vroom("SG_ref.csv", col_types = "ffnn")
      # 
      # removedSp <- c("ACSA3","FAGR","TSCA","PIRU","BEAL2","ACRU","ABBA","ACPE")
      # 
      # SG_ref$splist <- sub("^0+", "", SG_ref$splist)
      # 
      # 
      # #remove rows that have plant ID instead of fia sp codes
      # data<- data[!grepl(paste(removedSp, collapse="|"), data$Species),]
      # 
      # #remove leading zeros in fia species cods
      # data$Species <- sub("^0+", "", data$Species)
      # 
      # #join fvs data SG ref so we have specific gravity for all species
      # df1 <- data%>%
      #   full_join(SG_ref, by = c("Species" = "splist"))
      # 
      # #create coloumns for Basal area, Basal area per tree, and Basal area per acre
      # df1.2 <- df1%>%
      #   mutate(TPH = TPA*2.47105,
      #          TF = TPH/`Total Plots`,
      #          RD = TF*(0.00015+0.00218*sg)*((DBH*2.54)/25)^1.6,
      #          prime_AGS = Gen_sp %in% prime  & `Ags Ugs` == "AGS",  # next four lines create boolians for if a tree is a primary AGS or secondary/tertiary
      #          sec_AGS = Gen_sp %in% sec & `Ags Ugs` == "AGS",
      #          tert_AGS = Gen_sp %in% tert & `Ags Ugs` == "AGS",
      #          UGS = `Ags Ugs` == "UGS")
      # 
      # standRD <- df1.2%>%
      #   group_by(StandID)%>%
      #   summarise(PrimeRD = sum(if_else(prime_AGS == TRUE,RD,0)),
      #             SecRD = sum(if_else(sec_AGS == TRUE,RD,0)),
      #             TertRD = sum(if_else(tert_AGS == TRUE,RD,0)),
      #             UGSRD = sum(if_else(UGS == TRUE,RD,0))
      #   )
      # standRD.Table <- as.data.table(standRD) 
      # 
      # standRD.Table <- standRD.Table[, `:=` (Deg = case_when(PrimeRD >= 0.4 ~ 1,
      #                                                        PrimeRD + SecRD >= 0.4~ 2,
      #                                                        PrimeRD + SecRD + TertRD >= 0.4 ~ 3,
      #                                                        PrimeRD + SecRD + TertRD + UGSRD >= 0.4 ~4,
      #                                                        TRUE ~ 5)), by = .(StandID)]
      # 
      # dfStandRD <- as.data.frame(standRD.Table)
      # 
      # dfStandRD_v2 <- dfStandRD%>%
      #   distinct()%>%
      #   na.omit()
      
      # dfStandRD_v2 <- dfStandRD_v2%>%
      #     dplyr::group_by(StandID)%>%
      #     mutate(StandRD  = sum(PrimeRD,SecRD,TertRD,UGSRD))
      
      
      
    })

  ## Downloader Data Table ####
    output$downloadData <- downloadHandler(
      
      
      filename = function() {
        
        
        paste("dataTable","csv", sep=".")
      },
      content = function(file) {
        
        
        dataTable <- fvs_calc(datasetInput(),primaryInput(),SecondaryInput(),TertiaryInput())
        
        write.table(dataTable, file, sep = ",", row.names = FALSE)
      }
    )
    
    
  }
)
