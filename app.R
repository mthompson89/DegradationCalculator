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
           column(6, align = "center", numericInput(
             inputId = hwsci_name,
             label = "BA Proportion",
             value = 0.0,
             min = 0.0,
             max = 1.0,
             step = 0.05,
             width = validateCssUnit(200)
           )),
           column(6, align = "center", numericInput(
             inputId = paste0(hwsci_name, "AGS", sep = ''),
             label = "Proportion AGS",
             value = 0.0,
             min = 0.0,
             max = 1.0,
             step = 0.05,
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
           column(6, align = "center", numericInput(
             inputId = swsci_name,
             label = "BA Proportion",
             value = 0.0,
             min = 0.0,
             max = 1.0,
             step = 0.05,
             width = validateCssUnit(200)
           )),
           column(6, align = "center", numericInput(
             inputId = paste0(swsci_name, "AGS", sep = ''),
             label = "Proportion AGS",
             value = 0.0,
             min = 0.0,
             max = 1.0,
             step = 0.05,
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
                 selectInput('BAInput','1. Prism Plot Data or Known Basal Area (sqft/acre) by Species?',
                             choices =  c('Prism Plot Data','Basal Area percentage'),selected = "Prism Plot Data"),
                 bsTooltip("BAInput", "Choose if you are entering 1. Data directly from a prism plot 2. The percent basal area of each species.",
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
                 uiOutput('speciesInputs'),
                 uiOutput('KnownBANote'),
                 fluidPage(
                   column(6,uiOutput("HWTrDtInputs")),
                   column(6,uiOutput('SWTrDtInputs'))
                 ),
                 width = 6
               ),
    ### Main panel#####
               mainPanel(
                 # fluidRow(style = "padding-left: 200px",align = "center",
                 #          tableOutput("testTable")
                 # ),
                 fluidRow(style = "padding-left: 200px",align = "center",
                           plotlyOutput("plot",width = 600, height = 700),

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
    #load dataframe in as reactive so it can be dynamically adjusted
    SpDataframe <- reactive({
      as.data.frame(read_csv('SG_ref.csv')) %>%
        filter(desirability < 4)
    })
    
    #get the specific gravity reference and filter to region
    sg_ref <- reactive({
      sg_ref <- SpDataframe() 
    })
    
    #separate each desirability group
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
          filter(cname %in% input$spInput) %>% 
          filter(HW_SW == "HW")
      })
      swdf <- reactive({
        sg_ref() %>%
          filter(cname %in% input$spInput) %>%
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
      tryCatch({
        
      
      # Apply the function to each row of the dataframe to create input pairs
      inputhw_pairs_ui <- lapply(1:nrow(hwdf()), function(i) {
        createhw_input_pair_ui(hwdf()$cname[i], hwdf()$Gen_sp[i])
      })
      
      # Combine input pairs into a single UI element
      do.call(tagList, inputhw_pairs_ui)
      }, error = function(e){
        NULL
      })
      
      
    })
    
    sw_inputs <- reactive({
      tryCatch({
      # Apply the function to each row of the dataframe to create input pairs
      inputsw_pairs_ui <- lapply(1:nrow(swdf()), function(i) {
        createsw_input_pair_ui(swdf()$cname[i], swdf()$Gen_sp[i])
      })
      
      # Combine input pairs into a single UI element
      do.call(tagList, inputsw_pairs_ui)
    }, error = function(e){
      NULL
    })
    
    })
    
    perhw_inputs <- reactive({
      tryCatch({
      # Apply the function to each row of the dataframe to create input pairs
      inputhw_pairs_ui <- lapply(1:nrow(hwdf()), function(i) {
        createhwslider_input_pair_ui(hwdf()$cname[i], hwdf()$Gen_sp[i])
      })
      
      # Combine input pairs into a single UI element
      do.call(tagList, inputhw_pairs_ui)
    }, error = function(e){
      NULL
    })
    
    })
    
    persw_inputs <- reactive({
      tryCatch({
      # Apply the function to each row of the dataframe to create input pairs
      inputsw_pairs_ui <- lapply(1:nrow(swdf()), function(i) {
        createswslider_input_pair_ui(swdf()$cname[i], swdf()$Gen_sp[i])
      })
      
      # Combine input pairs into a single UI element
      do.call(tagList, inputsw_pairs_ui)
    }, error = function(e){
      NULL
    })
    
    })
    
    SpeciesDataInput <- reactive({
      
      splist <- sg_ref()$Gen_sp
      
      data <- purrr::map_dfr(splist, function(id) {
        
        val <- input[[id]]
        if (is.null(val) || val == 0) return(NULL)
        
        ags <- input[[paste0(id, "AGS")]]
        ugs <- input[[paste0(id, "UGS")]]
        
        tibble(
          Gen_sp  = id,
          AGSValue = val,
          AGSProp  = if (input$BAInput == "Basal Area percentage") val * ags else 0,
          UGSValue = if (input$BAInput == "Basal Area percentage") {
            (1 - ags) * val
          } else {
            ugs
          }
        )
      })
      
      req(nrow(data) > 0)
      
      data <- data %>%
        mutate(
          AGSBA =
            if (input$BAInput == "Prism Plot Data") {
              (AGSValue * input$BAF) / input$Nsweeps
            } else if (input$BAInput == "Basal Area percentage") {
              AGSProp * input$totBA
            } else {
              AGSValue
            },
          
          UGSBA =
            if (input$BAInput == "Prism Plot Data") {
              (UGSValue * input$BAF) / input$Nsweeps
            } else if (input$BAInput == "Basal Area percentage") {
              UGSValue * input$totBA
            } else {
              UGSValue
            },
          
          TOTBA = AGSBA + UGSBA
        ) %>%
        left_join(sg_ref(), by = "Gen_sp")
      
      data
    })
    
  ## renderUI inputs ####
    output$basalArea <- renderUI({
      if (is.null(input$BAInput))
        return()
      
      switch(input$BAInput,
             "Prism Plot Data" = numericInput("BAF","1a. Basal Area Factor",value = 5), 
             
             # "Known Basal Area by Species" = NULL,
             
             "Basal Area percentage" = NULL
             
      )
    })
    output$basalArea2 <- renderUI({
      if (is.null(input$BAInput))
        return()
      
      switch(input$BAInput,
             "Prism Plot Data" = numericInput("Nsweeps","1b. Number of prism points", value = 1),
             
             # "Known Basal Area by Species" = NULL,
             
             "Basal Area percentage" = NULL
             
      )
    })
    
    output$totalBA <- renderUI({
      if (is.null(input$BAInput))
        return()
      
      switch(input$BAInput,
             "Prism Plot Data" = NULL,
             
             # "Known Basal Area by Species" = numericInput("totspBA","1a. Total Basal Area of Stand", value = 0),#totspBA
             
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
             # 
             # "Known Basal Area by Species" = fluidRow(
             #   h5(strong("3. Input Basal Area by Species")),
             #   column(6, align = "center", h3(strong("Hardwood"))),
             #   column(6, align = "center", h3(strong("Softwood")))
             # ),
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
    
    output$speciesInputs <- renderUI({
      selectInput("spInput","2b. Select Species Present In Stand",
                  sg_ref()$cname,multiple = T)
    })
    
    output$HWTrDtInputs <- renderUI({
      if (is.null(input$BAInput))
        return()
      if(is.null(input$spInput))
        return()
      switch(input$BAInput,
             "Prism Plot Data" = hw_inputs(),
             # "Known Basal Area by Species" = hw_inputs(),
             'Basal Area percentage' = perhw_inputs()
      )
    })
    output$SWTrDtInputs <- renderUI({
      if (is.null(input$BAInput))
        return()
      if(is.null(input$spInput))
        return()
      switch(input$BAInput,
             "Prism Plot Data" = sw_inputs(),
             # "Known Basal Area by Species" = sw_inputs(),
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
  
 
  
 
  ## Render Plot #####      
    output$plot  <- renderPlotly({

      
      validate(
        need(
          {
            # Try running the reactive safely
            data <- try(SpeciesDataInput(), silent = TRUE)
            
            # If there was an error, return FALSE (trigger message)
            if (inherits(data, "try-error")) {
              FALSE
            } else {
              # Otherwise, check if AGSValue sums to more than 0
              sum(data$AGSValue, na.rm = TRUE) > 0
            }
          },
          "Please input tree species to calculate degradation class. Results will appear here."
        )
      )
      
 
      Avg_SG <- SpeciesDataInput() %>% 
        mutate(wtSG = sg*TOTBA) %>% 
        reframe(Avg_SG = sum(wtSG)/sum(TOTBA)) %>% 
        pull(Avg_SG)
      

      
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
     
      plot
      
    })
  ## Render Degradation Value ####   
    output$degValue <- renderText({
      
      validate(
        need(
          {
            # Try running the reactive safely
            data <- try(SpeciesDataInput(), silent = TRUE)
            
            # If there was an error, return FALSE (trigger message)
            if (inherits(data, "try-error")) {
              FALSE
            } else {
              # Otherwise, check if AGSValue sums to more than 0
              sum(data$AGSValue, na.rm = TRUE) > 0
            }
          },
          "Please input tree species to calculate degradation class. Results will appear here."
        )
      )
      # 
      # validate(
      #   need(sum(SpeciesDataInput()$AGSValue)>0, 
      #  
      #        "Please Input Tree Species To Calculate Degradation Class
      #                Results Will Appear Here")
      # )
      
    
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

      if(input$BAInput == "Basal Area percentage"){

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
    

    output$allInputs  <- renderPrint({
     print(hw_inputs())
   })

  ## Render Display Table (treelist input) ####
    output$FVS <- renderDataTable({
      
      fvs_calc(datasetInput(),primaryInput(),SecondaryInput(),TertiaryInput())
     
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
