#import libraries####
library(shiny)
library(rsconnect)
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

options(app_idle_timeout = 0)
#function builder####
#functions for calculating stocking percentages
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

# function to read in and calculate deg for a FVS output
fvs_calc <- function(data, pos){
    SG_ref <- vroom("SG_ref.csv", col_types = "ffnn")
    desireability <- vroom("TreeDesireability.csv")
    
    removedSp <- c("ACSA3","FAGR","TSCA","PIRU","BEAL2","ACRU","ABBA","ACPE")
    
    SG_ref$splist <- sub("^0+", "", SG_ref$splist)
    
    prime_sp <- na.omit(desireability$Primary)
    second_sp <- na.omit(desireability$Secondary)
    tert_sp <- na.omit(desireability$Tertiary)
    
    
    #remove rows that have plant ID instead of fia sp codes
    data<- data[!grepl(paste(removedSp, collapse="|"), data$Species),]
    
    #remove leading zeros in fia species cods
    data$Species <- sub("^0+", "", data$Species)
    
    #join fvs data SG ref so we have specific gravity for all species
    df1 <- data%>%
        full_join(SG_ref, by = c("Species" = "splist"))
    
    #create coloumns for Basal area, Basal area per tree, and Basal area per acre
    df1 <- df1%>%
        group_by(StandID, Year,Species)%>%
        mutate(BA = ((DBH^2)* 0.005454),
               BA_per_Tree = (BA*TPA))
    #  BA_per_Acre = sum(BA_per_Tree))
    #creat mean DBH per stand (per year) as well as mean SG so we can calculate C-line, B-line, and A-line Basal area
    df1 <- df1%>%
        group_by(StandID, Year)%>%
        mutate(Stand_mean_DBH = mean(DBH),
               Stand_mean_SG = mean(sg),
               Cline_BA = C_lineBA(Stand_mean_DBH,Stand_mean_SG),
               Bline_BA = B_lineBA(Stand_mean_DBH,Stand_mean_SG),
               Aline_BA = A_lineBA(Stand_mean_DBH,Stand_mean_SG))%>%
        select(StandID, Species, Gen_sp, sg, Year, DBH, Stand_mean_DBH, Stand_mean_SG, TPA, BA, BA_per_Tree,Cline_BA,Bline_BA, Aline_BA)
    
    #convert datafram to data.table to make the next line of code run faster
    dt1 <- as.data.table(df1)
    #calculate the total basal area of primary species, secondary species and tertiary species by year, by stand
    dt2 <- dt1[, `:=`(prime_BA = sum(BA_per_Tree[which(Gen_sp %in% prime_sp)]),
                      second_BA = sum(BA_per_Tree[which(Gen_sp %in% second_sp)]),
                      tert_BA = sum(BA_per_Tree[which(Gen_sp %in% tert_sp)])
    ), by = .(StandID, Year)]
    
    #calculate degradation catagory by compairing primary secondary, and tertiary basal areas to C-line
    dt2.1 <- dt2[, `:=` (Deg = case_when(prime_BA >= Cline_BA ~ 1,
                                         prime_BA + second_BA >= Cline_BA~ 2,
                                         prime_BA + second_BA + tert_BA >= Cline_BA ~ 3,
                                         TRUE ~ 4)), by = .(StandID, Year)]
    dt2.2 <- dt2.1%>%
        select(StandID,Year, Stand_mean_DBH, Cline_BA, Bline_BA, Aline_BA, prime_BA, second_BA, tert_BA, Deg)%>%
        distinct()
    
}

#Define names for reference later ####
sp_Names <- c("Pinus resinosa","Pinu resinosa",
              "Acer saccharum","Acer saccharum",
              "Betula alleghaniensis","Betula alleghaniensis",
              "Betula payrifera", "Betula payrifera",
              "Pinus strobus","Pinus strobus",
              "Quercus rubra","Quercus rubra",
              "Prunus serotina","Prunus serotina",
              "Quercus alba","Quercus alba",
              "Acer rubrum","Acer rubrum",
              "Tsuga canadensis","Tsuga canadensis",
              "Thuja occidentailis","Thuja occidentailis",
              "Fraxinus americana","Fraxinus americana",
              "Picea mariana","Picea mariana",
              "Picea glauca","Picea glauca",
              "Betula lenta","Betula lenta",
              "abies Balsamea","abies Balsamea",
              "Fagus grandifolia","Fagus grandifolia",
              "Populaus tremuloides","Populaus tremuloides",
              "Betual populifolia","Betual populifolia",
              "Populus grandidentata","Populus grandidentata",
              "Populus balsamifera","Populus balsamifera",
              "Ulmus americana","Ulmus americana",
              "Picea rubens", "Picea rubens",
              "Larix laricina", "Larix laricina")

#specific gravity for each species, SG are doubled because of AGS and UGS calculation later
Sp_SG <- c(0.4,0.4,0.63,0.63,0.62,0.62,0.55,0.55,0.35,0.35,0.63,0.63,0.5,0.5,0.68,0.68,0.54,0.54,0.4,0.4,0.31,0.31,0.6,0.6,0.46,0.46,
           0.4,0.4, 0.4,0.4,0.65,0.65,0.35,0.35,0.64,0.64,0.38,0.38,0.51,0.51,0.39,0.39,0.34,0.34,0.5,0.5, 0.53,0.53)

#rename specific gravity  values to the appropriate species names
names(Sp_SG) <- sp_Names
options(shiny.maxRequestSize = 3000*1024^2)

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
                          padding: 10px;
                          width: 90%;
                      }"))
                ),
        navbarPage(
             theme = shinytheme("flatly"),  
            "Degradation Calculator",
            ### How To ####
            tabPanel("How To",
                     fluidRow(align = "center",
                          tags$div(
                                   class = "content",
                                   tags$div(
                                             id = "images", 
                                             tags$img(
                                               src = "HowTo1.jpg",
                                               width = "auto",
                                               height = "auto"
                                             ),
                                             tags$img(
                                               src = "HowTo2.jpg",
                                               width = "auto",
                                               height = "auto"
                                             ),
                                             tags$img(
                                               src = "HowTo3.jpg",
                                               width = "auto",
                                               height = "auto"
                                             ),
                                             tags$img(
                                               src = "HowTo4.jpg",
                                               width = "auto",
                                               height = "auto"
                                             ),
                                             tags$img(
                                               src = "HowTo5.jpg",
                                               width = "auto",
                                               height = "auto"
                                             ),
                                             tags$img(
                                               src = "HowTo6.jpg",
                                               width = "auto",
                                               height = "auto"
                                             ),
                                             tags$img(
                                               src = "HowTo7.jpg",
                                               width = "auto",
                                               height = "auto"
                                             ),
                                             tags$img(
                                               src = "HowTo8.jpg",
                                               width = "auto",
                                               height = "auto"
                                             ),
                                             tags$img(
                                               src = "HowTo9.jpg",
                                               width = "auto",
                                               height = "auto"
                                             ),
                                             tags$img(
                                               src = "HowTo10.jpg",
                                               width = "auto",
                                               height = "auto"
                                             ),
                                             tags$img(
                                               src = "HowTo11.jpg",
                                               width = "auto",
                                               height = "auto"
                                             ),
                                             tags$img(
                                               src = "HowTo12.jpg",
                                               width = "auto",
                                               height = "auto"
                                             ),
                                             tags$img(
                                               src = "HowTo13.jpg",
                                               width = "auto",
                                               height = "auto"
                                             ),
                                             tags$img(
                                               src = "HowTo14.jpg",
                                               width = "auto",
                                               height = "auto"
                                             ),
                                             tags$img(
                                               src = "HowTo15.jpg",
                                               width = "auto",
                                               height = "auto"
                                             ),
                                             tags$img(
                                               src = "HowTo16.jpg",
                                               width = "auto",
                                               height = "auto"
                                             ),
                                             tags$img(
                                               src = "HowTo17.jpg",
                                               width = "auto",
                                               height = "auto"
                                             )
                                          )
                                  )
                               )
                     ),
            # Prism Plots #####
            tabPanel("Plot Input",
                     sidebarPanel(
                       # dropMenu(
                       #   actionButton("go0", "How To", height = '50%', styleclass = "info"),
                       #   img(src='giphy.gif', align = "center"),
                       #   theme = "light-border",
                       #   placement = "right",
                       #   arrow = FALSE
                       # ),
                       #input if a Basal area prism is going to be used or known basal area by species
                        selectInput('BAInput','1. Prism Plot Data or Known Basal Area(sqft/acre) by Species?',
                                   choices =  c('Prism Plot Data','Known Basal Area by Species')),
                       bsTooltip("BAInput", "Choose if you are entering data directly from a prism plot or if you are entering the known basal area (sqft/acre) by species in the stand.",
                                 "right", options = list(container = "body")),
                       uiOutput('basalArea'),
                       bsTooltip("basalArea", "Enter the basal area factor prism you are using to conduct the prism plot",
                                 "right", options = list(container = "body")),
                       uiOutput('basalArea2'),
                       bsTooltip("basalArea2", "Enter the number of prism plots you conducted in the stand",
                                 "right", options = list(container = "body")),
                       #input wether stand avg dbh or stand class size will be used
                       selectInput("dbhInput", "2. Select Stand Class Size or Mean DBH of Stand",
                                   c("Stand Class Size","Mean DBH of Stand")),
                       bsTooltip("dbhInput", "Choose whether you are going to use the average stand size class to calculate DBH or if you have calculated the mean DBH of the stand",
                                 "right", options = list(container = "body")),
                       uiOutput("ui"),
                       fluidRow(
                         h5(strong("3. Input tree data")),
                         h6('NOTE: If you selected "Prism Plot" above, enter the number of individuals that were counted during the plot(s), if you selected "Known Basal Area (sqft/acre) by Species", enter the total basal area of AGS and UGS by species in the stand '),
                         column(6, align = "center", h3(strong("Hardwood"))),
                         column(6, align = "center", h3(strong("Softwood")))
                       ),
                       fluidRow(style = "overflow-y:scroll; max-height: 400px; position:relative;",
                                ### Tree sp. Inputs ######
                                column(6, align = "center",
                                       h5(strong("American Beech")),
                                       h6("Fagus grandifolia"),
                                       column(6,align = "right",numericInput("American Beech", "AGS", 0, min = 0, width = validateCssUnit(100))),
                                       column(6,align = "left",numericInput("FagusGrandifolia_UGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
                                       
                                       h5(strong("American Elm")),
                                       h6("(Ulmus americana)"),
                                       column(6,align = "right",numericInput("American Elm", "AGS", 0, min = 0, width = validateCssUnit(100))),
                                       column(6,align = "left",numericInput("UlmusAmericana_UGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
                                       
                                       h5(strong("Balsam Poplar")),
                                       h6("(Populus balsamifera)"),
                                       column(6,align = "right", numericInput("Balsam Poplar", "AGS", 0, min = 0, width = validateCssUnit(100))),
                                       column(6,align = "left",numericInput("PopulusBalsamifera_UGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
                                       
                                       h5(strong("Bigtooth Aspen")),
                                       h6("(Populus grandidentata)"),
                                       column(6,align = "right",numericInput("Bigtooth Aspen", "AGS", 0, min = 0, width = validateCssUnit(100))),
                                       column(6,align = "left",numericInput("PopulusGrandidentata_UGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
                                       
                                       h5(strong("Black Birch")),
                                       h6("(Betula lenta)"),
                                       column(6,align = "right", numericInput("Black Birch", "AGS", 0, min = 0, width = validateCssUnit(100))),
                                       column(6,align = "left",numericInput("BetulaLenta_UGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
                                       
                                       h5(strong("Black Cherry")),
                                       h6("(Prunus serotina)"),
                                       column(6,align = "right",numericInput("Black Cherry", "AGS", 0, min = 0, width = validateCssUnit(100))),
                                       column(6,align = "left",numericInput("PrunusSerotina_UGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
                                       
                                       h5(strong("Grey Birch")),
                                       h6("(Betual populifolia)"),
                                       column(6,align = "right",numericInput("Grey Birch", "AGS", 0, min = 0, width = validateCssUnit(100))),
                                       column(6,align = "left",numericInput("BetualPopulifolia_UGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
                                       
                                       h5(strong("Quaking Aspen")),
                                       h6("(Populaus tremuloides)"),
                                       column(6,align = "right",numericInput("Quaking Aspen", "AGS", 0, min = 0, width = validateCssUnit(100))),
                                       column(6,align = "left",numericInput("PopulausTremuloides_UGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
                                       
                                       h5(strong("Red Maple")),
                                       h6("(Acer rubrum)"),
                                       column(6,align = "right",numericInput("Red Maple", "AGS", 0, min = 0, width = validateCssUnit(100))),
                                       column(6,align = "left",numericInput("AcerRubrum_UGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
                                       
                                       h5(strong("Red Oak")),
                                       h6("(Quercus rubra)"),
                                       column(6,align = "right",numericInput("Red Oak", "AGS", 0, min = 0, width = validateCssUnit(100))),
                                       column(6,align = "left",numericInput("QuercusRubra_UGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
                                       
                                       h5(strong("Sugar Maple")),
                                       h6("(Acer saccharum)"),
                                       column(6,align = "right",numericInput("Sugar Maple", "AGS", 0, min = 0, width =  validateCssUnit(100))),
                                       column(6,align = "left",numericInput("AcerSaccharum_UGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
                                       
                                       h5(strong("White Ash")),
                                       h6("(Fraxinus americana)"),
                                       column(6,align = "right",numericInput("White Ash", "AGS", 0, min = 0, width = validateCssUnit(100))),
                                       column(6,align = "left",numericInput("FraxinusAmericana_UGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
                                       
                                       h5(strong("White Birch")),
                                       h6("(Betula papyrifera)"),
                                       column(6,align = "right",numericInput("White Birch", "AGS", 0, min = 0, width = validateCssUnit(100))),
                                       column(6,align = "left",numericInput("BetulaPapyrifera_UGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
                                       
                                       h5(strong("White Oak")),
                                       h6("(Quercus alba)"),
                                       column(6,align = "right",numericInput("White Oak", "AGS", 0, min = 0, width = validateCssUnit(100))),
                                       column(6,align = "left",numericInput("QuercusAlba_UGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
                                       
                                       h5(strong("Yellow Birch")),
                                       h6("(Betula allegheniensis)"),
                                       column(6,align = "right",numericInput("Yellow Birch", "AGS", 0, min = 0, width = validateCssUnit(100))),
                                       column(6,align = "left",numericInput("BetulaAllegheniensis_UGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
                                ),
                                column(6, align = "center",
                                       h5(strong("Arborvitae")),
                                       h6("(Thuja occidentailis)"),
                                       column(6,align = "right",numericInput("Arborvitae", "AGS", 0, min = 0, width = validateCssUnit(100))),
                                       column(6,align = "left",numericInput("ThujaOccidentailis_UGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
                                       
                                       h5(strong("Balsam Fir")),
                                       h6("(Abies balsamea)"),
                                       column(6,align = "right",numericInput("Balsam Fir", "AGS", 0, min = 0, width = validateCssUnit(100))),
                                       column(6,align = "left",numericInput("AbiesBalsamea_UGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
                                       
                                       h5(strong("Black Spruce")),
                                       h6("(Picea Mariana)"),
                                       column(6,align = "right",numericInput("Black Spruce", "AGS", 0, min = 0, width = validateCssUnit(100))),
                                       column(6,align = "left",numericInput("PiceaMariana_UGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
                                       
                                       h5(strong("Eastern Hemlock")),
                                       h6("(Tsuga canadensis)"),
                                       column(6,align = "right",numericInput("Eastern Hemlock", "AGS", 0, min = 0, width = validateCssUnit(100))),
                                       column(6,align = "left",numericInput("TsugaCanadensis_UGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
                                       
                                       h5(strong("Eastern Tamarack")),
                                       h6("(Larix laricina)"),
                                       column(6,align = "right",numericInput("Eastern Tamarack", "AGS", 0, min = 0, width = validateCssUnit(100))),
                                       column(6,align = "left",numericInput("LarixLaricina_UGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
                                       
                                       h5(strong("Red Pine")),
                                       h6("(Pinus resinosa)"),
                                       column(6,align = "right",numericInput("Red Pine", "AGS", 0, min = 0, width = validateCssUnit(150))),
                                       column(6,align = "left",numericInput("PinusResinosa_UGS", "UGS", 0, min = 0, width =  validateCssUnit(150))),
                                       
                                       h5(strong("Red Spruce")),
                                       h6("(Picea rubens)"),
                                       column(6,align = "right",numericInput("Red Spruce", "AGS", 0, min = 0, width = validateCssUnit(100))),
                                       column(6,align = "left",numericInput("PiceaRubens_UGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
                                       
                                       h5(strong("White Pine")),
                                       h6("(Pinus strobus)"),
                                       column(6,align = "right",numericInput("White Pine", "AGS", 0, min = 0, width = validateCssUnit(100))),
                                       column(6,align = "left",numericInput("PinusStrobus_UGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
                                       
                                       h5(strong("White Spruce")),
                                       h6("(Picea glauca)"),
                                       column(6,align = "right",numericInput("White Spruce", "AGS", 0, min = 0, width = validateCssUnit(100))),
                                       column(6,align = "left",numericInput("PiceaGlauca_UGS", "UGS", 0, min = 0, width = validateCssUnit(100))),
                                       
                                       
                                )
                       )
                     ),
                     mainPanel(
                         fluidRow(style = "padding-left: 200px",align = "center",
                               plotlyOutput("plot",width = 600, height = 700),
                         ),
                         fluidRow(style = "padding: 50px,50px,50px,50px", align = "center",
                         htmlOutput("degValue")
                         )
   
                     )
            ),
            # FVS Outputs #####
            tabPanel("FVS Output", 
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
                       fileInput("FVS_Sheet", "Upload FVS Ouput",
                                 accept = ".csv",
                                 multiple = FALSE,
                                 width = 750),
                       downloadButton("downloadData", "Download")
                       
                     ),
                     mainPanel(
                       conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                        tags$div("Please Wait While App is Working", id = "UpdateAnimate", class = "loading dots")),
                       dataTableOutput("FVS")
                       
                        )
                     ),
            ## Tree Category Selector#####
            tabPanel("Tree Category Selector",
                     # useShinyalert(slickROutput("slickR")),
                       fluidRow(align = "center",
                       h3("Change species desirablity categories"),
                       h5("Click and drag species into prefered desirability boxes. To reset species, simply refresh app")
                       ),
                       fluidRow(align = "center",
                       bucket_list(
                         header = c(""),
                               add_rank_list(
                                 text = "Primary",
                                 labels =c("Red Pine",'Sugar Maple', 'Yellow Birch', 'White Birch',
                                           'White Pine' , 'Red Oak' ,'Black Cherry','White Oak', 'Red Spruce', 'Eastern Tamarack'),
                                 input_id = "prime"
                               ),
                               add_rank_list(
                                 text = "Secondary",
                                 labels = c('Red Maple', 'Eastern Hemlock', 'Arborvitae', 'White Ash',
                                            'Black Spruce', 'White Spruce', 'Black Birch'),
                                 input_id = "sec"
                               ),
                               add_rank_list(
                                 text = "Tertiary",
                                 labels = c('Balsam Fir', 'American Beech', 'Quaking Aspen', 'Grey Birch',
                                            'Bigtooth Aspen', 'Balsam Poplar','American Elm'),
                                 input_id = "tert"
                               ),
                         
                         group_name = "bucket_list_group"
                              )
                          ),
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
        ),
    ),
    ### SERVER #####
    server = function(input, output) {
      
      
      
      datasetInput <- reactive({
        file <- input$FVS_Sheet
        
        ext <- tools::file_ext(file$datapath)
        validate(need(ext == "csv", "please upload a csv file"))
        
        vroom(file$datapath, col_types = c("MgmtID" = "f","StandID" = "f", "Species" = "f"))
        
      })

      output$basalArea <- renderUI({
        if (is.null(input$BAInput))
          return()

        switch(input$BAInput,
               "Prism Plot Data" = numericInput("BAF","1a. Basal Area Factor",value = 5), 
               
               "Known Basal Area by Species" = NULL
               
        )
      })
      output$basalArea2 <- renderUI({
        if (is.null(input$BAInput))
          return()

        switch(input$BAInput,
               "Prism Plot Data" = numericInput("Nsweeps","1b. Number of prism points", value = 1),
               
               "Known Basal Area by Species" = NULL
               
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
                   
                   "Mean DBH of Stand" =  numericInput("exactDBH","2a. Mean DBH of Stand", 0),
                  
            )
        })
      
        
      output$plot  <- renderPlotly({
            validate(
                need(sum(input$'Red Pine',input$PinusResinosa_UGS,input$'Sugar Maple',input$AcerSaccharum_UGS,
                         input$'Yellow Birch',input$BetulaAllegheniensis_UGS, input$'White Birch',input$BetulaPapyrifera_UGS,
                         input$'White Pine',input$PinusStrobus_UGS, input$'Red Oak',input$QuercusRubra_UGS,
                         input$'Black Cherry',input$PrunusSerotina_UGS, input$'White Oak',input$QuercusAlba_UGS,
                         input$'Red Maple',input$AcerRubrum_UGS, input$'Eastern Hemlock',input$TsugaCanadensis_UGS,
                         input$'Arborvitae',input$ThujaOccidentailis_UGS, input$'White Ash',input$FraxinusAmericana_UGS,
                         input$'Black Spruce',input$PiceaMariana_UGS, input$'White Spruce',input$PiceaGlauca_UGS,
                         input$'Black Birch',input$BetulaLenta_UGS, input$'Balsam Fir',input$AbiesBalsamea_UGS,
                         input$'American Beech',input$FagusGrandifolia_UGS, input$'Quaking Aspen',input$PopulausTremuloides_UGS,
                         input$'Grey Birch',input$BetualPopulifolia_UGS, input$'Bigtooth Aspen',input$PopulusGrandidentata_UGS,
                         input$'Balsam Poplar',input$PopulusBalsamifera_UGS, input$'American Elm',input$UlmusAmericana_UGS,
                         input$'Red Spruce', input$PiceaRubens_UGS, input$'Eastern Tamarack', input$LarixLaricina_UGS) > 0, "Please Input Tree Species To Calculate Degradtaion Class")
                    )
            PlotSp <- c(input$'Red Pine',input$PinusResinosa_UGS,input$'Sugar Maple',input$AcerSaccharum_UGS,
                        input$'Yellow Birch',input$BetulaAllegheniensis_UGS, input$'White Birch',input$BetulaPapyrifera_UGS,
                        input$'White Pine',input$PinusStrobus_UGS, input$'Red Oak',input$QuercusRubra_UGS,
                        input$'Black Cherry',input$PrunusSerotina_UGS, input$'White Oak',input$QuercusAlba_UGS,
                        input$'Red Maple',input$AcerRubrum_UGS, input$'Eastern Hemlock',input$TsugaCanadensis_UGS,
                        input$'Arborvitae',input$ThujaOccidentailis_UGS, input$'White Ash',input$FraxinusAmericana_UGS,
                        input$'Black Spruce',input$PiceaMariana_UGS, input$'White Spruce',input$PiceaGlauca_UGS,
                        input$'Black Birch',input$BetulaLenta_UGS, input$'Balsam Fir',input$AbiesBalsamea_UGS,
                        input$'American Beech',input$FagusGrandifolia_UGS, input$'Quaking Aspen',input$PopulausTremuloides_UGS,
                        input$'Grey Birch',input$BetualPopulifolia_UGS, input$'Bigtooth Aspen',input$PopulusGrandidentata_UGS,
                        input$'Balsam Poplar',input$PopulusBalsamifera_UGS, input$'American Elm',input$UlmusAmericana_UGS,
                        input$'Red Spruce', input$PiceaRubens_UGS, input$'Eastern Tamarack', input$LarixLaricina_UGS)
            
            
            names(PlotSp) <- sp_Names
            SG_sum <- Sp_SG*PlotSp
            Avg_SG <- sum(SG_sum)/sum(PlotSp)
            
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
            
            if(input$BAInput == 'Prism Plot Data'){
              
                    plot_data <- sapply(input$bucket_list_group, function(category){
                      ifelse(length(category) == 0, 0,
                             sum(sapply(category, function(item) input[[item]]))*(input$BAF/input$Nsweeps))
                    })
                    
                    UGS <- sum(input$PinusResinosa_UGS,input$AcerSaccharum_UGS,input$BetulaAllegheniensis_UGS,input$BetulaPapyrifera_UGS,input$PinusStrobus_UGS,
                             input$QuercusRubra_UGS,input$PrunusSerotina_UGS,input$QuercusAlba_UGS,input$AcerRubrum_UGS,input$TsugaCanadensis_UGS,
                             input$ThujaOccidentailis_UGS,input$FraxinusAmericana_UGS,input$PiceaMariana_UGS,input$PiceaGlauca_UGS,input$BetulaLenta_UGS,input$AbiesBalsamea_UGS,
                             input$FagusGrandifolia_UGS,input$PopulausTremuloides_UGS,input$BetualPopulifolia_UGS,input$PopulusGrandidentata_UGS,input$PopulusBalsamifera_UGS,
                             input$UlmusAmericana_UGS,input$PiceaRubens_UGS, input$LarixLaricina_UGS)*(input$BAF/input$Nsweeps)

            }
            else{
                    plot_data <- sapply(input$bucket_list_group, function(category){
                      ifelse(length(category) == 0, 0,
                             sum(sapply(category, function(item) input[[item]])))
                    })
                    
                    UGS <- c(input$PinusResinosa_UGS,input$AcerSaccharum_UGS,input$BetulaAllegheniensis_UGS,input$BetulaPapyrifera_UGS,input$PinusStrobus_UGS,
                             input$QuercusRubra_UGS,input$PrunusSerotina_UGS,input$QuercusAlba_UGS,input$AcerRubrum_UGS,input$TsugaCanadensis_UGS,
                             input$ThujaOccidentailis_UGS,input$FraxinusAmericana_UGS,input$PiceaMariana_UGS,input$PiceaGlauca_UGS,input$BetulaLenta_UGS,input$AbiesBalsamea_UGS,
                             input$FagusGrandifolia_UGS,input$PopulausTremuloides_UGS,input$BetualPopulifolia_UGS,input$PopulusGrandidentata_UGS,input$PopulusBalsamifera_UGS,
                             input$UlmusAmericana_UGS,input$PiceaRubens_UGS, input$LarixLaricina_UGS)
                  }
            
            totalBA <- sum(plot_data[1],plot_data[2],plot_data[3],UGS)
            
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

            plot <- plot_ly(x = "Stand", y = plot_data[1], type = "bar", name = "Primary", marker = list(color = 'rgb(110, 235, 131)')) %>%
                            add_trace(y = plot_data[2], name = "Secondary", marker = list(color = 'rgb(40, 83, 107)'))%>%
                            add_trace(y = plot_data[3], name = "Tertiary",marker = list(color = 'rgb(217, 131, 36)'))%>%
                            add_trace(y = ~UGS, name = "Unacceptable Growing Stock", marker = list(color = 'rgb(140, 28, 19)') )%>%
                            layout(yaxis = list(title = "Basal Area"),xaxis = list(title = ""), barmode = "stack")%>%
                            layout(shapes = list(hline(c_line, color = "black"),hline(b_line, color = "black"),hline(a_line, color = "black")))%>%
                            add_annotations(x = 0, y = c_line, xref = "x", yref = "y", text = paste("C-Line ",c_line,"Sq Ft/acre"), showarrow = F, yshift = 10, xshift = "right")%>%
                            add_annotations(x = 0, y = b_line, xref = "x", yref = "y", text = paste("B-Line ",b_line,"Sq Ft/acre"), showarrow = F, yshift = 10)%>%
                            add_annotations(x = 0, y = a_line, xref = "x", yref = "y", text = paste("A-Line ",a_line,"Sq Ft/acre"), showarrow = F, yshift = 10)%>%
                            add_annotations(x = 0, y = totalBA, xref = "x", yref = "y", text = paste("Total Basal Area",totalBA,"Sq Ft/acre"), showarrow = F, yshift = 10)%>%
                            layout(paper_bgcolor= 'transparent')%>%
                            layout(plot_bgcolor = 'transparent')

            
            plot
            
        })
        
      output$degValue <- renderText({
            validate(
                need(sum(input$'Red Pine',input$PinusResinosa_UGS,input$'Sugar Maple',input$AcerSaccharum_UGS,
                         input$'Yellow Birch',input$BetulaAllegheniensis_UGS, input$'White Birch',input$BetulaPapyrifera_UGS,
                         input$'White Pine',input$PinusStrobus_UGS, input$'Red Oak',input$QuercusRubra_UGS,
                         input$'Black Cherry',input$PrunusSerotina_UGS, input$'White Oak',input$QuercusAlba_UGS,
                         input$'Red Maple',input$AcerRubrum_UGS, input$'Eastern Hemlock',input$TsugaCanadensis_UGS,
                         input$'Arborvitae',input$ThujaOccidentailis_UGS, input$'White Ash',input$FraxinusAmericana_UGS,
                         input$'Black Spruce',input$PiceaMariana_UGS, input$'White Spruce',input$PiceaGlauca_UGS,
                         input$'Black Birch',input$BetulaLenta_UGS, input$'Balsam Fir',input$AbiesBalsamea_UGS,
                         input$'American Beech',input$FagusGrandifolia_UGS, input$'Quaking Aspen',input$PopulausTremuloides_UGS,
                         input$'Grey Birch',input$BetualPopulifolia_UGS, input$'Bigtooth Aspen',input$PopulusGrandidentata_UGS,
                         input$'Balsam Poplar',input$PopulusBalsamifera_UGS, input$'American Elm',input$UlmusAmericana_UGS,
                         input$'Red Spruce', input$PiceaRubens_UGS, input$'Eastern Tamarack', input$LarixLaricina_UGS) > 0, "Please Input Tree Species To Calculate Degradtaion Class")
            )
            PlotSp <- c(input$'Red Pine',input$PinusResinosa_UGS,input$'Sugar Maple',input$AcerSaccharum_UGS,
                        input$'Yellow Birch',input$BetulaAllegheniensis_UGS, input$'White Birch',input$BetulaPapyrifera_UGS,
                        input$'White Pine',input$PinusStrobus_UGS, input$'Red Oak',input$QuercusRubra_UGS,
                        input$'Black Cherry',input$PrunusSerotina_UGS, input$'White Oak',input$QuercusAlba_UGS,
                        input$'Red Maple',input$AcerRubrum_UGS, input$'Eastern Hemlock',input$TsugaCanadensis_UGS,
                        input$'Arborvitae',input$ThujaOccidentailis_UGS, input$'White Ash',input$FraxinusAmericana_UGS,
                        input$'Black Spruce',input$PiceaMariana_UGS, input$'White Spruce',input$PiceaGlauca_UGS,
                        input$'Black Birch',input$BetulaLenta_UGS, input$'Balsam Fir',input$AbiesBalsamea_UGS,
                        input$'American Beech',input$FagusGrandifolia_UGS, input$'Quaking Aspen',input$PopulausTremuloides_UGS,
                        input$'Grey Birch',input$BetualPopulifolia_UGS, input$'Bigtooth Aspen',input$PopulusGrandidentata_UGS,
                        input$'Balsam Poplar',input$PopulusBalsamifera_UGS, input$'American Elm',input$UlmusAmericana_UGS,
                        input$'Red Spruce', input$PiceaRubens_UGS, input$'Eastern Tamarack', input$LarixLaricina_UGS)
            
            
            names(PlotSp) <- sp_Names
            SG_sum <- Sp_SG*PlotSp
            Avg_SG <- sum(SG_sum)/sum(PlotSp)
            
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
            
            if(input$BAInput == 'Prism Plot Data'){
              
              plot_data <- sapply(input$bucket_list_group, function(category){
                ifelse(length(category) == 0, 0,
                       sum(sapply(category, function(item) input[[item]]))*(input$BAF/input$Nsweeps))
              })
              
              UGS <- sum(input$PinusResinosa_UGS,input$AcerSaccharum_UGS,input$BetulaAllegheniensis_UGS,input$BetulaPapyrifera_UGS,input$PinusStrobus_UGS,
                         input$QuercusRubra_UGS,input$PrunusSerotina_UGS,input$QuercusAlba_UGS,input$AcerRubrum_UGS,input$TsugaCanadensis_UGS,
                         input$ThujaOccidentailis_UGS,input$FraxinusAmericana_UGS,input$PiceaMariana_UGS,input$PiceaGlauca_UGS,input$BetulaLenta_UGS,input$AbiesBalsamea_UGS,
                         input$FagusGrandifolia_UGS,input$PopulausTremuloides_UGS,input$BetualPopulifolia_UGS,input$PopulusGrandidentata_UGS,input$PopulusBalsamifera_UGS,
                         input$UlmusAmericana_UGS,input$PiceaRubens_UGS, input$LarixLaricina_UGS)*(input$BAF/input$Nsweeps)
              
            }
            else{
              plot_data <- sapply(input$bucket_list_group, function(category){
                ifelse(length(category) == 0, 0,
                       sum(sapply(category, function(item) input[[item]])))
              })
              
              UGS <- c(input$PinusResinosa_UGS,input$AcerSaccharum_UGS,input$BetulaAllegheniensis_UGS,input$BetulaPapyrifera_UGS,input$PinusStrobus_UGS,
                       input$QuercusRubra_UGS,input$PrunusSerotina_UGS,input$QuercusAlba_UGS,input$AcerRubrum_UGS,input$TsugaCanadensis_UGS,
                       input$ThujaOccidentailis_UGS,input$FraxinusAmericana_UGS,input$PiceaMariana_UGS,input$PiceaGlauca_UGS,input$BetulaLenta_UGS,input$AbiesBalsamea_UGS,
                       input$FagusGrandifolia_UGS,input$PopulausTremuloides_UGS,input$BetualPopulifolia_UGS,input$PopulusGrandidentata_UGS,input$PopulusBalsamifera_UGS,
                       input$UlmusAmericana_UGS,input$PiceaRubens_UGS, input$LarixLaricina_UGS)
            }
            
            PAG <- plot_data[1]
            PSAG <- sum(plot_data[1],plot_data[2])
            PSTAG <- sum(plot_data[1],plot_data[2],plot_data[3])
            totalBA <- sum(plot_data[1],plot_data[2],plot_data[3],UGS)
            
            
            if(PAG > cline){
                paste("<font size= '+3'>Stand degradation level is ","<p><font color=\"#FF0000\"><font size= '+3'><b>",1)
            }else if(PSAG > cline){
                paste("<font size= '+3'>Stand degradation level is ","<p><font color=\"#FF0000\"><font size= '+3'><b>",2)
            }else if(PSTAG > cline){
                paste("<font size= '+3'>Stand degradation level is ","<p><font color=\"#FF0000\"><font size= '+3'><b>",3)
            }else if(totalBA > cline){
                paste("<font size= '+3'>Stand degradation level is ","<p><font color=\"#FF0000\"><font size= '+3'><b>",4)
            }else {
                paste("<font size= '+3'>Stand degradation level is ","<p><font color=\"#FF0000\"><font size= '+3'><b>",5)
            }
            
        })
        
      output$FVS <- renderDataTable({
      
      fvs_calc(datasetInput())
          
        })
        output$downloadData <- downloadHandler(
          
          filename = function() {
            
            paste("dataTable","csv", sep=".")
          },
          content = function(file) {
            dataTable <- fvs_calc(datasetInput())
            write.table(dataTable, file, sep = ",", row.names = FALSE)
          }
        )
    
    }
)