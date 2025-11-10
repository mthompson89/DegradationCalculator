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

# Avg_SG <- sum(weight_SG)/sum(SpeciesDataInput()$TOTBA)


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


# weight_SG <- SpeciesDataInput() %>%
#   mutate(wtSG = sg*TOTBA) %>%
#   reframe(Avg_SG = sum(wtSG)/sum(TOTBA)) %>%
#   pull(Avg_SG)

# Avg_SG <- sum(weight_SG)/sum(AGSBA+UGSBA)


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