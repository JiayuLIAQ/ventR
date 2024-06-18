fluidPage(theme = shinytheme("united"),
                
                navbarPage(
                  "Ventilation calculator",
                  
                  tabPanel(
                    "Home",
                    
                    # Character of the space----------------------------------------
                    fluidRow(column(
                      width = 12,
                      titlePanel("Characteristics of the space"),
                      fluidRow(
                        
                        column(width = 4,
                               numericInput(
                                 "area",
                                 label = p(HTML("Floor area (m<sup>2</sup>)")),
                                 value = 80
                               )),
                        column(width = 4,
                               numericInput(
                                 "height",
                                 label = p(HTML("Ceiling height (m)")),
                                 value = 2.6
                               )),
                        column(
                          width = 4,
                          selectInput(
                            "room_type",
                            label = p(HTML("Space type")),
                            choices = list("Office" = 1.17,
                                           "Classroom" = 0.22),
                            selected = 1.17
                          )
                        )
                        
                      )
                    )),
                     # CO2----------------------------------------                    
                    fluidRow(column(
                      width = 12,
                      titlePanel("Time-series data"),
                      fluidRow(
                        column(width = 4,
                               textAreaInput(
                                 "time", label = p(HTML("Time Series (in min)")), height = "270px", value = time_default, placeholder = time_default
                               )),
                        column(width = 4,
                               textAreaInput(
                                 "indoor_co2", label = p(HTML("Indoor CO<sub>2</sub> level (ppm)")), height = "270px", value = indoor_co2_default, placeholder = indoor_co2_default
                               )),
                        column(
                          width = 4,
                          radioButtons(
                            "outdoor_co2_selection",
                            label = p(HTML("Outdoor CO<sub>2</sub> level (ppm)")),
                            choices = c(
                              'Select default value (400 ppm)' = 'outdoor_co2_default',
                              'Type in time-series data' = 'outdoor_co2_type_in'
                            ),
                            selected = 'outdoor_co2_default'
                          ),
                          uiOutput("ui_outdoor_co2")
                        )
                      ),
                      fluidRow(column(
                        width = 12,
                        column(width = 2,
                               actionButton("loadBtn_1", "Load Data")),
                        column(width = 10, plotOutput("plot_co2"))
                      ))
                    )), 
                    # occupancy-----------------------------------------
                    fluidRow(column(
                      width = 12,
                      titlePanel("Occupancy"),
                      
                      column(width = 2,
                             numericInput(
                               "occup_type",
                               label = p(HTML("How many age groups of occupants in the space")),
                               value = 2,
                               min = 1,
                               max = 6
                             )),
                      uiOutput("ui_occup")
                    )),
                    
                    # fluidRow(column(
                    #   width = 10,
                    #   offset = 2,
                    #   uiOutput("inputs")
                    # )),
                    
                
                    fluidRow(width = 12,
                             column(width = 2, actionButton("loadBtn_2", "Load Data")), 
                    column(width = 10,
                           plotOutput(
                             "plot_occup"
                           ))
                    ) ,
                    # result-----------------------------------------
                    # fluidRow(column(
                    #   width = 12, plotOutput("plot_ach")
                    # ))
                    
                    fluidRow(column(
                      width = 12,
                      titlePanel("Results based on the steady-stage assumption"),
                      htmlOutput("text_ach_1"), #Render the HTML text
                    )),
                    
                    fluidRow(column(
                      width = 12,
                      titlePanel("Results based on linearization"),
                      plotOutput(
                        "plot_ach"
                      ),
                      # verbatimTextOutput("text_ach")
                      htmlOutput("text_ach_2") #Render the HTML text
                    )),
                    fluidRow(column(
                      width = 12,
                      titlePanel("Results based on the segment period non-linear method"),
                      sliderInput(
                        paste0("seg_range"),
                        label = "Segment range",
                        min = 0,
                        max = 121,
                        value = c(77,121),
                        step = 1,
                        ticks = F
                      ),    
                      htmlOutput("text_ach_3"), #Render the HTML text
                    )),
                  )
                ))

