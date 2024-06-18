function(input, output) {
  output$ui_outdoor_co2 <- renderUI({
    if (is.null(input$outdoor_co2_selection))
      return()
    
    switch(
      input$outdoor_co2_selection,
      "outdoor_co2_default" =  numericInput("outdoor_co2",
                                            # label = "Ventilation rate (air changes per hour)",
                                            label = NULL,
                                            value = 400),
      "outdoor_co2_type_in" =   textAreaInput(
        "outdoor_co2",
        label = NULL,
        height = "150px",
        value = outdoor_co2_default,
        placeholder = outdoor_co2_default
      )
      
    )
  })
  
  
  output$ui_occup <- renderUI({
    n <- input$occup_type
    lapply(seq_len(n), function(i) {
      column(
        width = 2,
        
        h5(HTML(paste0(
          "<b>", "Type ", i, "</b>"
        ))),
        
        sliderInput(
          paste0("age_", i),
          label = "Age range",
          min = 0,
          max = 100,
          value = c(20, 60),
          step = 10,
          ticks = F
        ),    
        selectInput(
          paste0("activity_", i),
          label = "Activity",
          choices = c("1.0 Met", "1.2 Met", "1.4 Met", "1.6 Met", "2.0 Met", "3.0 Met", "4.0 Met"),
          selected = c("2.0 Met")
        ),
        selectInput(
          paste0("gender_", i),
          label = "Gender",
          choices = c("Males", "Females", "Unspecified"),
          selected = c("Unspecified")
        ),
        radioButtons(
          paste0("group_", i, "_type"),
          label = NULL,
          choices = c(
            'Constant' = 'constant',
            'Type in time-series data' = 'occupancy_type_in'
          ),
          selected = 'occupancy_type_in'
        ),
        conditionalPanel(
          condition = sprintf("input['group_%d_type'] == 'occupancy_type_in'", i),
                       textAreaInput(paste0("group_", i, "_time_series"),
                                     height = "270px",
                                     label = NULL,
                                     value = occup_default,
                                     placeholder = occup_default)
        ),
        conditionalPanel(
          condition = sprintf("input['group_%d_type'] == 'constant'", i),
          numericInput(
            paste0("group_", i, "_constant"),
            label = NULL,
            value = 10,
            min = 0
          )
        )
      )
    })
  })
  
  
  # output$inputs <- renderUI({
  #   n <- input$occup_type
  #   lapply(seq_len(n), function(i) {
  #     column(width = 2,
  # 
  #            tagList(if (input[[paste0("group_", i, "_type")]] == "constant") {
  #              numericInput(
  #                paste0("group_", i, "_input"),
  #                label = NULL,
  #                value = 10,
  #                min = 0
  #              )
  #            } else {
  #              textAreaInput(paste0("group_", i, "_input"),
  #                            label = NULL,
  #                            value = occup_default,
  #                            placeholder = occup_default)
  #            }))
  #   })
  # })

  data_co2 <- reactive({
    req(input$loadBtn_1)
    time <- read.table(text = input$time, header = F) %>% setDT %>% setnames("Time")
    indoor_co2 <-
      read.table(text = input$indoor_co2, header = F) %>% setDT %>% setnames("Indoor")
    
    if (input$outdoor_co2_selection == "outdoor_co2_type_in") {
      outdoor_co2 <-
        read.table(text = input$outdoor_co2, header = F) %>% setDT %>% setnames("Outdoor")
      data <- cbind(time, indoor_co2, outdoor_co2)
    } else {
      data <- cbind(time, indoor_co2)
      data[, Outdoor := input$outdoor_co2 %>% as.numeric]
    }
    
    # data <- merge(time_series, co2, by = "minute")
    # data$minute <- as.POSIXct(data$minute, format = "%Y-%m-%d %H:%M:%S")
    data
  })
  
  
  output$plot_co2 <- renderPlot({
    dt <- data_co2() %>% setDT
    # dt %>% setnames(c("Time", "Indoor", "Outdoor"))
    
    # p <-
    dt %>% melt(id = "Time") %>%
      ggplot(aes(x = Time, y = value, color = variable)) +
      geom_line() +
      scale_color_discrete("") +
      ylab(expression("C" * O[2] * " Concentration (ppm)")) +
      xlab("Time (min)") +
      LJYtheme_basic +
      theme(legend.position = "top")
    # ylab(data_names[2])
    # p
  })
  
  
  
  data_occup <- reactive({
    req(input$loadBtn_2)
    data <-
      read.table(text = input$time, header = F) %>% setDT %>% setnames("Time")
    
    for (i in 1:input$occup_type) {
      
      if (input[[paste0("group_", i, "_type")]] == "constant") {
        data <- data[, paste0("occup_", i) := input[[ paste0("group_", i, "_constant") ]] ]
      } else {
        
        occup <-
          read.table(text = input[[ paste0("group_", i, "_time_series") ]], header = F) %>% setDT %>% setnames(paste0("occup_", i))
        data <- cbind(data, occup)
      }
      
      data[, paste0("activity_", i) := input[[ paste0("activity_", i) ]] ]
      data[, paste0("gender_", i) := input[[ paste0("gender_", i) ]] ]
      
      
      co2_gen_dt  <- rbind(copy(co2_gen_met_dt),
      copy(co2_gen_met_dt)[, .(E_l_s = mean(E_l_s, na.rm = T) ), 
                           by = .(age_lower, age_upper, body_mass, BMR, met)] %>% .[, gender := "Unspecified"]
      )
      
      co2_gen_dt [ age_lower >= input[[ paste0("age_", i) ]][1] &
                      age_upper <= input[[ paste0("age_", i) ]][2] &
                       gender == input[[ paste0("gender_", i) ]] &
                       met == input[[ paste0("activity_", i) ]] %>% str_sub(start = 1, end = 3) %>% as.numeric , paste0("select_", i)  := T]
      
      # input[[ paste0("age_", i) ]][2]
      
      data[, paste0("E_", i) := mean(co2_gen_dt[get(paste0("select_", i))]$E_l_s, na.rm = T) * get(paste0("occup_", i)) * 3600 / 1000]
      
      
      # 
      # var_name <- paste0("occup_", i)
      # 
      # assign(var_name, input[[paste0("group_", i, "_input")]])
      # 
      # if (is.numeric(get(paste0("occup_", i)))) {
      #   data <- data[, paste0("occup_", i) := get(paste0("occup_", i))]
      #   
      # } else {
      #   occup <-
      #     read.table(text = get(paste0("occup_", i)), header = F) %>% setDT %>% setnames(paste0("occup_", i))
      #   data <- cbind(data, occup)
      # }
    }
    
    data
  })
  
  
  output$plot_occup <- renderPlot({
    dt <- data_occup() %>% setDT
    dt %>% melt(id = "Time", measure.vars = patterns("occup_")) %>%
      ggplot(aes(x = Time, y = value, color = variable)) +
      geom_line() +
      scale_color_discrete("", labels = paste("Type",c(1:100)) ) +
      ylab("Occupancy") +
      xlab("Time (min)") +
      LJYtheme_basic +
      theme(legend.position = "top")

  })
  
  
  data_ach <- reactive({
    
    volume <- input$area * input$height
    
    dt_co2 <- data_co2() %>% setDT
    dt_occup <- data_occup() %>% setDT
    
    dt_occup <- dt_occup %>% melt(id = "Time", measure.vars = patterns("E_")) %>% 
      .[, .(E = sum(value, na.rm = T) ), by = Time]
    
    dt <- dt_co2[dt_occup, on = .(Time)]
    
    # dt %>% melt(id = "Time") %>%
    #   ggplot(aes(x = Time, y = value, color = variable)) +
    #   geom_line() +
    #   # scale_color_discrete("", labels = paste("Type",c(1:100)) ) +
    #   ylab("Occupancy") +
    #   xlab("Time (min)") +
    #   theme_bw() +
    #   theme(legend.position = "top")
    # 
    fun_ACH_per_step <- function(delta_co2, delta_time, E, co2_outdoor, co2_indoor, volume) {
      ( (volume * (delta_co2 / delta_time) - 1E6* E ) / (co2_outdoor - co2_indoor) ) / volume
    }
    
    dt[, delta_co2 := Indoor- shift(Indoor)] %>% 
      .[, delta_time := Time-shift(Time) ] %>%
      .[, ACH := fun_ACH_per_step(delta_co2 = delta_co2,
                                  delta_time = delta_time/60,
                                  E = E ,
                                  co2_outdoor = Outdoor,
                                  co2_indoor = Indoor,
                                  volume = volume
      )] 
    
  })
  
  # method 1--------
  output$text_ach_1 <- renderText({
    volume <- input$area * input$height
    
    dt <- data_ach() %>% setDT
    
    mean_type_1_occup <- mean(dt$occup_1, na.rm = T)
    mean_type_2_occup <- mean(dt$occup_2, na.rm = T)
    
    co2_indoor <- mean(dt$Indoor, na.rm = T)
    co2_outdoor <- mean(dt$Outdoor, na.rm = T)
    
    mean_E <- mean(dt$E, na.rm = T)
    
    mean_ACH <- 1E6* mean_E/ ( (co2_indoor - co2_outdoor) * volume)
    
    mytxt <- paste0(
      "<span style='font-size: 20px;'>The estimated ventilation rate: <b><br>Mean : ", sprintf("%.*f", 2,mean_ACH)," ACH</b><br>" ) 
    mytxt
  })
  
  # method 2--------
  output$plot_ach <- renderPlot({
    dt <- data_ach() %>% setDT
  p1 <- dt %>% 
    ggplot(aes(x = Time, y = ACH)) +
    geom_line() +
    # scale_color_discrete("", labels = paste("Type",c(1:100)) ) +
    ylab("Air changes per hour (ACH)") +
    xlab("Time (min)") +
    # theme_bw() +
    LJYtheme_basic 
  
  p2 <- dt %>% 
    ggplot(aes(x = 1, y = ACH)) +
    geom_boxplot() +
    stat_summary(geom = "point", fun.y = mean, na.rm = T, shape = 18, size = 6, color = "blue") +
    # scale_color_discrete("", labels = paste("Type",c(1:100)) ) +
    # ylab("Occupancy") +
    # xlab("Time (min)") +
    LJYtheme_basic + theme(axis.title.x = element_blank(),
                           axis.text.x = element_blank(),
                           axis.ticks.x = element_blank(),
                           axis.line.x = element_blank(),
                           axis.title.y = element_blank(),
                           axis.text.y = element_blank(),
                           axis.ticks.y = element_blank(),
                           axis.line.y = element_blank())
  
  p1 + p2 + plot_layout(widths = c(10,1))
  
  })

  output$text_ach_2 <- renderText({
    
    dt <- data_ach() %>% setDT
    
    mean_ACH <- mean(dt$ACH, na.rm = T)
    std_ACH <- sd(dt$ACH, na.rm = T)
    median_ACH <- median(dt$ACH, na.rm = T)
    quantile_ACH_25 <- quantile( dt$ACH, na.rm = T, probs = 0.25 )
    quantile_ACH_75 <- quantile( dt$ACH, na.rm = T, probs = 0.75 )
    
    mytxt <- paste0(
                   "<span style='font-size: 20px;'></b>The estimated ventilation rate:<b><br>",
                   "<span style='font-size: 16px;'>Mean (standard deviation): ", sprintf("%.*f", 2,mean_ACH)," (",sprintf("%.*f", 2,std_ACH),") ACH<br>", 
                   "<span style='font-size: 16px;'>Median [interquartile range]: ", sprintf("%.*f", 2,median_ACH) ,"[", sprintf("%.*f", 2,quantile_ACH_25), "–", sprintf("%.*f", 2, quantile_ACH_75), "] ACH<br>")
                   # "The ventilation rate is <b>lower</b> than the requirement in SS 553 (<em>Code of practice for air-conditioning and mechanical ventilation in buildings<em/>).</span><br>",
                   # "<span style='font-size: 12px;'><em>[[This is just a template! No real calculation is performed in this version!]]<em/></span>",
                   # "<span style='font-size: 12px;'><em>[[Please contact <a href='mailto: jiayu.li@berkeley.edu'>jiayu.li@berkeley.edu</a>  if you would have any questions.]]<em/></span><br>") #Can add "<br>" for line break
    mytxt
  })
  
  
  # method 3--------
  output$plot_ach_3 <- renderPlot({
    dt <- data_ach() %>% setDT
    p1 <- dt %>% 
      ggplot(aes(x = Time, y = ACH)) +
      geom_line() +
      # scale_color_discrete("", labels = paste("Type",c(1:100)) ) +
      ylab("Air changes per hour (ACH)") +
      xlab("Time (min)") +
      # theme_bw() +
      LJYtheme_basic 
    
    p2 <- dt %>% 
      ggplot(aes(x = 1, y = ACH)) +
      geom_boxplot() +
      stat_summary(geom = "point", fun.y = mean, na.rm = T, shape = 18, size = 6, color = "blue") +
      # scale_color_discrete("", labels = paste("Type",c(1:100)) ) +
      # ylab("Occupancy") +
      # xlab("Time (min)") +
      LJYtheme_basic + theme(axis.title.x = element_blank(),
                             axis.text.x = element_blank(),
                             axis.ticks.x = element_blank(),
                             axis.line.x = element_blank(),
                             axis.title.y = element_blank(),
                             axis.text.y = element_blank(),
                             axis.ticks.y = element_blank(),
                             axis.line.y = element_blank())
    
    p1 + p2 + plot_layout(widths = c(10,1))
    
  })
  
  output$text_ach_3 <- renderText({
    volume <- input$area * input$height
    
    dt <- data_ach() %>% setDT
    
    dt <- dt[ Time >= input$seg_range[1] & Time <= input$seg_range[2]]
    
    # dt[, time := .I ]
    # Laussmann, D., and Helm, D. (2011) Air Change Measurements Using Tracer Gases: Methods and Results. Significance of air change for indoor air quality. In Chemistry, Emission Control, Radioactive Pollution and Indoor Air Quality (Vols 1-25, Vol. 14).
    fun.ACH <- function (C_in_0, C_out, ACH, t, E = 0, Vr)  (C_in_0 - C_out) * exp(-ACH * t) + C_out + E*10^6/(ACH * Vr) * (1 - exp(-ACH * t))
    
    ach_dt <- dt %>% 
      # setorder(CC_id,seg, Minute) %>% .[]
      .[, {
        model <- nls(Indoor ~ fun.ACH(C_in_0 = Indoor [1],
                                   C_out = Outdoor, 
                                   ACH, 
                                   t = (Time - Time[1] + 1)/60, #???
                                   E = E ,
                                   Vr = volume),
                     data = .SD, start = list(ACH = 1))
        list( ACH = coef(model))
      }]
    
    
    
    mytxt <- paste0(
      "<span style='font-size: 20px;'>The estimated ventilation rate: <b><br>Mean (standard deviation): ", sprintf("%.*f", 2,ach_dt$ACH)," ACH</b><br>", 
      # "<span style='font-size: 20px;'>Median [interquartile range]: ", sprintf("%.*f", 2,median_ACH) ,"[", sprintf("%.*f", 2,quantile_ACH_25), "–", sprintf("%.*f", 2, quantile_ACH_75), "] ACH<br>",
      # "The ventilation rate is <b>lower</b> than the requirement in SS 553 (<em>Code of practice for air-conditioning and mechanical ventilation in buildings<em/>).</span><br>",
      # "<span style='font-size: 12px;'><em>[[This is just a template! No real calculation is performed in this version!]]<em/></span>",
      "<span style='font-size: 12px;'><em>[[Please contact <a href='mailto: jiayu.li@berkeley.edu'>jiayu.li@berkeley.edu</a>  if you would have any questions.]]<em/></span><br>") #Can add "<br>" for line break
    mytxt
  })
  
}



