require(shiny)
require(shinyWidgets)
require(shinydashboard)
require(shinythemes)
require(ggplot2)
require(ggrepel)
require(plotly)
require(RColorBrewer)
require(showtext)
require(DT)
require(dygraphs)
require(xts)
require(scales)


showtext_auto()

server <- shinyServer(function(input, output, session) {
  
  validurl <- reactive({
    pass = TRUE
    return (pass)
  })
  
  dyCSScontrol <- function(dygraph){
    dygraph$x$css <- '
          .dygraph-legend {
          width: auto !important;
          min-width: 150px;
          color: white;
          background-color: #005ce6 !important;
          font-size: 20px !important;
          padding-left:5px;
          border-color:#BABABA;
          border-style:solid;
          border-width:thin;
          transition:0s 4s;
          z-index: 80 !important;
          box-shadow: 2px 2px 5px rgba(0, 0, 0, .3);
          border-radius: 3px;
          }
          
          .dygraph-legend > span {
          color: black;
          padding-left:5px;
          padding-right:2px;
          margin-left:-5px;
          background-color: white !important;
          display: block;
          }
          
          .dygraph-legend > span:first-child {
          margin-top:2px;
          }
          
          .dygraph-legend > span > span{
          display: inline;
          }
          
          .highlight {
          border-left: 2px solid #BABABA;
          padding-left:3px !important;
          }
          '
    dygraph
  }
  
  ##### Program Page Outputs #####

  prog_temp <- reactive({  

    prog = df_prog
    
    ### For hierarchical filtering structures, upstream layers control the options to be shown in downstream filters.
    ### freeze (isolate) the instantaneous update to choices in upstream filters (backward update), 
    ### only allow single-way filtering (forward update; downstream) to prevent app crashing or alternating,
    ### only update constrained options in upstream filters after performing selecting action on downstream filters ("SUBMIT" button)
    
    input$trigger_filter_from_circular_node
    
    isolate({ 
      
      eventclick$x = TRUE
      
      ### get three scenarios / states of data-frames based on the selections in 3 output-related filters
      f1 <- df_prog[,c("output_main", "output", "subcode")] %>% filter(output_main %in% input$slicer_page_1_output_main)
      f2 <- df_prog[,c("output_main", "output", "subcode")] %>% filter(output %in% input$slicer_page_1_output)
      f3 <- df_prog[,c("output_main", "output", "subcode")] %>% filter(subcode %in% input$slicer_page_1_pcode)
      
      ### get constrained values / states of the 3 layers of filters based on the above scenarios
      output_main_1 <- unique(f1$output_main)
      output_main_2 <- unique(f2$output_main)
      output_main_3 <- unique(f3$output_main)
      output_1 <- unique(f1$output)
      output_2 <- unique(f2$output)
      output_3 <- unique(f3$output)
      subcode_1 <- unique(f1$subcode)
      subcode_2 <- unique(f2$subcode)
      subcode_3 <- unique(f3$subcode)
      
      lm1 <- length(output_main_1)
      lm2 <- length(output_main_2)
      lm3 <- length(output_main_3)
      lo1 <- length(output_1)
      lo2 <- length(output_2)
      lo3 <- length(output_3)
      ls1 <- length(subcode_1)
      ls2 <- length(subcode_2)
      ls3 <- length(subcode_3)
      
      ## scenario 1: change on output main => no button triggered update needed
      
      ## scenario 2: change on output
      ### button triggered update 1)  add output => count(lm2 not in lm1) > 0
      ### button triggered update 2)  deduct output => count(lm1 not in lm2) > 0
      
      ## scenario 3: change on subcode
      ### button triggered update 1)  add non-B2 pcode or first B2 pcode => count(lo3 not in lo2) > 0
      ### button triggered update 2)  deduct non-B2 pcode or last B2 pcode => count(lo2 not in lo3) > 0
      ### button triggered update 3)  change already-exist B2 pcode 
      ###                               => count(ls2 not in ls3) > 0 and count(lo3 not in lo2) = 0 
      ###                               => no button triggered update needed
      
      if ( length(output_main_2[!(output_main_2 %in% output_main_1)]) > 0 | length(output_main_1[!(output_main_1 %in% output_main_2)]) > 0 ) {
        
        updatePickerInput(session, "slicer_page_1_output_main", 
                          selected = c(unique(f2$output_main)))
        
        rv_node1$r <- unique(f2$output_main)
        rv_node2$r <- input$slicer_page_1_output
        rv_node3$r <- input$slicer_page_1_pcode
        
      } else if ( length(output_3[!(output_3 %in% output_2)]) > 0 | length(output_2[!(output_2 %in% output_3)]) > 0 ) {
        
        updatePickerInput(session, "slicer_page_1_output_main", 
                          selected = c(unique(f3$output_main)))
        
        updatePickerInput(session, "slicer_page_1_output", 
                          selected = c(unique(f3$output)))
        
        rv_node1$r <- unique(f3$output_main)
        rv_node2$r <- unique(f3$output)
        rv_node3$r <- input$slicer_page_1_pcode
        
      } else {
        
        eventclick$x = FALSE
        
        rv_node1$r <- input$slicer_page_1_output_main
        rv_node2$r <- input$slicer_page_1_output
        rv_node3$r <- input$slicer_page_1_pcode
      }
      
      prog %>% 
        filter(Year %in% input$slicer_page_1_year) %>% 
        filter(Quarter %in% input$slicer_page_1_quarter) %>% 
        filter(Month %in% input$slicer_page_1_month) %>%
        filter(output_main %in% rv_node1$r) %>% 
        filter(output %in% rv_node2$r) %>% 
        filter(subcode %in% rv_node3$r)
    })
  })

  observe({
    f <- df_prog[,c("Year", "Quarter")] %>% filter(Year %in% input$slicer_page_1_year)
    
    updatePickerInput(session, "slicer_page_1_quarter", 
                      choices = sort(unique(f$Quarter)),
                      selected = c(unique(f$Quarter)))
  })
  
  observe({
    f <- df_prog[,c("Year", "Quarter", "Month")] %>% 
          filter(Year %in% input$slicer_page_1_year) %>% 
          filter(Quarter %in% input$slicer_page_1_quarter)
    
    updatePickerInput(session, "slicer_page_1_month", 
                      choices = unique(f$Month[order(match(f$Month, month_levels))]),
                      selected = c(unique(f$Month)))
  })

  rv_node1 <- reactiveValues(r = c())
  rv_node2 <- reactiveValues(r = c())
  rv_node3 <- reactiveValues(r = c())
  
  eventclick <- reactiveValues(x = FALSE)
  
  observeEvent(input$slicer_page_1_output_main, {

    if (!isolate(eventclick$x == TRUE)) {
      f <- df_prog[,c("output_main", "output", "subcode")] %>% 
              filter(output_main %in% input$slicer_page_1_output_main)

      updatePickerInput(session, "slicer_page_1_output", 
                        selected = c(unique(f$output)))
    }

    eventclick$x = FALSE
  })
  
  observeEvent(input$slicer_page_1_output, {

    if (!isolate(eventclick$x == TRUE)) {
      f <- df_prog[,c("output_main", "output", "subcode")]  %>% 
              filter(output %in% input$slicer_page_1_output)

      updatePickerInput(session, "slicer_page_1_pcode", 
                        selected = c(unique(f$subcode)))
    }

    eventclick$x = FALSE
  })
  
  output$page_1_box_1 <- renderbs4InfoBox({
    temp = prog_temp()
    prog_count = length(unique(temp$programcode))
    valuelabel = prog_count
    vbox <- bs4InfoBox(
        value = valuelabel, title = "Total Number of Programs", color = "info", icon = icon("calendar", lib = "font-awesome")
      )
    return(vbox)
  })

  output$page_1_box_2 <- renderbs4InfoBox({
    temp = prog_temp()
    sess_count = dim(temp)[1]
    valuelabel = sess_count
    vbox <- bs4InfoBox(
        value = scales::comma(valuelabel), title = "Total Number of Sessions", color = "teal", icon = icon("clock", lib = "font-awesome")
      )
    return(vbox)
  })

  output$page_1_box_3 <- renderbs4InfoBox({
    temp = prog_temp()
    temp$lead_time = as.Date(temp$activity_enddate) - as.Date(temp$activity_startdate)
    temp = temp %>% dplyr::group_by(programcode) %>% dplyr::summarise(lead_time = mean(lead_time))
    valuelabel = round(mean(temp$lead_time), 2)
    vbox <- bs4InfoBox(
        value = paste0(valuelabel, " Days"), title = "Average Program Lead Time", color = "lightblue", icon = icon("timeline", lib = "font-awesome")
      )
    return(vbox)
  })

  output$page_1_box_4 <- renderbs4InfoBox({
    temp = prog_temp()
    hour_count = sum(temp$implementationhour)
    valuelabel = hour_count
    vbox <- bs4InfoBox(
        value = scales::comma(valuelabel), title = "Total Program Session Hours", color = "olive", icon = icon("stopwatch", lib = "font-awesome")
      )
    return(vbox)
  })

  output$page_1_box_5 <- renderbs4InfoBox({
    temp = prog_temp()
    temp = temp %>% 
              dplyr::group_by(programcode) %>% 
              dplyr::summarise(enrol_num = mean(enrollment_number), 
                               session_num = n(), 
                               enrol_sess = mean(enrollment_number) * n())
    enrol_count = sum(temp$enrol_num)
    enrol_sess_count = sum(temp$enrol_sess)
    valuelabel = HTML(paste(scales::comma(enrol_count), "   (summing all programs)", tags$br(), 
                            scales::comma(enrol_sess_count), "   (summing all program sessions)", sep = ""))
    vbox <- bs4InfoBox(
        value = valuelabel, title = "Enrollment Statistics", color = "purple", icon = icon("user-pen", lib = "font-awesome")
      )
    return(vbox)
  })

  output$page_1_box_6 <- renderbs4InfoBox({
    temp = prog_temp()
    temp = temp %>% 
              dplyr::group_by(programcode) %>% 
              dplyr::summarise(attend_num = sum(attendance_number),
                               enrol_sess = mean(enrollment_number) * n(),
                               attend_rate = sum(attendance_number) / (mean(enrollment_number) * n()) * 100)
    attendance_count = sum(temp$attend_num)
    attendance_rate = round(mean(temp$attend_rate, na.rm=TRUE), 1)
    valuelabel = HTML(paste(scales::comma(attendance_count), "   (Total number of attendance)", tags$br(), 
                            attendance_rate, "%   (Average attendance rate)", sep = ""))
    vbox <- bs4InfoBox(
        value = valuelabel, title = "Attendance Statistics", color = "indigo", icon = icon("user-check", lib = "font-awesome")
      )
    return(vbox)
  })
  
  output$page_1_plot_type_by_year_selection <- renderUI({
    if (input$radio_btn_page_1_plot_type != "By Year") {
      pickerInput('slicer_page_1_year_for_2nd_dim', label = h6("Please select one single Year:"),
                  choices = sort(unique(df_prog$Year)), 
                  multiple = FALSE,
                  selected = sort(unique(df_prog$Year))[length(sort(unique(df_prog$Year)))],
                  options = list(style = "btn-secondary"))
    }
  })
  
  page_1_count_data = function(temp) {
    
    if (input$radio_btn_page_1_plot_type != "By Year") { 
      temp = subset(temp, Year == input$slicer_page_1_year_for_2nd_dim)
    }
    
    if (input$radio_btn_page_1_plot_type == "By Year") { 
      first = "Year"
      levels = sort(unique(temp$Year))
    }
    else if (input$radio_btn_page_1_plot_type == "By Quarter") { 
      first = "Quarter"
      levels = sort(unique(temp$Quarter))
    }
    else if (input$radio_btn_page_1_plot_type == "By Month") { 
      first = "Month"
      levels = month_levels
      temp$Month = factor(temp$Month, levels=levels)
      temp = temp[order(temp$Month), ]
    }
    
    if (input$slicer_page_1_secondary_dimension == "Output Main"){ second = "output_main" }
    else if (input$slicer_page_1_secondary_dimension == "Output"){ second = "output" }
    else if (input$slicer_page_1_secondary_dimension == "Program Code (Subcode) Category"){ second = "subcode" }
    else if (input$slicer_page_1_secondary_dimension == "Nil") { second = "" }
    
    if (input$slicer_page_1_secondary_dimension == "Nil") { group_cols = c(first) } 
    else { group_cols = c(first, second) }
    
    if (input$slicer_page_1_metric == "Program Count") { 
      table_temp = temp %>% dplyr::group_by_at(group_cols) %>% dplyr::summarize(value = length(unique(programcode))) 
    } else if (input$slicer_page_1_metric == "Program Session Count") { 
      table_temp = temp %>% dplyr::group_by_at(group_cols) %>% dplyr::summarize(value = n())
    } else if (input$slicer_page_1_metric == "Enrollment Count (summing all programs)") { 
      temp = temp %>% dplyr::group_by_at(c(group_cols, "programcode")) %>% dplyr::summarise(enrollment_number = mean(enrollment_number))
      table_temp = temp %>% dplyr::group_by_at(group_cols) %>% dplyr::summarize(value = sum(enrollment_number))
    } else if (input$slicer_page_1_metric == "Enrollment Count (summing all sessions)") { 
      temp = temp %>% dplyr::group_by_at(c(group_cols, "programcode")) %>% dplyr::summarise(session_enrollment_number = mean(enrollment_number) * n())
      table_temp = temp %>% dplyr::group_by_at(group_cols) %>% dplyr::summarize(value = sum(session_enrollment_number))
    } else if (input$slicer_page_1_metric == "Attendance Count") { 
      temp = temp %>% dplyr::group_by_at(c(group_cols, "programcode")) %>% dplyr::summarise(attendance_number = sum(attendance_number))
      table_temp = temp %>% dplyr::group_by_at(c(group_cols)) %>% dplyr::summarize(value = sum(attendance_number))
    } else if (input$slicer_page_1_metric == "Program Hours") { 
      temp = temp %>% dplyr::group_by_at(c(group_cols, "programcode")) %>% dplyr::summarise(program_hours = sum(implementationhour))
      table_temp = temp %>% dplyr::group_by_at(c(group_cols)) %>% dplyr::summarize(value = sum(program_hours))
    } else if (input$slicer_page_1_metric == "Program Lead Time") { 
      temp = temp %>% dplyr::group_by_at(c(group_cols, "programcode")) %>% dplyr::summarise(lead_time = mean(lead_time))
      table_temp = temp %>% dplyr::group_by_at(c(group_cols)) %>% dplyr::summarize(value = round(mean(lead_time), 2))
    }
    
    return(list(table_temp, first, second, levels))
  }

  output$page_1_count_plot <- renderPlotly({
    
    temp = prog_temp()
    temp$lead_time = as.Date(temp$activity_enddate) - as.Date(temp$activity_startdate)
    data = page_1_count_data(temp)
    table_temp = data[[1]]
    first = data[[2]]
    second = data[[3]]
    levels = data[[4]]
    
    ref_ylim = table_temp %>% dplyr::group_by_at(first) %>% dplyr::summarise(value = sum(value))

    p = plot_ly() %>%
          layout(title = "", 
                  xaxis = list(title = first), 
                  yaxis = list(title = input$slicer_page_1_metric, 
                                range = list(0, ifelse(input$radio_btn_page_1_plot_type == "By Month",
                                                      max(table_temp$value),
                                                      ifelse(input$slicer_page_1_secondary_dimension == "Nil", 
                                                                          max(table_temp$value), 
                                                                          max(ref_ylim$value)
                                                                        )) * 1.1)), 
                  hovermode = 'x unified') %>%
          config(displayModeBar = F)
      
    if (input$radio_btn_page_1_plot_type != "By Month") { 
      if (input$slicer_page_1_secondary_dimension != "Nil") {
        p2 = p %>% add_trace(
              data = table_temp, 
              x = ~ factor(.data[[first]], levels=levels), y = ~ value, 
              color = ~ .data[[second]], 
              legendgroup = ~ .data[[second]],
              type = "bar", width = 0.6,
              hovertemplate = ~ paste0(first, " : %{x}", "<br>", second, " : ", .data[[second]], "<br>", input$slicer_page_1_metric, " : %{y}<extra></extra>")
            ) %>%
            layout(barmode = "stack", 
                   legend = list(traceorder = "grouped", orientation = "h", yanchor = "top", y = -0.25)
            )
      } else {
        p2 = p %>% add_trace(
            data = table_temp, 
            x = ~ factor(.data[[first]], levels=levels), y = ~ value, text = ~ value,
            type = "bar", width = 0.6,
            marker = list(color = "rgb(139,131,120)"), 
            hovertemplate = paste0(first, " : %{x}", "<br>", input$slicer_page_1_metric, " : %{y}<extra></extra>"),
            hoverlabel = list(font=list(color='white')),
            textposition = "outside" 
          )
      } 
    } else if (input$radio_btn_page_1_plot_type == "By Month") { 
      if (input$slicer_page_1_secondary_dimension != "Nil") {    
        p2 = p %>% add_trace(
              data = data.frame(table_temp), 
              x = ~ factor(.data[[first]], levels=levels), y = ~ value, 
              color = ~ .data[[second]], 
              legendgroup = ~ .data[[second]],
              type = "scatter", mode = 'lines+markers',
              line = list(width=3),
              hovertemplate = ~ paste0(first, ": %{x}", "<br>", second, " : ", .data[[second]], "<br>", input$slicer_page_1_metric, " %{y}<extra></extra>")
            ) %>%
            layout(
                legend = list(traceorder = "grouped", orientation = "h", yanchor = "top", y = -0.25)
            )
      } else {
        p2 = p %>% add_trace(
              data = table_temp, 
              x = ~ factor(.data[[first]], levels=levels), y = ~ value, text = ~ value, 
              type = "scatter", mode = 'lines+markers+text', 
              line = list(color = "rgb(139,131,120)", width=3),
              hovertemplate = paste0(first, ": %{x}", "<br>", input$slicer_page_1_metric, " %{y}<extra></extra>"),
              textposition = "top left", hoverinfo = "text"
            )
      }
    }
    return(p2)
  })
  
  output$page_1_count_table <- renderDataTable({
    
    temp = df_prog
    temp$lead_time = as.Date(temp$activity_enddate) - as.Date(temp$activity_startdate)
    data = page_1_count_data(temp)
    table_temp = data[[1]]
    colnames(table_temp)[length(table_temp)] = input$slicer_page_1_metric
    
    datatable(table_temp,
              caption = tags$caption(
                style = 'caption-side:top;color:black;',
                tags$b("TABLE: ")
              ),
              options = list(dom = "Blfrtip", lengthMenu = c(10, 20, 50), pageLength = 50, autoWidth = FALSE,
                             columnDefs = list(list(width = '180px', targets = "_all")), 
                             scrollX = "300px", scrollY = "220px", fixedHeader=FALSE), 
              fillContainer = T, rownames = FALSE, escape = FALSE)
  })
  
  output$page_1_attend_up <- renderValueBox({
    temp = prog_temp()
    temp = temp %>% 
      dplyr::group_by(programcode) %>% 
      dplyr::summarise(base = mean(enrollment_number) * n(), ref = sum(attendance_number))
    
    valuelabel = round(nrow(subset(temp, ref > base)) / nrow(temp) * 100, 2)
    valuelabel = paste0(valuelabel, "%")
      
    vbox = valueBox(
      tags$span(style='font-size:25px', valuelabel), 
      "Percentage of programs with additional walk-in attendance (exceeding number of pre-registration / enrollment)", 
      color = "navy"
    )
    return(vbox)
  })
  
  output$page_1_attend_down <- renderValueBox({
    temp = prog_temp()
    temp = temp %>% 
      dplyr::group_by(programcode) %>% 
      dplyr::summarise(base = mean(enrollment_number) * n(), ref = sum(attendance_number))
    
    valuelabel = round(nrow(subset(temp, ref <= base)) / nrow(temp) * 100, 2)
    valuelabel = paste0(valuelabel, "%")
    
    vbox = valueBox(
      tags$span(style='font-size:25px', valuelabel), 
      "Percentage of programs with expected attendance (not exceeding number of pre-registration / enrollment)", 
      color = "info"
    )
    return(vbox)
  })
  
  progress_columns = function(df) {
    out = df %>%
      dplyr::group_by(output, subcode, programcode) %>% 
      dplyr::summarize(program_count = length(unique(programcode)),
                       session_count = n(),
                       enrollment_number = mean(enrollment_number),
                       attendance_number = sum(attendance_number))
    out = out %>%
      dplyr::group_by(output, subcode) %>% 
      dplyr::summarise(program_count = sum(program_count),
                       session_count = sum(session_count),
                       enrollment_number = sum(enrollment_number),
                       attendance_number = sum(attendance_number))
    return(out)
  }
  
  progress_target = function(target) {
    
    if (input$radio_btn_page_1_progress_year == "2020-2021") { 
      target = target[,c("Output","subcode",
                         "units_deliverables","units_beneficiaries","units_attendance",
                         "DeliverablesY1","BeneficiariesY1","AttendanceY1")] 
    } else if (input$radio_btn_page_1_progress_year == "2021-2022") { 
      target = target[,c("Output","subcode",
                         "units_deliverables","units_beneficiaries","units_attendance",
                         "DeliverablesY2","BeneficiariesY2","AttendanceY2")] 
    } else if (input$radio_btn_page_1_progress_year == "2022-2023") { 
      target = target[,c("Output","subcode",
                         "units_deliverables","units_beneficiaries","units_attendance",
                         "DeliverablesY3","BeneficiariesY3","AttendanceY3")] 
    } else {
      target = target[,c("Output","subcode",
                         "units_deliverables","units_beneficiaries","units_attendance",
                         "DeliverablesTotal","BeneficiariesTotal","AttendanceTotal")]
    }
    return(target)
  }
  
  gauge_chart_producer = function(kpi_y, col_units_kpi_y, kpi_y_default_measure, kpi_y_default_measure_ref_col, kpi_y_nondefault_measure_ref_col) {
    
    temp <- df_prog
    target <- df_target
    temp <- subset(temp, output == input$slicer_page_1_progress_output)
    
    if (input$radio_btn_page_1_progress_year != "Entire 3Y-Period") {
      temp <- subset(temp, Year == input$radio_btn_page_1_progress_year)
    }
    
    stat_current = progress_columns(temp)
    
    target$Output = trimws(target$Output)
    target = subset(target, Output == input$slicer_page_1_progress_output)
    
    target = progress_target(target)
    
    if (nrow(stat_current) == 0) {
      stat_current = dplyr::bind_rows(stat_current,
                                      data.frame("output" = input$slicer_page_1_progress_output,
                                                 "subcode" = target$subcode[target$Output == input$slicer_page_1_progress_output],
                                                 "program_count" = 0,
                                                 "session_count" = 0,
                                                 "enrollment_number" = 0,
                                                 "attendance_number" = 0
                                                 ))
    }
    
    if (input$slicer_page_1_progress_output != "Disease specific basic training workshop") {
      
      merged = merge(target, stat_current, by.x=c("Output","subcode"), by.y=c("output","subcode"), all.x=T)
      
    } else {
      
      stat_current = stat_current %>% 
        dplyr::group_by(output) %>% 
        dplyr::summarise(program_count = sum(program_count),
                         session_count = sum(session_count),
                         enrollment_number = sum(enrollment_number),
                         attendance_number = sum(attendance_number))
      
      target = target %>% 
        dplyr::group_by(Output, units_deliverables, units_beneficiaries) %>%
        dplyr::summarize_all(funs(mean))
      
      merged = merge(target, stat_current, by.x=c("Output"), by.y=c("output"), all.x=T)
    }
    
    pivot_col = colnames(target)[grepl(kpi_y, colnames(target))]
    merged[, pivot_col][is.na(merged[, pivot_col])] = 0
    merged[, col_units_kpi_y][is.na(merged[, col_units_kpi_y])] = kpi_y_default_measure
    
    target_col = pivot_col 
    eval_col = ifelse(merged[, col_units_kpi_y] == kpi_y_default_measure, kpi_y_default_measure_ref_col, kpi_y_nondefault_measure_ref_col)
    
    engineer_gauge = merged[,c(target_col, eval_col)]
    engineer_gauge$absdiff = abs(engineer_gauge[, target_col] - engineer_gauge[, eval_col])
    engineer_gauge = reshape2::melt(engineer_gauge)
    
    if (engineer_gauge[engineer_gauge$variable == target_col, "value"] >= engineer_gauge[engineer_gauge$variable == eval_col, "value"]) {
      
      engineer_gauge = subset(engineer_gauge, variable != target_col)
      engineer_gauge = engineer_gauge %>% dplyr::mutate(percentage = value/sum(value))
      engineer_gauge$percentage[is.na(engineer_gauge$percentage)] = 0
      engineer_gauge$variable = droplevels(engineer_gauge$variable)
      engineer_gauge = engineer_gauge %>% add_row(variable = "zzz", value = 0, percentage = 1)
      
      plot_type = ">="
      
    } else {
      
      engineer_gauge = subset(engineer_gauge, variable != "absdiff")
      engineer_gauge = engineer_gauge %>% dplyr::mutate(percentage = value/max(value))
      engineer_gauge$variable = droplevels(engineer_gauge$variable)
      engineer_gauge = engineer_gauge %>% add_row(variable = "zzz_t", value = 0, percentage = engineer_gauge[engineer_gauge$variable == target_col, "percentage"])
      engineer_gauge = engineer_gauge %>% add_row(variable = "zzz_e", value = 0, percentage = 1)
      
      plot_type = "<"
    }
    
    return(list(engineer_gauge, eval_col, target_col, plot_type))
  }
  
  gauge_chart_plotter = function(gauge_output, gauge_name) {
    
    gauge = gauge_output[[1]]
    eval_col = gauge_output[[2]]
    target_col = gauge_output[[3]]
    plot_type = gauge_output[[4]]
    rfactor = gauge$variable[!(gauge$variable %in% c("absdiff", "zzz"))]
    
    if (plot_type == ">=") {
      
      p = ggplot(gauge, aes(x=as.character(1.5), y=percentage, fill=factor(variable, levels=c(rfactor, "absdiff", "zzz")))) +
        geom_bar(width = 0.45, stat = "identity") + 
        labs(fill = "", title=paste0(gauge_name, " (", input$radio_btn_page_1_progress_year, "): ")) +
        coord_polar(theta="y", start=-pi/2) +
        ylim(0,1) +
        scale_y_reverse() + 
        scale_fill_manual(values=c("#59D8B5","#DADFAF","#FFFFFF")) +
        geom_text_repel(aes(label = ifelse(variable != "absdiff", 
                                           ifelse(variable != "zzz", paste0("(completed: ", value, ")"), ""), 
                                           ifelse(value == 0, "", paste0("(not completed: ", value, ")")))
        ), 
        position = position_stack(vjust = 0.4), hjust = 0.5, size=3) +
        annotate("text", label = paste0(round(gauge[gauge$variable == eval_col, "percentage"] * 100, 1), "%"), 
                 x=0.2, y=0.5, size=10, color="#D9774A")
      
    }  else {
      
      p = ggplot(gauge[gauge$variable %in% c(eval_col, "zzz_e"), ], aes(x=as.character(1.5), y=percentage, fill=variable)) +
        geom_bar(width = 0.45, stat = "identity") + 
        labs(fill = "", title=paste0(gauge_name, " (", input$radio_btn_page_1_progress_year, "): ")) +
        coord_polar(theta="y", start=-pi/2) +
        ylim(0,1) +
        scale_y_reverse() + 
        scale_fill_manual(values=c("#59D8B5","#FFFFFF")) +
        annotate("text", label = paste0("> ", gauge[gauge$variable == eval_col, "percentage"] * 100, "%"), 
                 x=0.2, y=0.5, size=10, color="#D9774A") +
        annotate("text", label = paste0("(completed: ", gauge[gauge$variable == eval_col, "value"], ")"),
                 x=0.4, y=0.5, size=3) +
        annotate("text", label = paste0("(target: ", gauge[gauge$variable == target_col, "value"], ")"),
                 x=0.5, y=0.5, size=3)
    }
    
    p = p + 
      theme_void() +
      theme(plot.title = element_text(size=10),
            plot.margin = margin(t=0, r=0, b=-5, l=0, "cm"),
            legend.position = "none")
    
    return(p)
  }
  
  output$gauge_deliver <- renderPlot({
    
    gauge_output = gauge_chart_producer("Deliverables", "units_deliverables", "session", "session_count", "program_count")
    gauge_plot = gauge_chart_plotter(gauge_output, "Deliverables")
    
    return(gauge_plot)
  })
  
  output$gauge_benefit <- renderPlot({
    
    gauge_output = gauge_chart_producer("Beneficiaries", "units_beneficiaries", "enrol", "enrollment_number", "attendance_number")
    gauge_plot = gauge_chart_plotter(gauge_output, "Beneficiaries")
    
    return(gauge_plot)
  })
  
  output$gauge_attend <- renderPlot({
    
    gauge_output = gauge_chart_producer("Attendance", "units_attendance", "attend", "attendance_number", "attendance_number")
    gauge_plot = gauge_chart_plotter(gauge_output, "Attendance")
    
    return(gauge_plot)
  })
  
  B2_distribution = function(col, name) {
    
    temp = df_prog
    temp = subset(temp, output == "Disease specific basic training workshop")
    if (input$radio_btn_page_1_progress_year != "Entire 3Y-Period") {
      temp <- subset(temp, Year == input$radio_btn_page_1_progress_year)
    }
    
    stat_current = progress_columns(temp)
    
    stat_current = stat_current[, c("subcode", col)]
    colnames(stat_current) = c("subcode","value")
    
    p = plot_ly(data = stat_current, values = ~value, labels = ~subcode, 
                type = "pie", textposition = 'inside', textinfo = 'label+value+percent', hoverinfo = 'text',
                text = ~paste('Program Code (Subcode) Category : ', subcode, '<br>', col, " : ", value), 
                marker = list(colors = c('rgb(211,94,96)', 'rgb(198,183,123)', 'rgb(144,103,167)', 'rgb(171,104,87)'), 
                              line = list(color = '#FFFFFF', width = 1)),
                showlegend = FALSE) %>%
                layout(title = list(text = paste0("Disease specific basic training workshop:  ", "<br>", name), 
                                    font = list(size = 14)))
    
    return(p)
  }
  
  output$distribution_B2_deliver <- renderPlotly({
    p = B2_distribution("session_count", "Deliverables")
    return(p)
  })
  
  output$distribution_B2_benefit <- renderPlotly({
    p = B2_distribution("enrollment_number", "Beneficiaries")
    return(p)
  })
  
  output$distribution_B2_attend <- renderPlotly({
    p = B2_distribution("attendance_number", "Attendance")
    return(p)
  })
  
  output$page_1_progress_table <- renderDataTable({
    
    temp = df_prog
    stat_current = progress_columns(temp)
    target = df_target
    target$Output = trimws(target$Output)
    
    target = dplyr::bind_cols(progress_target(target), target[,c("Code","Output_main")])
    target = unique(target[,!names(target) %in% c("subcode","Code")])
    
    merged = merge(target, stat_current, by.x=c("Output"), by.y=c("output"), all.x=T)
    merged[,c("program_count","session_count","enrollment_number","attendance_number")][
      is.na(merged[,c("program_count","session_count","enrollment_number","attendance_number")])
      ] = 0
    
    merged$Progress_Deliverables = ifelse(is.na(merged$units_deliverables) | is.na(merged[, colnames(target)[grepl("Deliverables", colnames(target))]]), NA, 
                                          ifelse(merged$units_deliverables == "count", 
                                                 merged$program_count / merged[, colnames(target)[grepl("Deliverables", colnames(target))]] * 100,
                                                 ifelse(merged$units_deliverables == "session",
                                                        merged$session_count / merged[, colnames(target)[grepl("Deliverables", colnames(target))]] * 100, NA)))
    
    merged$Progress_Beneficiaries = ifelse(is.na(merged$units_beneficiaries) | is.na(merged[, colnames(target)[grepl("Beneficiaries", colnames(target))]]), NA, 
                                           ifelse(merged$units_beneficiaries == "enrol", 
                                                  merged$enrollment_number / merged[, colnames(target)[grepl("Beneficiaries", colnames(target))]] * 100,
                                                  ifelse(merged$units_beneficiaries == "attend",
                                                         merged$attendance_number / merged[, colnames(target)[grepl("Beneficiaries", colnames(target))]] * 100,
                                                         ifelse(merged$units_beneficiaries == "join",
                                                                merged$attendance_number / merged[, colnames(target)[grepl("Beneficiaries", colnames(target))]] * 100, NA))))
    
    merged$Progress_Attendance = ifelse(is.na(merged[, colnames(target)[grepl("Attendance", colnames(target))]]), NA,
                                        merged$attendance_number / merged[, colnames(target)[grepl("Attendance", colnames(target))]] * 100)
    
    merged[,(length(merged)-2):length(merged)] = round(merged[,(length(merged)-2):length(merged)], 2)
    merged = merged[,c(
      "Output","Output_main","subcode",
      colnames(target)[grepl("Deliverables", colnames(target))], 
      colnames(target)[grepl("Beneficiaries", colnames(target))],
      colnames(target)[grepl("Attendance", colnames(target))],
      "program_count","session_count","enrollment_number","attendance_number",
      "Progress_Deliverables","Progress_Beneficiaries","Progress_Attendance"
      )]
    
    gicon <- function(x) as.character(icon(x, lib = "glyphicon"))
    merged$Progress_Deliverables_Flag = ifelse(merged$Progress_Deliverables >= 100, 
                                               as.character(icon("ok", lib = "glyphicon")), 
                                               as.character(icon("exclamation-sign", lib = "glyphicon")))
    merged$Progress_Beneficiaries_Flag = ifelse(merged$Progress_Beneficiaries >= 100, 
                                                as.character(icon("ok", lib = "glyphicon")), 
                                                as.character(icon("exclamation-sign", lib = "glyphicon")))
    merged$Progress_Attendance_Flag = ifelse(merged$Progress_Attendance >= 100, 
                                             as.character(icon("ok", lib = "glyphicon")), 
                                             as.character(icon("exclamation-sign", lib = "glyphicon")))
    
    merged$Progress_Deliverables = sapply(merged$Progress_Deliverables, function(x) paste0(format(x, nsamll = 2), "%"))
    merged$Progress_Beneficiaries = sapply(merged$Progress_Beneficiaries, function(x) paste0(format(x, nsamll = 2), "%"))
    merged$Progress_Attendance = sapply(merged$Progress_Attendance, function(x) paste0(format(x, nsamll = 2), "%"))
    
    merged = dplyr::bind_cols(
      merged[, c("Progress_Deliverables_Flag","Progress_Beneficiaries_Flag","Progress_Attendance_Flag",
                 "Progress_Deliverables","Progress_Beneficiaries","Progress_Attendance")],
      merged[, colnames(merged)[grepl("Progress_", colnames(merged)) == F]]
    )
    
    datatable(merged,
              caption = tags$caption(
                style = 'caption-side:top;color:black;',
                tags$b("Program Progress Review Table：")
              ),
              options = list(dom = "Blfrtip", lengthMenu = c(10, 20, 50), pageLength = 50, autoWidth = TRUE,
                             columnDefs = list(list(width = '120px', targets = "_all")), 
                             scrollX = "300px", scrollY = "600px", fixedHeader=FALSE), 
              fillContainer = T, rownames = FALSE, escape = FALSE)
  })
  
  ##### Screening Page Outputs #####
  
  screening_dataframe <- reactive({
    if (input$slicer_survey == "Pain Screening") {
      data = screen_pain
    } else if (input$slicer_survey == "Sleep Disorder Screening") {
      data = screen_sleep
    } else if (input$slicer_survey == "Cardiovascular Disease Screening") {
      data = screen_cvd
    }
    return(data)
  })
  
  output$slicer_page_2_collector <- renderUI({
    temp <- screening_dataframe()
    description <- unique(temp$Description)
    pickerInput(
      inputId = "slicer_collect", label = "Survey Collectors：", 
      choices = sort(description),
      selected = sort(description),
      options = list(`actions-box` = TRUE, size = 10, `none-Selected-Text` = "please select collector(s)： "), 
      multiple = TRUE
    )
  })
  
  output$page_2_date_range_selector <- renderUI({
    temp <- screening_dataframe()
    dateInput(inputId = "page_2_date_picker",
              label = "Starting Date From: ",
              min = min(temp$date),
              max = max(temp$date),
              value = min(temp$date),
              format = "yyyy-mm-dd", startview = "month", weekstart = 1)
  })
  
  dygraph_time_series_plot = function(dim, dimname, labdiv, zero_required) {
    
    temp <- screening_dataframe()
    temp <- subset(temp, Description %in% input$slicer_collect)
    
    if (length(input$page_2_date_picker) == 0) {} 
    else { temp <- subset(temp, as.Date(temp$date) >= input$page_2_date_picker) }
    
    if (input$page_2_review_perspective == "View By Day") {
      aggre_temp <- temp %>% dplyr::group_by_at(c("date", dim)) %>% dplyr::summarise(count = n())
    } else if (input$page_2_review_perspective == "View By Week") {
      aggre_temp <- temp %>% dplyr::group_by_at(c("week", dim)) %>% dplyr::summarise(count = n())
    } else if (input$page_2_review_perspective == "View By Month") {
      aggre_temp <- temp %>% dplyr::group_by_at(c("month", dim)) %>% dplyr::summarise(count = n())
    }
    
    colnames(aggre_temp)[1] = "time_range"
    series_aggre_temp <- reshape2::dcast(aggre_temp, as.formula(paste0("time_range ~ ", dim)), value.var = "count")
    series_aggre_temp[is.na(series_aggre_temp)] = 0
    
    if (zero_required == TRUE) {
      for (m in 1:length(input$slicer_collect)) {
        if (!(input$slicer_collect[m] %in% screening_dataframe()[, dim])) {
          series_aggre_temp[, input$slicer_collect[m]] = 0
        }
      }
    }
    
    if (input$page_2_review_perspective != "View By Month") {
      series_aggre_temp$time_range <- as.Date(series_aggre_temp$time_range)
      tsdata <- xts(series_aggre_temp[,2:length(series_aggre_temp)], series_aggre_temp$time_range)
    } else {
      tsdata <- series_aggre_temp %>% 
        ts(start=c(
          min(as.numeric(substr(series_aggre_temp$time_range,1,4))), 
          min(as.numeric(substr(subset(series_aggre_temp, as.numeric(substr(series_aggre_temp$time_range,1,4)) == min(as.numeric(substr(series_aggre_temp$time_range,1,4))))$time_range,6,7)))),
          freq=12)
      tsdata = tsdata[,2:dim(tsdata)[2]]
    }
    
    p = dygraph(tsdata, main = paste0("Number of people who had undergone ", input$slicer_survey, " (based on ", dimname, ")")) %>%
      dyLegend(width = 400, show = "always", hideOnMouseOut = FALSE, labelsDiv = labdiv, labelsSeparateLines = TRUE) %>%
      dyAxis("y", label = "Respondent Counts", valueRange = c(0, round(max(series_aggre_temp[,2:length(series_aggre_temp)]) * 1.2, 0))) %>%
      dyRangeSelector() %>% 
      dyCSScontrol()
    
    return(p)
  }
  
  output$time_series_plot_01 <- renderDygraph({
    p = dygraph_time_series_plot("Description", "Collectors", "legendDiv1", TRUE)
    return(p)
  })
  
  output$time_series_plot_02 <- renderDygraph({
    p = dygraph_time_series_plot("Risk_Category", "Risk Categories", "legendDiv2", FALSE)
    return(p)
  })
  
  output$page_2_pie_age <- renderPlotly({
    data = screening_dataframe()
    data = data %>% dplyr::group_by(Age) %>% dplyr::summarise(value = n())
    
    p = plot_ly(data = data, values = ~value, labels = ~Age, 
                type = "pie", textposition = 'inside', textinfo = 'label+value+percent', hoverinfo = 'text',
                text = ~paste('Age Group : ', Age, '<br>', "N : ", value), 
                marker = list(colors = c('rgb(246,249,150)', 'rgb(95,184,165)'), 
                              line = list(color = '#FFFFFF', width = 1)),
                showlegend = FALSE) %>%
      layout(title = list(text = "Participant Demographics: Age Group", 
                          font = list(size = 14)))
    return(p)
  })
  
  output$page_2_pie_gender <- renderPlotly({
    data = screening_dataframe()
    data = data %>% dplyr::group_by(Gender) %>% dplyr::summarise(value = n())
    
    p = plot_ly(data = data, values = ~value, labels = ~Gender, 
                type = "pie", textposition = 'inside', textinfo = 'label+value+percent', hoverinfo = 'text',
                text = ~paste('Gender : ', Gender, '<br>', "N : ", value), 
                marker = list(colors = c('rgb(249,168,150)', 'rgb(150,186,249)'), 
                              line = list(color = '#FFFFFF', width = 1)),
                showlegend = FALSE) %>%
      layout(title = list(text = "Participant Demographics: Gender", 
                          font = list(size = 14)))
    return(p)
  })
  
  output$page_2_risk_percent_bar_age <- renderPlotly({
    data = screening_dataframe()
    data = data %>% dplyr::group_by(Age, Risk_Category) %>% dplyr::summarise(value = n())
    data = data %>% dplyr::group_by(Age) %>% dplyr::mutate(percent = value / sum(value) * 100)
    
    p = plot_ly(data = data, x = ~percent, y = ~Age, color = ~Risk_Category, name = ~Risk_Category,
                type = "bar", orientation = "h",
                hovertemplate = ~paste0(input$slicer_survey, "<br>", Age, ": ", round(percent, 2), "%"),
                colors = c('#E1C8B4','#ffc107','#e83e8c')) %>%
          layout(title = list(text = "Proportions of Risk categories detected by: Age Group", 
                              font = list(size = 14)),
                 barmode = "stack",
                 bargap = 0.5)
    
    return(p)
  })
  
  output$page_2_risk_percent_bar_gender <- renderPlotly({
    data = screening_dataframe()
    data = data %>% dplyr::group_by(Gender, Risk_Category) %>% dplyr::summarise(value = n())
    data = data %>% dplyr::group_by(Gender) %>% dplyr::mutate(percent = value / sum(value) * 100)
    
    p = plot_ly(data = data, x = ~percent, y = ~Gender, color = ~Risk_Category, name = ~Risk_Category,
                type = "bar", orientation = "h",
                hovertemplate = ~paste0(input$slicer_survey, "<br>", Gender, ": ", round(percent, 2), "%"),
                colors = c('#E1C8B4','#ffc107','#e83e8c')) %>%
      layout(title = list(text = "Proportions of Risk categories detected by: Gender", 
                          font = list(size = 14)),
             barmode = "stack",
             bargap = 0.5)
    
    return(p)
  })
  
  radar_plotter = function(data, title, input_name) {
    data <- subset(data, Description %in% input$slicer_collect)
    if (length(input$page_2_date_picker) == 0) {} 
    else { data <- subset(data, as.Date(data$date) >= input$page_2_date_picker) }
    
    impact_data = data[, 8:(length(data)-5)]
    impact_data = impact_data %>% dplyr::summarise_all(mean)
    
    p = plot_ly(type = 'scatterpolar',
                r = as.numeric(impact_data[1,]),
                theta = colnames(impact_data),
                fill = "toself",
                name = input_name
    ) %>% layout(polar=list(radialaxis = list(visible = T, range = c(0, round(max(impact_data[1,]), -1)))),
                 title=title,
                 margin=list(t=50))
    return(p)
  }
  
  output$page_2_radar_survey_1 <- renderPlotly({
    chart = radar_plotter(screen_pain, "Average Score of Impacts from Pains", "Pain Screeing")
    return(chart)
  })
  
  output$page_2_radar_survey_2 <- renderPlotly({
    chart = radar_plotter(screen_sleep, "Average Score of Impacts from Sleep Disorders", "Sleep Disorder Screeing")
    return(chart)
  })
  
  output$page_2_radar_survey_3 <- renderPlotly({
    chart = radar_plotter(screen_cvd, "Average Score of Risk Items of Cardiovascular Disease", "CVD Screeing")
    return(chart)
  })
  
  output$page_2_map <- renderPlotly({
    data = dplyr::bind_rows(screen_pain[,1:7], screen_sleep[,1:7], screen_cvd[,1:7])
    major_region = c()
    for (n in 1:nrow(data)) {
      for (d in 1:length(region_dict)) {
        if (data$Region[n] %in% region_dict[[d]]) {
          major_region[n] = names(region_dict)[d]
        }
      }
    }
    data$major_region = major_region
    
    inner_dat = data %>% dplyr::group_by(major_region, Region) %>% dplyr::summarise(case_count = n())
    outer_dat = inner_dat %>% dplyr::group_by(major_region) %>% dplyr::summarise(case_count = sum(case_count))
    colnames(outer_dat)[1] = "Region"
    dat = bind_rows(outer_dat, inner_dat)
    colnames(dat) = c("group_var","case_count","parent")
    dat$parent[is.na(dat$parent)] = "All Districts"
    
    k = plot_ly(type='treemap',
                labels = c("All Districts", dat$group_var),
                parents = c("", dat$parent),
                values = c(sum(dat$case_count) / 2, dat$case_count),
                text_info = "label+value",
                branchvalues = "total",
                domain=list(column=0),
                marker=list(colors=brewer.pal(length(region_dict), "Pastel1"))) %>%
      layout(title = list(text = "Treemap for number of participants by Regions", x = 0.05),
             margin=list(t=50))
    
    return(k)
  })
  
  ##### Volunteer Page Outputs #####
  
  output$picker_2nd_dim = renderUI({
    if (input$switch_2nd_dim == TRUE) {
      if (input$picker_1st_dim == "School") {
        choice_opts = c("Department", "Grade", "Year of Services")
      } else if (input$picker_1st_dim == "Department") {
        choice_opts = c("Grade", "Year of Services")
      } else if (input$picker_1st_dim == "Grade") {
        choice_opts = c("School", "Department", "Year of Services")
      } else if (input$picker_1st_dim == "Year of Services") {
        choice_opts = c("School", "Department", "Grade")
      }
      pickerInput('slicer_2nd_dim', label = "Secondary Grouping :",
                  choices = choice_opts, 
                  multiple = FALSE,
                  selected = choice_opts[1], 
                  options = list(style = "btn-warning"))
    }
  })
  
  output$picker_1st_dim_single = renderUI({
    if (input$switch_2nd_dim == TRUE) {
      if (input$picker_1st_dim == "School") {
        choice_opts = school_opts
      } else if (input$picker_1st_dim == "Department") {
        choice_opts = dept_opts
      } else if (input$picker_1st_dim == "Grade") {
        choice_opts = grade_opts
      } else if (input$picker_1st_dim == "Year of Services") {
        choice_opts = yos_opts
      }
      pickerInput('slicer_1st_dim_single', label = "Choosen value of the First Dimension :",
                  choices = choice_opts, 
                  multiple = FALSE,
                  selected = choice_opts[1], 
                  options = list(style = "btn-warning"))
    }
  })
  
  output$page_3_box_full_member_percent = renderbs4InfoBox({
    tem = subset(vol, EMPLOYMENT_DATE < as.Date(paste0(substr(input$page_3_group_button_year, 6, 9), "-04-01")) &
                      EMPLOYMENT_DATE >= as.Date(paste0(substr(input$page_3_group_button_year, 1, 4), "-04-01")))
    
    valuelabel = round(nrow(subset(tem, TERM == "Full Membership")) / nrow(tem) * 100, 2)
    
    vbox <- bs4InfoBox(
      value = paste0(valuelabel, "%"), 
      title = "Full Membership workforce (%)", 
      color = "orange", icon = icon("users-rectangle", lib = "font-awesome")
    )
    return(vbox)
  })
  
  output$page_3_box_full_member_leavers_percent = renderbs4InfoBox({
    tem = subset(vol, EMPLOYMENT_DATE < as.Date(paste0(substr(input$page_3_group_button_year, 6, 9), "-04-01")) &
                      EMPLOYMENT_DATE >= as.Date(paste0(substr(input$page_3_group_button_year, 1, 4), "-04-01")))
    
    valuelabel = round(nrow(subset(tem, TERM == "Full Membership" & !is.na(Leaving_Type))) / nrow(subset(tem, !is.na(Leaving_Type))) * 100, 2)
    
    vbox <- bs4InfoBox(
      value = paste0(valuelabel, "%"), 
      title = "Leavers with Full Membership (%)", 
      color = "orange", icon = icon("users-slash", lib = "font-awesome")
    )
    return(vbox)
  })
  
  output$page_3_box_permanent_member_percent = renderbs4InfoBox({
    tem = vol
    valuelabel = round(nrow(subset(tem, TYPE == "PERMANENT")) / nrow(tem) * 100, 2)
    vbox <- bs4InfoBox(
      value = paste0(valuelabel, "%"), 
      title = "Permanent Service Commitments (%)", 
      color = "lightblue", icon = icon("users-rectangle", lib = "font-awesome")
    )
    return(vbox)
  })
  
  output$page_3_box_permanent_member_leavers_percent = renderbs4InfoBox({
    tem = vol
    valuelabel = round(nrow(subset(tem, TYPE == "PERMANENT" & !is.na(Leaving_Type))) / nrow(subset(tem, !is.na(Leaving_Type))) * 100, 2)
    vbox <- bs4InfoBox(
      value = paste0(valuelabel, "%"), 
      title = "Leavers with Permanent Service Commitments (%)", 
      color = "lightblue", icon = icon("users-slash", lib = "font-awesome")
    )
    return(vbox)
  })
  
  observe({
    df <- subset(vol, SCHOOL %in% input$page_3_slicer_school)
    updatePickerInput(session, "page_3_slicer_dept", 
                      choices = sort(unique(df$DEPARTMENT)),
                      selected = sort(unique(df$DEPARTMENT))
    )
  })
  
  time_specific_global_hc = function(start, end) {
    tem_list = list()
    for (t in 1:length(start)) {
      tem = vol %>% dplyr::summarise(
        begin_HC = sum(EMPLOYMENT_DATE <= start[t] & (is.na(TERMINATION_DATE) | TERMINATION_DATE > start[t]), na.rm=T),
        end_HC = sum(EMPLOYMENT_DATE <= end[t] & (is.na(TERMINATION_DATE) | TERMINATION_DATE > end[t]), na.rm=T)
      )
    }
    tem = data.frame(do.call("cbind", tem))
    rownames(tem) = NULL
    return(tem)
  }
  
  yos_info_extractor = function(yos_query_table) {
    
    ### YOS status per month (rolling)
    yos_query_table$YOS_below_1_year = apply(yos_query_table, 1, function(x) length(x[!is.na(x) & x == "< 1 year"]))
    yos_query_table$YOS_below_3_years = apply(yos_query_table, 1, function(x) length(x[!is.na(x) & x == "1 year - < 3 years"]))
    yos_query_table$YOS_below_5_years = apply(yos_query_table, 1, function(x) length(x[!is.na(x) & x == "3 year - < 5 years"]))
    yos_query_table$YOS_over_5_years = apply(yos_query_table, 1, function(x) length(x[!is.na(x) & x == "5 years or above"]))
    
    ### YOS at time of leave
    yos_query_table$YOS_max = ifelse(yos_query_table$YOS_over_5_years > 0, "5 years or above",
                                     ifelse(yos_query_table$YOS_below_5_years > 0, "3 year - < 5 years", 
                                            ifelse(yos_query_table$YOS_below_3_years > 0, "1 year - < 3 years", 
                                                   "< 1 year")))
    return(yos_query_table)
  }
  
  yos_determinator = function(merged_yos_df, slicer_value, account_period_start, account_period_end) {
    
    ### determine rows to pass to plots (controlled by YOS slicer)
    merged_yos_df$inclusion_col = FALSE
    
    if ("< 1 year" %in% slicer_value) {
      merged_yos_df$inclusion_col[merged_yos_df$inclusion_col == FALSE & merged_yos_df$YOS_below_1_year > 0] = TRUE
    }
    if ("1 year - < 3 years" %in% slicer_value) {
      merged_yos_df$inclusion_col[merged_yos_df$inclusion_col == FALSE & merged_yos_df$YOS_below_3_years > 0] = TRUE
    }
    if ("3 year - < 5 years" %in% slicer_value) {
      merged_yos_df$inclusion_col[merged_yos_df$inclusion_col == FALSE & merged_yos_df$YOS_below_5_years > 0] = TRUE
    }
    if ("5 years or above" %in% slicer_value) {
      merged_yos_df$inclusion_col[merged_yos_df$inclusion_col == FALSE & merged_yos_df$YOS_over_5_years > 0] = TRUE
    }
    
    merged_yos_df$inclusion_col[
      merged_yos_df$inclusion_col == FALSE & 
        rowSums(merged_yos_df[,c("YOS_below_1_year","YOS_below_3_years","YOS_below_5_years","YOS_over_5_years")]) == 0 & 
        merged_yos_df$TERMINATION_DATE >= account_period_start &
        merged_yos_df$TERMINATION_DATE <= account_period_end
    ] = TRUE
    
    return(merged_yos_df)
  }
  
  page_3_react_overall_data = reactive({
    temp = vol
    temp = subset(temp,
                  SCHOOL %in% input$page_3_slicer_school &
                  DEPARTMENT %in% input$page_3_slicer_dept & 
                  GRADE %in% input$page_3_slicer_grade &
                  TERM %in% input$slicer_contract_term &
                  TYPE %in% input$slicer_contract_type)
    return(temp)
  })
  
  page_3_react_overall_data_with_yos_year = reactive({
    temp = page_3_react_overall_data()
    
    yos_query_table = yos_dat
    yos_query_table = yos_info_extractor(yos_query_table)
    rows = rownames(temp)
    yos_query_table = yos_query_table[rows, ]
    
    temp = dplyr::bind_cols(temp, yos_query_table)
    return(temp)
  })
  
  page_3_react_overall_data_with_yos_month = reactive({
    temp = page_3_react_overall_data()
    
    yos_query_table = yos_dat
    
    if (input$page_3_group_button_year == "2020-2021") {
      yos_query_table = yos_query_table[,c(length(yos_query_table), 1:12)]
    } else if (input$page_3_group_button_year == "2021-2022") {
      yos_query_table = yos_query_table[,c(length(yos_query_table), 13:24)]
    } else if (input$page_3_group_button_year == "2022-2023") {
      yos_query_table = yos_query_table[,c(length(yos_query_table), 25:36)]
    }
    yos_query_table = yos_info_extractor(yos_query_table)
    
    rows = rownames(temp)
    yos_query_table = yos_query_table[rows, ]
    
    temp = dplyr::bind_cols(temp, yos_query_table)
    return(temp)
  })
  
  page_3_react_group_overall_data = reactive({
    temp = vol
    temp = subset(temp,
                  TERM %in% input$slicer_contract_term &
                  TYPE %in% input$slicer_contract_type)
    return(temp)
  })
  
  page_3_react_group_overall_data_year = reactive({
    temp = page_3_react_group_overall_data()
    
    yos_query_table = yos_dat
    yos_query_table = yos_info_extractor(yos_query_table)
    rows = rownames(temp)
    yos_query_table = yos_query_table[rows, ]
    
    temp = dplyr::bind_cols(temp, yos_query_table)
    return(temp)
  })
  
  page_3_react_group_overall_data_month = reactive({
    temp = page_3_react_group_overall_data()
    
    yos_query_table = yos_dat
    if (input$page_3_group_button_year == "2020-2021") {
      yos_query_table = yos_query_table[,c(length(yos_query_table), 1:12)]
    } else if (input$page_3_group_button_year == "2021-2022") {
      yos_query_table = yos_query_table[,c(length(yos_query_table), 13:24)]
    } else if (input$page_3_group_button_year == "2022-2023") {
      yos_query_table = yos_query_table[,c(length(yos_query_table), 25:36)]
    }
    yos_query_table = yos_info_extractor(yos_query_table)
    
    rows = rownames(temp)
    yos_query_table = yos_query_table[rows, ]
    
    temp = dplyr::bind_cols(temp, yos_query_table)
    return(temp)
  })
  
  group_df_producer = function(df_arg, group_1=NA, group_2=NA) {
    if (!is.na(group_1) & !is.na(group_2)) {
      df_arg = df_arg %>% group_by_at(c(group_1, group_2))
    } else if (!is.na(group_1) & is.na(group_2)) {
      df_arg = df_arg %>% group_by_at(group_1)
    } else {
    }
    return(df_arg)
  }
  
  group_df_transform = function(first_dim, switch, second_dim, data, grp_cols_mapping) {
    
    if (first_dim == "Department" | first_dim == "School" | first_dim == "Grade") {
      if (switch == TRUE) {
        if (second_dim != "Year of Services") {
          grouped_data = group_df_producer(df_arg = data, group_1 = grp_cols_mapping[first_dim], group_2 = grp_cols_mapping[second_dim])
        } else {
          grouped_data = group_df_producer(df_arg = data, group_1 = grp_cols_mapping[first_dim])
        }
      } else {
        grouped_data = group_df_producer(df_arg = data, group_1 = grp_cols_mapping[first_dim])
      }
    } else if (first_dim == "Year of Services") {
      grouped_data = data
    }
    
    return(grouped_data)
  }
  
  page_3_react_plot_module = function(data, time_opts, time_name, dates_start, dates_end) {
    
    ## YOS rolling status for monthly data:  using YOS_max for #leaves (YOS when terminated); 
    yos_data = data[,c(12:(length(dates_start)+12-1))]
    colnames(data)[12:(length(dates_start)+12-1)] = colnames(yos_data)
    
    ## computed dataframe
    tem = list()
    for (n in 1:length(dates_start)) {
      sym_yos_var = sym(colnames(yos_data)[n])
      if (input$page_3_slicer_view_mode == "Group-specific") {
        temdata = data %>% 
          dplyr::summarise(
            Period = time_opts[n],
            from = dates_start[n],
            to = dates_end[n],
            new = sum(EMPLOYMENT_DATE <= dates_end[n] & EMPLOYMENT_DATE >= dates_start[n], na.rm=T),
            leave_active = sum(YOS_max %in% input$page_3_slicer_yos & Leaving_Type == "Active Leave" & 
                                 TERMINATION_DATE <= dates_end[n] & TERMINATION_DATE >= dates_start[n], na.rm=T),
            leave_passive = sum(YOS_max %in% input$page_3_slicer_yos & Leaving_Type == "Passive Leave" & 
                                  TERMINATION_DATE <= dates_end[n] & TERMINATION_DATE >= dates_start[n], na.rm=T),
            begin_HC = sum(YOS_max %in% input$page_3_slicer_yos & 
                             EMPLOYMENT_DATE <= dates_start[n] & (is.na(TERMINATION_DATE) | TERMINATION_DATE >= dates_start[n]), na.rm=T),
            end_HC = sum(YOS_max %in% input$page_3_slicer_yos & 
                           EMPLOYMENT_DATE <= dates_end[n] & (is.na(TERMINATION_DATE) | TERMINATION_DATE >= dates_end[n]), na.rm=T)
          )
      } else if (input$page_3_slicer_view_mode == "Fixed Total") {
        temdata = data %>% 
          dplyr::summarise(
            Period = time_opts[n],
            from = dates_start[n],
            to = dates_end[n],
            new = sum(EMPLOYMENT_DATE <= dates_end[n] & EMPLOYMENT_DATE >= dates_start[n], na.rm=T),
            leave_active = sum(YOS_max %in% input$page_3_slicer_yos & Leaving_Type == "Active Leave" & 
                                 TERMINATION_DATE <= dates_end[n] & TERMINATION_DATE >= dates_start[n], na.rm=T),
            leave_passive = sum(YOS_max %in% input$page_3_slicer_yos & Leaving_Type == "Passive Leave" & 
                                  TERMINATION_DATE <= dates_end[n] & TERMINATION_DATE >= dates_start[n], na.rm=T)
          )
        temdata_hc = time_specific_global_hc(dates_start[n], dates_end[n])
        temdata = cbind(temdata, temdata_hc)
      }
      colnames(temdata)[colnames(temdata) == "Period"] = time_name
      tem[[n]] = temdata
    }
    
    tem = do.call("rbind", tem)
    rownames(tem) = NULL
    
    if (time_name == "Year") {
      denominator = (tem$begin_HC + tem$end_HC) / 2 * 12
    } else if (time_name == "Month") {
      denominator = (tem$begin_HC + tem$end_HC) / 2
    }
    
    tem$dropout_active = round(tem$leave_active / denominator, 3) * 100
    tem$dropout_passive = round(tem$leave_passive / denominator, 3) * 100
    tem$total_turnover = tem$dropout_active + tem$dropout_passive
    
    return(tem)
  }
  
  page_3_react_group_plot_module = function(data, time_opts, time_name, dates_start, dates_end) {
    
    ## YOS rolling status for monthly data:  using YOS_max for #leaves (YOS when terminated); 
    yos_data = data[,c(12:(length(dates_start)+12-1))]
    colnames(data)[12:(length(dates_start)+12-1)] = colnames(yos_data)
    
    grp_cols_mapping = {}
    grp_cols_mapping["Department"] = "DEPARTMENT"
    grp_cols_mapping["School"] = "SCHOOL"
    grp_cols_mapping["Grade"] = "GRADE"
    
    grouped_data = group_df_transform(input$picker_1st_dim, 
                                      input$switch_2nd_dim, 
                                      input$slicer_2nd_dim,
                                      data, 
                                      grp_cols_mapping)
    
    tem = list()
    
    for (n in 1:length(dates_start)) {
      
      if (input$picker_1st_dim == "Year of Services") {
        yos_groups_col = colnames(yos_data)[n]
        grouped_data_2 = grouped_data %>% dplyr::group_by_at(yos_groups_col)
        if (input$switch_2nd_dim == TRUE) {
          grouped_data_2 = grouped_data_2 %>% dplyr::group_by_at(grp_cols_mapping[input$slicer_2nd_dim], .add=TRUE)
        }
      } else if (input$switch_2nd_dim == TRUE) {
        if (input$slicer_2nd_dim == "Year of Services") {
          yos_groups_col = colnames(yos_data)[n]
          grouped_data_2 = grouped_data %>% dplyr::group_by_at(yos_groups_col, .add=TRUE)
        } else {
          grouped_data_2 = grouped_data 
        }
      } else {
          grouped_data_2 = grouped_data 
      }
      
      if (input$page_3_slicer_view_mode == "Group-specific") {
        
        temdata = grouped_data_2 %>% 
          dplyr::summarise(
            Period = time_opts[n],
            from = dates_start[n],
            to = dates_end[n],
            new = sum(EMPLOYMENT_DATE <= dates_end[n] & EMPLOYMENT_DATE >= dates_start[n], na.rm=T),
            leave_active = sum(Leaving_Type == "Active Leave" & TERMINATION_DATE <= dates_end[n] & TERMINATION_DATE >= dates_start[n], na.rm=T),
            leave_passive = sum(Leaving_Type == "Passive Leave" & TERMINATION_DATE <= dates_end[n] & TERMINATION_DATE >= dates_start[n], na.rm=T),
            begin_HC = sum(EMPLOYMENT_DATE <= dates_start[n] & (is.na(TERMINATION_DATE) | TERMINATION_DATE >= dates_start[n]), na.rm=T),
            end_HC = sum(EMPLOYMENT_DATE <= dates_end[n] & (is.na(TERMINATION_DATE) | TERMINATION_DATE >= dates_end[n]), na.rm=T)
          )
        
        if (input$picker_1st_dim == "Year of Services") {
          colnames(temdata)[1] = "yos"
          temdata = subset(temdata, !is.na(yos))
        } else if (input$switch_2nd_dim == TRUE) {
          if (input$slicer_2nd_dim == "Year of Services") {
            colnames(temdata)[2] = "yos"
            temdata = subset(temdata, !is.na(yos))
          }
        }
        
      } else if (input$page_3_slicer_view_mode == "Fixed Total") {
        
        temdata = grouped_data_2 %>% 
          dplyr::summarise(
            Period = time_opts[n],
            from = dates_start[n],
            to = dates_end[n],
            new = sum(EMPLOYMENT_DATE <= dates_end[n] & EMPLOYMENT_DATE >= dates_start[n], na.rm=T),
            leave_active = sum(Leaving_Type == "Active Leave" & TERMINATION_DATE <= dates_end[n] & TERMINATION_DATE >= dates_start[n], na.rm=T),
            leave_passive = sum(Leaving_Type == "Passive Leave" & TERMINATION_DATE <= dates_end[n] & TERMINATION_DATE >= dates_start[n], na.rm=T)
          )
        
        if (input$picker_1st_dim == "Year of Services") {
          colnames(temdata)[1] = "yos"
          temdata = subset(temdata, !is.na(yos))
        } else if (input$switch_2nd_dim == TRUE) { 
          if (input$slicer_2nd_dim == "Year of Services") {
            colnames(temdata)[2] = "yos"
            temdata = subset(temdata, !is.na(yos))
          }
        }
        
        temdata_hc = time_specific_global_hc(dates_start[n], dates_end[n])
        temdata = cbind(temdata, temdata_hc)
      }
      
      colnames(temdata)[colnames(temdata) == "Period"] = time_name
      tem[[n]] = temdata
    }
    
    tem = dplyr::bind_rows(tem)
    rownames(tem) = NULL
    
    if (time_name == "Year") {
      denominator = (tem$begin_HC + tem$end_HC) / 2 * 12
    } else if (time_name == "Month") {
      denominator = (tem$begin_HC + tem$end_HC) / 2
    }
    
    tem$dropout_active = round(tem$leave_active / denominator, 3) * 100
    tem$dropout_passive = round(tem$leave_passive / denominator, 3) * 100
    tem$total_turnover = tem$dropout_active + tem$dropout_passive
    
    return(tem)
  }
  
  page_3_react_plot_df_year = reactive({
    
    ## extract annual starts and ends
    dates_start = make_df_periods_start[c(1,13,25)]
    dates_end = make_df_periods_end[c(12,24,36)]
    ## get inclusion data
    data = page_3_react_overall_data_with_yos_year()
    data = yos_determinator(data, input$page_3_slicer_yos, dates_start[1], dates_end[length(dates_end)])
    data = subset(data, inclusion_col == TRUE)
    
    tem = page_3_react_plot_module(data, yr_opts, "Year", dates_start, dates_end)
    
    return(tem)
  })
  
  page_3_react_group_plot_df_year = reactive({
    
    ## extract annual starts and ends
    dates_start = make_df_periods_start[c(1,13,25)]
    dates_end = make_df_periods_end[c(12,24,36)]
    ## get inclusion data
    data = page_3_react_group_overall_data_year()
    
    tem = page_3_react_group_plot_module(data, yr_opts, "Year", dates_start, dates_end)
    
    return(tem)
  })
  
  page_3_react_plot_df_month = reactive({
    ## extract annual starts and ends
    if (input$page_3_group_button_year == "2020-2021") {
      dates_start = make_df_periods_start[c(1:12)]
      dates_end = make_df_periods_end[c(1:12)]
    } else if (input$page_3_group_button_year == "2021-2022") {
      dates_start = make_df_periods_start[c(13:24)]
      dates_end = make_df_periods_end[c(13:24)]
    } else if (input$page_3_group_button_year == "2022-2023") {
      dates_start = make_df_periods_start[c(25:36)]
      dates_end = make_df_periods_end[c(25:36)]
    }
    ## get inclusion data
    data = page_3_react_overall_data_with_yos_month()
    data = yos_determinator(data, input$page_3_slicer_yos, dates_start[1], dates_end[length(dates_end)])
    data = subset(data, inclusion_col == TRUE)
    
    tem = page_3_react_plot_module(data, mon_opts, "Month", dates_start, dates_end)
    
    return(tem)
  })
  
  page_3_react_group_plot_df_month = reactive({
    ## extract annual starts and ends
    if (input$page_3_group_button_year == "2020-2021") {
      dates_start = make_df_periods_start[c(1:12)]
      dates_end = make_df_periods_end[c(1:12)]
    } else if (input$page_3_group_button_year == "2021-2022") {
      dates_start = make_df_periods_start[c(13:24)]
      dates_end = make_df_periods_end[c(13:24)]
    } else if (input$page_3_group_button_year == "2022-2023") {
      dates_start = make_df_periods_start[c(25:36)]
      dates_end = make_df_periods_end[c(25:36)]
    }
    ## get inclusion data
    data = page_3_react_group_overall_data_month()
    
    tem = page_3_react_group_plot_module(data, mon_opts, "Month", dates_start, dates_end)
    
    return(tem)
  })
  
  output$page_3_year_plot = renderPlotly({
    tem = page_3_react_plot_df_year()
    
    ## plotting
    fig <- plot_ly(tem, x = ~Year, y = ~dropout_active, 
                   type = 'bar', name = 'Active Dropout', 
                   text = ~ paste0(dropout_active, "%"), textposition = 'auto',
                   hovertext = ~ paste0(Year, " (Active Reason Dropout Rate): <br> ", dropout_active, "%"), hoverinfo = "text",
                   marker = list(color = 'rgb(229,150,59)'))
    fig <- fig %>% add_trace(x = ~Year, y = ~dropout_passive, 
                             type = 'bar', name = 'Passive Dropout', 
                             text = ~ paste0(dropout_passive, "%"), textposition = 'auto',
                             hovertext = ~ paste0(Year, " (Passive Reason Dropout Rate): <br> ", dropout_passive, "%"), hoverinfo = "text",
                             marker = list(color = 'rgb(204,204,204)'))
    fig <- fig %>% layout(
      title = list(text = "Dropout Rate by Year", x=0),
      xaxis = list(title = 'Year'), 
      yaxis = list(title = 'Dropout Rate (%)'), 
      barmode = 'stack')
    
    return(fig)
  })
  
  output$page_3_month_plot = renderPlotly({
    tem = page_3_react_plot_df_month()
    
    ## plotting
    fig <- plot_ly(tem, x = ~factor(Month, levels=mon_opts), y = ~dropout_active, text = ~dropout_active,
                   type = 'scatter', mode = 'lines+markers+text', name = 'Active Dropout', 
                   hovertemplate = paste0(input$page_3_group_button_year, " ", "%{x}", " (Active Resaon Dropout): <br> ", "%{text}", "%"), 
                   texttemplate = paste0('%{text}', "%"),
                   textposition = 'top right', hoverinfo = 'text',
                   marker = list(size = 8, color = 'rgb(229,150,59)'),
                   line = list(color = 'rgb(229,150,59)', width = 4))
    fig <- fig %>% add_trace(x = ~factor(Month, levels=mon_opts), y = ~dropout_passive, text = ~dropout_passive,
                             type = 'scatter', mode = 'lines+markers+text', name = 'Passive Dropout', 
                             text = ~ paste0(input$page_3_group_button_year, " ", "%{x}", " (Passive Reason Dropout): <br> ", "%{text}", "%"), 
                             texttemplate = paste0('%{text}', "%"),
                             textposition = 'top right', hoverinfo = 'text',
                             marker = list(size = 8, color = 'rgb(204,204,204)'),
                             line = list(color = 'rgb(204,204,204)', width = 4))
    fig <- fig %>% layout(
      title = list(text = paste0("Dropout Rate by Month in ", input$page_3_group_button_year), x=0),
      xaxis = list(title = 'Month'), 
      yaxis = list(title = 'Dropout Rate (%)'), 
      hovermode = 'x unified')
    
    return(fig)
  })
  
  output$page_3_data_year = renderDataTable({
    tem = page_3_react_plot_df_year()
    tem$total_turnover = tem$dropout_active + tem$dropout_passive
    
    datatable(tem,
              options = list(dom = "Blfrtip", lengthMenu = c(10, 20, 50), pageLength = 50, autoWidth = TRUE,
                             columnDefs = list(list(width = '180px', targets = "_all")), 
                             scrollX = "500px", scrollY = "600px", fixedHeader=FALSE), 
              fillContainer = T, rownames = FALSE)
  })
  
  output$page_3_data_month = renderDataTable({
    tem = page_3_react_plot_df_month()
    tem$total_turnover = tem$dropout_active + tem$dropout_passive
    
    datatable(tem,
              options = list(dom = "Blfrtip", lengthMenu = c(10, 20, 50), pageLength = 50, autoWidth = TRUE,
                             columnDefs = list(list(width = '180px', targets = "_all")), 
                             scrollX = "500px", scrollY = "600px", fixedHeader=FALSE), 
              fillContainer = T, rownames = FALSE)
  })
  
  group_plotter_common_fn = function(tem, xcol, xlevels, target) {
    
    if (input$switch_2nd_dim == TRUE) {
      colnames(tem)[1:2] = c(input$picker_1st_dim, input$slicer_2nd_dim)
      tem = tem[tem[, input$picker_1st_dim] == input$slicer_1st_dim_single, ]
      tem = tem[!is.na(tem[,input$slicer_2nd_dim]), ]
      fig <- plot_ly(data = tem) %>%
        add_trace(x = ~factor(.data[[xcol]], levels=xlevels), y = ~.data[[target]], color = ~.data[[input$slicer_2nd_dim]], 
                  text = ~ paste0(.data[[target]], "%"), textposition="top right",
                  type = 'scatter', mode = 'lines+markers+text', name = ~.data[[input$slicer_2nd_dim]], 
                  text = ~ paste0(.data[[target]], "%"), textposition = 'auto',
                  hovertext = ~ paste0(.data[[input$slicer_2nd_dim]], " <br> ", 
                                       .data[[input$picker_1st_dim]], "; ", 
                                       ifelse(xcol == "Month", input$page_3_group_button_year, ""), " ", .data[[xcol]], 
                                       " (", input$group_button_leave_reason, "): <br> ", .data[[target]], "%"), 
                  hoverinfo = "text")
    } else {
      colnames(tem)[1] = input$picker_1st_dim
      tem = tem[!is.na(tem[,input$picker_1st_dim]), ]
      fig <- plot_ly(data = tem) %>%
        add_trace(x = ~factor(.data[[xcol]], levels=xlevels), y = ~.data[[target]], color = ~.data[[input$picker_1st_dim]], 
                  text = ~ paste0(.data[[target]], "%"), textposition="top right",
                  type="scatter", mode = "lines+markers+text", name = ~.data[[input$picker_1st_dim]], 
                  hovertext = ~ paste0(.data[[input$picker_1st_dim]], " <br> ", 
                                       ifelse(xcol == "Month", input$page_3_group_button_year, ""), " ", .data[[xcol]], 
                                       " (", input$group_button_leave_reason, "): <br> ", .data[[target]], "%"), 
                  hoverinfo = "text")
    }
    
    fig = fig %>%
        layout(title = list(text = ifelse(xcol == "Month", 
                                          paste0("Dropout Rate by Month in ", input$page_3_group_button_year, " by ", input$picker_1st_dim),
                                          paste0("Dropout Rate by Year by ", input$picker_1st_dim)), 
                            x=0),
               yaxis = list(title = ifelse(input$group_button_leave_reason == "Total", 
                                           paste0(input$group_button_leave_reason, " Dropout Rate (%)"),
                                           paste0(input$group_button_leave_reason, " Rate (%)")),
                            range = c(0, max(tem[, target]) * 1.1)),
               xaxis = list(title = xcol),
               legend = list(orientation = "h", yanchor = "top", y = -0.25)
               )
    return(fig)
  }
  
  output$page_3_group_plot_year = renderPlotly({
    
    tem = page_3_react_group_plot_df_year()
    
    if (input$group_button_leave_reason == "Active Dropout") {
      target = "dropout_active"
    } else if (input$group_button_leave_reason == "Passive Dropout") {
      target = "dropout_passive"
    } else {
      target = "total_turnover"
    }
    
    fig = group_plotter_common_fn(tem, "Year", yr_opts, target)
    
    return(fig)
  })
  
  output$page_3_group_plot_month = renderPlotly({
    
    tem = page_3_react_group_plot_df_month()
    
    if (input$group_button_leave_reason == "Active Dropout") {
      target = "dropout_active"
    } else if (input$group_button_leave_reason == "Passive Dropout") {
      target = "dropout_passive"
    } else {
      target = "total_turnover"
    }
    
    fig = group_plotter_common_fn(tem, "Month", mon_opts, target)
    
    return(fig)
  })
  
  output$page_3_data_group_year = renderDataTable({
    
    tem = page_3_react_group_plot_df_year()
    
    if (input$group_button_leave_reason == "Active Dropout") {
      target = "dropout_active"
    } else if (input$group_button_leave_reason == "Passive Dropout") {
      target = "dropout_passive"
    } else {
      target = "total_turnover"
    }
    
    if (input$switch_2nd_dim == TRUE) {
      colnames(tem)[1:2] = c(input$picker_1st_dim, input$slicer_2nd_dim)
      tem = tem[tem[, input$picker_1st_dim] == input$slicer_1st_dim_single, ]
    } else {
      colnames(tem)[1] = input$picker_1st_dim
    }
    
    datatable(tem,
              options = list(dom = "Blfrtip", lengthMenu = c(10, 20, 50), pageLength = 50, autoWidth = TRUE,
                             columnDefs = list(list(width = '180px', targets = "_all")), 
                             scrollX = "500px", scrollY = "600px", fixedHeader=FALSE), 
              fillContainer = T, rownames = FALSE)
  })
  
  output$page_3_data_group_month = renderDataTable({
    
    tem = page_3_react_group_plot_df_month()
    
    if (input$group_button_leave_reason == "Active Dropout") {
      target = "dropout_active"
    } else if (input$group_button_leave_reason == "Passive Dropout") {
      target = "dropout_passive"
    } else {
      target = "total_turnover"
    }
    
    if (input$switch_2nd_dim == TRUE) {
      colnames(tem)[1:2] = c(input$picker_1st_dim, input$slicer_2nd_dim)
      tem = tem[tem[, input$picker_1st_dim] == input$slicer_1st_dim_single, ]
    } else {
      colnames(tem)[1] = input$picker_1st_dim
    }
    
    datatable(tem,
              options = list(dom = "Blfrtip", lengthMenu = c(10, 20, 50), pageLength = 50, autoWidth = TRUE,
                             columnDefs = list(list(width = '180px', targets = "_all")), 
                             scrollX = "500px", scrollY = "600px", fixedHeader=FALSE), 
              fillContainer = T, rownames = FALSE)
  })
  
  ##### Engagement Measure/Evaluation Page Outputs #####
  
  observe({
    sel_x = input$page_4_picker_x_var
    updatePickerInput(session, 'page_4_picker_y_var', 
                      choices = oname[!oname %in% sel_x], 
                      selected = oname[!oname %in% sel_x][1])
  })
  
  output$page_4_scatter = renderPlotly({
    dfx = olist[[which(oname == input$page_4_picker_x_var)]]
    dfy = olist[[which(oname == input$page_4_picker_y_var)]]
    p = plot_ly(x = dfx$score, y = dfy$score, tpye = "scatter", marker = list(color = 'rgb(229,150,59)', size=25),
                hovertemplate = paste0(input$page_4_picker_x_var, " : ", dfx$score, "<br>", 
                                       input$page_4_picker_y_var, " : ", dfy$score, "<extra></extra>")) %>%
          layout(xaxis = list(title = input$page_4_picker_x_var),
                 yaxis = list(title = input$page_4_picker_y_var),
                 showlegend = F)
    return(p)
  })
  
  output$page_4_corr_o8 = renderPlotly({
    
    cor_olist = list()
    for (i in 1:length(olist)) {
      cor_olist[[i]] = olist[[i]][,c("score")]
    }
    cor_olist = do.call("cbind", cor_olist)
    colnames(cor_olist) = oname
    
    fig = plot_ly(x = oname, 
                  y = oname,
                  z = cor(cor_olist, use = "complete.obs"),
                  type = "heatmap", 
                  colors = "YlOrRd")
    return(fig)
  })
  
  output$page_4_radar_subitem = renderPlotly({
    subitem_df = olist[[which(oname == input$page_4_picker_subitem)]]
    subitem_df = subitem_df[,colnames(subitem_df)[!colnames(subitem_df) %in% c("score","name")]]
    subitem_df = subitem_df %>% dplyr::summarise_all(mean, na.rm=TRUE)
    
    p = plot_ly(type = 'scatterpolar',
                r = as.numeric(subitem_df[1,]),
                theta = colnames(subitem_df),
                fill = "toself",
                name = input$page_4_picker_subitem
    ) %>% layout(polar=list(radialaxis = list(visible = T, range = c(0, round(max(subitem_df[1,]), -1)))),
                 title=title,
                 margin=list(t=50))
    return(p)
  })
  
})

