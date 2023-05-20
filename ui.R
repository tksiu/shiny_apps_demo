require(shiny)
require(shinyWidgets)
require(shinydashboard)
require(shinythemes)
require(shinyBS)
require(bs4Dash)
require(bsplus)
require(ggplot2)
require(plotly)
require(showtext)
require(DT)
require(dygraphs)

showtext_auto()

ui <- bs4Dash::dashboardPage(
  controlbar = dashboardControlbar(
    skinSelector(), 
    width = 250,
    pinned = FALSE),
  
  bs4Dash::dashboardHeader(
    title = tags$img(src='appicon-style-an-icon-about-engaging-a-group-of-people-to-participate-in-health-promotion-events.png', 
                     style = 'height: 220px; width: 220px; position: relative; left: 5%; top: 5%'
                    ), 
    skin = "dark",
    status = "warning",
    titleWidth = "100%",
    rightUi = dropdownMenu(type = "notifications",
                            icon = icon("calendar"),
                            badgeStatus = "success",
                            notificationItem(
                                text = paste("Last Update Time: ", last_update_time, sep = ""),
                                icon("sync-alt"),
                                status = "success"
                            ))
  ),
  
  dashboardSidebar(
    collapsed = FALSE,
    width = 250,
    status = "warning",
    sidebarMenu(
      fluidRow(),
      fluidRow(),
      fluidRow(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      menuItem("Programs", tabName = "program", icon = icon("people-roof", lib = "font-awesome")),
      menuItem("Screenings", tabName = "screening", icon = icon("magnifying-glass-chart", lib = "font-awesome")),
      menuItem("Volunteers", tabName = "volunteer", icon = icon("people-carry-box", lib = "font-awesome")),
      menuItem("Engagement", tabName = "engagement", icon = icon("universal-access", lib = "font-awesome"))
    )
  ),
  
  dashboardBody(
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    tags$style("@import url(https://use.fontawesome.com/releases/v6.4.0/css/all.css);"),
    tags$style(
      HTML(
        ".checkbox-inline { 
            margin-left: 10px;
            margin-right: 10px;
         }
         .checkbox-inline+.checkbox-inline {
            margin-left: 10px;
            margin-right: 10px;
         }
         .dropdown-item.active {
            background-color: #ffc107;
         }
         .material-switch > label { 
         background-color: #17a2b8; 
         }
         .glyphicon-ok {color:#2dc79f}
         .glyphicon-exclamation-sign {color:#e5992b}
         "
      )
    ),
    tags$head(
      tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                  type="text/javascript")
    ),
    tags$head(tags$style(HTML('
                              .main-header .logo {
                              font-family: "Microsoft JhengHei";
                              font-weight: bold;
                              font-size: 20px;
                              padding: 0 0px;
                              }
                              .card-title {
                              font-size: 20px;
                              }
                              .box.box-solid.box-primary {
                              font-family: "Microsoft JhengHei";
                              font-weight: bold;
                              }
                              .box-title {
                              font-family: "Microsoft JhengHei";
                              font-weight: bold;
                              }
                              .bs-actionsbox .btn-group button {
                              background: #ffc107;
                              }
                              .main-sidebar {
                              font-family: "Microsoft JhengHei";
                              font-weight: bold;
                              }
                              .control-sidebar .row {
                              margin-left: 0px; margin-right: 0px;
                              }
                              .nav-tabs-custom .nav-tabs li.active {
                              border-top-color: #8b0000;
                              }
                              .nav-sidebar>.nav-item .nav-icon.fa, 
                              .nav-sidebar>.nav-item .nav-icon.fab, 
                              .nav-sidebar>.nav-item .nav-icon.fad, 
                              .nav-sidebar>.nav-item .nav-icon.fal, 
                              .nav-sidebar>.nav-item .nav-icon.far, 
                              .nav-sidebar>.nav-item .nav-icon.fas, 
                              .nav-sidebar>.nav-item .nav-icon.ion, 
                              .nav-sidebar>.nav-item .nav-icon.svg-inline--fa {
                              font-size: 1.5rem;
                              }
                              .irs-single, .irs-bar-edge, .irs-bar, .irs-from, .irs-to {
                              background: #8b0000;
                              border-top: #8b0000;
                              border-bottom: #8b0000;
                              }
                              .content-wrapper { 
                              overflow: auto; 
                              }
                              table.dataTable {
                              width: "100%"; 
                              }
                              table.dataTable thead {
                              background-color: #ffc107;
                              }'))),
    
    tabItems(
      
      tabItem(tabName = "program",
          fluidPage(
              fluidRow(
                  div(
                    h2("Health Promotion and Education Program Progress", style="font-size: 30px; font-weight: bold; ")
                  )
              ),
              br(),
              fluidRow(
                  box(
                      title = "Filter Panel:  ", status = "gray-dark", solidHeader = TRUE, collapsible = TRUE,
                      width = 12,
                      fluidRow(
                        column(
                          width = 3,
                          pickerInput('slicer_page_1_year', label = h4("Year (Session Date)"),
                                      choices = sort(unique(df_prog$Year)), 
                                      multiple = TRUE,
                                      selected = sort(unique(df_prog$Year))[1:length(unique(df_prog$Year))],
                                      options = list(`actions-box` = TRUE, 
                                                     `none-selected-text` = "please select: ", 
                                                     `selected-text-format` = paste0("count > ", length(unique(df_prog$Year))-1,
                                                     `count-selected-text` = "All selected", size = 10)))
                        ),
                        column(
                          width = 3,
                          pickerInput('slicer_page_1_quarter', label = h4("Quarter (Session Date)"),
                                      choices = sort(unique(df_prog$Quarter)), 
                                      multiple = TRUE,
                                      selected = sort(unique(df_prog$Quarter)),
                                      options = list(`actions-box` = TRUE, 
                                                     `none-selected-text` = "please select: ", 
                                                     `selected-text-format` = paste0("count > ", length(unique(df_prog$Quarter))-1,
                                                     `count-selected-text` = "All selected", size = 10)))
                        ),
                        column(
                          width = 3,
                          pickerInput('slicer_page_1_month', label = h4("Month (Session Date)"),
                                      choices = unique(df_prog$Month[order(match(df_prog$Month, month_levels))]),
                                      multiple = TRUE,
                                      selected = unique(df_prog$Month[order(match(df_prog$Month, month_levels))]),
                                      options = list(`actions-box` = TRUE, 
                                                     `none-selected-text` = "please select: ", 
                                                     `selected-text-format` = paste0("count > ", length(unique(df_prog$Month))-1,
                                                     `count-selected-text` = "All selected", size = 10)))
                        )
                      ),
                      fluidRow(
                        column(
                          width = 3,
                          pickerInput('slicer_page_1_output_main', label = h4("Output (Main Category)"),
                                      choices = choice_output_main, 
                                      multiple = TRUE,
                                      selected = choice_output_main,
                                      options = list(`actions-box` = TRUE, 
                                                     `none-selected-text` = "please select: ", 
                                                     `selected-text-format` = paste0("count > ", length(choice_output_main)-1,
                                                     `count-selected-text` = "All selected", size = 10)))
                        ),
                        column(
                          width = 3,
                          pickerInput('slicer_page_1_output', label = h4("Output"),
                                      choices = choice_output, 
                                      multiple = TRUE,
                                      selected = choice_output,
                                      options = list(`actions-box` = TRUE, 
                                                     `none-selected-text` = "please select: ", 
                                                     `selected-text-format` = paste0("count > ", length(choice_output)-1,
                                                     `count-selected-text` = "All selected", size = 10)))
                        ),
                        column(
                          width = 3,
                          pickerInput('slicer_page_1_pcode', label = h4("Program Code (Sub Category)"),
                                      choices = choice_pcode, 
                                      multiple = TRUE,
                                      selected = choice_pcode,
                                      options = list(`actions-box` = TRUE, 
                                                     `none-selected-text` = "please select: ", 
                                                     `selected-text-format` = paste0("count > ", length(choice_pcode)-1,
                                                     `count-selected-text` = "All selected", size = 10)))
                        ),
                        column(width=1),
                        column(
                          width = 2,
                          style = "margin-top: 45px;", 
                          actionButton("trigger_filter_from_circular_node", "SUBMIT", 
                                      style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                        )
                      )
                  )
              ),
              br(),
              br(),
              fluidRow(
                column(width = 6, 
                    fluidRow(
                      column(width=6,
                            bs4InfoBoxOutput("page_1_box_1", width=12)
                      ),
                      column(width=6,
                            bs4InfoBoxOutput("page_1_box_2", width=12)
                      )
                    ),
                    fluidRow(
                      column(width=6,
                            bs4InfoBoxOutput("page_1_box_3", width=12)
                      ),
                      column(width=6,
                            bs4InfoBoxOutput("page_1_box_4", width=12)
                      )
                    ),
                    fluidRow(
                      column(width=6,
                            bs4InfoBoxOutput("page_1_box_5", width=12)
                      ),
                      column(width=6,
                            bs4InfoBoxOutput("page_1_box_6", width=12),
                            popover(tag = actionButton("goButton", "    Click me to see an explanation!", icon("circle-question"), style="border-color: #6610f2;"), 
                                    title = "Attendance Rate Remarks: ",
                                    content = "* Attendance Rate may exceed 100%, because of the walk-in participants who have not registered for enrollment in prior.",
                                    placement = "bottom")
                      )
                    ),
                    br(),
                    fluidRow(
                      column(width = 6,
                             radioGroupButtons(inputId = "radio_btn_page_1_progress_year", 
                                               choices = c("Entire 3Y-Period", sort(unique(df_prog$Year))),
                                               individual = TRUE,
                                               checkIcon = list(
                                                 "yes" = icon("check")
                                               ),
                                               status = "secondary",
                                               selected = "Entire 3Y-Period")
                      ),
                      column(width = 6,
                             pickerInput('slicer_page_1_progress_output', 
                                         choices = choice_output, 
                                         multiple = FALSE,
                                         selected = choice_output[3], 
                                         options = list(style = "btn-warning"))
                      )
                    ),
                    br(),
                    fluidRow(
                      column(width=4,
                            plotOutput("gauge_deliver")
                      ),
                      column(width=4,
                             plotOutput("gauge_benefit")
                      ),
                      column(width=4,
                             plotOutput("gauge_attend")
                      ),
                      
                    ),
                    fluidRow(
                      column(width=4,
                             plotlyOutput("distribution_B2_deliver")
                      ),
                      column(width=4,
                            plotlyOutput("distribution_B2_benefit")
                      ),
                      column(width=4,
                             plotlyOutput("distribution_B2_attend")
                      )
                    ),
                ), 
                column(width = 6,
                    box(
                        status = "orange", solidHeader = FALSE, collapsible = FALSE,
                        width = 12,
                        fluidRow(
                            column(width = 6,
                                   radioGroupButtons(inputId = "radio_btn_page_1_plot_type", 
                                                     choices = c("By Year", "By Quarter", "By Month"),
                                                     individual = TRUE,
                                                     checkIcon = list(
                                                       "yes" = icon("check")
                                                     ),
                                                     status = "secondary",
                                                     selected = "By Year")
                            ),
                            column(width = 6,
                                   uiOutput("page_1_plot_type_by_year_selection")
                            )
                        ),
                        fluidRow(
                            column(width = 4,
                                   pickerInput('slicer_page_1_secondary_dimension', label = h4("Groupings"),
                                               choices = c("Nil","Output Main","Output","Program Code (Subcode) Category"), 
                                               multiple = FALSE,
                                               selected = "Nil", 
                                               options = list(style = "btn-warning"))
                            ),
                            column(width = 4,
                                   pickerInput('slicer_page_1_metric', label = h4("Metrics"),
                                               choices = c("Program Count","Program Session Count",
                                                           "Enrollment Count (summing all programs)",
                                                           "Enrollment Count (summing all sessions)",
                                                           "Attendance Count", 
                                                           "Program Hours", 
                                                           "Program Lead Time"), 
                                               multiple = FALSE,
                                               selected = "Program Count", 
                                               options = list(style = "btn-warning"))
                            )
                        ),
                        fluidRow(
                            column(width = 12,
                                   plotlyOutput("page_1_count_plot")
                            )
                        ),
                        br(),
                        fluidRow(
                            column(width = 4,
                                   valueBoxOutput("page_1_attend_down", width=12)
                            ),
                            column(width = 4,
                                   valueBoxOutput("page_1_attend_up", width=12)
                            )
                        ),
                        br(),
                        fluidRow(
                            column(width = 12,
                                   dataTableOutput("page_1_count_table")
                            )
                        )
                    )
                )
              ),
              fluidRow(
                box(status = "orange", solidHeader = FALSE, collapsible = FALSE,
                    width = 12,
                    fluidRow(
                      column(width = 12,
                             dataTableOutput("page_1_progress_table")
                      )
                    )
                )
              )
          )
      ),
      
      tabItem(tabName = "screening",
          fluidPage(
              fluidRow(
                  div(
                    h2("Health Risk Assessment Screening Surveys", style="font-size: 30px; font-weight: bold; ")
                  )
              ),
              br(),
              fluidRow(
                box(
                  title = "Completed Survey Statistics：", status = "info", solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  fluidRow(
                    column(width = 3,
                           pickerInput(
                             inputId = "slicer_survey", label = "Choose survey：", 
                             choices = c("Pain Screening", "Sleep Disorder Screening", "Cardiovascular Disease Screening"),
                             selected = "Pain Screening",
                             multiple = FALSE, 
                             options = list(style = "btn-warning"))
                    ),
                    column(width = 3,
                           uiOutput('slicer_page_2_collector')
                    )
                  ),
                  fluidRow(
                    column(width = 3,
                           radioGroupButtons(inputId = "page_2_review_perspective", 
                                             label = "",
                                             choices = c("View By Day","View By Week","View By Month"),
                                             individual = TRUE,
                                             checkIcon = list(
                                               "yes" = icon("check")
                                             ),
                                             status = "secondary",
                                             selected = "View By Day")
                    ),
                    column(width = 3,
                           uiOutput('page_2_date_range_selector')
                    )
                  ),
                  fluidRow(
                    column(width = 9,
                           dygraphOutput('time_series_plot_01')),
                    column(width = 3,
                           box(verbatimTextOutput("legendDiv1"), title = "", collapsible = FALSE, width=12))
                  ),
                  br(),
                  fluidRow(
                    column(width = 9,
                           dygraphOutput('time_series_plot_02')),
                    column(width = 3,
                           box(verbatimTextOutput("legendDiv2"), title = "", collapsible = FALSE, width=12))
                  ),
                  br(),
                  br(),
                  fluidRow(
                     column(width = 3,
                            plotlyOutput("page_2_pie_age")
                     ),
                     column(width = 3,
                            plotlyOutput("page_2_pie_gender")
                     ),
                     column(width = 3,
                            plotlyOutput("page_2_risk_percent_bar_age")
                     ),
                     column(width = 3,
                            plotlyOutput("page_2_risk_percent_bar_gender")
                     )
                  ),
                  br(),
                  fluidRow(
                    column(width = 4,
                           plotlyOutput("page_2_radar_survey_1")
                    ),
                    column(width = 4,
                           plotlyOutput("page_2_radar_survey_2")
                    ),
                    column(width = 4,
                           plotlyOutput("page_2_radar_survey_3")
                    )
                  ),
                  br(),
                  fluidRow(
                    column(width = 12,
                           plotlyOutput("page_2_map")
                    )
                  )
                )
              )
          )
      ),
      
      tabItem(tabName = "volunteer",
          fluidPage(
              fluidRow(
                  div(
                    h2("Volunteer Workforce Dropout", style="font-size: 30px; font-weight: bold; ")
                  )
              ),
              br(),
              fluidRow(
                box(width = 12, status = "info", solidHeader = FALSE, collapsible = FALSE,
                    fluidRow(
                      column(width=3,
                             radioGroupButtons(
                               inputId = "page_3_group_button_year",
                               label = "Choose Year shown for the monthly breakdown:",
                               choices = c("2020-2021", "2021-2022", "2022-2023"),
                               individual = TRUE,
                               selected = "2022-2023",
                               status = "danger",
                               checkIcon = list(
                                 yes = tags$i(class = "fa fa-circle", 
                                              style = "color: silk"),
                                 no = tags$i(class = "fa fa-circle-o", 
                                             style = "color: silk"))
                             )),
                      column(width=2, 
                             pickerInput('page_3_slicer_view_mode', 
                                         label = "Calculation Methods: ",
                                         choices = c("Group-specific Rate [Dynamic Average Number of Volunteers (Denominator)]" = "Group-specific",
                                                     "Share to Population's Total [Fixed Average Number of Volunteers (Denominator)]" = "Fixed Total"),
                                         multiple = FALSE, 
                                         selected = "Group-specific",
                                         options = list(style = "btn-secondary")
                             )
                      ),
                      column(width=1,
                             dropMenu(actionButton("goButton2", "", icon("info-circle"), style="background-color: #39cccc;"),
                                     tags$div(
                                     HTML(paste0(
                                       tags$span("Explanation: "), tags$br(), tags$br(),
                                       tags$span("`Group-specific Rate` means the average number (begin and end) would be subjected to the filtering criteria;"), tags$br(), tags$br(),
                                       tags$span("`Share to Population's Total` means it will always be fixed to the overall average number over the whole workforce."), tags$br(), tags$br(),
                                       tags$span("For example, a unit has 5 staffs left, with the unit's average volunteers = 20, and population average volunteers = 200, "), tags$br(), tags$br(),
                                       tags$span("if using the `Group-specific Rate` method, the turnover would be 25% (specific to the unit's condition), "), tags$br(), tags$br(),
                                       tags$span("while using the `Share to Population's Total` method, the turnover would be 2.5% (contribution accounting to the total dropout rate).")
                                     ))),
                                     placement = "right",
                                     maxWidth = "200px")
                      ),
                      column(width=2,
                             pickerInput('slicer_contract_term', label = "Contract Term: ",
                                         choices = term_opts,
                                         multiple = TRUE, 
                                         selected = term_opts,
                                         options = list(style = "btn-secondary", 
                                                        `actions-box` = TRUE, `none-selected-text` = "please select: ", 
                                                        `selected-text-format` = paste0(
                                                          "count > ", length(term_opts)-1,
                                                          `count-selected-text` = "All Selected", size = 10)))
                      ),
                      column(width=2,
                             pickerInput('slicer_contract_type', label = "Contract Duration Type: ",
                                         choices = type_opts,
                                         multiple = TRUE, 
                                         selected = type_opts,
                                         options = list(style = "btn-secondary", 
                                                        `actions-box` = TRUE, `none-selected-text` = "please select: ", 
                                                        `selected-text-format` = paste0(
                                                          "count > ", length(type_opts)-1,
                                                          `count-selected-text` = "All Selected", size = 10)))
                      )
                    ),
                    br(),
                    fluidRow(
                      column(width = 3,
                             bs4InfoBoxOutput("page_3_box_full_member_percent", width=12)
                      ),
                      column(width = 3,
                             bs4InfoBoxOutput("page_3_box_full_member_leavers_percent", width=12)
                      ),
                      column(width = 3,
                             bs4InfoBoxOutput("page_3_box_permanent_member_percent", width=12)
                      ),
                      column(width = 3,
                             bs4InfoBoxOutput("page_3_box_permanent_member_leavers_percent", width=12)
                      )
                    )
                )
              ),
              fluidRow(
                box(width = 12, status = "info", solidHeader = FALSE, collapsible = FALSE,
                    fluidRow(
                      column(width = 2,
                             pickerInput('page_3_slicer_school', label = "School :",
                                         choices = school_opts, 
                                         multiple = TRUE,
                                         selected = school_opts,
                                         options = list(`actions-box` = TRUE, `none-selected-text` = "please select: ", 
                                                        `selected-text-format` = paste0(
                                                          "count > ", length(school_opts)-1,
                                                          `count-selected-text` = "All Selected", size = 10))
                             )),
                      column(width = 2,
                             pickerInput('page_3_slicer_dept', label = "Department :",
                                         choices = dept_opts, 
                                         multiple = TRUE,
                                         selected = dept_opts,
                                         options = list(`actions-box` = TRUE, `none-selected-text` = "please select: ", 
                                                        `selected-text-format` = paste0(
                                                          "count > ", length(dept_opts)-1,
                                                          `count-selected-text` = "All Selected", size = 10))
                             )),
                      column(width = 2,
                             pickerInput('page_3_slicer_grade', label = "Grade :",
                                         choices = grade_opts, 
                                         multiple = TRUE,
                                         selected = grade_opts,
                                         options = list(`actions-box` = TRUE, `none-selected-text` = "please select: ", 
                                                        `selected-text-format` = paste0(
                                                          "count > ", length(grade_opts)-1,
                                                          `count-selected-text` = "All Selected", size = 10))
                             )),
                      column(width = 2,
                             pickerInput('page_3_slicer_yos', label = "Year of Services :",
                                         choices = yos_opts, 
                                         multiple = TRUE,
                                         selected = yos_opts,
                                         options = list(`actions-box` = TRUE, `none-selected-text` = "please select: ", 
                                                        `selected-text-format` = paste0(
                                                          "count > ", length(yos_opts)-1,
                                                          `count-selected-text` = "All Selected", size = 10))
                             ))
                    ),
                    br(),
                    fluidRow(
                      column(width = 4,
                             plotlyOutput("page_3_year_plot", width = "100%"),
                             br(),
                             actionButton("page_3_year_datatable_button", "View Data", 
                                          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                             bsModal(id = "page_3_modal_year", 
                                     title = "Data Table for Years", 
                                     trigger = "page_3_year_datatable_button", 
                                     size = "large",
                                     dataTableOutput("page_3_data_year"))
                      ),
                      column(width = 8,
                             plotlyOutput("page_3_month_plot", width = "100%"),
                             br(),
                             actionButton("page_3_month_datatable_button", "View Data", 
                                          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                             bsModal(id = "page_3_modal_month", 
                                     title = "Data Table for Months", 
                                     trigger = "page_3_month_datatable_button", 
                                     size = "large",
                                     dataTableOutput("page_3_data_month"))
                      )
                    )
                )
              ),
              fluidRow(
                box(width = 12, status = "info", solidHeader = FALSE, collapsible = FALSE,
                  fluidRow(
                    column(width = 4, 
                           shinyWidgets::materialSwitch("switch_2nd_dim", 
                                          label = tags$b("Drill down to Second Dimension"), 
                                          value = FALSE,
                                          status = "warning")
                    )
                  ),
                  fluidRow(
                    column(width = 4,
                           pickerInput('picker_1st_dim', 
                                       label = tags$b("Selected First Dimension"), 
                                       choices = c("School","Department","Grade","Year of Services"), 
                                       multiple = FALSE,
                                       selected = c("School","Department","Grade","Year of Services")[1], 
                                       options = list(style = "btn-warning"))
                    ),
                    column(width = 4,
                           uiOutput("picker_2nd_dim")
                    ),
                    column(width = 4,
                           uiOutput("picker_1st_dim_single")
                    )
                  ),
                  fluidRow(
                    column(width=6,
                           radioGroupButtons(
                             inputId = "group_button_leave_reason",
                             label = "Choose Type of Dropouts:",
                             choices = c("Active Dropout", "Passive Dropout", "Total"),
                             individual = TRUE,
                             selected = "Total",
                             status = "danger",
                             checkIcon = list(
                               yes = tags$i(class = "fa fa-circle", 
                                            style = "color: silk"),
                               no = tags$i(class = "fa fa-circle-o", 
                                           style = "color: silk"))
                           ))
                  ),
                  br(),
                  fluidRow(
                    column(width = 4, 
                           plotlyOutput('page_3_group_plot_year', height = "400px"),
                           br(),
                           actionButton("page_3_group_year_datatable_button", "View Data", 
                                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                           bsModal(id = "page_3_modal_group_year", 
                                   title = "Annual Data Table breakdown by selected first (and second if any) dimension", 
                                   trigger = "page_3_group_year_datatable_button", 
                                   size = "large",
                                   dataTableOutput("page_3_data_group_year"))
                    ),
                    column(width = 8, 
                           plotlyOutput('page_3_group_plot_month', height = "400px"),
                           br(),
                           actionButton("page_3_group_month_datatable_button", "View Data", 
                                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                           bsModal(id = "page_3_modal_group_month", 
                                   title = "Monthly Data Table breakdown by selected first (and second if any) dimension", 
                                   trigger = "page_3_group_month_datatable_button", 
                                   size = "large",
                                   dataTableOutput("page_3_data_group_month"))
                    )
                  )
                )
              )
          )
      ),
      
      tabItem(tabName = "engagement",
          fluidPage(
              fluidRow(
                  div(
                    h2("Community Engagment Indicators", style="font-size: 30px; font-weight: bold; ")
                  )
              ),
              br(),
              fluidRow(
                column(width = 2, 
                       pickerInput('page_4_picker_x_var', 
                                   label = tags$b("Selected the engagement Outcome / Metric for X-axis"), 
                                   choices = oname, 
                                   multiple = FALSE,
                                   selected = oname[1], 
                                   options = list(style = "btn-warning"))
                ),
                column(width = 2, 
                       pickerInput('page_4_picker_y_var', 
                                   label = tags$b("Selected the engagement Outcome / Metric for Y-axis"), 
                                   choices = oname, 
                                   multiple = FALSE,
                                   selected = oname[2], 
                                   options = list(style = "btn-warning"))
                ),
                column(width = 2),
                column(width = 2,
                       pickerInput('page_4_picker_subitem', 
                                   label = tags$b("Selected the engagement Outcome / Metric for sub-item breakdown"), 
                                   choices = oname, 
                                   multiple = FALSE,
                                   selected = oname[1], 
                                   options = list(style = "btn-info"))
                )
              ),
              fluidRow(
                column(width = 12,
                       plotlyOutput("page_4_scatter")
                )
              ),
              br(),
              fluidRow(
                column(width = 6,
                       plotlyOutput("page_4_corr_o8")
                ),
                column(width = 6,
                       fluidRow(
                         plotlyOutput("page_4_radar_subitem")
                       )
                )
              )
          )
      )
    ),
    
    #responsive iframe
    HTML('<div data-iframe-height></div>')
  )
)

