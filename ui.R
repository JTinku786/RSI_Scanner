library("shinybusy")
library("shinycssloaders")
library("shinycustomloader")
library(shinyWidgets)
library(shinyjs)
library(shinythemes)
library(shinyjs)
library(rintrojs)
library(shinyalert)
library(shinydashboard)
library(highcharter)
library(quantmod)
library("RJSONIO")
library(magrittr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(slickR)
library(bizdays)

library("RQuantLib")


# js <- '.nav-tabs-custom .nav-tabs li.active {
#     border-top-color: #d73925;
# }"'


shinyUI(navbarPage(title = "Algorithmic RSI Indicator",
                   theme = "style/style.css",
                   footer = includeHTML("footer.html"),
                   fluid = TRUE, 
                   # collapsible = TRUE,
                   # ----------------------------------
                   # tab panel 1 - Home
                   tabPanel("Home",
                            
                            includeHTML("home.html"),
                            tags$script(src = "plugins/scripts.js"),
                            tags$head(
                              tags$link(rel = "stylesheet", 
                                        type = "text/css", 
                                        href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
                              tags$link(rel = "icon", 
                                        type = "image/png", 
                                        href = "images/logo_icon.png")
                            )
                   ),
                   tabPanel("Zone Identifier",
                            box(width=12,status="primary",solidHeader=TRUE,
                                fluidRow(
                                  column(2,
                                         numericInput("upper_limit", label=h6("Upper Limit :"), value=60, min =0,max = 100)
                                  ),
                                  column(2,
                                         numericInput("lower_limit", label=h6("Lower Limit:"), value=40, min =0,max = 100)
                                  ),
                                  column(2,
                                         dateInput("rsi_date_input","Select Date",value = Sys.Date(), min = NULL,max = NULL,format = "yyyy-mm-dd",startview = "month",weekstart = 0)
                                  ),
                                  column(2,
                                         selectizeInput("bot_timeframe_1", label = "Time Frame 1 :", choices=c("1 min"="1m","2 min"="2m","5 min"="5m","15 min"="15m","1 hour"="1h","1 Day"="1d","1 Week"="1wk","1 Month"="1mo"), selected = "1h", multiple = FALSE,
                                                        options = list(maxOptions = 10))
                                  ),
                                  column(2,
                                         selectizeInput("bot_timeframe_2", label = "Time Frame 2 :", choices=c("1 min"="1m","2 min"="2m","5 min"="5m","15 min"="15m","1 hour"="1h","1 Day"="1d","1 Week"="1wk","1 Month"="1mo"), selected = "1d", multiple = FALSE,
                                                        options = list(maxOptions = 10))
                                         
                                  ),
                                  column(2,
                                         selectizeInput("bot_timeframe_3", label = "Time Frame 3 :", choices=c("1 min"="1m","2 min"="2m","5 min"="5m","15 min"="15m","1 hour"="1h","1 Day"="1d","1 Week"="1wk","1 Month"="1mo"), selected = "1wk", multiple = FALSE,
                                                        options = list(maxOptions = 10))
                                         
                                  ),
                                  # column(4,
                                  #        
                                  #        selectizeInput("bot_timeframe", label = "Range :", choices=c("1 min"="1m","2 min"="2m","5 min"="5m","15 min"="15m","1 hour"="1h","1 Day"="1d","1 Week"="1wk","1 Month"="1mo"), selected = "1d", multiple = FALSE,
                                  #                       options = list(maxOptions = 10)),
                                  #        radioButtons("nifty_selection", "Select the Bucket:",
                                  #                     c("Nifty 50" = "nifty_50",
                                  #                       "Nifty 500" = "nifty_500"),
                                  #                     selected = "nifty_50")
                                  # ),
                                  column(6,
                                         tags$p("*If you select the interval as 1 min,2 min then we analyse for last 3 candles."),
                                         tags$p("*If you select the interval as 5 min then we analyse for last 1 days."),
                                         tags$p("*If you select the interval as 15 min, 1 hour then we analyse for last 5 days."),
                                         tags$p("*If you select the interval as 1 Day then we analyse for last 2 months."),
                                         tags$p("*If you select the interval as 1 Week, 1 Month then we analyse for last 36 months.")
                                         # ,
                                         # tags$a(href="https://docs.google.com/document/d/1NIcVwSS7HQt3my2zlnm5fW69U2u3qzMaq1GAh8iFlxg/edit", "Document", target="_blank")
                                  ),
                                  column(1,
                                         # submitButton("Run Trade",width = "100px")
                                         actionButton(inputId = "bot_submit",label = "Run Trade",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                  )
                                )
                                # fluidRow(
                                #   box(width=12,title= "Daily Zones :",status="primary",solidHeader=TRUE , collapsible = TRUE, collapsed = TRUE ,
                                #       div(style = 'overflow-y:scroll;',
                                #           withSpinner( DT::dataTableOutput("demand_daily_screener")))
                                #   )
                                #   # ,
                                #   # box(width=12,title= "Stocks In Zone :",status="primary",solidHeader=TRUE , collapsible = T, collapsed = T ,
                                #   #     div(style = 'overflow-y:scroll;',
                                #   #         withSpinner( DT::dataTableOutput("Stocks_Screener")))
                                #   # ),
                                #   # box(width=12,title= "Historical Times :",status="primary",solidHeader=TRUE , collapsible = T, collapsed = T ,
                                #   #     div(style = 'overflow-y:scroll;',
                                #   #         withSpinner( DT::dataTableOutput("Execution_Screener")))
                                #   # )
                                # )
                                
                            ),
                            fluidPage(
                              tabsetPanel(
                                tabPanel("Completed RSI Scanner", 
                                         fluidRow( 
                                           box(width=12,title= "RSI Accomplished :",status="primary",solidHeader=TRUE,
                                               div(style = 'overflow-y:scroll;',
                                                   withSpinner( DT::dataTableOutput("rsi_buyer")))
                                           ),
                                           box(width=12,title= "Breakout Time :",status="primary",solidHeader=TRUE , collapsible = TRUE, collapsed = TRUE,
                                               div(style = 'overflow-y:scroll;',
                                                   withSpinner( DT::dataTableOutput("rsi_breakout")))
                                           )
                                         )
                                )
                                ,
                                tabPanel("Potential RSI Scanner",
                                         fluidRow(
                                           box(width=12,title= "Likely RSI :",status="primary",solidHeader=TRUE,
                                               div(style = 'overflow-y:scroll;',
                                                   withSpinner( DT::dataTableOutput("potential_rsi")))
                                           )
                                         )
                                )
                                # ,
                                # tabPanel("Daily Voided/Obeyed",
                                #          fluidRow( 
                                #            box(width=12,title= "Voided :",status="primary",solidHeader=TRUE , collapsible = TRUE, collapsed = TRUE ,
                                #                div(style = 'overflow-y:scroll;',
                                #                    withSpinner( DT::dataTableOutput("daily_voided")))
                                #            )
                                #          )  
                                #          
                                #          
                                # )
                                
                              )
                            )
                            
                   ),
                   tabPanel("About us",
                            fluidRow(
                              column(width = 6,
                                     tags$div(class = "box box-solid",
                                              tags$div(class = "box-header with-border collapse",
                                                       tags$i(class = "fas fa-book-open"),
                                                       tags$h3(class = "box-title custom-box-header","The Short")
                                              ),
                                              tags$div(class = "box-body",
                                                       tags$p("Stocks Analysis is an innovative trading idea to make trading technology accessible to everyone. This is a one stop application to having a full trading platform in a web browser."),
                                                       tags$p("Short"),
                                                       tags$p("Sai Teja Reddy is currently working as a Data Scientist at one of the most reputed technology organization. Prior to that, worked as Sales Operations Analyst and Business Analyst at Uber with almost 2 years of experience. He began his carrer as a Developer and has good hands on experience with microsoft technologies. He is an Electrical graduate from National Insititute of Technology Durgapur."),
                                                       tags$p("Long"),
                                                       tags$p("I am a technology lover and good writer. The purpose was building programmed software which allows to automate the trading of Indian stock trades by acting upon high volumes, creating strategies and doing backtest to generate more profits."),
                                                       br()
                                                       
                                              )
                                     )
                              )
                            ),
                            includeHTML("about.html"),
                            shinyjs::useShinyjs(),
                            tags$head(
                              tags$link(rel = "stylesheet", 
                                        type = "text/css", 
                                        href = "plugins/carousel.css"),
                              tags$script(src = "plugins/holder.js")
                            ),
                            tags$style(type="text/css",
                                       ".shiny-output-error { visibility: hidden; }",
                                       ".shiny-output-error:before { visibility: hidden; }"
                            )
                            
                   )
                   
))


#                    
#                    
#                    
# 
# ui <- tagList(
#   
#   navbarPage("Demand And Supply Zone", selected = "Home", collapsible = TRUE, inverse = TRUE, theme = shinytheme("cosmo"),
#              useShinyjs(),
#              introjsUI(),
#              useShinyalert(),
#              header = tagList(
#                useShinydashboard()
#              ),
#              footer = includeHTML("footer.html"),
#   tags$style(js),  
#   tabBox(id = "tabs",
#          side = "right", width = 12,
#          height = "45px",
#          selected = "Home",
#          tabPanel("About us",
#                   fluidRow(
#                     column(width = 6,
#                            tags$div(class = "box box-solid",
#                                     tags$div(class = "box-header with-border collapse",
#                                              tags$i(class = "fas fa-book-open"),
#                                              tags$h3(class = "box-title custom-box-header","The Short")
#                                     ),
#                                     tags$div(class = "box-body",
#                                              tags$p("Stocks Analysis is an innovative trading idea to make trading technology accessible to everyone. This is a one stop application to having a full trading platform in a web browser."),
#                                              tags$p("Short"),
#                                              tags$p("Sai Teja Reddy is currently working as a Data Scientist at one of the most reputed technology organization. Prior to that, worked as Sales Operations Analyst and Business Analyst at Uber with almost 2 years of experience. He began his carrer as a Developer and has good hands on experience with microsoft technologies. He is an Electrical graduate from National Insititute of Technology Durgapur."),
#                                              tags$p("Long"),
#                                              tags$p("I am a technology lover and good writer. The purpose was building programmed software which allows to automate the trading of Indian stock trades by acting upon high volumes, creating strategies and doing backtest to generate more profits."),
#                                              br()
#                                              
#                                     )
#                            )
#                     )
#                   ),
#                   includeHTML("about.html"),
#                   shinyjs::useShinyjs(),
#                   tags$head(
#                     tags$link(rel = "stylesheet", 
#                               type = "text/css", 
#                               href = "plugins/carousel.css"),
#                     tags$script(src = "plugins/holder.js")
#                   ),
#                   tags$style(type="text/css",
#                              ".shiny-output-error { visibility: hidden; }",
#                              ".shiny-output-error:before { visibility: hidden; }"
#                   )
#                   
#          ),
#          # tabPanel("Home",
#          #          # fluidRow(column(width = 6,
#          #          #                 tags$div(class = "box box-solid",
#          #          #                          tags$div(class = "box-header with-border collapse",
#          #          #                                   tags$i(class = "fas fa-book-open"),
#          #          #                                   tags$h3(class = "box-title custom-box-header","Guide & Usage")
#          #          #                          ),
#          #          #                          tags$div(class = "box-body",
#          #          #                                   
#          #          #                                   tags$h4('Quick Manual'),
#          #          #                                   tags$li('Demand Zone : The demand zone is where all the big buyers are located.'),
#          #          #                                   tags$li(' Supply Zone :  The supply zone is where all the big sellers are located.'),
#          #          #                                   br(),
#          #          #                                   tags$p(HTML(paste0('Here is the ',a(href = 'https://docs.google.com/document/d/1NIcVwSS7HQt3my2zlnm5fW69U2u3qzMaq1GAh8iFlxg/edit', 'Document'),' about Supply and Demand Zone !!')))
#          #          #                                   
#          #          #                          )
#          #          #                 )
#          #          #                 
#          #          # )
#          #          # ) 
#          #          
#          #          slickROutput("slickr",width = "100%", height = "10px"),
#          #          hr(),
#          #          hr(),
#          #          box(width=12,status="primary",solidHeader=TRUE,height = 100,
#          #              column(12, align="center",
#          #                     uiOutput(outputId = "Disclaimer")
#          #              ),
#          #              column(12, align="center",
#          #                     uiOutput(outputId = 'follow')
#          #              ),
#          #              tags$head(tags$style("#info{color: white;
#          #                         font-size: 15px;
#          #                 font-style: italic;
#          #                 }"
#          #              )
#          #              ),
#          #              fluidRow(
#          #                column(6, align = "right",
#          #                       tags$a(href="https://www.linkedin.com/in/sai-teja-reddy-8b8016139/", "LinkedIn", target="_blank")
#          #                ),
#          #                column(6, align = "left",
#          #                       tags$a(href="https://www.instagram.com/sai_teja_reddy/?hl=en", "Instagram", target="_blank")
#          #                )
#          #              )
#          #          )
#          # ),
#          tabPanel("Zone Identifier",
#                   
#                   box(width=12,status="primary",solidHeader=TRUE,
#                       fluidRow(
#                         column(4,
#                                selectizeInput("bot_timeframe", label = "Range :", choices=c("1 min"="1m","2 min"="2m","5 min"="5m","15 min"="15m","1 hour"="1h","1 Day"="1d","1 Week"="1wk","1 Month"="1mo"), selected = "1d", multiple = FALSE,
#                                               options = list(maxOptions = 10))
#                         ),
#                         column(6,
#                                tags$p("*If you select the interval as 1 min,2 min then we analyse for last 2 days."),
#                                tags$p("*If you select the interval as 5 min then we analyse for last 5 days."),
#                                tags$p("*If you select the interval as 15 min, 1 hour then we analyse for last 60 days."),
#                                tags$p("*If you select the interval as 1 Day then we analyse for last 2 months."),
#                                tags$p("*If you select the interval as 1 Week, 1 Month then we analyse for last 36 months.")
#                                # ,
#                                # tags$a(href="https://docs.google.com/document/d/1NIcVwSS7HQt3my2zlnm5fW69U2u3qzMaq1GAh8iFlxg/edit", "Document", target="_blank")
#                         ),
#                         column(1,
#                                # submitButton("Run Trade",width = "100px")
#                                actionButton(inputId = "bot_submit",label = "Run Trade",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
#                         )
#                       ),
#                       fluidRow(
#                         box(width=12,title= "Daily Zones :",status="primary",solidHeader=TRUE , collapsible = T, collapsed = T ,
#                             div(style = 'overflow-y:scroll;',
#                                 withSpinner( DT::dataTableOutput("demand_daily_screener")))
#                         ),
#                         box(width=12,title= "Stocks In Zone :",status="primary",solidHeader=TRUE , collapsible = T, collapsed = T ,
#                             div(style = 'overflow-y:scroll;',
#                                 withSpinner( DT::dataTableOutput("Stocks_Screener")))
#                         ),
#                         box(width=12,title= "Historical Times :",status="primary",solidHeader=TRUE , collapsible = T, collapsed = T ,
#                             div(style = 'overflow-y:scroll;',
#                                 withSpinner( DT::dataTableOutput("Execution_Screener")))
#                         )
#                       )
#                       
#                   ) 
#                   
#          ),
#          tabPanel("Home",icon = icon("home"),
#                   # slickROutput("slickr",width = "100%", height = "100%"),
#                   # hr(),
#                   # hr(),
#                   # box(width=12,status="primary",solidHeader=TRUE,height = 100,
#                   #     column(12, align="center",
#                   #            uiOutput(outputId = "Disclaimer")
#                   #     ),
#                   #     column(12, align="center",
#                   #            uiOutput(outputId = 'follow')
#                   #     ),
#                   #     tags$head(tags$style("#info{color: white;
#                   #                font-size: 15px;
#                   #        font-style: italic;
#                   #        }"
#                   #     )
#                   #     ),
#                   #     fluidRow(
#                   #       column(6, align = "right",
#                   #              tags$a(href="https://www.linkedin.com/in/sai-teja-reddy-8b8016139/", "LinkedIn", target="_blank")
#                   #       ),
#                   #       column(6, align = "left",
#                   #              tags$a(href="https://www.instagram.com/sai_teja_reddy/?hl=en", "Instagram", target="_blank")
#                   #       )
#                   #     )
#                   # )
#                   
#                   includeHTML("home.html"),
#                   tags$script(src = "plugins/scripts.js"),
#                   tags$head(
#                     tags$link(rel = "stylesheet", 
#                               type = "text/css", 
#                               href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
#                     tags$link(rel = "icon", 
#                               type = "image/png", 
#                               href = "images/logo_icon.png")
#                   )
#          )
#   )
#   
#   )
# )





# ui <- dashboardPage(
#   skin = "green",
#   dashboardHeader(
# 
#     title = tags$a( tags$head(tags$style(HTML(".name { color: white }"))),
#                     'Supply and Demand Zone Dashboard',class="name")
# 
#   ),
# 
#   dashboardSidebar(
#     sidebarMenu(
#       menuItem("Introduction", tabName = "introduction", icon = icon("home")),
#       menuItem("Supply and Demand Zone Identifier", tabName = "zone_identifier", icon = icon("info"))
#     )
#   ),
#   dashboardBody(
# 
#     tabItems(
#       tabItem(
#         tabName = "introduction",
# 
#         tags$h2("Information"),
# 
#         fluidRow(column(width = 6,
#                         tags$div(class = "box box-solid",
#                                  tags$div(class = "box-header with-border collapse",
#                                           tags$i(class = "fas fa-book-open"),
#                                           tags$h3(class = "box-title custom-box-header","Guide & Usage")
#                                  ),
#                                  tags$div(class = "box-body",
# 
#                                           tags$h4('Quick Manual'),
#                                           tags$li('Demand Zone : The demand zone is where all the big buyers are located.'),
#                                           tags$li(' Supply Zone :  The supply zone is where all the big sellers are located.'),
#                                           br(),
#                                           tags$p(HTML(paste0('Here is the ',a(href = 'https://docs.google.com/document/d/1NIcVwSS7HQt3my2zlnm5fW69U2u3qzMaq1GAh8iFlxg/edit', 'Document'),' about Supply and Demand Zone !!')))
# 
#                                  )
#                         )
# 
#         )
#         # ,
#         # column(width = 6,
#         #        tags$div(class = "box box-solid custom-body",
#         #                 tags$div(class = "box-header with-border custom-box-header",
#         #                          tags$h2(" One line definition : ")
#         #                 ),
#         #                 tags$p('“A computer program is said to learn from experience ‘E’, with respect to some class of tasks ‘T’ and performance measure ‘P’ if its performance at tasks in ‘T’ as measured by ‘P’ improves with experience ‘E’."')
#         #        )
#         # )
#         )
# 
#       ),
#       tabItem(
#         tabName = "zone_identifier",
#         box(width=12,status="primary",solidHeader=TRUE,
#             fluidRow(
#               column(4,
#                 selectizeInput("bot_timeframe", label = "Range :", choices=c("1 min"="1m","2 min"="2m","5 min"="5m","15 min"="15m","1 hour"="1h","1 Day"="1d","1 Week"="1wk","1 Month"="1mo"), selected = "1d", multiple = FALSE,
#                                options = list(maxOptions = 10))
#               ),
#               column(6,
#                      tags$p("*If you select the interval as 1 min,2 min then we analyse for last 2 days."),
#                      tags$p("*If you select the interval as 5 min then we analyse for last 5 days."),
#                      tags$p("*If you select the interval as 15 min, 1 hour then we analyse for last 60 days."),
#                      tags$p("*If you select the interval as 1 Day then we analyse for last 2 months."),
#                      tags$p("*If you select the interval as 1 Week, 1 Month then we analyse for last 36 months.")
#                      # ,
#                      # tags$a(href="https://docs.google.com/document/d/1NIcVwSS7HQt3my2zlnm5fW69U2u3qzMaq1GAh8iFlxg/edit", "Document", target="_blank")
#                      ),
#               column(1,
#                      # submitButton("Run Trade",width = "100px")
#                      actionButton(inputId = "bot_submit",label = "Run Trade",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
#               )
#             ),
#             fluidRow(
#               box(width=12,title= "Daily Zones :",status="primary",solidHeader=TRUE , collapsible = T, collapsed = T ,
#                   div(style = 'overflow-y:scroll;',
#                       withSpinner( DT::dataTableOutput("demand_daily_screener")))
#               ),
#               box(width=12,title= "Stocks In Zone :",status="primary",solidHeader=TRUE , collapsible = T, collapsed = T ,
#                   div(style = 'overflow-y:scroll;',
#                       withSpinner( DT::dataTableOutput("Stocks_Screener")))
#               ),
#               box(width=12,title= "Historical Times :",status="primary",solidHeader=TRUE , collapsible = T, collapsed = T ,
#                   div(style = 'overflow-y:scroll;',
#                       withSpinner( DT::dataTableOutput("Execution_Screener")))
#               )
#             )
# 
#         )
#       )
#     )
# 
# 
#   )
# )
# 
# 
# # # Define UI for application that draws a histogram
# # ui <- fluidPage(
# #
# #   # Application title
# #   titlePanel("Demand and Supply Zone Identifier"),
# #
# #   # Sidebar with a slider input for number of bins
# #   sidebarLayout(
# #     sidebarPanel(
# #       # radioButtons("nifty_selection", "Select the Bucket:",
# #       #              c("Nifty 50" = "nifty_50",
# #       #                "Nifty 500" = "nifty_500"),
# #       #              selected = "nifty_500"),
# #       selectizeInput("bot_timeframe", label = "Range :", choices=c("1 min"="1m","2 min"="2m","5 min"="5m","15 min"="15m","1 hour"="1h","1 Day"="1d","1 Week"="1wk","1 Month"="1mo"), selected = "1d", multiple = FALSE,
# #                      options = list(maxOptions = 10)),
# #       # dateRangeInput("date_range", "Date range:", start = "2021-01-01", end = Sys.Date(),format = "yyyy-mm-dd"),
# #       # tags$p("*Note: If you select the date as 2021-01-01 then we start the analysis one day after the selected working date. If you select 2021-03-31 as end date then we stop the analysis one day before the selected working date."),
# #       tags$p("*If you select the interval as 1 min,2 min then we analyse for last 2 days."),
# #       tags$p("*If you select the interval as 5 min then we analyse for last 5 days."),
# #       tags$p("*If you select the interval as 15 min, 1 hour then we analyse for last 60 days."),
# #       tags$p("*If you select the interval as 1 Day then we analyse for last 2 months."),
# #       tags$p("*If you select the interval as 1 Week, 1 Month then we analyse for last 36 months."),
# #       tags$a(href="https://docs.google.com/document/d/1NIcVwSS7HQt3my2zlnm5fW69U2u3qzMaq1GAh8iFlxg/edit", "Document", target="_blank")
# #
# #     ),
# #
# #     # Show a plot of the generated distribution
# #     mainPanel(
# #       box(width=12,title= "Daily Zones :",status="primary",solidHeader=TRUE , collapsible = T, collapsed = F ,
# #       div(style = 'overflow-y:scroll;',
# #           withSpinner( DT::dataTableOutput("demand_daily_screener"))),
# #       ),
# #       box(width=12,title= "Stocks In Zone :",status="primary",solidHeader=TRUE , collapsible = T, collapsed = T ,
# #           div(style = 'overflow-y:scroll;',
# #               withSpinner( DT::dataTableOutput("Stocks_Screener"))),
# #       )
# #       # ,
# #       # box(width=12,title= "Weekly Zones :",status="primary",solidHeader=TRUE , collapsible = T, collapsed = F ,
# #       #     div(style = 'overflow-y:scroll;',
# #       #         withSpinner( DT::dataTableOutput("demand_weekly_screener")))
# #       # )
# #     )
# #   )
# # )