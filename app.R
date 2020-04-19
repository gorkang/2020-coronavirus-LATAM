
# Libraries ---------------------------------------------------------------

library(dplyr)
library(DT)
library(ggplot2)
library(ggrepel)
library(httr)
# library(jsonlite)
library(readr)
library(rvest)
library(tidyr)
library(scales)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(vroom)
library(cowplot)
library(purrr)



# Data preparation --------------------------------------------------------

cases_deaths = "deaths" #cases deaths

source(here::here("R/download_or_load.R"))
source(here::here("R/download_or_load_JH_API.R"))

source(here::here("R/fetch_worldometers_safely.R"))
source(here::here("R/data-download.R"))
source(here::here("R/data-preparation.R"))


# Launch data_download 
minutes_to_check_downloads = 30 # Every 12 minutes
auto_invalide <- reactiveTimer(minutes_to_check_downloads * 60 * 1000) 



dta_raw = read_csv(here::here("outputs/raw_data.csv"),
                   col_types =
                       cols(
                           country = col_character(),
                           time = col_date(format = ""),
                           cases_sum = col_double(),
                           cases_diff = col_double(),
                           deaths_sum = col_double(),
                           deaths_diff = col_double(),
                           source = col_character()
                       )) %>% 
    mutate(country = 
               case_when(
                   country == "USA" ~ "US",
                   TRUE ~ country
               ))

DF_population_countries = read_csv("data/population_countries.csv",
                                   col_types =
                                       cols(
                                           country = col_character(),
                                           population = col_double()
                                       )) %>%
    mutate(country =
               case_when(
                   country == "Korea, South" ~ "Korea (South)",
                   TRUE ~ country
               )) %>% 
    mutate(continent_name = 
               case_when(
                   country %in% c("Mexico", "Dominican Republic", "Panama", "Honduras", "Cuba", "Costa Rica", "Guatemala", "El Salvador") ~ "Latin America",
                   country %in% c("Russia", "Kazakhstan", "Azerbaijan", "Armenia") ~ "Asia",
                   continent_name == "South America" ~ "Latin America",
                   TRUE ~ continent_name
               )) %>% 
    mutate(country = 
               case_when(
                   country == "USA" ~ "US",
                   TRUE ~ country
               ))



# Global parameters -------------------------------------------------------

INPUT_min_n = 1
INPUT_cases_deaths = "deaths"


smooth = TRUE
log_scale = TRUE

relative = FALSE
INPUT_relative = relative

growth = 30
INPUT_growth = growth

highlight = " "
INPUT_highlight = highlight
VAR_highlight = highlight




# # Type
# accumulated_daily_pct = "accumulated"
# accumulated_daily_pct = "daily"
# 
# INPUT_accumulated_daily_pct = accumulated_daily_pct



# UI ----------------------------------------------------------------------

ui <- 
    function(request) {
        fluidPage(
            tags$head(includeHTML(("google-analytics.html"))),
            useShinyjs(),
            
        titlePanel(
            windowTitle = "Coronavirus LATAM Tracker - Facultad de Psicología - UAI",
            fluidRow(
                column(9, HTML("<a href=\"https://gorkang.shinyapps.io/2020-coronavirus-LATAM/\">Coronavirus LATAM tracker</a>")), 
                column(1, HTML("<a href=\"http://psicologia.uai.cl/\"><img src=\"UAI_mini.png\", alt =\ 'Universidad Adolfo Ibáñez'></a>"))
            )
        ),
    theme = shinytheme("flatly"),
    
    sidebarLayout(
        sidebarPanel(
            width = 2,

            div(
             
            HTML(paste0(
                a(img(src = "github_small.png", title = "Github repository"), href="https://github.com/gorkang/2020-coronavirus-LATAM", target = "_blank"), "&nbsp;&nbsp;",
                a(img(src = "issue_small.png", title = "Report an issue!"), href="https://github.com/gorkang/2020-coronavirus-LATAM/issues", target = "_blank"), "&nbsp;&nbsp;",
                a(img(src = "twitter_small.png", title = "@gorkang"), href="https://twitter.com/gorkang", target = "_blank"), "&nbsp;&nbsp;", 
                a(img(src = "https://cdn.buymeacoffee.com/buttons/bmc-new-btn-logo.svg", title = "Buy me a coffee", height = "26px"), href="https://www.buymeacoffee.com/I0rkAbM", target = "_blank"), "&nbsp;", 
                "<BR><BR>")),
            align = "center"
            ),

    selectInput(inputId = 'continent_name_input', 
                label = 'Continents',
                choices = c("Asia", "Europe", "Africa", "North America", "Latin America", "Oceania"), multiple = TRUE, 
                selectize = TRUE, 
                width = "100%", 
                selected = c("Asia", "Europe", "North America", "Latin America")),
    
    selectInput(inputId = 'countries_input', 
                label = 'Countries',
                choices = c("List of ALL countries will go here"), multiple = TRUE, 
                selectize = TRUE, 
                width = "100%", 
                selected = c("List of ALL countries will go here")),
    
    # uiOutput('highlight2'),
    
    # selectInput(inputId = "cases_deaths", label = "Cases or deaths", selected = "deaths", 
    #              choices = c("cases", "deaths", "CFR")),

    # radioButtons(inputId = "accumulated_daily_pct", label = "Accumulated, daily or %", selected = "accumulated", 
    #              choices = c("accumulated", "daily", "%"), inline = TRUE),
    
    # Dynamically change with cases_deaths
    # sliderInput('min_n_cases', paste0("Day 0 after ___ accumulated cases"), min = 1, max = 1000, value = 100), 
    # sliderInput('min_n_deaths', paste0("Day 0 after ___ accumulated deaths"), min = 1, max = 500, value = 10),
    # sliderInput('min_n_CFR', paste0("Day 0 after ___ accumulated deaths"), min = 1, max = 500, value = 10),
    
    # Dynamically change with accumulated_daily_pct
    # sliderInput("growth_accumulated", "Daily growth (%):", min = 0, max = 100, value = 30),
    # sliderInput("growth_daily", "Daily growth (%):", min = 0, max = 100, value = 20),
    # sliderInput("growth_pct", "Daily growth (%):", min = -50, max = 0, value = -10),
    
    HTML("<BR>"),
    
    # div(style="display:inline-block;width;45%;text-align: center;",
    #     shinyWidgets::switchInput(inputId = "log_scale", label = "Log", value = TRUE, size = "mini", labelWidth = "45%")
    #     ), 
    # HTML("&nbsp;&nbsp;"),
    # div(style="display:inline-block;45%;text-align: center;",
    #     shinyWidgets::switchInput(inputId = "smooth", label = "Smooth", value = FALSE, size = "mini", labelWidth = "45%")
    #     ),
    
    # RELATIVE
    # div(style="display:inline-block;45%;text-align: center;",
    #     HTML("&nbsp;&nbsp;"),
    #     
    # shinyWidgets::switchInput(inputId = "relative", label = "Relative/million", value = FALSE, size = "mini", labelWidth = "80%")
    # ),
    # HTML("<BR><BR>"),
    
    div( HTML("&nbsp;&nbsp;"), style="display:inline-block;65%;text-align: center;",
        bookmarkButton(label = "URL")
    ), 
    HTML("&nbsp;&nbsp;"),
    div(style="display:inline-block;30%;text-align: center;",
        downloadButton('downloadPlot', 'Plot')
    ),
    
    HTML("<BR><BR>"),
    
    uiOutput("WARNING"),
    
    hr(),
    
    p(HTML(
        paste0(
            a("Johns Hopkins Data", href="https://covid19api.com/", target = "_blank"), " updated on: [DATE WILL GO HERE]"#, as.character(file_info_JHU), " GMT"
            # "<BR>", a("worldometers.info", href="https://www.worldometers.info/coronavirus/#countries", target = "_blank"), " (last point) updated on: ", as.POSIXct(time_worldometer, format = "%B %d, %Y, %H:%M", tz = "GMT"), "GMT"
        )
    )
    )
    
    # HTML(paste0("Using code and ideas from ",  
    #         a("@JonMinton", href="https://github.com/JonMinton/COVID-19", target = "_blank"), ", ", 
    #         a("@christoph_sax", href="https://gist.github.com/christophsax/dec0a57bcbc9d7517b852dd44eb8b20b", target = "_blank"), ", ",
    #         a("@nicebread303", href="https://github.com/nicebread/corona", target = "_blank"), ", ",
    #         a("@rubenivangaalen", href="https://twitter.com/rubenivangaalen", target = "_blank"), ", ",
    #         a("@jburnmurdoch", href="https://twitter.com/jburnmurdoch", target = "_blank"), " and ", a(" @sdbernard", href="https://twitter.com/sdbernard", target = "_blank"))),
    
    
    ),

                
        # SHOW PLOT
        mainPanel(
            
            plotOutput("distPlot", height = "900px", width = "120%"),
            
            # hr(),
            # 
            # h3("Data shown in plot ", downloadButton('downloadData', '')),
            # 
            # hr(),
            # 
            # div(
            #     DT::dataTableOutput("mytable", width = "120%"), 
            #     align = "center"
            #     ),
            
            hr(),
            span(
                div(
                    HTML(paste0("Please always check oficial sources (e.g. ", a("WHO", href="https://www.who.int/emergencies/diseases/novel-coronavirus-2019", target = "_blank"), "), and be careful when using this or other information to create predictive models. ",
                                 "By ", a("@gorkang", href="https://twitter.com/gorkang", target = "_blank"))),
                    align = "center", 
                    style = "color:darkgrey")),
            hr()
            )
        )
    )
}


# Server ------------------------------------------------------------------

server <- function(input, output, session) {

    setBookmarkExclude(
        c('mytable_rows_current',
          'mytable_rows_all',
          'mytable_state',
          'mytable_search_columns',
          'mytable_search',
          'mytable_cell_clicked',
          'mytable_rows_selected'))
    
    
    withProgress(message = 'Downloading data', value = 1, min = 0, max = 4, {
        data_download()
    })
    
    # WARNING -----------------------------------------------------------------
    output$WARNING <- renderUI({
        # if (input$cases_deaths == "cases") {
        #     span(h6("REMEMBER, number os cases is not a trusworthy measure: ", br(), br(), "Number of cases is not equivalent to infections. It is limited by number of tests, and most countries are not testing enough. ", br(), br(), "Number of cases are not directly comparable (countries employ different testing strategies)."),
        #          style = "color:darkred")
        # } else if (input$cases_deaths == "deaths") {
            span(h6("REMEMBER: ", br(), br(), "Countries count deaths in different ways (e.g. Some count deaths at hospitals but not at nursing homes and other count both)."),
                 style = "color:darkred")
        # } else if (input$cases_deaths == "CFR") {
        #     span(h6("REMEMBER: ", br(), br(), "Case Fatality Rate (CFR) = deaths / cases.", br(), br(), "Given the number of cases is not trustworthy (e.g. not enough tests), CFR is generally not a precise measure (usually an overestimation)."),
        #          style = "color:darkred")
        # }
    })

    
    # Launch data downloading -------------------------------------------------

    observe({
        auto_invalide()
        message("\n\n* CHECKING IF WE HAVE TO DOWNLOAD DATA ---------------------- \n")
        data_download()
    })    
    
    # Dynamic menus -----------------------------------------------------------
    
    # observeEvent(input$cases_deaths,{
    #     if (input$cases_deaths == "cases") {
    #         hide("min_n_CFR")
    #         hide("min_n_deaths")
    #         show("min_n_cases")
    #     } else if (input$cases_deaths == "deaths") {
    #         hide("min_n_cases")
    #         hide("min_n_CFR")
    #         show("min_n_deaths")
    #     } else if (input$cases_deaths == "CFR") {
    #         hide("min_n_cases")
    #         show("min_n_CFR")
    #         hide("min_n_deaths")
    #     }
    # })
    
    # VAR_min_n = reactive({
    #     if (input$cases_deaths == "cases") {
    #         input$min_n_cases
    #     } else if (input$cases_deaths == "deaths") {
    #         input$min_n_deaths
    #     } else if (input$cases_deaths == "CFR") {
    #         input$min_n_CFR
    #     }
    # })
    
    
    # observeEvent(input$accumulated_daily_pct,{
    #     if (input$accumulated_daily_pct == "accumulated") {
    #         hide("growth_daily")
    #         hide("growth_pct")
    #         show("growth_accumulated")
    #     } else if (input$accumulated_daily_pct == "daily") {
    #         hide("growth_accumulated")
    #         hide("growth_pct")
    #         show("growth_daily")
    #     } else {
    #         hide("growth_daily")
    #         hide("growth_accumulated")
    #         show("growth_pct")
    #     }
    # })

    
    # VAR_growth = reactive({
    #     if (input$accumulated_daily_pct == "accumulated") {
    #         input$growth_accumulated
    #     } else if (input$accumulated_daily_pct == "daily") {
    #         input$growth_daily
    #     } else {
    #         input$growth_pct
    #     }
    # })
    
    # VAR_highlight = reactive({
    #     
    #     if (is.null(input$highlight)) {
    #         " "
    #     } else {
    #         input$highlight
    #     }
    # })

    
    # # Dinamically set highlight choices bases on input$countries_plot
    # outVar = reactive({ c(" ", input$countries_plot %>% sort()) })
    # output$highlight2 = renderUI({
    #     selectInput(inputId = 'highlight', 
    #                 label = 'Highlight countries',
    #                 choices = outVar(),
    #                 multiple = TRUE, 
    #                 selectize = TRUE, 
    #                 width = "100%", 
    #                 selected = " ")
    # })
    
    
    
    INPUT_countries_plot = reactive({
        # COUNTRIES
        dta_raw %>% 
            left_join(DF_population_countries %>% select(country, continent_name)) %>%
            filter(continent_name %in% input$continent_name_input) %>% 
            drop_na(continent_name) %>% 
            filter(deaths_sum > 1) %>% 
            distinct(country) %>% pull(country)
        
        # INPUT_countries_plot = countries_plot
        
        
    })

    
    

# final_df1() creation ------------------------------------------------------

    final_df1 = reactive({ 
        
        withProgress(message = 'Preparing data 2', value = 2, min = 0, max = 4, {
            
            
            # Type
            accumulated_daily_pct = "accumulated"
            INPUT_accumulated_daily_pct = accumulated_daily_pct
            
            INPUT_min_n = 10
            

            # Launch data preparation
            if (!is.null(INPUT_countries_plot())) {
                
                dta_temp = data_preparation(
                    data_source = "JHU",
                    cases_deaths = INPUT_cases_deaths,
                    countries_plot = INPUT_countries_plot(),
                    min_n = INPUT_min_n,
                    relative = INPUT_relative
                )  %>% 
                
                # If repeated values the same day, keep higher
                group_by(country, time) %>% 
                distinct(KEY = paste0(country, time, value), .keep_all = TRUE) %>% 
                select(-KEY) %>% 
                ungroup() %>% 
                
                # re-adjust after filtering
                group_by(country) %>%
                mutate(days_after_100 = as.numeric(0:(length(country) - 1)),
                       days_after_100 = 
                           case_when(
                               # source == "worldometers" ~ lag(days_after_100) + round(as.POSIXlt(as.POSIXct(time_worldometer, format = "%B %d, %Y, %H:%M", tz = "GMT"))$hour/24, 2), #.1
                               source == "worldometers" ~ lag(days_after_100), #.1
                               TRUE ~ days_after_100
                           )) %>% 
                
                mutate(days_after_100 = round(days_after_100, 0)) %>% 
                
                group_by(country) %>%
                
                # Get rid of the latest data if it either 0 or negative
                filter( !(days_after_100 == max(days_after_100, na.rm = TRUE) & diff <= 0)) %>% 
                filter(source == "JHU") %>% 
                
                
                # Create name_end labels
                mutate(
                    name_end =
                        case_when(
                            # days_after_100 == max(days_after_100, na.rm = TRUE) & time == max(time, na.rm = TRUE) ~ paste0(as.character(country), ": ", format(value, big.mark=","), " - ", days_after_100, " days"),
                            days_after_100 == max(days_after_100, na.rm = TRUE) & time == max(time, na.rm = TRUE) ~ paste0(as.character(country)),
                            # what == "lockdown" ~ "*",
                            TRUE ~ "")) %>% 
                mutate(highlight = country)
                
            }
        })
    }) 
    
    # final_df2() creation -----------------------------------------------------
    
    final_df2 = reactive({ 

        withProgress(message = 'Preparing data 2', value = 2, min = 0, max = 4, {
            
            accumulated_daily_pct = "daily"
            INPUT_accumulated_daily_pct = accumulated_daily_pct
            
            INPUT_min_n = 1
            
            # req(input$highlight)
            # req(VAR_highlight())
            # req(INPUT_min_n)
            # req(input$cases_deaths)
            # req(input$countries_plot)
            
            # VARS
            # INPUT_highlight = VAR_highlight()
            # INPUT_min_n = INPUT_min_n
            # INPUT_cases_deaths = input$cases_deaths
            # INPUT_countries_plot = input$countries_plot
            # INPUT_relative = input$relative
            # INPUT_accumulated_daily_pct = input$accumulated_daily_pct
            
            # message("INPUT_highlight = ", INPUT_highlight, "\nINPUT_min_n = ", INPUT_min_n, "\nINPUT_cases_deaths = ", INPUT_cases_deaths, "\nINPUT_countries_plot = ", INPUT_countries_plot, "\nINPUT_relative = ", INPUT_relative, "\nINPUT_accumulated_daily_pct = ", INPUT_accumulated_daily_pct, "\n")
            
            # Launch data preparation

            if (!is.null(INPUT_countries_plot())) {
                
                dta_temp = data_preparation(
                    data_source = "JHU",
                    cases_deaths = INPUT_cases_deaths,
                    countries_plot = INPUT_countries_plot(),
                    min_n = INPUT_min_n,
                    relative = INPUT_relative
                )  %>% 
                    
                    # If repeated values the same day, keep higher
                    group_by(country, time) %>% 
                    distinct(KEY = paste0(country, time, value), .keep_all = TRUE) %>% 
                    select(-KEY) %>% 
                    ungroup() %>% 
                    
                    # re-adjust after filtering
                    group_by(country) %>%
                    mutate(days_after_100 = as.numeric(0:(length(country) - 1)),
                           days_after_100 = 
                               case_when(
                                   # source == "worldometers" ~ lag(days_after_100) + round(as.POSIXlt(as.POSIXct(time_worldometer, format = "%B %d, %Y, %H:%M", tz = "GMT"))$hour/24, 2), #.1
                                   source == "worldometers" ~ lag(days_after_100), #.1
                                   TRUE ~ days_after_100
                               )) %>% 
                    
                    mutate(days_after_100 = round(days_after_100, 0)) %>% 
                    
                    group_by(country) %>%
                    
                    # Get rid of the latest data if it either 0 or negative
                    filter( !(days_after_100 == max(days_after_100, na.rm = TRUE) & diff <= 0)) %>% 
                    filter(source == "JHU") %>% 
                    
                    # Create name_end labels
                    mutate(
                        name_end =
                            case_when(
                                # days_after_100 == max(days_after_100, na.rm = TRUE) & time == max(time, na.rm = TRUE) ~ paste0(as.character(country), ": ", format(value, big.mark=","), " - ", days_after_100, " days"),
                                days_after_100 == max(days_after_100, na.rm = TRUE) & time == max(time, na.rm = TRUE) ~ paste0(as.character(country)),
                                # what == "lockdown" ~ "*",
                                TRUE ~ ""))
                
                # browser()
                dta_temp %>% 
                    
                    rename(value_temp = value,
                           value = diff) %>% 
                    ungroup()  %>% 
                    mutate(value_old = value) %>%
                    group_by(country) %>% 
                    
                    # Rolling mean of last 7 days --------------
                mutate(value = map_dbl(1:n(), ~ mean(value[(max(.x - 7, 1)):.x], na.rm = FALSE))) %>%
                    filter(value_temp >= 10)
                
                
                
                
            }
        })
    }) 

    # Growth line ---------------
    growth_line = reactive({

        withProgress(message = 'Growth line', value = 2, min = 0, max = 4, {
            
            INPUT_min_n = 10
        
        # LIMITS of DATA
        # if (INPUT_accumulated_daily_pct == "daily") {
        #     MAX_y = max(final_df2()$diff, na.rm = TRUE) * 1.1
        # # } else if (input$accumulated_daily_pct == "%") {
        # #     MAX_y = max(final_df2()$diff_pct, na.rm = TRUE) * 100
        # } else {
            MAX_y = max(final_df1()$value, na.rm = TRUE) * 1.1
        # }
        
        # To avoid error
        if (is.infinite(max(final_df1()$days_after_100, na.rm = TRUE))) {
            max_finaldf_days_after_100 = 10 
        } else {
            max_finaldf_days_after_100 = max(final_df1()$days_after_100, na.rm = TRUE)
        }
        
        line_factor = 1
            tibble(
                value = cumprod(c(INPUT_min_n, rep((100 + INPUT_growth) / 100, line_factor * max_finaldf_days_after_100))),
                days_after_100 = 0:(line_factor * max_finaldf_days_after_100)) %>% 
                filter(value <= MAX_y)
    
        })    
    })
    
    

    

    # final_plot1() -----------------------------------------------------------

    final_plot1 <- reactive({
        
        req(final_df1())
        
        withProgress(message = 'Preparing plot 1', value = 3, min = 0, max = 4, {
            
            DF_plot = final_df1() %>% ungroup()
            
            # Prepare vars for overlay
            reference_continent = DF_plot %>% filter(days_after_100 == max(days_after_100)) %>% head(1) %>% pull("continent_name")
            reference_country = DF_plot %>% filter(days_after_100 == max(days_after_100)) %>% head(1) %>% pull("country")
            list_continents_plot = DF_plot %>% ungroup() %>%  distinct(continent_name) %>% pull(continent_name)
            list_remaining_continents_plot = list_continents_plot[!list_continents_plot %in% reference_continent]
            
            if (!is_empty(list_remaining_continents_plot)) {
            
                ann_position = 
                    1:nrow(DF_plot %>% filter(continent_name %in% list_remaining_continents_plot) %>% ungroup() %>% distinct(continent_name)) %>%
                    map(~ max(
                        DF_plot %>% 
                            filter(continent_name == list_remaining_continents_plot[.x]) %>% 
                            pull(days_after_100))) %>% unlist()
                
                label_headstart = paste0("[", reference_country, " headstart]")
                
            
            } else {
                # If only one continent remaining
                list_remaining_continents_plot = reference_continent
                ann_position = Inf
                label_headstart = ""
            }
            
            
            ann_text <-
                data.frame(
                    days_after_100 = (ann_position + max(DF_plot$days_after_100))/2, # 39 Italy, # 55 China
                    value = 10,
                    lab = "text",
                    country = NA_character_,
                    continent_name = factor(
                        list_remaining_continents_plot, 
                        levels = list_remaining_continents_plot
                    )
                )
            
            
            
            # Define which countries get a smooth (have enough points)
            VALUE_span = 3
            counts_filter = DF_plot %>% count(country) %>% filter(n > VALUE_span)
            
            
            
            p_temp = ggplot(data = DF_plot, 
                            aes(x = days_after_100, y = value, group = as.factor(country), color = continent_name)) +
                scale_color_hue(l = 50) +
                
                # Trend line
                geom_line(data = growth_line(), aes(days_after_100, value), linetype = "dotted", inherit.aes = FALSE, alpha = 1, size = .9) +
                
                # Country points (last one bigger)
                geom_point(aes(size = .005 + as.integer(final_df1()$name_end != "" & final_df1()$name_end != "*") - .5), alpha = .1) +
                
                scale_x_continuous(breaks = seq(0, max(final_df1()$days_after_100, na.rm = TRUE) + 1, 10)) +
                labs(
                    x = paste0("Days after ",  INPUT_min_n ," accumulated ", "deaths"),
                    y = paste0("Confirmed ", "accumulated", " ", "deaths", " (log)")
                ) +
                theme_minimal(base_size = 12) +
                
                theme(legend.position = "none",
                      panel.grid.minor = element_line(size = 0.25),
                      panel.spacing = unit(1, "lines")
                )

            p_temp = p_temp +  
                
                geom_line(alpha = .4) +
                
                geom_smooth(data = DF_plot %>% filter(country %in% counts_filter$country), 
                            aes(x = days_after_100, y = value, group = as.factor(continent_name), color = continent_name),
                            method = "loess", span = 1.5, se = FALSE, size = 1, alpha = .9, na.rm = TRUE)

            # If any value is not " ", scale_color_identity
            if (any(' ' != VAR_highlight)) { p_temp =  p_temp + scale_color_identity() }
            
            
            # LIMITS
            MAX_y = max(final_df1()$value, na.rm = TRUE, na.rm = TRUE) * 1.1
            MIN_y = min(final_df1()$value, na.rm = TRUE, na.rm = TRUE) * 0.95
            
            if (MIN_y == 0) MIN_y = 1

            p_temp = p_temp +
                scale_y_log10(breaks = scales::log_breaks(n = 10), labels = function(x) format(x, big.mark = ",", scientific = FALSE), limits = c(MIN_y, MAX_y)) 
            
            
            # Annotation trend line
            which_position = 22
            x_axis = max(growth_line()$days_after_100[which_position], na.rm = TRUE) - 3.5
            y_axis = max(growth_line()$value[which_position], na.rm = TRUE)  # MAX 
            

            p_final1 <- p_temp + 
                annotate(geom = "text",
                         size = 2.8,
                         x = x_axis, 
                         y = y_axis, 
                         angle = 70,
                         alpha = .8,
                         label = paste0(INPUT_growth, "% growth")) +
                
                # Country label
                ggrepel::geom_label_repel(aes(label = name_end), show.legend = FALSE, segment.color = "grey", segment.size  = .15, alpha = .75, size = 3, seed = 30) +
                geom_rect(data = data.frame(continent_name = list_remaining_continents_plot),
                          aes(
                              xmin = ann_position,
                              xmax = Inf,
                              ymin = 0,
                              ymax = Inf),
                          alpha = 0.2,
                          fill = "grey",
                          inherit.aes = FALSE) +
                geom_text(data = ann_text, label = label_headstart, size = 3, alpha = .8, color = "black") +    
                facet_grid( ~ continent_name) +
                theme(aspect.ratio = 1)
            
            p_final1
            
            
        })
        
    })
    

    # final_plot2() -----------------------------------------------------------
    
    final_plot2 <- reactive({

        req(final_df2())
        
        withProgress(message = 'Preparing plot 2', value = 3, min = 0, max = 4, {

            # Define which countries get a smooth (have enough points)
            VALUE_span = 3
            counts_filter = final_df2() %>% count(country) %>% filter(n > VALUE_span)
            
            
            p_temp = ggplot(data = final_df2(), 
                            aes(x = value_temp, y = value, group = as.factor(country), color = continent_name)) +
                scale_color_hue(l = 50) +
                
                # Trend line
                geom_abline(intercept = -1, slope = 1, linetype = "dashed", alpha = .8, size = .5) +

                # Country points (last one bigger)
                geom_point(aes(size = .005 + as.integer(final_df2()$name_end != "" & final_df2()$name_end != "*") - .5), alpha = .1) +
                
                # scale_x_continuous(breaks = seq(0, max(final_df2()$value_temp, na.rm = TRUE) + 1, 5)) +
                scale_x_log10(breaks = scales::log_breaks(n = 7), labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
                
                labs(
                    x = "Confirmed acumulated deaths (log)",
                    y = paste0("Weekly average for confirmed ", "daily", " ", "deaths", " (log)",  if (relative == TRUE) " / million people")
                ) +
                theme_minimal(base_size = 12) +
                
                theme(legend.position = "none",
                      panel.grid.minor = element_line(size = 0.25),
                      panel.spacing = unit(1, "lines")

                ) +  
                
                geom_line(alpha = .4) +
                
                geom_smooth(data = final_df2() %>% filter(country %in% counts_filter$country), 
                            aes(x = value_temp, y = value, group = as.factor(continent_name), color = continent_name),
                            method = "loess", span = 1.5, se = FALSE, size = 1, alpha = .9, na.rm = TRUE) 
            
            #+ scale_color_identity() 
            
            
            # LIMITS
            MAX_y = max(final_df2()$value, na.rm = TRUE, na.rm = TRUE) * 1.1
            MIN_y = min(final_df2()$value, na.rm = TRUE, na.rm = TRUE) * 0.1 # In Log scale can't use 0
            if (MIN_y <= 0) MIN_y = 1

            p_temp = p_temp +
                # scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = function(x) format(x, big.mark = ",", scientific = FALSE), limits = c(MIN_y, MAX_y)) +
                scale_y_log10(breaks = scales::log_breaks(n = 10), labels = function(x) format(x, big.mark = ",", scientific = FALSE), limits = c(MIN_y, MAX_y)) 
                labs(y = paste0("Confirmed ", "daily", " ", cases_deaths))

        
            # x_axis = max(growth_line()$value_temp, na.rm = TRUE) - 1.5
            # y_axis = max(growth_line()$value, na.rm = TRUE) + 1 # MAX 

            p_final <- p_temp + 
                
                # Country label
                ggrepel::geom_label_repel(aes(label = name_end), show.legend = FALSE, segment.color = "grey", segment.size  = .15, alpha = .75, size = 3) +
                facet_grid( ~ continent_name) +
                theme(aspect.ratio = 1)
                
            p_final
            
        })
    })
    
    
    final_plot12 <- reactive({
        
        req(final_plot1())
        req(final_plot2())
        
        withProgress(message = 'Rendering plot', value = 4, min = 0, max = 4, {
            
            cowplot::plot_grid(
                # title,
                final_plot1(),
                final_plot2(),
                nrow = 2,
                # labels = c("A)", "B)"),
                rel_heights = c(1, 1)
                # nrow = 3,
                # labels = c("", "A)", "B)"),
                # rel_heights = c(0.1, 1, 1)
                
            )
            
        })
    })
    
    # Show plot
    output$distPlot <- renderCachedPlot({
        
        req(final_plot12())
        
        withProgress(message = 'Show plot', value = 4, min = 0, max = 4, {
            
        final_plot12()
            
        })
    }, cacheKeyExpr = list(final_df2(), growth_line()))
    

    
    output$downloadPlot <- downloadHandler(
        filename = function() { paste(Sys.Date(), "_corona.png", sep = "") },
        content = function(file) { ggsave(file, plot = final_plot12(), device = "png", width = 14, height = 10) }
    )

}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url") #, options = "test.mode"
