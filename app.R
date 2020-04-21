
# Libraries ---------------------------------------------------------------

library(cowplot)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(httr)
library(purrr)
library(readr)
library(scales)
library(shiny)
library(shinyjs)
library(shinythemes)
library(tidyr)
library(vroom)



# Data preparation --------------------------------------------------------

cases_deaths = "deaths"

source(here::here("R/download_or_load_JH_API.R"))

source(here::here("R/data-download.R"))
source(here::here("R/data-preparation.R"))

source(here::here("R/data-preparation-menu.R"))


# Launch data_download 
minutes_to_check_downloads = 30
auto_invalide <- reactiveTimer(minutes_to_check_downloads * 60 * 1000) 

file_info_JHU <- file.info("outputs/raw_JH.csv")$mtime
if( is_empty(file_info_JHU)) file_info_JHU = "..."


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
                column(1, HTML("<a href=\"http://psicologia.uai.cl/\", target = \"_blank\"><img src=\"UAI_mini.png\", alt ='Universidad Adolfo Ibáñez'></a>"))
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
    
    selectInput(inputId = 'filter_countries', 
                label = 'Filter out countries',
                choices = c(" ", V1_alternatives), multiple = TRUE, 
                selectize = TRUE, 
                width = "100%", 
                selected = c(" ")),
    
    HTML("<BR>"),
    
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
            a("Johns Hopkins Data", href="https://covid19api.com/", target = "_blank"), " updated on: ", as.character(file_info_JHU), " GMT"
        )
    )
    )
    ),

                
        # SHOW PLOT
        mainPanel(
            
            plotOutput("distPlot", height = "900px", width = "120%"),
            
            span(
                div(
                    HTML(
                        paste0(
            "<B>Evolution patterns of Coronavirus deaths by regions and countries</B> <BR><BR>
            <B>(Top)</B> Temporal dynamics of accumulated deaths.  The accumulated deaths grow over time for all the countries and regions by days since the first 10 reported deaths. The slope represents the rate of growth, and the length of the trajectory, the number of days since the death toll reached 10 for that country. This shows the time scale of the epidemic for each country and region. A dashed line of 30% growth in each region help to identify the similar growing rate across countries and regions.  A gray headset is shown to help visualize the time gap in comparison with the start of the COVID-19 outbreak in China. It can be appreciated how the LACs are, compared to most countries/regions, in the early days, although their growth is in line with Europe or North America. <BR><BR>
            <B>(Bottom)</B> Exponential growth of confirmed daily deaths by countries and regions. Rate of change shown as daily new deaths (weekly average) divided by total deaths. Moving in the diagonal implies the growth rate is exponential. As we can appreciate, all regions except Asia are growing exponentially, with Europe stating to slowly deviate from the diagonal. Although the number of deaths is still low in most LACs (except Brazil), the rate of change is clearly exponential and resembling other regions. Plots present a line for each country, plus a bold line with a smooth conditional mean by region (using the loess method). Both panels show how the epidemic started relatively late in LACs, which gave them a time buffer to react. Despite this, the global trajectories of the regions clearly suggest exponential growth similar to Europe or North America. <BR><BR>
            A version of the plot above appears in:  [LINK TO PAPER HERE]<BR><BR>
            Data retrieved from <a href='https://github.com/CSSEGISandData/COVID-19', target = '_blank'>Johns Hopkins CSSE</a> through <a href='https://covid19api.com/', target = '_blank'>covid19api</a>.
            The code for the creation of these plots can be found in <a href='https://github.com/gorkang/2020-coronavirus-LATAM', target = '_blank'>Github: 2020-coronavirus-LATAM</a>."

            )),
            
            align = "justify", 
            style = "color:darkgrey")),
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

    # setBookmarkExclude(
    #     c('mytable_rows_current'))
    
    
    # WARNING -----------------------------------------------------------------
    output$WARNING <- renderUI({

            span(h6("REMEMBER: ", br(), br(), "Countries count deaths in different ways (e.g. Some count deaths at hospitals but not at nursing homes and other count both)."),
                 style = "color:darkred")

        })

    
    # Launch data downloading -------------------------------------------------

    observe({
        
        withProgress(message = 'Download or load data...', value = 1, min = 0, max = 5, {
            
        auto_invalide()
        message("\n\n* CHECKING IF WE HAVE TO DOWNLOAD DATA ---------------------- \n")
        data_download()
        
        })
    })
    
    # Dynamic menus -----------------------------------------------------------
    
    INPUT_countries_plot = reactive({

        withProgress(message = 'Preparing raw data', value = 1, min = 0, max = 5, {
            
        # COUNTRIES
        dta_raw %>% 
            left_join(DF_population_countries %>% select(country, continent_name)) %>%
            filter(continent_name %in% input$continent_name_input) %>% 
            drop_na(continent_name) %>% 
            filter(deaths_sum > 1) %>% 
            distinct(country) %>% 
            filter(! country %in% input$filter_countries) %>% 
            pull(country)
        })
    })

    
    

# final_df1() creation ------------------------------------------------------

    final_df1 = reactive({ 
        
        req(INPUT_countries_plot())
        
        withProgress(message = 'Preparing data plot B', value = 2, min = 0, max = 5, {
            
            
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
                mutate(days_after_100 = as.numeric(0:(length(country) - 1))) %>% 
                mutate(days_after_100 = round(days_after_100, 0)) %>% 
                
                group_by(country) %>%
                
                # Get rid of the latest data if it either 0 or negative
                filter( !(days_after_100 == max(days_after_100, na.rm = TRUE) & diff <= 0)) %>% 
                filter(source == "JHU") %>% 
                
                
                # Create name_end labels
                mutate(
                    name_end =
                        case_when(
                            days_after_100 == max(days_after_100, na.rm = TRUE) & time == max(time, na.rm = TRUE) ~ paste0(as.character(country)),
                            TRUE ~ "")) %>% 
                mutate(highlight = country)
                
            }
        })
    }) 
    
    
    # final_df2() creation -----------------------------------------------------
    
    final_df2 = reactive({ 

        req(INPUT_countries_plot())
        
        withProgress(message = 'Preparing data plot B', value = 2, min = 0, max = 5, {
            
            accumulated_daily_pct = "daily"
            INPUT_accumulated_daily_pct = accumulated_daily_pct
            
            INPUT_min_n = 1
            
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
                    mutate(days_after_100 = as.numeric(0:(length(country) - 1))) %>% 
                    mutate(days_after_100 = round(days_after_100, 0)) %>% 
                    
                    group_by(country) %>%
                    
                    # Get rid of the latest data if it either 0 or negative
                    filter( !(days_after_100 == max(days_after_100, na.rm = TRUE) & diff <= 0)) %>% 
                    filter(source == "JHU") %>% 
                    
                    # Create name_end labels
                    mutate(
                        name_end =
                            case_when(
                                days_after_100 == max(days_after_100, na.rm = TRUE) & time == max(time, na.rm = TRUE) ~ paste0(as.character(country)),
                                TRUE ~ ""))
                

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

        req(final_df1())
        
        withProgress(message = 'Growth line', value = 2, min = 0, max = 5, {
            
            INPUT_min_n = 10
        
            MAX_y = max(final_df1()$value, na.rm = TRUE) * 1.1
        
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
        
        withProgress(message = 'Preparing plot A', value = 3, min = 0, max = 5, {
            
            # Prepare vars for overlay
            reference_continent = final_df1() %>% filter(days_after_100 == max(days_after_100)) %>% arrange(desc(days_after_100)) %>% head(1) %>% pull("continent_name")
            reference_country = final_df1() %>% filter(days_after_100 == max(days_after_100)) %>% arrange(desc(days_after_100)) %>% head(1) %>% pull("country")
            list_continents_plot = final_df1() %>% ungroup() %>%  distinct(continent_name) %>% pull(continent_name)
            list_remaining_continents_plot = list_continents_plot[!list_continents_plot %in% reference_continent]
            
            if (!is_empty(list_remaining_continents_plot)) {
            
                ann_position = 
                    1:nrow(final_df1() %>% filter(continent_name %in% list_remaining_continents_plot) %>% ungroup() %>% distinct(continent_name)) %>%
                    map(~ max(
                        final_df1() %>% 
                            filter(continent_name == list_remaining_continents_plot[.x]) %>% 
                            pull(days_after_100))) %>% unlist()
                
                label_headstart = paste0("[", reference_country, "'s headstart]")
                
            
            } else {
                # If only one continent remaining
                list_remaining_continents_plot = reference_continent
                ann_position = Inf
                label_headstart = ""
            }
            
            
            ann_text <-
                data.frame(
                    days_after_100 = (ann_position + max(final_df1()$days_after_100))/2, # 39 Italy, # 55 China
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
            counts_filter = final_df1() %>% count(country) %>% filter(n > VALUE_span)
            
            
            
            p_temp = ggplot(data = final_df1(), 
                            aes(x = days_after_100, y = value, group = as.factor(country), color = continent_name)) +
                # scale_color_hue(l = 50) +
                
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
                
                geom_smooth(data = final_df1() %>% filter(country %in% counts_filter$country), 
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
                theme(aspect.ratio = 1) %>% 
                scale_colour_manual(drop = TRUE,
                                    limits = levels(final_df1()$continent_name), 
                                    values = c('#cb4d42','#377eb8','#4daf4a','#984ea3','#ff7f00','#3B7080'))#c(brewer.pal(6,"Dark2")))
            
            p_final1
            
            
        })
        
    })
    

    # final_plot2() -----------------------------------------------------------
    
    final_plot2 <- reactive({

        req(final_df2())
        
        withProgress(message = 'Preparing plot B', value = 3, min = 0, max = 5, {

            # Define which countries get a smooth (have enough points)
            VALUE_span = 3
            counts_filter = final_df2() %>% count(country) %>% filter(n > VALUE_span)
            
            
            p_temp = ggplot(data = final_df2(), 
                            aes(x = value_temp, y = value, group = as.factor(country), color = continent_name)) +
                # scale_color_hue(l = 50) +
                
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


            p_final <- p_temp + 
                
                # Country label
                ggrepel::geom_label_repel(aes(label = name_end), show.legend = FALSE, segment.color = "grey", segment.size  = .15, alpha = .75, size = 3) +
                facet_grid( ~ continent_name) +
                theme(aspect.ratio = 1) + 
                scale_colour_manual(drop = TRUE,
                                    limits = levels(final_df2()$continent_name), 
                                    values = c('#cb4d42','#377eb8','#4daf4a','#984ea3','#ff7f00','#3B7080'))#c(brewer.pal(6,"Dark2")))
            
            p_final
            
        })
    })
    
    
    final_plot12 <- reactive({
        
        req(final_plot1())
        req(final_plot2())
        
        withProgress(message = 'Rendering plot', value = 4, min = 0, max = 5, {
            
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
        
        withProgress(message = 'Show plot', value = 5, min = 0, max = 5, {
            
        req(final_plot1())
        req(final_plot2())
        req(final_plot12())
        
        final_plot12()
            
        })
        
    }, cacheKeyExpr = list(final_df1(), final_df2(), final_plot1(), final_plot2(), final_plot12(), growth_line()))
    

    
    output$downloadPlot <- downloadHandler(
        filename = function() { paste(Sys.Date(), "_corona.png", sep = "") },
        content = function(file) { ggsave(file, plot = final_plot12(), device = "png", width = 16, height = 9) }
    )

}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")
