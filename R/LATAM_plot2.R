# library(tidyverse)
# library(data.table)
# source("R/data-preparation.R")

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

# Variables ---------------------------------------------------------------

# Data preparation
  
  min_n = 1
  INPUT_min_n = min_n 
  VAR_min_n = min_n
  
  cases_deaths = "deaths"
  INPUT_cases_deaths = cases_deaths
  
  INPUT_continent_name_input = c("Asia", "Europe", "North America", "Latin America")
  
  
  # countries_plot = c("USA", "Spain", "Italy", "France", "Germany", "United Kingdom", "Chile", "Denmark")
  
  countries_plot = dta_raw %>% 
    left_join(DF_population_countries %>% select(country, continent_name)) %>%
    filter(continent_name %in% INPUT_continent_name_input) %>% 
    drop_na(continent_name) %>% 
    filter(deaths_sum > 1) %>% 
    distinct(country) %>% 
    pull(country)
  
  INPUT_countries_plot = countries_plot


  
# Plot specific
  
  smooth = TRUE
  log_scale = TRUE
  

  relative = FALSE
  INPUT_relative = relative
  
  
  accumulated_daily_pct = "daily"
  INPUT_accumulated_daily_pct = accumulated_daily_pct
  
  growth = 30
  VAR_growth = growth

  highlight = " "
  INPUT_highlight = highlight
  VAR_highlight = highlight



# Data preparation --------------------------------------------------------

  dta = data_preparation(
    data_source = "JHU",
    cases_deaths = INPUT_cases_deaths,
    countries_plot = INPUT_countries_plot,
    min_n = INPUT_min_n,
    relative = INPUT_relative
  ) %>% 
    
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
  


# Highlight
if (any(' ' != VAR_highlight)) {
  
  # Create colors diccionary
  DF_colors_temp =
    dta_temp %>% 
    filter(country %in% VAR_highlight) %>% 
    distinct(country)
  
  # If the selected country does not have observations over the threshold
  if (nrow(DF_colors_temp) > 0) { 
    DF_colors = DF_colors_temp %>% 
      bind_cols(highlight = hue_pal(l = 50)(nrow(.)))
  } else {
    DF_colors = DF_colors_temp %>% 
      mutate(highlight = "")
  }
  
  
  final_df = dta_temp %>% 
    left_join(DF_colors, by = "country") %>% 
    mutate(highlight = 
             case_when(
               is.na(highlight) ~ "grey",
               TRUE ~ highlight
             ))
} else {

# HERE --------------------------------------------------------------------

  
  final_df2 = dta_temp %>% mutate(highlight = country)
  
}



 # Show accumulated or daily plot
    if (accumulated_daily_pct == "daily") {
      
      DF_plot = final_df2 %>% 
        rename(value_temp = value,
               value = diff) %>% 
        ungroup()  %>% 
        mutate(value_old = value) %>%
        group_by(country) %>% 
        
        # Rolling mean of last 7 days --------------
        mutate(value = map_dbl(1:n(), ~ mean(value[(max(.x - 7, 1)):.x], na.rm = FALSE))) %>%
        filter(value_temp >= 10)
      
        # mutate(value = frollmean(temp_final_df[, "value"], n = 7, na.rm = TRUE, by = "country") %>% unlist())
        
    } else if (accumulated_daily_pct == "%") {
      DF_plot = final_df2 %>% 
        rename(value_temp = value,
               value = diff_pct) %>% 
        mutate(value = value * 100)
    } else {
      DF_plot = final_df2
    }
    
    # Define which countries get a smooth (have enough points)
    VALUE_span = 3
    counts_filter = DF_plot %>% count(country) %>% filter(n > VALUE_span)
    
    
    
    
    
    

# Growth line -------------------------------------------------------------

    
      
      # LIMITS of DATA
      if (accumulated_daily_pct == "daily") {
        MAX_y = max(final_df2$diff, na.rm = TRUE) * 1.1
      } else if (accumulated_daily_pct == "%") {
        MAX_y = max(final_df2$diff_pct, na.rm = TRUE) * 100
      } else {
        MAX_y = max(final_df2$value, na.rm = TRUE) * 1.1
      }
      
      # To avoid error
      if (is.infinite(max(DF_plot$days_after_100, na.rm = TRUE))) {
        max_finaldf_days_after_100 = 10 
      } else {
        max_finaldf_days_after_100 = max(DF_plot$days_after_100, na.rm = TRUE)
      }
      
      # If we use 1.1 * to avoid overlaping y axis goes up a lot
      line_factor = 1
      if (accumulated_daily_pct == "%") {
        growth_line = tibble(
          value = cumprod(c(100, rep((100 + VAR_growth) / 100, line_factor * max_finaldf_days_after_100))),
          days_after_100 = 0:(line_factor * max_finaldf_days_after_100)) %>% 
          filter(value <= MAX_y)
      } else {
        growth_line = tibble(
          value = cumprod(c(VAR_min_n, rep((100 + VAR_growth) / 100, line_factor * max_finaldf_days_after_100))),
          days_after_100 = 0:(line_factor * max_finaldf_days_after_100)) %>% 
          filter(value <= MAX_y)
      }
      
  
    
    
    
    

# Text annotation Latam ---------------------------------------------------
    
      ann_text <-
        data.frame(
          value_temp = max(DF_plot$value_temp)/2, # 38 Italy, # 55 China
          value = 10,
          lab = "text",
          country = NA_character_,
          continent_name = factor(
            "Latin America",
            levels = c("Europe", "Latin America", "North America")
          )
        )
      
      
      

# COLORS ------------------------------------------------------------------

      DF_color_palletes = DF_plot %>% 
        ungroup() %>% 
        distinct(country, .keep_all = TRUE) %>% 
        select(country, continent_name) %>% 
        arrange(continent_name, country)  
        # count(continent_name)
        
        # DF_color_palletes %>% count(continent_name) %>% .[1, 1] %>% pull(country)
        

      
# DF_pallette_final = 1:nrow(DF_color_palletes %>% count(continent_name)) %>% 
#   
#   map( ~
#     
#     colorscale::create_single_scale(
#       color = c("#1D9A6C", "darkgreen", "red", "orange")[.x],
#       n_dark = 
#         DF_color_palletes %>% count(continent_name) %>% .[.x, 2] %>% pull(n) / 2,
#       # (DF_color_palletes[.x, 2]$n) / 2,
#       darkness = 0.5,
#       rotate_dark = -51,
#       saturation_dark = -0.14,
#       n_light = 
#         DF_color_palletes %>% count(continent_name) %>% .[.x, 2] %>% pull(n) / 2,
#         # (DF_color_palletes[.x, 2]$n) / 2,
#       lightness = 0.8,
#       rotate_light = 67,
#       saturation_light = 0.2
#     ) %>% as_tibble() %>% 
#       sample_n(DF_color_palletes %>% count(continent_name) %>% .[.x, 2] %>% pull(n))) %>%
#   bind_rows() %>% 
#   mutate(country = DF_color_palletes %>% count(country)%>% pull(country))


# DF_pallette_final
# pallette_final <- setNames(as.character(DF_pallette_final$country), DF_pallette_final$value)
# pallette_final <- setNames(as.character(DF_pallette_final$value), DF_pallette_final$country)

    
      # cols <- c("8" = "red", "4" = "blue", "6" = "darkgreen", "10" = "orange")
      # p + scale_colour_manual(values = pallette_final)
      
    
    # Draw plot ---------------------------------------------
    
    p_temp = ggplot(data = DF_plot, 
                    aes(x = value_temp, y = value, group = as.factor(country), color = continent_name)) +
        # scale_color_hue(l = 50) +
      
        # Trend line
        geom_abline(intercept = -1, slope = 1, linetype = "dashed", alpha = .8, size = .5) +
        # geom_line(data = growth_line, aes(value_temp, value), linetype = "dotted", inherit.aes = FALSE, alpha = 1) +
        
        # Country points (last one bigger)
        geom_point(aes(size = .005 + as.integer(DF_plot$name_end != "" & DF_plot$name_end != "*") - .5), alpha = .1) +
        
        # scale_x_continuous(breaks = seq(0, max(DF_plot$value_temp, na.rm = TRUE) + 1, 5)) +
        scale_x_log10(breaks = scales::log_breaks(n = 7), labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
          # breaks = scales::log_breaks(n = 10), labels = function(x) format(x, big.mark = ",", scientific = FALSE), limits = c(MIN_y, MAX_y)) 
      
        
        labs(
          # title = paste0("Coronavirus confirmed ", cases_deaths , if (relative == TRUE) " / million people"),
          # subtitle = paste0("Starting at ",  "10" ," or more accumulated ", cases_deaths),
          # subtitle = paste0("Starting at ",  VAR_min_n ," or more accumulated ", cases_deaths),
          x = "Confirmed acumulated deaths (log)",
          # x = paste0("Days after ",  VAR_min_n ," accumulated ", cases_deaths),
          y = paste0("Weekly average for confirmed ", accumulated_daily_pct, " ", cases_deaths, " (log)",  if (relative == TRUE) " / million people")
          # caption = paste0("Sources: Johns Hopkins CSSE")
          ) +
        theme_minimal(base_size = 12) +
        
        theme(legend.position = "none",
              panel.grid.minor = element_line(size = 0.25),
              panel.spacing = unit(1, "lines")
                                              # linetype = 'solid',
                                              # colour = "black")
        )
    
    # Smooth or not
    if(smooth == FALSE) {
      p_temp = p_temp + geom_line(alpha = .4) 
    } else {
      p_temp = p_temp +  
        
        geom_line(alpha = .4) +
      
        geom_smooth(data = DF_plot %>% filter(country %in% counts_filter$country), 
                    aes(x = value_temp, y = value, group = as.factor(continent_name), color = continent_name),
                    method = "loess", span = 1.5, se = FALSE, size = 1, alpha = .9, na.rm = TRUE)
    }
    
    # If any value is not " ", scale_color_identity
    if (any(' ' != VAR_highlight)) { p_temp =  p_temp + scale_color_identity() }
    
    
    # LIMITS
    if (accumulated_daily_pct == "daily") {
      MAX_y = max(final_df2$diff, na.rm = TRUE, na.rm = TRUE) * 1.1
      MIN_y = min(final_df2$diff, na.rm = TRUE, na.rm = TRUE) * 0.1 # In Log scale can't use 0
      if (MIN_y <= 0) MIN_y = 1
    } else if (accumulated_daily_pct == "%") {
      MAX_y = max(final_df2$diff_pct, na.rm = TRUE, na.rm = TRUE) * 100
      MIN_y = min(final_df2$diff_pct, na.rm = TRUE, na.rm = TRUE) * 1 # In Log scale can't use 0
    } else {
      MAX_y = max(final_df2$value, na.rm = TRUE, na.rm = TRUE) * 1.1
      MIN_y = min(final_df2$value, na.rm = TRUE, na.rm = TRUE) * 0.95
    }
    
    if (MIN_y == 0) MIN_y = 1
    # message("MIN_y: ", MIN_y, " MAX_y: ", MAX_y)
    
    # Scale, log or not
    if (log_scale == TRUE) {
      p_temp = p_temp +
        scale_y_log10(breaks = scales::log_breaks(n = 10), labels = function(x) format(x, big.mark = ",", scientific = FALSE), limits = c(MIN_y, MAX_y)) 
    } else {
      p_temp = p_temp +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = function(x) format(x, big.mark = ",", scientific = FALSE), limits = c(MIN_y, MAX_y)) +
        labs(y = paste0("Confirmed ", accumulated_daily_pct, " ", cases_deaths))
    }
    
    
    # Annotation trend line
    if (accumulated_daily_pct == "%") {
      x_axis = max(growth_line$value_temp, na.rm = TRUE) - .5
      y_axis = min(growth_line$value, na.rm = TRUE) # MIN
    } else {
      x_axis = max(growth_line$value_temp, na.rm = TRUE) - 1.5
      y_axis = max(growth_line$value, na.rm = TRUE) + 1 # MAX 
      # message("X: ", x_axis, " Y: ", y_axis)
    }
    
  p2_to_save <- p_temp + 
      
      # Country label
      ggrepel::geom_label_repel(aes(label = name_end), show.legend = FALSE, segment.color = "grey", segment.size  = .15, alpha = .75, size = 3) +
      facet_grid( ~ continent_name) +#, scales = "free_x"
      theme(aspect.ratio = 1) +
      scale_colour_manual(drop = TRUE,
                          limits = levels(DF_plot$continent_name), 
                          values = c('#cb4d42','#377eb8','#4daf4a','#984ea3','#ff7f00','#3B7080'))
    
    
    p2_to_save 
    
    # ggsave("NOT_GITHUB/DEV/LATAM/LATAM_plot2.png", p2_to_save, width = 16, height = 9, dpi = 300)
    