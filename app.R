library(shiny)
library(tidyverse)
library(lubridate)
library(ggpubr)

# John Hopkins data on GitHubt
confirmed <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
confirmed_us <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv'
deaths <- 'https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv'
deaths_us <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv'
recovered <- 'https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv'

# Import data and convert to tidy format
# The US specific datasets have extra columns so I removed those
process <- function(x, y) {
  if (str_detect(y, 'us') == T) {
    read_csv(x, col_types = cols()) %>%
      select(-c(UID, iso2, iso3, code3, FIPS, Admin2, Combined_Key)) %>%
      pivot_longer(5:ncol(.), names_to = 'date', values_to = y) %>%
      mutate(date = mdy(str_remove(date, 'X'))) %>%
      janitor::clean_names() %>%
      mutate(province_state = replace_na(province_state, 'All'))
  } else {
    read_csv(x, col_types = cols()) %>%
      pivot_longer(5:ncol(.), names_to = 'date', values_to = y) %>%
      mutate(date = mdy(date)) %>%
      janitor::clean_names() %>%
      mutate(province_state = replace_na(province_state, 'All'))
  }
}

# Used to calculate the rolling average
# n is the number of days to average over
rolling_average <- function(x,n) {
  stats::filter(x, rep(1/n,n), sides=1)
}

# Read in data and combine all the datasets
merged_data <- map2(list(confirmed, confirmed_us, deaths, deaths_us, recovered), 
                    list('confirmed', 'confirmed_us', 'deaths', 'deaths_us', 'recovered'), process) %>%
  reduce(., full_join, by = c('province_state', 'country_region', 'lat', 'long', 'date')) %>%
  pivot_longer(cols = c('confirmed', 'confirmed_us', 'deaths', 'deaths_us', 'recovered'), names_to = 'status') %>%
  
  # Some countries had recovered data in Province/State column
  filter(province_state != 'Recovered') %>%
  
  # Some data was NA for countries, assumed this meant that 0 cases were reported
  mutate(value = ifelse(province_state != 'All' & !(status %in% c('confirmed_us', 'deaths_us')), replace_na(value, 0), value),
        status = str_remove(status, '_us')) %>%
  drop_na() %>%
  
  # The US datasets reported multiple counties per state, summarise to state level
  group_by(province_state, country_region, status, date) %>%
  summarise(total = sum(value)) %>%
  ungroup() %>%
  
  # Arrange by date within each geographic location and status 
  # Calculate the number of cases added since previous day (New) and average of the past 7 days (including current)
  group_by(province_state, country_region, status) %>%
  arrange(date) %>%
  mutate(new = total - lag(total, default = total[1]),
         rolling_7day = rolling_average(new, 7)) %>%
  ungroup() 
  

# Summarise data to country level totals
sum_data <- merged_data %>%
  # Remove countries that did not have province/state level data and were already labelled as All
  filter(province_state != 'All' & status != 'recovered') %>%
  group_by(country_region, status, date) %>%
  summarise(total = sum(total),
            new = sum(new),
            rolling_7day = sum(rolling_7day)) %>%
  mutate(province_state = 'All')
  
# Combine the datasets
combined_data <- bind_rows(merged_data, sum_data) %>%
  # Some geographic areas had two rows for each date
  # Discard the one with 0 value and any rows with NA
  group_by(province_state, country_region, status, date) %>%
  arrange(total) %>%
  slice(1) %>%
  ungroup() %>%
  drop_na() %>%
  
  # Remove the cruise ships from Province/State
  # Also had 'Recovered' in this column for some countries
  filter(!province_state %in% c('Diamond Princess', 'Grand Princess')) %>%
  
  # Convert character columns to factors
  # Relevel and arrange to have values of interest the ones shown at start up
  mutate_if(is.character, as.factor) %>%
  mutate(province_state = relevel(province_state, ref = 'All')) %>%
  group_by(country_region) %>%
  arrange(province_state) %>%
  ungroup() %>%
  mutate(country_region = relevel(country_region, ref = 'Canada')) %>%
  arrange(country_region) %>%
  mutate(status = relevel(status, ref = 'confirmed')) %>%
  arrange(status)

# Set up Shiny UI
ui <- fluidPage(
  
  # App title
  titlePanel("COVID Data"),
  
  # Sidebar layout with a input and output definitions 
  sidebarLayout(
    
    # Sidebar panel for inputs 
    sidebarPanel(
      
      # Input: Selectors for choosing data 
      selectInput('country', 'Country:',
                  label = 'Choose a country:',
                  choices = unique(combined_data$country_region)),
      
      uiOutput('province'),
      
      uiOutput('status'),
      
      # Add a Slider Input to select date range
      sliderInput("Date_range_selector", "Select Date Range",
                  min = min(combined_data$date),
                  max = max(combined_data$date),
                  value = c(min(combined_data$date), max(combined_data$date))),
      
      # Add options to look at Total or New 
      radioButtons(inputId='aggregate', label='What would you like to see?', 
                   choices=c('Total','New')),
      
      # Add options to change y-axis scale
      radioButtons(inputId='scale', label='Choose y-axis scale:', 
                   choices=c('Linear','Log'))
    ),
    
    # Main panel for displaying outputs 
    mainPanel(
      
      # Output: Bar graph of number of cases
      plotOutput(outputId = "Plot"),
      
    )
  )
)

# Set up Shiny server 
server <- function(input, output) {
  
  # Render reactive UI
  # After country is chosen this will show Province/State from that country only
  output$province <- renderUI({
    province <- combined_data %>%
      filter(country_region == input$country) %>%
      pull(province_state)
    selectInput('province', 'Province:',
                label = "Choose a Province/Region:",
                choices = province)
  })
  
  # Render reactive UI
  # After country and Province/State is chosen this will show the case breakdowns available for this region
  output$status <- renderUI({
    status <- combined_data %>%
      filter(country_region == input$country & province_state == input$province) %>%
      pull(status)
    selectInput('status', 'Status:',
                label = "Patient status:",
                choices = status)
  })
  
  # Make a reactive dataset to use for graphs
  # filters datset based off of all the inputs
  filtered_df <- reactive({
    combined_data %>%
      filter(status == input$status & country_region == input$country & province_state == input$province & 
             between(date, input$Date_range_selector[1], input$Date_range_selector[2]))
  })
  
  # Render bar graph for output
  # Change title for new inputs
  output$Plot <- renderPlot({
    
  # Return an empty plot with "Data not available" printed 
  # when selection was not in dataset
  if (sum(filtered_df()$Total) == 0){
    x_spot <- as.numeric((max(filtered_df()$date) - min(filtered_df()$date)) / 2)
    x_spot <- min(filtered_df()$date) + x_spot
    p <- ggplot(filtered_df(), aes(x = date, y = New)) +
          geom_col() +
          annotate('text', x = as.Date(x_spot), y = 0, label = 'Data not available', size = 12) +
          labs(x = 'Date',
               y = paste(input$aggregate),
               caption = 'Data from Johns Hopkins University Center',
               title = paste('Number of', input$aggregate, input$status, 'cases in', input$province, input$country, sep = ' '))
    return(p)
  } else {
  # If looking at new cases, plot 7 Day Rolling Average
  if (input$aggregate == 'New'){
      p <- ggplot(filtered_df(), aes(x = date, y = new)) +
      geom_col() +
      geom_path(aes(x = date, y = rolling_7day, color = input$aggregate)) +
      scale_color_discrete(name = '7 Day Rolling Average', labels = 'New') +
      theme_pubclean() +
      theme(legend.position = 'top') +
      labs(x = 'Date',
           caption = 'Data from Johns Hopkins University Center',
           title = paste('Number of', input$aggregate, input$status, 'cases in', input$province, input$country, sep = ' '))
  } else {
      p <- ggplot(filtered_df(), aes(x = date, y = total)) +
      geom_col() +
      theme_pubclean() +
      theme(legend.position = 'top') +
      labs(x = 'Date',
           caption = 'Data from Johns Hopkins University Center',
           title = paste('Number of', input$aggregate, input$status, 'cases in', input$province, input$country, sep = ' '))
  }}
  # Change y-axis scale 
  if (input$scale == 'Linear'){
    p <- p + 
      ylab(input$aggregate)
    return(p)
  } else {
    p <- p + 
      scale_y_log10(labels = function(x) format(x, scientific = FALSE)) +
      ylab(paste0('log(', input$aggregate, ')'))
    return(p)
  }
  })
}
  
shinyApp(ui = ui, server = server)
