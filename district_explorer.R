library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(lubridate)
library(tidyverse)
library(gavir)


# replace root 
countdown_root <- file.path(set_root(), "CPMM", "Sandbox", "JL", "Countdown", "R version", "data")
# load in countdown dataset
countdown_in <- read_rds(file.path(countdown_root, "master_dataset.rds"))

countdown <- countdown_in %>%
  select(country, adminlevel_1, district, year, month, penta1, penta3, measles1, measles2, bcg, opv1, opv3) %>%
  pivot_longer(cols = c(penta1, penta3, measles1, measles2, bcg, opv1, opv3), names_to = "vaccine", values_to = "value") %>%
  mutate(date = make_date(year, month), 
         value = as.numeric(value))

# flag rows in the data which are greater than five standard deviations away from mean 
# make sure to group by district and vaccine

# Create a list of the unique districts
districts <- unique(countdown$district)

# Create a list of the unique vaccines
vaccines <- unique(countdown$vaccine)

# UI
ui <- fluidPage(
  titlePanel("Countdown Data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("district", "District", districts),
      selectInput("vaccine", "Vaccine", vaccines, selected = c("penta1", "penta3"), multiple = TRUE),
      sliderInput("date", "Date", 
                  min = min(countdown$date), max = max(countdown$date), 
                  value = c(min(countdown$date), max(countdown$date)), 
                  timeFormat = "%Y-%m")
    ),
    mainPanel(
      plotlyOutput("plot"),
      DTOutput("table"),
      DTOutput("missing_table")  # New table for missing/zero value percentages
    )
  )
)

# Server
server <- function(input, output) {
  
  # Missing/Zero Values Table
  output$missing_table <- renderDT({
    missing_data <- countdown %>%
      filter(district %in% input$district,
             vaccine %in% input$vaccine,
             date >= input$date[1] & date <= input$date[2]) %>%
      group_by(district, vaccine) %>%
      summarize(
        total = n(),
        missing = sum(is.na(value) | value == 0, na.rm = TRUE),
        pct = round((missing / total) * 100, 1)
      ) %>%
      mutate(indicator = paste0(vaccine, "_missing")) %>%
      ungroup() %>% 
      select(indicator,n = missing, pct) 
    
    outlier1 <- countdown %>%
      filter(district %in% input$district,
             vaccine %in% input$vaccine,
             date >= input$date[1] & date <= input$date[2]) %>%
      group_by(district, vaccine) %>%
      mutate(outlier = case_when(abs(value - mean(value, na.rm = TRUE)) > 5 * sd(value, na.rm = TRUE) ~ 1, T ~ 0))
    
    # add condition if there are no outliers
    if (nrow(outlier1) == 0) {
      outlier_plot <- data.frame()
    } else {
      outlier_plot <- outlier1 %>% filter(outlier == 1)
    }
    
    outliers <- outlier1 %>%
      group_by(district, vaccine) %>% 
      summarise(outlier = sum(outlier, na.rm = TRUE), 
                n = n()) %>%
      ungroup() %>% 
      mutate(pct = round((outlier/n) * 100, 1),
             indicator = paste0(vaccine, "_outliers")) %>%
      select(indicator, n = outlier, pct)
    
    
    # Conditionally compute % of districts where penta3 > penta1
    if (any(c("penta1", "penta3") %in% input$vaccine)) {
      neg_dropout <- countdown %>%
        filter(district %in% input$district, 
               date >= input$date[1] & date <= input$date[2]) %>%
        filter(vaccine %in% c("penta1", "penta3")) %>%
        group_by(district, month, year) %>% 
        mutate(neg_dropout = case_when(lag(value) < value ~ 1, TRUE ~ 0)) %>% 
        group_by(district) %>% 
        summarize(neg_dropout = sum(neg_dropout, na.rm = TRUE), 
                  n = n()) %>% 
        mutate(pct = round((neg_dropout/n) * 100, 1),
               indicator = "penta_negative_dropout") %>% 
        select(indicator,n = neg_dropout, pct)
      
      # Combine missing data and negative dropout data
      table <- bind_rows(missing_data, outliers, neg_dropout)
    } else {
      # Only use missing data if penta1 and penta3 are not selected
      table <- missing_data
    }
    
    datatable(table, 
              colnames = c("Indicator", "# months", "%"))
  })
  
  # Plot
  output$plot <- renderPlotly({
    filtered_data <- countdown %>%
      filter(district %in% input$district, 
             vaccine %in% input$vaccine, 
             date >= input$date[1] & date <= input$date[2])
    
    p <- ggplot(filtered_data, aes(x = date, y = value, color = vaccine)) +
      geom_line() +
      # geom_point(data = outlier_plot, aes(x = date, y = value), color = "red", size = 2) +
      labs(title = paste(input$district, paste(input$vaccine, collapse = ", ")),
           x = "Date",
           y = "Value") +
      scale_y_continuous(limits = c(0, NA)) +
      scale_color_gavi() +
      theme_minimal()
    
    ggplotly(p)
  })
  

  
}

# Run the app
shinyApp(ui = ui, server = server)
