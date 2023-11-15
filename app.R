library(shiny)
library(tidyverse)
library(ggplot2)
library(bslib)
library(scales)
library(DT)
library(thematic)
library(shinyWidgets)
thematic_shiny(font = "auto")

data <- read.csv(file = "./data/reexport_nov82023.csv")
source("./.R/func_clean.R")
cleaned_data <- clean_data(data)
current_date <- max(cleaned_data$Gf_Date)
year_min <- min(cleaned_data$g_year)
year_max <- max(cleaned_data$g_year)
names_list <- cleaned_data %>%
  select(Gf_CnBio_ID, Gf_CnBio_Name) %>%
  group_by(Gf_CnBio_ID, Gf_CnBio_Name) %>%
  summarize(n_gifts = n())
`%ni%` <- Negate(`%in%`)

##dummy_df for status
status_df <- cleaned_data %>%
  group_by(g_year, Gf_CnBio_ID) %>%
  summarize(total_gift = sum(Gf_Amount)) %>%
  ungroup() %>%
  filter(total_gift >= 2500) %>%
  left_join(names_list, by = join_by(Gf_CnBio_ID)) %>%
  mutate(comb_id = str_c(Gf_CnBio_ID, g_year)) %>%
  arrange(Gf_CnBio_ID, g_year) %>%
  mutate(prev = lag(Gf_CnBio_ID)) %>%
  mutate(status = as.factor(case_when(
    str_c(Gf_CnBio_ID, g_year - 1) %in% status_df$comb_id ~ "Returning",
    Gf_CnBio_ID == prev ~ "Found",
    .default = "New"
  ))) %>%
  select(g_year, Gf_CnBio_ID, status)

## dummy df for region
region_df <- cleaned_data %>%
  group_by(Gf_CnBio_ID, Gf_Gift_code, g_year) %>%
  summarize() %>%
  group_by(Gf_CnBio_ID, g_year) %>%
  summarize(count = n())
##need to account for members with multiple regions

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "quartz"),
  titlePanel(str_c("BENS Dashboard as of ", current_date)),
  mainPanel(
    tabsetPanel(
      type = "pills",
      tabPanel(
        "Full Data",
        sidebarLayout(
          sidebarPanel(
            sliderTextInput(
              inputId = "year1",
              label = "Year Select",
              choices = c(year_min:year_max),
              selected =c(year_min, year_max),
              grid = FALSE, dragRange = FALSE),
            sliderTextInput(
              inputId = "month1",
              label = "Month Select",
              choices = c(1:12),
              selected =c(1, 12),
              grid = FALSE, dragRange = FALSE),
            checkboxGroupInput("region1", "Which Region(s)",
                               choices = levels(cleaned_data$Gf_Gift_code),
                               selected = levels(cleaned_data$Gf_Gift_code)
            )
          ), ## sidebar panel
          mainPanel(
            dataTableOutput("full_data")
          )
        ) ## sidebar layout
        
      ),
      tabPanel(
        "Tab1",
        sidebarLayout(
          sidebarPanel(
            
          ), ## sidebar panel
          mainPanel(
            
          )
        ) ## sidebar layout
      ), ## tab1 panel
      
      tabPanel(
        "Tab2",
        sidebarLayout(
          sidebarPanel(
           
          ), ## sidebar panel
          mainPanel(
            
          ) ## main panel
        ) ## sidebar layout
      ), ## bivariate tab
      tabPanel(
        "Tab 3"
      )
    )
  )
) ## fluid page


server <- function(input, output, session) {
  grouped_df <- reactive({
    cleaned_data %>%
      filter(g_year %in% c(input$year1[1]:input$year1[2]), 
                          g_month %in% c(input$month1[1]:input$month1[2])) %>%
      filter(Gf_Gift_code %in% input$region1) %>%
      group_by(g_year, Gf_CnBio_ID) %>%
      summarize(total_gift = sum(Gf_Amount)) %>%
      left_join(names_list, by = join_by(Gf_CnBio_ID))-> temp
    ## appends club level
    temp %>%
      mutate(club_level = as.factor(case_when(
        total_gift >= 100000 ~ "Chairman's Club",
        total_gift >= 70000 ~ "Vice Chairman's Club",
        total_gift >= 40000 ~ "Directors Club",
        total_gift >= 20000 ~ "President's Club",
        total_gift >= 9000 ~ "Enterprise Club",
        total_gift >= 4000 ~ "Executives Club",
        total_gift >= 2500 ~ "Investors Club",
        total_gift < 2500 ~ "Non-member"
      ))) -> with_club
    ##appends status
    final_df <- with_club %>%
      left_join(status_df, by = join_by(Gf_CnBio_ID == Gf_CnBio_ID,
                                        g_year == g_year)) %>%
      mutate(status = as.factor(case_when(
        is.na(status) ~ "Non-member",
        .default = status
      )))

    final_df
  })
  
  output$full_data <- DT::renderDT(
    {
      grouped_df()
    },
    filter = "top",
    options = list(pageLength = 20)
  )

} # server

shinyApp(ui, server)
