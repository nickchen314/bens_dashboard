library(shiny)
library(tidyverse)
library(ggplot2)
library(bslib)
library(scales)
library(DT)
library(thematic)
library(shinyWidgets)
library(plotly)
library(pivottabler)
library(formattable)
thematic_shiny(font = "auto")

data <- read.csv(file = "data/reexport_nov82023.CSV", fileEncoding="WINDOWS-1250")
source("./.R/func_clean.R")
cleaned_data <- clean_data(data)
current_date <- max(cleaned_data$Gf_Date)
year_min <- min(cleaned_data$g_year)
year_max <- max(cleaned_data$g_year)
names_list <- cleaned_data %>%
  select(Gf_CnBio_ID, Gf_CnBio_Name) %>%
  group_by(Gf_CnBio_ID, Gf_CnBio_Name) %>%
  summarize(n_gifts = n())

##dummy_df for status
status_df1 <- cleaned_data %>%
  group_by(g_year, Gf_CnBio_ID) %>%
  summarize(total_gift = sum(Gf_Amount)) %>%
  ungroup() %>%
  filter(total_gift >= 2500) %>%
  left_join(names_list, by = join_by(Gf_CnBio_ID)) %>%
  mutate(comb_id = str_c(Gf_CnBio_ID, g_year)) %>%
  arrange(Gf_CnBio_ID, g_year) %>%
  mutate(prev = lag(Gf_CnBio_ID)) 
status_df <- status_df1 %>%
  mutate(status = as.factor(case_when(
    str_c(Gf_CnBio_ID, g_year - 1) %in% status_df1$comb_id ~ "Returning",
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
##FIXME need to account for members with multiple regions

##sets standardized colors for plots
status_colors <-setNames(c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF"), 
                        c("Found", "New", "Returning", "Non-member"))

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "materia"),
  titlePanel(str_c("BENS Dashboard as of ", current_date)),
  ##css for columns
  
  fileInput("file1", "PLACEHOLDER FILE UPLOAD",
            accept = c("text/csv",
                       "text/comma-separated-values,
                       .csv")),
  
  mainPanel(
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
            pickerInput("region1", "Which Region(s)?",
                               choices = levels(cleaned_data$Gf_Gift_code),
                               selected = levels(cleaned_data$Gf_Gift_code),
                        multiple = TRUE, 
                        options = pickerOptions(
                          actionsBox = TRUE, 
                          size = 10, 
                          selectedTextFormat = "count > 3"
                        )
            ), 
            pickerInput("member1", "Which Club Level?",
                               choices = c("Chairman's Club", 
                                           "Vice Chairman's Club",
                                           "Directors Club",
                                           "President's Club",
                                           "Enterprise Club",
                                           "Executives Club",
                                           "Investors Club",
                                           "Non-member"),
                               selected = c("Chairman's Club", 
                                            "Vice Chairman's Club",
                                            "Directors Club",
                                            "President's Club",
                                            "Enterprise Club",
                                            "Executives Club",
                                            "Investors Club",
                                            "Non-member"), 
                        multiple = TRUE, 
                        options = pickerOptions(
                          actionsBox = TRUE, 
                          size = 10, 
                          selectedTextFormat = "count > 3"
                        )
            ), 
            pickerInput("status1", "Which Status Level?",
                               choices = c("Found", "New", "Returning", "Non-member"),
                               selected = levels(status_df$status),
                        multiple = TRUE, 
                        options = pickerOptions(
                          actionsBox = TRUE, 
                          size = 10, 
                          selectedTextFormat = "count > 3"
                        )
            ),
            downloadButton('downFile',"Download Table")
          ), ## sidebar panel
          mainPanel(
            tabsetPanel(
              type = "pills",
              tabPanel(
                "Dashboard",
                mainPanel(
                  plotlyOutput("barplot"),
                  plotlyOutput("barplot2")
                )
              ), 
              tabPanel(
                "Summary Table",
                mainPanel(
                  dataTableOutput("summary_club"),
                  dataTableOutput("summary_club_gift")
                )
              ), 
              tabPanel(
                "Regional Summary",
                mainPanel(
                  dataTableOutput("summary_region")
                )
              ),#tab panel 3
              tabPanel(
                "Full Data",
                mainPanel(
                  dataTableOutput("full_data")
                )
              ) #tab panel 4
          )) #tabset panel first layer main panel
        )## sidebar layout
        
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
    with_status <- with_club %>%
      left_join(status_df, by = join_by(Gf_CnBio_ID == Gf_CnBio_ID,
                                        g_year == g_year)) %>%
      mutate(status = as.factor(case_when(
        is.na(status) ~ "Non-member",
        club_level == "Non-member" ~ "Non-member",
        .default = status
      )))
    ##filters by status and club inputs
    final_df <- with_status %>%
      filter(status %in% input$status1) %>%
      filter(club_level %in% input$member1)
    
    final_df
  })
  
  output$full_data <- DT::renderDT(
    {
      grouped_df()
    },
    filter = "top",
    options = list(pageLength = 20)
  )
  output$barplot <- renderPlotly({
    plot1 <- ggplot(data = grouped_df() %>%
                      mutate(status = fct_rev(fct_relevel(.f = status, "Returning")))) +
      geom_bar(aes(x = g_year, fill = status)) +
      labs(title = "BENS Member Count by Status") +
      xlab("Year") +
      ylab("Count of Members") + 
      scale_x_continuous(breaks=seq(year_min,year_max,2)) +
      theme(axis.title = element_text(face="bold"), 
            title = element_text(face="bold"),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      scale_fill_manual(values = status_colors)
    ggplotly(plot1) %>%
      layout(height = 400, width = 900)
  })
  output$barplot2 <- renderPlotly({
    plot1 <- ggplot(data = grouped_df() %>%
                      group_by(status, g_year) %>%
                      summarize(tot_gift = sum(total_gift)) %>%
                      mutate(status = fct_rev(fct_relevel(.f = status, "Returning")))) +
      geom_bar(aes(x = g_year, y = tot_gift, fill = status), stat = "identity") +
      labs(title = "BENS Annual Gift Amount by Status") +
      xlab("Year") +
      ylab("Total Gifts ($USD)") + 
      scale_x_continuous(breaks=seq(year_min,year_max,2)) +
      scale_y_continuous(labels = scales::dollar_format()) +
      theme(axis.title = element_text(face="bold"), 
            title = element_text(face="bold"),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      scale_fill_manual(values = status_colors)
    ggplotly(plot1) %>%
      layout(height = 400, width = 900)
  })
  
  output$downFile <- downloadHandler(
    filename = paste0("BENS_member_status_", current_date, ".csv") ,
    content = function(file) {
      write.csv(grouped_df(), file, row.names = FALSE)
    }
  )
  
  output$summary_club <- DT::renderDT(
    {
      grouped_df() %>% 
        group_by(club_level, g_year) %>%
        summarize(count = n()) %>%
        mutate(club_level = factor(
          club_level,
          levels = c("Chairman's Club", 
                     "Vice Chairman's Club",
                     "Directors Club",
                     "President's Club",
                     "Enterprise Club",
                     "Executives Club",
                     "Investors Club",
                     "Non-member")
                                  )) %>%
        arrange(g_year) %>%
        pivot_wider(names_from = g_year, values_from = count) %>%
        arrange(factor(club_level))
        
    },
    options = list(dom='t',ordering=F)
  )
  
  output$summary_club_gift <- DT::renderDT(
    {
      grouped_df() %>% 
        group_by(club_level, g_year) %>%
        summarize(t_gift = sum(total_gift))%>%
        mutate(club_level = factor(
          club_level,
          levels = c("Chairman's Club", 
                     "Vice Chairman's Club",
                     "Directors Club",
                     "President's Club",
                     "Enterprise Club",
                     "Executives Club",
                     "Investors Club",
                     "Non-member")
        )) %>%
        arrange(g_year) %>%
        mutate(t_gift = scales::dollar(t_gift)) %>%
        pivot_wider(names_from = g_year, values_from = t_gift) %>%
        arrange(factor(club_level))
    }
  )
  
  
} # server

shinyApp(ui, server)
