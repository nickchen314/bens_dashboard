library(shiny)
library(tidyverse)
library(ggplot2)
library(bslib)
library(scales)
library(DT)
library(thematic)
library(shinyWidgets)
library(plotly)
thematic_shiny(font = "auto")
source("./.R/func_clean.R")

##sets standardized colors for plots
status_colors <-setNames(c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF"), 
                        c("Found", "New", "Returning", "Non-member"))

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "materia"),
  titlePanel("BENS Dynamic Dashboard - by Nicholas Chen"),
  fileInput("file1", "Upload Dashboard RE Export Here",
            accept = c("text/csv",
                       "text/comma-separated-values,
                       .csv")),
  mainPanel(
        sidebarLayout(
          sidebarPanel(
            uiOutput("year_select"),
            sliderTextInput(
              inputId = "month1",
              label = "Month Select",
              choices = c(1:12),
              selected =c(1, 12),
              grid = FALSE, dragRange = FALSE),
            uiOutput("region_select"),
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
                               selected = c("Found", "New", "Returning"),
                        multiple = TRUE, 
                        options = pickerOptions(
                          actionsBox = TRUE, 
                          size = 10, 
                          selectedTextFormat = "count > 3"
                        )
            ),
            downloadButton('downFile',label = "Download Table")
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
                "Line Plots",
                mainPanel(
                  plotlyOutput("lineplot_count"),
                  plotlyOutput("lineplot_region")
                )
              ), 
              tabPanel(
                "Club Level Summary",
                mainPanel(
                  dataTableOutput("summary_club"),
                  dataTableOutput("summary_club_gift")
                )
              ), 
              tabPanel(
                "Regional Summary",
                mainPanel(
                  dataTableOutput("summary_region"),
                  dataTableOutput("summary_region_gift")
                )
              ), 
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
  ## loads data in from file input section
  cleaned_data <- reactive({clean_data(read.csv(file = req(input$file1$datapath), fileEncoding="WINDOWS-1250"))})
  ## calls clean data function
  current_date <- reactive({max(cleaned_data()$Gf_Date)})
  ## creates values for year slider
  year_min <- reactive({min(cleaned_data()$g_year)})
  year_max <- reactive({max(cleaned_data()$g_year)})
  
  ## creates names dataframe to append to cleaned status 
  names_list <- reactive({cleaned_data() %>%
    select(Gf_CnBio_ID, Gf_CnBio_Name) %>%
    group_by(Gf_CnBio_ID, Gf_CnBio_Name) %>%
    summarize(n_gifts = n()) ->df1
    df1
    })

  ##dummy_df for status
  status_df1 <- reactive({cleaned_data() %>%
    group_by(g_year, Gf_CnBio_ID) %>%
    summarize(total_gift = sum(Gf_Amount)) %>%
    ungroup() %>%
    filter(total_gift >= 2500) %>%
    left_join(names_list(), by = join_by(Gf_CnBio_ID)) %>%
    mutate(comb_id = str_c(Gf_CnBio_ID, g_year)) %>%
    arrange(Gf_CnBio_ID, g_year) %>%
    mutate(prev = lag(Gf_CnBio_ID))-> df1
    df1})
  status_df <- reactive({status_df1() %>%
    mutate(status = as.factor(case_when(
      str_c(Gf_CnBio_ID, g_year - 1) %in% status_df1()$comb_id ~ "Returning",
      Gf_CnBio_ID == prev ~ "Found",
      .default = "New"
    ))) %>%
    select(g_year, Gf_CnBio_ID, status) -> df1
    df1})
  
  ## dummy_df for region
  region_df <- reactive({cleaned_data() %>%
      group_by(g_year, Gf_CnBio_ID) %>%
      arrange(g_year, Gf_CnBio_ID, Gf_Date) %>%
      mutate(Gf_Gift_code = last(Gf_Gift_code)) %>%
      group_by(g_year, Gf_CnBio_ID, Gf_Gift_code) %>%
      summarize()})
  
  ## creates final df
  grouped_df <- reactive({
    cleaned_data() %>%
      filter(g_year %in% c(input$year1[1]:input$year1[2]), 
                          g_month %in% c(input$month1[1]:input$month1[2])) %>%
      group_by(g_year, Gf_CnBio_ID) %>%
      summarize(total_gift = sum(Gf_Amount)) %>%
      left_join(names_list(), by = join_by(Gf_CnBio_ID))-> temp
    # appends club level
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
    # appends status
    with_status <- with_club %>%
      left_join(status_df(), by = join_by(Gf_CnBio_ID == Gf_CnBio_ID,
                                        g_year == g_year)) %>%
      mutate(status = as.factor(case_when(
        is.na(status) ~ "Non-member",
        club_level == "Non-member" ~ "Non-member",
        .default = status
      )))
    
    # appends region
    with_region <- with_status %>%
      left_join(region_df(), by = join_by(Gf_CnBio_ID == Gf_CnBio_ID,
                                          g_year == g_year)) 
   
    # filters by status, region, and club inputs
    final_df <- with_region %>%
      filter(status %in% input$status1) %>%
      filter(club_level %in% input$member1) %>% 
      filter(Gf_Gift_code %in% input$region1) 
    
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
      scale_x_continuous(breaks=seq(year_min(),year_max(),2)) +
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
                      mutate(status = fct_rev(fct_relevel(.f = status, "Returning"))) %>%
                      mutate(gift = scales::dollar_format()(round(tot_gift)))) +
      geom_bar(aes(x = g_year, y = tot_gift, fill = status, text = paste("Gift($):", gift)), stat = "identity") +
      labs(title = "BENS Annual Gift Amount by Status") +
      xlab("Year") +
      ylab("Total Gifts ($USD)") + 
      scale_x_continuous(breaks=seq(year_min(),year_max(),2)) +
      scale_y_continuous(labels = scales::dollar_format()) +
      theme(axis.title = element_text(face="bold"), 
            title = element_text(face="bold"),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      scale_fill_manual(values = status_colors)
    
    ggplotly(plot1, tooltip = c("g_year", "status", "text")) %>%
      layout(height = 400, width = 900)
  })
  
  ## download button
  output$downFile <- downloadHandler(
    filename = paste0("BENS_member_status_", current_date(), ".csv"),
    content = function(file) {
      write.csv(req(grouped_df()), file, row.names = FALSE)
    }
  )
  
  ## lineplots
  output$lineplot_region <- renderPlotly({
    plot1 <- ggplot(data = grouped_df() %>%
                      group_by(Gf_Gift_code, g_year) %>%
                      summarize(tot_gift = sum(total_gift)) %>%
                      mutate(gift = scales::dollar_format()(round(tot_gift)))) +
      geom_line(aes(x = g_year, y = tot_gift, color = Gf_Gift_code, label = gift)) + 
      labs(title = "Annual Gift Amount by Region") +
      labs(color='Region') +
      xlab("Year") +
      ylab("Total Gifts ($USD)") +
      scale_x_continuous(breaks=seq(year_min(),year_max())) +
      scale_y_continuous(labels = scales::dollar_format()) +
      theme(axis.title = element_text(face="bold"),
          title = element_text(face="bold"),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 
  
      ggplotly(plot1, tooltip = c("g_year", "Gf_Gift_code", "label")) %>%
        layout(height = 400, width = 900)
  })
  
  output$lineplot_count <- renderPlotly({
    plot1 <- ggplot(data = grouped_df() %>%
                      group_by(Gf_Gift_code, g_year) %>%
                      summarize(count = n())) +
      geom_line(aes(x = g_year, y = count, color = Gf_Gift_code)) + 
      labs(title = "Member Count by Region") +
      labs(color='Region') +
      xlab("Year") +
      ylab("Count of Members") +
      scale_x_continuous(breaks=seq(year_min(),year_max())) +
      theme(axis.title = element_text(face="bold"),
            title = element_text(face="bold"),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 
    
    ggplotly(plot1) %>%
      layout(height = 400, width = 900)
  })
  
  ## summary tables
  output$summary_club <- DT::renderDT({
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
                     "Non-member"))) %>%
        arrange(g_year) %>%
        pivot_wider(names_from = g_year, values_from = count) %>%
        arrange(factor(club_level))
        
    },
    options = list(dom='t',ordering=F)
  )

  output$summary_club_gift <- DT::renderDT({
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
    },
    options = list(dom='t',ordering=F)
  )
  
  ## regional summary tables
  output$summary_region_gift <- DT::renderDT({
    grouped_df() %>% 
      group_by(Gf_Gift_code, g_year) %>%
      summarize(t_gift = sum(total_gift))%>%
      arrange(g_year) %>%
      mutate(t_gift = scales::dollar(t_gift)) %>%
      pivot_wider(names_from = g_year, values_from = t_gift) 
  },
  options = list(dom='t',ordering=F)
  )
  
  output$summary_region <- DT::renderDT({
    grouped_df() %>% 
      group_by(Gf_Gift_code, g_year) %>%
      summarize(count = n()) %>%
      arrange(g_year) %>%
      pivot_wider(names_from = g_year, values_from = count) 
  },
  options = list(dom='t',ordering=F)
  )
  
  ## dynamic inputs
  output$year_select <- renderUI({sliderTextInput(
    inputId = "year1",
    label = "Year Select",
    choices = c(year_min():year_max()),
    selected =c(year_min(), year_max()),
    grid = FALSE, dragRange = FALSE)})
  
  output$region_select <- renderUI({pickerInput("region1", "Which Region(s)?", 
                                      choices = levels(cleaned_data()$Gf_Gift_code),
                                      selected = levels(cleaned_data()$Gf_Gift_code),
                                      multiple = TRUE, 
                                      options = pickerOptions(
                                        actionsBox = TRUE, 
                                        size = 10, 
                                        selectedTextFormat = "count > 3"))})
  
} # server

shinyApp(ui, server)
