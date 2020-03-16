#### ==== SET UP ====####

require(shiny)
require(shinyWidgets)
require(shinydashboard)
require(shinydashboardPlus)
require(httr)
require(jsonlite)
require(tidyverse)
require(plotly)
require(leaflet)
require(viridis)
require(DT)
require(sf)
require(RColorBrewer)

#### ==== GLOBAL ====####
country_polygons <- st_read("ne_50m_admin_0_countries.shp")
countries <- country_polygons %>% as.data.frame %>% 
  select(NAME, ISO_A3, POP_EST, GDP_MD_EST, INCOME_GRP, CONTINENT, REGION_UN,SUBREGION, REGION_WB) %>%
  mutate(
    NAME = recode(NAME, 
                  "Bosnia and Herz." = "Bosnia and Herzegovina",
                  "Czechia" = "Czech Republic",
                  "Faeroe Is." = "Faeroe Islands",
                  "Dem. Rep. Congo" = "Democratic Republic of Congo",
                   "Dominican Rep." = "Dominican Republic",
                  "United States of America" = "United States",
                  "St-Martin" = "Saint Martin (French part)",
                  
                  
                  )
  )

#### ==== UI ====####
ui <- dashboardPagePlus(

  dashboardHeaderPlus(
    title = 'Corona Stats',
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "question"
    ),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Global",
        tabName = "tab1",
        icon = icon("globe")
      ),
      menuItem(
        text = "Country Details",
        tabName = "tab2",
        icon = icon("flag")
      ),
      menuItem(
        text = "Data",
        tabName = "tab3",
        icon = icon("database")
      )
    )
  ),
  dashboardBody(

    #### ---- CSS ----####
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "buzzwink.css"),
      #### ---- JS ----####
      tags$script(src = "enter_button.js")
    ),
    tabItems(
      tabItem(
        tabName = "tab1",
        fluidRow(
        box(
          width = 12, status = "primary",
            leafletOutput("worldMap"),
           splitLayout( 
             plotlyOutput("total_cases_p"),
          plotlyOutput("total_deaths_p")
           ),
            infoBoxOutput("infobox_date", width = 3),
            infoBoxOutput("infobox_cases", width = 3),
            infoBoxOutput("infobox_deaths", width = 3),
            infoBoxOutput("infobox_deathrate", width = 3)
        )
      )


        ),
      tabItem(
        tabName = "tab2",
           fluidRow(     box(
          width = 2, status = "primary", 
          uiOutput("continent_select"),
          uiOutput("country_select"),
          prettyRadioButtons(inputId = "stats_type", label = "Show stats relative to outbreak:", choices = c("Yes", "No"),
   selected = "No",inline = TRUE, status = "danger", fill = TRUE ,bigger = TRUE,  icon = icon("check"), animation = "jelly"),
   br(),
   br(),
          actionBttn("configure", "Configure", style = "material-flat", color = "danger")
        ),
box(
          width = 10, status = "primary", 
splitLayout(

plotlyOutput("country_cases_p"),
plotlyOutput("country_deaths_p")
)


)),
box( width = 12, status = "primary",

     splitLayout(
        plotlyOutput("country_comparison_bar_cases"),
       plotlyOutput("country_comparison_bar_deaths"),
       plotlyOutput("country_comparison_bar_rates")
       ),
            infoBoxOutput("infobox_date_c", width = 3),
            infoBoxOutput("infobox_cases_c", width = 3),
            infoBoxOutput("infobox_deaths_c", width = 3),
            infoBoxOutput("infobox_deathrate_c", width = 3)
)

      ), 
      tabItem(
        tabName = "tab3",
        box( width = 12, status = "primary",
             dataTableOutput("latest_table")
             )
      )
    )
  ),
  rightSidebar(
    background = "light",
    rightSidebarTabContent(
        id = 1,
        icon = "info",
        active = TRUE,
        
              a(actionButton('mail',"Send feedback", width = '100%', icon = shiny::icon("envelope")), href='mailto:l_busswinkel@hotmail.de'),
      actionButton('github',"Check the code on Github",width = '100%', icon = shiny::icon("github"), onclick ="window.open('https://github.com/elalemano/ShinyCountries', '_blank')"),
        actionButton('twitter',"Follow me on Twitter",width = '100%', icon = shiny::icon("twitter"),  onclick ="window.open('https://twitter.com/LycopersiconLBB', '_blank')"), 
         actionButton('linked_in', 'Get in touch on LinkedIn',width = '100%',icon = shiny::icon("linkedin"), onclick ="window.open('https://linkedin.com/in/lukas-busswinkel', '_blank')")
        )
  ),
  dashboardFooter(left_text = "Created March 2020 by Lukas Busswinkel", right_text = "v0.1")
)

#### ==== SERVER ====####

server <- function(input, output, session){

####==== Initial info ====####
observe({
  sendSweetAlert(session, title = "Corona Stats", 
  text = tags$span(
        "This app visualizes the corona virus statistics found at:",
        tags$br(),
        tags$a("https://ourworldindata.org/coronavirus-source-data", href = "https://ourworldindata.org/coronavirus-source-data", target = "_blank"),
        tags$br(),
        "I really hope this app can be useful to people and provide much needed information. If you have any questions or feedback, dont hesitate to let me know! My contact info you find by clicking on the ", icon("question"),
        " icon in the top right corner"), btn_labels = "Thanks!",
  type = "info", closeOnClickOutside = TRUE, html = TRUE
  )
})
  
  
####==== Data ====####
  
pull_data  <- reactive({
####---- Base country info ----####

####---- Corona data ----####
dat_all <- read_csv("http://cowid.netlify.com/data/full_data.csv") %>%
  left_join(countries, by = c("location" = "NAME")) %>% filter(!is.na(CONTINENT))

return(dat_all)
})

filter_data_continent <- reactive({
  
  req(input$continent_select)
  
 out <-  pull_data() %>% filter(CONTINENT %in% input$continent_select)
 
 return(out)
})

filter_data_country <- eventReactive(input$configure,{
  
 out <-  filter_data_continent() %>% filter(location %in% input$country_select)
 
 return(out)
})

dat_latest <- reactive({
  out <- pull_data() %>% filter(date == max(date))
  return(out)
})

filter_data_country_100 <- reactive({
  out <- filter_data_country() %>% filter(total_cases >= 100) %>% 
  group_by(location) %>%
  mutate(Day = row_number())
  
  return(out)
})

dat_totals_global_time <- reactive({
  out <- pull_data() %>% group_by(date) %>%
    summarize(
      TotalDeaths = sum(total_deaths, na.rm = T),
      TotalCases = sum(total_cases, na.rm = T)
      )
  return(out)
})

dat_totals_country <- reactive({
  req(input$country_select)
  out <- dat_latest() %>% filter(location %in% input$country_select) %>% group_by(location, date) %>%
    summarize(
      TotalCases = sum(total_cases, na.rm = T),
      TotalDeaths = sum(total_deaths, na.rm = T))
  return(out)
})


####==== Plots ====####

output$total_deaths_p <- renderPlotly({
  
  total_deaths <- dat_totals_global_time()
  
  plot_ly(data= total_deaths, x = ~date, y = ~TotalDeaths, type = 'bar',marker = list(color = brewer.pal(8, "Accent")[7])) %>%
    layout(
      xaxis = list(title = "Date",  tickangle = 315),
      yaxis = list(title = "Fatalities")
    )
  
})

output$total_cases_p <- renderPlotly({
  
  total_cases <- dat_totals_global_time()
  
  plot_ly(data= total_cases, x = ~date, y = ~TotalCases, type = 'bar', marker = list(color = brewer.pal(8, "Accent")[5])) %>%
    layout(
      xaxis = list(title = "Date",  tickangle = 315),
      yaxis = list(title = "Infections")
    )
  
})



####==== Country level graphs ====####
country_trends_graph_cases <- eventReactive(input$configure,{
  
    if(input$stats_type == "Yes"){
      total_cases <-  filter_data_country_100()
    plot_ly(data= total_cases, x = ~Day, y = ~total_cases, type = 'scatter', mode = 'lines+markers', color = ~location, 
            colors = brewer.pal(length(unique(total_cases$location)), "Accent")) %>% 
    layout(
      title = "Total cases by day after 100 infections",
      xaxis = list(title = 'Day', tickangle = 315),
      yaxis = list(title = "Number of infections"))
      
  } else {
      total_cases <-  filter_data_country()
      
    plot_ly(data= total_cases, x = ~date, y = ~total_cases, type = 'scatter', mode = 'lines+markers', color = ~location, 
            colors = brewer.pal(length(unique(total_cases$location)), "Accent")) %>% 
    layout(
      title = "Total cases by date",
      xaxis = list(title = 'Date', tickangle = 315),
      yaxis = list(title = "Number of infections")
    ) 
  }
})

output$country_cases_p <- renderPlotly({
country_trends_graph_cases()
})

country_trends_graph_deaths <- eventReactive(input$configure,{
  
    if(input$stats_type == "Yes"){
      total_cases <-  filter_data_country_100()
    plot_ly(data= total_cases, x = ~Day, y = ~total_deaths, type = 'scatter', mode = 'lines+markers', color = ~location, 
            colors = brewer.pal(length(unique(total_cases$location)), "Accent")) %>% 
    layout(
      title = "Total fatalities by day after 100 infections",
      xaxis = list(title = 'Day', tickangle = 315),
      yaxis = list(title = "Number of fatalities"))
      
  } else {
      total_cases <-  filter_data_country()
      
    plot_ly(data= total_cases, x = ~date, y = ~total_deaths, type = 'scatter', mode = 'lines+markers', color = ~location, 
            colors = brewer.pal(length(unique(total_cases$location)), "Accent")) %>% 
    layout(
      title = "Total fatalities by date",
      xaxis = list(title = 'Date', tickangle = 315),
      yaxis = list(title = "Number of fatalities")
    ) 
  }
})

output$country_deaths_p <- renderPlotly({
country_trends_graph_deaths()
})

####=== Country level bar charts ====####
country_comparison_bar_deaths_f <- eventReactive(input$configure,{
  plot_data <- dat_totals_country()
  plot_ly(data = plot_data, x = ~TotalDeaths, y = ~location, type = 'bar', color = ~location, orientation = 'h',
         colors = brewer.pal(length(unique(plot_data$location)), "Accent")) %>% 
          layout(
            title = "Number of Fatalities",
            yaxis = list( title = ""),
            xaxis = list(title = "")
            )
})

country_comparison_bar_cases_f <- eventReactive(input$configure,{

  plot_data <- dat_totals_country()
  plot_ly(data = plot_data, x = ~TotalCases, y = ~location, type = 'bar', color = ~location, orientation = 'h',
         colors = brewer.pal(length(unique(plot_data$location)), "Accent")) %>% 
          layout(
            yaxis = list(title = ""),
            title = "Number of infections",
            xaxis = list(title = "")
            )
})

country_comparison_bar_rates_f <- eventReactive(input$configure,{

  plot_data <- dat_totals_country() %>% mutate(FatalityRate = (TotalDeaths/TotalCases))
  plot_ly(data = plot_data, x = ~FatalityRate, y = ~location, type = 'bar', color = ~location, orientation = 'h',
         colors = brewer.pal(length(unique(plot_data$location)), "Accent")) %>% 
          layout(
            title = "Fatality Rate (%)",
            xaxis = list(tickformat = "%", title = ""),
            yaxis = list(title = "")
            )
})

output$country_comparison_bar_deaths <- renderPlotly(country_comparison_bar_deaths_f())
output$country_comparison_bar_cases <- renderPlotly(country_comparison_bar_cases_f())
output$country_comparison_bar_rates <- renderPlotly(country_comparison_bar_rates_f())

#### ---- World Map ----####
output$worldMap <- renderLeaflet({
  
  pal <- colorQuantile(palette = "YlOrRd", domain = c(min(dat_latest()$total_cases, na.rm = T):max(dat_latest()$total_cases, na.rm = T)), n = 100)
  
  map_data <- country_polygons %>% 
    left_join(dat_latest() %>% select(ISO_A3, total_cases, total_deaths), by = c("ISO_A3" = "ISO_A3"))
  
leaflet(data =map_data) %>%
  addPolygons(color =  ~pal(total_cases),
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.5,
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE))  %>%
  setView(8.404363, 39.013003, 3)

})


####==== Tables ====####
output$latest_table <- renderDT({

    datatable(
      dat_latest(),
      filter = "top",
      class = "row-border stripe hover", rownames = F, style = "bootstrap",
      extensions = c("ColReorder", "Buttons", "FixedHeader"),
      options = list(
        autoWidth = TRUE,
        scrollX = TRUE, dom = "ltirpB", fixedHeader = TRUE,
        buttons = c("copy", "csv", "excel", "pdf", "print"),
        pageLength = 10,
        lengthMenu = list(c(5, 10, 15, 20, 50, -1), list("5", "10", "15", "20", "50", "All"))
      )
    )
  })


####==== Dynamic UI ====####

output$continent_select <- renderUI({

  pickerInput("continent_select", "Select a Continent:", choices = pull_data()$CONTINENT %>% unique %>% as.character %>% sort, 
              selected = pull_data()$CONTINENT %>% unique %>% as.character %>% sort, multiple = TRUE, 
        options = list(
        `actions-box` = TRUE,
        `deselect-all-text` = "None",
        `select-all-text` = "All",
        `none-selected-text` = "Select a continent",
        selectedTextFormat = "count > 2"
      )
      )
              
})

output$country_select <- renderUI({
  req(input$continent_select)

  
  choices_tab <- filter_data_continent() %>% select(CONTINENT, location) %>% unique %>% group_by(CONTINENT) %>% 
    mutate(country_choices = paste(location, collapse = ", ")) %>% select(-location) %>% unique %>% 
    mutate(country_choices = str_split(country_choices, ", "))

listed_choices <- as.data.frame(choices_tab)[,2]
names(listed_choices) <- choices_tab$CONTINENT
  
selectizeInput("country_select", "Select a Country:", choices = listed_choices, 
              selected =  NULL, multiple = TRUE, options = list(maxItems = 8))
})


### Tab 1 info boxes #####
output$infobox_date <- renderInfoBox({
  infoBox("Last Updated", value = dat_latest()$date %>% unique, color = 'blue', icon = icon('calendar'), fill = TRUE)
})

output$infobox_cases <- renderInfoBox({
  infoBox("Total Cases", value = dat_latest()$total_cases %>% sum(., na.rm = T), color = 'orange', icon = icon('hashtag'), fill = TRUE)
})

output$infobox_deaths <- renderInfoBox({
  infoBox("Total Deaths", value = dat_latest()$total_deaths %>% sum(., na.rm = T), color = 'red', icon = icon('cross'), fill = TRUE)
})

output$infobox_deathrate <- renderInfoBox({
  infoBox("Fatality rate", value = round((dat_latest()$total_deaths %>% sum(., na.rm = T)) / (dat_latest()$total_cases %>% sum(., na.rm = T)), 4) * 100, color = 'yellow', icon = icon('percent'), fill = TRUE)
})

### Tab 2 info boxes #####
output$infobox_date_c <- renderInfoBox({
  infoBox("Last Updated", value = dat_totals_country()$date %>% unique, color = 'blue', icon = icon('calendar'), fill = TRUE)
})

output$infobox_cases_c <- renderInfoBox({
  infoBox("Total Cases", value = dat_totals_country()$TotalCases %>% sum(., na.rm = T), color = 'orange', icon = icon('hashtag'), fill = TRUE)
})

output$infobox_deaths_c <- renderInfoBox({
  infoBox("Total Deaths", value = dat_totals_country()$TotalDeaths %>% sum(., na.rm = T), color = 'red', icon = icon('cross'), fill = TRUE)
})

output$infobox_deathrate_c <- renderInfoBox({
  infoBox("Fatality rate", value = round((dat_totals_country()$TotalDeaths %>% sum(., na.rm = T)) / (dat_totals_country()$TotalCases %>% sum(., na.rm = T)), 4) * 100, color = 'yellow', icon = icon('percent'), fill = TRUE)
})
}

#### ==== END ====####
shinyApp(ui, server)