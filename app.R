# Body 

library(shiny)
library(shinydashboard)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(DT)
library(plotly)
library(shinycssloaders)
library(ggcorrplot)


# Import data
df <- load('C:/Users/Pierre-AntoineSALISB/OneDrive - OCSI GROUP/Bureau/DSTI_courses/R/AirBnB.Rdata')

# Data cleaning
# Remove $ sign and convert Price to integer
L$price <- gsub("\\$","",L$price)
L$price <- as.integer(L$price)
#  Rename listing_id as id in R DF
colnames(R)[colnames(R) == 'listing_id'] <- 'id'
# Add date from R DF to L DF with keeping all L
L<- left_join(L,R, by= 'id')
# Extract year in a new column
L$year <- substr(L$date, 1,4)
# Convert columns to numeric for corr mat
L$price <- as.numeric(L$price)
#L$room_type <- as.numeric(L$room_type)
L$number_of_reviews <- as.numeric(L$number_of_reviews)
L$availability_365 <- as.numeric(L$availability_365)
L$accommodates <- as.numeric(L$accommodates)


# Choices for selectinput 
c1 = L %>%
  select( - year, - date, - zipcode, - listing_url, - scrape_id, - last_scraped, - name, - summary, - space, - description, - experiences_offered, - neighborhood_overview, - notes, - transit, - access, - interaction, - house_rules, - thumbnail_url, - medium_url, - picture_url, - xl_picture_url, - host_id, - host_url, - host_name, - host_since, - host_location, - host_about, - host_response_time, - host_response_rate, - host_acceptance_rate, - host_is_superhost, - host_thumbnail_url, - host_picture_url, - host_neighbourhood, - host_listings_count, - host_total_listings_count, - host_verifications, - host_has_profile_pic, - host_identity_verified, - street, - neighbourhood, - neighbourhood_cleansed, - neighbourhood_group_cleansed, - city, - state, - market, - smart_location, - country_code, - country, - latitude, - longitude, - is_location_exact, - bathrooms, - bedrooms, - beds, - bed_type, - amenities, - square_feet, - weekly_price, - monthly_price, - security_deposit, - cleaning_fee, - guests_included, - extra_people, - minimum_nights, - maximum_nights, - calendar_updated, - has_availability, - availability_30, - availability_60, - availability_90, - calendar_last_scraped, - first_review, - last_review, - review_scores_rating, - review_scores_accuracy, - review_scores_cleanliness, - review_scores_checkin, - review_scores_communication, - review_scores_location, - review_scores_value, - property_type, -reviews_per_month, - calculated_host_listings_count, - require_guest_phone_verification, - require_guest_profile_picture, - cancellation_policy, - instant_bookable, - jurisdiction_names, - license, - requires_license) %>%
  names()

# Subset of number of appartment per owner
owner = L %>%
  group_by(host_id) %>%
  summarise(nb = n())

# Renting price per city quarter
# Don't forget to include 75116 since 75116 represents 16th North and 75016 represents 16th South
quarter_price <- L %>% 
  filter(zipcode == 75001:75116) %>%
  group_by(zipcode) %>% summarise(Mean_Price = mean(price))
mpa <- quarter_price[with(quarter_price,order(-Mean_Price)),] 
mpa <- mpa[1:21,]

###


ui <- fluidPage(
  
  dashboardPage(
    dashboardHeader(title = "Shiny App : R Project on AirBNB Dataset",
                    titleWidth = 650,
                    tags$li(class="dropdown",tags$a(href="https://github.com/SalisburyPierre-Antoine", icon("github"), "Source Code", target="_blank"))
                    
    ),
    dashboardSidebar(
      # sidbarmenu
      sidebarMenu(
        id = "sidebar",
        
        # first menuitem
        menuItem("Dataset", tabName = "data", icon = icon("airbnb")),
        menuItem(text = "visualization", tabName = "viz", icon = icon("chart-line")),
        selectInput(inputId = "var1", label = "select the variable" , choices = c1, selected = "price"),
        selectInput(inputId = "var3", label = "select the x variable" , choices = c1, selected = "accommodates"),
        selectInput(inputId = "var1", label = "select the y variable" , choices = c1, selected = "price")
      )
    ),
    dashboardBody(
      tags$style(HTML(".sidebar-menu li a { font-size: 20px; }")),
      tags$head(tags$style(HTML('
            .skin-blue .sidebar-menu > li.active > a {
            color:#ff3333; background-color:#ffffff
            },
            .skin-black .main-sidebar ELEMENT {
            color: #ffccdd;}
            .skin-blue .sidebar-menu > li:hover > a {font-color : #ffdd00;
              border-left-color: #ff0000 ; background-color:#ffffff;
            }'
      ))),
      tabItems(
        # first tab item
        tabItem(tabName = "data",
                # tab box
                tabBox(id="t1", width = 12,
                       tabPanel("About", icon=icon("house"),
                                
                                
                                fluidRow(
                                  column(width = 12, tags$br() ,
                                         tags$strong("This shiny application contains :")),
                                  column(width = 12, tags$br() ,
                                         tags$p(". Relationship between prices and appartement features")),
                                  column(width = 12, tags$br() ,
                                         tags$p(". Number of appartement per owners")),
                                  column(width = 12, tags$br() ,
                                         tags$p(". Renting price per city quarter (arrondissements)")),
                                  column(width = 12, tags$br() ,
                                         tags$p(". Reviews frequency according to years")))
                                
                                
                       ),
                       tabPanel("Structure", icon=icon("database"),verbatimTextOutput("structure")),
                       tabPanel("Summary Stats", icon=icon("chart-column"),verbatimTextOutput("summary")))
        ),
        # second tab item 
        tabItem(tabName = "viz",
                # tab box
                tabBox(id="t2",width = 12,
                       tabPanel(title = "Distribution", value = "distro", withSpinner(plotlyOutput("histplot"))),
                       tabPanel(title = "Relationship",
                                radioButtons(inputId ="fit" , label = "Select smooth method" , choices = c("loess", "lm"), selected = "lm" , inline = TRUE), 
                                withSpinner(plotlyOutput("plotpa3")), value="relation"),
                       tabPanel(title = "Box plot", value ="trends",withSpinner(plotlyOutput("plotpa4"))),
                       tabPanel(title = "Histogram1", value ="trends",withSpinner(plotlyOutput("plotpa5"))),
                       tabPanel(title = "Histogram2", value ="trends",withSpinner(plotlyOutput("plotpa6"))),
                       tabPanel("Correlation Matrix", id="corr" , withSpinner(plotlyOutput("cor"))),
                       tabPanel(title = "Date",value ="trends",withSpinner(plotlyOutput("plotpa7")))))
      
      
      
    )
    
    
  )))


server <- function (input,output) {
  
  
  # Structure
  output$structure <- renderPrint(
    
    L %>%
      str()
  )
  
  # Summary
  output$summary <- renderPrint(
    
    L %>%
      summary()
  
  )
  
  # Stacked histogram and boxplot
  output$histplot <- renderPlotly({
    
    
    # histogram
    p1 = L %>%
      plot_ly()%>%
      add_histogram(~get(input$var1))%>%
      layout(xaxis = list(title = input$var1))
    
    # Boxplot
    p2 = L %>%
      plot_ly() %>%
      add_boxplot(~get(input$var1)) %>%
      layout(yaxis = list(showticklabels = F))
    
    # stacking plots
    subplot(p2, p1, nrows = 2) %>%
      hide_legend() %>%
      layout(title= "Distribution chart - Histogram and Boxplot",
             yaxis = list(title = "Frequency"))
  })
    
  
  
  #GGPLOT3
  output$plotpa3 <- renderPlotly({
  
  pa3 <- L %>%
    ggplot(aes(x = get(input$var3),y = get(input$var1)))+ 
    geom_point(color="Red", alpha=0.1)+
    geom_smooth(method = get(input$fit),
                formula = y~x) +
    labs(title = paste("Relation b/w", input$var3 , "and" , input$var1),
         x = input$var3,
         y = input$var1) 

  
  ggplotly(pa3)
  
  })
  
  #GGPLOT4
  output$plotpa4 <- renderPlotly({
  
 pa4 <- L %>%
    ggplot(aes(x = room_type, y = price)) +
    geom_boxplot(aes(fill = room_type)) + scale_y_log10() +
    xlab("Room type") + 
    ylab("Price") +
    ggtitle("Price by room type",
            subtitle = "Highest avg price for entire property")
 
 ggplotly(pa4)
  
})
  
  #GGPLOT5
  output$plotpa5 <- renderPlotly({
    
    pa5 <- owner %>%
      ggplot(aes(x=nb)) +
      geom_histogram(col = "#1b98e0", binwidth = 1)+ 
      scale_y_log10() +
      xlab("Number of properties") + 
      ylab("Count of host") +
      ggtitle("Number of appertements per owner",
              subtitle = "Most of owners rent one appartement")
    
 ggplotly(pa5)
    
  })
  
  
#GGPLOT6
output$plotpa6 <- renderPlotly({
  
  pa6 <- mpa %>%
    ggplot(aes(x = reorder(zipcode, -Mean_Price), y = Mean_Price)) + 
    geom_bar(stat="identity",colour="black", fill = "tomato3") + 
    labs(title="Average Price of Rooms in each arrondisssement", subtitle = "From highest to lowest") + 
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5), legend.position = c(0.8, 0.5), axis.text.x = element_text(angle = 90)) + xlab("") + ylab("Mean Price")
  
  ggplotly(pa6)
  
})

#GGPLOT7
output$plotpa7 <- renderPlotly({
  
  pa7 <- L %>%
    ggplot(aes(x = year, y = number_of_reviews)) +
    geom_area(fill="lightblue", color="black") +
    labs(title = "Number of reviews evolution",
         x = "year",
         y = "number of reviews")
  
  ggplotly(pa7)
  
})


## Correlation plot
output$cor <- renderPlotly({
  LC <- L %>% 
    select( - year, - date, -room_type, - zipcode, - listing_url, - scrape_id, - last_scraped, - name, - summary, - space, - description, - experiences_offered, - neighborhood_overview, - notes, - transit, - access, - interaction, - house_rules, - thumbnail_url, - medium_url, - picture_url, - xl_picture_url, - host_id, - host_url, - host_name, - host_since, - host_location, - host_about, - host_response_time, - host_response_rate, - host_acceptance_rate, - host_is_superhost, - host_thumbnail_url, - host_picture_url, - host_neighbourhood, - host_listings_count, - host_total_listings_count, - host_verifications, - host_has_profile_pic, - host_identity_verified, - street, - neighbourhood, - neighbourhood_cleansed, - neighbourhood_group_cleansed, - city, - state, - market, - smart_location, - country_code, - country, - latitude, - longitude, - is_location_exact, - bathrooms, - bedrooms, - beds, - bed_type, - amenities, - square_feet, - weekly_price, - monthly_price, - security_deposit, - cleaning_fee, - guests_included, - extra_people, - minimum_nights, - maximum_nights, - calendar_updated, - has_availability, - availability_30, - availability_60, - availability_90, - calendar_last_scraped, - first_review, - last_review, - review_scores_rating, - review_scores_accuracy, - review_scores_cleanliness, - review_scores_checkin, - review_scores_communication, - review_scores_location, - review_scores_value, - property_type, -reviews_per_month, - calculated_host_listings_count, - require_guest_phone_verification, - require_guest_profile_picture, - cancellation_policy, - instant_bookable, - jurisdiction_names, - license, - requires_license)
    
  
  # Compute a correlation matrix
  corr <- round(cor(LC,use="complete.obs"), 1)
  
  # Compute a matrix of correlation p-values
  p.mat <- cor_pmat(LC)
  
  corr.plot <- ggcorrplot(
    corr, 
    hc.order = TRUE, 
    lab= TRUE,
    outline.col = "white",
    p.mat = p.mat
  )
  
  ggplotly(corr.plot)
  
 })

  }
  
shinyApp(ui = ui, server = server)






