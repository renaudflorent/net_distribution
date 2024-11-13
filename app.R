#this script is about to describe the evolution of population after survey operation
#the main goal is to determine the evolution of people and determine if the net will be sufficient for all the malagasy people
#the data is integrate in a REMOTE POSTGRESQL (RENDER)
#table name 'authentification', it means how evolve the population after the projection or the prevision.


#if the net will not sufficient the we should propose scenario that partner will make a vote to decide which is more suitable.

#LIB importation

library(shiny)
library(bslib)
library (ggplot2)
library(RPostgres)
library(tidyr)
library(dplyr)

con <- dbConnect(
  RPostgres::Postgres(),
  dbname= 'mydatabase_name',
  user= 'renaudflorent',
  password= 'YxWck3VSZrx9zNz9X1rxa1sMY2DA4xqf',
  host= 'dpg-csket5ij1k6c73c2kdig-a.oregon-postgres.render.com',
  port= '5432'
)
#importing data into data frame
vote_result <- dbGetQuery(con, "SELECT * FROM vote")



data<-read.csv("./database.csv",sep = ";")

population<- sum(data$net_prvision, na.rm = TRUE)


#DATA WRANGLING
#let calculate the demande and 
tcd<-data|>
  group_by(District)|>
  summarise(
    prevision=sum(net_prvision, na.rm = TRUE),
    demand=sum(net_demande,na.rm = TRUE)
    )|>
  mutate(pourentil=(demand-prevision)/prevision)

tcd_population<-data|>
  group_by(Region)|>
  summarise(
    prevision=sum(populatoin_projection, na.rm = TRUE),
    population_real=sum(Population_survey,na.rm = TRUE)
  )|>
  mutate(pourentil=(population_real-prevision)/prevision)




result<-t.test(data$populatoin_projection, data$Population_survey,paired = TRUE, conf.level = 0.95)

#conclusion
if (result$p.value < 0.05) {
  conclusion <- "\n Reject the null hypothesis: There is a \n significant difference between the population projection and \n obeserved  values."
} else {
  conclusion <- "\n Fail to reject the null hypothesis:  There is \n no significant difference between the population  \n projection  and obeserved values."
}

print(conclusion)


#SHINY APP

ui <- page_fluid(
  title = "Net Distribution",
  includeCSS("www/styles.css"),
  layout_columns(
    card(
      #MINI DASHBOARD
      card_header("Mini Dashboad"),
      layout_columns(
        col_widths = c(4, 8),
        
        div(
          div(
            selectizeInput(
              inputId = "selected_item_region",      
              label = "Select Region:",  
              choices = data$Region,
              selected = NULL,                
              options = list(placeholder = 'Type to search...')
            ),
          ),
          
          div(
            actionButton("reInit","Clear"),
          ),
          
        ),
        div(
          clsss="card",
          verbatimTextOutput("tesResult")
        )
        
      ),


      layout_column_wrap(
        width = 1/3,
        div(
          style="margin-top:20px",
          div(
            verbatimTextOutput("population")
            ),
          div(
            plotOutput("donnatPopulation")
          )

        ),

        div(
          style="margin-top:20px",
          div(
            verbatimTextOutput("household")
          ),
          div(

            plotOutput("donnatHousehold")
          )
        ),
        div(
          style="margin-top:20px",
          div(
            verbatimTextOutput("Mosquito_net")
          ),
          div(
            
            plotOutput("donnatNet")
          )
        )


      ),
      width= 300
      
    ),
    card(
      card_header("Graph"),
      div(
        class = "card",
        style = "height: 500px;",  
        div(
          class = "card-body",
          h4(class = "card-title", "Graph"),
          plotOutput("hist")
        )
      )
    ),
    card(
      card_header("Scenario simulator"),
      
      selectInput("params",label = "Parameter:", choices = list("Polulation growth rate"=1,"Net demande growth rate"=2,"Both")),
      sliderInput("rate","Rate", min = 0.00, value=12.0,max = 20,200, step = 0.01),
      layout_column_wrap(
        
        div(
          style = "height: 50px;",  
          div(class = "miniCard",
              textAreaInput("C1","Net ->HH (1 to 2 personn",value=1),
          )
        ),
        div(
          style = "height: 50px;",  
          div(class = "miniCard",
              textAreaInput("C2","Net ->HH (3 to 4 personn",value=2),
          )
        ),
        div(
          style = "height: 50px;",  
          div(class = "miniCard",
              textAreaInput("C3","Net ->HH (5 to 6 personn",value=3),
          )
        ),
        div(
          style = "height: 50px",  
          div(class = "miniCard",
              textAreaInput("C4","Net ->HH (+7 personn",value=4),
          )
        ),
        
      ),
      layout_column_wrap(
        div(
          style="margin-top:25px;display:flex-box;justify-content:space-between",
          div(
            verbatimTextOutput("net_updated")
          ),
          div(
            verbatimTextOutput("net_gap")
          ),
          width = 300
        )
        
      ),
      div(
        plotOutput("coverage")
      ),
      
    ),
    
    card(
      card_header("Voting system"),
      layout_column_wrap(
        card(
          textAreaInput("name",label = "Name"),
          textAreaInput("email",label = "E-mail"),
          selectInput("scenario",label = "CHoose the scenario which is more appropriate to you",choices = list("Scenario 1"="A","Scenario 2"="B","Scenario 3"="C")),
          div(
            styles="height:100px, width:200px",
            verbatimTextOutput("desciption"),
          ),
          
          textAreaInput("reason", label="Tell us why"),
          actionButton("submit","Submit")
        ),
        card(
     
          plotOutput("pie")
          
        ),
      ),
      
      width="300px"
      
      
    ),
    col_widths = c(6, 6),
  )
)



server <- function(input, output, session) {
  des=""
  population <- reactiveVal(sum(data$Population_survey, na.rm = TRUE))
  household <- reactiveVal(sum(data$Household_survey, na.rm = TRUE))
  netDemand <- reactiveVal(sum(data$net_demande, na.rm = TRUE))
  netDemand_update <- reactiveVal(sum(df$net_demande, na.rm = TRUE))
  net_gap<-reactiveVal(sum(df$newnet,na.rm = TRUE))
  des_val<-reactiveVal(des)
  
  output$population <- renderText({ paste("Population:", format(population(), big.mark = " ", scientific = FALSE)) })
  output$household <- renderText({ paste("Household:", format(household(),big.mark=" ",scientific = FALSE)) })
  output$Mosquito_net <- renderText({ paste("Net Demand:", format(netDemand(),big.mark=" ",scientific = FALSE)) })
  output$net_updated <- renderText({ paste("Net Demand Update:", format(netDemand_update(),big.mark=" ",scientific = FALSE)) })
  output$net_gap <- renderText({ paste("GAP/Surplus:", format(sum(data$net_prvision,na.rm = TRUE)-net_gap(),big.mark=" ",scientific = FALSE)) })
  output$desciption <- renderText({ paste("Description:",des_val(), collapse = "\n")})
  
  
  observeEvent(input$scenario ,{
    print(input$scenario)
    if(input$scenario=="A"){
      des="\n Population grouth rate more than \n 4% (Rate>4%) => Net for type of HH: 1,2,2,3"
      des_val(des)
    }else if(input$scenario=="B"){
      des="\n Net demande grouth rate more than \n 14%  (Rate>14%) => Net for type of \n HH: 1,2,2,3"
      des_val(des)
    }else if (input$scenario=="C"){
      des="\n Net demande grouth rate and Population \n grouth rate more than 4% (Rate>4%) => \n Net for type of  HH: 1,2,2,3"
      des_val(des)
    }
    print(des)
  })
  
  output$tesResult <-  renderText({ paste("T-Test CONCLUSION:",conclusion ,collapse = "\n")})
  
  observeEvent(input$reInit, {
    updateSelectizeInput(session, "selected_item_region", selected = "")
    new_population <- sum(data$Population_survey, na.rm = TRUE)
    new_hh <- sum(data$Household_survey, na.rm = TRUE)
    new_netDemand <- sum(data$net_demande, na.rm = TRUE)
  
  })
  
  observeEvent(input$selected_item_region, {
    selected_region <- input$selected_item_region
    if (is.null(selected_region) || selected_region == ""){
      new_population <- sum(data$Population_survey, na.rm = TRUE)
      new_hh <- sum(data$Household_survey, na.rm = TRUE)
      new_netDemand <- sum(data$net_demande, na.rm = TRUE)
      
    } else{
      new_population <- sum(data$Population_survey[data$Region == selected_region], na.rm = TRUE)
      new_hh <- sum(data$Household_survey[data$Region == selected_region], na.rm = TRUE)
      new_netDemand <- sum(data$net_demande[data$Region == selected_region], na.rm = TRUE)
    }
    
    # Update population based on the selected region
    
    population(new_population)  # Update the reactive value
    household(new_hh)
    netDemand(new_netDemand)
    
  
  })
  
  output$donnatPopulation <- renderPlot({
    selected_region <- input$selected_item_region
    
    # Filter data based on the selected region or take all data if null
    if (is.null(selected_region) || selected_region == "") {
      prevision <- sum(data$populatoin_projection, na.rm = TRUE)
      population_real <- sum(data$Population_survey, na.rm = TRUE)
    } else {
      region_data <- data %>% filter(Region == selected_region)
      prevision <- sum(region_data$populatoin_projection, na.rm = TRUE)
      population_real <- sum(region_data$Population_survey, na.rm = TRUE)
    }
    
    # Calculate percentage change
    if (prevision == 0) {
      pourcentil <- 0
    } else {
      pourcentil <- ((population_real - prevision) / prevision) * 100
    }
    
    # Print the calculated percentage for debugging
    print(paste("Selected Region Value: ", selected_region))
    print(paste("Calculated pourcentil: ", pourcentil))
    
    # Prepare data for the donut chart
    data_pop_donnat <- data.frame(
      category = c("Filled", "Empty"),
      value = c(pourcentil, 100 - pourcentil)
    )
    
    # Plot the donut chart
    ggplot(data_pop_donnat, aes(x = 2, y = value, fill = category)) +
      geom_col(width = 1, color = "white") +
      coord_polar(theta = "y") +
      xlim(0.5, 2.5) +  # Create a hole for the donut
      scale_fill_manual(values = c("Filled" = "#87CEEB", "Empty" = "#F0F0F0")) +  # Custom colors
      theme_void() +  # Remove axes and gridlines
      theme(legend.position = "none") +  # Remove legend
      # Add text in the center with the calculated percentage
      annotate(
        "text", x = 0.5, y = 0, label = ifelse(is.finite(pourcentil), paste0(round(pourcentil, 1), "%"), "N/A"),
        size = 12, color = "#000000", fontface = "bold"
      )
  })
  
  output$donnatHousehold <- renderPlot({
    selected_region <- input$selected_item_region
    
    # Filter data based on the selected region or take all data if null
    if (is.null(selected_region) || selected_region == "") {
      prevision <- sum(data$household_number_projection, na.rm = TRUE)
      household_real <- sum(data$Household_survey, na.rm = TRUE)
    } else {
      region_data <- data %>% filter(Region == selected_region)
      prevision <- sum(region_data$household_number_projection, na.rm = TRUE)
      household_real <- sum(region_data$Household_survey, na.rm = TRUE)
    }
    
    # Calculate percentage change
    if (prevision == 0) {
      pourcentil <- 0
    } else {
      pourcentil <- ((household_real - prevision) / prevision) * 100
    }
    

    # Prepare data for the donut chart
    data_pop_donnat <- data.frame(
      category = c("Filled", "Empty"),
      value = c(pourcentil, 100 - pourcentil)
    )
    
    # Plot the donut chart
    ggplot(data_pop_donnat, aes(x = 2, y = value, fill = category)) +
      geom_col(width = 1, color = "white") +
      coord_polar(theta = "y") +
      xlim(0.5, 2.5) +  # Create a hole for the donut
      scale_fill_manual(values = c("Filled" = "#87CEEB", "Empty" = "#F0F0F0")) +  # Custom colors
      theme_void() +  # Remove axes and gridlines
      theme(legend.position = "none") +  # Remove legend
      # Add text in the center with the calculated percentage
      annotate(
        "text", x = 0.5, y = 0, label = ifelse(is.finite(pourcentil), paste0(round(pourcentil, 1), "%"), "N/A"),
        size = 12, color = "#000000", fontface = "bold"
      )
  })
  
  
  observeEvent(input$rate,{
    #DATA WRANGLING
    df<-data|>
      group_by(District)|>
      summarise(
        population_prev=sum(populatoin_projection,na.rm = TRUE),
        population_real=sum(Population_survey,na.rm = TRUE),
        net_prev=sum(net_prvision,na.rm = TRUE),
        net_real=sum(net_demande,na.rm = TRUE),
        cat1=sum(Cat.1,na.rm = TRUE),
        cat2=sum(Cat.2,na.rm = TRUE),
        cat3=sum(Cat.3,na.rm = TRUE),
        cat4=sum(Cat.4,na.rm = TRUE)
        )|>
      mutate(
            param1=((population_real-population_prev)/population_prev)*100,
            param2=((net_real-net_prev)/net_prev)*100
            
       )
      
    param <- input$params
    if(param==1){
      #Let now caculate net need base on cat value
      c1<-as.numeric(input$C1)
      c2<-as.numeric(input$C2)
      c3<-as.numeric(input$C3)
      c4<-as.numeric(input$C4)
      df <- df |>
        mutate(
          newnet = case_when(
            param1 > input$rate ~ (c1 * cat1 + c2 * cat2 + c3 * cat3 + c4 * cat4),
            TRUE ~ net_real
          )
        )
      netDemand_update(sum(df$newnet))
      net_gap(sum(df$newnet))
      
    }else if (param==2){
      c1<-as.numeric(input$C1)
      c2<-as.numeric(input$C2)
      c3<-as.numeric(input$C3)
      c4<-as.numeric(input$C4)
      df <- df |>
        mutate(
          newnet = case_when(
            param2 > input$rate ~ (c1 * cat1 + c2 * cat2 + c3 * cat3 + c4 * cat4),
            TRUE ~ net_real
          )
        )
      netDemand_update(sum(df$newnet))
      net_gap(sum(df$newnet))
    } else{
      c1<-as.numeric(input$C1)
      c2<-as.numeric(input$C2)
      c3<-as.numeric(input$C3)
      c4<-as.numeric(input$C4)
      df <- df |>
        mutate(
          newnet = case_when(
            (param1 > input$rate & param2 > input$rate)~ (c1 * cat1 + c2 * cat2 + c3 * cat3 + c4 * cat4),
            TRUE ~ net_real
          )
        )
      netDemand_update(sum(df$newnet))
      net_gap(sum(df$newnet))
    }
    #print(df_update)
  })
  
  observeEvent(input$submit, {
    name <- dbQuoteString(con, input$name)
    mailAdress <- dbQuoteString(con, input$email)
    scena <- dbQuoteString(con, input$scenario)
    reason <- dbQuoteString(con, input$reason)
    
    # Safely format the query string using sprintf
    query <- sprintf(
      "INSERT INTO vote (name, email, scenario, reason) VALUES (%s, %s, %s, %s)",
      name, mailAdress, scena, reason
    )
    
    tryCatch({
      dbExecute(con, query)
      showNotification("Vote submitted successfully!", type = "message")
      #update pie chart
      vote_result <- dbGetQuery(con, "SELECT * FROM vote")
      vote_count <- vote_result %>%
        group_by(scenario) %>%
        summarise(count = n()) %>%
        mutate(percentage = (count / sum(count)) * 100)
      
      # Create a pie chart with percentages
      ggplot(vote_count, aes(x = "", y = count, fill = scenario)) +
        geom_col(width = 1, color = "white") +
        coord_polar(theta = "y") +
        theme_void() +  # Remove unnecessary axes
        labs(title = "Votes by Scenario") +
        geom_text(
          aes(label = paste0(round(percentage, 1), "%")),
          position = position_stack(vjust = 0.5),
          color = "#FFFFFF",
          size = 5  # Adjust size as needed
        ) +
        scale_fill_manual(values = c("#077936", "#0000FF", "#333333")) 
      
    }, error = function(e) {
      showNotification(paste("Error submitting vote:", e$message), type = "error")
    })
    
    # Optional: Keep the connection open if needed
    # dbDisconnect(con)
  })
  
  #ALL plot
  
  output$donnatNet <- renderPlot({
    selected_region <- input$selected_item_region
    
    # Filter data based on the selected region or take all data if null
    if (is.null(selected_region) || selected_region == "") {
      prevision <- sum(data$net_prvision, na.rm = TRUE)
      net_real <- sum(data$net_demande, na.rm = TRUE)
    } else {
      region_data <- data %>% filter(Region == selected_region)
      prevision <- sum(region_data$net_prvision, na.rm = TRUE)
      net_real <- sum(region_data$net_demande, na.rm = TRUE)
    }
    
    # Calculate percentage change
    if (prevision == 0) {
      pourcentil <- 0
    } else {
      pourcentil <- ((net_real - prevision) / prevision) * 100
    }
    
    
    # Prepare data for the donut chart
    data_pop_donnat <- data.frame(
      category = c("Filled", "Empty"),
      value = c(pourcentil, 100 - pourcentil)
    )
    
    # Plot the donut chart
    ggplot(data_pop_donnat, aes(x = 2, y = value, fill = category)) +
      geom_col(width = 1, color = "white") +
      coord_polar(theta = "y") +
      xlim(0.5, 2.5) +  # Create a hole for the donut
      scale_fill_manual(values = c("Filled" = "#87CEEB", "Empty" = "#F0F0F0")) +  # Custom colors
      theme_void() +  # Remove axes and gridlines
      theme(legend.position = "none") +  # Remove legend
      # Add text in the center with the calculated percentage
      annotate(
        "text", x = 0.5, y = 0, label = ifelse(is.finite(pourcentil), paste0(round(pourcentil, 1), "%"), "N/A"),
        size = 12, color = "#000000", fontface = "bold"
      )
  })
  
  output$pie <- renderPlot({
    
    vote_result <- dbGetQuery(con, "SELECT * FROM vote")
    vote_count <- vote_result %>%
      group_by(scenario) %>%
      summarise(count = n()) %>%
      mutate(percentage = (count / sum(count)) * 100)
    
    # Create a pie chart with percentages
    ggplot(vote_count, aes(x = "", y = count, fill = scenario)) +
      geom_col(width = 1, color = "white") +
      coord_polar(theta = "y") +
      theme_void() +  
      labs(title = "Votes by Scenario") +
      geom_text(
        aes(label = paste0(round(percentage, 1), "%")),
        position = position_stack(vjust = 0.5),
        color = "#FFFFFF",
        size = 5  
      ) +
      scale_fill_manual(
        values = c("#077936", "#0000FF", "#333333"),
        labels = c("Scenario 1", "Scenario 2", "Scenario 3")
      )
  })
  
  output$hist<-renderPlot({
    gh<-data|>
      group_by(Region)|>
      summarise(
        population_real=sum(Population_survey),
      )
    ggplot(gh, aes(x = Region, y = population_real)) +
      geom_col() +  # Use geom_col() to create a bar plot for categorical data
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      labs(
        title = "Population by Region",
        x = "Region",
        y = "Population"
      ) +
      scale_fill_brewer(palette = "Set3")
  })
  
  output$coverage<-renderPlot({
    
    #coverage definition: coverage = #net/population. it's preferable that its value must be close as possible to 1.8
    #DATA WRANGLING : calculate the coverage per District, and classify them to I.less than 1.40, II.between 1.41 and 1.80, III.between 1.81 and 2.00
    #IV.between 2.01 and 2.3 and V. more than 2.31
    
    #in simulator, we would like to maximize District with Third class (between 1.81 and 2.00)
    
    cv<-data|>
      group_by(District)|>
      summarise(
        population_prev=sum(populatoin_projection,na.rm = TRUE),
        population_real=sum(Population_survey,na.rm = TRUE),
        net_prev=sum(net_prvision,na.rm = TRUE),
        net_real=sum(net_demande,na.rm = TRUE),
        cat1=sum(Cat.1,na.rm = TRUE),
        cat2=sum(Cat.2,na.rm = TRUE),
        cat3=sum(Cat.3,na.rm = TRUE),
        cat4=sum(Cat.4,na.rm = TRUE),
        coverage=sum(population_real, na.rm = TRUE)/sum(net_demande, na.rm = TRUE),
      )|>
      mutate(
        param1=((population_real-population_prev)/population_prev)*100,
        param2=((net_real-net_prev)/net_prev)*100,
        classCoverage = case_when(
          coverage < 1.40 ~ "less than 1.40",
          coverage >= 1.41 & coverage <= 1.80 ~ "1.41 to 1.80",
          coverage >= 1.81 & coverage <= 2.00 ~ "1.81 to 2.00",
          coverage >= 2.01 & coverage <= 2.30 ~ "2.01 to 2.30",
          coverage > 2.31 ~ "more than 2.31"
        )
      )
    
    
    param <- input$params
    
    if(param==1){
      #Let now caculate net need base on cat value
      c1<-as.numeric(input$C1)
      c2<-as.numeric(input$C2)
      c3<-as.numeric(input$C3)
      c4<-as.numeric(input$C4)
      cvr <- cv |>
        mutate(
          newnet = case_when(
            param1 > input$rate ~ (c1 * cat1 + c2 * cat2 + c3 * cat3 + c4 * cat4),
            TRUE ~ net_real
          ),
          newCovrage=population_real/newnet,
          NewclassCoverage = case_when(
            newCovrage < 1.40 ~ "less than 1.40",
            newCovrage >= 1.41 & newCovrage <= 1.80 ~ "1.41 to 1.80",
            newCovrage >= 1.81 & newCovrage <= 2.00 ~ "1.81 to 2.00",
            newCovrage >= 2.01 & newCovrage <= 2.30 ~ "2.01 to 2.30",
            newCovrage > 2.31 ~ "more than 2.31"
          )
        )
      
      
    }else if (param==2){
      c1<-as.numeric(input$C1)
      c2<-as.numeric(input$C2)
      c3<-as.numeric(input$C3)
      c4<-as.numeric(input$C4)
      cvr <- cv |>
        mutate(
          newnet = case_when(
            param2 > input$rate ~ (c1 * cat1 + c2 * cat2 + c3 * cat3 + c4 * cat4),
            TRUE ~ net_real
          ),
          newCovrage=population_real/newnet,
          NewclassCoverage = case_when(
            newCovrage < 1.40 ~ "less than 1.40",
            newCovrage >= 1.41 & newCovrage <= 1.80 ~ "1.41 to 1.80",
            newCovrage >= 1.81 & newCovrage <= 2.00 ~ "1.81 to 2.00",
            newCovrage >= 2.01 & newCovrage <= 2.30 ~ "2.01 to 2.30",
            newCovrage > 2.31 ~ "more than 2.31"
          )
        )
    
    } else{
      c1<-as.numeric(input$C1)
      c2<-as.numeric(input$C2)
      c3<-as.numeric(input$C3)
      c4<-as.numeric(input$C4)
      cvr <- cv |>
        mutate(
          newnet = case_when(
            (param1 > input$rate & param2 > input$rate)~ (c1 * cat1 + c2 * cat2 + c3 * cat3 + c4 * cat4),
            TRUE ~ net_real
          ),
          newCovrage=population_real/newnet,
          NewclassCoverage = case_when(
            newCovrage < 1.40 ~ "less than 1.40",
            newCovrage >= 1.41 & newCovrage <= 1.80 ~ "1.41 to 1.80",
            newCovrage >= 1.81 & newCovrage <= 2.00 ~ "1.81 to 2.00",
            newCovrage >= 2.01 & newCovrage <= 2.30 ~ "2.01 to 2.30",
            newCovrage > 2.31 ~ "more than 2.31"
          )
        )
    }
    
    ggplot(cvr, aes(x = NewclassCoverage)) +
      geom_bar(fill = 'steelblue') +
      coord_flip() +  # Flips the axes to put categories on the y-axis
      theme_minimal() +
      labs(title = "Bar Plot with Categorical Variable on Y-axis",
           x = "Category",
           y = "Count")
  })
  
}

shinyApp(ui = ui, server = server)