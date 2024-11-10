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

# con <- dbConnect(
#   RPostgres::Postgres(),
#   dbname= 'mydatabase_name',  
#   user= 'renaudflorent',          
#   password= 'YxWck3VSZrx9zNz9X1rxa1sMY2DA4xqf',      
#   host= 'dpg-csket5ij1k6c73c2kdig-a.oregon-postgres.render.com',              
#   port= '5432'                   
# )
# #importing data into data frame
# data <- dbGetQuery(con, "SELECT * FROM district_list")

data<-read.csv("./database.csv")

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


#graphe: 1 histrogram to show
result<-t.test(data$populatoin_projection, data$Population_survey,paired = TRUE, conf.level = 0.95)

#conclusion
if (result$p.value < 0.05) {
  conclusion <- "Reject the null hypothesis: There is a significant difference between the projection and obeserved values."
} else {
  conclusion <- "Fail to reject the null hypothesis: There is no significant difference between the projection and obeserved values."
}

print(conclusion)


#SHINY APP
# Define UI for the application
ui <- page_fillable(
  title = "Net Distribution",
  includeCSS("www/styles.css"),
  layout_column_wrap(
    card(
      #MINI DASHBOARD
      card_header("Mini Dashboad"),
      class = "MainCard",
      div(
        div(class="myOutput",verbatimTextOutput("myOutput1")),
        div (verbatimTextOutput("household")),
        div(verbatimTextOutput("Mosquito_net"))
      )
    ),
    card(
      card_header("Graph")
    ),
    card(
      card_header("SImulation"),
      
      selectInput("params",label = "Parameter:", choices = list("Polulation growth rate"=1,"Net demande growth rate"=2,"Both")),
      sliderInput("rate","Rate", min = 0.000, value=0.120,max = 1,000, step = 0.001),
      layout_column_wrap(
        card(
          class = "minicard",
          height = "10px",
          textAreaInput("C1","HH (1 to 2 personn"),
          
        ),
        card(
          textAreaInput("C2","HH (3 to 4 personn"),
        )
        ,
        card(
          textAreaInput("C3","HH (5 to 6 personn"),
        ),
        card(
          textAreaInput("C4","HH (+ 7 personn"),
        )
        
      ),
      layout_column_wrap(
        card(
          "Mostiquo Net: 25 2545 252"
        ),
        card(
          "Gap: 25 2545"
        ),
        width = 300
      )
     
      
    ),
    
    card(
      card_header("Voting system"),
      layout_column_wrap(
        card(
          textAreaInput("name",label = "Name"),
          textAreaInput("email",label = "E-mail"),
          selectInput("scenario",label = "CHoose the scenario which is more appropriate to you",choices = list("Scenario 1"=1,"Scenario 2"=2,"Scenario 3"=3, "Scenario 4"=4)),
          textAreaInput("reason", label="Tell us why"),
          actionButton("submit","Submit")
        ),
        card(
          #let presente the result in pie chart
          card_header("vote result")
        ),
      ),
      
      width="300px"
      
      
    ),
    width = "900px" 
  )
)

# Define server logic
server <- function(input, output, session) {
  output$myOutput1 <- renderText({ paste("Population:", format(population, big.mark = " ", scientific = FALSE)) })
  output$household <- renderText({ paste("Household:", format(sum(data$household_number_projection, na.rm = TRUE),big.mark=" ",scientific = FALSE)) })
  output$Mosquito_net <- renderText({ paste("Net Demand:", format(sum(data$net_demande, na.rm = TRUE),big.mark=" ",scientific = FALSE)) })
}

# Run the application 
shinyApp(ui = ui, server = server)