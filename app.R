#this script is about to describe the evolution of population after survey operation
#the main goal is to determine the evolution of people and determine if the net will be sufficient for all the malagasy people
#the data is integrate in a REMOTE POSTGRESQL (RENDER)
#table name 'authentification', it means how evolve the population after the projection or the prevision.


#if the net will not sufficient the we should propose scenario that partner will make a vote to decide which is more suitable.

#LIB importation

library(shiny)
library (ggplot2)
library(RPostgres)

con <- dbConnect(
  RPostgres::Postgres(),
  dbname= 'mydatabase_name',  
  user= 'renaudflorent',          
  password= 'YxWck3VSZrx9zNz9X1rxa1sMY2DA4xqf',      
  host= 'dpg-csket5ij1k6c73c2kdig-a.oregon-postgres.render.com',              
  port= '5432'                   
)

data <- dbGetQuery(con, "SELECT * FROM district_list")
head(data)


#DATA WRANGLING

#SHINY APP
