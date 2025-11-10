# Time series analysis of labour market in India- Interactive dashboard
[Dashboard Link https://pooja-c.shinyapps.io/Employment_dashboard_copy/](https://pooja-c.shinyapps.io/Employment_dashboard_copy/) 

This project aims to understand labour market structure in India from the period 2017-18 to 2023-24 using periodic labour force survey. 
## Key indicators
###### Labour indicators-namely employment rate, labourforce partcipation rate and unemployment rate by demoraphic facotrs like gender, region and age category over time.
###### Shares of employment by sectors namely agriculture, secondary and tertiary by demographic factors over time
###### Shares of employment nature namely self employed, regular salaried,casual labour and unpaid family worker over time.

## Data processing steps-
 #### DATA COLLECTION
 -Employment data has been collected from MOSPI website
 #### DATA HARMONISATION-
 ###### Data has converted from text to CSV format
 ###### The column names were standardised across the years
 ###### The categorical variables has been recoded 
 ###### Package used: Tidyverse 

 #### DATA SUMMARY TABLES-
 ###### Labour market indicators, Sectoral shares and shares by employment nature has been calculated by different demographic factors like region, gender and age category
 ###### Employment elasticity has been calculated from employment growth and gdp growth
 ###### Package used- srvyr, tidyverse
 
 ### DATA VISUALISATION- Interactive dashboard has been constructed
 ###### Created basic layout with three menu- 1. Overview and Key Insights  2. Labour Market Indicators.  3. Employment Elasticity.
 ###### Created relevant plots using ggplot
 ###### Packages used ggplot2, Shiny, Shinydashboard
