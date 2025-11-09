#--Author: POOJA.C


#--------Packages
library(tidyverse)
library(plm)
library(ggplot2)
library(shiny)
library(shinydashboard)

#-------Structure of dashboard-----------
# Two main menu-namely labour market indicators and employment elasticity

# 1. Labour market Indicators-
# Shows three tabs namely The key indicators, sectoral shares, shares by employment type by different 
# demographic factors like gender, region and age category.

# 2.Employment Elasticity- 
#   -Shows two tabs
#   -Overview (Plot 1:Compares GDP and employment growth over time
#              Plot 2 Employment elasticity overtime)
#   -Employment elasticity by states

#-------------------

#-----------------------------Section A Labour Market Indicators- India


#Load data
lb_mkt_indicators<-read.csv("labour_mkt_indicators_india_time.csv") #time series labour market indicators data
sectoral_share<-read.csv("sectoral_share_india_time.csv") #time series sectoral share data
emp_type_time<-read.csv("emp_type_time.csv")  #time series employment share data
merged<-read.csv("merged.csv")  #employment growth and gdp growth
elasticity_df<-read.csv("elasticity_df.csv")
emp_gdp_growth<-read.csv("emp_gdp_growth.csv")   #employment and gdp growth  data


#-------------------------------------------------------------------------------


#-------------------------------UI Logic

#Setup UI interface
ui <- dashboardPage(
  dashboardHeader(titleWidth = 300,        # give more width
                      title ="Employment Dashboard"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview & Key Insights", tabName = "overview",icon=icon("dashboard")),
      menuItem("Labour Market Indicators", tabName = "lbt_mkt", icon = icon("users")),
      menuItem("Employment Elasticity", tabName = "emp_elas", icon = icon("chart-line")),
      menuItem("Appendix", tabName = "appendix", icon = icon("Tick mark"))
      
    )
  ),
  dashboardBody(

    tabItems(
      tabItem(
        tabName = "overview",
        fluidRow(
          box(
            width = 12,
            title = "Overview",
            p("This interactive dashboard presents the analysis of labour market structure in India between 2017 to 2024. The study uses the periodic labour force survey from 2017-18 to 2023-24.
Section A -Discuses key labour market indicators namely employment, unemployment and labour force rate by different demographic factors. Also, the study sheds light on sectoral shares and nature of employment in India. 
Section B -Employment elasticity to GDP is calculated and analysed across states (in progress). 
")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Key Insights",
            # Subheading 1
            #h4("Labour Market Indicators"),
            tags$ul(
              tags$li("Between 2017 to 2024, the employment rate has increased from 46.8 to 58.2 percent. 
                      Over the years share of male employment has remained higher, ranging between 70 to 75 percent, than female shares ranging between 22 to 40.3 percent."),
              tags$li("A region wise analysis shows that rural female employment has improved from 23.7 to 46.5 percent while urban female employment has grown from 18.2 to 26 percent only.")
            ),
            # Unemployment rate and young workforce
            h4("Unemployment rate and young workforce"),
            tags$ul(
              tags$li("Unemployment rate has consistently reduced from 6 to 3.2 percent in India. 
                      However, analyzing unemployment rate by age category gives a better picture. "),
              tags$li("The overall unemployment rate of young workforce (15-29) has remained above 10 percent indicating a massive youth unemployment in India.
                      However, over the years it has reduced from 17 to 10 percent which is a positive sign."),
              tags$li("Comparing other groups, the unemployment rate for young females in urban areas remained higher. 
                      In 2023-24, the female urban unemployment is 20.1 percent as opposed to their male counterparts with 12.8 percent."),
              tags$li("Also, during covid years (2019 to 2021) young urban female were affected whose unemployment remained highest with 25 percent as compared to other groups.")
            ),
            #Sectoral shares
            h4("Sectoral share"),
            tags$ul(
              tags$li("The major sectors considered are agriculture, secondary and tertiary. In India as always, agriculture sector has the major share of employment. 
                      Particularly the shares have significantly increased after covid years from 42.4 to 45.5 percent"),
              tags$li("A region wise review shows that in urban areas service sector share of employment has remained higher with 60 percent."),
              tags$li("Post covid pandemic the sectoral relocation of female and male shares of employment is crucial. For male the share of agriculture increased to 39.9 percent in 2019-20 and consistently reduced to 36.2 percent in 2023-24. 
                      Correspondingly, the share of tertiary sector, followed by secondary sector has increased consistently. "),
              tags$li("Whereas share of female employment in agriculture consistently increased from 59.8 percent in 2019-20 to 64.3 percent in 2023-24, while share of tertiary (23.6 to 20.1 percent) and secondary (16.6 to 15.5 percent) sector employment consistently reduced.")
            ),
            h4("Employment nature"),
            tags$ul(
              tags$li("The major categories of type of employment are regular salaried, casual labour and self -employed. 
                      Self-employed category includes own account workers who own their enterprise, then the employers who run business with hired workers and unpaid workers who work on their family business without payment. In this study we club own account worker and employer into self-employed category and present the unpaid worker separately."),
              tags$li("In India self-employed remains as the major type of employment raging between 38 to 39 percent. Post-covid the share of unpaid family workers has increased from 16 percent to 20 percent and share of casual labour has reduced consistently from 23.5 to 20 percent."),
             tags$li("For females the share of unpaid family helper remains the major type of employment."),
             tags$li("Post-covid shows that in urban regions the share of female in regular salaried has reduced from 54.3 percent to 50 percent."),
             tags$li("Post-covid for females in rural areas though unpaid family worker category has the major share, there has been consistent improvement in self -employed from 20.7 to 31.3 percent.")
              
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Conclusion",
            p("The overall employment rate has been improved. However, the analysis by different demographic factors shows important patterns.
The female employment has improved over the years, especially in rural areas. However, analyzing the sectoral shares, the females were largely employed in agriculture sector. In rural areas they predominantly work as unpaid family worker."),

p("Good news that there has been revival in secondary and tertiary sector post covid. However, the impact of covid pandemic is evident in reallocation of workers. Post covid the share of female in agriculture increased and decreased in other sectors, whereas for males the shares in secondary and tertiary increased, squeezing share in agriculture. 
Also, the share of females in regular salaried consistently reduced. However, in both rural and urban areas the share of female in self-employed category showed an upward movement. 
Young minds are the key to economic growth. Over the years the youth unemployment has reduced. However, the female unemployment rate in urban areas for young workforce was highest among other groups"),

p("Thus, with public policy in focus to address these issues there is a need to increase the quality of job in rural areas, need of increasing skill sets and education and need of creating employment opportunities (especially in urban areas with high proportion of unemployed females")

          )
        )
      ),
      # TabItem 1: LBT Market
      tabItem(tabName = "lbt_mkt",
              fluidRow(
                box(width = 3,
                    
                    # Dropdown for sex
                    selectInput("sex_select", "Sex:",
                                choices = c("All", "Male", "Female"),
                                selected = "All"),
                    
                    #Dropdown for Sector
                    selectInput("sector_select","Sector:",
                                choices = c("All","Urban","Rural"),
                                selected ="All"),
                
                    conditionalPanel(
                      condition = "input.lbt_tabs == 'tab1_lbt'",
                    
                    # For p1: dropdown for Y-variable
                    selectInput("yvar_select", "Indicator:",
                                choices = c("Employment rate"     = "employment_rate",
                                            "Labour Force Participation Rate (LFPR)" = "labourforce_rate",
                                            "Unemployment rate"   = "unemployment_rate"),
                                selected = "employment_rate"),
                    selectInput("agecat_select","Age Category:",
                                choices = c("All","60 and above","Middle Aged (30-50)","young workforce (15-29)"),
                                selected = "All")
                    
                    ),
                    
                ),
                box(width = 9,height = 10,
                    tabBox(
                      id = "lbt_tabs", title = "Labour Market Analysis", width = NULL, height = "400px",
                      tabPanel("Key Indicators", value = "tab1_lbt",
                               plotOutput("indicators", height = "500px")
                      ),
                      tabPanel("Sectoral Shares", value = "tab2_lbt",
                               plotOutput("sectoralshare", height = "500px")
                      ),
                      tabPanel("Employment Type", value = "tab3_lbt",
                               plotOutput("emptype_share", height = "500px")
                      ),
                      # tabPanel("Employment type by sectors", value = "tab4_lbt",
                      #          plotOutput("sectoremp_type")
                      # )
                    )
                )
              )
      ),
      
      # TabItem 2: Employment Growth (modified)
      tabItem(tabName = "emp_elas",
              fluidRow(
                box(width = 2,
                    
                    conditionalPanel(
                      condition = "input.emp_tabs == 'tab1_emp'",
                    # Input for filtering state (for p1, p2)
                    selectInput("state_sel", "Select State",
                                choices = unique(emp_gdp_growth$state),
                                selected = "All India"),
                    
                    ),
                    
                    conditionalPanel(
                      condition = "input.emp_tabs == 'tab3_emp'",
                    # Input for filtering year (for p3)
                    selectInput("year_sel", "Select Year",
                                choices = unique(elasticity_df$year),
                                selected = unique(elasticity_df$year)[1])
                    
                    ),
                ),
                box(width = 10,
                    tabBox(
                      id = "emp_tabs", title = "Employment Elasticity to GDP", width = NULL, height = "500px",
                      # Removed Trend tab
                      tabPanel("Overview", value = "tab1_emp",
                               fluidRow(
                                 column(6, plotOutput("growth")),
                                 column(6, plotOutput("elasticity"))
                               )
                      ),
                      tabPanel("States", value = "tab3_emp",
                               plotOutput("elasticity_states")
                      )
                    )
                )
              )
      ),

       #Appendix
     tabItem(tabName = "appendix",
             fluidRow(
               box(
                 width = 12,
                 title = "Data Processing and analysis methods",
                 tags$p("The study involved data collection of periodic labour force survey for the period between 2017-18 to 2023-24. 
                   The data was cleaned, harmonised across the time periods. Complied the cleaned files and produced summary tables. Further R shiny application was used to create the interactive dashboard.
                   All R codes are available in this",
                 tags$a(href = "https://github.com/Pooja-C-ux/PLFS-time-series", "Github link", target = "_blank")
               )
                 )
             )
             
             )


    )
  )
)


#----------------------------------SERVER LOGIC
server <- function(input, output, session) {
  
  #-----------------------------------------------------------------------------------------
  #                                 LABOUR MARKET INDICATORS
  #-------------------------------------------------------------------------------------------
  
#--------Filtering data  
  df1 <- reactive({
    
    req(input$sex_select, input$sector_select,input$agecat_select)
    
    lb_mkt_indicators %>%
      filter(sex == input$sex_select) %>% 
      filter(sector==input$sector_select) %>% 
      filter(age_cat==input$agecat_select)
    
  })
  #-------------------------------------Indicators
  
  # p1: time series
  output$indicators <- renderPlot({
    
req(df1())
  
    col <- switch(input$yvar_select,
                  "employment_rate" = "#1f77b4",
                  "unemployment_rate" = "#ff7f0e",
                  "labourforce_rate" = "darkgreen")
    
    
    # select Y variable dynamically
    
    ggplot(df1(), aes(x = year, y = .data[[ input$yvar_select ]])) +
      geom_bar(stat = "identity",fill=col) +
      geom_text(aes(label = .data[[ input$yvar_select ]]),
                vjust = -0.5, color = "black", size =4 ) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
     # scale_fill_manual(values=colour_vals)+
      labs(
        title   = paste0(
          switch(input$yvar_select,
                 employment_rate   = "Employment Rate",
                 labourforce_rate         = "Labour Force Participation Rate",
                 unemployment_rate = "Unemployment Rate"),
          " for ",
          ifelse(input$sex_select=="All", "all persons", tolower(input$sex_select)),
          " in ",
          ifelse(input$sector_select=="All","India", paste0(tolower(input$sector_select)," India")),
          " for ",
          ifelse(input$agecat_select=="All","15 and above age category",tolower(input$agecat_select))
        ),
        x       = "Year",
        y       = paste0(
          switch(input$yvar_select,
                 employment_rate   = "Employment Rate",
                 unemployment_rate = "Unemployment Rate",
                 labourforce_rate         = "Labour Force Participation Rate"),
          " (in %)"
        ),
        caption = "Source: Estimated from PLFS"
      )  +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold", size = 16),
            axis.text = element_text(size = 10,face = "bold"),
            axis.title = element_text(size = 12))
    
    
  }) 
  
  
  #--------------------------------SECTORAL SHARE
  
  #---Filter data
  df2 <- reactive({
    
    req(input$sex_select, input$sector_select,input$agecat_select)
    
    sectoral_share %>%
      filter(sex == input$sex_select) %>% 
      filter(sector==input$sector_select)
  })
  
  output$sectoralshare <- renderPlot({

    req(df2())
    
    ggplot(df2(), aes(x = year,
                    y = distribution,
                    colour = industry,
                    label = distribution,
                    group=industry)) +
      geom_point(size=2)+
      geom_line(stat = "identity",linewidth=1) +
      geom_text(aes(label=distribution),colour = "black", size = 4.5, vjust=-0.8) +
      scale_colour_manual(values = c(
        "Agriculture" = "#1b9e77",
        "Secondary"   = "#d95f02",
        "Tertiary"    = "#7570b3"
      )) +
      labs(
        title = paste0("Sectoral Share",
                      " for ",
                      ifelse(input$sex_select=="All", "all persons", tolower(input$sex_select)),
                      " in ",
                      ifelse(input$sector_select=="All","India", paste0(tolower(input$sector_select)," India"))
                      
                      ),
        x     = "Year",
        y     = "Distribution (%)",
        fill  = "Sector",
        caption = "Source: Estimated from PLFS"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 16),
            axis.text = element_text(size = 10,face = "bold"),
            axis.title = element_text(size = 12),
            legend.position = "bottom")+
      annotate(
        "rect",
        xmin = "2019-20",
        xmax = "2023-24",
        ymin = -Inf,
        ymax = Inf,
        fill="lightblue",
        alpha=0.2
      )+
      annotate("text",
               x     = "2019-20",
               y     = max(df2()$distribution) * 1.2,
               label = "Post COVID",
               colour = "black",
               hjust = 0,
               size   = 4,
               fontface = "bold")
  })
  
  
  #----------------------------------------Shares by Employment type 
  
  
  #Filter data
  
  df3 <- reactive({
    
    req(input$sex_select, input$sector_select)
    
    emp_type_time %>%
      filter(sex == input$sex_select) %>% 
      filter(sector==input$sector_select)
    
  })
  
  output$emptype_share <- renderPlot({
req(df3())
    
    ggplot(df3(), aes(x = year,
                    y = distribution,
                    colour = emp_usual_status,
                    label = distribution,
                    group = emp_usual_status)) +
      geom_point(size=2)+
      geom_line(stat = "identity",linewidth=1) +
      geom_text(aes(label=distribution),colour = "black", size = 4.5, vjust=-1,check_overlap = TRUE) +
      scale_colour_manual(values = c(
        "Casual labour"       = "#FF6F61",   # coral red
        "Self employed"        = "#4CC95D",   # vivid green‑ish
        "Unpaid family worker" = "#FFCC4D",   # bright gold
        "Regular salaried"     = "#00C49F"    # fresh teal‑green
      ))+
      labs(
        title = paste0("Share by employment type",
                      " for ",
                      ifelse(input$sex_select=="All", "all persons", tolower(input$sex_select)),
                      " in ",
                      ifelse(input$sector_select=="All","India", paste0(tolower(input$sector_select)," India"))
        ),
        x     = "Year",
        y     = "Distribution (%)",
        fill ="Employment type",
        caption = "Source: Estimated from PLFS"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 16),
            axis.text = element_text(size = 10,face = "bold"),
            axis.title = element_text(size = 12),
            legend.position = "bottom")+
      annotate(
        "rect",
        xmin = "2019-20",
        xmax = "2023-24",
        ymin = -Inf,
        ymax = Inf,
        fill="lightblue",
        alpha=0.2
      )+
      annotate("text",
               x     = "2019-20",
               y     = max(df2()$distribution) * 1.2,
               label = "Post COVID",
               colour = "black",
               hjust = 0,
               size   = 4,
               fontface = "bold")
  })
  
  
  #-----------------------------------------------------------------------------------------
  #                                     EMPLOYMENT ELASTICITY
  #-------------------------------------------------------------------------------------------
  
  
  # Reactive data for p1
  p1_data <- reactive({
    emp_gdp_growth %>%
      filter(state == input$state_sel) %>%
      select(year, state, emp_growth, gdp_growth) %>%
      pivot_longer(cols = c(emp_growth, gdp_growth),
                   names_to = "growth", values_to = "value")
  })
  
  output$growth <- renderPlot({
    p1_data() %>%
      ggplot(aes(x = year, y = value, color = growth, group = growth)) +
      geom_point(size = 3) +
      geom_line(size = 1) +
      geom_text(aes(label = round(value,1)), vjust = -0.8, size = 3.5,
                check_overlap = TRUE, colour = "black") +
      scale_color_manual(values = c("emp_growth" = "#4C9F70", 
                                    "gdp_growth" = "#6777FF"),
                         labels = c("emp_growth" = "Employment growth",
                                    "gdp_growth" = "GDP growth")) +
      labs(title = paste0("Employment growth vs GDP growth in ",input$state_sel),
           x = "Year", y = "Growth Rate (%)",
           caption = "Source: PLFS") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(face = "bold", size = 16),
            plot.caption = element_text(size = 10, face = "italic"),
            legend.title = element_blank(),
            legend.position = "bottom")
  })
  
  
  p2_data <- reactive({
    elasticity_df %>%
      filter(state == input$state_sel)
  })
  
  output$elasticity <- renderPlot({
    
    p2_data() %>%
      ggplot(aes(x = year, y = emp_elasticity,group = 1)) +
      geom_point(color = "steelblue", size = 3) +
      geom_text(aes(label = round(emp_elasticity,1)), 
                vjust = -0.8, size = 3.5, check_overlap = TRUE,colour="black")+
      geom_line(color = "steelblue", linewidth = 1) +
      labs(
        title = paste0("Employment elasticity in ",input$state_sel),
        x = "Year",
        y = "Employment Elasticity"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)
      )
  })
  
  # Reactive data for p3
  p3_data <- reactive({
    elasticity_df %>%
      filter(year == input$year_sel) %>%
      arrange(desc(emp_elasticity)) %>%
      slice_head(n = 10)
  })
  
  output$elasticity_states <- renderPlot({
    p3_data() %>%
      ggplot(aes(x = reorder(state, emp_elasticity), y = emp_elasticity, fill = state)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(title =paste0("Top 10 states by employment elasticity in the year ",input$year_sel),
           x = "State", y = "Employment Elasticity") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 16),
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 12))
  })
  
  
  
  
  
  # Employment Growth: Overview (now with two plots)
  output$plot_emp_overview <- renderPlot({
    plot(1:10, rnorm(10), main = paste("Employment Growth – Overview (", input$filter_emp, ")", sep=""))
  })
  
  output$plot_emp_comparison <- renderPlot({
    plot(1:10, rnorm(10, mean=2), main = paste("Employment Growth – Comparison (", input$filter_emp, ")", sep=""))
  })
  
  # Employment Growth: By Sector
  output$plot_emp_sector <- renderPlot({
    plot(1:10, rnorm(10, mean=6), main = paste("Employment Growth – By Sector (", input$filter_emp, ")", sep=""))
  })
}

shinyApp(ui, server)

