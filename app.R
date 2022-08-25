# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(ggplot2)
library(dplyr)

# Load data----


  # xPoint <- c(0, 1)
  # yPoint <- c(0, 1)
  # df <- data.frame( xPoint, yPoint)



baseData <- data.frame(
  fare= c(54, 84, 90, 93, 111, 141, 162, 183, 207, 231, 291, 300, 309, 360, 360, 429, 477),
  distance= c(90, 179, 184, 190, 270, 393, 502, 578, 681, 818, 1102, 1138, 1204, 1448, 1463, 1813, 1828)
  ) %>%
  mutate(yMean = mean(fare))


bodyData <- data.frame(
  triceps = c(19.5, 24.7, 30.7, 29.8, 19.1, 25.6, 31.4, 27.9,22.1, 25.5, 31.1, 30.4,
              18.7, 19.7, 14.6, 29.5, 27.7, 30.2, 22.7, 25.2),
  thigh = c(43.1, 49.8, 51.9, 54.3, 42.2, 53.9, 58.5, 52.1, 49.9, 53.5, 56.6, 56.7, 46.5, 44.2,
            42.7, 54.4, 55.3, 58.6, 48.2, 51),
  midarm = c(29.1, 28.2, 37, 31.1, 30.9, 23.7, 27.6, 30.6, 23.2, 24.8, 30, 28.3, 23, 28.6, 21.3,
             30.1, 25.7, 24.6, 27.1, 27.5),
  bodyfat = c(11.9, 22.8, 18.7, 20.1, 12.9, 21.7, 27.1, 25.4, 21.3, 19.3, 25.4, 27.2,
              11.7, 17.8, 12.8, 23.9, 22.6, 25.4, 14.8, 21.1)) %>%
    mutate(fatMean = mean(bodyfat))
  


 

# Load additional dependencies and setup functions
# source("global.R")

# Define UI for App ----
ui <- list(
  ## Create the app page ----
  dashboardPage(
    skin = "black",
    ### Create the app header ----
    dashboardHeader(
      title = "Exploring R Squared", # You may use a shortened form of the title here
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Exploring R Squared")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("home")
        )
      )
    ),
    ### Create the sidebar/left navigation menu ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Explore R Squared", tabName = "Explore1", icon = icon("wpexplorer")),
        menuItem("Explore partial R Squared", tabName = "Explore2", icon = icon("wpexplorer")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ### Create the content ----
    dashboardBody(
      tabItems(
        #### Set up the Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Exploring R Squared"), # This should be the full name.
          p("R squared is a statistical measure of how close the data are to the
            fitted regression line. This app allows you to explore it in simple 
            linear regression model and multvariant model."),
          h2("Instructions"),
          br(),
          tags$ol(
            tags$li("Click on the prerequisites button to review/learn the concepts
                    of R squared value and partial R squared value."),
            tags$li("click the go button to enter the prerequisite page.")
            
          ),
          ##### Go Button--location will depend on your goals
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "go1",
              label = "GO!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          ),
          ##### Create two lines of space
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This version of the app was developed and coded by Junjie He. Special 
            thanks to Pro. Neil for coding
            help.",
            br(),
            
            br(),
            br(),
            br(),
            br(),
            div(class = "updated", "Last Update: 7/3/2022 by JJH.")
          )
        ),
        #### Set up the Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Concepts of R squared and Partial R squared"),
          br(),
          box(
            title = strong("What is the R squared value ?"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "R squared value, denoted as \\(r^2\\), is the regression sum of squares
            divided by the total sum of squares. \\(r^2\\) = SSR/SST = 1 - SSE/SST. 
            Because R squared value is a proportion, it should between 0 and 1.
            If the value is equal to 1, it means that the model explains 
            all the variability of the response data around the mean. If 
            the value is equal to 0, it means that the model explains
            none of the variability of the response data around its mean "
            
          ),
          box(
            title = strong("how to intepret R squared value ?"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            p("there are two ways to inteprate the \\(r^2\\)"),
            tags$ul(
              tags$li("We can say \\(r^2\\) *100 percent of the variation in y is
                      reduced by taking into account predictor x."),
            tags$li("\\(r^2\\) *100 percent of the variation in y is explained by the
                    variation in predictor x.")
            )
          ),
          box(
            title = strong("What is the partial R squared ? How to inteprete it?"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "partial R squared value also called the coefficient of partial determination.
            In regression model, assuming that a full model will include all three
            predictors (Y ~ X1, X2 X3), partial R squared helps to explain what
            additional percent of variation can be explained by X2 and X3, given 
            the reduced model only include X1. In general, partial R squared gives 
            us the proportion of variation explained by X2 and X3 that cannot be 
            explained by X1."
          )
        ),
        
        
        #### Set up an Explore Page 1----
        tabItem(
          tabName = "Explore1",
          h2("R squared"),
          tags$ul(tags$li("Adjust the sliders to change the intercept (β0) and 
                              coefficient (β1)."),
                  tags$li("Exploring R squared by Flightfair and Distance data.")),
          br(),
          fluidRow(
            column(
              width = 4,
              wellPanel(
                h3("Data Info"),
                p("This data contains 17 observations of flightfare and related
                      flight distance. Using the slider to adjust intercept and 
                      coefficient to see the changes."),
                br(),
                sliderInput(
                  inputId = "b0",
                  label = "intercept (β0):",
                  min = 0,
                  max = 78,
                  value = 0
                ),
                br(),
                sliderInput(
                  inputId = "b1",
                  label = "coefficient/slope (β1):",
                  min = 0.14,
                  max = 0.32,
                  value = 0.14)
                
              ),
              checkboxInput(
                inputId = "fittedRegression",
                label = "Show the fitted regression line",
                value = FALSE,
                width = "100%"
              ),
              tableOutput(outputId = "fittedLine"),
            ),
            column(
              width = 8,
              plotOutput(outputId = "sstPLOT"),
              plotOutput(outputId = "ssrPLOT"),
              plotOutput(outputId = "ssePLOT"),
              plotOutput(outputId = "squarePLOT",height = "100px"),
            )
          ),
          br(),
          checkboxInput(
            inputId = "fillIN1",
            label = "check to see the value of SST and SSR",
            value = FALSE,
            width = "100%"
          ),
          tableOutput(outputId = "fill1")
        ),
        #### Set up an Explore Page 2----
        tabItem(
          tabName = "Explore2",
          h2("Partial R squared"),
          tags$ul(tags$li("Use the dropdown menu to select the full model and the
                          reduced model."),
                  tags$li("Exploring Partial R squared by Bodyfat data.")),
          br(),
          fluidRow(
            column(
              width = 4,
              wellPanel(
                h3("Data Info"),
                p("This data contains 20 observations of bodyfat and related information
                  about triceps, thigh and midarm."),
                br(),
              selectInput(
                inputId = 'fullMODEL',
                label = 'Select a full model',
                choices = list(
                  'Triceps + Thigh + Midarm' = "triceps + thigh + midarm",
                  'Triceps + Thigh' = "triceps + thigh",
                  'Triceps + Midarm' = "triceps + midarm",
                  'Thigh + Midarm' = "thigh + midarm"
                ),
                selected = 'Triceps + Thigh + Midarm'
              ),
              selectInput(
                inputId = 'reducedMODEL',
                label = 'Select a reduced model',
                choices = c("fill2")
              ),
              bsButton(
                inputId = "newSAMPLE", 
                label = "Simulate!", 
                icon = icon("retweet"),
                size = "large",
                style = "default"
              ),
              )
            ),
            column(
              width = 8,
              h3("Illustraton:"),
              tags$ul(tags$li("The plot below will show the relationship between 
                              full model and reduced model."),
                      tags$li("The red part represents the proportion of variation 
                              explained by the reduced model."),
                      tags$li("The green part shows the proportion of variation 
                              that cannot be explained by the reduced model, but 
                              can be explained by the full model (Partial R Squared). "),
                      tags$li("The combination of red part and green part represents
                              the proportion of variation explained by the full
                              model.")
               
              ),
              plotOutput(outputId = "partialPLOT"),
              tableOutput(outputId = "fill3"),
            ),
            # checkboxInput(
            #   inputId = "fillIN2",
            #   label = "check to see the R squared value of two models",
            #   value = FALSE,
            #   width = "100%"
            # ),
            # tableOutput(outputId = "fill3")
            
          ),
          # checkboxInput(
          #   inputId = "fillIN2",
          #   label = "check to see the value of SSR of Full Model and SSR of Reduced
          #   Model",
          #   value = FALSE,
          #   width = "100%"
          # ),
          # tableOutput(outputId = "fill3")
        ),
        #### Set up the References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          p(
            class = "hangingindent",
            "Bailey, E. (2022). shinyBS: Twitter bootstrap components for shiny.
            (v 0.61.1). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield, N. J. (2022). boastUtils: BOAST utlities.
            (v 0.1.12.3). [R package]. Available from
            https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Change, W., and Borges Ribeiro, B. (2021). shinydashboard: Create
            dashboards with 'shiny'. (v 0.7.2) [R package]. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng J., Allaire, J., Sievert, C., Schloerke, B., Xie, Y.,
            Allen, J., McPherson, J., Dipert, A., and Borges, B. (2021). shiny:
            Web application framework for R. (v 1.7.1). [R package]. Available
            from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Pennsylvania State University. (2018). STAT 462 Applied Regression Analysis. 
            Available from https://online.stat.psu.edu/stat462/"
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2022). shinyWidgets: Custom
            inputs widgets for shiny. (v 0.7.0). [R package]. Available from
            https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(
            class = "hangingindent",
            "Wickham, H. (2016). ggplot2: Elegant graphics for data analysis.
            Springer-Verlag:New York. (v 3.3.6) [R package]. Available from
            https://ggplot2.tidyverse.org"
          ),
          p(
            class = "hangingindent",
            "Wickham, H., François, R., Henry, L., Müller, K. (2021). dplyr: A 
            Grammar of Data Manipulation. R package version 1.0.6. Available from
            https://CRAN.R-project.org/package=dplyr"
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)



# Define server logic ----
server <- function(input, output, session) {
  
  plotData <- eventReactive(
    eventExpr = c(input$b0, input$b1),
    valueExpr = {
      baseData %>%
        mutate(
          yHat = input$b0 + input$b1 * distance
        )
    }
  )
  
  sst <- eventReactive(
    eventExpr = plotData(),
    valueExpr = {
      sum((plotData()$fare - plotData()$yMean)^2)
      
    }
  )
  
  sse <- eventReactive(
    eventExpr = plotData(),
    valueExpr = {
      sum((plotData()$fare - plotData()$yHat)^2)
    }
  )
  
  ssr <- eventReactive(
    eventExpr = plotData(),
    valueExpr = {
      sst()-sse()
    }
  )
  
  dataCollection <- eventReactive(
    eventExpr =  c(input$fullMODEL,input$reducedMODEL),
    valueExpr = {
      switch(
        EXPR = input$selectData,
        MEDData = MEDData,
        PERSONData = PERSONData
      )
    }
  )
  
  
  # sstMODEL <- eventReactive(
  #   eventExpr = bodyData(),
  #   valueExpr = {
  #     sum((bodyData()$bodyfat - bodyData()$fatMean)^2)
  #     
  #   }
  # )
  # 
  # ssrFULL <- eventReactive(
  #   eventExpr = bodyData(),
  #   valueExpr = {
  #     sum((temp1 - bodyData()$fatMean)^2)
  #     
  #   }
  # )
  # 
  # ssrREDUCED <- eventReactive(
  #   eventExpr = bodyData(),
  #   valueExpr = {
  #     sum((temp2 - bodyData()$fatMean)^2)
  #     
  #   }
  # )
  
  # sseFULL <- eventReactive(
  #   eventExpr = bodyData(),
  #   valueExpr = {
  #     sum((bodyData()$bodyfat - temp1 ))
  #   }
  # )
  # 
  # sseREDUCED <- eventReactive(
  #   eventExpr = bodyData(),
  #   valueExpr = {
  #     sum((bodyData()$bodyfat - temp2))
  #   }
  # )

  
  
  # below for the explore 1 page
   ## SST plot ----
  output$sstPLOT <- renderPlot(
    expr = {
      ggplot(
        data = plotData(),
        mapping = aes(x=distance, y=fare))+
        geom_point(na.rm = TRUE)+
        geom_hline(aes(yintercept = mean(fare)), color="blue")+
        geom_rect(
          mapping = aes(
            ymin = fare,
            ymax = yMean,
            xmin = distance,
            xmax = abs(fare-yMean) + distance,
          ),
          alpha = .15,fill="blue",
          na.rm = FALSE
        )+
        scale_x_continuous(breaks = c(200, 400, 600,800,1000,1200,1400,1600,1800,2000))+
        scale_y_continuous(breaks = c(200,400,600,800))+
        theme_bw()+
        ggtitle("Plot of SST") +
        theme(
          text = element_text(size = 18)
        )
    },
    alt = "fill this later"
  )
  
  
  ## SSR plot----
  observeEvent(
    eventExpr = plotData(),
    handlerExpr = {
      output$ssrPLOT <- renderPlot(
        expr = {
          ggplot(
            data = plotData(),
            mapping = aes(x = distance, y = fare))+
            geom_point(na.rm = TRUE)+
            geom_abline(intercept = input$b0,slope = input$b1)+
            geom_hline(aes(yintercept = mean(fare)), color = "blue")+
            geom_rect(
              mapping = aes(
                xmin = distance,
                xmax = distance + (yHat - yMean),
                ymin = yMean,
                ymax = yHat),
              na.rm = FALSE,
              alpha = .15,
              fill = psuPalette[2],
              color = psuPalette[2]
            )+
            scale_x_continuous(limits = c(200, 2200),
                               breaks = seq.int(from = 200, to = 2200, by = 200))+
            scale_y_continuous(
                               breaks = seq.int(from = 200, to = 600, by = 200))+
            theme_bw()+
            ggtitle("Plot of SSR") +
            theme(
              text = element_text(size = 18)
            )
        },
        alt = "fill this later"
      )
      
      
      
    }   
  )
  
  
  ## SSE plot----
  observeEvent(
    eventExpr = plotData(),
    handlerExpr = {
      output$ssePLOT <- renderPlot(
        expr = {
          ggplot(
            data = plotData(),
            mapping = aes(x = distance, y = fare))+
            geom_point(na.rm = TRUE)+
            geom_abline(intercept = input$b0,slope = input$b1)+
            geom_hline(aes(yintercept = mean(fare)), color = "blue")+
            geom_rect(
              mapping = aes(
                xmin = distance,
                xmax = distance + (yHat - fare),
                ymin = fare,
                ymax = yHat),
              na.rm = FALSE,
              alpha = .15, 
              fill = psuPalette[3],
            )+
            scale_x_continuous(
              limits = c(200, 2200),
              breaks = seq.int(from = 200, to = 2200, by = 200))+
            scale_y_continuous(
              breaks = seq.int(from = 200, to = 600, by = 200))+
            theme_bw()+
            ggtitle("Plot of SSE") +
            theme(
              text = element_text(size = 18)
            )
        },
        alt = "fill this later"
      )
      
      
      
    }   
  )
  
  ## barplot of R squared----
  observeEvent(
    eventExpr = plotData(),
    handlerExpr = {
      r = sqrt(ssr()/sst())
      output$squarePLOT <- renderPlot(
        expr = {
          ggplot(
            data = plotData(),
            mapping = aes()
          ) +
            theme_void() +
            geom_rect(
              mapping = aes(
                xmin = 0, 
                xmax = 1, 
                ymin = 0, 
                ymax = 0.25),
              na.rm = FALSE,
              color = "blue",
              fill = NA
            ) +
            geom_rect(
              mapping = aes(
                xmin = 0, 
                xmax = r, 
                ymin = 0, 
                ymax = 0.25),
              na.rm = FALSE,
              fill = psuPalette[2],
              color = psuPalette[2],
              alpha = 0.15
            ) +
            geom_rect(
              mapping = aes(
                xmin = r, 
                xmax = 1, 
                ymin = 0, 
                ymax = 0.25),
              na.rm = FALSE,
              fill = psuPalette[3],
              alpha = 0.15
            ) +
            ggtitle("BarPlot of R-sq") +
            theme(
              text = element_text(size = 18)
            )+
          # geom_rect(
          #   mapping = aes(xmin = 0, xmax = r, ymin = r, ymax = 1),
          #   fill = psuPalette[3],
          #   alpha = 0.15
          # ) +
          annotate(
            geom = "text",
            x = r/2,
            y = 0.125,
            label = paste("R-sq =", round(r^2, digits = 3)),
            size = 5
          )
        },
      )
    }
  )
  
  
  ## regression line----
  output$fittedLine <- renderText({
    if (input$fittedRegression){
      paste(" fare =",
            48.971770,"+", "(", 0.219687 ,")", "* distance")
    }})
  
  
  ## fill in sentence in explore page 1 ----
  output$fill1 <- renderText({
    if (input$fillIN1){
      paste("According to the model, SST =",prettyNum(sst(),big.mark = ","),
            ", SSR =", prettyNum(max(0,ssr()),big.mark = ","), 
            ", and SSE =", prettyNum(sse(),big.mark = ","), ". Can you notice 
            the r quared value? Can you interpret it? ")
    }})
  
  
  
  # below for the explore 2 page
  ## update the model selection ----
  observeEvent(input$fullMODEL, {
  if (input$fullMODEL == "triceps + thigh + midarm") {
    updateSelectInput(
      session = session,
      inputId = "reducedMODEL",
      choices = list('Triceps + Thigh' = "triceps + thigh", 
                     'Triceps + Midarm' = "triceps + midarm", 
                     'Thigh + Midarm' = "thigh + midarm", 
                     'Triceps' = "triceps",
                     'Thigh' = "thigh", 'Midarm' = "midarm" )
    )
  } else if (input$fullMODEL == "triceps + thigh") {
    updateSelectInput(
      session = session,
      inputId = "reducedMODEL",
      choices = list('Triceps' = "triceps",
                     'Thigh' = "thigh")
    )
  } else if (input$fullMODEL == "triceps + midarm") {
    updateSelectInput(
      session = session,
      inputId = "reducedMODEL",
      choices = list('Triceps' = "triceps",
                     'Midarm' = "midarm")
    )
  } else if (input$fullMODEL == "thigh + midarm") {
    updateSelectInput(
      session = session,
      inputId = "reducedMODEL",
      choices = list('Thigh' = "thigh", 
                     'Midarm' = "midarm")
    )
  }
  })
  
  # observeEvent(
  #   eventExpr = input$fullMODEL,
  #   handlerExpr = {
  #     print(input$fullMODEL)
  #     temp1 <- lm(
  #       formula = as.formula(paste("bodyfat",input$fullMODEL,sep = " ~ " )),
  #       data = bodyData)
  #     print(summary(temp1))
  #   }
  # )
  # 
  # observeEvent(
  #   eventExpr = input$reducedMODEL,
  #   handlerExpr = {
  #     print(input$reducedMODEL)
  #     temp2 <- lm(
  #       formula = as.formula(paste("bodyfat",input$reducedMODEL,sep = " ~ " )),
  #       data = bodyData)
  #     print(summary(temp2))
  #   },
  #   ignoreInit = TRUE
  # )

  ##plot in partial R squared part----
  observeEvent(
    eventExpr = input$newSAMPLE,
    handlerExpr = {
      temp1 <- lm(
        formula = as.formula(paste("bodyfat",input$fullMODEL,sep = " ~ " )),
        data = bodyData)
      temp2 <- lm(
        formula = as.formula(paste("bodyfat",input$reducedMODEL,sep = " ~ " )),
        data = bodyData)
      r1 <- summary(temp1)$r.squared # rsq1 of the full model
      r2 <- summary(temp2)$r.squared # rsq2 of the reduced model
      output$partialPLOT <- renderPlot(
        expr = {
          validate(
            need(
              expr = !is.null(input$newSAMPLE), 
              message = "click on the New Sample button to see the plot "
            )
          )
          ggplot(
            data = bodyData,
            mapping = aes()
          ) +
            coord_fixed()+
            geom_rect(
              mapping = aes(
                xmin = 0,
                xmax = sqrt(r2),
                ymin = 0,
                ymax = sqrt(r2)),
              na.rm = FALSE,
              fill = psuPalette[2],
              color = psuPalette[2],
              alpha = 0.15
            ) +
            geom_rect(
              mapping = aes(
                xmin = sqrt(r2),
                xmax = sqrt(r1),
                ymin = 0,
                ymax = sqrt(r1)),
              na.rm = FALSE,
              fill = psuPalette[3],
              alpha = 0.15
            ) +
            geom_rect(
              mapping = aes(
                xmin = 0,
                xmax = sqrt(r1),
                ymin = sqrt(r2),
                ymax = sqrt(r1)),
              na.rm = FALSE,
              fill = psuPalette[3],
              alpha = 0.15
            ) +
            geom_rect(
              mapping = aes(
                xmin = 0,
                xmax = 1,
                ymin = 0,
                ymax = 1),
              na.rm = FALSE,
              color = "blue",
              fill = NA
            ) +
            ggtitle("plot of partial R squared") +
            theme_void() +
            theme(
              text = element_text(size = 18),
              legend.position = "bottom"
            ) 
        },
        alt = "FILL IN"
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  ##fill in sentence in explore page 2----
  
  observeEvent(
    eventExpr = input$newSAMPLE,
    handlerExpr = {
      temp1 <- lm(
        formula = as.formula(paste("bodyfat",input$fullMODEL,sep = " ~ " )),
        data = bodyData)
      temp2 <- lm(
        formula = as.formula(paste("bodyfat",input$reducedMODEL,sep = " ~ " )),
        data = bodyData)
      r1 <- summary(temp1)$r.squared # rsq1 of the full model
      r2 <- summary(temp2)$r.squared # rsq2 of the reduced model
      output$fill3 <- renderText({
        if (input$newSAMPLE){
          paste("R squared value of reduced model =",
                round(r2, digits = 3),
                ", R squared value of full model =",  round(r1, digits = 3), 
                "." )
        }})
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
  

  
  ## Set Go Button ----
  observeEvent(
    eventExpr = input$go1,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "prerequisites"
        
      )
    }
  )
  
  
  
  
  ## Set up Info button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = "This App Template will help you get started building your own app"
      )
    }
  )
  
  
  
}
# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
