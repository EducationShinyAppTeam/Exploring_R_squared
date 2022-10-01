# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(ggplot2)
library(dplyr)

# Load data----
baseData <- data.frame(
  Fare= c(54, 84, 90, 93, 111, 141, 162, 183, 207, 231, 291, 300, 309, 360, 360, 429, 477),
  Distance= c(90, 179, 184, 190, 270, 393, 502, 578, 681, 818, 1102, 1138, 1204, 1448, 1463, 1813, 1828)
) %>%
  mutate(yMean = mean(Fare))


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
      title = "Exploring R-squared", # You may use a shortened form of the title here
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
        menuItem("Explore R-squared", tabName = "Explore1", icon = icon("wpexplorer")),
        menuItem("Explore Partial R-squared", tabName = "Explore2", icon = icon("wpexplorer")),
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
          h1("Exploring R-squared"), # This should be the full name.
          p("R-squared is a statistical measure of how close the data are to the
            fitted regression line. This app allows you to explore it in both a 
            simple linear regression model and a multivariate model."),
          h2("Instructions"),
          br(),
          tags$ol(
            tags$li("Click on the Go button or the Prerequisites link in the
                    left menu to review the concepts of R-squared and partial R-squared."),
            tags$li("Click the Explore links to see how R-squared or Partial
                    R-squared work in particular examples.")
            
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
            thanks to Professor Neil Hatfield for coding help.",
            br(),
            br(),
            "Cite this app as:",
            br(),
            boastUtils::citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 9/30/2022 by JJH.")
          )
        ),
        #### Set up the Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Concepts of R-squared and Partial R-squared"),
          br(),
          box(
            title = strong("What is the R-squared value?"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "R-squared value, denoted as \\(R^2\\), is the regression sum of squares
            divided by the total sum of squares. \\(R^2\\) = SSR/SST = 1 - SSE/SST. 
            Because SSR ≤ SST, \\(R^2\\) will be between 0 and 1.
            If the value is equal to 1, it means that the model explains 
            all the variability of the response data around the mean. If 
            the value is equal to 0, it means that the model explains
            none of the variability of the response data around its mean. "
          ),
          box(
            title = strong("How to interpret the R-squared value?"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            p("ways to inteprate the \\(R^2\\)"),
            tags$ul(
              tags$li("We can say that 100*\\(R^2\\) percent of the variation in
                      \\(Y\\) is reduced by taking into account the predictor \\(X\\) 
                      (or a set of predictors \\(X_1\\), \\(X_2\\),... \\(X_k\\))."),
              tags$li("We can say 100*\\(R^2\\) percent of the variation in \\(Y\\) is
                      explained by the variation in the predictor \\(X\\)  (or a set of
                      predictors \\(X_1\\), \\(X_2\\),... \\(X_k\\)) ."),
              tags$li("As the square of the correlation coefficient, \\(R^2\\) can also 
                    be viewed as a measure of association between \\(X\\) and \\(Y\\)  
                    (irrespective of calling one variable the predictor and the 
                    other the response.")
            )
          ),
          box(
            title = strong("What is the Partial R-squared ? How to inteprete it?"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "Partial R-squared, also called the coefficient of partial determination, 
            measures the part of the variation in the response that cannot be explained
            by a reduced model  but can be explained by a full model. For example, 
            we might consider a full regression model that will include three predictors
            (\\(Y\\) ~ \\(X_1\\), \\(X_2\\), \\(X_3\\)), and a reduced model based 
            on \\(X_1\\) alone.  In that case, the partial R-squared gives us the 
            proportion of variation explained by all three predictors that cannot 
            be explained by  \\(X_1\\) alone.  Partial R-squared helps to explain
            what additional percent of variation can be explained by \\(X_2\\) 
            and \\(X_3\\), given the reduced model that only includes \\(X_1\\)."
          )
        ),
        
        
        #### Set up an Explore Page 1----
        tabItem(
          tabName = "Explore1",
          h2("R-squared"),
          tags$ul("Explore R-squared by Flightfare and Distance 
                  data. Also adjust the sliders below to change the intercept (β0) and 
                  coefficient (β1)."),
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
                  max = 0.28,
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
          h2("Partial R-squared"),
          tags$ul("Explore the Partial R-squared value by using the drop-down menu
                  below to select the reduced model and the full model from Bodyfat 
                  dataset."),
          br(),
          fluidRow(
            column(
              width = 4,
              wellPanel(
                h3("Data Info"),
                p("Bodyfat data contains 20 observations. Each observatioin includes
                  measurements of bodyfat and corresponding measurements of triceps,
                  thigh and midarm from the objects of study."),
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
              # h3("Illustraton:"),
              plotOutput(outputId = "partialPLOT"),
              tableOutput(outputId = "fill3"),
            ),
          ),
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
            Web application framework for R. (v 1.7.1). [R package].
            Available from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Pardoe , Iain. 2012. Applied Regression Modeling Cover Applied Regression Modeling,
            2nd Edition. Wiley."
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
          yHat = input$b0 + input$b1 * Distance
        )
    }
  )
  
  sst <- eventReactive(
    eventExpr = plotData(),
    valueExpr = {
      sum((plotData()$Fare - plotData()$yMean)^2)
      
    }
  )
  
  sse <- eventReactive(
    eventExpr = plotData(),
    valueExpr = {
      sum((plotData()$Fare - plotData()$yHat)^2)
    }
  )
  
  ssr <- eventReactive(
    eventExpr = plotData(),
    valueExpr = {
      # sum((plotData()$yHat - plotData()$yMean)^2)
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
  
  # below for the explore 1 page
  ## SST plot ----
  output$sstPLOT <- renderPlot(
    expr = {
      ggplot(
        data = plotData(),
        mapping = aes(x=Distance, y=Fare))+
        geom_point(na.rm = TRUE)+
        geom_hline(aes(yintercept = mean(Fare)), color="blue")+
        geom_rect(
          mapping = aes(
            ymin = Fare,
            ymax = yMean,
            xmin = Distance,
            xmax = abs(Fare-yMean) + Distance,
          ),
          alpha = .15,
          fill= psuPalette[4],
          col = psuPalette[4],
          na.rm = FALSE
        )+
        scale_x_continuous(
          limits = c(0, 2200),
          breaks = seq.int(from = 0, to = 2200, by = 200)
        )+
        scale_y_continuous(
          limits = c(0, 600),
          breaks = seq.int(from = 0, to = 600, by = 200)
        )+
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
            mapping = aes(x = Distance, y = Fare))+
            geom_point(na.rm = TRUE)+
            geom_abline(intercept = input$b0,slope = input$b1)+
            geom_hline(aes(yintercept = mean(Fare)), color = "blue")+
            geom_rect(
              mapping = aes(
                xmin = Distance,
                xmax = Distance + abs(yHat - yMean),
                ymin = yMean,
                ymax = yHat),
              na.rm = FALSE,
              alpha = .2,
              fill = psuPalette[2],
              col = psuPalette[2],
            )+
            scale_x_continuous(
              limits = c(0, 2200),
              breaks = seq.int(from = 0, to = 2200, by = 200),
            )+
            scale_y_continuous(
              limits = c(0, 600),
              breaks = seq.int(from = 0, to = 600, by = 200)
            )+
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
            mapping = aes(x = Distance, y = Fare))+
            geom_point(na.rm = TRUE)+
            geom_abline(intercept = input$b0,slope = input$b1)+
            geom_hline(aes(yintercept = mean(Fare)), color = "blue")+
            geom_rect(
              mapping = aes(
                xmin = Distance,
                xmax = Distance + abs(yHat - Fare),
                ymin = Fare,
                ymax = yHat),
              na.rm = FALSE,
              alpha = .25,
              fill = psuPalette[3],
              col = psuPalette[3],
            )+
            scale_x_continuous(
              limits = c(0, 2200),
              breaks = seq.int(from = 0, to = 2200, by = 200))+
            scale_y_continuous(
              limits = c(0, 600),
              breaks = seq.int(from = 0, to = 600, by = 200))+
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
  
  ## bar plot of R-squared----
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
            ggtitle("Bar plot of R-squared") +
            theme(
              text = element_text(size = 18)
            )+
            annotate(
              geom = "text",
              x = r/2,
              y = -0.1,
              label = paste("R-squared =", round(r^2, digits = 3)),
              size = 8
            ) + 
            scale_y_continuous(
              expand = expansion(mult = 0, add = c(0.15, 0))
            )
        },
      )
    }
  )
  
  
  ## regression line----
  output$fittedLine <- renderText({
    if (input$fittedRegression){
      paste(" fare =",
            48.97,"+", "(", 0.22 ,")", "* distance")
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
  
  
  ##plot in partial R-squared part----
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
            ggtitle("plot of partial R-squared") +
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
          paste( tags$ul(tags$li("The plot below will show the relationship between 
                                the full model and the reduced model in terms of R-squared."),
                         tags$li("The red square represents the proportion of variation 
                              explained by the reduced model. The area can be denoted
                              as R-squared value of the reduced model, which is equal
                              to =",round(r2, digits = 3),"."),
                         tags$li("The green square represents the proportion of variation 
                               explained by the full model. The area of green squared
                              can be denoted as R-squared value of the full model, which
                              is equal to =", round(r1, digits = 3),"."),
                         tags$li("The difference between two squares represents the 
                              variation cannot be explained by the reduced model but can be
                              explained by the full model. The area of difference can be
                              denoted as Partial R-squared.")
                         
          )
          )
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
