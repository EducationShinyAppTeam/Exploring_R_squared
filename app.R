# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(ggplot2)
library(dplyr)
library(shinya11y)

# Create data frames ----
baseData <- data.frame(
  Fare = c(54, 84, 90, 93, 111, 141, 162, 183, 207, 231, 291, 300, 309, 360,
           360, 429, 477),
  Distance = c(90, 179, 184, 190, 270, 393, 502, 578, 681, 818, 1102, 1138, 
              1204, 1448, 1463, 1813, 1828)
) %>%
  mutate(yMean = mean(Fare))

bodyData <- data.frame(
  triceps = c(19.5, 24.7, 30.7, 29.8, 19.1, 25.6, 31.4, 27.9,22.1, 25.5, 31.1,
              30.4, 18.7, 19.7, 14.6, 29.5, 27.7, 30.2, 22.7, 25.2),
  thigh = c(43.1, 49.8, 51.9, 54.3, 42.2, 53.9, 58.5, 52.1, 49.9, 53.5, 56.6,
            56.7, 46.5, 44.2, 42.7, 54.4, 55.3, 58.6, 48.2, 51),
  midarm = c(29.1, 28.2, 37, 31.1, 30.9, 23.7, 27.6, 30.6, 23.2, 24.8, 30, 28.3,
             23, 28.6, 21.3, 30.1, 25.7, 24.6, 27.1, 27.5),
  bodyfat = c(11.9, 22.8, 18.7, 20.1, 12.9, 21.7, 27.1, 25.4, 21.3, 19.3, 25.4,
              27.2, 11.7, 17.8, 12.8, 23.9, 22.6, 25.4, 14.8, 21.1)
) %>%
  mutate(fatMean = mean(bodyfat))

# Define UI ----
ui <- list(
  dashboardPage(
    skin = "black",
    ## Header ----
    dashboardHeader(
      title = "Exploring R-squared", 
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Exploring_R_Squared")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("house")
        )
      )
    ),
    ## Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("gauge-high")),
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
    ## Body ----
    dashboardBody(
      use_tota11y(),
      tabItems(
        ### Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Exploring R-squared"),
          p("R-squared, \\(R^2\\) is a statistical measure of how close the data
            are to a fitted regression line. This app allows you to explore this
            measure in both simple and multiple linear regression models."),
          h2("Instructions"),
          br(),
          tags$ol(
            tags$li("Click on the Go button or the Prerequisites link in the
                    left menu to review the concepts of R-squared and partial
                    R-squared."),
            tags$li("Click the Explore links to see how R-squared or Partial
                    R-squared work in particular examples.")
          ),
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
            div(class = "updated", "Last Update: 8/4/2023 by NJH.")
          )
        ),
        ### Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Concepts of R-squared and Partial R-squared"),
          br(),
          box(
            title = strong("What is the SST, SSR, and SSE?"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            tags$ul(
              tags$li(
                strong("SST"), "is called the total sum of squares (or Sum of 
                Squares-Total) and quantifies how much the observed data points 
                vary around the mean of the independent variable/response."
              ),
              tags$li(
                strong("SSR"), "is called the regression sum of squares (or Sum
                of Squares-Regression) and quantifies how far the estimates that
                make up the regression line are from the mean of the independent 
                variable/response."
              ),
              tags$li(
                strong("SSE"),"is called the error sum of squares (or Sum of 
              Squares-Error) and quantifies how much the observated data points 
              vary around the estimated regression line."
              ),
            )
          ),
          box(
            title = strong("What is the R-squared value?"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "R-squared value, denoted as \\(R^2\\), is the regression sum of 
            squares divided by the total sum of squares: \\(R^2\\) = SSR/SST =
            1 - SSE/SST. Because SSR ≤ SST, \\(R^2\\) will be between 0 and 1.
            If the \\(R^2=1\\), then the model explains all the variability of 
            the response data around the mean. If \\(R^2=0\\), then the model
            explains none of the variability of the response data around its mean. "
          ),
          box(
            title = strong("How to interpret the R-squared value?"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            p("Ways to inteprate the \\(R^2\\)"),
            tags$ul(
              tags$li("We can say that 100*\\(R^2\\) percent of the variation in
                      \\(Y\\) is reduced by taking into account the predictor 
                      \\(X\\) (or a set of predictors \\(X_1, X_2,\\ldots,X_k\\))."),
              tags$li("We can say 100*\\(R^2\\) percent of the variation in \\(Y\\) is
                      explained by the variation in the predictor \\(X\\) (or a
                      set of predictors \\(X_1, X_2,\\ldots, X_k\\))."),
              tags$li("As the square of the correlation coefficient, \\(R^2\\) 
                      can also be viewed as a measure of association between
                      \\(X\\) and \\(Y\\) (irrespective of calling one variable
                      the predictor and the other the response).")
            )
          ),
          box(
            title = strong("What is the Partial R-squared? How to inteprete it?"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "Partial R-squared, also called the coefficient of partial determination, 
            measures the part of the variation in the response that cannot be
            explained by a reduced model but can be explained by a full model.
            For example, we might consider a full regression model that includes
            three predictors (\\(X_1\\), \\(X_2\\), and \\(X_3\\)) and a reduced
            model that only has \\(X_1\\). The partial R-squared gives us the 
            proportion of variation explained by all three predictors that cannot 
            be explained by \\(X_1\\) alone. Partial R-squared helps to explain
            what additional percent of variation can be explained by \\(X_2\\) 
            and \\(X_3\\), given the reduced model that only includes \\(X_1\\)."
          )
        ),
        ### Explore R-squared ----
        tabItem(
          tabName = "Explore1",
          withMathJax(),
          h2("R-squared"),
          p("Explore R-squared by Flightfare and Distance data. Adjust the sliders
            below to change the intercept \\(\\beta_0\\) and the coefficient
            \\(\\beta_1\\)."),
          br(),
          box(
            title = strong("Data info"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "This data contains 17 observations of flightfare and related flight
            distance."
          ),
          br(),
          fluidRow(
            column(
              width = 4,
              wellPanel(
                br(),
                sliderInput(
                  inputId = "b0",
                  label = "Intercept, \\(\\beta_0\\):",
                  min = 0,
                  max = 78,
                  value = 0
                ),
                br(),
                sliderInput(
                  inputId = "b1",
                  label = "Coefficient/slope, \\(\\beta_1\\):",
                  min = 0.14,
                  max = 0.28,
                  value = 0.14
                  ),
                br(),
                checkboxInput(
                  inputId = "fittedRegression",
                  label = "Show the fitted regression line",
                  value = FALSE,
                  width = "100%"
                ),
                tableOutput(outputId = "fittedLine"),
              ),
            ),
            column(
              width = 8,
              plotOutput(outputId = "sstPlot"),
            )
          ),
          br(),
          fluidRow(
            column(
              width = 6,
              plotOutput(outputId = "ssrPlot"),
            ),
            column(
              width = 6,
              plotOutput(outputId = "ssePlot"),
            )
          ),
          br(),
          fluidRow(
            column(
              width = 11,
              offset = 1,
              plotOutput(outputId = "squarePlot", height = "100px"),
            )
          ),
          br(),
          checkboxInput(
            inputId = "fillIn1",
            label = "check to see the value of SST and SSR",
            value = FALSE,
            width = "100%"
          ),
          tableOutput(outputId = "fill1")
        ),
        ### Explore Partial R-square----
        tabItem(
          tabName = "Explore2",
          h2("Partial R-squared"),
          p("Explore the Partial R-squared value by using the drop-down menu 
            below to select the reduced model and the full model from Bodyfat 
            dataset."),
          br(),
          box(
            title = strong("Data Info"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "The BodyFat dataset used here is a portion of the data for a study 
            of the realtion of amount of body fat (percentage) to several
            predictor variables which includes triceps skinfold thickness (mm),
            thigh circumference (cm), and midarm circumference (cm). There are
            20 people included in the dataset."
          ),
          br(),
          fluidRow(
            column(
              width = 4,
              wellPanel(
                selectInput(
                  inputId = 'fullModel',
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
                  inputId = 'reducedModel',
                  label = 'Select a reduced model',
                  choices = c("triceps + thigh + midarm")
                ),
                bsButton(
                  inputId = "newSample", 
                  label = "Compare!", 
                  icon = icon("retweet"),
                  size = "large",
                  style = "default"
                ),
              )
            ),
            column(
              width = 8,
              plotOutput(outputId = "partialPlot"),
              tableOutput(outputId = "fill3"),
            ),
          ),
        ),
        ### References Page ----
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
            "Kutner, Michael H., Christopher J. Nachtsheim, John Neter, and William Li. 
            2005. Applied Linear Statistical Models. Boston: McGraw-Hill Irwin. "
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
      sst() - sse()
    }
  )
  
  dataCollection <- eventReactive(
    eventExpr =  c(input$fullModel, input$reducedModel),
    valueExpr = {
      switch(
        EXPR = input$selectData,
        MEDData = MEDData,
        PERSONData = PERSONData
      )
    }
  )
  
  # below for the explore 1 page ----
  ## SST plot ----
  output$sstPlot <- renderPlot(
    expr = {
      ggplot(
        data = plotData(),
        mapping = aes(x = Distance, y = Fare))+
        geom_point(na.rm = TRUE)+
        geom_hline(aes(yintercept = mean(Fare)), color = "blue")+
        geom_rect(
          mapping = aes(
            ymin = Fare,
            ymax = yMean,
            xmin = Distance,
            xmax = abs(Fare - yMean) + Distance,
          ),
          alpha = .15,
          fill = psuPalette[4],
          col = psuPalette[4],
          na.rm = FALSE
        ) +
        scale_x_continuous(
          limits = c(0, 2200),
          breaks = seq.int(from = 0, to = 2200, by = 200)
        ) +
        scale_y_continuous(
          limits = c(0, 600),
          breaks = seq.int(from = 0, to = 600, by = 200)
        ) +
        theme_bw() +
        labs(
          title = "Plot of SST",
          x = "Distance (miles)",
          y = "Fare ($)"
        ) +
        theme(
          text = element_text(size = 18),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
        )
    },
    alt = "Dots are the price and distance of flights; horizontal line is the 
    mean fare. The rectangles show the difference between the actual price and
    the mean price for each flight"
  )
  
  ## SSR plot----
  observeEvent(
    eventExpr = plotData(),
    handlerExpr = {
      output$ssrPlot <- renderPlot(
        expr = {
          ggplot(
            data = plotData(),
            mapping = aes(x = Distance, y = Fare)
            ) +
            geom_point(na.rm = TRUE) +
            geom_abline(intercept = input$b0,slope = input$b1) +
            geom_hline(aes(yintercept = mean(Fare)), color = "blue") +
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
            ) +
            scale_x_continuous(
              limits = c(0, 2200),
              breaks = seq.int(from = 0, to = 2200, by = 200),
            ) +
            scale_y_continuous(
              limits = c(0, 600),
              breaks = seq.int(from = 0, to = 600, by = 200)
            ) +
            theme_bw() +
            labs(
              title = "Plot of SSR",
              x = "Distance (miles)",
              y = "Fare ($)"
            ) +
            theme(
              text = element_text(size = 18),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
            )
        },
        alt = "Dots are the price and distance of flights; horizontal line is the 
    mean fare; the regression line responds to the controls; The rectangles show
    the difference between the estimated and mean price for each flight"
      )
    }
  )
  
  ## SSE plot----
  observeEvent(
    eventExpr = plotData(),
    handlerExpr = {
      output$ssePlot <- renderPlot(
        expr = {
          ggplot(
            data = plotData(),
            mapping = aes(x = Distance, y = Fare)) +
            geom_point(na.rm = TRUE) +
            geom_abline(intercept = input$b0,slope = input$b1) +
            geom_hline(aes(yintercept = mean(Fare)), color = "blue") +
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
            ) +
            scale_x_continuous(
              limits = c(0, 2200),
              breaks = seq.int(from = 0, to = 2200, by = 200)) +
            scale_y_continuous(
              limits = c(0, 600),
              breaks = seq.int(from = 0, to = 600, by = 200)) +
            theme_bw() +
            labs(
              title = "Plot of SSE",
              x = "Distance (miles)",
              y = "Fare ($)"
            ) +
            theme(
              text = element_text(size = 18),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
            )
        },
        alt = "Dots are the price and distance of flights; horizontal line is the 
    mean fare; the regression line responds to the controls; The rectangles show
    the difference between the actual and estimated prices for each flight"
      )
    }
  )
  
  ## bar plot of R-squared----
  observeEvent(
    eventExpr = plotData(),
    handlerExpr = {
      r = sqrt(ssr()/sst())
      output$squarePlot <- renderPlot(
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
            ggtitle(bquote("Bar plot of" ~ R^2)) +
            theme(
              text = element_text(size = 18)
            ) +
            annotate(
              geom = "text",
              x = r/2,
              y = -0.1,
              label = bquote(R^2 ~ "=" ~ .(round(r^2, digits = 3))),
              size = 6
            ) + 
            scale_y_continuous(
              expand = expansion(mult = 0, add = c(0.15, 0))
            )
        },
        alt = "The full bar represents all of the variation in the data. The left
        portion shows what proportion, R-squared, our model explains"
      )
    }
  )
  
  
  ## regression line----
  output$fittedLine <- renderText({
    if (input$fittedRegression) {
      paste(" Fare =",
            48.97,"+", "(", 0.22 ,")", "* Distance")
    }
  }
  )
  
  ## fill in sentence in explore page 1 ----
  output$fill1 <- renderText({
    if (input$fillIn1) {
      paste("According to the model, SST =",prettyNum(sst(),big.mark = ","),
            ", SSR =", prettyNum(max(0,ssr()),big.mark = ","), 
            ", and SSE =", prettyNum(sse(),big.mark = ","), ". Can you notice 
            the R-squared value? Can you interpret it? ")
    }
  }
  )
  
  
  # below for the explore 2 page ----
  ## update the model selection ----
  observeEvent(
    eventExpr = input$fullModel, 
    handlerExpr = {
      newChoices <- switch(
        EXPR = input$fullModel,
        "triceps + thigh + midarm" = list('Triceps + Thigh' = "triceps + thigh", 
                                          'Triceps + Midarm' = "triceps + midarm", 
                                          'Thigh + Midarm' = "thigh + midarm", 
                                          'Triceps' = "triceps",
                                          'Thigh' = "thigh", 'Midarm' = "midarm"),
        "triceps + thigh" = list('Triceps' = "triceps", 'Thigh' = "thigh"),
        "triceps + midarm" = list('Triceps' = "triceps", 'Midarm' = "midarm"),
        "thigh + midarm" = list('Thigh' = "thigh", 'Midarm' = "midarm")
      )
      updateSelectInput(
        session = session,
        inputId = "reducedModel",
        choices = newChoices
      )
    }
  )
  
  
  ##plot in partial R-squared part----
  observeEvent(
    eventExpr = input$newSample,
    handlerExpr = {
      temp1 <- lm(
        formula = as.formula(paste("bodyfat", input$fullModel, sep = " ~ " )),
        data = bodyData
      )
      temp2 <- lm(
        formula = as.formula(paste("bodyfat", input$reducedModel, sep = " ~ " )),
        data = bodyData
      )
      r1 <- summary(temp1)$r.squared # rsq1 of the full model
      r2 <- summary(temp2)$r.squared # rsq2 of the reduced model
      output$partialPlot <- renderPlot(
        expr = {
          validate(
            need(
              expr = input[["newSample"]][1] > 0,
              message = "Each time click on the Compare! button to see the new 
              visualization of Partial R-squared after choosing new models."
            )
          )
          ggplot(
            data = bodyData,
            mapping = aes()
          ) +
            coord_fixed() +
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
            ggtitle("Visualization of Partial R-squared") +
            theme_void() +
            theme(
              text = element_text(size = 18),
              legend.position = "bottom"
            ) 
        },
        alt = "The outer square shows the explaining all variation. The inner
        filled square shows variation explained by reduced model. The middle filled
        square shows variation exlained by full model"
      )
    },
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )
  
  ##fill in sentence in explore page 2----
  observeEvent(
    eventExpr = input$newSample,
    handlerExpr = {
      temp1 <- lm(
        formula = as.formula(paste("bodyfat",input$fullModel,sep = " ~ " )),
        data = bodyData
      )
      temp2 <- lm(
        formula = as.formula(paste("bodyfat",input$reducedModel,sep = " ~ " )),
        data = bodyData
      )
      r1 <- summary(temp1)$r.squared # rsq1 of the full model
      r2 <- summary(temp2)$r.squared # rsq2 of the reduced model
      output$fill3 <- renderText({
        if (input$newSample) {
          paste("The visualization above shows the relationship between the full
                model and the reduced model in terms of R-squared. The blue edge
                square represents all possible variation has been explained.",
                "The red square represents the proportion of variation explained
                by the reduced model. The area of red square can be thought of as
                the R-squared value for the reduced model, which is equal to ",
                strong(round(r2, digits = 3),"."), "The green square which is
                under the red square represents the proportion of variation 
                explained by the full model. The area of green squared can be
                thought of as the R-squared value of the full model, which is 
                equal to ", strong(round(r1, digits = 3),"."), "The difference 
                between the areas of two squares represents the variation cannot
                be explained by the reduced model but can be explained by the full 
                model. This difference in area (proportion of variance explained)
                represents the value of Partial R-squared."
          )
        }
      }
      )
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
        text = "This App helps you learn the materials about R-squared. "
      )
    }
  )
}
# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
