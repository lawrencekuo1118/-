# ui.R — restored from https://hopesmasher1118.shinyapps.io/GGplaystore2021/

dashboardPage(
  skin = "black",

  dashboardHeader(
    title = "2021 Google Playstore APP Analytics Dashboard",
    titleWidth = 350
  ),

  dashboardSidebar(
    width = 300,
    collapsed = TRUE,
    sidebarSearchForm(
      textId = "searchText",
      buttonId = "searchButton",
      label = "Search..."
    ),
    selectInput(
      "category",
      label = h4("Category"),
      choices = category_choices,
      selected = "Action"
    ),
    sliderInput(
      "rating",
      label = h4("App Rating"),
      min = 0,
      max = slider_max$rating,
      value = c(0, slider_max$rating),
      step = 0.1
    ),
    sliderInput(
      "price",
      label = h4("Price"),
      min = 0,
      max = slider_max$price,
      value = c(0, slider_max$price),
      step = 5
    ),
    sliderInput(
      "install.count",
      label = h4("Installs in total"),
      min = 0,
      max = slider_max$install.count,
      value = c(0, slider_max$install.count),
      step = 1000
    ),
    sliderInput(
      "performance",
      label = h4("App Performance"),
      min = 0,
      max = slider_max$performance,
      value = c(0, slider_max$performance),
      step = 10000
    ),
    sliderInput(
      "rating.count",
      label = h4("Rating Counts"),
      min = 0,
      max = slider_max$rating.count,
      value = c(0, slider_max$rating.count),
      step = 1000
    ),
    sliderInput(
      "app_size",
      label = h4("App Size in Mb"),
      min = 0,
      max = slider_max$app_size,
      value = c(0, slider_max$app_size),
      step = 1
    ),
    checkboxGroupInput(
      "ad.supported",
      label = h4("Ad.Supported"),
      choices = c("Yes" = "TRUE", "No" = "FALSE"),
      selected = c("TRUE", "FALSE"),
      inline = TRUE
    ),
    checkboxGroupInput(
      "in.app.purchases",
      label = h4("In.App.Purchases"),
      choices = c("Yes" = "TRUE", "No" = "FALSE"),
      selected = c("TRUE", "FALSE"),
      inline = TRUE
    ),
    checkboxGroupInput(
      "free_or_not",
      label = h4("Free / Paid"),
      choices = c("Free" = "TRUE", "Paid" = "FALSE"),
      selected = c("TRUE", "FALSE"),
      inline = TRUE
    ),
    checkboxGroupInput(
      "editors.choice",
      label = h4("Editors Choice"),
      choices = c("Yes" = "TRUE", "No" = "FALSE"),
      selected = c("TRUE", "FALSE"),
      inline = TRUE
    )
  ),

  dashboardBody(
    h2(h3("How's the APP perform in Google Playstore 2021?")),
    h2(h5("a LU-LEI KUO Shiny App")),

    valueBoxOutput("ibox1", width = 4),
    valueBoxOutput("ibox2", width = 4),
    valueBoxOutput("ibox3", width = 4),

    fluidRow(
      tabBox(
        title = "Data Graphs",
        side = "right",
        width = 12,
        id = "data_graphs",
        tabPanel(
          "General View",
          p("this section gives a general view on the raw data"),
          plotOutput("DefaultPlot", height = "300px"),
          DTOutput("DefaultTable"),
          hr(),
          fluidRow(
            column(6, plotOutput("CategoryPlot", height = "300px")),
            column(6, plotOutput("FreeOrNotPlot", height = "300px"))
          )
        ),
        tabPanel(
          "Basic Information",
          p("this section gives a basic view on each features"),
          verbatimTextOutput("app_category"),
          fluidRow(
            column(6, plotOutput("RatingPlot", height = "300px")),
            column(6, plotOutput("InstallationPlot", height = "300px"))
          ),
          hr(),
          fluidRow(
            column(6, plotOutput("PricePlot", height = "300px")),
            column(6, plotOutput("PricePaidPlot", height = "300px"))
          ),
          hr(),
          fluidRow(
            column(6, plotOutput("RatingCountPlot", height = "300px")),
            column(6, plotOutput("SizePlot", height = "300px"))
          ),
          hr(),
          plotOutput("PerformancePlot", height = "300px")
        ),
        tabPanel(
          "Value Analysis",
          p("this section calculates the approximate revenue of an app"),
          p("Exp.Revenue = Price * Install.Counts"),
          plotOutput("RevenuePlot", height = "300px"),
          hr(),
          p("the most profit way is to do or not do both..."),
          plotOutput("InAppPlot", height = "300px")
        ),
        tabPanel(
          "Performance",
          p("this section analyzes the app performances"),
          p("Performance = Rating * Install.Counts"),
          h4("Apps under Editors Choice"),
          DTOutput("EditorsChoiceTable"),
          hr(),
          h4("Our Best Performed Apps"),
          fluidRow(
            column(3, DTOutput("Top10AppTable", height = "300px")),
            column(
              9,
              plotOutput("Top10AppPlot", height = "300px"),
              plotOutput("BestRatingPlot", height = "300px"),
              plotOutput("BestInstallationPlot", height = "300px")
            )
          ),
          hr(),
          h4("TRASH APPs"),
          p("2nd part identified the worst performed apps, with Performance < 3000"),
          DTOutput("TrashAppTable")
        )
      )
    ),

    fluidRow(
      tabBox(
        title = "Statistic Facts",
        side = "right",
        width = 12,
        id = "statistic_facts",
        tabPanel(
          "Data Summary",
          p("this section only apply on the 5 continuous numeric features: Price, Rating, Rating.Counts, Size.MB, and the target, Install.Count"),
          verbatimTextOutput("summary")
        ),
        tabPanel(
          "Correlation Heatmap",
          plotOutput("CorHeatmap", height = "300px")
        )
      )
    )
  )
)
