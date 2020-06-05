library(shinycssloaders)

Sys.setlocale(category = "LC_ALL", locale = "cht")

header <- dashboardHeader(
  title = "Capstone Project",
  titleWidth = 180,
  dropdownMenu(
    type = 'messages',
    messageItem(
      from = NULL,
      icon = icon(NULL),
      message = "Welcome to the Textile Trend Dashboard!"
    ),
    messageItem(
      from = NULL,
      icon = icon(NULL),
      message = "Learn More About our Project Here!",
      href = "http://www.google.com"
    )
  )
)

sidebar <-
  dashboardSidebar(
    width = 180,
    sidebarMenu(
      id = "sidebar",
      menuItem(
        text = "初期嘗試",
        tabName = "slide1",
        icon = icon("shopping-cart")),
      menuItem(
        text = "市場趨勢",
        tabName = "dashboard1",
        icon = icon("shopping-cart")
      ),
      conditionalPanel(
        "input.sidebar == 'dashboard1'",
        selectInput(
          "dt_range1",
          "Date Range",
          choices = c("Past 30 Days", "Past 90 Days", "Past Year", "This Year"),
          selected = "Past Year"
        ),
        checkboxGroupInput(
          "gender1",
          "Gender",
          choices = c('Men', 'Women'),
          inline = T,
          selected = 'Women'
        )
      ),
      menuItem(
        text = "品牌趨勢",
        tabName = "dashboard2",
        icon = icon("chart-line")
      ),
      conditionalPanel(
        "input.sidebar == 'dashboard2'",
        selectInput(
          "dt_range2",
          "Date Range",
          choices = c("Past 30 Days", "Past 90 Days", "Past Year", "This Year"),
          selected = "Past Year"
        ),
        checkboxGroupInput(
          "gender2",
          "Gender",
          choices = c('Men', 'Women'),
          inline = T,
          selected = 'Women'
        ),
        checkboxGroupInput(
          "brand",
          "Brands",
          choices = c("Nike",
                      "Adidas",
                      "Under Armour",
                      "Lululemon"),
          inline = T,
          selected = "Nike"
        ),
        tags$head(tags$style(
          HTML(
            ".checkbox-inline {
                    margin-left: 0px;
                    margin-right: 10px;
          }
         .checkbox-inline+.checkbox-inline {
                    margin-left: 0px;
                    margin-right: 10px;
          }
        "
          )
        ))
      ),
      menuItem(
        text = "目標校正",
        tabName = "dashboard3",
        icon = icon("globe-americas")
      ),
      menuItem(
        text = "WEAVA媒合平台",
        tabName = "dashboard4",
        icon = icon("rss-square")
      ),
      menuItem(
        text = "未來展望",
        tabName = "dashboard5",
        icon = icon("clipboard")
      ),
      menuItem(
        text = "About",
        tabName = "dashboard6",
        icon = icon("info-circle")
      )
    )
  )

body <-
  dashboardBody(tabItems(
    tabItem(
      tabName = "slide1",
      tabBox(
        width=12,
        tabPanel("布廠目前經營問題"),
        tabPanel("初步構想"))
    ),
    tabItem(
      tabName = "dashboard1",
      fluidRow(
        column(width = 8, shinycssloaders::withSpinner(
          plotlyOutput("trend_line", height = "280px")
        )),
        column(
          width = 4,
          h4("What's Trending", style = "text-align: center;"),
          shinycssloaders::withSpinner(dataTableOutput("trending", height = "280px"))
        )
      ),
      fluidRow(
        column(
          width = 4,
          align = "center",
          h4("Features"),
          checkboxGroupInput(
            "sel_features",
            label = NULL,
            choices = c(
              'Comfort',
              'Functionality',
              'Structure',
              'Sustainable',
              'Trends'
            ),
            selected = 'Functionality',
            inline = T
          ),
          shinycssloaders::withSpinner(plotOutput("feature_pie", height = "230px"))
        ),
        column(
          width = 8,
          h4("Materials", style = "text-align: center;"),
          shinycssloaders::withSpinner(plotlyOutput("material_bar", height = "280px"))
        )
      )
    ),
    tabItem(tabName = "dashboard2",
            fluidRow(column(
              width = 8,
              h4("Materials", style = "text-align: center;"),
              shinycssloaders::withSpinner(plotlyOutput("brand_trend", height = "310px"))
            ))),
    tabItem(tabName = "dashboard3",
            # ,fluidRow(
            #   column(width = 4, align = "center",
            #          bsButton('nylon', "Nylon", style = 'primary', type = 'toggle', size = 'large'),
            #          conditionalPanel('input.nylon != 0',
            #                           plotlyOutput("nylon_bar", height = "300px"))),
            #   column(width = 4, align = "center",
            #          bsButton('polyester', "Polyester", style = 'primary', type = 'toggle', size = 'large'),
            #          conditionalPanel('input.polyester != 0',
            #                           plotlyOutput("polyester_bar", height = "300px"))),
            #   column(width = 4, align = "center",
            #          bsButton('others', "Others", style = 'primary', type = 'toggle', size = 'large'),
            #          conditionalPanel('input.others != 0',
            #                           plotlyOutput("others_bar", height = "300px")))),
            # br(),
            # fluidRow(column(
            #   width = 12,
            #   shinycssloaders::withSpinner(
            #     leafletOutput("trade_map", height = "300px"),
            #     type = 6,
            #     size = 0.6
            #   )
            # ))
            tabBox(
              width=12,
              tabPanel("問題與發現"),
              tabPanel("驗證假設"),
              tabPanel("行動方案"))
            ),
    tabItem(tabName = "dashboard4",
            tabBox(
              width=12,
              tabPanel("平台功能"),
              tabPanel("品牌方使用歷程"),
              tabPanel("廠商使用歷程"),
              tabPanel("設計師使用歷程"))
            ),
    tabItem(
      tabName = "dashboard5",
      fluidRow(column(width = 12, h4(
        "Summary & Link to Our Report"
      ))),
      downloadButton('download', "Download Reports")
    ),
    tabItem(tabName = "dashboard6",
            fluidRow(column(
              width = 12,
              h3("2020 Capstone Project - Textile Trend Discovery"),
              h4("Prof. Chen, Wun-Hwa")
            )))
  ))

options(
  spinner.size = 0.5,
  spinner.color = "#A9A9A9",
  spinner.type = 7
)
