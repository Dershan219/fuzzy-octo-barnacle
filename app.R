library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyBS)
library(shinypop)
library(shinyalert)
library(ggplot2)
library(ggsci)
library(ggpubr)
library(ggrepel)
library(plotly)
library(forcats)
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(DT)
library(scales)
library(lubridate)
library(leaflet)
library(hash)

# preliminary------------------------------------------------------------------------------------------------------------
Sys.setlocale(category = "LC_ALL", locale = "cht")

hashmap <- hash()
hashmap[c(
  "防水",
  "透濕",
  "撥水",
  "防風機能",
  "反射保溫機制",
  "羽絨",
  "化纖素材",
  "刷毛布",
  "中空保溫纖維",
  "保暖禦寒",
  "溼度管理",
  "溫度調節",
  "遮蔽性涼感",
  "單向導溼快乾",
  "結構性透氣",
  "蒸發性涼感",
  "涼爽透氣",
  "接觸涼感(非金屬類)",
  "接觸涼感(金屬類)"
)] <- 'Comfort'
hashmap[c("人體工學舒適設計",
          "四面彈",
          "提升運動效能設計",
          "一體成型",
          "輕量",
          "舒適伸展",
          "肌肉支持",
          "舒適平滑縫合",
          "梭織彈性")] <- 'Structure'
hashmap[c("反光功能", "抗紫外線", "異味控制", "抗菌功能")] <- 'Functionality'
hashmap[c("環保訴求")] <- 'Sustainable'
hashmap[c("Athleisure", "3D列印", "數位印花", "超輕量")] <- 'Trends'

source(
  "https://raw.githubusercontent.com/Dershan219/textile-dashboard/master/capstone_layout.R",
  local = F
)
source(
  "https://raw.githubusercontent.com/Dershan219/textile-dashboard/master/capstone_func.R",
  local = F
)

# ecommerce_data---------------------------------------------------------------------------------------------------------
men_data_amazon <-
  fread(
    "https://raw.githubusercontent.com/Dershan219/textile-dashboard/master/data/Amazon_leggings_men.csv",
    encoding = 'UTF-8',
    drop = 1
  ) %>% mutate(
    Materials = paste(
      Material_1,
      Material_2,
      Material_3,
      Material_4,
      Material_5,
      sep = ','
    ),
    Features = paste(
      Feature_1,
      Feature_2,
      Feature_3,
      Feature_4,
      Feature_5,
      Feature_6,
      Feature_7,
      Feature_8,
      Feature_9,
      Feature_10,
      Feature_11,
      sep = ','
    )
  ) %>% select(-8:-23)
women_data_amazon <-
  fread(
    "https://raw.githubusercontent.com/Dershan219/textile-dashboard/master/data/Amazon_leggings_women.csv",
    encoding = 'UTF-8',
    drop = 1
  ) %>% mutate(
    Materials = paste(
      Material_1,
      Material_2,
      Material_3,
      Material_4,
      Material_5,
      Material_6,
      sep = ','
    ),
    Features = paste(
      Feature_1,
      Feature_2,
      Feature_3,
      Feature_4,
      Feature_5,
      Feature_6,
      Feature_7,
      Feature_8,
      Feature_9,
      sep = ','
    )
  ) %>% select(-8:-22)
amazon_data <- rbind(men_data_amazon, women_data_amazon)

men_data_dicks <-
  fread(
    "https://raw.githubusercontent.com/Dershan219/textile-dashboard/master/data/2019_dicks_men.csv",
    encoding = "UTF-8"
  ) %>% mutate(
    Materials = paste(
      Material_1,
      Material_2,
      Material_3,
      Material_4,
      Material_5,
      sep = ','
    ),
    Features = paste(
      Feature_1,
      Feature_2,
      Feature_3,
      Feature_4,
      Feature_5,
      Feature_6,
      Feature_7,
      sep = ','
    )
  ) %>% select(-9:-20)
women_data_dicks <-
  fread(
    "https://raw.githubusercontent.com/Dershan219/textile-dashboard/master/data/2019_dicks_women.csv",
    encoding = "UTF-8"
  ) %>% mutate(
    Materials = paste(
      Material_1,
      Material_2,
      Material_3,
      Material_4,
      Material_5,
      Material_6,
      Material_7,
      Material_8,
      sep = ','
    ),
    Features = paste(
      Feature_1,
      Feature_2,
      Feature_3,
      Feature_4,
      Feature_5,
      Feature_6,
      Feature_7,
      Feature_8,
      Feature_9,
      sep = ','
    )
  ) %>% select(-9:-25)
dicks_data <- rbind(men_data_dicks, women_data_dicks)

# brand_data-------------------------------------------------------------------------------------------------------------
brand_data <-
  fread(
    "https://raw.githubusercontent.com/Dershan219/textile-dashboard/master/data/brand_leggings.csv",
    encoding = 'UTF-8'
  )
brand_data$Features <-
  brand_data$Features %>% str_replace_all("\\r\n", ".") %>%
  str_extract_all('(?<=,)([^,.]+)(?=.)') %>% lapply(., paste, collapse = ',') %>% unlist()
brand_data$Materials <-
  brand_data$Materials %>% str_replace_all("\\r\n", ".") %>%
  str_extract_all('(?<=,)([^,.]+)(?=.)') %>% lapply(., paste, collapse = ',') %>% unlist()

# supply_data------------------------------------------------------------------------------------------------------------
supply_data <-
  fread(
    'https://raw.githubusercontent.com/Dershan219/textile-dashboard/master/data/supply_data.csv',
    header = T,
    encoding = 'UTF-8'
  ) %>% na_if(0)

# shiny app--------------------------------------------------------------------------------------------------------------
ui <-
  dashboardPage(header <- header,
                sidebar <- sidebar,
                body <- body)

server <- function(input, output) {
  # amazon trend-----------------------------------------------------------------------------------------------------------
  trend_line_df <- reactive({
    amazon_data %>% subset(Gender %in% tolower(input$gender1)) %>% get_material_trend(input$dt_range1) %>% subset(count >=
                                                                                                                    5)
  })
  
  feature_pie_df <- reactive({
    amazon_data %>% subset(Gender %in% tolower(input$gender1)) %>% get_features(input$dt_range1, dict=hashmap)
  })
  
  material_bar_df <- reactive({
    amazon_data %>% subset(Gender %in% tolower(input$gender1)) %>% materials_features(input$dt_range1, input$sel_features, dict=hashmap) %>% subset(Freq >= 20)
  })
  
  trend_line <- reactive({
    shiny::validate(need(nrow(trend_line_df()) > 0, "No Data Avaliable"))
    trend_line_df() %>% ungroup() %>% ggplot(aes(
      x = as.Date(Listed_Date),
      y = count,
      group = Materials,
      color = Materials,
      text = paste(
        'Date: ',
        Listed_Date,
        '<br>Material: ',
        Materials,
        '<br>Products: ',
        count
      )
    )) +
      geom_line() + scale_x_date(date_labels = "%m-%y") + labs(x = NULL, y = 'Products') + theme_minimal() +
      theme(legend.title = element_blank()) + scale_color_manual(values = c(get_palette("jco", length(
        unique(trend_line_df()$Materials)
      ))))
  })
  
  feature_pie <- reactive({
    shiny::validate(need(nrow(feature_pie_df()) > 0, "No Data Available"))
    feature_pie_df() %>% ggplot(aes(
      ymax = ymax,
      ymin = ymin,
      xmax = 4,
      xmin = 3,
      fill = .
    )) +
      geom_rect() + coord_polar(theta = "y") + xlim(c(0, 4)) + theme_void() +
      geom_text_repel(
        x = 5,
        aes(
          y = (ymax + ymin) / 2,
          label = paste0(., "\n", percent(pct, accuracy = 0.1))
        ),
        size = 4,
        fontface = "bold",
        segment.color = 'transparent',
        color = 'gray20'
      ) +
      scale_fill_manual(values = get_palette("Blues", 5)) +
      theme(
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0, 0, 0, 0), "mm")
      ) + labs(x = NULL, y = NULL)
  })
  
  material_bar <- reactive({
    shiny::validate(need(nrow(material_bar_df()) > 0, "No Data Avaliable"))
    material_bar_df() %>% ggplot(aes(
      x = reorder(., Freq),
      y = Freq,
      text = paste('Products: ', Freq)
    )) +
      geom_bar(stat = 'identity', fill = "#3C8DBC") + coord_flip() + labs(x = NULL, y = 'Products') +
      theme_minimal() + theme(plot.margin = unit(c(0, 0, 2, 0), "mm"))
  })
  
  output$trend_line <- renderPlotly({
    ggplotly(trend_line(), tooltip = c('text')) %>% layout(plot_bgcolor = 'rgba(0, 0, 0, 0)', paper_bgcolor = 'rgba(0, 0, 0, 0)')
  })
  
  output$trending <- renderDataTable({
    trend_line_df() %>% get_trending() %>% select(-2) %>% head(5)
  }, options = list(pageLength = 5, dom = 't'), rownames = F)
  
  output$feature_pie <- renderPlot({
    feature_pie()
  }, bg = "transparent", execOnResize = T)
  
  output$material_bar <- renderPlotly({
    ggplotly(material_bar(), tooltip = 'text') %>% layout(plot_bgcolor = 'rgba(0, 0, 0, 0)', paper_bgcolor = 'rgba(0, 0, 0, 0)')
  })
  
  # brand trend------------------------------------------------------------------------------------------------------------
  brand_trend_df <- reactive({
    brand_data %>% subset((Gender %in% tolower(input$gender2)) &
                            (tolower(Brand) %in% tolower(input$brand))) %>% get_materials(input$dt_range2)
  })
  
  brand_trend <- reactive({
    shiny::validate(need(nrow(brand_trend_df()) > 0, "No Data Avaliable"))
    brand_trend_df() %>% ggplot(aes(
      x = reorder(., Freq),
      y = Freq,
      text = paste('Products: ', Freq)
    )) +
      geom_bar(stat = 'identity', fill = "#3C8DBC") + coord_flip() + labs(x = NULL, y = 'Products') +
      theme_minimal() + theme(plot.margin = unit(c(0, 0, 2, 0), "mm"))
  })
  
  output$brand_trend <- renderPlotly({
    ggplotly(brand_trend(), tooltip = c('text')) %>%
      layout(plot_bgcolor = 'rgba(0, 0, 0, 0)', paper_bgcolor = 'rgba(0, 0, 0, 0)')
  })
  
  # supply & demand--------------------------------------------------------------------------------------------------------
  # output$nylon_bar <- renderPlotly({
  #   ggplotly(plot_supply_bar('Nylon'), tooltip = c('text')) %>%
  #     layout(
  #       plot_bgcolor = 'rgba(0, 0, 0, 0)',
  #       paper_bgcolor = 'rgba(0, 0, 0, 0)',
  #       hoverlabel = list(font = list(size = 12))
  #     ) %>%
  #     config(displayModeBar = FALSE)
  # })
  # 
  # output$polyester_bar <- renderPlotly({
  #   ggplotly(plot_supply_bar('Polyester'), tooltip = c('text')) %>%
  #     layout(
  #       plot_bgcolor = 'rgba(0, 0, 0, 0)',
  #       paper_bgcolor = 'rgba(0, 0, 0, 0)',
  #       hoverlabel = list(font = list(size = 12))
  #     ) %>%
  #     config(displayModeBar = FALSE)
  # })
  # 
  # output$others_bar <- renderPlotly({
  #   ggplotly(plot_supply_bar('Others'), tooltip = c('text')) %>%
  #     layout(
  #       plot_bgcolor = 'rgba(0, 0, 0, 0)',
  #       paper_bgcolor = 'rgba(0, 0, 0, 0)',
  #       hoverlabel = list(font = list(size = 12))
  #     ) %>%
  #     config(displayModeBar = FALSE)
  # })
  # 
  # output$trade_map <- renderLeaflet({
  #   leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
  #     setView(110, 23, zoom = 3)
  # })
  # 
}

shinyApp(ui = ui, server = server)