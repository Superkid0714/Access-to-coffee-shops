library(shiny)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(ggplot2)

# 외부 데이터 로드 (coffee_shop.rds 파일 포함)
coffee_shop <- readRDS("coffee_shop.rds")

# UI 정의
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(
    top = 10, right = 10,
    selectInput(
      inputId = "sel_brand",
      label = tags$span(style = "color: black;", "프랜차이즈를 선택하시오"),
      choices = unique(coffee_shop$brand),
      selected = unique(coffee_shop$brand)[1]  # 기본 선택값 변경
    ),
    sliderInput(
      inputId = "range",
      label = tags$span(style = "color: black;", "접근성 범위를 선택하시오"),
      min = 0, max = 100,
      value = c(60, 80), step = 10
    ),
    plotOutput("density", height = 230)
  )
)

# 서버 로직 정의
server <- function(input, output, session) {
  # 선택된 브랜드 및 범위에 따른 데이터 필터링
  brand_sel <- reactive({
    coffee_shop %>%
      filter(
        brand == input$sel_brand,
        metro_idx >= input$range[1],
        metro_idx <= input$range[2]
      )
  })
  
  # 선택된 브랜드에 따른 밀도 데이터 생성
  plot_sel <- reactive({
    coffee_shop %>%
      filter(brand == input$sel_brand)
  })
  
  # 밀도 그래프 출력
  output$density <- renderPlot({
    ggplot(data = with(density(plot_sel()$metro_idx), data.frame(x, y)), aes(x = x, y = y)) +
      geom_line() +
      xlim(0, 100) +
      xlab("접근성 지수") + ylab("빈도") +
      geom_vline(xintercept = input$range[1], color = "red", size = 0.5) +
      geom_vline(xintercept = input$range[2], color = "red", size = 0.5) +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
  })
  
  # 지도 출력
  output$map <- renderLeaflet({
    leaflet(brand_sel()) %>%
      addTiles() %>%
      setView(lng = 127.0381, lat = 37.59512, zoom = 11) %>%
      addPulseMarkers(
        lng = ~x, lat = ~y,
        label = ~name,
        icon = makePulseIcon()
      )
  })
}

# Shiny 앱 실행
shinyApp(ui = ui, server = server)
