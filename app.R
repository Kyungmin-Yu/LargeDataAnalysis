library(data.table)
library(shiny)
library(tidyverse)

dat1 = fread("half_map_dat1.csv")
dat2 = fread("half_map_dat2.csv")
data = rbind(dat1, dat2)
map_plot = function(colname){
  plot = data %>% 
    ggplot()+
    geom_polygon(aes(x=long, y=lat,group=group, fill={{colname}} %>% as.numeric()))+ 
    scale_fill_gradient(low='#FDEBB0', high='#24966B')+
    theme(panel.background = element_rect(fill='white', color='black', linetype='solid'),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          strip.background = element_rect(fill="white", color="black"),
          legend.title=element_text(face="bold", size=10),
          strip.text = element_text(face="bold"))
  return(plot)
}

ui = fluidPage(
  titlePanel("서울시 지역 변수 시각화하기"),
  
  fluidPage(
    helpText("서울시 법정동의 정보를 시각화"),
    
    selectInput("var",
                label = "변수를 선택하세요",
                choices = c("교통안전지수", "음식점수", "인구", "자동차등록", "주점수"),
                selected = "교통안전지수"),

  selectInput("year",
              label = "연도를 선택하세요",
              choices = c("2017", "2018", "2019"),
              selected = "2017")),
  mainPanel(
    plotOutput(outputId = "distPlot")
  )
)

server = function(input, output){
  
  Input = reactive({
    data[, input$var]
  })
  
  
  output$distPlot = renderPlot({
    data %>% 
      filter(기간 == input$year) %>% 
      ggplot()+
      geom_polygon(aes(x=long, y=lat,group=group, fill=get(Input())))+ 
      scale_fill_gradient(low='#FDEBB0', high='#24966B')+
      theme(panel.background = element_rect(fill='white', color='black', linetype='solid'),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            strip.background = element_rect(fill="white", color="black"),
            legend.title=element_text(face="bold", size=10),
            strip.text = element_text(face="bold"))+
      labs(fill = input$var)
  })
}

shinyApp(ui, server)
