# setwd()
# work_dir <- 'e:/Praca/Nauka/Artykul 16/'
work_dir <- 'https://socialmath.pl/public_resources/'
# library(shiny)
library(ggplot2)
library(ggrepel)

df = read.csv2(paste0(work_dir,"aggregated_data.csv"), header=TRUE, fileEncoding = "UTF-8")
df$Data <- as.Date(df$Data, format = "%Y-%m-%d")
distance_data = read.csv2(paste0(work_dir,"distance_data.csv"), header=TRUE, fileEncoding = "UTF-8")

theme_set(
  theme_bw() +
    theme(legend.position = "bottom") +
	theme(plot.title = element_text(hjust = 0.5))
  )

#if (interactive()) {
ui <- fluidPage(
	plotOutput("plot1"),
	plotOutput("plot2"),
    fluidRow(selectInput("Partia", "Wybierz partię", choices = c("Wszystkie",setNames(df$Partia, df$Partia))), align="center") 
  )
  
  
  
server <- function(input, output) {

output$plot1 <- renderPlot({
p <- ggplot(distance_data, aes(x = x, y = y)) + geom_point(shape=23, fill="#00a0e3", color="black", size=6)
p <- p+labs(title="", x ="", y = "") + theme(text = element_text(size = 14), axis.text.y=element_blank())
p <- p + geom_label_repel(aes(label = etykieta), size=5, box.padding   = 0.5, point.padding = 0.5, segment.color = 'grey50')
p
})

output$plot2 <- renderPlot({

if (input$Partia != "Wszystkie") {
df2 <- subset(df, df$Partia == input$Partia)
} else {
df2 <- df
}

p <- ggplot(df2, aes(x = Data, y = value)) + 
  geom_smooth(aes(color = Partia),method = loess,se = FALSE, span = 0.3) + scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1))
p <- p+labs(title="Zgodność głosowań poszczególnych\n partii z głosowaniami PiS w kolejnych latach", x ="okres głosowań", y = "poziom zgodności") + theme(text = element_text(size = 12)) 
p  
  
  }, res = 96)
  
# output$result <- renderText({paste("You chose", input$Partia) })

}
#}

shinyApp(ui = ui, server = server)

