shinyUI(fluidPage(
  titlePanel("Corestone infoflow"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Завантажте файл з даними',
                accept = c(".xlsx")),
      tags$hr(),
      downloadButton('downloadPlot',"Завантажити в pdf!"),
      downloadButton('download',"Завантажити в png!")
    ),
    mainPanel("Візуалізація",plotOutput('plot', width = "1132px", height = "800px"))
  )
)
)