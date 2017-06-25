shinyUI(fluidPage(
  titlePanel("Corestone infoflow"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Завантажте файл з даними',
                accept = c(".xlsx")),
      tags$hr(),
      downloadButton('down',"Завантажити в pdf!"),
      downloadButton('do',"Завантажити в png!")
    ),
      mainPanel("Візуалізація",plotOutput('plot', width = "1132px", height = "800px"))
    )
  )
)