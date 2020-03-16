

library(shiny)
library(shinyaccordion)

# Define UI for application that draws a histogram
ui <- fluidPage(
  sidebarLayout(
            mainPanel(
            accordion(
                inputId = 'my-accordion',
                accordionBox('Title1', 'Content1'),
                accordionBox('Title2', 'Content2'),
                accordionBox('Title3', 'Content3'),
                accordionBox('Title4', 'Content4'),
                accordionBox('Title5', 'Content5'),
                selected = 'Title2'
            ),
            accordion(
              inputId = 'my-accordion-h',direction = "horizontal",
              accordionBox('Title1', 'Content1'),
              accordionBox('Title2', 'Content2'),
              accordionBox('Title3', 'Content3'),

              selected = 'Title3',
              width = "50%"
            )),
            sidebarPanel(
            verbatimTextOutput('opts')
            )
  )


)

# Define server logic required to draw a histogram
server <- function(input, output) {


output$opts = renderPrint({reactiveValuesToList(input)})
}

# Run the application
shinyApp(ui = ui, server = server)
