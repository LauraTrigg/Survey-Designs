library(shiny)

ui <- fluidPage(
  titlePanel("Person Trade-Off Exercise"),
  div(style = "text-align: center; font-size: 18px; margin-bottom: 20px;", 
      p("All patients are aged 20 and are in perfect health. Please use the sliding scale to select 
how many people should receive 10 years for Programme B to be equally valuable as 
Programme A.")),
  mainPanel(
    plotOutput('plot', height = "600px"),
 
    div(style = "display: flex; justify-content: space-between; width: 90%;",
        div(style = "margin-left: calc(15% + 10px); width: 350px; height: 350px; 
            display: grid; justify-content: center; align-items: center; 
            border: 1px solid black; background-color: white;",
            uiOutput("stickmen_display_A")),
        
        div(style = "width: 350px; height: 350px; display: 
            grid; justify-content: center; align-items: center; 
            border: 1px solid black; background-color: white;",
            uiOutput("stickmen_display_B"))
    ),
    
    sliderInput("no_people", "Number of People Benefiting from Option B", min = 1, max = 100, value = 1, width = "100%"),
    actionButton("submit", "Submit")
  )
)

server <- function(input, output) {
  
  #Render the bar chart
  output$plot <- renderPlot({
    barplot(c(2, 10), names.arg = c("Option A", "Option B"), col = "blue", ylim = c(0, 11), border = NA)
    title("Gains Per Person", col.main = "gray")
    axis(2, col.axis = "white")
    box(col = "white")
  })
  
  #Render the stickmen for option A
  output$stickmen_display_A <- renderUI({
    box_width <- 300
    box_height <- 300
    Option_A_gains_pp <- 25
    cols <- ceiling(sqrt(Option_A_gains_pp))
    rows <- ceiling(Option_A_gains_pp / cols)
    stickman_width <- box_width / cols * 0.9
    stickman_height <- box_height / rows * 0.9
    
    stickman_svg <- sprintf("<svg width='%f' height='%f' viewBox='0 0 30 60' xmlns='http://www.w3.org/2000/svg'>
      <circle cx='15' cy='10' r='5' stroke='black' stroke-width='2' fill='none'/>
      <line x1='15' y1='15' x2='15' y2='40' stroke='black' stroke-width='2'/>
      <line x1='15' y1='22' x2='5' y2='30' stroke='black' stroke-width='2'/>
      <line x1='15' y1='22' x2='25' y2='30' stroke='black' stroke-width='2'/>
      <line x1='15' y1='40' x2='5' y2='55' stroke='black' stroke-width='2'/>
      <line x1='15' y1='40' x2='25' y2='55' stroke='black' stroke-width='2'/>
    </svg>", stickman_width, stickman_height)
    
    stickmen <- paste(rep(stickman_svg, Option_A_gains_pp), collapse = " ")
    grid_container <- sprintf("<div style='display: grid; grid-template-columns: repeat(%d, 1fr); grid-template-rows: repeat(%d, 1fr); gap: 2px;'>%s</div>", cols, rows, stickmen)
    HTML(grid_container)
  })
  
  #Render the stickmen for option B
  output$stickmen_display_B <- renderUI({
    box_width <- 300
    box_height <- 300
    cols <- ceiling(sqrt(input$no_people))
    rows <- ceiling(input$no_people / cols)
    stickman_width <- box_width / cols * 0.9
    stickman_height <- box_height / rows * 0.9
    
    stickman_svg <- sprintf("<svg width='%f' height='%f' viewBox='0 0 30 60' xmlns='http://www.w3.org/2000/svg'>
      <circle cx='15' cy='10' r='5' stroke='black' stroke-width='2' fill='none'/>
      <line x1='15' y1='15' x2='15' y2='40' stroke='black' stroke-width='2'/>
      <line x1='15' y1='22' x2='5' y2='30' stroke='black' stroke-width='2'/>
      <line x1='15' y1='22' x2='25' y2='30' stroke='black' stroke-width='2'/>
      <line x1='15' y1='40' x2='5' y2='55' stroke='black' stroke-width='2'/>
      <line x1='15' y1='40' x2='25' y2='55' stroke='black' stroke-width='2'/>
    </svg>", stickman_width, stickman_height)
    
    stickmen <- paste(rep(stickman_svg, input$no_people), collapse = " ")
    grid_container <- sprintf("<div style='display: grid; grid-template-columns: repeat(%d, 1fr); grid-template-rows: repeat(%d, 1fr); gap: 2px;'>%s</div>", cols, rows, stickmen)
    HTML(grid_container)
  })
  
  #Pop-up box to confirm response
  observeEvent(input$submit, {
    showModal(modalDialog(
      title = "Thank you for your response!",
      paste("This means you believe that 20 people gaining 5 extra years of life is equally valuable as", 
            input$no_people, 
            "people gaining 10 extra years of life. Is this correct?"),
      footer = tagList(
        actionButton("yes_continue", "Yes, Continue", class = "btn-primary"),
        actionButton("no_go_back", "No, Go Back", class = "btn-danger")
      )
    ))
  })
  
  # Action when "Yes, Continue" is clicked
  observeEvent(input$yes_continue, {
    removeModal()  # Close the modal
    showNotification("You have confirmed your response!", type = "message")
    # You can add additional logic here, such as moving to the next question
  })
 
  # Action when "No, Go Back" is clicked
  observeEvent(input$no_go_back, {
    removeModal()  
  })
  
}

shinyApp(ui = ui, server = server)
.
.
.
.


.
