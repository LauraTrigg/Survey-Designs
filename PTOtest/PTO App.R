#setting up the variables 

optionA <- list (
  gains = c(1,2,5,20,50,
            1,2,5,20,50,
            1,2,5,20,50,
            1,2,5,20,50,
            1,2,5,20,50),
  people = c(100,50,20,5,2,
             100,50,20,5,2,
             100,50,20,5,2,
             100,50,20,5,2,
             100,50,20,5,2)
)

healthstate <- list ("This is a description of a 0.2 health state", #0.2 health state 
                     "This is a description of a 0.2 health state",
                     "This is a description of a 0.2 health state",
                     "This is a description of a 0.2 health state",
                     "This is a description of a 0.2 health state",
                     "This is a description of a 0.4 health state", #0.4 health state
                     "This is a description of a 0.4 health state", 
                     "This is a description of a 0.4 health state", 
                     "This is a description of a 0.4 health state", 
                     "This is a description of a 0.4 health state", 
                     "This is a description of a 0.6 health state", #0.6 health state
                     "This is a description of a 0.6 health state",
                     "This is a description of a 0.6 health state",
                     "This is a description of a 0.6 health state",
                     "This is a description of a 0.6 health state",
                     "This is a description of a 0.8 health state", #0.8 health state
                     "This is a description of a 0.8 health state",
                     "This is a description of a 0.8 health state",
                     "This is a description of a 0.8 health state",
                     "This is a description of a 0.8 health state",
                     "This is a description of a 1 health state", #1 health state 
                     "This is a description of a 1 health state",
                     "This is a description of a 1 health state",
                     "This is a description of a 1 health state",
                     "This is a description of a 1 health state"
)


library(shiny)

# UI
ui <- fluidPage(
  
  # Custom CSS for responsiveness
  tags$head(tags$style(HTML("
    /* Center everything */
    .container {
      max-width: 1000px;  /* Limit width on large screens */
      margin: auto;  /* Center content */
    }
    
    /* Make text scale with screen size */
    h1, h2, h3 {
      font-size: calc(1rem + 1vw);
    }
    
    /* Scale main sections */
    .content-section {
      width: 90%;
      max-width: 900px;  /* Prevents excessive stretching */
      margin: auto;
    }

    /* Adjust bar chart */
    .chart-container {
      width: 90%;
      max-width: 800px;
      margin: auto;
    }

    /* Make option boxes responsive */
    .option-box {
      width: 45%;
      min-width: 200px;
      padding: 15px;
      text-align: center;
      border-radius: 8px;
    }
    
    /* Stickmen display */
    .stickmen-container {
      display: flex;
      justify-content: center;
      flex-wrap: wrap;
      gap: 30px;
    }
    
    .stickmen-box {
      width: 300px;
      height: 300px;
      display: flex;
      align-items: center;
      justify-content: center;
      border: 2px solid black;
      background-color: white;
      padding: 10px;
      border-radius: 8px;
    }
    
    /* Responsive scaling */
    @media (max-width: 768px) {
      .option-box {
        width: 90%;  /* Stack option boxes */
        margin-bottom: 10px;
      }
      
      .stickmen-box {
        width: 250px;
        height: 250px;
      }
    }

    @media (max-width: 480px) {
      h1, h2, h3 {
        font-size: calc(1rem + 0.5vw);
      }
      
      .stickmen-box {
        width: 200px;
        height: 200px;
      }
    }
  "))),
  
  # Centered Title Panel
  titlePanel(div("Person Trade-Off Exercise", class = "content-section", style = "text-align: center; font-weight: bold; font-size: 26px;")),
  
  # Page Number Display
  div(class = "content-section", style = "text-align: center; font-size: 18px; font-weight: bold; margin-top: 10px;",
      uiOutput("page_number")),
  
  # Instruction Section
  div(class = "content-section", style = "text-align: center; font-size: 18px; margin-bottom: 20px; padding: 10px; background-color: #f8f9fa; border-radius: 8px;", 
      p("All patients are aged 20. Please use the sliding scale to select 
         how many people should receive 10 years for Programme B to be equally valuable as 
         Programme A.")),  
  
  # Health State Display
  div(class = "content-section", style = "text-align: center; font-size: 18px; font-weight: bold; background-color: #eef7ff; 
               padding: 10px; margin-bottom: 20px; border-radius: 8px;",
      uiOutput("healthstate_text")),  
  
  # Bar Chart Section
  div(class = "chart-container", style = "display: flex; justify-content: center; margin-bottom: 20px;",
      plotOutput('plot', height = "400px", width = "100%")),
  
  # Side-by-side Option A and B Descriptions
  div(class = "content-section", style = "display: flex; justify-content: center; gap: 40px; flex-wrap: wrap; margin-bottom: 20px;",
      div(class = "option-box", style = "background-color: peachpuff; border: 1px solid peachpuff;", 
          strong("Option A"), br(), 
          uiOutput("discription_of_option_A")),
      div(class = "option-box", style = "background-color: darkseagreen; border: 1px solid darkseagreen;", 
          strong("Option B"), br(),
          uiOutput("discription_of_option_B")),
  ),
  
  # Stickmen Display Section
  div(class = "stickmen-container",
      div(class = "stickmen-box", uiOutput("stickmen_display_A")),
      div(class = "stickmen-box", uiOutput("stickmen_display_B"))
  ),
  
  # Slider Input
  div(class = "content-section", style = "text-align: center; margin-bottom: 30px;",
      sliderInput("no_people", "Number of People Benefiting from Option B", min = 1, max = 100, value = 1, width = "80%")),
  
  # Submit Button
  div(class = "content-section", style = "text-align: center;",
      actionButton("submit", "Submit", class = "btn btn-primary btn-lg", 
                   style = "padding: 12px 30px; font-size: 18px;"))
)


server <- function(input, output, session) {
  
  page <- reactiveVal(1)
  output$page_number <- renderUI({
    div(style = "text-align: center; font-size: 16px; font-weight: bold; margin-top: 10px;",
        paste("Question", page()))
  })
  
  #Text for the health state
  output$healthstate_text <- renderUI({
    div(style = "text-align: center; font-size: 16px; font-weight: bold; margin-top: 10px;",
        paste("Health State: ", healthstate[[page()]]))
  })
  
  
  #Render the bar chart
  output$plot <- renderPlot({
    barplot(c(optionA$gains[[page()]], 10), names.arg = c("Option A", "Option B"), 
            col = c("peachpuff","darkseagreen"), ylim = c(0, 11), border = NA)
    title("Gains Per Person", 
          ylab = "Years Gained",
          col.main = "black")
    axis(2, col.axis = "black")
    box(col = "white")
  })
  
  #Text for under the bar charts 
  output$discription_of_option_A <- renderUI({
    div(style = "text-align: center; font-size: 16px; font-weight: bold; margin-top: 10px;",
        paste(optionA$people[[page()]], "people will gain", optionA$gains[[page()]], "years of life each"))
  })
  output$discription_of_option_B <- renderUI({
    div(style = "text-align: center; font-size: 16px; font-weight: bold; margin-top: 10px;",
        paste(input$no_people, "people will gain 10 years of life each"))
  })
  
  
  #Render the stickmen for option A
  output$stickmen_display_A <- renderUI({
    box_width <- 300
    box_height <- 300
    Option_A_gains_pp <- optionA$people[[page()]]
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
    if (page() < length(optionA$gains)) {
      page(page() + 1)  # Increment page number
      removeModal()  # Close the modal dialog
      showNotification("You have confirmed your response")
      updateSliderInput(session, "no_people", value = 1)  # Reset the slider
    } else {
      showModal(modalDialog(
        title = "Survey Complete!",
        "Thank you for participating. Your responses have been recorded.",
        easyClose = TRUE
      ))
    }
  })
  
  # Action when "No, Go Back" is clicked
  observeEvent(input$no_go_back, {
    removeModal()  
  })
  
}

shinyApp(ui = ui, server = server)