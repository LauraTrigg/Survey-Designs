#setting up the variables 

#If splitting into two blocks, have a variable that is optionA_block_A, and 
#optionB_block_B to assign to optionA/health state to show the different questions 
#to different blocks of participants. 

#Input lists
{
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

healthstatedescriptor <- list ("This is a description of a 0.2 health state", #0.2 health state 
                     "This is a description of a 0.2 health state",
                     "This is a description of a 0.2 health state",
                     "This is a description of a 0.2 health state",
                     "This is a description of a 0.2 health state",
                     
                     "This is a description of a 0.4 health state", #0.4 health state
                     "This is a description of a 0.4 health state", 
                     "This is a description of a 0.4 health state", 
                     "This is a description of a 0.4 health state", 
                     "This is a description of a 0.4 health state", 
                     
                     "They have no problems walking about, with self-care or performinig usual activities. 
                     They have extreme pain or discomfort. 
                     They are moderately anxious or depressed", #0.601 health state
                     "They have no problems walking about, with self-care or performinig usual activities. 
                     They have extreme pain or discomfort. 
                     They are moderately anxious or depressed",
                     "They have no problems walking about, with self-care or performinig usual activities. 
                     They have extreme pain or discomfort. 
                     They are moderately anxious or depressed",
                     "They have no problems walking about, with self-care or performinig usual activities. 
                     They have extreme pain or discomfort. 
                     They are moderately anxious or depressed",
                     "They have no problems walking about, with self-care or performinig usual activities. 
                     They have extreme pain or discomfort. 
                     They are moderately anxious or depressed",
                     
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

healthstate <-  list (0.2,0.2,0.2,0.2,0.2,
                      0.4,0.4,0.4,0.4,0.4,
                      0.6,0.6,0.6,0.6,0.6,
                      0.8,0.8,0.8,0.8,0.8,
                      1.0,1.0,1.0,1.0,1.0)


}

library(shiny)

# UI
ui <- fluidPage(
  
  # Custom CSS for responsiveness & improved design
 { tags$head(tags$style(HTML("
    /* Center and limit width */
    .container {
      max-width: 1000px;
      margin: auto;
    }
    
    /* Scale main sections */
    .content-section {
      width: 90%;
      max-width: 900px;
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
      background-color: white;
      border: 2px solid #ddd;
    }

    /* Stickmen display */
    .stickmen-container {
      display: flex;
      justify-content: center;
      flex-wrap: wrap;
      gap: 50px;
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
        width: 90%;
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
  ")))},
  
 # Centered Title Panel
 titlePanel(div("Person Trade-Off Exercise", class = "content-section", 
                style = "text-align: center; font-weight: bold; font-size: 24px; color: #333; margin-bottom: 5px;")),
 
 # Page Number Display
 div(class = "content-section", style = "text-align: center; font-size: 16px; font-weight: bold; margin-top: 2px; margin-bottom: 5px;",
     uiOutput("page_number")),
 
 # Instruction Section
 div(class = "content-section", 
     style = "text-align: center; font-size: 16px; margin-bottom: 3px; 
            padding: 8px; background-color: #f8f9fa; border-radius: 4px;",
     p("All patients are aged 20. Please use the sliding scale to select 
      how many people should receive 10 years for Programme B to be equally valuable as 
      Programme A.")),  
 
 # Health State Description + Heart (Side-by-Side)
 div(class = "content-section", style = "display: flex; align-items: center; margin-bottom: 3px;",
     # Heart on the left
     div(style = "flex: 1; min-width: 100px; display: flex; justify-content: flex-start;",
         uiOutput("healthstate_heart")),
     # Text on the right
     div(style = "flex: 3; text-align: left; font-size: 16px; font-weight: bold; background-color: #eef7ff; 
             padding: 6px; border-radius: 6px;",
         uiOutput("healthstate_text"))
 ),
 
 # Bar Chart Section
 div(class = "chart-container", style = "display: flex; justify-content: center; margin-bottom: 15px;",
     plotOutput('plot', height = "370px", width = "100%")),  # Reduced height
 
 # Side-by-side Option A and B Descriptions
 div(class = "content-section", 
     style = "display: flex; justify-content: center; gap: 30px; flex-wrap: wrap; margin-bottom: 15px;",
     div(class = "option-box", style = "background-color: peachpuff; padding: 1px;", 
         strong("Option A"), br(), 
         uiOutput("discription_of_option_A")),
     div(class = "option-box", style = "background-color: darkseagreen; padding: 10px;", 
         strong("Option B"), br(),
         uiOutput("discription_of_option_B"))
 ),
 
 # Stickmen Display Section
 div(class = "stickmen-container", style = "margin-bottom: 20px; gap: 70px;",
     div(class = "stickmen-box", uiOutput("stickmen_display_A")),
     div(class = "stickmen-box", uiOutput("stickmen_display_B"))
 ),
 
 # Slider Input
 div(class = "content-section", style = "text-align: center; margin-bottom: 20px;",
     sliderInput("no_people", "Number of People Benefiting from Option B", 
                 min = 1, max = 100, value = 1, width = "100%")),
 
 # Submit Button
 div(class = "content-section", style = "text-align: center; margin-bottom: 5px;",
     actionButton("submit", "Submit", class = "btn btn-primary btn-md", 
                  style = "padding: 10px 25px; font-size: 16px;"))
 
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
        paste("All patients have the following health conditions: ", healthstatedescriptor[[page()]]))
  })
  
  ##Rendering and filling in the heart for health states 
  output$healthstate_heart <- renderUI({
    health_state <- healthstate[[page()]]  # Get current health state (0.2, 0.4, etc.)
    fill_percentage <- health_state * 100  # Convert to percentage (e.g., 0.6 -> 60%)
    
    # Convert fill percentage into correct Y-position and height
    total_height <- 24  # Total SVG height
    fill_height <- round((fill_percentage / 100) * total_height, 2)  # Rounded fill height
    fill_y <- total_height - fill_height  # Start filling from the bottom
    
    heart_svg <- sprintf("
    <svg viewBox='0 -4 24 27' width='120' height='110' xmlns='http://www.w3.org/2000/svg'>
      <!-- Heart Outline with Thin Stroke -->
      <path fill='none' stroke='black' stroke-width='1.2' 
            d='M12 22C12 22 4 14 4 8.5C4 5 6.5 2 10 2C11.9 2 13.4 3.2 14 4.3C14.6 3.2 16.1 2 18 2C21.5 2 24 5 24 8.5C24 14 16 22 16 22H12Z'/>
s
      <!-- Clipping Mask for the Fill -->S
      <defs>
        <clipPath id='heart-clip'>
          <path d='M12 22C12 22 4 14 4 8.5C4 5 6.5 2 10 2C11.9 2 13.4 3.2 14 4.3C14.6 3.2 16.1 2 18 2C21.5 2 24 5 24 8.5C24 14 16 22 16 22H12Z'/>
        </clipPath>
      </defs>

      <!-- Correctly Sized Filled Heart (Clipped to Shape) -->
      <rect x='0' y='%f' width='24' height='%f' fill='red' opacity='0.8' clip-path='url(#heart-clip)' />
    </svg>", fill_y, fill_height)
    
    div(style = "text-align: center; margin: 20px 0;",  # 20px top & bottom margin
        HTML(heart_svg))  # Insert the heart SVG
  })

  #Render the bar chart
  output$plot <- renderPlot({
    library(ggplot2)
    
    # Create a data frame for plotting
    data <- data.frame(
      Option = c("Option A", "Option B"),
      Gains = c(optionA$gains[[page()]], 10)  # Option B always has 10 years
    )
    
    # Find the max y-axis limit (adding a small buffer for visibility)
    max_y <- max(data$Gains) + 1  # Ensures labels are fully visible
    
    # Generate the improved bar chart
    ggplot(data, aes(x = Option, y = Gains, fill = Option)) +
      geom_bar(stat = "identity", width = 0.6, show.legend = FALSE) +  
      geom_text(aes(label = Gains), vjust = -0.3, size = 6, fontface = "bold") +  
      scale_fill_manual(values = c("peachpuff", "darkseagreen")) +  # Custom colors
      scale_y_continuous(breaks = seq(0, max_y, by = 1), limits = c(0, max_y)) +  
      labs(title = "Gains Per Person", 
           x = "", 
           y = "") +
      theme_minimal(base_size = 14) +  # Cleaner look
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),  # Center title
        axis.text.x = element_text(face = "bold", size = 18),
        axis.text.y = element_blank(), #element_text(face = "bold", size = 13),
        panel.grid = element_blank(),  # Remove all gridlines
        panel.background = element_rect(fill = "white", color = NA)  # White background
      )
  })
  

  #Text for under the bar charts 
  output$discription_of_option_A <- renderUI({
    div(style = "text-align: center; font-size: 16px; margin-top: 10px;",
        paste(optionA$people[[page()]], "people will gain", optionA$gains[[page()]], "years of life each"))
  })
  output$discription_of_option_B <- renderUI({
    div(style = "text-align: center; font-size: 16px;  margin-top: 10px;",
        if (input$no_people == 1) {
          paste(input$no_people, "person will gain 10 years of life")
        } else
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
      paste("This means you believe that",optionA$people[[(page())]],"people living for" , optionA$gains[[(page())]] ,"years is 
            equal to ", input$no_people, "people living for 10 years. Is this correct?"),
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