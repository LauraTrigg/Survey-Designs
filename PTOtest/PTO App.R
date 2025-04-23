#Connecting to shinyapps.io 
#install.packages('rsconnect')
library(sortable)
#setting up the variables 

#If splitting into two blocks, have a variable that is optionA_block_A, and 
#optionB_block_B to assign to optionA/health state to show the different questions 
#to different blocks of participants. 

#Input lists   
{
  #Input Lists
  #Full list
  optionA <- list(
    gains = c(5, 10, 2, 2, 20),
    #20, 1, 1, 5, 0.5, 5, 0.75, 0.75, 0.5, 0.5, 2, 5, 5, 2, 20, 0.5, 1, 0.75, 0.5, 0.75, 2, 20, 1, 20, 0.75, 5, 1
    people = c(20, 10, 50, 50, 5)
    #5, 100, 100, 20, 200, 20, 150, 150, 200, 200, 50, 20, 200, 50, 5, 200, 100, 150, 200, 150, 50, 5, 100, 5, 150, 20, 100
  )
  
  healthstatedescriptor <- list(
    "A","B", "C","D"
      )

  
  healthstate <- list(
    0.6, 1, 0.2, 0.6, 0.8
  )
  # 0.601, 0.408, 0.408, 0.198, 0.601, 0.408, 0.8,0.8, 0.965, 0.965, 0.601, 0.198, 0.965, 0.601, 0.8, 0.965, 0.408, 0.8, 0.965, 0.8, 0.601, 0.198, 0.408, 0.198, 0.8, 0.198, 0.408
  
} #longlist 

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
  
  # Introduction Page (Initially Visible)
  uiOutput("page_content")
)

server <- function(input, output, session) {
  
  page <- reactiveVal(1)
  survey_page <- reactiveVal(1)
  survey_complete <- reactiveVal(FALSE)
  endofsurvey <- reactiveVal(FALSE)
  
  # Observers to update the page when buttons are clicked
  observeEvent(input$next1, { page(2) })
  observeEvent(input$start_examples, { page(3)})
  observeEvent(input$next2, { page(4) })
  observeEvent(input$start_survey, { 
    page(5)
    survey_page(1)
    })
  observeEvent(input$Back, { page(page() - 1) })
  
  # UI for all pages
  output$page_content <- renderUI({
    
    # Ranking task page after main survey
    if (survey_complete() && !endofsurvey()) {
      return(tagList(
        div(style = "text-align: center; max-width: 800px; margin: auto; margin-top: 50px;",
            h2("🎉 Survey Complete!"),
            p("Thank you for participating. Your responses have been recorded.",
              style = "font-size: 18px; margin-top: 20px;"),
            p("One final task: Please rank the treatment options below from most valuable (top) to least valuable (bottom).",
              style = "font-size: 18px; margin-top: 30px; font-weight: bold;")
        ),
        
        # Drag-and-drop ranking list
        div(style = "max-width: 500px; margin: auto; margin-top: 30px;",
            rank_list(
              text = "Drag to rank:",
              labels = c(
                "200 people live for an extra 6 months",
                "150 people live for an extra 9 months",
                "100 people live for an extra year",
                "50 people live for an extra 2 years",
                "20 people live for an extra 5 years",
                "10 people live for an extra 10 years",
                "5 people live for an extra 20 years"
              ),
              input_id = "ranking_result"
            )
        ),
        
        div(style = "text-align: center; margin-top: 30px;",
            actionButton("submit_ranking", "Submit Ranking", class = "btn btn-success",
                         style = "font-size: 16px; padding: 10px 25px;")
        )
      ))
    }
    
    # Final thank you page after ranking is submitted
    if (survey_complete() && endofsurvey()) {
      return(tagList(
        div(style = "text-align: center; max-width: 800px; margin: auto; margin-top: 50px;",
            h2("✅ All Done!"),
            p("Thanks again — your ranking has been submitted.",
              style = "font-size: 18px; margin-top: 20px;"),
            p("You may now close this window.",
              style = "font-size: 16px; margin-top: 10px; font-style: italic;")
        )
      ))
    }
    
    #Introduction page 1 
    if (page() == 1) {
      tagList(
        div(style = "text-align: center; max-width: 800px; margin: auto;",
            h2("Welcome to the Survey"),
            p("In this survey, you will help decide how to give out a life-saving treatment.",
              style = "font-size: 20px; margin-top: 20px;"),
            
            p("Each question gives you two choices: Option A and Option B.",
              style = "font-size: 20px; margin-top: 20px;"),
            
            p("Option A shows how many people get the treatment and how many extra years they will live.",
              style = "font-size: 20px; margin-top: 20px;"),
            
            p("In Option B, everyone who gets the treatment lives 10 more years. Anyone who doesn’t get it will die right away.",
              style = "font-size: 20px; margin-top: 20px;"),
            
            p("Your job is to choose how many people should get the treatment in Option B so that both options feel equally fair or valuable.",
              style = "font-size: 20px; margin-top: 20px; font-weight: bold;"),
            
            p("The treatment doesn’t make people feel better. It only adds extra years to their life.",
              style = "font-size: 20px; margin-top: 20px;"),
                        actionButton("next1", "Next",
                         style = "font-size: 18px; padding: 10px 20px; margin-top: 20px;")
        )
      )
      
    } 
    #Introduction page 2
    else if (page() == 2) {
      tagList(
        div(style = "text-align: center; max-width: 800px; margin: auto;",
            h2("Before You Start"),
            p("Before we begin, imagine you're in charge of the country’s healthcare budget.",
              style = "font-size: 20px; margin-top: 30px;"),
            
            p("You have 200 patients who need a life-saving treatment. They are all 20 years old and in the same health.",
              style = "font-size: 20px; margin-top: 20px;"),
            
            p("You need to decide how to use this treatment in the best way possible.",
              style = "font-size: 20px; margin-top: 20px;"),
            
            p("In each question, think about how to balance these things:",
              style = "font-size: 20px; margin-top: 20px;"),
            
            p("- Giving treatment to the most people", style = "font-size: 20px; margin-top: 20px;"),
            p("- Helping people live longer", style = "font-size: 20px; margin-top: 20px;"),
            p("- Or finding a fair balance between both", style = "font-size: 20px; margin-top: 20px;"),
            
            p("Important things to know:", style = "font-size: 20px; margin-top: 20px; font-weight: bold;"),
            
            p("- You are in charge of how the treatment is given out", style = "font-size: 20px; margin-top: 20px;"),
            p("- There are 200 patients who are 20 years old", style = "font-size: 20px; margin-top: 20px;"),
            p("- If someone doesn’t get the treatment, they will die immediately", style = "font-size: 20px; margin-top: 20px;"),
            
            p("Please make each decision based on what you think is best for the country.",
              style = "font-size: 20px; margin-top: 20px;"),
            
            
            div(style = "display: flex; justify-content: center; gap: 20px; margin-top: 20px;",
                actionButton("start_examples", "Start Examples",  
                             style = "font-size: 18px; padding: 10px 20px;"),
                
                actionButton("Back", "Back",  
                             style = "font-size: 18px; padding: 10px 20px;")
            )
        )
      )
      
    }
    #Example page 1
    else if (page() == 3) {
      tagList(
        # Centered Title Panel
        div(style = "text-align: center; max-width: 800px; margin: auto;",
            h2("Example"),
        ),
        
        # Instructions
        div(class = "content-section", 
            style = "text-align: center; font-size: 16px; margin-bottom: 8px; padding: 6px; border-radius: 4px;",
            p("Here’s a quick example to show how the survey works."),
            p("You’ll see two treatment options. In this example, Option A means 20 people live for 5 more years each. Option B means 10 people live for 10 more years each."),
            p("The diagram below shows how long each person lives for each option.")
        ),
        
        # Health State Visual
        div(class = "content-section", style = "display: flex; align-items: center; margin-bottom: 10px;",
            div(style = "flex: 1; display: flex; justify-content: center;",
                uiOutput("healthstate_heart")
            ),
            div(style = "flex: 10; text-align: center; font-size: 16px; padding: 6px 8px; border-radius: 6px; margin-left: 6px;",
                p("Imagine that the health of the patients can be represented by a percentage. 100% is fully healthy, and 0% is dead.
                  The shape on the left shows this. The filled part of the shape shows how healthy the patients are."),
                p(paste("In this example, all patients have a health score of", healthstate[[survey_page()]] * 100, "%."))
            )
        ),
        
        # Bar Chart
        div(class = "chart-container", style = "margin-bottom: 10px;",
            plotOutput('plot', height = "250px", width = "100%")
        ),
        
        # Option Descriptions
        div(class = "content-section", style = "display: flex; justify-content: center; gap: 20px; flex-wrap: wrap; margin-bottom: 10px;",
            div(class = "option-box", style = "background-color: peachpuff;", 
                strong("Option A"), br(), uiOutput("discription_of_option_A")
            ),
            div(class = "option-box", style = "background-color: darkseagreen; padding: 10px;", 
                strong("Option B"), br(),
                div(style = "text-align: center; font-size: 16px; margin-top: 10px;",
                    "10 people will gain 10 years of life each")
            )
            
        ),
        
        # Navigation Buttons
        div(style = "text-align: center; margin-top: 10px;",
            actionButton("next2", "Next", style = "font-size: 16px; padding: 8px 20px; margin-right: 10px;"),
            actionButton("Back", "Back", style = "font-size: 16px; padding: 8px 20px;")
        )
      )
    }
    #Example page 2
    else if (page() == 4) {
      tagList(
        div(style = "text-align: center; max-width: 800px; margin: auto;",
            h2("Example"),
        ),
        
        # Instructions
        div(class = "content-section", 
            style = "text-align: center; font-size: 16px; margin-bottom: 0px; padding: 1px; border-radius: 4px;",
            p("Now use the slider below to adjust how many people should be treated in Option B to make it equally valuable to Option A."),
            p("The stick figures show how many people benefit from the treatment.")
        ),
        
        # Health State + Scale
        div(class = "content-section", style = "display: flex; align-items: center; margin-bottom: 10px;",
            div(style = "flex: 1; display: flex; justify-content: center;",
                uiOutput("healthstate_heart")
            ),
            div(style = "flex: 10; text-align: center; font-size: 16px; padding: 6px 8px; border-radius: 6px; margin-left: 6px;",
                p("Health is shown on a scale from 0 to 100%."),
                p(paste("In this example, all patients have a health score of", healthstate[[survey_page()]] * 100,"%."))
            )
        ),
        
        # Bar Chart
        div(class = "chart-container", style = "margin-bottom: 10px;",
            plotOutput('plot', height = "250px", width = "100%")
        ),
        
        # Option Descriptions
        div(class = "content-section", style = "display: flex; justify-content: center; gap: 20px; flex-wrap: wrap; margin-bottom: 10px;",
            div(class = "option-box", style = "background-color: peachpuff;", 
                strong("Option A"), br(), uiOutput("discription_of_option_A")
            ),
            div(class = "option-box", style = "background-color: darkseagreen; padding: 10px;", 
                strong("Option B"), br(),
                div(style = "text-align: center; font-size: 16px; margin-top: 10px;",
                    "10 people will gain 10 years of life each")
            )
            
        ),
        
        # Stickmen Display
        div(class = "stickmen-container", style = "margin-bottom: 10px; gap: 40px;",
            div(class = "stickmen-box", uiOutput("stickmen_display_A")),
            div(class = "stickmen-box", uiOutput("stickmen_display_B"))
        ),
        
        # Slider Input
        div(class = "content-section", style = "text-align: center; margin-bottom: 10px;",
            sliderInput("no_people", "Move the slider to pick how many people should get Option B so it's just as good as Option A.", 
                        min = 0, max = 20, value = 10, width = "100%")
        ),
        
        # Buttons
        div(class = "content-section", style = "text-align: center; margin-bottom: 10px;",
            actionButton("start_survey", "Start Survey", class = "btn btn-primary btn-md", 
                         style = "padding: 8px 20px; font-size: 16px; margin-right: 10px;"),
            actionButton("Back", "Back", style = "font-size: 16px; padding: 8px 20px;")
        )
      )
    }
    #Survey page
    else {
      
      tagList(
        
        # Centered Title Panel
        div(h1(uiOutput("page_number")),
        ),

        
        # Instruction Section
        div(class = "content-section", 
            style = "text-align: center; font-size: 16px; margin-bottom: 3px; 
            padding: 2px; border-radius: 4px;",
            p("All patients are aged 20. Please use the sliding scale to select 
            how many people should receive 10 years for Programme B to be equally valuable as 
            Programme A.")),  
        
        # Health State + Scale (less vertical space)
        div(class = "content-section", style = "display: flex; align-items: center; margin-bottom: 6px;",
            div(style = "flex: 1; display: flex; justify-content: center;",
                uiOutput("healthstate_heart")
            ),
            div(style = "flex: 10; text-align: center; font-size: 16px; padding: 4px 6px; border-radius: 6px; margin-left: 6px;",
                p("Health is shown on a scale from 0 to 100%."),
                p(paste("All patients have a health score of", healthstate[[survey_page()]] * 100, "%."))
            )
        ),
        
        # Bar Chart Section
        div(class = "chart-container", style = "display: flex; justify-content: center; margin-bottom: 6px;",
            plotOutput('plot', height = "250px", width = "100%")),
        
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
            sliderInput("no_people", "Move the slider to pick how many people should get Option B so it's just as good as Option A.", 
                        min = 0, max = 20, value = 10, width = "100%")),
        
        # Submit Button
        div(class = "content-section", style = "text-align: center; margin-bottom: 5px;",
            actionButton("submit", "Submit", class = "btn btn-primary btn-md", 
                         style = "padding: 10px 25px; font-size: 16px;"))
      )
    }
  })

  output$page_number <- renderUI({
    h2(paste("Question", survey_page()),
       style = "text-align: center; margin-top: 10px;")
  })
  
  
  #Picture for health state
  output$healthstate_heart <- renderUI({
    health_state <- healthstate[[survey_page()]]
    fill_percentage <- round(health_state * 100)
    
    total_height <- 80
    fill_height <- round(health_state * total_height)
    fill_y <- total_height - fill_height
    
    fill_color <- if (health_state > 0.7) {
      "#4CAF50"  # green
    } else if (health_state > 0.4) {
      "#FFC107"  # amber
    } else {
      "#F44336"  # red
    }
    
    # Create 10 tick marks and hide last
    tick_positions <- seq(10, 90, by = 10)
    visible_ticks <- tick_positions[1:9] 
    
    tick_lines <- paste(sapply(visible_ticks, function(pct) {
      y <- 10 + (80 - (pct / 100) * 80)  # Map 0–100 to 10–90 SVG y-range
      sprintf('<line x1="20" y1="%f" x2="23" y2="%f" stroke="#333" stroke-width="1"/>', y, y)
    }), collapse = "\n")
    
    svg_html <- sprintf('
  <svg viewBox="0 0 60 100" width="80" height="160" xmlns="http://www.w3.org/2000/svg">
    <!-- Container outline -->
    <rect x="20" y="10" width="20" height="80" rx="6" ry="6" stroke="#000" stroke-width="2" fill="none"/>

    <!-- Fill rectangle clipped to health state -->
    <clipPath id="clip_fill">
      <rect x="20" y="%f" width="20" height="%f"/>
    </clipPath>
    <rect x="20" y="10" width="20" height="80" rx="6" ry="6" fill="%s" clip-path="url(#clip_fill)" />

    <!-- Tick marks (excluding top and bottom) -->
    %s
  </svg>',
                        10 + fill_y, fill_height, fill_color, tick_lines
    )
    
    div(style = "text-align: center; margin: 10px 0;",
        HTML(svg_html))
  })
  
  #Render the bar chart
  output$plot <- renderPlot({
    library(ggplot2)
    
    # Create a data frame for plotting
    data <- data.frame(
      Option = c("Option A", "Option B"),
      Gains = c(optionA$gains[[survey_page()]], 10)  # Option B always has 10 years
    )
    
    # Find the max y-axis limit
    max_y <- max(data$Gains) + 1  # Ensures labels are fully visible
    
    # Generate the improved bar chart
    ggplot(data, aes(x = Option, y = Gains, fill = Option)) +
      geom_bar(stat = "identity", width = 0.6, show.legend = FALSE) +  
      geom_text(aes(label = Gains), vjust = -0.3, size = 6) +  
      scale_fill_manual(values = c("peachpuff", "darkseagreen")) + 
      scale_y_continuous(breaks = seq(0, max_y, by = 1), limits = c(0, max_y)) +  
      labs(title = "Extra years per patient", 
           x = "",  
           y = "") +
      theme_minimal(base_size = 14) + 
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),  # Center title
        axis.text.x = element_text(face = "bold", size = 18),
        axis.text.y = element_blank(), #element_text(face = "bold", size = 13),
        panel.grid = element_blank(),  # Remove all gridlines
        panel.background = element_rect(fill = "white", color = NA) 
      )
  })

  #Text for under the bar charts 
  output$discription_of_option_A <- renderUI({
    div(style = "text-align: center; font-size: 16px; margin-top: 10px;",
        paste(optionA$people[[survey_page()]], "people will gain", optionA$gains[[survey_page()]], "years of life each"))
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
    Option_A_gains_pp <- optionA$people[[survey_page()]]
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
    if (input$no_people == 0) {
      return(div(style = "width: 100%; height: 100%;"))  # Empty box
    }
    
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
    grid_container <- sprintf(
      "<div style='display: grid; grid-template-columns: repeat(%d, 1fr); grid-template-rows: repeat(%d, 1fr); gap: 2px;'>%s</div>",
      cols, rows, stickmen
    )
    
    HTML(grid_container)
  })
  
  
  #Pop-up box to confirm response
  observeEvent(input$submit, {
    showModal(modalDialog(
      title = "Thank you for your response!",
      paste("This means you believe that",optionA$people[[(survey_page())]],"people living for" , optionA$gains[[(survey_page())]] ,"years is 
            equal to ", input$no_people, "people living for 10 years. Is this correct?"),
      footer = tagList(
        actionButton("yes_continue", "Yes, Continue", class = "btn-primary"),
        actionButton("no_go_back", "No, Go Back", class = "btn-danger")
      )
    ))
  })
  
  # Action when "No, Go Back" is clicked
  observeEvent(input$no_go_back, {
    removeModal()  
  })
  
  # Action when "Yes, Continue" is clicked
  observeEvent(input$yes_continue, {
    total_questions <- length(optionA$gains)
    if (survey_page() < total_questions) {
      # Not the last question: process response & go to next
      survey_page(survey_page() + 1)
      removeModal()
      showNotification("You have confirmed your response")
      updateSliderInput(session, "no_people", value = 1)
    } else if (survey_page() == total_questions) {
      removeModal()
      showNotification("Final response recorded. Thank you!")
      survey_complete(TRUE)
    }
  })
  
  #Action when 'Submit Ranking' is clicked
  observeEvent(input$submit_ranking, {
    endofsurvey(TRUE)
  })
  
  }

shinyApp(ui = ui, server = server)