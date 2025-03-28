#Connecting to shinyapps.io 
#install.packages('rsconnect')
#library(sortable)
#setting up the variables 

#If splitting into two blocks, have a variable that is optionA_block_A, and 
#optionB_block_B to assign to optionA/health state to show the different questions 
#to different blocks of participants. 

#Input lists   
{
  #Input Lists
  #Full list
  optionA <- list(
    gains = c(10, 2, 2, 20, 20, 1, 1, 5, 0.5, 5, 0.75, 0.75, 0.5, 0.5, 2, 5, 5, 2, 20, 0.5, 1, 0.75, 0.5, 0.75, 2, 20, 1, 20, 0.75, 5, 1),
    people = c(10, 50, 50, 5, 5, 100, 100, 20, 200, 20, 150, 150, 200, 200, 50, 20, 200, 50, 5, 200, 100, 150, 200, 150, 50, 5, 100, 5, 150, 20, 100)
  )
  
  healthstatedescriptor <- list(
    "They have no problems walking about, with self-care or performing usual activities. They have no pain or discomfort and are not anxious or depressed.",
    "They have no problems walking about, with self-care or performing usual activities. They have no pain or discomfort and are not anxious or depressed.",
    "They have no problems walking about, with self-care or performing usual activities. They have extreme pain or discomfort. They are moderately anxious or depressed",
    "They have no problems walking about, with self-care and are experiencing no pain or discomfort. However, they have some problems washing or dressing themselves and are moderately anxious or depressed",
    "They have no problems walking about, with self-care and are experiencing no pain or discomfort. However, they have some problems washing or dressing themselves and are moderately anxious or depressed",
   "They have some problems walking about, and are unable to wash or dress themselves. They have extreme pain or discomfort, but have no problems with self-care and are not anxious or depressed",
    "They have some problems walking about, and are unable to wash or dress themselves. They have extreme pain or discomfort, but have no problems with self-care and are not anxious or depressed",
    "They are confined to bed, and are unable to perform usual activities. They have moderate pain or discomfort, and are moderately anxious or depressed. However, they have no problems with self care",
    "They have no problems walking about, with self-care or performing usual activities. They have extreme pain or discomfort. They are moderately anxious or depressed",
    "They have some problems walking about, and are unable to wash or dress themselves. They have extreme pain or discomfort, but have no problems with self-care and are not anxious or depressed",
    "They have no problems walking about, with self-care and are experiencing no pain or discomfort. However, they have some problems washing or dressing themselves and are moderately anxious or depressed",
    "They have no problems walking about, with self-care and are experiencing no pain or discomfort. However, they have some problems washing or dressing themselves and are moderately anxious or depressed",
    "They have no problems walking about, with self-care or performing usual activities. They have no pain or discomfort and are not anxious or depressed.",
    "They have no problems walking about, with self-care or performing usual activities. They have no pain or discomfort and are not anxious or depressed.",
    "They have no problems walking about, with self-care or performing usual activities. They have extreme pain or discomfort. They are moderately anxious or depressed",
    "They are confined to bed, and are unable to perform usual activities. They have moderate pain or discomfort, and are moderately anxious or depressed. However, they have no problems with self care",
    "They have no problems walking about, with self-care or performing usual activities. They have no pain or discomfort and are not anxious or depressed.",
    "They have no problems walking about, with self-care or performing usual activities. They have extreme pain or discomfort. They are moderately anxious or depressed",
    "They have no problems walking about, with self-care and are experiencing no pain or discomfort. However, they have some problems washing or dressing themselves and are moderately anxious or depressed",
    "They have no problems walking about, with self-care or performing usual activities. They have no pain or discomfort and are not anxious or depressed.",
    "They have some problems walking about, and are unable to wash or dress themselves. They have extreme pain or discomfort, but have no problems with self-care and are not anxious or depressed",
    "They have no problems walking about, with self-care and are experiencing no pain or discomfort. However, they have some problems washing or dressing themselves and are moderately anxious or depressed",
    "They have no problems walking about, with self-care or performing usual activities. They have no pain or discomfort and are not anxious or depressed.",
    "They have no problems walking about, with self-care and are experiencing no pain or discomfort. However, they have some problems washing or dressing themselves and are moderately anxious or depressed",
    "They have no problems walking about, with self-care or performing usual activities. They have extreme pain or discomfort. They are moderately anxious or depressed",
    "They are confined to bed, and are unable to perform usual activities. They have moderate pain or discomfort, and are moderately anxious or depressed. However, they have no problems with self care",
    "They have no problems walking about, with self-care and are experiencing no pain or discomfort. However, they have some problems washing or dressing themselves and are moderately anxious or depressed",
    "They have no problems walking about, with self-care and are experiencing no pain or discomfort. However, they have some problems washing or dressing themselves and are moderately anxious or depressed",
    "They are confined to bed, and are unable to perform usual activities. They have moderate pain or discomfort, and are moderately anxious or depressed. However, they have no problems with self care",
    "They have some problems walking about, and are unable to wash or dress themselves. They have extreme pain or discomfort, but have no problems with self-care and are not anxious or depressed"
  )
  
  healthstate <- list(
    0.965,0.965, 0.601, 0.8, 0.601, 0.408, 0.408, 0.198, 0.601, 0.408, 0.8,0.8, 0.965, 0.965, 0.601, 0.198, 0.965, 0.601, 0.8, 0.965, 0.408, 0.8, 0.965, 0.8, 0.601, 0.198, 0.408, 0.198, 0.8, 0.198, 0.408
  )
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
            p("In this study, you will be asked to make decisions about how 
              healthcare resources should be spread across different groups of 
              patients. These decisions involve trade-offs between the number of
              patients receiving a treatment, and the number of years they will
              live for once they've had the treatment.", 
              style = "font-size: 20px; margin-top: 20px;"),
            p("In each question, there are two options: options A and B.
              In option A, you will be told how many people will get the treatment, 
              and how long they will live for once they have had it. For option B, 
              the patients who get the treatment will live for 10 more years. 
              Patients who do not recieve the treatment will die immediately.
              ", 
              style = "font-size: 20px; margin-top: 20px;"),
            p( "Your task is to decide how many patients should get the treatment so
              that you think that option A and option B are equally valuable.", 
               style = "font-size: 20px; margin-top: 20px; font-weight: bold;"),
            p("The treatment will not change the patient’s quality of life,
              it will only change how long they will live for.", 
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
            h2("Welcome to the Survey"),
            p("Before you start, please imagine you are in charge of a healthcare 
              budget for the country. It is your job to decide how best to 
              distribute these life-extending treatments.",
              style = "font-size: 20px; margin-top: 30px;"),
            p("There are 200 patients that need this treatment, and they are 
              identical with respect to age and health condition.",
              style = "font-size: 20px; margin-top: 20px;"),
            p("This task reflects real-world challenges in healthcare decision-making, 
              where resources are finite, and trade-offs must be considered. You must 
              consider different factors, including:",
              style = "font-size: 20px; margin-top: 20px;"),
            
            p("- Maximizing total life expectancy", style = "font-size: 20px; margin-top: 20px;"),
            p("- Treating the highest number of patients", style = "font-size: 20px; margin-top: 20px;"),
            p("- Or, a trade-off between the two.", style = "font-size: 20px; margin-top: 20px;"),
            
            p("Key things to remember:", style = "font-size: 20px; margin-top: 20px; font-weight: bold;"),
            p("- You are in charge of the healthcare budget for the country", style = "font-size: 20px; margin-top: 20px;"),
            p("- There are 200 patients that need treatment, and they are all aged 20", style = "font-size: 20px; margin-top: 20px;"),
            p("- Those who do not receive this treatment will die immediately", style = "font-size: 20px; margin-top: 20px;"),
            
            p("Please answer based on what you think is best for the country.", style = "font-size: 20px; margin-top: 20px;"),
            
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
        titlePanel(div("Person Trade-Off Exercise", class = "content-section", 
                       style = "text-align: center; font-weight: bold; font-size: 24px; color: #333; margin-bottom: 5px;")),
        
        # Page Number Display
        div(class = "content-section", style = "text-align: center; font-size: 16px; font-weight: bold; margin-top: 2px; margin-bottom: 5px;",
            uiOutput("page_number")),
        
        # Instruction Section
        div(class = "content-section", 
            style = "text-align: center; font-size: 16px; margin-bottom: 3px; 
            padding: 8px; background-color: #f8f9fa; border-radius: 4px; font-weight : bold;",
            p("In the following questions, you will see a visual representation of 
        the trade-off you are making. In this example, there 
        are 20 people who will live for 5 years each in Option A, and in Option B, there 
        are 10 people who will live for 10 years each. The bar chart shows 
        you how long each person will live in each option.")  
        ),  
        
        # Health State Description + Heart (Side-by-Side)
        div(class = "content-section", style = "display: flex; align-items: center; margin-bottom: 3px;",
            # Heart on the left
            div(style = "flex: 1; min-width: 100px; display: flex; justify-content: flex-start;",
                uiOutput("healthstate_heart")),
            
            # Text on the right
            div(style = "flex: 3; text-align: left; font-size: 16px; font-weight: bold; background-color: #eef7ff; 
        padding: 6px; border-radius: 6px;",
                p("In the survey, the level of health of each participant will 
           be varied. Here you will see a description of the level of 
           health for each participant. On a scale of 0 to 100%,
              the imagine to the left will show you how healthy the individual is.")
            )
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
                p("10 people will gain 10 years of life each"))
        ),
        
        actionButton("next2", "Next",
                     style = "font-size: 18px; padding: 10px 20px; margin-top: 20px;"),
        actionButton("Back", "Back",  
                     style = "font-size: 18px; padding: 10px 20px; margin-top: 20px;")
        
      )
    } 
    #Example page 2
    else if (page() == 4) {
      tagList(
        
        # Centered Title Panel
        titlePanel(div("Person Trade-Off Exercise", class = "content-section", 
                       style = "text-align: center; font-weight: bold; font-size: 24px; color: #333; margin-bottom: 5px;")),
        
        # Page Number Display
        div(class = "content-section", style = "text-align: center; font-size: 16px; font-weight: bold; margin-top: 2px; margin-bottom: 5px;",
            uiOutput("page_number")),
        
        # Instruction Section
        div(class = "content-section", 
            style = "text-align: center; font-size: 16px; margin-bottom: 3px; 
            padding: 8px; background-color: #f8f9fa; border-radius: 4px;font-weight: bold;",
            p("Now you can see a slider which you will use to show how many 
              people should benefit in Option B to make options A and B equally
              valuable. The stick figures are used to show you how many people
              will benefit from the treatment. Practice using the slider 
              to change the number of people that will recieve treatment 
              in Option B")),  
        
        # Health State Description + Heart (Side-by-Side)
        div(class = "content-section", style = "display: flex; align-items: center; margin-bottom: 3px;",
            # Heart on the left
            div(style = "flex: 1; min-width: 100px; display: flex; justify-content: flex-start;",
                uiOutput("healthstate_heart")),
            # Text on the right
            div(style = "flex: 3; text-align: left; font-size: 16px; background-color: #eef7ff; 
             padding: 6px; border-radius: 6px;",
                p("In the survey, the level of health of each participant will 
           be varied. Here you will see a description of the level of 
           health for each participant. On a scale of 0 to 100%,
              the imagine to the left will show you how healthy the individual is"))
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
        

        div(class = "content-section", style = "text-align: center; margin-bottom: 5px;",
            actionButton("start_survey", "Start Survey", class = "btn btn-primary btn-md", 
                         style = "padding: 10px 25px; font-size: 16px;")),
        
        actionButton("Back", "Back",  
                     style = "font-size: 18px; padding: 10px 20px; margin-top: 20px;")
      )
    }
    #Survey page
    else {
      
      tagList(
        
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
    }
  })

  output$page_number <- renderUI({
    div(style = "text-align: center; font-size: 16px; font-weight: bold; margin-top: 10px;",
        paste("Question", survey_page()))
  }) 
  
  #Text for the health state
  output$healthstate_text <- renderUI({
    div(style = "text-align: center; font-size: 16px; font-weight: bold; margin-top: 10px;",
        paste("All patients have the following health conditions: ", healthstatedescriptor[[survey_page()]]))
  })
  
  # Rendering and filling in the rectangle for health states 
  output$healthstate_heart <- renderUI({
    health_state <- healthstate[[survey_page()]]  # Get current health state (0.2, 0.4, etc.)
    fill_percentage <- health_state * 100  # Convert to percentage (e.g., 0.6 -> 60%)
    
    # Convert fill percentage into correct Y-position and height
    total_height <- 24  # Total SVG height
    total_width <- 20   # Total SVG width
    fill_height <- round((fill_percentage / 100) * total_height, 2)  # Rounded fill height
    fill_y <- total_height - fill_height  # Start filling from the bottom
    
    rectangle_svg <- sprintf("
  <svg viewBox='0 0 24 24' width='110' height='110' xmlns='http://www.w3.org/2000/svg'>
    <!-- Rectangle Outline -->
    <rect x='0' y='0' width='24' height='24' stroke='black' stroke-width='1.2' fill='none' />
    
    <!-- Filling Rectangle -->
    <rect x='0' y='%f' width='24' height='%f' fill='red' opacity='1' />
  </svg>", fill_y, fill_height)
    
    div(style = "text-align: center; margin: 20px 0;",  # 20px top & bottom margin
        HTML(rectangle_svg))  # Insert the rectangle SVG
  })
  
  #Render the bar chart
  output$plot <- renderPlot({
    library(ggplot2)
    
    # Create a data frame for plotting
    data <- data.frame(
      Option = c("Option A", "Option B"),
      Gains = c(optionA$gains[[survey_page()]], 10)  # Option B always has 10 years
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