# SEISMIC AP Shiny App 
# Load required packages #
if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load("tidyverse", "dplyr", "shiny", "gridExtra")

# Import raw dataset (change the .csv name to your own filename if needed) #
# Imported .csv must have the following columns:
#   discipline: A string labeling the course sequence (e.g., the name of the first course in the sequence)
#   apscore_full: A number from 0-5 indicating the score on the AP exam associated with that discipline, with 0 meaning the student did not take the exam
#   numgrade_2: A numeric grade (0-4) in the second course in the sequence
#   numgrade_2.fitted: The numeric grade in the second course in the sequence fitted with a linear regression
#   skipped_course: A string indicating whether or not the student skipped the first course in the sequence

df_viz <- read_csv("~/SEISMIC_AP_VIZ.csv")
  
#   ==== Build Shiny App ====
# Define UI ####
ui <- fluidPage(
  titlePanel("Grades by AP Score"),
  sidebarLayout(
    sidebarPanel(
      #Select course to plot
      selectInput(inputId = "discipline", 
                  label = "Select a course:",
                  choices = unique(df_viz$discipline), 
                  selected = "CHEM")
    ),
  mainPanel(tabPanel(
      "Plot",
      fluidRow(
        plotOutput("linePlotUncontrolled"),
        plotOutput("linePlotFitted"),
        plotOutput("histogram")
      )
    )))
)

# Define Server ####
server <- function(input, output) {
  #Subset data based on the given course sequence
  #though we'll let ggplot do most of the summary later, we'll precalculate some of the means and standard errors to use for error bars
  chartData <- reactive({
    df_viz %>%
      filter(!is.na(skipped_course)) %>%
      filter(discipline == input$discipline) %>%
      group_by(apscore_full, skipped_course) %>%
      mutate(n = n()) %>%
      mutate(numgrade_2.mean = mean(numgrade_2, na.rm = TRUE),
             numgrade_2.sd = sd(numgrade_2, na.rm = TRUE),
             numgrade_2.n = sum(!is.na(numgrade_2)),
             numgrade_2.se = numgrade_2.sd / sqrt(numgrade_2.n),
             numgrade_2.fitted.mean = mean(numgrade_2.fitted, na.rm = TRUE),
             numgrade_2.fitted.sd = sd(numgrade_2.fitted, na.rm = TRUE),
             numgrade_2.fitted.n = sum(!is.na(numgrade_2.fitted)),
             numgrade_2.fitted.se = numgrade_2.fitted.sd / sqrt(numgrade_2.fitted.n))
  })
  
  #Get a string based on the chosen course to use in figure labels
  #Modify this to match the strings in your discipline variable
  subj.label <- reactive({
    case_when(
      input$discipline == "BIO" ~ "Biology",
      input$discipline == "CHEM" ~ "Chemistry",
      input$discipline == "PHYS"~ "Physics"
    )
  })
  
  output$linePlotUncontrolled <- renderPlot({
    ggplot(data=chartData(), aes(y = numgrade_2, x = apscore_full, color = as.factor(skipped_course), fill = as.factor(skipped_course), na.omit = TRUE)) +
      geom_point(stat = 'summary', fun.y = 'mean', size=3) +
      geom_errorbar(aes(ymin = numgrade_2.mean - numgrade_2.se, ymax = numgrade_2.mean + numgrade_2.se), width=0.1) + #add standard error bars
      geom_smooth(stat = 'summary', method = 'loess') + 
      #on y and x axes: sec.axis = dup_axis(labels = NULL) adds tick marks to the opposite side of the bounding box without adding duplicated labels
      #x axis: since non-apscore_full takers are coded as 0, manually relabel the tick marks to replace "0" with "Didn't Take"
      scale_x_continuous(sec.axis = dup_axis(labels = NULL), labels=c("Didn't Take", "1", "2", "3", "4", "5")) +
      #y-axis: set breaks to every 0.5 grade points
      scale_y_continuous(sec.axis = dup_axis(labels = NULL), breaks=seq(0, 4, by=0.5)) +
      theme_classic() +
      theme(
        panel.border = element_rect(color = "black", fill=NA), #put a border around the whole plot
        axis.title.x.top = element_blank(), #no x axis title on top
        axis.title.y.right = element_blank(), #no y axis title on the right
        legend.position=c(0.25, 0.85), #position the legend inside the plot (may want to make this adjustable with Shiny input options)
        legend.background = element_blank(),
        legend.box.background = element_rect(color = "black") #black rectangle surrounding the legend
      ) +
      #construct labels using the subj.label() defined earlier
      labs(x = "AP Score", y = paste("Mean Grade in", subj.label(), "2"), title= paste(subj.label(), "Uncontrolled Model"))
  })
  
  output$linePlotFitted <- renderPlot({
    ggplot(data=chartData(), aes(y = numgrade_2.fitted, x = apscore_full, color = as.factor(skipped_course), fill = as.factor(skipped_course), na.omit = TRUE)) +
      geom_point(stat = 'summary', fun.y = 'mean', size=3) +
      geom_errorbar(aes(ymin = numgrade_2.fitted.mean - numgrade_2.fitted.se, ymax = numgrade_2.fitted.mean + numgrade_2.fitted.se), width=0.1) +
      geom_smooth(stat = 'summary', method = 'loess') +
      scale_x_continuous(sec.axis = dup_axis(labels = NULL), breaks=seq(0, 5, by=1), labels=c("Didn't Take", "1", "2", "3", "4", "5")) +
      scale_y_continuous(sec.axis = dup_axis(labels = NULL), breaks=seq(0, 4, by=0.5)) +
      theme_classic() +
      theme(
        panel.border = element_rect(color = "black", fill=NA),
        axis.title.x.top = element_blank(),
        axis.title.y.right = element_blank(),
        legend.position=c(0.25, 0.85),
        legend.background = element_blank(),
        legend.box.background = element_rect(color = "black")
      ) +
      labs(x = "AP Score", y = paste("Mean Grade in", subj.label(), "2"), title= paste(subj.label(), "Fitted Model"))
  })
  
  output$histogram <- renderPlot({
    ggplot(data=chartData() %>% filter(apscore_full != 0), aes(x = apscore_full, color = as.factor(skipped_course), fill = as.factor(skipped_course), na.omit = TRUE)) +
      geom_histogram(stat='count', position = position_dodge(preserve = "single")) +
      scale_x_continuous(sec.axis = dup_axis(labels = NULL)) +
      scale_y_continuous(sec.axis = dup_axis(labels = NULL)) +
      theme_classic() +
      theme(
        panel.border = element_rect(color = "black", fill=NA),
        axis.title.x.top = element_blank(),
        axis.title.y.right = element_blank(),
        legend.position=c(0.25, 0.85),
        legend.background = element_blank(),
        legend.box.background = element_rect(color = "black")
      ) +
      labs(x = "AP Score", y = "Number of Students", title= paste("Histogram of AP", subj.label(), "Scores"))
  })
}

# Launch Shiny App ####
shinyApp(ui = ui, server = server) # The link in the output wouldn't run anymore since I paused analyses on my kernel
# but if you run the codes in your local computer it should take you to a Shiny app with your own data.

