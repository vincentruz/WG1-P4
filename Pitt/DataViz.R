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

# Load clean dataset
# df_clean <- read.csv("~/YOUR FILE PATH HERE.csv")
df_clean <- read.csv("~/Box Sync/LSAP_LRDC/Research Projects/SEISMIC/AP/SEISMIC_AP/SEISMIC_AP_CLEAN.csv")

# Filter for student level inclusion/exclusion criteria
df_clean <- df_clean %>%
  # Include
  filter(transfer == 0) %>%
  filter(tookcourse_2 == 1) %>%
  filter(cohort >= 2013 & cohort <= 2018) %>%
  # Exclude
  filter(international == 0) %>%
  mutate(ethniccode_cat = relevel(as.factor(ethniccode_cat), ref= "1"))

# Create subset dataframes for each analysis sample (for each discipline)

# Bio
# Took 2nd course in sequence
df_bio2 <- df_clean %>%
  subset(discipline == "BIO") %>%
  subset(apyear >= 2013)

# Chem
# Took 2nd course in sequence
df_chem2 <- df_clean %>%
  subset(discipline == "CHEM") %>%
  subset(apyear >= 2014)

# Phys
# Took 2nd course in sequence
df_phys2 <- df_clean %>%
  subset(discipline == "PHYS") %>%
  subset(apyear >= 2015)

# For each Course
# BIO
# Generate estimates
fit_bio <- lm(numgrade_2 ~ scale(apscore_full) + factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) +
                scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
              df_bio2, na.action=na.exclude)
# Add column of fitted values
df_viz_bio <- df_bio2 %>%
  mutate(numgrade_2.fitted = fitted(fit_bio))

#CHEM
# Generate estimates
fit_chem <- lm(numgrade_2 ~ scale(apscore_full) + factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
               df_chem2, na.action=na.exclude)
# Add column of fitted values
df_viz_chem <- df_chem2 %>%
  mutate(numgrade_2.fitted = fitted(fit_chem))

#PHYSICS
# Generate estimates
fit_phys <- lm(numgrade_2 ~ scale(apscore_full) + factor(firstgen) + factor(lowincomeflag) + factor(gender) + factor(ethniccode_cat) +
                 scale(hsgpa) + scale(mathsr) + scale(englsr) + factor(crs_term_2), 
               df_phys2, na.action=na.exclude)
# Add column of fitted values
df_viz_phys <- df_phys2 %>%
  mutate(numgrade_2.fitted = fitted(fit_phys))

#Combine Disciplines
df_viz <- bind_rows(df_viz_bio, df_viz_chem, df_viz_phys) %>%
  mutate(skipped_course = if_else(eligible_to_skip == 0 & skipped_course == 1, 
                                  as.numeric(NA), as.numeric(skipped_course)))

head(names(df_viz))
  
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
                  selected = "CHEM"),
      #Select interaction terms
      selectInput(inputId = "var1", 
                  label = "Select a variable:",
                  choices = c("gender", "firstgen", "lowincomeflag", "ethniccode_cat", "urm"), 
                  selected = "gender")
    ),
  mainPanel(tabPanel(
      "Plot",
      fluidRow(
        plotOutput("linePlotUncontrolled1"),
        plotOutput("linePlotUncontrolled2"),
        plotOutput("linePlotFitted1"),
        plotOutput("linePlotFitted2"),
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
      filter(discipline == input$discipline) 
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

  output$linePlotUncontrolled1 <- renderPlot({
    ggplot(data=chartData(), aes(y = numgrade_2, x = apscore_full, color = as.factor(skipped_course), fill = as.factor(skipped_course), na.omit = TRUE)) +
      geom_point(stat = 'summary', fun.y = 'mean', size=3) +
      stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.1) +
      geom_smooth(stat = 'summary', method = 'loess') + 
      scale_x_continuous(limits = c(0,5), sec.axis = dup_axis(labels = NULL), breaks=seq(0, 5, by=1), labels=c("Didn't Take", "1", "2", "3", "4", "5")) +
      scale_y_continuous(limits = c(0,4), sec.axis = dup_axis(labels = NULL), breaks=seq(0, 4, by=0.5)) +
      theme_classic() +
      theme(
        panel.border = element_rect(color = "black", fill=NA), #put a border around the whole plot
        axis.title.x.top = element_blank(), #no x axis title on top
        axis.title.y.right = element_blank(), #no y axis title on the right
        legend.position=c(0.85, 0.25), #position the legend inside the plot (may want to make this adjustable with Shiny input options)
        legend.background = element_blank(),
        legend.box.background = element_rect(color = "black") #black rectangle surrounding the legend
      ) +
      #construct labels using the subj.label() defined earlier
      labs(x = "AP Score", y = paste("Mean Grade in", subj.label(), "2"), title= paste(subj.label(), "Uncontrolled Model"))
  })
  
  output$linePlotUncontrolled2 <- renderPlot({
    ggplot(data=chartData(), aes(y = numgrade_2, x = apscore_full, color = as.factor(skipped_course), fill = as.factor(skipped_course), na.omit = TRUE)) +
      facet_wrap(input$var1) +
      geom_point(stat = 'summary', fun.y = 'mean', size=3) +
      stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.1) +
      geom_smooth(stat = 'summary', method = 'loess') + 
      scale_x_continuous(limits = c(0,5), sec.axis = dup_axis(labels = NULL), breaks=seq(0, 5, by=1), labels=c("Didn't Take", "1", "2", "3", "4", "5")) +
      scale_y_continuous(limits = c(0,4), sec.axis = dup_axis(labels = NULL), breaks=seq(0, 4, by=0.5)) +
      theme_classic() +
      theme(
        panel.border = element_rect(color = "black", fill=NA), #put a border around the whole plot
        axis.title.x.top = element_blank(), #no x axis title on top
        axis.title.y.right = element_blank(), #no y axis title on the right
        legend.position=c(0.85, 0.25), #position the legend inside the plot (may want to make this adjustable with Shiny input options)
        legend.background = element_blank(),
        legend.box.background = element_rect(color = "black") #black rectangle surrounding the legend
      ) +
      #construct labels using the subj.label() defined earlier
      labs(x = "AP Score", y = paste("Mean Grade in", subj.label(), "2"), title= paste(subj.label(), "Uncontrolled Model"))
  })
  
  output$linePlotFitted1 <- renderPlot({
    ggplot(data=chartData(), aes(y = numgrade_2.fitted, x = apscore_full, color = as.factor(skipped_course), fill = as.factor(skipped_course), na.omit = TRUE)) +
      geom_point(stat = 'summary', fun.y = 'mean', size=3) +
      stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.1) +
      geom_smooth(stat = 'summary', method = 'loess') +
      scale_x_continuous(limits = c(0,5), sec.axis = dup_axis(labels = NULL), breaks=seq(0, 5, by=1), labels=c("Didn't Take", "1", "2", "3", "4", "5")) +
      scale_y_continuous(limits = c(0,4), sec.axis = dup_axis(labels = NULL), breaks=seq(0, 4, by=0.5)) +
      theme_classic() +
      theme(
        panel.border = element_rect(color = "black", fill=NA),
        axis.title.x.top = element_blank(),
        axis.title.y.right = element_blank(),
        legend.position=c(0.85, 0.25),
        legend.background = element_blank(),
        legend.box.background = element_rect(color = "black")
      ) +
      labs(x = "AP Score", y = paste("Mean Grade in", subj.label(), "2"), title= paste(subj.label(), "Fitted Model"))
    })
  
  output$linePlotFitted2 <- renderPlot({
    ggplot(data=chartData(), aes(y = numgrade_2.fitted, x = apscore_full, color = as.factor(skipped_course), fill = as.factor(skipped_course), na.omit = TRUE)) +
      facet_wrap(input$var1) +
      geom_point(stat = 'summary', fun.y = 'mean', size=3) +
      stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.1) +
      geom_smooth(stat = 'summary', method = 'loess') +
      scale_x_continuous(limits = c(0,5), sec.axis = dup_axis(labels = NULL), breaks=seq(0, 5, by=1), labels=c("Didn't Take", "1", "2", "3", "4", "5")) +
      scale_y_continuous(limits = c(0,4), sec.axis = dup_axis(labels = NULL), breaks=seq(0, 4, by=0.5)) +
      theme_classic() +
      theme(
        panel.border = element_rect(color = "black", fill=NA),
        axis.title.x.top = element_blank(),
        axis.title.y.right = element_blank(),
        legend.position=c(0.85, 0.25),
        legend.background = element_blank(),
        legend.box.background = element_rect(color = "black")
      ) +
      labs(x = "AP Score", y = paste("Mean Grade in", subj.label(), "2"), title= paste(subj.label(), "Fitted Model"))
  })
  
  output$histogram <- renderPlot({
    ggplot(data=chartData() %>% filter(apscore_full != 0), aes(x = apscore_full, color = as.factor(skipped_course), fill = as.factor(skipped_course), na.omit = TRUE)) +
      facet_wrap(input$var1) +
      geom_histogram(stat='count', position = position_dodge(preserve = "single")) +
      scale_x_continuous(sec.axis = dup_axis(labels = NULL)) +
      scale_y_continuous(sec.axis = dup_axis(labels = NULL)) +
      theme_classic() +
      theme(
        panel.border = element_rect(color = "black", fill=NA),
        axis.title.x.top = element_blank(),
        axis.title.y.right = element_blank(),
        legend.position=c(0.85, 0.25),
        legend.background = element_blank(),
        legend.box.background = element_rect(color = "black")
      ) +
      labs(x = "AP Score", y = "Number of Students", title= paste("Histogram of AP", subj.label(), "Scores"))
    })
}

# Launch Shiny App ####
shinyApp(ui = ui, server = server) # The link in the output wouldn't run anymore since I paused analyses on my kernel
# but if you run the codes in your local computer it should take you to a Shiny app with your own data.

