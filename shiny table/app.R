library(shiny)
library(DT)
library(readxl)
library(tidyverse)

percentage_tab <- read_excel("/Users/kweisman/Documents/Research (Stanford)/Writing/Robots (CADA)/table_shiny/Machines with minds TABLE.xlsx", sheet = 1) %>%
  mutate(Lens = gsub("[:digits:]) ", "", Lens),
         Lens = case_when(grepl("living", Lens) ~ "LIVING CREATURE (bodily)",
                          grepl("social", Lens) ~ "SOCIAL PARTNER (social-emotional)",
                          grepl("goal", Lens) ~ "GOAL-DIRECTED AGENT (perceptual-cognitive)")) %>%
  mutate_at(vars(starts_with("%")),
            funs(. %>% substr(1, 5) %>% as.numeric() %>% round(2) %>% format(nsmall = 2)))

mean_tab <- read_excel("/Users/kweisman/Documents/Research (Stanford)/Writing/Robots (CADA)/table_shiny/Machines with minds TABLE.xlsx", sheet = 2) %>%
  rename(`Mean (normalized)` = `Mean (normalized 0-1)`) %>%
  mutate(Lens = gsub("[:digits:]) ", "", Lens),
         Lens = case_when(grepl("living", Lens) ~ "LIVING CREATURE (bodily)",
                          grepl("social", Lens) ~ "SOCIAL PARTNER (social-emotional)",
                          grepl("goal", Lens) ~ "GOAL-DIRECTED AGENT (perceptual-cognitive)")) %>%
  mutate_at(vars(starts_with("Mean")),
            funs(. %>% substr(1, 5) %>% as.numeric() %>% round(2) %>% format(nsmall = 2)))

ui <- fluidPage(
  title = "Machines with minds: Tables",
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        'input.dataset === "percentage_tab"',
        checkboxGroupInput("show_vars_percentage", 
                           "Select which colums to display:",
                           choices = names(percentage_tab),
                           # choices = c("Lens", "Capacity", "Study",
                           #             "Age group", "N", "% Endorsing"),
                           # selected = names(percentage_tab),
                           selected = c("Lens", "Capacity", "Study",
                                        "Age group", "N", "% Endorsing"),
                           width = "200px")
      ),
      conditionalPanel(
        'input.dataset === "mean_tab"',
        checkboxGroupInput("show_vars_mean", 
                           "Select which colums to display:",
                           choices = names(mean_tab),
                           # choices = c("Lens", "Capacity", "Study",
                           #             "Age group", "N", "% Endorsing"),
                           # selected = names(mean_tab),
                           selected = c("Lens", "Capacity", "Study",
                                        "Age group", "N", "Mean (normalized)"),
                           width = "200px")
      )
    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("percentages", DT::dataTableOutput("mytable1")),
        tabPanel("means", DT::dataTableOutput("mytable2"))
      )
    )
  )
)

server <- function(input, output) {
  
  # choose columns to display
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(percentage_tab[, input$show_vars_percentage, drop = FALSE])
  })

  # choose columns to display
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(mean_tab[, input$show_vars_mean, drop = FALSE])
  })
  
}

shinyApp(ui, server)
