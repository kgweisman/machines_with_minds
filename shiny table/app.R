library(RCurl)
library(tidyverse)
library(shiny)
library(DT)

percentage_url <- getURL("https://raw.githubusercontent.com/kgweisman/machines_with_minds/master/data/machines_with_minds_table_percentage.csv")

mean_url <- getURL("https://raw.githubusercontent.com/kgweisman/machines_with_minds/master/data/machines_with_minds_table_mean.csv")

percentage_tab <- read.csv(text = percentage_url) %>%
  rename(`Proportion Endorsing` = `X..Endorsing`,
         `Proportion Ambivalent or Pretending` = `X..Ambivalent.or.Pretending`,
         `Proportion Rejecting` = `X..Rejecting`) %>%
  mutate(Lens = gsub("[:digits:]) ", "", Lens),
         Lens = case_when(grepl("living", Lens) ~ "LIVING CREATURE (bodily)",
                          grepl("social", Lens) ~ "SOCIAL PARTNER (social-emotional)",
                          grepl("goal", Lens) ~ "GOAL-DIRECTED AGENT (perceptual-cognitive)")) %>%
  mutate_at(vars(starts_with("Proportion")),
            funs(gsub("%", "", .) %>% as.numeric()/100)) %>%
  filter(!is.na(`Proportion Endorsing`), `Proportion Endorsing` != "") %>%
  mutate_at(vars(starts_with("Proportion")),
            funs(format(., nsmall = 2)))
  
mean_tab <- read.csv(text = mean_url) %>%
  rename(`Mean (Normalized)` = `Mean..normalized.0.1.`) %>%
  mutate(Lens = gsub("[:digits:]) ", "", Lens),
         Lens = case_when(grepl("living", Lens) ~ "LIVING CREATURE (bodily)",
                          grepl("social", Lens) ~ "SOCIAL PARTNER (social-emotional)",
                          grepl("goal", Lens) ~ "GOAL-DIRECTED AGENT (perceptual-cognitive)")) %>%
  mutate(`Mean (Normalized)` = as.numeric(`Mean (Normalized)`)) %>%
  filter(!is.na(`Mean (Normalized)`)) %>%
  mutate(`Mean (Normalized)` = format(`Mean (Normalized)`, nsmall = 2))

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
                                        "Age group", "N", "Proportion Endorsing"),
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
                                        "Age group", "N", "Mean (Normalized)"),
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
    # DT::datatable(percentage_tab[, input$show_vars_percentage, drop = FALSE])
    DT::datatable(percentage_tab)
  })

  # choose columns to display
  output$mytable2 <- DT::renderDataTable({
    # DT::datatable(mean_tab[, input$show_vars_mean, drop = FALSE])
    DT::datatable(mean_tab)
  })
  
}

shinyApp(ui, server)
