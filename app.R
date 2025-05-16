library(shiny)
library(tidyverse)
library(readxl)
library(bslib)
library(ggplot2)

# Load data
ergebnisse_raw <- read_excel("wg_clash_ergebnisse.xlsx")

# Data wrangling
ergebnisse <- ergebnisse_raw %>% 
  pivot_longer(where(is.numeric),
               names_to = "Team",
               values_to = "Punkte") %>% 
  pivot_wider(names_from = Spieltag,
              values_from = Punkte) %>% 
  rename(spieltag_1 = "1. Spieltag",
         spieltag_2 = "2. Spieltag",
         spieltag_3 = "3. Spieltag",
         spieltag_4 = "4. Spieltag")

tabelle_details <- ergebnisse %>% 
  group_by(Team) %>%
  summarise(
    spieltag_1 = sum(spieltag_1, na.rm = TRUE),
    spieltag_2 = sum(spieltag_2, na.rm = TRUE),
    spieltag_3 = sum(spieltag_3, na.rm = TRUE),
    spieltag_4 = sum(spieltag_4, na.rm = TRUE)
  ) %>% 
  mutate(gesamt = spieltag_1 + spieltag_2 + spieltag_3 + spieltag_4) %>% 
  arrange(desc(gesamt))

tabelle_details_cum <- tabelle_details %>% 
  mutate(
    spieltag_2 = spieltag_1 + spieltag_2,
    spieltag_3 = spieltag_2 + spieltag_3,
    spieltag_4 = spieltag_3 + spieltag_4
  ) %>% 
  pivot_longer(
    cols = starts_with("spieltag"),
    names_to = "Spieltag",
    values_to = "Punkte"
  ) %>%
  mutate(
    Spieltag = factor(Spieltag, levels = c("spieltag_1", "spieltag_2", "spieltag_3", "spieltag_4"))
  ) %>%
  unique()

# UI
ui <- fluidPage(
  theme = bs_theme(
    preset = "darkly",  # Dark mode theme
    base_font = font_google("Atkinson Hyperlegible Next"),
    primary = "#12936E",  # Accent color
    secondary = "#12936E" # You can change this if you'd like a different secondary color
  ),
  
  # Add overall margin and padding
  tags$head(
    tags$style(HTML("
      body {
        margin: 40px;
        padding: 20px;
      }
      h3 {
        color: #12936E;  /* Accent color for headers */
      }
    "))
  ),
  
  titlePanel("WG CLASH 2025 Dashboard"),
  
  fluidRow(
    h3("Tabelle"),
    tableOutput("tabelle_details")
  ),
  
  fluidRow(
    h3("Punkteverlauf"),
    plotOutput("punkte_plot", height = "400px")
  )
)




# Server
server <- function(input, output, session) {
  
  output$tabelle_details <- renderTable(tabelle_details, digits = 0)
  
  output$punkte_plot <- renderPlot({
    ggplot(tabelle_details_cum, aes(x = Spieltag, y = Punkte, color = Team, group = Team)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      theme_dark() +  # Dark background for the plot
      labs(
        title = "Punkteverlauf pro Team über die Spieltage",
        x = "Spieltag",
        y = "Punkte"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.title = element_blank(),  # Optional: remove legend title
        legend.text = element_text(color = "white")  # Make legend text white
      )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
