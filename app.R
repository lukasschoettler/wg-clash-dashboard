library(shiny)
library(tidyverse)
library(readxl)
library(bslib)
library(ggplot2)
library(DT)

ergebnisse_raw <- read_excel("wg_clash_ergebnisse.xlsx")

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
  select(Team, where(~ is.numeric(.x) && sum(.x) != 0)) %>% 
  rowwise() %>%
  mutate(
    cum_punkte = list(cumsum(c_across(starts_with("spieltag_"))))
  ) %>%
  ungroup() %>%
  select(Team, cum_punkte) %>%
  unnest_wider(cum_punkte, names_sep = "_") %>%
  rename_with(~ paste0("spieltag_", seq_along(.)), starts_with("cum_punkte_")) %>%
  pivot_longer(
    cols = starts_with("spieltag_"),
    names_to = "Spieltag",
    values_to = "Punkte"
  ) %>%
  mutate(
    Spieltag = factor(Spieltag, levels = paste0("spieltag_", 1:4))
  ) %>%
  arrange(Team, Spieltag)

ui <- fluidPage(
  theme = bs_theme(
    preset = "minty",
    base_font = font_google("Atkinson Hyperlegible Next"),
    primary = "#12936E",
    secondary = "#12936E"),
  
  fluidRow(
    h3("Tabelle"),
    DTOutput("tabelle_details")
  ),
  
  fluidRow(
    h3("Punkteverlauf"),
    plotOutput("punkte_plot", height = "400px")
  )
)

server <- function(input, output, session) {
  
  output$tabelle_details <- renderDataTable({
    dt <- datatable(
      tabelle_details,
      options = list(paging = FALSE, searching = FALSE)
    )
    
    numeric_cols <- names(tabelle_details)[2:5]
    
    for (col in numeric_cols) {
      max_val <- max(tabelle_details[[col]], na.rm = TRUE)
      if (max_val > 0) {
        dt <- dt %>%
          formatStyle(
            columns = col,
            backgroundColor = styleEqual(max_val, "lightyellow")
          )
      }
    }
    
    dt
  })
  
  
  
  output$punkte_plot <- renderPlot({
    ggplot(tabelle_details_cum, aes(x = Spieltag, y = Punkte, color = Team, group = Team)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      labs(
        title = "Punkteverlauf pro Team über die Spieltage",
        x = "Spieltag",
        y = "Punkte"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.title = element_blank(),
      )
  })
  
}

shinyApp(ui = ui, server = server)