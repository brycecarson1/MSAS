library(shiny)
library(shinyWidgets)
library(ggplot2)
library(gt)
library(gtExtras)
library(DT)
library(dplyr)
library(bslib)
library(stringr) 
library(tidyverse)

l_pairs <- read.csv("l_pair_stats.csv")
r_pairs <- read.csv("r_pair_stats.csv")
l_cluster <- read.csv("l_cluster_stats.csv")
r_cluster <- read.csv("r_cluster_stats.csv")

l_pairs['hand'] = "L"
r_pairs['hand'] = "R"
r_cluster['hand'] = "R"
l_cluster['hand'] = "L"

pairs <- rbind(l_pairs, r_pairs)
clusters <- rbind(l_cluster, r_cluster)


clusters <- clusters |> 
  mutate(
    release_speed = round(release_speed, 2),
    release_spin_rate = round(release_spin_rate),
    pfx_x = round(pfx_x, 2),
    pfx_z = round(pfx_z, 2),
    woba_value = round(woba_value, 3),
    ground_ball = round(ground_ball, 2),
    popup = round(popup, 2),
    sw_str = round(sw_str, 2),
    weak_contact = round(weak_contact, 2))

pairs <- pairs |> 
  mutate(
    mean_woba_value = round(mean_woba_value, 3),
    mean_ground_ball = round(100 * mean_ground_ball, 2),
    mean_popup = round(100 * mean_popup, 2),
    mean_sw_str = round(100 * mean_sw_str, 2),
    mean_weak_contact = round(100 * mean_weak_contact, 2))

pairs$combination <- gsub("['()]", "", pairs$combination)
pairs$combination <- lapply(pairs$combination, function(x) unlist(strsplit(x, ",")))

all_pitches <- unique(clusters$pitch_type)
all_clusters <- unique(clusters$pitch_cluster)

ui <- navbarPage("MLB Pitch Clustering",
                 theme = bs_theme(bootswatch = "lumen"),
                 
                 tabPanel("Pitch Cluster Stats",
                          fluidRow(
                            column(3,
                                   selectInput(
                                     inputId = "hand",
                                     label = "RHP or LHP:",
                                     choices = c("LHP" = "L", "RHP" = "R"),
                                     selected = "R")
                            ),
                            column(3,
                                   pickerInput("pitches",
                                               "Select Pitch Types:",
                                               choices = c("All" = "all", all_pitches),
                                               options = list(`actions-box` = TRUE),
                                               multiple = TRUE,
                                               selected = "all"))),
                          
                          mainPanel(
                            fluidRow(
                              align = "center",
                              gt_output(outputId = "cluster"),
                            ), width = 12
                            )),
                 
                 tabPanel("Cluster Pairing Stats",
                          fluidRow(
                            column(3,
                                   selectInput(
                                     inputId = "hand_c",
                                     label = "RHP or LHP:",
                                     choices = c("LHP" = "L", "RHP" = "R"),
                                     selected = "R")
                            ),
                            
                            column(3,
                                   pickerInput("clusters",
                                               "Select Clusters:",
                                               choices = c("All" = "all", all_clusters),
                                               options = list(`actions-box` = TRUE),
                                               multiple = TRUE,
                                               selected = "all"))),

                            mainPanel(
                              fluidRow(
                                align = "center",
                                gt_output(outputId = "pair"),
                              ), width = 12
                            )))
                 


server <- function(input, output, session) {
  output$cluster <- render_gt({
    
    temp_clusters <- clusters |> 
      filter(hand == input$hand)
      
    if (!"all" %in% input$pitches) {
      temp_clusters <- temp_clusters |> 
        filter(pitch_type %in% input$pitches)
    }

    gt_table <- temp_clusters |> 
      select("pitch_type", "pitch_cluster", "release_speed", "release_spin_rate", "pfx_x", "pfx_z",
      "woba_value", "ground_ball", "popup", "sw_str", "weak_contact") |> 
      gt() |> 
      gt_theme_espn() |>
      gt_hulk_col_numeric(ground_ball) |> 
      gt_hulk_col_numeric(popup) |> 
      gt_hulk_col_numeric(sw_str) |> 
      gt_hulk_col_numeric(weak_contact) |> 
      gt_hulk_col_numeric(woba_value, reverse = TRUE) |> 
      cols_align(align = "center") |> 
      opt_interactive(use_sorting = TRUE) |>
      opt_interactive(use_page_size_select = TRUE,
                      page_size_default = 31) |> 
      fmt_percent(columns = c(ground_ball, popup, sw_str, 
                              weak_contact),
                  scale_values = FALSE) |> 
      cols_label(pitch_type = "Pitch Type",
                 pitch_cluster = "Cluster",
                 release_speed = "Velocity",
                 release_spin_rate = "Spin Rate",
                 pfx_x = "Horizontal Movement (in.)",
                 pfx_z = "Vertical Movement (in.)",
                 woba_value = "wOBA",
                 ground_ball = "Ground Ball %",
                 popup = "Pop-up %",
                 sw_str = "Swinging Strike %",
                 weak_contact = "Weak Contact %") |> 
      tab_header(title = "Pitch Cluster Stats",
      subtitle = "Weak Contact is when contact is categorized as 'Weak', 'Topped', 'Under', or 'Flare/Burner' 
      based on launch angle and exit velocity. 
      Movement is from the catcher's perspective.") |> 
      opt_align_table_header(align = "center")

      
    gt_table
  })
  
  output$pair <- render_gt({
    
    temp_pairs <- pairs |> 
      filter(hand == input$hand_c)
    
    if (!"all" %in% input$clusters) {
        selected_clusters_regex <- paste(input$clusters, collapse="|")
        temp_pairs <- temp_pairs |> 
          filter(str_detect(combination, selected_clusters_regex))
    }
    
    pair_table <- temp_pairs |> 
      select("combination", "mean_woba_value", "mean_ground_ball", "mean_popup", "mean_sw_str", "mean_weak_contact") |> 
      gt() |> 
      gt_theme_espn() |>
      gt_hulk_col_numeric(mean_ground_ball) |> 
      gt_hulk_col_numeric(mean_popup) |> 
      gt_hulk_col_numeric(mean_sw_str) |> 
      gt_hulk_col_numeric(mean_weak_contact) |> 
      gt_hulk_col_numeric(mean_woba_value, reverse = TRUE) |> 
      cols_align(align = "center") |> 
      opt_interactive(use_sorting = TRUE) |> 
      opt_interactive(use_page_size_select = TRUE,
                      page_size_default = 20,
                      pagination_type = "jump") |> 
      cols_label(combination = "Pairing",
                 mean_woba_value = "wOBA",
                 mean_ground_ball = "Ground Ball %",
                 mean_popup = "Pop-up %",
                 mean_sw_str = "Swinging Strike %",
                 mean_weak_contact = "Weak Contact %")
    
    pair_table
  })
  
  
}
shinyApp(ui, server)







