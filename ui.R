library(shiny)
library(shinythemes)
library(ggplot2)
library(gggenes)
library(dplyr)
library(tidyr)
library(patchwork)
library(tidyHeatmap)
library(BiocManager)
library(bslib)
library(ggvenn)
library(ggupset)
library(pdftools)
library(png)
library(DT)
library(stringr)
source("loading_data.R")
source('functions.R')
source('plots_code.R')

#### UI ####


options_app <- c("genome", "RNAplot", "CHIPplot")

text_content1 <- text_help_info_navigation

text_content2 <- "nothing to see here"



ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .sidebar-scroll {
        height: calc(100vh - 70px);
        overflow-y: auto;
      }
      .main-scroll {
        height: calc(100vh - 70px);
        overflow-y: auto;
      }
    "))
  ),
  theme = bs_theme(version = 5, bootswatch = "yeti"),
  navbarPage("Strep.N.A.",
             tabPanel("RNA-seq & ChIP-seq visualization",
                      layout_sidebar(
                        sidebar = sidebar(
                          width = 500,
                          div(class = "sidebar-scroll",
                              accordion(
                                accordion_panel("Navigation", class = "mb-3",div(
                                  class = "d-grid gap-2",
                                  div(
                                    class = "btn-group w-100",
                                    actionButton("btn_left", "◀", class = "btn-primary"),
                                    actionButton("btn_right", "▶", class = "btn-primary")
                                  ),
                                  div(
                                    class = "btn-group w-100",
                                    actionButton("btn_in", "Zoom in 🔍", class = "btn-secondary"),
                                    actionButton("btn_out", "Zoom out 🔍", class = "btn-secondary")
                                  )
                                ),
                                numericInput("lower_value", "Minimal value of plot", value = 4260000, step = 10000),
                                numericInput("higher_value", "Maximum value of plot", value = 4280000, step = 10000),
                                actionButton("apply_changes", "Apply Changes", class = "btn-primary w-100 mt-3")
                                ),
                                accordion_panel("Selection",
                                                card(
                                                bslib::input_switch("switch_species", label = HTML("Select <em>Streptomyces</em> species")),
                                                verbatimTextOutput("switch_value"),
                                                selectInput('options', 'Show/hide visualizations', options_app,
                                                            multiple=TRUE, selectize=TRUE,
                                                            selected = c('genome', 'RNAplot')),
                                                selectizeInput("select_gene", 
                                                               label = "Choose gene from list", 
                                                               choices = "all",
                                                               selected = "all",
                                                               options = list(maxOptions = 10000))),
                                                card(card_header('RNA-seq Plot Data'),
                                                selectInput('rna_select_1', 
                                                            label = NULL,
                                                            selected = "no data selected",
                                                            choices = c("no data selected"),
                                                            selectize = TRUE),
                                                conditionalPanel(condition = 'input.rna_select_1 != "no data selected"',
                                                                 uiOutput('comparison_1')
                                                ),
                                                selectInput('rna_select_2', 
                                                            label = ' ',  
                                                            selected = "no data selected",
                                                            choices = c("no data selected"),
                                                            selectize = TRUE),
                                                conditionalPanel(condition = 'input.rna_select_2 != "no data selected"',
                                                                 uiOutput('comparison_2')
                                                ),
                                                selectInput('rna_select_3', 
                                                            label = ' ',  
                                                            selected = "no data selected",
                                                            choices = c("no data selected"),
                                                            selectize = TRUE),
                                                conditionalPanel(condition = 'input.rna_select_3 != "no data selected"',
                                                                 uiOutput('comparison_3')
                                                ), full_screen = TRUE),
                                                card(card_header("ChIP-seq Plot Data"),
                                                selectInput('chip_select', 
                                                            label = NULL,  
                                                            selected = "no data selected",
                                                            choices = c("no data selected"),
                                                            selectize = TRUE),
                                                conditionalPanel(condition = 'input.chip_select != "no data selected"',
                                                                 uiOutput('comparison_chip')
                                                ), full_screen = TRUE)
                                ),
                                accordion_panel("Plot settings",
                                                h4("LogFC Filtering"),
                                                numericInput("higher_logFC", label = ("-higher value"), value = 0, step = 0.5, min = 0),
                                                numericInput("lower_logFC", label = ("-lower value"), value = 0, step = 0.5, max = 0),
                                                input_switch("my_switch", "Leave only significant genes (FDR <= 0.05)", value = FALSE),
                                                textOutput("switch_status")
                                                
                                ),
                                accordion_panel("Upload data",
                                                bslib::input_switch("switch_filetype", label = HTML("Is RNA-seq file in app format?")),
                                                verbatimTextOutput("switch_value_filetype"),
                                                fileInput("uploaded_file", "RNA-seq file"),
                                                conditionalPanel(
                                                  condition = "output.fileUploaded",
                                                  textAreaInput("file_name", "Add name for your file")
                                                ),
                                              
                                                fileInput("uploaded_file_chip", "CHIP-seq file"),
                                                conditionalPanel(
                                                  condition = "output.fileUploaded_chip",
                                                  textAreaInput("file_name_chip", "Add name for your file")
                                                )),
                                accordion_panel("Plot Download",
                                                downloadButton('download_plot', 'Download png plot'),
                                                numericInput('width_plot', 'Plot width [cm]', 45, min = 5, max = 1000),
                                                numericInput('height_plot', 'Plot height [cm]', 22, min = 5, max = 1000),
                                                numericInput('res_plot', 'Resolution', 300, min = 100, max = 500)
                                )
                              ))
                        ),
                        mainPanel(
                          div(class = "main-scroll",
                              tabsetPanel(type = "pills",
                                          tabPanel("Visualization",
                                                   plotOutput("all_plots", height = '800px')
                                          )),
                              tabsetPanel(type = 'pills',
                                          tabPanel("RNAseq on plot",
                                                   DT::DTOutput("rna_table")
                                          ),
                                          tabPanel("RNAseq no filter",
                                                   DT::DTOutput("rna_table_nofilter")
                                          ),
                                          tabPanel("CHIPseq in table",
                                                   DT::DTOutput("chip_table")
                                          ))), width = 12
                        )
                      )
             ),
             tabPanel("Comparison of data",
                      layout_sidebar(
                        sidebar = sidebar(
                          width = 500,
                          div(class = "sidebar-scroll",
                              accordion(
                                          accordion_panel("Selection",
                                                          card(card_header('Venn Diagram Data'),
                                                   selectInput('venn_select_1', 
                                                               label = NULL, 
                                                               selected = "no data selected",
                                                               choices = c("no data selected"),
                                                               selectize = TRUE),
                                                   conditionalPanel(
                                                     condition = 'input.venn_select_1 != "no data selected"',
                                                     uiOutput('comparison_venn_1')
                                                   ),
                                                   selectInput('venn_select_2', 
                                                               label = ' ',  
                                                               selected = "no data selected",
                                                               choices = c("no data selected"),
                                                               selectize = TRUE),
                                                   conditionalPanel(
                                                     condition = 'input.venn_select_2 != "no data selected"',
                                                     uiOutput('comparison_venn_2')
                                                   )),card(card_header("Heatmap gene name input"),
                                                   textAreaInput("gene_list", NULL))
                                          ),
                                          accordion_panel("Venn LogFC option",
                                                   numericInput("higher_logFC_venn", label = ("higher logFC"), value = 1.5, step = 0.1, min = 0),
                                                   numericInput("lower_logFC_venn", label = ("lower logFC"), value = -1.5, step = 0.1, max = 0)),
                                          accordion_panel("Plot download", 
                                                    card("Plot Download Venn",
                                                   downloadButton('download_plot_venn', 'Download png plot'),
                                                   numericInput('width_venn', 'Plot width [cm]', 20, min = 5, max = 1000),
                                                   numericInput('height_venn', 'Plot height [cm]', 14, min = 5, max = 1000),
                                                   numericInput('res_venn', 'Resolution', 200, min = 100, max = 500)),
                                                   card("Plot Download Heat",
                                                   downloadButton('download_plot_heat', 'Download png plot'),
                                                   numericInput('width_heat', 'Plot width [cm]', 20, min = 5, max = 1000),
                                                   numericInput('height_heat', 'Plot height [cm]', 14, min = 5, max = 1000),
                                                   numericInput('res_heat', 'Resolution', 200, min = 100, max = 500))
                                          )
                              ))
                        ),
                        mainPanel(
                          div(class = "main-scroll",
                              tabsetPanel(
                                type='pills', 
                                tabPanel("Venn", 
                                         plotOutput("venn_plot", height = '800px' ),
                                         DT::DTOutput('venn_table_common'), DT::DTOutput('venn_table_uncommon')), 
                                tabPanel("Heatmap", plotOutput("heatmap_plot", height = '800px' ),
                                         DT::DTOutput('heatmap_table')))), width = 12
                        )
                      )
             ),
             nav_panel("Help&Info",
            navset_pill(
            nav_panel(
               "Usage help",
               card(
                 p("Click the button below to download templates for data upload:"),
                 downloadButton("downloadExcel", "Download File"),
                 card_header("Tips for using the app"),
                 card_body(
                   markdown(text_content1)
                 )
               )
             ),
             
            nav_panel(
              "Data&packages",
              card(
                card_header("Packages used in app creation"),
                card_body(
                  DT::DTOutput("table_packages")
                )
              ),
              card(
                card_header("Publications from which data was taken"),
                card_body(
                  DT::DTOutput("table_data")
                )
              )
            ),
             
             nav_panel(
               "About",
               card(
                 card_header("Additional information about author"),
                 card_body(
                   markdown(text_content2)
                 )
               )
             )))
  )
)