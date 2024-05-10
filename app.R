ui <- page_fluid(
  theme = bs_theme(5, 
                   secondary = "#0072B2", 
                   base_font = font_google("Inter")),
  layout_column_wrap(width = 1/2, min_height = "95vh",
                     card(card_header("Draw sample"),
                          card_body(
                            downloadButton("structure_download", "Structure", style = "width: 33%"),
                            fileInput("prefix_upload", "Upload Prefixes", accept = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
                            tags$br(),
                            layout_column_wrap(width = 1/3,
                                               numericInput("sample_size_input", NULL, value = 0, min = 0) |> 
                                                 tooltip("Size of sample", placement = "bottom"),
                                               input_task_button("draw_btn", "Draw"),
                                               downloadButton("sample_download", "Sample")
                                               )
                          )
                          ),
                     card(full_screen = TRUE,
                          card_header("Distribution details"),
                          card_body(plotOutput("plot_distribution"))
                          )
                     )
)

server <- function(input, output, session) {
  
  output$structure_download <- downloadHandler(
    filename = function() {
      "structure_for_prefixes.xlsx"
    },
    content = function(file) {
      prepare_structure(file)
    }
  )
  
  prefixes <- reactive({
    req(input$prefix_upload$datapath)
    req(tools::file_ext(input$prefix_upload$datapath) == "xlsx")
    get_prefixes(input$prefix_upload$datapath)
  })
  
  generated_sample <- reactive({
    req(prefixes())
    req(input$sample_size_input > 0)
    draw_sample(input$sample_size_input, prefixes())
  }) |> 
    bindEvent(input$draw_btn)
  
  output$plot_distribution <- renderPlot({
    req(generated_sample())
    show_operators_distribution(transform_to_df(generated_sample()))
  })
  
  output$sample_download <- downloadHandler(
    filename = function() {
      paste0("generated_sample_", format(Sys.time(), '%Y-%m-%d %H:%M:%S'), ".xlsx")
    },
    content = function(file) {
      save_sample(transform_to_df(generated_sample()), file)
    }
  )
  
}

shinyApp(ui, server)
