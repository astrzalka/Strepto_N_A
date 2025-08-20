#### SERVER #####




# AS added to allow shiny to load files bigger than default 5 Mb
options(shiny.maxRequestSize=30*1024^2) 


server <- function(input, output, session) {
  
  #### SWITCH FOR SPECIES ####  
  
  switch_status <- reactive({
    switch_status <- input$switch_species
    return(switch_status)
  })
  output$switch_value <- reactive({
    if (switch_status() == FALSE)
    {switch_text <- "Streptomyces coelicolor"}
    else
    {switch_text <- "Streptomyces venezuelae"}
    return(switch_text)
  })
  
  #### SWITCH FOR FILE RNA SEQ FORMAT ####
  
  switch_status_filetype <- reactive({
    switch_status <- input$switch_filetype
    return(switch_status)
  })
  output$switch_value_filetype <- reactive({
    if (switch_status_filetype() == FALSE)
    {switch_text <- "Yes"}
    else
    {switch_text <- "No"}
    return(switch_text)
  })
  
  
  
  #### server-side select choice of genes ####
  
  
  
  observe({
    if(switch_status() == FALSE) {
      genelist <- read.csv("datasets/genes_scoelicolor.txt", sep = '')
    } else {
      genelist <- read.csv("datasets/sven_genes_vnz.txt", sep = '')
    }
    
    gene_list_database <- c("all", genelist$gene)
    
    updateSelectizeInput(session, 
                         inputId = "select_gene", 
                         choices = gene_list_database,
                         selected = "all",
                         server = TRUE)
    
    updateSelectizeInput(session, 
                         inputId = "select_gene_venn", 
                         choices = gene_list_database,
                         selected = "all",
                         server = TRUE)
    
    updateSelectizeInput(session, 
                         inputId = "select_gene_intime", 
                         choices = gene_list_database,
                         selected = "all",
                         server = TRUE)
  })
  
  changes_applied <- reactiveVal(FALSE) # This will store whether the button has been pressed
  
  observeEvent(input$apply_changes, {
    changes_applied(TRUE)  # Set the value to TRUE when the button is pressed
  })
  
  
  #### APPLY CHANGES BUTTON ####
  
  changes_applied_lower <- eventReactive(c(input$apply_changes, input$btn_left, input$btn_right, input$btn_in, input$btn_out), {
    input$lower_value
  }, ignoreNULL = FALSE)
  
  changes_applied_higher <- eventReactive(c(input$apply_changes, input$btn_left, input$btn_right, input$btn_in, input$btn_out), {
    input$higher_value
  }, ignoreNULL = FALSE)
  
  
  #### ZOOM BUTTONS ####
  button_states <- reactiveValues(
    b1 = FALSE,
    b2 = FALSE,
    b3 = FALSE,
    b4 = FALSE
  )
  
  ### button left
  
  observeEvent(input$btn_left, {
    button_states$b1 <- TRUE
    new_value_low <- input$lower_value - 10000
    new_value_high <- input$higher_value - 10000
    updateNumericInput(
      session = session, inputId = "lower_value", value = new_value_low)
    updateNumericInput(
      session = session, inputId = "higher_value", value = new_value_high)
    button_states$b1 <- FALSE
  })
  
  ### button right
  
  observeEvent(input$btn_right, {
    button_states$b2 <- TRUE
    new_value_low <- input$lower_value + 10000
    new_value_high <- input$higher_value + 10000
    updateNumericInput(
      session = session, inputId = "lower_value", value = new_value_low)
    updateNumericInput(
      session = session, inputId = "higher_value", value = new_value_high)
    button_states$b2 <- FALSE
  })
  
  ### button in
  
  observeEvent(input$btn_in, {
    button_states$b3 <- TRUE
    new_value_low <- input$lower_value + 10000
    new_value_high <- input$higher_value - 10000
    updateNumericInput(
      session = session, inputId = "lower_value", value = new_value_low)
    updateNumericInput(
      session = session, inputId = "higher_value", value = new_value_high)
    button_states$b3 <- FALSE
  })
  
  ### button out
  
  observeEvent(input$btn_out, {
    button_states$b4 <- TRUE
    new_value_low <- input$lower_value - 10000
    new_value_high <- input$higher_value + 10000
    updateNumericInput(
      session = session, inputId = "lower_value", value = new_value_low)
    updateNumericInput(
      session = session, inputId = "higher_value", value = new_value_high)
    button_states$b4 <- FALSE
  })
  
  
  
  switch_state <- reactive({
    input$my_switch
  })
  
  output$switch_status <- renderText({
    if(switch_state()) {
      "ON"
    } else {
      "OFF"
    }
  })
  
  
  #### PLOT GENOME INPUT ####
  
  plotgenomeInput <- reactive({
    req(changes_applied())
    
    # Determine which dataset to use based on switch status
    file_path <- if(switch_status()) {
      "datasets/sven_genes_vnz.txt"
    } else {
      "datasets/genes_scoelicolor.txt"
    }
    
    # Read the data once
    plot_data <- read.csv(file_path, sep = '')
    
    # Filter if a specific gene is selected
    if(input$select_gene != "all") {
      plot_data <- plot_data %>% filter(gene == input$select_gene)
    }
    
    return(plot_data)
  })
  
  
  
  
  #### USER DATA UPLOAD RNASEQ ####
  
  user_data_upload <- reactive({
    req(changes_applied())
    
    user_file <- input$uploaded_file$datapath
    if(is.null(user_file)){
      return(NULL)
    } else {
      user_file <- read.csv(user_file, sep = "\t")}
    
    if(switch_status_filetype() == FALSE){
      user_file <- user_file
    } else {
      user_file <- data_noformat_formating(user_file)}
    
    
    # Use input$file_name directly here
    user_file$data_name <- input$file_name
    return(user_file)
    
  })
  
  
  
  #### USER DATA UPLOAD CHIPSEQ ####
  user_data_upload_chip <- reactive({
    req(changes_applied())
    
    user_file <- input$uploaded_file_chip$datapath
    if(is.null(user_file)){
      return(NULL)
    } else {
      user_file <- read.csv(user_file, sep = "\t")
      # Use input$file_name directly here
      user_file$data_name <- input$file_name_chip
      return(user_file)
    }
  })
  
  output$fileUploaded_chip <- reactive({
    !is.null(input$uploaded_file_chip)
  })
  outputOptions(output, "fileUploaded_chip", suspendWhenHidden = FALSE)
  
  
  
  #### MERGING USER DATA ####
  
  merged_user <- reactive({
    req(changes_applied())
    
    user_data <- user_data_upload()
    
    if (is.null(user_data)) {
      return(NULL)
    }
    
    # Determine which genome data to use based on switch status
    if (switch_status() == FALSE) {
      data_genome <- read.csv("datasets/genes_scoelicolor.txt", sep = '')
    } else {
      data_genome <- read.csv("datasets/sven_genes_vnz.txt", sep = '')
    }
    
    
    # Merge the data and remove NA values
    merged_data <- user_data %>%
      left_join(data_genome, by = "gene") %>%
      na.omit()
    
    if(switch_status() == TRUE) {
      merged_data <- merged_data %>% select("gene","logFC","FDR","add_variable","start","end","strand", 'data_name')
    }
    
    return(merged_data)
  })
  
  
  
  output$fileUploaded <- reactive({
    !is.null(input$uploaded_file)
  })
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
  
  
  
  
  
  
  #### LOADING RNA-SEQ DATA ####
  
  #loading_data::data_in_app
  dataset_loaders <- data_in_app
  
  
  #loading_data::data_sven/data_scoe
  data_loaded_rna <- reactive({
    base_datasets <- if (switch_status()) {
      data_sven
    } else {
      data_scoe
    }
    
    if (!is.null(input$file_name)) base_datasets <- c(base_datasets, input$file_name)
    base_datasets
    
  })
  observe({
    choices <- c("no data selected", data_loaded_rna())
    sapply(paste0("rna_select_", 1:3), function(id) {
      updateSelectInput(session, id, choices = choices)
    })
  })
  
  # Cache loaded datasets
  loaded_datasets <- reactiveVal(list())
  
  dataselection_rnaseq_before_LHfilter <- reactive({
    req(changes_applied())
    
    # Get selected datasets (excluding "no data selected")
    selected_datasets <- setdiff(
      c(input$rna_select_1, input$rna_select_2, input$rna_select_3),
      "no data selected"
    )
    
    # Filter selected datasets based on data order
    data_order <- data_loaded_rna()
    selected_datasets <- intersect(selected_datasets, data_order)
    
    if (length(selected_datasets) == 0) return(NULL)
    
    # Get current cache
    current_cache <- loaded_datasets()
    
    # Load only uncached datasets
    for (dataset_name in selected_datasets) {
      if (is.null(current_cache[[dataset_name]])) {
        if (dataset_name == input$file_name) {
          current_cache[[dataset_name]] <- merged_user()
        } else if (dataset_name %in% names(dataset_loaders)) {
          current_cache[[dataset_name]] <- dataset_loaders[[dataset_name]]()
        }
      }
    }
    
    # Update cache
    loaded_datasets(current_cache)
    
    # Combine selected datasets
    do.call(rbind, current_cache[selected_datasets])
  })
  
  
  output$table_test <- renderDataTable({dataselection_rnaseq_before_LHfilter()})
  
  
  #### LOWER/HIGHERVALUE ####
  
  
  
  
  
  observeEvent(input$select_gene,{
    if(input$select_gene != 'all'){
      updateNumericInput(
        session = session, inputId = "lower_value", value = plotgenomeInput()$start
      )
    }
  })
  
  observeEvent(input$select_gene,{
    if(input$select_gene != 'all'){
      updateNumericInput(
        session = session, inputId = "higher_value", value = plotgenomeInput()$end
      )
    }
  })
  
  
  #### FDR FILTER ####
  
  
  dataselection_rnaseq_FDR_filter <- reactive({
    data_rna <- dataselection_rnaseq_before_LHfilter()
    if(switch_state()) {
      data_rna <- data_rna %>% filter(FDR <= 0.05)
      return(data_rna)
    } else {
      return(data_rna)
    }
  })
  
  
  #### RNA SELECTION LOW/HIGH, comparison FILTER ####
  
  dataselection_rnaseq <- reactive({
    
    req(changes_applied())
    
    data_rna <- dataselection_rnaseq_FDR_filter()
    
    data_rna <- data_rna %>% filter(
      start >= changes_applied_lower(), 
      end <= changes_applied_higher(), 
      add_variable %in% c(input$comparison_1, input$comparison_2, input$comparison_3))
    return(data_rna)
  })
  
  
  
  
  #### CONDITIONAL FOR comparisonS ####
  
  
  # AS tworzy selectInput do wyboru kontrastów na podstawie danych wybranych w rna_select_1
  output$comparison_1 <- renderUI({
    if (input$rna_select_1 == 'no data selected'){
      return(NULL)
    }
    
    data_rna <- dataselection_rnaseq_before_LHfilter()
    
    # AS wszystkie unikalne kontrasty w danych 
    grupy <- data_rna %>% filter(data_name == input$rna_select_1) %>% pull(add_variable) %>% unique()
    
    selectInput("comparison_1", "Choose comparisons for analysis",
                choices = grupy, selected = grupy[1], multiple = TRUE)
    
  })
  
  # AS tworzy selectInput do wyboru kontrastów na podstawie danych wybranych w rna_select_1
  output$comparison_2 <- renderUI({
    if (input$rna_select_2 == 'no data selected'){
      return(NULL)
    }
    
    data_rna <- dataselection_rnaseq_before_LHfilter()
    
    # AS wszystkie unikalne kontrasty w danych 
    grupy <- data_rna %>% filter(data_name == input$rna_select_2) %>% pull(add_variable) %>% unique()
    
    selectInput("comparison_2", "Choose comparisons for analysis",
                choices = grupy, selected = grupy[1], multiple = TRUE)
    
  })
  
  
  # AS tworzy selectInput do wyboru kontrastów na podstawie danych wybranych w rna_select_1
  output$comparison_3 <- renderUI({
    if (input$rna_select_3 == 'no data selected'){
      return(NULL)
    }
    
    data_rna <- dataselection_rnaseq_before_LHfilter()
    
    # AS wszystkie unikalne kontrasty w danych 
    grupy <- data_rna %>% filter(data_name == input$rna_select_3) %>% pull(add_variable) %>% unique()
    
    selectInput("comparison_3", "Choose comparisons for analysis",
                choices = grupy, selected = grupy[1], multiple = TRUE)
    
  })
  
  
  
  
  
  lower_logFC <- reactive({ input$lower_logFC })
  higher_logFC <- reactive({ input$higher_logFC })
  
  #### FILTERING GENOME DATA ####
  
  filtergenomedata <- reactive({
    
    req(changes_applied())
    
    
    if(switch_status() == FALSE)
    {
      plot_data_genome <- read.csv("datasets/genes_scoelicolor.txt", sep = '')
    }
    else{
      plot_data_genome <- read.csv("datasets/sven_genes_vnz.txt", sep = '')
    }
    
    plot_data_genome_filter <- plot_data_genome %>% filter(start >= changes_applied_lower(), end <= changes_applied_higher())
    
    return(plot_data_genome_filter)
  })
  
  #### Load TSS data ####
  
  TSSdata <- reactive({
    
    req(changes_applied())
    
    if(switch_status() == FALSE)
    {
      tss_data <- read.table("datasets/TSS/tss_coelicolor.txt")
    }
    else{
      tss_data <- read.table("datasets/TSS/tss_venezuelae.txt")
    }
    
    tss_data_filter <- tss_data %>% filter(Start_tss >= changes_applied_lower(), Start_tss <= changes_applied_higher())
    
    return(tss_data_filter)
    
  })
  
  
  #### GENOMEPLOT ####
  
  genomeplot <- reactive({
    
    req(changes_applied())
    
    plot_data_genome <- filtergenomedata()
    plot_data_genome <- plot_data_genome %>% mutate(strand_plot = ifelse(strand == '+', 1, 0))
    plot_data_genome <- plot_data_genome %>% distinct(gene, .keep_all = TRUE)
    
    #plots_code::genome_plot_create
    
    genome_plot <- genome_plot_create(
      plot_data_genome = plot_data_genome, 
      lower = changes_applied_lower(), 
      higher = changes_applied_higher())
    
    return(genome_plot)
  })
  
  
  #### TSSPLOT ####
  
  TSSplot <- reactive({
    
    req(changes_applied())
    
    tss_data <- TSSdata()
    
    #plots_code::tss_plot_create
    
    tss_plot <- tss_plot_create(tss_data,
                                lower = changes_applied_lower(), 
                                higher = changes_applied_higher())
    
  })
  
  
  #### RNAPLOT ####
  
  RNAplot <- reactive({
    
    req(changes_applied())
    
    plot_data_rna <- dataselection_rnaseq()
    plot_data_rna_highlog <- plot_data_rna %>% filter(logFC >= higher_logFC())
    plot_data_rna_lowlog <- plot_data_rna %>% filter(logFC <= lower_logFC())
    plot_data_rna_logFC_filtered <- rbind(plot_data_rna_lowlog, plot_data_rna_highlog)
    plot_data_rna <- plot_data_rna_logFC_filtered %>% mutate(strand_plot = ifelse(strand == '-', 0, 1))
    
    #plots_code::rna_plot_create
    
    rna_plot <- rna_plot_create(
      plot_data_rna = plot_data_rna,
      lower = changes_applied_lower(),
      higher = changes_applied_higher())
    return(rna_plot)
  })
  
  
  
  
  
  #### CHIPSEQ SELECTION ####
  
  # Define available ChIP-seq datasets as a constant
  CHIP_DATASETS <- reactive({
    if (input$switch_species) {
      data_chipseq_sven
    } else {
      data_chipseq_scoe
    }
  })
  
  # Dynamic choices reactive
  available_datasets <- reactive({
    if (!is.null(input$file_name_chip)) {
      return(c("no data selected", CHIP_DATASETS(), input$file_name_chip))
    } else {
      return(c("no data selected", CHIP_DATASETS()))
    }
  })
  
  observe({
    updateSelectInput(session, "chip_select", 
                      choices = available_datasets())
  })
  
  # Main data loading reactive
  dataselection_chipseq_before_LHfilter <- reactive({
    req(changes_applied())
    
    # Create a named list of data loading functions
    data_loaders <- data_load_chipseq
    
    # Get selected datasets (excluding "no data selected")
    selected_datasets <- input$chip_select[input$chip_select != "no data selected"]
    
    # Handle both preset and uploaded data
    all_data <- list()
    
    # Load preset datasets
    preset_datasets <- intersect(CHIP_DATASETS(), selected_datasets)
    if (length(preset_datasets) > 0) {
      preset_data <- lapply(preset_datasets, function(dataset) {
        data_loaders[[dataset]]()
      })
      all_data <- c(all_data, preset_data)
    }
    
    # Add uploaded data if selected
    if (!is.null(input$file_name_chip) && input$file_name_chip %in% selected_datasets) {
      uploaded_data <- user_data_upload_chip()
      if (!is.null(uploaded_data)) {
        all_data <- c(all_data, list(uploaded_data))
      }
    }
    
    # Combine all data
    do.call(rbind, all_data)
  })
  
  # Filtered data reactive
  dataselection_chipseq <- reactive({
    req(changes_applied())
    
    dataselection_chipseq_before_LHfilter() %>%
      filter(
        chromStart >= changes_applied_lower(),
        chromEnd <= changes_applied_higher(),
        name %in% input$comparison_chip
      )
  })
  
  # Dynamic UI for comparison selection
  output$comparison_chip <- renderUI({
    req(input$chip_select != 'no data selected')
    
    grupy <- dataselection_chipseq_before_LHfilter() %>%
      filter(data_name == input$chip_select) %>%
      pull(name) %>%
      unique()
    
    selectInput("comparison_chip", 
                "Choose comparisons for analysis",
                choices = grupy, 
                selected = grupy[1], 
                multiple = TRUE)
  })
  
  #### CHIP SEQ PLOT ####
  
  draw_chip_plot <- reactive({
    
    req(changes_applied())
    req(input$chip_select != "no data selected")
    
    if(is.null(input$chip_select)){
      return(NULL)}
    
    data_chip <- dataselection_chipseq()
    
    #plots_code::chip_plot_create
    
    chip_seq_final_plot <- chip_plot_create(
      data_chip = data_chip,
      lower = changes_applied_lower(),
      higher = changes_applied_higher()
    )
    
    return(chip_seq_final_plot)
    
    
  })
  
  
  
  
  #### TABLES INPUT ####
  
  
  tableInput_rna <- reactive({
    
    req(changes_applied())
    
    table_data <- dataselection_rnaseq()
    table_data_rna_lowlog <- table_data %>% filter(logFC >= higher_logFC())
    table_data_rna_highlog <- table_data %>% filter(logFC <= lower_logFC())
    table_data_rna_logFC_filtered <- rbind(table_data_rna_highlog, table_data_rna_lowlog)
    table_data_rna_logFC_filtered$logFC <- round(table_data_rna_logFC_filtered$logFC, 2)
    return(table_data_rna_logFC_filtered)
  })
  output$rna_table <- DT::renderDT({
    
    req(changes_applied())
    
    table_data <- tableInput_rna()
    rownames(table_data) <- NULL
    return(table_data)
  })
  
  tableInput_nofilter <- reactive({
    
    req(changes_applied())
    
    table_data1 <- dataselection_rnaseq_before_LHfilter()
    table_data1$logFC <- round(table_data1$logFC, 2)
    return(table_data1)
  })
  output$rna_table_nofilter <- DT::renderDT({
    
    req(changes_applied())
    
    table_data1 <- tableInput_nofilter()
    rownames(table_data1) <- NULL
    return(table_data1)
  })
  
  tableInput_chip <- reactive({
    
    req(changes_applied())
    
    table_data1 <- dataselection_chipseq()
    return(table_data1)
  })
  output$chip_table <- DT::renderDT({
    
    req(changes_applied())
    
    table_data1 <- tableInput_chip()
    return(table_data1)
  })
  
  tableInput_chip_nofilter <- reactive({
    
    req(changes_applied())
    
    table_data1 <- dataselection_chipseq_before_LHfilter()
    return(table_data1)
  })
  output$chip_table_nofilter <- DT::renderDT({
    
    req(changes_applied())
    
    table_data1 <- tableInput_chip_nofilter()
    return(table_data1)
  })
  
  ##inputpackages
  tableInput_packages <- reactive({
    
    
    
    table_data1 <- read.csv("datasets/packages.txt", sep = '\t')
    return(table_data1)
  })
  
  output$table_packages <- DT::renderDT({
    
    
    
    table_data <- tableInput_packages()
    rownames(table_data) <- NULL
    return(table_data)
  })
  
  
  ##inputdata
  tableInput_data <- reactive({
    
    
    
    table_data1 <- read.csv("datasets/data_table_inapp.txt", sep = '\t')
    return(table_data1)
  })
  
  output$table_data <- DT::renderDT({
    
    
    
    table_data <- tableInput_data()
    rownames(table_data) <- NULL
    return(table_data)
  })
  
  
  
  #### PATCHWORK PLOTING MAIN PLOTS ####
  
  plot_all_patchwork <- reactive({
    req(changes_applied())
    
    all_possible_choices <- c('genome', 'RNAplot', 'CHIPplot', 'TSSplot')
    selected_plots <- input$options
    
    plot_list <- list()
    heights <- numeric()
    
    if ('genome' %in% selected_plots) {
      plot_list$genome <- genomeplot()
      heights <- c(heights, 1)
    }
    
    if ('TSSplot' %in% selected_plots) {
      
      plot_list$TSSplot <- TSSplot()
      heights <- c(heights, 1)
    }
    
    if ('RNAplot' %in% selected_plots) {
      rna_data <- tryCatch({
        dataselection_rnaseq()
      }, error = function(e) NULL)
      
      if (!is.null(rna_data) && nrow(rna_data) > 0) {
        plot_list$RNAplot <- RNAplot()
        heights <- c(heights, 8)
      }
    }
    
    if ('CHIPplot' %in% selected_plots && input$chip_select != "no data selected") {
      chip_data <- tryCatch({
        dataselection_chipseq()
      }, error = function(e) NULL)
      
      if (!is.null(chip_data) && nrow(chip_data) > 0) {
        plot_list$CHIPplot <- draw_chip_plot()
        heights <- c(heights, 2)
      }
    }
    
    if (length(plot_list) == 0) return(NULL)
    
    p_all <- patchwork::wrap_plots(plot_list, ncol = 1, heights = heights)
    return(p_all)
  })
  
  output$all_plots <- renderPlot({ plot_all_patchwork() })
  
  
  
  #### PLOT DOWNLOAD ####
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("combined_plots_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      plot <- plot_all_patchwork()
      
      selected_number <- which(c('genome', 'RNAplot', 'CHIPplot') %in% input$options)
      total_height <- sum(c(1, 8, 2)[selected_number])
      
      ggsave(
        filename = file,
        plot = plot,
        device = "png",
        width = input$width_plot,
        height = input$height_plot,
        units = "cm",
        dpi = input$res_plot
      )
    }
  )
  
  
  
  #### CONDITIONAL FOR comparisonS COMPARSION ####
  
  
  output$comparison_venn_1 <- renderUI({
    if (input$venn_select_1 == 'no data selected'){
      return(NULL)
    }
    
    data_rna <- dataselection_venn()
    
    grupy <- data_rna %>% filter(data_name == input$venn_select_1) %>% pull(add_variable) %>% unique()
    
    selectInput("comparison_venn_1", "Choose comparisons for venn",
                choices = grupy, selected = grupy[1], multiple = TRUE)
    
  })
  
  output$comparison_venn_2 <- renderUI({
    if (input$venn_select_2 == 'no data selected'){
      return(NULL)
    }
    
    data_rna <- dataselection_venn()
    
    grupy <- data_rna %>% filter(data_name == input$venn_select_2) %>% pull(add_variable) %>% unique()
    
    selectInput("comparison_venn_2", "Choose comparisons for venn",
                choices = grupy, selected = grupy[1], multiple = TRUE)
    
  })
  
  
  #### COMPARSION DATA LOAD ####
  
  
  #loading_data::data_in_app
  dataset_loaders_venn <- data_in_app
  
  
  #loading_data::data_sven/data_scoe
  data_loaded_venn <- reactive({
    base_datasets <- c(data_sven, data_scoe)
    if (!is.null(input$file_name)) base_datasets <- c(base_datasets, input$file_name)
    base_datasets
  })
  
  observe({
    choices <- c("no data selected", data_loaded_venn())
    sapply(paste0("venn_select_", 1:2), function(id) {
      updateSelectInput(session, id, choices = choices)
    })
  })
  
  loaded_datasets_venn <- reactiveVal(list())
  
  dataselection_venn <- reactive({
    req(changes_applied())
    
    # Get selected datasets (excluding "no data selected")
    selected_datasets <- setdiff(
      c(input$venn_select_1, input$venn_select_2),
      "no data selected"
    )
    
    # Filter selected datasets based on data order
    data_order <- data_loaded_venn()
    selected_datasets <- intersect(selected_datasets, data_order)
    
    if (length(selected_datasets) == 0) return(NULL)
    
    # Get current cache
    current_cache_venn <- loaded_datasets_venn()
    
    # Load only uncached datasets
    for (dataset_name in selected_datasets) {
      if (is.null(current_cache_venn[[dataset_name]])) {
        if (dataset_name == input$file_name) {
          current_cache_venn[[dataset_name]] <- merged_user()
        } else if (dataset_name %in% names(dataset_loaders_venn)) {
          current_cache_venn[[dataset_name]] <- dataset_loaders_venn[[dataset_name]]()
        }
      }
    }
    
    # Update cache
    loaded_datasets_venn(current_cache_venn)
    
    # Combine selected datasets
    do.call(rbind, current_cache_venn[selected_datasets])
  })
  
  
  
  #### FILTERING DATA FOR VENN AND HEAT #####
  
  filter_data_for_heatmap <- reactive({
    req(input$comparison_venn_1)
    req(input$gene_list)
    
    data_rna <- dataselection_venn()
    gene_string <- input$gene_list
    gene_vector <- unlist(strsplit(gene_string, ", "))
    
    filtered_data <- data_rna %>% 
      filter(
        gene %in% gene_vector,
        add_variable %in% c(input$comparison_venn_1, input$comparison_venn_2),
        !is.infinite(abs(logFC))
      ) %>%
      filter(!if_all(everything(), is.na)) %>%
      select(gene, logFC, add_variable) %>%
      pivot_wider(id_cols = gene, names_from = add_variable, values_from = logFC) -> filtered_data
    
    filtered_data %>% filter(!if_all(2:ncol(filtered_data), is.na)) %>%
      pivot_longer(cols = 2:ncol(filtered_data), names_to = 'add_variable', values_to = 'logFC') -> filtered_data
    
    return(filtered_data)
  })
  
  
  filter_data_for_venn <- reactive({
    higher_logFC <- input$higher_logFC_venn
    lower_logFC <- input$lower_logFC_venn
    data_rna <- dataselection_venn()
    data_rna1 <- data_rna %>% filter(add_variable %in% c(input$comparison_venn_1, input$comparison_venn_2),
                                     logFC >= input$higher_logFC_venn)
    data_rna2 <- data_rna %>% filter(add_variable %in% c(input$comparison_venn_1, input$comparison_venn_2),
                                     logFC <= input$lower_logFC_venn)
    data_rna_filtered <- rbind(data_rna1, data_rna2)
    data_rna_filtered <- data_rna_filtered %>% filter(FDR <= 0.05)
    return(data_rna_filtered)
    
  })
  
  prep_data_venn <- reactive({
    data_set_venn <- filter_data_for_venn()
    gene_lists <- split(data_set_venn$gene, data_set_venn$add_variable)
    return(gene_lists)
  })
  
  
  #### VENN PLOT CREATE ####
  
  #plots_code::venn_plot_create
  
  output$venn_plot <- renderPlot({ 
    venn_plot_create(
      gene_lists = prep_data_venn(),
      data_set_venn = filter_data_for_venn()) })
  
  
  #### TABLE UNDER VENN ####
  
  data_venn_table_common <- reactive({
    
    gene_lists <- prep_data_venn()
    
    if(length(gene_lists) < 2){return(NULL)}
    
    result <- find_common_elements(gene_lists)
    
    return(result)
    
  })
  
  data_venn_table_uncommon <- reactive({
    gene_lists <- prep_data_venn()
    
    if(length(gene_lists) < 2){return(NULL)}
    
    result <- find_uncommon_elements(gene_lists)
    
    return(result)
  })
  
  
  
  
  output$venn_table_common <- renderDataTable({data_venn_table_common()})
  output$venn_table_uncommon <- renderDataTable({data_venn_table_uncommon()})
  
  
  
  
  
  #### HEATMAP PLOT####
  
  
  output$heatmap_plot <- renderPlot({
    heat_plot_create(
      heat_data = filter_data_for_heatmap()
    ) })
  
  
  tableInput_heatmap <- reactive({
    
    req(changes_applied())
    
    table_data1 <- filter_data_for_heatmap()
    table_data1$logFC <- round(table_data1$logFC, 2)
    return(table_data1)
  })
  
  
  output$heatmap_table <- DT::renderDT({
    table_data <- tableInput_heatmap()
    rownames(table_data) <- NULL
    return(table_data)
  })
  
  
  
  
  
  #### PLOT DOWNLOAD VENN AND HEAT ####
  
  output$download_plot_venn <- downloadHandler(
    filename = function() {
      paste0("venn_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      # Explicitly create the plot at the time of download
      plot <- venn_plot_create(
        gene_lists = prep_data_venn(),
        data_set_venn = filter_data_for_venn())
      
      # Save the plot with ggsave
      ggsave(
        filename = file,
        plot = plot,
        device = "png",
        width = input$width_venn,
        height = input$height_venn,
        units = "cm",
        dpi = input$res_venn
      )
    }
  )
  
  output$download_plot_heat <- downloadHandler(
    filename = function() {
      paste0("heatmap_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      # Get the plot
      p <- heat_plot_create(
        heat_data = filter_data_for_heatmap()
      )  # Your function that creates the tidyheatmap
      
      # For tidyheatmap, we need to use pdf first then convert to png
      # because tidyheatmap is based on ComplexHeatmap
      tmp_pdf <- tempfile(fileext = ".pdf")
      
      # Save as PDF first
      pdf(tmp_pdf, 
          width = input$width_heat/2.54,    # Convert cm to inches
          height = input$height_heat/2.54)   # Convert cm to inches
      print(p)
      dev.off()
      
      # Convert PDF to PNG using pdftools
      png::writePNG(
        pdftools::pdf_render_page(
          tmp_pdf, 
          dpi = input$res_heat
        ),
        target = file
      )
      
      # Clean up temporary file
      unlink(tmp_pdf)
    },
    contentType = "image/png"
  )
  
  output$downloadExcel <- downloadHandler(
    filename = function() {
      paste0("templates_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      plik_wejsciowy <- file.path("datasets/templates_for_app.xlsx")
      file.copy(plik_wejsciowy, file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  #### Gene Inspector Tabset ####
  
  # Reactive logic to update gene name based on species selection
  observeEvent(input$switch_species, {
    if (input$switch_species) {
      # When switch is ON (TRUE) - for example, different species
      updateTextInput(session, "gene_name", value = "vnz_13325")  # The same gene but S. venezuelae
    } else {
      # When switch is OFF (FALSE) - default species
      updateTextInput(session, "gene_name", value = "SCO2950")  # Default for S. coelicolor
    }
  })
  
  # Change value of the choose species switch, added on the gene inspector page so that the user does not have to switch between pages
  observeEvent(input$master_switch, {
    bslib::update_switch("switch_species", value = input$master_switch, session = session)
  })
  # bidirectional control (both switches affect each other)
  observeEvent(input$switch_species, {
    bslib::update_switch("master_switch", value = input$switch_species, session = session)
  })
  
  #### Load RNA-seq data ####
  
  load_all_rnaseq_data <- reactive({
    # Get the appropriate dataset list based on switch status
    if (switch_status()) {
      # Venezuelae datasets
      dataset_names <- data_sven
    } else {
      # Coelicolor datasets
      dataset_names <- data_scoe
    }
    
    # Load all datasets for the selected organism
    loaded_data_list <- list()
    
    for (dataset_name in dataset_names) {
      if (dataset_name %in% names(data_in_app)) {
        # Load the dataset using the corresponding loader function
        loaded_data_list[[dataset_name]] <- data_in_app[[dataset_name]]()
      }
    }
    
    # Combine all loaded datasets into one data frame
    if (length(loaded_data_list) > 0) {
      combined_data <- do.call(rbind, loaded_data_list)
      return(combined_data)
    } else {
      return(NULL)
    }
  })
  
  #### Plot RNA-seq data ####
  
  output$rna_plot <- renderPlot({
    
    rnaseq_data <- load_all_rnaseq_data()
    
    rnaseq_data %>% dplyr::filter(gene == input$gene_name, FDR <= 0.05, abs(logFC) > input$logFC) %>%
      mutate(direction = ifelse(logFC > 0, 'up', 'down'))->
      rnaseq_data_filtered
    
    rnaseq_data_filtered %>% ggplot(aes(x = logFC, y = add_variable))+
      geom_segment(aes(xend = 0, yend = add_variable, color = direction), linewidth = 1.5)+
      geom_point(size = 3)+
      scale_color_brewer(palette = 'Set1', guide = NULL)+
      theme_classic()+
      facet_grid(data_name~., scales = 'free_y', space = 'free_y')+
      ylab('Comparison')+
      theme(text = element_text(size = 16))
    
  })
  
  output$rna_data <- renderDT({
    rnaseq_data <- load_all_rnaseq_data()
    
    rnaseq_data %>% dplyr::filter(gene == input$gene_name, FDR <= 0.05, abs(logFC) > input$logFC) %>%
      mutate(direction = ifelse(logFC > 0, 'up', 'down'))->
      rnaseq_data_filtered
    return(rnaseq_data_filtered)
  })
  
  #### Description panel ####
  
  load_all_description <- reactive({
    # Get the appropriate dataset list based on switch status
    if (switch_status()) {
      # Venezuelae datasets
      genes_table <- read.table('datasets/sven_genes_table.txt')
    } else {
      # Coelicolor datasets
      genes_table <- read.table('datasets/scoeli_genes_table.txt')
    }
    return(genes_table)
  })
  
  # scoeli_genes_table <- read.table('datasets/scoeli_genes_table.txt')
  # Function to get gene information
  get_gene_info <- reactive({
    # Get the gene data
    scoeli_genes_table <- load_all_description()
    
    gene_id <- trimws(input$gene_name)
    
    # Find the gene in the table
    gene_data <- scoeli_genes_table[scoeli_genes_table$gene == gene_id, ]
    
    if (nrow(gene_data) == 0) {
      return(list(
        found = FALSE,
        message = paste("Gene", gene_id, "not found in database.")
      ))
    } else {
      return(list(
        found = TRUE,
        data = gene_data
      ))
    }
  })
  
  # Render gene description in a more compact format
  output$gene_description <- renderUI({
    gene_info <- get_gene_info()
    
    if (!gene_info$found) {
      return(
        div(
          class = "alert alert-warning",
          icon("exclamation-triangle"),
          gene_info$message
        )
      )
    }
    
    gene_data <- gene_info$data
    
    # Create a more compact layout with columns
    div(
      class = "container-fluid p-0",
      
      # Title row
      div(
        class = "row mb-2",
        div(
          class = "col-12",
          h5(
            span(gene_data$gene, style = "color: #28a745; font-weight: bold;"),
            " - ",
            span(gene_data$product, style = "font-style: italic; font-size: 0.9em;")
          )
        )
      ),
      
      # Info rows in 2 columns
      div(
        class = "row",
        
        # Column 1
        div(
          class = "col-6",
          div(
            class = "mb-1",
            tags$b("Alt. names: "),
            tags$span(style = "font-size: 0.85em;", gene_data$gene_names)
          ),
          div(
            class = "mb-1",
            tags$b("Position: "),
            tags$span(style = "font-size: 0.85em;",
                      paste(formatC(gene_data$start, big.mark=","), "-",
                            formatC(gene_data$end, big.mark=",")))
          ),
          div(
            class = "mb-1",
            tags$b("Strand: "), gene_data$strand,
            tags$span(style = "margin-left: 10px;"),
            tags$b("Length: "),
            paste(formatC(gene_data$end - gene_data$start + 1, big.mark=","), "bp")
          )
        ),
        
        # Column 2
        div(
          class = "col-6",
          div(
            class = "mb-1",
            tags$b("Protein: "),
            tags$span(paste(gene_data$protein_length, "aa")),
            tags$span(style = "margin-left: 10px;"),
            tags$b("UniProt: "),
            tags$a(href = paste0("https://www.uniprot.org/uniprot/", gene_data$Entry),
                   target = "_blank", gene_data$Entry)
          ),
          div(
            class = "mb-1 text-truncate",
            style = "max-width: 100%;",
            tags$b("Protein: "),
            tags$span(
              style = "font-size: 0.85em; font-style: italic;",
              title = gene_data$protein_name,
              gene_data$protein_name
            )
          ),
          div(
            style = "font-size: 0.85em; overflow: visible; white-space: normal;",
            title = gene_data$note,
            tags$b("Note: "),
            gene_data$note
          )
        )
      )
    )
  })
  
#### Normalized expression over time plot ####
  
  
  # Create expression time plot in the bottom left card
  output$expressionTimePlot <- renderPlot({
    
    if (input$switch_species) {
      # Species switch is ON - load S. venezuelae datasets
      expression_data <- read.table('datasets/sven_rna_normalized.txt')
      
      # add genes for comparison
      if(!is.null(input$compare_genes)){
        add_genes <- input$compare_genes
        add_genes <- trimws(unlist(strsplit(add_genes, split = ',')))
      } else {
        add_genes <- NULL
      }
      
      ggplot(expression_data %>% filter(gene %in% c(input$gene_name, add_genes)), 
             aes(x = hours2, y = expression, color = gene, shape = factor(experiment))) +
        geom_line(size = 1, linetype = 2) +
        geom_point(size = 3) +
        labs(
          #title = paste("Normalized expression of", long_data$gene),
          x = "Growth Time (hours)",
          y = "Expression Level"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text = element_text(size = 14),
          panel.grid.minor = element_blank(),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14),
          legend.position = 'bottom'
        )+
        scale_shape_discrete(name = 'exp')
      
    } else {
      # Species switch is OFF - load S. coelicolor datasets  
      expression_data <- read.table('datasets/yeong_2016_rna.txt')
      # Reshape the data for plotting
      long_data <- tidyr::pivot_longer(
        expression_data,
        cols = c("M", "T", "L", "S"),
        names_to = "time_point",
        values_to = "expression"
      )
      # add genes for comparison
      if(!is.null(input$compare_genes)){
        add_genes <- input$compare_genes
        add_genes <- trimws(unlist(strsplit(add_genes, split = ',')))
        long_data <- filter(long_data, gene %in% c(input$gene_name, add_genes))
      } else {
        # filter
        long_data <- filter(long_data, gene == input$gene_name)
      }
      
      # Add hours information
      long_data$hours <- factor(
        long_data$time_point,
        levels = c("M", "T", "L", "S"),
        labels = c("14", "18", "22", "36")
      )
      long_data$hours2 <- as.numeric(as.character(long_data$hours))
      
      
      # Create the line plot
      ggplot(long_data, aes(x = hours2, y = expression, color = gene)) +
        geom_line(size = 1, linetype = 2) +
        geom_point(size = 3) +
        labs(
          title = paste("Normalized expression of", long_data$gene),
          x = "Growth Time (hours)",
          y = "Expression Level"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text = element_text(size = 14),
          panel.grid.minor = element_blank(),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14),
          legend.position = 'bottom'
        )
    }
    
  })
  
#### 16S RNA Tree plot ####
  
  tree_rna <- readRDS('datasets/tree_rna_small.rds')
  
  for(i in 1:length(tree_rna$tip.label)){
    tree_rna$tip.label[i] <- paste(strsplit(tree_rna$tip.label[i], "\\s+")[[1]][1:2], collapse = " ")
  }
  
  output$rna_tree_plot <- renderPlot({
    
    tabela_gene <- read.delim(paste0('datasets/wyniki_homology_search/', input$gene_name, '_homologysearch_full.txt'))
    tabela_gene %>% dplyr::group_by(organism) %>%
      dplyr::filter(Score >= input$score, length >= input$length_min, length <= input$length_max) %>%
      dplyr::mutate(n = dplyr::n(),
                    label = paste(strsplit(organism, "\\s+")[[1]][1:2], collapse = " ")) %>%
      select(label, n) %>%
      distinct() -> tabela_gene
    
    tree_rna %>% tidytree::full_join(tabela_gene) -> tree_rna_full
    
    ggtree::ggtree(tree_rna_full)+
      ggtree::geom_tiplab(size=4)+
      ggtree::geom_tree(aes(color=n), size = 1.5)+
      xlim(NA, 1.5)+
      scale_color_viridis_c()+
      theme(legend.position = 'bottom')
  })
  
  output$tree_data <- renderDT({
    tabela_gene <- read.delim(paste0('datasets/wyniki_homology_search/', input$gene_name, '_homologysearch_full.txt'))
    tabela_gene %>% dplyr::group_by(organism) %>%
      dplyr::filter(Score >= input$score, length >= input$length_min, length <= input$length_max) %>%
      dplyr::mutate(n = dplyr::n(),
                    label = paste(strsplit(organism, "\\s+")[[1]][1:2], collapse = " "),
                    Score = round(Score, 1),
                    identity = round(identity2, 2),
                    uniprot_accs = sub("^[^|]+\\|([^|]+)\\|.*", "\\1", subject_name)) %>%
      ungroup() %>%
      select(Organism = label, uniprot_accs, Score, identity, proteom, length, sequence)
  })
  
  #### ChIP-seq plot #####
  
  all_chipseq_data <- reactive({
    # Get the appropriate dataset list based on species switch
    if (input$switch_species) {
      # Species switch is ON - load S. venezuelae datasets
      dataset_loaders <- data_load_chipseq
      dataset_names <- data_chipseq_sven
    } else {
      # Species switch is OFF - load S. coelicolor datasets  
      dataset_loaders <- data_load_chipseq
      dataset_names <- data_chipseq_scoe
    }
    
    # Load all datasets for the selected species
    all_data <- list()
    
    for (dataset_name in dataset_names) {
      tryCatch({
        # Load the dataset using the appropriate loader function
        dataset <- dataset_loaders[[dataset_name]]()
        
        # Add dataset name as a column for identification
        if (!is.null(dataset) && nrow(dataset) > 0) {
          dataset$dataset_source <- dataset_name
          all_data[[dataset_name]] <- dataset
        }
      }, error = function(e) {
        cat("Error loading dataset", dataset_name, ":", conditionMessage(e), "\n")
      })
    }
    
    # Combine all datasets into a single data.frame
    if (length(all_data) > 0) {
      combined_data <- do.call(rbind, all_data)
      return(combined_data)
    } else {
      return(NULL)
    }
  })
  
  genes_chipseq_plot <- reactive({
    # Get the appropriate dataset list based on species switch
    if (input$switch_species) {
      # Species switch is ON - load S. venezuelae datasets
      genes <- read.table('datasets/sven_genes_vnz.txt')
      
    } else {
      # Species switch is OFF - load S. coelicolor datasets  
      genes <- read.table('datasets/genes_scoelicolor.txt')
      
    }
    
    return(genes)
  })
  
  # Dynamic UI for ChIP-seq dataset selection
  output$chip_data_inspector_ui <- renderUI({
    chip_data <- all_chipseq_data()
    
    if (is.null(chip_data)) {
      # Return disabled selectInput when no data available
      selectInput(
        'chip_data_inspector',
        'Choose ChIP-seq datasets',
        choices = c("No data available"),
        selected = NULL,
        multiple = TRUE
      )
    } else {
      # Create id column and get unique values
      chip_data_with_id <- chip_data %>% 
        mutate(id = paste(name, data_name, sep = '.'))
      
      unique_ids <- unique(chip_data_with_id$id)
      
      selectInput(
        'chip_data_inspector',
        'Choose ChIP-seq datasets',
        choices = unique_ids,
        selected = if (length(unique_ids) > 0) unique_ids[1] else NULL,
        multiple = TRUE
      )
    }
  })
  
  # the ui will generate even if the accordion panel is hidden from the user
  outputOptions(output, "chip_data_inspector_ui", suspendWhenHidden = FALSE)
  
  
  chip_plot <- reactive({
    
    # chip_data <- read.table('datasets/scoe_chip.txt') %>% mutate(id = paste(name, data_name, sep = '.'))
    chip_data <- all_chipseq_data()
    chip_data %>% mutate(id = paste(name, data_name, sep = '.')) -> chip_data
    
    scoelicolor_genes <- genes_chipseq_plot()
    
    gene <- scoelicolor_genes %>% filter(gene == input$gene_name)
    min <- gene$start - (input$flank + 1000)
    max <- gene$end + input$flank + 1000
    gene_plot <-  scoelicolor_genes %>% filter(start >= min, end <= max)
    
    chip_filtered <- chip_data %>% filter(chromStart >= gene$start - input$flank - 1000, chromEnd <= gene$end+input$flank + 1000) %>%
      filter(id %in% input$chip_data_inspector)
    
    ggplot(gene_plot, aes(x = start, xend = end, y = 0, yend = 0))+
      geom_segment(size = 3)+
      theme_classic()+
      coord_cartesian(xlim = c(gene$start-input$flank, gene$end+input$flank),
                      ylim = c(-0.9, 0.4))+
      theme(axis.line.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            text = element_text(size = 14),
            legend.position = 'bottom',
            legend.direction = 'vertical')+
      geom_segment(data = chip_filtered, aes(x = chromStart, y = -0.5, xend = chromEnd, yend = -0.5, color = id),
                   size = 3, alpha = 0.5)+
      geom_text(aes(x = (start+end)/2, y = -0.1, label = gene))+
      scale_color_discrete(name = 'ChIP-seq')+
      xlab('genomic position')
    
  })
  
  output$chipseq_plot <- renderPlot({chip_plot()})

}