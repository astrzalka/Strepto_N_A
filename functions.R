# Function to calculate common elements for combinations of vectors from a list
find_common_elements <- function(vectors, min_overlap = 1) {
  # Get vector names
  vector_names <- names(vectors)
  if (is.null(vector_names)) {
    vector_names <- paste0("Vector", seq_along(vectors))
  }
  
  n <- length(vectors)
  
  # Check for at least two vectors
  if (n < 2) {
    stop("Please provide at least two vectors.")
  }
  
  # Get all combinations of the vectors (excluding single-vector combinations)
  all_combinations <- lapply(2:n, function(k) combn(seq_along(vectors), k, simplify = FALSE))
  all_combinations <- unlist(all_combinations, recursive = FALSE)
  
  # Find common elements for each combination
  results <- lapply(all_combinations, function(indices) {
    combo_name <- paste0("Common to: ", paste(vector_names[indices], collapse = ", "))
    common <- Reduce(intersect, vectors[indices])
    list(combination = combo_name, elements = common)
  })
  
  # Filter by minimum overlap (optional)
  if (min_overlap > 1) {
    results <- Filter(function(x) length(x$elements) >= min_overlap, results)
  }
  
  # Convert to a data frame for easy readability
  output <- do.call(rbind, lapply(results, function(res) {
    data.frame(
      Combination = res$combination,
      Elements = paste(res$elements, collapse = ", "),
      stringsAsFactors = FALSE
    )
  }))
  
  return(output)
}


find_uncommon_elements <- function(vectors, min_overlap = 1) {
  # Get vector names
  vector_names <- names(vectors)
  if (is.null(vector_names)) {
    vector_names <- paste0("Vector", seq_along(vectors))
  }
  
  n <- length(vectors)
  
  # Check for at least two vectors
  if (n < 2) {
    stop("Please provide at least two vectors.")
  }
  
  # Check for maximum of 4 vectors
  if (n > 4) {
    warning("This function is designed for up to 4 vectors. Using only the first 4 vectors.")
    vectors <- vectors[1:4]
    vector_names <- vector_names[1:4]
    n <- 4
  }
  
  # Create a result list
  results <- list()
  
  # Process each vector to find elements unique to it
  for (i in 1:n) {
    other_indices <- setdiff(1:n, i)
    other_vectors_union <- Reduce(union, vectors[other_indices])
    unique_elements <- setdiff(vectors[[i]], other_vectors_union)
    
    results[[i]] <- list(
      combination = paste0("Unique to: ", vector_names[i]),
      elements = unique_elements
    )
  }
  
  # Convert to a data frame for easy readability
  output <- do.call(rbind, lapply(results, function(res) {
    data.frame(
      Combination = res$combination,
      Elements = paste(res$elements, collapse = ", "),
      Count = length(res$elements),
      stringsAsFactors = FALSE
    )
  }))
  
  return(output)
}


data_noformat_formating <- function(data_test){
  
  #spliting data and removing suffixes
  dt_logFC <- data_test %>%
    select(gene, contains("logFC")) %>%
    rename_with(~str_remove(., "logFC"), -1)
  
  dt_FDR <- data_test %>%
    select(gene, contains("FDR")) %>%
    rename_with(~str_remove(., "FDR"), -1)
  
  #conversion to tidy
  dt_logFC <- dt_logFC %>% 
    pivot_longer(
      cols = !gene, 
      names_to = "add_variable", 
      values_to = "logFC"
    )
  
  dt_FDR <- dt_FDR %>% 
    pivot_longer(
      cols = !gene, 
      names_to = "add_variable", 
      values_to = "FDR"
    )
  
  #conversion to numerical
  dt_logFC$logFC <- gsub(",", ".", dt_logFC$logFC)
  dt_FDR$FDR <- gsub(",", ".", dt_FDR$FDR)
  dt_logFC$logFC <- as.numeric(dt_logFC$logFC)
  dt_FDR$FDR <- as.numeric(dt_FDR$FDR)
  
  #joining back into one
  data_in_tidy <- dt_logFC %>%
    left_join(dt_FDR, by = c("gene", "add_variable"))
  
  return(data_in_tidy)
}

load_table_packages <- function(){
  loaded_table <- read.csv("datasets/packages.txt", sep = '\t')
  return(loaded_table)
}

load_table_data <- function(){
  loaded_table <- read.csv("datasets/data_table_inapp.txt", sep = '\t')
  return(loaded_table)
}


text_help_info_navigation <- HTML('
  <div style="font-family: Arial, sans-serif; line-height: 1.6; padding: 10px;">
     
     <div style="font-size: 1.2em; margin-top: 1em; font-weight: bold;"><h2>📊 RNA-seq & ChIP-seq visualization</h2></div>
    <ul>
      <li>This page allows you to visualize and explore genomic data in various plot types (genome, RNA-seq, microarrays, ChIP-seq).</li>
      <li>The page consists of a sidebar for selecting data and settings, and a main panel for displaying the plots.</li>
    </ul>
    
    <div style="font-size: 1.2em; margin-top: 1em; font-weight: bold;">🧭 Tab: Navigation</div>
    <ul>
      <li><strong>Left / Right arrows</strong> – shift the genome plot by 10,000 base pairs on the X-axis.</li>
      <li><strong>Zoom in / Zoom out</strong> – zoom into or out of the displayed genome region.</li>
      <li><strong>Minimal / Maximum value of plot</strong> – manually set the plot axis limits.</li>
      <li><strong>Apply Changes</strong> – confirms manual axis changes and must be used to trigger reactivity.</li>
    </ul>

    <div style="font-size: 1.2em; margin-top: 1em; font-weight: bold;">🧬 Tab: Selection</div>
    <ul>
      <li><strong>Select Streptomyces species</strong> – switch between <em>S. coelicolor</em> and <em>S. venezuelae</em>.</li>
      <li><strong>Show/hide visualizations</strong> – choose which plot types to display (Genome, RNA-seq, Microarray, ChIP-seq).</li>
      <li><strong>Choose gene from list</strong> – zoom to the selected gene on the genome.</li>
      <li><strong>RNA-seq Plot Data</strong> – choose up to 3 RNA-seq datasets.</li>
      <li><strong>ChIP-seq Plot Data</strong> – choose one ChIP-seq dataset.</li>
      <li><strong>Choose comparison for analysis</strong> – select type of comparison (e.g., strain, timepoint, conditions).</li>
    </ul>

    <div style="font-size: 1.2em; margin-top: 1em; font-weight: bold;">📊 Tab: Plot settings</div>
    <ul>
      <li><strong>LogFC Filtering</strong> – filter plot based on lower/higher log fold change values.</li>
      <li><strong>FDR Filtering</strong> – show only data with FDR ≤ 0.05.</li>
    </ul>

    <div style="font-size: 1.2em; margin-top: 1em; font-weight: bold;">⬆️ Tab: Upload data</div>
    <ul>
      <li><strong>Browse… / drag & drop file</strong> – upload data file from your computer.</li>
      <li><strong>Is RNA-seq file in app format? (Yes/No)</strong> – indicate if the format matches app structure.</li>
      <li><strong>Custom file name</strong> – assign a name that will appear in plot selectors.</li>
    </ul>

    <div style="font-size: 1.2em; margin-top: 1em; font-weight: bold;">⬇️ Tab: Plot download</div>
    <ul>
      <li><strong>Download png plot</strong> – export plot as a PNG image.</li>
      <li><strong>Resolution and size settings</strong> – optionally adjust width, height, and DPI before download.</li>
    </ul>

    <div style="font-size: 1.2em; margin-top: 1em; font-weight: bold;">🧾 Additional: Data tables under plots</div>
    <ul>
      <li><strong>RNAseq on plot</strong> – genes visible on the RNA-seq plot.</li>
      <li><strong>RNAseq no filter</strong> – full RNA-seq dataset (unfiltered).</li>
      <li><strong>ChIPseq in table</strong> – genes shown on the ChIP-seq plot.</li>
      <li><strong>Search:</strong> – search bar to filter table contents.</li>
    </ul>

  </div>

  <div style="font-family: Arial, sans-serif; line-height: 1.6; padding: 10px;">
    
    <div style="font-size: 1.2em; margin-top: 1em; font-weight: bold;">📊 Comparison of data</div>
    <ul>
      <li>This page allows comparing datasets visually via Venn diagrams and heatmaps.</li>
      <li>Similar to the RNA-seq & ChIP-seq page, it consists of a sidebar for inputs and a main panel for plots.</li>
    </ul>

    <div style="font-size: 1.2em; margin-top: 1em; font-weight: bold;">🧬 Tab: Selection</div>
    <ul>
      <li><strong>Venn Diagram Data</strong> – choose up to 4 comparisons from RNA-seq or microarray datasets (including user uploads) for the Venn diagram and heatmap.</li>
      <li><strong>Heatmap Gene Name Input</strong> – type gene names (comma-separated) to filter and display them in the heatmap, e.g., SCO6276, SCO6277.</li>
    </ul>

    <div style="font-size: 1.2em; margin-top: 1em; font-weight: bold;">🔧 Tab: Venn LogFC option</div>
    <ul>
      <li>Allows setting upper and lower logFC thresholds for Venn diagram filtering.</li>
      <li>Does not affect the Heatmap.</li>
      <li>Only comparisons with logFC outside the set range are shown (default: -1.5 to 1.5).</li>
      <li>FDR is always ≤ 0.05 by default – cannot be changed.</li>
    </ul>

    <div style="font-size: 1.2em; margin-top: 1em; font-weight: bold;">⬇️ Tab: Plot download</div>
    <ul>
      <li><strong>Plot Download Venn</strong> – download the Venn diagram as a PNG file.</li>
      <li><strong>Plot Download Heat</strong> – download the heatmap as a PNG file.</li>
    </ul>

    <div style="font-size: 1.2em; margin-top: 1em; font-weight: bold;">📌 Plot behavior and logic</div>
    <ul>
      <li>Venn diagram supports max. 4 comparisons – if more are selected, an "UpSet" plot is shown instead.</li>
      <li>Below the Venn diagram, genes from shared or unique subsets are listed in tables.</li>
      <li>These gene lists can be copied into the Heatmap gene input field.</li>
      <li>Heatmap shows only genes from selected comparisons, filtered by FDR ≤ 0.05 and the typed gene list.</li>
      <li>Gray cells in the heatmap indicate missing values for selected genes.</li>
      <li>Clustering: similar genes and comparisons are grouped using dendrograms (left for genes, top for comparisons).</li>
    </ul>

  </div>
')


