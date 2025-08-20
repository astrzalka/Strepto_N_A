#### GENOME PLOT ####

genome_plot_create <- function(plot_data_genome, lower, higher){
  ggplot(plot_data_genome, aes(
    xmin = start,
    xmax = end,
    y = "genes",
    label = gene,
    fill = strand,
    forward = strand_plot
  )) +
    geom_gene_arrow(arrowhead_height = grid::unit(10, "mm"),
                    arrow_body_height = grid::unit(8, "mm")) +
    geom_gene_label(grow = TRUE, height = grid::unit(5, "mm")) +
    scale_fill_brewer(palette = "Set3") +
    coord_cartesian(xlim = c(lower, higher), expand = FALSE) + 
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_discrete(expand = c(0, 0)) +  
    theme_classic() +
    theme(
      axis.title.y = element_blank(),       
      axis.text.y = element_blank(),        
      axis.ticks.y = element_blank(),       
      axis.line.y = element_blank(),
      axis.line.x = element_blank(),
      axis.title = element_text(size = 16),        
      axis.text = element_text(size = 14),
      legend.position = "none",    
      plot.title = element_text(size = 18, face = "bold"),  
      strip.text = element_text(size = 14)
    )
}



#### RNA PLOT ####

rna_plot_create <- function(plot_data_rna, lower, higher){
  plot_data_rna %>% ggplot(aes(xmin = start, xmax = end, y = add_variable, label = gene, fill = logFC, forward = strand_plot)) +
    geom_gene_arrow(arrowhead_height = grid::unit(10, "mm"), arrow_body_height = grid::unit(8, "mm")) +
    facet_wrap(~data_name, scales = 'free', ncol = 1, strip.position = "right") +
    facet_grid(data_name~., scales = 'free', space = 'free_y')+
    geom_gene_label(grow = TRUE, height = grid::unit(5, "mm")) +
    scale_fill_distiller(palette = 'RdBu', direction = 1, limits = c(-3, 3), oob = scales::squish)+
    coord_cartesian(xlim = c(lower, higher)) +
    scale_x_continuous(expand = c(0,0))+
    theme_classic() +
    theme(
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      axis.line.x = element_blank(),
      axis.title = element_text(size = 16),        
      axis.text = element_text(size = 14),        
      plot.title = element_text(size = 18, face = "bold"), 
      strip.text = element_text(size = 14)#,
    )
}


#### CHIPSEQ PLOT ####

chip_plot_create <- function(data_chip, lower, higher){
  
  ggplot(data_chip, aes(y = name, x = chromStart)) +
    geom_segment(aes(xend = chromEnd, yend = name, color = name),
                 size = 8, alpha = 0.6) +
    scale_color_brewer(palette = "Set2") +
    coord_cartesian(xlim = c(lower, higher)) +
    scale_x_continuous(expand = c(0,0))+
    theme_classic() +
    theme(
      legend.position = "none",
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      axis.line.x = element_blank(),
      axis.title = element_text(size = 16),      
      axis.text = element_text(size = 14),         
      plot.title = element_text(size = 18, face = "bold"),  
      strip.text = element_text(size = 14)#,
    )
  
}


#### VENN PLOT ####

venn_plot_create <- function(gene_lists, data_set_venn){

  
  if(length(gene_lists) <= 1){
    return(NULL)
  } else if(length(gene_lists) <= 4){
    ggvenn(
      gene_lists,
      fill_color = c("#0073C2FF", "#EFC000FF", 'red3', 'green3'),
      stroke_size = 0.5,
      set_name_size = 4
    )
  } else{
    
    data_set_venn %>%
      group_by(gene) %>%
      summarize(add_variable = list(add_variable)) %>%
      ggplot(aes(x=add_variable)) +
      geom_bar() +
      scale_x_upset(n_intersections = 20)
  }}


#### HEATMAP CREATE ####

heat_plot_create <- function(heat_data){
  tidyHeatmap::heatmap(.data = dplyr::tibble(heat_data),
                       .row = gene,
                       .column = add_variable,
                       .value = logFC,
                       palette_value = circlize::colorRamp2(
                         seq(-5, 5, length.out = 11),
                         RColorBrewer::brewer.pal(11, "RdBu"))) -> p_heat
  print(p_heat)
}

#### TSS PLOT CREATE ####

tss_plot_create <- function(tss_data_plot, lower, higher){
  tss_data_plot %>% ggplot(aes(x = Start_tss))+
    geom_segment(data = tss_data_plot %>% filter(Strand == '-'), 
                 aes(xend = Start_tss, y = 0, yend = -1), color = 'aquamarine4', size = 1)+
    geom_segment(data = tss_data_plot %>% filter(Strand == '+'), 
                 aes(xend = Start_tss, y = 0, yend = 1), color = 'sienna3', size = 1)+
    theme_minimal()+
    geom_hline(yintercept = 0)+
    theme(axis.title = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank())+
    coord_cartesian(xlim = c(lower, higher), expand = FALSE) 
    
  
}
