
library(rtracklayer)
library(dplyr)

read_tss_wig <- function(file, type) {
  # Read the WIG file
  wig_10_for <- import(file, format = "wig")
  
  # Check the result
  wig_10_for
  
  wig_10_data <- data.frame(score = wig_10_for$score, start = 1:8222198)
  
  wig_10_data %>% filter(score > 100) -> wig_10_data_filtered
  
  tss_i <- 1
  wig_10_data_filtered$tss <- NA
  
  for(i in 1: nrow(wig_10_data_filtered)){
    
    if(i == 1){
      wig_10_data_filtered$tss[i] <- paste('TSS_', type, '_', tss_i, sep = '')
    } else if(wig_10_data_filtered$start[i] == wig_10_data_filtered$start[i-1]+1){
      wig_10_data_filtered$tss[i] <- paste('TSS_', type, '_', tss_i, sep = '')
    } else {
      tss_i <- tss_i + 1
      wig_10_data_filtered$tss[i] <- paste('TSS_', type, '_', tss_i, sep = '')
    }
    
  }
  
  wig_10_data_filtered %>% group_by(tss) %>%
    summarise(start = mean(start), score = mean(score)) %>%
    arrange(start) -> wig_10_for_data_summ
  
  return(wig_10_for_data_summ)
}


wig_10_for <- read_tss_wig(file = 'datasets/TSS/sven/10h_for_diff.wig', type = 'for_10')
wig_10_rev <- read_tss_wig(file = 'datasets/TSS/sven/10h_rev_diff.wig', type = 'rev_10')
wig_14_for <- read_tss_wig(file = 'datasets/TSS/sven/14h_for_diff.wig', type = 'for_14')
wig_14_rev <- read_tss_wig(file = 'datasets/TSS/sven/14h_rev_diff.wig', type = 'rev_14')
wig_18_for <- read_tss_wig(file = 'datasets/TSS/sven/18h_for_diff.wig', type = 'for_18')
wig_18_rev <- read_tss_wig(file = 'datasets/TSS/sven/18h_rev_diff.wig', type = 'rev_18')
wig_24_for <- read_tss_wig(file = 'datasets/TSS/sven/24h_for_diff.wig', type = 'for_24')
wig_24_rev <- read_tss_wig(file = 'datasets/TSS/sven/24h_rev_diff.wig', type = 'rev_24')

tss_data_sven <- rbind(wig_10_for, wig_10_rev, wig_14_for, wig_14_rev, wig_18_for, wig_18_rev, wig_24_for, wig_24_rev)

tss_data_sven %>% select(TSS = tss, Start_tss = start) %>%
  mutate(Strand = ifelse(grepl('for', TSS), '+', '-')) -> tss_data_sven

write.table(tss_data_sven, 'datasets/TSS/tss_venezuelae.txt')
