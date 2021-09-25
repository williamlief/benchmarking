library(tidyverse)

compare_plot <- function(comparison, title) {
  
  files <- list.files("output/", comparison, full.names = T)
  names(files) <- gsub("\\.RDS", "", gsub(".*-", "", files))
  
  dat <- map_dfr(files,
                 readRDS,
                 .id = "system") %>% 
    mutate(time = time / 1e9)
  
  ggplot(dat, aes(x = expr, y = time, color = system)) +
    geom_boxplot() +
    theme_minimal() +
    labs(y = "time (seconds)", 
         x = NULL, 
         subtitle = "Results from ten microbenchmark trials, same hardware dual booted",
         title = title) 
  
  ggsave(paste0("figures/", comparison, ".jpg"),
         bg = "white",
         width = 7, height = 7)
}

compare_plot("synth_methods", "Synthetic Control Packages")
  
