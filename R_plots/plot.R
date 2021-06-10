# Plots for vaccine equity in Kenya study 

# load libraries
library (data.table)
library (ggplot2)
library (ggpubr)
library (dplyr)

# clear workspace
rm (list = ls ())

# ------------------------------------------------------------------------------
# Plot - Vaccination coverage
# Vaccination coverage in Kenya among children aged 12-23 months by 
# socioeconomic, geographic, maternal, child, and birth setting characteristics.
#   socioeconomic (household wealth, religion, ethnicity)
#   geographic (area of residence, region)
#   maternal (maternal age at birth, maternal education, maternal marital status, sex of household head) 
#   child (sex of child, birth order) 
#   birth setting 
# ------------------------------------------------------------------------------

# read file with plot data
plot_data <- fread ("data_coverage.csv")
plot_list <- vector (mode = "list", length = 4)

# loop through plots for vaccination coverage
for (i in 1:5) {
  
  plot_dat <- switch (i, 
                      # socioeconomic characteristics
                      "1" = plot_data [characteristics == "Household wealth" | 
                                         characteristics == "Religion" |
                                         characteristics == "Ethnicity"],
                      # geographic characteristics
                      "2" = plot_data [characteristics == "Place of residence" | 
                                         characteristics == "Region"],
                      # maternal characteristics
                      "3" = plot_data [characteristics == "Maternal age" | 
                                         characteristics == "Maternal education" | 
                                         characteristics == "Maternal marital status" |
                                         characteristics == "Maternal household head status"], 
                      # child characteristics
                      "4" = plot_data [characteristics == "Sex of child" | 
                                         characteristics == "Birth order"],
                      # birth setting characteristics
                      "5" = plot_data [characteristics == "Place of birth"]
                      )
  
  # plot title
  plot_title <- switch (i, 
                        "1" = "Socioeconomic characteristics", 
                        "2" = "Geographic characteristics", 
                        "3" = "Maternal characteristics", 
                        "4" = "Child characteristics", 
                        "5" = "Place of birth characteristics")
  
  # plot
  plot_list [[i]] <- ggplot (data = plot_dat, 
                             aes (x = reorder (specific_characteristics, desc (specific_order)), 
                                  y = coverage, 
                                  fill = -coverage)) + 
    geom_bar (stat = "identity", width = 0.75, alpha=0.9) + 
    geom_errorbar (aes (ymin = low_95ci, ymax = high_95ci, width = 0.25), 
                   col = "orange") + 
    labs (x = "",
          y = "Basic vaccination coverage (%)", 
          title = plot_title
          ) +
    coord_flip () + 
    facet_grid (characteristics ~ ., scales = "free") +
    theme_bw () + 
    theme (legend.position="none") + 
    theme (plot.title = element_text (size = 30)) + 
    theme (axis.text = element_text (size = 22)) + 
    theme (strip.text.y = element_text (size = 18)) + 
    scale_y_continuous (labels = function (x) paste0 (x, "%")) + 
    theme (axis.title.x = element_blank (), 
           axis.title.y = element_blank ()) 
}

# arrange plot columns and rows
p <- ggarrange (plotlist = plot_list, ncol = 5, nrow = 1)

# # plot title
# p <- annotate_figure (p,
#                       # top = text_grob ("Basic vaccination coverage by socioeconomic, geographic, maternal, and child characteristics",
#                       top = text_grob ("  Full immunisation coverage among children aged 12-23 months in Kenya by
#    socioeconomic, geographic, maternal, child, and place of birth characteristics
# (1-dose BCG, 3-dose DTP-HepB-Hib, 3-dose polio, 1-dose measles, 3-dose PCV)\n",
#                                        color = "black", 
#                                        size = 40))

# print plot
print (p)

# save plot to file
ggsave (filename = "plot_socioeconomic_geographic_maternal_child_birthsetting_coverage.jpg", 
        plot = p, 
        units = "in", width = 40, height = 20, 
        dpi = 600)

ggsave (filename = "plot_socioeconomic_geographic_maternal_child_birthsetting_coverage.eps", 
        plot = p, 
        units = "in", width = 40, height = 20, 
        device = cairo_ps)


# ------------------------------------------------------------------------------
# Plot - Adjusted odds ratios
# Inequities in vaccination coverage in Kenya associated with 
# socioeconomic, geographic, maternal, child, and birth setting characteristics.
# ------------------------------------------------------------------------------

# read file with plot data
plot_data <- fread ("data_aor.csv")
plot_list <- vector (mode = "list", length = 2)

# loop through 11 subplots
for (i in 1:10) {
  
  plot_dat <- switch (i, 
                      "1" = plot_data [characteristics == "Household wealth"], 
                      "2" = plot_data [characteristics == "Place of residence"],
                      "3" = plot_data [characteristics == "Region"],
                      "4" = plot_data [characteristics == "Maternal education"],
                      "5" = plot_data [characteristics == "Birth order"],
                      "6" = plot_data [characteristics == "Place of birth"],
                      "7" = plot_data [characteristics == "Maternal age"],
                      "8" = plot_data [characteristics == "Maternal marital status"],
                      "9" = plot_data [characteristics == "Maternal household head status"], 
                     "10" = plot_data [characteristics == "Sex of child"]
                      )
  
  # plot
  plot_list [[i]] <- ggplot (data = plot_dat, 
                             aes (x = reorder (specific_characteristics, desc (specific_order)), 
                             # aes (x = specific_characteristics,
                                  y = AOR, 
                                  fill = factor (AOR))) + 
    scale_fill_brewer (palette = "Reds") +
    geom_bar (stat = "identity", width = 0.75, alpha=0.9) + 
    geom_errorbar (aes (ymin = low_95ci, ymax = high_95ci, width = 0.25), 
                   col = "black") + 
    coord_flip () + 
    facet_grid (characteristics ~ ., scales = "free") +
    theme_bw () + 
    theme (legend.position="none") + 
    theme (axis.title.x = element_blank (),
           axis.title.y = element_blank ()) +
    theme (strip.text.y = element_text (size = 11))  + 
    theme (text = element_text(size=15))
}

# arrange plot columns and rows
p <- ggarrange (plotlist = plot_list, ncol = 3, nrow = 4)

# # plot title
# p <- annotate_figure (p,
#                    # top = text_grob ("Adjusted odds ratios of basic vaccination coverage in children aged 12-23 months
#                      top = text_grob ("  Adjusted odds ratios of full immunisation coverage among children aged 12-23 months in Kenya
# (1-dose BCG, 3-dose DTP-HepB-Hib, 3-dose polio, 1-dose measles, 3-dose PCV)\n",
#                                       color = "black", 
#                                       size = 20))

# print plot
print (p)

# save plot to file
ggsave (filename = "plot_aor.jpg", 
        plot = p, 
        units = "in", width = 15, height = 12, 
        dpi = 600)

ggsave (filename = "plot_aor.eps", 
        plot = p, 
        units = "in", width = 15, height = 12,
        device = cairo_ps)



