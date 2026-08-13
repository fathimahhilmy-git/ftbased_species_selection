library(ggplot2)
library(dplyr)
library(scales)

result_composite <- rank_closed
result_plot <- result_composite %>%
  arrange(desc(relative_abundance)) %>%
  mutate(
    rank = row_number(),
    log_rank = log(rank)
  )

top_n <- 5
label_data <- result_plot %>%
  slice(1:top_n) %>%
  mutate(
    label_rank = rank   # label diganti jadi nomor ranking
  )


ggplot(result_plot, aes(x = log_rank, y = relative_abundance)) +
  geom_point(aes(color = log_rank), size = 3) +
  scale_color_gradient(low = "#E69F00", high = "#009E73") +
  geom_text(
    data = label_data,
    aes(label = label_rank),
    hjust = -0.1,
    size = 3,
    family = "Times New Roman",
    fontface = "bold"
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1)
  ) +
  theme_classic() +
  labs(
    x = "Species rank (log scale)",
    y = "Predicted relative abundance (%)"
  ) +
  theme(
    plot.title = element_text(
      hjust = 0.5,      # center title
      size = 12,
      face = "bold"
    )
  ) +
  
  xlim(min(result_plot$log_rank), max(result_plot$log_rank) + 0.5)
