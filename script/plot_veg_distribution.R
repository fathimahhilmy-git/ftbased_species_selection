library(ggplot2)
library(tidyr)


data <- data.frame(
  Class = factor(c("Seedling", "Sapling", "Poles", "Trees", "Large trees"),
                 levels = c("Seedling", "Sapling", "Poles", "Trees", "Large trees")),
  Closed = c(1637, 3679, 267, 180, 10),
  Open   = c(472, 1070, 66, 60, 0)
)


data_long <- pivot_longer(
  data,
  cols = c(Closed, Open),
  names_to = "Condition",
  values_to = "Density"
)

# Plot
ggplot(data_long,
       aes(x = Class,
           y = Density,
           fill = Condition)) +
  
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  
  geom_text(
    aes(label = Density),
    position = position_dodge(width = 0.8),
    vjust = -0.4,
    size = 3
  ) +
  
  scale_fill_manual(
    values = c(
      "Closed" = "#009E73",
      "Open"   = "#E69F00"
    )
  ) +
  
  labs(
    x = NULL,
    y = "Number of Individuals/ha",
    fill = NULL
  ) +
  
  theme_classic(base_size = 10) +
  
  theme(
    axis.text = element_text(colour = "black"),
    legend.position = c(0.88, 0.88),   # kanan atas dalam area plot
    legend.justification = c("right", "top"),
    legend.background = element_blank()
  ) +
  
  expand_limits(y = max(data_long$Density) * 1.15)

