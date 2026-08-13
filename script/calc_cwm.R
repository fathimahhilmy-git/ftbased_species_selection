library(dplyr)

data <- read.csv("data/dataftcom_12.csv", stringsAsFactors = FALSE)

trait_columns <- c(
  "ft_la",
  "ft_sla",
  "ft_ldmc",
  "ft_stomata",
  "ft_dpph",
  "ft_dpphrsa",
  "ft_proline",
  "ft_chla",
  "ft_chlb",
  "ft_totalchl",
  "nr_basalarea"
)

data <- data %>%
  mutate(
    across(
      all_of(trait_columns),
      ~ as.numeric(gsub(",", ".", as.character(.x)))
    )
  )

safe_wmean <- function(x, w) {
  idx <- !is.na(x) & !is.na(w)
  
  if (sum(idx) == 0) {
    return(NA_real_)
  }
  
  if (sum(w[idx]) == 0) {
    return(NA_real_)
  }
  
  weighted.mean(x[idx], w[idx])
}

cwm_canopy_class <- data %>%
  filter(
    !is.na(canopy_class),
    canopy_class != ""
  ) %>%
  group_by(canopy_class) %>%
  summarise(
    across(
      starts_with("ft_"),
      ~ safe_wmean(.x, nr_basalarea),
      .names = "{.col}_cwm"
    ),
    .groups = "drop"
  )

write.csv(
  cwm_canopy_class,
  "results/cwm_canopy_12.csv",
  row.names = FALSE
)