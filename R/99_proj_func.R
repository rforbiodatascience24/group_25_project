

perform_PCA <- function(data_set, type_experiment, gene_list){

  pc_df <- data_set |>
    filter(str_starts(experiment, type_experiment)) |>
    select(experiment, group, gene_list, replicate)

  pc_fit <- pc_df |>
    select(-experiment, -group, -replicate) |>
    select(where(~ var(.) > 0)) |>  # Exclude columns with zero variance
    scale() |>  # Scale the data
    prcomp()  # Perform PCA


  plt <- pc_fit |>
    augment(pc_df) |>
    ggplot(aes(.fittedPC1, .fittedPC2, color = group, shape = replicate)) +
    geom_point(size = 3, alpha = 0.8) +  # Plot points
    labs(
      title = paste("PCA Plot for", type_experiment, "experiment"),
      x = "Principal Component 1",
      y = "Principal Component 2",
      color = "Group",
      shape = "Replicate"
    ) +
    theme_minimal(base_size = 14) +  # Clean theme
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 14),
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 12)  # Turn off all grid lines
    ) +
    scale_color_manual(
      values = c("control" = "blue", "tau" = "red")
    )


  return(plt)


}

perform_PCA_on_all_conditions <- function(data_set, gene_list){

  pc_df <- data_set |>
    select(experiment, group, gene_list, replicate)

  pc_fit <- pc_df |>
    select(-experiment, -group, -replicate) |>
    select(where(~ var(.) > 0)) |>  # Exclude columns with zero variance
    scale() |>  # Scale the data
    prcomp()  # Perform PCA


  plt <- pc_fit |>
    augment(pc_df) |>
    ggplot(aes(.fittedPC1, .fittedPC2, color = group, shape = replicate)) +
    geom_point(size = 3, alpha = 0.5) +  # Plot points
    labs(
      title = paste("PCA Plot for all experiments"),
      x = "Principal Component 1",
      y = "Principal Component 2",
      color = "Group",
      shape = "Replicate"
    ) +
    theme_minimal(base_size = 14) +  # Clean theme
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 14),
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 12)  # Turn off all grid lines
    ) +
    scale_color_manual(
      values = c("control" = "blue", "tau" = "red")
    )


  return(plt)


}
