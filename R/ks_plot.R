data <- data.frame(
  value = c(19, 6, 5, 41, 25, 22, 12, 18, 15, 14, 21),
  group = factor(c(rep("Gruppe 1", 6), rep("Gruppe 2", 5)))
)

data <- data.frame(
  value = c(29, 24, 38, 34, 15, 41, 42),
  group = factor(c(rep("Gruppe 1", 4), rep("Gruppe 2", 3)))
)


ks.plot <- function(
  data,
  alternative = c("less", "two.sided", "greater"),
  plot_parallels = c("all", "top", "bottom", "none"),
  D12 = T,
  D12plus = F,
  D12minus = F
) {
  n1 <- data |>
    dplyr::filter(group == levels(data$group)[1]) |>
    nrow()

  n2 <- data |>
    dplyr::filter(group == levels(data$group)[2]) |>
    nrow()

  data_processed <- data |>
    dplyr::arrange(value) |>
    dplyr::mutate(
      # Den "Wert" für die aktuelle Zeile berechnen
      # Formel: 1 / (n1 * n2) * Gruppengrösse_des_Wertes
      # Wenn Gruppe 1: 1/(n1*n2) * n1 = 1/n2
      # Wenn Gruppe 2: 1/(n1*n2) * n2 = 1/n1
      step_val = (1 / (n1 * n2)) * dplyr::if_else(group == "Gruppe 1", n2, n1),

      # Inkremente für f1 und f2 definieren
      # Wenn Gruppe 1, erhöht sich f1, f2 bleibt (Inkrement 0)
      inc_f1 = dplyr::if_else(group == "Gruppe 1", step_val, 0),
      inc_f2 = dplyr::if_else(group == "Gruppe 2", step_val, 0),

      # Kumulierte Summe bilden (Dies erfüllt: "sonst Wert aus vorheriger Zeile")
      f1 = cumsum(inc_f1),
      f2 = cumsum(inc_f2),
      D12 = abs(f1 - f2),
      D12plus = f1 - f2,
      D12minus = f2 - f1,
      label_num = dplyr::if_else(value == 0, NA_real_, value)
    ) |>
    dplyr::select(-c(step_val, inc_f1, inc_f2)) |>
    tibble::as_tibble() |>
    tibble::add_case(
      value = 0,
      group = NULL,
      f1 = 0,
      f2 = 0,
      D12 = 0,
      D12plus = 0,
      D12minus = 0,
      .before = 1
    )

  distance_col <- switch(
    alternative,
    "two.sided" = "D12",
    "greater" = "D12plus",
    "less" = "D12minus"
  )

  row_d12 <- data_processed |>
    dplyr::slice_max(order_by = .data[["D12"]])

  row_d12plus <- data_processed |>
    dplyr::slice_max(order_by = .data[["D12plus"]])

  row_d12minus <- data_processed |>
    dplyr::slice_max(order_by = .data[["D12minus"]])

  #### Parallele oben
  parallel_top <- ggplot2::geom_segment(
    data = distance,
    ggplot2::aes(
      x = 0,
      xend = 1 - .data[[distance_col]],
      y = .data[[distance_col]],
      yend = 1
    ),
    colour = "slategray"
  )

  #### Parallele unten
  parallel_bottom <- ggplot2::geom_segment(
    data = distance,
    ggplot2::aes(
      x = .data[[distance_col]],
      xend = 1,
      y = 0,
      yend = 1 - .data[[distance_col]]
    ),
    colour = "slategray"
  )

  #### D12
  segment_d12 <- ggplot2::geom_segment(
    data = row_d12,
    ggplot2::aes(
      x = f1,
      y = f1,
      xend = f1,
      yend = f2
    ),
    colour = "red",
    arrow = ggplot2::arrow(
      angle = 20,
      length = ggplot2::unit(0.30, "cm"),
      ends = "both",
      type = "closed"
    )
  )

  #### D12+
  segment_d12plus <- ggplot2::geom_segment(
    data = row_d12plus,
    ggplot2::aes(
      x = f1,
      y = f1,
      xend = f1,
      yend = f2
    ),
    colour = "purple",
    arrow = ggplot2::arrow(
      angle = 20,
      length = ggplot2::unit(0.30, "cm"),
      ends = "both",
      type = "closed"
    )
  )

  #### D12-
  segment_d12minus <- ggplot2::geom_segment(
    data = row_d12minus,
    ggplot2::aes(
      x = f1,
      y = f1,
      xend = f1,
      yend = f2
    ),
    colour = "brown",
    arrow = ggplot2::arrow(
      angle = 20,
      length = ggplot2::unit(0.30, "cm"),
      ends = "both",
      type = "closed"
    )
  )

  # Basisplot erstellen
  plot <- ggplot2::ggplot(
    data = data_processed,
    ggplot2::aes(x = f1, y = f2)
  ) +
    ggplot2::geom_line(color = "steelblue") +
    ggplot2::geom_point(color = "steelblue") +
    ggplot2::geom_text(
      ggplot2::aes(label = label_num),
      nudge_x = 0.03,
      nudge_y = 0.03,
      na.rm = T
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 1, 1 / n1),
      labels = scales::label_number(accuracy = 0.01)
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, 1, 1 / n2),
      labels = scales::label_number(accuracy = 0.01)
    ) +

    # Mittellinie
    ggplot2::annotate(
      "segment",
      x = 0,
      xend = 1,
      y = 0,
      yend = 1,
      color = "black",
      linetype = "dashed"
    ) +

    ggplot2::labs(
      x = expression(italic(F) * "(1)"),
      y = expression(italic(F) * "(2)")
    )

  # Konditional Elemente hinzufügen
  if (alternative == "two.sided" & plot_parallels == "all") {
    plot <- plot + parallel_top + parallel_bottom
  } else if (alternative == "two.sided" & plot_parallels == "top") {
    plot <- plot + parallel_top
  } else if (alternative == "two.sided" & plot_parallels == "bottom") {
    plot <- plot + parallel_bottom
  } else if (
    alternative == "greater" &
      (plot_parallels == "bottom" | plot_parallels == "all")
  ) {
    plot <- plot + parallel_bottom
  } else if (
    alternative == "less" & (plot_parallels == "top" | plot_parallels == "all")
  ) {
    plot <- plot + parallel_top
  }

  if (D12) {
    plot <- plot + segment_d12
  }

  if (D12plus) {
    plot <- plot + segment_d12plus
  }

  if (D12minus) {
    plot <- plot + segment_d12minus
  }

  return(plot)
}

ks.plot(data, alternative = "greater", plot_parallels = "all", F, T, T)
