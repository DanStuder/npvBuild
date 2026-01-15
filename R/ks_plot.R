data <- data.frame(value = c(19,6,5,41,25,22, 12,18,15,14,21),
                   group = factor(c(rep("Gruppe 1", 6), rep("Gruppe 2", 5))))

n1 <- data |>
  filter(group == levels(data$group)[1]) |>
  nrow()
n2 <- data |>
  filter(group == levels(data$group)[2]) |>
  nrow()

data_processed <- data |>
  arrange(value) |>
  mutate(
    # 1. Gruppengrössen berechnen
    # n1 = sum(group == "Gruppe 1"),
    # n2 = sum(group == "Gruppe 2"),

    # 2. Den "Wert" für die aktuelle Zeile berechnen
    # Formel: 1 / (n1 * n2) * Gruppengrösse_des_Wertes
    # Wenn Gruppe 1: 1/(n1*n2) * n1 = 1/n2
    # Wenn Gruppe 2: 1/(n1*n2) * n2 = 1/n1
    step_val = (1 / (n1 * n2)) * if_else(group == "Gruppe 1", n2, n1),

    # 3. Inkremente für f1 und f2 definieren
    # Wenn Gruppe 1, erhöht sich f1, f2 bleibt (Inkrement 0)
    inc_f1 = if_else(group == "Gruppe 1", step_val, 0),
    inc_f2 = if_else(group == "Gruppe 2", step_val, 0),

    # 4. Kumulierte Summe bilden (Dies erfüllt: "sonst Wert aus vorheriger Zeile")
    f1 = cumsum(inc_f1),
    f2 = cumsum(inc_f2),
    D12 = abs(f1 - f2),
    D12plus = f1 - f2,
    D12minus = f2 - f1
  ) |>
  select(-c(step_val, inc_f1, inc_f2)) |>
  tibble::as_tibble() |>
  tibble::add_case(value = 0, group = NULL, f1 = 0, f2 = 0, D12 = 0, D12plus = 0, D12minus = 0, .before = 1)


ggplot(data = data_processed,
       aes(x = f1, y = f2)) +
  geom_line() +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = seq(0,1,1/n1)) +
  scale_y_continuous(breaks = seq(0,1,1/n2))
