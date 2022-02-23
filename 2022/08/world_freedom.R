# install.packages("tidyverse")
# install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load(2022, week = 8)

sa <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador",
        "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela") |>
      toupper()

freedom_df <-
  tuesdata$freedom |>
  dplyr::mutate(country = country |>
                  toupper() |>
                  stringr::str_extract(pattern = "^[^\\(]+") |>
                  stringr::str_trim()) |>
  dplyr::filter(country %in% sa)

countries <-
  freedom_df |>
  dplyr::filter(year == 2020) |>
  dplyr::select(country, year, CL) |>
  dplyr::arrange(CL)

freedom_df$country <- factor(freedom_df$country, levels = countries$country)

freedom_plot <-
  freedom_df |>
  ggplot2::ggplot(
    ggplot2::aes(x = year, y = CL, group = country, colour = country, fill = country)
  ) +
  ggplot2::geom_line(size = 1.5, show.legend = FALSE) +
  ggplot2::geom_point(size = 3) +
  ggplot2::theme_minimal(base_size = 14, base_family = "Ubuntu Mono") +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 panel.grid.minor.y = ggplot2::element_blank(),
                 panel.grid = ggplot2::element_line(colour = "grey80", ),
                 plot.background = ggplot2::element_rect(fill = "#e6e6ea"),
                 legend.title = ggplot2::element_blank(),
                 legend.position = c(0.95, 0.5),
                 legend.background = ggplot2::element_rect(fill = "white")) +
  ggplot2::scale_y_reverse(limits = c(7, 1), breaks = seq(1, 7, by = 1)) +
  ggplot2::scale_x_continuous(limits = c(1995, 2023), breaks = seq(1995, 2020, 5)) +
  ggplot2::scale_color_brewer(palette = "Paired") +
  ggplot2::labs(title = "Civil Freedom in South America",
                subtitle = "Changes in Civil Liberty from 1995 to 2020",
                y = "Civil Liberty Rating from 1 (best) to 7 (worst)",
                caption = "Source: UN and Freedom House · Graphic: @ramongss · #TidyTuesday")

freedom_plot |>
  ggplot2::ggsave(filename = "./plots/world_freedom.png",
                  device = "png",
                  width = 12,
                  height = 6.75,
                  units = "in")