# install.packages(tidyverse)
# install.packages(tidytuesdayR)
# install.packages(ggtext)

tuesdata <- tidytuesdayR::tt_load(2022, week = 4)

boardgame_plot <-
  tuesdata$ratings |>
  dplyr::left_join(tuesdata$details, by = "id") |>
  dplyr::filter(maxplayers == 1 & yearpublished >= 1990) |>
  dplyr::group_by(yearpublished) |>
  dplyr::summarise(n = dplyr::n()) |>
  tidyr::complete(yearpublished = seq(1990, 2021)) |>
  ggplot2::ggplot(ggplot2::aes(x = yearpublished, y = n)) +
  ggplot2::geom_segment(ggplot2::aes(x = yearpublished, xend = yearpublished,
                                     yend = -Inf)) +
  ggplot2::geom_point(ggplot2::aes(y = n), size = 10, colour = "#377EB8") +
  ggplot2::geom_text(ggplot2::aes(label = ifelse(n >= 20, n, NA),
                                  family = "Fredoka One"), colour = "white") +
  ggplot2::theme_minimal(base_size = 16, base_family = "Fredoka One") +
  ggplot2::theme(axis.title = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_text(angle = 90, vjust = .5),
                 plot.background = ggplot2::element_rect(fill = "white"),
                 plot.title = ggplot2::element_text(size = 30),
                 plot.subtitle = ggtext::element_markdown(size = 20),
                 plot.caption = ggtext::element_markdown(),
                 text = ggplot2::element_text(colour = "#4D4D4D"),
                 panel.grid.major.x = ggplot2::element_blank(),
                 panel.grid.minor.x = ggplot2::element_blank()) +
  ggplot2::scale_y_continuous(limits = c(0,34), expand = c(0,0)) +
  ggplot2::scale_x_continuous(breaks = seq(1990, 2021)) +
  ggplot2::labs(caption = "Source: <span style='color:#377EB8'>BoardGameGeek</span> · Graphic: <span style='color:#377EB8'>@ramongss</span>",
                title = "THE YEAR OF SOLOGAMES",
                subtitle = "<span style='color:#377EB8'>2021</span> was the year that published most <span style='color:#377EB8'>single-player</span> board games")

boardgame_plot |>
  ggplot2::ggsave(filename = "./plots/boardgame.png",
                  device = "png",
                  width = 12,
                  height = 6.75,
                  units = "in")
