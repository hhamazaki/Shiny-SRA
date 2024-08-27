# ADFG GGplot theme to match ADFG publications requirements for plots



theme_adfg_bbc = function ()
  ### BBC STYLE only changed font to serif
{
  font <- "serif"
  ggplot2::theme(
    text = element_text(family = font),
    plot.title = ggplot2::element_text(
      family = font,
      size = 28,
      face = "bold",
      color = "#222222"
    ),
    plot.subtitle = ggplot2::element_text(
      family = font,
      size = 22,
      margin = ggplot2::margin(9, 0, 9, 0)
    ),
    plot.caption = ggplot2::element_blank(),
    legend.position = "top",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(
      family = font,
      size = 12,
      color = "#222222"
    ),
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(
      family = font,
      size = 12,
      color = "#222222"
    ),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5,
                                                                 b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(fill = "white"),
    strip.text = ggplot2::element_text(size = 12, hjust = 0)
  )
}





# Theme cowplot adjusted --------------------------------------------------

theme_adfg = function (font_size = 14,
                       font_family = "serif",
                       line_size = 0.5,
                       rel_small = 12 / 14,
                       rel_tiny = 11 / 14,
                       rel_large = 16 / 14)
{
  ### cowplot STYLE only changed font to serif and legend position
  half_line <- font_size / 2
  small_size <- rel_small * font_size
  theme_classic(base_size = font_size, base_family = font_family) %+replace%
    theme(
      line = element_line(
        color = "black",
        linewidth = line_size,
        linetype = 1,
        lineend = "butt"
      ),
      rect = element_rect(
        fill = NA,
        color = NA,
        size = line_size,
        linetype = 1
      ),
      text = element_text(
        family = font_family,
        face = "plain",
        color = "black",
        size = font_size,
        hjust = 0.5,
        vjust = 0.5,
        angle = 0,
        lineheight = 0.9,
        margin = margin(),
        debug = FALSE
      ),
      axis.line = element_line(
        color = "black",
        size = line_size,
        lineend = "square"
      ),
      axis.line.x = NULL,
      axis.line.y = NULL,
      axis.text = element_text(color = "black",
                               size = small_size),
      axis.text.x = element_text(margin = margin(t = small_size / 4),
                                 vjust = 1),
      axis.text.x.top = element_text(margin = margin(b = small_size / 4),
                                     vjust = 0),
      axis.text.y = element_text(margin = margin(r = small_size / 4),
                                 hjust = 1),
      axis.text.y.right = element_text(margin = margin(l = small_size / 4),
                                       hjust = 0),
      axis.ticks = element_line(color = "black",
                                size = line_size),
      axis.ticks.length = unit(half_line / 2,
                               "pt"),
      axis.title.x = element_text(margin = margin(t = half_line / 2),
                                  vjust = -0.75),
      axis.title.x.top = element_text(margin = margin(b = half_line / 2),
                                      vjust = -0.75),
      axis.title.y = element_text(
        angle = 90,
        margin = margin(r = half_line /
                          2),
        vjust = 3
      ),
      axis.title.y.right = element_text(
        angle = -90,
        margin = margin(l = half_line / 2),
        vjust = 0
      ),
      legend.background = element_blank(),
      legend.spacing = unit(font_size, "pt"),
      legend.spacing.x = NULL,
      legend.spacing.y = NULL,
      legend.margin = margin(0,
                             0, 0, 0),
      legend.key = element_blank(),
      legend.key.size = unit(1.1 *
                               font_size, "pt"),
      legend.key.height = NULL,
      legend.key.width = NULL,
      legend.text = element_text(size = rel(rel_small)),
      legend.text.align = NULL,
      legend.title = element_text(hjust = 0),
      legend.title.align = NULL,
      legend.position = "bottom",
      legend.direction = NULL,
      legend.justification = c("left",
                               "center"),
      legend.box = NULL,
      legend.box.margin = margin(0,
                                 0, 0, 0),
      legend.box.background = element_blank(),
      legend.box.spacing = unit(font_size, "pt"),
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      panel.grid.major = NULL,
      panel.grid.minor = NULL,
      panel.grid.major.x = NULL,
      panel.grid.major.y = NULL,
      panel.grid.minor.x = NULL,
      panel.grid.minor.y = NULL,
      panel.spacing = unit(half_line, "pt"),
      panel.spacing.x = NULL,
      panel.spacing.y = NULL,
      panel.ontop = FALSE,
      strip.background = element_rect(fill = "grey80"),
      strip.text = element_text(
        size = rel(rel_small),
        margin = margin(half_line / 2, half_line /
                          2, half_line / 2,
                        half_line / 2)
      ),
      strip.text.x = NULL,
      strip.text.y = element_text(angle = -90),
      strip.placement = "inside",
      strip.placement.x = NULL,
      strip.placement.y = NULL,
      strip.switch.pad.grid = unit(half_line / 2,
                                   "pt"),
      strip.switch.pad.wrap = unit(half_line / 2,
                                   "pt"),
      plot.background = element_blank(),
      plot.title = element_text(
        face = "bold",
        size = rel(rel_large),
        hjust = 0,
        vjust = 1,
        margin = margin(b = half_line)
      ),
      plot.subtitle = element_text(
        size = rel(rel_small),
        hjust = 0,
        vjust = 1,
        margin = margin(b = half_line)
      ),
      plot.caption = element_text(
        size = rel(rel_tiny),
        hjust = 1,
        vjust = 1,
        margin = margin(t = half_line)
      ),
      plot.tag = element_text(
        face = "bold",
        hjust = 0,
        vjust = 0.7
      ),
      plot.tag.position = c(0, 1),
      plot.margin = margin(half_line,
                           half_line, half_line, half_line),
      complete = TRUE
    )
}


# test data ---------------------------------------------------------------

# line_df <- gapminder%>%
#   filter(country %in% head(unique(gapminder$country)))
# 
# ggplot(line_df, aes(x = year, y = lifeExp)) +
#   geom_line(colour = "#1380A1", linewidth = 1) +
#   geom_hline(yintercept = 0, linewidth = 1, colour="#333333") +
#   # theme(text = element_text(family = "Times New Roman")) +
#   theme_adfg()+
#   facet_wrap(.~country)+
#   labs(title="Living longer",
#        subtitle = "Life expectancy in Malawi 1952-2007")

# Viridis alternating colors function -------------------------------------


# Function to create a custom Viridis palette with alternating light and dark colors
create_alternating_viridis_palette <- function(n_colors, starting_color = "#38598CFF") {
  # Generate a Viridis palette with the desired number of colors
  full_palette <- viridis(n_colors)
  
  # Find the index of the starting color in the Viridis palette
  start_index <- which(full_palette == starting_color)
  
  # If the starting color is not found, use the first color as default
  if (length(start_index) == 0) {
    start_index <- 1
  }
  
  # Determine the number of light and dark colors needed
  n_light <- ceiling(n_colors / 2)
  n_dark <- n_colors - n_light
  
  # Select light and dark colors from the palette, starting from the specified color
  light_colors <- full_palette[seq(start_index, length(full_palette), by = 2)][1:n_light]
  dark_colors <- full_palette[seq(start_index + 1, length(full_palette), by = 2)][1:n_dark]
  
  # Combine light and dark colors into the final palette
  alternating_palette <- c(light_colors, dark_colors)
  
  return(alternating_palette)
}

# # Example: Create a custom Viridis palette with 5 colors (alternating light and dark)
# my_colors <- create_alternating_viridis_palette(5)
# 
# # Output the palette
# my_colors


