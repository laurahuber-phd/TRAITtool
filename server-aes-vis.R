library(ggplot2)

globalcolorpick <- reactive({
  if(is.null(input$gblcol)) {"D"} else {input$gblcol}
}) %>% bindEvent(input$gblcol,ignoreNULL=FALSE)

scale_color_continuous <- function(...) {viridis::scale_color_viridis(option=globalcolorpick())}
scale_fill_continuous <- function(...) {viridis::scale_fill_viridis(option=globalcolorpick())}
scale_color_discrete <- function(...) {viridis::scale_colour_viridis(option=globalcolorpick(),discrete=TRUE)}
scale_fill_discrete <- function(...) {viridis::scale_fill_viridis(option=globalcolorpick(),discrete=TRUE)}

options(
  ggplot2.continuous.colour = scale_color_continuous,
  ggplot2.continuous.fill = scale_fill_continuous,
  ggplot2.discrete.colour = scale_color_discrete,
  ggplot2.discrete.fill = scale_fill_discrete)

ggplot2::theme_set(theme_bw() + theme(
  legend.position = "bottom",
  legend.box.margin = margin(5,5,5,5),
  legend.spacing = unit(0,"pt"),
  plot.title = element_text(face="bold"),
  plot.caption = element_text(face="italic"),
  text = element_text(size=(15),family="Times"),
  legend.key.height = unit(10,"pt"),
  legend.key.width = unit(10,"pt")
))

