base_theme <- theme(
  legend.position = "bottom",
  legend.box.margin = margin(5,5,5,5),
  legend.spacing = unit(0,"pt"),
  plot.title = element_text(face="bold"),
  plot.caption = element_text(face="italic"),
  text = element_text(size=(15),family="Times"),
  legend.key.height = unit(10,"pt"),
  legend.key.width = unit(10,"pt")
)