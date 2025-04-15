light <- bs_theme(#bootswatch = "morph",
  bootswatch = "lumen",
  #bootswatch = "flatly",
  #base_font = font_google("Merriweather"),
  heading_font = font_google("Lora"),
  base_font = font_google("Inter"),
  #heading_font = font_google("Archivo Black"),
) 
dark <- bs_theme(bootswatch="solar",
                 heading_font = font_google("Lora"),
                 base_font = font_collection(font_google("Inter"),"Montserrat"),
                 #bg="#CDDEF5"
)

coldf <- RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$colorblind==TRUE,]
colnames <- rownames(coldf)

SelectColorsCard <- tagList(
  shinyWidgets::radioGroupButtons("gblcol",
                                  label="Plotting color schemes",
                                  c("default",colnames),
                                  selected="default",
                                  direction="vertical",
                                  justified = TRUE,
  )
)