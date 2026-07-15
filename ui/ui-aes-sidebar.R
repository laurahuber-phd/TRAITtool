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

navigation <- tagList(
  HTML("<h5>Navigate to page</h5>"),
  actionButton("go_home", "Home"),
  actionButton("go_upload", "Upload"),
  actionButton("go_review", "Review"),
  actionButton("go_filter", "Filter"),
  actionButton("go_trait", "Trait Assignment"),
  actionButton("go_ta","Trait Analysis"),
  #actionButton("go_tempa","Temperature Analysis"),
  #actionButton("go_stat", "Analysis Figures"),
  actionButton("go_desc", "Detailed Analysis"),
  actionButton("go_help", "Help Page"),
  actionButton("go_settings", "Settings"),
  actionButton("go_rm", "ReadMe Page"),
  actionButton("go_report", "Report Page")
)