helppage <- nav_panel(
  "There are some requirements that your files must meet to be analyzed properly. Examine the example dataset below, which shows the results of an experiment that examines how the use of different primers and dilution impacts what species are identified in a mock community.",
  card(height=600,
       verbatimTextOutput('psog'),
       verbatimTextOutput("pstext")),
  card(
    card_header("Example metadata file"),
    "The metadata file should include any relevant information connecting the sequencing samples to the samples you collected. Examples include where they were collected and date of collection.",
    tooltip(span(img(src="Example_metadata.png", height="75%", width="75%", align = "center")),
            "You should have one column that connects the sequencing sample ID to the IDs in the OTU table (labeled 'Barcode' here). This should have one unique value per row (no repeated IDs) and may or may not be the same as your sample ID. In our example metadata, we have a 'Barcode' column with unique values that match the column names in our OTU table.",
            placement="auto"),
  ),
  card(
    card_header("Example OTU table"),
    "This is a numeric table that connects each sequence to each sample and shows which sequences can be found across multiple samples.The OTU table should be set up so each sample is a column, and each row is a different OTU sequence.",
    tooltip(span(img(src="Example_otu.png", height="75%", width="75%", align = "center")),
            "Notice how in our example, the column names in the OTU table are the same as the 'Barcode' column in the metadata table. We can then see which ASVs are found in which barcode.",
            placement="auto")
  ),
  card(
    card_header("Example taxonomy table"),
    "Each row of the taxonomy table should correspond to a different sequence, and taxonomic ranks should be in columns. The first row of the OTU and taxonomy tables should have the same sequence names. These will be used to connect the taxonomic identity of a sequence to your samples.",
    tooltip(span(img(src="Example_taxa.png", height="75%", width="75%", align = "center")),
            "Notice how in our example, the first row of the OTU and taxonomy tables are the same.",
            placement="auto")
  )
)