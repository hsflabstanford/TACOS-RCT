# save as publish_to_gdoc.R
library(rmarkdown)
library(googledrive)

# Set up authentication via:
drive_auth()

# Knit Rmd to Word
rmd_file <- "TACOS-pre-registered-hypotheses.Rmd"
docx_output <- ".TACOS-pre-registered-hypotheses.docx"
render(rmd_file, output_format = "word_document", output_file = docx_output)

# Upload to Google Drive (will convert to Google Doc format automatically)
uploaded_file <- drive_upload(
  media = docx_output,
  name = "TACOS-intermediate-file", 
  type = "document", # Ensures conversion to Google Doc
  overwrite = TRUE   # Replace if exists with same name
)

# Print the URL to the uploaded document
cat("Document uploaded: ", as.character(uploaded_file$web_viewlink), "\n")

# Optional: Remove the temporary docx file
# file.remove(docx_output)
