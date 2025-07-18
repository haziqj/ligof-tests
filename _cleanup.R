# Cleanup script for Quarto
files_to_remove <- c(
  list.files(pattern = "\\.(bst|spl|cls|ipynb|sty)$")
  # list.files("site_libs", full.names = TRUE, recursive = TRUE)
)

# Iterate over files and delete them if they exist
for (file in files_to_remove) {
  if (file.exists(file)) {
    file.remove(file)
    cat("Deleted:", file, "\n")
  } else {
    cat("File not found:", file, "\n")
  }
}

# Code to delete entire folder site_libs
fol_to_del <- c("site_libs", list.files(pattern = "_files"))
for (i in fol_to_del) {
  if (dir.exists(i)) {
    unlink(i, recursive = TRUE)
    cat("Deleted:", i, "\n")
  } else {
    cat("Folder not found:", i, "\n")
  }
}

# Fix the alignment of tfoot in the Quarto-generated HTML

# Path to the HTML file
html_file <- "docs/index.html"

# Check if the file exists
if (file.exists(html_file)) {
  # Read the file content
  html_content <- readLines(html_file)
  
  # Locate the line with <tfoot class="gt_footnotes">
  html_content <- gsub(
    pattern = '<tfoot class="gt_footnotes">',
    replacement = '<tfoot class="gt_footnotes" style="text-align: left;">',
    x = html_content
  )
  
  # Write the updated content back to the file
  writeLines(html_content, html_file)
  cat("Updated text alignment in:", html_file, "\n")
} else {
  cat("File not found:", html_file, "\n")
}