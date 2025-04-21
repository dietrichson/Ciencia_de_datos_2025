library(tidyverse)
library(yaml)

# Function to extract metadata from qmd files
extract_metadata <- function(file_path) {
  # Read the entire file
  lines <- readLines(file_path)

  # Find yaml section between first and second "---"
  yaml_start <- which(lines == "---")[1]
  yaml_end <- which(lines == "---")[2]

  if (length(yaml_start) == 0 || length(yaml_end) == 0) {
    return(NULL)
  }

  # Parse yaml section
  yaml_text <- paste(lines[(yaml_start + 1):(yaml_end - 1)], collapse = "\n")
  metadata <- try(yaml.load(yaml_text), silent = TRUE)

  if (inherits(metadata, "try-error") || is.null(metadata$date)) {
    return(NULL)
  }

  # Extract class number from filename for presentations
  class_num <- NULL
  if (grepl("^\\d+-", basename(file_path))) {
    class_num <- as.numeric(gsub("^(\\d+)-.+$", "\\1", basename(file_path)))
  }

  list(
    title = metadata$title,
    date = as.Date(metadata$date),
    class_num = class_num,
    file_path = file_path
  )
}

# Copy files from sasha to docs
copy_notes_to_docs <- function(note_files) {
  for (file in note_files) {
    # Create target path in docs directory
    target_path <- file.path("docs", basename(file))

    # Copy the file
    file.copy(file, target_path, overwrite = TRUE)

    # If there's an associated files directory, copy it too
    files_dir <- gsub("\\.qmd$", "_files", file)
    if (dir.exists(files_dir)) {
      target_files_dir <- file.path("docs", basename(files_dir))
      if (dir.exists(target_files_dir)) {
        unlink(target_files_dir, recursive = TRUE)
      }
      dir.create(target_files_dir, recursive = TRUE)
      file.copy(
        list.files(files_dir, full.names = TRUE),
        target_files_dir,
        recursive = TRUE
      )
    }
  }
}

# Generate index.md
generate_index <- function() {
  # Find all presentation and note files
  pres_files <- list.files("docs/presentaciones",
    pattern = "^\\d+-.+\\.qmd$",
    full.names = TRUE
  )
  note_files <- list.files("sasha",
    pattern = "\\.qmd$",
    full.names = TRUE
  )

  # Extract metadata from all files
  pres_data <- keep(map(pres_files, extract_metadata), ~ !is.null(.x))
  note_data <- keep(map(note_files, extract_metadata), ~ !is.null(.x))

  # Copy notes to docs directory
  copy_notes_to_docs(map_chr(note_data, "file_path"))

  # Create index content
  index_content <- c(
    "# Seminario de Ciencia de Datos\n",
    "## Presentaciones y Notas\n"
  )

  # Get all unique dates
  all_dates <- unique(c(
    map_dbl(pres_data, ~ as.numeric(.$date)),
    map_dbl(note_data, ~ as.numeric(.$date))
  ))

  # Sort by date
  for (current_date in sort(all_dates)) {
    date_str <- as.Date(current_date, origin = "1970-01-01")

    # Find presentation for this date
    pres <- Filter(function(x) as.numeric(x$date) == current_date, pres_data)

    if (length(pres) > 0) {
      pres <- pres[[1]]
      # Add presentation header and link
      html_file <- gsub("\\.qmd$", ".html", basename(pres$file_path))
      index_content <- c(
        index_content,
        sprintf("\n### Clase %d: %s", pres$class_num, pres$title),
        sprintf("* [Presentación](presentaciones/%s)", html_file)
      )
    }

    # Find notes for this date
    notes <- Filter(function(x) as.numeric(x$date) == current_date, note_data)

    # Add note links
    if (length(notes) > 0) {
      note_links <- map_chr(notes, function(note) {
        note_html <- gsub("\\.qmd$", ".html", basename(note$file_path))
        sprintf("* [Notas](%s)", note_html)
      })
      index_content <- c(index_content, note_links)
    }
  }

  # Write index.md file
  writeLines(index_content, "docs/index.md")
}

# Run the generator
generate_index()
