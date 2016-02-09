R_NOTEBOOK_VERSION <- "1.0"

rnb_read <- function(rmdPath, cachePath = NULL) {
  if (!file.exists(rmdPath))
    stop("No file at path '", rmdPath, "'")

  rmdPath <- normalizePath(rmdPath, winslash = "/", mustWork = TRUE)
  contents <- suppressWarnings(readLines(rmdPath))

  # Resolve the cache directory
  if (is.null(cachePath)) {
    cachePath <- paste(
      tools::file_path_sans_ext(rmdPath),
      ".Rnb.cached",
      sep = ""
    )
  }

  if (!file.exists(cachePath))
    stop("No cache directory at path '", cachePath, "'")
  cachePath <- normalizePath(cachePath, winslash = "/", mustWork = TRUE)

  # Begin collecting the units that form the Rnb data structure
  rnbData <- list()

  # store reference to source path
  rnbData[["source_path"]] <- rmdPath
  rnbData[["cache_path"]]  <- cachePath

  # Keep the original source data
  rnbData[["contents"]] <- contents

  # Read the chunk information
  chunkInfoPath <- file.path(cachePath, "chunks.csv")
  chunkInfo <- read.table(chunkInfoPath, header = TRUE, sep = ",", stringsAsFactors = FALSE)
  rnbData[["chunk_info"]] <- chunkInfo

  # Collect all of the HTML files, alongside their dependencies
  htmlFiles <- list.files(cachePath, pattern = "html$", full.names = TRUE)
  chunkData <- lapply(htmlFiles, function(file) {
    dependenciesDir <- paste(tools::file_path_sans_ext(file), "files", sep = "_")
    dependenciesFiles <- list.files(dependenciesDir, full.names = TRUE, recursive = TRUE)
    list(
      html = read_file(file),
      deps = lapply(dependenciesFiles, read_file)
    )
  })
  names(chunkData) <- tools::file_path_sans_ext(basename(htmlFiles))
  rnbData[["chunk_data"]] <- chunkData

  # Read in the 'libs' directory.
  rnbData[["lib"]] <- list()

  libDir <- file.path(cachePath, "lib")
  if (file.exists(libDir)) {
    owd <- setwd(libDir)
    libFiles <- list.files(libDir, recursive = TRUE)
    libData <- lapply(libFiles, read_file)
    names(libData) <- libFiles
    rnbData[["lib"]] <- libData
    setwd(owd)
  }

  rnbData
}

rnb_mask_chunks <- function(contents, chunk_info) {

  masked <- contents
  for (i in rev(seq_len(nrow(chunk_info)))) {

    start <- chunk_info[["row"]][[i]] - 1
    end   <- start + chunk_info[["row_count"]][[i]] + 1
    id    <- chunk_info[["chunk_id"]][[i]]

    masked <- c(
      masked[1:(start - 1)],
      paste('<!-- rnb-chunk-id', id, '-->'),
      masked[(end + 1):length(masked)]
    )
  }

  masked

}

extract_body <- function(html) {
  begin <- regexpr('<body[^>]*>', html, perl = TRUE)
  end   <- regexpr('</body>', html, perl = TRUE)

  contents <- substring(html, begin + attr(begin, "match.length"), end - 1)
  trim(contents)
}

rnb_fill_chunk <- function(rnbData, chunkId) {
  builder <- string_builder()
  chunkInfo <- rnbData$chunk_info[rnbData$chunk_info$chunk_id == chunkId, ]
  chunkData <- rnbData$chunk_data[[chunkId]]

  # parse the chunk header as we'll need to interleave
  # that information within the output
  pattern <- paste(
    "^\\s*```+{",
    "(\\w+)",
    "\\s*",
    "([^}]*)",
    "}\\s*$",
    sep = ""
  )

  chunkHeader <- rnbData$contents[chunkInfo$row - 1]
  matches <- regex_matches(pattern, chunkHeader)

  chunkClass   <- matches[[1]]
  chunkOptions <- matches[[2]]

  startIdx <- chunkInfo$row
  endIdx   <- chunkInfo$row + chunkInfo$row_count - 1
  visible  <- chunkInfo$visible

  builder$appendf('<div class="%s_chunk" data-chunk-id="%s" data-chunk-options="%s">',
                  chunkClass,
                  chunkId,
                  chunkOptions)
  builder$indent()

  # Input code
  builder$appendf('<code class="%s_input">', chunkClass)
  builder$indent()
  builder$append(rnbData$contents[startIdx:endIdx])
  builder$unindent()
  builder$append('</code>')

  # Output code
  bodyContent <- extract_body(chunkData$html)
  bodyContent <- inject_base64_data(bodyContent, chunkId, rnbData)
  builder$appendf('<div class="%s_output" visible="%s">', chunkClass, visible)
  builder$indent()
  builder$append(bodyContent)
  builder$unindent()
  builder$append('</div>')

  builder$unindent()
  builder$append('</div>')

  builder$data()
}

rnb_fill_chunks <- function(html, rnbData) {
  filled <- html
  for (i in seq_along(filled)) {
    line <- filled[[i]]
    if (starts_with(line, '<!-- rnb-chunk-id') && ends_with(line, '-->')) {
      chunkId <- sub('<!-- rnb-chunk-id\\s*(\\S+)\\s*-->', '\\1', line)
      chunkData <- rnbData$chunk_data[[chunkId]]
      if (is.null(chunkData))
        stopf("no chunk with id '%s'", chunkId)

      filled[[i]] <- paste(rnb_fill_chunk(rnbData, chunkId), collapse = "\n")
    }
  }
  filled
}

rnb_prepare <- function(rnbData) {

  # first, render our .Rmd to transform markdown to html
  contents <- rnbData$contents
  chunk_info <- rnbData$chunk_info

  # mask out chunks (replace with placeholders w/id)
  masked <- rnb_mask_chunks(contents, chunk_info)

  # use pandoc to convert md to html
  input  <- tempfile("rnb-tempfile-input", fileext = ".md")
  output <- tempfile("rnb-tempfile-output", fileext = ".html")
  cat(masked, file = input, sep = "\n")
  pandoc_convert(input = input, output = output)

  # read the HTML
  html <- readLines(output)

  # replace chunk placeholders with their actual data
  html <- rnb_fill_chunks(html, rnbData)

  # extract yaml header
  frontMatter <- partition_yaml_front_matter(contents)$front_matter
  yaml <- parse_yaml_front_matter(frontMatter)

  # begin building our output file
  builder <- string_builder()

  builder$append('<html>')

  # add document comments within an HTML comment
  builder$append(
    '<!-- document-source',
    caTools::base64encode(paste(contents, collapse = "\n")),
    '-->'
  )

  # write header output
  builder$append('<head>')
  builder$indent()
  builder$append(sprintf('<meta name="r-notebook-version" content="%s" />', R_NOTEBOOK_VERSION))
  builder$append(sprintf('<title>%s</title>', yaml$title))
  builder$append('<script type="text/yaml">')
  builder$indent()
  builder$append(frontMatter[2:(length(frontMatter) - 1)])
  builder$unindent()
  builder$append('</script>')
  builder$unindent()
  builder$append('</head>')

  # write body output
  builder$append('<body>')
  builder$append(html)
  builder$append('</body>')

  # close html
  builder$append('</html>')

  unlist(builder$data())

}

inject_base64_data <- function(html, chunkId, rnbData) {

  # we'll be calling pandoc in the cache dir, so make sure
  # we save our original dir
  owd <- getwd()
  on.exit(setwd(owd), add = TRUE)
  setwd(rnbData$cache_path)

  # use pandoc to render the HTML fragment, thereby injecting
  # base64-encoded dependencies
  input  <- "rnb-base64-inject-input.html"
  output <- "rnb-base64-inject-output.html"

  cat(html, file = input, sep = "\n")
  opts <- c("--self-contained")
  pandoc_convert(input, output = output, options = opts)

  result <- read_file(output)
  extract_body(result)
}

extract_rmd <- function(rnbDoc) {
  startIdx <- which(rnbDoc == '<!-- document-source')
  base64doc <- rnbDoc[startIdx + 1]
  strsplit(caTools::base64decode(base64doc, character()), "\n", fixed = TRUE)[[1]]
}
