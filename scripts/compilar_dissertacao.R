local({
  args <- commandArgs(trailingOnly = TRUE)

  project_root <- if (length(args) >= 1) {
    normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
  } else {
    normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  }

  repo_url <- if (length(args) >= 2) {
    args[[2]]
  } else {
    "https://ftp.math.utah.edu/pub/tex/historic/systems/texlive/2025/tlnet-final"
  }

  helper_file <- file.path(project_root, "scripts", "funcoes_dissertacao.R")
  sys.source(helper_file, envir = environment())

  pdf_path <- compilar_dissertacao(
    abrir_pdf = FALSE,
    project_root = project_root,
    repo_url = repo_url
  )

  cat(pdf_path, "\n")
})
