.dissertacao_repo_tinytex <- function() {
  "https://ftp.math.utah.edu/pub/tex/historic/systems/texlive/2025/tlnet-final"
}

.dissertacao_localizar_raiz <- function(start = getwd()) {
  atual <- normalizePath(start, winslash = "/", mustWork = TRUE)

  repeat {
    tem_rproj <- file.exists(file.path(atual, "dissertacao.Rproj"))
    tem_latex <- file.exists(file.path(atual, "trabalho_LaTeX", "main.tex"))

    if (tem_rproj || tem_latex) {
      return(atual)
    }

    pai <- dirname(atual)
    if (identical(pai, atual)) {
      stop(
        "Não consegui localizar a raiz do projeto da dissertação.",
        call. = FALSE
      )
    }

    atual <- pai
  }
}

.dissertacao_pdf_path <- function(project_root = .dissertacao_localizar_raiz()) {
  normalizePath(
    file.path(project_root, "trabalho_LaTeX", "main.pdf"),
    winslash = "/",
    mustWork = TRUE
  )
}

compilar_dissertacao <- function(
  abrir_pdf = FALSE,
  project_root = .dissertacao_localizar_raiz(),
  repo_url = .dissertacao_repo_tinytex()
) {
  if (!requireNamespace("tinytex", quietly = TRUE)) {
    stop(
      "O pacote R 'tinytex' não está instalado nesta versão do R.",
      call. = FALSE
    )
  }

  latex_dir <- file.path(project_root, "trabalho_LaTeX")
  if (!dir.exists(latex_dir)) {
    stop(sprintf("Pasta da dissertação não encontrada: %s", latex_dir), call. = FALSE)
  }

  original_wd <- getwd()
  on.exit(setwd(original_wd), add = TRUE)

  setwd(latex_dir)

  # Mantém a compilação alinhada com a release já instalada do TinyTeX.
  tinytex::tlmgr_repo(repo_url)

  pdf_file <- tinytex::latexmk(
    "main.tex",
    engine = "pdflatex",
    bib_engine = "biber",
    emulation = TRUE,
    install_packages = TRUE,
    clean = FALSE
  )

  pdf_path <- normalizePath(
    file.path(latex_dir, pdf_file),
    winslash = "/",
    mustWork = TRUE
  )

  message("PDF gerado em: ", pdf_path)

  if (abrir_pdf) {
    utils::browseURL(pdf_path)
  }

  invisible(pdf_path)
}

abrir_dissertacao_pdf <- function(project_root = .dissertacao_localizar_raiz()) {
  pdf_path <- .dissertacao_pdf_path(project_root)
  utils::browseURL(pdf_path)
  invisible(pdf_path)
}

compilar_e_abrir_dissertacao <- function(
  project_root = .dissertacao_localizar_raiz(),
  repo_url = .dissertacao_repo_tinytex()
) {
  compilar_dissertacao(
    abrir_pdf = TRUE,
    project_root = project_root,
    repo_url = repo_url
  )
}
