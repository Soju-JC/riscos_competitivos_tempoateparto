[CmdletBinding()]
param(
  [switch]$AbrirPdf
)

$ErrorActionPreference = "Stop"

$projectRoot = Split-Path -Parent $PSScriptRoot
$latexDir = Join-Path $projectRoot "trabalho_LaTeX"
$pdfPath = Join-Path $latexDir "main.pdf"
$compileScript = Join-Path $PSScriptRoot "compilar_dissertacao.R"
$repoUrl = "https://ftp.math.utah.edu/pub/tex/historic/systems/texlive/2025/tlnet-final"

function Get-RscriptPath {
  $command = Get-Command Rscript.exe -ErrorAction SilentlyContinue
  if ($command) {
    return $command.Source
  }

  $rBase = "C:\Program Files\R"
  if (Test-Path $rBase) {
    $candidate = Get-ChildItem $rBase -Directory -Filter "R-*" |
      Sort-Object Name -Descending |
      ForEach-Object {
        Join-Path $_.FullName "bin\Rscript.exe"
      } |
      Where-Object { Test-Path $_ } |
      Select-Object -First 1

    if ($candidate) {
      return $candidate
    }
  }

  return $null
}

if (-not (Test-Path $compileScript)) {
  throw "Script de compilação não encontrado: $compileScript"
}

$rscriptPath = Get-RscriptPath
if (-not $rscriptPath) {
  throw "Não encontrei o Rscript.exe. Verifique se o R está instalado neste computador."
}

Write-Host "Compilando a dissertação em $latexDir" -ForegroundColor Cyan

& $rscriptPath --vanilla $compileScript $projectRoot $repoUrl

if ($LASTEXITCODE -ne 0) {
  exit $LASTEXITCODE
}

if (-not (Test-Path $pdfPath)) {
  throw "A compilação terminou, mas o PDF final não foi encontrado em $pdfPath"
}

Write-Host "PDF gerado em: $pdfPath" -ForegroundColor Green

if ($AbrirPdf) {
  Start-Process $pdfPath
}
