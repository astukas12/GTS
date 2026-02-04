# launch-gts-nascar-single-script.R
# ───────────────────────────────────────────────────────────────
# Golden Ticket Sims - NASCAR Simulator
# One-file launcher: downloads full repo ZIP → unzips → runs app
# Double-click friendly if .R files are associated with R/RStudio
# ───────────────────────────────────────────────────────────────

cat("\n")
cat("==============================================\n")
cat("   Golden Ticket Sims - NASCAR Simulation     \n")
cat("==============================================\n\n")
cat("Downloading full repository (ZIP) from GitHub...\n")

# ── Configuration ───────────────────────────────────────────────
repo_zip_url <- "https://github.com/astukas12/GTS/archive/refs/heads/main.zip"
local_base_dir <- path.expand("~/GTS")          # Change this path if desired (e.g. file.path(getwd(), "GTS"))
extract_dir    <- file.path(local_base_dir, "GTS-main")  # GitHub ZIP usually names folder repo-branch
final_app_dir  <- file.path(extract_dir, "Nascar")

# ── Step 1: Create target folder if missing ─────────────────────
dir.create(local_base_dir, showWarnings = FALSE, recursive = TRUE)

# ── Step 2: Download ZIP ────────────────────────────────────────
zip_file <- file.path(local_base_dir, "GTS-main.zip")

cat("Downloading ZIP file...\n")
dl_ok <- utils::download.file(
  url      = repo_zip_url,
  destfile = zip_file,
  mode     = "wb",                # binary mode – important for ZIP
  quiet    = FALSE
)

if (dl_ok != 0) {
  stop("Download failed. Check internet connection or try again later.")
}
cat("✓ ZIP downloaded to:", zip_file, "\n")

# ── Step 3: Unzip (overwrite if already exists) ─────────────────
cat("Unzipping repository...\n")
utils::unzip(
  zipfile   = zip_file,
  exdir     = local_base_dir,
  overwrite = TRUE
)
cat("✓ Unzipped to:", extract_dir, "\n")

# Clean up ZIP file (optional – comment out if you want to keep it)
unlink(zip_file)

# ── Step 4: Verify app folder exists ────────────────────────────
if (!dir.exists(final_app_dir)) {
  stop("Nascar folder not found after unzip. Repo structure may have changed.\n",
       "Expected path: ", final_app_dir)
}

# ── Step 5: Install missing packages (your original list + shiny) ─
required_packages <- c(
  "shiny", "shinydashboard", "shinyWidgets",
  "tidyverse", "data.table", "readxl",
  "DT", "plotly", "lpSolve",
  "memoise", "shinycssloaders", "shinyjs"
)

cat("Checking / installing required packages...\n")
new_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]

if (length(new_packages) > 0) {
  cat("Installing:", paste(new_packages, collapse = ", "), "\n")
  install.packages(new_packages, quiet = TRUE)
  cat("✓ Packages installed\n")
} else {
  cat("✓ All required packages already installed\n")
}

# ── Step 6: Launch the Shiny app ────────────────────────────────
cat("\nLaunching NASCAR simulator...\n")
cat("App directory:", final_app_dir, "\n")
cat("(CoreFunctions/ and other files are now available via relative paths)\n\n")

shiny::runApp(
  appDir       = final_app_dir,
  launch.browser = TRUE,
  host         = "127.0.0.1",
  port         = NULL
)

# If runApp() exits (e.g. user closes browser), give friendly message
cat("\nApp session ended.\n")
cat("To run again, just source this script once more.\n")