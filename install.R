# DataExplorerPro Installation Script
# AI-Powered Data Exploration Studio for RStudio

cat("=================================================\n")
cat("  DataExplorerPro - Installation Script\n")
cat("=================================================\n\n")

# Check R version
r_version <- getRversion()
if (r_version < "4.0.0") {
  cat("⚠️  Warning: R 4.0.0 or higher is recommended.\n")
  cat("   Current version:", r_version, "\n\n")
}

# Install required packages
cat("📦 Installing required packages...\n\n")

required_packages <- c(
  "shiny",
  "plotly", 
  "dplyr",
  "tidyr",
  "ggplot2",
  "skimr",
  "DT",
  "jsonlite",
  "httr",
  "glue",
  "purrr",
  "stringr",
  "lubridate",
  "scales",
  "htmltools",
  "bslib",
  "gargoyle",
  "shinyjs",
  "readxl",
  "arrow",
  "RColorBrewer"
)

# Install missing packages
installed_packages <- rownames(installed.packages())
missing_packages <- required_packages[!required_packages %in% installed_packages]

if (length(missing_packages) > 0) {
  cat("Installing packages:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages, repos = "https://cloud.r-project.org", quiet = FALSE)
} else {
  cat("✓ All required packages are already installed.\n")
}

cat("\n")

# Check for Ollama and available models
cat("🤖 Checking Ollama installation...\n")

# Check if Ollama is installed
ollama_installed <- tryCatch({
  system("ollama --version", intern = TRUE, timeout = 5)
  TRUE
}, error = function(e) FALSE)

if (!ollama_installed) {
  cat("⚠️  Ollama is not installed.\n")
  cat("   Install from: https://ollama.com\n")
  cat("   Or run: curl -fsSL https://ollama.ai/install.sh | sh\n\n")
} else {
  cat("✓ Ollama is installed.\n")
  
  # Check if Ollama is running
  cat("🔍 Checking if Ollama is running...\n")
  ollama_running <- tryCatch({
    response <- httr::POST("http://localhost:11434/api/tags", timeout = 3)
    httr::status_code(response) == 200
  }, error = function(e) FALSE)
  
  if (!ollama_running) {
    cat("⚠️  Ollama is installed but not running.\n")
    cat("   Start Ollama with: ollama serve\n")
    cat("   Or click the Ollama icon in your system tray.\n\n")
  } else {
    cat("✓ Ollama is running.\n")
    
    # Check for existing models
    cat("📋 Checking for existing models...\n")
    tryCatch({
      response <- httr::POST("http://localhost:11434/api/tags", timeout = 5)
      if (httr::status_code(response) == 200) {
        result <- httr::content(response, as = "parsed")
        models <- sapply(result$models, function(x) x$name)
        
        if (length(models) > 0) {
          cat("✓ Found", length(models), "installed model(s):\n")
          for (m in models) {
            cat("   - ", m, "\n")
          }
          cat("\n   No need to pull - models are ready!\n")
        } else {
          cat("⚠️  No models found. You need to pull a model.\n\n")
          cat("📥 Pulling llama3.2 (recommended for best results)...\n")
          cat("   This may take a few minutes on first run...\n\n")
          
          # Pull the model
          pull_result <- system("ollama pull llama3.2", intern = TRUE, timeout = 600)
          cat("✓ Model pulled successfully!\n")
        }
      }
    }, error = function(e) {
      cat("⚠️  Could not fetch model list. You may need to pull a model manually.\n")
      cat("   Run: ollama pull llama3.2\n")
    })
  }
}

cat("\n")

# Install the DataExplorerPro package
cat("📦 Installing DataExplorerPro package...\n")

# Get the directory where this script is located
script_dir <- getwd()
package_path <- script_dir

# Install from local source
install.packages(package_path, repos = NULL, type = "source")

cat("\n")

# Verify installation
cat("✅ Verifying installation...\n")
if ("DataExplorerPro" %in% rownames(installed.packages())) {
  cat("✓ DataExplorerPro installed successfully!\n")
} else {
  cat("❌ Installation failed. Please check errors above.\n")
}

cat("\n")
cat("=================================================\n")
cat("  Installation Complete!\n")
cat("=================================================\n\n")

cat("📖 Usage:\n")
cat("   1. In RStudio, go to Addins > DataExplorerPro\n")
cat("   2. Or run: DataExplorerPro::addin()\n")
cat("   3. Or run: DataExplorerPro::run_app()\n\n")

cat("📝 Requirements:\n")
cat("   - R 4.0+\n")
cat("   - Ollama running (for AI features)\n")
cat("   - 2GB RAM minimum\n\n")

cat("🔗 Resources:\n")
cat("   - Documentation: See README.md\n")
cat("   - GitHub: [Add your repo URL]\n")
cat("   - Issues: [Add your repo URL]/issues\n\n")

cat("Happy Data Exploring! 🚀\n")