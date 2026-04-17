#!/usr/bin/env Rscript

# ============================================================
# main_tide.R
# Entry-point yang lebih user-friendly untuk analisis pasut.
#
# Tujuan script ini:
# 1) Menampilkan bantuan penggunaan yang mudah dipahami.
# 2) Memvalidasi input dasar (misalnya --input wajib ada dan file valid).
# 3) Memanggil fungsi utama dari tide_analysis.R.
# ============================================================

print_help <- function() {
  cat("\n===================== MAIN TIDE (R) =====================\n")
  cat("Script ini menjalankan analisis tinggi muka air (pasut):\n")
  cat("- pilih sensor terbaik otomatis\n")
  cat("- deteksi outlier + QC flag\n")
  cat("- offset correction\n")
  cat("- low-pass filter\n")
  cat("- konstanta harmonik (4 dan 9 konstanta)\n\n")

  cat("Pemakaian:\n")
  cat("  Rscript main_tide.R --input <file.csv/xlsx> [opsi]\n\n")

  cat("Opsi umum:\n")
  cat("  --input PATH             File input (wajib)\n")
  cat("  --sheet NAME/INDEX       Sheet Excel (default: 1)\n")
  cat("  --timestamp-col NAME     Nama kolom waktu (default: Timestamp)\n")
  cat("  --sensor-cols C1,C2      Kandidat sensor manual (dipisah koma)\n")
  cat("  --sensor-regex REGEX     Regex auto-detect sensor\n")
  cat("  --offset NUM             Offset meter (default: 0)\n")
  cat("  --outlier-z NUM          Ambang z-score outlier (default: 4)\n")
  cat("  --cutoff-hours NUM       Periode cutoff low-pass jam (default: 30)\n")
  cat("  --save-prefix PREFIX     Prefix output (default: output/tide_analysis)\n")
  cat("  --help                   Tampilkan bantuan ini\n\n")

  cat("Contoh cepat:\n")
  cat("  Rscript main_tide.R --input data.xlsx --timestamp-col \"Timestamp\" \\\n")
  cat("    --sensor-regex \"^(PRS|RAD)\" --save-prefix output/tide\n\n")

  cat("Contoh sensor manual:\n")
  cat("  Rscript main_tide.R --input data.xlsx --timestamp-col \"Timestamp\" \\\n")
  cat("    --sensor-cols \"PRS1 (m),PRS2 (m),RAD1 (m)\"\n")
  cat("=========================================================\n\n")
}

# Cari path script saat ini agar source() tetap benar walau dipanggil dari folder lain.
get_script_dir <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    script_path <- normalizePath(sub("^--file=", "", file_arg[1]))
    return(dirname(script_path))
  }
  # Fallback untuk mode interaktif/development.
  return(getwd())
}

# Ambil nilai dari CLI: --key value
get_arg_value <- function(args, key) {
  idx <- which(args == key)
  if (length(idx) == 0) return(NULL)
  pos <- idx[1] + 1
  if (pos > length(args) || startsWith(args[pos], "--")) return(NULL)
  args[pos]
}

run <- function() {
  args <- commandArgs(trailingOnly = TRUE)

  # Tampilkan help jika user minta atau belum memberikan argumen.
  if (length(args) == 0 || any(args %in% c("-h", "--help"))) {
    print_help()
    quit(status = 0)
  }

  # Validasi argumen wajib --input agar error lebih jelas.
  input_path <- get_arg_value(args, "--input")
  if (is.null(input_path) || input_path == "") {
    cat("[ERROR] Argumen --input wajib diisi.\n\n")
    print_help()
    quit(status = 1)
  }
  if (!file.exists(input_path)) {
    cat(sprintf("[ERROR] File input tidak ditemukan: %s\n", input_path))
    cat("Pastikan path benar, lalu jalankan kembali.\n")
    quit(status = 1)
  }

  # Source library analisis.
  script_dir <- get_script_dir()
  source(file.path(script_dir, "tide_analysis.R"))

  # Jalankan proses utama dengan handler error yang ramah pengguna.
  tryCatch(
    {
      cat("\n[INFO] Menjalankan analisis pasut...\n")
      main()
      cat("[INFO] Analisis selesai.\n")
    },
    error = function(e) {
      cat(sprintf("\n[ERROR] Proses gagal: %s\n", conditionMessage(e)))
      cat("Tips: cek nama kolom timestamp/sensor dan format file input.\n")
      quit(status = 1)
    }
  )
}

run()
