# ============================================================
# main_tide.R (INTERAKTIF)
#
# Cara pakai (tanpa Rscript):
#   source("main_tide.R")
#   start_tide_app()
#
# Script ini akan menanyakan input langkah demi langkah.
# ============================================================

resolve_script_dir <- function() {
  # Coba cari path file script saat ini dari frame source().
  frames <- sys.frames()
  if (length(frames) > 0) {
    for (i in rev(seq_along(frames))) {
      ofile <- frames[[i]]$ofile
      if (!is.null(ofile) && nzchar(ofile)) {
        return(dirname(normalizePath(ofile)))
      }
    }
  }
  # Fallback jika path tidak terdeteksi.
  getwd()
}

script_dir <- resolve_script_dir()
source(file.path(script_dir, "tide_analysis.R"))

ask <- function(prompt_text, default = NULL) {
  if (is.null(default)) {
    val <- readline(paste0(prompt_text, ": "))
  } else {
    val <- readline(paste0(prompt_text, " [", default, "]: "))
    if (!nzchar(trimws(val))) val <- as.character(default)
  }
  trimws(val)
}

ask_numeric <- function(prompt_text, default) {
  repeat {
    val <- ask(prompt_text, default)
    num <- suppressWarnings(as.numeric(val))
    if (is.finite(num)) return(num)
    cat("Input harus angka. Coba lagi.\n")
  }
}

ask_existing_file <- function() {
  repeat {
    path <- ask("Masukkan path file data (.csv/.xlsx/.xls)")
    if (!nzchar(path)) {
      cat("Path file tidak boleh kosong.\n")
      next
    }
    if (!file.exists(path)) {
      cat("File tidak ditemukan. Cek kembali path Anda.\n")
      next
    }
    return(path)
  }
}

ask_harmonic_mode <- function() {
  cat("\nPilih mode konstanta harmonik:\n")
  cat("  1) all      -> hitung 4, 9, dan UKHO TotalTide Plus\n")
  cat("  2) 4        -> 4 konstanta utama\n")
  cat("  3) 9        -> 9 konstanta\n")
  cat("  4) ukho_ttp -> UKHO TotalTide Plus\n")

  repeat {
    cval <- ask("Pilihan", "1")
    if (cval %in% c("1", "all")) return("all")
    if (cval %in% c("2", "4")) return("4")
    if (cval %in% c("3", "9")) return("9")
    if (cval %in% c("4", "ukho_ttp")) return("ukho_ttp")
    cat("Pilihan tidak valid. Masukkan 1/2/3/4.\n")
  }
}

build_config_interactive <- function() {
  cat("\n========== Tide Analysis Interactive ==========")
  cat("\nIsi parameter berikut (tekan Enter untuk default).\n\n")

  input_path <- ask_existing_file()
  sheet <- ask("Sheet Excel (abaikan untuk CSV)", "1")
  timestamp_col <- ask("Nama kolom waktu", "Timestamp")

  cat("\nMode pemilihan sensor:\n")
  cat("  1) Otomatis pakai regex\n")
  cat("  2) Manual (daftar kolom sensor)\n")

  sensor_cols <- NULL
  sensor_regex <- "^(PRS|RAD|WL|TIDE)"

  mode_sensor <- ask("Pilih mode sensor (1/2)", "1")
  if (mode_sensor == "2") {
    sensor_cols <- ask("Daftar kolom sensor (pisahkan koma), contoh: PRS1 (m),PRS2 (m),RAD1 (m)")
  } else {
    sensor_regex <- ask("Regex deteksi sensor", "^(PRS|RAD|WL|TIDE)")
  }

  offset <- ask_numeric("Offset (meter)", 0)
  outlier_z <- ask_numeric("Ambang outlier robust z-score", 4)
  cutoff_hours <- ask_numeric("Cutoff low-pass (jam)", 30)
  harmonic_set <- ask_harmonic_mode()
  save_prefix <- ask("Prefix output", "output/tide_analysis")

  list(
    input = input_path,
    sheet = sheet,
    timestamp_col = timestamp_col,
    sensor_cols = sensor_cols,
    sensor_regex = sensor_regex,
    offset = offset,
    outlier_z = outlier_z,
    cutoff_hours = cutoff_hours,
    harmonic_set = harmonic_set,
    save_prefix = save_prefix
  )
}

start_tide_app <- function() {
  repeat {
    cfg <- build_config_interactive()

    cat("\n[INFO] Menjalankan analisis...\n")
    ok <- TRUE

    tryCatch(
      {
        run_analysis(cfg)
      },
      error = function(e) {
        ok <<- FALSE
        cat(sprintf("\n[ERROR] %s\n", conditionMessage(e)))
        cat("Cek kembali parameter/kolom data Anda.\n")
      }
    )

    if (ok) cat("\n[INFO] Analisis selesai dengan sukses.\n")

    again <- tolower(ask("Ingin menjalankan analisis lagi? (y/n)", "n"))
    if (!again %in% c("y", "yes")) break
  }

  cat("\nTerima kasih. Sesi interaktif selesai.\n")
}

cat("main_tide.R berhasil dimuat. Jalankan: start_tide_app()\n")
