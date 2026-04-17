#!/usr/bin/env Rscript

# Analisis pasut (R):
# - Plot domain waktu
# - Outlier detection (MAD robust z-score)
# - Offset correction
# - Low-pass filter (Butterworth / moving-average fallback)
# - Estimasi konstanta harmonik (4 konstanta utama + 9 konstanta)
# - Pemilihan sensor terbaik otomatis (berdasarkan outlier & completeness)
# - QC flag data

suppressWarnings({
  options(stringsAsFactors = FALSE)
})

MAIN_4 <- data.frame(
  constituent = c("M2", "S2", "K1", "O1"),
  speed_deg_per_hour = c(28.9841042, 30.0, 15.0410686, 13.9430356)
)

MAIN_9 <- data.frame(
  constituent = c("M2", "S2", "N2", "K2", "K1", "O1", "P1", "Q1", "M4"),
  speed_deg_per_hour = c(28.9841042, 30.0, 28.4397295, 30.0821373, 15.0410686, 13.9430356, 14.9589314, 13.3986609, 57.9682084)
)

parse_args <- function() {
  defaults <- list(
    input = NULL,
    sheet = "1",
    timestamp_col = "Timestamp",
    sensor_cols = NULL,
    sensor_regex = "^(PRS|RAD|WL|TIDE)",
    offset = 0,
    outlier_z = 4,
    cutoff_hours = 30,
    save_prefix = "output/tide_analysis"
  )

  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0 || any(args %in% c("-h", "--help"))) {
    cat("Usage:\n")
    cat("Rscript tide_analysis.R --input data.xlsx [options]\n\n")
    cat("Options:\n")
    cat("  --input PATH             file input (.csv/.xlsx/.xls)\n")
    cat("  --sheet NAME_OR_INDEX    sheet excel (default: 1)\n")
    cat("  --timestamp-col NAME     kolom waktu (default: Timestamp)\n")
    cat("  --sensor-cols C1,C2      daftar kolom sensor kandidat\n")
    cat("  --sensor-regex REGEX     regex deteksi sensor otomatis\n")
    cat("  --offset NUM             offset meter (default: 0)\n")
    cat("  --outlier-z NUM          ambang robust z-score (default: 4)\n")
    cat("  --cutoff-hours NUM       cutoff low-pass jam (default: 30)\n")
    cat("  --save-prefix PREFIX     prefix output file\n")
    quit(status = 0)
  }

  i <- 1
  while (i <= length(args)) {
    key <- args[[i]]
    val <- if (i + 1 <= length(args)) args[[i + 1]] else NULL
    if (!startsWith(key, "--") || is.null(val) || startsWith(val, "--")) {
      stop(sprintf("Argumen tidak valid di posisi %d: %s", i, key))
    }
    nm <- gsub("-", "_", sub("^--", "", key))
    if (!nm %in% names(defaults)) stop(sprintf("Argumen tidak dikenali: %s", key))
    defaults[[nm]] <- val
    i <- i + 2
  }

  defaults$offset <- as.numeric(defaults$offset)
  defaults$outlier_z <- as.numeric(defaults$outlier_z)
  defaults$cutoff_hours <- as.numeric(defaults$cutoff_hours)

  if (is.null(defaults$input) || defaults$input == "") {
    stop("--input wajib diisi")
  }
  defaults
}

load_data <- function(input_path, sheet) {
  ext <- tolower(tools::file_ext(input_path))
  if (ext == "csv") {
    return(utils::read.csv(input_path, check.names = FALSE))
  }
  if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package 'readxl' belum terpasang untuk membaca Excel")
    }
    sheet_use <- suppressWarnings(as.integer(sheet))
    if (is.na(sheet_use)) sheet_use <- sheet
    return(readxl::read_excel(input_path, sheet = sheet_use))
  }
  stop("Format file tidak didukung. Pakai .csv/.xlsx/.xls")
}

robust_outlier <- function(x, z_thresh = 4) {
  med <- stats::median(x, na.rm = TRUE)
  mad_val <- stats::median(abs(x - med), na.rm = TRUE)
  if (is.na(mad_val) || mad_val == 0) {
    return(rep(FALSE, length(x)))
  }
  z <- 0.6745 * (x - med) / mad_val
  abs(z) > z_thresh
}

infer_dt_hours <- function(ts) {
  d <- diff(as.numeric(ts)) / 3600
  d <- d[is.finite(d) & d > 0]
  if (length(d) == 0) stop("Gagal inferensi interval sampling")
  stats::median(d)
}

lowpass_filter <- function(x, dt_hours, cutoff_hours) {
  if (requireNamespace("signal", quietly = TRUE)) {
    fs <- 1 / dt_hours
    fc <- 1 / cutoff_hours
    wn <- min(max(fc / (0.5 * fs), 1e-6), 0.999999)
    bf <- signal::butter(4, wn, type = "low")
    return(as.numeric(signal::filtfilt(bf, x)))
  }

  warning("Package 'signal' tidak tersedia, fallback moving average")
  k <- max(round(cutoff_hours / dt_hours), 3)
  filt <- stats::filter(x, rep(1 / k, k), sides = 2)
  as.numeric(ifelse(is.na(filt), x, filt))
}

interpolate_time <- function(ts, x) {
  keep <- is.finite(x)
  if (sum(keep) < 2) return(x)
  out <- approx(x = as.numeric(ts[keep]), y = x[keep], xout = as.numeric(ts), method = "linear", rule = 2)$y
  as.numeric(out)
}

calc_sensor_quality <- function(df, sensor_col, outlier_z) {
  vals <- suppressWarnings(as.numeric(df[[sensor_col]]))
  valid <- is.finite(vals)
  completeness <- sum(valid) / length(vals)

  outlier_mask <- rep(FALSE, length(vals))
  if (sum(valid) > 3) {
    outlier_mask[valid] <- robust_outlier(vals[valid], z_thresh = outlier_z)
  }
  outlier_count <- sum(outlier_mask, na.rm = TRUE)
  outlier_rate <- if (sum(valid) == 0) 1 else outlier_count / sum(valid)

  # Skor: completeness tinggi lebih baik, outlier rate rendah lebih baik
  score <- 0.7 * completeness + 0.3 * (1 - outlier_rate)

  data.frame(
    sensor = sensor_col,
    completeness = completeness,
    valid_count = sum(valid),
    outlier_count = outlier_count,
    outlier_rate = outlier_rate,
    score = score
  )
}

choose_best_sensor <- function(df, timestamp_col, explicit_cols, regex_cols, outlier_z) {
  cols <- names(df)
  excluded <- c(timestamp_col, "TZ", "tz", "Time", "Date", "Battery (V)", "Solar (V)")

  if (!is.null(explicit_cols) && nchar(explicit_cols) > 0) {
    candidates <- trimws(strsplit(explicit_cols, ",")[[1]])
  } else {
    regex_hits <- cols[grepl(regex_cols, cols, ignore.case = TRUE, perl = TRUE)]
    numeric_hits <- cols[sapply(df, function(x) {
      suppressWarnings(sum(is.finite(as.numeric(x)))) > max(5, 0.3 * length(x))
    })]
    candidates <- unique(c(regex_hits, numeric_hits))
  }

  candidates <- setdiff(candidates, excluded)
  candidates <- candidates[candidates %in% names(df)]
  if (length(candidates) == 0) stop("Tidak ada kolom sensor kandidat yang ditemukan")

  quality <- do.call(rbind, lapply(candidates, function(cn) calc_sensor_quality(df, cn, outlier_z)))
  quality <- quality[order(-quality$score, -quality$completeness, quality$outlier_count), ]
  rownames(quality) <- NULL

  list(best = quality$sensor[1], table = quality)
}

harmonic_fit <- function(t_hours, y, constituents_df) {
  X <- matrix(1, nrow = length(t_hours), ncol = 1)
  colnames(X) <- "mean"

  for (i in seq_len(nrow(constituents_df))) {
    spd <- constituents_df$speed_deg_per_hour[i]
    om <- spd * pi / 180
    c_col <- cos(om * t_hours)
    s_col <- sin(om * t_hours)
    X <- cbind(X, c_col, s_col)
    colnames(X)[(ncol(X)-1):ncol(X)] <- c(paste0(constituents_df$constituent[i], "_cos"), paste0(constituents_df$constituent[i], "_sin"))
  }

  fit <- lm.fit(x = X, y = y)
  coef <- fit$coefficients

  rows <- list()
  idx <- 2
  for (i in seq_len(nrow(constituents_df))) {
    a <- coef[idx]
    b <- coef[idx + 1]
    amp <- sqrt(a^2 + b^2)
    phase <- (atan2(-b, a) * 180 / pi) %% 360
    rows[[i]] <- data.frame(
      constituent = constituents_df$constituent[i],
      speed_deg_per_hour = constituents_df$speed_deg_per_hour[i],
      amplitude_m = amp,
      phase_deg = phase,
      a_cos = a,
      b_sin = b
    )
    idx <- idx + 2
  }

  out <- do.call(rbind, rows)
  out <- out[order(-out$amplitude_m), ]
  attr(out, "mean_level_m") <- coef[1]
  rownames(out) <- NULL
  out
}

plot_timeseries <- function(df, file_png, sensor_name) {
  grDevices::png(file_png, width = 1600, height = 700, res = 130)
  on.exit(grDevices::dev.off(), add = TRUE)

  y_min <- min(c(df$level_raw_m, df$level_clean_m, df$level_lowpass_m), na.rm = TRUE)
  y_max <- max(c(df$level_raw_m, df$level_clean_m, df$level_lowpass_m), na.rm = TRUE)

  plot(df$timestamp, df$level_raw_m, type = "l", col = "grey60", lwd = 1,
       xlab = "Waktu", ylab = "Elevasi (m)", ylim = c(y_min, y_max),
       main = paste0("Analisis Tinggi Muka Air - Sensor Terpilih: ", sensor_name))
  lines(df$timestamp, df$level_clean_m, col = "dodgerblue3", lwd = 1.2)
  lines(df$timestamp, df$level_lowpass_m, col = "firebrick", lwd = 2)

  out_idx <- which(df$qc_flag == 2)
  if (length(out_idx) > 0) {
    points(df$timestamp[out_idx], df$level_offset_m[out_idx], pch = 4, col = "black", cex = 0.8)
  }

  legend("topleft",
         legend = c("Raw", "Clean (offset + outlier fill)", "Low-pass", "Outlier"),
         col = c("grey60", "dodgerblue3", "firebrick", "black"),
         lty = c(1, 1, 1, NA), pch = c(NA, NA, NA, 4), bty = "n")
  grid(col = "grey85")
}

main <- function() {
  args <- parse_args()

  input_path <- args$input
  save_prefix <- args$save_prefix
  out_dir <- dirname(save_prefix)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  df <- load_data(input_path, args$sheet)
  if (!args$timestamp_col %in% names(df)) stop("Kolom timestamp tidak ditemukan")

  choose <- choose_best_sensor(
    df = df,
    timestamp_col = args$timestamp_col,
    explicit_cols = args$sensor_cols,
    regex_cols = args$sensor_regex,
    outlier_z = args$outlier_z
  )
  best_sensor <- choose$best
  sensor_quality <- choose$table

  ts <- as.POSIXct(df[[args$timestamp_col]], tz = "UTC")
  if (any(is.na(ts))) {
    ts <- as.POSIXct(df[[args$timestamp_col]], format = "%m/%d/%Y %H:%M", tz = "UTC")
  }

  level <- suppressWarnings(as.numeric(df[[best_sensor]]))

  base <- data.frame(timestamp = ts, level_raw_m = level)
  base <- base[is.finite(as.numeric(base$timestamp)), ]
  base <- base[order(base$timestamp), ]

  base$level_offset_m <- base$level_raw_m + args$offset

  # QC flags:
  # 0 = GOOD
  # 1 = MISSING
  # 2 = OUTLIER
  # 3 = INTERPOLATED
  base$qc_flag <- ifelse(is.na(base$level_offset_m), 1, 0)

  valid <- is.finite(base$level_offset_m)
  out_mask <- rep(FALSE, nrow(base))
  if (sum(valid) > 3) {
    out_mask[valid] <- robust_outlier(base$level_offset_m[valid], z_thresh = args$outlier_z)
  }
  base$qc_flag[out_mask] <- 2

  masked <- base$level_offset_m
  masked[out_mask] <- NA
  filled <- interpolate_time(base$timestamp, masked)

  interp_idx <- is.na(masked) & is.finite(filled)
  base$qc_flag[interp_idx & base$qc_flag != 2] <- 3
  base$level_clean_m <- filled

  dt_hours <- infer_dt_hours(base$timestamp)
  base$level_lowpass_m <- lowpass_filter(base$level_clean_m, dt_hours = dt_hours, cutoff_hours = args$cutoff_hours)

  qc_labels <- c("GOOD", "MISSING", "OUTLIER", "INTERPOLATED")
  base$qc_label <- qc_labels[base$qc_flag + 1]

  t_hours <- as.numeric(difftime(base$timestamp, min(base$timestamp), units = "hours"))
  y <- base$level_clean_m

  h4 <- harmonic_fit(t_hours, y, MAIN_4)
  h9 <- harmonic_fit(t_hours, y, MAIN_9)

  processed_csv <- paste0(save_prefix, "_processed.csv")
  quality_csv <- paste0(save_prefix, "_sensor_quality.csv")
  h4_csv <- paste0(save_prefix, "_harmonic_4const.csv")
  h9_csv <- paste0(save_prefix, "_harmonic_9const.csv")
  plot_png <- paste0(save_prefix, "_timeseries.png")

  utils::write.csv(base, processed_csv, row.names = FALSE)
  utils::write.csv(sensor_quality, quality_csv, row.names = FALSE)
  utils::write.csv(h4, h4_csv, row.names = FALSE)
  utils::write.csv(h9, h9_csv, row.names = FALSE)

  plot_timeseries(base, plot_png, best_sensor)

  cat("\n=== Ringkasan Analisis ===\n")
  cat(sprintf("Input                    : %s\n", input_path))
  cat(sprintf("Sensor terbaik           : %s\n", best_sensor))
  cat(sprintf("Jumlah data              : %d\n", nrow(base)))
  cat(sprintf("Completeness sensor      : %.2f%%\n", 100 * sensor_quality$completeness[1]))
  cat(sprintf("Outlier terdeteksi       : %d\n", sum(base$qc_flag == 2, na.rm = TRUE)))
  cat(sprintf("Interval sampling (jam)  : %.5f\n", dt_hours))
  cat(sprintf("Offset diterapkan (m)    : %.4f\n", args$offset))
  cat(sprintf("Low-pass cutoff (jam)    : %.2f\n", args$cutoff_hours))
  cat(sprintf("Mean level (4 konstanta) : %.5f m\n", attr(h4, "mean_level_m")))
  cat(sprintf("Mean level (9 konstanta) : %.5f m\n", attr(h9, "mean_level_m")))

  cat("\nOutput files:\n")
  cat(sprintf("- %s\n", processed_csv))
  cat(sprintf("- %s\n", quality_csv))
  cat(sprintf("- %s\n", h4_csv))
  cat(sprintf("- %s\n", h9_csv))
  cat(sprintf("- %s\n", plot_png))
}

main()
