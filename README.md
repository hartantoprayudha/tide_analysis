# tide_analysis
Analisis data tinggi muka air (pasut) dalam **bahasa R**.

## Fitur utama
- Plot domain waktu (raw, clean, low-pass, outlier).
- Deteksi outlier (MAD robust z-score).
- Koreksi offset data.
- Low-pass filter (Butterworth, fallback moving average).
- Perhitungan konstanta harmonik:
  - 4 konstanta utama: **M2, S2, K1, O1**
  - 9 konstanta: **M2, S2, N2, K2, K1, O1, P1, Q1, M4**
- Pemilihan **sensor terbaik otomatis** dari sensor yang tersedia,
  dengan kriteria: **completeness data** dan **jumlah/rate outlier**.
- Pemberian **QC flag** per data:
  - `0 = GOOD`
  - `1 = MISSING`
  - `2 = OUTLIER`
  - `3 = INTERPOLATED`

## Script
- `tide_analysis.R`

## Dependensi
Minimal:
- R base (stats, utils, grDevices)

Opsional:
- `readxl` (jika input `.xlsx`/`.xls`)
- `signal` (untuk Butterworth low-pass; jika tidak ada pakai moving average)

Instal package opsional:
```r
install.packages(c("readxl", "signal"))
```

## Contoh penggunaan
```bash
Rscript tide_analysis.R \
  --input data.xlsx \
  --sheet 1 \
  --timestamp-col "Timestamp" \
  --sensor-regex "^(PRS|RAD)" \
  --offset 0.0 \
  --outlier-z 4.0 \
  --cutoff-hours 30 \
  --save-prefix output/tide
```

Jika ingin menetapkan kandidat sensor manual:
```bash
Rscript tide_analysis.R \
  --input data.xlsx \
  --timestamp-col "Timestamp" \
  --sensor-cols "PRS1 (m),PRS2 (m),RAD1 (m)"
```

## Output
- `*_processed.csv` (termasuk kolom `qc_flag`, `qc_label`)
- `*_sensor_quality.csv` (ranking kualitas sensor & sensor terbaik)
- `*_harmonic_4const.csv`
- `*_harmonic_9const.csv`
- `*_timeseries.png`
