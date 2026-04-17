# tide_analysis
Analisis data tinggi muka air (pasut) dalam **bahasa R**.

## Fitur utama
- Plot domain waktu (raw, clean, low-pass, outlier).
- Deteksi outlier (MAD robust z-score).
- Koreksi offset data.
- Low-pass filter (Butterworth, fallback moving average).
- Pemilihan **sensor terbaik otomatis** berdasarkan **completeness** dan **outlier rate**.
- QC flag per data:
  - `0 = GOOD`
  - `1 = MISSING`
  - `2 = OUTLIER`
  - `3 = INTERPOLATED`
- Perhitungan konstanta harmonik:
  - `4` -> M2, S2, K1, O1
  - `9` -> M2, S2, N2, K2, K1, O1, P1, Q1, M4
  - `ukho_ttp` -> set kompatibel UKHO TotalTide Plus (operasional)
  - `all` -> hitung semua mode sekaligus

## Script
- `tide_analysis.R` (engine fungsi analisis)
- `main_tide.R` (**entry-point interaktif**, tidak perlu `Rscript`)

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

## Cara penggunaan interaktif (tanpa Rscript)
1. Buka R / RStudio.
2. Jalankan:
```r
source("main_tide.R")
start_tide_app()
```
3. Ikuti prompt interaktif sampai selesai.

Pada mode interaktif, Anda akan diminta mengisi:
- path file data,
- kolom timestamp,
- mode pemilihan sensor (otomatis/manual),
- parameter outlier, offset, low-pass,
- mode konstanta harmonik (`all`, `4`, `9`, `ukho_ttp`),
- prefix output.

## Output
- `*_processed.csv` (termasuk `qc_flag`, `qc_label`)
- `*_sensor_quality.csv` (ranking kualitas sensor)
- `*_harmonic_4const.csv` (mode `4`/`all`)
- `*_harmonic_9const.csv` (mode `9`/`all`)
- `*_harmonic_ukho_ttp.csv` (mode `ukho_ttp`/`all`)
- `*_timeseries.png`

## Catatan perubahan `main_tide.R`
- Menjadi wizard interaktif, sehingga penggunaan tidak wajib melalui `Rscript`.
- Input parameter dilakukan langkah demi langkah dengan prompt yang mudah dipahami.
- Validasi file input dan validasi angka dilakukan di awal.
- Menampilkan pesan `[INFO]` / `[ERROR]` agar status proses lebih jelas.
