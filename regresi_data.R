# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Install packages 
pkgs <- c("readxl", "ggplot2", "dplyr", "broom", "ggpubr")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    try(install.packages(p, type = "binary"))
  }
}

# Load libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)

# Import data excel 
file_path <- "Tugas 2 - data_pendapatan.xlsx"
# baca tanpa header untuk mengantisipasi header kosong
raw <- read_excel(file_path, col_names = FALSE)

print("Nama kolom awal:")
print(names(raw))
print("2 baris pertama data:")
print(head(raw,2))

# Membersihkan data sesuai struktur yang terlihat sebelumnya:
# asumsi: baris pertama adalah header teks/label yang tidak berguna -> hapus
# kolom: [1]=index (atau kosong), [2]=pendapatan, [3]=kebahagiaan

if (ncol(raw) < 3) stop("File memiliki kurang dari 3 kolom. Periksa file Excel.")

# slice(-1) → untuk menghapus baris pertama
# setNames() → memberi nama kolom
# kolom 1 → index
# kolom 2 → pendapatan
# kolom 3 → kebahagiaan
# select() → hanya ambil pendapatan & kebahagiaan.
# mutate(... as.numeric) → ubah semua kolom menjadi numerik.

df <- raw %>%
  slice(-1) %>%
  setNames(c("index","pendapatan","kebahagiaan")) %>%
  select(pendapatan, kebahagiaan) %>%
  mutate(across(everything(), as.numeric))

# Cek data
# str() → menunjukkan tipe data tiap kolom.
# summary() → statistik dasar (min, max, mean, median).

print(str(df))
print(summary(df))

# Histogram variabel terikat (kebahagiaan)
# Menggambar histogram untuk melihat distribusi kebahagiaan normal

png("histogram_kebahagiaan.png", width = 800, height = 600)
p1 <- ggplot(df, aes(x = kebahagiaan)) +
  geom_histogram(bins = 20, fill = "lightblue", color = "black") +
  ggtitle("Histogram Kebahagiaan") +
  xlab("Kebahagiaan") + ylab("Frekuensi")
print(p1)
dev.off()

# Scatter plot pendapatan vs kebahagiaan
# Untuk melihat keduanya punya hubungan linear atau tidak
png("scatter_pendapatan_kebahagiaan.png", width = 800, height = 600)
p2 <- ggplot(df, aes(x = pendapatan, y = kebahagiaan)) +
  geom_point() +
  ggtitle("Scatter: Pendapatan vs Kebahagiaan") +
  xlab("Pendapatan") + ylab("Kebahagiaan")
print(p2)
dev.off()

# Regresi linear sederhana
# lm(y ~ x) = model regresi linear.
# summary() memberikan: koefisien regresi, p-value, R-squared dan statistik lainnya.
model <- lm(kebahagiaan ~ pendapatan, data = df)
model_sum <- summary(model)
print(model_sum)

# Simpan ringkasan model ke file teks
capture.output(model_sum, file = "summary_model.txt")

# Cek homoskedastisitas - residual plot (simpan PNG)
# .resid → residual
# .fitted → nilai prediksi model
# Kalau titik menyebar acak → homoskedastisitas terpenuhi.
png("residuals_vs_fitted.png", width = 800, height = 600)
resid_df <- broom::augment(model)
p3 <- ggplot(resid_df, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ggtitle("Residuals vs Fitted") +
  xlab("Fitted values") + ylab("Residuals")
print(p3)
dev.off()

# Visualisasi regresi lengkap: points + regression line + equation + labels
# ambil koefisien untuk membuat persamaan
coef <- coef(model)
intercept <- round(coef[1], 3)
slope <- round(coef[2], 3)
eqn <- paste0("\u03C7 = ", intercept, " + ", slope, " x")

png("regresi_linear_plot.png", width = 900, height = 700)
p4 <- ggplot(df, aes(x = pendapatan, y = kebahagiaan)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  annotate("text", x = min(df$pendapatan, na.rm = TRUE),
           y = max(df$kebahagiaan, na.rm = TRUE),
           label = paste0("y = ", intercept, " + ", slope, " x"),
           hjust = 0, vjust = 1, size = 5) +
  ggtitle("Regresi Linear: Pendapatan vs Kebahagiaan") +
  xlab("Pendapatan") + ylab("Kebahagiaan")
print(p4)
# menyimpan semua hasil grafik 
dev.off()

# Informasi akhir
cat("Selesai. File yang disimpan: histogram_kebahagiaan.png, scatter_pendapatan_kebahagiaan.png,\n")
cat("residuals_vs_fitted.png, regresi_linear_plot.png, summary_model.txt\n")
