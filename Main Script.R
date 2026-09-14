# ==========================
# 1. PERSIAPAN LIBRARY
# ==========================
library(readxl)
library(mirt)
library(dplyr)

# ==========================
# 2. IMPORT DATA EXCEL
# ==========================
pg_data <- read_excel("Dataset.xlsx", sheet = "PG")
pg_data <- pg_data[,-1]
pgk_data <- read_excel("Dataset.xlsx", sheet = "PGK")
pgk_data <- pgk_data[,-1]
m_data <- read_excel("Dataset.xlsx", sheet = "M")
m_data <- m_data[,-1]
is_data <- read_excel("Dataset.xlsx", sheet = "IS")
is_data <- is_data[,-1]

# ==========================
# 3. MODEL IRT
# ==========================
# ---- PG (Pilihan Ganda) | 2PL ----
model_pg <- mirt(pg_data, 1, itemtype = "2PL")

# ---- PGK (Pilihan Ganda Komples) | GRM (Polytomous) ----
model_pgk <- mirt(pgk_data, 1, itemtype = "graded")

# ---- M (Menjodohkan) | GRM (Polytomous) ----
model_m <- mirt(m_data, 1, itemtype = "graded")

# ---- IS (Isian Singkat) | 2PL ----
model_is <- mirt(is_data, 1, itemtype = "2PL")

# Item fit
itemfit(model_pg)
itemfit(model_pgk)
itemfit(model_m)
itemfit(model_is)

# IRT parameters
coef(model_pg, IRTpars = TRUE, simplify = TRUE)
coef(model_pgk, IRTpars = TRUE, simplify = TRUE)
coef(model_m, IRTpars = TRUE, simplify = TRUE)
coef(model_is, IRTpars = TRUE, simplify = TRUE)

# Factor Loadings
summary(model_pg)
summary(model_pgk)
summary(model_m)
summary(model_is)

# ==========================
# 4. VISUALISASI DAN RELIABILITAS SOAL
# ==========================

# ICC Plot
plot(model_pg, type = "trace", main = "PG ICC Plot")
plot(model_pgk, type = "trace",  main = "PGK ICC Plot")
plot(model_m, type = "trace",  main = "M ICC Plot")
plot(model_is, type = "trace",  main = "IS ICC Plot")

# Item Information Function
plot(model_pg, type = "infotrace",  main = "PG IIF Plot")
plot(model_pgk, type = "infotrace",  main = "PGK IIF Plot")
plot(model_m, type = "infotrace",  main = "M IIF Plot")
plot(model_is, type = "infotrace",  main = "IS IIF Plot")

# Scale information and conditional standard errors
plot(model_pg, type = 'infoSE',  main = "PG SE Plot")
plot(model_pgk, type = 'infoSE',  main = "PGK SE Plot")
plot(model_m, type = 'infoSE',  main = "M SE Plot")
plot(model_is, type = 'infoSE',  main = "IS SE Plot")

# Conditional reliability
plot(model_pg, type = 'rxx',  main = "PG CR Plot")
plot(model_pgk, type = 'rxx',  main = "PGK CR Plot")
plot(model_m, type = 'rxx',  main = "M CR Plot")
plot(model_is, type = 'rxx',  main = "IS CR Plot")

# Marginal reliability
marginal_rxx(model_pg)
marginal_rxx(model_pgk)
marginal_rxx(model_m)
marginal_rxx(model_is)

# Scale characteristic curve
plot(model_pg, type = 'score',  main = "PG SC Plot")
plot(model_pgk, type = 'score',  main = "PGK SC Plot")
plot(model_m, type = 'score',  main = "M SC Plot")
plot(model_is, type = 'score',  main = "IS SC Plot")

# ==========================
# 5. SKOR ABILITAS PESERTA (THETA)
# ==========================
theta_pg <- fscores(model_pg)
theta_pgk <- fscores(model_pgk)
theta_m <- fscores(model_m)
theta_is <- fscores(model_is)

# Gabungkan seluruh skor dalam satu tabel
theta_all <- data.frame(
  Responden = read_excel("Dataset.xlsx", sheet = "PG")$`...1`,
  PG = theta_pg[,1],
  PGK = theta_pgk[,1],
  M = theta_m[,1],
  IS = theta_is[,1]
)
head(theta_all)
writexl::write_xlsx(theta_all,"All Theta.xlsx")
