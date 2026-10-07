# ============================================================
# 產業分析實作：完整 RStudio 程式
# 依原 R Console 成功執行紀錄整理
#
# 建議資料夾結構：
# 產業分析實作/
# ├── 產業分析實作_完整R程式.R
# ├── SRDA.sav
# └── traffic_data/
#     ├── accident_01.csv
#     ├── accident_02.csv
#     ├── ...
#     └── accident_12.csv
# ============================================================


# ============================================================
# 0. 套件
# ============================================================

required_packages <- c("readr", "dplyr", "ggplot2", "haven")

new_packages <- required_packages[
  !(required_packages %in% rownames(installed.packages()))
]

if (length(new_packages) > 0) {
  install.packages(new_packages, repos = "https://cloud.r-project.org")
}

library(readr)
library(dplyr)
library(ggplot2)
library(haven)


# ============================================================
# 0-1. macOS 中文繪圖字型
# ============================================================

if (Sys.info()[["sysname"]] == "Darwin") {
  quartzFonts(
    Chinese = quartzFont(rep("Heiti TC", 4))
  )
  plot_family <- "Chinese"
} else {
  plot_family <- ""
}


# ============================================================
# 第一部分：112 年 A2 交通事故資料
# ============================================================


# ============================================================
# 1. 找出 12 個月 CSV
# ============================================================

traffic_folder <- if (dir.exists("traffic_data")) {
  "traffic_data"
} else {
  "."
}

files <- list.files(
  path = traffic_folder,
  pattern = "^accident_[0-9]+\\.csv$",
  full.names = TRUE
)

# 依檔名中的月份排序
month_no <- as.integer(
  sub("accident_([0-9]+)\\.csv", "\\1", basename(files))
)

files <- files[order(month_no)]

cat("找到交通事故 CSV：", length(files), "個\n")
print(basename(files))

if (length(files) == 0) {
  warning("找不到 accident_01.csv ~ accident_12.csv。")
}


# ============================================================
# 2. 讀取並合併全年交通事故資料
# ============================================================

if (length(files) > 0) {

  data_list <- lapply(seq_along(files), function(i) {

    x <- read_csv(
      files[i],
      show_col_types = FALSE
    )

    # 使用英文 month，避免中文 locale 問題
    x$month <- as.integer(
      sub(
        "accident_([0-9]+)\\.csv",
        "\\1",
        basename(files[i])
      )
    )

    return(x)
  })

  accident_112 <- bind_rows(data_list)

  cat("\n資料維度：\n")
  print(dim(accident_112))

  cat("\n總筆數：", nrow(accident_112), "\n")
  cat("總欄位數：", ncol(accident_112), "\n")

  cat("\n各月份筆數：\n")
  print(table(accident_112$month))


  # ==========================================================
  # 3. 每月事故紀錄數
  # ==========================================================

  month_count <- as.data.frame(
    table(accident_112$month)
  )

  names(month_count) <- c("month", "count")

  month_count$month <- as.integer(
    as.character(month_count$month)
  )

  cat("\n每月事故紀錄數：\n")
  print(month_count)


  # ==========================================================
  # 4. 每月事故紀錄數敘述統計
  # ==========================================================

  cat("\n每月事故紀錄數 summary：\n")
  print(summary(month_count$count))

  cat("\n平均數：", mean(month_count$count), "\n")
  cat("中位數：", median(month_count$count), "\n")
  cat("標準差：", sd(month_count$count), "\n")
  cat("最小值：", min(month_count$count), "\n")
  cat("最大值：", max(month_count$count), "\n")


  # ==========================================================
  # 5. 各月份每日平均事故紀錄數
  # ==========================================================

  # 2023 年各月天數
  days <- c(
    31, 28, 31, 30,
    31, 30, 31, 31,
    30, 31, 30, 31
  )

  month_count$days <- days[month_count$month]

  month_count$daily_avg <-
    month_count$count / month_count$days

  cat("\n各月份每日平均事故紀錄數：\n")
  print(month_count)


  # ==========================================================
  # 6. 交通事故圖 1：各月事故紀錄數
  # ==========================================================

  p_accident_1 <- ggplot(
    month_count,
    aes(x = factor(month), y = count)
  ) +
    geom_col() +
    geom_text(
      aes(label = count),
      vjust = -0.3,
      size = 3.5,
      family = plot_family
    ) +
    labs(
      title = "2023 年 A2 交通事故各月紀錄數",
      x = "月份",
      y = "事故紀錄數"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      text = element_text(family = plot_family),
      plot.title = element_text(
        family = plot_family,
        face = "bold"
      )
    )

  print(p_accident_1)


  # ==========================================================
  # 7. 交通事故圖 2：各月每日平均事故紀錄數
  # ==========================================================

  p_accident_2 <- ggplot(
    month_count,
    aes(x = factor(month), y = daily_avg)
  ) +
    geom_col() +
    geom_text(
      aes(label = round(daily_avg, 1)),
      vjust = -0.3,
      size = 3.5,
      family = plot_family
    ) +
    labs(
      title = "2023 年各月每日平均 A2 交通事故紀錄數",
      x = "月份",
      y = "每日平均事故紀錄數"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      text = element_text(family = plot_family),
      plot.title = element_text(
        family = plot_family,
        face = "bold"
      )
    )

  print(p_accident_2)


  # ==========================================================
  # 8. 缺失值概況
  # ==========================================================

  missing <- data.frame(
    variable = seq_len(ncol(accident_112)),
    missing_n = colSums(is.na(accident_112))
  )

  missing$missing_pct <-
    missing$missing_n / nrow(accident_112) * 100

  missing <- missing[
    order(-missing$missing_pct),
  ]

  cat("\n缺失比例最高的前 15 個欄位：\n")
  print(head(missing, 15))


  # ==========================================================
  # 9. 儲存全年合併資料
  # ==========================================================

  saveRDS(
    accident_112,
    "accident_112.rds"
  )
}


# ============================================================
# 第二部分：SRDA 資料
# 主題：居住狀態 × 家庭聚會頻率
# ============================================================


# ============================================================
# 10. 讀取 SRDA.sav
# ============================================================

if (!file.exists("SRDA.sav")) {
  stop("找不到 SRDA.sav，請將檔案放在 RStudio Project 資料夾。")
}

srda <- read_sav("SRDA.sav")

cat("\nSRDA 樣本數：", nrow(srda), "\n")
cat("SRDA 變數數：", ncol(srda), "\n")


# ============================================================
# 11. 變數標籤
# ============================================================

labels <- sapply(srda, function(x) {

  lab <- attr(x, "label")

  if (is.null(lab)) {
    ""
  } else {
    as.character(lab)
  }
})

result <- data.frame(
  variable = names(srda),
  label = labels,
  stringsAsFactors = FALSE
)

cat("\n本分析使用的 SRDA 變數：\n")
print(
  result[
    result$variable %in% c("g06a", "j01d"),
  ]
)


# ============================================================
# 12. 建立分析資料
# ============================================================

df <- data.frame(
  live = as.numeric(srda$g06a),
  gathering = as.numeric(srda$j01d)
)

# g06a：
# 0~70 為有效的同住人數。
# 0 表示除受訪者本人外沒有其他同住者，因此定義為「獨居」。
#
# j01d：
# 1 = 從未
# 2 = 偶爾
# 3 = 經常
#
# 不知道、拒答、遺漏值不納入分析。

df2 <- subset(
  df,
  live >= 0 & live <= 70 &
    gathering %in% c(1, 2, 3)
)

df2$live_group <- ifelse(
  df2$live == 0,
  "獨居",
  "同住"
)

df2$live_group <- factor(
  df2$live_group,
  levels = c("獨居", "同住")
)

df2$gathering <- factor(
  df2$gathering,
  levels = c(1, 2, 3),
  labels = c("從未", "偶爾", "經常")
)

cat("\nSRDA 有效分析樣本數：", nrow(df2), "\n")


# ============================================================
# 13. 交叉表
# ============================================================

tab_new <- table(
  "居住狀態" = df2$live_group,
  "家庭聚會頻率" = df2$gathering
)

cat("\n交叉表：\n")
print(tab_new)


# ============================================================
# 14. 列百分比
# ============================================================

row_percent <- round(
  prop.table(tab_new, margin = 1) * 100,
  2
)

cat("\n列百分比：\n")
print(row_percent)


# ============================================================
# 15. Pearson 卡方獨立性檢定
# ============================================================

chi_new <- suppressWarnings(
  chisq.test(tab_new)
)

cat("\nPearson 卡方檢定：\n")
print(chi_new)

cat("\n預期次數：\n")
print(chi_new$expected)


# ============================================================
# 16. Monte Carlo 模擬卡方檢定
# ============================================================

# 因為部分儲格的預期次數低於 5，
# 另以 Monte Carlo 模擬法估計 p 值。
set.seed(123)

chi_sim <- chisq.test(
  tab_new,
  simulate.p.value = TRUE,
  B = 10000
)

cat("\nMonte Carlo 卡方檢定：\n")
print(chi_sim)


# ============================================================
# 17. SRDA 圖 1：分組長條圖
# ============================================================

p1 <- ggplot(
  df2,
  aes(
    x = live_group,
    fill = gathering
  )
) +
  geom_bar(
    position = position_dodge(width = 0.9),
    width = 0.8
  ) +
  stat_count(
    aes(label = after_stat(count)),
    geom = "text",
    position = position_dodge(width = 0.9),
    vjust = -0.4,
    size = 5,
    family = plot_family
  ) +
  labs(
    title = "居住狀態與家庭聚會頻率",
    x = "居住狀態",
    y = "人數",
    fill = "家庭聚會頻率"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = plot_family),
    plot.title = element_text(
      family = plot_family,
      size = 18,
      face = "bold"
    )
  )

print(p1)


# ============================================================
# 18. SRDA 圖 2：家庭聚會頻率圓餅圖
# ============================================================

pie_data <- as.data.frame(
  table(df2$gathering)
)

names(pie_data) <- c(
  "家庭聚會頻率",
  "人數"
)

pie_data$百分比 <-
  pie_data$人數 / sum(pie_data$人數) * 100

pie_data$標籤 <- paste0(
  pie_data$家庭聚會頻率,
  "\n",
  pie_data$人數,
  "人\n",
  round(pie_data$百分比, 1),
  "%"
)

p2 <- ggplot(
  pie_data,
  aes(
    x = "",
    y = 人數,
    fill = 家庭聚會頻率
  )
) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = 標籤),
    position = position_stack(vjust = 0.5),
    family = plot_family,
    size = 4.5
  ) +
  labs(
    title = "家庭聚會頻率分布",
    subtitle = "有效樣本之家庭聚會參與情形",
    fill = "家庭聚會頻率"
  ) +
  theme_void() +
  theme(
    text = element_text(family = plot_family),
    plot.title = element_text(
      family = plot_family,
      size = 18,
      face = "bold"
    ),
    plot.subtitle = element_text(
      family = plot_family,
      size = 13
    )
  )

print(p2)


# ============================================================
# 19. SRDA 圖 3：折線圖
# ============================================================

line_data <- as.data.frame(
  prop.table(
    table(
      df2$live_group,
      df2$gathering
    ),
    margin = 1
  ) * 100
)

names(line_data) <- c(
  "居住狀態",
  "家庭聚會頻率",
  "百分比"
)

p3 <- ggplot(
  line_data,
  aes(
    x = 家庭聚會頻率,
    y = 百分比,
    group = 居住狀態,
    linetype = 居住狀態,
    shape = 居住狀態
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 4) +
  geom_text(
    aes(
      label = paste0(
        round(百分比, 1),
        "%"
      )
    ),
    vjust = -0.8,
    family = plot_family,
    size = 4
  ) +
  scale_y_continuous(
    limits = c(0, 65),
    breaks = seq(0, 60, 10)
  ) +
  labs(
    title = "居住狀態與家庭聚會頻率",
    subtitle = "比較獨居與同住者的家庭聚會頻率分布",
    x = "家庭聚會頻率",
    y = "百分比（%）",
    linetype = "居住狀態",
    shape = "居住狀態"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = plot_family),
    plot.title = element_text(
      family = plot_family,
      size = 18,
      face = "bold"
    ),
    plot.subtitle = element_text(
      family = plot_family,
      size = 13
    )
  )

print(p3)


# ============================================================
# 20. SRDA 圖 4：100% 堆疊比例圖
# ============================================================

plot4_data <- as.data.frame(
  prop.table(
    table(
      df2$live_group,
      df2$gathering
    ),
    margin = 1
  ) * 100
)

names(plot4_data) <- c(
  "居住狀態",
  "聚會頻率",
  "百分比"
)

p4 <- ggplot(
  plot4_data,
  aes(
    x = 居住狀態,
    y = 百分比,
    fill = 聚會頻率
  )
) +
  geom_col(width = 0.7) +
  geom_text(
    aes(
      label = ifelse(
        百分比 == 0,
        "",
        paste0(round(百分比, 1), "%")
      )
    ),
    position = position_stack(vjust = 0.5),
    family = plot_family,
    size = 5
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20)
  ) +
  labs(
    title = "不同居住狀態的家庭聚會頻率比例",
    x = "居住狀態",
    y = "百分比（%）",
    fill = "家庭聚會頻率"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = plot_family),
    plot.title = element_text(
      family = plot_family,
      size = 18,
      face = "bold"
    )
  )

print(p4)


# ============================================================
# 21. 最終統計摘要
# ============================================================

cat("\n")
cat("============================================\n")
cat("SRDA 最終分析摘要\n")
cat("============================================\n")

cat(
  "有效樣本數：",
  nrow(df2),
  "\n"
)

cat(
  "Pearson X-squared：",
  round(unname(chi_new$statistic), 4),
  "\n"
)

cat(
  "Pearson p-value：",
  round(chi_new$p.value, 4),
  "\n"
)

cat(
  "Monte Carlo p-value：",
  round(chi_sim$p.value, 4),
  "\n"
)

if (chi_sim$p.value < 0.05) {

  cat(
    "結論：拒絕虛無假設，居住狀態與家庭聚會頻率具有顯著關聯。\n"
  )

} else {

  cat(
    "結論：不拒絕虛無假設，目前沒有足夠統計證據認為居住狀態與家庭聚會頻率具有顯著關聯。\n"
  )
}


# ============================================================
# 22. 儲存圖表（需要交作業時可直接使用）
# ============================================================

if (!dir.exists("output")) {
  dir.create("output")
}

if (exists("p_accident_1")) {
  ggsave(
    "output/交通事故_各月紀錄數.png",
    p_accident_1,
    width = 9,
    height = 6,
    dpi = 300
  )
}

if (exists("p_accident_2")) {
  ggsave(
    "output/交通事故_每日平均紀錄數.png",
    p_accident_2,
    width = 9,
    height = 6,
    dpi = 300
  )
}

ggsave(
  "output/SRDA_分組長條圖.png",
  p1,
  width = 9,
  height = 6,
  dpi = 300
)

ggsave(
  "output/SRDA_家庭聚會頻率圓餅圖.png",
  p2,
  width = 8,
  height = 7,
  dpi = 300
)

ggsave(
  "output/SRDA_居住狀態折線圖.png",
  p3,
  width = 9,
  height = 6,
  dpi = 300
)

ggsave(
  "output/SRDA_100百分比堆疊圖.png",
  p4,
  width = 9,
  height = 6,
  dpi = 300
)

cat("\n程式執行完成。圖表已輸出至 output 資料夾。\n")
