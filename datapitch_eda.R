# EDA for the data pitch. Run from the project root.
# Writes the four figures to Presentations/figures/ and prints every number
# quoted on the slides.

# Reuse data already loaded in the session, otherwise fetch it.
if (!exists("df", envir = globalenv(), inherits = FALSE)) {
  source("data_fetch_PD.R")      # df: X6 = PAY_0, Y = default next month
}
stock_objects <- c("ff", "returns", "tickers", "df_prices", "data")
if (!all(sapply(stock_objects, exists, envir = globalenv(), inherits = FALSE))) {
  source("data_fetch_stocks.R")  # ff, returns, tickers
}

library(ggplot2)
library(dplyr)
library(tidyr)

out_dir <- "Presentations/figures"
dir.create(out_dir, showWarnings = FALSE)

# Fits one half-width column on the 16:9 slides.
fig_width <- 70
fig_height <- 58

ink <- "#0b0b0b"
ink_soft <- "#52514e"
muted <- "#898781"
grid_col <- "#e1e0d9"
base_col <- "#c3c2b7"
blue <- "#2a78d6"

theme_pitch <- function() {
  theme_minimal(base_size = 7.5, base_family = "Helvetica") +
    theme(
      plot.title = element_text(colour = ink, face = "bold", size = 8),
      plot.subtitle = element_text(colour = ink_soft, size = 6.8,
                                   margin = margin(b = 3)),
      plot.title.position = "plot",
      plot.caption = element_text(colour = ink_soft, size = 5.6, hjust = 0),
      plot.caption.position = "plot",
      axis.title = element_blank(),
      axis.text = element_text(colour = ink_soft, size = 6.4),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = grid_col, linewidth = 0.25),
      plot.background = element_blank(),
      plot.margin = margin(2, 8, 2, 2)
    )
}

save_fig <- function(plot, name) {
  ggsave(file.path(out_dir, paste0(name, ".pdf")), plot, device = "pdf",
         width = fig_width, height = fig_height, units = "mm")
}

signed_pct <- function(x) sprintf("%+.0f%%", x * 100)

# ---- Idea A, plot 1: cumulative Fama-French factor returns ----

# The slide's data card defines the codes. The risk-free rate is the benchmark,
# not a factor, so it is drawn in grey underneath the three factors.
factor_codes <- c("Mkt-RF", "SMB", "HML", "RF")
factor_cols <- setNames(c(blue, "#eb6834", "#1baf7a", muted), factor_codes)

ff_cum <- ff |>
  select(Date, all_of(factor_codes)) |>
  pivot_longer(-Date, names_to = "factor", values_to = "ret") |>
  mutate(factor = factor(factor, levels = factor_codes)) |>
  arrange(Date) |>
  group_by(factor) |>
  mutate(growth = cumprod(1 + ret)) |>
  ungroup()

ff_end <- ff_cum |> filter(Date == max(Date))

covid <- ff_cum |>
  filter(factor == "Mkt-RF") |>
  slice_min(growth, n = 1)

p_factors <- ggplot(ff_cum, aes(Date, growth, colour = factor)) +
  geom_hline(yintercept = 1, colour = base_col, linewidth = 0.35) +
  geom_line(data = filter(ff_cum, factor == "RF"), linewidth = 0.45,
            lineend = "round") +
  geom_line(data = filter(ff_cum, factor != "RF"), linewidth = 0.32,
            lineend = "round", linejoin = "round") +
  geom_point(data = ff_end, size = 1, show.legend = FALSE) +
  geom_text(data = ff_end, aes(label = signed_pct(growth - 1)), colour = ink,
            hjust = 0, nudge_x = 50, size = 2.35, family = "Helvetica",
            show.legend = FALSE) +
  annotate("text", x = covid$Date, y = 0.605, label = "COVID crash, Mar 2020",
           colour = ink_soft, size = 2.05, hjust = 0, family = "Helvetica") +
  scale_colour_manual(values = factor_cols, breaks = factor_codes, name = NULL) +
  scale_y_log10(breaks = c(0.8, 1, 1.5, 2),
                labels = function(b) ifelse(b == 1, "0%", signed_pct(b - 1))) +
  scale_x_date(breaks = as.Date(paste0(seq(2020, 2026, 2), "-01-01")),
               date_labels = "%Y", expand = expansion(mult = c(0.01, 0.005))) +
  coord_cartesian(ylim = c(0.59, 2.35), clip = "off") +
  labs(title = "Market compounds, size and value swing",
       subtitle = "Cumulative factor returns, 2020 to Sep 2026, log scale") +
  theme_pitch() +
  theme(legend.position = "top", legend.justification = "left",
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(0, 0, -6, -6),
        legend.key.width = unit(9, "pt"),
        legend.key.height = unit(6, "pt"),
        legend.text = element_text(colour = ink, size = 6.2,
                                   margin = margin(l = 1.5, r = 3)),
        panel.grid.major.x = element_blank(),
        plot.margin = margin(2, 22, 2, 2))

save_fig(p_factors, "fig_ff_cumulative")

# ---- Idea A, plot 2: fat tails of daily stock returns ----

stocks <- returns[, unique(tickers)]  # DIS appears twice in the ticker list
z <- scale(as.matrix(stocks))
n_days <- nrow(z)
normal_q <- qnorm(ppoints(n_days))
keep <- abs(normal_q) > 2 | seq_len(n_days) %% 8 == 0  # thin the centre

qq <- tibble(
  stock = rep(colnames(z), each = sum(keep)),
  normal = rep(normal_q[keep], ncol(z)),
  sample = as.vector(apply(z, 2, sort)[keep, ])
)

tail_obs <- mean(abs(z) > 4)
tail_norm <- 2 * pnorm(-4)
tail_note <- sprintf(paste0("Beyond 4 SD: %.2f%% of stock-days\n",
                            "Normal distribution: %.4f%% (%.0fx fewer)"),
                     tail_obs * 100, tail_norm * 100, tail_obs / tail_norm)

p_tails <- ggplot(qq, aes(normal, sample, group = stock)) +
  geom_hline(yintercept = 0, colour = base_col, linewidth = 0.3) +
  geom_line(colour = muted, alpha = 0.35, linewidth = 0.25) +
  geom_abline(slope = 1, intercept = 0, colour = blue, linewidth = 0.55) +
  annotate("text", x = 3.45, y = 1.7, label = "Normal", colour = ink,
           size = 2.2, hjust = 1, family = "Helvetica") +
  annotate("text", x = -3.5, y = 13.5, label = tail_note, colour = ink,
           size = 2.05, hjust = 0, vjust = 1, lineheight = 0.95,
           family = "Helvetica") +
  scale_x_continuous(breaks = -3:3, limits = c(-3.5, 3.5)) +
  scale_y_continuous(breaks = seq(-15, 15, 5)) +
  labs(title = "Daily returns have fat tails",
       subtitle = "Normal QQ plot, one grey line per stock (99)",
       x = "Normal quantile", y = "Standardised return (SD)") +
  theme_pitch() +
  theme(axis.title = element_text(colour = ink_soft, size = 6.2),
        panel.grid.major = element_line(colour = grid_col, linewidth = 0.2))

save_fig(p_tails, "fig_stock_tails")

# ---- Idea B, plot 1: default rate and class imbalance ----

status_levels <- c("Not late", "1 month late", "2 months late",
                   "3+ months late")

by_status <- df |>
  mutate(status = cut(X6, c(-Inf, 0, 1, 2, Inf), labels = status_levels)) |>
  group_by(status) |>
  summarise(n = n(), defaults = sum(Y), .groups = "drop") |>
  mutate(status = as.character(status))

rates <- bind_rows(
  tibble(status = "All clients", n = nrow(df), defaults = sum(df$Y)),
  by_status
) |>
  rowwise() |>
  mutate(rate = defaults / n,
         # prop.test without continuity correction gives the Wilson interval.
         lo = prop.test(defaults, n, correct = FALSE)$conf.int[1],
         hi = prop.test(defaults, n, correct = FALSE)$conf.int[2]) |>
  ungroup() |>
  mutate(row = sprintf("%s\nn = %s", status,
                       format(n, big.mark = ",", trim = TRUE)),
         row = factor(row, levels = rev(row)),
         benchmark = status == "All clients")

overall <- rates$rate[rates$benchmark]

p_default <- ggplot(rates, aes(rate, row, fill = benchmark)) +
  geom_vline(xintercept = 0, colour = base_col, linewidth = 0.35) +
  geom_vline(xintercept = overall, colour = ink_soft, linewidth = 0.3) +
  geom_col(width = 0.5) +
  geom_linerange(aes(xmin = lo, xmax = hi), colour = ink, linewidth = 0.35) +
  geom_label(aes(x = hi, label = sprintf("%.1f%%", rate * 100)), hjust = 0,
             nudge_x = 0.012, colour = ink, fill = "white", size = 2.35,
             family = "Helvetica", label.size = NA,
             label.padding = unit(0.6, "pt")) +
  scale_fill_manual(values = c(`TRUE` = muted, `FALSE` = blue),
                    guide = "none") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25),
                     labels = function(b) paste0(b * 100, "%"),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(title = "22% default; risk jumps once clients are late",
       subtitle = "Share defaulting next month, by repayment status",
       caption = paste0("Status in Sep 2005 (PAY_0); codes up to 0 count as ",
                        "not late.\nWhiskers: 95% Wilson interval.")) +
  theme_pitch() +
  theme(panel.grid.major = element_blank(),
        axis.text.y = element_text(colour = ink, size = 6.1, lineheight = 0.9))

save_fig(p_default, "fig_default_rate")

# ---- Idea B, plot 2: collinearity of the monthly bill amounts ----

# One dot per client: September bill (X12, BILL_AMT1) against August (X13).
bills <- tibble(aug = df$X13 / 1000, sep = df$X12 / 1000)
axis_max <- 500
axis_min <- -20
n_hidden <- sum(bills$aug > axis_max | bills$sep > axis_max |
                bills$aug < axis_min | bills$sep < axis_min)

bill_cor <- cor(df[, paste0("X", 12:17)])
bill_note <- sprintf("r = %.2f\nAll six bill months: r = %.2f to %.2f",
                     cor(bills$aug, bills$sep),
                     min(bill_cor[upper.tri(bill_cor)]),
                     max(bill_cor[upper.tri(bill_cor)]))

p_collinear <- ggplot(bills, aes(aug, sep)) +
  geom_abline(slope = 1, intercept = 0, colour = ink_soft, linewidth = 0.3) +
  geom_point(colour = blue, alpha = 0.12, size = 0.25, stroke = 0) +
  annotate("text", x = axis_min, y = axis_max, label = bill_note, colour = ink,
           size = 2.2, hjust = 0, vjust = 1, lineheight = 0.95,
           family = "Helvetica") +
  scale_x_continuous(breaks = seq(0, 500, 100)) +
  scale_y_continuous(breaks = seq(0, 500, 100)) +
  coord_cartesian(xlim = c(axis_min, axis_max), ylim = c(axis_min, axis_max)) +
  labs(title = "Monthly bill amounts are nearly collinear",
       subtitle = "Sep vs Aug bill, one dot per client; line: same amount",
       x = "August bill (NT$ 1,000)", y = "September bill (NT$ 1,000)",
       caption = sprintf("Axes cut at NT$ 500k; %d of 30,000 clients lie outside.",
                         n_hidden)) +
  theme_pitch() +
  theme(axis.title = element_text(colour = ink_soft, size = 6.2),
        panel.grid.major = element_line(colour = grid_col, linewidth = 0.2))

save_fig(p_collinear, "fig_history_collinearity")

# ---- Numbers quoted on the slides ----

pct <- function(x, digits = 1) sprintf(paste0("%.", digits, "f%%"), 100 * x)
factors <- c("Mkt-RF", "SMB", "HML")

cat("== Idea A: Fama-French factors and stock prices ==\n")
cat("Factor window:", format(range(ff$Date)), "| trading days:", nrow(ff), "\n")
cat("Price window:", format(range(df_prices$Date)), "| days:", nrow(df_prices), "\n")
cat("Merged panel:", format(range(data$Date)), "| days:", nrow(data), "\n")

stocks <- returns[, unique(tickers)]
cat("Distinct stocks:", ncol(stocks), "| ticker list entries:", length(tickers),
    "| duplicated:", tickers[duplicated(tickers)], "\n")

growth <- sapply(ff[factors], function(x) cumprod(1 + x))
cat("Cumulative factor returns:",
    paste(factors, pct(growth[nrow(growth), ] - 1, 0), collapse = ", "), "\n")
cat("Cumulative risk-free return (RF):", pct(prod(1 + ff$RF) - 1, 0), "\n")
cat("Daily returns per stock:", nrow(stocks), "\n")
hml_low <- which.min(growth[, "HML"])
cat("HML low:", format(ff$Date[hml_low]), pct(growth[hml_low, "HML"] - 1),
    "| gain since:", pct(growth[nrow(growth), "HML"] / growth[hml_low, "HML"] - 1), "\n")
worst <- which.min(ff$`Mkt-RF`)
cat("Worst market day:", format(ff$Date[worst]), pct(ff$`Mkt-RF`[worst]), "\n")
t_stat <- sapply(ff[factors], function(x) mean(x) / (sd(x) / sqrt(length(x))))
cat("t-statistics of mean daily factor returns:",
    paste(factors, round(t_stat, 2), collapse = ", "), "\n")

moments <- function(x) {
  d <- x - mean(x)
  c(skew = mean(d^3) / mean(d^2)^1.5, kurt = mean(d^4) / mean(d^2)^2)
}
m <- sapply(stocks, moments)
jarque_bera_p <- pchisq(nrow(stocks) / 6 * (m["skew", ]^2 + (m["kurt", ] - 3)^2 / 4),
                        df = 2, lower.tail = FALSE)
cat("Kurtosis across stocks: median", round(median(m["kurt", ]), 1),
    "| range", round(range(m["kurt", ]), 1), "\n")
cat("Stocks rejecting normality (Jarque-Bera, 5%):", sum(jarque_bera_p < 0.05),
    "of", ncol(stocks), "\n")
z <- scale(as.matrix(stocks))
cat("Stock-days beyond 4 SD:", pct(mean(abs(z) > 4), 2),
    "| under normality:", pct(2 * pnorm(-4), 4), "\n")

cat("\n== Idea B: credit-card default (X6 = PAY_0, Y = default) ==\n")
cat("Clients:", nrow(df), "| missing values:", sum(is.na(df)),
    "| duplicate rows ignoring ID:", sum(duplicated(df[, -1])), "\n")
cat("Defaults:", sum(df$Y), pct(mean(df$Y)), "\n")
late <- df$X6 >= 1
cat("Clients at least one month late:", pct(mean(late)),
    "| their share of all defaults:", pct(sum(df$Y[late]) / sum(df$Y)), "\n")
cat("PAY_0 codes -2 or 0 (undocumented):", pct(mean(df$X6 %in% c(-2, 0))), "\n")
cat("EDUCATION codes 0, 5, 6:", sum(df$X3 %in% c(0, 5, 6)),
    "| MARRIAGE code 0:", sum(df$X4 == 0), "\n")
cat("Default rate by sex (1 = male, 2 = female):",
    paste(names(tapply(df$Y, df$X2, mean)), pct(tapply(df$Y, df$X2, mean)),
          collapse = ", "), "\n")

families <- list(repayment_status = paste0("X", 6:11),
                 bill_amount = paste0("X", 12:17),
                 amount_paid = paste0("X", 18:23))
for (f in names(families)) {
  r <- cor(df[, families[[f]]])
  cat(sprintf("Pairwise correlations, %s: %.2f to %.2f\n", f,
              min(r[upper.tri(r)]), max(r[upper.tri(r)])))
}
