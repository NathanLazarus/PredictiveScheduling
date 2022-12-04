pacman::p_load(openxlsx, zoo, data.table, Hmisc, ggplot2, cowplot, foreach, estimatr, iterators, gtools, xtable)

data = data.table(read.xlsx("OregonScheduling.xlsx", sheet = 2, startRow = 2))

setnames(data, 4:7, c("n_firms", "worker_avg_wages", "worker_avg_hours", "worker_avg_retention"))

# setnames(data, c(names(data)[1:3], "n_firms, worker_avg_wages, worker_avg_hours, worker_avg_retention", names(data)[8:length(names(data))]))


data[, treated_industry := Industry %in% c("44-45", "72")]

data[, good_control_industry := Industry %in% c("42", "48-49", "62", "71")]

data[, Quarter := as.yearqtr(gsub("([0-9]{4})([0-9])", "\\1 Q\\2", Quarter))]

data[, treated_quarter := Quarter >= as.yearqtr("2018 Q3")]

# Can be compositional issues in aggregating across industry

varcols = c("Wages", "Hours", "Retention", "Hours.Variance")
diff_data =
  setnames(
    data[
      treated_industry == TRUE,
      lapply(.SD, wtd.mean, n_firms),
      .SDcols = varcols,
      by = "Quarter"
      ],
    c("Quarter", paste0("treated_", varcols))
  )[
    setnames(
      data[
        good_control_industry == TRUE,
        lapply(.SD, wtd.mean, n_firms),
        .SDcols = varcols,
        by = "Quarter"
        ],
      c("Quarter", paste0("good_control_", varcols))
    ),
    on = .(Quarter)
  ][
    setnames(
      data[
        treated_industry == FALSE,
        lapply(.SD, wtd.mean, n_firms),
        .SDcols = varcols,
        by = "Quarter"
        ],
      c("Quarter", paste0("all_control_", varcols))
    ),
    on = .(Quarter)
  ]

long_diffs = melt(diff_data, measure = patterns("^treated_", "^good_control_", "^all_control_"), id.vars = "Quarter", value.name = c("treated", "good_control", "all_control"))

setattr(long_diffs$variable, "levels", varcols)

base_vals =
  long_diffs[
    Quarter == as.yearqtr("2018 Q2")
  ][,
  `:=`(base_diff_good_control = treated - good_control,
       base_diff_all_control = treated - all_control)
  ]

long_diffs[
  base_vals,
  on = .(variable),
  `:=`(
    base_diff_good_control = i.base_diff_good_control,
    base_diff_all_control = i.base_diff_all_control
  )]

long_diffs[, diff_good_control := treated - good_control - base_diff_good_control]
long_diffs[, diff_all_control := treated - all_control - base_diff_all_control]
long_diffs[, treated_quarter := Quarter >= as.yearqtr("2018 Q3")]

to_plot = rbind(
  setnames(long_diffs[, .SD, .SDcols = !"diff_good_control"], "diff_all_control", "diff_control")[, diff_type := "all_control"],
  setnames(long_diffs[, .SD, .SDcols = !"diff_all_control"], "diff_good_control", "diff_control")[, diff_type := "good_control"]
  )

throwaway = foreach(thisvar = varcols) %do% {
 ggplot(to_plot[variable == thisvar]) +
    geom_line(aes(x = Quarter, y = diff_control, linetype = treated_quarter, color = diff_type)) +
    # scale_x_continuous(breaks = -3:5) +
    theme_cowplot() +
    xlab("Quarter") +
    ggtitle(thisvar) +
    theme(
      axis.title.y = element_blank(),
      plot.title = element_text(hjust = 0)
    )
  ggsave(paste0(thisvar, "DiD.pdf"))
}


to_plot_raw = rbind(
  setnames(long_diffs[, .(Quarter, treated_quarter, variable, treated)], "treated", "raw_value")[, group := "treated"],
  setnames(long_diffs[, .(Quarter, treated_quarter, variable, good_control)], "good_control", "raw_value")[, group := "good_control"],
  setnames(long_diffs[, .(Quarter, treated_quarter, variable, all_control)], "all_control", "raw_value")[, group := "all_control"]
  )

throwaway = foreach(thisvar = varcols) %do% {
 ggplot(to_plot_raw[variable == thisvar]) +
    geom_line(aes(x = Quarter, y = raw_value, linetype = treated_quarter, color = group)) +
    # scale_x_continuous(breaks = -3:5) +
    theme_cowplot() +
    xlab("Quarter") +
    ggtitle(thisvar) +
    theme(
      axis.title.y = element_blank(),
      plot.title = element_text(hjust = 0)
    )
  ggsave(paste0(thisvar, "raw.pdf"))
}



data[, quarter_of_year := quarter(Quarter)]
data[, year := year(Quarter)]

data[, covid := Quarter >= as.yearqtr("2020 Q1")]

data[, treated := treated_industry * treated_quarter]

summary(lm(Wages ~ factor(Quarter) + Industry + treated, data = data))

summary(lm(Wages ~ factor(Quarter) + Industry + treated, data = data))

data[, in_good_control_sample := (treated_industry == TRUE | good_control_industry == TRUE)]

CJ.dt = function(X,Y) {
  stopifnot(is.data.table(X),is.data.table(Y))
  k = NULL
  X = X[, c(k = 1, .SD)]
  setkey(X, k)
  Y = Y[, c(k = 1, .SD)]
  setkey(Y, NULL)
  X[Y, allow.cartesian = TRUE][, k := NULL][]
}

spec_table = CJ.dt(CJ.dt(data.table(sample_restriction = c(FALSE, TRUE)), data.table(covid = c(FALSE, TRUE))),data.table(var = varcols))

regs = foreach(spec = iter(spec_table, by = "row"), .combine = rbind) %do% {
  reg_formula = as.formula(paste0(spec$var, " ~ factor(Quarter) + Industry + treated"))
  reg = lm_robust(
    reg_formula,
    weights = n_firms,
    data = data[in_good_control_sample >= spec$sample_restriction & covid <= spec$covid]
  )
  p_str = stars.pval(coef(summary(reg))["treated", "Pr(>|t|)"])
  data.table(
    t(coef(summary(reg))["treated", 1:2])
  )[, `:=`(outcome = spec$var, good_control_only = spec$sample_restriction, covid = spec$covid, pval_str = p_str)]
}

regs[, estimate_str := paste0(as.character(signif(regs$Estimate, 4)), pval_str),
   ][, se_str := paste0("(", as.character(signif(regs$`Std. Error`, 3)), ")")]

# dcast(regs, estimate_str + se_str + good_control_only + covid ~ outcome)


reg_table =
  rbind(
    dcast(regs, good_control_only + covid ~ outcome, value.var = "estimate_str")[, value_type := "estimate"],
    dcast(regs, good_control_only + covid ~ outcome, value.var = "se_str")[, value_type := "se"]
  )

setkey(reg_table, good_control_only, covid, value_type)

print(xtable(reg_table[, !"value_type"]), include.rownames = FALSE)