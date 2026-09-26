# ################################################################################################ #
# ## Combine Costs Then Generate Cost/Utilization Table and Comparison Figure ## ----------------- #
# ################################################################################################ #

# Requires, from the prior scripts (run in order), plus the main script:

#   df.clinical                             (main script, requires any data-cleaning to fit format)
#   df.PFS_costs_nonanes, df.PFS_costs_anes (01, 02)
#   df.facility_cost_index                  (03)
#   df.readmit_cost_by_window               (04)

# Restricts to the 0-90 day post-operative window and produces:
#   (1) df.total_cost_90       - one row per patient, all cost components combined
#   (2) tbl_costs              - descriptive + adjusted-mean cost/utilization table
#   (3) fig_2panel             - Panel A (stacked mean cost components) + Panel B (total cost
#                                 box-and-whisker, with a toggle for log scale), Robotic vs Open

WINDOW_LOWER <- 0
WINDOW_UPPER <- 90
flag.save.figure <- TRUE

# ------------------------------------------------------------------------------------------------ #
# -- (1) Combine cost components ----------------------------------------------------------------- #
# ------------------------------------------------------------------------------------------------ #

# professional fees (non-anesthesia + anesthesia), summed within the follow-up window
df.professional_cost_90 <- bind_rows(df.PFS_costs_nonanes, df.PFS_costs_anes) %>%
    filter(days_from_surgery >= WINDOW_LOWER, days_from_surgery <= WINDOW_UPPER) %>%
    group_by(PMRN) %>%
    summarize(
        Anesthesia_cost_2025usd = sum(payment_2025usd[cost_component == "Anesthesia"],
                                      na.rm = TRUE),
        OtherCPT_cost_2025usd   = sum(payment_2025usd[cost_component == "Other CPT"],
                                      na.rm = TRUE),
        .groups = "drop") %>%
    mutate(Total_PFS_cost_2025usd = Anesthesia_cost_2025usd + OtherCPT_cost_2025usd)

# facility (index admission is window-invariant; readmissions are window-specific)
df.facility_cost_90 <- df.facility_cost_index %>%
    select(PMRN, Facility_Payment_2025) %>%
    left_join(df.readmit_cost_by_window %>%
                  filter(window == "0 to 90d post-op") %>%
                  select(PMRN, Readmit_Facility_2025),
              by = "PMRN") %>%
    mutate(Readmit_Facility_2025  = coalesce(Readmit_Facility_2025, 0),
           Facility_Total_2025usd = Facility_Payment_2025 + Readmit_Facility_2025)

# EDIT: list the clinical covariates you want carried through (must include Robotic or comparison
# variable as well as any covariates you'll adjust for in the GLM below)

clinical_covariates <- c("Robotic", "Age", "Race.White", "Post_Chemo")

df.total_cost_90 <- df.clinical %>%
    distinct(PMRN, across(all_of(clinical_covariates))) %>%
    left_join(df.professional_cost_90, by = "PMRN") %>%
    left_join(df.facility_cost_90, by = "PMRN") %>%
    mutate(across(c(Anesthesia_cost_2025usd, OtherCPT_cost_2025usd, Total_PFS_cost_2025usd,
                    Facility_Payment_2025, Readmit_Facility_2025, Facility_Total_2025usd),
                  ~ coalesce(., 0)),
           Total_Cost_2025usd = Total_PFS_cost_2025usd + Facility_Total_2025usd)

stopifnot(!any(is.na(df.total_cost_90$Total_Cost_2025usd)))

# ------------------------------------------------------------------------------------------------ #
# -- (2) Table of descriptive costs + adjusted (GLM) cost estimates ------------------------------ #
# ------------------------------------------------------------------------------------------------ #

fmt_p2 <- function(p) if (is.na(p)) "-" else if (p < 0.001) "<0.001" else sprintf("%.3f", p)
money  <- function(x) paste0("$", formatC(x, format = "f", big.mark = ",", digits = 0))

# ------------------------------------------------------------------------------------------------ #
# -- Adjusted total cost: Gamma GLM, log link ---------------------------------------------------- #
# ------------------------------------------------------------------------------------------------ #
# Gamma-with-log-link is appropriate for cost data which is anticipated to be strictly positive,
# right-skewed distribution.
# It handles the skew without the re-transformation bias of log-OLS, and the log link makes the
# treatment effect a multiplicative cost ratio. Marginal means are "recycled predictions" - average
# predictions over the observed covariate distribution - rather than holding covariates at their
# means (which can describe a patient who doesn't actually exist in the data).

adj_covariates <- c("Age", "Race.White", "Post_Chemo")  # EDIT: must match clinical_covariates above

fit_adj_cost_GLMgammalog <- function(dat, covariates) {
    d <- dat %>%
        select(Robotic, Total_Cost_2025usd, all_of(covariates)) %>%
        filter(!is.na(Robotic), !is.na(Total_Cost_2025usd), Total_Cost_2025usd > 0) %>%
        tidyr::drop_na() %>%
        mutate(Robotic = factor(Robotic, levels = c(0, 1), labels = c("Open", "Robotic")))
    
    fml <- as.formula(paste("Total_Cost_2025usd ~ Robotic +", paste(covariates, collapse = " + ")))
    fit <- glm(fml, family = Gamma(link = "log"), data = d)
    
    emm     <- emmeans(fit, ~ Robotic, type = "response", weights = "proportional")
    emm_df  <- as.data.frame(emm)
    ratio   <- contrast(emm, method = "revpairwise", type = "response")
    ratio_df <- as.data.frame(confint(ratio))
    ratio_p  <- as.data.frame(ratio)$p.value[1]
    
    getv  <- function(df, nms) {
        hit <- intersect(nms, names(df))
        if (length(hit)) df[[hit[1]]] else rep(NA, nrow(df)) }
    est   <- getv(emm_df, c("response", "emmean", "rate"))
    lo    <- getv(emm_df, c("asymp.LCL", "lower.CL"))
    hi    <- getv(emm_df, c("asymp.UCL", "upper.CL"))
    r_est <- getv(ratio_df, c("ratio", "estimate"))
    r_lo  <- getv(ratio_df, c("asymp.LCL", "lower.CL"))
    r_hi  <- getv(ratio_df, c("asymp.UCL", "upper.CL"))
    
    list(fit = fit, n = nrow(d),
         adj_open    = sprintf("%s (%s, %s)",
                               money(est[emm_df$Robotic == "Open"]),
                               money(lo[emm_df$Robotic == "Open"]),
                               money(hi[emm_df$Robotic == "Open"])),
         adj_robotic = sprintf("%s (%s, %s)",
                               money(est[emm_df$Robotic == "Robotic"]),
                               money(lo[emm_df$Robotic == "Robotic"]),
                               money(hi[emm_df$Robotic == "Robotic"])),
         ratio       = sprintf("%.2f (%.2f, %.2f)", r_est[1], r_lo[1], r_hi[1]),
         p           = fmt_p2(ratio_p))
}

adj90 <- fit_adj_cost_GLMgammalog(df.total_cost_90,
                                  adj_covariates)

adj_rows <- tibble(
    Variable = c("Total cost, adjusted mean (95% CI)", "  Cost ratio, robotic vs open (95% CI)"),
    Overall = c("", ""), Open = c(adj90$adj_open, ""), Robotic = c(adj90$adj_robotic, adj90$ratio),
    p = c(adj90$p, ""), Test = c("Gamma GLM (log link)", ""))

# ------------------------------------------------------------------------------------------------ #
# -- Descriptive cost/utilization rows: median [IQR] and mean (SD), by group --------------------- #
# ------------------------------------------------------------------------------------------------ #

# EDIT: list the variables to show in Table, and whether each is continuous ("cont"), binary
# ("bin"), or a dollar amount ("cost" - same handling as "cont", formatted with $ and commas)
vars_tbl_costs <- tibble::tribble(
    ~var,                     ~label,                             ~ type,
    "LOS",                    "Length of stay, days",             "cont",
    "Readmission_calc",       "Readmission, any",                 "bin",
    "Total_Cost_2025usd",     "Total cost (2025 USD)",            "cost",
    "Facility_Total_2025usd", "    Total facility component",     "cost",
    "Total_PFS_cost_2025usd", "    Total professional component", "cost"
)

build_tbl_costs_block <- function(dat) {
    dat <- dat[!is.na(dat$Robotic), ]
    g <- factor(dat$Robotic, levels = c(0, 1), labels = c("Open", "Robotic"))
    
    fmt_val <- function(x, kind, stat) {
        x <- x[!is.na(x)]
        if (length(x) == 0) return("-")
        if (stat == "mean") {
            if (kind == "cost") sprintf("$%s (%s)",
                                        formatC(mean(x), format = "f", big.mark = ",", digits = 0),
                                        formatC(sd(x), format = "f", big.mark = ",", digits = 0))
            else sprintf("%.1f (%.1f)", mean(x), sd(x))
        } else {
            q <- quantile(x, c(.25, .5, .75), na.rm = TRUE)
            if (kind == "cost") sprintf("$%s [%s, %s]",
                                        formatC(q[2], format = "f", big.mark = ",", digits = 0),
                                        formatC(q[1], format = "f", big.mark = ",", digits = 0),
                                        formatC(q[3], format = "f", big.mark = ",", digits = 0))
            else sprintf("%.1f [%.1f, %.1f]", q[2], q[1], q[3])
        }
    }
    
    rows <- list(tibble(Variable = "Number of patients",
                        Overall = as.character(nrow(dat)),
                        Open = as.character(sum(g == "Open")),
                        Robotic = as.character(sum(g == "Robotic")),
                        p = "",
                        Test = ""))
    
    for (i in seq_len(nrow(vars_tbl_costs))) {
        v <- vars_tbl_costs$var[i]; lab <- vars_tbl_costs$label[i]; ty <- vars_tbl_costs$type[i]
        x <- dat[[v]]
        if (ty == "bin") {
            tb <- table(x, g)
            use_fisher <- any(tryCatch(suppressWarnings(chisq.test(tb)$expected),
                                       error = function(e) 0) < 5)
            p <- if (use_fisher) fisher.test(tb)$p.value else suppressWarnings(chisq.test(tb)$p.value)
            pct <- function(idx) {
                n <- sum(x[idx] == 1, na.rm = TRUE)
                d <- sum(!is.na(x[idx]))
                if (d == 0) "-" else sprintf("%d (%.1f%%)", n, 100 * n / d) }
            rows[[length(rows) + 1]] <- tibble(Variable = paste0(lab, ", n (%)"),
                                               Overall = pct(rep(TRUE, length(x))),
                                               Open = pct(g == "Open"),
                                               Robotic = pct(g == "Robotic"),
                                               p = fmt_p2(p),
                                               Test = if (use_fisher) "Fisher exact" else "Chi-squared")
        } else {
            p_w <- suppressWarnings(wilcox.test(x ~ g)$p.value)
            rows[[length(rows) + 1]] <- tibble(Variable = paste0(lab, ", median [IQR]"),
                                               Overall = fmt_val(x, ty, "median"),
                                               Open = fmt_val(x[g == "Open"], ty, "median"),
                                               Robotic = fmt_val(x[g == "Robotic"], ty, "median"),
                                               p = fmt_p2(p_w), Test = "Wilcoxon rank-sum")
        }
    }
    bind_rows(rows)
}

tbl_costs <- build_tbl_costs_block(df.total_cost_90) %>% bind_rows(adj_rows)

if (flag.save.table) {
    write.csv(tbl_costs,
              file.path(f.tables,
                        paste0("Table02_cost_and_utilization_0to90d_", date.analysis, ".csv")),
              row.names = FALSE)
}

# ################################################################################################ #
# -- (3) Figure: Robotic vs Open, 0-90d window, 2-panel ------------------------------------------ #
# ################################################################################################ #

LOG_SCALE_B <- FALSE  # toggle: TRUE = natural-log total cost axis in Panel B

group_colors     <- c("Open"    = "#BC3C29",
                      "Robotic" = "#0072B5")

component_levels <- c("Professional: Anesthesia",
                      "Professional: Non-Anesthesia",
                      "Facility: Readmissions",
                      "Facility: Index admission")

component_colors <- c("Professional: Anesthesia"     = "#FFDC91",
                      "Professional: Non-Anesthesia" = "#E18727",
                      "Facility: Readmissions"      = "#BDBDBD",
                      "Facility: Index admission"    = "#595959")

base_theme <- theme_classic(base_size = 11, base_family = "sans") +
    theme(legend.position = "inside", legend.position.inside = c(1, 1),
          legend.justification = c("right", "top"), legend.title = element_blank(),
          legend.text = element_text(size = rel(0.6)), legend.key.size = unit(0.6, "lines"),
          plot.title = element_text(size = 11, face = "bold"), strip.background = element_blank())

build_panel_a <- function(dat) {
    plot_dat <- dat %>%
        mutate(Group = factor(Robotic, levels = c(0, 1), labels = c("Open", "Robotic"))) %>%
        group_by(Group) %>%
        summarize(`Professional: Anesthesia`     = mean(Anesthesia_cost_2025usd, na.rm = TRUE),
                  `Professional: Non-Anesthesia` = mean(OtherCPT_cost_2025usd, na.rm = TRUE),
                  `Facility: Index admission`    = mean(Facility_Payment_2025, na.rm = TRUE),
                  `Facility: Readmissions`       = mean(Readmit_Facility_2025, na.rm = TRUE),
                  .groups = "drop") %>%
        tidyr::pivot_longer(-Group, names_to = "Component", values_to = "Mean_cost") %>%
        mutate(Component = factor(Component, levels = component_levels))
    
    totals <- plot_dat %>% group_by(Group) %>% summarize(total = sum(Mean_cost), .groups = "drop")
    
    ggplot(plot_dat, aes(x = Group, y = Mean_cost, fill = Component)) +
        geom_col(width = 0.6, color = "white", linewidth = 0.2) +
        geom_text(data = totals, aes(x = Group, y = total, label = dollar(total, accuracy = 1)),
                  inherit.aes = FALSE, vjust = -0.4, size = 3.2, fontface = "bold") +
        scale_fill_manual(values = component_colors, breaks = component_levels) +
        scale_y_continuous(labels = dollar, expand = expansion(mult = c(0, 0.20))) +
        labs(x = NULL, y = "Average cost per patient", title = "A") +
        guides(fill = guide_legend(nrow = 4)) + base_theme +
        coord_cartesian(clip = "off") +
        theme(legend.position.inside = c(1.05, 1.1))
}

build_panel_b <- function(dat, log_scale = FALSE) {
    plot_dat <- dat %>% mutate(Group = factor(Robotic, levels = c(0, 1),
                                              labels = c("Open", "Robotic")))
    p <- ggplot(plot_dat, aes(x = Group, y = Total_Cost_2025usd, fill = Group)) +
        geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.85, linewidth = 0.4) +
        geom_jitter(width = 0.08, size = 1.1, alpha = 0.35, color = "grey20") +
        scale_fill_manual(values = group_colors, guide = "none") +
        labs(x = NULL, y = "Total cost per patient", title = "B") +
        base_theme + theme(legend.position = "none")
    if (log_scale) {
        p + scale_y_continuous(transform = "log", labels = label_scientific(digits = 2)) +
            labs(y = "Log cost per patient, ln(2025 USD)")
    } else {
        p + scale_y_continuous(labels = dollar)
    }
}

build_rplnd_figure_2panel <- function(dat, log_scale_b = LOG_SCALE_B, out_file = flag.save.figure) {
    fig <- (build_panel_a(dat) | build_panel_b(dat, log_scale = log_scale_b)) +
        plot_layout(guides = "keep") +
        plot_annotation(theme = theme(plot.margin = margin(5, 8, 5, 5)))
    
    if (out_file) {
        suffix <- if (log_scale_b) "_log" else ""
        path <- file.path(f.figures,
                          paste0("Figure01_cost_openVrobotic_0to90d", suffix, "_",
                                 date.analysis, ".png"))
        ggsave(path, fig, width = 7, height = 3.25, dpi = 600, units = "in", bg = "white")
        message("Saved: ", path)
    }
    fig
}

fig_2panel     <- build_rplnd_figure_2panel(df.total_cost_90, log_scale_b = FALSE)
fig_2panel_log <- build_rplnd_figure_2panel(df.total_cost_90, log_scale_b = TRUE)

# ------------------------------------------------------------------------------------------------ #
# -- Script End ---------------------------------------------------------------------------------- #
# ------------------------------------------------------------------------------------------------ #