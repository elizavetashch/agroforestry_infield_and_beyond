# ============================================================================
# VISUALIZATION, VALIDATION & DIAGNOSTICS FOR m5 (pfr / gaussian)
# ============================================================================

library(refund)
library(mgcv)
library(ggplot2)
library(patchwork)
library(gratia)   # for draw(), appraise(), derivatives()

# ── 0. Quick sanity numbers ──────────────────────────────────────────────────
cat("=== Model summary numbers ===\n")
cat("Adj. R²       :", 0.819, "\n")
cat("Dev. explained:", "83 %\n")
cat("Scale est. (σ²):", round(m5$scale, 6), "\n")
cat("σ (residual SD):", round(sqrt(m5$scale), 4), "\n")
cat("n              :", nobs(m5), "\n\n")


# ── 1. Overdispersion (Gaussian: just check scale vs raw variance) ───────────
# For Gaussian there is no overdispersion in the count-data sense,
# but we verify the estimated σ² is consistent with residual scatter.

res   <- residuals(m5, type = "response")
fit   <- fitted(m5)

cat("=== Overdispersion / scale check ===\n")
cat("Pearson χ²  / df  :", 
    round(sum(res^2 / m5$scale) / m5$df.residual, 3),
    "  (should be ≈ 1 for a well-calibrated Gaussian model)\n")
cat("Empirical σ² of residuals:", round(var(res), 6), "\n")
cat("Model scale est. (σ²)    :", round(m5$scale,  6), "\n\n")


# ── 2. Residual diagnostics ──────────────────────────────────────────────────

# 2a. gratia::appraise() — four standard plots in one call
appraise(m5, point_alpha = 0.35, line_col = "#E63946")

# 2b. Manual residual plots (more control)
diag_df <- data.frame(
  fitted    = fit,
  residuals = res,
  obs       = seq_along(res)
)

p_rv <- ggplot(diag_df, aes(fitted, residuals)) +
  geom_point(alpha = 0.35, size = 1.2, colour = "#457B9D") +
  geom_hline(yintercept = 0, colour = "#E63946", linewidth = 0.8) +
  geom_smooth(se = FALSE, colour = "grey30", linewidth = 0.7, method = "loess") +
  labs(title = "Residuals vs Fitted", x = "Fitted (log yield)", y = "Residual") +
  theme_bw()

p_qq <- ggplot(diag_df, aes(sample = residuals)) +
  stat_qq(alpha = 0.35, colour = "#457B9D") +
  stat_qq_line(colour = "#E63946", linewidth = 0.8) +
  labs(title = "Normal Q-Q", x = "Theoretical quantiles", y = "Sample quantiles") +
  theme_bw()

p_hist <- ggplot(diag_df, aes(residuals)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "#457B9D", colour = "white", alpha = 0.8) +
  geom_density(colour = "#E63946", linewidth = 0.8) +
  labs(title = "Residual distribution", x = "Residual", y = "Density") +
  theme_bw()

p_scale <- ggplot(diag_df, aes(fitted, sqrt(abs(residuals)))) +
  geom_point(alpha = 0.35, size = 1.2, colour = "#457B9D") +
  geom_smooth(se = FALSE, colour = "#E63946", linewidth = 0.8, method = "loess") +
  labs(title = "Scale-Location", x = "Fitted", y = "√|Residual|") +
  theme_bw()

(p_rv | p_qq) / (p_hist | p_scale)


# ── 3. Observed vs Fitted ─────────────────────────────────────────────────────

p_of <- ggplot(diag_df, aes(fitted, fitted + residuals)) +
  geom_point(alpha = 0.35, size = 1.2, colour = "#457B9D") +
  geom_abline(slope = 1, intercept = 0, colour = "#E63946", linewidth = 0.8) +
  labs(title = "Observed vs Fitted (log scale)",
       x = "Fitted log(yield)", y = "Observed log(yield)") +
  theme_bw()

print(p_of)


# ── 4. Functional (lf) coefficient curves  β(t) ─────────────────────────────
# Extract via plot.gam(); suppress device output, collect the data

extract_lf_curve <- function(model, term_index, term_label, argvals) {
  pd <- plot(model, select = term_index, plot = FALSE)[[1]]
  data.frame(
    t     = pd$x,
    beta  = pd$fit,
    se    = pd$se,
    lower = pd$fit - 1.96 * pd$se,
    upper = pd$fit + 1.96 * pd$se,
    term  = term_label
  )
}

# Identify which select= indices correspond to the three lf() terms
# (check with plot(m5, pages=1) if unsure — usually 1, 2, 3)
lf_curves <- rbind(
  extract_lf_curve(m5, 1, "β(t): swf_matrix",      swf_argvals),
  extract_lf_curve(m5, 2, "β(t): swf_x_prop",      swf_argvals),
  extract_lf_curve(m5, 3, "β(t): swf_x_prop_dist", swf_argvals)
)

p_lf <- ggplot(lf_curves, aes(t, beta)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#ADE8F4", alpha = 0.5) +
  geom_line(colour = "#023E8A", linewidth = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "#E63946") +
  facet_wrap(~term, scales = "free_y") +
  labs(title = "Functional coefficient curves (lf terms)",
       x = "Argument (t along swf_argvals)",
       y = expression(hat(beta)(t))) +
  theme_bw()

print(p_lf)


# ── 5. Scalar smooth effects (gratia) ────────────────────────────────────────
# draw() picks up all smooth terms automatically
draw(m5,
     select   = c("s(log(distance_to_tree_strip))",
                  "s(prop_swf_within)",
                  "s(fert_N)"),
     residuals = TRUE,
     rug       = TRUE) &
  theme_bw()


# ── 6. Random effects ─────────────────────────────────────────────────────────
re_field <- ranef_terms <- data.frame(
  field = levels(z_swf$field),
  re    = as.numeric(coef(m5)[grep("s\\(field\\)", names(coef(m5)))])
)

# Simpler: use gratia::variance_components()
vc <- variance_comp(m5)
print(vc)

# Caterpillar plot for field random effects
field_re <- data.frame(
  field = rownames(coef(m5$lme, level = 1)),   # adjust if not lme-based
  re    = coef(m5$lme, level = 1)[, 1]
)
# If the above errors, use:
field_re_raw <- m5$coefficients[grep("field", names(m5$coefficients))]
field_re     <- data.frame(field = names(field_re_raw), re = field_re_raw)

ggplot(field_re, aes(x = reorder(field, re), y = re)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  labs(title = "Field random effects", x = "Field", y = "RE estimate") +
  theme_bw()


# ── 7. Concurvity check (multicollinearity of smooth terms) ──────────────────
cat("=== Concurvity ===\n")
cc <- concrvity(m5)          # gratia wrapper
print(cc)

# or base mgcv:
print(concurvity(m5, full = TRUE))
print(concurvity(m5, full = FALSE))   # pairwise; values > 0.8 are concerning


# ── 8. Influential observations (Cook's distance proxy) ──────────────────────
infl <- influence(m5, type = "cook")
plot(infl, type = "h",
     main = "Cook's distance (approx.)",
     ylab = "Cook's D", xlab = "Observation index")
abline(h = 4 / nobs(m5), col = "red", lty = 2)


# ── 9. Cross-validation R² (k-fold, lightweight) ─────────────────────────────
set.seed(123)
n     <- nobs(m5)
k     <- 5
folds <- sample(rep(1:k, length.out = n))
cv_ss_res <- cv_ss_tot <- numeric(k)

for (f in seq_len(k)) {
  train <- z_swf[folds != f, ]
  test  <- z_swf[folds == f, ]
  
  m_cv <- update(m5, data = train)
  pred  <- predict(m_cv, newdata = test, type = "response")
  obs   <- test$yield_log
  
  cv_ss_res[f] <- sum((obs - pred)^2,   na.rm = TRUE)
  cv_ss_tot[f] <- sum((obs - mean(obs))^2, na.rm = TRUE)
}

cv_r2 <- 1 - sum(cv_ss_res) / sum(cv_ss_tot)
cat(sprintf("5-fold CV R²: %.3f\n", cv_r2))




# ============================================================================
# FIXED: lf() curves  |  scalar smooths  |  LOO-CV  |  Cook's D
# ============================================================================
library(refund); library(mgcv); library(ggplot2); library(patchwork)


# ── 1. Functional coefficient curves: β(t) via PredictMat() -----------------
#   plot.gam() crashes on pfr's by-variable smooths with non-conformable matrices.
#   Fix: evaluate the basis directly, setting the by-variable = 1.

extract_lf_coef <- function(model, lf_var_name, argvals) {
  sm_labels <- sapply(model$smooth, function(s) s$label)
  idx <- grep(paste0("L\\.", lf_var_name, "\\b"), sm_labels)
  if (!length(idx)) idx <- grep(lf_var_name, sm_labels)   # fallback
  if (!length(idx)) stop("Smooth not found for: ", lf_var_name)
  sm    <- model$smooth[[idx[1]]]
  first <- sm$first.para
  last  <- sm$last.para
  
  newdat          <- setNames(data.frame(argvals), sm$term)
  newdat[[sm$by]] <- 1        # by = 1 isolates the pure coefficient function
  
  Xp   <- mgcv::PredictMat(sm, data = newdat)
  beta <- as.numeric(Xp %*% coef(model)[first:last])
  se   <- sqrt(pmax(0, diag(Xp %*% model$Vp[first:last, first:last] %*% t(Xp))))
  
  data.frame(t = argvals, beta = beta, se = se,
             lower = beta - 1.96 * se, upper = beta + 1.96 * se)
}

lf_vars <- c("swf_matrix", "swf_x_prop", "swf_x_prop_dist")
lf_curves <- do.call(rbind, lapply(lf_vars, function(v) {
  cbind(extract_lf_coef(m5, v, swf_argvals), term = v)
}))

p_lf <- ggplot(lf_curves, aes(t, beta)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#90CAF9", alpha = 0.4) +
  geom_line(colour = "#1565C0", linewidth = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "#C62828", linewidth = 0.7) +
  facet_wrap(~term, scales = "free_y", ncol = 1) +
  labs(title = "Functional predictor effects  β(t)",
       subtitle = "Shaded = pointwise 95 % CI  |  dashed = zero",
       x = "Argument (swf_argvals)", y = expression(hat(beta)(t))) +
  theme_bw(base_size = 11)

print(p_lf)


# ── 2. Scalar smooth effects: plot.gam(plot=FALSE) instead of gratia::draw() -
#   gratia::draw() tries to fetch matrix columns from the data frame and fails.
#   plot.gam() only needs the model object.
#
#   Smooth term indices (confirm with lapply(m5$smooth, \(s) s$label)):
#   1 s(swf_matrix.tmat):L.swf_matrix        <- lf
#   2 s(swf_x_prop.tmat):L.swf_x_prop        <- lf
#   3 s(swf_x_prop_dist.tmat):L.swf_x_prop_dist <- lf
#   4 s(log(distance_to_tree_strip))
#   5 s(prop_swf_within)
#   6 s(fert_N)
#   7 s(crop_unified, distance_to_tree_strip)  <- re
#   8 s(field)                                 <- re
#   9 s(year)                                  <- re

scalar_terms <- list(
  list(idx = 4, xlab = "log(distance_to_tree_strip)"),
  list(idx = 5, xlab = "prop_swf_within"),
  list(idx = 6, xlab = "fert_N")
)

smooth_plots <- lapply(scalar_terms, function(s) {
  pd <- tryCatch(plot(m5, select = s$idx, plot = FALSE)[[1]],
                 error = function(e) { message(e); NULL })
  if (is.null(pd)) return(NULL)
  df <- data.frame(x = pd$x, fit = pd$fit, se = pd$se,
                   lower = pd$fit - 1.96 * pd$se,
                   upper = pd$fit + 1.96 * pd$se)
  ggplot(df, aes(x, fit)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#90CAF9", alpha = 0.4) +
    geom_line(colour = "#1565C0", linewidth = 0.9) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    labs(title = paste0("s(", s$xlab, ")"),
         x = s$xlab, y = "Partial effect (log yield)") +
    theme_bw(base_size = 11)
})
wrap_plots(Filter(Negate(is.null), smooth_plots), nrow = 1)


# ── 3. Cross-validation: exact LOO via hat matrix ----------------------------
#   update() with pfr doesn't re-run lf()'s data expansion → length mismatch.
#   For penalised GAMs, LOO residuals via  e_i / (1 - h_ii)  are exact and free.

hat_vals  <- hatvalues(m5)            # influence matrix diagonal
res_raw   <- residuals(m5, type = "response")
loo_res   <- res_raw / (1 - hat_vals)

loo_r2    <- 1 - sum(loo_res^2) / sum((m5$y - mean(m5$y))^2)
loo_rmse  <- sqrt(mean(loo_res^2))

cat("=== LOO cross-validation (hat-matrix, exact) ===\n")
cat(sprintf("  In-sample  R²  : 0.819\n"))
cat(sprintf("  LOO        R²  : %.3f\n",  loo_r2))
cat(sprintf("  Optimism gap   : %.3f\n",  0.819 - loo_r2))
cat(sprintf("  In-sample RMSE : %.4f\n",  sqrt(mean(res_raw^2))))
cat(sprintf("  LOO RMSE       : %.4f  (log-yield units)\n", loo_rmse))

ggplot(data.frame(fitted = fitted(m5), loo_res, leverage = hat_vals),
       aes(fitted, loo_res)) +
  geom_point(aes(colour = leverage), alpha = 0.5, size = 1.4) +
  scale_colour_gradient(low = "#B3E5FC", high = "#B71C1C",
                        name = "Leverage") +
  geom_hline(yintercept = 0, colour = "#C62828", linewidth = 0.8) +
  geom_smooth(se = FALSE, colour = "grey30", linewidth = 0.6, method = "loess") +
  labs(title = "LOO residuals vs Fitted",
       subtitle = "High-leverage points (darker) drive optimism most",
       x = "Fitted log(yield)", y = "LOO residual") +
  theme_bw(base_size = 11)


# ── 4. Cook's distance: better threshold + labelled re-plot ------------------
#   The 4/n rule (≈ 0.006 for n = 647) is far too conservative — flags >95 % 
#   of observations.  Use mean + 3 SD instead.

cook_d    <- cooks.distance(m5)
thresh_sd <- mean(cook_d) + 3 * sd(cook_d)

cat("\n=== Cook's distance ===\n")
cat(sprintf("  4/n threshold (classic)  : %.4f  → flags %d obs (%.0f%%)\n",
            4/length(cook_d), sum(cook_d > 4/length(cook_d)),
            100*mean(cook_d > 4/length(cook_d))))
cat(sprintf("  Mean + 3 SD threshold    : %.4f  → flags %d obs\n",
            thresh_sd, sum(cook_d > thresh_sd)))

top5 <- order(cook_d, decreasing = TRUE)[1:5]
cat("  Top-5 influential rows:", paste(top5, collapse = ", "), "\n")
cat("  Their Cook's D        :", paste(round(cook_d[top5], 3), collapse = ", "), "\n")
cat("  NOTE: cluster ~obs 330–380 warrants inspection (same crop/field/year?)\n")

cook_df <- data.frame(obs = seq_along(cook_d), cook = cook_d)
ggplot(cook_df, aes(obs, cook)) +
  geom_segment(aes(xend = obs, yend = 0), colour = "#457B9D", alpha = 0.55) +
  geom_hline(yintercept = thresh_sd,
             colour = "#E63946", linetype = "dashed", linewidth = 0.8) +
  geom_point(data = subset(cook_df, cook > thresh_sd),
             colour = "#E63946", size = 2) +
  ggrepel::geom_text_repel(              # install ggrepel if not present
    data    = subset(cook_df, cook > thresh_sd),
    aes(label = obs), size = 3, colour = "#E63946") +
  labs(title = "Cook's Distance",
       subtitle = sprintf("Dashed = mean + 3 SD (%.3f)  |  4/n = %.3f is too conservative for n = %d",
                          thresh_sd, 4/nrow(cook_df), nrow(cook_df)),
       x = "Observation index", y = "Cook's D") +
  theme_bw(base_size = 11)
