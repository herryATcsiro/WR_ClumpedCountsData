
# ---- Load packages (namespaced to avoid conflicts) ----
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)

# actual values for TB are 0 to 125
# this is for 
fam = brmsfamily("negbinomial",link = "identity")
f1 <- bf(
  TB  ~ (exp(a) * A^b) / g + s,
  a+b+g ~ 1,
  s~gp(x,y, scale=TRUE, iso=T, cov="matern32", k=100),
  nl = TRUE
)
p1 = c(
  # Non-linear scale parameters (broad, but not extreme)
  prior(normal(4, 1), nlpar = a),          # exp(a) median ≈ 75 TB; wide but centered log(75)≈ 4
  prior(normal(1.5, 0.3),     nlpar = g, lb = 0), # g > 0; semi-diffuse half-N on divisor
  prior(normal(0, 1),     nlpar = b, lb = 0), # b ≥ 0; A is fixed (25), so keep near 0 but broad
  # GP hyperparameters for s(x,y) with SCALE = TRUE (coords internally scaled to max distance = 1)
  prior(exponential(0.1),      class = "sdgp",  nlpar = s),   # Mean of Exponential(rate)=1/rate = 1/0.1≈10 units
  prior(lognormal(log(0.2), 0.5),    class = "lscale", coef = "gpxy", nlpar = s), 
  # Negative binomial alpha (shape)
  prior(normal(0, 5), class = "shape")     # dispersion parameter, overdispersed (variance 11× mean).
)




# ---- Settings ----
set.seed(123)
n_draws <- 100000   # increase if you want sharper tail estimates
A       <- 25       # fixed A in your model

# ---- Draws from your priors (exactly as specified) ----
# a ~ Normal(4, 0.5)
a <- rnorm(n_draws, mean = 4, sd = 1)

# g ~ Normal(1.5, 0.5) with lb = 0 (emulate brms class 'b' bound)
g_raw <- rnorm(n_draws, mean = 1.5, sd = 0.3)
g     <- pmax(g_raw, 0)

# b ~ Normal(0, 0.5) with lb = 0 (emulate brms class 'b' bound)
b_raw <- rnorm(n_draws, mean = 0,   sd = 1)
b     <- pmax(b_raw, 0)

# sdgp ~ Exponential(0.1)
sdgp <- rexp(n_draws, rate = 0.1)   # mean = 10

# lscale ~ LogNormal(log(0.2), 0.5)  (on scaled distance domain; not used for marginal s)
lscale <- rlnorm(n_draws, meanlog = log(0.2), sdlog = 0.5)

# shape ~ Normal(0, 5) on log(shape) by default in negbinomial (not needed for mu)
shape_log <- rnorm(n_draws, mean = 0, sd = 5)

# ---- Baseline mean and full mean (at an arbitrary location) ----
mu_base <- (exp(a) * A^b) / pmax(g, .Machine$double.eps)
quantile(mu_base)


# GP marginal at a single point: s ~ Normal(0, sdgp)
s  <- rnorm(n_draws, mean = 0, sd = sdgp)
mu <- mu_base + s

# ---- Summaries: quantiles and validity checks (mu must be > 0 for negbinomial) ----
qs_base <- quantile(mu_base, c(0.01, 0.1, 0.5, 0.9, 0.99))
qs_mu   <- quantile(mu,      c(0.01, 0.1, 0.5, 0.9, 0.99))

prop_mu_nonpos <- mean(mu <= 0)   # proportion of invalid means due to large negative s

cat("\nQuantiles mu_base:\n"); print(qs_base)
cat("\nQuantiles mu (with GP s):\n"); print(qs_mu)
cat(sprintf("\nProportion mu <= 0: %.4f\n", prop_mu_nonpos))

# ---- Tidy for plotting ----
pri_df <- tibble(
  mu_base = mu_base,
  mu      = mu,
  sdgp    = sdgp,
  lscale  = lscale
)

# For plots of valid means, filter mu > 0 (negbinomial requires positive mean)
pri_valid <- pri_df %>% filter(mu > 0)

# ---- Visualization 1: baseline mean only ----
ggplot(pri_df, aes(x = mu_base)) +
  geom_histogram(bins = 100, fill = "grey40", alpha = 0.7) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Prior range for baseline mean (μ_base) without GP s",
    x = expression(mu[base] == (exp(a) * 25^b) / g), y = "Frequency"
  ) +
  coord_cartesian(xlim=c(0,2*10^6))+
  theme_bw()

# ---- Visualization 2: full mean μ (includes GP s); show only valid μ > 0 ----
ggplot(pri_valid, aes(x = mu)) +
  geom_histogram(bins = 100, fill = "steelblue", alpha = 0.7) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Prior range for μ = μ_base + s (showing μ > 0 only)",
    subtitle = sprintf("Invalid μ (≤ 0) fraction: %.2f%%", 100 * prop_mu_nonpos),
    x = expression(mu), y = "Frequency"
  ) +
  coord_cartesian(xlim=c(0,2*10^6))+
  theme_bw()

# ---- Visualization 3 (optional): overlay densities of μ_base vs μ (μ > 0) ----
pri_df %>%
  mutate(which = "mu_base") %>%
  dplyr::select(x = mu_base, which) %>%
  bind_rows(pri_valid %>% mutate(which = "mu") %>% dplyr::select(x = mu, which)) %>%
  ggplot(aes(x = x, color = which, fill = which)) +
  geom_density(alpha = 0.25) +
  scale_x_continuous(labels = scales::comma) +
  scale_color_manual(values = c("mu_base" = "grey20", "mu" = "steelblue")) +
  scale_fill_manual(values  = c("mu_base" = "grey60", "mu" = "steelblue")) +
  labs(
    title = "Overlay of prior densities: μ_base (grey) vs μ (blue)",
    x = "Value", y = "Density"
  ) +
  coord_cartesian(xlim=c(0,1*10^6))+
    theme_bw()

# ---- Tabulate key quantiles for reporting ----
tbl_q <- tibble(
  quantile = c("1%", "10%", "50%", "90%", "99%"),
  mu_base  = as.numeric(qs_base),
  mu       = as.numeric(qs_mu)
)

tbl_q

