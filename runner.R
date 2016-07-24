library("ggplot2")

source("utils.R")

set.seed(1)

sigma <- 0.05

res <- simulate(log, 0.99, 1.01, 1000, sigma)
print(res$r2_linear)
print(res$r2_true)
print(res$e2_linear)
p <- plot_models(res)
ggsave(file.path("graphs", "small_range.png"), height = 7, width = 12)

res <- simulate(log, 1, 1000, 1000, sigma)
print(res$r2_linear)
print(res$r2_true)
print(res$e2_linear)
p <- plot_models(res)
ggsave(file.path("graphs", "large_range.png"), height = 7, width = 12)

x_maxs <- seq(1.1, 1000, by = 0.01)
r2_linears <- rep(NA, length(x_maxs))
r2_quadratics <- rep(NA, length(x_maxs))
r2_trues <- rep(NA, length(x_maxs))
e2_constants <- rep(NA, length(x_maxs))
e2_linears <- rep(NA, length(x_maxs))
e2_quadratics <- rep(NA, length(x_maxs))

for (i in 1:length(x_maxs)) {
    res <- simulate(log, 1, x_maxs[i], 1000, sigma)
    r2_linears[i] <- res$r2_linear
    r2_quadratics[i] <- res$r2_quadratic
    r2_trues[i] <- res$r2_true
    e2_constants[i] <- res$e2_constant
    e2_linears[i] <- res$e2_linear
    e2_quadratics[i] <- res$e2_quadratic
}

model_comparisons <- data.frame(
    x_min = 1,
    x_max = c(x_maxs, x_maxs, x_maxs),
    r2 = c(r2_linears, r2_quadratics, r2_trues),
    e2 = c(e2_constants, e2_linears, e2_quadratics),
    r2_metric = rep(c("linear", "quadratic", "true"), each = length(x_maxs)),
    e2_metric = rep(c("constant", "linear", "quadratic"), each = length(x_maxs))
)

p <- ggplot(
    model_comparisons,
    aes(x = x_max - x_min, y = r2, color = r2_metric)
) +
    geom_point() +
    scale_x_log10() +
    theme_bw() +
    ylab("R^2") +
    labs(color = "")
ggsave(file.path("graphs", "r2.png"), height = 7, width = 12)

p <- ggplot(
    model_comparisons,
    aes(x = x_max - x_min, y = e2, color = e2_metric)
) +
    geom_point() +
    scale_x_log10() +
    theme_bw() +
    ylab("E^2") +
    labs(color = "")
ggsave(file.path("graphs", "e2.png"), height = 7, width = 12)
