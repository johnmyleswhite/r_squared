# Given a simulation design, simulate everything out.
#
# Arguments:
#
# f: The target f(x) of the regression problem.
# x_min: The lowest value of x in the problem.
# x_max: The highest value of x in the problem.
# n: The number of points between x_min and x_max including the ends.
# sigma: The variance of the noise's normal distribution.
#
# Returns:
#
# obs: A DataFrame containing x, x^2 and y.
# r2_linear: R^2 for the linear model.
# r2_quadratic: R^2 for the quadratic model.
# r2_true: R^2 for the true function, f(x).
# e2_constant: E^2 for the constant model.
# e2_linear: E^2 for the linear model.
# e2_quadratic: E^2 for the quadratic model.
# mse_constant: MSE for the constant model.
# mse_linear: MSE for the linear model.
# mse_quadratic: MSE for the quadratic model.
# mse_true: MSE for the true function, f(x).
# y_pred_constant: A vector of predictions, y[i], for the constant model.
# y_pred_linear: A vector of predictions, y[i], for the linear model.
# y_pred_quadratic: A vector of predictions, y[i], for the quadratic model.
# y_pred_true: A vector of predictions, y[i], for the true model, f(x).
# f: The input function, f(x).

simulate <- function(f, x_min, x_max, n, sigma) {
    # Generate a data set.
    x <- seq(x_min, x_max, length.out = n)
    x2 <- x^2
    y <- f(x) + rnorm(n, mean = 0, sd = sigma)
    obs <- data.frame(x = x, x2 = x2, y = y)

    # Fit a constant model and evaluate its MSE.
    fit_constant <- mean(y)
    y_pred_constant <- rep(fit_constant, n)
    mse_constant <- mean((y_pred_constant - y)^2)

    # Fit a linear model and evaluate its MSE.
    fit_linear <- lm(y ~ x, data = obs)
    y_pred_linear <- predict(fit_linear)
    mse_linear <- mean((y_pred_linear - y)^2)

    # Fit a quadratic model and evaluate its MSE.
    fit_quadratic <- lm(y ~ x + x2, data = obs)
    y_pred_quadratic <- predict(fit_quadratic)
    mse_quadratic <- mean((y_pred_quadratic - y)^2)

    # Predict data using the true model and evaluate its MSE.
    y_pred_true <- f(x)
    mse_true <- mean((y_pred_true - y)^2)

    # Return everything.
    return(
        list(
            "obs"=obs,
            "r2_linear"=1 - mse_linear / mse_constant,
            "r2_quadratic"=1 - mse_quadratic / mse_constant,
            "r2_true"=1 - mse_true / mse_constant,
            "e2_constant"=mse_constant / mse_true - 1,
            "e2_linear"=mse_linear / mse_true - 1,
            "e2_quadratic"=mse_quadratic / mse_true - 1,
            "mse_constant"=mse_constant,
            "mse_linear"=mse_linear,
            "mse_quadratic"=mse_quadratic,
            "mse_true"=mse_true,
            "y_pred_constant"=y_pred_constant,
            "y_pred_linear"=y_pred_linear,
            "y_pred_quadratic"=y_pred_quadratic,
            "y_pred_true"=y_pred_true,
            "f"=f
        )
    )
}

# Plot the raw data and all of the predictions from the fitted models based
# on the full results of a simulation run.
#
# Arguments:
#
# res: The results from a call to the simulate() function.
#
# Returns:
#
# p: A ggplot2 plot.

plot_models <- function(res) {
    linear_data <- data.frame(x = res$obs$x, y = res$y_pred_linear)
    quadratic_data <- data.frame(x = res$obs$x, y = res$y_pred_quadratic)
    true_data <- data.frame(x = res$obs$x, y = res$y_pred_true)

    p <- ggplot(res$obs, aes(x = x, y = y)) +
        geom_point(alpha = 0.2) +
        geom_line(aes(color = "linear model"), data = linear_data) +
        geom_line(aes(color = "quadratic model"), data = quadratic_data) +
        geom_line(aes(color = "true model"), data = true_data) +
        theme_bw() +
        labs(color = "")

    return(p)
}
