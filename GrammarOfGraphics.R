#Grammar of Graphics (ggplot)
library(reshape2)
library(ggplot2)
data(tips)
ggplot(aes(x = total_bill, y = tip), data = tips) +
  geom_point(aes(color = sex)) +
  geom_smooth(method = 'lm')

layer_point <- geom_point(
  mapping = aes(x = total_bill, y = tip, color = sex),
  data = tips,
  size = 3
)
ggplot() + layer_point

model <- lm(tip ~ total_bill, data = tips)
fitted_tips <- data.frame(
  total_bill = tips$total_bill,
  predict(model, interval = "confidence")
)
head(fitted_tips)

layer_line <- geom_line(
  mapping = aes(x = total_bill, y = fit),
  data = fitted_tips,
  color = 'darkred'
)
ggplot() + layer_point + layer_line

layer_ribbon <- geom_ribbon(
  mapping = aes(x = total_bill, ymin = lwr, ymax = upr),
  data = fitted_tips,
  alpha = .3
)

ggplot() + layer_point + layer_line + layer_ribbon

layer_smooth <- geom_line(
  mapping = aes(x = total_bill, y = tip),
  data = tips,
  stat = "smooth",
  method = "lm"
)

ggplot() + layer_point + layer_smooth

ggplot() +
  layer_point +
  layer_smooth +
  facet_wrap(~ day)

#can facet across two variables with facet_grid
ggplot() +
  layer_point +
  layer_smooth +
  facet_grid(smoker ~ day)
#data sets: economics, presidential
data(economics)
data(presidential)

