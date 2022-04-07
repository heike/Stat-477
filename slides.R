## ----setup, include=FALSE, warning=FALSE, message=FALSE----------------------------------------
options(htmltools.dir.version = FALSE)
library(ggplot2)
library(tidyverse)
library(lme4)
xaringanExtra::use_panelset()


## ----echo = FALSE, message=FALSE, warning=FALSE, fig.height=3.5--------------------------------
dframe <- data.frame(x = factor(1:5), y = c(110, 100, 85, 102, 105))
dframe %>% ggplot(aes( x = x, weight = y)) +
  geom_bar(width=.66) +
  theme_minimal() +
  annotate("text", label="A", size = 12, x = 2, y = 50, colour = "white") +
  annotate("text", label="B", size = 12, x = 4, y = 50, colour = "white")


## ----echo=FALSE, fig.height= 3.5---------------------------------------------------------------
bars <- read.csv("jnd-bar-results.csv")

bars %>% filter(userID=="8_91") %>%
  filter(intensity == 50, distance == 9) %>%
  ggplot(aes(x = X, y = Yes)) + 
  geom_point() +
  theme_bw() +
  xlab("Height of bar B") + 
  ylab("Number of responses: B is taller than A") +
  ggtitle("Participant: 8_91")


## ----echo=FALSE--------------------------------------------------------------------------------
id8_91 <- bars %>% filter(userID=="8_91") %>%
  filter(intensity == 50, distance == 9) %>%
  mutate(`height B` = X)


## ----------------------------------------------------------------------------------------------
id8_91 %>% select(`height B`, Total, Yes, distance, intensity)


## ----highlight.output=c(12, 13)----------------------------------------------------------------
logr8_91 <- glm(cbind(Yes, Total-Yes)~`height B`, 
                data = id8_91, family = binomial())
summary(logr8_91)


## ----echo=FALSE, fig.height = 3.5--------------------------------------------------------------
pred.frame <- tibble(`height B` = seq(48, 52, by=0.01))
pred.frame$preds <- predict(logr8_91, newdata = pred.frame, type="response")

id8_91 %>%
  ggplot(aes(x = `height B`, y = Yes/Total)) + 
  geom_line(aes(y = preds), data = pred.frame) +
  geom_point(colour = "steelblue", size = 4) +
  theme_bw() +
  xlab("Height of bar B") + 
  ylab("Number of responses: B is taller than A") +
  ggtitle("Participant: 8_91")


## ----echo=FALSE--------------------------------------------------------------------------------
id8_91 <- bars %>% filter(userID=="8_91") %>%
  filter(intensity == 50, distance == 93) %>%
  mutate(`height B` = X)


## ----------------------------------------------------------------------------------------------
id8_91 %>% select(`height B`, Total, Yes, distance, intensity)


## ----highlight.output=c(12, 13)----------------------------------------------------------------
logr8_91 <- glm(cbind(Yes, Total-Yes)~`height B`, 
                data = id8_91, family = binomial())
summary(logr8_91)


## ----echo=FALSE, fig.height = 3.5--------------------------------------------------------------
pred.frame <- tibble(`height B` = seq(45, 55, by=0.01))
pred.frame$preds <- predict(logr8_91, newdata = pred.frame, type="response")

id8_91 %>%
  ggplot(aes(x = `height B`, y = Yes/Total)) + 
  geom_line(aes(y = preds), data = pred.frame) +
  geom_point(colour = "steelblue", size = 4) +
  theme_bw() +
  xlab("Height of bar B") + 
  ylab("Number of responses: B is taller than A") +
  ggtitle("Participant: 8_91")


## ----echo=FALSE--------------------------------------------------------------------------------
id8_91 <- bars %>% filter(userID=="8_91") %>%
  filter(intensity == 50) %>%
  mutate(`height B` = X)


## ----echo = TRUE-------------------------------------------------------------------------------
id8_91 %>% group_by(distance) %>% summarize(`Number of responses` = sum(Total))


## ----highlight.output=c(12, 13)----------------------------------------------------------------
logr8_91_dfactor <- glm(cbind(Yes, Total-Yes) ~ 
  factor(distance)-1 + `height B`:factor(distance), 
  data = id8_91, family = binomial())


## ----echo=FALSE, fig.height = 3.5--------------------------------------------------------------
pred.frame <- tibble(expand.grid(`height B` = seq(40, 60, by=0.01), distance = c(9,  93, 177, 261, 345)))
pred.frame$preds_factor <- predict(logr8_91_dfactor, newdata = pred.frame, type="response")

id8_91 %>%
  ggplot(aes(x = `height B`, y = Yes/Total)) + 
  geom_line(aes(y = preds_factor, group = distance, 
                colour = factor(distance)), data = pred.frame) +
  geom_point(aes(colour = factor(distance)), size = 4) +
  scale_colour_brewer("Distance", palette = "Paired") +
  theme_bw() +
  xlab("Height of bar B") + 
  ylab("Number of responses: B is taller than A") +
  ggtitle("Participant: 8_91")


## ----echo=FALSE--------------------------------------------------------------------------------
dt <- broom::tidy(logr8_91_dfactor) %>% 
  mutate(
    distance = parse_number(term),
    type = rep(c("main", "interaction"), each=5)
  )
dtw <- data.frame(
  main=dt$estimate[dt$type=="main"],
  interaction=dt$estimate[dt$type=="interaction"], 
  distance = unique(dt$distance))
dtw <- dtw %>% mutate(
  pse = -main/interaction,
  slope_50 = interaction/4
)
dtlong <- dtw %>% dplyr::select(distance, pse, slope_50) %>%
  pivot_longer(-distance, names_to="term", values_to = "estimate")


## ---- echo=FALSE, fig.height = 3.5-------------------------------------------------------------
dtlong %>% ggplot(aes(x = distance, y = estimate)) +
  geom_point() +
  theme_bw() +
  facet_wrap(~term, scales="free_y")


## ----------------------------------------------------------------------------------------------
logr8_91_dnum <- glm(
  cbind(Yes, Total-Yes) ~ `height B` * log(distance), 
  data = id8_91, family = binomial())


## ----echo=FALSE, fig.height = 3.5--------------------------------------------------------------
pred.frame <- tibble(expand.grid(`height B` = seq(40, 60, by=0.01), distance = c(9,  93, 177, 261, 345)))
pred.frame$preds_numeric <- predict(logr8_91_dnum, newdata = pred.frame, type="response")

id8_91 %>%
  ggplot(aes(x = `height B`, y = Yes/Total)) + 
  geom_line(aes(y = preds_numeric, group = distance, 
                colour = factor(distance)), data = pred.frame) +
  geom_point(aes(colour = factor(distance)), size = 4) +
  scale_colour_brewer("Distance", palette = "Paired") +
  theme_bw() +
  xlab("Height of bar B") + 
  ylab("Number of responses: B is taller than A") +
  ggtitle("Participant: 8_91")


## ----------------------------------------------------------------------------------------------
anova(logr8_91_dnum, logr8_91_dfactor)


## ----results=c(-1:-4)--------------------------------------------------------------------------
summary(logr8_91_dnum)


## ----echo = FALSE------------------------------------------------------------------------------
id8_91 <- bars %>% filter(userID=="8_91")

