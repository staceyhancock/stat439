### Berkeley Graduate Admissions Data
berkeley <- data.frame(
  Sex = rep(c("Men","Women"), c(2681,1835)),
  Status = rep(c("Admitted","Denied","Admitted","Denied"),
               c(1195,1486,559,1276)),
  Program = rep(rep(c("A","B","C","D","E","F"),4),
                c(511,352,120,137,53,22,
                  314,208,205,270,138,351,
                  89,17,202,132,95,24,
                  19,8,391,243,298,317))
)

library(tidyverse)

## Marginal relationship
berkeley %>% ggplot(aes(x = Sex, fill = Status)) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  labs(
    title = "Proportion Admitted by Sex",
    x = "Sex",
    y = "Proportion"
  ) 

## Conditional relationship
berkeley %>% ggplot(aes(x = Sex, fill = Status)) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  labs(
    title = "Proportion Admitted by Sex within Program",
    x = "Sex",
    y = "Proportion"
  ) +
  facet_wrap(~Program)
