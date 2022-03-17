## 3. Extract the diamonds that have very high and very low residuals.
## Is there anything unusual about these diamonds? Are the particularly bad or good, or do you think these are pricing errors?
library(dplyr)
library(ggplot2)
library(modelr)

diamonds2 <- diamonds %>% 
  filter(carat <= 2.5) %>% 
  mutate(lprice = log2(price), lcarat = log2(carat))

mod_diamond <- lm(lprice ~ lcarat, data = diamonds2)

diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamond, "lresid")

resid_quants <- quantile(diamonds2$lresid)

filtered <- diamonds2 %>% 
  filter( 
    !((lresid < resid_quants[["25%"]]) & (lresid > resid_quants[["75%"]])) 
    )

ggplot(filtered, aes(cut, price)) + geom_boxplot()
