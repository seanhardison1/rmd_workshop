library(tidyverse)
library(mgcv)
library(gratia)
library(ggeffects)

# research question - how have densities of smooth cordgrass (S. alterniflora) 
# changed over time across the VCR?

# load data---
load(here::here("data/processed_spartina_biomass.rdata"))

# visualize----
ggplot(spart_bio) +
  geom_point(aes(x = year, y = mean_transect_biomass)) +
  facet_grid(habitat_type~transect)

# model fitting----
# based on Pedersen et al. 2019, we can start by fitting the "GS" model.
# The GS model says that all habitat types will share a 
# common smooth i.e. the same temporal trend, with group-level smooths
# that are penalized according to how wiggly they are relative to the overall trend. 
# This is the GAM version of a random slope and intercept GLMM. The hypothesis
# is that there is a common global trend across habitat types and sampling 
# sites.

# the model - transect X habitat type X sampling site combinations form
# the lowest level of replication
mod <- 
  gam(mean_transect_biomass ~ s(year) + # overall trend
                            s(year, habitat_type, bs = "fs") + # habitat-level variation ("random slope")
                            s(year, site, bs = "fs"), #site-level variation ("random slope")
    family = "nb",
    method = "REML",
    data = spart_bio)

summary(mod)

# visualize----
gratia::draw(mod)

# evaluate model fit----
gratia::appraise(mod) # the model fits surprisingly well

# prediction----
# predict from the model using observations
pred_out2 <- cbind(spart_bio,
                  predict(mod,
                          newdata = spart_bio,
                          type = "response",
                          se.fit = T))

# visualize output at the level of site
site_pred_plt <- 
  ggplot(data = pred_out2) +
  geom_point(aes(x = year, y = mean_transect_biomass,
                 color = site),
             alpha = 0.5, size = 0.5) +
  geom_line(aes(y = fit, x = year, group = site,
                color = site)) +
  facet_grid(habitat_type ~ transect) +
  theme_bw() +
  labs(y = "Cordgrass biomass density",
       x = "Year")

# visualize output at the level of the VCR (the overall smooth)
overall_preds <-
  # predictions are back-transformed by default
  ggpredict(mod, terms = "year", type = "re") %>% 
  as_tibble()

vcr_pred_plt <- 
  ggplot() +
  geom_point(data = spart_bio,
             aes(x = year, y = mean_transect_biomass),
             alpha = 0.5) +
  geom_line(data = overall_preds,
            aes(y = predicted, 
                x = x)) +
  geom_ribbon(data = overall_preds,
              aes(ymin = conf.low,
                  ymax = conf.high,
                  x = x),
              alpha = 0.25) +
  labs(y = "Cordgrass biomass density",
       x = "Year") +
  theme_bw()

# do the predicted values make sense?
t <- spart_bio %>% 
  filter(year == 1999)

# arithmetic mean
m1 <- mean(t$mean_transect_biomass)

# adjusted prediction for 1999 from GAMM
m2 <- 
  ggpredict(mod, terms = "year[1999]") %>% 
  as_tibble() %>% 
  pull(predicted)

test_plt <- 
  ggplot(data = t) +
  geom_histogram(aes(mean_transect_biomass)) +
  geom_vline(xintercept = m1) +
  geom_vline(xintercept = m2, color = "blue") +
  labs(x = "Cordgrass biomass density (for 1999 only)",
       y = "Frequency") +
  theme_bw()

save(mod, site_pred_plt, vcr_pred_plt, test_plt,
     file = here::here("data/model_fitting_results.rdata"))
