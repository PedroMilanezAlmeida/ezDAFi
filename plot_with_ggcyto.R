library(ggcyto)

autoplot(gs[[1]])

autoplot(gs_SOM[[1]])

ggcyto(gs_SOM, 
       aes(x = CD4, 
           y = CD3),
       subset = "root") + 
  geom_hex(bins = 128)

ggcyto(gs,
       aes(x = CD4, 
           y = CD3),
       subset = "CD3") + 
  geom_hex(bins = 128)

ggplot(gs_pop_get_data(gs, "DAFi_CD4"), 
       aes(x = `Comp-B 710-A`,
           y = `Comp-R 780-A`)) + 
  geom_hex(bins = 128) +
  xlim(c(-10,250)) +
  ylim(c(-10,275))

ggplot(gs_pop_get_data(gs, "DAFi_CD8"), 
       aes(x = `Comp-B 710-A`,
           y = `Comp-R 780-A`)) + 
  geom_hex(bins = 128)  +
  xlim(c(-10,250)) +
  ylim(c(-10,275))

ggplot(gs_pop_get_data(gs, "DAFi_DPT"), 
       aes(x = `Comp-B 710-A`,
           y = `Comp-R 780-A`)) + 
  geom_hex(bins = 128)  +
  xlim(c(-10,250)) +
  ylim(c(-10,275))

ggplot(gs_pop_get_data(gs, "DAFi_DNT"), 
       aes(x = `Comp-B 710-A`,
           y = `Comp-R 780-A`)) + 
  geom_hex(bins = 128)  +
  xlim(c(-10,250)) +
  ylim(c(-10,275))


ggplot(gs_pop_get_data(gs_SOM, "DNT"), 
       aes(x = `Comp-B 710-A`,
           y = `Comp-R 780-A`)) + 
  geom_hex(bins = 128)  +
  xlim(c(-10,250)) +
  ylim(c(-10,275))
