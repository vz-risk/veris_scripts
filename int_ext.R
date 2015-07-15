DESCRIPTION <- "Add discovery_method.Prt, discovery_method.Int, and discovery_method.Ext columns to the verisr object for filtering internal vs external vs partner breaches."

load("./data.Rda")

int <- vz %>% select(starts_with('discovery_method.Int'), plus.master_id) %>% gather(k, discovery_method.Int, -plus.master_id) %>% filter(discovery_method.Int) %>% select(-k) %>% unique
vz_disc <- full_join(vz, int)
ext <- vz %>% select(starts_with('discovery_method.Ext'), plus.master_id) %>% gather(k, discovery_method.Ext, -plus.master_id) %>% filter(discovery_method.Ext) %>% select(-k) %>% unique
vz_disc <- full_join(vz_disc, ext)
prt <- vz %>% select(starts_with('discovery_method.Prt'), plus.master_id) %>% gather(k, discovery_method.Prt, -plus.master_id) %>% filter(discovery_method.Prt) %>% select(-k) %>% unique
vz_disc <- full_join(vz_disc, prt)


tf_int <- is.na(vz_disc$discovery_method.Int)
vz_disc$discovery_method.Int[tf_int] <- FALSE
vz$discovery_method.Int <- vz_disc$discovery_method.Int

tf_ext <- is.na(vz_disc$discovery_method.Ext)
vz_disc$discovery_method.Ext[tf_ext] <- FALSE
vz$discovery_method.Ext <- vz_disc$discovery_method.Ext

tf_prt <- is.na(vz_disc$discovery_method.Prt)
vz_disc$discovery_method.Prt[tf_prt] <- FALSE
vz$discovery_method.Prt <- vz_disc$discovery_method.Prt