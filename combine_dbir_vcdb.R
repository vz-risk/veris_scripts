DESCRIPTION <- "Combine VCDB and DBIR data.  In this case, records dealing with healthcare."

vz_healthcare <- vz %>% filter(victim.industry2.62 | attribute.confidentiality.data.variety.Medical | grepl('Patient', attribute.confidentiality.data_victim)) %>% filter(attribute.confidentiality.data_disclosure.Potentially | attribute.confidentiality.data_disclosure.Yes) %>% filter(source_id != "vcdb")  # filter dbir
vcdb_healthcare <- vcdb %>% filter(victim.industry2.62 | attribute.confidentiality.data.variety.Medical | grepl('Patient', attribute.confidentiality.data_victim)) %>% filter(attribute.confidentiality.data_disclosure.Potentially | attribute.confidentiality.data_disclosure.Yes) # filter VCDB
vcdb_vz_names <- intersect(names(vz_healthcare), names(vcdb_healthcare))  # find the common names
vz_healthcare <- vz_healthcare %>% select(one_of(vcdb_vz_names)) # select common dbir columns
vcdb_healthcare <- vcdb_healthcare %>% select(one_of(vcdb_vz_names))  # select common vcdb columns
healthcare <- rbind(vcdb_healthcare, vz_healthcare)  # Join the data frames
class(healthcare) <- c("verisr", class(healthcare))  # cast it back to verisr
healthcare <- healthcare %>% unique(.)  # remove duplicate rows