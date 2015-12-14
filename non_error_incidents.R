DESCRIPTION <- "Select incidents that are clearly malicious but not necessarily breaches of confidentiality."

chunk <- vz %>% filter(!`pattern.Lost and Stolen Assets`, !`pattern.Miscellaneous Errors`)