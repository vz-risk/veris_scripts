DESCRIPTION = "Add an actor.Collusion column to the data."

vz <- vz %>% mutate(actor.Collusion = actor.External + actor.Internal + actor.Partner > 1)

vz[vz$actor.Unknown == T, "Actor"] <- "Unknown"
vz[vz$actor.Partner == T, "Actor"] <- "Partner"
vz[vz$actor.Internal == T, "Actor"] <- "Internal"
vz[vz$actor.External == T, "Actor"] <- "External"
vz[vz$actor.Collusion == T, "Actor"] <- "Collusion"

chunk <- vz %>% 
  filter(Actor != "Unknown") %>% 
  group_by(Actor) %>% 
  summarize(x = n()) %>% 
  ungroup() %>% 
  mutate(n = sum(x)) %>% 
  mutate(freq = x/n)

# Order as desired
chunk <- chunk %>% mutate(Actor = factor(Actor, levels=c("External", "Collusion", "Internal", "Partner"))) %>% arrange(Actor)

# add freq as a pretty percentage
chunk <- chunk %>% mutate(pct = paste0(round(freq * 100),"%"))

# add position for text labels
chunk <- chunk %>% mutate(pos = cumsum(freq) - (0.5 * freq))

gg <- chunk %>% ggplot()
gg <- gg + aes(x="Actor", y=freq, fill=Actor)
gg <- gg + geom_bar(stat="identity", position="stack", width=.3)
gg <- gg + geom_text(aes(label = pct, y = pos), size = 6)
gg <- gg + coord_flip()
gg <- gg + theme_bw()
gg <- gg + labs(x=NULL, y=NULL)
gg <- gg + scale_y_continuous("", expand=c(0,0))
gg <- gg + scale_x_discrete("", expand=c(0,0))
gg <- gg + theme(
  panel.border = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  panel.grid.major = element_blank()
)
gg <- gg + scale_fill_manual(values=c("#0291A5", "#4FAC95", "#9BC684", "#FEE18B"))
gg