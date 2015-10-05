DESCRIPTION <- "How to use association rules to understand what drives a subset of the data"

require(arules)

# Topic to find out about. (In this case, incidents discovered in years or more)
# Subset to columns containing 'variety', 'vector', and our topic.  This gets us 4A's and some other columns
# Replace "timeline.discovery.unit.Years" with your topic
chunk <- vcdb %>% select(contains("variety"), contains("vector"), timeline.discovery.unit.Years) %>% as.data.frame()

#  Create the association rules (replace "timeline.discovery.unit.Years" with your topic)
rules <- chunk %>% apriori(appearance = list(lhs = c("timeline.discovery.unit.Years"), default="rhs"), parameter=list(support=0.0, confidence=0.25))

# Look at the rules you created.  (If it's too long, do rules[1:10] or such)
inspect(rules)

# for more information, look at our blog on association rules and the DBIR at https://securityblog.verizonenterprise.com/?p=6692