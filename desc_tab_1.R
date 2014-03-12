# Descriptive data ####
# Want n schools w/ meaningful stunting, n schools w/ meaningful underweightness
# Some spatial distribution. - Distribution of dichotomised outcome
# WHO underweight indicator suggests <10% low prev, 10 - 19 % medium prev, 20-29 high, >30 very high
# Stunting: <20 % low prev, 20-29 medium, 30-39 high, >40 very high prevalence
df$stun.class <- NA
df$stun.class[df$stunted < 0.2] <- "Low (\\textless 20 \\%)"
df$stun.class[df$stunted >= 0.2 & df$stunted <0.3] <- "Medium (20 - 29 \\%)"
df$stun.class[df$stunted >= 0.3 & df$stunted <0.4] <- "High (30 - 39 \\%)"
df$stun.class[df$stunted >= 0.4] <- "Very high (\\ge 40 \\%)"
df$stun.class <- factor(df$stun.class, 
                        levels = c("Low (\\textless 20 \\%)", "Medium (20 - 29 \\%)", 
                                   "High (30 - 39 \\%)", "Very high (\\ge 40 \\%)"))
table(df$stun.class, useNA = "ifany")

df$under.class <- NA
df$under.class[df$underweight < 0.1] <- "Low (\\textless 10 \\%)"
df$under.class[df$underweight >= 0.1 & df$underweight < 0.2] <- "Medium (10 - 19 \\%)"
df$under.class[df$underweight >= 0.2 & df$underweight < 0.3] <- "High (20 - 29 \\%)"
df$under.class[df$underweight >= 0.3] <- "Very high (\\ge 30 \\%)"
df$under.class <- factor(df$under.class, 
                         levels = c("Low (\\textless 10 \\%)", "Medium (10 - 19 \\%)", 
                                    "High (20 - 29 \\%)", "Very high (\\ge 30 \\%)"))
table(df$under.class, useNA = "ifany")

stunt <- ddply(df, .(stun.class), summarise, stunt = length(sch_id), 
               n = length(df$sch_id))
stunt$pc <- round(binom.confint(stunt$stunt, stunt$n, methods = "exact")$mean*100, 2)
stunt$lci <- round(binom.confint(stunt$stunt, stunt$n, methods = "exact")$lower*100, 2)
stunt$uci <- round(binom.confint(stunt$stunt, stunt$n, methods = "exact")$upper*100, 2)
stunt$var <- "Prevalence stunting"
names(stunt)[1] <- "class"
names(stunt)[2] <- "out"
stunt

under <- ddply(df, .(under.class), summarise, under = length(sch_id), 
               n = length(df$sch_id))

under$pc <- round(binom.confint(under$under, under$n, methods = "exact")$mean*100, 2)
under$lci <- round(binom.confint(under$under, under$n, methods = "exact")$lower*100, 2)
under$uci <- round(binom.confint(under$under, under$n, methods = "exact")$upper*100, 2)
under$var <- "Prevalence underweight"
names(under)[1] <- "class"
names(under)[2] <- "out"
under

both <- rbind(stunt, under)
both$n <- NULL
both <- both[,c(6,1,2,3,4,5)]
both$var[2:4] <- ""
both$var[6:8] <- ""
both$per.cent <- paste(sprintf("%.2f", both$pc), " (", sprintf("%.2f", both$lci),
                       " - ", sprintf("%.2f", both$uci), ")", sep = "")
both$pc <- NULL
both$lci <- NULL
both$uci <- NULL
names(both) <- c("Outcome", "Group", "n schools", "Per cent of schools")
both

out <- xtable(both, 
              caption = "Distribution of the prevalence of stunted growth and wasting in schools in coastal regions of Kenya, DATE", 
              label = "tab1")
digits(out)[4] <- 0
print(out, booktabs = TRUE)