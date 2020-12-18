#load the necessary libraries
library(Matching)
library(xtable)
library(gridExtra)
library(cobalt)
library(rbounds)

#see the first 5 rows of the dataset
head(match.data)

#confirm there are no null values
colSums(is.na(match.data))

#check the structure of the dataset
str(match.data)

head(genmatch.covar)

#replication code, table for genetic matching
match.pctVV <- Match(Y= match.data$pctVV, Tr = match.data$treat, X = genmatch.covar, Weight.matrix=genmatch.output, BiasAdjust=FALSE, caliper = c(rep(10,43),.5))
match.elected <- Match(Y= match.data$elected, Tr = match.data$treat, X = genmatch.covar, Weight.matrix=genmatch.output,  caliper = c(rep(10,43),.5), BiasAdjust=FALSE)
bal.data <- with(match.data, data.frame(match.party, male, occ, edu, yob, lat, long, ran.prior, incumbent, log.valid.votes, prior.pctVV, party.prior.pctVV, elec.year, log.total.assets, gini_2000, pt_pres_1998, income_2000, psdb_2000, pt_2000, hdi_2000, uf.ba, uf.sp, uf.mg, uf.rs, log.num.apps))
bal.fmla <- as.formula(paste("treat~",paste(names(bal.data),collapse="+")))
match.bal <- MatchBalance(bal.fmla, data = data.frame(treat = match.data$treat, bal.data), match.out = match.pctVV,nboots=3000, print.level=0)
before.sd.diff <- sapply(match.bal$BeforeMatching, function(x) x[[1]])
after.sd.diff <- sapply(match.bal$AfterMatching, function(x) x[[1]])
before.t.p <- matrix(sapply(match.bal$BeforeMatching, function(x) x$tt[3]))
after.t.p <- matrix(sapply(match.bal$AfterMatching, function(x) x$tt[3]))
before.ks.p <- matrix(sapply(match.bal$BeforeMatching, function(x) x$ks$ks.boot.pvalue))
after.ks.p <- matrix(sapply(match.bal$AfterMatching, function(x) x$ks$ks.boot.pvalue))
bal.stats <- data.frame(var = names(data.frame(model.matrix(as.formula(paste("~",paste(names(bal.data),collapse="+"))), bal.data)[,-1])), before.sd.diff, after.sd.diff, before.t.p = unlist(before.t.p), after.t.p = unlist(after.t.p), before.ks.p, after.ks.p)
bal.stats <- bal.stats[c(4,6,12,15,18:27,29:49),]
rownames(bal.stats)  <- c("Party: PFL", "Party: PMDB", "Party: PSDB", "Party: PT", "Male", "Occupation: Blue Collar", "Occupation: Education", "Occupation: Government", "Occupation: Media", "Occupation: None", "Occupation: Other", "Occupation: Politician", "Occupation: White Collar", "Education: Some Superior or More", "Year of Birth", "Latitude", "Longitude", "Ran Previously", "Incumbency", "Log Electorate", "Prior Vote Share", "Party Prior Vote Share", "Election Year", "Total Assets", "2000 Gini", "PT Pres Vote Share (1998)", "GDP Per Capita (2000)", "PSDB Mayor Vote Share (2000)", "PT Mayor Vote Share (2000)", "HDI (2000)", "State: Bahia", "State: Sao Paulo", "State: Minas Gerais", "State: Rio Grande do Sul", "Log Number of Applications")
bal.stats$before.ks.p[bal.stats$before.ks.p=="NULL"] <- NA
bal.stats$after.ks.p[bal.stats$after.ks.p=="NULL"] <- NA
bal.stats$before.ks.p <- unlist(bal.stats$before.ks.p)
bal.stats$after.ks.p <- unlist(bal.stats$after.ks.p)
bal.stats <- bal.stats[order(abs(bal.stats$before.sd.diff), decreasing = TRUE),]
bal.stats <- bal.stats[,-1]
bal.check.table <- xtable(bal.stats, display=c("s",rep("fg",6)), caption = "Balance Statistics")
digits(bal.check.table) <-2
align(bal.check.table) <- "r|rrrrrr"
print(bal.check.table,latex.environment = "center", include.rownames = TRUE,hline.after=c(0,nrow(bal.check.table)),floating.environment = "sidewaystable")

sprintf("The smallest p-value after matching is: %s", match.bal$AMsmallest.p.value)
sprintf("The variable with smallest p-value after matching is: %s", match.bal$AMsmallestVarName)
sprintf("The estimate of the treatment effect is: %s", match.pctVV$est)

#Balance tables
#check the balance of some of the covariates before and after matching
occ_bal <- bal.plot(match.pctVV , "match.partyPCB", treat = match.data$treat, covs = genmatch.covar, type = "histogram", mirror = TRUE, which = "both")
num_app_bal <- bal.plot(match.pctVV , "log.num.apps", treat = match.data$treat, covs = genmatch.covar, type = "histogram", mirror = TRUE, which = "both")
assets_bal <- bal.plot(match.pctVV , "log.total.assets", treat = match.data$treat, covs = genmatch.covar, type = "histogram", mirror = TRUE, which = "both")
educ_bal <- bal.plot(match.pctVV , "occEducation", treat = match.data$treat, covs = genmatch.covar, type = "histogram", mirror = TRUE, which = "both")

#display plots in a grid
grid.arrange(occ_bal, num_app_bal, assets_bal, educ_bal, nrow = 2)

#perform a sensitivity analysis
psens(match.pctVV , Gamma=1.5, GammaInc=.1)
