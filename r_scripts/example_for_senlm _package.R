


                  # Code from the paper:
  # Non-linear models of species' responses to environmental and spatial gradients


# Rcode_S1.txt
#
# R code associated with the vignette to demonstrate the R package ‘senlm’.
#

###########################################################################################
#
#   1. Introduction and
#   2. Fitting a model using senlm
#
# Install the senlm package
# (Note that the following 2 lines do not need to be run again once the package has been installed)
install.packages("devtools")
devtools::install_github("PRIMER-e/senlm")

# Load the senlm package
library("senlm")

# Data: an excerpt from the NOAA groundfish trawl data consisting of 20 years of data (1999 - 2018) for 310 species 
fish <- read.csv(file.choose(), row.names = 1 ) # The file is called "Data_S1.csv"
head(fish)

# For simplicity in what follows, rename the depth variable in the data frame:
colnames(fish)[5] <- "depth"

# For starters, let's focus on a single species: Sebastolobus alascanus, also removing any rows with 'NA',
# and further simplifying the column names to "x" and "y"
dat1 <- na.omit(subset(fish, select = c(depth,Sebastolobus.alascanus) ) )
colnames(dat1) <- c("x", "y")

# Consider a scatterplot
plot(dat1$x, dat1$y, las = 1, xlab = "Depth (m)", ylab = "Count per trawl", ylim = c(0,800))
mygrey <- grey(level = 0.65, alpha = 0.4)
points(dat1$x, dat1$y, pch = 21, col = mygrey, bg = mygrey) 

# An asymmetric unimodal response, with large numbers of zeros, is readily apparent.

# Let's fit one of the senlm models (sech + zinb) and see what it looks like.
sech.zinb <- senlm(data = dat1, xvar = "x", yvar = "y",
                   mean_fun = "sech", err_dist = "zinb") 
# This might take a minute, due to the sheer size of the data file... (over 5000 rows)

# The summary shows the estimated model parameters and information criteria for this model
summary(sech.zinb)

# We can also get 95% confidence intervals on the parameters (estimated using the Fisher information matrix):
# (Note: occasionally, due to numerical instability of the estimated Hessian matrix,
#  the standard errors are not calculated.)
rbind(lower=sech.zinb$lb, upper=sech.zinb$ub)

###########################################################################################
#
#   3. Plotting results
#

# A default plot can be drawn right away from the model object to get a visualisation of the fitted mean function
plot(sech.zinb)

# We can make this quite a bit prettier plot with a few tweaks, including changing the scale to sqrt, if we wish...
plot(dat1$x, sqrt(dat1$y), las = 1, yaxt = "n", xlab = "Depth (m)", ylab = "Count per trawl", ylim = c(0,sqrt(800)))
mygrey <- grey(level = 0.65, alpha = 0.4)
points(dat1$x, sqrt(dat1$y), pch = 21, col = mygrey, bg = mygrey) 
axis(side = 2, at = sqrt(seq(0,800, by = 100)), labels = seq(0,800, by = 100), las = 1 ) 

# add the fitted line:
predict.x <- seq( from=min(dat1$x), to = max(dat1$x), length.out = 100000 )
yfit.sech <- predict(sech.zinb, predict.x)
lines(predict.x, sqrt(yfit.sech), col = "#224188", lwd = 2)

# add on the estimated values for m and H for this model
m = sech.zinb$theta["m"] ; H = sech.zinb$theta["H"]
axis(side = 1, at = m, label = "m", col.axis = "red", col.ticks = "red")
lines(x = c(m, m), y = c(-100, sqrt(H)), col = "red", lwd = 2, lty = "dashed") 
lines(x = c( -100, m), y = c(sqrt(H), sqrt(H)), col = "red", lwd = 2, lty = "dashed")
text (x = 0, y = sqrt(120), "H", col = "red")

# The line shows the mean for the NB distribution in the ZINB mixture model, which excludes excess zeros.
# We can also obtain values for the expectation of the response variable that includes excess zeros, thus:
theta <- sech.zinb$theta
E.y <- (1 - theta["pi"]) * yfit.sech
lines(predict.x, sqrt(E.y), col = "#FFA500", lwd = 2)

# A legend:
mylines = c("excluding excess zeros", "including excess zeros")
legend(x = 700, y = sqrt(800), title = "Mean abundance", cex = 0.8,
       legend = mylines, lwd = 2, col = c("#224188","#FFA500"))

# In this case, we only have about a 7% reduction in the mean.
# Our estimated expectation for the number of individuals per trawl at the modal depth is:
# expected mean abundance per trawl at m
H.y <- (1 - theta["pi"]) * theta["H"]

# 95% CI for this
H.y.low <- (1 - theta["pi"]) * sech.zinb$lb["H"]
H.y.up <- (1 - theta["pi"]) * sech.zinb$ub["H"]

# result
EY.at.m <- c(H.y, H.y.low, H.y.up)
names(EY.at.m) <- c("expected.val","95% lower","95% upper")
EY.at.m


###########################################################################################
#
#   4. Comparing model fits visually
#

# Let's consider what a quadratic GLM model with ZINB errors would look like for these data.
library(pscl) # has zeroinfl() function
glm.zinb <- zeroinfl(data = dat1, y ~ x + I(x^2) | 1, dist = "negbin")
summary(glm.zinb)

# Compare the AIC with what we get for the sech.zinb model
AIC(glm.zinb)

# What about a Gaussian curve with NB errors?
gauss.nb <- senlm(data = dat1, xvar = "x", yvar = "y",
                  mean_fun = "gaussian", err_dist = "negbin") 
summary(gauss.nb) # note that the AIC here is also not as good...

# Let's re-draw the plot and put fitted curves on it for all three models:
# Start with the previous code for the (sqrt-scaled) plot (without the red lines showing H and m)
plot(dat1$x, sqrt(dat1$y), las = 1, yaxt = "n", xlab = "Depth (m)", ylab = "Count per trawl", ylim = c(0,sqrt(800)))
mygrey <- grey(level = 0.65, alpha = 0.4)
points(dat1$x, sqrt(dat1$y), pch = 21, col = mygrey, bg = mygrey) 
axis(side = 2, at = sqrt(seq(0,800, by = 100)), labels = seq(0,800, by = 100), las = 1 ) 

# Now for the fitted lines:
mycols = c("#224188","#CC6677","#882255")

# Add the fitted line first for the sech model:
predict.x <- seq( from=min(dat1$x), to = max(dat1$x), length.out = 100000 )
yfit.sech <- predict(sech.zinb, predict.x)
lines(predict.x, sqrt(yfit.sech), col = mycols[1], lwd = 2)

# Add to this the quadratic GLM + ZINB errors.  
yfit.glm <- predict(glm.zinb, type = "response")
glmx <- dat1$x[order(dat1$x)]; glmy <- yfit.glm[order(dat1$x)]
lines(glmx, sqrt(glmy), col = mycols[2], lwd = 2)

# And then the Gaussian model with NB errors
yfit.gauss <- predict(gauss.nb, predict.x)
lines(predict.x, sqrt(yfit.gauss), col = mycols[3], lwd = 2)

# A legend:
mods = c("sech + zinb", "GLM, quadratic + zinb", "Gauss + nb")
legend(x = 800, y = sqrt(800), legend = mods, lwd = 2, col = mycols, cex = 0.8) 

###########################################################################################
#
#   5. Comparing senlm models for a given data type
#
# Which mean function and error distribution combination(s) would be most appropriate to use for these data?
# In this example, we will consider a different species, Antimora microlepis.
# To help speed calculations, we will also take a subset of data; specifically, for the years from 1999-2004. 
# Here is our new dataframe
dat2 <- na.omit(subset(fish, year <=2004, select = c(depth, Antimora.microlepis) ) )
colnames(dat2) <- c("x", "y")

# First, examine a simple scatter plot (always a good idea!)
plot(dat2$x, dat2$y, xlab = "Depth(m)", ylab = "Count per trawl", las=1, ylim = c(0,80) )
mygrey <- grey(level = 0.65, alpha = 0.4)
points(dat2$x, dat2$y, pch = 21, col = mygrey, bg = mygrey) 

# We can detect a bit of bi-modality here. The "trough" in the middle (around 600-800m) could correspond to an "oxygen minimum zone".
# Let's create a list of some reasonable possible models for count data (note: there are more...).
count.models <- set_models(mean_fun = c("sech","hofV","mixgaussian"), err_dist = c("poisson","zip","zipl","negbin","zinb","zinbl") )
count.models[,1:2]

# Next, let's fit all of these potential models (using the "msenlm" function) and then see, in a summary,
# which ones do best by reference to information criteria (Note: the following line will probably take a few minutes to run...)
fits <- msenlm(models = count.models, data = dat2, xvar = "x", yvar = "y")
multi <- summary(fits)

# We can put these results in order of increasing values of (say) AICc, so the "best" models are at the top of the list.
multi[order(multi$AICc),c(4,5,6,24:28)]

# Looks like the mixed gaussian mean function, with zinb errors, is the preferred model here (out of this possible suite).
# Here is a summary of that model
mixg.zinb <- senlm(data = dat2, xvar = "x", yvar = "y", mean_fun = "mixgaussian", err_dist = "zinb") 
summary(mixg.zinb)

# We can add this fitted model to the plot (excluding excess zeros)
predict.x <- seq( from=min(dat2$x), to = max(dat2$x), length.out = 100000 )
yfit.mixg.zinb <- predict(mixg.zinb, predict.x)
lines(predict.x, yfit.mixg.zinb, col = "#00BFC4", lwd = 2)
legend(x = 20, y = 80, legend = "mix.gauss + zinb", lwd = 2, col = "#00BFC4", cex = 0.8) 

# Also, for any given error distribution, the mixed gaussian mean function generally does better than other choices.

# It is interesting (and important to note) that if we use different error structures, then we can get very different fitted model curves,
# even if we use the same mean function...
# For example, let's look visually at what we would get using the mixed gaussian function with different error distributions for these data...

# For clarity, here are our five additional models:
mixg.zinbl <- senlm(data = dat2, xvar = "x", yvar = "y", mean_fun = "mixgaussian", err_dist = "zinbl") 
mixg.negbin <- senlm(data = dat2, xvar = "x", yvar = "y", mean_fun = "mixgaussian", err_dist = "negbin") 
mixg.zipl <- senlm(data = dat2, xvar = "x", yvar = "y", mean_fun = "mixgaussian", err_dist = "zipl") 
mixg.zip <- senlm(data = dat2, xvar = "x", yvar = "y", mean_fun = "mixgaussian", err_dist = "zip") 
mixg.pois <- senlm(data = dat2, xvar = "x", yvar = "y", mean_fun = "mixgaussian", err_dist = "poisson") 

# Let's get fitted values for these other five models:
yfit.mixg.zinbl <- predict(mixg.zinbl, predict.x)
yfit.mixg.negbin <- predict(mixg.negbin, predict.x)
yfit.mixg.zipl <- predict(mixg.zipl, predict.x)
yfit.mixg.zip <- predict(mixg.zip, predict.x)
yfit.mixg.pois <- predict(mixg.pois, predict.x)

# Re-drawing the plot
# Start again with the previous code for the (sqrt-scaled) plot (without the red lines showing H and m)
plot(dat2$x, dat2$y, xlab = "Depth(m)", ylab = "Count per trawl", type = "n", las=1, ylim = c(0,80) )
mygrey <- grey(level = 0.65, alpha = 0.4)
points(dat2$x, dat2$y, pch = 21, col = mygrey, bg = mygrey) 

# Add the fitted lines with colours of your choice...
mycols2 <- c("#F8766D","#00BFC4","#619CFF","#C77CFF","#00BA38","#CD9600")
lines(predict.x, yfit.mixg.zinbl, col = mycols2[1], lwd = 2)
lines(predict.x, yfit.mixg.zinb, col = mycols2[2], lwd = 2)
lines(predict.x, yfit.mixg.negbin, col = mycols2[3], lwd = 2)
lines(predict.x, yfit.mixg.zipl, col = mycols2[4], lwd = 2)
lines(predict.x, yfit.mixg.zip, col = mycols2[5], lwd = 2)
lines(predict.x, yfit.mixg.pois, col = mycols2[6], lwd = 2)

# And the legend:
mods = c("zinbl","zinb", "negbin", "zipl", "zip", "pois")
legend(x = 20, y = 80, legend = mods, lwd = 2, col = mycols2) 

# Note: the fitted mean functions for ZINB and negbin are virtually identical.
# They cannot be distinguished from one another on the plot.
#
# It is clear that, for these data, the use of either of the linked error distributions (zinbl or zipl)
# resulted in failure to detect the bimodal nature of the mean function. In other words, a poor choice for
# the error structure can result in counter-intuitive fitted mean functions,
# potentially missing important features of the data in subsequent summaries/plots.

###########################################################################################
#
#   6. Visualising error structures on plots
#
# Consider the the above model for Antimora microlepis
# How can we visualise the error structure?
# A plot showing the quantiles may be helpful here.

# Load the ZIM library
# This is useful for obtaining quantile functions for zero-inflated distributions
library(ZIM) 

# Consider the model comprised of the mixed gaussian mean function, with zinb errors.
# We already have estimates of "mu" for the negative binomial model in the mixture.
# These are provided as fitted values from the model at each position along the gradient.

mu = yfit.mixg.zinb

# We also have all of the parameter estimates from the model here:
theta = mixg.zinb$theta

# We can generate the quantile values for the full zinb model for each of these positions as well.
# First specify quantiles of interest.
quants <- c( seq(0.1,0.9, by = 0.1), 0.95 )

# Choose some colours.
mycols3 <- c("firebrick1","indianred1","orange1","olivedrab1","seagreen3",
             "turquoise","royalblue","slateblue","violet","violetred3") 

# Create a matrix where quantiles will be held
my.quants <- matrix(rep(NA, length(quants)*length(predict.x)), ncol = length(quants), nrow = length(predict.x) )

# Let's get the quantiles for every value of mu along the gradient
for (ix in 1:length(predict.x) ) {
  my.quants[ix,] = qzinb(quants, k = theta["phi"], lambda = mu[ix],
                         omega = theta["pi"], lower.tail = TRUE, log.p = FALSE)
  
}

# And our plot with the mean function looks like this:
plot(dat2$x, dat2$y, las = 1, xlab = "Depth (m)", ylab = "Count per trawl",
     ylim = c(0,80) )
mygrey <- grey(level = 0.65, alpha = 0.4)
points(dat2$x, dat2$y, pch = 21, col = mygrey, bg = mygrey) 
lines(predict.x, mu, col = "black", lwd = 2)

# Now add the quantiles from the zinb distribution onto the plot...

for (i in 1:length(quants)) {
  lines(predict.x, my.quants[,i], col = mycols3[i], lwd = 2)
}

# And add a legend
legend.names <- paste(".", c(seq(10,90,by = 10), 95), sep = "")
legend(x = 0, y = 80, legend = legend.names, lwd = 2, col = mycols3) 




