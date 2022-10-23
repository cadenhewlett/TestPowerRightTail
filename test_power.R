###############################################################################################################
# This program contains calculation and, importantly, visualization of a Hypothesis test of Two Means
# In this setting, we use the hypothesis that U_A > U_0, i.e. the alternative is greater than the null.
# 1.	Setup: Sets the Working Directory, default Plot Settings and imports libraries.
# 2.	Finding Power and Beta: Fairly self-explanatory; however, in this setting we test at alpha = 0.05.
# 3.  Visualization: Using Pre-determined ranges, use the curve() funciton to plot the distribution and 
#     the polygon function for the filled-in areas. Finally, uses a LaTeX library for on-plot text and Legend.
# Hewlett, Caden 2021-03-19
############################################################################################################

# ------------------------------- 1: Setup -------------------------------------------
library(latex2exp)

#set up par
par(
  mfrow = c(1, 1),
  #mfcol fills column-wise
  bg = "grey95",
  #background colour
  cex.main = 1.2,
  #chracter expansion of titles by 20%
  cex.lab = 1.1,
  col.main = "black",
  #change title colours
  family = "serif",
  #font family is "serif"
  xaxs = "i",
  # i means 'internal' x-axis
  col.axis = "grey45",
  #axis colour
  fg = "grey40" #foreground colour (not sure if this works)
)

#------------------------------- 2: Finding Power and Beta -------------------------------------------

# Let's create some theoretical Normal Characteristics.
sigma = 15 #theoretical population s.d. of this distribution
mu_O = 100 # Population Mean 
mu_A = 130 # Alternative Mean
alpha = .05 # Test mu_A > mu_O  (mu_O < mu_A) at this level.

# Find the Critical Value (assuming standard Z-Test for Means)
critical = qnorm((1-alpha), mu_O, sigma)

# Calculate the Power: Probability of all Values Greater than the Critical Value
# Under the distribution garnered by Ha (i.e. with mu_A and sigma)
power = pnorm(q = critical, mu_A, sigma, lower.tail = FALSE)

#Determine the probability of a Type II error under these parameters.
beta = 1 - power

#------------------------------- 3: Visualize the Results -------------------------------------------
x_limit = c(50, 180)
left = seq(x_limit[1], critical, length.out = 100) #100 vals between 50 and the critical point
right = seq(critical, x_limit[2], length.out = 100) #from critical to top limit

#Calculate Densities
y_HO_right = dnorm(right, mu_O, sigma) #density curve starting from right section
y_HA_left = dnorm(left, mu_A, sigma) #see above, but for HA and it's mean
y_HA_right = dnorm(right, mu_A, sigma) #see above, but right instead of left

#distribution of The Null Mean 
curve(dnorm(x, mu_O, sigma), #normal density with Null mean
      xlim = x_limit, 
      lwd = 2, col = "red", 
      xlab = "X-Value", ylab = "Density",
      main = "Normal Distribution Under Null and Alternative Hypotheses",
      ylim = c(0, 0.03), xaxs = "i")

#distribution of the Alternative Mean
curve(dnorm(x, mu_A, sigma), #normal density with Alternative mean.
      xlim = x_limit, col = 'blue', lwd = 2,
      ylim = c(0, 0.03), add = TRUE)


polygon(c(right, rev(right)), c(y_HA_right, numeric(length(right))), border=NA,
        density=5, lty=2, lwd=2, angle=45, col="darkgray")

text(critical+3,  0.01,  adj=0, label="Power", cex=1.2)

#Filled-in Areas

#Alpha: area to the right of the critical point in Null Distribution
polygon(c(right, rev(right)), #mess around with 'right', glue it to itself flipped
        c(y_HO_right, numeric(length(right))), #then set y to be  the distribution, but also a series of 0s n in size
        border = NA, #remove border (dist. already has it)
        col=rgb(1, 0.3, 0.3, 0.5) #set nice transparent red
        )

#Beta: area to the left of the critical point in the Alternative Distribution
polygon(c(left, rev(left)), #mess around with 'right', glue it to itself flipped
        c(y_HA_left, numeric(length(left))), #then set y to be  the distribution, but also a series of 0s n in size
        border = NA, #remove border (dist. already has it)
        col=rgb(0.3, 0.3, 0.8, 0.4) #set nice transparent red
)

#Labels for Alpha and Beta
text(x = (critical + 5),  0.0015, TeX('$\\alpha$'), col = 'grey35', cex = 1.4)
text(x = (critical - 12), 0.004,  TeX('$\\beta$'), col = 'grey35', cex = 1.4)


#Line for the critical point
abline(v = critical, lwd = 2, col = 'darkred', lty = 'dashed')

#write TeX equations for our 2 Distributions
tex_titles = c(TeX('$X \\sim N(\\mu_0, \\, \\sigma^2)\\;$'), 
                TeX('$Y \\sim N(\\mu_A, \\, \\sigma^2)\\;\\;$'))

#Create a Legend with this and the other stuff
legend("topright",
       legend = c(tex_titles, "Critical Point"), #legend is our column names and avg.
       col = c('red', 'blue', 'darkred'), #alternating colours
       lty = c(rep(1, 2), 2), #1 for straight lines, 2 for dashed
       lwd = c(rep(2, 2), 3), #setting parameters
       bg = 'grey90',
       box.col = 'black', #box parameters
       box.lwd = 1.3,
       box.lty = 'dashed',
       text.col = "grey20",
       text.font = 2,
       horiz = F ,
       inset = c(0.01, 0.01)
)

