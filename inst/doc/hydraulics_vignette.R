## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(hydraulics)

## ----waterprops-1-------------------------------------------------------------
nu = kvisc(T = 55, units = 'Eng')
cat(sprintf("Kinematic viscosity: %.3e ft2/s\n", nu))

## ----waterprops-2-------------------------------------------------------------
rho = dens(T = 25, units = 'SI')
cat(sprintf("Water density: %.3f kg/m3\n", rho))

## ----waterprops-3, out.width="50%"--------------------------------------------
Ts <- seq(0, 100, 10)
nus <- kvisc(T = Ts, units = 'SI')
xlbl <- expression("Temperature, " (degree*C))
ylbl <- expression("Kinematic viscosity," ~nu~ (m^{2}/s))
par(cex=0.8, mgp = c(2,0.7,0))
plot(Ts, nus, xlab = xlbl, ylab = ylbl, type="l")

## ----waterprops-4-------------------------------------------------------------
T <- 25
Dens <- dens(T = T, units = 'SI', ret_units = TRUE)
Dvisc <- dvisc(T = T, units = 'SI', ret_units = TRUE)
#Calculate kinematic viscosity and units are generated correctly
Kvisc <- Dvisc / Dens
Kvisc

## ----waterprops-5-------------------------------------------------------------
units::set_units(Kvisc, m^2/s)
units::set_units(Kvisc, ft^2/s)

## ----waterprops-6-------------------------------------------------------------
vps <- svp(T = 10, units = "SI", ret_units = T)
vps
#convert to psi - notice the need to enclose "in" with backticks since "in" 
#has other meanings in R
units::set_units(vps,lbf/`in`^2)

## ----waterprops-7, out.width="50%"--------------------------------------------
Temperature <- units::set_units(seq(0, 100, 10), degree_Celsius)
Kinematic_Viscosity <- kvisc(T = Temperature, units = 'SI', ret_units = TRUE)
par(cex=0.8, mar = par("mar") + c(0, .2, 0, 0))
plot(Temperature, Kinematic_Viscosity, type="l")

## ----constants-1, echo=FALSE--------------------------------------------------
knitr::kable(data.frame(Fr=c("<1.0","=1.0",">1.0"), Condition=c("subcritical","critical","supercritical")), format="pipe", padding=0)

## ----pipe-1-------------------------------------------------------------------
D <- 20/12    #20 inch converted to ft
L <- 10560    #ft
Q <- 4        #ft^3/s
T <- 60       #degrees F
ks <- 0.0005  #ft

ans <- darcyweisbach(Q = Q,D = D, L = L, ks = ks, 
                     nu = kvisc(T=T, units="Eng"), units = c("Eng"))
cat(sprintf("Reynolds no: %.0f\nFriction Fact: %.4f\nHead Loss: %.2f ft\n", 
            ans$Re, ans$f, ans$hf))

## ----pipe-2, message=FALSE, warning=FALSE-------------------------------------
ans <- darcyweisbach(Q = 4.0,D = 20/12, L = 10560, ks = 0.0005, nu = kvisc(T=T, units="Eng"),
                     units = "Eng", ret_units = TRUE)
knitr::kable(format(as.data.frame(ans), digits = 2), format = "pipe")

## ----pipe-3-------------------------------------------------------------------
Q <- 37.5     #flow in ft^3/s
L <- 8000     #pipe length in ft
hf <- 215     #head loss due to friction, in ft
T <- 68       #water temperature, F
ks <- 0.0008  #pipe roughness, ft
ans <- darcyweisbach(Q = Q, hf = hf, L = L, ks = ks, nu = kvisc(T=T, units='Eng'), units = c('Eng'))
cat(sprintf("Reynolds no: %.0f\nFriction Fact: %.4f\nDiameter: %.2f ft\n", ans$Re, ans$f, ans$D))

## ----pipe-4, message=FALSE, out.width="50%"-----------------------------------
Qs <- seq(30, 45, 1.0)    #flows in ft^3/s
L <- 8000                 #pipe length in ft
hf <- 215                 #head loss due to friction, in ft
T <- 68                   #water temperature, F
ks <- 0.0008              #pipe roughness, ft
ans <- mapply(darcyweisbach, Q=Qs, MoreArgs = 
                 list(hf = hf, L = L, ks = ks, nu = kvisc(T=T, units='Eng'), units = 'Eng'))
ans <- as.data.frame(t(ans))
plot(ans$Q, ans$D, xlab = "Q, ft^3/s", ylab = "D, ft", type="l")
grid()

## ----pipe-5, echo=FALSE-------------------------------------------------------
knitr::kable(data.frame(Q_liter_s=c("0.20","0.24","0.30"), Headloss_m=c("0.052","0.073","0.110")), format="pipe", padding=0)

## ----pipe-6, message=FALSE, fig.width = 5, fig.asp = .62----------------------
Qs = c(0.00020, 0.00024, 0.00030) #converted to m^3/s
hfs <- c(0.052,0.073,0.110)
ans <- mapply(darcyweisbach, Q=Qs, hf=hfs, MoreArgs = 
                 list(L = 3.0, D = 0.025, nu = kvisc(T=20, units='SI'), units = 'SI'))
ks_values = unlist((as.data.frame(t(ans)))$ks)
cat(round(ks_values,6))
cat(paste0("\nMean Roughness, ks = ",round(mean(ks_values),6), " m"))
Re_values <- unlist((as.data.frame(t(ans)))$Re)
f_values <- unlist((as.data.frame(t(ans)))$f)
moody(Re = Re_values, f = f_values)

## ----manningc-1, message=FALSE, warning=FALSE---------------------------------
ans <- manningc(Q=0.01, n=0.013, Sf=0.001, d = 0.2, units="SI", ret_units = TRUE)
knitr::kable(format(as.data.frame(ans), digits = 2), format = "pipe", padding=0)

## ----manningc-2, message=FALSE, warning=FALSE, out.width="40%"----------------
ans <- manningc(Q=0.01, n=0.013, Sf=0.001, d = 0.2, units="SI", ret_units = TRUE)
xc_circle(y = ans$y, d=ans$d, units = "SI")

## ----manningt-1---------------------------------------------------------------
ans <- manningt(Q = 360., n = 0.015, m = 1, b = 20.0, y = 3.0, units = "Eng")
cat(sprintf("Slope: %.5f ft\n", ans$Sf))
knitr::kable(format(as.data.frame(ans), digits = 2), format = "pipe", padding=0)

## ----manningt-2---------------------------------------------------------------
ans <- manningt(Q = 360., n = 0.015, m = 1, b = 20.0, y = 3.0, units = "Eng", ret_units = TRUE)
knitr::kable(format(as.data.frame(ans), digits = 2), format = "pipe", padding=0)

## ----manningt-2.1, message=FALSE, warning=FALSE, fig.width = 4, fig.asp = 0.6----
xc_trap( y = 3.0, b = 20.0, m = 1.0, units = "Eng")

## ----manningt-2.2, message=FALSE, warning=FALSE, fig.width = 4, fig.asp = 0.6----
ans <- manningt(Q = 360., n = 0.015, m = 1, y = 3.0, Sf = 0.00088, units = "Eng")
knitr::kable(format(as.data.frame(ans), digits = 2), format = "pipe", padding=0)
cat(sprintf("Optimal bottom width: %.5f ft\n", ans$bopt))

## ----manningt-2.3, message=FALSE, warning=FALSE, fig.width = 4, fig.asp = 0.6----
ans <- manningt(Q = 360., n = 0.015, m = 1, b = 4.767534, Sf = 0.00088, units = "Eng")
cat(sprintf("Optimal depth: %.5f ft\n", ans$yopt))

## ----manningt-3, fig.width = 4, fig.asp = 1.0---------------------------------
spec_energy_trap( Q = 360, b = 20, m = 1, scale = 4, units = "Eng" )

## ----manningt-4, fig.width = 4, fig.asp = 1.0---------------------------------
spec_energy_trap( Q = 360, b = 20, m = 1, scale = 4, y=3.0, units = "Eng" )

## ----manningt-5, message=FALSE, fig.width = 5, fig.asp = .62------------------
ns <- seq(0.011, 0.021, 0.002)
ys <- seq(1.5, 2.1, 0.1)
ny <- expand.grid(n=ns, y=ys)
ans <- mapply(manningt, n = ny$n, y = ny$y, MoreArgs = list(m = 2, Sf = 0.0005, b = 3, units = "SI"))
x <- as.data.frame(t(ans))
#to simplify plotting, select columns of interest and change each from list to numeric
x2 <- data.frame(Q=unlist(x$Q),y=unlist(x$y),n=unlist(x$n))
ggplot2::ggplot(data=x2,ggplot2::aes(x=y,y=Q, group=n, colour=n)) + ggplot2::geom_line() +
  ggplot2::labs(x = "y, m", y = expression(paste("Q, ", ~m^3/s)))

## ----pump-1, message=FALSE, echo=FALSE, fig.align = 'center', out.width = "60%", fig.cap = "A simple hydraulic system (from https://www.castlepumps.com)"----
knitr::include_graphics('./TDH_Example_2_Diagram.png')

## ----pump-2, message=FALSE----------------------------------------------------
ans <- darcyweisbach(Q = 1,D = 20/12, L = 3884, ks = 0.0005, nu = 1.23e-5, units = "Eng")
cat(sprintf("Coefficient K: %.3f\n", ans$hf))

## ----pump-3, message=FALSE----------------------------------------------------
scurve <- systemcurve(hs = 30, K = ans$hf, units = "Eng")

## ----pump-4, message=FALSE, echo=FALSE, fig.align = 'center', out.width = "60%", fig.cap = "A sample set of pump curves (from https://www.gouldspumps.com). The three red dots are points selected to approximate the curve"----
knitr::include_graphics('./goulds_pump_3409.png')

## ----pump-5, echo=FALSE-------------------------------------------------------
knitr::kable(data.frame(type=c("poly1","poly2","poly3"), Equation=c("$h=a+{b}{Q}+{c}{Q}^2$","$h=a+{c}{Q}^2$","$h_{shutoff}+{c}{Q}^2$")), format="pipe", padding=0)

## ----pump-6, message=FALSE----------------------------------------------------
qgpm <- units::set_units(c(0, 5000, 7850), gallons/minute)
qcfs <- units::set_units(qgpm, ft^3/s)
hft <- c(81, 60, 20) #units are already in ft so setting units is optional
pcurve <- pumpcurve(Q = qcfs, h = hft, eq = "poly2", units = "Eng")

## ----pump-7, message=FALSE----------------------------------------------------
pcurve$p

## ----pump-8, message=FALSE----------------------------------------------------
oppt <- operpoint(pcurve = pcurve, scurve = scurve)
cat(sprintf("Operating Point: Q = %.3f, h = %.3f\n", oppt$Qop, oppt$hop))
oppt$p

## ----hc-1, message=FALSE, echo=FALSE, fig.align = 'center', out.width = "75%", fig.cap = "A sample pipe network with pipe numbers indicated in black"----
knitr::include_graphics('./hardycross_system.png')

## ----hc-2, message=FALSE------------------------------------------------------
dfpipes <- data.frame(
  ID = c(1,2,3,4,5,6,7,8,9,10),                                #pipe ID
  D = c(0.3,0.2,0.2,0.2,0.2,0.15,0.25,0.15,0.15,0.25),         #diameter in m
  L = c(250,100,125,125,100,100,125,100,100,125),              #length in m
  f = c(.01879,.02075,.02075,.02075,.02075,.02233,.01964,.02233,.02233,.01964)
)
loops <- list(c(1,2,3,4,5),c(4,6,7,8),c(3,9,10,6))
Qs <- list(c(.040,.040,.02,-.02,-.04),c(.02,0,0,-.02),c(-.02,.02,0,0))

## ----hc-3, message=FALSE------------------------------------------------------
ans <- hardycross(dfpipes = dfpipes, loops = loops, Qs = Qs, n_iter = 3, units = "SI")
knitr::kable(ans$dfloops, digits = 4, format = "pipe", padding=0)

## ----hc-4, message=FALSE------------------------------------------------------
knitr::kable(ans$dfpipes, digits = 4, format = "pipe", padding=0)

## ----hc-5, message=FALSE------------------------------------------------------
dfpipes <- data.frame(
  ID = c(1,2,3,4,5,6,7,8,9,10),                         #pipe ID
  D = c(0.3,0.2,0.2,0.2,0.2,0.15,0.25,0.15,0.15,0.25),  #diameter in m
  L = c(250,100,125,125,100,100,125,100,100,125),       #length in m
  ks = rep(0.00025,10)                                  #absolute roughness, m
)
loops <- list(c(1,2,3,4,5),c(4,6,7,8),c(3,9,10,6))
Qs <- list(c(.040,.040,.02,-.02,-.04),c(.02,0,0,-.02),c(-.02,.02,0,0))

## ----hc-6, message=FALSE------------------------------------------------------
ans <- hardycross(dfpipes = dfpipes, loops = loops, Qs = Qs, n_iter = 3, units = "SI")
knitr::kable(ans$dfpipes, digits = 4, format = "pipe", padding=0)

