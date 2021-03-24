#' Compute discharge and hyrdaulic geometry
#' @param Ac Vector of catchment area for calibration points in $m^2$
#' @param Qc Vector of discharge for calibration points in $m^3/s$
#' @param Ap Vector or raster of catchment area for prediction
#' @param use_prior Logical, use the prior for scaling? See 'details'
#' @details Computes discharge from catchment area as \eqn{\log Q = \log b + m \log A}.
#' 		If a single point in `calib` is included, the relationship will be re-parameterised by
#'  	adjusting the intercept parameter `b` so that the calibration point falls on the line
#' 		(while keeping the slope the same).
#'
#' 		With multiple points, a regression model is fit. If `use_prior == TRUE`, this is a Bayesian model
#' 		using the parameters from Burgers et al (2014). These priors are calibrated from a variety of rivers,
#' 		and the prior is quite strong, so it is possible with a small number of calibration points that the line
#' 		is quite far from the calibration. In this case, either re-run this function with a single calibration point,
#' 		or run with `use_prior = FALSE` (but this is only recommended if `range(Ac)` is similar to `range(Ap)`.
#'
#' 		For discharge scaling, catchment area units are expected to be in
#' 		\eqn{m^2}, and discharge will be computed in \eqn{m^3 s^{-1}}.
#'
#' 		Following computation of discharge, other aspects of hydralic geometry are computed following Raymond et al (2012).
#' @references Burgers HE et al. 2014. Size relationships of water discharge in rivers: scaling
#' 		of discharge with catchment area, main-stem lengthand precipitation.
#' 		*Hydrological Processes*. **28**:5769-5775.
#'
#' Raymond, PA et al. 2012. Scaling the gas transfer velocity and hydraulic
#' 		geometry in streams and small rivers. *Limnology and Oceanography: Fluids and
#' 		Environments*. **2**:41-53.
#' @examples
#' \donttest{
#'     library(sf)
#'     data(kamp_q)
#'     data(kamp_dem)
#'
#'     kamp = delineate(kamp_dem)
#'     kamp_Tp = pixel_topology(kamp)
#'     CA = catchment(kamp, type = 'reach', Tp = kamp_Tp)
#'
#'     ## need to transform coordinate system for kamp_q to match
#'     ## need to snap to stream
#'     ## need to make sure this works
#'     ## should probably update kamp_q projection to match kamp_dem
#'
#'     kamp_q$ca = catchment(kamp, type = 'points', y = st_geometry(kamp_q), Tp = kamp_Tp)
#'     x = hydraulic_geometry(kamp_q$ca, kamp_q$discharge, CA)
#' }
#' @return A data.frame with the following variables:
#'     * CA; the input catchment area
#'     * Q; discharge
#'     * v; water velocity
#'     * z; water depth
#'     * w; width
#'     * Ax; stream cross-sectional area
#' @export
hydraulic_geometry = function(Ac, Qc, Ap, use_prior = TRUE) {
	if(length(Ac) != length(Qc))
		stop("Ac and Qc must have the same length")

	## set up units to make conversion easy
	if(!is(Ap, "units")) {
		warning("No units set for Ap, assuming m^2")
		Ap = units::set_units(Ap, "m^2")
	}
	if(!is(Ac, "units")) {
		warning("No units set for Ac, assuming m^2")
		Ac = units::set_units(Ac, "m^2")
	}
	if(!is(Qc, "units")) {
		warning("No units set for Qc, assuming m^3/s")
		Qc = units::set_units(Qc, "m^3/s")
	}

	## convert the input units to those expected from Burgers et al, then drop the units
	Ap_km = units::set_units(Ap, "km^2")
	Ac = units::set_units(Ac, "km^2")
	Qc = units::set_units(Qc, "km^3/day")
	Ap_km = units::drop_units(Ap_km)
	Ac = units::drop_units(Ac)
	Qc = units::drop_units(Qc)

	if(length(Ac) == 1) {
		pars = .q_prior[1,]
		pars["log_intercept"] <- log(Qc) - pars["slope"] * log(Ac)
	} else {
		pars = .q_calib(Ac, Qc)
	}
	qq = .q_predict(pars, Ap_km)
	Qp = units::set_units(qq, "km^3/day")
	Qp = units::set_units(Qp, "m^3/s")

	## parameters from Raymond et al
	res = data.frame(
		CA = Ap,
		Q =	Qp,
		v = units::set_units(exp(-1.64 + 0.285 * log(qq)), "m/s"),
		z = units::set_units(exp(-0.895 + 0.294 * log(qq)), "m"),
		w = units::set_units(exp(2.56 + 0.423 * log(qq)), "m"))
	res
}

#' @name q_calib
#' @rdname q_calib
#' @title Calibrate discharge
#' Runs a bayesian linear regression to compute discharge from catchment area
#' @param x A [raster] stream map, such as one created by [delineate()].
#' @return An `sf` stream layer
#' @keywords internal
.q_calib = function(a, q) {
	par = rnorm(3, .q_prior()[1,], .q_prior()[2,]*2)
	names(par) = c("log_intercept", "slope", 'log_sd')
	optim(par, .q_lpost, a = a, q = q, control=list(fnscale=-1))$par
}

#' @rdname q_calib
#' @keywords internal
.q_prior = function() {
	matrix(c(log(6.3e-6), 0.4137399, 0.78, 0.03571429, 0, 5), nrow=2,
		   dimnames = list(c("pr_mean", "pr_sd"), c("log_intercept", "slope", 'log_sd')))
}

#' @rdname q_calib
#' @keywords internal
.q_predict = function(par, a) {
	exp(par["log_intercept"] + par["slope"] * log(a))
}

#' @rdname q_calib
#' @keywords internal
.q_lpost = function(par, a, q) {
	sum(dnorm(q, .q_predict(par, a), exp(par['log_sd']), log=TRUE)) +
		sum(dnorm(par, .q_prior()[1,], .q_prior()[2,], log=TRUE))
}
