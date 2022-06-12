# GENERIC METHODS
#' @include AllClasses.R
NULL

# S4 dispatch to S3 generics ===================================================
setGeneric("autoplot", package = "ggplot2")

# Mutators =====================================================================
## Extract ---------------------------------------------------------------------
#' Get or Set Parts of an Object
#'
#' Getters and setters to retrieve or set parts of an object.
#' @param x An object from which to get or set element(s).
# @param value A possible value for the element(s) of `x`.
#' @return
#'  * `set_*()` returns an object of the same sort as `x` with the new values
#'    assigned.
#'  * `get_*()` returns the part of `x`.
# @example inst/examples/ex-mutators.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name mutators
#' @rdname mutators
#' @aliases get set
NULL

#' @rdname mutators
#' @aliases get_dates-method
setGeneric(
  name = "get_dates",
  def = function(x) standardGeneric("get_dates")
)

#' @rdname mutators
#' @aliases get_groups-method
setGeneric(
  name = "get_groups",
  def = function(x) standardGeneric("get_groups")
)

#' @rdname mutators
#' @aliases get_model-method
setGeneric(
  name = "get_model",
  def = function(x) standardGeneric("get_model")
)

#' @rdname mutators
#' @aliases get_weights-method
setGeneric(
  name = "get_weights",
  def = function(x) standardGeneric("get_weights")
)

# Resampling methods ===========================================================
#' Resampling Methods
#'
#' @description
#'  * `bootstrap()` generate bootstrap estimations of a statistic.
#'  * `jackknife()` generate jackknife estimations of a statistic.
#' @param object An object.
#' @param n A non-negative [`integer`] specifying the number of bootstrap
#'  replications.
#' @param f A [`function`] that takes a single numeric vector (the result of
#'  the resampling procedure) as argument.
#' @param interval A [`character`] string giving the type of confidence
#'  interval to be returned. It must be one "`student`" (the default),
#'  "`normal`" or "`percentiles`". Any unambiguous substring can be given.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param ... Currently not used
#' @return
#'  If `f` is `NULL`, `bootstrap()` and `jackknife()` return a [`data.frame`]
#'  with the following elements (else, returns the result of `f` applied to the
#'  `n` resampled values) :
#'  \describe{
#'   \item{original}{The observed value.}
#'   \item{mean}{The bootstrap/jackknife estimate of mean.}
#'   \item{bias}{The bootstrap/jackknife estimate of bias.}
#'   \item{error}{The boostrap/jackknife estimate of standard erro.}
#'  }
#' @author N. Frerebeau
#' @docType methods
#' @family resampling methods
#' @name resample
#' @rdname resample
NULL

#' @rdname resample
#' @aliases bootstrap-method
setGeneric(
  name = "bootstrap",
  def = function(object, ...) standardGeneric("bootstrap")
)

#' @rdname resample
#' @aliases jackknife-method
setGeneric(
  name = "jackknife",
  def = function(object, ...) standardGeneric("jackknife")
)

# Dating Methods ===============================================================
## Mean Ceramic Date -----------------------------------------------------------
#' Mean Ceramic Date
#'
#' Estimates the Mean Ceramic Date of an assemblage.
#' @param object A length-\eqn{p} [`numeric`] vector, an \eqn{m \times p}{m x p}
#'  `numeric` [`matrix`] or [`data.frame`] of count data (absolute frequencies).
#' @param dates A length-\eqn{p} [`numeric`] vector of dates expressed in CE
#'  years (BCE years must be given as negative numbers).
#' @param ... Currently not used.
#' @details
#'  The Mean Ceramic Date (MCD) is a point estimate of the occupation of an
#'  archaeological site (South 1977). The MCD is estimated as the weighted mean
#'  of the date midpoints of the ceramic types (based on absolute dates or the
#'  known production interval) found in a given assemblage. The weights are the
#'  relative frequencies of the respective types in the assemblage.
#'
#'  A bootstrapping procedure is used to estimate the confidence interval of a
#'  given MCD. For each assemblage, a large number of new bootstrap replicates
#'  is created, with the same sample size, by resampling the original
#'  assemblage with replacement. MCDs are calculated for each replicates and
#'  upper and lower boundaries of the confidence interval associated with each
#'  MCD are then returned.
#' @note
#'  All results are rounded to zero decimal places (sub-annual precision does
#'  not make sense in most situations). You can change this behavior with
#'  `options(kairos.precision = x)` (for `x` decimal places).
#' @return
#'  A single [`numeric`] value or a [MeanDate-class] object.
#' @seealso [plot_mcd][plot()], [bootstrap()], [jackknife()], [simulate()]
#' @references
#'  South, S. A. (1977). *Method and Theory in Historical Archaeology*.
#'  New York: Academic Press.
#' @example inst/examples/ex-mcd.R
#' @author N. Frerebeau
#' @family dating methods
#' @docType methods
#' @rdname mcd
#' @aliases mcd-method
setGeneric(
  name = "mcd",
  def = function(object, dates, ...) standardGeneric("mcd")
)

## Event Dates -----------------------------------------------------------------
#' Event and Accumulation Dates
#'
#' @description
#'  * `event()` fit a date event model.
#'  * `predict_event()` and `predict_accumulation()` estimates the event and
#'    accumulation dates of an assemblage.
#' @param object An \eqn{m \times p}{m x p} `numeric` [`matrix`] or a
#'  [`data.frame`] of count data (absolute frequencies).
#' @param data A `numeric` [`matrix`] or a [`data.frame`] of count data
#'  (absolute frequencies)for which to predict event and accumulation dates.
#' @param dates A [`numeric`] vector of dates expressed in CE years (BCE years
#'  must be given as negative numbers). If named, the names must match
#'  the row names of `object`.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param cutoff An [`integer`] giving the cumulative percentage of variance
#'  used to select CA factorial components for linear model fitting (see
#'  details). All compounds with a cumulative percentage of variance of less
#'  than the `cutoff` value will be retained.
#' @param margin A [`numeric`] vector giving the subscripts which the prediction
#'  will be applied over: `1` indicates rows, `2` indicates columns.
#' @param progress A [`logical`] scalar: should a progress bar be displayed?
#' @inheritParams dimensio::bootstrap
#' @param ... Further arguments to be passed to internal methods.
#' @details
#'  This is an implementation of the chronological modeling method proposed by
#'  Bellanger and Husi (2012, 2013).
#'
#'  Event and accumulation dates are density estimates of the occupation and
#'  duration of an archaeological site (Bellanger and Husi 2012, 2013).
#'  The event date is an estimation of the *terminus post-quem* of an
#'  archaeological assemblage. The accumulation date represents the
#'  "chronological profile" of the assemblage. According to Bellanger and Husi
#'  (2012), accumulation date can be interpreted "at best \[...\] as a formation
#'  process reflecting the duration or succession of events on the scale of
#'  archaeological time, and at worst, as imprecise dating due to contamination
#'  of the context by residual or intrusive material." In other words,
#'  accumulation dates estimate occurrence of archaeological events and rhythms
#'  of the long term.
#'
#'  This method relies on strong archaeological and statistical assumptions
#'  (see `vignette("kairos")`).
#' @section Resampling:
#'  If `jackknife()` is used, one type/fabric is removed at a
#'  time and all statistics are recalculated. In this way, one can assess
#'  whether certain type/fabric has a substantial influence on the date
#'  estimate.
#'  A three columns `data.frame` is returned, giving the results of
#'  the resampling procedure (jackknifing fabrics) for each assemblage (in rows)
#'  with the following columns:
#'  \describe{
#'   \item{`mean`}{The jackknife mean (event date).}
#'   \item{`bias`}{The jackknife estimate of bias.}
#'   \item{`error`}{The standard error of predicted means.}
#'  }
#'
#'  If `bootstrap()` is used, a large number of new bootstrap assemblages is
#'  created, with the same sample size, by resampling each of the original
#'  assemblage with replacement. Then, examination of the bootstrap statistics
#'  makes it possible to pinpoint assemblages that require further
#'  investigation.
#'
#'  A five columns `data.frame` is returned, giving the bootstrap
#'  distribution statistics for each replicated assemblage (in rows)
#'  with the following columns:
#'  \describe{
#'   \item{`min`}{Minimum value.}
#'   \item{`mean`}{Mean value (event date).}
#'   \item{`max`}{Maximum value.}
#'   \item{`Q5`}{Sample quantile to 0.05 probability.}
#'   \item{`Q95`}{Sample quantile to 0.95 probability.}
#'  }
#' @note
#'  All results are rounded to zero decimal places (sub-annual precision does
#'  not make sense in most situations). You can change this behavior with
#'  `options(kairos.precision = x)` (for `x` decimal places).
#'
#'  Bellanger *et al.* did not publish the data supporting their demonstration:
#'  no replication of their results is possible. This implementation must be
#'  considered **experimental** and subject to major changes in a future
#'  release.
#' @return
#'  * `event()` returns an [EventDate-class] object.
#'  * `predict_event()` returns a [`data.frame`].
#'  * `predict_accumulation()` returns a [MeanDate-class] object.
#'  * `bootstrap()` and `jackknife()` return a [`data.frame`].
#' @seealso [plot_event][plot()]
#' @references
#'  Bellanger, L. & Husi, P. (2013). Mesurer et modéliser le temps inscrit dans
#'  la matière à partir d'une source matérielle : la céramique médiévale.
#'  In *Mesure et Histoire Médiévale*. Histoire ancienne et médiévale.
#'  Paris: Publication de la Sorbonne, p. 119-134.
#'
#'  Bellanger, L. & Husi, P. (2012). Statistical Tool for Dating and
#'  Interpreting Archaeological Contexts Using Pottery. *Journal of
#'  Archaeological Science*, 39(4), 777-790. \doi{10.1016/j.jas.2011.06.031}.
#'
#'  Bellanger, L., Tomassone, R. & Husi, P. (2008). A Statistical Approach for
#'  Dating Archaeological Contexts. *Journal of Data Science*, 6, 135-154.
#'
#'  Bellanger, L., Husi, P. & Tomassone, R. (2006). Une approche statistique
#'  pour la datation de contextes archéologiques. *Revue de Statistique
#'  Appliquée*, 54(2), 65-81.
#'
#'  Bellanger, L., Husi, P. & Tomassone, R. (2006). Statistical Aspects of
#'  Pottery Quantification for the Dating of Some Archaeological Contexts.
#'  *Archaeometry*, 48(1), 169-183. \doi{10.1111/j.1475-4754.2006.00249.x}.
#'
#'  Poblome, J. & Groenen, P. J. F. (2003). Constrained Correspondence Analysis
#'  for Seriation of Sagalassos Tablewares. In Doerr, M. & Apostolis, S. (eds.),
#'  *The Digital Heritage of Archaeology*. Athens: Hellenic Ministry of Culture.
#' @example inst/examples/ex-event.R
#' @author N. Frerebeau
#' @family dating methods
#' @docType methods
#' @name event
#' @rdname event
NULL

#' @rdname event
#' @aliases event-method
setGeneric(
  name = "event",
  def = function(object, dates, ...) standardGeneric("event"),
  valueClass = "EventDate"
)

#' @rdname event
#' @aliases predict_event-method
setGeneric(
  name = "predict_event",
  def = function(object, data, ...) standardGeneric("predict_event"),
  valueClass = "data.frame"
)

#' @rdname event
#' @aliases predict_accumulation-method
setGeneric(
  name = "predict_accumulation",
  def = function(object, data, ...) standardGeneric("predict_accumulation"),
  valueClass = "MeanDate"
)

# Chronological Modelling ======================================================
## Aoristic Analysis -----------------------------------------------------------
#' Aoristic Analysis
#'
#' Computes the aoristic sum.
#' @param x A [`numeric`] vector. If `y` is missing, must be a [`list`] (or a
#'  [`data.frame`]) with `numeric` components (columns) `from` and `to`.
#' @param y A [`numeric`] vector. If missing, an attempt is made to interpret
#'  `x` in a suitable way.
#' @param step A length-one [`integer`] vector giving the step size, i.e. the
#'  width of each time step in the time series (in years CE; defaults to
#'  \eqn{1} - i.e. annual level).
#' @param start A length-one [`numeric`] vector giving the beginning of the time
#'  window (in years CE).
#' @param stop A length-one [`numeric`] vector giving the end of the time
#'  window (in years CE).
#' @param weight A [`logical`] scalar: should the aoristic sum be weighted by
#'  the length of periods (default). If `FALSE` the aoristic sum is the number
#'  of elements within a time block.
#' @param groups A [`factor`] vector in the sense that `as.factor(groups)`
#'  defines the grouping. If `x` is a `list` (or a `data.frame`), `groups` can
#'  be a length-one vector giving the index of the grouping component (column)
#'  of `x`.
#' @param ... Currently not used.
#' @details
#'  Aoristic analysis is used to determine the probability of contemporaneity of
#'  archaeological sites or assemblages. The aoristic analysis distributes the
#'  probability of an event uniformly over each temporal fraction of the period
#'  considered. The aoristic sum is then the distribution of the total number of
#'  events to be assumed within this period.
#'
#'  Muller and Hinz (2018) pointed out that the overlapping of temporal
#'  intervals related to period categorization and dating accuracy is likely to
#'  bias the analysis. They proposed a weighting method to overcome this
#'  problem. This method is not implemented here (for the moment), see the
#'  [\pkg{aoristAAR} package](https://github.com/ISAAKiel/aoristAAR).
#' @return
#'  An [AoristicSum-class] object.
#' @seealso [roc()], [plot()][plot_aoristic]
#' @references
#'  Crema, E. R. (2012). Modelling Temporal Uncertainty in Archaeological
#'  Analysis. *Journal of Archaeological Method and Theory*, 19(3): 440-61.
#'  \doi{10.1007/s10816-011-9122-3}.
#'
#'  Johnson, I. (2004). Aoristic Analysis: Seeds of a New Approach to Mapping
#'  Archaeological Distributions through Time. *In* Ausserer, K. F., Börner, W.,
#'  Goriany, M. & Karlhuber-Vöckl, L. (ed.), *Enter the Past - The E-Way into
#'  the Four Dimensions of Cultural Heritage*, Oxford: Archaeopress, p. 448-52.
#'  BAR International Series 1227.
#'  \doi{10.15496/publikation-2085}
#'
#'  Müller-Scheeßel, N. & Hinz, M. (2018). *Aoristic Research in R: Correcting
#'  Temporal Categorizations in Archaeology*. Presented at the Human History and
#'  Digital Future (CAA 2018), Tubingen, March 21.
#'  <https://www.youtube.com/watch?v=bUBukex30QI>.
#'
#'  Palmisano, A., Bevan, A. & Shennan, S. (2017). Comparing Archaeological
#'  Proxies for Long-Term Population Patterns: An Example from Central Italy.
#'  *Journal of Archaeological Science*, 87: 59-72.
#'  \doi{10.1016/j.jas.2017.10.001}.
#'
#'  Ratcliffe, J. H. (2000). Aoristic Analysis: The Spatial Interpretation of
#'  Unspecific Temporal Events. *International Journal of Geographical
#'  Information Science*, 14(7): 669-79. \doi{10.1080/136588100424963}.
#'
#'  Ratcliffe, J. H. (2002). Aoristic Signatures and the Spatio-Temporal
#'  Analysis of High Volume Crime Patterns. *Journal of Quantitative
#'  Criminology*, 18(1): 23-43. \doi{10.1023/A:1013240828824}.
#' @example inst/examples/ex-aoristic.R
#' @author N. Frerebeau
#' @family chronological analysis
#' @docType methods
#' @name aoristic
#' @rdname aoristic
NULL

#' @rdname aoristic
#' @aliases aoristic-method
setGeneric(
  name = "aoristic",
  def = function(x, y, ...) standardGeneric("aoristic"),
  valueClass = "AoristicSum"
)

## Rate of Change --------------------------------------------------------------
#' Rate of Change
#'
#' Computes the rate of change from an aoristic analysis.
#' @param object An [`AoristicSum-class`] object.
#' @param n A non-negative [`integer`] giving the number of replications (see
#'  details).
#' @param ... Currently not used.
#' @return
#'  A [RateOfChange-class] object.
#' @seealso [aoristic()], [plot()][plot_aoristic]
#' @references
#'  Baxter, M. J. & Cool, H. E. M. (2016). Reinventing the Wheel? Modelling
#'  Temporal Uncertainty with Applications to Brooch Distributions in Roman
#'  Britain. *Journal of Archaeological Science*, 66: 120-27.
#'  \doi{10.1016/j.jas.2015.12.007}.
#'
#'  Crema, E. R. (2012). Modelling Temporal Uncertainty in Archaeological
#'  Analysis. *Journal of Archaeological Method and Theory*, 19(3): 440-61.
#'  \doi{10.1007/s10816-011-9122-3}.
#' @example inst/examples/ex-aoristic.R
#' @author N. Frerebeau
#' @family chronological analysis
#' @docType methods
#' @name roc
#' @rdname roc
NULL

#' @rdname roc
#' @aliases roc-method
setGeneric(
  name = "roc",
  def = function(object, ...) standardGeneric("roc"),
  valueClass = "RateOfChange"
)

## Apportion -------------------------------------------------------------------
#' Chronological Apportioning
#'
#' @param object An \eqn{m \times p}{m x p} `numeric` [`matrix`] or a
#'  [`data.frame`] of count data (absolute frequencies).
#' @param s0 A length-\eqn{m} [`numeric`] vector giving the site beginning dates
#'  expressed in CE years (BCE years must be given as negative numbers).
#' @param s1 A length-\eqn{m} [`numeric`] vector giving the site end dates
#'  expressed in CE years (BCE years must be given as negative numbers).
#' @param t0 A length-\eqn{p} [`numeric`] vector giving the type beginning dates
#'  expressed in CE years (BCE years must be given as negative numbers).
#' @param t1 A length-\eqn{p} [`numeric`] vector giving the type end dates
#'  expressed in CE years (BCE years must be given as negative numbers).
#' @param from A length-one [`numeric`] vector giving the beginning of the
#'  period of interest (in years CE).
#' @param to A length-one [`numeric`] vector giving the end of the period of
#'  interest (in years CE).
#' @param step A length-one [`integer`] vector giving the step size, i.e. the
#'  width of each time step for apportioning (in years CE; defaults to
#'  \eqn{25}).
#' @param method A [`character`] string specifying the distribution to be used
#'  (type popularity curve). It must be one of "`uniform`" (uniform
#'  distribution) or "`truncated`" (truncated standard normal distribution).
#'  Any unambiguous substring can be given.
#' @param z An [`integer`] value giving the lower and upper truncation points
#'  (defaults to \eqn{2}). Only used if `method` is "`truncated`".
#' @param progress A [`logical`] scalar: should a progress bar be displayed?
#' @param ... Currently not used.
#' @references
#'  Roberts, J. M., Mills, B. J., Clark, J. J., Haas, W. R., Huntley, D. L. &
#'  Trowbridge, M. A. (2012). A Method for Chronological Apportioning of Ceramic
#'  Assemblages. *Journal of Archaeological Science*, 39(5): 1513-20.
#'  \doi{10.1016/j.jas.2011.12.022}.
#' @author N. Frerebeau
#' @family chronological analysis
#' @docType methods
#' @name apportion
#' @rdname apportion
NULL

#' @rdname apportion
#' @aliases apportion-method
setGeneric(
  name = "apportion",
  def = function(object, ...) standardGeneric("apportion"),
  valueClass = "CountApportion"
)

## Frequency Increment Test ----------------------------------------------------
#' Frequency Increment Test
#'
#' @param object An \eqn{m \times p}{m x p} `numeric` [`matrix`] or a
#'  [`data.frame`] of count data (absolute frequencies).
#' @param dates A [`numeric`] vector of dates expressed in CE years (BCE years
#'  must be given as negative numbers).
#' @param ... Currently not used.
#' @details
#'  The Frequency Increment Test (FIT) rejects neutrality if the distribution
#'  of normalized variant frequency increments exhibits a mean that deviates
#'  significantly from zero.
#' @return
#'  An [IncrementTest-class] object.
#' @example inst/examples/ex-fit.R
#' @seealso [plot()][plot_fit]
#' @references
#'  Feder, A. F., Kryazhimskiy, S. & Plotkin, J. B. (2014). Identifying
#'  Signatures of Selection in Genetic Time Series. *Genetics*, 196(2):
#'  509-522. \doi{10.1534/genetics.113.158220}.
#' @author N. Frerebeau
#' @family chronological analysis
#' @docType methods
#' @name fit
#' @rdname fit
NULL

#' @rdname fit
#' @aliases fit-method
setGeneric(
  name = "fit",
  def = function(object, dates, ...) standardGeneric("fit"),
  valueClass = "IncrementTest"
)

# Plot =========================================================================
## matrix ----------------------------------------------------------------------
#' Abundance vs Time Plot
#'
#' Produces an abundance *vs* time diagram.
#' @param object An \eqn{m \times p}{m x p} `numeric` [`matrix`] or a
#'  [`data.frame`] of count data (absolute frequencies).
#' @param dates A [`numeric`] vector of dates.
#' @param facet A [`logical`] scalar: should a matrix of panels defined by
#'  type/taxon be drawn?
#' @param ... Currently not used.
#' @return
#'  A [`ggplot`][ggplot2::ggplot] object.
#' @example inst/examples/ex-plot.R
#' @author N. Frerebeau
#' @family plotting methods
#' @docType methods
#' @name plot_time
#' @rdname plot_time
NULL

#' @rdname plot_time
#' @aliases plot_time-method
setGeneric(
  name = "plot_time",
  def = function(object, dates, ...) standardGeneric("plot_time")
)

## AoristicSum -----------------------------------------------------------------
#' Plot Aoristic Analysis
#'
#' @param object,x An [AoristicSum-class] object.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param facet A [`logical`] scalar: should a matrix of panels defined by
#'  groups be drawn?
#' @param ... Currently not used.
#' @return
#'  * `autoplot()` returns a [`ggplot`][ggplot2::ggplot] object.
#'  * `plot()` is called it for its side-effects: it results in a graphic being
#'    displayed (invisibly returns `x`).
#' @seealso [aoristic()], [roc()]
#' @example inst/examples/ex-aoristic.R
#' @author N. Frerebeau
#' @family plotting methods
#' @docType methods
#' @name plot_aoristic
#' @rdname plot_aoristic
NULL

## MeanDate ---------------------------------------------------------------------
#' MCD Plot
#'
#' @param object,x A [MeanDate-class] object.
#' @param select A [`numeric`] or [`character`] vector giving the selection of
#'  the assemblage that are drawn.
#' @param decreasing A [`logical`] scalar: should the sort be increasing or
#'  decreasing?
#' @param ... Currently not used.
#' @return
#'  * `autoplot()` returns a [`ggplot`][ggplot2::ggplot] object.
#'  * `plot()` is called it for its side-effects: it results in a graphic being
#'    displayed (invisibly returns `x`).
#' @seealso [mcd()]
#' @example inst/examples/ex-mcd.R
#' @author N. Frerebeau
#' @family plotting methods
#' @docType methods
#' @name plot_mcd
#' @rdname plot_mcd
NULL

## EventDate -------------------------------------------------------------------
#' Event Plot
#'
#' Produces an activity or a tempo plot.
#' @param object,x A [EventDate-class] object.
#' @param type A [`character`] string indicating the type of plot.
#'  It must be one of "`activity`" (default) or "`tempo`".
#'  Any unambiguous substring can be given.
#' @param select A [`numeric`] or [`character`] vector giving the selection of
#'  the assemblage that are drawn.
#' @param n A length-one non-negative [`numeric`] vector giving the desired
#'  length of the vector of quantiles for density computation.
#' @param event A [`logical`] scalar: should the distribution of the event date
#'  be displayed? Only used if type is "`activity`".
#' @param ... Currently not used.
#' @section Event and Acccumulation Dates:
#'  `plot()` displays the probability estimate density curves of archaeological
#'  assemblage dates (*event* and *accumulation* dates; Bellanger and Husi
#'  2012). The *event* date is plotted as a line, while the *accumulation* date
#'  is shown as a grey filled area.
#'
#'  The accumulation date can be displayed as a tempo plot (Dye 2016) or an
#'  activity plot (Philippe and Vibet 2017):
#'  \describe{
#'   \item{Tempo plot}{A tempo plot estimates the cumulative occurrence of
#'   archaeological events, such as the slope of the plot directly reflects the
#'   pace of change.}
#'   \item{Activity plot}{An activity plot displays the first derivative of the
#'   tempo plot.}
#'  }
#' @return
#'  * `autoplot()` returns a [`ggplot`][ggplot2::ggplot] object.
#'  * `plot()` is called it for its side-effects: it results in a graphic being
#'    displayed (invisibly returns `x`).
#' @references
#'  Bellanger, L. & Husi, P. (2012). Statistical Tool for Dating and
#'  Interpreting Archaeological Contexts Using Pottery. *Journal of
#'  Archaeological Science*, 39(4), 777-790. \doi{10.1016/j.jas.2011.06.031}.
#'
#'  Dye, T. S. (2016). Long-Term Rhythms in the Development of Hawaiian
#'  Social Stratification. *Journal of Archaeological Science*, 71, 1-9.
#'  \doi{10.1016/j.jas.2016.05.006}.
#'
#'  Philippe, A. & Vibet, M.-A. (2017). Analysis of Archaeological Phases Using
#'  the R Package ArchaeoPhases. *Journal of Statistical Software, Code
#'  Snippets*, 93(1), 1-25. \doi{10.18637/jss.v093.c01}.
#' @example inst/examples/ex-event.R
#' @author N. Frerebeau
#' @family plotting methods
#' @seealso [event()]
#' @docType methods
#' @name plot_event
#' @rdname plot_event
NULL

## IncrementTest ---------------------------------------------------------------
#' Detection of Selective Processes
#'
#' Produces an abundance *vs* time diagram.
#' @param object,x An object to be plotted.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param roll A [`logical`] scalar: should each time series be subsetted to
#'  look for episodes of selection?
#' @param window An odd [`integer`] giving the size of the rolling
#'  window. Only used if `roll` is `TRUE`.
#' @param ... Currently not used.
#' @details
#'  Results of the frequency increment test can be displayed on an abundance
#'  *vs* time diagram aid in the detection and quantification of selective
#'  processes in the archaeological record. If `roll` is `TRUE`, each time
#'  series is subsetted according to `window` to see if episodes of selection
#'  can be identified among decoration types that might not show overall
#'  selection. If so, shading highlights the data points where
#'  [fit()] identifies selection.
#' @return
#'  * `autoplot()` returns a [`ggplot`][ggplot2::ggplot] object.
#'  * `plot()` is called it for its side-effects: it results in a graphic being
#'    displayed (invisibly returns `x`).
#' @note
#'  Displaying FIT results on an abundance *vs* time diagram is adapted from Ben
#'  Marwick's [original idea](https://github.com/benmarwick/signatselect/).
#' @seealso [fit()]
#' @example inst/examples/ex-fit.R
#' @author N. Frerebeau
#' @family plotting methods
#' @docType methods
#' @name plot_fit
#' @rdname plot_fit
NULL

# Seriation Methods ============================================================
#' Matrix Seriation
#'
#' @description
#'  * `seriate_*()` computes a permutation order for rows and/or columns.
#'  * `permute()` rearranges a data matrix according to a permutation order.
#'  * `get_order()` returns the seriation order for rows and columns.
#' @param object,x An \eqn{m \times p}{m x p} `numeric` [`matrix`] or a
#'  [`data.frame`] of count data (absolute frequencies).
#' @param order A [PermutationOrder-class] object giving the permutation
#'  order for rows and columns.
#' @param EPPM A [`logical`] scalar: should the seriation be computed on EPPM
#'  instead of raw data?
#' @param margin A [`numeric`] vector giving the subscripts which the
#'  rearrangement will be applied over: `1` indicates rows, `2` indicates
#'  columns, `c(1, 2)` indicates rows then columns, `c(2, 1)` indicates columns
#'  then rows.
#' @param axes An [`integer`] vector giving the subscripts of the CA axes to be
#'  used.
#' @param stop An [`integer`] giving the stopping rule (i.e. maximum number of
#'  iterations) to avoid infinite loop.
#' @param cutoff A function that takes a numeric vector as argument and returns
#'  a single numeric value (see below).
#' @param n A non-negative [`integer`] giving the number of bootstrap
#'  replications.
#' @param ... Further arguments to be passed to internal methods.
#' @section Seriation:
#'  The matrix seriation problem in archaeology is based on three conditions
#'  and two assumptions, which Dunell (1970) summarizes as follows.
#'
#'  The homogeneity conditions state that all the groups included in a
#'  seriation must:
#'  \enumerate{
#'   \item{Be of comparable duration.}
#'   \item{Belong to the same cultural tradition.}
#'   \item{Come from the same local area.}
#'  }
#'
#'  The mathematical assumptions state that the distribution of any historical
#'  or temporal class:
#'  \enumerate{
#'   \item{Is continuous through time.}
#'   \item{Exhibits the form of a unimodal curve.}
#'  }
#'  Theses assumptions create a distributional model and ordering is
#'  accomplished by arranging the matrix so that the class distributions
#'  approximate the required pattern. The resulting order is inferred
#'  to be chronological.
#'
#'  The following seriation methods are available:
#'  \describe{
#'   \item{`seriate_average()`}{Correspondence analysis-based seriation
#'   (average ranking). Correspondence analysis (CA) is an effective method for
#'   the seriation of archaeological assemblages. The order of the rows and
#'   columns is given by the coordinates along one dimension of the CA space,
#'   assumed to account for temporal variation. The direction of temporal change
#'   within the correspondence analysis space is arbitrary: additional
#'   information is needed to determine the actual order in time.}
#'   \item{`seriate_rank()`}{Reciprocal ranking seriation. These procedures
#'   iteratively rearrange rows and/or columns according to their weighted rank
#'   in the data matrix until convergence.
#'   Note that this procedure could enter into an infinite loop.
#'   If no convergence is reached before the maximum number of iterations, it
#'   stops with a warning.}
#'  }
#' @section Correspondence Analysis:
#'  `bootstrap()` allows to identify samples that are subject to
#'  sampling error or samples that have underlying structural relationships
#'  and might be influencing the ordering along the CA space.
#'
#'  This relies on a partial bootstrap approach to CA-based seriation where each
#'  sample is replicated `n` times. The maximum dimension length of
#'  the convex hull around the sample point cloud allows to remove samples for
#'  a given `cutoff` value.
#'
#'  According to Peebles and Schachner (2012), "\[this\] point removal procedure
#'  \[results in\] a reduced dataset where the position of individuals within the
#'  CA are highly stable and which produces an ordering consistent with the
#'  assumptions of frequency seriation."
#' @return
#'  * `seriate_*()` returns a [PermutationOrder-class] object.
#'  * `permute()` returns either a permuted `matrix` or a permuted `data.frame`
#'    (the same as `object`).
#'  * `bootstrap()` returns a [RefineCA-class] object.
#' @references
#'  Desachy, B. (2004). Le sériographe EPPM: un outil informatisé de sériation
#'  graphique pour tableaux de comptages. *Revue archéologique de Picardie*,
#'  3(1), 39-56. \doi{10.3406/pica.2004.2396}.
#'
#'  Dunnell, R. C. (1970). Seriation Method and Its Evaluation. *American
#'  Antiquity*, 35(03), 305-319. \doi{10.2307/278341}.
#'
#'  Ihm, P. (2005). A Contribution to the History of Seriation in Archaeology.
#'  In C. Weihs & W. Gaul (Eds.), *Classification: The Ubiquitous
#'  Challenge*. Berlin Heidelberg: Springer, p. 307-316.
#'  \doi{10.1007/3-540-28084-7_34}.
#'
#'  Peeples, M. A., & Schachner, G. (2012). Refining correspondence
#'  analysis-based ceramic seriation of regional data sets. *Journal of
#'  Archaeological Science*, 39(8), 2818-2827.
#'  \doi{10.1016/j.jas.2012.04.040}.
#' @seealso [dimensio::ca()]
#' @example inst/examples/ex-seriation.R
#' @author N. Frerebeau
#' @family seriation methods
#' @docType methods
#' @name seriation
#' @rdname seriation
NULL

#' @rdname seriation
#' @aliases seriate_average-method
setGeneric(
  name = "seriate_average",
  def = function(object, ...) standardGeneric("seriate_average"),
  valueClass = "PermutationOrder"
)

#' @rdname seriation
#' @aliases seriate_rank-method
setGeneric(
  name = "seriate_rank",
  def = function(object, ...) standardGeneric("seriate_rank"),
  valueClass = "PermutationOrder"
)

#' @rdname seriation
#' @aliases refine-method
setGeneric(
  name = "refine",
  def = function(object, ...) standardGeneric("refine"),
  valueClass = "RefineCA"
)

# @rdname seriation
# @aliases seriate_constrain-method
# setGeneric(
#   name = "seriate_constrain",
#   def = function(object, constrain, ...) standardGeneric("seriate_constrain"),
#   valueClass = "PermutationOrder"
# )

# @rdname seriation
# @aliases seriate_idds-method
# setGeneric(
#   name = "seriate_idds",
#   def = function(object, ...) standardGeneric("seriate_idds"),
#   valueClass = "PermutationOrder"
# )

#' @rdname seriation
#' @aliases permute-method
setGeneric(
  name = "permute",
  def = function(object, order, ...) standardGeneric("permute")
)

#' @rdname seriation
#' @aliases get_order-method
setGeneric(
  name = "get_order",
  def = function(x, ...) standardGeneric("get_order")
)
