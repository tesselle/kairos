# GENERIC METHODS
#' @include AllClasses.R
NULL

# S4 dispatch to base S3 generic ===============================================
setGeneric("autoplot", function(object, ...) standardGeneric("autoplot"))
if (!isGeneric("plot"))
  setGeneric("plot", function(x, y, ...) standardGeneric("plot"))

# Extract ======================================================================
## Subset ----------------------------------------------------------------------
#' Extract or Replace Parts of an Object
#'
#' Operators acting on objects to extract or replace parts.
#' @param x An object from which to extract element(s) or in which to replace
#'  element(s).
#' @param i A [`character`] string specifying elements to extract.
#'  Any unambiguous substring can be given (see details).
#' @return
#'  A subsetted object.
# @example inst/examples/ex-mutator.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name subset
#' @rdname subset
NULL

# Dating Methods ===============================================================
## Mean Ceramic Date -----------------------------------------------------------
#' Mean Ceramic Date
#'
#' Estimates the Mean Ceramic Date of an assemblage.
#' @param object A [arkhe::CountMatrix-class] or a [DateEvent-class] object.
#' @param dates A [`numeric`] vector of dates. If named, the names must match
#'  the row names of `object`.
#' @inheritParams stats_bootstrap
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
#' @return
#'  * `date_mcd()` returns a [DateMCD-class] object.
#'  * `bootstrap_mcd()` and `jackknife_mcd()` return a `data.frame`.
#' @seealso [plot_time()]
#' @references
#'  South, S. A. (1977). *Method and Theory in Historical Archaeology*.
#'  New York: Academic Press.
#' @example inst/examples/ex-date_mcd.R
#' @author N. Frerebeau
#' @family dating methods
#' @docType methods
#' @rdname date_mcd
#' @aliases date_mcd-method
setGeneric(
  name = "date_mcd",
  def = function(object, dates, ...) standardGeneric("date_mcd"),
  valueClass = "DateMCD"
)

#' @rdname date_mcd
#' @aliases bootstrap_mcd-method
setGeneric(
  name = "bootstrap_mcd",
  def = function(object, ...) standardGeneric("bootstrap_mcd"),
  valueClass = "data.frame"
)

#' @rdname date_mcd
#' @aliases jackknife_mcd-method
setGeneric(
  name = "jackknife_mcd",
  def = function(object, ...) standardGeneric("jackknife_mcd"),
  valueClass = "data.frame"
)

## Event Model -----------------------------------------------------------------
#' Event and Accumulation Dates
#'
#' @description
#'  * `date_event()` fit a date event model.
#'  * `predict_event()` and `predict_accumulation()` estimates the event and
#'    accumulation dates of an assemblage.
#' @param object A [arkhe::CountMatrix-class] or a [DateEvent-class] object.
#' @param data A [arkhe::CountMatrix-class] object for which to predict event
#'  and accumulation dates.
#' @param dates A [`numeric`] vector of dates. If named, the names must match
#'  the row names of `object`.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param cutoff An [`integer`] giving the cumulative percentage of variance
#'  used to select CA factorial components for linear model fitting (see
#'  details). All compounds with a cumulative percentage of variance of less
#'  than the `cutoff` value will be retained.
#' @param margin A [`numeric`] vector giving the subscripts which the prediction
#'  will be applied over: `1` indicates rows, `2` indicates columns.
#' @param progress A [`logical`] scalar: should a progress bar be displayed?
#' @inheritParams stats_bootstrap
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
#'  (2012), accumulation date can be interpreted "at best [...] as a formation
#'  process reflecting the duration or succession of events on the scale of
#'  archaeological time, and at worst, as imprecise dating due to contamination
#'  of the context by residual or intrusive material." In other words,
#'  accumulation dates estimate occurrence of archaeological events and rhythms
#'  of the long term.
#'
#'  This method relies on strong archaeological and statistical assumptions.
#'  Use it only if you know what you are doing (see references below and the
#'  vignette: `utils::vignette("dating")`).
#' @section Date Model:
#'  If `jackknife_event()` is used, one type/fabric is removed at a
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
#'  If `bootstrap_event` is used, a large number of new bootstrap assemblages is
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
#'  Bellanger *et al.* did not publish the data supporting their demonstration:
#'  no replication of their results is possible. This implementation must be
#'  considered **experimental** and subject to major changes in a future
#'  release.
#' @return
#'  * `date_event()` returns a [DateEvent-class] object.
#'  * `predict_event()`, `predict_accumulation()`, `bootstrap_event()`
#'    and `jackknife_event()` return a `data.frame`.
#' @seealso [plot_event()]
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
#' @example inst/examples/ex-date_event.R
#' @author N. Frerebeau
#' @family dating methods
#' @docType methods
#' @name event
#' @rdname event
NULL

#' @rdname event
#' @aliases date_event-method
setGeneric(
  name = "date_event",
  def = function(object, dates, ...) standardGeneric("date_event"),
  valueClass = "DateEvent"
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
  valueClass = "data.frame"
)

#' @rdname event
#' @aliases bootstrap_event-method
setGeneric(
  name = "bootstrap_event",
  def = function(object, ...) standardGeneric("bootstrap_event")
)

#' @rdname event
#' @aliases jackknife_event-method
setGeneric(
  name = "jackknife_event",
  def = function(object, ...) standardGeneric("jackknife_event"),
  valueClass = "data.frame"
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
#'  width of each time step in the time series (in years AD; defaults to
#'  \eqn{1}).
#' @param start A length-one [`numeric`] vector giving the beginning of the time
#'  window (in years AD).
#' @param stop A length-one [`numeric`] vector giving the end of the time
#'  window (in years AD).
#' @param weight A [`logical`] scalar: . Defaults to `FALSE` (the aoristic sum
#'  is the number of elements within a time step).
#' @param groups A [`factor`] vector in the sense that `as.factor(groups)`
#'  defines the grouping. If `x` is a `list` (or a `data.frame`), `groups` can
#'  be a length-one vector giving the index of the grouping component (column)
#'  of `x`.
#' @param ... Currently not used.
#' @return
#'  An [AoristicSum-class] object.
#' @seealso [rate_roc()], [plot_time()]
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
#'  Ratcliffe, J. H. (2000). Aoristic Analysis: The Spatial Interpretation of
#'  Unspecific Temporal Events. *International Journal of Geographical
#'  Information Science*, 14(7): 669-79. \doi{10.1080/136588100424963}.
#' @example inst/examples/ex-aoristic.R
#' @author N. Frerebeau
#' @family chronological analysis
#' @docType methods
#' @name aoristic
#' @rdname aoristic
NULL

#' @rdname aoristic
#' @aliases sum_aoristic-method
setGeneric(
  name = "sum_aoristic",
  def = function(x, y, ...) standardGeneric("sum_aoristic"),
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
#' @seealso [sum_aoristic()], [plot_time()]
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
#' @aliases rate_roc-method
setGeneric(
  name = "rate_roc",
  def = function(object, ...) standardGeneric("rate_roc"),
  valueClass = "RateOfChange"
)

## Apportion -------------------------------------------------------------------
#' Chronological Apportioning
#'
#' @param object An \eqn{m \times p}{m x p} [arkhe::CountMatrix-class] object.
#' @param s0 A length-\eqn{m} [`numeric`] vector giving the site beginning dates
#'  (in years AD).
#' @param s1 A length-\eqn{m} [`numeric`] vector giving the site end dates
#'  (in years AD).
#' @param t0 A length-\eqn{p} [`numeric`] vector giving the type end dates
#'  (in years AD).
#' @param t1 A length-\eqn{p} [`numeric`] vector giving the type end dates
#'  (in years AD).
#' @param from A length-one [`numeric`] vector giving the beginning of the
#'  period of interest (in years AD).
#' @param to A length-one [`numeric`] vector giving the end of the period of
#'  interest (in years AD).
#' @param step A length-one [`integer`] vector giving the step size, i.e. the
#'  width of each time step for apportioning (in years AD; defaults to
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
#' @param object A [arkhe::CountMatrix-class] object.
#' @param dates A [`numeric`] vector of dates.
#' @param ... Currently not used.
#' @details
#'  The Frequency Increment Test (FIT) rejects neutrality if the distribution
#'  of normalized variant frequency increments exhibits a mean that deviates
#'  significantly from zero.
#' @return
#'  An [IncrementTest-class] object.
#' @example inst/examples/ex-test_fit.R
#' @seealso [plot_time()]
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
#' @aliases test_fit-method
setGeneric(
  name = "test_fit",
  def = function(object, ...) standardGeneric("test_fit"),
  valueClass = "IncrementTest"
)

# Plot =========================================================================
## Plot abundance --------------------------------------------------------------
#' Abundance vs Time Plot
#'
#' Produces an abundance *vs* time diagram.
#' @param object,x An object to be plotted.
#' @param dates A [`numeric`] vector of dates.
#' @param facet A [`logical`] scalar: should a matrix of panels defined by
#'  type/taxon be drawn?
#' @param level A length-one [`numeric`] vector giving the
#'  confidence level.
#' @param roll A [`logical`] scalar: should each time series be
#'  subsetted to look for episodes of selection?
#' @param window An odd [`integer`] giving the size of the rolling
#'  window. Only used if `roll` is `TRUE`.
#' @param ... Currently not used.
#' @section Detection of Selective Processes:
#'  Results of the frequency increment test can be displayed on an abundance
#'  *vs* time diagram aid in the detection and quantification of selective
#'  processes in the archaeological record. If `roll` is `TRUE`, each time
#'  series is subsetted according to `window` to see if episodes of selection
#'  can be identified among decoration types that might not show overall
#'  selection. If so, shading highlights the data points where
#'  [test_fit()] identifies selection.
#' @return
#'  * `plot_time()` returns a [`ggplot`][`ggplot2::ggplot`] object.
#'  * `autoplot()` returns a [`ggplot`][`ggplot2::ggplot`] object.
#'  * `plot()` is called it for its side-effects: it results in a graphic being
#'  displayed (invisibly returns `x`).
#' @note
#'  Displaying FIT results on an abundance *vs* time diagram is adapted from Ben
#'  Marwick's original [idea](https://github.com/benmarwick/signatselect/).
#' @seealso [date_mcd()], [sum_aoristic()], [test_fit()]
#' @example inst/examples/ex-plot_time.R
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
  def = function(object, ...) standardGeneric("plot_time")
)

## Plot events -----------------------------------------------------------------
#' Event Plot
#'
#' Produces an activity or a tempo plot.
#' @param object,x A [DateEvent-class] object.
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
#'  `plot_event()` plots the probability estimate density curves of
#'  archaeological assemblage dates (*event* and *accumulation* dates; Bellanger
#'  and Husi 2012). The *event* date is plotted as a line, while the
#'  *accumulation* date is shown as a grey filled area.
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
#'  * `autoplot()` returns a [`ggplot`][`ggplot2::ggplot`] object.
#'  * `plot()` is called it for its side-effects: it results in a graphic being
#'  displayed (invisibly returns `x`).
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
#' @example inst/examples/ex-date_event.R
#' @author N. Frerebeau
#' @family plotting methods
#' @seealso [date_event()]
#' @docType methods
#' @name plot_event
#' @rdname plot_event
NULL
