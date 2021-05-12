# GENERIC METHODS
#' @include AllClasses.R
NULL

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
#' @family mutator
#' @name subset
#' @rdname subset
NULL

# Mean Ceramic Date ============================================================
#' Mean Ceramic Date
#'
#' Estimates the Mean Ceramic Date of an assemblage.
#' @param object A [CountMatrix-class] or a [DateEvent-class] object.
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
#'  `date_mcd()` returns a [DateMCD-class] object.
#'
#'  `bootstrap_mcd()` and `jackknife_mcd()` return a `data.frame`.
#' @references
#'  South, S. A. (1977). *Method and Theory in Historical Archaeology*.
#'  New York: Academic Press.
#' @example inst/examples/ex-date_mcd.R
#' @author N. Frerebeau
#' @family dating
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

# Event Model ==================================================================
#' Event and Accumulation Dates
#'
#' @description
#'  `date_event()` fit a date event model.
#'
#'  `predict_event()` and `predict_accumulation()` estimates the event and
#'  accumulation dates of an assemblage.
#' @param object A [CountMatrix-class] or a [DateEvent-class] object.
#' @param data A [CountMatrix-class] object for which to predict event and
#'  accumulation dates.
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
#'  `date_event()` returns a [DateEvent-class] object.
#'
#'  `predict_event()`, `predict_accumulation()`, `bootstrap_event()`
#'  and `jackknife_event()` return a `data.frame`.
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
#' @family dating
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

# Plot =========================================================================
#' Date and Time Plot
#'
#' `plot_date()` produces an activity or tempo plot.
#'
#' `plot_time()` produces an abundance *vs* time diagram.
#' @param object An object of class [DateEvent-class] to be plotted.
#' @param dates A [`numeric`] vector of dates.
#' @param type A [`character`] string indicating the type of plot.
#'  It must be one of "`activity`" (default) or "`tempo`".
#'  Any unambiguous substring can be given.
#' @param select A [`numeric`] or [`character`] vector giving the selection of
#'  the assemblage that are drawn.
#' @param n A length-one non-negative [`numeric`] vector giving the desired
#'  length of the vector of quantiles for density computation.
#' @param event A [`logical`] scalar: should the distribution of the event date
#'  be displayed? Only used if type is "`activity`".
#' @param facet A [`logical`] scalar: should a matrix of panels defined by
#'  type/taxon be drawn? Only used if `highlight` is `NULL`.
#' @param ... Further arguments to be passed to internal methods.
#' @section Event and Acccumulation Dates:
#'  `plot_date()` plots the probability estimate density curves of
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
#' @section Detection of Selective Processes:
#'  Results of the frequency increment test can be displayed on an abundance
#'  *vs* time diagram aid in the detection and quantification of selective
#'  processes in the archaeological record. If `roll` is `TRUE`, each time
#'  series is subsetted according to `window` to see if episodes of selection
#'  can be identified among decoration types that might not show overall
#'  selection. If so, shading highlights the data points where
#'  [test_fit()] identifies selection.
#' @return
#'  A [ggplot2::ggplot] object.
#' @note
#'  Displaying FIT results on an abundance *vs* time diagram is adapted from Ben
#'  Marwick's original [idea](https://github.com/benmarwick/signatselect/).
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
#' @example inst/examples/ex-plot_line.R
#' @author N. Frerebeau
#' @family plot
#' @seealso [date_event()], [test_fit()]
#' @docType methods
#' @name plot_date
#' @rdname plot_date
NULL

#' @rdname plot_date
#' @aliases plot_date-method
setGeneric(
  name = "plot_date",
  def = function(object, ...) standardGeneric("plot_date")
)

#' @rdname plot_date
#' @aliases plot_time-method
setGeneric(
  name = "plot_time",
  def = function(object, dates, ...) standardGeneric("plot_time")
)

# Frequency Increment Test =====================================================
#' Frequency Increment Test
#'
#' @param object A \eqn{m \times p}{m x p} matrix of count data.
#' @param dates A [`numeric`] vector of dates.
#' @param level A length-one [`numeric`] vector giving the
#'  confidence level.
#' @param roll A [`logical`] scalar: should each time series be
#'  subsetted to look for episodes of selection?
#'  Only used if \code{highlight} is "\code{FIT}" (see details).
#' @param window An odd [`integer`] giving the size of the rolling
#'  window. Only used if \code{roll} is \code{TRUE}.
#' @param ... Currently not used.
#' @details
#'  The Frequency Increment Test (FIT) rejects neutrality if the distribution
#'  of normalized variant frequency increments exhibits a mean that deviates
#'  significantly from zero.
#' @return
#'  If \code{simplify} is \code{FALSE}, returns a list (default), else returns
#'  a matrix.
#' @example inst/examples/ex-test_fit.R
#' @author N. Frerebeau
#' @references
#'  Feder, A. F., Kryazhimskiy, S. & Plotkin, J. B. (2014). Identifying
#'  Signatures of Selection in Genetic Time Series. *Genetics*, 196(2),
#'  509-522. \doi{10.1534/genetics.113.158220}.
#' @family statistics
#' @docType methods
#' @name test_fit
#' @rdname test_fit
NULL

#' @rdname test_fit
#' @aliases test_fit-method
setGeneric(
  name = "test_fit",
  def = function(object, ...) standardGeneric("test_fit")
)
