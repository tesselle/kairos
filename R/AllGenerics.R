# GENERIC METHODS
#' @include AllClasses.R
NULL

# Import S4 generics ===========================================================
#' @importMethodsFrom arkhe jackknife
#' @importMethodsFrom arkhe bootstrap
NULL

# Mutators =====================================================================
## Subset ----------------------------------------------------------------------
#' Extract or Replace Parts of an Object
#'
#' Operators acting on objects to extract or replace parts.
#' @param x An object from which to extract element(s) or in which to replace
#'  element(s).
#' @param i,j,k Indices specifying elements to extract or replace.
#' @param drop A [`logical`] scalar: should the result be coerced to
#'  the lowest possible dimension? This only works for extracting elements,
#'  not for the replacement.
# @param value A possible value for the element(s) of `x`.
# @param ... Currently not used.
#' @return
#'  A subsetted object.
# @example inst/examples/ex-subset.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name subset
#' @rdname subset
NULL

## Extract ---------------------------------------------------------------------
#' Get or Set Parts of an Object
#'
#' Getters and setters to retrieve or set parts of an object.
#' @param object An object from which to get or set element(s).
# @param value A possible value for the element(s) of `x`.
#' @param ... Currently not used.
# @return
#  * `set_*()` returns an object of the same sort as `x` with the new values
#    assigned.
#  * `get_*()` returns the part of `x`.
# @example inst/examples/ex-mutators.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name mutators
#' @rdname mutators
#' @aliases get set
NULL

## Time series -----------------------------------------------------------------
#' Sampling Times
#'
#' Get the times at which a time series was sampled.
#' @param x An \R object.
#' @param calendar An [`aion::TimeScale-class`] object specifying the target
#'  calendar (see [aion::calendar()]). If `NULL` (the default), *rata die* are
#'  returned.
#' @return
#'  A [`numeric`] vector.
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name series
#' @rdname series
NULL

## Coerce ----------------------------------------------------------------------
#' Coerce to a Data Frame
#'
#' @param x An object.
#' @param calendar An [`aion::TimeScale-class`] object specifying the target
#'  calendar (see [aion::calendar()]). If `NULL`, *rata die* are returned.
#' @param row.names,optional Currently not used.
#' @param ... Further parameters to be passed to [data.frame()].
#' @return
#'  A [`data.frame`] with an extra `time` column giving the (decimal) years at
#'  which the time series was sampled.
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name data.frame
#' @rdname data.frame
NULL

# Mean Ceramic Dates ===========================================================
#' Mean Ceramic Date
#'
#' Estimates the Mean Ceramic Date of an assemblage.
#' @param object A \eqn{m \times p}{m x p} `numeric` [`matrix`] or
#'  [`data.frame`] of count data (absolute frequencies giving the number of
#'  individuals for each category, i.e. a contingency table). A [`data.frame`]
#'  will be coerced to a `numeric` `matrix` via [data.matrix()].
#' @param dates A length-\eqn{p} [`numeric`] vector of dates expressed in years.
#' @param calendar An [`aion::TimeScale-class`] object specifying the calendar
#'  of `dates` (see [aion::calendar()]). Defaults to Gregorian Common Era.
#' @param ... Currently not used.
#' @details
#'  The Mean Ceramic Date (MCD) is a point estimate of the occupation of an
#'  archaeological site (South 1977). The MCD is estimated as the weighted mean
#'  of the date midpoints of the ceramic types (based on absolute dates or the
#'  known production interval) found in a given assemblage. The weights are the
#'  relative frequencies of the respective types in the assemblage.
#' @return
#'  A [`MeanDate-class`] object.
#' @seealso [`plot()`][plot.MeanDate], [`bootstrap()`][bootstrap.MeanDate],
#'  [`jackknife()`][jackknife.MeanDate]
#' @references
#'  South, S. A. (1977). *Method and Theory in Historical Archaeology*.
#'  New York: Academic Press.
#' @example inst/examples/ex-mcd.R
#' @author N. Frerebeau
#' @family mean ceramic date tools
#' @family dating methods
#' @docType methods
#' @rdname mcd
#' @aliases mcd-method
setGeneric(
  name = "mcd",
  def = function(object, dates, ...) standardGeneric("mcd")
)

# Event Dates ==================================================================
#' Event and Accumulation Dates
#'
#' Fits a date event model.
#' @param object A \eqn{m \times p}{m x p} `numeric` [`matrix`] or
#'  [`data.frame`] of count data (absolute frequencies giving the number of
#'  individuals for each category, i.e. a contingency table). A [`data.frame`]
#'  will be coerced to a `numeric` `matrix` via [data.matrix()].
#' @param dates A [`numeric`] vector of dates. If named, the names must match
#'  the row names of `object`.
#' @param calendar An [`aion::TimeScale-class`] object specifying the calendar
#'  of `dates` (see [aion::calendar()]). Defaults to Gregorian Common Era.
#' @param rank An [`integer`] specifying the number of CA factorial components
#'  to be use for linear model fitting (see details). If `NULL` (the default),
#'  axes corresponding to at least 60% of the inertia will be used.
#' @param sup_row A [`numeric`] or [`logical`] vector specifying the indices of
#'  the supplementary rows.
#' @param total A length-one [`numeric`] vector specifying the minimum total of
#'  a row/column. Rows/columns whose total is less than this value will be
#'  omitted from the analysis.
#' @param verbose A [`logical`] scalar: should \R report extra information
#'  on progress?
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
#'  Dates are converted to *[rata die][aion::RataDie-class]* before any
#'  computation.
#'
#'  This method relies on strong archaeological and statistical assumptions
#'  (see `vignette("event")`).
#' @return
#'  An [`EventDate-class`] object.
#' @seealso [`plot()`][plot.EventDate], [`bootstrap()`][bootstrap.EventDate],
#'  [`jackknife()`][jackknife.EventDate]
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
#' @family event date tools
#' @family dating methods
#' @docType methods
#' @aliases event-method
setGeneric(
  name = "event",
  def = function(object, dates, ...) standardGeneric("event"),
  valueClass = "EventDate"
)

#' Predict Event and Accumulation Dates
#'
#' Estimates the event and accumulation dates of an assemblage.
#' @param object An [`EventDate-class`] object.
#' @param data A `numeric` [`matrix`] or a [`data.frame`] of count data
#' (absolute frequencies) for which to predict event and accumulation dates.
#' @param calendar An [`aion::TimeScale-class`] object specifying the target
#'  calendar (see [aion::calendar()]). If `NULL`, *rata die* are returned.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param margin A [`numeric`] vector giving the subscripts which the prediction
#'  will be applied over: `1` indicates rows, `2` indicates columns.
#' @param ... Further arguments to be passed to internal methods.
#' @return
#'  A [`data.frame`].
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
#' @example inst/examples/ex-event.R
#' @author N. Frerebeau
#' @family event date tools
#' @docType methods
#' @aliases predict_event-method
setGeneric(
  name = "predict_event",
  def = function(object, data, ...) standardGeneric("predict_event"),
  valueClass = "data.frame"
)

#' @rdname predict_event
#' @aliases predict_accumulation-method
setGeneric(
  name = "predict_accumulation",
  def = function(object, data, ...) standardGeneric("predict_accumulation"),
  valueClass = "data.frame"
)

#' Density of Event and Accumulation Dates
#'
#' Estimates the event and accumulation density.
#' @param object An [`EventDate-class`] object.
#' @param dates A [`numeric`] vector of dates expressed as `calendar` years or
#'  *rata die* (if `calendar` is `NULL`).
#' @param calendar An [`aion::TimeScale-class`] object specifying the calendar
#'  of `dates` (see [aion::calendar()]). If `NULL` (the default), *rata die* are
#'  expected.
#' @param type A [`character`] string indicating the type of plot.
#'  It must be one of "`activity`" (default) or "`tempo`" (see details).
#'  Any unambiguous substring can be given.
#' @param n A length-one non-negative [`numeric`] vector giving the desired
#'  length of the vector of quantiles for density computation.
#' @param ... Currently not used.
#' @return
#'  An [`aion::TimeSeries-class`] object.
#' @example inst/examples/ex-event.R
#' @author N. Frerebeau
#' @family event date tools
#' @docType methods
#' @aliases density_event-method
setGeneric(
  name = "density_event",
  def = function(object, ...) standardGeneric("density_event")
)

#' @rdname density_event
#' @aliases density_accumulation-method
setGeneric(
  name = "density_accumulation",
  def = function(object, ...) standardGeneric("density_accumulation")
)

#' Extract Event Date Model Results
#'
#' @description
#'  * `summary()` summarizes linear model fit.
#'  * `coef()` extracts model coefficients (see [stats::coef()]).
#'  * `fitted()` extracts model fitted values (see [stats::fitted()]).
#'  * `residuals()` extracts model residuals (see [stats::residuals()]).
#'  * `sigma()` extracts the residual standard deviation (see [stats::sigma()]).
#'  * `terms()` extracts model terms (see [stats::terms()]).
#' @param x,object An [`EventDate-class`] object.
#' @param calendar An [`aion::TimeScale-class`] object specifying the target
#'  calendar (see [aion::calendar()]). If `NULL` (the default), *rata die* are
#'  returned.
#' @param ... Currently not used.
#' @example inst/examples/ex-event.R
#' @author N. Frerebeau
#' @family event date tools
#' @docType methods
#' @name model_event
#' @rdname model_event
NULL

# Aoristic Analysis ============================================================
#' Aoristic Analysis
#'
#' Computes the aoristic sum.
#' @param x,y A [`numeric`] vector giving the lower and upper boundaries of the
#'  time intervals, respectively. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param step A length-one [`integer`] vector giving the step size, i.e. the
#'  width of each time step in the time series (defaults to \eqn{1},
#'  i.e. annual level).
#' @param start A length-one [`numeric`] vector giving the beginning of the time
#'  window.
#' @param end A length-one [`numeric`] vector giving the end of the time
#'  window.
#' @param calendar An [`aion::TimeScale-class`] object specifying the calendar
#'  of `x` and `y` (see [aion::calendar()]). Defaults to Gregorian Common Era.
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
#'  An [`AoristicSum-class`] object.
#' @seealso [`plot()`][plot.AoristicSum]
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
#' @family aoristic analysis
#' @docType methods
#' @aliases aoristic-method
setGeneric(
  name = "aoristic",
  def = function(x, y, ...) standardGeneric("aoristic"),
  valueClass = "AoristicSum"
)

#' Rate of Change
#'
#' Computes the rate of change from an aoristic analysis.
#' @param object An [`AoristicSum-class`] object.
#' @param n A non-negative [`integer`] giving the number of replications (see
#'  details).
#' @param ... Currently not used.
#' @return
#'  A [`RateOfChange-class`] object.
#' @seealso [`plot()`][plot.AoristicSum]
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
#' @family aoristic analysis
#' @docType methods
#' @aliases roc-method
setGeneric(
  name = "roc",
  def = function(object, ...) standardGeneric("roc"),
  valueClass = "RateOfChange"
)

# Chronological Apportioning ===================================================
#' Chronological Apportioning
#'
#' @param object A \eqn{m \times p}{m x p} `numeric` [`matrix`] or
#'  [`data.frame`] of count data (absolute frequencies giving the number of
#'  individuals for each category, i.e. a contingency table). A [`data.frame`]
#'  will be coerced to a `numeric` `matrix` via [data.matrix()].
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
#' @return
#'  A [`CountApportion-class`] object.
#' @references
#'  Roberts, J. M., Mills, B. J., Clark, J. J., Haas, W. R., Huntley, D. L. &
#'  Trowbridge, M. A. (2012). A Method for Chronological Apportioning of Ceramic
#'  Assemblages. *Journal of Archaeological Science*, 39(5): 1513-20.
#'  \doi{10.1016/j.jas.2011.12.022}.
#' @example inst/examples/ex-apportion.R
#' @author N. Frerebeau
#' @family chronological apportioning methods
#' @family chronological analysis
#' @docType methods
#' @aliases apportion-method
setGeneric(
  name = "apportion",
  def = function(object, ...) standardGeneric("apportion"),
  valueClass = "CountApportion"
)

# Frequency Increment Test =====================================================
#' Frequency Increment Test
#'
#' @param object A \eqn{m \times p}{m x p} `numeric` [`matrix`] or
#'  [`data.frame`] of count data (absolute frequencies giving the number of
#'  individuals for each category, i.e. a contingency table). A [`data.frame`]
#'  will be coerced to a `numeric` `matrix` via [data.matrix()].
#' @param dates A length-\eqn{m} [`numeric`] vector of dates.
#' @param calendar An [`aion::TimeScale-class`] object specifying the calendar
#'  of `dates` (see [aion::calendar()]). Defaults to Gregorian Common Era.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param roll A [`logical`] scalar: should each time series be subsetted to
#'  look for episodes of selection?
#' @param window An odd [`integer`] giving the size of the rolling
#'  window. Only used if `roll` is `TRUE`.
#' @param ... Currently not used.
#' @details
#'  The Frequency Increment Test (FIT) rejects neutrality if the distribution
#'  of normalized variant frequency increments exhibits a mean that deviates
#'  significantly from zero.
#'
#'  If `roll` is `TRUE`, each time series is subsetted according to `window` to
#'  see if episodes of selection can be identified among variables that might
#'  not show overall selection.
#' @return
#'  An [`IncrementTest-class`] object.
#' @seealso [`plot()`][plot.IncrementTest]
#' @example inst/examples/ex-fit.R
#' @references
#'  Feder, A. F., Kryazhimskiy, S. & Plotkin, J. B. (2014). Identifying
#'  Signatures of Selection in Genetic Time Series. *Genetics*, 196(2):
#'  509-522. \doi{10.1534/genetics.113.158220}.
#' @author N. Frerebeau
#' @family chronological analysis
#' @docType methods
#' @aliases fit-method
setGeneric(
  name = "fit",
  def = function(object, dates, ...) standardGeneric("fit"),
  valueClass = "IncrementTest"
)

# Plot =========================================================================
#' Abundance vs Time Plot
#'
#' Produces an abundance *vs* time diagram.
#' @param object A \eqn{m \times p}{m x p} `numeric` [`matrix`] or
#'  [`data.frame`] of count data (absolute frequencies giving the number of
#'  individuals for each category, i.e. a contingency table). A [`data.frame`]
#'  will be coerced to a `numeric` `matrix` via [data.matrix()].
#' @param dates A [`numeric`] vector of dates.
#' @param calendar An [`aion::TimeScale-class`] object specifying the target
#'  calendar (see [aion::calendar()]).
#' @param ... Further parameters to be passed to [aion::plot()].
#' @return
#'   `plot_time()` is called it for its side-effects: it results in a graphic
#'   being displayed (invisibly returns `object`).
#' @example inst/examples/ex-plot.R
#' @author N. Frerebeau
#' @family plotting methods
#' @docType methods
#' @aliases plot_time-method
setGeneric(
  name = "plot_time",
  def = function(object, dates, ...) standardGeneric("plot_time")
)

#' MCD Plot
#'
#' @param x A [`MeanDate-class`] object.
#' @param calendar An [`aion::TimeScale-class`] object specifying the target
#'  calendar (see [aion::calendar()]).
#' @param interval A [`character`] string giving the type of confidence
#'  interval to be returned. It must be one "`student`" (the default),
#'  "`normal`", "`percentiles`" or "`range`" (min-max).
#'  Any unambiguous substring can be given.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#'  Only used if `interval` is not "`range`".
#' @param decreasing A [`logical`] scalar: should the sort be increasing or
#'  decreasing?
#' @param main A [`character`] string giving a main title for the plot.
#' @param sub A [`character`] string giving a subtitle for the plot.
#' @param ann A [`logical`] scalar: should the default annotation (title and x,
#'  y and z axis labels) appear on the plot?
#' @param axes A [`logical`] scalar: should axes be drawn on the plot?
#' @param frame.plot A [`logical`] scalar: should a box be drawn around the
#'  plot?
#' @param panel.first An an `expression` to be evaluated after the plot axes are
#'  set up but before any plotting takes place. This can be useful for drawing
#'  background grids.
#' @param panel.last An `expression` to be evaluated after plotting has taken
#'  place but before the axes, title and box are added.
#' @param ... Further [graphical parameters][graphics::par].
#' @return
#'  `plot()` is called it for its side-effects: it results in a graphic being
#'  displayed (invisibly returns `x`).
#' @seealso [mcd()]
#' @example inst/examples/ex-mcd.R
#' @author N. Frerebeau
#' @family plotting methods
#' @docType methods
#' @name plot.MeanDate
#' @rdname plot.MeanDate
NULL

#' Plot Aoristic Analysis
#'
#' @param x An [`AoristicSum-class`] object.
#' @param calendar An [`aion::TimeScale-class`] object specifying the target
#'  calendar (see [aion::calendar()]).
#' @param type A [`character`] string specifying whether bar or density should
#'  be plotted? It must be one of "`bar`" or "`density`". Any unambiguous
#'  substring can be given.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @inheritParams aion::plot
#' @return
#'   `plot()` is called it for its side-effects: it results in a graphic being
#'   displayed (invisibly returns `x`).
#' @seealso [aoristic()]
#' @example inst/examples/ex-aoristic.R
#' @author N. Frerebeau
#' @family plotting methods
#' @docType methods
#' @name plot.AoristicSum
#' @rdname plot.AoristicSum
NULL

#' Plot Event and Accumulation Dates
#'
#' Produces an activity or a tempo plot.
#' @param x An [`EventDate-class`] object.
#' @param calendar An [`aion::TimeScale-class`] object specifying the target
#'  calendar (see [aion::calendar()]).
#' @param type A [`character`] string indicating the type of plot.
#'  It must be one of "`activity`" (default) or "`tempo`" (see details).
#'  Any unambiguous substring can be given.
#' @param event A [`logical`] scalar: should the distribution of the event date
#'  be displayed? Only used if type is "`activity`".
#' @param select A [`numeric`] or [`character`] vector giving the selection of
#'  the assemblage that are drawn.
#' @param n A length-one non-negative [`numeric`] vector giving the desired
#'  length of the vector of quantiles for density computation.
#' @param eps A length-one [`numeric`] value giving the cutoff below which
#'  values will be removed.
#' @param col.accumulation A color specification for the accumulation density
#'  curve.
#' @param col.event A color specification for the event density curve.
#' @inheritParams aion::plot
#' @section Event and Acccumulation Dates:
#'  `plot()` displays the probability estimate density curves of archaeological
#'  assemblage dates (*event* and *accumulation* dates; Bellanger and Husi
#'  2012). The *event* date is plotted as a line, while the *accumulation* date
#'  is shown as a grey filled area.
#'
#'  The accumulation date can be displayed as a tempo plot (Dye 2016) or an
#'  activity plot (Philippe and Vibet 2020):
#'  \describe{
#'   \item{`tempo`}{A tempo plot estimates the cumulative occurrence of
#'   archaeological events, such as the slope of the plot directly reflects the
#'   pace of change.}
#'   \item{`activity`}{An activity plot displays the first derivative of the
#'   tempo plot.}
#'  }
#' @return
#'  `plot()` is called it for its side-effects: it results in a graphic being
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
#'  Philippe, A. & Vibet, M.-A. (2020). Analysis of Archaeological Phases Using
#'  the R Package ArchaeoPhases. *Journal of Statistical Software, Code
#'  Snippets*, 93(1), 1-25. \doi{10.18637/jss.v093.c01}.
#' @seealso [event()]
#' @example inst/examples/ex-event.R
#' @author N. Frerebeau
#' @family plotting methods
#' @docType methods
#' @name plot.EventDate
#' @rdname plot.EventDate
NULL

#' Detection of Selective Processes
#'
#' Produces an abundance *vs* time diagram.
#' @param x An [`IncrementTest-class`] object to be plotted.
#' @param calendar An [`aion::TimeScale-class`] object specifying the target
#'  calendar (see [aion::calendar()]).
#' @param col.neutral,col.selection,col.roll A vector of colors.
#' @inheritParams aion::plot
#' @details
#'  Results of the frequency increment test can be displayed on an abundance
#'  *vs* time diagram aid in the detection and quantification of selective
#'  processes in the archaeological record. If `roll` is `TRUE`, each time
#'  series is subsetted according to `window` to see if episodes of selection
#'  can be identified among decoration types that might not show overall
#'  selection. If so, shading highlights the data points where
#'  [fit()] identifies selection.
#' @return
#'  `plot()` is called it for its side-effects: it results in a graphic being
#'  displayed (invisibly returns `x`).
#' @note
#'  Displaying FIT results on an abundance *vs* time diagram is adapted from Ben
#'  Marwick's [original idea](https://github.com/benmarwick/signatselect/).
#' @seealso [fit()]
#' @example inst/examples/ex-fit.R
#' @author N. Frerebeau
#' @family plotting methods
#' @docType methods
#' @name plot.IncrementTest
#' @rdname plot.IncrementTest
NULL

# Resample =====================================================================
#' Bootstrap Mean Ceramic Dates
#'
#' Generates bootstrap estimations of an [MCD][mcd()].
#' @param object A [`MeanDate-class`] object (typically returned by [mcd()]).
#' @param n A non-negative [`integer`] specifying the number of bootstrap
#'  replications.
#' @param f A [`function`] that takes a single numeric vector (the result of
#'  the resampling procedure) as argument.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#'  Must be a single number between \eqn{0} and \eqn{1}. Only used if `f` is
#'  `NULL`.
#' @param interval A [`character`] string giving the type of confidence
#'  interval to be returned. It must be one "`basic`" (the default), "`normal`"
#'  or "`percentiles`" (see [arkhe::confidence_bootstrap()]). Any unambiguous
#'  substring can be given. Only used if `f` is `NULL`.
#' @param seed An object specifying if and how the random number generator
#'  should be initialized (see [stats::simulate()]).
#' @param calendar An [`aion::TimeScale-class`] object specifying the target
#'  calendar (see [aion::calendar()]).
#' @return
#'  If `f` is `NULL`, `bootstrap()` returns a [`data.frame`] with the following
#'  elements (else, returns the result of `f` applied to the `n` resampled
#'  values) :
#'  \describe{
#'   \item{`original`}{The observed value.}
#'   \item{`mean`}{The bootstrap estimate of mean.}
#'   \item{`bias`}{The bootstrap estimate of bias.}
#'   \item{`error`}{The boostrap estimate of standard error.}
#'   \item{`lower`}{The lower limit of the bootstrap confidence interval at `level`.}
#'   \item{`upper`}{The upper limit of the bootstrap confidence interval at `level`.}
#'  }
#' @seealso [mcd()]
#' @example inst/examples/ex-mcd.R
#' @author N. Frerebeau
#' @docType methods
#' @family resampling methods
#' @name bootstrap.MeanDate
#' @rdname bootstrap.MeanDate
NULL

#' Jackknife Mean Ceramic Dates
#'
#' Generate jackknife estimations of an [MCD][mcd()].
#' @param object A [`MeanDate-class`] object (typically returned by [mcd()]).
#' @param f A [`function`] that takes a single numeric vector (the result of
#'  the resampling procedure) as argument.
#' @param calendar An [`aion::TimeScale-class`] object specifying the target
#'  calendar (see [aion::calendar()]).
#' @return
#'  If `f` is `NULL`, `jackknife()` returns a [`data.frame`] with the following
#'  elements (else, returns the result of `f` applied to the `n` resampled
#'  values) :
#'  \describe{
#'   \item{`original`}{The observed value.}
#'   \item{`mean`}{The jackknife estimate of mean.}
#'   \item{`bias`}{The jackknife estimate of bias.}
#'   \item{`error`}{The jackknife estimate of standard erro.}
#'  }
#' @seealso [mcd()]
#' @example inst/examples/ex-mcd.R
#' @author N. Frerebeau
#' @docType methods
#' @family resampling methods
#' @name jackknife.MeanDate
#' @rdname jackknife.MeanDate
NULL

#' Bootstrap Event Dates
#'
#' Generates bootstrap estimations of an [event date][event()].
#' @param object An [`EventDate-class`] object (typically returned by [event()]).
#' @param n A non-negative [`integer`] specifying the number of bootstrap
#'  replications.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param probs A [`numeric`] vector of probabilities with values in
#'  \eqn{[0,1]}.
#' @param calendar An [`aion::TimeScale-class`] object specifying the target
#'  calendar (see [aion::calendar()]). If `NULL`, *rata die* are returned.
#' @param progress A [`logical`] scalar: should a progress bar be displayed?
#' @param ... Currently not used.
#' @details
#'  A large number of new bootstrap assemblages is created, with the same sample
#'  size, by resampling each of the original assemblage with replacement. Then,
#'  examination of the bootstrap statistics makes it possible to pinpoint
#'  assemblages that require further investigation.
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
#' @seealso [event()]
#' @return
#'  A [`data.frame`].
#' @author N. Frerebeau
#' @docType methods
#' @family resampling methods
#' @name bootstrap.EventDate
#' @rdname bootstrap.EventDate
NULL

#' Jackknife Event Dates
#'
#' Generates jackknife estimations of an [event date][event()].
#' @param object An [`EventDate-class`] object (typically returned by [event()]).
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param calendar An [`aion::TimeScale-class`] object specifying the target
#'  calendar (see [aion::calendar()]). If `NULL`, *rata die* are returned.
#' @param progress A [`logical`] scalar: should a progress bar be displayed?
#' @param verbose A [`logical`] scalar: should \R report extra information
#'  on progress?
#' @param ... Further arguments to be passed to internal methods.
#' @details
#'  One type/fabric is removed at a time and all statistics are recalculated.
#'  In this way, one can assess whether certain type/fabric has a substantial
#'  influence on the date estimate.
#'
#'  A three columns `data.frame` is returned, giving the results of the
#'  resampling procedure (jackknifing fabrics) for each assemblage (in rows)
#'  with the following columns:
#'  \describe{
#'   \item{`mean`}{The jackknife mean (event date).}
#'   \item{`lower`}{The lower boundary of the confidence interval.}
#'   \item{`upper`}{The upper boundary of the confidence interval.}
#'  }
#' @return
#'  A [`data.frame`].
#' @seealso [event()]
#' @author N. Frerebeau
#' @docType methods
#' @family resampling methods
#' @name jackknife.EventDate
#' @rdname jackknife.EventDate
NULL

# Seriation Methods ============================================================
## Reciprocal ranking ----------------------------------------------------------
#' Reciprocal Ranking Seriation
#'
#' @param object A \eqn{m \times p}{m x p} `numeric` [`matrix`] or
#'  [`data.frame`] of count data (absolute frequencies giving the number of
#'  individuals for each category, i.e. a contingency table). A [`data.frame`]
#'  will be coerced to a `numeric` `matrix` via [data.matrix()].
#' @param EPPM A [`logical`] scalar: should the seriation be computed on EPPM
#'  instead of raw data?
#' @param margin A [`numeric`] vector giving the subscripts which the
#'  rearrangement will be applied over: `1` indicates rows, `2` indicates
#'  columns, `c(1, 2)` indicates rows then columns, `c(2, 1)` indicates columns
#'  then rows.
#' @param stop An [`integer`] giving the stopping rule (i.e. maximum number of
#'  iterations) to avoid infinite loop.
#' @param ... Currently not used.
#' @details
#'  This procedure iteratively rearrange rows and/or columns according to their
#'  weighted rank in the data matrix until convergence.
#'
#'  Note that this procedure could enter into an infinite loop. If no
#'  convergence is reached before the maximum number of iterations, it stops
#'  with a warning.
#' @return
#'  A [RankPermutationOrder-class] object.
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
#' @example inst/examples/ex-seriation.R
#' @author N. Frerebeau
#' @family seriation methods
#' @docType methods
#' @aliases seriate_rank-method
setGeneric(
  name = "seriate_rank",
  def = function(object, ...) standardGeneric("seriate_rank"),
  valueClass = "PermutationOrder"
)

## Average Ranking -------------------------------------------------------------
#' Correspondence Analysis-Based Seriation
#'
#' @param object A \eqn{m \times p}{m x p} `numeric` [`matrix`] or
#'  [`data.frame`] of count data (absolute frequencies giving the number of
#'  individuals for each category, i.e. a contingency table). A [`data.frame`]
#'  will be coerced to a `numeric` `matrix` via [data.matrix()].
#' @param margin A [`numeric`] vector giving the subscripts which the
#'  rearrangement will be applied over: `1` indicates rows, `2` indicates
#'  columns, `c(1, 2)` indicates rows then columns, `c(2, 1)` indicates columns
#'  then rows.
#' @param axes An [`integer`] vector giving the subscripts of the CA axes to be
#'  used.
#' @param sup_row A `vector` specifying the indices of the supplementary rows
#'  (see [dimensio::ca()]).
#' @param sup_col A `vector` specifying the indices of the supplementary columns
#'  (see [dimensio::ca()]).
#' @param ... Currently not used.
#' @details
#'  Correspondence analysis (CA) is an effective method for the seriation of
#'  archaeological assemblages. The order of the rows and columns is given by
#'  the coordinates along one dimension of the CA space, assumed to account for
#'  temporal variation. The direction of temporal change within the
#'  correspondence analysis space is arbitrary: additional information is needed
#'  to determine the actual order in time.
#' @return
#'  An [`AveragePermutationOrder-class`] object.
#' @references
#'  Ihm, P. (2005). A Contribution to the History of Seriation in Archaeology.
#'  In C. Weihs & W. Gaul (Eds.), *Classification: The Ubiquitous
#'  Challenge*. Berlin Heidelberg: Springer, p. 307-316.
#'  \doi{10.1007/3-540-28084-7_34}.
#' @seealso [dimensio::ca()]
#' @example inst/examples/ex-seriation.R
#' @author N. Frerebeau
#' @family seriation methods
#' @docType methods
#' @aliases seriate_average-method
setGeneric(
  name = "seriate_average",
  def = function(object, ...) standardGeneric("seriate_average"),
  valueClass = "PermutationOrder"
)

# @rdname seriate_constrain
# @aliases seriate_constrain-method
# setGeneric(
#   name = "seriate_constrain",
#   def = function(object, constrain, ...) standardGeneric("seriate_constrain"),
#   valueClass = "PermutationOrder"
# )

# @rdname seriate_idds
# @aliases seriate_idds-method
# setGeneric(
#   name = "seriate_idds",
#   def = function(object, ...) standardGeneric("seriate_idds"),
#   valueClass = "PermutationOrder"
# )

#' Coerce an \R Object to a Seriation Order
#'
#' @param object An \R object.
#' @param margin A [`numeric`] vector giving the subscripts which the
#'  rearrangement will be applied over: `1` indicates rows, `2` indicates
#'  columns, `c(1, 2)` indicates rows then columns, `c(2, 1)` indicates columns
#'  then rows.
#' @param axes An [`integer`] vector giving the subscripts of the CA axes to be
#'  used.
#' @param ... Currently not used.
#' @return
#'  A [`PermutationOrder-class`] object.
#' @author N. Frerebeau
#' @family seriation methods
#' @docType methods
#' @aliases as_seriation-method
setGeneric(
  name = "as_seriation",
  def = function(object, ...) standardGeneric("as_seriation")
)

## Refine ----------------------------------------------------------------------
#' Refine CA-based Seriation
#'
#' @param object A [`PermutationOrder-class`] object (typically returned by
#'  [seriate_average()]) or a [`dimensio::BootstrapCA-class`] object (typically
#'  returned by [dimensio::bootstrap()]).
#' @param cutoff A function that takes a numeric vector as argument and returns
#'  a single numeric value (see below).
#' @param n A non-negative [`integer`] giving the number of bootstrap
#'  replications.
#' @param margin A length-one [`numeric`] vector giving the subscripts which the
#'  refinement will be applied over: `1` indicates rows, `2` indicates columns.
#' @param axes An [`integer`] vector giving the subscripts of the CA axes to be
#'  used.
#' @param ... Currently not used.
#' @details
#'  `refine()` allows to identify samples that are subject to sampling error or
#'  samples that have underlying structural relationships and might be
#'  influencing the ordering along the CA space.
#'
#'  This relies on a partial bootstrap approach to CA-based seriation where each
#'  sample is replicated `n` times. The maximum dimension length of the convex
#'  hull around the sample point cloud allows to remove samples for a given
#'  `cutoff` value.
#'
#'  According to Peebles and Schachner (2012), "\[this\] point removal procedure
#'  \[results in\] a reduced dataset where the position of individuals within
#'  the CA are highly stable and which produces an ordering consistent with the
#'  assumptions of frequency seriation."
#'
#'  See `vignette("seriation")`.
#' @return
#'  A [`list`] with the following elements:
#'  \describe{
#'   \item{`length`}{A [`numeric`] vector giving the convex hull maximum
#'   dimension length.}
#'   \item{`cutoff`}{A [`numeric`] value giving the cutoff value for samples
#'   selection.}
#'   \item{`exclude`}{An [`integer`] vector giving the subscript of the
#'   observations to be removed.}
#'   \item{`margin`}{A [`numeric`] value specifying the dimension along which
#'   the refinement procedure has been applied: `1` indicates rows,
#'   `2` indicates columns.}
#'  }
#' @references
#'  Peeples, M. A., & Schachner, G. (2012). Refining correspondence
#'  analysis-based ceramic seriation of regional data sets. *Journal of
#'  Archaeological Science*, 39(8), 2818-2827.
#'  \doi{10.1016/j.jas.2012.04.040}.
#' @seealso [dimensio::bootstrap()]
#' @author N. Frerebeau
#' @docType methods
#' @family seriation methods
#' @aliases refine-method
setGeneric(
  name = "refine",
  def = function(object, ...) standardGeneric("refine")
)

## Assess ----------------------------------------------------------------------
#' Statistical Significance of Seriation Solutions
#'
#' Tests the significance of seriation solutions.
#' @param object A [`PermutationOrder-class`] object giving the permutation
#'  order for rows and columns (typically returned by [seriate_average()]).
#' @param axes An [`integer`] vector giving the subscripts of the CA axes to be
#'  used.
#' @param n A non-negative [`integer`] giving the number of bootstrap
#'  replications.
#' @param progress A [`logical`] scalar: should a progress bar be displayed?
#' @param ... Currently not used.
#' @return
#'  A [`list`] with the following elements:
#'  \describe{
#'   \item{`random`}{A [`numeric`] vector giving the randomized total number of
#'   modes values.}
#'   \item{`observed`}{A [`numeric`] value giving the observed total number of
#'   modes.}
#'   \item{`expected`}{A [`numeric`] value giving the expected total number of
#'   modes if all types had unimodal distributions.}
#'   \item{`maximum`}{A [`numeric`] value giving the maximum total number of
#'   modes.}
#'   \item{`coef`}{A [`numeric`] value giving the seriation coefficient (a value
#'   close to 1 indicates a strong fit to the seriation model, while a value
#'   close to 0 indicates a poor fit).}
#'  }
#' @references
#'  Porčić, M. (2013). The Goodness of Fit and Statistical Significance of
#'  Seriation Solutions. *Journal of Archaeological Science*, 40(12): 4552-4559.
#'  \doi{10.1016/j.jas.2013.07.013}.
#' @example inst/examples/ex-assess.R
#' @author N. Frerebeau
#' @docType methods
#' @family seriation methods
#' @aliases assess-method
setGeneric(
  name = "assess",
  def = function(object, ...) standardGeneric("assess")
)

## Permute ---------------------------------------------------------------------
#' Rearrange a Data Matrix
#'
#' Rearranges a data matrix according to a permutation order.
#' @param object A \eqn{m \times p}{m x p} `numeric` [`matrix`] or
#'  [`data.frame`] of count data (absolute frequencies giving the number of
#'  individuals for each category, i.e. a contingency table).
#' @param order A [`PermutationOrder-class`] object giving the permutation
#'  order for rows and columns.
#' @param ... Currently not used.
#' @return
#'  A permuted `matrix` or a permuted `data.frame` (the same as `object`).
#' @example inst/examples/ex-seriation.R
#' @author N. Frerebeau
#' @family seriation methods
#' @docType methods
#' @aliases permute-method
setGeneric(
  name = "permute",
  def = function(object, order, ...) standardGeneric("permute")
)

#' Permutation Order
#'
#' Returns the seriation order for rows and/or columns.
#' @param object A [`PermutationOrder-class`] object giving the permutation
#'  order for rows and columns.
#' @param ... Currently not used.
#' @return
#'  An [`integer`] vector.
#' @example inst/examples/ex-seriation.R
#' @author N. Frerebeau
#' @family seriation methods
#' @docType methods
#' @name order
#' @rdname order
NULL

#' @rdname order
#' @aliases order_rows-method
setGeneric(
  name = "order_rows",
  def = function(object, ...) standardGeneric("order_rows")
)

#' @rdname order
#' @aliases order_columns-method
setGeneric(
  name = "order_columns",
  def = function(object, ...) standardGeneric("order_columns")
)
