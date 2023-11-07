is_scalar_or_null = function(x) {
  assertthat::is.scalar(x) || is.null(x)
}

is_count_or_null = function(x) {
  assertthat::is.count(x) || is.null(x)
}

as_or_null = function(x, f) {
  if (is.null(x)) {
    return(x)
  }
  f(x)
}

#' Read a WFDB record and return the signal and record descriptors as
#' attributes in a Record or MultiRecord object.
#'
#' @param path The name of the WFDB record to be read, without any
#' file extensions. If the argument contains any path delimiter characters,
#' the argument will be interpreted as PATH/BASE_RECORD.
#' Both relative and absolute paths are accepted.
#' If the pn_dir parameter is set, this parameter should contain
#' just the base record name, and the files fill be searched for
#' remotely. Otherwise, the data files will be searched for in the local path.
#' @param sample_from (integer or NULL)
#' The starting sample number to read for all channels; 0-indexed.
#' @param sample_to (integer or NULL)
#' The sample number at which to stop reading for
#' all channels. Reads the entire duration by default; 0-indexed
#' @param channels List of integer indices specifying the channels to be read.
#' Reads all channels by default.
#' @param physical Specifies whether to return signals in
#' physical units in the `p_signal` field (TRUE), or
#' digital units in the `d_signal` field (`FALSE`).
#' @param m2s Used when reading multi-segment records.
#' Specifies whether to directly return a WFDB `MultiRecord` object (`FALSE`),
#' or to convert it into and return a WFDB `Record` object (`TRUE`).
#' @param smooth_frames Specifies whether to smooth the samples in signals
#' with more than one sample per frame and return an (MxN) uniform numpy a
#' rray as the d_signal or p_signal field (True), or to return a list of
#'  1d numpy arrays containing every expanded sample as the e_d_signal or
#'  e_p_signal field (False).
#' @param ignore_skew Used when reading records with at least one skewed
#' signal. Specifies whether to apply the skew to align the signals in the
#' output variable (False), or to ignore the skew field and load in all
#' values contained in the dat files unaligned (True).
#' @param return_float_type The numpy array `dtype` of the returned signals.
#' Options are: 64, 32, 16, and 8, where the value represents the
#' numpy `int` or `float` `dtype.` Note that the value cannot be 8 when
#' physical is True since there is no float8 format.
#' @param force_channels Used when reading multi-segment variable layout
#' records. Whether to update the layout specification record,
#' and the converted Record object if m2s is True, to match the input
#' channels argument, or to omit channels in which no read segment
#' contains the signals.
#' @param channel_names List of channel names to return. If this
#' parameter is specified, it takes precedence over channels.
#' @param warn_empty Whether to display a warning if the specified channel
#' indices or names are not contained in the record, and no signal is returned.
#'
#' @note https://wfdb.readthedocs.io/en/latest/io.html#module-wfdb.io
#'
#' @return A `Record` output
#' @export
#' @rdname wf_read
#'
#' @examples
wf_read_record = function(
    path,
    sample_from = 0L,
    sample_to = NULL,
    channels = NULL,
    physical = TRUE,
    m2s = TRUE,
    smooth_frames = TRUE,
    ignore_skew = FALSE,
    return_float_type = c(64L, 32L, 16L, 8L),
    force_channels = TRUE,
    channel_names = NULL,
    warn_empty = FALSE,
    ...) {
  path = normalizePath(path.expand(path), mustWork = FALSE)
  path = tools::file_path_sans_ext(path, compression = TRUE)

  return_float_type = return_float_type[1]
  assertthat::assert_that(
    is_scalar_or_null(sample_to),
    assertthat::is.count(sample_from) || sample_from == 0,
    assertthat::is.flag(physical),
    assertthat::is.flag(m2s),
    assertthat::is.flag(smooth_frames),
    assertthat::is.flag(ignore_skew),
    assertthat::is.flag(force_channels),
    assertthat::is.flag(warn_empty),
    is_count_or_null(return_float_type)
  )
  return_float_type = as.integer(return_float_type)
  sample_to = as_or_null(sample_to, as.integer)
  sample_from = as_or_null(sample_from, as.integer)

  mod = wfdb()
  res = mod$rdrecord(
    record_name = path,
    sampfrom = sample_from,
    sampto = sample_to,
    channels = channels,
    physical = physical,
    m2s = m2s,
    smooth_frames = smooth_frames,
    ignore_skew = ignore_skew,
    return_res = return_float_type,
    force_channels = force_channels,
    channel_names = channel_names,
    warn_empty = warn_empty,
    ...
  )
  return(res)
}

#' @export
#' @rdname wf_read
wf_read_sample = function(
    path,
    sample_from = 0L,
    sample_to = NULL,
    channels = NULL,
    return_float_type = c(64L, 32L, 16L, 8L),
    channel_names = NULL,
    warn_empty = FALSE,
    ...) {
  path = normalizePath(path.expand(path), mustWork = FALSE)
  path = tools::file_path_sans_ext(path, compression = TRUE)

  return_float_type = return_float_type[1]
  assertthat::assert_that(
    is_scalar_or_null(sample_to),
    assertthat::is.count(sample_from) || sample_from == 0,
    assertthat::is.flag(warn_empty),
    is_count_or_null(return_float_type)
  )
  return_float_type = as.integer(return_float_type)
  sample_to = as_or_null(sample_to, as.integer)
  sample_from = as_or_null(sample_from, as.integer)

  mod = wfdb()
  res = mod$rds(
    record_name = path,
    sampfrom = sample_from,
    sampto = sample_to,
    channels = channels,
    return_res = return_float_type,
    channel_names = channel_names,
    warn_empty = warn_empty,
    ...
  )
  return(res)
}

#' @export
#' @rdname wf_read
wf_read = function(
    path,
    sample_from = 0L,
    sample_to = NULL,
    channels = NULL,
    physical = TRUE,
    m2s = TRUE,
    smooth_frames = TRUE,
    ignore_skew = FALSE,
    return_float_type = c(64L, 32L, 16L, 8L),
    force_channels = TRUE,
    channel_names = NULL,
    warn_empty = FALSE,
    ...) {
  res = wf_read_record(
    path = path,
    sample_from = sample_from,
    sample_to = sample_to,
    channels = channels,
    physical = physical,
    m2s = m2s,
    smooth_frames = smooth_frames,
    ignore_skew = ignore_skew,
    return_float_type = return_float_type,
    force_channels = force_channels,
    channel_names = channel_names,
    warn_empty = warn_empty,
    ...)
  df = res$to_dataframe()
  for (i in seq_len(ncol(df))) {
    attr(df[[i]], "units") = res$units[i]
  }
  attr(df, "comments") = res$comments
  attr(df, "checksum") = res$checksum
  df
}


#' @export
#' @rdname wf_read
#' @param rd_segments Used when reading multi-segment headers.
#' If `TRUE`, segment headers will also be read
#' (into the record object’s segments field).
wf_read_header = function(
    path,
    rd_segments = TRUE,
    ...) {
  path = normalizePath(path.expand(path), mustWork = FALSE)
  path = tools::file_path_sans_ext(path, compression = TRUE)

  assertthat::assert_that(
    assertthat::is.flag(rd_segments)
  )

  mod = wfdb()
  res = mod$rdheader(
    record_name = path,
    rd_segments = rd_segments,
    ...
  )
  return(res)
}
