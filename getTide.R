#' \code{getTide} Extracts water level info for given location and time period
#' @param lat Latitude (in decimal degrees)
#' @param lon Longitude (in decimal degrees)
#' @param date String representing year, month and day (e.g. "2009-09-01")
#' @param time Start and end time of a specific period (e.g. start and end of a haulout)
#' @param type Type of data. TAB=table of high and low tide, PRE=predictions(astronomic tide), 
#' OBS=observations (measured water level), or ALL=Predictions, weather effect and forecast. 
#' @param interval Interval of observations. Either 10 (10 min) or 60 (1 hour)
#' @return Data frame wit columns: \code{time}: Time of observation (in UTC), 
#' \code{level}: Water level relative to chart datum, and \code{flag}: Type of data
#' @details Water level info is downloaded via API from kartverket.no. For details of format, see \link{https://api.sehavniva.no/tideapi_protocol.pdf}
#' @family SMRU SRDL functions
#' @seealso \code{\link{fields.SRDLdb}} for table field names,
#'   \code{\link{ref.SRDLdb}} for ref ID codes,
#'   \code{\link{dep.SRDLdb}} for retrieving deployments metadata,
#'   \code{\link{get.SRDLdb}} for querying and retrieving data from database table
#' @author Martin Biuw
#' @examples
#' map.SRDLdb(hp4$diag)
#' map.SRDLdb(hp4$diag, type="leaflet")
#' @importFrom leaflet leaflet
#' @importFrom mapdata worldHires
#' @importFrom  maps map
#' @importFrom randomcoloR randomColor
#' @export

getTide <- function(lat=70.3, lon=25.5, date="2009-09-01", time=NA, type='ALL', interval=10) {
  require(httr)
  require(XML)
  require(xml2)
  
  if(any(is.na(time))) {
      time <- lubridate::as_datetime(date)+c(0,86400)
      ttime <- as.character(time)
      ttime <- paste(ttime, rep('T00:00', 2), sep='')
  } else {
    ttime <- format(time, '%Y-%m-%d %H:%M')
    ttime <- gsub(' ', 'T', ttime)
  }
  
  theUrl <- paste("http://api.sehavniva.no/tideapi.php?tide_request=locationdata&lat=",
                  lat, "&lon=", lon, "&datatype=", type, "&refcode=CD&fromtime=",
                  ttime[1], "&totime=", ttime[2], "&interval=", interval, sep='')
  
  resp <- GET(theUrl) 
  cont <- as_list(read_xml(resp))
  dat <- lapply(grep('data', names(cont$tide$locationdata)), function(x) {
    tmp <- lapply(cont$tide$locationdata[[x]], function(xx) {
      df <- data.frame(time=lubridate::as_datetime(attr(xx, 'time')),
                       level=as.numeric(attr(xx, 'value')))
      if(!is.null(attr(xx, 'flag'))) {
        names(df)[2] <- attr(xx, 'flag')
      } else {
        names(df)[2] <- 'weather'
      } 
      df
    })
    tmp <- do.call('rbind', tmp)
    row.names(tmp) <- c(1:nrow(tmp))
    tmp
  })
  if(length(dat)>=2) {
    out <- merge(dat[[1]], dat[[2]], by='time')
    if(length(dat)>=3) {
      for(i in 3:length(dat)) {
        out <- merge(out, dat[[i]], by='time')
      }
    }
  } else {
    out <- dat[[1]]
  }
  
  if(length(out)>2) {
    par(mfrow=c(length(out)-2, 1), mar=c(1,4,1,1))
    matplot(out[,1], out[,c(2,3)], type='l', lty=1, 
            ylab='cm above CD') 
    if(length(out)==4) {
      plot(out[,1], out[,4], type='l', col=3,
           ylab=names(out)[4])
    }
  } else {
    plot(out[,1], out[,2], ylab=names(out)[2], type='l')
  } 
  out
}


