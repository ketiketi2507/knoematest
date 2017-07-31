#' knoematest package!
#' This package works with datasets from knoema.com
#' @param datasetId is Dataset's ID specified as a string
#' @param selection is list where all the dimensions of the dataset are listed and a selection from them
#' @param type is optional. By default equal "ts". Other supported variants are "xts" and "zoo"
#' @param client is optional. By default use public user for knoema.com
#' By default the package allows you to work only with public datasets from the site knoema.com.
#' If you want work with private datasets or from other hosts, you need create ApiClient
#' client = ApiClient("host","some_app_id","some_app_secret") and then use this client in funcion knoema.get
#' You can get parameters app_id and app_secret after registering on the site knoema.com, in the section "My profile - Apps - create new" (or use existing applications)
#' @examples
#' knoema.get('IMFWEO2017Apr',list(country="512;914",subject ="NGDP_RPCH"))
#' knoema.get('IMFWEO2017Apr',list(country="512;914",subject ="NGDP_RPCH"),type="xts")
#' knoema.get('IMFWEO2017Apr',list(country="512;914",subject ="NGDP_RPCH"),type="zoo", client)
#' @export

knoema.get <- function(datasetId, selection,type="ts",client = ApiClient())
{
  dataset <- client$get_dataset(datasetId)
  if (is.character(dataset))
  {
    error = simpleError(dataset)
    stop(error)
  }
  if (is.null(dataset))
  {
    error = simpleError('You have incorrect parameters app_id or app_secret')
    stop(error)
  }    
  data_reader <- DataReader(client,dataset,selection)
  series <- data_reader$get_frame(type)
  return (series)
}
