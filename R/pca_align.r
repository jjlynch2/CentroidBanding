#' Conducts PCA and returns scores
#'
#' @param first_configuration Point cloud to be aligned along principal components
#'
#' @keywords pca_align
#' @export
#' @examples
#' pca_align()

pca_align <- function(configuration = NULL) {
	results <- scale(configuration, scale=F)%*%eigen(var(configuration))$vectors
	return(results)
}