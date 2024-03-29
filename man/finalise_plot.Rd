% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/doc_theme_pem.R
\name{finalise_plot}
\alias{finalise_plot}
\title{Arrange alignment and save BBC ggplot chart}
\usage{
finalise_plot(plot_name, filename, width_pixels = 640, height_pixels = 450)
}
\arguments{
\item{plot_name}{The variable name of the plot you have created that you want to format and save}

\item{filename}{Exact filepath that you want the plot to be saved to}

\item{width_pixels}{Width in pixels that you want to save your chart to - defaults to 640}

\item{height_pixels}{Height in pixels that you want to save your chart to - defaults to 450}

\item{source_name}{The text you want to come after the text 'Source:' in the bottom left hand side of your side}

\item{logo_image_path}{File path for the logo image you want to use in the right hand side of your chart,
which needs to be a PNG file - defaults to BBC blocks image that sits within the data folder of your package}
}
\value{
(Invisibly) an updated ggplot object.
}
\description{
Running this function will save your plot with the correct guidelines for publication for a BBC News graphic.
It will left align your title, subtitle and source, add the BBC blocks at the bottom right and save it to your specified location.
}
\examples{
finalise_plot(plot_name = myplot,
source = "The source for my data",
save_filepath = "filename_that_my_plot_should_be_saved_to-nc.png",
width_pixels = 640,
height_pixels = 450,
logo_image_path = "logo_image_filepath.png"
)

}
\keyword{finalise_plot}
