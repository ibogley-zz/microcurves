#`Plots a firm's cost curves given its total cost function in terms of quantity supplied.`
#' @export
Cost_curves <- function(TC_formula="(q^2) + 100",qmin = 0,qmax = 25) {
  TC_formula <- parse(text = TC_formula)
  MC_formula <- D(parse(text =TC_formula),"q")
  z = 0
  FC <- eval(parse(text = gsub("q","z",TC_formula)))
  data <- dplyr::mutate(data.frame(q = qmin:qmax),TC = eval(TC_formula),AC = TC/q, MC = eval(MC_formula),AVC = TC-TC[1])
    ggplot2::ggplot() + ggplot2::geom_line(data=data,ggplot2::aes(x = q, y = TC, col = "Total Cost"),size = 2) +
    ggplot2::geom_line(data=data,ggplot2::aes(x = q, y = AC, col = "Average Cost"),size = 1.5) +
    ggplot2::geom_line(data=data,ggplot2::aes(x = q, y = MC, col = "Marginal Cost"), size = 1.5) +
    ggplot2::geom_line(data=data,ggplot2::aes(x = q, y = AVC, col = "Average Variable Cost"), size = 1.5) +
    ggplot2::scale_color_manual(values = c("red","blue","green","black")) +
    ggplot2::labs(title = "Cost Curves",subtitle = paste("Total Cost function:",TC_formula)) +
    ggplot2::ylab("Cost ($)") + ggplot2::xlab("Quantity Produced (q)") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = .5),plot.subtitle = ggplot2::element_text(hjust = .5)) +
    ggplot2::geom_hline(yintercept = 0) + ggplot2::geom_vline(xintercept = 0)
}
