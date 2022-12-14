.onAttach <- function(lib, pkg)  {
  vers <- utils::packageDescription("containr",
                                    fields ="Version")

  bsu_rule_color <- "#7FD2FF"
  bsu_main_color <- "#18bc9c"

  bsu_main <- crayon::make_style(bsu_main_color)

  msg <- paste0(
    cli::rule(left = "ContainR", col = bsu_rule_color, line = 2),
    bsu_main('\nVersion: '), vers, " \n",
    bsu_main('Please Contain Yourself.'), " \n",
    cli::rule(left = "", col = bsu_rule_color, line = 2)
  )

  packageStartupMessage(msg)

}
