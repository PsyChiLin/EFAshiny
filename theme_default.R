theme_default <- function(base_size = 10, base_family = ""){
        theme_bw(base_size = base_size, base_family = base_family) %+replace%
                theme( strip.background = element_blank()
                )
}