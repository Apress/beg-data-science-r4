## ----echo=FALSE, message=FALSE, warning=FALSE, results='hide'-----------------
Sys.setlocale("LC_MESSAGES", 'en_GB.UTF-8')
Sys.setenv(LANG = "en_US.UTF-8")

suppressPackageStartupMessages(library(dplyr, quietly = TRUE))
suppressPackageStartupMessages(library(ggplot2, quietly = TRUE))
suppressPackageStartupMessages(library(magrittr, quietly = TRUE))
suppressPackageStartupMessages(library(tidyverse, quietly = TRUE))
suppressPackageStartupMessages(library(tidyr, quietly = TRUE))
suppressPackageStartupMessages(library(knitr, quietly = TRUE))

knit_hooks$set(plot = function(x, options) {
	base = sub("\\s+$", "", hook_plot_md(x, options))
    paste0(base, "{#fig:", options$label, "}")
})

opts_chunk$set(fig.width=4, fig.height=4, 
			   fig.pos="t", dev="pdf")
theme_set(theme_bw() + 
	theme(axis.text.x = element_text(size = 6),
		  strip.text = element_text(size = 8)))

options(width = 50, 
        str = strOptions(vec.len = 3, 
                         strict.width = "cut"))

set.seed(5)

