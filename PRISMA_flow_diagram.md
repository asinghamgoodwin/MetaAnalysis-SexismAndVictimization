PRISMA article inclusion diagram
================

``` r
#install.packages("PRISMAstatement")
library(PRISMAstatement)
```

Here is an example from
<https://cran.r-project.org/web/packages/PRISMAstatement/vignettes/PRISMA.html>
:

``` r
prisma(found = 750,
       found_other = 123,
       no_dupes = 776, 
       screened = 776, 
       screen_exclusions = 13, 
       full_text = 763,
       full_text_exclusions = 17, 
       qualitative = 746, 
       quantitative = 319,
       width = 800, height = 800)
```

<!--html_preserve-->

<div id="htmlwidget-0b8491b245cef0c152c8" class="grViz html-widget" style="width:672px;height:480px;">

</div>

<script type="application/json" data-for="htmlwidget-0b8491b245cef0c152c8">{"x":{"diagram":"digraph prisma {\n    node [shape=\"box\", fontsize = 10];\n    graph [splines=ortho, nodesep=1, dpi = 72]\n    a -> nodups;\n    b -> nodups;\n    a [label=\"Records identified through\ndatabase searching\n(n = 750)\"];\n    b [label=\"Additional records identified\nthrough other sources\n(n = 123)\"]\n    nodups -> incex;\n    nodups [label=\"Records after duplicates removed\n(n = 776)\"];\n    incex -> {ex; ft}\n    incex [label=\"Records screened\n(n = 776)\"];\n    ex [label=\"Records excluded\n(n = 13)\"];\n    {rank=same; incex ex}\n    ft -> {qual; ftex};\n    ft [label=\"Full-text articles assessed\nfor eligibility\n(n = 763)\"];\n    {rank=same; ft ftex}\n    ftex [label=\"Full-text articles excluded,\nwith reasons\n(n = 17)\"];\n    qual -> quant\n    qual [label=\"Studies included in qualitative synthesis\n(n = 746)\"];\n    quant [label=\"Studies included in\nquantitative synthesis\n(meta-analysis)\n(n = 319)\"];\n  }","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>

<!--/html_preserve-->
