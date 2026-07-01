#show: doc => presentation(
$if(title)$
  title: [$title$],
$endif$
$if(subtitle)$
  subtitle: [$subtitle$],
$endif$
$if(author)$
  authors: [$for(author)$$author$$sep$ \ $endfor$],
$endif$
$if(date)$
  date: [$date$],
$endif$
$if(titlegraphic)$
  titlegraphic: "$titlegraphic$",
$endif$
$if(titlecolor)$
  titlecolor: "$titlecolor$",
$endif$
$if(titlefontsize)$
  titlefontsize: $titlefontsize$,
$endif$
$if(fontsize)$
  fontsize: $fontsize$,
$endif$
  toc: $if(toc)$$toc$$else$false$endif$,
$if(toc-title)$
  toc-title: "$toc-title$",
$endif$
$if(bg-path)$
  bg-path: "$bg-path$",
$endif$
  doc,
)
