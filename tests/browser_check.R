# Browser check -- NOT part of run_all.R (the runner only picks up test_*.R).
#
# Some failures are invisible headlessly. Rendering the table with
# server = FALSE once produced "Invalid JSON response" in every browser while
# all 257 headless assertions passed, because the failure was entirely
# client-side. This drives a real Chrome via chromote and checks:
#
#   * that DataTables raises no alert;
#   * that the sample data rows really are tinted by group, in exactly two
#     colours with exactly one change down the table -- i.e. one clean
#     boundary at the median split.
#
# Run it deliberately:  NOT_CRAN=true Rscript tests/browser_check.R
# Takes about a minute and needs chromote plus a Chrome install.

options(chromote.timeout = 180)
library(chromote)
PORT <- 7414
srv <- callr::r_bg(function(p) shiny::runApp(".", port=p, launch.browser=FALSE,
                                             host="127.0.0.1"),
                   args=list(p=PORT), supervise=TRUE)
on.exit(try(srv$kill(), silent=TRUE), add=TRUE)
Sys.sleep(12)
b <- ChromoteSession$new(); b$default_timeout <- 180
b$Page$navigate(sprintf("http://127.0.0.1:%d", PORT)); Sys.sleep(8)
ev <- function(code) {
  r <- b$Runtime$evaluate(code, returnByValue=TRUE)
  if (!is.null(r$exceptionDetails)) return(paste("JSERR:", r$exceptionDetails$text))
  r$result$value
}
ev("window.__al=[]; window.alert=function(m){window.__al.push(String(m));};
    document.getElementById('to_ttest').click(); 'ok'"); Sys.sleep(3)
ev("var e=document.getElementById('ttest-iv_use'); e.checked=true;
    e.dispatchEvent(new Event('change',{bubbles:true}));
    var c=document.querySelectorAll('input[name=\"ttest-iv_cols\"]');
    for(var i=0;i<c.length;i++){if(c[i].value=='raw'){c[i].checked=true;
      c[i].dispatchEvent(new Event('change',{bubbles:true}));}}
    var n=document.getElementById('ttest-n'); n.value=40;
    n.dispatchEvent(new Event('change',{bubbles:true})); 'ok'"); Sys.sleep(5)
for (i in 1:10) {
  ev("document.getElementById('ttest-generate').click(); 'ok'"); Sys.sleep(4)
  if (isTRUE(ev("document.getElementById('ttest-iv_force')!==null"))) break
}
cat("uneven split after", i, "generate(s)\n")
cat("alerts:", ev("JSON.stringify(window.__al)"), "\n")
cat("distinct row background colours:",
    ev("(function(){var s={};document.querySelectorAll('#ttest-data_table tbody tr')
        .forEach(function(r){s[getComputedStyle(r).backgroundColor]=1;});
        return JSON.stringify(Object.keys(s));})()"), "\n")
cat("colour changes down the table (should be 1 boundary):",
    ev("(function(){var p=null,n=0;document.querySelectorAll('#ttest-data_table tbody tr')
        .forEach(function(r){var c=getComputedStyle(r).backgroundColor;
        if(p!==null&&c!==p)n++;p=c;});return n;})()"), "\n")
b$close()
