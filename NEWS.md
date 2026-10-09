# PSPclean (development version)

* `dataPurification_SKPSP()` now removes every measurement of a tree that died of a cause in `codesToExclude`; it stopped with an error before (#24).
* `getPSP()` gains `codesToExclude`, damage agent codes to exclude by source (`BC`, `AB`, `SK`, `NFI`) (#24).
* `getPSP(forGMCS = TRUE)` now removes BC trees damaged by mountain pine beetle: it passed the code `"IMB"`, which matches no BC code, instead of `"IBM"` (#24).
