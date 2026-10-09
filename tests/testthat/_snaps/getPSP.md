# getPSP() rejects codesToExclude it cannot use, before downloading

    Code
      getPSP("ON", tempdir(), codesToExclude = list(ON = "IB"))
    Condition
      Error in `getPSP()`:
      ! codesToExclude must be a list named by source, of BC, AB, SK and NFI (not ON)

---

    Code
      getPSP("BC", tempdir(), codesToExclude = c(BC = "IBM"))
    Condition
      Error in `getPSP()`:
      ! codesToExclude must be a list named by source, of BC, AB, SK and NFI

---

    Code
      getPSP("BC", tempdir(), codesToExclude = list("IBM"))
    Condition
      Error in `getPSP()`:
      ! codesToExclude must be a list named by source, of BC, AB, SK and NFI

