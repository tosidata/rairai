## Closes

<!-- GitHub issue: #NNN -->

---

## What changed

<!-- One bullet per logical unit (file or subsystem). Be specific — mention
     function names, R6 methods, or S3 classes touched. -->

-
-

## What was deliberately NOT changed

<!-- State the scope boundary. What adjacent code did you consider but leave
     alone, and why? This saves the reviewer from wondering "why didn't they
     also fix X?" -->

---

## Risk & review focus


---

## Test evidence

<!-- Paste or summarise the `devtools::test()` output after your changes.
     List any NEW test files or test cases added.
     If no tests were added, explain why (e.g. "pure docs change"). -->

<details>
<summary>devtools::test() summary</summary>

```
# paste output here
```

</details>

New tests added:
- <!-- e.g. tests/testthat/test-xxx.R — test_that("resolve() returns NULL for unknown path") -->

---

## Checklist

- [ ] `air format R` run — no formatting diff
- [ ] `R -e "devtools::document()"` — no roxygen errors
- [ ] `R -e "devtools::check()"` — no roxygen errors
- [ ] No new bare `TODO` comments — converted to Github issues if needed
- [ ] Vignettes and `README.md` updated if design decisions changed
