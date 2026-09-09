# Likert scales: design decisions

Working notes for the `with_scales` branch. Records decisions made in
discussion so we don't re-litigate them, and tracks what is still open.

Baseline is tag `v1.0.0` (commit 4928cb3) — the three-generator app with no
scale support. That tag is the fallback if this work is abandoned or split
into a separate app.

## Background: the reference implementation

`~/Documents/R Projects/ResearchMethodsDataSimulation/dataSimulationWithScales/`
has two scale functions, and they work in **opposite directions**:

- `generate_likert_scales()` — IV side. Generates items first, means them,
  then **median-splits** the mean to *create* the grouping variable. The
  scale is the source; group membership is derived from it. Hard-wired to a
  2x2 design (`N * 4` rows).
- `create_dv_scale()` — DV side. Takes an *already generated* continuous DV,
  rescales it into the Likert range, and reverse-engineers integer items whose
  mean approximates it. The continuous value is the source; items are fitted.

Two defects in that code, not to be ported as-is:

1. `create_dv_scale()` rescales using `min()`/`max()` of the **observed
   sample**, forcing the sample min to exactly `likert_min` and the sample max
   to exactly `likert_max`. This is sample-dependent (identical population
   parameters produce different mappings run to run) and severs the
   population-parameter link this app is built to teach.
2. `generate_valid_values()` has a `while` loop that cannot terminate when the
   target sum is unreachable — e.g. every item pegged at `likert_max` while
   `diff` is still positive. It spins forever.

## Decisions

### Column selection
- The CSV download is controlled by **checkboxes** choosing which column
  groups to include: the individual items, the scale mean, and the original
  continuous variable.
- Rationale: the instructor decides per assignment whether scale construction
  is part of the exercise, instead of the app committing to one pedagogy.
  "Compute the scale mean yourself" and "analyze the provided scale mean" are
  both supported by the same build.

### Peeking
- **Ignored by design.** Students can re-tick the boxes and reveal the scale
  mean or the continuous variable. Accepted: these are low-stakes practice
  datasets. No URL-parameter presets, no hidden instructor mode.

### Results boxes
- The results boxes report **what is actually in the file**, as defined by the
  checkboxes. The student's write-up should match data they can reproduce.

### Descriptive statistics
- When the scales box is ticked, the descriptives show **both** rows, always:
  - `This sample` — the continuous variable
  - `This sample as scales` — the scale version
- Both are shown together deliberately, for pedagogical contrast.
- **One of them is highlighted and ticked** to indicate which version everything
  else on the page is built from: the result boxes, the plot, and the CSV. The
  tick alone carries this — an earlier "✓ in your CSV" header made the column
  read as being only about the download.

### Scale definition inputs
- Ticking the scales box reveals **additional inputs defining the scale**
  (number of items, min value, max value), following the pattern already used
  in the 2x2 app.

### Generation method: latent trait with thresholds

Chosen over the 2x2 app's "fit items to a target mean" approach.

    z      = (score - population mean) / population SD
    item*  = z + e,        e ~ N(0, sigma_e^2)
    item   = the response category item* falls into

- The mapping comes from the **population parameters the student typed**, never
  the observed sample range. Same settings always map the same way.
- `sigma_e` is derived from a target reliability (Spearman-Brown inverted), so
  the scale carries genuine measurement error.
- Closed form, no search: the reference app's non-terminating `while` loop has
  no equivalent here.
- Thresholds are evenly spaced over +/- `SCALE_SPAN` (2.5) SDs of the
  standardized item, giving a bell-shaped response distribution with realistic
  floor and ceiling effects.

Measured behaviour (5,000 cases, `scratchpad/test_scales.R`): observed
Cronbach's alpha lands ~.015-.02 **below** the target, because chopping into
categories adds a little noise beyond the modelled item error. Close enough
that the input reads as honest; worth knowing it is not exact.

### Attenuation is kept, not corrected

Measurement error attenuates effects, so the scale versions come out weaker
than the population parameters:

- t-test, population d = 1.08 -> d = 0.92 on 4-item scales at alpha .8
- paired, population rho = .60 -> r = .42-.47 on 6-item scales

This is deliberate and is the point of showing both descriptives columns side
by side. It is **not** corrected for.

## Decisions made while building

### Independent t-test IV: decorative, and exactly consistent
Resolved in favour of keeping the panel's existing controls. The student still
sets both group means and SDs; those still drive the DV. The IV scale is
generated separately, then its rows are ordered so the low half of the scale
means go to Group 1 and the high half to Group 2. A median split of
`Group_Scale_Mean` therefore reproduces the grouping **exactly**, with no need
for the reference app's tie-fixing block. If two scale means tie across the
split, the app's assignment is still a valid split, but a student's own median
split might break the tie the other way.

### Paired panel gets no IV scale
The IV there is the repeated measure itself, so a Likert IV has no meaning.
Only the DV is offered, as one definition applied to both measurements.

### Shared mappings where comparability matters
Both t-test groups, and both paired conditions, are standardized against a
**common** reference mean and SD. Standardizing each to its own centre would
erase the very difference being tested — verified as an explicit test case.

### Table mirrors the CSV
The on-screen data table shows exactly the columns the download will contain,
so students never see a column they cannot get.

### Plots and result boxes follow the CSV
Every result box, and the plots, use whichever version of a variable is in the
file. On a scale metric the population reference lines are dropped, since they
are in the wrong units.

### Column-group labels are per variable
`scaleControlsUI()` takes a `raw_label` argument naming the third column group.
It defaults to "Original continuous score", but the t-test IV passes "Group
membership", since for a grouping IV that column holds the group label rather
than a score.

### Descriptives layout
Implemented as an extra **column** ("This sample as scales") rather than a row
— the rows are statistic names, so a column is the parallel structure. The
column whose numbers drive the result boxes, the plot and the CSV is
highlighted and flagged with a check mark.

### Redrawing scales without redrawing the sample

**Every variable** has its own **Generate Scale Scores** button, inside its own
scale box. It draws a fresh set of Likert items from the sample already on
screen, so students can watch one fixed set of "true" scores turn into
different scale scores each time — the measurement error made visible.
Redrawing one variable's scale leaves every other variable's scale untouched,
and **Generate Data** still draws a whole new sample.

This required splitting the scale settings in two:

- **Snapshotted** (item count, low/high, reliability): re-read only when a
  button is pressed, exactly as the population parameters are. Editing them
  does nothing until asked.
- **Live** (the on/off tick and the CSV column picker): applied immediately.
  These only change what is *displayed*, so they must never trigger a redraw —
  ticking "include the continuous score" must not silently change the scale
  numbers underneath it.

Each variable has its own snapshot (`spec_x`, `spec_y`, `spec_iv`, `spec_dv`)
and its own `scaled_*` reactive, so neither turning a scale on nor redrawing it
disturbs any other variable. Ticking a box for the first time generates
immediately using the current snapshot, rather than leaving the student staring
at an empty box until they find the button.

Verified in `scratchpad/test_redraw.R` and `test_tick.R`: the sample is
byte-identical across scale redraws, the column picker changes no numbers, item
settings take effect only on the button, and the t-test IV median split still
reproduces the groups after every redraw.

### Typical response: floor and ceiling effects

Thresholds are symmetric about zero, so an unshifted scale always averages the
middle of its response range. No setting of `SCALE_SPAN` can change that — it
widens or narrows symmetrically — so the app could not produce a floor or
ceiling effect at all. Shifting the latent distribution against the thresholds
is what does that.

Each variable now has a **Typical response** slider: numbers on the axis, zone
labels reading *Floor effects — Well targeted — Ceiling effects*, defaulting to
the midpoint (which reproduces the previous centred behaviour exactly).

The required shift is a one-dimensional root find. For items ~ N(mu, 1),
E[response] = k_min + sum(pnorm(mu - cuts)), which is monotone in mu, so
`scale_shift_for()` solves it with `uniroot`, capping mu at +/-10 at the
endpoints where the true shift is infinite.

Measured (20,000 cases, 4 items, alpha .8, 1-7 scale) — the realized mean
tracks the request exactly, and the endpoints are deliberately degenerate:

| target | realized mean | % at 7 | % at 1 |
|--------|---------------|--------|--------|
| 1      | 1.00          | 0.0    | 99.9   |
| 2      | 2.00          | 0.0    | 43.1   |
| 4      | 4.00          | 3.7    | 3.7    |
| 6      | 6.00          | 42.6   | 0.0    |
| 7      | 7.00          | 99.9   | 0.0    |

The slider's bounds follow the Low/High inputs via `observe_scale_range()`,
which clamps the current position into the new range rather than snapping it
back to the midpoint.

`SCALE_SPAN` stays fixed at 2.5. Measured across 1.5-3.5, the correlation
between the scale mean and the true score varies only between .879 and .887,
peaking at 2.5 — it is a cosmetic knob controlling the shape of the response
distribution, not an accuracy one, so it did not earn a control.

### Degenerate data is reachable on purpose, and explained

The endpoints of the slider, and a population SD of 0, really do leave a
variable with no variance. That is deliberate: watching a measure fail is the
lesson. But the statistics fail in three different ways, so the condition is
detected **up front** rather than caught afterwards:

- `t.test(var.equal = TRUE)` throws *"data are essentially constant"*
- `cor()` and `cor.test()` return `NA` with only a warning — which would print
  as though it were a finding
- `summary(lm())$fstatistic` returns `NULL`, crashing the line that reads it

**This was already broken in v1.0.0**, independent of scales: the SD inputs
allow `min = 0`, and setting one produced raw R errors (*subscript out of
bounds*, *'a' and 'b' must be finite*) on the correlation and paired pages.

Messages go through `validate()`/`need()`, styled as explanation boxes rather
than Shiny's default grey italic. Three tiers:

1. **No variability at all** — `msg_no_variance()`.
2. **No variability within groups** — `msg_no_within_variance()`. Separately
   reachable when groups differ but nobody inside a group does; t is infinite
   rather than undefined.
3. **Computable but barely** — `caution_note()` appends a note to the primary
   result box when a scale shows two or fewer distinct values, or more than 60%
   of responses sit at an endpoint. This is the ceiling-effect sweet spot.

Surfaces that can still render keep rendering, because that is where the lesson
lands: the descriptives table shows the means with `SD = 0.00` and an em dash
where a statistic is undefined (`fmt()` and `fmt_r()` now map non-finite values
to em dashes), and the plots still draw the flat row of identical points with
an overlaid note.

The ANOVA and point-biserial boxes refuse on the *same* condition as the
t-test, since both claim to mirror it — otherwise a student would see the
t-test decline while the ANOVA below it reported F = Inf.

## Tests

`tests/` holds a regression suite. Run it all:

    Rscript tests/run_all.R

It prints a per-file summary and exits non-zero if anything fails, so it can
gate a deploy. Individual files run the same way (`Rscript tests/test_guard.R`),
and both forms work from the project root or from inside `tests/`.

| File | Covers |
|------|--------|
| `test_scales.R` | engine: item range, alpha targeting, sample-independent mapping, degenerate inputs |
| `test_target.R` | Typical response maths, floor/ceiling reachability, guard helpers |
| `test_server.R` | column selection and CSV assembly in all three modules |
| `test_outputs.R` | every output renders under 12 scale configurations |
| `test_redraw.R` | redrawing scales leaves the sample byte-identical |
| `test_tick.R` | ticking generates immediately; unticking restores |
| `test_perbutton.R` | per-variable buttons are independent; IV column label |
| `test_slider_wiring.R` | every slider reaches its generator |
| `test_guard.R` | degenerate settings explain themselves instead of erroring |
| `test_plot_axes.R` | scale variables are plotted on their full response range |
| `test_code_block.R` | the R code box runs and reproduces the reported analysis |

`helper.R` finds `app.R` by walking up from the working directory, loads it
without calling `shinyApp()`, and provides `ok()` plus the tally the runner
reads back.

Two notes for anyone adding tests:

- **Pin the seed.** `test_guard.R` and `test_outputs.R` originally had none, and
  a result flipped purely because running from a different directory changed the
  RNG state.
- **Do not use `on.exit()` at the top level of a test file.** Sourced by the
  runner it fires as soon as that one expression finishes, so cleanup runs
  before any test does; run standalone it never fires at all. `test_plot_axes.R`
  shadows `plot()` and tore the shadow down instantly this way, passing
  standalone and failing 12 assertions under the runner. Clean up explicitly at
  the bottom of the file instead.
- **Endpoint targets are likely degenerate, not certainly so.** With four items
  at target 7 about 99.6% of people max out, so a sample of 30 keeps some
  variance roughly a tenth of the time. Assert on population SD = 0 when a test
  needs guaranteed constancy.

`test_slider_wiring.R` exists because of a bug that shipped past the other
tests: `make_scale_items()` gained a `target` argument that only the correlation
module passed, so three of the four sliders silently did nothing. The engine was
tested, the wiring was not. That file also counts the generator call sites and
asserts every one passes a target, to catch the same class of mistake if a
fourth panel is added.

### Plot axes follow the response range

Whenever a variable is displayed as a scale, its axis spans the scale's full
`kmin`-`kmax` range instead of shrinking to fit the data. A ceiling effect then
reads as a pile-up against the top of the frame rather than being hidden by
axes that quietly rescale, and redrawing the scale shows the *points* moving
inside a fixed frame.

Each axis is decided by its own variable, so a scaled Y against a continuous X
keeps the model-based frame on the x axis and the response range on the y.
`scale_range()` also repairs a crossed Low/High pair.

Low/High are generation settings, so the axis follows the scale that currently
*exists*: changing the response range does not move the frame until the scale
is regenerated, since until then the displayed data still lives on the old
range.

`renderPlot` draws on its own graphics device, so `par("usr")` afterwards reads
nothing. `test_plot_axes.R` shadows `plot()` to record the limits the app
actually requests.

### The R code box has to actually run

It claims to reproduce what the app just did, so it is evaluated in the tests
rather than eyeballed. It had been wrong four ways at once:

- **t-test: `object 'Score' not found`.** The block created `group1` and
  `group2` and never combined them, so the snippet referenced a variable that
  did not exist. It now emits `Score <- c(group1, group2)` and a matching
  `Group` factor.
- **The Typical response shift was missing.** The snippet had no `mu`, so an
  off-centre scale came out centred — the code silently did something different
  from the app. `mu` is now computed and printed.
- **Paired emitted only one measurement**, and reused the name `z`, clobbering
  the latent that builds the correlated scores. Names are now prefixed per
  variable (`Score1_z`, `Score2_sigma`, ...) and both measurements are emitted
  through the same mapping.
- **The analysis lines tested the wrong variable.** The block ended with
  `t.test(group2, group1)` even when the results box had reported a test of the
  scale means. The final lines now follow whichever version is in the CSV.

Also: item draws use `length(<var>_z)` rather than `n`, because a t-test's
`Score` is `2n` rows while its `n` is per group — `rnorm(n, ...)` would have
recycled silently.

`base::findInterval` is base R, so the block needs no library call.

## Open questions

1. **Packaging.** Currently built as checkbox-in-place on the three existing
   panels. A fourth "scale data" page or a separate app remain possible; the
   generation functions are self-contained enough to move.

2. **`APP_VERSION` is now `1.1.0-dev`** so the footer does not claim to be the
   tagged v1.0.0 build. Settle the real number before deploying.
