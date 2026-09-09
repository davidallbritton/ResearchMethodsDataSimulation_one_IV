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
- **One of them is highlighted** to indicate which corresponds to the sample
  data that will actually be downloaded as CSV.

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

### Descriptives layout
Implemented as an extra **column** ("This sample as scales") rather than a row
— the rows are statistic names, so a column is the parallel structure. The
column whose numbers match the CSV is highlighted and flagged with a check
mark.

### Redrawing scales without redrawing the sample

Each panel's scale box has its own **Generate Scale Scores** button. It draws a
fresh set of Likert items from the sample already on screen, so students can
watch one fixed set of "true" scores turn into different scale scores each
time — the measurement error made visible. **Generate Data** still draws a
whole new sample.

This required splitting the scale settings in two:

- **Snapshotted** (item count, low/high, reliability): re-read only when a
  button is pressed, exactly as the population parameters are. Editing them
  does nothing until asked.
- **Live** (the on/off tick and the CSV column picker): applied immediately.
  These only change what is *displayed*, so they must never trigger a redraw —
  ticking "include the continuous score" must not silently change the scale
  numbers underneath it.

Each variable also has its own `scaled_*` reactive, so turning a scale on for
one variable does not redraw another's. Ticking a box for the first time
generates immediately using the current snapshot, rather than leaving the
student staring at an empty box until they find the button.

Verified in `scratchpad/test_redraw.R` and `test_tick.R`: the sample is
byte-identical across scale redraws, the column picker changes no numbers, item
settings take effect only on the button, and the t-test IV median split still
reproduces the groups after every redraw.

## Open questions

1. **Packaging.** Currently built as checkbox-in-place on the three existing
   panels. A fourth "scale data" page or a separate app remain possible; the
   generation functions are self-contained enough to move.

2. **"Original continuous score" label on the t-test IV.** For the IV that
   column is the group label, not a continuous score, so the checkbox label is
   slightly off there.

3. **`SCALE_SPAN` is fixed at 2.5** and not exposed. It controls how much
   responses pile into the middle categories versus the extremes.

4. **`APP_VERSION` is now `1.1.0-dev`** so the footer does not claim to be the
   tagged v1.0.0 build. Settle the real number before deploying.
