#### GR static vs. toxic curves

Necessary columns:

  + **cell_line** = name of the cell line used in the experiment
  + **treatment** = name of the treatment used in the experiment
  + **time** = the number of hours elapsed between the time of treatment (time0) and the end of the assay
  + **concentration** = concentration value (not log-transformed) of the treatment (all concentrations must be positive numbers)
  + **cell_count** = treated live cell count (end of assay) - measure of cell number in the treated well at the end of the assay
  + **dead_count** = treated dead cell count (end of assay) - measure of cell number in the treated well at the end of the assay
  + **cell\_count\_\_ctrl** - control live cell count (end of assay) - measure of live cell number in the control well (e.g. untreated or DMSO-treated) from the same plate at the end of the assay
    + **dead\_count\_\_ctrl** - control live cell count (end of assay) - measure of dead cell number in the control well (e.g. untreated or DMSO-treated) from the same plate at the end of the assay
  + **cell\_count\_\_time0** - initial cell count (time of treatment) - measure of cell number at the beginning of the assay. (measure of cells in an untreated well grown in parallel until the time of treatment)
  + **dead\_count\_\_time0** - initial dead cell count (time of treatment) - measure of dead cell number at the beginning of the assay. (measure of cells in an untreated well grown in parallel until the time of treatment)

You may use a surrogate of cell number (such as CellTiter-Glo® staining) for the 'cell counts'.

You may include additional columns to be used for grouping the dose-response experiments.