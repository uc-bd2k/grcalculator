#### Case A: Multiple measurements per row

Necessary columns:

  + **cell_line** = name of the cell line used in the experiment
  + **treatment** = name of the treatment used in the experiment
  + **concentration** = concentration value (not log-transformed) of the treatment (all concentrations must be positive numbers)
  + **cell\_count** = treated cell count (end of assay) - measure of cell number in the treated well at the end of the assay
  + **cell\_count\_\_ctrl** - control cell count (end of assay) - measure of cell number in the control well (e.g. untreated or DMSO-treated) from the same plate at the end of the assay
  + **treatment\_duration\_\_hrs** = the time in hours between the time of treatment and the end of the assay, when cell number is measured. Use 0 for measurements at the beginning of the assay and the duration of the assay after treatment for measurements at the end of the assay, e.g. "72" for a typical 3-day (72 hour) assay. (Note: time-course calculations have not been implemented yet, all measurements must be at the time of treatment or at the end of the assay.)
  + **division\_time** - the time in hours that it takes cells from each (untreated) cell line to double in population number.

You may use a surrogate of cell number (such as CellTiter-Glo® staining) for the 'cell counts'.

You may include additional columns to be used for grouping the dose-response experiments.

When your file is uploaded, another popup will open in which you can type the division times for the cell lines used as well as the assay duration.
