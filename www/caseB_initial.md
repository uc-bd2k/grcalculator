#### Case B: One measurement per row

Necessary columns: 

  + **cell_line** = name of the cell line used in the experiment
  + **treatment** = name of the treatment used in the experiment (use a dash "-" for untreated measurements)
  + **concentration** = concentration value (not log-transformed) of the treatment - use 0 for untreated measurements - concentrations for other measurements must be positive numbers
  + **cell_count** = measure of cell number
  + **treatment\_duration\_\_hrs** = the time in hours between the time of treatment and the end of the assay, when cell number is measured. Use 0 for measurements at the beginning of the assay and the duration of the assay after treatment for measurements at the end of the assay, e.g. "72" for a typical 3-day (72 hour) assay. (Note: time-course calculations have not been implemented yet, all measurements must be at the time of treatment or at the end of the assay.)

You may use a surrogate of cell number (such as CellTiter-Glo® staining) for the 'cell counts'.

You may include additional columns to be used for grouping the dose-response experiments.
