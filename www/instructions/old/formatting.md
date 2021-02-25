<h3>Instructions</h3>
<div class="ui styled fluid active accordion">
  <div class="title">
    <h4 style = "color:black">
      <i class="dropdown icon"></i>
      Formatting input files
    </h4>
  </div>
  <div class="content">
  <div class="accordion">
      <div class="active title">
        <h4 style = "color:black">
          <i class="dropdown icon"></i>
          Case A (recommended) - multiple cell counts per row 
        </h4>
      </div>
      <div class="active content">
    <h4>File types (.csv or .tsv)</h4>

<p>Input files may be either comma- or tab-separated text files (.csv or .tsv).</p>

<h4>Necessary columns</h4>

<p>The first line in the input file needs to contain column names that exactly match those in our example files.</p>

<h5>Metadata columns: <font color="blue">1) cell_line   2) treatment   3) concentration </font></h5>

<p>The columns "cell_line", "treatment", and "concentration" are always required, but you may also have other metadata columns. For example, <a href = "https://github.com/uc-bd2k/grcalculator/blob/master/resources/Heiser_al_GRmetrics_wSubType_Taxol.tsv">this dataset</a> from Heiser et al.<a href="https://doi.org/10.1073/pnas.1018854108">[3]</a></sup> contains column for clinical subtype (HR+, Her2amp, etc.) and molecular subtypes (Luminal, Basal, etc.). </p>

<h5>Cell count columns: <font color="blue">1) cell_count   2) cell_count__ctrl   3) cell_count__time0</font></h5>

<p>In the simplest case, the GR metrics method requires only live cell counts, or some surrogate of cell count such as cell-titer glo. We require end-of-assay counts for cells at each treated concentration <b>("cell_count")</b> as well as end-of-assay counts of corresponding un-treated cells <b>("cell_count__ctrl")</b> and beginning-of-assay counts <b>("cell_count__time0")</b>. The control and beginning-of-assay counts may be repeated as necessary.
</p>
<p><b>Note that "cell_count__ctrl" and "cell_count__time0" have one underscore between "cell" and "count", and two underscores after "count".</b></p>
<p>You may find an example dataset <a href = "https://raw.githubusercontent.com/uc-bd2k/grcalculator/update/resources/caseA_example.csv">here</a>.</p>
<img src = "images/data_examples/caseA_blur.png" width = "700px">
</div>
</div>
    <div class="accordion">
      <div class="title">
        <h4 style = "color:black">
          <i class="dropdown icon"></i>
          Using (un-treated) cell division time instead of initial cell count
        </h4>
      </div>
      <div class="content"><p>The purpose of measuring cell count at the beginning of the assay is to allow for estimation of growth-rates of treated and un-treated cells. In place of this measurement you may separately measure the growth-rate of each (un-treated) cell line.</p>
<p>In this case, you may swap the <b>"cell_count__time0"</b> column with two columns: <b>"treatment_duration__hrs"</b> – the length of the assay (in hours), and <b>"division_time"</b> - the number of hours it takes cells from each (untreated) cell line to double in population.</p>
<p><b>Note that "cell_count__ctrl" and "cell_count__time0" have one underscore between "cell" and "count", and two underscores after "count".</b></p>
<p>You may find an example dataset <a href = "https://raw.githubusercontent.com/uc-bd2k/grcalculator/update/resources/caseA_div_example.csv">here</a>.</p>
<img src = "images/data_examples/caseA_div_blur.png" width = "700px">
    </div>
  </div>
  <div class="accordion">
      <div class="title">
        <h4 style = "color:black">
          <i class="dropdown icon"></i>
          Alternative format: one cell-count per line
        </h4>
      </div>
      <div class="content"><p>The formatting above is recommended, but if you prefer, you may format your input data with only one column per line for cell counts <b>(Case B)</b>. In this case, only the <b>"cell_count"</b> column should be included <b>(no "cell_count__ctrl" or "cell_count__time0" columns)</b>. Un-treated measurements should be indicated by a dash "-" in the <b>"treatment"</b> column and a "0" in the <b>"concentration"</b> column. A <b>"treatment_duration__hrs"</b> column must be included <b>(two underscores after "duration")</b>, with "0" indicating measurements at the beginning of the assay and a number of hours (e.g. "72" for a 3-day assay) for measurements at the end of the assay.</p>
<p>You may find example datasets for this formatting with the case when one has <a href = "https://raw.githubusercontent.com/uc-bd2k/grcalculator/update/resources/caseB_example.csv">initial cell counts</a> or when one instead has estimates of <a href = "https://raw.githubusercontent.com/uc-bd2k/grcalculator/update/resources/caseB_div_example.csv">cell division time</a>.</p>
<img src = "images/data_examples/caseB_blur.png" width = "700px">
    </div>
  </div>



    
  

    
  

    
  
</div>
</div>
<div class="ui styled fluid active accordion">
  <div class="title">
    <h4 style = "color:black">
      <i class="dropdown icon"></i>
      Step-by-step example
    </h4>
  </div>
  <div class="content">
    <p>testing</p>
  </div>
</div>



<div class="ui styled fluid active accordion">
  <div class="title">
    <h4 style = "color:black">
      <i class="dropdown icon"></i>
      Static/Toxic GR
    </h4>
  </div>
  <div class="content">
    <p>testing2</p>
  </div>
</div>
