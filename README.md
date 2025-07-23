# maudr

**maudr** is an R package designed to generate personalised enzyme kinetics datasets and model answers for undergraduate laboratory practicals.
It enables scalable, individualised teaching and streamlines the otherwise-tedious marking and feedback process.

## ⬇️ Installation
> _Requires an internet connection and R ≥ 4.1._

Copy-paste this code block into an R script to then install **maudr**

``` r
install.packages("remotes")

remotes::install_github("lewis-ward/maudr")
```
## 📈 Usage

The **maudr** package is designed to be simple for educators and students to use.

> The main function is: `runMaudr()`

`runMaudr()` handles:
- Randomised assignment of kinetic parameters
- Generation of `.xlsx` files for students
- Generation of model answer PDFs (Absorbance vs Time, Lineweaver–Burk, and Michaelis–Menten plots)

### 📁 Directory Setup

Before running `runMaudr()`, ensure there is an RStudio project open and a working directory structured like this:
<pre> project_folder/ 
  ├── .Rproj 
  ├── data/ 
  └── output/ </pre>

`data/` should contain three `.xlsx` files formatted like this:

#### **`student_names.xlsx`**  
  Must include one row per student, with the following column headers:
  - `student_no` – unique ID (e.g. "u123456")
  - `first_name` – student’s first name
  - `surname` – student’s surname

#### **`enzyme_properties.xlsx`**  
  The following column headers:
  - `rxn_substrate`
  - `Kcat`
  - `Km`
  - `Vmax`
  - `enzyme_conc`
  - `inhibition_actual`

#### **`variable_storage.xlsx`**  
  The following column headers:
  - `time_sec`
  - `substrate_conc_mM`
  - `extinction_coeff`
  - `cuvette_volume_L`
  - `enzyme_volume_ml`

> All files must be `.xlsx` and placed in the `data/` folder. Column names must match exactly.

In the even of `data/` being left empty, the package will populate it with its own pre-generated defaults.

`output/` is where individual student `.xlsx` datasets and model-answer PDFs will be saved.

---

### ▶️ Example

```r
library(maudr)

# Run the full pipeline
runMaudr()
```
### ⚙️ Customisation

Control the output format using arguments in `runMaudr()`:

```r
# Generate only a single combined PDF (no individual files)
runMaudr(individual = FALSE, combined = TRUE)

# Generate only individual PDFs (default behaviour)
runMaudr(individual = TRUE, combined = FALSE)

# Generate both (default)
runMaudr(individual = TRUE, combined = TRUE) 
```
