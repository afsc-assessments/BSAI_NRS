# create_template for BSAI NRS

library(asar)
create_template(
  format = "pdf", # optional "html"
  type = "safe", # add'l options: "safe", "nemt", "pfmc"
  office = "AFSC",
  region = "Bering Sea and Aleutian Islands",
  species = "northern rock sole",
  spp_latin = " Lepidopsetta polyxystra ",
  authors = c("Carey McGilliard" = "AFSC"),
  file_dir =  "C:/GitProjects/bsai_nrs/doc" # folder where the "report" folder will be saved
)
