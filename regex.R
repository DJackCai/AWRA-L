### list files with pattern "181grid1_S0_tunedIFs.txt" coming from supercomputer output 
### and then extract the number after the "grid"

catname = "181"
## ".*": anything 

list_files = list.files(pattern = paste0("^",catname,"grid.*S0_tunedIFs.txt"))

  ### split the strings: after grid, all numbers (\\d+)
  
  exist_grids = as.numeric(sub("^.*?grid(\\d+).*", "\\1",list_IF ))
