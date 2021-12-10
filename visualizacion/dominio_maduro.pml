# Script para observa el dominio maduro
set ray_opaque_background, off
load data/processed/2AFM.pdb
remove solvent
select dominio_maduro , resi 33-361
delete (not dominio_maduro)
zoom dominio_maduro 

label 37/ca, "α-Helix 1"
label 43/ca, "α-Helix 2"
label 60/ca, "α-Helix 3"
label 69/ca, "α-Helix 4"
label 74/ca, "α-Helix 5"
label 97/ca, "α-Helix 6"
label 174/ca, "α-Helix 7"
label 180/ca, "α-Helix 8"
label 225/ca, "α-Helix 9"
label 263/ca, "α-Helix 10"
label 281/ca, "α-Helix 11"
label 312/ca, "α-Helix 12"
label 339/ca, "α-Helix 13"
label 360/ca, "α-Helix 14"

label 111/ca, "β-Sheet 1"
label 126/ca, "β-Sheet 2"
label 198/ca, "β-Sheet 3"
label 140/ca, "β-Sheet 4"
label 247/ca, "β-Sheet 5"
label 320/ca, "β-Sheet 6"
label 113/ca, "β-Sheet 7"

python

import imageio

cmd.ray(1000, 1000)
filename = "dominio_maduro"+".png"
cmd.png(filename)


python end
