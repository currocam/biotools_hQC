# Script para comparar posición de cofactores
# predecida Vs experimental
set bg_rgb,[rgb(0.18, 0.19, 0.24)] 
load data/processed/2AFM.pdb
load data/raw/Q16769_AlphaFill.cif
alignto 2AFM,
remove solvent
color brown, inorganic AND 2AFM
color purple, inorganic AND Q16769_AlphaFill
select cofactors, byres inorganic  expand 5
remove (not cofactors)
hide cartoon,
show sticks, 
center cofactors
zoom



python

import imageio

# Number of degrees to rotate each frame
step = 1

# Vector to hold images
images = []

for a in range(0,180,step):
  cmd.rotate("y", float(step)) # Rotate around Y-axis
  cmd.ray(500, 500)
  filename = "file"+str(a)+".png"
  cmd.png(filename)
  images.append(imageio.imread(filename))
  cmd.ray(500, 500)
  filename = "file"+str(a)+".png"
  cmd.png(filename)
  images.append(imageio.imread(filename))

imageio.mimsave('animation.gif', images)

python end
