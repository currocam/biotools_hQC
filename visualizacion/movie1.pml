# Script para comparar estructura predecida secretora y 
# y estructura experimental retenida
set bg_rgb,[rgb(0.18, 0.19, 0.24)] 
load data/processed/2AFM.pdb
load data/raw/AF-Q16769-F1-model_v1.pdb
alignto 2AFM,
zoom

python

import imageio

# Number of degrees to rotate each frame
step = 5

# Vector to hold images
images = []

for a in range(0,360,step):
  cmd.rotate("y", float(step)) # Rotate around Y-axis
  cmd.ray(500, 500)
  filename = "file"+str(a)+".png"
  cmd.png(filename)
  images.append(imageio.imread(filename))

imageio.mimsave('animation.gif', images)

python end
