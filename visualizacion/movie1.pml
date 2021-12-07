load data/processed/2AFM.pdb
load data/raw/Q16769_AlphaFill.cif
align 2AFM, Q16769_AlphaFill
# Load PyMol model, file extension will be autodetected
load testmodel

# Start of python script
python

import imageio

# Number of degrees to rotate each frame
step = 10

# Vector to hold images
images = []

for a in range(0,360,step):
  cmd.rotate("y", float(step)) # Rotate around Y-axis
  cmd.ray(256,256) # Raytrace 256x256 image
  filename = "file"+str(a)+".png"
  cmd.png(filename)
  images.append(imageio.imread(filename))

# Create gif animation from images
imageio.mimsave('animation.gif', images)

# End of python script
python end
