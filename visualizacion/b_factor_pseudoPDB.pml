set ray_opaque_background, off
load write_PDB/data/2AFM_CA_pseudo.pdb
spectrum b, blue_white_red, minimum=10, maximum=50
as surface, 
label b >60, "(%s, %s)" % (resn, resi)
python

import imageio

step = 1

images = []

for a in range(0,180,step):
  cmd.rotate("y", float(step)) 
  cmd.ray(500, 500)
  filename = "file"+str(a)+".png"
  cmd.png(filename)
  images.append(imageio.imread(filename))

imageio.mimsave('pseudoPDB_b_factor.gif', images)

python end

