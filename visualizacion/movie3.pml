# Script para observar sitio de unión
set ray_opaque_background, off
load data/processed/2AFM.pdb
remove solvent
select sitio_union, byres res 392 expand 5
remove (not sitio_union)
hide cartoon
show sticks
zoom sitio_union
label n. CA and sitio_union, "(%s, %s)" % (resn, resi)

python

import imageio

cmd.ray(500, 500)
filename = "sitio_union"+".png"
cmd.png(filename)


python end
