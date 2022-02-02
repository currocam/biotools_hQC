## Visualización de las estructuras

A continuación, realizamos una serie de animaciones donde se busca poder hacer uso de software específico para comparar la estructura elegida, 2AFM, con una segunda estructura. Destacar que el archivo 2AFM.pdb utilizado es un archivo que ha sido procesado para tener una única copia de la proteína. Los archivos de estructura *raw* y procesados pueden encontrar en en el directorio [visualización/data](https://github.com/currocam/biotools_hQC/tree/master/visualizacion/data). Además, se muestran, a modo de ejemplo, algunos de los scripts utilizados, para asegurar la reproducibilidad, para obtener las imágenes. Todos los scrips utilizados se pueden encontrar en el [directorio visualización del repositorio](https://github.com/currocam/biotools_hQC/tree/master/visualizacion).  

### AlphaFill

Además de la estructura 2AFM se ha elegido una segunda estructura con el objetivo de poder compararlas en el software de visualización elegido. Esa segunda estructura corresponde a la estructura predecida por AlphaFold de Q16769, la cual corresponde a QPCT, es decir, a la isoforma secretora de hQC. Se ha escogido por dos razones, en primer lugar para comparar la estructura elegida, que corresponde a la isoforma retenida, de la secretora y, en segunda lugar, comparar una estructura cuyo origen es experimental con una obtenida mediante el uso de IA. 

No obstante, no podemos hacer uso de ella directamente debido a que los modelos estructurales de la base de datos AlphaFold no tienen en cuenta todas las entidades químicas distintas de los residuos de aminoácidos naturales y no poseen, por tanto cofactores. Esto supone un inconveniente en nuestro caso porque, como vimos en el apartado anterior, la proteína hQC posee un cofactor de Zn2+ que es imprescindible para que tenga lugar la catálisis y es, por tanto, de interés. Esto es así porque estos algoritmos no son capaces de resolver el problema del plegamiento de las proteínas mediante la  comprensión de los principios físicos subyacentes, sino que han descubierto intrincados patrones en base a las estructuras tridimensionales determinadas estructuralmente. 

Este inconveniente podemos resolverlo haciendo uso del algoritmo AlphaFill, el cual ha sido recientemente publicado en forma de preprint. Este algortimo enriquece los modelos de la base de datos AlphaFold "transplantando" moléculas pequeñas e iones comunes que se hayan observado en complejos con proteínas homólogas muy similares en modelos determinados experimentalmente del banco de datos PDB-REDO7 [^1]. El funcionamiento del algoritmo es, a grandes rasgos, como se muestra a continuación: 

1. BLAST con la secuencia de AlphaFold con las secuencias alojadas en LAHMA webserver. 
2. Selección homólogos muy cercanos.
3. Alineamiento de los esqueletos peptídicos.
4. Integración de los compuestos en los modelos de AlphaFOld si no estaban previamente. 
5. Generación nuevo modelo.

## Visualización de proteínas

A continuación, se muestra una animación en la que se pueden observar las estructuras 2AFM y Q16769 de AlphaFold tras ser alineadas. Se puede observar que, exceptuando la cola de la proteína y la ausencia de cofactores, ambas son extremadamente similares.  

|![](images/movie1.gif)|
|:--:|
|Animación de las estructuras 2AFM y Q16769. Elaboración propia usando pymol.|

Para realizar esta animación se ha hecho uso de un pequeño script que combina comandos en pymol con Python. 

```python
set ray_opaque_background, off
load data/processed/2AFM.pdb
load data/raw/AF-Q16769-F1-model_v1.pdb
alignto 2AFM,

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

imageio.mimsave('movie1.gif', images)

python end
```

A continuación, se muestra la estructura experimental y la obtenida de AlphaFill alineadas. Se ha coloreado las sustancias inorgánicas de la estructura experimental en marrón y las de la estructura predecida en morado. Se puede observar que, en este, el algoritmo AlphaFill no funciona adecuadamente. En primer lugar porque, aunque no se pueda apreciar porque están superpuestos, aunque es capaz de determinar a la perfección que es necesario algún tipo de molécula cargada en el sitio catalítico, se equivoca colocando otro ion distinto  al Zn2+. En segundo lugar, porque no es capaz de predecir la presencia de un grupo sulfato y, en tercer lugar, porque predice la presencia de multitud de iones Zn fuera del sitio catalítico. 

El código que se ha utilizado para conseguir dicha animación es el siguiente: 

```python
set ray_opaque_background, off
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

step = 1
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
```


|![](images/movie2.gif)|
|:--:|
|Animación de las estructuras 2AFM y Q16769 mostrando cofactores. Elaboración propia.|

## Referencias
[^1]: Hekkelman, Maarten L., Ida de Vries, Robbie P. Joosten, y Anastassis Perrakis. «AlphaFill: Enriching the AlphaFold Models with Ligands and Co-Factors». Preprint. Bioinformatics, 27 de noviembre de 2021. https://doi.org/10.1101/2021.11.26.470110.
