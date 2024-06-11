
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.colors import ListedColormap
import viridis

# Generate data similar to the volcano data in R
volcano = np.loadtxt('https://raw.githubusercontent.com/holtzy/The-Python-Graph-Gallery/master/static/data/volcano.csv', delimiter=",")

# Viridis color palette
plt.figure(figsize=(7, 5))
plt.imshow(volcano, cmap='viridis')
plt.title('Viridis colors: Default size')
plt.colorbar()
plt.show()

# Bigger image with Viridis colors
plt.figure(figsize=(8, 6))
plt.imshow(volcano, cmap='viridis')
plt.title('Viridis colors: Bigger size')
plt.colorbar()
plt.show()

# Smaller image with Viridis colors
plt.figure(figsize=(6, 4))
plt.imshow(volcano, cmap='viridis')
plt.title('Viridis colors: Smaller size')
plt.colorbar()
plt.show()

# Magma color palette
plt.figure(figsize=(7, 5))
plt.imshow(volcano, cmap='magma')
plt.title('Magma colors')
plt.colorbar()
plt.show()
