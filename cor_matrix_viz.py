# -*- coding: utf-8 -*-
"""
Created on Fri Jul 12 13:54:19 2019

@author: Zach
"""

import numpy as np
import matplotlib.pyplot as plt

#mat = np.loadtxt("cor_matrix.csv", delimiter=",")
cor = np.loadtxt(open("cor_matrix.csv", "rb"), delimiter=",")

plt.imshow(cor);
plt.colorbar()
plt.savefig('cor_matrix.png')