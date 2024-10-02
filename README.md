# **LiProS**
A FAIR (*Findable, Accessible, Interoperable, and Reusable*) workflow to predict the most appropriate lipophilicity formalism for small molecules.
---

<img src="https://i.imgur.com/FjTNXlA.png" width="250">

---

$\log{D}$ is an evergrowing physicochemical property applied in drug design, environmental chemistry, medicinal chemistry, and food chemistry. It is often not possible to measure the $\log{D}$ of a molecule. Therefore, the use of thermodynamically derived equations has been successful in facilitating straightforward calculations. In this case, 2 main equations have been used.

### **Equation 1:**

$$\log{D_{\text{pH}}} = \log{P_{\text{N}}}-\log{\left(1+10^{\delta}\right)}$$

### **Equation 2:**

$$\log{D_{\text{pH}}} = \log{\left(P_{\text{N}}+P_{\text{I}}^{\text{app}}\cdot10^{\delta}\right)}-\log{\left(1+10^{\delta}\right)}$$

**Equation 1** is often used due to its simplicity. On the other hand, **Equation 2** often gives more accurate computations, but it requires more experimental data (the applied ionic partition coefficient of a molecule ($P_{\text{I}}^{\text{app}}$)).


*Which equation do we use and in which cases?* This script will help you with this decision with the aim of the most accurate and efficient lipophilicity calculations.


Herein, you can see the motivation for these scripts and our preliminary models (*ChemPhysChem*, **2023**, *24(24)*, e202300548): https://doi.org/10.1002/cphc.202300548

Click here to access our Google Colab Script: https://colab.research.google.com/drive/1w9Vkqm4kIBQPn5AeSYnIwAs6vCTd8G7u?usp=sharing

---

This repository contains every script, dataset, and supplementary information of our manuscript *"LiProS: FAIR simulation workflow to Predict Accurate Lipophilicity Profiles for Small Molecules."*
