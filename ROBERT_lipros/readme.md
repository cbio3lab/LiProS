# **ROBERT ML MODELS**

We used ROBERT, an automated ML protocol for chemistry. You can find more information about its purpose and application in this preprint: Dalmau, D.; Requena, J. V. A., *ChemRxiv*, **2023**. https://doi.org/10.26434/chemrxiv-2023-k994h.

Moreover, you can find further info about its documentation here: https://robert.readthedocs.io/en/latest/index.html


If you want to reproduce our data, you can download this directory, and run the following lines in your terminal:

1. First, you have to install ROBERT with:

```conda install -c conda forge robert```

```pip install robert==1.0.5```

2. Run the following line:

 ```python -m robert --y "response" --csv_name "AQME-ROBER_lipros_data.csv" --type "clas" --names "code_name"```

 WARNING: check if python is installed in your computer. Additionally, your `numpy` module must be of any version below 2.0.
