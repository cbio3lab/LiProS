# **NaProRE-CR: Natural Products Repository of Costa Rica:**

Herein, you can find all the information about the experiments performed on NaProRE-CR using **LiProS**.

## 1. Run `ionizable_group_counter.ipynb`

This will select you the ionizable molecules from the original dataset (either `NAPRORE_v4.2.csv` or `NAPRORE_v4.2.xlsx`). Compare them with the molecules from `naprore_sample.xlsx`.

## 2. Run `naprore_sample.xlsx`in LiProS

This .xlsx file already has the acid/basic classification of each ionizable compound along with its estimated pKa (calculated with https://playground.calculators.cxn.io/).


## 3. Run `most_common_fragments.ipynb`

This will give you the most common fragments for each NP pathway as indicated in Fig. 5. It will give you a list of SMILES codes with each fragment.
