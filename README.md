# 🦌 D.E.E.R. Shiny - Differential Expression and Enrichment in R

**Auteur :** Philippe Stocker
**Affiliation :** Université de Rouen Normandie – Master 2 Bioinformatique
**Contact :** [philippe.stocker@univ-rouen.fr](mailto:philippe.stocker@univ-rouen.fr)

---

## 🎯 Objectif du projet

**D.E.E.R.** (Differential Expression and Enrichment in R) est une application **R Shiny** développée dans le cadre d’un projet de Master 2.
Son objectif est de fournir un outil **ergonomique, interactif et modulaire** pour :

* Explorer les résultats d’**analyse d’expression différentielle** (RNA-seq) ;
* Visualiser les **log2 Fold Change** sous forme de **Volcano Plot** ;
* Intégrer à terme des modules d’**enrichissement fonctionnel (GO, KEGG)** ;
* Offrir une interface claire et harmonisée pour la recherche biomédicale.

---

## 📂 Structure du projet

```bash
./
├── ui.R                    # Interface utilisateur (UI)
├── server.R                # Logique serveur (interactions et calculs)
├── www/
│   ├── DEER_logo.svg       # Logo officiel du projet
│   └── deer_theme.css      # Thème custom (interface Shiny)
```

---

## 🚀 Lancer l’application

Depuis RStudio :

- Exécuter le script et clicker sur `run app`

Une fois lancée, l’application est accessible à l’adresse :
👉 **[http://127.0.0.1:xxxx](http://127.0.0.1:xxxx)**

---

## 🔮 Évolutions prévues

* [ ] Ajout d'un module **Volcano Plot interactif** (log2FC uniquement, puis log2FC + p-value)
* [ ] Intégration d’un module **d’enrichissement fonctionnel**
* [ ] Connexion à des API (Ensembl, UniProt, NCBI) pour enrichir les gènes
* [ ] Possibilité d’exporter et télécharger les figures générés et des rapports PDF.
* [ ] Image Docker de l'application à des fin de déploiement

---

## 🧠 Mentions et références

* **R Shiny** pour la construction de l’interface interactive.
* **shinydashboard** pour la mise en page modulaire.
* **fresh** pour la création du thème CSS.
* **DT** pour l’affichage des tables interactives.
* **shinycssloaders** pour la gestion du chargement visuel.

---

## 🧩 Licence

Ce projet est diffusé sous licence **GPL-3**.
Les utilisateurs sont libres de le modifier, redistribuer et utiliser à des fins de recherche et d’enseignement.

---

> 🦌 *D.E.E.R. Shiny — Exploring differential expression with elegance and precision.*

