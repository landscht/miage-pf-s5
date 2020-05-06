#   Interprétation

Ce dépôt correspond au TP de PF « Interprétation ».


##  Instructions pour rendre votre travail avec gitlab

Pour permettre à votre chargé de TD de suivre votre travail sur ce projet :

-   *forkez* ce dépôt (bouton _Fork_),
-   dans le dépôt *forké*, ajoutez votre chargé de TD aux membres du
    projet avec l’accès _Developer_,
-   éditez ce fichier `README.md` pour indiquer vos noms (membres du
    binôme) et supprimer ce paragraphe d’instructions.


##  Contenu du dépôt

Ce dépôt contient le module `Parser.hs` (dans la version définissant
les instances des classes `Functor`, `Applicative`, `Alternative` et
`Monad`).
Le mini-haddock explique les différentes fonctions qu’il fournit.

N’hésitez pas à vous inspirer des fichiers du dépôt du 1^er dépôt de
TP, en particulier `premiers-contacts.cabal`, etc. si vous voulez
utiliser les outils comme `stack` et/ou `cabal` pour travailler sur
votre machine personnelle : il vous suffira de supprimer la ligne
indiquant la dépendance `gloss` et d’ajouter les autres qui seraient
nécessaires (`cabal build` indique les dépendances manquantes s’il y
en a) pour avoir un fichier `.cabal` exploitable.


##  Intégration continue

Si vous souhaitez utiliser l’intégration continue pour vérifier que
votre source compile correctement, ajoutez un fichier `.gitlab-ci.yml`
contenant par exemple :

```yaml
image: commonci-fil:latest

build:
  script: ghc -Wall -Werror -dynamic -fdefer-typed-holes Interprete.hs
```

N’hésitez pas à effectuer plus de tests que juste la compilation.
