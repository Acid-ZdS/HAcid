Language.Acid.Interpreter
=========================

Ce module gère l'exécution directe de notre programme. C'est à dire qu'il permet
d'exécuter un programme sans avoir besoin de le compiler.

L'interprétation d'un AST est la réduction de chaque noeud en une unique valeur
finale.

Par exemple, si nous voulions évaluer l'expression `(+ (*3 2) 7)`, nous devrions
procéder ainsi:

```
    +
   / \          (+)
 (*)  7   -->   / \   -->   42
 / \           6   7
3   2
```
