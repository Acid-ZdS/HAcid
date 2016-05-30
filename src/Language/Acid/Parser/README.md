Language.Acid.Parser
====================

Ce module s'occupe de la première étape de notre interpréteur/compilateur: le
*parsing*.

Le parsing est la transformation de notre code Acid en ce qu'on appelle un *AST*
. Un AST (de l'anglais *Abstract Syntax Tree*, arbre de syntaxe abstraite), est
une représentation du code compréhensible pour notre programme.

Notre AST est représentés par les types `Program`, `Statement`, `Expr` et
`Literal`, dans le module [`Language.Acid.Parser.AST`](AST.hs).

On dit qu'un AST est un arbre car on le représente souvent de cette manière
(ici pour le code Acid `(+ (* 3 2) 7)`):

```
    +
   / \
  *   7
 / \
3   2
```

Dans notre exemple, notre AST aura la valeur suivante:

```haskell
Call
	(Variable "+")
	[ Call
		(Variable "*")
		[ Literal (IntL 3)
		, Literal (IntL 2) ]
	, Literal (IntL 7) ]
```

Comme notre langage est fonctionnel, nous considérons les fonctions comme
des valeurs ordinaires, dites [de première classe](https://fr.wikipedia.org/wiki/Objet_de_premi%C3%A8re_classe).

Cette propriété nous permet d'appeler une fonction en spécifiant seulement les
premiers arguments:

*(Voir [partial.acid](../../../../../examples/partial.acid))*

```lisp
(define double (* 2))
```

On peut alors appeler notre fonction `double` de cette manière: `(double 5)`
(qui nous donne donc `10`). C'est ce qu'on appelle l'application partielle.

Cette fonctionnalité est à double tranchant: elle nous permet d'exprimer plus
facilement nos fonctions, en nous évitant d'avoir à écrire tout le temps
`lambda`. Mais dans des langages où l'application s'écrit avec des parenthèses,
cela devient fastidieux si nous voulons enchaîner la même fonction, comme nous
serions susceptible de faire pour des opérateurs binaires. En effet, si nous
voulons calculer la somme de 1 à 5, nous devons écrire
`(+ (+ (+ (+ 1 2) 3) 4) 5)` (ou `(+ 1 (+ 2 (+ 3 (+ 4 5))))`, étant donné que
l'addition est associative).

Dans un langage ne gérant pas l'application partielle, nous aurions pu écrire
`(+ 1 2 3 4 5)`, ce qui est beaucoup plus lisible. Dans ce cas, `+` est ce qu'on
appelle une fonction variadique, c'est à dire une fonction qui possède une
arité[^déf_arité] variable. Dans un langage fonctionnel, il est impossible
d'écrire (proprement) une fonction variadique.

On aurait pu écrire notre expression en utilisant la notation infixe
`1 + 2 + 3 + 4 + 5`, mais c'est assez compliqué à parser (en tous cas si on veut
pouvoir définir nos propres opérateurs) et c'est surtout pas conforme à la
syntaxe de LISP.

Notre système de fonction supporte à la fois des fonctions "normales" et des
fonctions variadiques, en implémentant un nouveau constructeur `Variadic` à
notre type `Value`.

[^déf_arité]: nombre d'argument qu'une fonction accepte
