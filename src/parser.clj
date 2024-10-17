(ns parser
  (:require [grammars :as wae]))

(defn parser-AE
  "Convierte una expresión simbólica en una expresión aritmética (AE) de la gramática."
  [exp]
  (cond
    ;;caso 1:Si es un número, lo convertimos a NumG
    (number? exp)
    (wae/numG exp)

    ;; Caso 2:si es una operación de suma <+>, la convertimos a AddG
    (and (list? exp) (= '+ (first exp)))
    (let [[_ izq der] exp] ;; el primer elemento es '+', los otros dos son las subexpresiones
      (wae/addG (parser-AE izq) (parser-AE der)))

    ;caso 3:si es una operación de resta <->, la convertimos a SubG
    (and (list? exp) (= '- (first exp)))
    (let [[_ izq der] exp] ;;el primer elemento es '-', los otros dos son las subexpresiones
      (wae/subG 
        (parser-AE izq) 
        (parser-AE der)))

    ;si no es ninguno de los casos anteriores, lanzamos un error
    :else
    (throw (IllegalArgumentException. "Expresión AE inválida"))))


(defn parser-WAE
  "Convierte una expresión simbólica en una expresión WAE de la gramática."
  [exp]
  (cond
    ;; Caso 1: Si es un número, lo convertimos a NumG
    (number? exp)
    (wae/numG exp)

    ;; Caso 2: Si es un símbolo, lo convertimos en IdG
    (symbol? exp)
    (wae/idG exp)

    ; caso 3: Si es una operacion de suma (+), la convertimos a AddG
    (and 
      (list? exp) 
      (= '+ (first exp))
    )
    
    (let [[_ izq der] exp]
      (wae/addG 
        (parser-WAE izq) 
        (parser-WAE der)
      )
    )

    ;;caso 4: si es una operacion de resta (-) la convertimos a SubG
    (and (list? exp) (= '- (first exp)))
    (let [[_ izq der] exp]
      (wae/subG (
        parser-WAE izq) 
        (parser-WAE der))
      )

    ;;caso 5: Si es una expresión 'with', la convertimos en WithG
    (and (list? exp) (= 'with (first exp)))
    (let [[_ [id valor] body] exp]
      (wae/withG (wae/bindings id 
        (parser-WAE valor)
      ) 
        (parser-WAE body)
      )
    )

    ;si no es niguno de los casos anterires, lanzamos un error.
    :else
    (throw (IllegalArgumentException. "Expresión WAE inválida"))))
