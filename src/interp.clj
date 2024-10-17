; Continuara...

(ns interp
  (:require [grammars :as wae]))

(defn interp-AE
  "Interpreta una expresión AE y devuelve su valor numérico."
  [exp]
  (cond
    ;;Caso 1:i es una expresiopn NumG, devolvemos el número directamente.
    (instance? wae/NumG exp)
        
        (:n exp)

    ;caso 2: si es una expresion AddG, evaluamos las dos subepresiones y las sumamos
    (instance? wae/AddG exp)
    (+ (interp-AE (:izq exp)) 
        
        (interp-AE (:der exp)))

    ;;caso 3: sii es una expresión SubG, evaluamos las dos subexprsiones y las restaos.
    (instance? wae/SubG exp)
    (- (interp-AE (:izq exp)) 
        
        (interp-AE (:der exp)))

    ;; Si no es ninguno de loscasos anteriores lanzmos un error
    :else
    (throw (IllegalArgumentException. "Expresión AE inválida"))))


(defn interp-WAE
  "Interpreta una expresión WAE en un entorno y devuelve su valor numérico."
  ([exp]
   (interp-WAE exp {})) ;;iniciamos con un entorno vacío.
  ([exp env]
   (cond
     ;;caso 1: Si es una expresión NumG, devolvemos el número directamente
     (instance? wae/NumG exp)
     (:n exp)

     ;;caso 2: Si es una expresión IdG, buscamos su valor en el entorno
     (instance? wae/IdG exp)
     (if-let [val (get env (:i exp))]
       val
       (throw (IllegalArgumentException. 
            
            (str "Variable no definida: " (:i exp)))))

     ;;;caso 3: Si es una expresión AddG, evaluamos las dos subexpresiones y las sumamos
     (instance? wae/AddG exp)
        (+ (interp-WAE (:izq exp) env) 
            
            (interp-WAE (:der exp) env))

     ;;caso 4: Si es una expresión SubG, evaluamos las dos subexpresiones y las restamos.
     (instance? wae/SubG exp)
        (- (interp-WAE (:izq exp) env) 
            (interp-WAE (:der exp) env))

     ;;caso 5: Si es una expresión WithG, evaluamos la asignación y extendemos el entorno
     (instance? wae/WithG exp)
        (let [id (:id (:assign exp))  ;;tomamos el identificador
            
            val (interp-WAE (:value 
                (:assign exp)) env) ;;evaluamos el valor asignado
            
            new-env (assoc env id val)];;extendemos el entorno
        (interp-WAE 
            
            (:body exp) new-env));ealuamos el cuerpo con el nuevo entorno

     ;si no es ninguno de loscasos anteriores, lanzamos un error.
     :else
     (throw (IllegalArgumentException. "Expresión WAE inválida")))))
