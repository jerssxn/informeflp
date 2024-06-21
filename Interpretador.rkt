#lang eopl

;******************************
;;;;; Interpretador para lenguaje con condicionales, ligadura local, procedimientos,
;;;;; procedimientos recursivos, ejecución secuencial y asignación de variables

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <expresion>
;;                      <a-program (exp)>
;;  <expresion>    ::= <number>
;;                      <lit-exp (datum)>
;;                  ::= <identificador>
;;                      <var-exp (id)>
;;                  ::= <primitive> ({<expresion>}*(,))
;;                      <primapp-exp (prim rands)>
;;                  ::= if <expresion> then <expresion> else <expresion>
;;                      <if-exp (exp1 exp2 exp23)>
;;                  ::= let {<identificador> = <expresion>}* in <expresion>
;;                      <let-exp (ids rands body)>
;;                  ::= proc({<identificador>}*(,)) <expresion>
;;                      <proc-exp (ids body)>
;;                  ::= (<expresion> {<expresion>}*)
;;                      <app-exp proc rands>
;;                  ::= letrec  {identificador ({identificador}(,)) = <expresion>} in <expresion>
;;                     <letrec-exp(proc-names idss bodies bodyletrec)>
;;                  ::= begin <expresion> {; <expresion>}* end
;;                     <begin-exp (exp exps)>
;;                  ::= set <identificador> = <expresion>
;;                     <set-exp (id rhsexp)>
;;  <primitive>     ::= + | - | * | add1 | sub1 

;******************************

;******************************
;Especificación Léxica

(define scanner-spec-simple-interpreter
'((white-sp
   (whitespace) skip)
  (comment
   ("%" (arbno (not #\newline))) skip)
  (identificador
   (letter (arbno (or letter digit "?"))) symbol)
  (digitoDecimal
   (digit (arbno digit)) number)
  (digitoDecimal
   ("-" digit (arbno digit)) number)
  (digitoBinario
   ("b" (or "0" "1") (arbno (or "0" "1"))) string)
  (digitoBinario
   ("-" "b" (or "0" "1") (arbno (or "0" "1"))) string)
  (digitoOctal
   ("0x" (or "0" "1" "2" "3" "4" "5" "6" "7")(arbno (or "0" "1" "2" "3" "4" "5" "6" "7"))) string)
  (digitoOctal
   ("-" "0x" (or "0" "1" "2" "3" "4" "5" "6" "7") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7"))) string)
  (digitoHexadecimal
   ("hx" (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))) string)
  (digitoHexadecimal
   ("-" "hx" (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))) string)
  (flotante
   (digit (arbno digit) "." digit (arbno digit)) number)
  (flotante
   ("-" digit (arbno digit) "." digit (arbno digit)) number)
  ))

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((program ((arbno struct-decl) expresion) a-program)
    
    ;; Expresiones numéricas
    (expresion (numero-exp) num-exp)  
    (numero-exp (digitoDecimal) decimal-num)
    (numero-exp (digitoBinario) bin-num)
    (numero-exp (digitoOctal) octal-num)
    (numero-exp (digitoHexadecimal) hex-num)
    (numero-exp (flotante) flotante-num)

    ;; Identificadores y cadenas
    (expresion (identificador) var-exp)
    (expresion ("\"" identificador (arbno identificador) "\"") cadena-exp)
    (expresion ("true") true-exp)
    (expresion ("false") false-exp)

    ;; Listas
    (expresion ("list" "(" (separated-list expresion ",") ")") lista-exp)
    (expresion ("cons" "(" expresion expresion ")") cons-exp)
    (expresion ("empty") empty-list-exp)
    ;; Primitivas listas
    (expresion (primitivaListas "(" expresion ")") prim-list-exp)
    (primitivaListas ("first") first-primList)
    (primitivaListas ("rest") rest-primList)
    (primitivaListas ("empty?") empty-primList)

    ;; Condicionales
    (expresion ("if" expresion "{" expresion "else" expresion "}") if-exp)

    ;; Funciones y llamadas
    (expresion ("func" "(" (separated-list identificador ",") ")" expresion) func-exp)
    (expresion ("call" expresion "(" (separated-list expresion ",") ")") app-exp)
    
    ;; Expresiones aritméticas
    (expresion ("(" expresion primitive expresion ")") prim-num-exp)
    ;; Primitivas numéricas
    (primitive ("+") sum-prim)
    (primitive ("-") minus-prim)
    (primitive ("*") mult-prim)
    (primitive ("mod") mod-prim)
    (primitive ("pow") elevar-prim)
    (primitive ("<") menor-prim)
    (primitive (">") mayor-prim)
    (primitive ("<=") menorigual-prim)
    (primitive (">=") mayorigual-prim)
    (primitive ("!=") diferente-prim)
    (primitive ("==") igual-prim)

    ;; Ligaduras modificables
    (expresion ("var" (arbno identificador "=" expresion) "in" expresion) lvar-exp)
    (expresion ("let" (arbno identificador "=" expresion) "in" expresion) let-exp)

    ;; Bloques de expresiones
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
    (expresion ("set" identificador "=" expresion) set-exp)

    ;; Estructuras
    (struct-decl ("struct" identificador "{" (arbno identificador) "}") struct-exp)
    ;; Instanciación y uso de estructuras
    (expresion ("new" identificador "(" (separated-list expresion ",") ")") new-struct-exp)
    (expresion ("get" expresion "." identificador) get-struct-exp)
    (expresion ("set-struct" expresion "." identificador "=" expresion) set-struct-exp)

    ;; Primitivas de cadenas
    (expresion (primitivaCadena "(" (separated-list expresion ",") ")") prim-cad-exp)
    (primitivaCadena ("concat") concat-primCad)
    (primitivaCadena ("string-length") length-primCad)
    (primitivaCadena ("elementAt") index-primCad)

    ;; Iteradores
    (expresion ("for" identificador "from" expresion "until" expresion "by" expresion "do" expresion) for-exp)
    (expresion ("while" expresion "{" expresion "}") while-exp)
    
    ;; Switch
    (expresion ("switch" "(" expresion ")" "{" (arbno "case" expresion ":" expresion) "default" ":" expresion "}") switch-exp)

    ;; Arrays y primitivas de arrays
    (expresion ("array" "(" (separated-list expresion ",") ")") array-exp)
    (primitivaArray ("length") length-primArr)
    (primitivaArray ("index") index-primArr)
    (primitivaArray ("slice") slice-primArr)
    (primitivaArray ("setlist") setlist-primArr)
    (expresion (primitivaArray "(" (separated-list expresion ",") ")") prim-array-exp)

    ;; Reconocimiento de patrones
    (expresion ("match" expresion "{" (arbno regular-exp "=>" expresion) "}") match-exp)
    ;; Expresiones regulares
    (regular-exp (identificador "::" identificador) list-match-exp)
    (regular-exp ("numero" "(" identificador ")") num-match-exp)
    (regular-exp ("cadena" "(" identificador ")") cad-match-exp)
    (regular-exp ("boolean" "(" identificador ")") bool-match-exp)
    (regular-exp ("array" "(" (separated-list identificador ",") ")") array-match-exp)
    (regular-exp ("empty") empty-match-exp)
    (regular-exp ("default") default-match-exp)

    ;; Primitivas booleanas
    (primitivaBooleana ("and") and-prim)
    (primitivaBooleana ("or") or-prim)
    (primitivaBooleana ("xor") xor-prim)
    (primitivaBooleana ("not") not-prim)
    (expresion (primitivaBooleana "(" (separated-list expresion ",") ")") prim-bool-exp)
    
    ;; Expresión literal
    ;;(expresion (number) lit-exp)
    ))

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;*******************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (struc-dec body)
                 (update-struct-env! struc-dec)
                 (eval-expresion body (empty-env))))))


;eval-expresion: <expresion> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define eval-expresion
  (lambda (exp env)
    (cases expresion exp
      (num-exp (tipo_numero)
        (cases numero-exp tipo_numero
          (decimal-num (dato)  dato)
          (bin-num (dato)  dato)
          (octal-num (dato) dato)
          (hex-num (dato) dato)
          (flotante-num (dato) dato)
        )
      )
      (prim-bool-exp (prim args)
        (operaciones_para_boleanos prim (eval-rands args env))
      )
      (var-exp (id) (apply-env env id))
      (true-exp () #T)
      (false-exp () #F)
      (prim-num-exp (exp1 prim exp2)
                   (let ((eexp1 (eval-expresion exp1 env))
                        (eexp2 (eval-expresion exp2 env))
                      )
                     (apply-primitive prim eexp1 eexp2)))
      (prim-cad-exp (prim rands)
        (let ((args (eval-rands rands env)))
          (apply-primitive_string prim args)
        )
      )
      (if-exp (test-exp true-exp false-exp)
              (if (eval-expresion test-exp env)
                  (eval-expresion true-exp env)
                  (eval-expresion false-exp env)))
      (func-exp (ids body)
                (closure ids body env))
      (app-exp (rator rands)
               (let ((proc (eval-expresion rator env))
                     (args (eval-rands rands env)))
                 (if (procval? proc)
                     (apply-procedure proc args env)
                     (eopl:error 'eval-expresion
                                 "Attempt to apply non-procedure ~s" proc))))
      (set-exp (id rhs-exp)
        (let ((argu (eval-expresion rhs-exp env)))
          (modificar-env env id argu)
          'void)
        )
        
      (begin-exp (exp exps) 
                 (let loop ((acc (eval-expresion exp env))
                             (exps exps))
                    (if (null? exps) 
                        acc
                        (loop (eval-expresion (car exps) 
                                               env)
                              (cdr exps)))))
      (let-exp (ids rands body)
               (let ((args (eval-rands rands env)))
                 (eval-expresion body
                                  (extend-env ids args env))))
      (lvar-exp (ids rands body)
        (let ((args (eval-rands rands (extend-mod-env ids (list->vector rands) env))))
                 (eval-expresion body
                                  (extend-mod-env ids (list->vector args) env))))
      (cadena-exp (identificador Lidentifica)
        (letrec
          [
            (crear_string
              (lambda (lids)
                (cond
                  [(null? lids) ""]
                  [else (string-append " " (symbol->string (car lids)) (crear_string (cdr lids)))]
                )
              )
            )
          ]
          (string-append (symbol->string identificador) (crear_string Lidentifica))
        )
      ) 
      (lista-exp (Lexp)
        (eval-rands Lexp env))
      (cons-exp (exp1 exp2)
        (cons (eval-rand exp1 env) (eval-rand exp2 env)))
      (prim-list-exp (prim exp)
        (let ((arg (eval-rand exp env)))
          (apply-list prim arg)))
      (while-exp (boolean_exp body_exp)
          (cond 
              [(eval-expresion boolean_exp env)
                (eval-expresion body_exp env)
                (eval-expresion exp env)
              ]
              [else 'void]
          )
      )
      (for-exp (var start-exp end-exp sum-exp body-exp)
        (let ((start (eval-expresion start-exp env))
              (end (eval-expresion end-exp env))
              (sum (eval-expresion sum-exp env))
            )
          (let loop ((i start))
            (when (< i end)
              (eval-expresion body-exp (extend-env (list var) (list i) env))
              (loop (+ i sum)))))
      )
      (switch-exp (var_exp list_caso list_exp default_exp)
        (letrec ((valor (eval-expresion var_exp env))
            (coinciden
              (lambda (caso list_e valor)
                (cond
                  [(null? caso) (eval-expresion default_exp env)]
                  [(equal? valor (eval-expresion (car caso) env)) (eval-expresion (car list_e) env)]
                  [else (coinciden (cdr caso) (cdr list_e) valor)]
                )
              )
            )
          )
          (coinciden list_caso list_exp valor)
        )
      )
      (array-exp (lista) 
        (list->vector (eval-rands lista env))
      )
      (prim-array-exp (primitiva lista_argumentos)
        (primitiva-array primitiva (eval-rands lista_argumentos env))
      )
      (empty-list-exp () '())
      (match-exp (exp_var list_casos lista_exp)
        (let ((valor (eval-expresion exp_var env)))
          (detector_patron valor list_casos lista_exp env)
        )
      )
      (new-struct-exp (identi lista_atributos)
        (let ((evalu_list (eval-rands lista_atributos env)) (struct (lookup-struct identi)))
          (if (= (length (struct-decl->attributes struct)) (length evalu_list))
            (list (struct-decl->attributes struct) (list->vector evalu_list))
            (eopl:error "Error el elemento no contiene la lista de atributos requerida, requerido:" (struct-decl->attributes struct))
          )
        )
      )
      (get-struct-exp (struc atributo)
        (let ((struct (eval-rand struc env)))
          (vector-ref  (cadr struct) (find-attribute (car struct) atributo 0) )
        )
      )
      (set-struct-exp (strucVar atributo nuevo_valor)
        (letrec ((struct (eval-rand strucVar env)) (eNuevo_val (eval-rand nuevo_valor env)))
          (vector-set! (cadr struct) (find-attribute (car struct) atributo 0) eNuevo_val)
          'void
        )
      )
    )
  )
)


(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expresion rand env)))


(define operaciones_para_boleanos
(lambda (op args)
  (cases primitivaBooleana op 
    (and-prim () 
      (and (car args) (cadr args)))
    (or-prim () 
      (or (car args) (cadr args)))
    (not-prim () 
      (not (car args)))
    (xor-prim () 
      (or (and (car args) (not (cadr args))) (and (not (car args)) (cadr args)))))))


(define quitar_caracter 
  (lambda (str ch)
    (cond
      [(null? str) '()]
      [else 
       (if (char=? ch (car str))
           (quitar_caracter (cdr str) ch)
           (cons (car str) (quitar_caracter (cdr str) ch)))
      ])))

(define eliminarCaracter
  (lambda (stri ch)
    (list->string (quitar_caracter (string->list stri) ch))))

(define replace-char 
  (lambda (str ch new-ch)
    (cond
      [(null? str) '()]
      [else 
       (if (char=? ch (car str))
           (cons new-ch (replace-char (cdr str) ch new-ch))
           (cons (car str) (replace-char (cdr str) ch new-ch)))])))

(define reemplazar-caracter
  (lambda (stri ch new-ch)
    (list->string (replace-char (string->list stri) ch new-ch))))

(define operar_numeros
  (lambda (operacion arg1 arg2)
    (cond
      [(string? arg1)
       (cond 
         [(or (equal? (string-ref arg1 0) #\b) 
              (and (equal? (string-ref arg1 1) #\b) (equal? (string-ref arg1 0) #\-)))
          (list 
            (operacion 
              (string->number (eliminarCaracter arg1 #\b) 2) (string->number (eliminarCaracter arg2 #\b) 2))
            2)
         ]
         [(or (equal? (string-ref arg1 0) #\h) 
              (and (equal? (string-ref arg1 1) #\h) (equal? (string-ref arg1 0) #\-)))
          (list 
            (operacion 
              (string->number (reemplazar-caracter arg1 #\h #\#) 16) 
              (string->number (reemplazar-caracter arg2 #\h #\#) 16))
            16)
         ]
         [(or (equal? (string-ref arg1 1) #\x) 
              (and (equal? (string-ref arg1 2) #\x) (equal? (string-ref arg1 0) #\-)))
          (list 
            (operacion 
              (string->number (eliminarCaracter arg1 #\x) 8) 
              (string->number (eliminarCaracter arg2 #\x) 8))
            8)
         ]
         [else (eopl:error "Error de formato")]
       )
      ]
      [(number? arg1) (list (operacion arg1 arg2) 10)]
      [else (eopl:error "valor incorreccto")])))

(define retorno-igual
  (lambda (out base) out))

(define convertir-numero
  (lambda ( list_numero_temporal  )
    (let* ((es-negativo? (< (car list_numero_temporal) 0))
           (abs-num (abs (car list_numero_temporal)))
           (base (abs (cadr list_numero_temporal)))
          )
      (cond
        [(eqv? base 2) (if es-negativo? 
          (string-append "-" "b" (number->string abs-num 2))
          (string-append "b" (number->string abs-num 2)))
        ]
        [(eqv? base 8) (if es-negativo? 
          (string-append "-" "0x" (number->string abs-num 8))
          (string-append "0x" (number->string abs-num 8)))      
        ]
        [(eqv? base 16) (if es-negativo? 
          (string-append "-" "hx" (number->string abs-num 16))
          (string-append "hx" (number->string abs-num 16)))
        ]
        [(eqv? base 10) abs-num]
        [else (eopl:error "Base no soportada")]))))

(define apply-primitive
  (lambda (prim arg1 arg2)
    (cases primitive prim
      (sum-prim () (convertir-numero (operar_numeros + arg1 arg2 )))
      (minus-prim () (convertir-numero (operar_numeros - arg1 arg2)))
      (mult-prim () (convertir-numero (operar_numeros * arg1 arg2 )))
      (mayor-prim () (car (operar_numeros > arg1 arg2 )))
      (menor-prim () (car (operar_numeros < arg1 arg2 )))
      (menorigual-prim () (car (operar_numeros <= arg1 arg2)))
      (mayorigual-prim () (car (operar_numeros >= arg1 arg2)))
      (diferente-prim () ( not (equal? arg1 arg2)))
      (igual-prim () (equal? arg1 arg2))
      (mod-prim () (convertir-numero (operar_numeros remainder arg1 arg2)))
      (elevar-prim () (convertir-numero (operar_numeros expt arg1 arg2))))))


(define apply-primitive_string
  (lambda (prim args)
    (cases primitivaCadena prim
      (concat-primCad () (apply string-append args))
      (length-primCad () (string-length (car args)))
      (index-primCad ()  (string  (string-ref (car args) (cadr args)))))))

(define apply-list
  (lambda (prim arg)
    (cases primitivaListas prim
      (first-primList () (car arg))
      (rest-primList () (cdr arg))
      (empty-primList () (null? arg)))))


(define subvector
  (lambda (vect inicio final)
    (cond
      [(= inicio final) (cons (vector-ref vect inicio) '())]
      [else (cons (vector-ref vect inicio) (subvector vect (+ inicio 1) final))])))

(define primitiva-array
  (lambda (prim arg)
    (cases primitivaArray prim
      (length-primArr () (vector-length (car arg)))
      (index-primArr () (vector-ref (car arg) (cadr arg)))
      (slice-primArr () (list->vector (subvector  (car arg) (cadr arg) (caddr arg))))
      (setlist-primArr () 
        (vector-set! (car arg) (cadr arg) (caddr arg))
        (car arg)))))


(define detector_patron 
  (lambda (valor primt_match expresi_match env)
    (cases regular-exp (car primt_match)
      (empty-match-exp () 
        (if (null? valor)
          (eval-expresion (car expresi_match) env)
          (detector_patron valor (cdr primt_match) (cdr expresi_match) env)))
      (list-match-exp (cabeza cola) 
        (if (list? valor)
          (eval-expresion (car expresi_match) (extend-env (cons cabeza (cons cola '())) (list (car valor) (cdr valor)) env))
          (detector_patron valor (cdr primt_match) (cdr expresi_match) env)))
      (num-match-exp (ids) 
        (if (number? valor)
          (eval-expresion (car expresi_match) (extend-env (list ids) (list valor) env))
          (if (string? valor)
            (cond
              [(or (equal? (string-ref valor 0) #\b) (and (equal? (string-ref valor 1) #\b) (equal? (string-ref valor 0) #\-)))
                (eval-expresion (car expresi_match) (extend-env (list ids) (list valor) env))
              ]
              [(or (equal? (string-ref valor 0) #\h) (and (equal? (string-ref valor 1) #\h) (equal? (string-ref valor 0) #\-)))
                (eval-expresion (car expresi_match) (extend-env (list ids) (list valor) env))
              ]
              [(or (equal? (string-ref valor 1) #\x) (and (equal? (string-ref valor 2) #\x) (equal? (string-ref valor 0) #\-)))
                (eval-expresion (car expresi_match) (extend-env (list ids) (list valor) env))
              ]
              [else (detector_patron valor (cdr primt_match) (cdr expresi_match) env)]
            )
            (detector_patron valor (cdr primt_match) (cdr expresi_match) env))))
      (cad-match-exp (ids) 
        (if (string? valor)
          (cond
            [(not (or (equal? (string-ref valor 0) #\b) 
                      (and (equal? (string-ref valor 1) #\b) 
                          (equal? (string-ref valor 0) #\-))))
            (not (or (equal? (string-ref valor 0) #\h) 
                      (and (equal? (string-ref valor 1) #\h) 
                          (equal? (string-ref valor 0) #\-))))
            (not (or (equal? (string-ref valor 1) #\x) 
                      (and (equal? (string-ref valor 2) #\x) 
                          (equal? (string-ref valor 0) #\-))))
            (eval-expresion (car expresi_match) (extend-env (list ids) (list valor) env))
            ]
            [else (detector_patron valor (cdr primt_match) (cdr expresi_match) env)]
          )
          (detector_patron valor (cdr primt_match) (cdr expresi_match) env)))
      (bool-match-exp (ids) 
        (if (boolean? valor)
          (eval-expresion (car expresi_match) (extend-env (list ids) (list valor) env))
          (detector_patron valor (cdr primt_match) (cdr expresi_match) env)))
      (array-match-exp (ids) 
        (if (vector? valor)
          (eval-expresion (car expresi_match) (extend-env ids (transfomar_vector ids valor 0) env))
          (detector_patron valor (cdr primt_match) (cdr expresi_match) env)))
      (default-match-exp () 
        (eval-expresion (car expresi_match) env)))))

(define transfomar_vector
  (lambda (lis_ids vect acc)
    (cond
      [(null? (cdr lis_ids)) (cons (subvector vect acc (- (vector-length vect) 1)) '())]
      [else (cons (vector-ref vect acc) (transfomar_vector (cdr lis_ids) vect (+ acc 1)))])))

(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expresion?)
   (env environment?)))

(define apply-procedure
  (lambda (proc args envI)
    (cases procval proc
      (closure (ids body env)
        (cases environment env
          (extend-mod-env (lid lval next-env) 
            (eval-expresion body (extend-env ids args envI))
          )
          (else (eval-expresion body (extend-env ids args envI))))))))


(define-datatype environment environment?
  (empty-env)
  (extend-env
   (syms (list-of symbol?))
   (valores (list-of scheme-value?))
   (env environment?))
  (extend-mod-env
   (syms (list-of symbol?))
   (valores vector?)
   (env environment?)))

(define scheme-value? (lambda (v) #t))

;funcion para buscar una variable en un ambiente
(define buscador 
  (lambda (listIds listVal valor_vuscado next-amb acc env)
    (cond
      [(null? listIds) (apply-env next-amb valor_vuscado)]
      [(equal? (car listIds) valor_vuscado) 
        (cond
          [(vector? listVal) (if (expresion? (vector-ref listVal acc)) (eval-expresion (vector-ref listVal acc) env) (vector-ref listVal acc))]
          [(list? listVal) (list-ref listVal acc)]
        )
      ]
      [else (buscador (cdr listIds) listVal valor_vuscado next-amb (+ acc 1) env)]
    )  
  ) 
)

(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env () (eopl:error "variable no encontrada " sym))
      (extend-env (lid lval next-env)
        (buscador lid lval sym next-env 0 env))
      (extend-mod-env (lid lval next-env)
        (buscador lid lval sym next-env 0 env)))))


(define buscador!
  (lambda (lid lval valBus next-amb)
    (cond
      [(null? lid) (extend-env (apply-env next-amb valBus))]
      [(equal? (car lid) valBus) (car lval)]
      [else (buscador! (cdr lid) (cdr lval) valBus next-amb)])))

(define modificar-env
  (lambda (env sym val)
    (cases environment env
      (extend-mod-env (symB Vect envNext)
        (letrec
          [
            (encontrar_indice
              (lambda (Lsym acc)
                (cond
                  [(null? Lsym) (eopl:error "indice no encontrado " sym)]
                  [(equal? (car Lsym) sym) acc]
                  [else (encontrar_indice (cdr Lsym) (+ acc 1))]
                )
              )
            )
            (indice (encontrar_indice symB 0))
          ]
          (vector-set! Vect indice val)
        )
      )
      (extend-env (symB list next-env)
        (modificar-env next-env sym val)
      )
      (else (eopl:error "variable no encontrada en un ambiente valido")))))


(define struct-decl->name
  (lambda (struct)
    (cases struct-decl struct
      (struct-exp (struct-name field-list)
        struct-name)
      (else (eopl:error "Invalid struct"))
    )
  )
)

(define struct-decl->attributes
  (lambda (struct)
    (cases struct-decl struct
      (struct-exp (struct-name field-list)
        field-list)
      (else (eopl:error "Invalid struct")))))

(define struct-env '())

(define update-struct-env!
  (lambda (s-decls)
    (set! struct-env s-decls)))

(define lookup-struct
  (lambda (name)
    (let loop ((env struct-env))
      (cond
        [(null? env) (eopl:error "Unknown struct: " name)]
        [(eqv? (struct-decl->name (car env)) name) (car env)]
        [else (loop (cdr env))]))))

(define find-attribute
  (lambda (attributes attr acc)
    (cond
      [(null? attributes) (eopl:error "Attribute not found: " attr)]
      [(equal? (car attributes) attr) acc]
      [else (find-attribute (cdr attributes) attr (+ acc 1))])))

(show-the-datatypes)


(interpretador)

#|
No tocar
Exporar funciones
|#
(provide (all-defined-out))
