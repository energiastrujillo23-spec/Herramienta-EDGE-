; ================================================================
; ROBOT DE AREAS Y PDF - AUTOMATIZACION COMPLETA
; Version 8.1 (Tabla Minimalista 3 Cols + Zonas Dinamicas + PDF Auto A1)
; ================================================================

(vl-load-com)

(setq factor-m2 1.0)
;; En AutoCAD: "70" = 70% de Transparencia (30% de Opacidad)
(setq valor-transparencia "70")

;; Catalogo dinamico en sesion (se inicializa con las zonas base y admite nuevas)
(if (not (boundp '*ROBOT-CATALOGO*))
  (setq *ROBOT-CATALOGO* nil)
)

; ================================================================
; CATALOGO DINAMICO DE NOMBRES Y COLORES (ACI)
; ================================================================
(defun obtener-catalogo ()
  (if (not *ROBOT-CATALOGO*)
    (setq *ROBOT-CATALOGO*
      (list
        (list "Dry Storage" 211)                  ; Rosa
        (list "General Sales Area" 5)             ; Azul rey
        (list "Office" 3)                         ; Verde
        (list "Mechanical & Electrical Room" 30)  ; Naranja
        (list "Kitchen & Food Preparation" 1)     ; Rojo
        (list "Corridors and loby" 8)             ; Gris
      )
    )
  )
  *ROBOT-CATALOGO*
)

(defun registrar-zona-en-catalogo (nombre color / cat nombre-up encontrado)
  (setq cat (obtener-catalogo)
        nombre-up (strcase (vl-string-trim " " nombre))
        encontrado nil)
  (foreach item cat
    (if (= (strcase (car item)) nombre-up)
      (setq encontrado item)
    )
  )
  (if (not encontrado)
    (progn
      (setq *ROBOT-CATALOGO* (append cat (list (list (vl-string-trim " " nombre) color))))
      (list (vl-string-trim " " nombre) color)
    )
    encontrado
  )
)

(defun clasificar-nombre (cadena / texto cat res)
  (if (and cadena (/= (vl-string-trim " " cadena) ""))
    (progn
      (setq texto (strcase (limpiar-mtext cadena))
            cat   (obtener-catalogo)
            res   nil)
      ;; 1. Coincidencia exacta o por nombre en catalogo dinamico
      (foreach item cat
        (if (and (not res)
                 (or (= texto (strcase (car item)))
                     (wcmatch texto (strcat "*" (strcase (car item)) "*"))))
          (setq res item)
        )
      )
      ;; 2. Reglas de patrones flexibles para zonas base
      (if (not res)
        (setq res
          (cond
            ((wcmatch texto "*DRY*STORAGE*")
              (list "Dry Storage" 211))
            ((wcmatch texto "*GENERAL*SALES*")
              (list "General Sales Area" 5))
            ((wcmatch texto "*OFFICE*")
              (list "Office" 3))
            ((wcmatch texto "*MECHANICAL*,*ELECTRICAL*")
              (list "Mechanical & Electrical Room" 30))
            ((wcmatch texto "*KITCHEN*,*FOOD*")
              (list "Kitchen & Food Preparation" 1))
            ((wcmatch texto "*CORRIDOR*,*LOBY*,*LOBBY*")
              (list "Corridors and loby" 8))
            (T nil)
          )
        )
      )
      res
    )
    nil
  )
)

; ================================================================
; FUNCIONES AUXILIARES
; ================================================================

(defun limpiar-mtext (str / pos)
  (if (not str) (setq str ""))
  (while (setq pos (vl-string-search "\\P" str))
    (setq str (strcat (substr str 1 pos) " " (substr str (+ pos 3))))
  )
  (while (setq pos (vl-string-search "\\p" str))
    (setq str (strcat (substr str 1 pos) " " (substr str (+ pos 3))))
  )
  (while (setq pos (vl-string-search ";" str))
    (setq str (substr str (+ pos 2)))
  )
  (vl-string-trim " {}" str)
)

(defun verificar-y-cerrar-polilinea (ent / obj res-closed cerrada pt-ini pt-fin dist)
  (setq obj (vlax-ename->vla-object ent)
        cerrada nil)
  (if (not (vl-catch-all-error-p (setq res-closed (vl-catch-all-apply 'vla-get-Closed (list obj)))))
    (progn
      (setq cerrada (= :vlax-true res-closed))
      (if (not cerrada)
        (progn
          (setq pt-ini (vl-catch-all-apply 'vlax-curve-getStartPoint (list ent))
                pt-fin (vl-catch-all-apply 'vlax-curve-getEndPoint (list ent)))
          (if (and (listp pt-ini) (listp pt-fin))
            (progn
              (setq dist (distance pt-ini pt-fin))
              (if (< dist 0.25)
                (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-put-Closed (list obj :vlax-true))))
                  (setq cerrada T)
                )
              )
            )
          )
        )
      )
    )
  )
  cerrada
)

(defun centro-objeto (obj / minpt maxpt p1 p2 ins-var)
  (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-GetBoundingBox (list obj 'minpt 'maxpt))))
    (progn
      (setq p1 (vlax-safearray->list minpt)
            p2 (vlax-safearray->list maxpt))
      (list (/ (+ (car p1) (car p2)) 2.0)
            (/ (+ (cadr p1) (cadr p2)) 2.0)
            0.0)
    )
    (if (not (vl-catch-all-error-p (setq ins-var (vl-catch-all-apply 'vla-get-InsertionPoint (list obj)))))
      (vlax-safearray->list (vlax-variant-value ins-var))
      '(0.0 0.0 0.0)
    )
  )
)

(defun obtener-vertices-polilinea (ent / datos tipo pts subent subdatos)
  (setq datos (entget ent)
        tipo (cdr (assoc 0 datos))
        pts nil)
  (cond
    ((= tipo "LWPOLYLINE")
      (foreach d datos
        (if (= (car d) 10)
          (setq pts (cons (cdr d) pts))
        )
      )
    )
    ((= tipo "POLYLINE")
      (setq subent (entnext ent))
      (while (and subent
                  (setq subdatos (entget subent))
                  (/= (cdr (assoc 0 subdatos)) "SEQEND"))
        (if (= (cdr (assoc 0 subdatos)) "VERTEX")
          (setq pts (cons (cdr (assoc 10 subdatos)) pts))
        )
        (setq subent (entnext subent))
      )
    )
  )
  (reverse pts)
)

(defun punto-dentro-polilinea-p (pt ent / pts p1 p2 x y x1 y1 x2 y2 dentro)
  (setq pts (obtener-vertices-polilinea ent))
  (if (and pt (> (length pts) 2))
    (progn
      (setq x (car pt) y (cadr pt) dentro nil p1 (last pts))
      (foreach p2 pts
        (setq x1 (car p1) y1 (cadr p1) x2 (car p2) y2 (cadr p2))
        (if (and (or (and (<= y1 y) (< y y2)) (and (<= y2 y) (< y y1)))
                 (< x (+ x1 (/ (* (- x2 x1) (- y y1)) (float (- y2 y1))))))
          (setq dentro (not dentro))
        )
        (setq p1 p2)
      )
      dentro
    )
    nil
  )
)

(defun buscar-categoria-por-texto-interno (ent ss-textos / i t-ent t-obj pt-c pt-ins ins-var txt-str info)
  (setq info nil)
  (if ss-textos
    (progn
      (setq i 0)
      (while (and (< i (sslength ss-textos)) (not info))
        (setq t-ent (ssname ss-textos i)
              t-obj (vlax-ename->vla-object t-ent)
              txt-str (vl-catch-all-apply 'vla-get-TextString (list t-obj)))
        (if (not (vl-catch-all-error-p txt-str))
          ;; Ignorar etiquetas viejas que ya tengan " m2"
          (if (not (wcmatch (strcase (limpiar-mtext txt-str)) "* M2*"))
            (if (setq info (clasificar-nombre txt-str))
              (progn
                (setq pt-c (centro-objeto t-obj)
                      pt-ins (if (not (vl-catch-all-error-p (setq ins-var (vl-catch-all-apply 'vla-get-InsertionPoint (list t-obj)))))
                               (vlax-safearray->list (vlax-variant-value ins-var))
                               pt-c))
                (if (not (or (punto-dentro-polilinea-p pt-c ent)
                             (punto-dentro-polilinea-p pt-ins ent)))
                  (setq info nil)
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )
  info
)

(defun detectar-zona (ent ss-textos / capa info)
  (setq capa (cdr (assoc 8 (entget ent))))
  (setq info (clasificar-nombre capa))
  (if (not info)
    (setq info (buscar-categoria-por-texto-interno ent ss-textos))
  )
  info
)

(defun obtener-area-m2 (ent / obj area-val)
  (setq obj (vlax-ename->vla-object ent))
  (if (and (vlax-property-available-p obj 'Area)
           (not (vl-catch-all-error-p (setq area-val (vl-catch-all-apply 'vla-get-Area (list obj))))))
    (/ area-val factor-m2)
    0.0
  )
)

(defun obtener-polilineas ()
  (ssget "_X" '((0 . "LWPOLYLINE,POLYLINE") (410 . "Model") (-4 . "<NOT") (8 . "TABLA_LEYENDAS_AREAS") (-4 . "NOT>")))
)

(defun obtener-textos ()
  (ssget "_X" '((0 . "TEXT,MTEXT") (410 . "Model") (-4 . "<NOT") (8 . "TABLA_LEYENDAS_AREAS") (-4 . "NOT>")))
)

(defun asegurar-capa (nombre color / acad doc layers layer l-ename l-data)
  (setq acad (vlax-get-acad-object)
        doc (vla-get-ActiveDocument acad)
        layers (vla-get-Layers doc))
  (if (vl-catch-all-error-p (setq layer (vl-catch-all-apply 'vla-Item (list layers nombre))))
    (setq layer (vla-Add layers nombre))
  )
  (vl-catch-all-apply 'vla-put-Lock (list layer :vlax-false))
  (vl-catch-all-apply 'vla-put-Freeze (list layer :vlax-false))
  (vl-catch-all-apply 'vla-put-LayerOn (list layer :vlax-true))
  (vl-catch-all-apply 'vla-put-color (list layer color))
  ;; Asegurar que la capa no tenga transparencia propia distinta que altere el Hatch
  (setq l-ename (vlax-vla-object->ename layer)
        l-data  (entget l-ename))
  (if (assoc 440 l-data)
    (entmod (vl-remove (assoc 440 l-data) l-data))
  )
  layer
)

;; Fuerza exactamente 70% de TRANSPARENCIA (30% de Opacidad -> Alpha = 77 -> DXF 440 = 33554509)
(defun aplicar-opacidad-70 (h-ename hatch-obj / edata valor-dxf-440)
  (setq valor-dxf-440 (+ 33554432 77))
  (vl-catch-all-apply 'vla-put-EntityTransparency (list hatch-obj valor-transparencia))
  (setq edata (entget h-ename))
  (if (assoc 440 edata)
    (setq edata (subst (cons 440 valor-dxf-440) (assoc 440 edata) edata))
    (setq edata (append edata (list (cons 440 valor-dxf-440))))
  )
  (entmod edata)
  (entupd h-ename)
)

;; Limpia hatches previos y tablas anteriores
(defun limpiar-elementos-previos (/ ss-h ss-tab i ent capa)
  (if (setq ss-h (ssget "_X" '((0 . "HATCH"))))
    (progn
      (setq i 0)
      (while (< i (sslength ss-h))
        (setq ent (ssname ss-h i)
              capa (cdr (assoc 8 (entget ent))))
        (if (or (clasificar-nombre capa)
                (= (strcase capa) "TABLA_LEYENDAS_AREAS"))
          (vl-catch-all-apply 'entdel (list ent))
        )
        (setq i (1+ i))
      )
    )
  )
  (if (setq ss-tab (ssget "_X" '((8 . "TABLA_LEYENDAS_AREAS"))))
    (progn
      (setq i 0)
      (while (< i (sslength ss-tab))
        (vl-catch-all-apply 'entdel (list (ssname ss-tab i)))
        (setq i (1+ i))
      )
    )
  )
)

(defun crear-hatch-area (ent nombre-capa color / acad doc mspace obj-poly loop-array hatch-obj h-ename)
  (setq acad (vlax-get-acad-object)
        doc (vla-get-ActiveDocument acad)
        mspace (vla-get-ModelSpace doc)
        obj-poly (vlax-ename->vla-object ent))
  (asegurar-capa nombre-capa color)
  (if (not (vl-catch-all-error-p
             (setq hatch-obj (vl-catch-all-apply
                               'vla-AddHatch
                               (list mspace acHatchPatternTypePreDefined "SOLID" :vlax-true)))))
    (progn
      (setq loop-array (vlax-make-safearray vlax-vbObject '(0 . 0)))
      (vlax-safearray-put-element loop-array 0 obj-poly)
      (if (and (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-AppendOuterLoop (list hatch-obj loop-array))))
               (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-Evaluate (list hatch-obj)))))
        (progn
          (vl-catch-all-apply 'vla-put-Layer (list hatch-obj nombre-capa))
          (vl-catch-all-apply 'vla-put-Color (list hatch-obj acByLayer))
          (setq h-ename (vlax-vla-object->ename hatch-obj))
          (aplicar-opacidad-70 h-ename hatch-obj)
          hatch-obj
        )
        (progn
          (vl-catch-all-apply 'vla-Delete (list hatch-obj))
          nil
        )
      )
    )
    nil
  )
)

; ================================================================
; CALCULO DE CAJA ENVOLVENTE Y ESCALA AUTOMATICA PARA LA TABLA
; ================================================================
(defun obtener-limites-ss (ss / i ent obj minpt maxpt p1 p2 min-x min-y max-x max-y)
  (setq min-x nil min-y nil max-x nil max-y nil i 0)
  (if ss
    (while (< i (sslength ss))
      (setq ent (ssname ss i)
            obj (vlax-ename->vla-object ent))
      (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-GetBoundingBox (list obj 'minpt 'maxpt))))
        (progn
          (setq p1 (vlax-safearray->list minpt)
                p2 (vlax-safearray->list maxpt))
          (if (or (not min-x) (< (car p1) min-x)) (setq min-x (car p1)))
          (if (or (not min-y) (< (cadr p1) min-y)) (setq min-y (cadr p1)))
          (if (or (not max-x) (> (car p2) max-x)) (setq max-x (car p2)))
          (if (or (not max-y) (> (cadr p2) max-y)) (setq max-y (cadr p2)))
        )
      )
      (setq i (1+ i))
    )
  )
  (if (and min-x min-y max-x max-y)
    (list min-x min-y max-x max-y)
    nil
  )
)

;; Obtiene la caja del plano real (marco + solapa) ignorando entidades perdidas muy lejanas
(defun obtener-caja-plano-real (ss-polys / lim-p p-min-x p-min-y p-max-x p-max-y p-ancho p-alto
                                           ss-todo i ent obj minpt maxpt p1 p2
                                           min-x min-y max-x max-y lim-x1 lim-x2 lim-y1 lim-y2)
  (setq lim-p (obtener-limites-ss ss-polys))
  (setq ss-todo (ssget "_X" '((410 . "Model") (-4 . "<NOT") (8 . "TABLA_LEYENDAS_AREAS,DEFPOINTS") (-4 . "NOT>"))))
  (if (not lim-p)
    (obtener-limites-ss ss-todo)
    (progn
      (setq p-min-x (nth 0 lim-p)
            p-min-y (nth 1 lim-p)
            p-max-x (nth 2 lim-p)
            p-max-y (nth 3 lim-p)
            p-ancho (max 10.0 (- p-max-x p-min-x))
            p-alto  (max 10.0 (- p-max-y p-min-y))
            lim-x1  (- p-min-x (* 0.8 p-ancho))
            lim-x2  (+ p-max-x (* 1.2 p-ancho))
            lim-y1  (- p-min-y (* 0.8 p-alto))
            lim-y2  (+ p-max-y (* 0.8 p-alto))
            min-x p-min-x min-y p-min-y max-x p-max-x max-y p-max-y
            i 0)
      (if ss-todo
        (while (< i (sslength ss-todo))
          (setq ent (ssname ss-todo i)
                obj (vlax-ename->vla-object ent))
          (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-GetBoundingBox (list obj 'minpt 'maxpt))))
            (progn
              (setq p1 (vlax-safearray->list minpt)
                    p2 (vlax-safearray->list maxpt))
              ;; Solo incluir entidades dentro del entorno razonable de la lamina
              (if (and (>= (car p1) lim-x1) (<= (car p2) lim-x2)
                       (>= (cadr p1) lim-y1) (<= (cadr p2) lim-y2))
                (progn
                  (if (< (car p1) min-x) (setq min-x (car p1)))
                  (if (< (cadr p1) min-y) (setq min-y (cadr p1)))
                  (if (> (car p2) max-x) (setq max-x (car p2)))
                  (if (> (cadr p2) max-y) (setq max-y (cadr p2)))
                )
              )
            )
          )
          (setq i (1+ i))
        )
      )
      (list min-x min-y max-x max-y)
    )
  )
)

(defun calcular-caja-dibujo (ss-polys / lim min-x min-y max-x max-y ancho alto)
  (setq lim (obtener-caja-plano-real ss-polys))
  (if lim
    (progn
      (setq min-x (nth 0 lim)
            min-y (nth 1 lim)
            max-x (nth 2 lim)
            max-y (nth 3 lim)
            ancho (max 10.0 (- max-x min-x))
            alto  (max 10.0 (- max-y min-y)))
      ;; Colocar la tabla pegada al costado derecho del marco (1.5% del ancho) y alineada arriba
      (list (list (+ max-x (* ancho 0.015)) max-y 0.0)
            (max (/ alto 38.0) 0.5))
    )
    (list '(0.0 0.0 0.0) 1.0)
  )
)

;; Calcula la caja envolvente total (lamina real + tabla) para el encuadre exacto del PDF
(defun calcular-ventana-impresion (/ lim-plano ss-tab lim-tab min-x min-y max-x max-y pad-x pad-y)
  (setq lim-plano (obtener-caja-plano-real (obtener-polilineas))
        ss-tab    (ssget "_X" '((8 . "TABLA_LEYENDAS_AREAS") (410 . "Model")))
        lim-tab   (obtener-limites-ss ss-tab))
  (cond
    ((and lim-plano lim-tab)
      (setq min-x (min (nth 0 lim-plano) (nth 0 lim-tab))
            min-y (min (nth 1 lim-plano) (nth 1 lim-tab))
            max-x (max (nth 2 lim-plano) (nth 2 lim-tab))
            max-y (max (nth 3 lim-plano) (nth 3 lim-tab))))
    (lim-plano
      (setq min-x (nth 0 lim-plano)
            min-y (nth 1 lim-plano)
            max-x (nth 2 lim-plano)
            max-y (nth 3 lim-plano)))
    (lim-tab
      (setq min-x (nth 0 lim-tab)
            min-y (nth 1 lim-tab)
            max-x (nth 2 lim-tab)
            max-y (nth 3 lim-tab)))
  )
  (if (and min-x min-y max-x max-y)
    (progn
      (setq pad-x (* 0.02 (max 1.0 (- max-x min-x)))
            pad-y (* 0.02 (max 1.0 (- max-y min-y))))
      (list (list (- min-x pad-x) (- min-y pad-y))
            (list (+ max-x pad-x) (+ max-y pad-y)))
    )
    nil
  )
)

; ================================================================
; TABLA NATIVA INCOLAPSABLE (3 COLUMNAS MINIMALISTAS) + RESPALDO
; ================================================================
(defun dibujar-leyenda-geometrica (pt-ins escala resumen area-total / x0 y0 fila-h col0-w col1-w col2-w total-w
                                                                     y-act txt-h item nombre color area
                                                                     poly-sw h-sw)
  (setq x0     (car pt-ins)
        y0     (cadr pt-ins)
        fila-h (* 1.2 escala)
        col0-w (* 2.5 escala)
        col1-w (* 11.0 escala)
        col2-w (* 5.0 escala)
        total-w (+ col0-w col1-w col2-w)
        txt-h  (* 0.35 escala)
        y-act  y0)

  (asegurar-capa "TABLA_LEYENDAS_AREAS" 7)

  (defun _rect (xa ya xb yb col-aci)
    (entmake
      (list
        '(0 . "LWPOLYLINE")
        '(100 . "AcDbEntity")
        '(8 . "TABLA_LEYENDAS_AREAS")
        (cons 62 col-aci)
        '(100 . "AcDbPolyline")
        '(90 . 4)
        '(70 . 1)
        (cons 10 (list xa ya))
        (cons 10 (list xb ya))
        (cons 10 (list xb yb))
        (cons 10 (list xa yb))
      )
    )
    (entlast)
  )

  (defun _texto (xt yt h str col-aci)
    (entmake
      (list
        '(0 . "TEXT")
        '(8 . "TABLA_LEYENDAS_AREAS")
        (cons 62 col-aci)
        (cons 10 (list xt yt 0.0))
        (cons 40 h)
        (cons 1 str)
      )
    )
  )

  ;; Encabezado Principal
  (_rect x0 (- y-act fila-h) (+ x0 total-w) y-act 7)
  (_texto (+ x0 (* 0.4 escala)) (- y-act (* 0.72 fila-h)) (* txt-h 1.1) "CUADRO DE AREAS Y LEYENDAS (TRANSPARENCIA 70%)" 7)
  (setq y-act (- y-act fila-h))

  ;; Sub-encabezado de 3 columnas (sin columna de % y encabezado simplificado "ZONA")
  (_rect x0 (- y-act fila-h) (+ x0 total-w) y-act 7)
  (_texto (+ x0 (* 0.3 escala)) (- y-act (* 0.7 fila-h)) txt-h "COLOR" 7)
  (_texto (+ x0 col0-w (* 0.3 escala)) (- y-act (* 0.7 fila-h)) txt-h "ZONA" 7)
  (_texto (+ x0 col0-w col1-w (* 0.3 escala)) (- y-act (* 0.7 fila-h)) txt-h "AREA (m2)" 7)
  (setq y-act (- y-act fila-h))

  ;; Filas del catalogo (solo muestra visual de color, sin texto dentro)
  (foreach item resumen
    (setq nombre (car item)
          color  (cadr item)
          area   (caddr item))
    (_rect x0 (- y-act fila-h) (+ x0 total-w) y-act 7)

    (setq poly-sw (_rect (+ x0 (* 0.25 escala))
                         (- y-act (* 0.82 fila-h))
                         (+ x0 col0-w (* -0.25 escala))
                         (- y-act (* 0.18 fila-h))
                         color))
    (if poly-sw
      (progn
        (setq h-sw (crear-hatch-area poly-sw "TABLA_LEYENDAS_AREAS" color))
        (if h-sw (vl-catch-all-apply 'vla-put-Color (list h-sw color)))
      )
    )

    (_texto (+ x0 col0-w (* 0.3 escala)) (- y-act (* 0.7 fila-h)) txt-h nombre 7)
    (_texto (+ x0 col0-w col1-w (* 0.3 escala)) (- y-act (* 0.7 fila-h)) txt-h (strcat (rtos area 2 2) " m2") 7)
    (setq y-act (- y-act fila-h))
  )

  ;; Fila Total General
  (_rect x0 (- y-act fila-h) (+ x0 total-w) y-act 7)
  (_texto (+ x0 (* 0.3 escala)) (- y-act (* 0.7 fila-h)) (* txt-h 1.05) "TOTAL" 7)
  (_texto (+ x0 col0-w (* 0.3 escala)) (- y-act (* 0.7 fila-h)) (* txt-h 1.05) "AREA TOTAL CLASIFICADA" 7)
  (_texto (+ x0 col0-w col1-w (* 0.3 escala)) (- y-act (* 0.7 fila-h)) (* txt-h 1.05) (strcat (rtos area-total 2 2) " m2") 7)
)

(defun crear-tabla-leyendas (pt-ins escala resumen area-total / acad doc mspace num-filas fila-h txt-h
                                                               old-clayer tabla fila item nombre color area accm-col)
  (setq acad (vlax-get-acad-object)
        doc (vla-get-ActiveDocument acad)
        mspace (vla-get-ModelSpace doc)
        num-filas (+ 3 (length resumen))
        fila-h (* 1.2 escala)
        txt-h  (* 0.35 escala)
        old-clayer (getvar "CLAYER"))

  (asegurar-capa "TABLA_LEYENDAS_AREAS" 7)
  (vl-catch-all-apply 'setvar (list "CLAYER" "TABLA_LEYENDAS_AREAS"))

  ;; Crear tabla nativa AcDbTable con 3 columnas (COLOR | ZONA | AREA (m2))
  (if (not (vl-catch-all-error-p
             (setq tabla (vl-catch-all-apply
                           'vla-AddTable
                           (list mspace (vlax-3d-point pt-ins) num-filas 3 fila-h (* 5.0 escala))))))
    (progn
      (vl-catch-all-apply 'vla-put-RegenerateTableSuppressed (list tabla :vlax-true))
      (vl-catch-all-apply 'vla-put-Layer (list tabla "TABLA_LEYENDAS_AREAS"))
      (vl-catch-all-apply 'vla-put-Color (list tabla acByLayer))
      (vl-catch-all-apply 'vla-put-VertCellMargin (list tabla (* 0.15 escala)))
      (vl-catch-all-apply 'vla-put-HorzCellMargin (list tabla (* 0.25 escala)))

      ;; Anchos de columna proporcionales a la escala del plano
      (vl-catch-all-apply 'vla-SetColumnWidth (list tabla 0 (* 2.8 escala)))
      (vl-catch-all-apply 'vla-SetColumnWidth (list tabla 1 (* 11.0 escala)))
      (vl-catch-all-apply 'vla-SetColumnWidth (list tabla 2 (* 5.0 escala)))

      ;; Fila 0: Titulo Principal
      (vl-catch-all-apply 'vla-SetRowHeight (list tabla 0 (* 1.5 escala)))
      (vl-catch-all-apply 'vla-SetText (list tabla 0 0 "CUADRO DE AREAS Y LEYENDAS (TRANSPARENCIA 70%)"))
      (vl-catch-all-apply 'vla-SetCellAlignment (list tabla 0 0 acMiddleCenter))
      (vl-catch-all-apply 'vla-SetCellTextHeight (list tabla 0 0 (* txt-h 1.15)))

      ;; Fila 1: Encabezados (sin columna de % y encabezado simplificado "ZONA")
      (vl-catch-all-apply 'vla-SetRowHeight (list tabla 1 fila-h))
      (vl-catch-all-apply 'vla-SetText (list tabla 1 0 "COLOR"))
      (vl-catch-all-apply 'vla-SetText (list tabla 1 1 "ZONA"))
      (vl-catch-all-apply 'vla-SetText (list tabla 1 2 "AREA (m2)"))
      (foreach col '(0 1 2)
        (vl-catch-all-apply 'vla-SetCellAlignment (list tabla 1 col acMiddleCenter))
        (vl-catch-all-apply 'vla-SetCellTextHeight (list tabla 1 col txt-h))
      )

      ;; Objeto TrueColor nativo de la propia tabla
      (setq accm-col (vl-catch-all-apply 'vla-get-TrueColor (list tabla)))

      ;; Filas de Categorias
      (setq fila 2)
      (foreach item resumen
        (setq nombre (car item)
              color  (cadr item)
              area   (caddr item))

        (vl-catch-all-apply 'vla-SetRowHeight (list tabla fila fila-h))

        ;; Aplicar color ACI al fondo de la celda de la columna 0 sin texto ni nomenclatura
        (if (and accm-col (not (vl-catch-all-error-p accm-col)))
          (progn
            (vl-catch-all-apply 'vla-put-ColorIndex (list accm-col color))
            (vl-catch-all-apply 'vla-SetCellBackgroundColorNone (list tabla fila 0 :vlax-false))
            (vl-catch-all-apply 'vla-SetCellBackgroundColor (list tabla fila 0 accm-col))
          )
        )

        (vl-catch-all-apply 'vla-SetText (list tabla fila 0 ""))
        (vl-catch-all-apply 'vla-SetText (list tabla fila 1 nombre))
        (vl-catch-all-apply 'vla-SetText (list tabla fila 2 (strcat (rtos area 2 2) " m2")))

        (vl-catch-all-apply 'vla-SetCellAlignment (list tabla fila 0 acMiddleCenter))
        (vl-catch-all-apply 'vla-SetCellAlignment (list tabla fila 1 acMiddleLeft))
        (vl-catch-all-apply 'vla-SetCellAlignment (list tabla fila 2 acMiddleRight))

        (foreach col '(0 1 2)
          (vl-catch-all-apply 'vla-SetCellTextHeight (list tabla fila col txt-h))
        )
        (setq fila (1+ fila))
      )

      ;; Ultima Fila: Total General
      (vl-catch-all-apply 'vla-SetRowHeight (list tabla fila (* 1.3 escala)))
      (vl-catch-all-apply 'vla-SetText (list tabla fila 0 "TOTAL"))
      (vl-catch-all-apply 'vla-SetText (list tabla fila 1 "AREA TOTAL CLASIFICADA"))
      (vl-catch-all-apply 'vla-SetText (list tabla fila 2 (strcat (rtos area-total 2 2) " m2")))

      (vl-catch-all-apply 'vla-SetCellAlignment (list tabla fila 0 acMiddleCenter))
      (vl-catch-all-apply 'vla-SetCellAlignment (list tabla fila 1 acMiddleLeft))
      (vl-catch-all-apply 'vla-SetCellAlignment (list tabla fila 2 acMiddleRight))
      (foreach col '(0 1 2)
        (vl-catch-all-apply 'vla-SetCellTextHeight (list tabla fila col txt-h))
      )

      (vl-catch-all-apply 'vla-put-RegenerateTableSuppressed (list tabla :vlax-false))
      (vl-catch-all-apply 'vla-Update (list tabla))
      (if old-clayer (vl-catch-all-apply 'setvar (list "CLAYER" old-clayer)))
      tabla
    )
    (progn
      (if old-clayer (vl-catch-all-apply 'setvar (list "CLAYER" old-clayer)))
      (dibujar-leyenda-geometrica pt-ins escala resumen area-total)
    )
  )
)

; ================================================================
; PROCESAMIENTO CENTRAL Y ACTUALIZACION AUTOMATICA
; ================================================================
(defun procesar-plano-y-tabla (/ acad doc ss-polys ss-textos i ent info nombre color area
                                 acumulados par-actual nuevo-par area-total
                                 claves-vistas pt-c clave-dedup datos-caja pt-tabla escala
                                 cont-zonas)
  (setq acad (vlax-get-acad-object)
        doc  (vla-get-ActiveDocument acad))

  (limpiar-elementos-previos)

  (setq acumulados nil)
  (foreach cat (obtener-catalogo)
    (setq acumulados (append acumulados (list (list (car cat) (cadr cat) 0.0))))
  )

  (setq ss-polys      (obtener-polilineas)
        ss-textos     (obtener-textos)
        claves-vistas nil
        area-total    0.0
        cont-zonas    0)

  (if ss-polys
    (progn
      (setq i 0)
      (while (< i (sslength ss-polys))
        (setq ent (ssname ss-polys i))
        (if (verificar-y-cerrar-polilinea ent)
          (if (setq info (detectar-zona ent ss-textos))
            (progn
              (setq nombre (car info)
                    color  (cadr info)
                    area   (obtener-area-m2 ent)
                    pt-c   (centro-objeto (vlax-ename->vla-object ent)))
              (setq clave-dedup (strcat nombre "_"
                                        (rtos (car pt-c) 2 2) "_"
                                        (rtos (cadr pt-c) 2 2) "_"
                                        (rtos area 2 2)))
              (if (not (member clave-dedup claves-vistas))
                (progn
                  (setq claves-vistas (cons clave-dedup claves-vistas))
                  (crear-hatch-area ent nombre color)
                  (setq par-actual (assoc nombre acumulados))
                  (if (not par-actual)
                    (progn
                      (registrar-zona-en-catalogo nombre color)
                      (setq acumulados (append acumulados (list (list nombre color area))))
                    )
                    (progn
                      (setq nuevo-par (list nombre color (+ (caddr par-actual) area)))
                      (setq acumulados (subst nuevo-par par-actual acumulados))
                    )
                  )
                  (setq area-total (+ area-total area)
                        cont-zonas (1+ cont-zonas))
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )

  (setq datos-caja (calcular-caja-dibujo ss-polys)
        pt-tabla   (car datos-caja)
        escala     (cadr datos-caja))

  (crear-tabla-leyendas pt-tabla escala acumulados area-total)
  (vl-catch-all-apply 'vla-ZoomExtents (list acad))

  (list cont-zonas area-total)
)

; ================================================================
; INCORPORACION DINAMICA DE NUEVAS ZONAS (CONFIRMACION CON ENTER)
; ================================================================
(defun mostrar-catalogo-consola (/ idx cat)
  (setq cat (obtener-catalogo)
        idx 1)
  (princ "\n--- Zonas registradas ---")
  (foreach item cat
    (princ (strcat "\n  [" (itoa idx) "] " (car item) " (Color ACI " (itoa (cadr item)) ")"))
    (setq idx (1+ idx))
  )
  (princ "\n-------------------------")
)

(defun resolver-entrada-zona (entrada / limpia num cat)
  (setq limpia (vl-string-trim " " (if entrada entrada ""))
        cat    (obtener-catalogo))
  (cond
    ((= limpia "") nil)
    ((and (numberp (setq num (read limpia)))
          (>= num 1)
          (<= num (length cat)))
      (nth (1- num) cat))
    (T (clasificar-nombre limpia))
  )
)

(defun solicitar-color-aci-valido (/ col-in col-num listo)
  (setq listo nil col-num nil)
  (while (not listo)
    (setq col-in (getstring "\nIngrese codigo de color ACI para la nueva zona (1-255): "))
    (setq col-in (vl-string-trim " " (if col-in col-in "")))
    (cond
      ((= col-in "")
        (princ "\n[VALIDACION] El codigo de color no puede estar vacio."))
      ((and (numberp (setq col-num (read col-in)))
            (= (type col-num) 'INT)
            (>= col-num 1)
            (<= col-num 255))
        (setq listo T))
      (T
        (princ "\n[VALIDACION] Ingrese un numero entero valido entre 1 y 255."))
    )
  )
  col-num
)

(defun solicitar-zona-para-polilinea (ent ss-textos / auto-info entrada zona-sel nombre-limpio resp-nueva color-nuevo listo)
  (setq auto-info (detectar-zona ent ss-textos)
        zona-sel  nil
        listo     nil)
  (mostrar-catalogo-consola)
  (while (not listo)
    (if auto-info
      (setq entrada (getstring T (strcat "\nZona detectada: [" (car auto-info) "] (Enter para confirmar, o ingrese numero/nombre): ")))
      (setq entrada (getstring T "\nIngrese numero o nombre de zona (Enter para confirmar): "))
    )
    (setq nombre-limpio (vl-string-trim " " (if entrada entrada "")))
    (cond
      ((and (= nombre-limpio "") auto-info)
        (setq zona-sel auto-info
              listo    T))
      ((= nombre-limpio "")
        (princ "\n[VALIDACION] Entrada vacia. Ingrese un numero o nombre de zona valido."))
      ((setq zona-sel (resolver-entrada-zona nombre-limpio))
        (setq listo T))
      (T
        (princ (strcat "\n[VALIDACION] Zona no reconocida: \"" nombre-limpio "\"."))
        (initget "Si No")
        (setq resp-nueva (getkword "\n¿Desea registrarla dinamicamente como nueva zona en el catalogo? [Si/No] <No>: "))
        (if (= resp-nueva "Si")
          (progn
            (setq color-nuevo (solicitar-color-aci-valido))
            (setq zona-sel (registrar-zona-en-catalogo nombre-limpio color-nuevo))
            (setq listo T)
          )
          (princ "\nIntente nuevamente con un numero o nombre de zona valido.")
        )
      )
    )
  )
  zona-sel
)

(defun agregar-zonas-interactivo (/ ss-sel ss-textos i ent obj-ent zona-info nombre color
                                     continuar resp-cont agregadas res-proc)
  (setq continuar T
        agregadas 0)
  (while continuar
    (princ "\n[AGREGAR ZONA] Seleccione las polilineas de la nueva area y presione Enter (Esc para terminar): ")
    (setq ss-sel (ssget '((0 . "LWPOLYLINE,POLYLINE") (410 . "Model") (-4 . "<NOT") (8 . "TABLA_LEYENDAS_AREAS") (-4 . "NOT>"))))
    (if (and ss-sel (> (sslength ss-sel) 0))
      (progn
        (setq ss-textos (obtener-textos)
              i 0)
        (while (< i (sslength ss-sel))
          (setq ent (ssname ss-sel i))
          (if (verificar-y-cerrar-polilinea ent)
            (progn
              (setq zona-info (solicitar-zona-para-polilinea ent ss-textos))
              (if zona-info
                (progn
                  (setq nombre (car zona-info)
                        color  (cadr zona-info))
                  (asegurar-capa nombre color)
                  (setq obj-ent (vlax-ename->vla-object ent))
                  (vl-catch-all-apply 'vla-put-Layer (list obj-ent nombre))
                  (setq agregadas (1+ agregadas))
                  (princ (strcat "\n -> Area asignada a \"" nombre "\" (Color ACI " (itoa color) ")."))
                )
              )
            )
            (princ "\n[VALIDACION] La polilinea seleccionada esta abierta y no pudo cerrarse automaticamente.")
          )
          (setq i (1+ i))
        )
        (setq res-proc (procesar-plano-y-tabla))
        (princ (strcat "\n[ACTUALIZACION] Tabla y visualizacion actualizadas. Zonas totales: "
                       (itoa (car res-proc)) " | Area total: " (rtos (cadr res-proc) 2 2) " m2"))
        (initget "Si No")
        (setq resp-cont (getkword "\n¿Desea agregar otra zona/area? [Si/No] <No>: "))
        (if (/= resp-cont "Si")
          (setq continuar nil)
        )
      )
      (progn
        (princ "\n[AVISO] No se seleccionaron polilineas validas.")
        (setq continuar nil)
      )
    )
  )
  agregadas
)

; ================================================================
; EXPORTACION AUTOMATICA A PDF (PARAMETROS DE REFERENCIA A1)
; ================================================================
(defun buscar-media-iso-a1 (layout / media-list encontrado)
  (setq encontrado "ISO_expand_A1_(594.00_x_841.00_MM)")
  (if (not (vl-catch-all-error-p
             (setq media-list (vl-catch-all-apply 'vla-GetCanonicalMediaNames (list layout)))))
    (foreach m (vlax-safearray->list (vlax-variant-value media-list))
      (if (wcmatch (strcase m) "*ISO*EXPAND*A1*")
        (setq encontrado m)
      )
    )
  )
  encontrado
)

(defun obtener-nombre-local-media (layout canonical / local-name)
  (setq local-name (vl-catch-all-apply 'vla-GetLocaleMediaName (list layout canonical)))
  (if (or (vl-catch-all-error-p local-name) (not local-name) (= local-name ""))
    "ISO expand A1 (594.00 x 841.00 MM)"
    local-name
  )
)

(defun obtener-ruta-pdf (/ dir base)
  (setq dir  (if (= (getvar "DWGTITLED") 1)
               (getvar "DWGPREFIX")
               (getvar "MYDOCUMENTSPREFIX"))
        base (if (= (getvar "DWGTITLED") 1)
               (vl-filename-base (getvar "DWGNAME"))
               "Plano_Areas_Zonas"))
  (if (not (member (substr dir (strlen dir)) '("\\" "/")))
    (setq dir (strcat dir "\\"))
  )
  (strcat dir base "_AREAS.pdf")
)

(defun exportar-pdf-plano (/ acad doc layout ventana pmin pmax media-a1 media-local
                             pdf-path old-bg old-tpo old-fd exito)
  (setq acad     (vlax-get-acad-object)
        doc      (vla-get-ActiveDocument acad)
        layout   (vla-get-ActiveLayout doc)
        ventana  (calcular-ventana-impresion)
        pdf-path (obtener-ruta-pdf)
        exito    nil)

  (if (not ventana)
    (progn
      (princ "\n[PDF ERROR] No hay geometria visible en Model Space para exportar.")
      nil
    )
    (progn
      (setq old-bg  (getvar "BACKGROUNDPLOT")
            old-tpo (getvar "PLOTTRANSPARENCYOVERRIDE")
            old-fd  (getvar "FILEDIA"))
      (vl-catch-all-apply 'setvar (list "BACKGROUNDPLOT" 0))
      (vl-catch-all-apply 'setvar (list "PLOTTRANSPARENCYOVERRIDE" 2))
      (vl-catch-all-apply 'setvar (list "FILEDIA" 0))

      ;; Asegurar regeneracion completa antes de trazar
      (vl-catch-all-apply 'vla-Regen (list doc acAllViewports))

      ;; Eliminar PDF previo si existe para evitar bloqueo de sobrescritura
      (if (findfile pdf-path)
        (vl-catch-all-apply 'vl-file-delete (list pdf-path))
      )

      ;; Detectar nombre exacto de papel ISO expand A1 (594.00 x 841.00 MM)
      (vl-catch-all-apply 'vla-put-ConfigName (list layout "DWG To PDF.pc3"))
      (vl-catch-all-apply 'vla-RefreshPlotDeviceInfo (list layout))
      (setq media-a1    (buscar-media-iso-a1 layout)
            media-local (obtener-nombre-local-media layout media-a1)
            pmin        (car ventana)
            pmax        (cadr ventana))

      ;; Ejecutar _.-PLOT en coordenadas WCS para garantizar plano completo + tabla centrados y ajustados
      (vl-catch-all-apply
        '(lambda ()
           (command "_.-PLOT"
                    "_Y"
                    "Model"
                    "DWG To PDF.pc3"
                    media-local
                    "_Millimeters"
                    "_Landscape"
                    "_N"
                    "_Window"
                    pmin
                    pmax
                    "_Fit"
                    "_Center"
                    "_Y"
                    "monochrome.ctb"
                    "_N"
                    "_As"
                    pdf-path
                    "_N"
                    "_Y")
         )
      )

      (if (findfile pdf-path)
        (setq exito T)
      )

      (if old-bg (vl-catch-all-apply 'setvar (list "BACKGROUNDPLOT" old-bg)))
      (if old-tpo (vl-catch-all-apply 'setvar (list "PLOTTRANSPARENCYOVERRIDE" old-tpo)))
      (if old-fd (vl-catch-all-apply 'setvar (list "FILEDIA" old-fd)))

      (if exito
        (progn
          (princ (strcat "\n[PDF GENERADO] Archivo exportado correctamente en:\n  " pdf-path))
          pdf-path
        )
        (progn
          (princ (strcat "\n[PDF ERROR] No se pudo crear el PDF en: " pdf-path
                         "\nVerifique que exista la impresora 'DWG To PDF.pc3', el estilo 'monochrome.ctb' y permisos de escritura en la carpeta."))
          nil
        )
      )
    )
  )
)

; ================================================================
; CARGA DE ZONAS EXTRA DESDE EL PORTAL WEB (SI EXISTE extra_zones.txt)
; ================================================================
(defun cargar-zonas-web (ruta-txt / fp linea pos nom col-str col-num)
  (if (and ruta-txt (findfile ruta-txt))
    (progn
      (setq fp (open ruta-txt "r"))
      (while (setq linea (read-line fp))
        (setq linea (vl-string-trim " \t\r\n" linea))
        (if (and (/= linea "") (setq pos (vl-string-search "|" linea)))
          (progn
            (setq nom     (vl-string-trim " " (substr linea 1 pos))
                  col-str (vl-string-trim " " (substr linea (+ pos 2)))
                  col-num (read col-str))
            (if (and (/= nom "") (numberp col-num) (= (type col-num) 'INT) (>= col-num 1) (<= col-num 255))
              (registrar-zona-en-catalogo nom col-num)
            )
          )
        )
      )
      (close fp)
    )
  )
)

; ================================================================
; COMANDO BATCH PARA ACCORECONSOLE (SIN PAUSAS INTERACTIVAS)
; ================================================================
(defun c:ROBOTAREAS_BATCH (/ acad doc res-proc cont-zonas area-total old-cmdecho ruta-extra)
  (setq old-cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (vl-catch-all-apply 'setvar (list "TRANSPARENCYDISPLAY" 1))

  (setq acad (vlax-get-acad-object)
        doc  (vla-get-ActiveDocument acad)
        ruta-extra (strcat (getvar "DWGPREFIX") "extra_zones.txt"))

  (cargar-zonas-web ruta-extra)

  (setq res-proc   (procesar-plano-y-tabla)
        cont-zonas (car res-proc)
        area-total (cadr res-proc))

  (princ (strcat "\n[ROBOTAREAS_BATCH] Zonas procesadas: " (itoa cont-zonas)
                 " | Area total: " (rtos area-total 2 2) " m2\n"))

  (exportar-pdf-plano)
  (vl-catch-all-apply 'vla-Save (list doc))

  (setvar "CMDECHO" old-cmdecho)
  (princ)
)
