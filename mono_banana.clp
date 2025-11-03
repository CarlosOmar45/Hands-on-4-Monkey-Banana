(deftemplate monkey-at (slot pos))
(deftemplate box-at    (slot pos))
(deftemplate banana-at (slot pos))
(deftemplate on-box    (slot value))
(deftemplate holding   (slot what))

(deffacts estado-inicial
  (monkey-at (pos ventana))
  (box-at    (pos puerta))
  (banana-at (pos centro))
  (on-box    (value no))
)

(defrule mover-de-ventana-a-puerta
  ?m <- (monkey-at (pos ?mp))
  ?bx <- (box-at (pos ?bp))
  (test (not (eq ?mp ?bp)))
  =>
  (printout t "Accion: el mono se mueve de " ?mp " a " ?bp "." crlf)
  (retract ?m)
  (assert (monkey-at (pos ?bp)))
)

(defrule empujar-caja-a-centro
  ?m   <- (monkey-at (pos ?p))
  ?box <- (box-at (pos ?p))
  ?ban <- (banana-at (pos ?bpos))
  (test (not (eq ?p ?bpos)))
  =>
  (printout t "Accion: el mono empuja la caja de " ?p " a " ?bpos "." crlf)
  (retract ?box ?m)
  (assert (box-at (pos ?bpos)))
  (assert (monkey-at (pos ?bpos)))
)

(defrule subirse-a-caja
  ?m <- (monkey-at (pos ?p))
  ?b <- (box-at (pos ?p))
  ?o <- (on-box (value no))
  (not (holding (what banana)))
  =>
  (printout t "Accion: el mono se sube a la caja en " ?p "." crlf)
  (retract ?o)
  (assert (on-box (value yes)))
)

(defrule agarrar-banana
  (on-box (value yes))
  ?b   <- (box-at (pos ?p))
  ?ban <- (banana-at (pos ?p))
  (not (holding (what banana)))
  =>
  (printout t "Accion: el mono agarra la banana." crlf)
  (retract ?ban)
  (assert (holding (what banana)))
)

(defrule bajarse-con-banana
  (holding (what banana))
  ?o <- (on-box (value yes))
  =>
  (printout t "Accion: el mono se baja de la caja con la banana." crlf)
  (retract ?o)
  (assert (on-box (value no)))
)

(defrule estado-final
  (holding (what banana))
  =>
  (printout t "Estado final: el mono tiene la banana, ahora puede comerla." crlf)
)
