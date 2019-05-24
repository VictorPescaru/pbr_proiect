(deffacts initialFact
	(menu)
)
(defglobal
?*iterator1* = 0

)

(defrule menu
	?a <- (menu)
	=>
	(clear-window)
	(printout t " 1 ► Statistici note" crlf)
	(printout t " 2 ► " crlf)
	(printout t " 3 ► " crlf)
	(printout t " 4 ► " crlf)
	(printout t " 5 ► " crlf)
	(printout t " 6 ► " crlf)
	(printout t " 7 ► " crlf)
	(printout t " 8 ► " crlf)
	(printout t " 9 ► Clear Screen" crlf crlf" Optiune → ")
	(assert(command (read)))
	(printout t crlf)
	(retract ?a)
)
(defrule backToMenu
	?a <- (backToMenu)
	=>
	(printout t crlf crlf "Pentru reintoarcere la meniu apasati 0"crlf)
	(assert (returnToMenu (read)))
	(retract ?a)
)
(defrule returnToMenu
	?a <- (returnToMenu 0)
	=>
	(retract ?a)
	(assert (menu))
)


(defrule executeCommand1
	?x <- (command 1)
	=>
	(clear-window)
	(printout t " ♦ Statistici note" crlf)
	(printout t "   1 ► Total note apasate " crlf)
	(printout t "   2 ► Nota specifica " crlf)
	(printout t "   3 ► Note de tip" crlf)
	(printout t crlf "   Optiune → ")
	(assert(command1 (read)))
	(retract ?x)
)
(defrule executeCommand1_1
	?x <- (command1 1)
	=>
	(assert (totalNoteApasate))
	(bind ?*iterator1* 0)
	(retract ?x)
)
(defrule totalNoteApasate
	(totalNoteApasate)
	(event (order ?) (trackID ?) (delta ?) (tip "NoteOn") (data ?channel ?note ?velocity))
	(test (< 0 ?velocity))
	=>
	(bind ?*iterator1* (+ ?*iterator1* 1))
)
(defrule printTotalNoteApasate
	?a <- (totalNoteApasate)
	=>
	(retract ?a)
	(printout t "   Total note apasate: " ?*iterator1* crlf)
	(bind ?*iterator1* 0)
	(assert (backToMenu))

)

;(defrule executeCommand1_2
;
;)