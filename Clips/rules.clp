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
	(printout t crlf crlf " 0 ► Back to menu "crlf)
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

(defrule executeCommand1_2
	?x <- (command1 2)
	=>
	(assert (notaSpecifica))
	(bind ?*iterator1* 0)
	(retract ?x)
)
(defrule notaSpecifica
	?x <- (notaSpecifica)
	=>
	(retract ?x)
	(printout t "   Introdu nota → ")
	(assert(notaSpecifica_1 (read)))
)
(defrule notaSpecifica_1
	?x <- (notaSpecifica_1 ?nota)
	(event (order ?) (trackID ?) (delta ?) (tip "NoteOn") (data $?data&:(= (nth$ 2 $?data) ?nota)))
	
	=>
	(bind ?*iterator1* (+ ?*iterator1* 1))
)
(defrule printNotaSpecifica

	?x <-(notaSpecifica_1 ?nota)
	=>
	(retract ?x)
	(printout t "   Numar note: " ?*iterator1* crlf)
	(bind ?*iterator1* 0)
	(assert (backToMenu))

)
(defrule executeCommand1_3
	?x <- (command1 3)
	=>
	(assert (notaTip))
	(bind ?*iterator1* 0)
	(retract ?x)
)
(deffunction noteOfType
	(?type)
	(switch (str-cat ?type)
		(case "A" then 21)
		(case "A#" then 22)
		(case "B" then 23)
		(case "C" then 24)
		(case "C#" then 25)
		(case "D" then 26)
		(case "D#" then 27)
		(case "E" then 28)
		(case "F" then 29)
		(case "F#" then 30)
		(case "G" then 31)
		(case "G#" then 32)
	)
)
(defrule notaTip
	?x <- (notaTip)
	=>
	(retract ?x)
	(printout t "   Introdu tipul de nota (ex C,C#,D etc) → ")
	(bind ?*iterator1*  (noteOfType (read)))
	(assert (notaTip_1 ?*iterator1*))
)

(deffunction isNoteOfType
	(?type ?nota)
	(bind ?*iterator1* ?type)
	(while (> ?nota ?type)
		(bind ?nota (- ?nota 12))
	)
	(if (= ?nota ?type) then
		(return TRUE)
		else
		(return FALSE)
	)
)


(defrule notaTip_1
	?x <- (notaTip_1 ?tip)
	(event (order ?) (trackID ?) (delta ?) (tip "NoteOn") (data $?data))
	(test (isNoteOfType ?tip (nth$ 2 $?data)))
	
	=>
	(bind ?*iterator1* (+ ?*iterator1* 1))
)
(defrule printNotaTip

	?x <-(notaTip_1 ?tip)
	=>
	(retract ?x)
	(printout t "   Numar note: " ?*iterator1* crlf)
	(bind ?*iterator1* 0)
	(assert (backToMenu))

)

