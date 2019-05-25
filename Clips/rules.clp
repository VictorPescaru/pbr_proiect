
(deftemplate user_event
    (slot order (type INTEGER))
    (slot tip (type STRING))
	(multislot set) 
    (multislot data)
)

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
	(printout t " 2 ► Gasiti o anumite secventa" crlf)
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

(defrule executeCommand1
	?x <- (command 2)
	=>
	(clear-window)
	(printout t " ♦ Introduceti numarul de note urmate de informatiile fiecarei note pe cate un rand" crlf)
	(assert(nr_note (read)))
	(retract ?x)
)

(defrule copyNrNote
	(nr_note ?n)
	=>
	(assert (copy_nr_note ?n))
)

(defrule readNotes
	?nr <- (copy_nr_note ?n1&:(<> ?n1 0))
	(nr_note ?n2)
	=>
	(assert ( user_event (order (- ?n2 ?n1)) (tip (read)) (set nil) (data (read) (read) (read)) ))
	(retract ?nr)
	(assert (copy_nr_note (- ?n1 1)))
)

(defrule matchFirst
	?f <- (user_event (order ?o1&:(eq ?o1 0)) (tip ?t1) (set nil) (data ?x ?y ?z))
	(event (order ?o2) (trackID ?t2) (tip ?t)  (delta ?) (data ?x ?y ?z))
	=>
	(retract ?f)
	(assert (user_event (order ?o2) (tip ?t1) (set ?t2 ?o1) (data ?x ?y ?z)) )
)

(defrule findSequence
	(nr_note ?n)
	(user_event (order ?o1) (tip ?t1) (set ?t2 ?o2) (data ?x ?y ?z))
	(not (exists (user_event (order ?o3&:(<> ?o3 0)) (tip ?t3) (data ?x1 ?y1 ?z1))
				(event (order ?o4&:(eq ?o4 (+ ?o2 ?o3))) (trackID ?t2) (tip ?t4) (data ?x2 ?y2 ?z2&:(or (neq ?t3 ?t4) (<> ?x1 ?x2) (<> ?y1 ?y2) (<> ?z1 ?z2))  ))))
	=>
	(printout t " ♦ it exists " ?t2 ", " ?o2 crlf)
)

(defrule resetElement
	(nr_note ?n)
	?u <- (user_event (order ?o1) (tip ?t1) (set ?t2 ?o2) (data ?x ?y ?z))
	(exists (user_event (order ?o3&:(<> ?o3 0)) (tip ?t3) (data ?x1 ?y1 ?z1))
			(event (order ?o4&:(eq ?o4 (+ ?o2 ?o3))) (trackID ?t2) (tip ?t4) (data ?x2 ?y2 ?z2&:(or (neq ?t3 ?t4) (<> ?x1 ?x2) (<> ?y1 ?y2) (<> ?z1 ?z2))  )))
	=>
	(retract ?u)
	(assert (user_event (order ?o1) (tip ?t1) (data ?x ?y ?z)) )
)