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
?*iterator2* = 0 
?*iterator3* = 0 
?*iterator4* = 0 
?*iterator5* = 0 
?*iterator6* = 0
)

(defrule menu
	?a <- (menu)
	=>
	(clear-window)
	(printout t " 1 ► Statistici note" crlf)
	(printout t " 2 ► Gasiti o anumita secventa" crlf)
	(printout t " 3 ► Durate " crlf)
	(printout t " 4 ► Apartenenta gen " crlf)
	(printout t " 5 ► Exit" crlf)

	(printout t crlf crlf " Optiune → ")
	(assert(command (read)))
	(printout t crlf)
	(retract ?a)
)
(defrule backToMenu
	?a <- (backToMenu)
	=>
	(printout t crlf crlf " 0 ► Back to menu "crlf " ")
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
	(printout t "   Introdu nota [0-127] → ")
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

(defrule executeCommand2
	?x <- (command 2)
	=>
	(clear-window)
	(printout t " ♦ Introduceti numarul de note urmate de informatiile fiecarei note pe cate un rand" crlf  "Numar note → ")
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
	(printout t " Tip → ")
	(bind ?*iterator1* (read))
	(printout t " Data[0] → ")
	(bind ?*iterator2* (read))	
	(printout t " Data[1] → ")
	(bind ?*iterator3* (read))	
	(printout t " Data[2] → ")
	(bind ?*iterator4* (read))
	(assert ( user_event (order (- ?n2 ?n1)) (tip ?*iterator1*) (set nil) (data ?*iterator2* ?*iterator3* ?*iterator4*) ))
	(retract ?nr)
	(assert (copy_nr_note (- ?n1 1)))
	(assert (state 1))
)
(defrule matchFirst
	(state ?s&:(eq ?s 1))
	?f <- (user_event (order ?o1&:(eq ?o1 0)) (tip ?t1) (set nil) (data ?x ?y ?z))
	(event (order ?o2) (trackID ?t2) (tip ?t)  (delta ?) (data ?x ?y ?z))
	=>
	(printout t "match " ?t2 ", " ?o2 crlf)
	(retract ?f)
	(assert (user_event (order ?o1) (tip ?t1) (set ?t2 ?o2) (data ?x ?y ?z)) )
)
(defrule findSequence
	(nr_note ?n)
	(user_event (order ?o1) (tip ?t1) (set ?t2 ?o2) (data ?x ?y ?z))
	(not (exists (user_event (order ?o3&:(<> ?o3 0)) (tip ?t3) (data ?x1 ?y1 ?z1))
				(event (order ?o4&:(eq ?o4 (+ ?o2 ?o3))) (trackID ?t2) (tip ?t4) (data ?x2 ?y2 ?z2&:(or (neq ?t3 ?t4) (<> ?x1 ?x2) (<> ?y1 ?y2) (<> ?z1 ?z2))  ))))
	=>
	(printout t " it exists " ?t2 ", " ?o2 crlf)
	(assert (idx 0))
)
(defrule printSequence
	?aux <- (idx ?i&:(<> ?i 5))
	(nr_note ?n)
	(user_event (order ?o1&:(eq 0 ?o1))  (set ?t2 ?o2) )
	(event (order ?o3&:(eq ?o3 (+ ?o2 ?n ?i))) (trackID ?t2) (data ?x ?y ?z))
	=>
	(printout t " nota " ?x ", " ?y ", " ?z crlf)
	(retract ?aux)
	(assert (idx (+ ?i 1)))
)
(defrule resetElement
	(nr_note ?n)
	?u <- (user_event (order ?o1) (tip ?t1) (set ?t2 ?o2) (data ?x ?y ?z))
	(exists (user_event (order ?o3&:(<> ?o3 0)) (tip ?t3) (data ?x1 ?y1 ?z1))
			(event (order ?o4&:(eq ?o4 (+ ?o2 ?o3))) (trackID ?t2) (tip ?t4) (data ?x2 ?y2 ?z2&:(or (neq ?t3 ?t4) (<> ?x1 ?x2) (<> ?y1 ?y2) (<> ?z1 ?z2))  )))
	=>
	(printout t " reset " crlf)	
	(retract ?u)
	(assert (user_event (order ?o1) (set nil) (tip ?t1) (data ?x ?y ?z)) )
)
(defrule command2ReturnToMenu
	(declare (salience -50))
	?a <- (nr_note ?n)
	=>
	(retract ?a)
	(assert (backToMenu))
)
(defrule executeCommand5
	?a <- (command 5)
	=>
	(clear-window)
)

(defrule executeCommand3
	?x <- (command 3)
	=>
	(clear-window)
	(printout t " ♦ Durate" crlf)
	(printout t "   1 ► Durata unei note" crlf)
	(printout t "   2 ► Durata medie a melodiei " crlf)
	(printout t crlf "   Optiune → ")
	(assert(command3 (read)))
	(retract ?x)
) 
(defrule executeComand3_1
	?a <- (command3 1)
	=>
	(printout t "   TrackID → ")
	(bind ?*iterator2* (read))	
	(printout t "   Order   → ")
	(bind ?*iterator1* (read))
	(assert (durataMedieAUneiNote ?*iterator1* ?*iterator2*))
	(retract ?a)
)
(deffunction computeDutarion
	(?trackID ?order ?channel ?note ?velocity)
	(bind ?*iterator1* 0) ; tip
	(bind ?*iterator2* 0) ; channel
	(bind ?*iterator3* 0) ; nota
	(bind ?*iterator4* 0) ;velocity
	(bind ?*iterator5* 0) ;total delta
	(bind ?order (+ ?order 1))

	; (while  ( and   (test(not(eq ?*iterator1* "NotaOff")));true
					; (<> ?*iterator2* ?channel);true
					; (<> ?*iterator3* ?note); true
					; (<> ?*iterator4* ?velocity)  ;true
					; (exists (event (order ?order) (trackID ?trackID) (delta ?d) (tip ?tip) (data ?d1 ?d2 ?d3))) 
				 
			; )
				; (bind ?*iterator1* ?tip)
		; (bind ?*iterator2* ?d1)
		; (bind ?*iterator3* ?d2)
		; (bind ?*iterator4* ?d3)
		; (bind ?*iterator5* (+ ?*iterator5* ?d))
		; (bind ?order (+ ?order 1))
	; )
	; (if  (event (order ?order) (trackID ?trackID) (delta ?d) (tip ?tip) (data ?d1 ?d2 ?d3))
	; then (printout t "alfa"))

		; (if ( < ?order 10) then
		; (bind ?*iterator1*  1))
	; (while (= ?*iterator1* 1) 
		; (printout t ?order crlf)
		; (if ( < ?order 10) then
		; (bind ?*iterator1* 1 )
		; else (bind ?*iterator1* 0 ))
		; (bind ?order (+ ?order 1))
	; )
	

		; (bind ?*iterator1* ?tip)
		; (bind ?*iterator2* ?d1)
		; (bind ?*iterator3* ?d2)
		; (bind ?*iterator4* ?d3)
		; (bind ?*iterator5* (+ ?*iterator5* ?d)
		; (bind ?order (+ ?order 1))
	; )
	; (return ?*iterator5*)
)

(defrule durataMedieAUneiNote
	?a <- (durataMedieAUneiNote ?order ?trackID)
	(event (order ?order) (trackID ?trackID) (delta ?delta) (tip "NoteOn") (data ?ch ?nt ?vl))
	=>
	(assert (durMed ?order ?trackID ?ch ?nt ?vl))
	(bind ?*iterator1* 0)
	(bind ?*iterator2* 0)
)
(defrule durataMedieAUneiNote2
	?a <- (durMed ?order ?trackID ?d1 ?d2 ?d3)
	(event (order ?order2) (trackID ?trackID) (delta ?delta2) (tip ?tip) (data ?ch ?nt ?))
	(test (eq ?order2 (+ ?order ?*iterator1*)))
	=>
	(if (= ?*iterator1* 0) then
		(bind ?*iterator1* 1)
		(retract ?a)
		(assert (durMed ?order ?trackID ?d1 ?d2 ?d3))
		else
		(bind ?*iterator1* (+ ?*iterator1* 1))
		(bind ?*iterator2* (+ ?*iterator2* ?delta2 ))
		(if 
		(and (eq ?tip "NoteOff") 
				(eq ?d1 ?ch)
				 (eq ?d2 ?nt))
			
			then
			(retract ?a)
			(printout t  "   Durata  → " ?*iterator2* crlf)
			(assert (backToMenu))
			else
				(retract ?a)
				(assert (durMed ?order ?trackID ?d1 ?d2 ?d3))
		)
	)
)

(defrule executeComand3_2
	?a <- (command3 2)
	=>
	(assert (durataMedieAMelodiei))
	(retract ?a)
	(bind ?*iterator3* 0)
	(bind ?*iterator4* 0)
)
(defrule durataMedieAMelodiei
	?a <-(durataMedieAMelodiei)
	(event (order ?order) (trackID ?trackID) (delta ?delta) (tip "NoteOn") (data ?ch ?nt ?vel))
	=>
	(assert (durataMedieNota ?order ?trackID ?ch ?nt))
	(bind ?*iterator1* 0)
	(bind ?*iterator2* 0)
	(bind ?*iterator4* (+ ?*iterator4* 1))
)
(defrule durataMedieMelodiei2
	?a <- (durataMedieNota ?order ?trackID ?d1 ?d2)
	(event (order ?order2) (trackID ?trackID) (delta ?delta2) (tip ?tip) (data ?ch ?nt ?))
	(test (eq ?order2 (+ ?order ?*iterator1*)))
	=>
	(if (= ?*iterator1* 0) then
		(bind ?*iterator1* 1)
		(retract ?a)
		(assert (durataMedieNota ?order ?trackID ?d1 ?d2 ))
		else
		(bind ?*iterator1* (+ ?*iterator1* 1))
		
		(bind ?*iterator2* (+ ?*iterator2* ?delta2 ))
		(if 
		(and (eq ?tip "NoteOff") 
				(eq ?d1 ?ch)
				 (eq ?d2 ?nt))
			
			then
			(retract ?a)
			;(printout t  ?order"   Durata  → " ?*iterator2* crlf)
			(bind ?*iterator3* (+ ?*iterator3* ?*iterator2*))
			else
				(retract ?a)
				(assert (durataMedieNota ?order ?trackID ?d1 ?d2 ))
		)
	)
)
(defrule durataMedieMelodiei3
	(declare (salience -100))
	?a <-(durataMedieAMelodiei)
	=>
	(printout t "   Note in total → " ?*iterator4* crlf)
	(printout t "   Total durata  → " ?*iterator3* " units" crlf)
	(bind ?*iterator6* (/ ?*iterator3* ?*iterator4* ))
	(printout t "   Medie durata  → " ?*iterator6* " units" crlf)
	(retract ?a)
	(assert (backToMenu))

)
(defrule executeCommand4
	?x <- (command 4)
	=>
	(assert (apartenentaGen))
)
(defrule apartenentaGen
	?a <- (apartenentaGen)
	=>
	(retract ?a)
	(if
		(= ?*iterator6* 0)
			then
			(printout t "Ruleaza media duratelor intai" crlf)
			else
			(if
				(and (< 60 ?*iterator6*) (< ?*iterator6* 90))
				then
				(printout "Dub" crlf)
				else
				(if
					(and (< 115 ?*iterator6*) (< ?*iterator6* 130))
					then
					(printout "House" crlf)
					else
					(if
						(and (< 200 ?*iterator6*) (< ?*iterator6* 240))
						then
						(printout  t "Jazz" crlf)
					
					
					)
			
			
				)
			
			
			)
	
	
	
	)
)


(defrule executeCommandNone
	?a <- (command ?alfa&~1&~2&~3&~4)
	
	=>
	(printout t " Optiune invalida" crlf)
	(assert(returnToMenu 0))
	(retract ?a)
)