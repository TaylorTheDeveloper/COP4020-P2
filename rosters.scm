; Taylor Brockhoeft
; fsuid: tjb12
; Scheme Grade Roster
; COP 4020 Programming Languages
; April 15, 2015

(define smallest_id
	(lambda (lst min)
	 (cond ((null? lst) min) ; null, return the curent smallest id
	   ; (display "1")
	   ((< (string->number(car(car lst))) (string->number(car min)))
	   ; (display "2")
	   (smallest_id (cdr lst) (car lst) ))
	   ; (display "2")
	(else (smallest_id (cdr lst) min))
	)
    )
) 

(define remove_id ;just remove this element, rather than whole student
	(lambda (lst id)
		(cond (( null? lst) '() )	
		    ((equal? (car lst) id) (cdr lst))
		    (else (cons (car lst) (remove_id (cdr lst) id)))
		)
	)
)


(define select_sort_by_id
	(lambda (roster)
		(cond ( (null? roster) '() ) ; check if null, return empty
		(else (cons (smallest_id roster (car roster)) (select_sort_by_id (remove_id roster(smallest_id roster (car roster)))))
	)))
)

(define select_sort_by_name
	(lambda (roster)
		(cond ( (null? roster) '() ) ; check if null, return empty
		(else (cons (smallest_id roster (car roster)) (select_sort_by_name (remove_id roster(smallest_id roster (car roster)))))
	)))
)


(define sort_by_id ; can't use comparators
	(lambda (x y)
		(if (string<? (car x) (car y)) #t #f)
	)
)


(define display_roster ; Don't use this function, can't use comparators for assignment
	(lambda (l)
		(cond
			((null? l))
			(else
				(display "ID = ")
				(display (car (car l)))
				(display "    name = ")
				(display (cadr (car l)))
				(display "    Grade= ")
				(display (caddr (car l)))
				(newline)
				(display_roster (cdr l))
			)
		)
	)
)



(define add_student_to_roster
        (lambda (n a)
                (cond ((= n 0) (begin
                	(display "\tEnter student ID: ")
                        ; call again, but with 1 instead so to read the next field
                        ; note- look at do loops, might work better than this
                        (add_student_to_roster 1 (list (read-line)))
                        ))
                ((= n 1) (begin
                	(display "\tEnter student name: ")
                        ; append list and read next field
                	(add_student_to_roster 2 (list (car a) (read-line)))
                	))
                ((= n 2) (begin
                        (display "\tEnter student grade: ")
                        ; append (ID) and (Name) and user input
                        (list (car a) (cadr a) (read-line))
                        ))
                )
        )
)


(define remove_student_from_roster
	(lambda (input r)
		(cond 
			((null? r) ; student can't be found in roster
				(display "\t")
				(display input)
				(display " is not in the student rosterxx.\n")
			) 
			((or (equal? (cadr (car r)) input) (equal? (car (car r)) input))
				(display "\n\tRemoved student: ")
				(display input)
				(newline)
				(cdr r)
			)
			(else ; move down list
				(cons (car r) (remove_student_from_roster input (cdr r)))
			)
		)
	)
)


; 4/13/15 cant get name to work. id works fine however
; note-grade prints out with brackets? why? integer?
(define display_student_info
	(lambda (input i)
		(cond 
			((null? i); student can't be found in roster
				(display "\t")
				(display input)
				(display " is not in the student roster.\n")
			) 
			((or (equal? (cadr (car i)) input) (equal? (car (car i)) input)) ; print out found student
				(newline)
				(display "\tid= ")
				(display (caar i))
				(newline)
				(display "\tname= ")
				(display (car (cdr (car i))))
				(newline)
				(display "\tgrade= ")
				(display (cdr (cdr (car i))))
				(newline)
				(newline)
			)
			(else ; If first list element isn't it, look down the list
				(display_student_info input (cdr i))
			)
		)
	)
)



; roster selection handler

(define performtask
  (lambda (n roster) 
    (cond ((= n 0) (begin
                    (display "\n\tRoster\n")
                    (menu '())
                    ))
          ((= n 1) (begin
                    (display "\n\tLoad Roster From File:\n")
                    (display "\n\tEnter file name to load: ")
                	(let ((readfile (open-input-file (read-line))))
				(let ((loadroster (read readfile)))
					(close-input-port readfile)
					(menu loadroster)
				)
                    	)

                    ))
	((= n 2) (begin
                    (display "\n\tStore roster to a file:")
			(display "\n\tEnter file name to store: ")
			(let ((writefile (open-output-file (read-line))))
				(write roster writefile)
				(close-output-port writefile)
			)
			(menu roster)
                    ))
	((= n 3) (begin
                    (display "\n\tDisplay Roster by ID:\n")
		    ;(display_roster (sort roster sort_by_id))
		    (display_roster (select_sort_by_id roster))
                    (menu roster)
                    ))
	((= n 4) (begin
                    (display "\n\tDisplay Roster by name- not yet implemented.\n")
		    (display_roster (select_sort_by_id roster))
                    (menu roster)
                    ))
	((= n 5) (begin
                    (display "\n\tDisplay Roster by grade- not yet implemented.\n")
		    (display_roster (select_sort_by_id roster))
                    (menu roster)
                    ))
	((= n 6) (begin
                    (display "\n\tDisplay student info.\n")
		    (display "\tPlease Enter Student ID: ")
                    (display_student_info (read-line) roster)
		    (menu roster)
                    ))
       ((= n 7) (begin
                    (display "\n\tAdd student.\n")
                    (menu (cons (add_student_to_roster 0 '()) roster))
                    ))
	((= n 8) (begin
                    (display "\n\tRemove student.\n")
    		    (display "\tPlease Enter Student ID: ")
                    (menu (remove_student_from_roster (read-line) roster))
                    ))
          ((= n 9) (begin
                    (display "\n\tExiting Roster\n")
                    #t
                    ))
           (else (begin
                    (display "\n\ttask no. ")
                    (display n)
                    (display " does not exist.\n\n")
                    (menu roster)
                  )
            )
     )
   )
)

; menu for roster
(define menu
  (lambda (roster)
     (begin
        (display "\t============================\n")
        (display "\t   MENU\n")
        (display "\t============================\n")
        (display "\t0. Reset roster\n")
        (display "\t1. Load roster from file\n")
        (display "\t2. Store roster from file\n")
        (display "\t3. Display roster sorted by ID\n")
        (display "\t4. Display roster sorted by name\n")
        (display "\t5. Display roster sorted by grade\n")
        (display "\t6. Display student info\n")
        (display "\t7. Add a student to roster\n")
        (display "\t8. Remove a student from roster\n")
        (display "\t9. Exit\n\n")
        (display "\tEnter your choice: ")
        (performtask (read) roster)
      )
   )
)
