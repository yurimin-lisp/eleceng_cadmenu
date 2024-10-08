
;-----------------------------------------------------------------------------
;전역변수
;SYMSC 심볼크기
;TXTH 텍스트 높이
;TX 텍스트내용
;SM 심볼이름
;---------------------------------------------------------------------------

;;--------------------------------------------------------
;;사용자 설정부
(setq drawlay "SYM") ;; 삽입될 레이어 설정
(setq symlaycol 2) ;; 레이어 색상


;;---------------------------------------------------------
;;함수 정의
(defun cuistartup ( / CMDE)
   (setq SYMSC (getvar "USERI5")) ;;심볼스케일
   (setq TXTH 1.0) ;;텍스트 높이
   (setq TX "TXT") ;;입력문자열
   (setq SM "SYM") ;;심볼이름
   
   (if (= SYMSC 0)
      (progn
        (setq SYMSC (getvar "LTSCALE") )
      )
      (Prin1)
   )
   (setq CMDE (getvar "CMDECHO"))
   (setvar "CMDECHO" 0)
   (scale_setting)
   (setvar "CMDECHO" CMDE)
)

(defun scale_setting ()
  (setvar "USERI5" SYMSC)
  (setvar "LTSCALE" SYMSC)
  (setq TXTH (* SYMSC 2.5))

  (princ "\nSYM SCALE : ")
  (princ SYMSC)
  (princ ", TEXT HEIGHT : ")
  (princ TXTH)
  (princ "\n 만약 위 축적값이 틀렸을경우 'DRAWSCALE'명령 혹은 메뉴의 스케일조절 명령을 사용하시오")
  (princ " ")

)


;;---------------------------------------------------------
;;최초 로드 명령어
(cuistartup)







;;스케일 설정
(defun c:DRAWSCALE ()
   (if (or (null SYMSC) (null TXTH))
      (cuistartup)
   )
   (princ "\n (SYM SCALE : ")
   (princ SYMSC)
   (princ ", TEXT HEIGHT : ")
   (princ TXTH)
   (princ ")")
   (SETQ SYMSC (GETINT " ENTER NEW SYMBOL SCALE : "))
   (scale_setting)
)


;-----------------------------------------------------------------------------
;;메뉴버튼 눌렀을때 심볼 넣기
;;SM 변수로 으로 심볼명 가져오기

;;스케일 따라서
(defun c:SYMINPUT1 (/ CLA POINT1 POINT2 ANG OSN)
   (if (or (null SYMSC) (null TXTH))
      (cuistartup)
   )

	(setq CLA (getvar "CLAYER"))
   (setq POINT1(getpoint "\n Enter Insert Point : ") POINT2(getpoint POINT1", Angle : "))
   (if (= POINT2 nil)
      (setq ANG 90)
      (setq ANG (/ (* (ANGLE POINT1 POINT2) 180.0) pi))
   )
   
   (if (tblsearch "layer" drawlay)
      (princ)
      (command "LAYER" "M" drawlay "C" symlaycol drawlay "")
   )
   (setvar "CLAYER" drawlay)
   (SETQ OSN (GETVAR "OSMODE"))
   (SETVAR "OSMODE" 0)
   (command "-INSERT" SM POINT1 SYMSC SYMSC (- ANG 90))
   
   (setvar "CLAYER" CLA)
   (SETVAR "OSMODE" OSN)
)

;;스케일1
(defun c:SYMINPUT2 (/ CLA POINT1 POINT2 ANG OSN)
   (if (or (null SYMSC) (null TXTH))
      (cuistartup)
   )

	(setq CLA (getvar "CLAYER"))
   (setq POINT1(getpoint "\n Enter Insert Point : ") POINT2(getpoint POINT1", Angle : "))
   (if (= POINT2 nil)
      (setq ANG 90)
      (setq ANG (/ (* (ANGLE POINT1 POINT2) 180.0) pi))
   )
   (if (tblsearch "layer" drawlay)
      (princ)
      (command "LAYER" "M" drawlay "C" symlaycol drawlay "")
   )
   (setvar "CLAYER" drawlay)
   (SETQ OSN (GETVAR "OSMODE"))
   (SETVAR "OSMODE" 0)
   (command "-INSERT" SM POINT1 1 1 (- ANG 90))
   (setvar "CLAYER" CLA)
   (SETVAR "OSMODE" OSN)
)


;;각도지정없이 넣기
(defun c:SYMINPUT3 (/ CLA POINT1 OSN)
   (if (or (null SYMSC) (null TXTH))
      (cuistartup)
   )

	(setq CLA (getvar "CLAYER"))
   (setq POINT1(getpoint "\n Enter Point : "))
   (if (tblsearch "layer" drawlay)
      (princ)
      (command "LAYER" "M" drawlay "C" symlaycol drawlay "")
   )
   (setvar "CLAYER" drawlay)
   (SETQ OSN (GETVAR "OSMODE"))
   (SETVAR "OSMODE" 0)
   (command "-INSERT" SM POINT1 SYMSC SYMSC 0)
   (setvar "CLAYER" CLA)
   (SETVAR "OSMODE" OSN)
)

;;각도 180 넣기
(defun c:SYMINPUT4 (/ CLA POINT1 OSN)
   (if (or (null SYMSC) (null TXTH))
      (cuistartup)
   )

	(setq CLA (getvar "CLAYER"))
   (setq POINT1(getpoint "\n Enter Point : "))
   (if (tblsearch "layer" drawlay)
      (princ)
      (command "LAYER" "M" drawlay "C" symlaycol drawlay "")
   )
   (setvar "CLAYER" drawlay)
   (SETQ OSN (GETVAR "OSMODE"))
   (SETVAR "OSMODE" 0)
   (command "-INSERT" SM POINT1 SYMSC SYMSC 180)
   (setvar "CLAYER" CLA)
   (SETVAR "OSMODE" OSN)
)



;;모터용
(defun c:SYMINPUT5 (/ CLA POINT1 POINT2 ANG OSN)
   (if (or (null SYMSC) (null TXTH))
      (cuistartup)
   )

	(setq CLA (getvar "CLAYER"))
   (setq POINT1(getpoint "\n Enter Insert Point : ") POINT2(getpoint POINT1", Angle : "))
   (if (= POINT2 nil)
      (setq ANG 90)
      (setq ANG (/ (* (ANGLE POINT1 POINT2) 180.0) pi))
   )
   
   (if (tblsearch "layer" drawlay)
      (princ)
      (command "LAYER" "M" drawlay "C" symlaycol drawlay "")
   )
   (setvar "CLAYER" drawlay)
   (SETQ OSN (GETVAR "OSMODE"))
   (SETVAR "OSMODE" 0)
   (command "-INSERT" "ESYM_MOTCON" POINT1 SYMSC SYMSC 0)
   (command "-INSERT" SM POINT1 SYMSC SYMSC (- ANG 90))
   
   (setvar "CLAYER" CLA)
   (SETVAR "OSMODE" OSN)
)

;;선 중간에 텍스트 넣기
;;텍스트내용은 TX 변수값
(defun c:LINETEXT (/ CMDE WHI POINT P1 P2 OBJ OBJX COD LST LST_temp CLA CCE OSN CST ANG)
   (if (or (null SYMSC) (null TXTH))
      (cuistartup)
   )

   (setq CMDE (getvar "cmdecho"))
   (setvar "cmdecho" 0)

   (setq WHI 0)
   (while (= WHI 0)
      (setq POINT (getpoint "\nCUT POINT"))
      (setq P1 (list (+ (car POINT) 0.001) (+ (cadr POINT) 0.001)))
      (setq P2 (list (- (car POINT) 0.001) (- (cadr POINT) 0.001)))

      (setq OBJ (ssget "C" P2 P1 '((0 . "LINE,*LWPOLYLINE,*POLYLINE"))))

      (if (= OBJ nil)
         (princ "INPUT ERROR")
         (setq WHI 1)
      )
   )

   
   
   (if (= "LINE" (cdr (assoc 0 (entget (ssname OBJ 0)))))
      (progn 
         (setq LST (list (cdr (assoc 10 (entget (ssname OBJ 0)))) (cdr (assoc 11 (entget (ssname OBJ 0))))))
      )
      (if (= "LWPOLYLINE" (cdr (assoc 0 (entget (ssname OBJ 0)))))
         (progn
            (setq OBJX (vlax-ename->vla-object (ssname OBJ 0)))
            (setq COD (vlax-get OBJX 'coordinates))
            (setq WHI 0)
            (while (< WHI (length COD) )
               (setq LST_temp (list (nth (+ WHI 0) COD) (nth (+ WHI 1) COD)))
               (setq LST (cons LST_temp LST))
               (setq WHI (+ WHI 2))
            )
         )
         (if (= "POLYLINE" (cdr (assoc 0 (entget (ssname OBJ 0)))))
            (progn
               (setq OBJX (vlax-ename->vla-object (ssname OBJ 0)))
               (setq COD (vlax-get OBJX 'coordinates))
               (setq WHI 0)
               (while (< WHI (length COD))
                  (setq LST_temp (list (nth (+ WHI 0) COD) (nth (+ WHI 1) COD) (nth (+ WHI 2) COD) ))
                  (setq LST (cons LST_temp LST))
                  (setq WHI (+ WHI 3))
               )
            )
            (princ)
         )
      )
   
   )


   (setq WHI 0)


   

   

   (while (< WHI (- (length LST) 1))

      (setq P1 (nth WHI LST))
      
      (setq P2 (nth (+ WHI 1) LST))
   
   
      (if (< (abs (- (ANGLE P1 POINT) (ANGLE POINT P2))) 0.02)
         (progn
            (setq P1 (list (car P1) (cadr P1)))
            (setq P2 (list (car P2) (cadr P2)))
            (setq ANG (/ (* (ANGLE P1 P2) 180.0) pi))
         )
         (setq ANG 45)
      )
      (setq WHI (+ WHI 1))
   )

   
   (setq ANG (fix ANG))
   
   (setq ANG (rem ANG 180))
   



   (setq CLA (getvar "CLAYER"))
   (setq CCE (getvar "CECOLOR"))
   (setq OSN (getvar "OSMODE"))
   (setq CST (getvar "TEXTSTYLE"))
   (setvar "CLAYER" (cdr (assoc 8 (entget (ssname OBJ 0)))))
   (command "CECOLOR" 7)
   (setvar "OSMODE" 0)


   (IF (or (< ANG 10) (> ANG 170))
      (progn
         (setq P1 (list (+ (car POINT) (/ (* (+ (strlen TX) 0.2) TXTH) 2)) (+ (cadr POINT) 0.01 )))
         (setq P2 (list (- (car POINT) (/ (* (+ (strlen TX) 0.2) TXTH) 2)) (- (cadr POINT) 0.01 )))
      )
      (IF (and (> ANG 80) (< ANG 100))
         (progn
            (setq P1 (list (+ (car POINT) 0.01 ) (+ (cadr POINT) (/ TXTH 1.35))))
            (setq P2 (list (- (car POINT) 0.01 ) (- (cadr POINT) (/ TXTH 1.35))))
         )
         (progn
            (setq P1 (list (+ (car POINT) (/ (* (+ (strlen TX) 0.2) TXTH) 2)) (+ (cadr POINT) (/ TXTH 1.35))))
            (setq P2 (list (- (car POINT) (/ (* (+ (strlen TX) 0.2) TXTH) 2)) (- (cadr POINT) (/ TXTH 1.35))))
         )
      )
   )

   ;;(command "rectangle" P1 P2)

   (command "break" OBJ P1 P2)
   (command "style" "맑은고딕" "malgun.ttf" "0" "1" "0" "N" "N" )
   (command "TEXT" "J" "MC" POINT TXTH 0 TX)

   (setvar "CLAYER" CLA)
   (command "CECOLOR" CCE)
   (setvar "OSMODE" OSN)
   (setvar "cmdecho" CMDE)
)
   


(defun c:CIRCUIT (/ CLA POINT1 OSN TXT l_point r_point)
	(if (or (null SYMSC) (null TXTH))
      (cuistartup)
   )
   (setq POINT1(getpoint "\n Enter Point : "))
   (setq TXT (getstring "\n CIRCUIT :"))
   (setq CLA (getvar "CLAYER"))
   (setq CCE (getvar "CECOLOR"))
   (setq OSN (getvar "OSMODE"))

   (command "CECOLOR" "BYLAYER")
   (setvar "OSMODE" 0)

   (if (tblsearch "layer" "TH")
      (princ)
      (command "LAYER" "M" "TH" "C" "1" "TH" "")
   )
   (setvar "CLAYER" "TH")
   (SETQ OSN (GETVAR "OSMODE"))
   (SETVAR "OSMODE" 0)
   (command "-INSERT" SM POINT1 TXTH TXTH 0)
   (setq l_point (list (- (car POINT1) (* TXTH 0.7) ) (- (cadr POINT1) (* TXTH 0.5))))
   (setq r_point (list (+ (car POINT1) (* TXTH 0.7) ) (- (cadr POINT1) (* TXTH 0.5))))
   (command "style" "맑은고딕" "malgun.ttf" "0" "1" "0" "N" "N" )
   (command "CECOLOR" 7)
  
   (IF (= (STRLEN TXT) 1)
      (command "_.text" "s" "맑은고딕" "j" "MC" point1 TXTH 0 TXT)
      (command "_.text" "s" "맑은고딕" "j" "f" l_point r_point TXTH TXT)
   )
   
  
  

   (setvar "CLAYER" CLA)
   (command "CECOLOR" CCE)
   (SETVAR "OSMODE" OSN)
)



(DEFUN c:UNRP () ;;도면단위재지정
   (command "-units" 2 4 1 4 0 "n")
   (command "-dwgunits" 3 2 4 "y" "y" "n")
)



;;종단저항 표기
(DEFUN C:FRESIS (/ N P CLA CCE OSN I OUTTXT)
   (if (or (null SYMSC) (null TXTH))
      (cuistartup)
   )
   (SETQ N (GETINT "\NINPUT RESIS NUM : "))
   (SETQ P (GETPOINT "\NPICK POINT : "))
   (setq CLA (getvar "CLAYER"))
   (setq CCE (getvar "CECOLOR"))
   (setq OSN (getvar "OSMODE"))
   (SETQ I 0)
   (WHILE (< I N)
      (IF (= I 0)
         (SETQ OUTTXT "Ω")
         (SETQ OUTTXT (strcat OUTTXT "Ω"))
      )
      (SETQ I (+ I 1))   
   )
   (command "style" "맑은고딕" "malgun.ttf" "0" "1" "0" "N" "N" )

   (if (tblsearch "layer" drawlay)
      (princ)
      (command "LAYER" "M" drawlay "C" symlaycol drawlay "")
   )
   (setvar "CLAYER" drawlay)
   (command "_.text" "s" "맑은고딕" "j" "ML" P (/ TXTH 2) 0 OUTTXT)

   (setvar "CLAYER" CLA)
   (command "CECOLOR" CCE)
   (SETVAR "OSMODE" OSN)


)






