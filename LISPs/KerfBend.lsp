;;; ====================================================================
;;; KerfBend.lsp  -  Kerf-bending planner and drawer for AutoCAD 2026
;;; ====================================================================
;;;
;;; WHAT IS KERF BENDING?
;;;   You cut a row of saw cuts ("kerfs") across a board, almost all the
;;;   way through, leaving a thin uncut "web" at the bottom. The board can
;;;   then bend toward the cut face: each kerf squeezes closed a little.
;;;
;;; COMMANDS (type them at the AutoCAD command line):
;;;   KERFBEND  - type in the bend (inside OR outside radius, angle,
;;;               straight legs). Prints the plan, then draws:
;;;                 1. Plan view (cut layout) with dimensions
;;;                 2. Flat side view showing the kerf notches
;;;                 3. Realistic bent preview: flat ribs between kerfs,
;;;                    kerf notches closing up, true arcs in the web
;;;                 4. A notes block with all numbers and kerf positions
;;;   KERFPATH  - pick a line / arc / circle / polyline / ellipse / spline
;;;               that is already drawn and use it as the OUTSIDE face of
;;;               the bent wood. Works out every bend along it (even
;;;               changing radii), draws the real bent board (ribs and
;;;               closed kerfs) on top of your curve, and draws the flat
;;;               cut layout where you pick.
;;;   KERFCALC  - same questions as KERFBEND, only prints the plan
;;;   KERFHELP  - short help text
;;;
;;; UNITS: works in inches or millimetres. Just be consistent.
;;;   Defaults are inch values in imperial drawings, mm in metric ones.
;;;
;;; THE MATH (simple bending model):
;;;   d  = kerf depth          = thickness - web
;;;   h  = lever to bend line  = d + web/2   (bend line = middle of web)
;;;   Each kerf can close an angle of  k / h  (k = kerf width)
;;;   Number of kerfs   N = roundup( bend angle(rad) * h / k )
;;;   Flat length of a bend = length along the bend line (middle of web)
;;;   KERFBEND: equal spacing S = flat bend length / N
;;;   KERFPATH: kerfs placed at equal steps of turning angle, so they
;;;             crowd together where the curve is tighter.
;;;
;;; Layers created: KERF-OUTLINE, KERF-CUTS, KERF-CUTS-F1, KERF-BEND,
;;;                 KERF-PREVIEW, KERF-DIMS, KERF-NOTES, KERF-WARN (red)
;;; ====================================================================

(vl-load-com)
(setq kb:doc (vla-get-ActiveDocument (vlax-get-acad-object)))

;;; --------------------------------------------------------------------
;;; Remembered answers (kept between runs in the same drawing session)
;;; --------------------------------------------------------------------
(defun kb:defaults (/ lst)
  (setq lst
    (if (= (getvar "MEASUREMENT") 1)
      ;; metric (mm)
      '((*kb:th* . 18.0) (*kb:web* . 2.0) (*kb:kerf* . 3.0)
        (*kb:rad* . 150.0) (*kb:ang* . 90.0) (*kb:wid* . 300.0)
        (*kb:l1* . 150.0) (*kb:l2* . 150.0))
      ;; imperial (inches)
      '((*kb:th* . 0.75) (*kb:web* . 0.0625) (*kb:kerf* . 0.125)
        (*kb:rad* . 6.0) (*kb:ang* . 90.0) (*kb:wid* . 12.0)
        (*kb:l1* . 6.0) (*kb:l2* . 6.0))))
  (foreach pair lst
    (if (null (eval (car pair))) (set (car pair) (cdr pair))))
  (if (null *kb:rtype*) (setq *kb:rtype* "Inside"))
)

;;; --------------------------------------------------------------------
;;; Input helpers (press Enter to keep the value shown in < >)
;;; --------------------------------------------------------------------
(defun kb:getlen (msg def bits / v)
  (initget bits)
  (setq v (getdist (strcat "\n" msg " <" (rtos def) ">: ")))
  (if v v def)
)

(defun kb:getnum (msg def bits / v)
  (initget bits)
  (setq v (getreal (strcat "\n" msg " <" (rtos def 2 2) ">: ")))
  (if v v def)
)

;; Material questions (used by KERFBEND, KERFCALC and KERFPATH)
(defun kb:askmat (/ ok)
  (setq *kb:th* (kb:getlen "Material thickness" *kb:th* 6))
  (setq ok nil)
  (while (not ok)
    (setq *kb:web*
      (kb:getlen "Uncut material to leave at bottom of each kerf (web)"
                 (if (< *kb:web* *kb:th*) *kb:web* (* 0.1 *kb:th*)) 6))
    (if (< *kb:web* *kb:th*)
      (setq ok T)
      (princ "\n  ** The web must be thinner than the material. Try again.")))
  (setq *kb:kerf* (kb:getlen "Saw blade (or router bit) kerf width" *kb:kerf* 6))
)

(defun kb:ask (/ ok v)
  (kb:defaults)
  (princ "\n--- Kerf bending setup (Enter = keep value in < >) ---")
  (kb:askmat)
  ;; inside or outside radius
  (initget "Inside Outside")
  (setq v (getkword (strcat "\nWhich radius will you enter? [Inside/Outside] <"
                            *kb:rtype* ">: ")))
  (if v (setq *kb:rtype* v))
  (setq ok nil)
  (while (not ok)
    (if (= *kb:rtype* "Outside")
      (progn
        (setq v (kb:getlen "Outside bend radius (the face WITHOUT kerfs)"
                           (+ *kb:rad* *kb:th*) 6))
        (if (> v *kb:th*)
          (setq *kb:rad* (- v *kb:th*) ok T)
          (princ (strcat "\n  ** Outside radius must be bigger than the thickness ("
                         (rtos *kb:th*) "). Try again."))))
      (setq *kb:rad* (kb:getlen "Inside bend radius (the face WITH the kerfs)" *kb:rad* 6)
            ok T)))
  (setq ok nil)
  (while (not ok)
    (setq *kb:ang* (kb:getnum "Bend angle in degrees (less than 360)" *kb:ang* 6))
    (if (< *kb:ang* 360.0)
      (setq ok T)
      (princ "\n  ** The angle must be less than 360. Try again.")))
  (setq *kb:wid* (kb:getlen "Panel width (the length of each kerf cut)" *kb:wid* 6))
  (setq *kb:l1*  (kb:getlen "Straight length BEFORE the bend" *kb:l1* 4))
  (setq *kb:l2*  (kb:getlen "Straight length AFTER the bend" *kb:l2* 4))
  T
)

;;; --------------------------------------------------------------------
;;; KERFBEND calculation (single bend, constant radius)
;;; --------------------------------------------------------------------
(defun kb:calc (/ d h a theta nf n rn lb s)
  (setq d     (- *kb:th* *kb:web*)
        h     (+ d (/ *kb:web* 2.0))
        a     (* pi (/ *kb:ang* 180.0))
        theta (/ *kb:kerf* h)              ; max angle one kerf can close
        nf    (/ a theta)
        n     (fix nf))
  (if (> (- nf n) 1e-6) (setq n (1+ n)))
  (if (< n 1) (setq n 1))
  (setq rn (+ *kb:rad* h)
        lb (* a rn)
        s  (/ lb n))
  (setq *kb:res*
    (list (cons 'd d) (cons 'h h) (cons 'a a) (cons 'theta theta)
          (cons 'n n) (cons 'rn rn) (cons 'lb lb) (cons 's s)
          (cons 'tot (+ *kb:l1* lb *kb:l2*))
          (cons 'per (/ a n))
          (cons 'land (- s *kb:kerf*))))
)

(defun kb:r (key) (cdr (assoc key *kb:res*)))

;; Centre of kerf number i (0 = first), measured from LEFT end of panel
(defun kb:pos (i) (+ *kb:l1* (* (kb:r 's) (+ i 0.5))))

;;; --------------------------------------------------------------------
;;; Report text
;;; --------------------------------------------------------------------
(defun kb:commonwarn (/ out)
  (if (> (/ *kb:web* *kb:th*) 0.3)
    (setq out (cons "CAUTION: web is over 30% of the thickness - it may crack when bent." out)))
  (setq out (cons "TIP: cut a test strip first - wood and plywood vary." out))
  (reverse out)
)

(defun kb:warnings (/ out)
  (if (<= (kb:r 'land) 0.0)
    (setq out (list "WARNING: kerfs overlap! Use a bigger radius, a thinner blade, or a thinner web."))
    (if (< (kb:r 'land) *kb:kerf*)
      (setq out (list "CAUTION: wood left between kerfs is thinner than the blade - fragile."))))
  (append out (kb:commonwarn))
)

;; dg = degree symbol to use ("%%d" for drawing text, " deg" for command line)
(defun kb:report (dg / rd)
  (setq rd (/ 180.0 pi))
  (append
    (list
      (strcat "Material thickness: " (rtos *kb:th*))
      (strcat "Web left uncut: " (rtos *kb:web*))
      (strcat "Kerf (blade) width: " (rtos *kb:kerf*))
      (strcat "Inside radius: " (rtos *kb:rad*)
              "    Outside radius: " (rtos (+ *kb:rad* *kb:th*))
              "    (you entered the " (strcase *kb:rtype* T) " radius)")
      (strcat "Bend angle: " (rtos *kb:ang* 2 2) dg)
      (strcat "Panel width: " (rtos *kb:wid*))
      ""
      (strcat "KERF DEPTH (blade height): " (rtos (kb:r 'd)))
      (strcat "NUMBER OF KERFS: " (itoa (kb:r 'n)))
      (strcat "KERF SPACING (centre to centre): " (rtos (kb:r 's)))
      (strcat "Wood left between kerfs: " (rtos (kb:r 'land)))
      (strcat "Bend zone length (on flat board): " (rtos (kb:r 'lb)))
      (strcat "First kerf centre from start of bend: " (rtos (/ (kb:r 's) 2.0)))
      (strcat "TOTAL FLAT PANEL LENGTH: " (rtos (kb:r 'tot)))
      (strcat "Each kerf closes " (rtos (* rd (kb:r 'per)) 2 2) dg
              " of a possible " (rtos (* rd (kb:r 'theta)) 2 2) dg
              " (" (rtos (* 100.0 (/ (kb:r 'per) (kb:r 'theta))) 2 0) "% closed)")
      "")
    (kb:warnings))
)

;; Numbered list, 5 per line
(defun kb:listlines (strs / i out line)
  (setq i 0 out nil line "")
  (foreach s strs
    (setq line (strcat line (itoa (1+ i)) ") " s "    "))
    (if (= (rem (1+ i) 5) 0) (setq out (cons line out) line ""))
    (setq i (1+ i)))
  (if (/= line "") (setq out (cons line out)))
  (reverse out)
)

(defun kb:poslines (/ i l)
  (setq i 0 l nil)
  (repeat (kb:r 'n) (setq l (cons (rtos (kb:pos i)) l) i (1+ i)))
  (kb:listlines (reverse l))
)

(defun kb:print ()
  (princ "\n\n=============== KERF BENDING PLAN ===============")
  (foreach s (kb:report " deg") (princ (strcat "\n  " s)))
  (princ "\n  Kerf centres measured from the LEFT end of the panel:")
  (foreach s (kb:poslines) (princ (strcat "\n    " s)))
  (princ "\n=================================================")
  (princ "\n  (Press F2 to see the full list in the text window.)\n")
  (princ)
)

(defun kb:join (lst sep / s)
  (setq s (car lst))
  (foreach x (cdr lst) (setq s (strcat s sep x)))
  s
)

;;; --------------------------------------------------------------------
;;; Drawing helpers (points are relative to the picked base point,
;;; except kb:wline which uses real WCS points)
;;; --------------------------------------------------------------------
(defun kb:abs (p) (list (+ kb:bx (car p)) (+ kb:by (cadr p)) 0.0))

(defun kb:space ()
  (if (= 1 (getvar "CVPORT"))
    (vla-get-PaperSpace kb:doc)
    (vla-get-ModelSpace kb:doc))
)

(defun kb:layer (name col lt)
  (if (not (tblsearch "LAYER" name))
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord") (cons 2 name) '(70 . 0)
                   (cons 62 col) (cons 6 lt))))
)

(defun kb:loadlt (name / f)
  (if (not (tblsearch "LTYPE" name))
    (progn
      (setq f (findfile (if (= (getvar "MEASUREMENT") 1) "acadiso.lin" "acad.lin")))
      (if f (vl-catch-all-apply 'vla-Load (list (vla-get-Linetypes kb:doc) name f)))))
  (tblsearch "LTYPE" name)
)

(defun kb:mklayers ()
  (kb:layer "KERF-OUTLINE" 7 "Continuous")
  (kb:layer "KERF-CUTS"    1 "Continuous")
  (kb:layer "KERF-CUTS-F1" 6 "Continuous")
  (kb:layer "KERF-BEND"    4 (if (kb:loadlt "DASHED") "DASHED" "Continuous"))
  (kb:layer "KERF-PREVIEW" 3 "Continuous")
  (kb:layer "KERF-DIMS"    8 "Continuous")
  (kb:layer "KERF-NOTES"   2 "Continuous")
  (kb:layer "KERF-WARN"    1 "Continuous")
)

;; Linetype scale so dashes come out about 1.5 x text height
(defun kb:ltscale (txth)
  (/ (* 1.5 txth)
     (if (= (getvar "MEASUREMENT") 1) 12.7 0.5)
     (max 1e-6 (getvar "LTSCALE")))
)

(defun kb:lwp (pts bulges closed lay) (kb:lwpd pts bulges closed lay 1.0))

;; Same, with a linetype scale (for dashed lines). Linetype runs
;; continuously around corners (PLINEGEN on).
(defun kb:lwpd (pts bulges closed lay lts)
  (entmake
    (append
      (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 lay) (cons 48 lts)
            '(100 . "AcDbPolyline") (cons 90 (length pts))
            (cons 70 (+ 128 (if closed 1 0))))
      (apply 'append
        (mapcar '(lambda (p b)
                   (list (cons 10 (list (+ kb:bx (car p)) (+ kb:by (cadr p))))
                         (cons 42 (float b))))
                pts bulges))))
)

(defun kb:rect (x1 y1 x2 y2 lay)
  (kb:lwp (list (list x1 y1) (list x2 y1) (list x2 y2) (list x1 y2))
          '(0 0 0 0) T lay)
)

(defun kb:line (p1 p2 lay lts)
  (entmake (list '(0 . "LINE") (cons 8 lay)
                 (cons 10 (kb:abs p1)) (cons 11 (kb:abs p2)) (cons 48 lts)))
)

;; Line between real (WCS) points
(defun kb:wline (p1 p2 lay)
  (entmake (list '(0 . "LINE") (cons 8 lay) (cons 10 p1) (cons 11 p2)))
)

(defun kb:text (p h str lay / o)
  (setq o (vla-AddMText (kb:space) (vlax-3d-point (kb:abs p)) 0.0 str))
  (vla-put-Height o h)
  (vla-put-Layer o lay)
  o
)

(defun kb:dim (p1 p2 p3)
  (vl-cmdf "_.DIMLINEAR" "_non" (kb:abs p1) "_non" (kb:abs p2) "_non" (kb:abs p3))
)

;; Switch to World UCS and size dimensions to the drawing
(defun kb:dimstart (txth / dt)
  (command-s "_.UCS" "_W")
  (setq kb:ucsset T)
  (setq dt (getvar "DIMTXT"))
  (if (> dt 0) (vl-catch-all-apply 'setvar (list "DIMSCALE" (/ txth dt))))
  (setvar "CLAYER" "KERF-DIMS")
)

(defun kb:dimend ()
  (command-s "_.UCS" "_P")
  (setq kb:ucsset nil)
)

;; Remove a vertex when it sits on top of the next one (zero-length legs)
(defun kb:dedupe (pts buls / out ob i np)
  (setq i 0 np (length pts))
  (foreach p pts
    (if (not (equal p (nth (rem (1+ i) np) pts) 1e-9))
      (setq out (cons p out) ob (cons (nth i buls) ob)))
    (setq i (1+ i)))
  (list (reverse out) (reverse ob))
)

;;; --------------------------------------------------------------------
;;; REALISTIC BENT PROFILE
;;; --------------------------------------------------------------------
;;; Models the board the way it really bends: the solid ribs between
;;; kerfs stay flat and rigid, and only the thin web under each kerf
;;; bends (as a true arc), so each kerf notch closes up into a "V".
;;; Every corner of the bent outline comes from the same kerf positions
;;; as the flat cut layout, so the two line up exactly.
;;;
;;; Flat board coordinates: x along the board (from xa to xb),
;;; y across the thickness: y = 0 is Face 1, y = thickness is Face 2.
;;; zone = one kerf: (x0 x1 turn yn face)
;;;   turn = angle that kerf bends (radians, + = counter-clockwise)
;;;   yn   = height of the neutral line (middle of the web) for it
;;;   face = 2 -> notch cut from Face 2, 1 -> notch cut from Face 1
;;; p0 / h0 = where the y=0 line starts and its direction (radians)
;;; w = +1 if Face 2 is to the LEFT of the direction of travel, -1 if right
;;; Returns (points bulges) for one closed polyline.
;;; --------------------------------------------------------------------
(defun kb:lft (h) (list (- (sin h)) (cos h)))

(defun kb:add2 (p v s) (list (+ (car p) (* s (car v))) (+ (cadr p) (* s (cadr v)))))

(defun kb:tan4 (a) (/ (sin (/ a 4.0)) (cos (/ a 4.0))))

;; Point on a rigid (flat) part: dx along from frame p, height y
(defun kb:rigid (p h w dx y)
  (kb:add2 (kb:add2 p (list (cos h) (sin h)) dx) (kb:lft h) (* w y))
)

;; Point at height y in a bending web, where the board direction is h
(defun kb:arcpt (c kap yn w h y / rho)
  (setq rho (- (/ 1.0 kap) (* w (- y yn))))
  (list (+ (car c) (* rho (sin h))) (- (cadr c) (* rho (cos h))))
)

;; Point at the start (atend = nil) or end (atend = T) of a kerf frame
(defun kb:zp (f atend y w)
  (kb:arcpt (nth 5 f) (nth 2 f) (nth 3 f) w
            (+ (nth 4 f) (if atend (nth 7 f) 0.0)) y)
)

(defun kb:bentprofile (zones xa xb p0 h0 w / tk wb x p h fr x0 x1 dt kap yn c
                                            pe he b bot top all)
  (setq tk *kb:th* wb *kb:web* x xa p p0 h h0 fr nil)
  ;; 1. walk along the board, building a frame for each kerf
  (foreach z zones
    (setq x0 (min xb (max x (car z)))
          x1 (min xb (max x0 (cadr z)))
          dt (caddr z))
    (if (and (> x1 x0) (/= dt 0.0))
      (progn
        (setq p (kb:rigid p h w (- x0 x) 0.0))           ; flat rib up to kerf
        (setq kap (/ dt (- x1 x0)) yn (cadddr z))
        (setq c (kb:add2 (kb:add2 p (kb:lft h) (* w yn)) (kb:lft h) (/ 1.0 kap)))
        ;; frame: (x0 x1 kappa yn heading centre face turn)
        (setq fr (cons (list x0 x1 kap yn h c (nth 4 z) dt) fr))
        (setq h (+ h dt) p (kb:arcpt c kap yn w h 0.0) x x1))))
  (setq pe (kb:rigid p h w (- xb x) 0.0) he h)            ; frame at the far end
  (setq fr (reverse fr))
  ;; 2. Face 1 edge, left to right (built backwards, then reversed)
  (setq bot (list (list (kb:rigid p0 h0 w 0.0 0.0) 0.0)))
  (foreach f fr
    (setq b (kb:tan4 (nth 7 f)))
    (if (= (nth 6 f) 2)
      ;; web bends on this face: smooth arc
      (setq bot (cons (list (kb:zp f T 0.0 w) 0.0)
                (cons (list (kb:zp f nil 0.0 w) b) bot)))
      ;; kerf cut from this face: wall, arched floor, wall
      (setq bot (cons (list (kb:zp f T 0.0 w) 0.0)
                (cons (list (kb:zp f T (- tk wb) w) 0.0)
                (cons (list (kb:zp f nil (- tk wb) w) b)
                (cons (list (kb:zp f nil 0.0 w) 0.0) bot)))))))
  (setq bot (cons (list (kb:rigid pe he w 0.0 0.0) 0.0) bot))
  ;; 3. Face 2 edge, right to left
  (setq top (list (list (kb:rigid pe he w 0.0 tk) 0.0)))
  (foreach f (reverse fr)
    (setq b (- (kb:tan4 (nth 7 f))))
    (if (= (nth 6 f) 2)
      (setq top (cons (list (kb:zp f nil tk w) 0.0)
                (cons (list (kb:zp f nil wb w) 0.0)
                (cons (list (kb:zp f T wb w) b)
                (cons (list (kb:zp f T tk w) 0.0) top)))))
      (setq top (cons (list (kb:zp f nil tk w) 0.0)
                (cons (list (kb:zp f T tk w) b) top)))))
  (setq top (cons (list (kb:rigid p0 h0 w 0.0 tk) 0.0) top))
  (setq all (append (reverse bot) (reverse top)))
  (kb:dedupe (mapcar 'car all) (mapcar 'cadr all))
)

;;; --------------------------------------------------------------------
;;; Start / finish (save and restore settings, one UNDO step)
;;; --------------------------------------------------------------------
(defun kb:start ()
  (setq kb:oldvars
    (mapcar '(lambda (v) (cons v (getvar v)))
            '("OSMODE" "CMDECHO" "CLAYER" "DIMSCALE")))
  (vla-StartUndoMark kb:doc)
  (setq kb:started T)
  (setvar "OSMODE" 0)
  (setvar "CMDECHO" 0)
)

(defun kb:finish ()
  (if kb:ucsset
    (progn (command-s "_.UCS" "_P") (setq kb:ucsset nil)))
  (if kb:started
    (progn
      (foreach v kb:oldvars
        (vl-catch-all-apply 'setvar (list (car v) (cdr v))))
      (vla-EndUndoMark kb:doc)
      (setq kb:started nil)))
)

;;; --------------------------------------------------------------------
;;; KERFBEND drawing
;;; --------------------------------------------------------------------
(defun kb:draw (bp / th web k r w l1 l2 d n s lb tot rn a txth gap lts
                     i x xl xr y0 top pts ro c zones prof minx ox oy dd notes
                     sa ca ta p3 p4 p5 p6)
  (setq kb:bx (car bp) kb:by (cadr bp))
  (setq th *kb:th* web *kb:web* k *kb:kerf* r *kb:rad* w *kb:wid*
        l1 *kb:l1* l2 *kb:l2*
        d (kb:r 'd) n (kb:r 'n) s (kb:r 's) lb (kb:r 'lb)
        tot (kb:r 'tot) rn (kb:r 'rn) a (kb:r 'a))
  (setq txth (/ (max tot w) 50.0)
        gap  (max (* 8.0 txth) (* 0.3 w))
        lts  (kb:ltscale txth))
  (kb:mklayers)

  ;; ---------- 1. PLAN VIEW (cut layout) ----------
  (kb:rect 0 0 tot w "KERF-OUTLINE")
  (setq i 0)
  (repeat n
    (setq x (kb:pos i))
    (kb:rect (- x (/ k 2.0)) 0 (+ x (/ k 2.0)) w "KERF-CUTS")
    (setq i (1+ i)))
  ;; dashed lines at start and end of the bend zone
  (kb:line (list l1 (- txth)) (list l1 (+ w txth)) "KERF-BEND" lts)
  (kb:line (list (+ l1 lb) (- txth)) (list (+ l1 lb) (+ w txth)) "KERF-BEND" lts)
  (kb:text (list 0 (+ w (* 8.0 txth))) (* 1.4 txth)
           "PLAN VIEW - CUT LAYOUT (kerfs in red, bend zone dashed)" "KERF-NOTES")

  ;; ---------- 2. FLAT SIDE VIEW with notches ----------
  (setq y0 (- (+ gap th)) top (+ y0 th))
  (setq pts (list (list 0 y0) (list tot y0) (list tot top)))
  (setq i (1- n))
  (while (>= i 0)
    (setq x (kb:pos i) xl (- x (/ k 2.0)) xr (+ x (/ k 2.0)))
    (setq pts (append pts (list (list xr top) (list xr (- top d))
                                (list xl (- top d)) (list xl top))))
    (setq i (1- i)))
  (setq pts (append pts (list (list 0 top))))
  (setq pts (kb:dedupe pts (mapcar '(lambda (p) 0) pts)))
  (kb:lwp (car pts) (cadr pts) T "KERF-OUTLINE")
  (kb:text (list 0 (- y0 (* 2.0 txth))) (* 1.4 txth)
           "SIDE VIEW - FLAT (cut kerfs into the TOP face = inside of the bend)" "KERF-NOTES")

  ;; ---------- 3. BENT PREVIEW (realistic: flat ribs, closed kerfs) ----------
  (setq ro (+ r th)
        c  (list l1 (+ th r)))                    ; design bend centre
  ;; one zone per kerf, exactly where it is on the flat board
  (setq zones nil i 0)
  (repeat n
    (setq x (kb:pos i))
    (setq zones (cons (list (- x (/ k 2.0)) (+ x (/ k 2.0)) (/ a n) (/ web 2.0) 2) zones)
          i (1+ i)))
  (setq prof (kb:bentprofile (reverse zones) 0.0 tot '(0.0 0.0) 0.0 1))
  ;; shift so the preview sits to the right of the plan view
  (setq minx (apply 'min (mapcar 'car (car prof))))
  (setq ox (- (+ tot gap) minx) oy 0.0)
  (defun kb:sh (p) (list (+ (car p) ox) (+ (cadr p) oy)))
  (kb:lwp (mapcar 'kb:sh (car prof)) (cadr prof) T "KERF-PREVIEW")
  ;; dashed DESIGN outline (perfect arcs) to compare with the real shape
  (setq sa (sin a) ca (cos a) ta (kb:tan4 a))
  (setq p3 (list (+ l1 (* ro sa)) (- (cadr c) (* ro ca)))
        p4 (list (+ (car p3) (* l2 ca)) (+ (cadr p3) (* l2 sa)))
        p6 (list (+ l1 (* r sa)) (- (cadr c) (* r ca)))
        p5 (list (+ (car p6) (* l2 ca)) (+ (cadr p6) (* l2 sa))))
  (setq pts (kb:dedupe (mapcar 'kb:sh (list '(0.0 0.0) (list l1 0.0) p3 p4
                                            p5 p6 (list l1 th) (list 0.0 th)))
                       (list 0 ta 0 0 0 (- ta) 0 0)))
  (kb:lwpd (car pts) (cadr pts) T "KERF-BEND" lts)
  ;; centre mark (design centre of the bend)
  (setq dd (* 0.5 txth))
  (kb:line (kb:sh (list (- l1 dd) (cadr c))) (kb:sh (list (+ l1 dd) (cadr c))) "KERF-BEND" 1.0)
  (kb:line (kb:sh (list l1 (- (cadr c) dd))) (kb:sh (list l1 (+ (cadr c) dd))) "KERF-BEND" 1.0)
  (kb:text (kb:sh (list minx (* -1.5 txth))) (* 1.4 txth)
           (strcat "BENT PREVIEW - inside R " (rtos r) ", outside R " (rtos ro)
                   ", " (rtos *kb:ang* 2 2) "%%d  (dashed = design arcs)")
           "KERF-NOTES")

  ;; ---------- 4. NOTES ----------
  (setq notes
    (kb:join
      (append (list "{\\LKERF BENDING PLAN}")
              (kb:mtcolor (kb:report "%%d"))
              (list "Kerf centres measured from the LEFT end of the panel:")
              (if (<= n 100) (kb:poslines) (list "(too many to list - see command line, F2)")))
      "\\P"))
  (kb:text (list 0 (- y0 (* 5.0 txth))) txth notes "KERF-NOTES")

  ;; ---------- 5. DIMENSIONS ----------
  (kb:dimstart txth)
  (kb:dim (list 0 w) (list tot w) (list 0 (+ w (* 4.5 txth))))
  (if (> l1 0) (kb:dim (list 0 w) (list l1 w) (list 0 (+ w (* 2.0 txth)))))
  (kb:dim (list l1 w) (list (+ l1 lb) w) (list l1 (+ w (* 2.0 txth))))
  (if (> l2 0) (kb:dim (list (+ l1 lb) w) (list tot w) (list tot (+ w (* 2.0 txth)))))
  (kb:dim (list 0 0) (list 0 w) (list (* -3.0 txth) 0))
  (if (>= n 2)
    (kb:dim (list (kb:pos 0) top) (list (kb:pos 1) top) (list (kb:pos 0) (+ top (* 2.0 txth)))))
  (kb:dim (list tot y0) (list tot top) (list (+ tot (* 3.0 txth)) y0))
  (kb:dimend)
  (princ)
)

;;; ====================================================================
;;; KERFPATH - plan kerfs from an existing curve
;;; ====================================================================
;;; The picked curve is one face of the wood ("Face 1"). The wood goes to
;;; the side you pick (default: the inside of the curve, so the curve is
;;; the OUTSIDE face). The opposite face is "Face 2".
;;;  - Where the curve bends toward the wood, the picked line is the
;;;    outside of the bend, so kerfs go in Face 2.
;;;  - Where it bends away from the wood (S-curves), the picked line is
;;;    the inside of that bend, so those kerfs go in Face 1.
;;;
;;; Step record:  (type turn ds dflat s F)
;;;   type 0 straight, 1 bend with kerfs on Face 2, 2 bend with kerfs on
;;;   Face 1, 3 sharp corner. s = distance along curve, F = flat distance.
;;; Kerf record:  (flatpos face pathdist turn)
;;; Bend record:  (type startF endF turn n minInsideR kerfs spMin spMax)
;;; --------------------------------------------------------------------

(defun kb:curvep (e / typ)
  (setq typ (cdr (assoc 0 (entget e))))
  (and (not (wcmatch typ "XLINE,RAY"))
       (not (vl-catch-all-error-p
              (vl-catch-all-apply 'vlax-curve-getEndParam (list e)))))
)

;; Unit tangent (2D) at distance d along the curve
(defun kb:tan (e d / p v l)
  (setq p (if (>= d kb:plen)
            (vlax-curve-getEndParam e)
            (vlax-curve-getParamAtDist e (max 0.0 d))))
  (if p (setq v (vlax-curve-getFirstDeriv e p)))
  (if v (setq l (sqrt (+ (* (car v) (car v)) (* (cadr v) (cadr v))))))
  (if (and l (> l 1e-12)) (list (/ (car v) l) (/ (cadr v) l)) nil)
)

(defun kb:ptat (e d)
  (if (>= d kb:plen)
    (vlax-curve-getEndPoint e)
    (vlax-curve-getPointAtDist e (max 0.0 d)))
)

;; Normal pointing toward the wood
(defun kb:nw (tg wside) (list (* wside (- (cadr tg))) (* wside (car tg))))

(defun kb:addv (p v s)
  (list (+ (car p) (* s (car v))) (+ (cadr p) (* s (cadr v)))
        (if (caddr p) (caddr p) 0.0))
)

;; Unit tangent (2D) at a curve parameter
(defun kb:tanp (e p / v l)
  (setq v (vl-catch-all-apply 'vlax-curve-getFirstDeriv (list e p)))
  (if (and v (not (vl-catch-all-error-p v)))
    (progn
      (setq l (sqrt (+ (* (car v) (car v)) (* (cadr v) (cadr v)))))
      (if (> l 1e-12) (list (/ (car v) l) (/ (cadr v) l)))))
)

;; Signed angle from direction t0 to direction t1 (+ = counter-clockwise)
(defun kb:ang2 (t0 t1)
  (atan (- (* (car t0) (cadr t1)) (* (cadr t0) (car t1)))
        (+ (* (car t0) (car t1)) (* (cadr t0) (cadr t1))))
)

;; Polyline vertices (where lines/arcs join): ((dist tBefore tAfter) ...)
;; Measured exactly just before and just after each vertex, so bends
;; start and stop exactly where your arcs do, and any joint that is
;; not perfectly smooth shows up as a real corner.
(defun kb:vertices (e / ep i d tb ta out)
  (if (wcmatch (cdr (assoc 0 (entget e))) "LWPOLYLINE,POLYLINE")
    (progn
      (setq ep (fix (+ 0.5 (vlax-curve-getEndParam e))) i 1)
      (while (< i ep)
        (setq d  (vlax-curve-getDistAtParam e (float i))
              tb (kb:tanp e (- i 1e-7))
              ta (kb:tanp e (+ i 1e-7)))
        (if (and d tb ta) (setq out (cons (list d tb ta) out)))
        (setq i (1+ i)))))
  (reverse out)
)

;; Walk the curve in small steps, measuring how much it turns each step.
;; Returns ((turn length startDist) ...). A corner is a step of length 0.
(defun kb:sampleraw (e / len ns ds i grid vts pts tg pa a c t0 out)
  (setq len (vlax-curve-getDistAtParam e (vlax-curve-getEndParam e)))
  (setq kb:plen len)
  (setq ns (fix (min 20000.0 (max 2000.0 (/ len (* 0.25 *kb:th*))))))
  (setq ds (/ len ns))
  ;; even grid of distances
  (setq grid nil i ns)
  (while (>= i 0) (setq grid (cons (* i ds) grid) i (1- i)))
  ;; merge in the polyline vertices: points are (dist tBefore tAfter)
  (setq vts (kb:vertices e) pts nil)
  (foreach g grid
    (while (and vts (<= (car (car vts)) g))
      (if (or (null pts) (> (car (car vts)) (+ (car (car pts)) 1e-9)))
        (setq pts (cons (car vts) pts)))
      (setq vts (cdr vts)))
    (if (or (null pts) (> (- g (car (car pts))) (* 1e-9 (max 1.0 len))))
      (setq tg (kb:tan e g) pts (cons (list g tg tg) pts))))
  (setq pts (reverse pts))
  ;; turn between neighbouring points
  (setq out nil pa (car pts))
  (foreach pb (cdr pts)
    (setq t0 (caddr pa))
    ;; corner at pa? (tiny ones under 0.06 deg are treated as smooth)
    (if (and (cadr pa) (caddr pa))
      (progn
        (setq c (kb:ang2 (cadr pa) (caddr pa)))
        (if (> (abs c) 0.001)
          (setq out (cons (list c 0.0 (car pa)) out))
          (setq t0 (cadr pa)))))
    (setq a (if (and t0 (cadr pb)) (kb:ang2 t0 (cadr pb)) 0.0))
    (setq out (cons (list a (- (car pb) (car pa)) (car pa)) out))
    (setq pa pb))
  (reverse out)
)

(defun kb:classify (raw wside / th web f out dphi a ds typ dl)
  (setq th *kb:th* web *kb:web* f 0.0 out nil)
  (foreach r raw
    (setq dphi (car r) a (abs dphi) ds (cadr r))
    (setq typ (cond ((< a 1e-7) 0)
                    ((<= ds 0.0) 3)                               ; exact corner
                    ((and (> a 0.02) (< (/ ds a) (* 0.05 th))) 3)
                    ((= (if (> dphi 0.0) 1 -1) wside) 1)
                    (T 2)))
    ;; flat length = length along the middle of the web
    (setq dl (cond ((= typ 1) (- ds (* 0.5 web a)))
                   ((= typ 2) (+ ds (* (- th (* 0.5 web)) a)))
                   (T ds)))
    (setq out (cons (list typ a ds dl (caddr r) f) out))
    (setq f (+ f dl)))
  (setq kb:flen f)
  (reverse out)
)

;; Group steps of the same type into runs: ((type step step ...) ...)
(defun kb:regions (steps / out cur ctyp)
  (setq out nil cur nil ctyp nil)
  (foreach st steps
    (if (and cur (= (car st) ctyp))
      (setq cur (cons st cur))
      (progn
        (if cur (setq out (cons (cons ctyp (reverse cur)) out)))
        (setq ctyp (car st) cur (list st)))))
  (if cur (setq out (cons (cons ctyp (reverse cur)) out)))
  (reverse out)
)

;; Work out the kerfs for one bend region
(defun kb:bendinfo (reg theta wside / typ sts phi minr r0 n j tau cum a f kerfs
                                      lst sp spmin spmax prev face sgn)
  (setq typ (car reg) sts (cdr reg) phi 0.0 minr nil
        lst (last sts) face (if (= typ 1) 2 1)
        ;; real-world turning direction: toward the wood for Face 2 kerfs
        sgn (if (= typ 1) wside (- wside)))
  (foreach st sts
    (setq a (cadr st) phi (+ phi a))
    (if (> a 0.0)
      (progn
        (setq r0 (- (/ (caddr st) a) (if (= typ 1) *kb:th* 0.0)))
        (setq minr (if minr (min minr r0) r0)))))
  (setq n (fix (/ phi theta)))
  (if (> (- (/ phi theta) n) 1e-6) (setq n (1+ n)))
  (if (< phi (* 0.1 theta)) (setq n 0))          ; too gentle to need kerfs
  (setq j 0 cum 0.0 kerfs nil)
  (if (> n 0)
    (progn
      (setq tau (* 0.5 (/ phi n)))
      (foreach st sts
        (setq a (cadr st))
        (while (and (< j n) (<= tau (+ cum a)))
          (setq f (if (> a 0.0) (/ (- tau cum) a) 0.0))
          (setq kerfs (cons (list (+ (nth 5 st) (* f (nth 3 st)))
                                  face
                                  (+ (nth 4 st) (* f (nth 2 st)))
                                  (* sgn (/ phi n)))
                            kerfs))
          (setq j (1+ j) tau (* (+ j 0.5) (/ phi n))))
        (setq cum (+ cum a)))
      ;; safety net for rounding at the very end
      (while (< j n)
        (setq kerfs (cons (list (+ (nth 5 lst) (nth 3 lst)) face
                                (+ (nth 4 lst) (nth 2 lst))
                                (* sgn (/ phi n)))
                          kerfs)
              j (1+ j)))))
  (setq kerfs (reverse kerfs))
  (setq prev nil spmin nil spmax nil)
  (foreach kf kerfs
    (if prev
      (progn
        (setq sp (- (car kf) prev))
        (setq spmin (if spmin (min spmin sp) sp)
              spmax (if spmax (max spmax sp) sp))))
    (setq prev (car kf)))
  (list typ (nth 5 (car sts)) (+ (nth 5 lst) (nth 3 lst)) phi n
        (if minr minr 0.0) kerfs spmin spmax)
)

(defun kb:preport (dg / rd out i b)
  (setq rd (/ 180.0 pi))
  (setq out
    (list
      (strcat "Material thickness: " (rtos *kb:th*))
      (strcat "Web left uncut: " (rtos *kb:web*))
      (strcat "Kerf (blade) width: " (rtos *kb:kerf*))
      (strcat "Panel width: " (rtos *kb:wid*))
      ""
      (strcat "KERF DEPTH (blade height): " (rtos (- *kb:th* *kb:web*)))
      (strcat "TOTAL KERFS: " (itoa (length *kb:pkerfs*)))
      (strcat "Length of picked curve: " (rtos kb:plen))
      (strcat "TOTAL FLAT PANEL LENGTH: " (rtos kb:flen))
      "Face 1 = the face along the picked curve.  Face 2 = the opposite face."
      "Left end of the flat panel = START end of the picked curve."
      ""))
  (setq i 0)
  (foreach b *kb:pbends*
    (setq i (1+ i))
    (setq out
      (append out
        (list
          (strcat "BEND " (itoa i) ": " (rtos (* rd (nth 3 b)) 2 2) dg
                  " - cut kerfs into FACE " (if (= (car b) 1) "2" "1")
                  (if (= (car b) 1) " (picked curve is the outside)"
                                    " (picked curve is the inside)"))
          (strcat "   " (itoa (nth 4 b)) " kerfs, from " (rtos (nth 1 b))
                  " to " (rtos (nth 2 b))
                  "; tightest inside radius " (rtos (nth 5 b)))
          (strcat "   Spacing: "
                  (cond ((null (nth 7 b)) "single kerf")
                        ((equal (nth 7 b) (nth 8 b) 1e-3) (rtos (nth 7 b)))
                        (T (strcat "varies " (rtos (nth 7 b)) " to " (rtos (nth 8 b))
                                   " (closer where the curve is tighter)"))))))))
  (setq out (append out (list "")))
  ;; warnings
  (setq i 0)
  (foreach b *kb:pbends*
    (setq i (1+ i))
    (if (<= (nth 5 b) 0.0)
      (setq out (append out (list (strcat "WARNING: bend " (itoa i)
                  " is too tight for this thickness - the inside face would pass through itself.")))))
    (if (nth 7 b)
      (if (<= (- (nth 7 b) *kb:kerf*) 0.0)
        (setq out (append out (list (strcat "WARNING: kerfs overlap in bend " (itoa i)
                    " - use a thinner blade, thinner web, or gentler curve."))))
        (if (< (- (nth 7 b) *kb:kerf*) *kb:kerf*)
          (setq out (append out (list (strcat "CAUTION: wood between kerfs in bend " (itoa i)
                      " is thinner than the blade - fragile."))))))))
  (foreach kk *kb:pkinks*
    (setq out (append out (kb:cornertext kk dg))))
  (append out (kb:commonwarn))
)

;; Full explanation of one corner (kink record: flatStart turn curveStart
;; flatEnd curveEnd). Continuation lines start with two spaces so they
;; keep the warning colour in the drawing notes.
(defun kb:cornertext (kk dg / rd c prev next lst)
  (setq rd (/ 180.0 pi) c (* rd (cadr kk)))
  ;; bends that touch this corner
  (foreach b *kb:pbends*
    (if (equal (nth 2 b) (car kk) 1e-6) (setq prev b))
    (if (equal (nth 1 b) (nth 3 kk) 1e-6) (setq next b)))
  (setq lst
    (list
      (strcat "WARNING - CORNER at " (rtos (car kk)) " from the left end of the flat panel ("
              (rtos (nth 2 kk)) " along your curve):")
      (strcat "  Your curve changes direction suddenly here by " (rtos c 2 2) dg
              ", instead of curving smoothly.")))
  (if prev
    (setq lst (append lst (list
      (strcat "  The bend just before it turns " (rtos (* rd (nth 3 prev)) 2 2) dg
              "; with the corner, the total change of direction is "
              (rtos (+ c (* rd (nth 3 prev))) 2 2) dg ".")))))
  (if next
    (setq lst (append lst (list
      (strcat "  The bend just after it turns " (rtos (* rd (nth 3 next)) 2 2) dg
              "; with the corner, the total change of direction is "
              (rtos (+ c (* rd (nth 3 next))) 2 2) dg ".")))))
  (append lst
    (list
      (strcat "  Wood can only bend smoothly, so the board would need a " (rtos c 2 2) dg
              " kink at this point. Kerfs cannot make a kink.")
      (if (< c 5.0)
        "  The corner is too small to see, but it is in the drawing: the two pieces of the curve do not meet tangent."
        "  A corner this size needs a joint, or separate pieces of wood.")
      "  The bends on each side are planned separately and the corner is left out of them."
      "  The bent preview is split into separate pieces here, and the corner is circled in red."))
)

;; Colour warning lines for MText: WARNING red, CAUTION orange.
;; Lines starting with two spaces keep the colour of the line above.
(defun kb:mtcolor (lst / out col)
  (foreach s lst
    (cond ((wcmatch s "WARNING*") (setq col "1"))
          ((wcmatch s "CAUTION*") (setq col "30"))
          ((and col (wcmatch s "  *")))
          (T (setq col nil)))
    (setq out (cons (if col (strcat "{\\C" col ";" s "}") s) out)))
  (reverse out)
)

(defun kb:ppos ()
  (kb:listlines
    (mapcar '(lambda (kf) (strcat (rtos (car kf)) " F" (itoa (cadr kf)))) *kb:pkerfs*))
)

(defun kb:pprint ()
  (princ "\n\n=========== KERF BENDING PLAN - FROM CURVE ===========")
  (foreach s (kb:preport " deg") (princ (strcat "\n  " s)))
  (princ "\n  Kerf centres from the LEFT end of the panel (F1/F2 = face to cut):")
  (foreach s (kb:ppos) (princ (strcat "\n    " s)))
  (princ "\n======================================================")
  (princ "\n  (Press F2 to see the full list in the text window.)\n")
  (princ)
)

;; Draw the real bent board on top of the picked curve.
;; The board is "bent" from its start end, kerf by kerf, exactly as the
;; flat layout is cut. A sharp corner can't bend, so the board is split
;; there and the next piece starts again on the curve.
(defun kb:drawinplace (e wside / segs xa sa xb zones p tg prof hw txh cr)
  (kb:mklayers)
  (setq kb:bx 0.0 kb:by 0.0 hw (* 0.5 *kb:kerf*))
  ;; pieces between sharp corners: (flatStart curveStart flatEnd)
  (setq xa 0.0 sa 0.0 segs nil)
  (foreach kk *kb:pkinks*
    (setq segs (cons (list xa sa (car kk)) segs)
          xa (nth 3 kk) sa (nth 4 kk)))
  (setq segs (reverse (cons (list xa sa kb:flen) segs)))
  (foreach sg segs
    (setq xa (car sg) xb (caddr sg))
    (if (> xb xa)
      (progn
        (setq zones nil)
        (foreach kf *kb:pkerfs*
          (if (and (>= (car kf) xa) (<= (car kf) xb))
            (setq zones
              (cons (list (- (car kf) hw) (+ (car kf) hw) (nth 3 kf)
                          (if (= (cadr kf) 2)
                            (* 0.5 *kb:web*)
                            (- *kb:th* (* 0.5 *kb:web*)))
                          (cadr kf))
                    zones))))
        (setq p  (kb:ptat e (cadr sg))
              tg (kb:tan e (cadr sg)))
        (if (null tg) (setq tg '(1.0 0.0)))
        (setq prof (kb:bentprofile (reverse zones) xa xb
                                   (list (car p) (cadr p))
                                   (atan (cadr tg) (car tg)) wside))
        (kb:lwp (car prof) (cadr prof) T "KERF-PREVIEW"))))
  ;; dashed DESIGN outline: your curve is one face, so add the other
  ;; face (your curve copied one thickness toward the wood) and the ends
  (kb:designface e wside)
  ;; circle every corner in red, with a short label
  (setq txh (/ (max kb:flen *kb:wid*) 50.0)
        cr  (max (* 1.5 *kb:th*) txh))
  (foreach kk *kb:pkinks*
    (setq p (kb:ptat e (nth 2 kk)))
    (if p
      (progn
        (entmake (list '(0 . "CIRCLE") '(8 . "KERF-WARN") (cons 10 p) (cons 40 cr)))
        (kb:text (list (+ (car p) cr) (+ (cadr p) cr)) txh
                 (strcat "CORNER " (rtos (* (/ 180.0 pi) (cadr kk)) 2 2)
                         "%%d - can't be kerf-bent (see notes)")
                 "KERF-WARN"))))
)

;; Offset helper - returns list of new objects or nil
(defun kb:offset (obj dist / r)
  (setq r (vl-catch-all-apply 'vla-Offset (list obj dist)))
  (if (vl-catch-all-error-p r)
    nil
    (vlax-safearray->list (vlax-variant-value r)))
)

(defun kb:mindist (objs ref / best dd)
  (foreach o objs
    (setq dd (distance ref (vlax-curve-getClosestPointTo o ref)))
    (setq best (if best (min best dd) dd)))
  best
)

(defun kb:delall (objs) (foreach o objs (vl-catch-all-apply 'vla-Delete (list o))))

(defun kb:designface (e wside / obj lts mid tg ref ra rb da db keep p)
  (setq obj (vlax-ename->vla-object e)
        lts (kb:ltscale (/ (max kb:flen *kb:wid*) 50.0)))
  (setq mid (kb:ptat e (* 0.5 kb:plen))
        tg  (kb:tan e (* 0.5 kb:plen)))
  (if (null tg) (setq tg '(1.0 0.0)))
  (setq ref (kb:addv mid (kb:nw tg wside) (* 0.5 *kb:th*)))
  ;; offset both ways, keep the copy on the wood side
  (setq ra (kb:offset obj *kb:th*)
        rb (kb:offset obj (- *kb:th*)))
  (setq da (if ra (kb:mindist ra ref))
        db (if rb (kb:mindist rb ref)))
  (cond ((and da (or (null db) (<= da db))) (setq keep ra) (kb:delall rb))
        (db (setq keep rb) (kb:delall ra))
        (T (princ "\n  (Could not draw the dashed design face - curve too tight for this thickness.)")))
  (foreach o keep
    (vla-put-Layer o "KERF-BEND")
    (vla-put-Linetype o "ByLayer")
    (vla-put-LinetypeScale o lts)
    (if (vlax-property-available-p o 'LinetypeGeneration T)
      (vla-put-LinetypeGeneration o :vlax-true)))
  ;; ends
  (if (not (vlax-curve-isClosed e))
    (foreach d (list 0.0 kb:plen)
      (setq p (kb:ptat e d) tg (kb:tan e d))
      (if (and p tg)
        (entmake (list '(0 . "LINE") '(8 . "KERF-BEND") (cons 48 lts)
                       (cons 10 p) (cons 11 (kb:addv p (kb:nw tg wside) *kb:th*)))))))
)

;; Draw the flat cut layout at the picked point
(defun kb:drawflat (bp / tot w k k2 d th txth gap lts x y0 top pts bnds i)
  (setq kb:bx (car bp) kb:by (cadr bp))
  (setq tot kb:flen w *kb:wid* k *kb:kerf* k2 (/ *kb:kerf* 2.0)
        th *kb:th* d (- *kb:th* *kb:web*))
  (setq txth (/ (max tot w) 50.0)
        gap  (max (* 8.0 txth) (* 0.3 w))
        lts  (kb:ltscale txth))
  (kb:mklayers)

  ;; ---------- PLAN VIEW ----------
  (kb:rect 0 0 tot w "KERF-OUTLINE")
  (foreach kf *kb:pkerfs*
    (setq x (car kf))
    (kb:rect (- x k2) 0 (+ x k2) w (if (= (cadr kf) 2) "KERF-CUTS" "KERF-CUTS-F1")))
  (foreach b *kb:pbends*
    (kb:line (list (nth 1 b) (- txth)) (list (nth 1 b) (+ w txth)) "KERF-BEND" lts)
    (kb:line (list (nth 2 b) (- txth)) (list (nth 2 b) (+ w txth)) "KERF-BEND" lts))
  (foreach kk *kb:pkinks*
    (kb:line (list (car kk) (* -2.0 txth)) (list (car kk) (+ w (* 2.0 txth))) "KERF-WARN" 1.0)
    (kb:text (list (+ (car kk) (* 0.3 txth)) (* -1.0 txth)) (* 0.8 txth)
             "CORNER - see notes" "KERF-WARN"))
  (kb:text (list 0 (+ w (* 8.0 txth))) (* 1.4 txth)
           "PLAN VIEW - CUT LAYOUT (red = cut Face 2, magenta = cut Face 1, bends dashed)"
           "KERF-NOTES")

  ;; ---------- FLAT SIDE VIEW (top = Face 2, bottom = Face 1) ----------
  (setq y0 (- (+ gap th)) top (+ y0 th))
  (setq pts (list (list 0 y0)))
  (foreach kf *kb:pkerfs*
    (if (= (cadr kf) 1)
      (setq x (car kf)
            pts (append pts (list (list (- x k2) y0) (list (- x k2) (+ y0 d))
                                  (list (+ x k2) (+ y0 d)) (list (+ x k2) y0))))))
  (setq pts (append pts (list (list tot y0) (list tot top))))
  (foreach kf (reverse *kb:pkerfs*)
    (if (= (cadr kf) 2)
      (setq x (car kf)
            pts (append pts (list (list (+ x k2) top) (list (+ x k2) (- top d))
                                  (list (- x k2) (- top d)) (list (- x k2) top))))))
  (setq pts (append pts (list (list 0 top))))
  (setq pts (kb:dedupe pts (mapcar '(lambda (p) 0) pts)))
  (kb:lwp (car pts) (cadr pts) T "KERF-OUTLINE")
  (kb:text (list 0 (- y0 (* 2.0 txth))) (* 1.4 txth)
           "SIDE VIEW - FLAT (top = Face 2, bottom = Face 1)" "KERF-NOTES")

  ;; ---------- NOTES ----------
  (kb:text (list 0 (- y0 (* 5.0 txth))) txth
    (kb:join
      (append (list "{\\LKERF BENDING PLAN - FROM CURVE}")
              (kb:mtcolor (kb:preport "%%d"))
              (list "Kerf centres from the LEFT end of the panel (F1/F2 = face to cut):")
              (if (<= (length *kb:pkerfs*) 200)
                (kb:ppos)
                (list "(too many to list - see command line, F2)")))
      "\\P")
    "KERF-NOTES")

  ;; ---------- DIMENSIONS ----------
  (kb:dimstart txth)
  (kb:dim (list 0 w) (list tot w) (list 0 (+ w (* 4.5 txth))))
  (setq bnds (list 0.0))
  (foreach b *kb:pbends* (setq bnds (append bnds (list (nth 1 b) (nth 2 b)))))
  (setq bnds (append bnds (list tot)))
  (setq i 0)
  (while (< i (1- (length bnds)))
    (if (> (- (nth (1+ i) bnds) (nth i bnds)) 1e-6)
      (kb:dim (list (nth i bnds) w) (list (nth (1+ i) bnds) w)
              (list (nth i bnds) (+ w (* 2.0 txth)))))
    (setq i (1+ i)))
  (kb:dim (list 0 0) (list 0 w) (list (* -3.0 txth) 0))
  (kb:dim (list tot y0) (list tot top) (list (+ tot (* 3.0 txth)) y0))
  (kb:dimend)
  (princ)
)

;;; --------------------------------------------------------------------
;;; Commands
;;; --------------------------------------------------------------------
(defun kb:err (msg)
  (kb:finish)
  (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*,*BREAK*")))
    (princ (strcat "\nKerf bend error: " msg)))
  (princ)
)

(defun c:KERFBEND (/ *error* bp)
  (setq *error* kb:err kb:started nil kb:ucsset nil)
  (kb:ask)
  (kb:calc)
  (kb:print)
  (initget 1)
  (setq bp (trans (getpoint "\nPick the lower-left corner for the drawing: ") 1 0))
  (kb:start)
  (kb:draw bp)
  (kb:finish)
  (princ "\nKerf bend drawing done. Use ZOOM > Extents if you can't see it all.")
  (princ)
)

(defun c:KERFCALC (/ *error*)
  (setq *error* kb:err kb:started nil kb:ucsset nil)
  (kb:ask)
  (kb:calc)
  (kb:print)
)

(defun c:KERFPATH (/ *error* es e pt cp tg wside raw tsum steps theta bp)
  (setq *error* kb:err kb:started nil kb:ucsset nil)
  (kb:defaults)
  (princ "\n--- Kerf bending from an existing curve ---")
  ;; 1. pick the curve
  (setq e nil)
  (while (not e)
    (setvar "ERRNO" 0)
    (setq es (entsel "\nSelect the line/arc/polyline/spline that is the OUTSIDE face of the bent wood: "))
    (cond
      ((and (null es) (= (getvar "ERRNO") 52)) (exit))
      ((null es) (princ "\n  Nothing picked - try again."))
      ((kb:curvep (car es)) (setq e (car es)))
      (T (princ "\n  That can't be used. Pick a line, arc, circle, polyline, ellipse or spline."))))
  (redraw e 3)
  ;; 2. material
  (princ "\n(Enter = keep value in < >)")
  (kb:askmat)
  (setq *kb:wid* (kb:getlen "Panel width (the length of each kerf cut)" *kb:wid* 6))
  ;; 3. measure the curve
  (princ "\nMeasuring the curve...")
  (setq raw (kb:sampleraw e))
  (setq tsum (apply '+ (mapcar 'car raw)))
  ;; 4. which side is the wood on?
  (setq pt (getpoint "\nPick a point on the side where the wood goes <Enter = inside of the curve>: "))
  (redraw e 4)
  (if pt
    (progn
      (setq pt (trans pt 1 0)
            cp (vlax-curve-getClosestPointTo e pt)
            tg (kb:tan e (vlax-curve-getDistAtPoint e cp)))
      (if (null tg) (setq tg '(1.0 0.0)))
      (setq wside (if (> (- (* (car tg) (- (cadr pt) (cadr cp)))
                            (* (cadr tg) (- (car pt) (car cp))))
                         0.0)
                    1 -1)))
    (setq wside (if (>= tsum 0.0) 1 -1)))
  ;; 5. work out the bends and kerfs
  (setq steps (kb:classify raw wside))
  (setq theta (/ *kb:kerf* (- *kb:th* (* 0.5 *kb:web*))))
  (setq *kb:pbends* nil *kb:pkinks* nil)
  (foreach rg (kb:regions steps)
    (cond
      ((member (car rg) '(1 2))
        (setq *kb:pbends* (cons (kb:bendinfo rg theta wside) *kb:pbends*)))
      ((= (car rg) 3)
        ;; (flatStart turn curveStart flatEnd curveEnd)
        (setq *kb:pkinks*
          (cons (list (nth 5 (cadr rg))
                      (apply '+ (mapcar 'cadr (cdr rg)))
                      (nth 4 (cadr rg))
                      (+ (nth 5 (last rg)) (nth 3 (last rg)))
                      (+ (nth 4 (last rg)) (nth 2 (last rg))))
                *kb:pkinks*)))))
  (setq *kb:pbends* (vl-remove-if '(lambda (b) (= 0 (nth 4 b))) (reverse *kb:pbends*))
        *kb:pkinks* (reverse *kb:pkinks*))
  (setq *kb:pkerfs* (apply 'append (mapcar '(lambda (b) (nth 6 b)) *kb:pbends*)))
  (if (null *kb:pbends*)
    (progn
      (princ "\nNo curves that need kerfs were found - this piece is flat (or only gently curved).")
      (if *kb:pkinks* (princ "\nIt only has sharp corners, which need joints, not kerfs."))
      (exit)))
  (kb:pprint)
  ;; 6. draw
  (initget 1)
  (setq bp (trans (getpoint "\nPick the lower-left corner for the FLAT cut layout: ") 1 0))
  (kb:start)
  (kb:drawinplace e wside)
  (kb:drawflat bp)
  (kb:finish)
  (princ "\nDone. Real bent board drawn on your curve (green), design outline dashed (cyan),")
  (princ "\nflat layout at the picked point.")
  (princ "\nUse ZOOM > Extents if you can't see it all.")
  (princ)
)

(defun c:KERFHELP ()
  (princ "\n--- KERF BENDING HELP ---")
  (princ "\n KERFBEND : type in a bend (inside OR outside radius) and draw it.")
  (princ "\n KERFPATH : pick an existing line/arc/polyline/spline as the OUTSIDE")
  (princ "\n            face of the bent wood; plans and draws all its bends.")
  (princ "\n KERFCALC : plan only - prints depth, count, spacing and positions.")
  (princ "\n Answers you type are remembered for the next run.")
  (princ "\n Web     = wood left uncut under each kerf. Plywood: leave at least one ply.")
  (princ "\n Inside radius  = the face WITH the kerfs.")
  (princ "\n Outside radius = the face WITHOUT kerfs (inside + thickness).")
  (princ "\n Kerf    = width of your saw blade or bit (measure a real test cut).")
  (princ "\n Kerf depth = set your blade height to this number.")
  (princ "\n Face 1 / Face 2 (KERFPATH): Face 1 runs along your picked curve.")
  (princ "\n Always cut a test strip first - real wood varies.")
  (princ)
)

(princ "\nKerfBend loaded. Commands: KERFBEND, KERFPATH, KERFCALC, KERFHELP")
(princ)
