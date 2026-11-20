;;; ------------------------------------------------------------------
;;; VCPREP.lsp  -  Step-by-step cleanup of a drawing for VCarve import
;;;
;;; Command:
;;;   VCPREP    walks through the cleanup one step at a time, highlights
;;;             what it found, and asks before changing anything
;;;
;;; Steps:
;;;   1. Explode and flatten
;;;        - explodes blocks, regions and multilines (nested blocks too)
;;;        - turns splines, ellipses and old-style / 3D polylines into
;;;          plain polylines VCarve handles well
;;;        - puts everything at Z=0 and flips "upside-down" arcs and
;;;          polylines (drawn with a mirrored or flipped UCS) right side up
;;;        - removes polyline widths
;;;   2. Remove junk and duplicates
;;;        - dimensions, hatches, leaders, fills, images, 3D objects
;;;        - text (delete it, or keep it on the ENGRAVE layer)
;;;        - tiny pieces shorter than the gap size
;;;        - dashed / center / hidden lines (optional, default keep)
;;;        - lines stacked on top of each other (any layer), then OVERKILL
;;;          to merge lines that partly overlap
;;;   3. Join and close
;;;        - joins loose lines, arcs and polylines whose ends are within
;;;          the gap size, and closes outlines that almost meet
;;;        - marks the ends of anything still open with red circles
;;;          (layer VCPREP-CHECK) and lets you close them, retry with a
;;;          bigger gap, leave them open, or stop and fix them by hand
;;;   4. Sort onto cut layers (or skip if the layers are already good)
;;;        - guesses: outer edge of a part = PROFILE-OUTSIDE, shapes inside
;;;          a part = PROFILE-INSIDE or POCKET, small circles = DRILL,
;;;          open lines = ON-LINE; you confirm or pick each group
;;;   Then saves only the cleaned shapes to a separate DXF (R2000 format),
;;;   moved so the lower-left corner of the parts sits at 0,0.
;;;
;;; Copy or Selected:
;;;   Copy (default) makes a copy of the selection beside the original and
;;;   cleans the copy; your original linework is not changed. At the end
;;;   you choose whether to keep the cleaned copy in the drawing.
;;;   Selected cleans the selected objects themselves (use this to carry
;;;   on with a copy you already partly cleaned).
;;;
;;; Input: sizes are in drawing units; type ' for feet and " for inches
;;; (1/64"  0.5"  1') in any drawing. Defaults scale from INSUNITS.
;;;
;;; Housekeeping:
;;;   - the whole run is one UNDO step (type U once to undo all of it)
;;;   - works in the World UCS and puts your UCS back afterwards
;;;   - locked layers are unlocked while working and locked again after
;;;   - cut layer names/colours can be changed in *vp:laydefs* below
;;; ------------------------------------------------------------------

(vl-load-com)

;; Cut layers: key, layer name, colour. Edit the names to match the
;; layer names you like to use in VCarve.
(setq *vp:laydefs*
  '((OUT     "PROFILE-OUTSIDE" 1)
    (IN      "PROFILE-INSIDE"  5)
    (POCKET  "POCKET"          3)
    (DRILL   "DRILL"           6)
    (OPEN    "ON-LINE"         4)
    (ENGRAVE "ENGRAVE"         2)))

(setq *vp:checklayer* "VCPREP-CHECK")

;; ---------- small helpers ----------

(defun vp:doc () (vla-get-activedocument (vlax-get-acad-object)))
(defun vp:ln (key) (cadr (assoc key *vp:laydefs*)))
(defun vp:lc (key) (caddr (assoc key *vp:laydefs*)))
(defun vp:alive (e) (and e (entget e)))
(defun vp:typ (e) (cdr (assoc 0 (entget e))))
(defun vp:obj (e) (vlax-ename->vla-object e))

(defun vp:uf ( / u)
  (setq u (getvar "INSUNITS"))
  (cond ((= u 4) 25.4)
        ((= u 5) 2.54)
        ((= u 6) 0.0254)
        ((= u 2) (/ 1.0 12.0))
        (T 1.0)))

(defun vp:n (n word)
  (strcat (itoa n) " " word (if (= n 1) "" "s")))

(defun vp:joinstr (lst sep / s)
  (setq s "")
  (foreach x lst
    (setq s (if (= s "") x (strcat s sep x))))
  s)

;; Replace group CODE in entity list ED, or add it if missing.
(defun vp:put (ed code val)
  (if (assoc code ed)
    (subst (cons code val) (assoc code ed) ed)
    (append ed (list (cons code val)))))

(defun vp:ss (lst / ss)
  (setq ss (ssadd))
  (foreach e lst (if (vp:alive e) (ssadd e ss)))
  ss)

(defun vp:sslist (ss / i l)
  (if ss
    (repeat (setq i (sslength ss))
      (setq l (cons (ssname ss (setq i (1- i))) l))))
  l)

(defun vp:cancel ()
  (while (> (getvar "CMDACTIVE") 0) (command)))

;; Keyword prompt. KWS like "Fix Skip Stop"; returns the keyword or DEF.
(defun vp:ask (msg kws def / r)
  (initget kws)
  (setq r (getkword (strcat "\n" msg " [" (vl-string-translate " " "/" kws)
                            "] <" def ">: ")))
  (if r r def))

;; ---------- size input (same rules as HFLAT) ----------

(defun vp:num (s)
  (cond ((= s "") 0.0)
        ((distof s 2))
        ((distof s 4))
        ((distof s 5))))

(defun vp:parselen (s / i ft in)
  (setq s (vl-string-trim " " s))
  (cond
    ((setq i (vl-string-search "'" s))
     (setq ft (vp:num (vl-string-trim " " (substr s 1 i)))
           in (vp:num (vl-string-trim " -\"" (substr s (+ i 2)))))
     (if (and ft in) (* (+ (* 12.0 ft) in) (vp:uf))))
    ((wcmatch s "*\"")
     (if (setq in (vp:num (vl-string-trim " \"" s)))
       (* in (vp:uf))))))

(defun vp:getnum (msg def / v r again)
  (setq again T)
  (while again
    (initget 134)
    (setq v (getdist (strcat "\n" msg " <" (rtos def) ">: ")))
    (cond ((null v) (setq r def again nil))
          ((numberp v) (setq r v again nil))
          ((setq r (vp:parselen v))
           (if (> r 0)
             (setq again nil)
             (princ "\nValue must be greater than zero.")))
          (T (princ "\nEnter a number, or inches/feet like 1/64\" or 1'."))))
  r)

;; ---------- setup / restore ----------

(defun vp:old (v) (cdr (assoc v *vp:vars*)))

(defun vp:begin ( / ctl)
  (setq *vp:vars* nil *vp:lck* nil *vp:lstate* nil *vp:ucs* nil
        *vp:undo* nil *vp:hl* nil *vp:marks* nil *vp:work* nil)
  (foreach v '("CMDECHO" "OSMODE" "3DOSMODE" "CLAYER" "PEDITACCEPT"
               "PLINETYPE" "PLINECONVERTMODE" "FILEDIA" "HIGHLIGHT")
    (if (getvar v) (setq *vp:vars* (cons (cons v (getvar v)) *vp:vars*))))
  (setvar "CMDECHO" 0)
  (setq ctl (getvar "UNDOCTL"))
  (if (= 1 (logand ctl 3))
    (progn (command "._UNDO" "_BE") (setq *vp:undo* T)))
  (setvar "OSMODE" 0)
  (if (getvar "3DOSMODE") (setvar "3DOSMODE" 0))
  (if (getvar "PEDITACCEPT") (setvar "PEDITACCEPT" 1))   ; PEDIT takes lines/arcs without asking
  (setvar "PLINETYPE" 2)                                 ; lightweight polylines
  (if (getvar "PLINECONVERTMODE") (setvar "PLINECONVERTMODE" 1)) ; splines -> arcs, not short lines
  (setvar "HIGHLIGHT" 1)
  (command "._UCS" "_W")
  (setq *vp:ucs* T)
  ;; unlock every layer for now; vp:restore locks them again
  (vlax-for l (vla-get-layers (vp:doc))
    (if (= (vla-get-lock l) :vlax-true)
      (progn (vla-put-lock l :vlax-false)
             (setq *vp:lck* (cons l *vp:lck*))))))

;; Put back the user's UCS, settings and layers. Safe to call twice.
(defun vp:restore ()
  (vp:unhl)
  (if *vp:ucs*
    (progn (command "._UCS" "_P") (setq *vp:ucs* nil)))
  (foreach p *vp:vars*
    (if (/= (car p) "CMDECHO")
      (vl-catch-all-apply 'setvar (list (car p) (cdr p)))))
  (foreach s *vp:lstate*
    (vl-catch-all-apply 'vla-put-layeron (list (car s) (caddr s)))
    (vl-catch-all-apply 'vla-put-freeze (list (car s) (cadr s))))
  (foreach l *vp:lck*
    (vl-catch-all-apply 'vla-put-lock (list l :vlax-true)))
  (setq *vp:lstate* nil *vp:lck* nil))

(defun vp:finish ()
  (if *vp:undo*
    (progn (command "._UNDO" "_E") (setq *vp:undo* nil)))
  (if (vp:old "CMDECHO") (setvar "CMDECHO" (vp:old "CMDECHO")))
  (setq *vp:vars* nil))

(defun vp:err (m)
  (vp:cancel)
  (vp:restore)
  (vp:finish)
  (if (and m (not (wcmatch (strcase m) "*CANCEL*,*QUIT*,*EXIT*")))
    (princ (strcat "\nError: " m)))
  (princ "\nVCPREP stopped. Type U to undo the whole run if you want.")
  (princ))

;; Make sure a cut layer exists, is on and thawed. Returns its name.
(defun vp:mklayer (key / nm lays lay)
  (setq nm   (vp:ln key)
        lays (vla-get-layers (vp:doc)))
  (if (tblsearch "LAYER" nm)
    (progn
      (setq lay (vla-item lays nm))
      (if (not (assoc lay *vp:lstate*))
        (setq *vp:lstate* (cons (list lay (vla-get-freeze lay) (vla-get-layeron lay))
                                *vp:lstate*)))
      (if (= (vla-get-freeze lay) :vlax-true)
        (vl-catch-all-apply 'vla-put-freeze (list lay :vlax-false)))
      (vla-put-layeron lay :vlax-true))
    (progn
      (setq lay (vla-add lays nm))
      (vla-put-color lay (vp:lc key))))
  nm)

(defun vp:checklay ( / lays lay)
  (setq lays (vla-get-layers (vp:doc)))
  (if (tblsearch "LAYER" *vp:checklayer*)
    (progn
      (setq lay (vla-item lays *vp:checklayer*))
      (vl-catch-all-apply 'vla-put-freeze (list lay :vlax-false))
      (vla-put-layeron lay :vlax-true))
    (progn
      (setq lay (vla-add lays *vp:checklayer*))
      (vla-put-color lay 1)))
  *vp:checklayer*)

;; ---------- working set ----------

;; Throwaway point used as a bookmark: everything created after it is new.
(defun vp:bookmark ()
  (vp:checklay)
  (entmake (list '(0 . "POINT") (cons 8 *vp:checklayer*) '(10 0.0 0.0 0.0)))
  (entlast))

;; Add everything created after bookmark M to the working set, delete M.
;; Returns the new entities.
(defun vp:adopt (m / e new)
  (setq e (entnext m))
  (while e
    (if (and (not (member (vp:typ e) '("VERTEX" "ATTRIB" "SEQEND")))
             (/= (cdr (assoc 8 (entget e))) *vp:checklayer*))
      (setq new (cons e new)))
    (setq e (entnext e)))
  (entdel m)
  (setq *vp:work* (append *vp:work* (reverse new)))
  (vp:clean)
  (reverse new))

(defun vp:clean ()
  (setq *vp:work* (vl-remove-if-not 'vp:alive *vp:work*)))

(defun vp:hl (lst)
  (vp:unhl)
  (foreach e lst (if (vp:alive e) (redraw e 3)))
  (setq *vp:hl* lst))

(defun vp:unhl ()
  (foreach e *vp:hl* (if (vp:alive e) (redraw e 4)))
  (setq *vp:hl* nil))

(defun vp:bbox (lst / mn mx a b r)
  (foreach e lst
    (setq r (vl-catch-all-apply 'vla-getboundingbox (list (vp:obj e) 'a 'b)))
    (if (not (vl-catch-all-error-p r))
      (progn
        (setq a (vlax-safearray->list a)
              b (vlax-safearray->list b))
        (setq mn (if mn (mapcar 'min mn a) a)
              mx (if mx (mapcar 'max mx b) b)))))
  (if mn (list mn mx)))

(defun vp:zoom (lst / bb m)
  (if (setq bb (vp:bbox lst))
    (progn
      (setq m (* 0.05 (max (- (car (cadr bb)) (car (car bb)))
                           (- (cadr (cadr bb)) (cadr (car bb)))
                           (* 0.1 (vp:uf)))))
      (command "._ZOOM" "_W"
               (list (- (car (car bb)) m) (- (cadr (car bb)) m))
               (list (+ (car (cadr bb)) m) (+ (cadr (cadr bb)) m))))))

(defun vp:move-all (from to / p0 p1)
  (setq p0 (vlax-3d-point from) p1 (vlax-3d-point to))
  (foreach e *vp:work*
    (vl-catch-all-apply 'vla-move (list (vp:obj e) p0 p1))))

;; Selection -> working set. In Copy mode the objects are copied to the
;; right of the original and the copies become the working set.
(defun vp:setup (ss mode / lst bb dx p0 p1 o)
  (foreach e (vp:sslist ss)
    (if (and (/= 1 (cdr (assoc 67 (entget e))))           ; model space only
             (/= "VIEWPORT" (vp:typ e))
             (/= *vp:checklayer* (cdr (assoc 8 (entget e)))))
      (setq lst (cons e lst))))
  (setq lst (reverse lst))
  (if (= mode "Copy")
    (progn
      (setq bb (vp:bbox lst)
            dx (if bb (- (car (cadr bb)) (car (car bb))) 0.0)
            dx (+ dx (max (* 0.25 dx) (* 12.0 (vp:uf))))
            p0 (vlax-3d-point '(0.0 0.0 0.0))
            p1 (vlax-3d-point (list dx 0.0 0.0)))
      (foreach e lst
        (setq o (vl-catch-all-apply 'vla-copy (list (vp:obj e))))
        (if (not (vl-catch-all-error-p o))
          (progn
            (vla-move o p0 p1)
            (setq *vp:work* (cons (vlax-vla-object->ename o) *vp:work*)))))
      (setq *vp:work* (reverse *vp:work*))
      (princ (strcat "\nMade a copy of " (vp:n (length *vp:work*) "object")
                     " to the right of the original. Cleaning the copy.")))
    (progn
      (setq *vp:work* lst)
      (princ (strcat "\nCleaning " (vp:n (length lst) "selected object") " directly."))))
  *vp:work*)

;; ---------- curve helpers ----------

(defun vp:curvep (e)
  (member (vp:typ e) '("LINE" "ARC" "CIRCLE" "LWPOLYLINE" "ELLIPSE" "SPLINE")))

(defun vp:closedp (e / r)
  (setq r (vl-catch-all-apply 'vlax-curve-isClosed (list e)))
  (and (not (vl-catch-all-error-p r)) r))

(defun vp:len (e / r)
  (setq r (vl-catch-all-apply
            '(lambda () (vlax-curve-getDistAtParam e (vlax-curve-getEndParam e)))))
  (if (vl-catch-all-error-p r) nil r))

(defun vp:2d (p) (list (car p) (cadr p)))

;; Split an LWPOLYLINE entity list into (header vertices tail).
;; Each vertex is (point start-width end-width bulge).
(defun vp:lw-parse (ed / hdr vs tail v seen c)
  (foreach g ed
    (setq c (car g))
    (cond ((= c 10)
           (if v (setq vs (cons v vs)))
           (setq v (list (cdr g) 0.0 0.0 0.0) seen T))
          ((and seen (= c 40)) (setq v (list (car v) (cdr g) (caddr v) (cadddr v))))
          ((and seen (= c 41)) (setq v (list (car v) (cadr v) (cdr g) (cadddr v))))
          ((and seen (= c 42)) (setq v (list (car v) (cadr v) (caddr v) (cdr g))))
          ((and seen (= c 91)) nil)
          (seen (setq tail (cons g tail)))
          (T (setq hdr (cons g hdr)))))
  (if v (setq vs (cons v vs)))
  (list (reverse hdr) (reverse vs) (reverse tail)))

(defun vp:lw-build (hdr vs tail closed / fl)
  (setq fl  (cdr (assoc 70 hdr))
        fl  (if fl fl 0)
        hdr (vp:put hdr 90 (length vs))
        hdr (vp:put hdr 70 (if closed (logior fl 1) (logand fl (~ 1)))))
  (append hdr
          (apply 'append
                 (mapcar '(lambda (v)
                            (list (cons 10 (vp:2d (car v)))
                                  (cons 40 (cadr v))
                                  (cons 41 (caddr v))
                                  (cons 42 (cadddr v))))
                         vs))
          tail))

;; New flat LWPOLYLINE at Z=0 with the layer/colour/linetype of SRC.
;; PTS are WCS points, BULS a matching list of bulges (or nil).
(defun vp:mkpoly (src pts buls closed / ed props)
  (setq ed    (entget src)
        props (vl-remove-if-not '(lambda (g) (member (car g) '(6 62 370 420))) ed))
  (if (null buls) (setq buls (mapcar '(lambda (p) 0.0) pts)))
  (if (entmake
        (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (assoc 8 ed))
                props
                (list '(100 . "AcDbPolyline")
                      (cons 90 (length pts))
                      (cons 70 (if closed 1 0))
                      '(38 . 0.0))
                (apply 'append
                       (mapcar '(lambda (p b) (list (cons 10 (vp:2d p)) (cons 42 b)))
                               pts buls))
                (list '(210 0.0 0.0 1.0))))
    (entlast)))

;; Replace a curve (ellipse, or a spline AutoCAD couldn't convert) with a
;; polyline traced along it in short straight steps.
(defun vp:trace (e / len cl n i pts p new)
  (if (and (setq len (vp:len e)) (> len 0))
    (progn
      (setq cl (vp:closedp e)
            n  (max 64 (min 2000 (fix (/ len (* 0.05 (vp:uf))))))
            i  0)
      (repeat (if cl n (1+ n))
        (setq p (if (= i n)
                  (vlax-curve-getEndPoint e)
                  (vlax-curve-getPointAtDist e (* len (/ (float i) n)))))
        (if p (setq pts (cons p pts)))
        (setq i (1+ i)))
      (if (and (> (length pts) 1)
               (setq new (vp:mkpoly e (reverse pts) nil cl)))
        (progn (entdel e) new)))))

;; Old-style 2D or 3D POLYLINE -> flat LWPOLYLINE. Returns the new entity,
;; 'tilt if it is drawn in a tilted plane, nil for meshes.
(defun vp:poly->lw (e / hdr fl is3d n nz elev v vd p b pts buls new)
  (setq hdr (entget e)
        fl  (cdr (assoc 70 hdr))
        is3d (= 8 (logand fl 8))
        n   (cdr (assoc 210 hdr))
        nz  (if n (caddr n) 1.0)
        elev (caddr (cdr (assoc 10 hdr))))
  (cond
    ((/= 0 (logand fl 80)) nil)                              ; polyface / mesh
    ((and (not is3d) (< (abs nz) 0.999999)) 'tilt)
    (T
     (setq v (entnext e))
     (while (and v (= "VERTEX" (vp:typ v)))
       (setq vd (entget v))
       (if (= 0 (logand (cdr (assoc 70 vd)) 16))             ; skip spline frame points
         (progn
           (setq p (cdr (assoc 10 vd))
                 b (cdr (assoc 42 vd))
                 b (if b b 0.0))
           (if is3d
             (setq b 0.0)
             (setq p (trans (list (car p) (cadr p) elev) e 0)
                   b (if (< nz 0) (- b) b)))
           (setq pts (cons p pts) buls (cons b buls))))
       (setq v (entnext v)))
     (if (and (> (length pts) 1)
              (setq new (vp:mkpoly e (reverse pts) (reverse buls) (= 1 (logand fl 1)))))
       (progn (entdel e) new)))))

;; Flatten one object to Z=0 in the World plane. With DRY set, only reports.
;; Returns T if it needs/needed a change, 'tilt if it is in a tilted plane.
(defun vp:flatten (e dry / ed typ n nz p1 p2 c cw sp ep need elev parts hdr vs tail)
  (setq ed  (entget e)
        typ (cdr (assoc 0 ed))
        n   (cdr (assoc 210 ed))
        nz  (if n (caddr n) 1.0))
  (cond
    ((= typ "LINE")
     (setq p1 (cdr (assoc 10 ed)) p2 (cdr (assoc 11 ed))
           need (or (> (abs (caddr p1)) 1e-9) (> (abs (caddr p2)) 1e-9)))
     (if (and need (not dry))
       (entmod (vp:put (vp:put ed 10 (list (car p1) (cadr p1) 0.0))
                       11 (list (car p2) (cadr p2) 0.0))))
     need)
    ((= typ "POINT")
     (setq p1 (cdr (assoc 10 ed)) need (> (abs (caddr p1)) 1e-9))
     (if (and need (not dry))
       (entmod (vp:put ed 10 (list (car p1) (cadr p1) 0.0))))
     need)
    ((member typ '("CIRCLE" "ARC" "LWPOLYLINE"))
     (cond
       ((< (abs nz) 0.999999) 'tilt)
       ((= typ "LWPOLYLINE")
        (setq elev (cdr (assoc 38 ed)) elev (if elev elev 0.0)
              parts (vp:lw-parse ed)
              need (or (< nz 0) (> (abs elev) 1e-9)
                       (and (assoc 43 ed) (/= 0.0 (cdr (assoc 43 ed))))
                       (vl-some '(lambda (v) (or (/= 0.0 (cadr v)) (/= 0.0 (caddr v))))
                                (cadr parts))))
        (if (and need (not dry))
          (progn
            (setq hdr (car parts)
                  hdr (vp:put hdr 38 0.0)
                  hdr (if (assoc 43 hdr) (vp:put hdr 43 0.0) hdr)
                  vs  (mapcar '(lambda (v)
                                 (list (vp:2d (trans (list (car (car v)) (cadr (car v)) elev) e 0))
                                       0.0 0.0
                                       (if (< nz 0) (- (cadddr v)) (cadddr v))))
                              (cadr parts))
                  tail (vp:put (caddr parts) 210 '(0.0 0.0 1.0)))
            (entmod (vp:lw-build hdr vs tail (= 1 (logand 1 (cdr (assoc 70 ed))))))))
        need)
       (T
        (setq c (cdr (assoc 10 ed))
              need (or (< nz 0) (> (abs (caddr c)) 1e-9)))
        (if (and need (not dry))
          (progn
            (setq cw (trans c e 0)
                  ed (vp:put ed 10 (list (car cw) (cadr cw) 0.0)))
            (if (and (= typ "ARC") (< nz 0))
              ;; seen from above, a flipped arc runs the other way round
              (setq sp (vlax-curve-getStartPoint e)
                    ep (vlax-curve-getEndPoint e)
                    ed (vp:put (vp:put ed 50 (angle cw ep)) 51 (angle cw sp))))
            (entmod (vp:put ed 210 '(0.0 0.0 1.0)))))
        need)))
    (T nil)))

;; ---------- step 1: explode and flatten ----------

(defun vp:explodable (e / ed bd)
  (setq ed (entget e))
  (cond ((= "INSERT" (cdr (assoc 0 ed)))
         (setq bd (tblsearch "BLOCK" (cdr (assoc 2 ed))))
         (and bd (= 0 (logand 4 (cdr (assoc 70 bd))))))       ; not an xref
        ((member (cdr (assoc 0 ed)) '("REGION" "MLINE")) T)))

(defun vp:step1 ( / blks spl ell hpl flats msg k pass todo failed m r attd
                    nspl tilt res done)
  (vp:clean)
  (foreach e *vp:work*
    (cond ((vp:explodable e) (setq blks (cons e blks)))
          ((= "SPLINE" (vp:typ e)) (setq spl (cons e spl)))
          ((= "ELLIPSE" (vp:typ e)) (setq ell (cons e ell)))
          ((= "POLYLINE" (vp:typ e)) (setq hpl (cons e hpl)))
          ((vp:flatten e T) (setq flats (cons e flats)))))
  (princ "\n\n--- Step 1 of 4: Explode and flatten ---")
  (if (not (or blks spl ell hpl flats))
    (progn (princ "\nNothing to explode or flatten.") T)
    (progn
      (setq msg (vl-remove nil
                  (list (if blks (vp:n (length blks) "block/region"))
                        (if spl (vp:n (length spl) "spline"))
                        (if ell (vp:n (length ell) "ellipse"))
                        (if hpl (vp:n (length hpl) "old-style/3D polyline"))
                        (if flats (strcat (vp:n (length flats) "object")
                                          " off Z=0 or upside down")))))
      (princ (strcat "\nFound " (vp:joinstr msg ", ") " (highlighted)."))
      (vp:hl (append blks spl ell hpl flats))
      (setq k (vp:ask "Fix them?" "Fix Skip Stop" "Fix"))
      (vp:unhl)
      (cond
        ((= k "Stop") nil)
        ((= k "Skip") T)
        (T
         ;; explode, repeating for blocks nested inside blocks
         (setq pass 0 attd 0)
         (while (and (< pass 10)
                     (setq todo (vl-remove-if
                                  '(lambda (e) (member e failed))
                                  (vl-remove-if-not 'vp:explodable *vp:work*))))
           (foreach e todo
             (setq m (vp:bookmark)
                   r (vl-catch-all-apply 'vla-explode (list (vp:obj e))))
             (if (vl-catch-all-error-p r)
               (progn
                 (setq failed (cons e failed))
                 (entdel m))
               (progn
                 (vla-delete (vp:obj e))
                 ;; attribute tags left behind by exploding are just labels
                 (foreach n (vp:adopt m)
                   (if (= "ATTDEF" (vp:typ n))
                     (progn (entdel n) (setq attd (1+ attd))))))))
           (setq pass (1+ pass)))
         (vp:clean)
         ;; splines: AutoCAD's own conversion (keeps arcs), else trace them
         (setq nspl 0)
         (foreach e (vl-remove-if-not '(lambda (e) (= "SPLINE" (vp:typ e))) *vp:work*)
           (setq m (vp:bookmark))
           (command "._SPLINEDIT" e "_P" "10")
           (vp:cancel)
           (vp:adopt m)
           (if (vp:alive e)
             (progn
               (setq m (vp:bookmark))
               (vp:trace e)
               (vp:adopt m)))
           (if (not (vp:alive e)) (setq nspl (1+ nspl))))
         ;; ellipses (also arcs from non-uniformly scaled blocks)
         (foreach e (vl-remove-if-not '(lambda (e) (= "ELLIPSE" (vp:typ e))) *vp:work*)
           (setq m (vp:bookmark))
           (vp:trace e)
           (vp:adopt m)
           (if (not (vp:alive e)) (setq nspl (1+ nspl))))
         ;; old-style and 3D polylines
         (foreach e (vl-remove-if-not '(lambda (e) (= "POLYLINE" (vp:typ e))) *vp:work*)
           (setq m (vp:bookmark)
                 res (vp:poly->lw e))
           (vp:adopt m)
           (if (= res 'tilt) (setq tilt (cons e tilt)))
           (if (not (vp:alive e)) (setq nspl (1+ nspl))))
         ;; flatten everything
         (setq done 0)
         (foreach e *vp:work*
           (setq res (vp:flatten e nil))
           (cond ((= res 'tilt) (setq tilt (cons e tilt)))
                 (res (setq done (1+ done)))))
         (vp:clean)
         (princ (strcat "\nExploded " (vp:n (- (length blks) (length failed)) "block/region")
                        " (plus any nested inside), converted "
                        (vp:n nspl "curve") " to polylines, flattened "
                        (vp:n done "object") "."))
         (if (> attd 0)
           (princ (strcat "\nRemoved " (vp:n attd "attribute tag") " left by exploding.")))
         (if failed
           (princ (strcat "\n" (vp:n (length failed) "block")
                          " could not be exploded (xref or set to not explode);"
                          " step 2 lists them with the other junk.")))
         (if tilt
           (progn
             (vp:hl tilt)
             (princ (strcat "\n" (vp:n (length tilt) "shape")
                            " are drawn tilted in 3D (not lying flat), highlighted."
                            " They can't be flattened without changing shape."))
             (setq k (vp:ask "Delete them?" "Delete Keep Stop" "Keep"))
             (vp:unhl)
             (if (= k "Delete") (foreach e tilt (if (vp:alive e) (entdel e))))
             (vp:clean)
             (/= k "Stop"))
           T))))))

;; ---------- step 2: junk and duplicates ----------

(defun vp:dashedp (e / ed lt la)
  (setq ed (entget e)
        lt (cdr (assoc 6 ed)))
  (if (or (null lt) (= (strcase lt) "BYLAYER"))
    (setq la (tblsearch "LAYER" (cdr (assoc 8 ed)))
          lt (cdr (assoc 6 la))))
  (and lt (not (wcmatch (strcase lt) "CONTINUOUS,BYBLOCK"))))

(defun vp:rk (v fuzz / s)
  (setq s (rtos (/ v fuzz) 2 0))
  (if (= s "-0") "0" s))

(defun vp:pk (p fuzz) (strcat (vp:rk (car p) fuzz) "," (vp:rk (cadr p) fuzz)))

(defun vp:pair (a b) (if (< a b) (strcat a ";" b) (strcat b ";" a)))

;; Shape fingerprint: length, both ends and two points along the way,
;; rounded to FUZZ and independent of drawing direction and layer.
(defun vp:sig (e fuzz / r)
  (setq r (vl-catch-all-apply
            '(lambda ( / len)
               (setq len (vlax-curve-getDistAtParam e (vlax-curve-getEndParam e)))
               (if (> len fuzz)
                 (strcat (vp:rk len fuzz) "|"
                         (vp:pair (vp:pk (vlax-curve-getStartPoint e) fuzz)
                                  (vp:pk (vlax-curve-getEndPoint e) fuzz))
                         "|"
                         (vp:pair (vp:pk (vlax-curve-getPointAtDist e (/ len 3.0)) fuzz)
                                  (vp:pk (vlax-curve-getPointAtDist e (/ (* 2.0 len) 3.0)) fuzz)))))))
  (if (vl-catch-all-error-p r) nil r))

;; Extra copies of shapes stacked exactly on top of another shape.
(defun vp:dups (lst fuzz / k keyed keys idx prev cur dups)
  (foreach e lst
    (if (setq k (vp:sig e fuzz)) (setq keyed (cons (cons k e) keyed))))
  (setq keyed (reverse keyed)
        keys  (mapcar 'car keyed)
        idx   (vl-sort-i keys '<))
  (foreach i idx
    (setq cur (nth i keyed))
    (if (and prev (= (car cur) (car prev)))
      (setq dups (cons (cdr cur) dups))
      (setq prev cur)))
  dups)

(defun vp:del (lst) (foreach e lst (if (vp:alive e) (entdel e))) (vp:clean))

(defun vp:tolayer (lst key / nm o)
  (setq nm (vp:mklayer key))
  (foreach e lst
    (if (vp:alive e)
      (progn
        (setq o (vp:obj e))
        (vl-catch-all-apply 'vla-put-layer (list o nm))
        (vl-catch-all-apply 'vla-put-color (list o 256))))))   ; ByLayer

(defun vp:step2 (gap / k txt notes tiny dash dups curves fuzz m n0 stop len)
  (vp:clean)
  (setq fuzz (* 0.001 (vp:uf)))
  (princ "\n\n--- Step 2 of 4: Remove junk and duplicates ---")
  (foreach e *vp:work*
    (cond
      ((wcmatch (vp:typ e) "TEXT,MTEXT,ATTDEF") (setq txt (cons e txt)))
      ((or (wcmatch (vp:typ e)
             "DIMENSION,ARC_DIMENSION,*LEADER,TOLERANCE,HATCH,ACAD_TABLE,WIPEOUT,IMAGE,OLE2FRAME,XLINE,RAY,SOLID,TRACE,3DFACE,3DSOLID,BODY,*SURFACE,MESH,REGION,MLINE,INSERT,SHAPE,*UNDERLAY,POINTCLOUD*,ACAD_PROXY_ENTITY")
           (= "POLYLINE" (vp:typ e)))                       ; meshes left from step 1
       (setq notes (cons e notes)))
      ((and (vp:curvep e) (not (vp:closedp e))
            (setq len (vp:len e)) (< len gap))
       (setq tiny (cons e tiny)))
      ((and (vp:curvep e) (vp:dashedp e)) (setq dash (cons e dash)))))
  ;; dimensions, hatches etc.
  (if notes
    (progn
      (vp:hl notes)
      (princ (strcat "\n" (vp:n (length notes) "dimension/hatch/leader/fill/image/3D object")
                     " or block that can't be cut (highlighted)."))
      (setq k (vp:ask "Delete them?" "Delete Keep Stop" "Delete"))
      (vp:unhl)
      (cond ((= k "Stop") (setq stop T))
            ((= k "Delete") (vp:del notes)))))
  ;; text
  (if (and txt (not stop))
    (progn
      (vp:hl txt)
      (princ (strcat "\n" (vp:n (length txt) "text object") " (highlighted)."
                     " Engrave = keep it on the " (vp:ln 'ENGRAVE) " layer."))
      (setq k (vp:ask "What should happen to the text?" "Delete Engrave Keep Stop" "Delete"))
      (vp:unhl)
      (cond ((= k "Stop") (setq stop T))
            ((= k "Delete") (vp:del txt))
            ((= k "Engrave")
             (vp:tolayer txt 'ENGRAVE)
             (princ "\nNote: VCarve may not bring in text from a DXF. If it is missing after import, explode it to outlines first (Express Tools TXTEXP) or retype it in VCarve.")))))
  ;; tiny pieces
  (if (and tiny (not stop))
    (progn
      (vp:hl tiny)
      (princ (strcat "\n" (vp:n (length tiny) "tiny piece")
                     " shorter than the gap size (highlighted)."))
      (setq k (vp:ask "Delete them?" "Delete Keep Stop" "Delete"))
      (vp:unhl)
      (cond ((= k "Stop") (setq stop T))
            ((= k "Delete") (vp:del tiny)))))
  ;; dashed lines
  (if (and dash (not stop))
    (progn
      (vp:hl dash)
      (princ (strcat "\n" (vp:n (length dash) "line")
                     " with a dashed, center or hidden linetype (highlighted) -"
                     " often centerlines, fold lines or hidden edges."))
      (setq k (vp:ask "Delete them?" "Delete Keep Stop" "Keep"))
      (vp:unhl)
      (cond ((= k "Stop") (setq stop T))
            ((= k "Delete") (vp:del dash)))))
  ;; duplicates and overlaps
  (if (not stop)
    (progn
      (vp:clean)
      (setq curves (vl-remove-if-not 'vp:curvep *vp:work*)
            dups   (vp:dups curves fuzz))
      (if dups
        (progn
          (vp:hl dups)
          (princ (strcat "\n" (vp:n (length dups) "duplicate")
                         " stacked on top of other lines (highlighted)."))
          (setq k (vp:ask "Delete the extra copies and merge overlapping lines?"
                          "Delete Keep Stop" "Delete"))
          (vp:unhl))
        (setq k (vp:ask "No exact duplicates. Merge lines that partly overlap (AutoCAD OVERKILL)?"
                        "Yes No Stop" "Yes")))
      (cond
        ((= k "Stop") (setq stop T))
        ((member k '("Delete" "Yes"))
         (vp:del dups)
         (setq curves (vl-remove-if-not 'vp:curvep *vp:work*)
               n0     (length *vp:work*))
         (if curves
           (progn
             (setq m (vp:bookmark))
             (command "._-OVERKILL" (vp:ss curves) "" "")
             (vp:cancel)
             (vp:adopt m)))
         (princ (strcat "\nDeleted " (vp:n (length dups) "duplicate") "; OVERKILL merged or removed "
                        (vp:n (max 0 (- n0 (length *vp:work*))) "overlapping piece") "."))))))
  (not stop))

;; ---------- step 3: join and close ----------

(defun vp:openp (e)
  (and (vp:curvep e) (/= "CIRCLE" (vp:typ e)) (not (vp:closedp e))))

(defun vp:join (gap / lst m)
  (setq lst (vl-remove-if-not
              '(lambda (e) (and (vp:openp e)
                                (member (vp:typ e) '("LINE" "ARC" "LWPOLYLINE"))))
              *vp:work*))
  (if lst
    (progn
      (setq m (vp:bookmark))
      (command "._PEDIT" "_M" (vp:ss lst) "" "_J" "_J" "_B" gap "")
      (vp:cancel)
      (vp:adopt m))))

;; Close polylines whose two ends are within GAP of each other.
(defun vp:close-near (gap / parts vs d cnt fuzz)
  (setq cnt 0 fuzz (* 0.001 (vp:uf)))
  (foreach e *vp:work*
    (if (and (= "LWPOLYLINE" (vp:typ e)) (not (vp:closedp e)))
      (progn
        (setq parts (vp:lw-parse (entget e))
              vs    (cadr parts)
              d     (distance (vp:2d (car (car vs))) (vp:2d (car (last vs)))))
        (cond
          ;; ends sit on top of each other: drop the doubled point and close
          ((and (<= d fuzz) (>= (length vs) 3))
           (entmod (vp:lw-build (car parts) (reverse (cdr (reverse vs))) (caddr parts) T))
           (setq cnt (1+ cnt)))
          ;; small gap: close it with a short straight segment
          ((and (<= d gap) (> d fuzz)
                (or (>= (length vs) 3) (/= 0.0 (cadddr (car vs)))))
           (setq vs (append (reverse (cdr (reverse vs)))
                            (list (list (car (last vs)) 0.0 0.0 0.0))))
           (entmod (vp:lw-build (car parts) vs (caddr parts) T))
           (setq cnt (1+ cnt)))))))
  cnt)

(defun vp:clear-marks ()
  (foreach e *vp:marks* (if (vp:alive e) (entdel e)))
  (setq *vp:marks* nil))

(defun vp:mark-ends (lst / r)
  (vp:clear-marks)
  (vp:checklay)
  (setq r (max (* 0.01 (getvar "VIEWSIZE")) (* 0.05 (vp:uf))))
  (foreach e lst
    (foreach p (list (vlax-curve-getStartPoint e) (vlax-curve-getEndPoint e))
      (if (entmake (list '(0 . "CIRCLE") (cons 8 *vp:checklayer*) '(62 . 1)
                         (list 10 (car p) (cadr p) 0.0) (cons 40 r)))
        (setq *vp:marks* (cons (entlast) *vp:marks*))))))

(defun vp:step3 (gap / open k cnt again res closed n)
  (vp:clean)
  (princ "\n\n--- Step 3 of 4: Join loose lines and close gaps ---")
  (setq open (vl-remove-if-not 'vp:openp *vp:work*))
  (if (null open)
    (progn (princ "\nAll shapes are already closed.") T)
    (progn
      (vp:hl open)
      (princ (strcat "\n" (vp:n (length open) "loose or open piece") " (highlighted)."))
      (setq k (vp:ask (strcat "Join pieces whose ends are within " (rtos gap) " and close shapes?")
                      "Fix Skip Stop" "Fix"))
      (vp:unhl)
      (cond
        ((= k "Stop") nil)
        ((= k "Skip") T)
        (T
         (setq again T res T)
         (while again
           (vp:join gap)
           (setq cnt (vp:close-near gap))
           (vp:clean)
           (setq open (vl-remove-if-not 'vp:openp *vp:work*))
           (princ (strcat "\nJoined and closed shapes (" (vp:n cnt "outline")
                          " closed across a small gap)."))
           (if (null open)
             (progn (princ "\nAll shapes are closed now.") (setq again nil))
             (progn
               (vp:mark-ends open)
               (vp:hl open)
               (princ (strcat "\n" (vp:n (length open) "shape")
                              " still open (ends marked with red circles):"))
               (princ "\n  Close  = close each open outline with a straight line, end to start")
               (princ "\n  Bigger = try again with a bigger gap size")
               (princ "\n  Leave  = keep them open (they go on the ON-LINE layer in step 4)")
               (princ "\n  Stop   = end here and fix the marked spots by hand")
               (setq k (vp:ask "What now?" "Close Bigger Leave Stop" "Leave"))
               (vp:unhl)
               (cond
                 ((= k "Bigger")
                  (vp:clear-marks)
                  (setq gap (vp:getnum "New gap size" (* 2.0 gap))))
                 ((= k "Close")
                  (vp:clear-marks)
                  (setq closed 0 n 0)
                  (foreach e open
                    (if (and (vp:alive e) (= "LWPOLYLINE" (vp:typ e))
                             (> (length (cadr (vp:lw-parse (entget e)))) 2))
                      (progn
                        (vla-put-closed (vp:obj e) :vlax-true)
                        (setq closed (1+ closed)))
                      (setq n (1+ n))))
                  (princ (strcat "\nClosed " (vp:n closed "outline") "."))
                  (if (> n 0)
                    (princ (strcat " " (vp:n n "single line/arc")
                                   " left open (nothing to close it against).")))
                  (setq again nil))
                 ((= k "Leave")
                  (vp:clear-marks)
                  (setq again nil))
                 (T
                  (princ "\nStopped. Fix the spots marked with red circles, then run VCPREP again,")
                  (princ "\nselect the cleaned shapes and choose Selected to carry on.")
                  (setq again nil res nil))))))
         res)))))

;; ---------- step 4: sort onto cut layers ----------

;; Outline of a closed shape as a list of 2D points.
(defun vp:outline (e / pts vs i j len n)
  (if (= "LWPOLYLINE" (vp:typ e))
    (progn
      (setq vs (cadr (vp:lw-parse (entget e))) i 0)
      (foreach v vs
        (if (= 0.0 (cadddr v))
          (setq pts (cons (vp:2d (vlax-curve-getPointAtParam e i)) pts))
          (progn
            (setq j 0)
            (repeat 8
              (setq pts (cons (vp:2d (vlax-curve-getPointAtParam e (+ i (/ j 8.0)))) pts)
                    j (1+ j)))))
        (setq i (1+ i))))
    (progn
      (setq len (vp:len e) n 96 i 0)
      (repeat n
        (setq pts (cons (vp:2d (vlax-curve-getPointAtDist e (* len (/ (float i) n)))) pts)
              i (1+ i)))))
  (reverse (vl-remove nil pts)))

(defun vp:inpoly (p pts / x y in j)
  (setq x (car p) y (cadr p) j (last pts))
  (foreach i pts
    (if (not (eq (> (cadr i) y) (> (cadr j) y)))
      (if (< x (+ (car i) (/ (* (- (car j) (car i)) (- y (cadr i)))
                            (- (cadr j) (cadr i)))))
        (setq in (not in))))
    (setq j i))
  in)

;; For each closed shape, how many other shapes it sits inside.
(defun vp:depths (lst / data a pts mn mx d res)
  (foreach e lst
    (setq pts (vp:outline e))
    (if (> (length pts) 2)
      (setq mn (list (apply 'min (mapcar 'car pts)) (apply 'min (mapcar 'cadr pts)))
            mx (list (apply 'max (mapcar 'car pts)) (apply 'max (mapcar 'cadr pts)))
            a  (vl-catch-all-apply 'vlax-curve-getArea (list e))
            a  (if (vl-catch-all-error-p a) 0.0 (abs a))
            data (cons (list e a mn mx pts
                             (vp:2d (vlax-curve-getPointAtDist e (* 0.37 (vp:len e)))))
                       data))))
  (foreach s data
    (setq d 0)
    (foreach b data
      (if (and (not (eq (car s) (car b)))
               (> (cadr b) (cadr s))
               (>= (car (nth 2 s)) (car (nth 2 b))) (>= (cadr (nth 2 s)) (cadr (nth 2 b)))
               (<= (car (nth 3 s)) (car (nth 3 b))) (<= (cadr (nth 3 s)) (cadr (nth 3 b)))
               (vp:inpoly (nth 5 s) (nth 4 b)))
        (setq d (1+ d))))
    (setq res (cons (cons (car s) d) res)))
  res)

(defun vp:pick (msg / ss)
  (princ (strcat "\n" msg))
  (setq ss (ssget))
  (vl-remove-if-not '(lambda (e) (member e *vp:work*)) (vp:sslist ss)))

;; Confirm one group. Returns nil on Stop, else the list put on the layer.
(defun vp:group (lst key desc / k sel)
  (setq lst (vl-remove-if '(lambda (e) (member e *vp:placed*)) lst))
  (if (null lst)
    T
    (progn
      (vp:hl lst)
      (princ (strcat "\n" (vp:n (length lst) "shape") " look like " desc
                     " (highlighted) -> layer " (vp:ln key) "."))
      (princ "\n  Accept = put them on that layer   Pick = select the ones that go there yourself")
      (setq k (vp:ask "OK?" "Accept Pick Skip Stop" "Accept"))
      (vp:unhl)
      (cond ((= k "Stop") nil)
            ((= k "Skip") T)
            (T
             (setq sel (if (= k "Pick")
                         (vp:pick (strcat "Select the shapes for layer " (vp:ln key) ": "))
                         lst))
             (vp:tolayer sel key)
             (setq *vp:placed* (append sel *vp:placed*))
             T)))))

(defun vp:step4 ( / k curves closed open circ drill rest dm dep outs ins pk res)
  (vp:clean)
  (setq *vp:placed* nil)
  (princ "\n\n--- Step 4 of 4: Sort shapes onto cut layers ---")
  (princ "\nChoose Skip if the layers are already set up the way you want them in VCarve.")
  (setq k (vp:ask "Sort shapes onto cut layers?" "Auto Skip Stop" "Auto"))
  (cond
    ((= k "Stop") nil)
    ((= k "Skip") T)
    (T
     (setq curves (vl-remove-if-not 'vp:curvep *vp:work*))
     (foreach e curves
       (if (vp:closedp e) (setq closed (cons e closed)) (setq open (cons e open))))
     (setq circ (vl-remove-if-not '(lambda (e) (= "CIRCLE" (vp:typ e))) closed))
     (if circ
       (progn
         (setq dm (vp:getnum "Circles this size across or smaller are drill holes"
                             (* 0.5 (vp:uf))))
         (setq drill (vl-remove-if-not
                       '(lambda (e) (<= (* 2.0 (cdr (assoc 40 (entget e)))) (+ dm 1e-8)))
                       circ))))
     (setq rest (vl-remove-if '(lambda (e) (member e drill)) closed)
           dep  (vp:depths rest))
     (foreach p dep
       (if (= 0 (rem (cdr p) 2))
         (setq outs (cons (car p) outs))
         (setq ins (cons (car p) ins))))
     (setq res T)
     ;; outside profiles
     (setq res (vp:group outs 'OUT "the outer edge of a part"))
     ;; inside shapes: cut through, pockets, or a mix
     (if res
       (progn
         (setq ins (vl-remove-if '(lambda (e) (member e *vp:placed*)) ins))
         (if ins
           (progn
             (vp:hl ins)
             (princ (strcat "\n" (vp:n (length ins) "shape")
                            " sit inside a part (highlighted): holes, cutouts or pockets."))
             (princ (strcat "\n  Through = all cut through (" (vp:ln 'IN) ")"
                            "   Pockets = all pockets (" (vp:ln 'POCKET) ")"
                            "   Mixed = you pick the pockets, the rest cut through"))
             (setq k (vp:ask "Which?" "Through Pockets Mixed Skip Stop" "Through"))
             (vp:unhl)
             (cond
               ((= k "Stop") (setq res nil))
               ((= k "Through") (vp:tolayer ins 'IN))
               ((= k "Pockets") (vp:tolayer ins 'POCKET))
               ((= k "Mixed")
                (setq pk (vl-remove-if-not '(lambda (e) (member e ins))
                                           (vp:pick "Select the shapes that are pockets: ")))
                (vp:tolayer pk 'POCKET)
                (vp:tolayer (vl-remove-if '(lambda (e) (member e pk)) ins) 'IN)))
             (if (member k '("Through" "Pockets" "Mixed"))
               (setq *vp:placed* (append ins *vp:placed*)))))))
     (if res (setq res (vp:group drill 'DRILL "drill holes")))
     (if res (setq res (vp:group open 'OPEN "open lines (cut on the line or engrave)")))
     (if res
       (princ (strcat "\nSorted " (vp:n (length *vp:placed*) "shape") " onto cut layers.")))
     res)))

;; ---------- save the DXF ----------

(defun vp:export (mode / bb ll fn def ok k)
  (vp:clean)
  (princ "\n\n--- Save for VCarve ---")
  (if (null *vp:work*)
    (princ "\nNothing left to save.")
    (progn
      (setq bb  (vp:bbox *vp:work*)
            ll  (list (car (car bb)) (cadr (car bb)) 0.0)
            def (strcat (getvar "DWGPREFIX")
                        (vl-filename-base (getvar "DWGNAME")) "-VCARVE.dxf")
            fn  (getfiled "Save the cleaned DXF for VCarve" def "dxf" 1))
      (if fn
        (progn
          (setq ok T)
          (if (and (findfile fn) (not (vl-file-delete fn)))
            (progn
              (alert (strcat "Can't replace\n" fn
                             "\n\nIs it open in VCarve? Close it there and run the save again."))
              (setq ok nil)))
          (if ok
            (progn
              ;; lower-left corner of the parts to 0,0 for the export only
              (vp:move-all ll '(0.0 0.0 0.0))
              (setvar "FILEDIA" 0)
              (command "._DXFOUT" fn "_O" (vp:ss *vp:work*) "" "_V" "2000" "16")
              (vp:cancel)
              (setvar "FILEDIA" (vp:old "FILEDIA"))
              (vp:move-all '(0.0 0.0 0.0) ll)
              (if (findfile fn)
                (princ (strcat "\nSaved " (vp:n (length *vp:work*) "object") " to " fn
                               "\nIn VCarve use File > Import Vectors and pick that file."))
                (progn (princ "\nThe DXF was not written.") (setq ok nil))))))
        (princ "\nNo file saved."))
      (if (= mode "Copy")
        (if ok
          (progn
            (setq k (vp:ask "Keep the cleaned copy in this drawing?" "Yes No" "No"))
            (if (= k "No")
              (progn (vp:del *vp:work*)
                     (princ "\nCleaned copy removed; your drawing is as it was."))))
          (princ "\nThe cleaned copy is left beside your original."))))))

;; ---------- command ----------

(defun c:VCPREP ( / *error* mode ss gap old res)
  (*push-error-using-command*)
  (setq *error* vp:err)
  (princ "\nVCPREP - gets a drawing ready to import into VCarve.")
  (setq mode (vp:ask "Clean a copy beside the original, or the selected objects themselves?"
                     "Copy Selected" "Copy"))
  (princ "\nSelect the parts to prepare: ")
  (setq ss (ssget))
  (if (null ss)
    (princ "\nNothing selected.")
    (progn
      (setq gap (vp:getnum "Close gaps up to this size automatically (default 1/64\")"
                           (* (vp:uf) (/ 1.0 64.0))))
      (vp:begin)
      ;; red marker circles left by an earlier stopped run
      (if (setq old (ssget "_X" (list (cons 8 *vp:checklayer*))))
        (foreach e (vp:sslist old) (entdel e)))
      (if (vp:setup ss mode)
        (progn
          (vp:zoom *vp:work*)
          (setq res (and (vp:step1) (vp:step2 gap) (vp:step3 gap) (vp:step4)))
          (if res
            (vp:export mode)
            (if (= mode "Copy")
              (princ "\nStopped. The cleaned copy so far is beside your original (U undoes the whole run).")
              (princ "\nStopped. Changes so far are kept (U undoes the whole run)."))))
        (princ "\nNothing usable in the selection."))
      (vp:restore)
      (vp:finish)))
  (*pop-error-mode*)
  (princ))

(princ "\nVCPREP loaded. Type VCPREP to clean a drawing for VCarve.")
(princ)
