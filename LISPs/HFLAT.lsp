;;; ------------------------------------------------------------------
;;; HFLAT.lsp  -  Parametric 3D Hollywood theatre flat
;;;
;;; Commands:
;;;   HFLAT     build (or reuse) a flat block of a given size and insert it
;;;   HFLATLIB  build a library of standard-width blocks at one height
;;;
;;; Hollywood (studio / hard) flat construction:
;;;   - 1x4 framing laid ON EDGE: 3/4" edge faces the skin, 3.5" deep,
;;;     so the frame is a deep box-like structure behind the skin
;;;   - rails run the full width; stiles and toggles butt between them
;;;   - 1/4" lauan skin on the front, no corner blocks or keystones
;;;   - if W > 48", a 2x4 (on edge: 1.5" face x 3.5" deep) stile at 4'
;;;
;;; Geometry (block origin = bottom-left corner of the FRONT face):
;;;   X = width, Y = depth (front face at Y=0, frame behind it), Z = height
;;;   Layer HFLAT-SKIN  : lauan / plywood skin, one solid per 4x8 sheet
;;;   Layer HFLAT-FRAME : rails + stiles + toggles (one solid)
;;;
;;; Defaults are in inches and scale automatically from INSUNITS
;;; (mm, cm, m, ft handled).  Parameters:
;;;   W  flat width            H  flat height
;;;   FW member face width     (1x4 on edge = 0.75" against the skin)
;;;   FD member depth          (1x4 on edge = 3.5" behind the skin)
;;;   ST skin thickness        (1/4" lauan = 0.25")
;;;   MS toggle spacing        (24" centre to centre, measured from the bottom)
;;;   MW/MD mid-stile face width / depth (2x4 on edge = 1.5" x 3.5")
;;;   XM mid-stile position    (48" from the left edge)
;;;   SW/SH facing sheet size  (48" x 96"; sheets start at the bottom-left
;;;                             corner, leftover pieces are cut to fit)
;;;   ZM 2x4 toggle height     (the toggle centred at 96" is a 2x4, same size as MW/MD;
;;;                             only appears on flats tall enough to have a toggle there)
;;; The 2x4 is centred at XM (pushed in against the right stile if the
;;; flat is only slightly wider than 48").  Toggles split around it.
;;;
;;; Block names:
;;;   HWFLAT_48x96        default lumber/skin sizes
;;;   HWFLAT_48x96_Cxxxxx changed lumber/skin sizes; the 5-character code is
;;;                       built from those sizes, so each combination gets
;;;                       its own block and is reused only when it matches
;;;
;;; Housekeeping:
;;;   - the block is always built in the World UCS so its geometry is
;;;     consistent; your UCS is restored before the block is inserted,
;;;     so the flat is placed in (and aligned to) your current UCS
;;;   - each command is one UNDO step
;;;   - HFLAT-FRAME / HFLAT-SKIN may be frozen or locked; they are
;;;     thawed/unlocked while building and put back afterwards
;;; ------------------------------------------------------------------

;; ---------- units / defaults ----------

(defun hf:uf ( / u)
  (setq u (getvar "INSUNITS"))
  (cond ((= u 4) 25.4)
        ((= u 5) 2.54)
        ((= u 6) 0.0254)
        ((= u 2) (/ 1.0 12.0))
        (T 1.0)))

;; Default parameter list, scaled to drawing units.
;; Order: FW FD ST MS XM MW MD ZM SW SH
(defun hf:defaults (uf)
  (mapcar '(lambda (x) (* x uf))
          '(0.75 3.5 0.25 24.0 48.0 1.5 3.5 96.0 48.0 96.0)))

(defun hf:samep (a b)
  (apply 'and (mapcar '(lambda (x y) (equal x y 1e-6)) a b)))

;; ---------- block naming ----------

;; Simple string hash, kept below 36^5 so it fits in 5 base-36 characters
;; and never overflows AutoLISP's 32-bit integers.
(defun hf:hash (s / h)
  (setq h 0)
  (foreach c (vl-string->list s)
    (setq h (rem (+ (* h 31) c) 60466176)))
  h)

(defun hf:b36 (n / d s)
  (setq d "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" s "")
  (repeat 5
    (setq s (strcat (substr d (1+ (rem n 36)) 1) s)
          n (/ n 36)))
  s)

(defun hf:name (w h p / uf nm)
  (setq uf (hf:uf)
        nm (strcat "HWFLAT_" (rtos w 2 2) "x" (rtos h 2 2)))
  (if (hf:samep p (hf:defaults uf))
    nm
    (strcat nm "_C"
            (hf:b36 (hf:hash (apply 'strcat
                                    (mapcar '(lambda (v) (strcat (rtos (/ v uf) 2 4) ","))
                                            p)))))))

;; ---------- setup / restore (UCS, sysvars, layers, undo) ----------

(defun hf:old (v) (cdr (assoc v *hf:vars*)))

(defun hf:begin ( / ctl)
  (setq *hf:vars* nil *hf:frz* nil *hf:lck* nil *hf:ucs* nil *hf:undo* nil)
  (foreach v '("CMDECHO" "OSMODE" "3DOSMODE" "CLAYER")
    (if (getvar v) (setq *hf:vars* (cons (cons v (getvar v)) *hf:vars*))))
  (setvar "CMDECHO" 0)
  ;; group everything into one UNDO step (only if UNDO is fully enabled)
  (setq ctl (getvar "UNDOCTL"))
  (if (= 1 (logand ctl 3))
    (progn (command "._UNDO" "_BE") (setq *hf:undo* T)))
  (setvar "OSMODE" 0)
  (if (getvar "3DOSMODE") (setvar "3DOSMODE" 0))
  ;; build in World so the block geometry is always X=width, Z=up
  (command "._UCS" "_W")
  (setq *hf:ucs* T))

;; Put back the user's UCS, layer and snaps. Safe to call more than once.
(defun hf:restore ()
  (if *hf:ucs*
    (progn (command "._UCS" "_P") (setq *hf:ucs* nil)))
  (if (hf:old "CLAYER") (setvar "CLAYER" (hf:old "CLAYER")))
  (foreach nm *hf:frz* (command "._-LAYER" "_F" nm ""))
  (foreach nm *hf:lck* (command "._-LAYER" "_LO" nm ""))
  (setq *hf:frz* nil *hf:lck* nil)
  (if (hf:old "OSMODE") (setvar "OSMODE" (hf:old "OSMODE")))
  (if (hf:old "3DOSMODE") (setvar "3DOSMODE" (hf:old "3DOSMODE"))))

;; Close the UNDO group and restore CMDECHO. Call last.
(defun hf:finish ()
  (if *hf:undo*
    (progn (command "._UNDO" "_E") (setq *hf:undo* nil)))
  (if (hf:old "CMDECHO") (setvar "CMDECHO" (hf:old "CMDECHO")))
  (setq *hf:vars* nil))

(defun hf:err (m)
  (while (> (getvar "CMDACTIVE") 0) (command))
  (hf:restore)
  (hf:finish)
  (if (and m (not (wcmatch (strcase m) "*CANCEL*,*QUIT*,*EXIT*")))
    (princ (strcat "\nError: " m)))
  (princ))

;; Make NM the current layer, creating it if needed. A frozen or locked
;; layer is thawed/unlocked for now and put back by hf:restore.
(defun hf:layer (nm col / d fl)
  (if (setq d (tblsearch "LAYER" nm))
    (progn
      (setq fl (cdr (assoc 70 d)))
      (if (= 1 (logand 1 fl))
        (progn (command "._-LAYER" "_T" nm "")
               (setq *hf:frz* (cons nm *hf:frz*))))
      (if (= 4 (logand 4 fl))
        (progn (command "._-LAYER" "_U" nm "")
               (setq *hf:lck* (cons nm *hf:lck*))))
      (setvar "CLAYER" nm))
    (command "._-LAYER" "_M" nm "_C" col nm "")))

;; ---------- geometry ----------

(defun hf:box (x y z dx dy dz)
  (command "._BOX" (list x y z) "_L" dx dy dz)
  (entlast))

;; 3D solids created after entity E0. Skips attributes, polyline
;; vertices etc. that follow E0 if it is a complex entity.
(defun hf:since (e0 / e ss)
  (setq ss (ssadd)
        e  (if e0 (entnext e0) (entnext)))
  (while e
    (if (= "3DSOLID" (cdr (assoc 0 (entget e))))
      (ssadd e ss))
    (setq e (entnext e)))
  ss)

;; Returns an error message if the sizes can't make a flat, else nil.
(defun hf:check (w h p / fw ms mw)
  (setq fw (nth 0 p) ms (nth 3 p) mw (nth 5 p))
  (cond ((or (< w (* 4 fw)) (< h (* 4 fw)))
         "Flat is too small for that lumber size.")
        ((<= ms (max fw mw))
         "Toggle spacing must be larger than the toggle size.")
        (T nil)))

;; Build the block definition if it does not exist yet. Returns block name.
;; P is the parameter list (see hf:defaults).
(defun hf:make (w h p / fw fd st ms xm mw md zm sw sh
                        nm e0 ssf ssa zc xc spans sp x0 x1 big th td x z)
  (mapcar 'set '(fw fd st ms xm mw md zm sw sh) p)
  (setq nm (hf:name w h p))
  (if (not (tblsearch "BLOCK" nm))
    (progn
      (setq e0 (entlast))
      ;; ---- frame ----
      (hf:layer "HFLAT-FRAME" 30)
      (hf:box 0 st 0 w fd fw)                          ; bottom rail (full width)
      (hf:box 0 st (- h fw) w fd fw)                   ; top rail (full width)
      (hf:box 0 st fw fw fd (- h (* 2 fw)))            ; left stile (between rails)
      (hf:box (- w fw) st fw fw fd (- h (* 2 fw)))     ; right stile (between rails)
      ;; ---- 2x4 stile at 4' when wider than 48" ----
      (if (> w (+ xm 0.000001))
        (progn
          (setq xc (min xm (- w fw (/ mw 2.0))))
          (hf:box (- xc (/ mw 2.0)) st fw mw md (- h (* 2 fw)))
          (setq spans (list (list fw (- xc (/ mw 2.0)))
                            (list (+ xc (/ mw 2.0)) (- w fw)))))
        (setq spans (list (list fw (- w fw)))))
      ;; ---- toggles (split around the 2x4) ----
      (foreach sp spans
        (setq x0 (car sp) x1 (cadr sp))
        (if (> (- x1 x0) 0.001)
          (progn
            (setq zc ms)                               ; first toggle at MS from bottom
            (while (progn
                     (setq big (< (abs (- zc zm)) 0.001)   ; toggle at 8' is a 2x4
                           th  (if big mw fw)
                           td  (if big md fd))
                     (<= (+ zc (/ th 2.0)) (- h (* 2 fw))))
              (hf:box x0 st (- zc (/ th 2.0)) (- x1 x0) td th)
              (setq zc (+ zc ms))))))
      (setq ssf (hf:since e0))
      (command "._UNION" ssf "")
      ;; ---- skin ----
      (hf:layer "HFLAT-SKIN" 54)
      ;; sliced into sw x sh sheets (4x8) starting at the bottom-left corner
      (setq x 0.0)
      (while (< x (- w 0.001))
        (setq z 0.0)
        (while (< z (- h 0.001))
          (hf:box x 0 z (min sw (- w x)) st (min sh (- h z)))
          (setq z (+ z sh)))
        (setq x (+ x sw)))
      ;; ---- block ----
      (setq ssa (hf:since e0))
      (command "._BLOCK" nm "0,0,0" ssa "")))
  nm)

;; ---------- input ----------

;; Distance prompt; rejects zero and negative values.
(defun hf:getnum (msg def / v)
  (initget 6)
  (setq v (getdist (strcat "\n" msg " <" (rtos def 2 3) ">: ")))
  (if v v def))

;; ---------- commands ----------

(defun c:HFLAT ( / *error* uf w h p adv msg nm)
  (*push-error-using-command*)
  (setq *error* hf:err)
  (setq uf (hf:uf)
        w  (hf:getnum "Flat width"  (* 48.0 uf))
        h  (hf:getnum "Flat height" (* 96.0 uf))
        p  (hf:defaults uf))
  (initget "Yes No")
  (setq adv (getkword "\nChange lumber/skin sizes? [Yes/No] <No>: "))
  (if (= adv "Yes")
    (setq p (list
              (hf:getnum "Member face width against skin (1x4 on edge = 0.75in)" (nth 0 p))
              (hf:getnum "Member depth behind skin (1x4 on edge = 3.5in)" (nth 1 p))
              (hf:getnum "Skin thickness" (nth 2 p))
              (hf:getnum "Toggle spacing (centre to centre)" (nth 3 p))
              (hf:getnum "Mid stile position from left edge" (nth 4 p))
              (hf:getnum "Mid stile face width (2x4 on edge = 1.5in)" (nth 5 p))
              (hf:getnum "Mid stile depth (2x4 on edge = 3.5in)" (nth 6 p))
              (hf:getnum "Height of the 2x4 toggle from bottom" (nth 7 p))
              (hf:getnum "Facing sheet width (4x8 = 48in)" (nth 8 p))
              (hf:getnum "Facing sheet height (4x8 = 96in)" (nth 9 p)))))
  (if (setq msg (hf:check w h p))
    (alert msg)
    (progn
      (hf:begin)
      (setq nm (hf:make w h p))
      (hf:restore)                       ; back to the user's UCS before inserting
      (setvar "CMDECHO" 1)
      (command "._-INSERT" nm "_S" 1 "_R" 0 pause)
      (hf:finish)
      (princ (strcat "\nInserted block " nm))))
  (*pop-error-mode*)
  (princ))

(defun c:HFLATLIB ( / *error* uf h p widths msg)
  (*push-error-using-command*)
  (setq *error* hf:err)
  (setq uf     (hf:uf)
        p      (hf:defaults uf)
        widths (mapcar '(lambda (x) (* x uf)) '(12.0 18.0 24.0 30.0 36.0 42.0 48.0))
        h      (hf:getnum "Library flat height" (* 96.0 uf)))
  (if (setq msg (hf:check (car widths) h p))
    (alert msg)
    (progn
      (hf:begin)
      (foreach w widths (hf:make w h p))
      (hf:restore)
      (hf:finish)
      (princ "\nStandard flat blocks created (insert with INSERT).")))
  (*pop-error-mode*)
  (princ))

(princ "\nHFLAT loaded. Commands: HFLAT, HFLATLIB")
(princ)
