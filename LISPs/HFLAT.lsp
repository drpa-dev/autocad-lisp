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
;;; ------------------------------------------------------------------

(defun hf:uf ( / u)
  (setq u (getvar "INSUNITS"))
  (cond ((= u 4) 25.4)
        ((= u 5) 2.54)
        ((= u 6) 0.0254)
        ((= u 2) (/ 1.0 12.0))
        (T 1.0)))

(defun hf:layer (nm col)
  (if (not (tblsearch "LAYER" nm))
    (command "._-LAYER" "_M" nm "_C" col nm "")
    (setvar "CLAYER" nm)))

(defun hf:box (x y z dx dy dz)
  (command "._BOX" (list x y z) "_L" dx dy dz)
  (entlast))

(defun hf:since (e0 / e ss)
  (setq ss (ssadd))
  (setq e (if e0 (entnext e0) (entnext)))
  (while e
    (ssadd e ss)
    (setq e (entnext e)))
  ss)

(defun hf:name (w h)
  (strcat "HWFLAT_" (rtos w 2 2) "x" (rtos h 2 2)))

;; Build the block definition if it does not exist yet. Returns block name.
(defun hf:make (w h fw fd st ms xm mw md zm sw sh / nm e0 ssf ssa zc xc spans sp x0 x1 big th td x z)
  (setq nm (hf:name w h))
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

(defun hf:getnum (msg def / v)
  (setq v (getdist (strcat "\n" msg " <" (rtos def 2 3) ">: ")))
  (if v v def))

(defun c:HFLAT ( / uf w h fw fd st ms xm mw md zm sw sh nm oldlay oldcmd oldosm adv *error*)
  (defun *error* (m)
    (if oldcmd (setvar "CMDECHO" oldcmd))
    (if oldosm (setvar "OSMODE" oldosm))
    (if oldlay (setvar "CLAYER" oldlay))
    (if (and m (not (wcmatch (strcase m) "*CANCEL*,*QUIT*,*EXIT*")))
      (princ (strcat "\nError: " m)))
    (princ))
  (setq uf (hf:uf)
        oldcmd (getvar "CMDECHO") oldosm (getvar "OSMODE") oldlay (getvar "CLAYER"))
  (setq w (hf:getnum "Flat width"  (* 48.0 uf))
        h (hf:getnum "Flat height" (* 96.0 uf))
        fw (* 0.75 uf) fd (* 3.5 uf) st (* 0.25 uf) ms (* 24.0 uf)
        xm (* 48.0 uf) mw (* 1.5 uf) md (* 3.5 uf) zm (* 96.0 uf) sw (* 48.0 uf) sh (* 96.0 uf))
  (initget "Yes No")
  (setq adv (getkword "\nChange lumber/skin sizes? [Yes/No] <No>: "))
  (if (= adv "Yes")
    (setq fw (hf:getnum "Member face width against skin (1x4 on edge = 0.75in)" fw)
          fd (hf:getnum "Member depth behind skin (1x4 on edge = 3.5in)" fd)
          st (hf:getnum "Skin thickness" st)
          ms (hf:getnum "Toggle spacing (centre to centre)" ms)
          mw (hf:getnum "Mid stile face width (2x4 on edge = 1.5in)" mw)
          md (hf:getnum "Mid stile depth (2x4 on edge = 3.5in)" md)
          xm (hf:getnum "Mid stile position from left edge" xm)
          zm (hf:getnum "Height of the 2x4 toggle from bottom" zm)
          sw (hf:getnum "Facing sheet width (4x8 = 48in)" sw)
          sh (hf:getnum "Facing sheet height (4x8 = 96in)" sh)))
  (if (or (< w (* 4 fw)) (< h (* 4 fw)))
    (alert "Flat is too small for that lumber size.")
    (progn
      (setvar "CMDECHO" 0)
      (setvar "OSMODE" 0)
      (command "._UCS" "_W")
      (setq nm (hf:make w h fw fd st ms xm mw md zm sw sh))
      (setvar "CLAYER" oldlay)
      (setvar "OSMODE" oldosm)
      (setvar "CMDECHO" 1)
      (command "._-INSERT" nm "_S" 1 "_R" 0 pause)
      (setvar "CMDECHO" oldcmd)
      (princ (strcat "\nInserted block " nm))))
  (princ))

(defun c:HFLATLIB ( / uf h widths fw fd st ms xm mw md zm sw sh oldcmd oldosm oldlay w *error*)
  (defun *error* (m)
    (if oldcmd (setvar "CMDECHO" oldcmd))
    (if oldosm (setvar "OSMODE" oldosm))
    (if oldlay (setvar "CLAYER" oldlay))
    (princ))
  (setq uf (hf:uf)
        oldcmd (getvar "CMDECHO") oldosm (getvar "OSMODE") oldlay (getvar "CLAYER")
        fw (* 0.75 uf) fd (* 3.5 uf) st (* 0.25 uf) ms (* 24.0 uf)
        xm (* 48.0 uf) mw (* 1.5 uf) md (* 3.5 uf) zm (* 96.0 uf) sw (* 48.0 uf) sh (* 96.0 uf)
        widths (mapcar '(lambda (x) (* x uf)) '(12.0 18.0 24.0 30.0 36.0 42.0 48.0)))
  (setq h (hf:getnum "Library flat height" (* 96.0 uf)))
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)
  (command "._UCS" "_W")
  (foreach w widths (hf:make w h fw fd st ms xm mw md zm sw sh))
  (setvar "CLAYER" oldlay)
  (setvar "OSMODE" oldosm)
  (setvar "CMDECHO" oldcmd)
  (princ "\nStandard flat blocks created (insert with INSERT).")
  (princ))

(princ "\nHFLAT loaded. Commands: HFLAT, HFLATLIB")
(princ)
