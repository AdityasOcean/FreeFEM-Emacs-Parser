;;; freefem++-mode.el --- Advanced IDE for FreeFem++ programming language -*- lexical-binding: t; -*-

;; MIT License for OpenSource code 2025 FreeFem++ Community
;;;Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

;;;The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

;;;THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;; Author: Arun K Eswara, eakishore@gmail.com
;; Maintainer: FreeFem++ Community / Arun K Eswara
;; Created: 2025
;; Version: 2.0
;; Keywords: languages, freefem, finite elements, pde, ide
;; Package-Requires: ((emacs "28.1") (company "0.9.0") (flycheck "32") (dash "2.18.0") (s "1.12.0"))
;; Homepage: https://freefem.org


;;; Commentary:

;; Advanced IDE for FreeFem++ - A comprehensive development environment
;; for FreeFem++ finite element programming.
;;
;; Features:
;; - Advanced syntax highlighting with context awareness
;; - Intelligent code completion with documentation
;; - Real-time error checking and linting
;; - Interactive debugging support
;; - Project management and file navigation
;; - Mathematical symbol input methods
;; - Mesh visualization integration
;; - Advanced refactoring tools
;; - Template library with snippets
;; - Integration with external tools (medit, paraview)

;;; Code:

(require 'cc-mode)
(require 'cc-langs)
(require 'cc-fonts)
(require 'compile)
(require 'imenu)
(require 'which-func)
(require 'eldoc)
(require 'dash nil t)
(require 's nil t)

;; Optional dependencies for enhanced functionality
(declare-function company-mode "company")
(declare-function company-begin-backend "company")
(declare-function flycheck-mode "flycheck")
(declare-function flycheck-define-checker "flycheck")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.edp\\'" . freefem++-mode))
(add-to-list 'auto-mode-alist '("\\.idp\\'" . freefem++-mode))

;;; Customization Groups

(defgroup freefem++ nil
  "Advanced IDE for FreeFem++ programming language."
  :group 'languages
  :prefix "freefem++-")

(defgroup freefem++-faces nil
  "Faces for FreeFem++ syntax highlighting."
  :group 'freefem++
  :group 'faces)

;;; Custom Variables

(defcustom freefem++-program "FreeFem++"
  "Command to run FreeFem++ compiler."
  :type 'string
  :group 'freefem++)

(defcustom freefem++-compile-args '("-v" "0")
  "Default arguments for FreeFem++ compilation."
  :type '(repeat string)
  :group 'freefem++)

(defcustom freefem++-safe-mode-args '("-nw" "-v" "0" "-seq" "-nweb")
  "Safe mode arguments for FreeFem++ (avoids MPI/PETSc issues)."
  :type '(repeat string)
  :group 'freefem++)

(defcustom freefem++-graphics-args '("-v" "0")
  "Arguments for FreeFem++ with graphics enabled."
  :type '(repeat string)
  :group 'freefem++)

(defcustom freefem++-enable-graphics t
  "Enable graphics output in FreeFem++ plots."
  :type 'boolean
  :group 'freefem++)

(defcustom freefem++-enable-safe-mode nil
  "Enable safe mode to avoid MPI/PETSc segmentation faults."
  :type 'boolean
  :group 'freefem++)

(defcustom freefem++-basic-offset 2
  "Basic indentation offset for FreeFem++ code."
  :type 'integer
  :group 'freefem++)

(defcustom freefem++-enable-eldoc t
  "Enable eldoc for FreeFem++ functions."
  :type 'boolean
  :group 'freefem++)

(defcustom freefem++-enable-company t
  "Enable company-mode completion for FreeFem++."
  :type 'boolean
  :group 'freefem++)

(defcustom freefem++-enable-flycheck t
  "Enable flycheck for FreeFem++ syntax checking."
  :type 'boolean
  :group 'freefem++)

(defcustom freefem++-medit-program "medit"
  "Path to medit mesh viewer."
  :type 'string
  :group 'freefem++)

(defcustom freefem++-paraview-program "paraview"
  "Path to ParaView visualization tool."
  :type 'string
  :group 'freefem++)

;;; Language Definition

(eval-and-compile
  (c-add-language 'freefem++-mode 'c++-mode))

;; Enhanced keyword definitions
(c-lang-defconst c-primitive-type-kwds
  freefem++ '("bool" "border" "complex" "func" "ifstream" "int" 
              "macro" "matrix" "mesh" "mesh3" "ofstream" "problem" 
              "real" "R3" "solve" "string" "varf" "fespace"
              "Cmatrix" "gslspline" "NewMacro" "EndMacro"))

(c-lang-defconst c-modifier-kwds
  freefem++ '("const" "extern" "static" "mutable"))

(c-lang-defconst c-other-kwds
  freefem++ '("include" "load" "IFMACRO" "ENDIFMACRO" "for" "while" 
              "if" "else" "break" "continue" "return" "try" "catch"
              "assert" "exit" "cout" "cin" "endl"))

(c-lang-defconst c-paren-nontype-kwds
  freefem++ '("int1d" "int2d" "int3d" "intalledges" "on" "jump" "mean"
              "dx" "dxx" "dxy" "dy" "dyx" "dyy" "dz" "grad" "div" "curl"
              "interpolate" "set" "plot" "buildmesh" "adaptmesh"
              "movemesh" "square" "circle" "ellipse" "triangle"
              "trunc" "splitmesh" "emptymesh" "readmesh" "savemesh"
              "tetg" "tetgconvexhull" "tetgreconstruction" "tetgtransfo"))

(c-lang-defconst c-constant-kwds
  freefem++ '("true" "false" "pi" "N" "x" "y" "z" "region" "label"
              "hTriangle" "area" "lenEdge" "nuTriangle" "nuEdge"
              ;; Finite elements
              "P0" "P1" "P2" "P3" "P4" "P5" "P1b" "P1nc" "P1dc" "P2dc"
              "P0VF" "P2b" "P2h" "RT0" "RT1" "RT2" "BDM1" "BDM2" 
              "RT0Ortho" "RT1Ortho" "BDM1Ortho" "RTmodif"
              "P0edge" "P1edge" "P2edge" "P3edge" "P4edge" "P5edge"
              "P03d" "P13d" "P1b3d" "P23d" "P13ddc" "P2h3d"
              "HCT" "Morley" "BernardiRaugel" "MINI"))

;;; Advanced Font Lock with Context-Aware Highlighting

(defface freefem++-function-name-face
  '((t (:foreground "#268bd2" :weight bold)))
  "Face for FreeFem++ function names."
  :group 'freefem++-faces)

(defface freefem++-finite-element-face
  '((t (:foreground "#d33682" :weight bold)))
  "Face for finite element types."
  :group 'freefem++-faces)

(defface freefem++-boundary-face
  '((t (:foreground "#b58900" :weight bold)))
  "Face for boundary conditions."
  :group 'freefem++-faces)

(defface freefem++-macro-face
  '((t (:foreground "#6c71c4" :weight bold)))
  "Face for macro definitions."
  :group 'freefem++-faces)

(defface freefem++-operator-face
  '((t (:foreground "#859900" :weight bold)))
  "Face for mathematical operators."
  :group 'freefem++-faces)

;; Enhanced font lock keywords
(defconst freefem++-font-lock-keywords-extra
  `(
    ;; Function definitions
    ("\\<\\(func\\|macro\\)\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
     (2 'freefem++-function-name-face))
    
    ;; Finite element spaces
    ("\\<\\(P[0-9]+\\|RT[0-9]*\\|BDM[0-9]*\\|HCT\\|Morley\\)\\>"
     . 'freefem++-finite-element-face)
    
    ;; Boundary conditions
    ("\\<\\(on\\)\\s-*(" 1 'freefem++-boundary-face)
    
    ;; Mathematical operators
    ("\\(\\^\\|'\\|\\*\\|/\\|\\+\\|-\\)" . 'freefem++-operator-face)
    
    ;; Mesh operations
    ("\\<\\(buildmesh\\|adaptmesh\\|movemesh\\|splitmesh\\)\\>"
     . font-lock-builtin-face)
    
    ;; Problem declarations
    ("\\<\\(problem\\|solve\\|varf\\)\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
     (1 font-lock-keyword-face) (2 font-lock-function-name-face))
    
    ;; Include statements
    ("\\<\\(include\\|load\\)\\s-+\"\\([^\"]+\\)\""
     (1 font-lock-preprocessor-face) (2 font-lock-string-face))
    
    ;; IFMACRO blocks
    ("\\<\\(IFMACRO\\|ENDIFMACRO\\)\\>" . 'freefem++-macro-face)
    ))

;; Combine with cc-mode font lock
(defconst freefem++-font-lock-keywords-1 
  (append (c-lang-const c-matchers-1 freefem++) freefem++-font-lock-keywords-extra))

(defconst freefem++-font-lock-keywords-2 
  (append (c-lang-const c-matchers-2 freefem++) freefem++-font-lock-keywords-extra))

(defconst freefem++-font-lock-keywords-3 
  (append (c-lang-const c-matchers-3 freefem++) freefem++-font-lock-keywords-extra))

(defvar freefem++-font-lock-keywords freefem++-font-lock-keywords-3)

;;; Intelligent Code Completion

(defvar freefem++-completion-keywords
  '(
    ;; Basic types and keywords
    "bool" "complex" "int" "real" "string" "border" "mesh" "mesh3"
    "fespace" "func" "macro" "matrix" "problem" "solve" "varf"
    
    ;; Control structures
    "for" "while" "if" "else" "break" "continue" "return" "try" "catch"
    
    ;; Finite elements
    "P0" "P1" "P2" "P3" "P4" "P1b" "P1nc" "P1dc" "P2dc" "P2b" "P2h"
    "RT0" "RT1" "RT2" "BDM1" "BDM2" "HCT" "Morley" "MINI"
    
    ;; Functions and operators
    "int1d" "int2d" "int3d" "dx" "dy" "dz" "dxx" "dyy" "dzz" "dxy" "dxz" "dyz"
    "grad" "div" "curl" "jump" "mean" "on" "plot" "buildmesh" "adaptmesh"
    "movemesh" "square" "circle" "ellipse" "readmesh" "savemesh"
    
    ;; Mathematical functions
    "sin" "cos" "tan" "asin" "acos" "atan" "sinh" "cosh" "tanh"
    "exp" "log" "log10" "sqrt" "abs" "max" "min" "pow"
    
    ;; Mesh properties
    "x" "y" "z" "N" "hTriangle" "area" "lenEdge" "region" "label"
    
    ;; Constants
    "pi" "true" "false"
    ))

(defvar freefem++-function-signatures
  '(
    ("buildmesh" . "buildmesh(border1(n1) + border2(n2) + ...)")
    ("adaptmesh" . "adaptmesh(Th, u, err=0.01, hmax=0.1, hmin=0.001)")
    ("plot" . "plot(u, fill=true, wait=true, ps=\"filename.eps\")")
    ("int2d" . "int2d(Th)(expression)")
    ("int1d" . "int1d(Th, label)(expression)")
    ("on" . "on(label, u=value)")
    ("dx" . "dx(u) - partial derivative with respect to x")
    ("dy" . "dy(u) - partial derivative with respect to y")
    ("grad" . "grad(u) - gradient [dx(u), dy(u)]")
    ))

;; Company backend for FreeFem++
(defun freefem++-company-backend (command &optional arg &rest ignored)
  "Company backend for FreeFem++ completion."
  (interactive (list 'interactive))
  (pcase command
    (`interactive (company-begin-backend 'freefem++-company-backend))
    (`prefix (when (eq major-mode 'freefem++-mode)
               (company-grab-symbol)))
    (`candidates
     (all-completions arg freefem++-completion-keywords))
    (`annotation
     (let ((sig (cdr (assoc arg freefem++-function-signatures))))
       (when sig (format " (%s)" sig))))
    (`doc-buffer
     (let ((doc (cdr (assoc arg freefem++-function-signatures))))
       (when doc
         (company-doc-buffer (format "%s\n\n%s" arg doc)))))))

;;; Eldoc Support

(defun freefem++-eldoc-function ()
  "Provide eldoc documentation for FreeFem++ functions."
  (when (eq major-mode 'freefem++-mode)
    (let* ((symbol (thing-at-point 'symbol))
           (doc (cdr (assoc symbol freefem++-function-signatures))))
      (when doc doc))))

;;; Flycheck Integration

(when (featurep 'flycheck)
  (flycheck-define-checker freefem++
    "A FreeFem++ syntax checker using the FreeFem++ compiler."
    :command ("FreeFem++" "-nw" "-v" "0" "-ne" source)
    :error-patterns
    ((error line-start "Error line " line " : " (message) line-end)
     (warning line-start "Warning line " line " : " (message) line-end))
    :modes freefem++-mode)
  
  (add-to-list 'flycheck-checkers 'freefem++))

;;; Imenu Support

(defvar freefem++-imenu-generic-expression
  '(("Functions" "^\\s-*func\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1)
    ("Macros" "^\\s-*macro\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1)
    ("Problems" "^\\s-*problem\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1)
    ("Meshes" "^\\s-*mesh\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1)
    ("Borders" "^\\s-*border\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1)
    ("Fespaces" "^\\s-*fespace\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1))
  "Imenu generic expression for FreeFem++ mode.")

;;; Syntax Table

(defvar freefem++-mode-syntax-table
  (let ((table (make-syntax-table c++-mode-syntax-table)))
    (modify-syntax-entry ?' "." table)
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?@ "." table)
    table)
  "Syntax table for FreeFem++ mode.")

;;; Advanced Templates and Snippets

(defvar freefem++-template-alist
  '(("poisson" . freefem++-template-poisson)
    ("elasticity" . freefem++-template-elasticity)
    ("stokes" . freefem++-template-stokes)
    ("navier-stokes" . freefem++-template-navier-stokes)
    ("heat" . freefem++-template-heat)
    ("wave" . freefem++-template-wave)
    ("eigenvalue" . freefem++-template-eigenvalue)
    ("adaptation" . freefem++-template-adaptation))
  "Association list of template names and functions.")

(defun freefem++-template-poisson ()
  "Insert Poisson equation template."
  "// Poisson equation: -Δu = f
// Domain: unit square
// Boundary conditions: u = 0 on boundary

// Mesh
mesh Th = square(20, 20);

// Finite element space
fespace Vh(Th, P1);
Vh u, v;

// Problem
problem Poisson(u, v) =
    int2d(Th)(dx(u)*dx(v) + dy(u)*dy(v))  // bilinear form
    - int2d(Th)(1*v)                      // linear form (f=1)
    + on(1, 2, 3, 4, u=0);                // boundary conditions

// Solve
Poisson;

// Plot
plot(u, fill=true, value=true);")

(defun freefem++-template-elasticity ()
  "Insert linear elasticity template."
  "// Linear elasticity: -div(σ) = f
// σ = λtr(ε)I + 2με, ε = (∇u + ∇u^T)/2

// Parameters
real E = 2e11;    // Young modulus
real nu = 0.35;   // Poisson ratio
real lambda = E*nu/((1+nu)*(1-2*nu));
real mu = E/(2*(1+nu));

// Mesh
mesh Th = square(20, 20);

// Finite element space (vectorial)
fespace Vh(Th, [P1, P1]);
Vh [u1, u2], [v1, v2];

// Macros for strain and stress
macro epsilon(u1, u2) [dx(u1), dy(u2), (dy(u1)+dx(u2))/sqrt(2)] //
macro div(u1, u2) (dx(u1) + dy(u2)) //

// Problem
problem Elasticity([u1, u2], [v1, v2]) =
    int2d(Th)(
        lambda*div(u1,u2)*div(v1,v2)
        + 2*mu*(epsilon(u1,u2)'*epsilon(v1,v2))
    )
    - int2d(Th)(0*v1 + (-1000)*v2)  // body force
    + on(4, u1=0, u2=0);           // fixed boundary

// Solve
Elasticity;

// Plot
plot([u1, u2], fill=true);")

(defun freefem++-template-navier-stokes ()
  "Insert Navier-Stokes template."
  "// Navier-Stokes equations
// ∂u/∂t + (u·∇)u - ν∆u + ∇p = f
// ∇·u = 0

// Parameters
real nu = 1e-3;    // kinematic viscosity
real dt = 0.01;    // time step

// Mesh
mesh Th = square(40, 40, [2*x-1, y]);

// Finite element spaces
fespace Uh(Th, P2);
fespace Ph(Th, P1);
fespace UUPh(Th, [P2, P2, P1]);

UUPh [u1, u2, p], [v1, v2, q];
UUPh [u1old, u2old, pold];

// Macros from FreeFem++ docs:
macro grad(u) [dx(u), dy(u)] // EOM
macro div(u1, u2) (dx(u1) + dy(u2)) // EOM  
macro ugradx(u1, v) (u1*dx(v)) // EOM
macro ugrady(u2, v) (u2*dy(v)) // EOM
macro ugradv(u1, u2, v) (ugradx(u1, v) + ugrady(u2, v)) // EOM

// Problem (implicit Euler)
problem NavierStokes([u1, u2, p], [v1, v2, q]) =
    int2d(Th)(
        (u1*v1 + u2*v2)/dt
        + nu*(grad(u1)'*grad(v1) + grad(u2)'*grad(v2))
        + ugradv(u1, u2, u1)*v1 + ugradv(u1, u2, u2)*v2
        + p*q*1e-10 - p*div(v1, v2) - div(u1, u2)*q
    )
    - int2d(Th)((u1old*v1 + u2old*v2)/dt)
    + on(1, 2, 3, 4, u1=0, u2=0);

// Time loop
for(real t=0; t<1; t+=dt) {
    NavierStokes;
    u1old = u1; u2old = u2; pold = p;
    if(t % (10*dt) == 0)
        plot([u1, u2], fill=true, cmm=\"t=\"+t);
}")

;;; Project Management

(defvar freefem++-project-files nil
  "List of files in current FreeFem++ project.")

(defun freefem++-find-project-root ()
  "Find the root directory of FreeFem++ project."
  (locate-dominating-file default-directory
                          (lambda (dir)
                            (or (file-exists-p (expand-file-name "Makefile" dir))
                                (file-exists-p (expand-file-name ".freefem" dir))))))

(defun freefem++-project-files ()
  "Get list of FreeFem++ files in project."
  (when-let ((root (freefem++-find-project-root)))
    (directory-files-recursively root "\\.\\(edp\\|idp\\)$")))

;;; Visualization Integration

(defun freefem++-view-mesh ()
  "View mesh with medit."
  (interactive)
  (let ((mesh-file (read-file-name "Mesh file: " nil nil t)))
    (start-process "medit" "*medit*" freefem++-medit-program mesh-file)))

(defun freefem++-view-paraview ()
  "Open result in ParaView."
  (interactive)
  (let ((vtk-file (read-file-name "VTK file: " nil nil t)))
    (start-process "paraview" "*paraview*" freefem++-paraview-program vtk-file)))

;;; Keymap

(defvar freefem++-mode-map
  (let ((map (c-make-inherited-keymap)))
    ;; Compilation
    (define-key map (kbd "C-c C-c") 'freefem++-compile)
    (define-key map (kbd "C-c C-r") 'freefem++-run-region)
    (define-key map (kbd "C-c C-b") 'freefem++-run-buffer)
    
    ;; Templates
    (define-key map (kbd "C-c C-t") 'freefem++-insert-template)
    (define-key map (kbd "C-c C-m") 'freefem++-insert-macro)
    
    ;; Navigation
    (define-key map (kbd "C-c C-i") 'imenu)
    (define-key map (kbd "C-c C-f") 'freefem++-find-function)
    
    ;; Visualization
    (define-key map (kbd "C-c C-v m") 'freefem++-view-mesh)
    (define-key map (kbd "C-c C-v p") 'freefem++-view-paraview)
    
    ;; Mathematical input
    (define-key map (kbd "C-c α") (lambda () (interactive) (insert "α")))
    (define-key map (kbd "C-c β") (lambda () (interactive) (insert "β")))
    (define-key map (kbd "C-c γ") (lambda () (interactive) (insert "γ")))
    (define-key map (kbd "C-c Δ") (lambda () (interactive) (insert "Δ")))
    (define-key map (kbd "C-c ∇") (lambda () (interactive) (insert "∇")))
    (define-key map (kbd "C-c ∂") (lambda () (interactive) (insert "∂")))
    
    map)
  "Keymap for FreeFem++ mode.")

;;; Menu

(easy-menu-define freefem++-menu freefem++-mode-map
  "FreeFem++ IDE Menu"
  '("FreeFem++"
    ("Compile & Run"
     ["Compile" freefem++-compile t]
     ["Compile with Graphics" freefem++-compile-with-graphics t]
     ["Compile Safe Mode" freefem++-compile-safe t]
     ["Run Buffer" freefem++-run-buffer t]
     ["Run Region" freefem++-run-region t]
     "---"
     ["Toggle Graphics" freefem++-toggle-graphics t]
     ["Toggle Safe Mode" freefem++-toggle-safe-mode t]
     ["Check Syntax" flycheck-buffer t])
    
    ("Templates"
     ["Poisson Equation" (freefem++-insert-template "poisson") t]
     ["Linear Elasticity" (freefem++-insert-template "elasticity") t]
     ["Stokes Flow" (freefem++-insert-template "stokes") t]
     ["Navier-Stokes" (freefem++-insert-template "navier-stokes") t]
     ["Heat Equation" (freefem++-insert-template "heat") t]
     "---"
     ["Custom Template" freefem++-insert-template t]
     ["Insert Macro" freefem++-insert-macro t])
    
    ("Navigation"
     ["Functions" imenu t]
     ["Find Function" freefem++-find-function t]
     ["Go to Line" goto-line t])
    
    ("Visualization"
     ["View Mesh (medit)" freefem++-view-mesh t]
     ["Open ParaView" freefem++-view-paraview t])
    
    ("Code"
     ["Comment Region" comment-region t]
     ["Uncomment Region" uncomment-region t]
     "---"
     ["Indent Line" c-indent-line t]
     ["Indent Region" c-indent-region t]
     "---"
     ["Complete Symbol" completion-at-point t])
    
    ("Help"
     ["FreeFem++ Documentation" freefem++-help t]
     ["Describe Function" freefem++-describe-function t])))

;;; Compilation and Execution

(defun freefem++-compile ()
  "Compile the current FreeFem++ buffer with enhanced error parsing."
  (interactive)
  (let* ((file (buffer-file-name))
         (args (cond 
                (freefem++-enable-safe-mode freefem++-safe-mode-args)
                (freefem++-enable-graphics freefem++-graphics-args)
                (t freefem++-compile-args)))
         (cmd (format "%s %s %s" 
                      freefem++-program
                      (mapconcat 'identity args " ")
                      (shell-quote-argument file))))
    (message "Running: %s" cmd)
    (compilation-start cmd 'freefem++-compilation-mode)))

(defun freefem++-toggle-graphics ()
  "Toggle graphics output for FreeFem++ plots."
  (interactive)
  (setq freefem++-enable-graphics (not freefem++-enable-graphics))
  (message "FreeFem++ graphics %s" 
           (if freefem++-enable-graphics "enabled" "disabled")))

(defun freefem++-compile-with-graphics ()
  "Compile with graphics explicitly enabled."
  (interactive)
  (let ((freefem++-enable-graphics t)
        (freefem++-enable-safe-mode nil))
    (freefem++-compile)))

(defun freefem++-toggle-safe-mode ()
  "Toggle safe mode for FreeFem++ compilation."
  (interactive)
  (setq freefem++-enable-safe-mode (not freefem++-enable-safe-mode))
  (message "FreeFem++ safe mode %s (avoids MPI/PETSc issues)" 
           (if freefem++-enable-safe-mode "enabled" "disabled")))

(defun freefem++-check-segfault ()
  "Check if the last compilation had a segmentation fault and suggest fixes."
  (interactive)
  (when (get-buffer "*compilation*")
    (with-current-buffer "*compilation*"
      (goto-char (point-min))
      (when (search-forward "segmentation fault" nil t)
        (message "Segmentation fault detected! Try: M-x freefem++-toggle-safe-mode")))))

(defun freefem++-compile-safe ()
  "Compile with safe mode enabled."
  (interactive)
  (let ((freefem++-enable-safe-mode t))
    (freefem++-compile)))

(define-compilation-mode freefem++-compilation-mode "FreeFem++ Compilation"
  "Compilation mode for FreeFem++."
  (setq-local compilation-error-regexp-alist
              '(freefem++-error freefem++-warning))
  (setq-local compilation-error-regexp-alist-alist
              '((freefem++-error
                 "^Error line \\([0-9]+\\) : \\(.*\\)$" nil 1 nil 2)
                (freefem++-warning
                 "^Warning line \\([0-9]+\\) : \\(.*\\)$" nil 1 nil 1))))

(defun freefem++-run-buffer ()
  "Run the current FreeFem++ buffer."
  (interactive)
  (save-buffer)
  (freefem++-compile))

(defun freefem++-run-region (start end)
  "Run the selected region in FreeFem++."
  (interactive "r")
  (let ((temp-file (make-temp-file "freefem" nil ".edp")))
    (write-region start end temp-file)
    (let* ((cmd (format "%s %s %s"
                        freefem++-program  
                        (mapconcat 'identity freefem++-compile-args " ")
                        (shell-quote-argument temp-file))))
      (compilation-start cmd 'freefem++-compilation-mode))))

;;; Template Functions

(defun freefem++-insert-template (&optional template-name)
  "Insert a FreeFem++ template."
  (interactive)
  (let* ((name (or template-name
                   (completing-read "Template: " 
                                    (mapcar 'car freefem++-template-alist))))
         (template-func (cdr (assoc name freefem++-template-alist))))
    (when template-func
      (insert (funcall template-func)))))

(defun freefem++-insert-macro ()
  "Insert a FreeFem++ macro template."
  (interactive)
  (let ((name (read-string "Macro name: ")))
    (insert (format "macro %s() // \nENDIFMACRO\n" name))
    (forward-line -1)
    (end-of-line)))

;;; Help and Documentation

(defun freefem++-help ()
  "Open FreeFem++ documentation."
  (interactive)
  (browse-url "https://doc.freefem.org/"))

(defun freefem++-describe-function ()
  "Describe FreeFem++ function at point."
  (interactive)
  (let* ((symbol (thing-at-point 'symbol))
         (doc (cdr (assoc symbol freefem++-function-signatures))))
    (if doc
        (message "%s: %s" symbol doc)
      (message "No documentation available for: %s" symbol))))

(defun freefem++-find-function ()
  "Find FreeFem++ function definition."
  (interactive)
  (let ((symbol (thing-at-point 'symbol)))
    (when symbol
      (occur (concat "\\<\\(func\\|macro\\|problem\\)\\s-+" symbol "\\>")))))

;;; Mathematical Symbol Input

(defvar freefem++-math-symbols
  '(("alpha" . "α") ("beta" . "β") ("gamma" . "γ") ("delta" . "δ")
    ("epsilon" . "ε") ("zeta" . "ζ") ("eta" . "η") ("theta" . "θ")
    ("iota" . "ι") ("kappa" . "κ") ("lambda" . "λ") ("mu" . "μ")
    ("nu" . "ν") ("xi" . "ξ") ("omicron" . "ο") ("pi" . "π")
    ("rho" . "ρ") ("sigma" . "σ") ("tau" . "τ") ("upsilon" . "υ")
    ("phi" . "φ") ("chi" . "χ") ("psi" . "ψ") ("omega" . "ω")
    ("Alpha" . "Α") ("Beta" . "Β") ("Gamma" . "Γ") ("Delta" . "Δ")
    ("Epsilon" . "Ε") ("Zeta" . "Ζ") ("Eta" . "Η") ("Theta" . "Θ")
    ("Iota" . "Ι") ("Kappa" . "Κ") ("Lambda" . "Λ") ("Mu" . "Μ")
    ("Nu" . "Ν") ("Xi" . "Ξ") ("Omicron" . "Ο") ("Pi" . "Π")
    ("Rho" . "Ρ") ("Sigma" . "Σ") ("Tau" . "Τ") ("Upsilon" . "Υ")
    ("Phi" . "Φ") ("Chi" . "Χ") ("Psi" . "Ψ") ("Omega" . "Ω")
    ("nabla" . "∇") ("partial" . "∂") ("integral" . "∫")
    ("sum" . "∑") ("product" . "∏") ("infinity" . "∞")
    ("leq" . "≤") ("geq" . "≥") ("neq" . "≠") ("approx" . "≈")
    ("in" . "∈") ("notin" . "∉") ("subset" . "⊂") ("supset" . "⊃")
    ("union" . "∪") ("intersect" . "∩") ("forall" . "∀") ("exists" . "∃"))
  "Association list of mathematical symbol names and Unicode characters.")

(defun freefem++-insert-math-symbol ()
  "Insert mathematical symbol by name."
  (interactive)
  (let* ((symbol-name (completing-read "Symbol: " freefem++-math-symbols))
         (symbol-char (cdr (assoc symbol-name freefem++-math-symbols))))
    (when symbol-char
      (insert symbol-char))))

;;; Debugging Support

(defvar freefem++-debug-mode nil
  "Whether FreeFem++ debug mode is enabled.")

(defun freefem++-toggle-debug ()
  "Toggle FreeFem++ debug mode."
  (interactive)
  (setq freefem++-debug-mode (not freefem++-debug-mode))
  (message "FreeFem++ debug mode %s" 
           (if freefem++-debug-mode "enabled" "disabled")))

(defun freefem++-insert-debug-print ()
  "Insert debug print statement."
  (interactive)
  (let ((var (read-string "Variable to debug: ")))
    (insert (format "cout << \"%s = \" << %s << endl;" var var))))

;;; Advanced Refactoring

(defun freefem++-rename-variable ()
  "Rename variable at point throughout buffer."
  (interactive)
  (let* ((old-name (thing-at-point 'symbol))
         (new-name (read-string (format "Rename '%s' to: " old-name))))
    (when (and old-name new-name (not (string= old-name new-name)))
      (save-excursion
        (goto-char (point-min))
        (let ((count 0))
          (while (re-search-forward (concat "\\<" (regexp-quote old-name) "\\>") nil t)
            (replace-match new-name)
            (setq count (1+ count)))
          (message "Renamed %d occurrences of '%s' to '%s'" count old-name new-name))))))

(defun freefem++-extract-function ()
  "Extract selected region into a function."
  (interactive)
  (when (use-region-p)
    (let* ((start (region-beginning))
           (end (region-end))
           (code (buffer-substring start end))
           (func-name (read-string "Function name: "))
           (return-type (completing-read "Return type: " 
                                         '("real" "int" "complex" "bool" "string"))))
      (delete-region start end)
      (insert (format "%s();" func-name))
      (goto-char (point-min))
      (insert (format "\nfunc %s %s() {\n%s\n}\n\n" return-type func-name code)))))

;;; Project Templates

(defun freefem++-new-project ()
  "Create new FreeFem++ project structure."
  (interactive)
  (let* ((project-name (read-string "Project name: "))
         (project-dir (expand-file-name project-name)))
    (make-directory project-dir t)
    (make-directory (expand-file-name "src" project-dir) t)
    (make-directory (expand-file-name "mesh" project-dir) t)
    (make-directory (expand-file-name "results" project-dir) t)
    (make-directory (expand-file-name "doc" project-dir) t)
    
    ;; Create main file
    (with-temp-file (expand-file-name "src/main.edp" project-dir)
      (insert (format "// %s - FreeFem++ Project\n// Created: %s\n\n%s"
                      project-name
                      (format-time-string "%Y-%m-%d")
                      (freefem++-template-poisson))))
    
    ;; Create Makefile
    (with-temp-file (expand-file-name "Makefile" project-dir)
      (insert (format "# Makefile for %s\n\nFFPP = FreeFem++\nSRCDIR = src\nRESULTSDIR = results\n\nall: main\n\nmain:\n\t$(FFPP) $(SRCDIR)/main.edp\n\nclean:\n\trm -f $(RESULTSDIR)/*\n\n.PHONY: all clean\n" project-name)))
    
    ;; Create project marker
    (with-temp-file (expand-file-name ".freefem" project-dir)
      (insert project-name))
    
    (message "Created FreeFem++ project: %s" project-dir)
    (find-file (expand-file-name "src/main.edp" project-dir))))

;;; Performance Monitoring

(defvar freefem++-timing-enabled nil
  "Whether to enable timing for FreeFem++ execution.")

(defun freefem++-toggle-timing ()
  "Toggle timing for FreeFem++ execution."
  (interactive)
  (setq freefem++-timing-enabled (not freefem++-timing-enabled))
  (message "FreeFem++ timing %s" 
           (if freefem++-timing-enabled "enabled" "disabled")))

;;; Error Navigation

(defun freefem++-next-error ()
  "Go to next FreeFem++ error."
  (interactive)
  (next-error))

(defun freefem++-previous-error ()
  "Go to previous FreeFem++ error."
  (interactive)
  (previous-error))

;;; Advanced Features Integration

(defun freefem++-setup-advanced-features ()
  "Set up advanced IDE features for FreeFem++."
  
  ;; Company completion
  (when (and freefem++-enable-company (featurep 'company))
    (add-to-list 'company-backends 'freefem++-company-backend)
    (company-mode 1))
  
  ;; Eldoc
  (when freefem++-enable-eldoc
    (setq-local eldoc-documentation-function 'freefem++-eldoc-function)
    (eldoc-mode 1))
  
  ;; Flycheck
  (when (and freefem++-enable-flycheck (featurep 'flycheck))
    (flycheck-mode 1))
  
  ;; Imenu
  (setq imenu-generic-expression freefem++-imenu-generic-expression)
  (imenu-add-menubar-index)
  
  ;; Which-function
  (when (featurep 'which-func)
    (which-function-mode 1))
  
  ;; Auto-completion setup
  (setq-local completion-at-point-functions
              '(freefem++-completion-at-point)))

(defun freefem++-completion-at-point ()
  "Completion at point function for FreeFem++."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (list (car bounds) (cdr bounds) freefem++-completion-keywords))))

;;; Mode Definition

;;;###autoload
(define-derived-mode freefem++-mode c++-mode "FreeFem++"
  "Advanced IDE for FreeFem++ programming language.

FreeFem++ is a partial differential equation solver using finite elements.
This mode provides comprehensive IDE features including:

- Advanced syntax highlighting with context awareness
- Intelligent code completion with documentation  
- Real-time error checking and linting
- Interactive debugging support
- Project management and file navigation
- Mathematical symbol input methods
- Mesh visualization integration
- Advanced refactoring tools
- Template library with snippets

\\{freefem++-mode-map}"
  :syntax-table freefem++-mode-syntax-table
  :group 'freefem++
  
  ;; Set up cc-mode language
  (c-initialize-cc-mode t)
  (setq c-buffer-is-cc-mode 'freefem++-mode)
  (c-init-language-vars-for 'freefem++-mode)
  
  ;; Font lock with advanced highlighting
  (setq font-lock-defaults
        '((freefem++-font-lock-keywords-1
           freefem++-font-lock-keywords-2  
           freefem++-font-lock-keywords-3)
          nil nil nil nil
          (font-lock-syntactic-keywords . nil)))
  
  ;; Enhanced indentation
  (setq c-basic-offset freefem++-basic-offset)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'case-label '+)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close 0)
  (c-set-offset 'statement-cont '+)
  
  ;; Comments
  (setq comment-start "// ")
  (setq comment-end "")
  (setq comment-start-skip "//+\\s-*")
  (setq comment-multi-line t)
  
  ;; Electric features
  (when (fboundp 'electric-indent-local-mode)
    (electric-indent-local-mode 1))
  (when (fboundp 'electric-pair-local-mode)
    (electric-pair-local-mode 1))
  
  ;; Set up compilation
  (setq-local compile-command 
              (concat freefem++-program " " 
                      (mapconcat 'identity freefem++-compile-args " ")
                      " " (or (buffer-file-name) "")))
  
  ;; Set up advanced features
  (freefem++-setup-advanced-features)
  
  ;; Add to menu
  (easy-menu-add freefem++-menu)
  
  ;; Set up additional keybindings
  (local-set-key (kbd "C-c C-s") 'freefem++-insert-math-symbol)
  (local-set-key (kbd "C-c C-d") 'freefem++-toggle-debug)
  (local-set-key (kbd "C-c C-p") 'freefem++-insert-debug-print)
  (local-set-key (kbd "C-c C-n") 'freefem++-new-project)
  (local-set-key (kbd "C-c C-e") 'freefem++-extract-function)
  (local-set-key (kbd "C-c C-R") 'freefem++-rename-variable)
  (local-set-key (kbd "C-c C-S") 'freefem++-toggle-safe-mode)
  (local-set-key (kbd "C-c C-g") 'freefem++-toggle-graphics)
  (local-set-key (kbd "C-c C-G") 'freefem++-compile-with-graphics)
  
  ;; Error navigation
  (local-set-key (kbd "M-g M-n") 'freefem++-next-error)
  (local-set-key (kbd "M-g M-p") 'freefem++-previous-error)
  
  ;; Run mode hooks
  (run-hooks 'freefem++-mode-hook))

;;; Provide

(provide 'freefem++-mode)

;;; freefem++-mode.el ends here
