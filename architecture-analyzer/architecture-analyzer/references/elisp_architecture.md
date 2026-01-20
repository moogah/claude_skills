# Emacs Lisp Architecture Reference

## Overview

Emacs Lisp (Elisp) is the extension and customization language for GNU Emacs. It's a dialect of Lisp designed specifically for text editing and editor extension. Understanding Elisp architecture requires grasping its unique characteristics that differ significantly from mainstream programming languages.

### Key Characteristics

- **S-expression syntax**: All code and data are represented as symbolic expressions
- **Interactive environment**: Code can be evaluated and modified while Emacs is running
- **Extension-oriented**: Designed for extending and customizing the editor
- **Global namespace by default**: All top-level definitions create global symbols
- **Buffer-centric**: Many operations work on text buffers as primary data structures
- **Event-driven**: Hooks and advice enable reactive programming patterns

### Differences from Mainstream Languages

Unlike typical application languages, Elisp:
- Has a global namespace requiring explicit naming conventions for modularity
- Supports both dynamic and lexical scoping (historically dynamic, transitioning to lexical)
- Provides buffer-local variables (variables with different values per buffer)
- Uses a mode system for composable behavior (major modes vs minor modes)
- Exposes extensive customization through hooks and advice

---

## Namespacing & Global Scope

### The Global Namespace Challenge

**All top-level definitions create global symbols.** There are no built-in modules, classes, or namespaces to isolate code. This means every `defun`, `defvar`, `defcustom`, and `defconst` creates a symbol in the global namespace.

### Package Prefix Convention

**Convention:** Prefix all symbols with your package name followed by a hyphen.

```elisp
;; Good: Package is "projectile"
(defun projectile-project-root ()
  "Return the project root directory.")

(defvar projectile-cache-file "~/.emacs.d/projectile.cache"
  "Path to projectile's cache file.")

;; Bad: No prefix (pollutes global namespace)
(defun project-root ()  ; Could conflict with project.el
  "Return the project root directory.")
```

### Private Functions Convention

**Convention:** Use double-dash `--` to indicate internal/private functions.

```elisp
;; Public API
(defun mypackage-process-data (data)
  "Process DATA and return result."
  (mypackage--validate-input data)
  (mypackage--transform data))

;; Private helpers (not part of public API)
(defun mypackage--validate-input (data)
  "Validate DATA structure. Internal use only."
  ...)

(defun mypackage--transform (data)
  "Transform DATA. Internal use only."
  ...)
```

### Anti-Pattern: Missing Prefixes

```elisp
;; ANTI-PATTERN: Generic names without package prefix
(defvar cache-file "~/.cache")           ; Conflicts likely!
(defun refresh ()                        ; Too generic!
  "Refresh the view.")

;; CORRECT: With package prefix
(defvar mypackage-cache-file "~/.cache")
(defun mypackage-refresh ()
  "Refresh mypackage's view.")
```

**Why this matters:** Missing prefixes cause subtle bugs when multiple packages define similarly-named symbols. The last loaded package wins, silently overwriting previous definitions.

---

## Variable Scoping

### Dynamic vs Lexical Scoping

Elisp historically used **dynamic scoping** (variable lookup based on call stack at runtime). Modern Elisp supports **lexical scoping** (variable lookup based on textual structure of code).

**Dynamic scoping is widely regarded as a mistake** but remains the default for backward compatibility.

### Enabling Lexical Binding

**Always use lexical binding in new code:**

```elisp
;;; mypackage.el --- My package description  -*- lexical-binding: t; -*-

;; Your code here
```

The `;;; -*- lexical-binding: t; -*-` header must be on the **first line** of the file.

### Performance Implications

Lexical binding is significantly faster than dynamic binding:

- **Variable access**: 30% faster with lexical binding
- **Variable assignment**: 650% faster with lexical binding

Source: [Some Performance Advantages of Lexical Scope](https://nullprogram.com/blog/2016/12/22/)

### When Dynamic Scoping is Still Used

Dynamic scoping is still needed for:
1. **Special variables** intended to be dynamically bound (e.g., `case-fold-search`, `load-path`)
2. **Customization variables** defined with `defcustom`
3. **Backward compatibility** with old code

**Important:** `defvar` creates a **special variable** (dynamically scoped) even in lexical-binding files.

```elisp
;;; -*- lexical-binding: t; -*-

;; This is dynamically scoped (special variable)
(defvar mypackage-config-dir "~/.config/mypackage"
  "Directory for mypackage configuration.")

;; This is lexically scoped (normal let-binding)
(let ((temp-value 42))
  (message "temp-value: %s" temp-value))
```

### Anti-Pattern: Not Using Lexical Binding

```elisp
;; ANTI-PATTERN: No lexical-binding header
;;; mypackage.el --- My package description

;; Code here runs with dynamic scoping (slower, error-prone)

;; CORRECT: Enable lexical binding
;;; mypackage.el --- My package description  -*- lexical-binding: t; -*-
```

---

## Variable Definition Forms

### defvar - Variables

Defines a **special variable** (dynamically scoped, even in lexical-binding files).

```elisp
(defvar mypackage-default-directory "~/.mypackage"
  "Default directory for mypackage files.")
```

**Characteristics:**
- Creates a global variable
- Always dynamically scoped (special variable)
- Won't overwrite value if variable already has one
- Requires a docstring

### defcustom - User-Customizable Variables

The **proper way** to define user-facing configuration options.

```elisp
(defcustom mypackage-auto-save t
  "When non-nil, automatically save state."
  :type 'boolean
  :group 'mypackage)
```

**Characteristics:**
- Shows up in `M-x customize` interface
- Includes type specification (`:type`)
- Can include validation
- Organized into customization groups
- Always use for user-facing options

### defconst - Constants

Defines constants (though Elisp doesn't enforce immutability).

```elisp
(defconst mypackage-version "1.2.3"
  "Version of mypackage.")
```

**Convention:** Use UPPERCASE for true constants, regular-case for configuration constants.

### defvar-local - Buffer-Local Variables

Modern way to define automatically buffer-local variables.

```elisp
(defvar-local mypackage-buffer-state nil
  "State specific to this buffer.")
```

Equivalent to:
```elisp
(defvar mypackage-buffer-state nil
  "State specific to this buffer.")
(make-variable-buffer-local 'mypackage-buffer-state)
```

### Anti-Pattern: Undefined Variables

```elisp
;; ANTI-PATTERN: Using setq without prior defvar
(setq mypackage-cache nil)  ; Compiler warning! Variable not declared

;; CORRECT: Always declare with defvar first
(defvar mypackage-cache nil
  "Cache for mypackage data.")
```

---

## Function Definition & Conventions

### defun - Standard Functions

```elisp
(defun mypackage-process-file (filename)
  "Process FILENAME and return the result.

FILENAME should be an absolute path to a text file."
  (with-temp-buffer
    (insert-file-contents filename)
    (mypackage--transform-buffer)))
```

**Requirements:**
- Docstring is mandatory for public functions
- First line of docstring should be a complete sentence
- Argument names in docstring should be UPPERCASE
- Private functions (with `--`) should note "Internal use only"

### Interactive Commands

Functions callable via `M-x` require an `(interactive)` form:

```elisp
(defun mypackage-refresh ()
  "Refresh the mypackage view."
  (interactive)
  (mypackage--rebuild-view)
  (message "Mypackage view refreshed"))
```

**Reading arguments interactively:**

```elisp
(defun mypackage-open-file (filename)
  "Open FILENAME in mypackage mode."
  (interactive "fFile to open: ")  ; "f" = read file
  (find-file filename)
  (mypackage-mode))

(defun mypackage-search (term)
  "Search for TERM in project."
  (interactive "sSearch term: ")  ; "s" = read string
  (mypackage--search-project term))
```

See `C-h f interactive` for all interactive codes.

### Naming Conventions

**Predicates** (functions returning boolean):
- Multi-word: `-p` suffix (`mypackage-active-p`)
- Single-word: `p` suffix (`bufferp`, `stringp`)

```elisp
(defun mypackage-active-p ()
  "Return non-nil if mypackage is active."
  (and (boundp 'mypackage-mode)
       mypackage-mode))
```

**Commands** often end in action verbs:
- `mypackage-refresh`
- `mypackage-toggle-mode`
- `mypackage-find-file`

### Anti-Pattern: Undocumented Functions

```elisp
;; ANTI-PATTERN: No docstring
(defun mypackage-process (data)
  (mapcar #'mypackage--transform data))

;; CORRECT: Always include docstring
(defun mypackage-process (data)
  "Process each element of DATA and return the results."
  (mapcar #'mypackage--transform data))
```

---

## Major & Minor Modes

### Major Modes

**One major mode per buffer, mutually exclusive.** Major modes define the primary editing behavior for a buffer (e.g., `python-mode`, `org-mode`, `text-mode`).

**Use `define-derived-mode` macro** (handles conventions automatically):

```elisp
(define-derived-mode mypackage-mode prog-mode "MyPackage"
  "Major mode for editing MyPackage files."
  ;; Set buffer-local variables
  (setq-local comment-start "# ")
  (setq-local comment-end "")

  ;; Set up syntax highlighting
  (setq-local font-lock-defaults '(mypackage-font-lock-keywords))

  ;; Set up indentation
  (setq-local indent-line-function #'mypackage-indent-line))
```

**What `define-derived-mode` does automatically:**
- Creates mode command (`mypackage-mode`)
- Creates mode hook (`mypackage-mode-hook`)
- Creates keymap (`mypackage-mode-map`)
- Runs parent mode initialization
- Runs mode hook

### Minor Modes

**Multiple minor modes can coexist in a buffer.** Minor modes add composable features (e.g., `auto-fill-mode`, `line-number-mode`).

**Use `define-minor-mode` macro:**

```elisp
(define-minor-mode mypackage-highlight-mode
  "Toggle highlighting of special syntax in buffer."
  :lighter " MyHL"
  :keymap mypackage-highlight-mode-map
  (if mypackage-highlight-mode
      (mypackage--enable-highlighting)
    (mypackage--disable-highlighting)))
```

**Parameters:**
- `:lighter` - Mode line indicator
- `:keymap` - Key bindings active when mode is on
- `:global` - Whether it's a global minor mode

### Mode Hooks

Every major mode provides a hook that runs after mode initialization:

```elisp
;; User configuration
(add-hook 'python-mode-hook #'mypackage-python-setup)

(defun mypackage-python-setup ()
  "Setup mypackage features for Python editing."
  (mypackage-highlight-mode 1)
  (setq-local mypackage-syntax-style 'python))
```

### Keymap Conventions

**Major mode keymaps** should use:
- `C-c` followed by control/digit/special character
- Example: `C-c C-c`, `C-c C-r`, `C-c 1`

**Minor mode keymaps** can use:
- `C-c` followed by punctuation (not letters)
- Example: `C-c !`, `C-c @`

**User bindings:**
- `C-c` followed by ordinary letters (reserved for users)
- Example: `C-c m`, `C-c p`

```elisp
(defvar mypackage-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-r") #'mypackage-refresh)
    (define-key map (kbd "C-c C-c") #'mypackage-compile)
    map)
  "Keymap for `mypackage-mode'.")
```

### Buffer-Local Settings in Modes

Modes should use `setq-local` or `make-local-variable` for buffer-specific configuration:

```elisp
(define-derived-mode mypackage-mode text-mode "MyPackage"
  "Major mode for MyPackage files."
  ;; Make settings buffer-local
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2)
  (setq-local comment-start "// ")

  ;; Set up buffer-local hooks
  (add-hook 'before-save-hook #'mypackage--cleanup-buffer nil t))
  ;;                                                        ^   ^
  ;;                                                        |   local
  ;;                                                        prepend=nil
```

### Anti-Patterns

**Major modes interfering with each other:**
```elisp
;; ANTI-PATTERN: Setting global variables in major mode
(define-derived-mode mypackage-mode text-mode "MyPackage"
  (setq indent-tabs-mode nil))  ; Affects ALL buffers!

;; CORRECT: Use setq-local
(define-derived-mode mypackage-mode text-mode "MyPackage"
  (setq-local indent-tabs-mode nil))  ; Affects only this buffer
```

---

## Buffer-Local Variables (In-Depth)

### What Are Buffer-Local Variables?

Buffer-local variables have **different values in different buffers**. When you switch buffers, the variable's value changes to that buffer's local value.

**Common use case:** Editor settings that vary by buffer type.

```elisp
;; Global default
(setq tab-width 8)

;; In buffer A
(setq-local tab-width 2)  ; tab-width is now 2 in buffer A

;; In buffer B
(setq-local tab-width 4)  ; tab-width is now 4 in buffer B

;; Switching to buffer A: tab-width is 2
;; Switching to buffer B: tab-width is 4
;; In new buffer C: tab-width is 8 (global default)
```

### Creating Buffer-Local Variables

#### Method 1: make-local-variable

**Use in major mode commands** to make a variable buffer-local in the **current buffer only**.

```elisp
(define-derived-mode my-mode text-mode "My"
  "My major mode."
  (make-local-variable 'my-mode-state)
  (setq my-mode-state 'initial))
```

**Effect:** Only buffers where `my-mode` is activated will have a buffer-local `my-mode-state`. Other buffers share the global value.

#### Method 2: make-variable-buffer-local

Makes a variable **automatically buffer-local in ALL buffers** (current and future).

```elisp
(defvar my-internal-state nil
  "Internal state. Automatically buffer-local.")

(make-variable-buffer-local 'my-internal-state)
```

**Effect:** Every `setq` on this variable makes it buffer-local in that buffer automatically.

**Rare use case:** When NO two buffers should ever share the same value.

#### Method 3: defvar-local

Modern macro combining `defvar` + `make-variable-buffer-local`:

```elisp
(defvar-local my-buffer-state nil
  "State specific to this buffer.")
```

Equivalent to:
```elisp
(defvar my-buffer-state nil
  "State specific to this buffer.")
(make-variable-buffer-local 'my-buffer-state)
```

#### Method 4: setq-local

Sets value **and** makes the variable buffer-local in one operation (Emacs 24.3+):

```elisp
(setq-local my-config-value 42)
```

Equivalent to:
```elisp
(make-local-variable 'my-config-value)
(setq my-config-value 42)
```

### When to Use Each Method

| Method | When to Use |
|--------|-------------|
| `make-local-variable` | In major mode commands (affects only current buffer) |
| `make-variable-buffer-local` | When NO two buffers should share a value (rare!) |
| `defvar-local` | Defining variables that are always buffer-local |
| `setq-local` | Setting buffer-local values (most common in code) |

**Do NOT use `make-variable-buffer-local` for user options:** Users may want different customizations via `setq` in their init file, which won't work correctly with auto buffer-local variables.

### setq Variants

**setq** - Sets value:
- If variable is already buffer-local: sets buffer-local value
- If variable is not buffer-local: sets global value

```elisp
(setq my-var 10)  ; Sets global or buffer-local if already local
```

**setq-local** - Sets value AND makes buffer-local:

```elisp
(setq-local my-var 10)  ; Always creates buffer-local binding
```

**setq-default** - Sets the **default (global) value**:

```elisp
(setq-default my-var 10)  ; Sets global default, doesn't affect current buffer
```

### Common Pitfalls

#### Pitfall 1: Making Buffer-Local Inside let-Binding

```elisp
;; ANTI-PATTERN: Unreliable unless buffer is current
(let ((buf (get-buffer "foo")))
  (with-current-buffer buf
    (make-local-variable 'my-var)  ; Works
    (setq my-var 42)))

;; ANTI-PATTERN: Doesn't work as expected
(let ((buf (get-buffer "foo")))
  (make-local-variable 'my-var)  ; Wrong buffer!
  (with-current-buffer buf
    (setq my-var 42)))
```

**Rule:** Always call `make-local-variable` when the target buffer is current.

#### Pitfall 2: Using make-local-variable on Hooks

```elisp
;; ANTI-PATTERN: Unnecessary
(make-local-variable 'before-save-hook)
(add-hook 'before-save-hook #'my-cleanup)

;; CORRECT: add-hook has LOCAL parameter
(add-hook 'before-save-hook #'my-cleanup nil t)
;;                                         ^   ^
;;                                         |   local
;;                                         prepend=nil
```

Hook variables **automatically become buffer-local** when you use the LOCAL argument in `add-hook`.

#### Pitfall 3: Using make-variable-buffer-local for User Options

```elisp
;; ANTI-PATTERN: User can't customize easily
(defcustom my-user-option nil
  "User-facing option."
  :type 'boolean)
(make-variable-buffer-local 'my-user-option)

;; User's init file:
(setq my-user-option t)  ; Only affects *scratch* buffer!

;; CORRECT: Don't make user options auto buffer-local
;; Users can make them buffer-local themselves if needed
```

#### Pitfall 4: Using setq in Init File for Auto Buffer-Local Variables

```elisp
;; In package code:
(defvar-local my-package-state nil)

;; User's init file:
(setq my-package-state 'active)  ; Only affects init buffer!

;; CORRECT: Users should use setq-default
(setq-default my-package-state 'active)
```

#### Pitfall 5: Creating Without Prior defvar

```elisp
;; ANTI-PATTERN: No defvar
(make-variable-buffer-local 'my-undeclared-var)  ; Compiler warning

;; CORRECT: Always declare first
(defvar my-declared-var nil
  "My variable documentation.")
(make-variable-buffer-local 'my-declared-var)
```

### Pattern: Major Modes and Buffer-Local State

```elisp
(define-derived-mode mypackage-mode prog-mode "MyPackage"
  "Major mode for MyPackage files."
  ;; Option 1: make-local-variable + setq
  (make-local-variable 'mypackage-parse-tree)
  (setq mypackage-parse-tree nil)

  ;; Option 2: setq-local (more concise, same effect)
  (setq-local comment-start "# ")
  (setq-local comment-end "")

  ;; Add buffer-local hook
  (add-hook 'before-save-hook #'mypackage-cleanup-buffer nil t))
```

---

## Hooks & Advice

### Hooks

Hooks are lists of functions called at specific points in execution. They implement the observer pattern.

#### Normal Hooks

**Normal hooks** (suffix `-hook`) are called with `run-hooks` and take **no arguments**.

```elisp
;; Define a hook
(defvar mypackage-after-load-hook nil
  "Hook run after mypackage loads.")

;; Add function to hook
(add-hook 'mypackage-after-load-hook #'my-setup-function)

;; Run the hook
(run-hooks 'mypackage-after-load-hook)
```

#### Abnormal Hooks

**Abnormal hooks** (suffix `-functions`) have special calling conventions:
- May receive arguments
- Return values may be used
- Called with `run-hook-with-args`, `run-hook-with-args-until-success`, etc.

```elisp
;; Define abnormal hook
(defvar mypackage-filter-functions nil
  "Hook for filtering data. Each function receives DATA and returns filtered DATA.")

;; Add function
(add-hook 'mypackage-filter-functions #'my-filter)

(defun my-filter (data)
  "Filter DATA by removing empty elements."
  (seq-remove #'string-empty-p data))

;; Run hook with argument
(run-hook-with-args 'mypackage-filter-functions data)
```

#### Buffer-Local Hooks

Use the **LOCAL** parameter in `add-hook` to make a hook buffer-local automatically:

```elisp
;; Add hook locally to current buffer only
(add-hook 'before-save-hook #'mypackage-cleanup-buffer nil t)
;;                                                       ^   ^
;;                                                       |   local
;;                                                       prepend=nil

;; Remove local hook
(remove-hook 'before-save-hook #'mypackage-cleanup-buffer t)
```

**The hook variable automatically becomes buffer-local** when you add a local hook function.

#### Mode Hooks

Every major mode defines a mode hook:

```elisp
;; Runs when python-mode is activated
(add-hook 'python-mode-hook #'my-python-setup)

(defun my-python-setup ()
  "Setup for Python buffers."
  (setq-local tab-width 4)
  (electric-pair-local-mode 1))
```

#### Anti-Pattern: Manually Making Hooks Buffer-Local

```elisp
;; ANTI-PATTERN: Unnecessary
(make-local-variable 'after-save-hook)
(add-hook 'after-save-hook #'my-function)

;; CORRECT: Use LOCAL parameter
(add-hook 'after-save-hook #'my-function nil t)
```

### Advice System

Advice allows modifying function behavior without changing the function definition. Use for aspect-oriented programming (logging, tracing, conditional modification).

**Two systems exist:**
1. **Old advice.el** (`defadvice`, 1993) - Complex, deprecated
2. **New nadvice.el** (`advice-add`, 2012) - **Preferred for all new code**

#### Advising Named Functions (advice-add)

**Recommended approach** for advising named functions.

```elisp
;; Define advice function
(defun mypackage-log-save (orig-fun &rest args)
  "Log when save functions are called."
  (message "Saving file: %s" (buffer-file-name))
  (apply orig-fun args))

;; Add advice
(advice-add 'save-buffer :around #'mypackage-log-save)

;; Remove advice
(advice-remove 'save-buffer #'mypackage-log-save)
```

**Advantages:**
- Works with macros and autoloaded functions
- Can add advice before function is defined
- Preserves original function's docstring
- Simpler than old `defadvice`

#### Advising Function Values (add-function)

For variables or object fields holding functions:

```elisp
;; Advise a variable holding a function
(add-function :before (symbol-function 'my-var-holding-function)
              #'my-before-advice)
```

**Less common** than `advice-add`. Use for variables, not for named functions.

#### Advice Combinators

**:before** - Run before the original function:

```elisp
(defun mypackage-before-save ()
  "Run before save-buffer."
  (message "About to save!"))

(advice-add 'save-buffer :before #'mypackage-before-save)
```

**:after** - Run after the original function:

```elisp
(defun mypackage-after-save (&rest _args)
  "Run after save-buffer."
  (message "Saved!"))

(advice-add 'save-buffer :after #'mypackage-after-save)
```

**:around** - Wrap the original function (most powerful):

```elisp
(defun mypackage-around-save (orig-fun &rest args)
  "Wrap save-buffer with custom logic."
  (message "Before save")
  (let ((result (apply orig-fun args)))
    (message "After save")
    result))

(advice-add 'save-buffer :around #'mypackage-around-save)
```

**:filter-return** - Modify return value:

```elisp
(defun mypackage-filter-return (result)
  "Modify RESULT returned by function."
  (upcase result))

(advice-add 'some-string-function :filter-return #'mypackage-filter-return)
```

**:filter-args** - Modify arguments before passing to function:

```elisp
(defun mypackage-filter-args (args)
  "Modify ARGS before passing to function."
  (mapcar #'upcase args))

(advice-add 'some-function :filter-args #'mypackage-filter-args)
```

**:override** - Replace the function entirely:

```elisp
(defun mypackage-override (&rest args)
  "Completely replace original function."
  (message "Original function replaced!"))

(advice-add 'some-function :override #'mypackage-override)
```

### When to Use Advice vs Hooks

**Prefer hooks when available** (cleaner, intended extension point):

```elisp
;; GOOD: Using a hook (if available)
(add-hook 'before-save-hook #'mypackage-cleanup)

;; AVOID: Advising when a hook exists
(advice-add 'save-buffer :before #'mypackage-cleanup)
```

**Use advice only when:**
- No hook exists for the extension point
- You need to modify arguments or return values
- You need to wrap behavior around a function

**For public packages:** Consider asking Emacs developers to add hooks rather than advising core functions. Hooks are more stable and maintainable.

### Anti-Patterns

**Overusing advice when hooks exist:**
```elisp
;; ANTI-PATTERN: Advising when a hook exists
(advice-add 'after-init :after #'my-init-function)

;; CORRECT: Use the hook
(add-hook 'after-init-hook #'my-init-function)
```

**Using old defadvice in new code:**
```elisp
;; ANTI-PATTERN: Old advice system
(defadvice save-buffer (before my-advice activate)
  "Old-style advice."
  (message "Saving..."))

;; CORRECT: New advice system
(advice-add 'save-buffer :before #'my-advice)
(defun my-advice (&rest _)
  "New-style advice."
  (message "Saving..."))
```

---

## Interactive Commands & Keymaps

### Interactive Specifications

The `(interactive)` form makes a function callable via `M-x` and bindable to keys.

#### Simple Interactive

```elisp
(defun mypackage-toggle-feature ()
  "Toggle mypackage feature on/off."
  (interactive)
  (setq mypackage-feature-enabled (not mypackage-feature-enabled))
  (message "Feature %s" (if mypackage-feature-enabled "enabled" "disabled")))
```

#### Reading Arguments

Use interactive codes to read arguments from the user:

```elisp
(defun mypackage-search-symbol (symbol)
  "Search for SYMBOL in project."
  (interactive "SSymbol: ")  ; S = read symbol
  (mypackage--search symbol))

(defun mypackage-open-file (filename)
  "Open FILENAME."
  (interactive "fFile: ")  ; f = read file (with completion)
  (find-file filename))

(defun mypackage-replace-region (start end text)
  "Replace region from START to END with TEXT."
  (interactive "r\nsReplacement text: ")  ; r = region, s = string
  (delete-region start end)
  (insert text))
```

**Common interactive codes:**
- `s` - Read string
- `S` - Read symbol
- `f` - Read file name
- `d` - Read directory
- `b` - Read buffer name
- `r` - Region (start and end positions)
- `n` - Read number
- `p` - Prefix argument as number

#### Using Prefix Arguments

```elisp
(defun mypackage-insert-times (n)
  "Insert text N times. With prefix arg, use that as N."
  (interactive "p")  ; p = prefix arg as number (defaults to 1)
  (dotimes (_ n)
    (insert "Hello\n")))

;; C-u 5 M-x mypackage-insert-times  → inserts "Hello" 5 times
```

#### Programmatic Interactive

```elisp
(defun mypackage-smart-command (arg)
  "Command that prompts differently based on context."
  (interactive
   (list
    (if (use-region-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (read-string "Input: "))))
  (message "You entered: %s" arg))
```

### Keymap Architecture

#### Creating Keymaps

```elisp
(defvar mypackage-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'mypackage-compile)
    (define-key map (kbd "C-c C-r") #'mypackage-run)
    (define-key map (kbd "C-c C-t") #'mypackage-test)
    map)
  "Keymap for `mypackage-mode'.")
```

#### Keymap Hierarchy

Emacs searches keymaps in order:
1. **Overriding keymaps** (highest priority)
2. **Local keymap** (major mode keymap)
3. **Minor mode keymaps** (in order of activation)
4. **Global keymap** (lowest priority)

#### Binding Conventions

**Major mode keymaps:**
- Use `C-c` + control/digit/special character
- Examples: `C-c C-c`, `C-c C-r`, `C-c 1`, `C-c !`

**Minor mode keymaps:**
- Use `C-c` + punctuation (not letters)
- Examples: `C-c !`, `C-c @`, `C-c %`

**Reserved for users:**
- `C-c` + letter (ordinary letters)
- Examples: `C-c m`, `C-c p`

```elisp
;; GOOD: Major mode uses C-c C-<char>
(define-key mypackage-mode-map (kbd "C-c C-c") #'mypackage-compile)

;; GOOD: Minor mode uses C-c <punctuation>
(define-key mypackage-minor-mode-map (kbd "C-c !") #'mypackage-toggle)

;; BAD: Stealing user binding space
(define-key mypackage-mode-map (kbd "C-c m") #'mypackage-menu)  ; Reserved!
```

#### Keymap in define-derived-mode

```elisp
(defvar mypackage-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'mypackage-compile)
    map))

(define-derived-mode mypackage-mode prog-mode "MyPkg"
  "Major mode for MyPackage files."
  ;; Keymap automatically used (via mypackage-mode-map)
  )
```

---

## External Data Handling (JSON, YAML, REST APIs)

### JSON Parsing

Elisp provides two approaches for JSON handling: native C-based functions (faster, modern) and traditional Elisp implementation (broader compatibility).

#### A. Native JSON Functions (Recommended, Emacs 27+)

**Fast native parsing** using libjansson (Emacs 27-29) or native implementation (Emacs 30+).

**json-parse-buffer** - Parse JSON from buffer:

```elisp
(defun mypackage-load-config (filepath)
  "Load JSON configuration from FILEPATH."
  (with-temp-buffer
    (insert-file-contents filepath)
    (goto-char (point-min))
    (json-parse-buffer :object-type 'alist
                      :array-type 'list
                      :null-object nil
                      :false-object :false)))
```

**json-parse-string** - Parse JSON from string:

```elisp
(let ((json-string "{\"name\": \"test\", \"value\": 42}"))
  (json-parse-string json-string :object-type 'plist))
;; => (:name "test" :value 42)
```

**json-serialize** - Convert Lisp to JSON:

```elisp
(json-serialize '((name . "test") (value . 42)))
;; => "{\"name\":\"test\",\"value\":42}"
```

**Configuration Options:**

- `:object-type` - How to represent JSON objects
  - `hash-table` (default) - Fast lookup
  - `alist` - Association list, easy to work with
  - `plist` - Property list, Lisp-style

- `:array-type` - How to represent JSON arrays
  - `array` (default) - Vector
  - `list` - Linked list

- `:null-object` - Lisp value for JSON `null`
- `:false-object` - Lisp value for JSON `false`

**Error Handling:**

```elisp
(condition-case err
    (json-parse-string "{invalid json}")
  (json-parse-error
   (message "JSON parse error: %s" (error-message-string err))))
```

#### B. Traditional json.el (Emacs 23+)

**Broader compatibility** but slower than native functions.

```elisp
(require 'json)

;; Parse JSON from buffer
(with-temp-buffer
  (insert-file-contents filepath)
  (goto-char (point-min))
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'string))
    (json-read)))

;; Encode Lisp to JSON
(json-encode '((name . "test") (value . 42)))
;; => "{\"name\":\"test\",\"value\":42}"
```

**Configuration Variables:**
- `json-object-type` - 'alist, 'plist, or 'hash-table
- `json-array-type` - 'vector or 'list
- `json-key-type` - 'string, 'symbol, or 'keyword
- `json-false` - Value for JSON false
- `json-null` - Value for JSON null

**When to use json.el:**
- Supporting Emacs < 27
- Compatibility with older code
- When native JSON isn't available

**Performance:** Native functions are significantly faster. Prefer them when available.

### YAML Parsing

#### yaml.el (GNU ELPA)

**Pure Elisp YAML parser**, no external dependencies required.

```elisp
(require 'yaml)

;; Parse YAML from string
(yaml-parse-string "
name: mypackage
version: 1.0
dependencies:
  - foo
  - bar
")
;; => ((name . "mypackage")
;;     (version . 1.0)
;;     (dependencies "foo" "bar"))

;; Encode Lisp to YAML
(yaml-encode '((name . "mypackage")
               (version . 1.0)
               (dependencies . ("foo" "bar"))))
```

**Performance Notes:**
- Pure Elisp implementation (slower than native JSON)
- May hit `max-lisp-eval-depth` on deeply nested files
- For large YAML files, consider increasing:
  ```elisp
  (setq max-lisp-eval-depth 10000)
  ```

**Tree-sitter Alternative:**

For better performance with large YAML files:
```elisp
(require 'yaml-ts-mode)  ; Emacs 29+ with tree-sitter
```

#### libyaml-based Implementation

For higher performance, some packages provide libyaml bindings (requires compilation).

### REST API Calls

#### A. request.el (Recommended)

**Full-featured HTTP client** with clean callback architecture.

**Installation:**
```elisp
;; Available on MELPA
(package-install 'request)
```

**Basic Usage:**

```elisp
(require 'request)

(request "https://api.example.com/data"
  :type "GET"
  :parser 'json-read
  :success (cl-function
            (lambda (&key data &allow-other-keys)
              (message "Received: %s" data)))
  :error (cl-function
          (lambda (&key error-thrown &allow-other-keys)
            (message "Error: %s" error-thrown))))
```

**POST with JSON:**

```elisp
(request "https://api.example.com/create"
  :type "POST"
  :data (json-encode '((name . "test") (value . 42)))
  :headers '(("Content-Type" . "application/json"))
  :parser 'json-read
  :success (cl-function
            (lambda (&key data &allow-other-keys)
              (message "Created: %s" data))))
```

**Error Handling:**

```elisp
(request "https://api.example.com/data"
  :parser 'json-read
  :success (cl-function
            (lambda (&key data response &allow-other-keys)
              (message "Success! Status: %s" (request-response-status-code response))
              (process-data data)))
  :error (cl-function
          (lambda (&key error-thrown response &allow-other-keys)
            (cond
             ((eq (car error-thrown) 'timeout)
              (message "Request timed out"))
             ((eq (car error-thrown) 'error)
              (message "HTTP error: %s" (request-response-status-code response)))
             (t
              (message "Unknown error: %s" error-thrown)))))
  :complete (cl-function
             (lambda (&key &allow-other-keys)
               (message "Request complete"))))
```

**Status Code Handling:**

```elisp
(request "https://api.example.com/data"
  :parser 'json-read
  :status-code '((200 . (lambda (&rest _) (message "OK")))
                 (404 . (lambda (&rest _) (message "Not found")))
                 (500 . (lambda (&rest _) (message "Server error")))))
```

**Synchronous Requests:**

```elisp
(let ((response (request "https://api.example.com/data"
                  :sync t
                  :parser 'json-read)))
  (request-response-data response))
```

**Error Types:**
- `success` - Request succeeded (2xx status code)
- `error` - HTTP error (4xx, 5xx status code)
- `timeout` - Request timed out
- `abort` - Request aborted
- `parse-error` - Parser failed

#### B. Built-in url.el

**Basic HTTP** functionality, always available (no external dependencies).

**Asynchronous Retrieval:**

```elisp
(require 'url)

(url-retrieve "https://api.example.com/data"
              (lambda (status)
                (if (plist-get status :error)
                    (message "Error: %s" (plist-get status :error))
                  ;; Success: parse response
                  (goto-char (point-min))
                  (re-search-forward "\n\n")  ; Skip headers
                  (let ((json-object-type 'alist))
                    (message "Data: %s" (json-read))))))
```

**Synchronous Retrieval:**

```elisp
(defun mypackage-fetch-data (url)
  "Fetch JSON data from URL synchronously."
  (with-current-buffer (url-retrieve-synchronously url t nil 10)
    ;; Arguments: url silent inhibit-cookies timeout(seconds)
    (goto-char (point-min))
    (re-search-forward "\n\n")  ; Skip HTTP headers
    (json-read-from-string (buffer-substring-no-properties (point) (point-max)))))
```

**Known Issues:**
- DNS queries are **synchronous** in Emacs ≤24 (blocks!)
- Less informative error handling than request.el
- Response buffer requires manual parsing
- No built-in JSON integration

**When to use url.el:**
- Avoiding external dependencies
- Simple GET requests
- Don't need advanced error handling

#### C. restclient.el (Interactive Testing)

**Interactive HTTP client** for manual testing and development.

```elisp
# Create a .rest file:
GET https://api.example.com/users
Content-Type: application/json

POST https://api.example.com/create
Content-Type: application/json

{
  "name": "test",
  "value": 42
}

# C-c C-c to execute request
```

**Good for:**
- Development and debugging
- Manual API testing
- Documenting API calls

**Not for:**
- Programmatic use in packages
- Automated requests

### Data Validation

#### validate.el (GNU ELPA)

**Schema validation** for Elisp data structures.

```elisp
(require 'validate)

;; Validate data against schema
(validate-value
 '((name . "test") (age . 25))
 '(alist :key-type symbol :value-type (or string number)))
;; => t (validation passed)

(validate-value
 '((name . "test") (age . "invalid"))
 '(alist :value-type number))
;; => Signals user-error with detailed message
```

**Validating Custom Variables:**

```elisp
(defcustom mypackage-config
  '((server . "localhost")
    (port . 8080))
  "Configuration for mypackage."
  :type '(alist :key-type symbol :value-type (or string integer)))

;; Validate user's configuration
(validate-variable 'mypackage-config)
;; Signals error if value doesn't match :type
```

**API Response Validation:**

```elisp
(defun mypackage-fetch-user (user-id)
  "Fetch user data for USER-ID from API."
  (request "https://api.example.com/users"
    :params `((id . ,user-id))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                ;; Validate response structure
                (condition-case err
                    (progn
                      (validate-value data
                                     '(alist :key-type symbol
                                            :value-type (or string number)))
                      (mypackage--process-user data))
                  (user-error
                   (message "Invalid API response: %s" (error-message-string err))))))))
```

**Schema Types:**

Uses same syntax as `defcustom` `:type`:
- `string`, `number`, `boolean`, `symbol`
- `(or type1 type2)` - Union types
- `(list type)` - List of specific type
- `(alist :key-type ktype :value-type vtype)` - Association lists
- `(plist :key-type ktype :value-type vtype)` - Property lists

### Common Patterns

**Pattern 1: Parse External Data → Validate → Transform**

```elisp
(defun mypackage-load-config (filepath)
  "Load and validate configuration from FILEPATH."
  (let ((data (with-temp-buffer
                (insert-file-contents filepath)
                (json-parse-buffer :object-type 'alist))))
    ;; Validate structure
    (validate-value data
                   '(alist :key-type symbol
                          :value-type (or string number list)))
    ;; Transform to internal format
    (mypackage--transform-config data)))
```

**Pattern 2: Async API Call with Error Handling**

```elisp
(defun mypackage-fetch-data (endpoint callback)
  "Fetch data from ENDPOINT, call CALLBACK with result."
  (request (concat mypackage-api-base endpoint)
    :parser 'json-read
    :timeout 10
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (condition-case err
                    (progn
                      (validate-value data mypackage-data-schema)
                      (funcall callback data))
                  (error
                   (message "Data validation failed: %s" err)))))
    :error (cl-function
            (lambda (&key error-thrown response &allow-other-keys)
              (message "API error (%s): %s"
                      (request-response-status-code response)
                      error-thrown)))))
```

**Pattern 3: Sync vs Async Decision**

```elisp
;; GOOD: Async for interactive commands (doesn't block UI)
(defun mypackage-refresh ()
  "Refresh data from server."
  (interactive)
  (message "Refreshing...")
  (request "https://api.example.com/data"
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (mypackage--update-display data)
                (message "Refreshed!")))))

;; ACCEPTABLE: Sync for internal functions (if quick)
(defun mypackage--fetch-metadata ()
  "Fetch small metadata synchronously. Internal use only."
  (let ((response (request "https://api.example.com/meta"
                    :sync t
                    :timeout 2
                    :parser 'json-read)))
    (request-response-data response)))
```

### Anti-Patterns

**Using synchronous requests in interactive commands:**
```elisp
;; ANTI-PATTERN: Blocks Emacs UI
(defun mypackage-refresh ()
  "Refresh data."
  (interactive)
  (let ((data (url-retrieve-synchronously "https://slow-api.com")))
    ...))  ; User can't do anything while waiting!

;; CORRECT: Use async
(defun mypackage-refresh ()
  "Refresh data."
  (interactive)
  (request "https://slow-api.com"
    :success (cl-function (lambda (&key data &allow-other-keys) ...))))
```

**Not validating external data structure:**
```elisp
;; ANTI-PATTERN: Assumes API structure
(let ((name (alist-get 'name data)))  ; Crashes if 'name missing!
  ...)

;; CORRECT: Validate first
(validate-value data '(alist :key-type symbol))
(let ((name (alist-get 'name data)))
  ...)
```

**Ignoring HTTP error codes:**
```elisp
;; ANTI-PATTERN: Only checking :success
(request url
  :success (lambda (&rest _) (message "Success!")))

;; CORRECT: Handle errors
(request url
  :success (cl-function (lambda (&key data &allow-other-keys) ...))
  :error (cl-function (lambda (&key error-thrown &allow-other-keys)
                       (message "Error: %s" error-thrown))))
```

**Using json.el when native functions available:**
```elisp
;; ANTI-PATTERN: Slow when native available
(require 'json)
(json-read)

;; CORRECT: Use native (Emacs 27+)
(if (fboundp 'json-parse-buffer)
    (json-parse-buffer :object-type 'alist)
  ;; Fallback for older Emacs
  (require 'json)
  (let ((json-object-type 'alist))
    (json-read)))
```

---

## Package Architecture

### File Structure Conventions

**Single-file package:**
```
mypackage.el
```

**Multi-file package:**
```
mypackage/
├── mypackage.el          ; Main entry point
├── mypackage-core.el     ; Core functionality
├── mypackage-utils.el    ; Utilities
└── mypackage-pkg.el      ; Package metadata (auto-generated)
```

### Package Headers

Every package file must include proper headers:

```elisp
;;; mypackage.el --- Short description  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Your Name

;; Author: Your Name <your.email@example.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (request "0.3.0"))
;; Keywords: tools convenience
;; URL: https://github.com/yourusername/mypackage

;;; Commentary:

;; Longer description of what the package does.
;; Can span multiple lines.
;;
;; Usage:
;;   (require 'mypackage)
;;   (mypackage-mode 1)

;;; Code:

;; Your code here

(provide 'mypackage)
;;; mypackage.el ends here
```

### provide and require System

**Every file must `provide` its feature:**

```elisp
;; At end of mypackage.el
(provide 'mypackage)
```

**Require dependencies at top of file:**

```elisp
;;; mypackage.el --- Description  -*- lexical-binding: t; -*-

(require 'json)
(require 'request)
(require 'mypackage-utils)

;;; Code:
...
```

### Autoloads

**Mark interactive commands and important entry points** with `;;;###autoload`:

```elisp
;;;###autoload
(defun mypackage-mode ()
  "Toggle mypackage mode."
  (interactive)
  ...)

;;;###autoload
(define-derived-mode mypackage-major-mode prog-mode "MyPkg"
  "Major mode for MyPackage files."
  ...)
```

**Effect:** When package is installed, these functions are available without requiring the entire package. The package loads automatically when the function is called.

### Multi-File Package Organization

**mypackage.el** (main entry point):
```elisp
;;; mypackage.el --- Main package file  -*- lexical-binding: t; -*-

(require 'mypackage-core)
(require 'mypackage-ui)

;;;###autoload
(defun mypackage-start ()
  "Start mypackage."
  (interactive)
  (mypackage-core-init)
  (mypackage-ui-show))

(provide 'mypackage)
```

**mypackage-core.el** (core logic):
```elisp
;;; mypackage-core.el --- Core functionality  -*- lexical-binding: t; -*-

(defun mypackage-core-init ()
  "Initialize core functionality."
  ...)

(provide 'mypackage-core)
```

**mypackage-ui.el** (UI):
```elisp
;;; mypackage-ui.el --- User interface  -*- lexical-binding: t; -*-

(require 'mypackage-core)

(defun mypackage-ui-show ()
  "Show UI."
  ...)

(provide 'mypackage-ui)
```

### Package Metadata

**Package-Requires header** specifies dependencies:

```elisp
;; Package-Requires: ((emacs "27.1") (request "0.3.0") (dash "2.19.0"))
```

**Format:** `((package "min-version") ...)`

**Emacs version:** Always specify minimum Emacs version.

---

## Common Anti-Patterns Summary

### Namespacing
- Missing package prefixes → Global namespace pollution
- Generic function names without prefixes
- Not using `--` for private functions

### Scoping
- Not using `;;; -*- lexical-binding: t; -*-` in new code
- Performance penalty (30% slower access, 650% slower assignment)
- Harder to reason about variable scope

### Variables
- Using `make-variable-buffer-local` for user options
- Making variables buffer-local inside `let`-bindings
- Using `make-local-variable` on hooks (hooks auto-localize via `add-hook`)
- Creating buffer-local variables without prior `defvar`
- Using `setq` in init file for auto buffer-local variables (use `setq-default`)
- Not declaring variables before use (compiler warnings)

### Advice
- Overusing advice when hooks exist
- Using old `defadvice` instead of `advice-add`
- Advising functions instead of using proper extension APIs

### Naming
- Face names ending in `-face` (deprecated convention)
- Inconsistent predicate suffixes (`-p` vs `p`)
- Generic command names

### Documentation
- Undocumented functions (always document public functions!)
- Missing docstrings for variables
- Not documenting arguments in docstrings

### Keybindings
- Violating key binding conventions
- Major modes using `C-c <letter>` (reserved for users)
- Minor modes conflicting with major mode bindings

### External Data
- Using synchronous HTTP requests in interactive commands (blocks UI)
- Not validating external data structure
- Ignoring HTTP error codes
- Using `json.el` when native functions are available (performance)
- Not handling JSON parse errors

### Abstraction
- Over-complicated abstractions for simple tasks
- Premature optimization
- Creating abstractions for one-time use

---

## Architectural Patterns

### Extension Through Hooks (Observer Pattern)

```elisp
;; Define extension points
(defvar mypackage-before-process-hook nil
  "Hook run before processing data.")

(defvar mypackage-after-process-hook nil
  "Hook run after processing data.")

(defun mypackage-process (data)
  "Process DATA with hooks for extension."
  (run-hooks 'mypackage-before-process-hook)
  (let ((result (mypackage--do-process data)))
    (run-hooks 'mypackage-after-process-hook)
    result))

;; Users extend functionality
(add-hook 'mypackage-before-process-hook #'my-preprocessing)
```

### Advice for Aspect-Oriented Programming

```elisp
;; Add cross-cutting concerns (logging, timing, caching)
(defun mypackage-log-advice (orig-fun &rest args)
  "Log calls to advised function."
  (message "Calling with args: %s" args)
  (let ((result (apply orig-fun args)))
    (message "Returned: %s" result)
    result))

(advice-add 'mypackage-critical-function :around #'mypackage-log-advice)
```

### Major Modes as Configuration Bundles

```elisp
;; Bundle related settings for a file type
(define-derived-mode mypackage-mode prog-mode "MyPkg"
  "Major mode for MyPackage files."
  ;; Syntax
  (setq-local comment-start "# ")
  (setq-local comment-end "")

  ;; Indentation
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2)

  ;; Font lock
  (setq-local font-lock-defaults '(mypackage-font-lock-keywords))

  ;; Hooks
  (add-hook 'before-save-hook #'mypackage-cleanup-buffer nil t))
```

### Minor Modes as Composable Features

```elisp
;; Add optional, composable features
(define-minor-mode mypackage-highlight-mode
  "Highlight special syntax."
  :lighter " HL"
  (if mypackage-highlight-mode
      (font-lock-add-keywords nil mypackage-highlight-keywords)
    (font-lock-remove-keywords nil mypackage-highlight-keywords))
  (font-lock-flush))

;; Users compose features
(add-hook 'prog-mode-hook #'mypackage-highlight-mode)
```

### Keymap Inheritance

```elisp
;; Base keymap
(defvar mypackage-base-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'mypackage-compile)
    map))

;; Derived mode inherits and extends
(defvar mypackage-special-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map mypackage-base-mode-map)
    (define-key map (kbd "C-c C-s") #'mypackage-special-command)
    map))
```

### Buffer-Local State Management

```elisp
;; Package maintains per-buffer state
(defvar-local mypackage-parse-tree nil
  "Parse tree for current buffer.")

(defvar-local mypackage-last-modified nil
  "Last modification time.")

(defun mypackage-update-state ()
  "Update buffer-local state."
  (setq mypackage-parse-tree (mypackage--parse-buffer))
  (setq mypackage-last-modified (current-time)))

;; Auto-update on changes
(define-derived-mode mypackage-mode prog-mode "MyPkg"
  "Major mode with buffer-local state."
  (add-hook 'after-change-functions #'mypackage--mark-dirty nil t))
```

---

## References

This reference is based on official Emacs documentation and community best practices:

- [GNU Emacs Lisp Reference Manual](https://www.gnu.org/software/emacs/manual/html_node/elisp/)
- [Emacs Lisp Coding Conventions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Coding-Conventions.html)
- [The Emacs Package Developer's Handbook](https://alphapapa.github.io/emacs-package-dev-handbook/)
- [Emacs Lisp Style Guide](https://github.com/bbatsov/emacs-lisp-style-guide)
- [Buffer-Local Variables Manual](https://www.gnu.org/software/emacs/manual/html_node/elisp/Buffer_002dLocal-Variables.html)
- [Advising Functions Manual](https://www.gnu.org/software/emacs/manual/html_node/elisp/Advising-Functions.html)
- [Request.el Documentation](https://tkf.github.io/emacs-request/)

---

*Last updated: 2026-01-20*
