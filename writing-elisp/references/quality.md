# Code Quality and Error Handling

## Linting Tools

### checkdoc - Docstring Validation

Validates that docstrings follow Emacs conventions:

```bash
# Check single file
/Applications/Emacs.app/Contents/MacOS/Emacs --batch \
  --eval "(checkdoc-file \"my-package.el\")"

# Check and show warnings
/Applications/Emacs.app/Contents/MacOS/Emacs --batch \
  --eval "(progn (require 'checkdoc) (checkdoc-file \"my-package.el\"))"
```

**What checkdoc catches:**
- First line not ending with period
- Parameter names not in UPPERCASE
- Missing docstrings on public functions
- Docstrings with incorrect formatting
- Missing function argument documentation

### package-lint - Package Convention Validation

Checks package structure and metadata:

```bash
# Requires package-lint to be installed
/Applications/Emacs.app/Contents/MacOS/Emacs --batch \
  -l package-lint \
  -f package-lint-batch-and-exit my-package.el
```

**What package-lint catches:**
- Missing or malformed headers (Author, Version, etc.)
- Incorrect Package-Requires format
- Function names not matching package prefix
- Missing lexical-binding header
- Invalid version numbers
- Incorrect file naming

### elisp-lint - Comprehensive Linting

Runs multiple checks in one command:

```bash
# Install elisp-lint first
# Then run comprehensive checks
elisp-lint my-package.el
```

**What elisp-lint catches:**
- All checkdoc issues
- All package-lint issues
- Byte compilation warnings
- Indentation problems
- Trailing whitespace

### Byte Compilation Warnings

```bash
# Compile and show all warnings
/Applications/Emacs.app/Contents/MacOS/Emacs --batch \
  --eval "(setq byte-compile-error-on-warn t)" \
  -f batch-byte-compile my-package.el
```

**Common warnings to fix:**
- Unused lexical variables
- Functions used but not defined (add declare-function)
- Obsolete function usage
- Free variables (typos or missing let bindings)

## Error Handling

### When to Use What

| Situation | Use | Example |
|-----------|-----|---------|
| Expected recoverable error | `condition-case` | File not found, parse error |
| Want to ignore all errors | `ignore-errors` | Optional enhancement that might fail |
| Want to demote errors to warnings | `with-demoted-errors` | Non-critical operation |
| Need cleanup after error | `unwind-protect` | Close file, release resource |
| Signal custom error | `signal` with `define-error` | Invalid state |
| User-facing error | `user-error` | Invalid input from interactive command |

### condition-case - Robust Error Handling

```elisp
;; Basic usage
(condition-case err
    (progn
      (delete-file "important.txt")
      (message "Deleted successfully"))
  (file-error
   (message "Failed to delete: %s" (error-message-string err))))

;; Catching multiple error types
(condition-case err
    (my-risky-operation)
  (file-error
   (message "File problem: %s" err))
  (json-parse-error
   (message "JSON problem: %s" err))
  (error
   (message "Unknown error: %s" err)))

;; With success/failure paths
(let ((success nil))
  (condition-case err
      (progn
        (do-risky-thing)
        (setq success t))
    (error
     (message "Failed: %s" (error-message-string err))))
  (when success
    (do-followup)))
```

### ignore-errors - Suppress All Errors

```elisp
;; Returns nil if error occurs
(ignore-errors
  (delete-file "maybe-exists.txt"))

;; Use for optional enhancements
(defun my-function ()
  "Do main work, optionally enhance."
  (do-main-work)
  ;; This might fail, but we don't care
  (ignore-errors
    (add-optional-feature)))
```

**Warning:** Don't overuse `ignore-errors`. It's better to explicitly handle expected errors with `condition-case`.

### with-demoted-errors - Errors Become Warnings

```elisp
;; Errors are logged but don't interrupt
(with-demoted-errors "Warning: %S"
  (load-optional-config))

;; Good for init files where one failure shouldn't break everything
(defun my-load-extensions ()
  "Load extensions, logging failures."
  (dolist (ext my-extensions)
    (with-demoted-errors "Failed to load %s: %%S" ext
      (require ext))))
```

### unwind-protect - Guaranteed Cleanup

```elisp
;; Cleanup always runs, even on error
(let ((buf (generate-new-buffer "*temp*")))
  (unwind-protect
      (progn
        (with-current-buffer buf
          (insert-file-contents "data.txt")
          (process-buffer)))
    ;; This ALWAYS runs, even if error occurs above
    (kill-buffer buf)))

;; Save and restore state
(defun my-with-temp-setting (value body-fn)
  "Run BODY-FN with temporary VALUE, restore after."
  (let ((old-value my-setting))
    (unwind-protect
        (progn
          (setq my-setting value)
          (funcall body-fn))
      (setq my-setting old-value))))
```

### Custom Error Types

```elisp
;; Define error hierarchy
(define-error 'my-package-error "My Package Error")
(define-error 'my-package-not-found "Not Found" 'my-package-error)
(define-error 'my-package-invalid "Invalid" 'my-package-error)

;; Signal errors
(defun my-get-item (id)
  "Get item by ID."
  (let ((item (find-item id)))
    (unless item
      (signal 'my-package-not-found (list id)))
    (unless (valid-p item)
      (signal 'my-package-invalid (list item)))
    item))

;; Catch specific error type
(condition-case err
    (my-get-item 123)
  (my-package-not-found
   (message "Item not found: %s" (cdr err)))
  (my-package-error
   (message "Package error: %s" (error-message-string err))))
```

### user-error - Interactive Command Errors

```elisp
;; For user-facing errors (not bugs)
(defun my-delete-item (id)
  "Delete item ID."
  (interactive "sItem ID: ")
  (when (string-empty-p id)
    (user-error "Item ID cannot be empty"))
  (unless (my-item-exists-p id)
    (user-error "Item %s does not exist" id))
  (my-delete-item-internal id))

;; user-error shows in minibuffer, doesn't show backtrace
;; Regular `error` shows backtrace (indicates bug, not user mistake)
```

## Common Anti-Patterns

### Code Smell Detector

| Smell | Problem | Fix |
|-------|---------|-----|
| No `lexical-binding: t` | Dynamic scope bugs | Add header |
| `(require 'cl)` | Deprecated package | Use `cl-lib` |
| Lambda in `add-hook` | Can't remove hook | Use named function |
| Repeated expensive call | Performance | Cache in let-binding |
| Missing docstring | Unclear API | Add docstring |
| `(setq x (append x (list item)))` | O(n²) accumulation | Use push/nreverse |
| `(progn ...)` in when/unless | Unnecessary | Remove progn |
| Nested `if` with 3+ branches | Hard to read | Use `cond` or `pcase` |
| `(if x y nil)` | Verbose | Use `when` |
| `(when (not x) y)` | Verbose | Use `unless` |

### Missing lexical-binding

```elisp
;; WRONG - no header
(defun make-counter ()
  (let ((count 0))
    (lambda () (setq count (1+ count)))))

;; CORRECT - with lexical-binding: t header
;;; -*- lexical-binding: t; -*-
(defun make-counter ()
  (let ((count 0))
    (lambda () (cl-incf count))))
```

### Deprecated cl Package

```elisp
;; WRONG
(require 'cl)
(defun* my-func (x &key (default 0))
  (loop for i from 1 to x collect i))

;; CORRECT
(require 'cl-lib)
(cl-defun my-func (x &key (default 0))
  (cl-loop for i from 1 to x collect i))
```

### Lambda in Hooks

```elisp
;; WRONG - can't remove this hook later
(add-hook 'before-save-hook
          (lambda () (delete-trailing-whitespace)))

;; CORRECT - named function
(defun my-delete-trailing-whitespace ()
  "Remove trailing whitespace."
  (delete-trailing-whitespace))

(add-hook 'before-save-hook #'my-delete-trailing-whitespace)

;; Now can remove: (remove-hook 'before-save-hook #'my-delete-trailing-whitespace)
```

### Repeated Expensive Calls

```elisp
;; WRONG - calls length twice
(defun process-items (items)
  (when (> (length items) 0)
    (message "Processing %d items" (length items))
    (process-each items)))

;; CORRECT - cache length
(defun process-items (items)
  (let ((count (length items)))
    (when (> count 0)
      (message "Processing %d items" count)
      (process-each items))))
```

### Inefficient List Building

```elisp
;; WRONG - O(n²) complexity
(let ((results nil))
  (dolist (item items)
    (setq results (append results (list item))))
  results)

;; CORRECT - O(n) with push/nreverse
(let ((results nil))
  (dolist (item items)
    (push item results))
  (nreverse results))

;; BEST - use mapcar if just transforming
(mapcar #'transform items)
```

### Global State Modification

```elisp
;; PROBLEMATIC - modifies global state
(defvar my-package-state nil)

(defun my-operation ()
  (setq my-package-state 'processing)
  (do-work)
  (setq my-package-state 'done))

;; BETTER - return state, let caller manage
(defun my-operation ()
  (let ((state 'processing))
    (do-work)
    'done))

;; OR use buffer-local variables
(defvar-local my-package-buffer-state nil
  "State for this buffer only.")
```

### Using eval (Almost Always Wrong)

```elisp
;; WRONG - eval is dangerous and usually unnecessary
(defun my-set-var (name value)
  (eval `(setq ,name ,value)))

;; CORRECT - use proper data structures
(defvar my-vars (make-hash-table))

(defun my-set-var (name value)
  (puthash name value my-vars))

;; OR if you really need symbols:
(defun my-set-var (name value)
  (set name value))  ; Direct symbol setting, no eval needed
```

## Integration with Validation Workflow

Add linting to your incremental validation:

```bash
# After writing function
./bin/tangle-org.sh file.org

# 1. Quick paren check
/Applications/Emacs.app/Contents/MacOS/Emacs --batch \
  --eval "(progn (find-file \"file.el\") (check-parens))"

# 2. Byte compile check
/Applications/Emacs.app/Contents/MacOS/Emacs --batch \
  -f batch-byte-compile file.el

# 3. Check docstrings (before committing)
/Applications/Emacs.app/Contents/MacOS/Emacs --batch \
  --eval "(checkdoc-file \"file.el\")"

# 4. Check package structure (for packages)
/Applications/Emacs.app/Contents/MacOS/Emacs --batch \
  -l package-lint \
  -f package-lint-batch-and-exit file.el
```

## Quick Quality Checklist

- ✅ `lexical-binding: t` header present
- ✅ Using `cl-lib`, not deprecated `cl`
- ✅ No lambdas in hooks
- ✅ Expensive calls cached in let-bindings
- ✅ Docstrings on all public functions
- ✅ Using push/nreverse, not repeated append
- ✅ Using appropriate error handling (not ignoring everything)
- ✅ Custom error types defined for domain errors
- ✅ No `eval` usage
- ✅ Byte compilation produces no warnings
- ✅ checkdoc passes
- ✅ package-lint passes (for packages)
