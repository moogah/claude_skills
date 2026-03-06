# Elisp Idioms and Conventions

## Naming Conventions

### Predicates End in `-p`

Functions that return boolean values (t/nil) should end with `-p`:

```elisp
;; WRONG
(defun is-valid (x)
  (and (stringp x) (> (length x) 0)))

;; CORRECT
(defun valid-p (x)
  (and (stringp x) (> (length x) 0)))

;; Standard library examples
(stringp x)      ; not (is-string x)
(numberp x)      ; not (is-number x)
(buffer-live-p x)
(file-exists-p path)
```

### Internal Functions Use `--`

Private/internal functions should use double-dash:

```elisp
;; Public API
(defun my-package-process-file (file)
  "Process FILE according to rules."
  (let ((data (my-package--load-file file)))
    (my-package--validate data)
    (my-package--transform data)))

;; Internal helpers (not part of public API)
(defun my-package--load-file (file)
  "Internal: Load FILE contents."
  ...)

(defun my-package--validate (data)
  "Internal: Validate DATA structure."
  ...)
```

### Package Namespacing

All functions in a package should share a common prefix:

```elisp
;; Package: my-todos
(defun my-todos-add (item)
  "Add ITEM to todos.")

(defun my-todos-remove (id)
  "Remove todo with ID.")

(defun my-todos-list ()
  "List all todos.")

;; Variables
(defvar my-todos-file "~/.todos")
(defvar my-todos-max-items 100)

;; Internal
(defun my-todos--format-item (item)
  "Internal formatter.")
```

### Buffer-Local Variables

Suggest the scope with naming:

```elisp
;; Buffer-local state
(defvar-local my-package-buffer-state nil
  "Current state for this buffer.")

(defvar-local my-package-local-cache nil
  "Buffer-local cache.")

;; Global state
(defvar my-package-global-config nil
  "Configuration applied to all buffers.")
```

### Constants

Use uppercase with hyphens:

```elisp
(defconst my-package-version "1.0.0"
  "Current package version.")

(defconst my-package-max-retries 3
  "Maximum number of retry attempts.")

(defconst my-package-default-timeout 30
  "Default timeout in seconds.")
```

### Destructive Operations (Convention)

While not enforced by Elisp, some packages use `!` suffix to indicate mutation:

```elisp
;; Non-destructive (returns new list)
(defun my-todos-add (todos item)
  "Return new list with ITEM added."
  (append todos (list item)))

;; Destructive (modifies in place) - convention only
(defun my-todos-add! (todos item)
  "Add ITEM to TODOS in place."
  (nconc todos (list item)))
```

**Note**: This is a community convention, not standard like `-p` for predicates.

## Idiomatic Control Flow

### `when` vs `if`

**Use `when` for single branch:**

```elisp
;; WRONG - if with implicit nil
(if (file-exists-p file)
    (delete-file file))

;; CORRECT
(when (file-exists-p file)
  (delete-file file))
```

**Use `if` for two branches:**

```elisp
;; CORRECT
(if (file-exists-p file)
    (delete-file file)
  (message "File doesn't exist"))
```

### `unless` for Negative Conditions

```elisp
;; Less clear
(when (not (buffer-modified-p))
  (kill-buffer))

;; More clear
(unless (buffer-modified-p)
  (kill-buffer))
```

### `cond` for Multiple Conditions

```elisp
;; WRONG - nested ifs
(if (< x 0)
    "negative"
  (if (= x 0)
      "zero"
    (if (< x 10)
        "small positive"
      "large positive")))

;; CORRECT - cond
(cond
 ((< x 0) "negative")
 ((= x 0) "zero")
 ((< x 10) "small positive")
 (t "large positive"))
```

### `and`/`or` for Short-Circuit Logic

```elisp
;; Guard pattern - return nil if condition fails
(defun process-file (file)
  (and (file-exists-p file)
       (file-readable-p file)
       (with-temp-buffer
         (insert-file-contents file)
         (buffer-string))))

;; Default value pattern
(defun get-config (key)
  (or (gethash key my-config-table)
      (getenv (upcase (symbol-name key)))
      "default-value"))

;; Avoiding explicit if
(and condition
     (do-something))  ; Only runs if condition is true

;; Same as:
(when condition
  (do-something))
```

### Avoid Unnecessary `progn`

```elisp
;; WRONG - progn not needed in many contexts
(when condition
  (progn
    (do-first)
    (do-second)))

;; CORRECT - when allows multiple forms
(when condition
  (do-first)
  (do-second))
```

**`progn` IS needed in:**
- `if` branches (each branch is single expression)
- Lambda bodies (single expression)
- Actually, both are wrong - `if` and `lambda` accept multiple forms too!

**`progn` really only needed for:**
- Macro expansion contexts where multiple forms need to be single expression
- Very rare advanced scenarios

```elisp
;; These all accept multiple forms without progn:
(when condition
  (form1)
  (form2))

(lambda (x)
  (form1)
  (form2))

(if condition
    (progn (form1) (form2))  ; Actually CAN do multiple forms!
  (progn (form3) (form4)))

;; Actually correct for if:
(if condition
    (form1)
  (form3))
;; Only single expression per branch naturally. Use cond for multiple.
```

### Early Return with `cl-return`

```elisp
(require 'cl-lib)

(defun find-valid-item (items)
  "Return first valid item or nil."
  (cl-block nil
    (dolist (item items)
      (when (valid-p item)
        (cl-return item)))
    nil))  ; Implicit return if nothing found
```

## Idiomatic Looping

### `dolist` for Simple Iteration

```elisp
;; Iterating for side effects
(dolist (file files)
  (process-file file))

;; Building a list
(let (results)
  (dolist (item items)
    (when (valid-p item)
      (push (transform item) results)))
  (nreverse results))  ; Remember to reverse!
```

### `dotimes` for Counted Loops

```elisp
;; Simple counting
(dotimes (i 10)
  (message "Count: %d" i))

;; With result
(dotimes (i 5 i)  ; Returns final value of i
  (insert "x"))
```

### `cl-loop` for Complex Iteration

```elisp
;; Collecting results
(cl-loop for x in items
         when (> x 0)
         collect (* x x))

;; Multiple accumulators
(cl-loop for x in numbers
         sum x into total
         count (> x 0) into positive
         finally return (list total positive))

;; Parallel iteration
(cl-loop for x in list1
         for y in list2
         collect (+ x y))
```

### `mapcar` vs `mapc` vs `cl-mapcar`

```elisp
;; mapcar - transform list, return results
(mapcar #'upcase '("a" "b" "c"))
;; => ("A" "B" "C")

;; mapc - iterate for side effects, return original list
(mapc (lambda (x) (message "Processing: %s" x))
      '("a" "b" "c"))
;; => ("a" "b" "c") but messages are shown

;; cl-mapcar - multiple lists
(cl-mapcar #'+ '(1 2 3) '(4 5 6))
;; => (5 7 9)
```

### When to Use Which Loop

| Use Case | Best Choice | Why |
|----------|-------------|-----|
| Simple iteration | `dolist` | Clearest intent |
| Counted loop | `dotimes` | Built for counting |
| Building list | `mapcar` or `cl-loop` | Functional style |
| Side effects | `mapc` or `dolist` | Shows intent |
| Complex logic | `cl-loop` | Most powerful |
| Filter & map | `cl-loop` or `seq-` functions | Efficient |

## Avoiding Common Pitfalls

### Push and Reverse Pattern

```elisp
;; WRONG - repeatedly appending to end (O(n²))
(let (results)
  (dolist (item items)
    (setq results (append results (list item))))
  results)

;; CORRECT - push to front, reverse once (O(n))
(let (results)
  (dolist (item items)
    (push item results))
  (nreverse results))
```

### String Building

```elisp
;; WRONG - repeated concatenation
(let ((result ""))
  (dolist (word words)
    (setq result (concat result word " ")))
  result)

;; CORRECT - collect then join
(mapconcat #'identity words " ")

;; OR use format for small known strings
(format "%s %s %s" first second third)
```

### Testing for Empty List

```elisp
;; All equivalent and idiomatic
(when items
  (process items))

(unless (null items)
  (process items))

;; Less common but valid
(when (consp items)  ; Tests for cons cell (non-empty list)
  (process items))
```

### Testing for Empty String

```elisp
;; WRONG
(when (> (length str) 0)
  (process str))

;; CORRECT
(unless (string-empty-p str)
  (process str))

;; OR
(when (> (length str) 0)  ; This is fine too if you need length anyway
  (process str))
```

## Function Definition Idioms

### Optional Arguments

```elisp
;; Using &optional
(defun my-function (required &optional opt1 opt2)
  "REQUIRED is mandatory, OPT1 and OPT2 are optional."
  (or opt1 "default1")
  (or opt2 "default2"))

;; Using cl-defun with keyword arguments (better for many optionals)
(cl-defun my-function (required &key (opt1 "default1") (opt2 "default2"))
  "REQUIRED is mandatory, :OPT1 and :OPT2 are keyword arguments."
  ...)

;; Call with keywords
(my-function "value" :opt2 "custom")
```

### Rest Arguments

```elisp
;; Collect remaining arguments
(defun my-sum (first &rest numbers)
  "Add FIRST and all NUMBERS."
  (apply #'+ first numbers))

(my-sum 1 2 3 4 5)  ; => 15
```

## Summary: Quick Idiom Checklist

- ✅ Predicates end in `-p`
- ✅ Internal functions use `--`
- ✅ Package prefix on all public symbols
- ✅ Use `when` for single branch, `if` for two
- ✅ Use `unless` for negative conditions
- ✅ Use `cond` for multiple conditions
- ✅ Use `and`/`or` for short-circuit logic
- ✅ Choose `dolist`/`dotimes`/`cl-loop` appropriately
- ✅ Push and reverse, don't repeatedly append
- ✅ Use `mapconcat` for string joining
