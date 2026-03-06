# Modern Elisp Features

## Lexical Binding (Mandatory)

**Always** include lexical binding header as the first line of every .el file:

```elisp
;;; -*- lexical-binding: t; -*-
```

### Why Lexical Binding Matters

- **Performance**: Significantly faster than dynamic binding
- **Correctness**: Prevents accidental variable capture in closures
- **Modern standard**: All new Emacs code uses lexical binding
- **Required for packages**: Package repositories expect lexical binding

### Example: Closure Bug Without Lexical Binding

```elisp
;; Without lexical-binding (BROKEN)
(defun make-adder (n)
  (lambda (x) (+ x n)))  ; n captured incorrectly in dynamic scope

(let ((n 5))
  (funcall (make-adder 10) 3))  ; Returns 8, not 13!

;; With lexical-binding: t (CORRECT)
(defun make-adder (n)
  (lambda (x) (+ x n)))  ; n captured correctly in lexical scope

(let ((n 5))
  (funcall (make-adder 10) 3))  ; Returns 13 as expected
```

## cl-lib: Modern Common Lisp Compatibility

### Use cl-lib, Never Old cl Package

```elisp
;; WRONG - Deprecated, pollutes namespace
(require 'cl)
(defun* my-function (x &key (default 0))
  (loop for i from 1 to x collect i))

;; CORRECT - Modern, namespaced
(require 'cl-lib)
(cl-defun my-function (x &key (default 0))
  (cl-loop for i from 1 to x collect i))
```

### Key cl-lib Functions to Use

| Old (avoid) | New (use) | Purpose |
|------------|-----------|---------|
| `defun*` | `cl-defun` | Functions with keyword args |
| `loop` | `cl-loop` | Iteration |
| `destructuring-bind` | `cl-destructuring-bind` | Pattern matching |
| `case` | `cl-case` | Switch statement |
| `return` | `cl-return` | Early return |
| `block` | `cl-block` | Named blocks |

### cl-loop Examples

```elisp
;; Collect items
(cl-loop for x in '(1 2 3 4 5)
         when (> x 2)
         collect (* x x))
;; => (9 16 25)

;; Sum with accumulator
(cl-loop for i from 1 to 10
         sum i)
;; => 55

;; Multiple values
(cl-loop for (key . value) in '((a . 1) (b . 2) (c . 3))
         when (> value 1)
         collect key)
;; => (b c)
```

## Pattern Matching with pcase

Modern alternative to nested if/cond statements:

```elisp
;; Old style - nested conditionals
(defun process-value (x)
  (cond
   ((stringp x)
    (if (string-empty-p x)
        (error "Empty string")
      (upcase x)))
   ((numberp x)
    (if (> x 0)
        (* x 2)
      0))
   (t (error "Invalid type"))))

;; Modern style - pcase
(defun process-value (x)
  (pcase x
    ((pred stringp)
     (if (string-empty-p x)
         (error "Empty string")
       (upcase x)))
    ((and (pred numberp) (guard (> x 0)))
     (* x 2))
    ((pred numberp) 0)
    (_ (error "Invalid type"))))
```

### pcase Pattern Types

```elisp
;; Exact match
(pcase x
  ('foo "matched foo")
  ("bar" "matched bar")
  (42 "matched 42"))

;; Type predicates
(pcase x
  ((pred stringp) "a string")
  ((pred numberp) "a number")
  ((pred listp) "a list"))

;; Guards (additional conditions)
(pcase x
  ((and (pred numberp) (guard (> x 0))) "positive")
  ((and (pred numberp) (guard (< x 0))) "negative"))

;; Destructuring
(pcase my-list
  (`(,first ,second . ,rest)
   (format "First: %s, Second: %s" first second)))

;; Combining patterns
(pcase value
  ((or 'yes 'true 't) "truthy")
  ((or 'no 'false 'nil) "falsy"))
```

## Named Let (Recursive Helper Pattern)

Modern alternative to separate helper functions:

```elisp
;; Old style - separate helper
(defun factorial-helper (n acc)
  (if (<= n 1)
      acc
    (factorial-helper (1- n) (* acc n))))

(defun factorial (n)
  (factorial-helper n 1))

;; Modern style - named let
(defun factorial (n)
  (named-let recur ((n n) (acc 1))
    (if (<= n 1)
        acc
      (recur (1- n) (* acc n)))))
```

Benefits:
- No namespace pollution with helper functions
- Clearer that recursion is internal implementation
- More concise

## Threading Macros (dash.el)

For clearer data transformation pipelines:

```elisp
(require 'dash)

;; Without threading - hard to read
(defun process-items (items)
  (mapcar (lambda (x) (format "%s" x))
          (seq-filter (lambda (x) (> x 0))
                      (mapcar (lambda (x) (* x 2))
                              items))))

;; With thread-last (->>)
(defun process-items (items)
  (->> items
       (-map (lambda (x) (* x 2)))
       (-filter (lambda (x) (> x 0)))
       (-map (lambda (x) (format "%s" x)))))

;; With thread-first (->)
(defun get-user-email (user-id)
  (-> user-id
      (get-user)
      (plist-get :profile)
      (plist-get :email)
      (downcase)))
```

### When to Use Which

- **`->>` (thread-last)**: Collection operations (map, filter, reduce)
- **`->` (thread-first)**: Object/data navigation (accessing nested properties)

## Modern Hook and Advice Patterns

### Hooks - Always Use Named Functions

```elisp
;; WRONG - Anonymous function, can't remove
(add-hook 'before-save-hook
          (lambda () (delete-trailing-whitespace)))

;; CORRECT - Named function, can remove/debug
(defun my-delete-trailing-whitespace ()
  "Remove trailing whitespace before saving."
  (delete-trailing-whitespace))

(add-hook 'before-save-hook #'my-delete-trailing-whitespace)
```

### Local vs Global Hooks

```elisp
;; Global hook - affects all buffers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Buffer-local hook - only this buffer
(add-hook 'after-save-hook #'my-function nil t)
;;                                        ^^  ^
;;                                        |   local flag
;;                                        append flag
```

### Advice - Use Sparingly

```elisp
;; Add advice to existing function
(defun my-save-advice (orig-fun &rest args)
  "Do something before/after save."
  (message "Saving...")
  (apply orig-fun args)
  (message "Saved!"))

(advice-add 'save-buffer :around #'my-save-advice)

;; Remove when done
(advice-remove 'save-buffer #'my-save-advice)
```

**When to use advice:**
- Modifying third-party packages you don't control
- Adding debug tracing temporarily
- Cross-cutting concerns (logging, timing)

**When NOT to use advice:**
- You control the code (just modify it directly)
- Simple extensions (use hooks instead)
- Multiple pieces of advice on same function (gets confusing)

## Summary Checklist

When writing modern Elisp:

- ✅ Always include `;;; -*- lexical-binding: t; -*-`
- ✅ Use `cl-lib` functions, never old `cl` package
- ✅ Consider `pcase` for complex conditionals
- ✅ Use `named-let` for recursive helpers
- ✅ Use threading macros (`dash.el`) for pipelines
- ✅ Always use named functions in hooks
- ✅ Use advice sparingly and document why
