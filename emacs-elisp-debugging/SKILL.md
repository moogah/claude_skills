---
name: emacs-elisp-debugging
description: Strategies for debugging Emacs Lisp code, especially missing parentheses and Common Lisp compatibility issues. Use when encountering mysterious elisp errors or when working with literate elisp in org-mode files.
version: 1.0.0
author: Jeff Farr
created: 2026-01-10
---

# Emacs Elisp Debugging

## Overview

Systematic debugging strategies for Emacs Lisp development, focusing on common error patterns like missing parentheses and Common Lisp vs Emacs Lisp compatibility issues.

## Key Patterns

### Recognizing Error Symptoms

**Symbol appearing as value = missing closing parenthesis**

```
ERROR: Wrong type argument: sequencep, some-function-name
DEBUG output: candidates=some-function-name
DEBUG output: candidates type=symbol
```

This pattern means a function definition is missing a closing `)`, causing Emacs to read past the function boundary and interpret the next `defun` name as the return value.

### Debugging Strategy for Paren Errors

When you suspect missing parentheses but can't spot them:

1. **Isolate with org-babel** (when using literate programming):
   - Create a test block with `:tangle no`
   - Copy the suspicious function
   - Add inline comments marking nesting levels
   - Evaluate with `C-c C-c` to test in isolation

2. **Manual paren counting**:
   ```elisp
   (defun example ()
     (let (var1)                    ; open 1
       (dolist (item items)         ; open 2
         (when condition            ; open 3
           (something)))            ; close 3,2 - MISSING close for 1!
     (return result)))              ; this closes defun but not let!
   ```

3. **Use Emacs built-in checks**:
   ```elisp
   M-x check-parens  ; in the buffer
   ; or batch mode:
   emacs --batch --eval "(progn (find-file \"file.el\") (check-parens))"
   ```

### Common Lisp vs Emacs Lisp Gotchas

**Always add `(require 'cl-lib)` at the top of your elisp files.**

Common incompatibilities:

| Common Lisp | Emacs Lisp | Note |
|-------------|------------|------|
| `coerce` | `cl-coerce` | Type conversion |
| `dotimes` with `(return)` | `cl-dotimes` with `(cl-return)` | Early loop exit |
| `return` | `cl-return` | Must be in `cl-` block form |
| `loop` | `cl-loop` | Full loop macro |

**Quick fix checklist:**
- [ ] Added `(require 'cl-lib)` at top?
- [ ] Changed `coerce` to `cl-coerce`?
- [ ] Using `cl-dotimes` with `cl-return`?
- [ ] Or using manual flag instead of `cl-return`?

**Alternative to cl-return:**
```elisp
;; Instead of:
(dotimes (i n)
  (when condition
    (cl-return)))  ; ERROR: no catch for tag

;; Use manual flag:
(let ((found nil))
  (cl-dotimes (i n)
    (unless found
      (when condition
        (setq found t)))))
```

## Example Session

**Symptom:**
```
completing-read: Wrong type argument: sequencep, jf/gptel--load-context-from-path
```

**Debug Process:**

1. **Recognize pattern**: Function symbol as value → missing `)`

2. **Isolate in org-babel**:
   ```org
   *** Test Function
   #+begin_src emacs-lisp :tangle no
   (defun test-function ()
     ...suspicious function code...)
   #+end_src
   ```

3. **Count parens with comments**:
   ```elisp
   (defun build-candidates (dirs)
     (let (candidates)              ; open 1
       (dolist (dir dirs)           ; open 2
         (when metadata             ; open 3
           (let* ((tree ...))       ; open 4
             (dolist (leaf ...)     ; open 5
               (push ... candidates))))) ; close 5,4,3,2 - need 4 closes!
     (nreverse candidates)))        ; close 1
   ```

4. **Found issue**: Only 3 closing parens after `push`, need 4

5. **Fix**: Add missing `)`

## When to Use This Skill

- Encountering "Wrong type argument" errors with function symbols
- Working with literate elisp in org-mode files
- Integrating Common Lisp libraries or code into Emacs Lisp
- Mysterious errors after adding CL-style constructs
- Debugging deeply nested elisp forms

## Related Skills

- **emacs-literate-programming**: General org-mode tangling workflow
- **skill-creator**: For capturing new elisp patterns discovered during debugging
