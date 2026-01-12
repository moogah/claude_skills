---
name: emacs-elisp-debugging
description: Strategies for debugging Emacs Lisp code, especially missing parentheses and Common Lisp compatibility issues. Use when encountering mysterious elisp errors or when working with literate elisp in org-mode files.
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

**Containing expression ends prematurely = extra closing parenthesis**

```
ERROR: (scan-error Containing expression ends prematurely 33145 33146)
```

This means there are more closing parens than opening parens somewhere earlier in the file. The position shown is where the scan stops, but the actual error is usually in a previous function. Use git diff or forward-sexp scanning to find the real problem.

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

## Git-Based Debugging Strategy

When a file was recently working, use git to isolate what changed rather than examining the entire file.

### Find Last Working Commit
```bash
git log --oneline -- path/to/file.el
```

Look for commits where the file loaded successfully (before the error appeared).

### Test Specific Commit
```bash
git show <commit-hash>:path/to/file.el > /tmp/working.el
/Applications/Emacs.app/Contents/MacOS/Emacs --batch --eval \
  "(progn (find-file \"/tmp/working.el\") (check-parens))"
```

If this passes, you know the error was introduced after this commit.

### Diff Against Working Version
```bash
git diff <working-commit> HEAD -- path/to/file.el
```

This shows exactly what changed. The paren error is likely in one of the modified functions.

### Example Workflow
```
1. File worked at commit 6f7aa22
2. File fails at current HEAD
3. Run: git diff 6f7aa22 HEAD -- file.el
4. See ~700 lines added in session auto-save functions
5. Extract one of the new functions to test in isolation
6. Run check-parens on extracted function
7. Find the function with the paren error
```

This approach is much faster than examining the entire file, especially when hundreds of lines were added.

## Automated Validation Tools

### Real-Time Checking

**flyparens** - Minor mode that checks for unbalanced parens on the fly:
```elisp
(use-package flyparens
  :straight t
  :hook (emacs-lisp-mode . flyparens-mode))
```

Highlights the first mismatched paren as you type, whether at point or not.

**Flycheck** - Already checks elisp with byte compiler automatically:
```elisp
;; Usually already configured
(use-package flycheck
  :straight t
  :hook (emacs-lisp-mode . flycheck-mode))
```

### Save Hooks

Add check-parens to save hook so you can't save invalid code:
```elisp
(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (add-hook 'before-save-hook #'check-parens nil t)))
```

Warning: This blocks saving until you fix the error.

### Forward-Sexp Scanning

Programmatically find exact position of paren mismatches:
```bash
/Applications/Emacs.app/Contents/MacOS/Emacs --batch --eval "
(condition-case err
    (with-temp-buffer
      (insert-file-contents \"file.el\")
      (goto-char (point-min))
      (condition-case scan-err
          (while (not (eobp)) (forward-sexp 1))
        (scan-error
         (message \"Scan error at position %d: %s\" (point) scan-err))))
  (error (message \"Error: %s\" err)))
"
```

Shows the exact character position where scanning fails.

## LLM Elisp Limitations

LLMs frequently produce parenthesis errors in these patterns:

### High-Risk Constructs
- **Deeply nested forms** (3+ levels): cl-loop with nested let* and lambdas
- **Long functions** (>50 lines): Easy to lose track of nesting
- **Complex backquote expressions**: Multiple levels of `,` and `,@`
- **Multiple nested conditionals**: when/if/cond with let forms inside

### Common Failure Example
```elisp
(cl-loop for i from 1 below (length path)
         do
         (let ((children ...))
           (when node
             (let* ((file ...)
                    (content ...))
               (push ... context))))))  ; Extra paren here!
```

The extra paren typically appears at the end of the most deeply nested form.

### Mitigation Strategies

1. **Write incrementally** - One function at a time, validate after each
2. **Break down complexity** - Extract helper functions to reduce nesting
3. **Never manually count parens** - Use automated tools (check-parens, forward-sexp)
4. **Test in isolation** - Extract complex functions to temp files for testing
5. **Use git frequently** - Commit working code so you can diff against last-good version

### Recommended Workflow

See the `writing-elisp` skill for proactive validation during code writing, including:
- Incremental validation commands
- Complexity thresholds (when to validate)
- Integration with literate programming
- Claude Code hooks for automatic validation

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

- **writing-elisp**: Proactive validation during code writing to prevent errors
- **emacs-literate-programming**: General org-mode tangling workflow
- **skill-creator**: For capturing new elisp patterns discovered during debugging
