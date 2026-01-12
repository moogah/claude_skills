---
name: writing-elisp
description: Guidance for writing valid Emacs Lisp code with incremental validation. Use when writing or modifying elisp functions, especially complex nested forms with cl-loop, multiple let* bindings, or lambdas.
---

# Writing Elisp

## Overview

This skill provides proactive guidance for writing syntactically valid Emacs Lisp code, with emphasis on incremental validation to catch errors early. LLMs frequently produce parenthesis errors in complex nested elisp - this skill helps prevent those issues through automated validation during the writing process.

## When to Use This Skill

- Before writing any elisp function >20 lines
- When writing complex nested forms (cl-loop, multiple let*, lambdas)
- When modifying existing elisp in literate org files
- After generating elisp code with an LLM
- When working with deeply nested expressions (3+ levels)

## Incremental Validation Workflow

The key principle: **validate after each function, before moving to the next**.

1. **Write in small chunks** - One function at a time, not entire files
2. **Validate immediately** - Run validation after each function
3. **Use automated tools** - Never rely on visual inspection or manual counting
4. **Test in isolation** - Extract to temp file if needed for testing
5. **Fix before continuing** - Don't accumulate errors

## Validation Commands

### Quick Paren Check (Fastest)
```bash
/Applications/Emacs.app/Contents/MacOS/Emacs --batch --eval \
  "(progn (find-file \"file.el\") (check-parens))"
```

Returns immediately with error or success. Use this after every function.

### Full Syntax Check with Byte Compiler
```bash
/Applications/Emacs.app/Contents/MacOS/Emacs --batch --eval \
  "(progn (find-file \"file.el\") (byte-compile-file \"file.el\"))"
```

Catches more issues but slower. Use before committing.

### Scan for Premature Expression Endings
```bash
/Applications/Emacs.app/Contents/MacOS/Emacs --batch --eval \
  "(with-temp-buffer
     (insert-file-contents \"file.el\")
     (goto-char (point-min))
     (condition-case scan-err
         (while (not (eobp)) (forward-sexp 1))
       (scan-error
         (message \"Scan error at position %d: %s\" (point) scan-err))))"
```

Shows exact position of paren mismatches. Use when check-parens reports errors.

## Complexity Thresholds

### Low Risk (2 nesting levels)
```elisp
(let ((x 1))
  (+ x 2))
```

**Action**: Validate after 5-10 functions

### Medium Risk (3 nesting levels)
```elisp
(let ((x (foo)))
  (when x
    (bar x)))
```

**Action**: Validate after each 2-3 functions

### High Risk (4+ nesting levels)
```elisp
(cl-loop for i from 1 below (length path)
         for target-id = (aref path i)
         do
         (let ((children (plist-get node :children)))
           (when node
             (let* ((file (plist-get node :file))
                    (content (when (file-exists-p full-path)
                              (with-temp-buffer
                                (insert-file-contents full-path)
                                (buffer-string)))))
               (push (list :type type :content content) context)))))
```

**Action**: Validate after EACH high-risk function
**Consider**: Breaking into smaller helper functions

## Integration with Literate Programming

When working with org-mode tangled files:

### After Writing Elisp Block
```org
#+begin_src emacs-lisp
(defun my-complex-function (arg1 arg2)
  "Documentation..."
  (let ((result (calculate arg1)))
    (when result
      (process result arg2))))
#+end_src
```

### Immediate Validation Step
```bash
./bin/tangle-org.sh file.org && \
/Applications/Emacs.app/Contents/MacOS/Emacs --batch --eval \
  "(progn (find-file \"file.el\") (check-parens))"
```

Add this as a habit after each significant elisp block.

## Example Workflow

**User Request:** "Add a function to load context from a tree path"

**Agent Response:**
1. Write function in org-mode code block
2. Tangle to .el file: `./bin/tangle-org.sh file.org`
3. Validate: Run check-parens on tangled .el file
4. If errors: Fix in .org file and repeat steps 2-3
5. If clean: Continue to next function
6. Before committing: Run byte-compile check on full file

## Common LLM Failure Patterns

LLMs frequently produce paren errors in:
- cl-loop with nested let* and lambdas (like example above)
- Functions >50 lines
- Complex backquote/unquote expressions
- Multiple nested when/if/cond forms

**Prevention Strategy:**
- Write these in 10-20 line chunks
- Validate after each chunk
- Use helper functions to reduce nesting

## Real-World Example

From actual debugging session where LLM produced:

```elisp
(push (list :type type :content content) context))))))
                                                   ^^^^^^
                                                   6 parens - should be 5!
```

The function had 4 nesting levels (cl-loop, let, when, let*). The extra paren was caught by check-parens immediately after tangling, not after committing or restarting Emacs.

## Integration with Claude Code Hooks

If using Claude Code with file-write hooks, the validation can be automated:

```python
# In .claude/hooks/validate_elisp.py
import subprocess
import sys

def validate_elisp(file_path):
    if not file_path.endswith('.el'):
        return True

    result = subprocess.run([
        '/Applications/Emacs.app/Contents/MacOS/Emacs',
        '--batch',
        '--eval',
        f'(progn (find-file "{file_path}") (check-parens))'
    ], capture_output=True, text=True)

    if result.returncode != 0:
        print(f"Elisp validation failed: {result.stderr}", file=sys.stderr)
        return False
    return True
```

## Tips

1. **Never skip validation** - "It looks right" is not reliable for elisp
2. **Start simple** - Write trivial version first, then add complexity
3. **Test incrementally** - Don't write 100+ lines before first validation
4. **Use git** - Commit working code frequently so you have rollback points
5. **Break down complexity** - Helper functions are cheaper than debugging

## References

For debugging when validation fails, see the `emacs-elisp-debugging` skill which covers:
- Git-based debugging strategies
- Automated validation tools (Flycheck, flyparens)
- Error pattern recognition
