# Performance Guidelines

## When to Optimize

**Don't optimize prematurely.** Profile first, optimize second.

### Optimization Priority

1. **Profile to find hotspots** - Use Emacs built-in profiler
2. **Fix algorithmic issues** - O(n²) → O(n) matters more than micro-optimizations
3. **Cache expensive operations** - Network calls, file I/O, parsing
4. **Optimize hot paths** - Only optimize code that runs frequently
5. **Measure impact** - Verify optimization actually helps

### Signs You Should Profile

- UI feels sluggish
- Operations take >100ms that should be instant
- Noticeable delay when typing or moving cursor
- High CPU usage when idle
- Functions called frequently (hooks, timers)

## Profiling

### Built-in Emacs Profiler

```elisp
;; Start profiler
M-x profiler-start RET cpu RET

;; Do the slow operation
M-x my-slow-function

;; View report
M-x profiler-report

;; Stop profiler
M-x profiler-stop
```

The report shows which functions consume most time. Focus optimization there.

### Benchmarking Specific Code

```elisp
(require 'benchmark)

;; Simple timing
(benchmark-run 1000
  (my-function))
;; => (2.5 4 0.8)  ; (total-time gc-count gc-time)

;; Compare implementations
(benchmark-run-compiled 1000
  (implementation-a))
;; vs
(benchmark-run-compiled 1000
  (implementation-b))
```

## Common Performance Issues

### 1. O(n²) List Operations

```elisp
;; SLOW - O(n²) - repeatedly append to end
(let ((result nil))
  (dotimes (i 10000)
    (setq result (append result (list i))))
  result)

;; FAST - O(n) - push to front, reverse once
(let ((result nil))
  (dotimes (i 10000)
    (push i result))
  (nreverse result))

;; FASTER - pre-allocate if size known
(let ((result (make-vector 10000 nil)))
  (dotimes (i 10000)
    (aset result i i))
  result)
```

### 2. Repeated String Concatenation

```elisp
;; SLOW - creates new string each iteration
(let ((result ""))
  (dolist (word words)
    (setq result (concat result word " ")))
  result)

;; FAST - join once
(mapconcat #'identity words " ")

;; ALSO FAST - collect then join
(string-join words " ")
```

### 3. Unnecessary Buffer Switching

```elisp
;; SLOW - switches buffer context
(defun process-files (files)
  (dolist (file files)
    (with-current-buffer (find-file-noselect file)
      (process-buffer))))

;; FASTER - if you don't need major mode
(defun process-files (files)
  (dolist (file files)
    (with-temp-buffer
      (insert-file-contents file)
      (process-buffer))))
```

### 4. Repeated Expensive Calls

```elisp
;; SLOW - calls expensive function 3 times
(defun my-function (items)
  (when (> (expensive-count items) 0)
    (message "Found %d items" (expensive-count items))
    (process-items (expensive-count items))))

;; FAST - cache the result
(defun my-function (items)
  (let ((count (expensive-count items)))
    (when (> count 0)
      (message "Found %d items" count)
      (process-items count))))
```

### 5. Regex Compilation

```elisp
;; SLOW - recompiles regex every call
(defun my-search (text)
  (string-match "\\(foo\\|bar\\|baz\\)" text))

;; FAST - compile once, reuse
(defconst my-search-regexp
  (rx (or "foo" "bar" "baz"))
  "Precompiled search pattern.")

(defun my-search (text)
  (string-match my-search-regexp text))
```

## Data Structure Choices

### Lists vs Vectors vs Hash Tables

| Operation | List | Vector | Hash Table |
|-----------|------|--------|------------|
| Access by index | O(n) | O(1) | N/A |
| Access by key | O(n) | N/A | O(1) |
| Append to end | O(n) | O(1)* | N/A |
| Insert at front | O(1) | O(n) | N/A |
| Search for value | O(n) | O(n) | O(n)† |
| Memory overhead | Low | Low | Medium |

*amortized, †iterating values

```elisp
;; Use lists for: small collections, frequent front insertion
(let ((items '(a b c)))
  (push 'd items))  ; Fast

;; Use vectors for: indexed access, known size
(let ((items (make-vector 1000 nil)))
  (aset items 500 'value))  ; Fast

;; Use hash tables for: key-based lookup
(let ((cache (make-hash-table :test 'equal)))
  (puthash "key" 'value cache)
  (gethash "key" cache))  ; Fast
```

## Caching Patterns

### Simple Memoization

```elisp
;; Cache results in hash table
(defvar my-cache (make-hash-table :test 'equal))

(defun my-expensive-function (arg)
  "Compute result, cache it."
  (or (gethash arg my-cache)
      (puthash arg
               (expensive-computation arg)
               my-cache)))

;; Clear cache when needed
(defun my-clear-cache ()
  "Reset cache."
  (clrhash my-cache))
```

### Time-Based Cache Invalidation

```elisp
(defvar my-cache (make-hash-table :test 'equal))
(defvar my-cache-timeout 60)  ; seconds

(defun my-cached-function (arg)
  "Compute result with time-based cache."
  (let* ((now (float-time))
         (cached (gethash arg my-cache))
         (value (car cached))
         (timestamp (cdr cached)))
    (if (and cached (< (- now timestamp) my-cache-timeout))
        value
      (let ((new-value (expensive-computation arg)))
        (puthash arg (cons new-value now) my-cache)
        new-value))))
```

### Buffer-Local Caching

```elisp
(defvar-local my-buffer-cache nil
  "Cache data for current buffer.")

(defun my-get-buffer-data ()
  "Get data, cache per-buffer."
  (or my-buffer-cache
      (setq my-buffer-cache (expensive-parse-buffer))))

;; Invalidate on buffer change
(defun my-clear-buffer-cache ()
  "Clear cache when buffer changes."
  (setq my-buffer-cache nil))

(add-hook 'after-change-functions
          (lambda (&rest _) (my-clear-buffer-cache))
          nil t)  ; Buffer-local hook
```

## Lazy Loading

### Autoloading

```elisp
;; Don't load heavy features at startup
;;;###autoload
(defun my-heavy-feature-command ()
  "Command that loads heavy feature on first use."
  (interactive)
  (require 'my-heavy-feature)
  (my-heavy-feature-run))

;; Instead of:
;; (require 'my-heavy-feature)  ; Loads at startup
```

### Deferred Initialization

```elisp
;; Initialize lazily
(defvar my-system-initialized nil)

(defun my-ensure-initialized ()
  "Initialize system if needed."
  (unless my-system-initialized
    (my-expensive-setup)
    (setq my-system-initialized t)))

(defun my-command ()
  "Command that ensures initialization."
  (interactive)
  (my-ensure-initialized)
  (do-work))
```

### with-eval-after-load

```elisp
;; Configure package after it loads, not at startup
(with-eval-after-load 'magit
  (setq magit-display-buffer-function
        #'magit-display-buffer-fullframe-status-v1))

;; Instead of:
;; (require 'magit)  ; Loads immediately
;; (setq magit-display-buffer-function ...)
```

## Buffer Operations

### Point Preservation

```elisp
;; Slower - save-excursion saves more state
(save-excursion
  (goto-char (point-min))
  (do-something))

;; Faster - only save point
(let ((orig-point (point)))
  (goto-char (point-min))
  (do-something)
  (goto-char orig-point))

;; Use save-excursion when you need mark preservation
;; Use manual point saving when you don't
```

### Buffer Modification

```elisp
;; Disable expensive features during bulk edits
(defun my-bulk-edit ()
  "Make many buffer changes efficiently."
  (let ((inhibit-modification-hooks t)  ; Skip change hooks
        (inhibit-read-only t))           ; Allow edits
    (with-silent-modifications             ; Don't mark as modified yet
      (dotimes (i 1000)
        (insert "x")))))
```

### with-temp-buffer vs reusable buffer

```elisp
;; For one-off operations
(with-temp-buffer
  (insert data)
  (process))

;; For repeated operations - reuse buffer
(defvar my-work-buffer nil)

(defun my-get-work-buffer ()
  "Get or create work buffer."
  (or (and (buffer-live-p my-work-buffer)
           my-work-buffer)
      (setq my-work-buffer (generate-new-buffer " *my-work*"))))

(defun my-repeated-operation (data)
  "Reuse buffer for efficiency."
  (with-current-buffer (my-get-work-buffer)
    (erase-buffer)
    (insert data)
    (process)))
```

## Algorithm Selection

### Searching

```elisp
;; For small lists (<100 items) - linear search is fine
(member item small-list)

;; For large lists with repeated searches - use hash table
(defvar my-large-set (make-hash-table :test 'equal))
(dolist (item large-list)
  (puthash item t my-large-set))
(gethash item my-large-set)  ; Much faster
```

### Sorting

```elisp
;; Default sort (destructive, modifies list)
(setq items (sort items #'<))

;; For large data - consider pre-sorted structures
;; Or sort once, binary search many times
```

## Avoiding Common Traps

### Don't Optimize Too Early

```elisp
;; START HERE - clear and correct
(defun my-function (items)
  (mapcar (lambda (item)
            (when (valid-p item)
              (transform item)))
          items))

;; Only if profiling shows this is slow, optimize to:
(defun my-function (items)
  (let ((result nil))
    (dolist (item items)
      (when (valid-p item)
        (push (transform item) result)))
    (nreverse result)))
```

### Measure Don't Guess

```elisp
;; Think you know which is faster? Measure it!
(benchmark-run 10000
  (string-match "foo" text))
;; vs
(benchmark-run 10000
  (string-search "foo" text))  ; string-search is faster for literals

;; Surprises happen - always measure
```

### Profile in Realistic Conditions

```elisp
;; Don't profile with 10 items when production has 10,000
;; Don't profile with small strings when production has large files
;; Performance characteristics change with scale
```

## Performance Checklist

- ✅ Profile before optimizing
- ✅ Fix algorithmic issues first (O(n²) → O(n))
- ✅ Cache expensive operations (network, file I/O, parsing)
- ✅ Use appropriate data structures (hash tables for lookup)
- ✅ Avoid repeated string concatenation
- ✅ Avoid repeated list append-to-end
- ✅ Cache expensive function call results in let-bindings
- ✅ Lazy load heavy features (autoload, with-eval-after-load)
- ✅ Measure impact of optimizations
- ✅ Keep code readable - optimize only hot paths

## Summary

**Rules of Performance:**
1. **Correct first, fast second** - Don't optimize broken code
2. **Profile to find hotspots** - 90% of time is spent in 10% of code
3. **Algorithm > micro-optimization** - O(n) vs O(n²) matters more than saving a few function calls
4. **Measure impact** - Verify optimizations actually help
5. **Keep it readable** - Future you needs to understand this code
