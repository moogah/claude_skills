# Documentation Standards

## High-Quality Docstrings

### Basic Structure

```elisp
(defun my-function (arg1 arg2)
  "Single line summary (imperative mood).

Detailed explanation if needed. Explain what the function does,
not how it does it (that's what comments are for).

ARG1 should be a string representing...
ARG2 can be nil to indicate... or a number for...

Returns a list of (RESULT . STATUS) where RESULT is...

Signals `my-error' if ARG1 is invalid.

Example:
  (my-function \"foo\" 10)
  => (\"result\" . success)"
  ...)
```

### Docstring Rules

1. **First line is complete sentence** ending with period
2. **First line stands alone** as summary (appears in completion)
3. **Blank line after first line** if there's more
4. **Use UPPERCASE for arguments** when referring to them
5. **Describe return value** if not obvious
6. **Document errors** that might be signaled
7. **Include examples** for complex functions
8. **Use imperative mood** ("Return the value", not "Returns the value")

### Examples

#### Simple Function

```elisp
(defun my-add (a b)
  "Add A and B."
  (+ a b))
```

#### Function with Optional Args

```elisp
(defun my-format-name (first last &optional middle)
  "Format name from FIRST, LAST, and optionally MIDDLE.

If MIDDLE is non-nil, format as \"FIRST MIDDLE LAST\".
Otherwise format as \"FIRST LAST\".

Returns formatted string."
  (if middle
      (format "%s %s %s" first middle last)
    (format "%s %s" first last)))
```

#### Function with Keyword Args

```elisp
(cl-defun my-query-api (endpoint &key (method 'GET) (timeout 30) data)
  "Query API at ENDPOINT with options.

METHOD is the HTTP method (default GET).
TIMEOUT is seconds to wait (default 30).
DATA is the request body for POST/PUT requests.

Returns parsed JSON response as alist.

Signals `my-api-error' if request fails.

Example:
  (my-query-api \"/users/123\" :method 'GET)
  => ((id . 123) (name . \"John\"))"
  ...)
```

#### Complex Function

```elisp
(defun my-parse-config (file)
  "Parse configuration from FILE.

FILE should be a path to a JSON configuration file.
If FILE is relative, it's resolved relative to `default-directory'.

The configuration must have these required keys:
  - version: String, semantic version
  - rules: Array of rule objects

Returns a plist with normalized configuration:
  (:version VERSION :rules RULES :timestamp TIMESTAMP)

Signals:
  - `file-error' if FILE doesn't exist or isn't readable
  - `json-parse-error' if FILE isn't valid JSON
  - `my-config-error' if required keys are missing

Example:
  (my-parse-config \"~/.config/my-app.json\")
  => (:version \"1.0.0\" :rules [...] :timestamp 1234567890)"
  ...)
```

### Interactive Functions

```elisp
(defun my-insert-date (prefix)
  "Insert current date at point.

With PREFIX argument, insert in ISO format (YYYY-MM-DD).
Without PREFIX, insert in human-readable format (Month DD, YYYY).

In Lisp code, PREFIX is a number (the raw prefix argument)."
  (interactive "P")
  (insert
   (if prefix
       (format-time-string "%Y-%m-%d")
     (format-time-string "%B %d, %Y"))))
```

## Package Structure

### Package Headers

Every package file should have a standard header:

```elisp
;;; my-package.el --- Brief description (one line)  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Your Name

;; Author: Your Name <email@example.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (dash "2.19"))
;; Keywords: convenience, tools
;; URL: https://github.com/username/my-package

;;; Commentary:

;; Detailed description of what the package does.
;; Can be multiple paragraphs explaining features,
;; usage, and background.
;;
;; Basic usage:
;;   (require 'my-package)
;;   (my-package-enable)

;;; Code:

(require 'dash)
;; ... your code here ...

(provide 'my-package)
;;; my-package.el ends here
```

### Header Components

| Field | Required | Purpose |
|-------|----------|---------|
| Brief description | Yes | Shows in package list |
| `lexical-binding: t` | Yes | Enable lexical binding |
| Copyright | Recommended | Legal attribution |
| Author | Yes | Contact info |
| Version | Yes | Semantic versioning |
| Package-Requires | If has deps | Dependency list |
| Keywords | Recommended | Categorization |
| URL | Recommended | Homepage/repo |
| Commentary | Recommended | User documentation |
| `provide` | Yes | Makes loadable |

### Version Numbers

Use semantic versioning (MAJOR.MINOR.PATCH):

```elisp
;; Version: 1.0.0   - Initial release
;; Version: 1.1.0   - Added new feature (backward compatible)
;; Version: 1.1.1   - Bug fix (no new features)
;; Version: 2.0.0   - Breaking change (not backward compatible)
```

### Package-Requires Format

```elisp
;; Single dependency
;; Package-Requires: ((emacs "27.1"))

;; Multiple dependencies
;; Package-Requires: ((emacs "27.1") (dash "2.19") (s "1.12"))

;; With explanation comments
;; Package-Requires: ((emacs "27.1")      ; Need pcase-let
;;                    (dash "2.19")       ; Need -map
;;                    (request "0.3.0"))  ; HTTP requests
```

## Autoloading

### Autoload Cookies

Mark public entry points with autoload cookies so they load on demand:

```elisp
;;;###autoload
(defun my-package-enable ()
  "Enable my-package features globally."
  (interactive)
  (add-hook 'after-init-hook #'my-package--setup))

;;;###autoload
(define-minor-mode my-package-mode
  "Toggle my-package mode."
  :lighter " MyPkg"
  :global t
  (if my-package-mode
      (my-package--enable)
    (my-package--disable)))

;; Internal function - no autoload
(defun my-package--setup ()
  "Internal setup function."
  ...)
```

### What to Autoload

**Do autoload:**
- Public commands (interactive functions)
- Minor modes and major modes
- Entry point functions
- Customization groups (when used with defgroup)

**Don't autoload:**
- Internal helper functions
- Variables (unless they're for customization)
- Simple constants
- Functions that are only called by other package functions

### Autoload for Major Modes

```elisp
;;;###autoload
(define-derived-mode my-config-mode prog-mode "MyConfig"
  "Major mode for editing my-config files."
  (setq-local comment-start "#")
  (setq-local comment-end ""))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.myconf\\'" . my-config-mode))
```

## Customization Groups

### defgroup and defcustom

```elisp
(defgroup my-package nil
  "Customization group for my-package."
  :group 'convenience
  :prefix "my-package-")

(defcustom my-package-auto-save t
  "Whether to automatically save changes.

When non-nil, changes are saved immediately.
When nil, user must save manually with `my-package-save'."
  :type 'boolean
  :group 'my-package)

(defcustom my-package-save-interval 60
  "Interval in seconds between auto-saves.

Only used when `my-package-auto-save' is non-nil."
  :type 'integer
  :group 'my-package)

(defcustom my-package-format-function #'my-package-default-format
  "Function to format output.

Should be a function taking one argument (the data to format)
and returning a formatted string."
  :type 'function
  :group 'my-package)
```

### Common Custom Types

```elisp
;; Boolean
:type 'boolean

;; String
:type 'string

;; Integer
:type 'integer

;; Number (int or float)
:type 'number

;; Symbol
:type 'symbol

;; Function
:type 'function

;; File path
:type 'file

;; Directory path
:type 'directory

;; Choice among options
:type '(choice (const :tag "Never" nil)
               (const :tag "Always" t)
               (integer :tag "After N seconds"))

;; List of strings
:type '(repeat string)

;; Alist
:type '(alist :key-type string :value-type integer)

;; Plist
:type '(plist :key-type symbol :value-type string)
```

## Forward Declarations

Avoid byte-compiler warnings for functions defined elsewhere:

```elisp
;; Declare functions from other packages
(declare-function org-element-at-point "org-element" ())
(declare-function projectile-project-root "projectile" ())

;; Use them without warnings
(defun my-function ()
  (when (org-element-at-point)
    (let ((root (projectile-project-root)))
      ...)))
```

## File Structure Template

```elisp
;;; my-package.el --- Package description  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Author Name
;; Author: Author Name <email@example.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience
;; URL: https://github.com/user/my-package

;;; Commentary:

;; Package documentation here.

;;; Code:

;;; Requirements
(require 'cl-lib)
(require 'subr-x)  ; For string utilities

;;; Customization
(defgroup my-package nil
  "My package customization."
  :group 'convenience)

(defcustom my-package-setting t
  "A setting."
  :type 'boolean
  :group 'my-package)

;;; Variables
(defvar my-package--internal-state nil
  "Internal state (not customizable).")

;;; Internal Functions
(defun my-package--helper (arg)
  "Helper function for ARG."
  ...)

;;; Public API
;;;###autoload
(defun my-package-do-something ()
  "Do something useful."
  (interactive)
  ...)

(provide 'my-package)
;;; my-package.el ends here
```

## Quick Documentation Checklist

- ✅ Lexical binding header on first line
- ✅ Package headers (Author, Version, Package-Requires)
- ✅ Docstring on every public function
- ✅ First line of docstring is complete sentence
- ✅ UPPERCASE for parameter names in docstrings
- ✅ Document return values and errors
- ✅ Autoload cookies on public entry points
- ✅ Customization group with defgroup/defcustom
- ✅ Forward declarations for external functions
- ✅ File ends with (provide 'package-name)
