# Register entry — shape tier

A **shape** entry describes a plist / struct / alist / record that crosses a module boundary. The motivating failure: the same logical thing (a validation result, a violation-info plist, a YAML-parsed config, a handler output) gets constructed in multiple modules with subtly different fields, forcing fallback chains and silent data loss at consumers.

Shape entries are the most common register entry. Most "the system worked in tests but broke in real use" failures are shape failures.

## Required fields

```yaml
entry_id: register/shape/<short-name>
tier: shape
status: speculated | confirmed | divergent | reconciled
load_bearing: true | false
title: <one-line, e.g. "violation-info plist">
purpose: <one paragraph — what this shape carries between which modules>

required_keys:
  - name: tool
    type: string
    description: <what consumers need this for>
  - name: resource
    type: string
    description: <...>

optional_keys:
  - name: reason
    type: string | null
    description: <why this is optional, what consumers fall back to>

producers:
  - file: scope-validation.el
    function: build-violation-info
  - file: scope-shell-tools.el
    function: <fn>

consumers:
  - file: scope-expansion.el
    function: format-violation-message
    reads_keys: [tool, resource, reason]

validator: |
  (defun shape/validate-violation-info (val)
    "Returns nil on success, error symbol on failure."
    (cond ((not (plistp val)) 'not-a-plist)
          ((not (stringp (plist-get val :tool))) 'missing-tool)
          ...))

test_corpus:
  valid:
    - { tool: "bash", resource: "rm", reason: "blocked" }
  invalid:
    - { tool: "bash" }   # missing required :resource

discovered_from: <change-name or task-name>
discovered_by: architect
```

## Status-specific fields

When `status: reconciled`, add:

```yaml
why_tests_missed: <one sentence — why per-site tests passed while the shape was wrong>
reconciliation_note_path: register/notes/<entry-id>.md
prior_shape: <YAML excerpt of what the entry said before>
```

When `status: divergent`, add:

```yaml
divergence_note: <what's mismatched and where>
escalation: architect | user
```

## When to create one

The Architect creates shape entries during plan-phase forward-mode whenever:

- A new module boundary is introduced.
- A task's brief mentions "this returns/accepts a {plist,struct,record,dict}" and the shape isn't already entried.
- A boundary-translation scan during execute reveals a shape being reshaped at >1 site.

## When to mark `load_bearing: true`

Set `load_bearing: true` when:

- The shape is consumed by ≥2 modules.
- The shape carries data that, if mis-shaped, fails silently (no exception; just wrong behaviour).
- Past incidents in `why_tests_missed` show this shape has bitten before.

Load-bearing shapes get on-touch Architect review during execute (see `flows/execute.md`).
