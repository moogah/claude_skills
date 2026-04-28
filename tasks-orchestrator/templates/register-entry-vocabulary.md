# Register entry — vocabulary tier

A **vocabulary** entry pins a closed set of allowed values — error codes, op types, validation types, status enums. The motivating failure: layer A produces N variants, layer B accepts M, the translation logic is inlined at multiple call sites, and some call sites forget the translation entirely. Bash-parser produced 11+ operation types; scope.yml accepted 3 sections; the mapping was inlined as a `pcase` in two places and skipped at a third, silently routing `:read-metadata` violations into `:paths.write`.

Vocabulary entries are second-most common after shape entries and the most under-spec'd in practice.

## Required fields

```yaml
entry_id: register/vocabulary/<short-name>
tier: vocabulary
status: speculated | confirmed | divergent | reconciled
load_bearing: true | false
title: <one-line, e.g. "filesystem operation types">
purpose: <one paragraph — what this vocabulary names, who chose the values>

members:
  - value: read
    description: <...>
    producer:
      file: scope-bash-parser.el
      function: extract-read-ops
  - value: write
    description: <...>
    producer:
      file: scope-bash-parser.el
      function: extract-write-ops

consumer_mapping:
  - consumer: scope.yml
    consumer_field: paths.read
    accepts: [read, read-metadata, match-pattern]
    routing_function: scope-add-path-to-scope
  - consumer: scope.yml
    consumer_field: paths.write
    accepts: [write, create, append, delete, modify]
    routing_function: scope-add-path-to-scope

canonical_mapping_function: |
  ;; The single source of truth for value → consumer-field routing.
  ;; If you find a pcase / cond / cl-case translating these values
  ;; anywhere else, that's a duplication bug.
  (defun vocabulary/op-to-section (op)
    (pcase op
      ((or 'read 'read-metadata 'match-pattern) :read)
      ((or 'write 'create 'append 'delete 'modify) :write)
      ((or 'execute) :execute)))

validator: |
  (defun vocabulary/validate-op (op)
    (memq op '(read read-metadata match-pattern
               write create append delete modify
               execute)))

closed_set: true   # are these the only allowed values, or is the set extensible?

discovered_from: <change-name or task-name>
discovered_by: architect
```

## Status-specific fields

When `status: reconciled`, add:

```yaml
why_tests_missed: <one sentence — typically "per-call-site tests pinned their own subset; no test covered the full vocabulary or the translation table">
reconciliation_note_path: register/notes/<entry-id>.md
members_added: [<list of values that emerged from impl>]
members_removed: [<list of values that turned out unused>]
```

When `status: divergent`, add:

```yaml
divergent_at: [<file:function pairs where the impl uses values not in this entry>]
escalation: architect | user
```

## When to create one

The Architect creates vocabulary entries during plan-phase forward-mode whenever:

- A task's brief or design-doc references "valid types are X, Y, Z" or "error codes E1, E2, E3".
- Boundary-translation scan finds a `pcase` / `cond` / `cl-case` / `match` translating between two layers' value sets.
- A new module is introduced that consumes someone else's enum-like output.

## When to mark `load_bearing: true`

Set `load_bearing: true` when:

- More than one consumer reads the vocabulary's values.
- A missing member at a consumer would silently misroute (not error visibly).
- The vocabulary is the boundary between a parsing layer and a policy layer.

The bash-parser → scope vocabulary mismatch is the canonical example: load-bearing because mis-routing `:read-metadata` into `:paths.write` was a security gap, not a visible bug.
