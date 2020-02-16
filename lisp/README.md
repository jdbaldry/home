# Lisp

Collection of Lisp code that doesn't have a better home.

## Dialects

The lisp source code in these files will be one of two dialects. Files ending in `.scm` are of MIT Scheme dialect, files ending in `.lisp` are Common Lisp.

## Dependencies

For Common Lisp:

```
sudo dnf install -y clisp
```

For MIT Scheme: https://www.gnu.org/software/mit-scheme/

## Running Programs

Running non-interactive scheme programs can be achieved with:

```
scheme --batch-mode --load table.scm --eval '(write-line (get-dict dict "test"))' --eval '(exit)'
```
