# Recommendations for future cleanup of `contrib/binding.py`

This document captures follow-up ideas that came out of the
mechanical cleanup pass on `binding.py`. They were left out of the
initial cleanup because they would either change generated output or
require behaviour-changing refactoring. Treat them as a roadmap, not a
backlog.

## 1. Split `binding.py` into a package

At 3000+ lines, `binding.py` is hard to navigate. The pieces are
already well separated:

- module-level QName constants and the `_get_type` / `_get_clean_doc`
  helpers,
- `GIR` and `GlobalsBinder`,
- `SubprogramProfile`,
- `GIRClass` (which is itself huge and could be split further by
  concern: methods, signals, properties, fields, virtual methods,
  records),
- the CLI / orchestration layer that is now in `main()`.

A `contrib/binding/` Python package with one module per concern would
make the code substantially easier to onboard new contributors onto.
The blocker is the wildcard `from adaformat import *` at the top of
`binding.py`: every submodule would need an explicit import list, and
the few names that `adaformat` re-exports indirectly would have to be
chased down.

## 2. Replace the wildcard `from adaformat import *`

Today, `binding.py` does `from adaformat import *`. This makes static
analysis and tooling weaker than it could be (IDE jump-to-definition,
unused-name detection, mypy / pyright). A possible plan:

1. Add `__all__` to `adaformat.py`, listing only the names actually
   meant to be public.
2. Switch `binding.py` to an explicit import list. Use `make generate`
   diff-clean as the acceptance test.

## 3. Replace `%`-style code generation with f-strings + a small DSL

The generator builds Ada source by interpolating `%(name)s` into
multi-line string templates. This works but has two costs:

- The substitution dict (`self._subst`) is filled out far from the
  templates that consume it; it is hard to know which keys a template
  needs without reading both ends.
- Whitespace inside the templates is load-bearing for the generated
  Ada layout, so the templates are awkward to edit.

A small generator-side helper that takes a typed `dataclass` of the
substitution context, plus an explicit-keyword `.format()`-like API,
would make the code less error-prone. Alternatively, migrating to
Jinja2 would be heavier but would let us keep templates in their own
files.

## 4. Move handwritten Ada bodies out of TOML

A non-trivial number of `[[Pkg.method]]` entries embed multi-line Ada
code in TOML `body = '''...'''` strings. These bodies are:

- Hard to syntax-highlight.
- Untestable except by running the generator and then building the
  output.
- Easy to drift out of sync with the generated subprogram profile.

We could either:

- Move large bodies into `*.ads`/`*.adb` snippet files under
  `contrib/binding/bodies/` and reference them by path from the TOML.
- Or move them entirely into the generated package's `extra.spec` /
  `extra.body` sections, treating the binding as
  "generate-the-spec, hand-write-the-body" for these few methods.

## 5. Reduce reliance on module-level globals

`binding.py` carries two module-level globals (`gir`, `gtkada`) that
are read from inside class methods (`record_binding`,
`constants_binding`, `enumeration_binding`, `_implements`,
`get_enumeration_values`). After the cleanup pass, `main()` still
assigns them. Threading `gir` and `gtkada` through `GIRClass` (as
constructor arguments and stored attributes) would eliminate the
globals and make the class testable in isolation.

## 6. Type-annotate the public surface

`binding.py` predates the widespread use of Python type hints. Adding
annotations to the public methods of `GIR`, `GIRClass`,
`SubprogramProfile`, `GlobalsBinder` (and to the `GtkAda*` classes in
`binding_gtkada.py`) would let `mypy` catch entire categories of
errors that today only surface as broken generation. The TOML data
shapes are well known — they could become `TypedDict`s.

## 7. Add a regression harness around the generator

Today the only acceptance test is "rerun `make generate` and diff
`src/generated/`". That works because the GIR inputs are vendored,
but it is slow (it regenerates ~500 Ada files) and fragile (any
whitespace change in a template is a failure). A faster harness
could:

- Pick a handful of representative widgets (`GtkButton`, `GtkEntry`,
  `GtkTreeView`, one `Gio` opaque type, one `GBoxed`).
- Snapshot the generated `*.ads` / `*.adb` for each.
- Run the snapshot test in CI on every PR that touches `contrib/`.

This would catch generator regressions without forcing the full
~270 000-line `tmp.ada` to be regenerated each time.

## 8. Inline the GIR XML namespace constants

The block of `nfoo = QName(uri, "foo").text` declarations at the top
of `binding.py` is opaque on first read. A small dataclass or enum
holding the resolved tags would document the namespaces clearly and
make the call sites self-documenting (e.g. `Tags.method` rather than
the bare `nmethod`).

## 9. Address the two known issues in the file header

The header comment at the top of `binding.py` records two known
issues that are still accurate:

- Missing handling of `<field>` nodes (see `GtkArrow` for instance).
- Some GIR docs contain xrefs like `#GtkMisc` which we don't rewrite
  to Ada cross-references in comments.

Both are localised changes (in `_fields` and `_get_clean_doc`
respectively) that could be tackled independently of the larger
refactorings above.
