# TOML override schema for the GtkAda binding generator

This document is the reference for the per-package `.toml` files under
[`contrib/binding/packages/`](packages/). Those files are loaded by
[`contrib/binding_gtkada.py`](../binding_gtkada.py) and consumed by the
generator in [`contrib/binding.py`](../binding.py). For an architectural
overview of the generator pipeline see the module docstring at the top
of `binding.py`.

A package file describes how to bind one Ada package: it picks the Ada
name, overrides the parent type, renames or hides methods, declares
extra Ada types and constants to inject into the package, and so on.
The on-disk filename does not have to match the package id. The
top-level table key inside the file is the **C type name** that
anchors the package (e.g. `[GtkButton]`, `[GIO]`).

```toml
[GtkButton]                # the C type anchoring this package
  [GtkButton.doc]
    screenshot = "gtk-button"
    group      = "Buttons and Toggles"

  [[GtkButton.method]]
    id  = "gtk_button_new_with_label"
    ada = "Gtk_New"
```

## Conventions used below

- A name in **bold** is a key that lives directly under the
  top-level table for the package (e.g. `bindtype`).
- A `[Pkg.<table>]` heading is a *table*, expected at most once.
- A `[[Pkg.<table>]]` heading is an *array of tables*, repeatable.
- "Default" is what the generator does when the key is missing.

## Table of contents

1. [Package-level attributes](#package-level-attributes)
2. [`[Pkg.doc]` — package documentation](#pkgdoc--package-documentation)
3. [`[[Pkg.parameter]]` — default parameter overrides](#pkgparameter--default-parameter-overrides)
4. [`[[Pkg.method]]` / `[[Pkg.function]]` / `[[Pkg.callback]]` / `[[Pkg.virtual_method]]`](#pkgmethod--pkgfunction--pkgcallback--pkgvirtual_method--subprogram-overrides)
5. [`[[Pkg.method.parameter]]` — per-parameter overrides](#pkgmethodparameter--per-parameter-overrides)
6. [`[Pkg.method.doc]` — per-method documentation](#pkgmethoddoc--per-method-documentation)
7. [Type declarations injected into the package](#type-declarations-injected-into-the-package)
8. [`[Pkg.extra]` — verbatim Ada/C injections](#pkgextra--verbatim-adac-injections)

## Package-level attributes

These keys sit directly under the package table.

| Key            | Type   | Meaning                                                                                                       | Default |
|----------------|--------|---------------------------------------------------------------------------------------------------------------|---------|
| `bindtype`     | bool   | When `false`, no Ada type is emitted; only the methods are bound (useful for namespace-only packages).        | `true`  |
| `into`         | string | C type of another package; the types and methods of *this* package are merged into the named package.         | unset   |
| `ada`          | string | Override the Ada package name.                                                                                | derived from the C type |
| `parent`       | string | Override the parent type of the widget type emitted by this package (GIR name).                               | from GIR |
| `obsolescent`  | bool   | When `true`, the generated package gets `pragma Obsolescent`.                                                 | `false` |

```toml
[GtkHButtonBox]
  into       = "GtkButtonBox"
  ada        = "Gtk.Hbutton_Box"
  parent     = "GtkButtonBox"
  obsolescent = true
```

## `[Pkg.doc]` — package documentation

A single table holding the Ada-side documentation block for the
package. All keys are optional.

| Key          | Type   | Meaning                                                                |
|--------------|--------|------------------------------------------------------------------------|
| `screenshot` | string | Generates a `<screenshot>...</screenshot>` tag.                        |
| `group`      | string | Generates a `<group>...</group>` tag (used by the documentation tool). |
| `testgtk`    | string | Generates a `<testgtk>...</testgtk>` tag (link to a testgtk example).  |
| `see`        | string | Generates a `<see>...</see>` cross-reference.                          |
| `text`       | string | Free-form text. Use `'''...'''` for multi-line paragraphs.             |

```toml
[GtkButton.doc]
  screenshot = "gtk-button"
  group      = "Buttons and Toggles"
  testgtk    = "create_buttons.adb"
```

## `[[Pkg.parameter]]` — default parameter overrides

An array of *default* parameter overrides applied to every method of
the package. The most common use is to rename the implicit `self`
parameter to a human-friendly Ada name.

| Key       | Type   | Meaning                                                                                  |
|-----------|--------|------------------------------------------------------------------------------------------|
| `name`    | string | **Required.** Lower-cased name of the parameter (or `varargs`).                          |
| `ada`     | string | Ada name to use everywhere this parameter appears.                                       |
| `type`    | string | Override the Ada type.                                                                   |
| `ctype`   | string | Override the C type used in the `pragma Import` wrapper.                                 |
| `direction` | string | One of `"in"`, `"out"`, `"access"`, `"inout"`.                                         |

```toml
[[GtkButton.parameter]]
  name = "self"
  ada  = "Button"
```

## `[[Pkg.method]]` / `[[Pkg.function]]` / `[[Pkg.callback]]` / `[[Pkg.virtual_method]]` — subprogram overrides

These four arrays use **the same schema**; they only differ in which
GIR node is matched.

- `[[Pkg.method]]` — match a `<method>`.
- `[[Pkg.function]]` — match a namespace-level `<function>` and add it
  to the current Ada package.
- `[[Pkg.callback]]` — match a `<callback>` referenced as a parameter.
- `[[Pkg.virtual_method]]` — match a `<virtual-method>`. For
  interfaces every virtual method is bound by default; for other
  classes none is bound. Use `bind = false` (or `true`) to override.

| Key                  | Type   | Meaning                                                                                                                                                    |
|----------------------|--------|------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `id`                 | string | **Required.** The C identifier of the method. For signals use `"::signal-name"`. For fields (which are not bound by default) use `gtkada_<ctype>_get_<field>` and `gtkada_<ctype>_set_<field>`. |
| `ada`                | string | Override the Ada subprogram name.                                                                                                                          |
| `bind`               | bool   | When `false`, suppress the binding for this method (or, for inherited interface methods, suppress the inherited copy).                                     |
| `obsolescent`        | bool   | When `true`, the generated subprogram gets `pragma Obsolescent`.                                                                                            |
| `transfer_ownership` | string | `"full"` if the return value must be freed by the caller; `"none"` if the C library owns it.                                                                |
| `return_as_param`    | string | Replace the function's return value with an `out` parameter of this name. Used to avoid Ada functions with `out` parameters.                                |
| `return`             | string | Override the C type of the return value. Use `"void"` to turn a function into a procedure.                                                                  |
| `classwide`          | bool   | When `true`, declare the subprogram class-wide rather than as a primitive operation.                                                                        |
| `body`               | string | Hand-written body inserted after the `is` keyword (use `'''...'''`). Use `%(auto)s` inside the string to splice the automatic body in.                       |
| `convention`         | string | Override the calling convention (e.g. `"C"`).                                                                                                              |

```toml
[[GtkButton.method]]
  id     = "gtk_button_new_with_label"
  ada    = "Gtk_New"

[[GtkButton.method]]
  id   = "gtk_button_new"
  bind = false
```

### `[[Pkg.method.parameter]]` — per-parameter overrides

Repeat as needed *inside* a method/function/callback/virtual_method
entry, in addition to the package-level defaults from
`[[Pkg.parameter]]`.

| Key          | Type   | Meaning                                                                                                                                |
|--------------|--------|----------------------------------------------------------------------------------------------------------------------------------------|
| `name`       | string | **Required.** Lower-cased GIR name (use `varargs` to replace the varargs parameter).                                                  |
| `ada`        | string | Override the Ada name. An empty string means *omit* the parameter from the Ada profile (it is still passed to C, with its default).   |
| `type`       | string | Override the Ada type. The value is passed to C unchanged unless it is `Glib.Object.GObject`, in which case it is treated as a GObject. |
| `ctype`      | string | Override the C type.                                                                                                                  |
| `default`    | string | Default value (Ada syntax).                                                                                                            |
| `direction`  | string | `"in"`, `"out"`, `"access"` or `"inout"`.                                                                                              |
| `allow_none` | string | `"1"` if the C side accepts a NULL value; an empty Ada string is then mapped to the null pointer.                                     |
| `caller_allocates` | string | `"1"` if the caller is responsible for allocating the value.                                                                    |
| `transfer_ownership` | string | `"full"` if the callee takes ownership.                                                                                       |

```toml
[[GtkButton.method.parameter]]
  name       = "label"
  default    = "\"\""
  allow_none = "1"
```

### `[Pkg.method.doc]` — per-method documentation

A single table per method.

| Key      | Type   | Meaning                                                                          |
|----------|--------|----------------------------------------------------------------------------------|
| `extend` | bool   | When `true`, append `text` to the documentation extracted from the GIR file.    |
| `text`   | string | Documentation paragraphs. `\n` forces a newline, blank lines start new paragraphs. |

## Type declarations injected into the package

These arrays let a TOML file declare additional Ada types (enums,
records, constants, lists) inside the generated package and register
the relevant C-to-Ada naming mappings with the binder.

### `[[Pkg.enum]]`

Bind a `<enumeration>` or `<bitfield>` as an Ada type inside this
package. The generator also auto-derives naming exceptions for the
members (overrideable in `data.cname_to_adaname`).

| Key          | Type   | Meaning                                                              | Default |
|--------------|--------|----------------------------------------------------------------------|---------|
| `ctype`      | string | **Required.** C type to bind.                                        |         |
| `ada`        | string | Override the Ada type name (no package qualifier needed).            | derived |
| `prefix`     | string | Stripped from each value to compute its Ada identifier.              | `"GTK_"` |
| `asbitfield` | bool   | Force a bitfield representation (modular type) even if GIR says enum. | `false` |
| `ignore`     | string | Space-separated list of values that must not be bound.               | `""` |

```toml
[[GtkEnums.enum]]
  ctype = "GtkIconSize"
  asbitfield = true
```

### `[[Pkg.constant]]`

Bind every namespace `<constant>` whose C name matches `prefix_regexp`,
stripping `prefix` to compute the Ada identifier.

| Key             | Type   | Meaning                                              |
|-----------------|--------|------------------------------------------------------|
| `prefix_regexp` | string | Python regex matching the C name.                    |
| `prefix`        | string | Stripped from the C name to compute the Ada name.    |

### `[[Pkg.record]]`

Bind a `<record>` (or `<union>`) as an Ada record type.

| Key       | Type   | Meaning                                                       | Default |
|-----------|--------|---------------------------------------------------------------|---------|
| `ctype`   | string | **Required.** C type to bind.                                 |         |
| `ada`     | string | Override the Ada record name.                                 | derived |
| `private` | bool   | Emit the record as a private type.                            | `false` |

#### `[[Pkg.record.field]]` — per-field type override

| Key     | Type   | Meaning                                              |
|---------|--------|------------------------------------------------------|
| `name`  | string | **Required.** Field name as it appears in the GIR.   |
| `ctype` | string | Override the C type for the field.                   |

#### `[[Pkg.record.union]]` — union discriminant mapping

One entry per value of the discriminant, mapping it to the corresponding
field.

| Key     | Type   | Meaning                                           |
|---------|--------|---------------------------------------------------|
| `value` | string | C enumerator value.                               |
| `field` | string | Name of the field that this value selects.        |

### `[[Pkg.list]]` and `[[Pkg.slist]]`

Instantiate `Glib.Glist.Generic_List` (resp. `Glib.GSlist.Generic_SList`)
inside the package, registering a new C type that the rest of the file
can use as a parameter type.

| Key       | Type   | Meaning                                                                                  |
|-----------|--------|------------------------------------------------------------------------------------------|
| `ada`     | string | Ada name for the list type.                                                              |
| `ctype`   | string | C type of the element contained in the list (a `List` / `SList` suffix is appended).      |
| `section` | string | Optional section name of the generated package where the instantiation should be emitted. |

### `[[Pkg.type]]`

Declare a hand-written Ada type or subtype.

| Key       | Type   | Meaning                                       |
|-----------|--------|-----------------------------------------------|
| `name`    | string | **Required.** Ada name of the type.           |
| `subtype` | bool   | When `true`, emit a `subtype` declaration.    |

## `[Pkg.extra]` — verbatim Ada/C injections

This is the escape hatch for code that the generator cannot derive
from GIR. Everything declared here is injected into the generated
package verbatim.

| Key    | Type   | Meaning                                                                                                  |
|--------|--------|----------------------------------------------------------------------------------------------------------|
| `body` | string | Code inserted into the package body. Inserted **before** the generated subprograms by default.            |

### `[[Pkg.extra.with_spec]]` — extra `with` clauses in the spec

| Key   | Type   | Meaning                                  | Default |
|-------|--------|------------------------------------------|---------|
| `pkg` | string | Package to `with`.                       |         |
| `use` | bool   | Whether to emit a `use` clause too.       | `true`  |

### `[[Pkg.extra.with_body]]` — extra `with` clauses in the body

Same keys as `with_spec`.

### `[[Pkg.extra.spec]]` — code injected into the spec

| Key       | Type   | Meaning                                          | Default |
|-----------|--------|--------------------------------------------------|---------|
| `text`    | string | Code to insert.                                  |         |
| `private` | bool   | Append to the private part rather than visible. | `false` |

### `[[Pkg.extra.type]]` — map a C type to an Ada type

Lets you declare ad-hoc Ada types that the generator can refer to as
return / parameter / field types. The mapping is also registered in
the naming table.

| Key     | Type   | Meaning                                                                |
|---------|--------|------------------------------------------------------------------------|
| `ctype` | string | **Required.** C type name.                                             |
| `ada`   | string | Ada type name.                                                          |
| `text`  | string | Optional Ada type declaration, placed after generated types but before subprograms. |

### `[[Pkg.extra.gir_element]]` — splice raw GIR XML

| Key   | Type   | Meaning                                                                       |
|-------|--------|-------------------------------------------------------------------------------|
| `xml` | string | XML element matching what would have appeared in the GIR file. Used to add a method or other GIR construct that the actual .gir does not expose. |

```toml
[GtkEnums.extra]
  body = '''
    function Convert (S : String) return System.Address is
    begin
       ...
    end Convert;
  '''

  [[GtkEnums.extra.with_spec]]
    pkg = "Glib.Glist"

  [[GtkEnums.extra.with_body]]
    pkg = "Ada.Unchecked_Conversion"
    use = false

  [[GtkEnums.extra.spec]]
    text = '''
      function Convert (S : String) return System.Address;
    '''
```
