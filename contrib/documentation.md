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
The **filename stem is the C type name** that anchors the package
(e.g. `GtkButton.toml` binds `GtkButton`, `GIO.toml` binds `GIO`). All
keys in the file therefore live at the top level — there is no
wrapping table.

```toml
# GtkButton.toml — the filename gives the anchoring C type.
[doc]
  screenshot = "gtk-button"
  group      = "Buttons and Toggles"

[[method]]
  id  = "gtk_button_new_with_label"
  ada = "Gtk_New"
```

## Conventions used below

- A name in **bold** is a top-level key in the file (e.g. `bindtype`).
- A `[<table>]` heading is a *table*, expected at most once.
- A `[[<table>]]` heading is an *array of tables*, repeatable.
- "Default" is what the generator does when the key is missing.

## Table of contents

1. [Package-level attributes](#package-level-attributes)
2. [`[doc]` — package documentation](#doc--package-documentation)
3. [`[[parameter]]` — default parameter overrides](#parameter--default-parameter-overrides)
4. [`[[method]]` / `[[function]]` / `[[callback]]` / `[[virtual_method]]`](#method--function--callback--virtual_method--subprogram-overrides)
5. [`[[method.parameter]]` — per-parameter overrides](#methodparameter--per-parameter-overrides)
6. [`[method.doc]` — per-method documentation](#methoddoc--per-method-documentation)
7. [Type declarations injected into the package](#type-declarations-injected-into-the-package)
8. [`[extra]` — verbatim Ada/C injections](#extra--verbatim-adac-injections)

## Package-level attributes

These keys sit at the top level of the file.

| Key            | Type   | Meaning                                                                                                       | Default |
|----------------|--------|---------------------------------------------------------------------------------------------------------------|---------|
| `bindtype`     | bool   | When `false`, no Ada type is emitted; only the methods are bound (useful for namespace-only packages).        | `true`  |
| `into`         | string | C type of another package; the types and methods of *this* package are merged into the named package.         | unset   |
| `ada`          | string | Override the Ada package name.                                                                                | derived from the C type |
| `parent`       | string | Override the parent type of the widget type emitted by this package (GIR name).                               | from GIR |
| `obsolescent`  | bool   | When `true`, the generated package gets `pragma Obsolescent`.                                                 | `false` |

```toml
# GtkHButtonBox.toml
into        = "GtkButtonBox"
ada         = "Gtk.Hbutton_Box"
parent      = "GtkButtonBox"
obsolescent = true
```

## `[doc]` — package documentation

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
[doc]
screenshot = "gtk-button"
group      = "Buttons and Toggles"
testgtk    = "create_buttons.adb"
```

## `[[parameter]]` — default parameter overrides

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
[[parameter]]
name = "self"
ada  = "Button"
```

## `[[method]]` / `[[function]]` / `[[callback]]` / `[[virtual_method]]` — subprogram overrides

These four arrays use **the same schema**; they only differ in which
GIR node is matched.

- `[[method]]` — match a `<method>`.
- `[[function]]` — match a namespace-level `<function>` and add it
  to the current Ada package.
- `[[callback]]` — match a `<callback>` referenced as a parameter.
- `[[virtual_method]]` — match a `<virtual-method>`. For
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
[[method]]
id     = "gtk_button_new_with_label"
ada    = "Gtk_New"

[[method]]
id   = "gtk_button_new"
bind = false
```

### `[[method.parameter]]` — per-parameter overrides

Repeat as needed *inside* a method/function/callback/virtual_method
entry, in addition to the package-level defaults from
`[[parameter]]`.

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
[[method.parameter]]
name       = "label"
default    = "\"\""
allow_none = "1"
```

### `[method.doc]` — per-method documentation

A single table per method.

| Key      | Type   | Meaning                                                                          |
|----------|--------|----------------------------------------------------------------------------------|
| `extend` | bool   | When `true`, append `text` to the documentation extracted from the GIR file.    |
| `text`   | string | Documentation paragraphs. `\n` forces a newline, blank lines start new paragraphs. |

## Type declarations injected into the package

These arrays let a TOML file declare additional Ada types (enums,
records, constants, lists) inside the generated package and register
the relevant C-to-Ada naming mappings with the binder.

### `[[enum]]`

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
# GtkEnums.toml
[[enum]]
ctype = "GtkIconSize"
asbitfield = true
```

### `[[constant]]`

Bind every namespace `<constant>` whose C name matches `prefix_regexp`,
stripping `prefix` to compute the Ada identifier.

| Key             | Type   | Meaning                                              |
|-----------------|--------|------------------------------------------------------|
| `prefix_regexp` | string | Python regex matching the C name.                    |
| `prefix`        | string | Stripped from the C name to compute the Ada name.    |

### `[[record]]`

Bind a `<record>` (or `<union>`) as an Ada record type.

| Key       | Type   | Meaning                                                       | Default |
|-----------|--------|---------------------------------------------------------------|---------|
| `ctype`   | string | **Required.** C type to bind.                                 |         |
| `ada`     | string | Override the Ada record name.                                 | derived |
| `private` | bool   | Emit the record as a private type.                            | `false` |

#### `[[record.field]]` — per-field type override

| Key     | Type   | Meaning                                              |
|---------|--------|------------------------------------------------------|
| `name`  | string | **Required.** Field name as it appears in the GIR.   |
| `ctype` | string | Override the C type for the field.                   |

#### `[[record.union]]` — union discriminant mapping

One entry per value of the discriminant, mapping it to the corresponding
field.

| Key     | Type   | Meaning                                           |
|---------|--------|---------------------------------------------------|
| `value` | string | C enumerator value.                               |
| `field` | string | Name of the field that this value selects.        |

### `[[list]]` and `[[slist]]`

Instantiate `Glib.Glist.Generic_List` (resp. `Glib.GSlist.Generic_SList`)
inside the package, registering a new C type that the rest of the file
can use as a parameter type.

| Key       | Type   | Meaning                                                                                  |
|-----------|--------|------------------------------------------------------------------------------------------|
| `ada`     | string | Ada name for the list type.                                                              |
| `ctype`   | string | C type of the element contained in the list (a `List` / `SList` suffix is appended).      |
| `section` | string | Optional section name of the generated package where the instantiation should be emitted. |

### `[[type]]`

Declare a hand-written Ada type or subtype.

| Key       | Type   | Meaning                                       |
|-----------|--------|-----------------------------------------------|
| `name`    | string | **Required.** Ada name of the type.           |
| `subtype` | bool   | When `true`, emit a `subtype` declaration.    |

## `[extra]` — verbatim Ada/C injections

This is the escape hatch for code that the generator cannot derive
from GIR. Everything declared here is injected into the generated
package verbatim.

| Key    | Type   | Meaning                                                                                                  |
|--------|--------|----------------------------------------------------------------------------------------------------------|
| `body` | string | Code inserted into the package body. Inserted **before** the generated subprograms by default.            |

### `[[extra.with_spec]]` — extra `with` clauses in the spec

| Key   | Type   | Meaning                                  | Default |
|-------|--------|------------------------------------------|---------|
| `pkg` | string | Package to `with`.                       |         |
| `use` | bool   | Whether to emit a `use` clause too.       | `true`  |

### `[[extra.with_body]]` — extra `with` clauses in the body

Same keys as `with_spec`.

### `[[extra.spec]]` — code injected into the spec

| Key       | Type   | Meaning                                          | Default |
|-----------|--------|--------------------------------------------------|---------|
| `text`    | string | Code to insert.                                  |         |
| `private` | bool   | Append to the private part rather than visible. | `false` |

### `[[extra.type]]` — map a C type to an Ada type

Lets you declare ad-hoc Ada types that the generator can refer to as
return / parameter / field types. The mapping is also registered in
the naming table.

| Key     | Type   | Meaning                                                                |
|---------|--------|------------------------------------------------------------------------|
| `ctype` | string | **Required.** C type name.                                             |
| `ada`   | string | Ada type name.                                                          |
| `text`  | string | Optional Ada type declaration, placed after generated types but before subprograms. |

### `[[extra.gir_element]]` — splice raw GIR XML

| Key   | Type   | Meaning                                                                       |
|-------|--------|-------------------------------------------------------------------------------|
| `xml` | string | XML element matching what would have appeared in the GIR file. Used to add a method or other GIR construct that the actual .gir does not expose. |

```toml
# GtkEnums.toml
[extra]
body = '''
    function Convert (S : String) return System.Address is
    begin
       ...
    end Convert;
  '''

[[extra.with_spec]]
pkg = "Glib.Glist"

[[extra.with_body]]
pkg = "Ada.Unchecked_Conversion"
use = false

[[extra.spec]]
text = '''
      function Convert (S : String) return System.Address;
    '''
```
