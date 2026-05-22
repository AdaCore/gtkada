# TOML override schema for the GtkAda binding generator

The GtkAda bindings under `src/generated/` are produced by
`contrib/binding.py`, which merges two sources of truth:

* the `.gir` files under `contrib/` (one per GObject-introspection
  namespace: GLib, GObject, Gtk, Gdk, Pango, Gio), which describe what
  the C library exposes; and
* per-package `.toml` override files under
  [`contrib/binding/packages/`](packages/), which describe what the
  Ada surface should look like.

This document is the reference for those `.toml` files. For an
architectural overview of the generator pipeline see the module
docstring at the top of [`contrib/binding.py`](binding.py).

## Pipeline at a glance

```
contrib/*.gir            ┐                ┌─► src/generated/tmp.ada ──gnatchop──► src/generated/*.ad{s,b}
contrib/binding/packages ┴── binding.py ──┤
                                          └─► src/misc_generated.c
```

To regenerate the bindings:

```sh
make generate
```

That target erases `src/generated/*.ad?`, runs `binding.py` over the
GIR files and the TOML directory, then runs `gnatchop` to split the
concatenated `tmp.ada` into one spec and one body per Ada package. The
companion C file holds the small amount of glue needed to wire Ada
virtual-method handlers into the GObject vtables.

## Enabling a type for binding

The set of types that participate in code generation lives in
[`contrib/data.py`](data.py):

* `interfaces` — GIR names of `<interface>` nodes; each gets its own
  Ada package.
* `binding` — C type names of `<class>`, `<record>`, `<union>` and
  boxed types.

A name prefixed with `--` is *not* bound — strip the dashes to opt
in (or add a new entry for a type that is not yet listed). Once a
name appears in one of these lists `binding.py` emits a default Ada
package; you can leave it at that, or refine the surface with a
per-package `.toml`.

## Adding or refining a binding

A package file describes how to bind one Ada package: it picks the
Ada name, overrides the parent type, renames or hides methods,
declares extra Ada types and constants to inject into the package,
and so on. The **filename stem is the C type name** that anchors the
package (e.g. `GtkButton.toml` binds `GtkButton`, `GIO.toml` binds
`GIO`). All keys in the file therefore live at the top level — there
is no wrapping table.

```toml
# GtkButton.toml — the filename gives the anchoring C type.
[doc]
screenshot = "gtk-button"
group      = "Buttons and Toggles"

[[method]]
id  = "gtk_button_new_with_label"
ada = "Gtk_New"
```

Typical workflow:

1. Make sure the GIR name (for interfaces) or C type (for everything
   else) is enabled in `contrib/data.py`.
2. Create `contrib/binding/packages/<CType>.toml`. Start empty — the
   generator already does a reasonable job by default.
3. Run `make generate` and inspect `src/generated/<package>.ads`.
4. For each problem in the generated output, add an entry to the
   TOML and regenerate. The common cases are below.

### Recognising what needs an override

When `binding.py` cannot map a GIR construct it falls back to a
synthetic identifier rather than failing. Those identifiers are the
flags pointing at the spot that needs an override:

| Symptom in the generated `.ads`                          | Likely fix                                              |
|----------------------------------------------------------|---------------------------------------------------------|
| `array_of_<Type>` references with no matching definition | declare the array with [`[[extra.type]]`](#extratype--map-a-c-type-to-an-ada-type), and the element type with [`[[record]]`](#record) if missing |
| Reference to an unknown record/struct type               | bind it with [`[[record]]`](#record)                    |
| Reference to an unknown enum/bitfield                    | bind it with [`[[enum]]`](#enum)                        |
| A method using a callback type that does not exist       | inject the access-to-subprogram via [`[[extra.spec]]`](#extraspec--code-injected-into-the-spec) and reference it from a [`[[method.parameter]]`](#methodparameter--per-parameter-overrides) `type` override |
| A method that cannot be expressed in Ada at all          | suppress it with `bind = false` and re-expose it through `[extra]` |

When in doubt, look for a similar pattern in an existing TOML — many
of the recipes you will need are already present in
[`contrib/binding/packages/`](packages/). The legacy gtk3 overrides
under [`packages/gtk3/`](packages/gtk3/) are *not* loaded by the
generator but are kept around as a quarry: when a widget that
existed in gtk3 needs the same adjustment in gtk4, copy the
relevant fragments over and adapt them to the current schema (in
particular, the top-level wrapping table used in gtk3 TOMLs is no
longer required).

> **Bind cleanly to what gtk4 exposes.** When a gtk3 widget has been
> reshaped or folded into another in gtk4 (for example `GtkHPaned` and
> `GtkVPaned`, which gtk4 replaces with an orientation parameter on
> `GtkPaned`), bind only the gtk4 surface. Do not add Ada
> subtype/constructor shims to preserve the gtk3 Ada API. Source-level
> compatibility with gtk3 is a non-goal.

Not every problem shows up in the generated `.ads`; some only surface
at build time. One recurring case:

| Symptom at build time                              | Likely fix                                                                                                                       |
|----------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------|
| `"Unchecked_To_<Enum>" is undefined`               | The enum is used as a signal-callback parameter and needs an `Unsafe_Enum_Nth` instantiation in the hand-maintained `src/gtk-arguments.ads`. |

## Conventions used below

* A name in **bold** is a top-level key in the file (e.g. `bindtype`).
* A `[<table>]` heading is a *table*, expected at most once.
* A `[[<table>]]` heading is an *array of tables*, repeatable.
* "Default" is what the generator does when the key is missing.

## Table of contents

1. [Pipeline at a glance](#pipeline-at-a-glance)
2. [Enabling a type for binding](#enabling-a-type-for-binding)
3. [Adding or refining a binding](#adding-or-refining-a-binding)
4. [Package-level attributes](#package-level-attributes)
5. [`[doc]` — package documentation](#doc--package-documentation)
6. [`[[parameter]]` — default parameter overrides](#parameter--default-parameter-overrides)
7. [`[[method]]` / `[[function]]` / `[[callback]]` / `[[virtual_method]]`](#method--function--callback--virtual_method--subprogram-overrides)
8. [`[[method.parameter]]` — per-parameter overrides](#methodparameter--per-parameter-overrides)
9. [`[method.doc]` — per-method documentation](#methoddoc--per-method-documentation)
10. [Type declarations injected into the package](#type-declarations-injected-into-the-package)
11. [`[extra]` — verbatim Ada/C injections](#extra--verbatim-adac-injections)

## Package-level attributes

These keys sit at the top level of the file.

| Key            | Type   | Meaning                                                                                                       | Default |
|----------------|--------|---------------------------------------------------------------------------------------------------------------|---------|
| `bindtype`        | bool   | When `false`, no Ada type is emitted; only the methods are bound (useful for namespace-only packages).        | `true`  |
| `ada_access_root` | bool | When `true`, emit subtype aiasing access type in namepsace package instead of declaring a new access type in the class package. | `false` |
| `into`            | string | C type of another package; the types and methods of *this* package are merged into the named package.         | unset   |
| `ada`             | string | Override the Ada package name.                                                                                | derived from the C type |
| `parent`          | string | Override the parent type of the widget type emitted by this package (GIR name).                               | from GIR |
| `obsolescent`     | bool   | When `true`, the generated package gets `pragma Obsolescent`.                                                 | `false` |

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
| `gtkada_demo`| string | Generates a `<gtkada_demo>...</gtkada_demo>` tag (link to a gtkada_demo example).  |
| `see`        | string | Generates a `<see>...</see>` cross-reference.                          |
| `text`       | string | Free-form text. Use `'''...'''` for multi-line paragraphs.             |

```toml
[doc]
screenshot  = "gtk-button"
group       = "Buttons and Toggles"
gtkada_demo = "create_buttons.adb"
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

* `[[method]]` — match a `<method>`.
* `[[function]]` — match a namespace-level `<function>` and add it
  to the current Ada package.
* `[[callback]]` — match a `<callback>` referenced as a parameter.
* `[[virtual_method]]` — match a `<virtual-method>`. For
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
