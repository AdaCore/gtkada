# GdkPixbuf formats

This command-line example lists the formats advertised by the installed
GdkPixbuf loaders, including their descriptions, filename extensions, MIME types,
and read/write support. Disabled formats are included and marked as disabled.
The list depends on the loaders installed on the machine and the GdkPixbuf
loader configuration. No graphical display or GTK initialization is required.

From the repository root, build and run it with:

```sh
gprbuild -p -P examples/pixbuf-formats/pixbuf_formats.gpr -XLIBRARY_TYPE=static
./examples/pixbuf-formats/bin/pixbuf_formats
```
