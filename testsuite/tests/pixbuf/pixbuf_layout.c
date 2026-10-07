/* Expose the C layout for the Ada Test_Format_Layout checks. */
#define GDK_PIXBUF_ENABLE_BACKEND
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <stddef.h>

gsize
pixbuf_format_layout (guint field)
{
  /* Keep these indices in sync with Fields in pixbuf.adb. */
  static const gsize layout[] = {
    sizeof (GdkPixbufFormat),
    offsetof (GdkPixbufFormat, name),
    offsetof (GdkPixbufFormat, signature),
    offsetof (GdkPixbufFormat, domain),
    offsetof (GdkPixbufFormat, description),
    offsetof (GdkPixbufFormat, mime_types),
    offsetof (GdkPixbufFormat, extensions),
    offsetof (GdkPixbufFormat, flags),
    offsetof (GdkPixbufFormat, disabled),
    offsetof (GdkPixbufFormat, license)
  };

  return field < G_N_ELEMENTS (layout) ? layout[field] : 0;
}
