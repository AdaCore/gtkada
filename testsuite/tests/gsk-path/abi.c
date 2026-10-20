#include <gtk/gtk.h>
#include <stddef.h>
size_t gtkada_test_path_point_size (void) { return sizeof (GskPathPoint); }
size_t gtkada_test_path_point_alignment (void) { return _Alignof (GskPathPoint); }
