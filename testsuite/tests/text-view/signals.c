/*
 * Emits representative GtkTextView signals so the Ada callback marshallers
 * are exercised through the real GObject signal system.
 */

#include <gtk/gtk.h>

void
gtkada_test_emit_text_view_signals (GtkTextView *view)
{
  GtkTextBuffer *buffer = gtk_text_view_get_buffer (view);
  GtkTextIter start, end, cursor;
  gboolean extend_handled = FALSE;

  gtk_text_buffer_get_bounds (buffer, &start, &end);
  cursor = start;

  g_signal_emit_by_name (view, "backspace");
  g_signal_emit_by_name (view, "delete-from-cursor", GTK_DELETE_CHARS, 0);
  g_signal_emit_by_name (view, "extend-selection",
                         GTK_TEXT_EXTEND_SELECTION_WORD,
                         &cursor, &start, &end, &extend_handled);
  g_signal_emit_by_name (view, "insert-at-cursor", "x");
  g_signal_emit_by_name (view, "move-cursor",
                         GTK_MOVEMENT_LOGICAL_POSITIONS, 1, FALSE);
  g_signal_emit_by_name (view, "move-viewport", GTK_SCROLL_STEPS, 1);
  g_signal_emit_by_name (view, "select-all", TRUE);
}
