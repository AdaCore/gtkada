
#include <gtk/gtk.h>
#include <gtkextra/gtkplot.h>
#include <gtkextra/gtkplotcanvas.h>

/********************************************************************
 **
 **  Gtk_Plot widget
 **
 ********************************************************************/

gint
ada_gtk_extra_plot_flag_is_set (GtkPlot* plot, guint8  flag) {
  return ((GTK_PLOT_FLAGS (plot) & flag) != 0);
}

void
ada_gtk_extra_plot_set_flags (GtkPlot* plot, guint8 flags) {
  GTK_PLOT_SET_FLAGS (plot, flags);
}

void
ada_gtk_extra_plot_unset_flags (GtkPlot* plot, guint8 flags) {
  GTK_PLOT_UNSET_FLAGS (plot, flags);
}

char*
ada_gtk_dataset_get_name (GtkPlotData* data) {
  return data->name;
}

GList*
ada_gtk_plot_get_datasets (GtkPlot* plot) {
  return plot->data_sets;
}

GList*
ada_gtk_plot_get_texts (GtkPlot* plot) {
  return plot->text;
}

gchar*
ada_gtk_plot_get_text_string (GtkPlotText* text) {
  return text->text;
}

void
ada_gtk_plot_get_text_position (GtkPlotText* text,
				gdouble* x,
				gdouble* y) {
  *x = text->x;
  *y = text->y;
}

/********************************************************************
 **
 **  Gtk_Plot_Canvas widget
 **
 ********************************************************************/

guint
ada_gtk_plot_canvas_flag_is_set (GtkPlotCanvas* canvas, guint16 flag) {
  return ((GTK_PLOT_CANVAS_FLAGS (canvas) & flag) != 0);
}

void
ada_gtk_plot_canvas_set_flags (GtkPlotCanvas* canvas, guint16 flags) {
  GTK_PLOT_CANVAS_SET_FLAGS (canvas, flags);
}

void
ada_gtk_plot_canvas_unset_flags (GtkPlotCanvas* canvas, guint16 flags) {
  GTK_PLOT_CANVAS_UNSET_FLAGS (canvas, flags);
}
