
#include <gtk/gtk.h>
#include <gtkextra/gtkplot.h>

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

char*
ada_gtk_dataset_get_name (GtkPlotData* data) {
  return data->name;
}
