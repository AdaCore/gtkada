
#include <gtk/gtk.h>

#include "gtkextra/gtkpsfont.h"
#include "gtkextra/gtkextracombobox.h"
#include "gtkextra/gtkfontcombo.h"
#include "gtkextra/gtkcolorcombo.h"
#include "gtkextra/gtksheet.h"
#include "gtkextra/gtkplot.h"
#include "gtkextra/gtkplotcanvas.h"

/********************************************************************
 **
 **  Gtk_Plot widget
 **
 ********************************************************************/

void
ada_gtk_plot_set_color (GtkPlotLine* line, GdkColor* color) {
  line->color = *color;
}

void
ada_gtk_plot_set_line_style (GtkPlotLine* line, GtkPlotLineStyle style) {
  line->line_style = style;
}

void
ada_gtk_plot_set_line_width (GtkPlotLine* line, gfloat width) {
  line->line_width = width;
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

GtkPlotCanvasType
ada_gtk_plot_canvas_get_item_type (GtkPlotCanvasChild * item) {
  return item->type;
}

void
ada_gtk_plot_canvas_set_draw_func
(GtkPlotCanvasChild* item,
 void (*draw) (GtkPlotCanvas *canvas, GtkPlotCanvasChild *child))
{
  item->draw_child = draw;
}

guint
ada_gtk_plot_canvas_get_alloc_width (GtkPlotCanvasChild* child) {
  return child->allocation.width;
}

guint
ada_gtk_plot_canvas_get_alloc_height (GtkPlotCanvasChild* child) {
  return child->allocation.height;
}

gint
ada_gtk_plot_canvas_get_alloc_x (GtkPlotCanvasChild* child) {
  return child->allocation.x;
}

gint
ada_gtk_plot_canvas_get_alloc_y (GtkPlotCanvasChild* child) {
  return child->allocation.x;
}

GdkPixmap*
ada_gtk_plot_canvas_get_pixmap (GtkPlotCanvas* canvas) {
  return canvas->pixmap;
}

GtkPlotCanvasFlag
ada_gtk_plot_canvas_get_child_flags (GtkPlotCanvasChild* child) {
  return child->flags;
}

void
ada_gtk_plot_canvas_set_child_flags (GtkPlotCanvasChild* child,
				     GtkPlotCanvasFlag flags) {
  child->flags = flags;
}

/********************************************************************
 **
 **  Gtk_Sheet widget
 **
 ********************************************************************/

GtkWidget*
ada_gtk_sheet_get_widget (GtkSheetChild* child) {
  return child->widget;
}

GtkSheetRange*
ada_gtk_sheet_get_range (GtkSheet* sheet) {
  return &(sheet->range);
}

gint
ada_gtk_sheet_get_column_width (GtkSheet* sheet, gint col) {
  return sheet->column[col].width;
}

gint
ada_gtk_sheet_get_row_height (GtkSheet* sheet, gint row) {
  return sheet->row[row].height;
}

/********************************************************************
 **
 **  Gtk_Extra_Combo_Box widget
 **
 ********************************************************************/

GtkWidget*
ada_gtk_extra_combo_box_get_button (GtkExtraComboBox* combo) {
  return combo->button;
}

GtkWidget*
ada_gtk_extra_combo_box_get_arrow (GtkExtraComboBox* combo) {
  return combo->arrow;
}

GtkWidget*
ada_gtk_extra_combo_box_get_frame (GtkExtraComboBox* combo) {
  return combo->frame;
}

/********************************************************************
 **
 **  Gtk_Color_Combo widget
 **
 ********************************************************************/

gint ada_gtk_extra_color_combo_get_ncols (GtkColorCombo* combo) {
  return combo->ncols;
}

gint ada_gtk_extra_color_combo_get_nrows (GtkColorCombo* combo) {
  return combo->nrows;
}

void ada_gtk_extra_color_combo_set_row (GtkColorCombo* combo, gint row) {
  combo->row = row;
}

void ada_gtk_extra_color_combo_set_column (GtkColorCombo* combo, gint col) {
  combo->column = col;
}

/********************************************************************
 **
 **  PsFont
 **
 ********************************************************************/

char*
ada_gtk_psfont_get_psname (GtkPSFont* font) {
  return font->psname;
}

