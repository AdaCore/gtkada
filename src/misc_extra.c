
#include <gtk/gtk.h>

#include "gtkextra/gtkpsfont.h"
#include "gtkextra/gtkcombobox.h"
#include "gtkextra/gtkfontcombo.h"

/* XXX ??? */
#if 0
#include "gtkextra/gtkplot.h"
#include "gtkextra/gtkplotcanvas.h"
#include "gtkextra/gtksheet.h"
#include "gtkextra/gtkcolorcombo.h"
#include "gtkextra/gtkitementry.h"

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

gint
ada_gtk_extra_sheet_flag_is_set (GtkSheet* sheet, guint16  flag) {
  return ((GTK_SHEET_FLAGS (sheet) & flag) != 0);
}

void
ada_gtk_extra_sheet_set_flags (GtkSheet* sheet, guint16 flags) {
  GTK_SHEET_SET_FLAGS (sheet, flags);
}

void
ada_gtk_extra_sheet_unset_flags (GtkSheet* sheet, guint16 flags) {
  GTK_SHEET_UNSET_FLAGS (sheet, flags);
}

GtkWidget*
ada_gtk_sheet_get_widget (GtkSheetChild* child) {
  return child->widget;
}

GtkSheetRange*
ada_gtk_sheet_get_range (GtkSheet* sheet) {
  return &(sheet->range);
}

gchar*
ada_gtk_sheet_get_column_title (GtkSheet* sheet, gint col) {
  return sheet->column[col].name;
}

gchar*
ada_gtk_sheet_get_row_title (GtkSheet* sheet, gint row) {
  return sheet->row[row].name;
}

gint
ada_gtk_sheet_get_column_width (GtkSheet* sheet, gint col) {
  return sheet->column[col].width;
}

gint
ada_gtk_sheet_get_row_height (GtkSheet* sheet, gint row) {
  return sheet->row[row].height;
}

#endif

/********************************************************************
 **
 **  Gtk_Combo_Box widget
 **
 ********************************************************************/

GtkWidget*
ada_gtk_combo_box_get_button (GtkComboBox* combo) {
  return combo->button;
}

GtkWidget*
ada_gtk_combo_box_get_arrow (GtkComboBox* combo) {
  return combo->arrow;
}

GtkWidget*
ada_gtk_combo_box_get_frame (GtkComboBox* combo) {
  return combo->frame;
}

/********************************************************************
 **
 **  Gtk_Font_Combo widget
 **
 ********************************************************************/

GtkWidget*
ada_gtk_font_combo_get_name_combo (GtkFontCombo* combo) {
  return combo->name_combo;
}

GtkWidget*
ada_gtk_font_combo_get_size_combo (GtkFontCombo* combo) {
  return combo->size_combo;
}

GtkWidget*
ada_gtk_font_combo_get_bold_button (GtkFontCombo* combo) {
  return combo->bold_button;
}

GtkWidget*
ada_gtk_font_combo_get_italic_button (GtkFontCombo* combo) {
  return combo->italic_button;
}

GdkFont*
ada_gtk_font_combo_get_font (GtkFontCombo* combo) {
  return combo->font;
}

// ???
#if 0

/********************************************************************
 **
 **  Gtk_Color_Combo widget
 **
 ********************************************************************/

gint ada_gtk_extra_color_combo_get_column (GtkColorCombo* combo) {
  return combo->column;
}

gint ada_gtk_extra_color_combo_get_row (GtkColorCombo* combo) {
  return combo->row;
}

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
 **  Gtk_Item_Entry widget
 **
 ********************************************************************/

gint
ada_gtk_item_entry_get_justification (GtkItemEntry* item) {
  return item->justification;
}

#endif

/********************************************************************
 **
 **  PsFont
 **
 ********************************************************************/

char*
ada_gtk_psfont_get_psname (GtkPSFont* font) {
  return font->psname;
}

