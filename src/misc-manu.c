#include "gtk/gtk.h"
#include <stdarg.h>

/***************************************************
 *  Functions for Objects
 ***************************************************/

gint
ada_object_get_type (GtkObject* object)
{
  return object->klass->type;
}

/***************************************************
 *  Functions to get the field of a color selection
 *  dialog
 ***************************************************/

GtkWidget*
ada_colorsel_dialog_get_colorsel (GtkColorSelectionDialog* dialog)
{
  return dialog->colorsel;
}

GtkWidget*
ada_colorsel_dialog_get_ok_button (GtkColorSelectionDialog* dialog)
{
  return dialog->ok_button;
}

GtkWidget*
ada_colorsel_dialog_get_reset_button (GtkColorSelectionDialog* dialog)
{
  return dialog->reset_button;
}

GtkWidget*
ada_colorsel_dialog_get_cancel_button (GtkColorSelectionDialog* dialog)
{
  return dialog->cancel_button;
}

GtkWidget*
ada_colorsel_dialog_get_help_button (GtkColorSelectionDialog* dialog)
{
  return dialog->help_button;
}

/********************************************************
 *  Functions to get the fields of a gamma curve
 ********************************************************/

GtkWidget*
ada_gamma_curve_get_curve (GtkGammaCurve* widget)
{
  return widget->curve;
}

gfloat
ada_gamma_curve_get_gamma (GtkGammaCurve* widget)
{
  return widget->gamma;
}

/******************************************
 ** Functions for Alignment
 ******************************************/

gfloat
ada_alignment_get_xalign (GtkAlignment* widget)
{
   return widget->xalign;
}

gfloat
ada_alignment_get_xscale (GtkAlignment* widget)
{
   return widget->xscale;
}

gfloat
ada_alignment_get_yalign (GtkAlignment* widget)
{
   return widget->yalign;
}

gfloat
ada_alignment_get_yscale (GtkAlignment* widget)
{
   return widget->yscale;
}


/******************************************
 ** Functions for Ruler
 ******************************************/

gfloat
ada_ruler_get_lower (GtkRuler* widget)
{
   return widget->lower;
}

gfloat
ada_ruler_get_max_size (GtkRuler* widget)
{
   return widget->max_size;
}

gfloat
ada_ruler_get_position (GtkRuler* widget)
{
   return widget->position;
}

gfloat
ada_ruler_get_upper (GtkRuler* widget)
{
   return widget->upper;
}

/******************************************
 ** Functions for Editable
 ******************************************/

guint
ada_editable_get_editable (GtkEditable* widget)
{
  return widget->editable;
}

gchar*
ada_editable_get_clipboard_text (GtkEditable* widget)
{
   return widget->clipboard_text;
}

guint
ada_editable_get_current_pos (GtkEditable* widget)
{
   return widget->current_pos;
}

guint
ada_editable_get_has_selection (GtkEditable* widget)
{
   return widget->has_selection;
}

guint
ada_editable_get_selection_end_pos (GtkEditable* widget)
{
   return widget->selection_end_pos;
}

guint
ada_editable_get_selection_start_pos (GtkEditable* widget)
{
   return widget->selection_start_pos;
}

/******************************************
 ** Functions for Aspect_Frame
 ******************************************/

gfloat
ada_aspect_frame_get_ratio (GtkAspectFrame* widget)
{
   return widget->ratio;
}

gfloat
ada_aspect_frame_get_xalign (GtkAspectFrame* widget)
{
   return widget->xalign;
}

gfloat
ada_aspect_frame_get_yalign (GtkAspectFrame* widget)
{
   return widget->yalign;
}

/******************************************
 ** Functions for Text
 ******************************************/

guint
ada_text_get_gap_position (GtkText* widget)
{
   return widget->gap_position;
}

guint
ada_text_get_gap_size (GtkText* widget)
{
   return widget->gap_size;
}

guchar*
ada_text_get_text (GtkText* widget)
{
   return widget->text;
}

guint
ada_text_get_text_end (GtkText* widget)
{
   return widget->text_end;
}

/******************************************
 ** Functions for File_Selection
 ******************************************/

GtkWidget*
ada_file_selection_get_action_area (GtkFileSelection* widget)
{
   return widget->action_area;
}
GtkWidget*
ada_file_selection_get_button_area (GtkFileSelection* widget)
{
   return widget->button_area;
}
GtkWidget*
ada_file_selection_get_cancel_button (GtkFileSelection* widget)
{
   return widget->cancel_button;
}

GtkWidget*
ada_file_selection_get_dir_list (GtkFileSelection* widget)
{
   return widget->dir_list;
}

GtkWidget*
ada_file_selection_get_file_list (GtkFileSelection* widget)
{
   return widget->file_list;
}

GtkWidget*
ada_file_selection_get_help_button (GtkFileSelection* widget)
{
   return widget->help_button;
}

GtkWidget*
ada_file_selection_get_history_pulldown (GtkFileSelection* widget)
{
   return widget->history_pulldown;
}

GtkWidget*
ada_file_selection_get_ok_button (GtkFileSelection* widget)
{
   return widget->ok_button;
}

GtkWidget*
ada_file_selection_get_selection_entry (GtkFileSelection* widget)
{
   return widget->selection_entry;
}

GtkWidget*
ada_file_selection_get_selection_text (GtkFileSelection* widget)
{
   return widget->selection_text;
}

/******************************************
 ** Functions for Dialog
 ******************************************/

GtkWidget*
ada_dialog_get_action_area (GtkDialog* widget)
{
   return widget->action_area;
}

GtkWidget*
ada_dialog_get_vbox (GtkDialog* widget)
{
   return widget->vbox;
}

/******************************************
 ** Functions for Toggle_Button
 ******************************************/

guint
ada_toggle_button_get_active (GtkToggleButton* widget)
{
   return widget->active;
}

/******************************************
 ** Functions for Combo
 ******************************************/

GtkWidget*
ada_combo_get_entry (GtkCombo* widget)
{
   return widget->entry;
}

