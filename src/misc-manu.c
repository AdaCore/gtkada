#include "gtk/gtk.h"

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

/******************************************************
 *  Functions to get the fields of signal handlers
 ******************************************************/

guint
ada_gtk_signal_connect (GtkObject           *object,
			const gchar         *name,
			GtkSignalFunc        func,
			gpointer             func_data,
			GtkSignalDestroy     destroy_func)
{
  return gtk_signal_connect_full (object, name, func, NULL, func_data,
				  destroy_func, FALSE, FALSE);
}

guint
ada_gtk_signal_connect_after (GtkObject           *object,
			      const gchar         *name,
			      GtkSignalFunc        func,
			      gpointer             func_data,
			      GtkSignalDestroy     destroy_func)
{
  return gtk_signal_connect_full (object, name, func, NULL, func_data,
				  destroy_func, FALSE, TRUE);
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

