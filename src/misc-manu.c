#include "gtk/gtk.h"


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
