#include <glib.h>
#include <gtk/gtkradiomenuitem.h>
#include <gtk/gtktogglebutton.h>
#include <gtk/gtkwidget.h>
 
/* object macros ****************************************************/

void
gtk_object_set_flags (GtkObject * object, gint flags)
{
  GTK_OBJECT_SET_FLAGS (object, flags);
}

void 
gtk_object_unset_flags (GtkObject * object, gint flags)
{
  GTK_OBJECT_UNSET_FLAGS (object, flags);
}

    
/* toggle_buttons ***************************************************/

gint 
get_state (GtkToggleButton *toggle_button)
{
  return toggle_button->active;
}

/* radio_menu_item **************************************************/

GtkWidget*
gtk_radio_menu_item_new_from_widget (GtkRadioMenuItem *group)
{
  GSList *l = NULL;
  if (group)
    l = gtk_radio_menu_item_group (group);
  return gtk_radio_menu_item_new (l);
}


/********************************************************************/

GtkWidget *
gtk_radio_menu_item_new_with_label_from_widget (GtkRadioMenuItem *group,
						const gchar      *label)
{
  GSList *l = NULL;
  if (group)
    l = gtk_radio_menu_item_group (group);
  return gtk_radio_menu_item_new_with_label (l, label);
}

