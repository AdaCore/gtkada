/*
-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------
*/

#include <glib.h>
#include <gdk/gdk.h>
#include <gtk/gtk.h>
#include <stdio.h>
 

/*
 *
 * object macros
 *
 */

guint32 ada_object_flags (GtkObject * object)
{
  return GTK_OBJECT_FLAGS (object);
}

void
ada_object_set_flags (GtkObject * object, guint32 flags)
{
  GTK_OBJECT_SET_FLAGS (object, flags);
}

void 
ada_object_unset_flags (GtkObject * object, guint32 flags)
{
  GTK_OBJECT_UNSET_FLAGS (object, flags);
}

guint32
ada_object_destroyed (GtkObject * object)
{
  return GTK_OBJECT_DESTROYED (object);
}

guint32
ada_object_floating (GtkObject * object)
{
  return GTK_OBJECT_FLOATING (object);
}

guint32
ada_object_connected (GtkObject * object)
{
  return GTK_OBJECT_CONNECTED (object);
}


/*
 *
 * Widget macros
 *
 */

guint32 
ada_widget_toplevel (GtkWidget * widget)
{
  return GTK_WIDGET_TOPLEVEL (widget);
}

guint32
ada_widget_no_window (GtkWidget * widget)
{
  return GTK_WIDGET_NO_WINDOW (widget);
}

guint32
ada_widget_realized (GtkWidget * widget)
{
  return GTK_WIDGET_REALIZED (widget);
}

guint32
ada_widget_mapped (GtkWidget * widget)
{
  return GTK_WIDGET_MAPPED (widget);
}

guint32
ada_widget_visible (GtkWidget * widget)
{
  return GTK_WIDGET_VISIBLE (widget);
}

guint32
ada_widget_drawable (GtkWidget * widget)
{
  return GTK_WIDGET_DRAWABLE (widget);
}

guint32 
ada_widget_sensitive (GtkWidget * widget)
{
  return GTK_WIDGET_SENSITIVE (widget);
}

guint32
ada_widget_parent_sensitive (GtkWidget * widget)
{
  return GTK_WIDGET_PARENT_SENSITIVE (widget);
}

guint32
ada_widget_is_sensitive (GtkWidget * widget)
{
  return GTK_WIDGET_IS_SENSITIVE (widget);
}

guint32
ada_widget_can_focus (GtkWidget * widget)
{
  return GTK_WIDGET_CAN_FOCUS (widget);
}

guint32
ada_widget_has_focus (GtkWidget * widget)
{
  return GTK_WIDGET_HAS_FOCUS (widget);
}

guint32
ada_widget_has_default (GtkWidget * widget)
{
  return GTK_WIDGET_HAS_DEFAULT (widget);
}

guint32
ada_widget_has_grab (GtkWidget * widget)
{
  return GTK_WIDGET_HAS_GRAB (widget);
}

guint32
ada_widget_basic (GtkWidget * widget)
{
  return GTK_WIDGET_BASIC (widget);
}

guint32
ada_widget_rc_style (GtkWidget * widget)
{
  return GTK_WIDGET_RC_STYLE (widget);
}

guint16
ada_widget_allocation_height (GtkWidget* widget)
{
  return widget->allocation.height;
}

guint16
ada_widget_allocation_width (GtkWidget* widget)
{
  return widget->allocation.width;
}

gint16
ada_widget_allocation_x (GtkWidget* widget)
{
  return widget->allocation.x;
}

gint16
ada_widget_allocation_y (GtkWidget* widget)
{
  return widget->allocation.y;
}


    
/*
 * 
 * toggle_buttons
 *
 */

gint 
ada_toggle_button_get_state (GtkToggleButton *toggle_button)
{
  return toggle_button->active;
}


/*
 *
 * radio_menu_item
 *
 */

GtkWidget*
ada_radio_menu_item_new_from_widget (GtkRadioMenuItem *group)
{
  GSList *l = NULL;
  if (group)
    l = gtk_radio_menu_item_group (group);
  return gtk_radio_menu_item_new (l);
}


GtkWidget *
ada_radio_menu_item_new_with_label_from_widget (GtkRadioMenuItem *group,
						const gchar      *label)
{
  GSList *l = NULL;
  if (group)
    l = gtk_radio_menu_item_group (group);
  return gtk_radio_menu_item_new_with_label (l, label);
}

/********************
 * Font_Selection
 ********************/

GtkWidget*
ada_gtk_font_selection_dialog_get_cancel (GtkFontSelectionDialog* fsd)
{
  return fsd->cancel_button;
}

GtkWidget*
ada_gtk_font_selection_dialog_get_ok (GtkFontSelectionDialog* fsd)
{
  return fsd->ok_button;
}

GtkWidget*
ada_gtk_font_selection_dialog_get_apply (GtkFontSelectionDialog* fsd)
{
  return fsd->apply_button;
}

/********************
 * Check_Menu_Item
 ********************/

guint
ada_check_menu_item_get_active (GtkCheckMenuItem* item) {
  return item->active;
}

/********************
 * Progress_Bar
 ********************/

GtkAdjustment*
ada_progress_bar_get_adjustment (GtkProgress* widget) {
  return widget->adjustment;
}

guint
ada_progress_get_activity_mode (GtkProgress* widget) {
  return widget->activity_mode;
}

/********************
 * GdkPoint
 ********************/

guint
ada_gdk_point_size () {
  return sizeof (GdkPoint);
}


/*
 *
 *  GdkCursor
 *
 */

GdkCursor* 
ada_gdk_cursor_new (gint cursor_type)
{
  return gdk_cursor_new (cursor_type);
}


/*
 *
 * GdkEventAny
 *
 */

GdkEventType
ada_gdk_event_any_get_event_type (GdkEventAny * event)
{
  return event->type;
}


void
ada_gdk_event_any_set_event_type (GdkEventAny * event, GdkEventType type)
{
  event->type = type;
}


GdkWindow *
ada_gdk_event_any_get_window (GdkEventAny * event)
{
  return event->window;
}

void
ada_gdk_event_any_set_window (GdkEventAny * event,
			      GdkWindow * window)
{
  event->window = window;
}


gint8
ada_gdk_event_any_get_send_event (GdkEventAny * event)
{
  return event->send_event;
}

void
ada_gdk_event_any_set_send_event (GdkEventAny * event, gint8 send_event)
{
  event->send_event = send_event;
}

/*
 *
 *  GdkEventExpose
 *
 */

GdkRectangle
ada_gdk_event_expose_get_area (GdkEventExpose * event)
{
  return event->area;
}

void
ada_gdk_event_expose_set_area (GdkEventExpose * event,
			       GdkRectangle * area)
{
  event->area = *area;
}

gint
ada_gdk_event_expose_get_count (GdkEventExpose * event)
{
  return event->count;
}

void
ada_gdk_event_expose_set_count (GdkEventExpose * event, gint count)
{
  event->count = count;
}


/*
 *
 * GdkEventConfigure
 *
 */

gint16
ada_gdk_event_configure_get_x (GdkEventConfigure * event)
{
  return event->x;
}

void
ada_gdk_event_configure_set_x (GdkEventConfigure * event, gint16 x)
{
  event->x = x;
}

gint16
ada_gdk_event_configure_get_y (GdkEventConfigure * event)
{
  return event->y;
}

void
ada_gdk_event_configure_set_y (GdkEventConfigure * event, gint16 y)
{
  event->y = y;
}

gint16
ada_gdk_event_configure_get_width (GdkEventConfigure * event)
{
  return event->width;
}

void
ada_gdk_event_configure_set_width (GdkEventConfigure * event, gint16 width)
{
  event->width = width;
}

gint16
ada_gdk_event_configure_get_height (GdkEventConfigure * event)
{
  return event->height;
}

void
ada_gdk_event_configure_set_height (GdkEventConfigure * event, gint16 height)
{
  event->height = height;
}

/*
 *
 * GdkEventButton
 *
 */

guint32
ada_gdk_event_button_get_state (GdkEventButton * event)
{
  return event->state;
}

guint32
ada_gdk_event_button_get_button (GdkEventButton * event)
{
  return event->button;
}

gint16
ada_gdk_event_button_get_x (GdkEventButton * event)
{
  return event->x;
}

void
ada_gdk_event_button_set_x (GdkEventButton * event, gint16 x)
{
  event->x = x;
}

gint16
ada_gdk_event_button_get_y (GdkEventButton * event)
{
  return event->y;
}

void
ada_gdk_event_button_set_y (GdkEventButton * event, gint16 y)
{
  event->y = y;
}


/*
 *
 * GdkEventMotion
 *
 */

gint16
ada_gdk_event_motion_get_x (GdkEventMotion * event)
{
  return event->x;
}

gint16
ada_gdk_event_motion_get_y (GdkEventMotion * event)
{
  return event->y;
}


/********************
 * GtkAdjustment
 ********************/

gfloat
ada_gtk_adjustment_get_value (GtkAdjustment * adjustment)
{
  return adjustment->value;
}

gfloat
ada_adjustment_get_lower (GtkAdjustment * adjustment)
{
  return adjustment->lower;
}

gfloat
ada_adjustment_get_upper (GtkAdjustment * adjustment)
{
  return adjustment->upper;
}

void
ada_adjustment_set_page_increment (GtkAdjustment * adjustment,
                                       gfloat value)
{
  adjustment->page_increment = value;
}

void
ada_gtk_adjustment_set_lower (GtkAdjustment * adjustment, gfloat lower)
{
  adjustment->lower = lower;
}

void
ada_gtk_adjustment_set_upper (GtkAdjustment * adjustment, gfloat upper)
{
  adjustment->upper = upper;
}

gfloat
gtk_adjustment_get_step_increment (GtkAdjustment * adjustment)
{
  return adjustment->step_increment;
}

void
ada_adjustment_set_page_size (GtkAdjustment * adjustment,
                                  gfloat value)
{
  adjustment->page_size = value;
}

guint32
ada_gdk_event_motion_get_state (GdkEventButton * event)
{
  return event->state;
}


/*
 *
 * GtkStyle
 *
 */

GdkGC *
ada_gtk_style_get_black_gc (GtkStyle * style)
{
  return style->black_gc;
}

GdkGC *
ada_gtk_style_get_bg_gc (GtkStyle * style, gint state)
{
  return style->bg_gc [state];
}

GdkGC *
ada_gtk_style_get_white_gc (GtkStyle * style)
{
  return style->white_gc;
}

/*************************************
 * GdkColor
 ************************************/

guint
ada_gdk_color_size () {
  return sizeof (GdkColor);
}

/*************************************
 * GdkRectangle
 ************************************/

guint
ada_gdk_rectangle_size () {
  return sizeof (GdkRectangle);
}

/***************************************************
 *  Functions for Objects
 ***************************************************/

gint
ada_object_get_type (GtkObject* object)
{
  return GTK_OBJECT_TYPE (object);
}

/***************************************************
 *  Functions for GtkArg
 ***************************************************/

gpointer
ada_gtkarg_value_object (GtkArg* args, guint num)
{
  gpointer return_value = NULL;
  switch (args [num].type % 256)
    {
    case GTK_TYPE_OBJECT:
      return_value = (gpointer)GTK_VALUE_OBJECT (args [num]);
      break;
    case GTK_TYPE_POINTER:
      return_value = (gpointer)GTK_VALUE_POINTER (args [num]);
      break;
    case GTK_TYPE_STRING:
      return_value = (gpointer)GTK_VALUE_STRING (args [num]);
      break;
    case GTK_TYPE_BOXED:
      return_value = (gpointer)GTK_VALUE_BOXED (args [num]);
      break;
    default:
      {
	fprintf (stderr, "request for an Object value (%d) when we have a %d\n",
		 GTK_TYPE_OBJECT, (args[num].type % 256));
      }
    }
  return return_value;
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

void
ada_editable_set_editable (GtkEditable* widget, guint val)
{
  widget->editable = val;
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

GtkAdjustment*
ada_text_get_hadj (GtkText* widget)
{
  return widget->hadj;
}

GtkAdjustment*
ada_text_get_vadj (GtkText* widget)
{
  return widget->vadj;
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

GtkWidget*
ada_combo_get_list (GtkCombo* widget)
{
  return widget->list;
}

/*******************************************
 ** Functions for Style
 *******************************************/

GdkColor
ada_style_get_bg (GtkStyle* style, gint state)
{
  return (style->bg [state]);
}

GdkColor
ada_style_get_black (GtkStyle* style)
{
  return (style->black);
}

GdkColor
ada_style_get_white (GtkStyle* style)
{
  return (style->white);
}

/********************************************
 ** Functions for Widget
 ********************************************/

GtkStyle*
ada_widget_get_style (GtkWidget* widget)
{
  return widget->style;
}

GdkWindow*
ada_widget_get_window (GtkWidget* widget)
{
  return widget->window;
}

GtkWidget*
ada_widget_get_parent (GtkWidget* widget)
{
  return widget->parent;
}

GtkSignalFunc
ada_widget_get_motion_notify (GtkWidget* widget)
{
  return (GtkSignalFunc)(GTK_WIDGET_CLASS (GTK_OBJECT (widget)->klass)
			 ->motion_notify_event);
}

/******************************************
 ** Functions for Pixmap
 ******************************************/

GdkBitmap*
ada_pixmap_get_mask (GtkPixmap* widget)
{
   return widget->mask;
}

GdkPixmap*
ada_pixmap_get_pixmap (GtkPixmap* widget)
{
   return widget->pixmap;
}

/*******************************************
 ** Functions for Notebook
 *******************************************/

gint
ada_notebook_get_tab_pos (GtkNotebook* widget)
{
   return widget->tab_pos;
}

GList*
ada_notebook_get_children (GtkNotebook* widget)
{
  return widget->children;
}

GtkNotebookPage*
ada_notebook_get_cur_page (GtkNotebook* widget)
{
  return widget->cur_page;
}

GtkWidget*
ada_notebook_get_menu_label (GtkNotebookPage* widget)
{
  return widget->menu_label;
}

GtkWidget*
ada_notebook_get_tab_label (GtkNotebookPage* widget)
{
  return widget->tab_label;
}

/**********************************************
 ** Functions for Box
 **********************************************/

GtkWidget*
ada_box_get_child (GtkBox* widget, gint num)
{
  if (num < g_list_length (widget->children))
    return ((GtkBoxChild*)(g_list_nth_data (widget->children, num)))->widget;
  else
    return NULL;
}

/**********************************************
 ** Functions for Glib.Glist
 **********************************************/

GList*
ada_list_next (GList* list)
{
  if (list)
    return list->next;
  else
    return NULL;
}


GList*
ada_list_prev (GList* list)
{
  if (list)
    return list->prev;
  else
    return NULL;
}

gpointer
ada_list_get_data (GList* list)
{
  return list->data;
}

/**********************************************
 ** Functions for Glib.GSlist
 **********************************************/

GSList*
ada_gslist_next (GSList* list)
{
  if (list)
    return list->next;
  else
    return NULL;
}

gpointer
ada_gslist_get_data (GSList* list)
{
  return list->data;
}

/******************************************
 ** Functions for Fixed
 ******************************************/

GList*
ada_fixed_get_children (GtkFixed* widget)
{
   return widget->children;
}

/******************************************
 ** Functions for Status bar
 ******************************************/

GSList *
ada_status_get_messages (GtkStatusbar* widget)
{
  return widget->messages;
}


/******************************************
 ** Functions for GtkList
 ******************************************/

GList*
ada_list_get_children (GtkList* widget)
{
   return widget->children;
}

GList*
ada_list_get_selection (GtkList* widget)
{
   return widget->selection;
}

/******************************************
 ** Functions for Tree
 ******************************************/

GList*
ada_tree_get_children (GtkTree* widget)
{
   return widget->children;
}

GList*
ada_tree_get_selection (GtkTree* widget)
{
   return widget->selection;
}

/*******************************************
 ** Functions for Tree_Item
 *******************************************/

GtkWidget*
ada_tree_item_get_subtree (GtkTreeItem* widget)
{
  return GTK_TREE_ITEM_SUBTREE (widget);
}


/******************************************
 ** Functions for CList
 ******************************************/

GList*
ada_clist_get_selection (GtkCList* widget)
{
   return widget->selection;
}

GtkWidget*
ada_clist_get_column_button (GtkCList* widget,
			     gint      column)
{
  if (widget->columns < column)
    return widget->column[column].button;
  else
    return NULL;
}

GdkWindow*
ada_clist_get_clist_window (GtkCList* widget)
{
  return widget->clist_window;
}


/*
 *
 * GdkWindowAttr
 *
 */

GdkWindowAttr*
ada_gdk_window_attr_new (void)
{
  GdkWindowAttr *result;
  
  result = (GdkWindowAttr*) g_new (GdkWindowAttr, 1);

  if (result)
    {
      result->title = NULL;
      result->visual = NULL;
      result->colormap = NULL;
      result->cursor = NULL;
      result->wmclass_name = NULL;
      result->wmclass_class = NULL;
      /* 
       * Here, we only set the pointers to NULL to avoid any dangling
       * pointer. All the other values are left as is. It is the
       * responsibility of the client to make sure they are properly
       * set before they are accessed.
       */
    }

  return result;
}

void
ada_gdk_window_attr_destroy (GdkWindowAttr *window_attr)
{
  g_return_if_fail (window_attr != NULL);

  if (window_attr->title) g_free (window_attr->title);
  if (window_attr->visual) gdk_visual_unref (window_attr->visual);
  if (window_attr->colormap) gdk_colormap_unref (window_attr->colormap);
  if (window_attr->cursor) gdk_cursor_destroy (window_attr->cursor);
  if (window_attr->wmclass_name) g_free (window_attr->wmclass_name);
  if (window_attr->wmclass_class) g_free (window_attr->wmclass_class);

  g_free (window_attr);
}

void
ada_gdk_window_attr_set_title (GdkWindowAttr *window_attr,
			       gchar * title)
{
  g_return_if_fail (window_attr != NULL);
  
  window_attr->title = title;
}

gchar*
ada_gdk_window_attr_get_title (GdkWindowAttr *window_attr)
{
  g_return_val_if_fail (window_attr != NULL, NULL);
  
  return window_attr->title;
}

void
ada_gdk_window_attr_set_event_mask (GdkWindowAttr *window_attr,
				    gint event_mask)
{
  g_return_if_fail (window_attr != NULL);
  
  window_attr->event_mask = event_mask;
}

gint
ada_gdk_window_attr_get_event_mask (GdkWindowAttr *window_attr)
{
  g_return_val_if_fail (window_attr != NULL, 0);
  
  return window_attr->event_mask;
}

void
ada_gdk_window_attr_set_x (GdkWindowAttr * window_attr, gint16 x)
{
  g_return_if_fail (window_attr != NULL);
  
  window_attr->x = x;
}

gint16
ada_gdk_window_attr_get_x (GdkWindowAttr *window_attr)
{
  g_return_val_if_fail (window_attr != NULL, 0);
  
  return window_attr->x;
}

void
ada_gdk_window_attr_set_y (GdkWindowAttr * window_attr, gint16 y)
{
  g_return_if_fail (window_attr != NULL);
  
  window_attr->y = y;
}
  
gint16
ada_gdk_window_attr_get_y (GdkWindowAttr *window_attr)
{
  g_return_val_if_fail (window_attr != NULL, 0);
  
  return window_attr->y;
}

void
ada_gdk_window_attr_set_width (GdkWindowAttr * window_attr, gint16 width)
{
  g_return_if_fail (window_attr != NULL);
  
  window_attr->width = width;
}
  
gint16
ada_gdk_window_attr_get_width (GdkWindowAttr *window_attr)
{
  g_return_val_if_fail (window_attr != NULL, 0);
  
  return window_attr->width;
}

void
ada_gdk_window_attr_set_height (GdkWindowAttr * window_attr, gint16 height)
{
  g_return_if_fail (window_attr != NULL);
  
  window_attr->height = height;
}
  
gint16
ada_gdk_window_attr_get_height (GdkWindowAttr *window_attr)
{
  g_return_val_if_fail (window_attr != NULL, 0);
  
  return window_attr->height;
}

void
ada_gdk_window_attr_set_wclass (GdkWindowAttr *window_attr,
				GdkWindowClass wclass)
{
  g_return_if_fail (window_attr != NULL);
  
  window_attr->wclass = wclass;
}

GdkWindowClass
ada_gdk_window_attr_get_wclass (GdkWindowAttr *window_attr)
{
  g_return_val_if_fail (window_attr != NULL, GDK_INPUT_OUTPUT);
  
  return window_attr->wclass;
}

void
ada_gdk_window_attr_set_visual (GdkWindowAttr *window_attr,
				GdkVisual *visual)
{
  g_return_if_fail (window_attr != NULL);
  
  window_attr->visual = visual;
}

GdkVisual*
ada_gdk_window_attr_get_visual (GdkWindowAttr *window_attr)
{
  g_return_val_if_fail (window_attr != NULL, NULL);
  
  return window_attr->visual;
}

void
ada_gdk_window_attr_set_colormap (GdkWindowAttr *window_attr,
				  GdkColormap *colormap)
{
  g_return_if_fail (window_attr != NULL);
  
  window_attr->colormap = colormap;
}

GdkColormap*
ada_gdk_window_attr_get_colormap (GdkWindowAttr *window_attr)
{
  g_return_val_if_fail (window_attr != NULL, NULL);
  
  return window_attr->colormap;
}

void
ada_gdk_window_attr_set_window_type (GdkWindowAttr *window_attr,
				     GdkWindowType window_type)
{
  g_return_if_fail (window_attr != NULL);
  
  window_attr->window_type = window_type;
}

GdkWindowType
ada_gdk_window_attr_get_window_type (GdkWindowAttr *window_attr)
{
  g_return_val_if_fail (window_attr != NULL, GDK_WINDOW_ROOT);
  
  return window_attr->window_type;
}

void
ada_gdk_window_attr_set_cursor (GdkWindowAttr *window_attr,
				GdkCursor *cursor)
{
  g_return_if_fail (window_attr != NULL);
  
  window_attr->cursor = cursor;
}

GdkCursor*
ada_gdk_window_attr_get_cursor (GdkWindowAttr *window_attr)
{
  g_return_val_if_fail (window_attr != NULL, NULL);
  
  return window_attr->cursor;
}

void
ada_gdk_window_attr_set_wmclass_name (GdkWindowAttr *window_attr,
				      gchar *wmclass_name)
{
  g_return_if_fail (window_attr != NULL);
  
  window_attr->wmclass_name = wmclass_name;
}

gchar*
ada_gdk_window_attr_get_wmclass_name (GdkWindowAttr *window_attr)
{
  g_return_val_if_fail (window_attr != NULL, NULL);
  
  return window_attr->wmclass_name;
}

void
ada_gdk_window_attr_set_wmclass_class (GdkWindowAttr *window_attr,
				      gchar *wmclass_class)
{
  g_return_if_fail (window_attr != NULL);
  
  window_attr->wmclass_class = wmclass_class;
}

gchar*
ada_gdk_window_attr_get_wmclass_class (GdkWindowAttr *window_attr)
{
  g_return_val_if_fail (window_attr != NULL, NULL);
  
  return window_attr->wmclass_class;
}

void
ada_gdk_window_attr_set_override_redirect (GdkWindowAttr *window_attr,
					   gboolean override_redirect)
{
  g_return_if_fail (window_attr != NULL);
  
  window_attr->override_redirect = override_redirect;
}

gboolean
ada_gdk_window_attr_get_override_redirect (GdkWindowAttr * window_attr)
{
  g_return_val_if_fail (window_attr != NULL, FALSE);
  
  return window_attr->override_redirect;
}
