/* gtkcombobox - combobox widget for gtk+
 * Copyright 1999-2001 Adrian E. Feiguin <feiguin@ifir.edu.ar>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <string.h>
#include <gtk/gtkhbox.h>
#include <gtk/gtktogglebutton.h>
#include <gtk/gtkarrow.h>
#include <gtk/gtkeventbox.h>
#include <gtk/gtkmain.h>
#include <gtk/gtksignal.h>
#include <gtk/gtkwindow.h>
#include <gdk/gdkkeysyms.h>
#include <gtk/gtkframe.h>
#include "gtkcombobox.h"

static void         gtk_combobox_class_init      (GtkComboBoxClass *klass);
static void         gtk_combobox_init            (GtkComboBox      *combobox);
static void         gtk_combobox_destroy         (GtkObject     *combobox);
static void         gtk_combobox_get_pos         (GtkComboBox      *combobox, 
                                               	  gint          *x, 
                                                  gint          *y, 
                                                  gint          *height, 
                                                  gint          *width);
static void         gtk_combobox_popup_display   (GtkComboBox *combobox);
static gint	    gtk_combobox_arrow_press     (GtkWidget * widget, 
					  	  GtkComboBox * combobox);
static gint         gtk_combobox_button_press    (GtkWidget     *widget,
				                  GdkEvent      *event,
                                                  gpointer data);
static void         gtk_combobox_size_allocate   (GtkWidget     *widget,
					          GtkAllocation *allocation);
static void         gtk_combobox_size_request    (GtkWidget     *widget,
					          GtkRequisition *requisition);


static GtkHBoxClass *parent_class = NULL;

static void
gtk_combobox_class_init (GtkComboBoxClass * klass)
{
  GtkObjectClass *object_class;
  GtkWidgetClass *widget_class;

  parent_class = gtk_type_class (gtk_hbox_get_type ());
  object_class = (GtkObjectClass *) klass;
  widget_class = (GtkWidgetClass *) klass;

  object_class->destroy = gtk_combobox_destroy;
  
  widget_class->size_allocate = gtk_combobox_size_allocate;
  widget_class->size_request = gtk_combobox_size_request;
}

static void
gtk_combobox_destroy (GtkObject * combobox)
{
  gtk_widget_destroy (GTK_COMBO_BOX (combobox)->popwin);
  gtk_widget_unref (GTK_COMBO_BOX (combobox)->popwin);

  if (GTK_OBJECT_CLASS (parent_class)->destroy)
    (*GTK_OBJECT_CLASS (parent_class)->destroy) (combobox);
}


static void
gtk_combobox_get_pos (GtkComboBox * combobox, gint * x, gint * y, gint * height, gint * width)
{
  GtkBin *popwin;
  GtkWidget *widget;

  gint real_height, real_width;
  GtkRequisition child_requisition;
  gint avail_height, avail_width;
  gint min_height, min_width;
  gint work_width;
  gint work_height;

  widget = GTK_WIDGET(combobox);
  popwin = GTK_BIN (combobox->popwin);

  gdk_window_get_origin (combobox->button->window, x, y);
  real_height = combobox->button->allocation.height;
  real_width = combobox->button->allocation.width + combobox->arrow->allocation.width;

  *y += real_height;
  avail_height = gdk_screen_height () - *y;
  avail_width = gdk_screen_width() - *x;

  gtk_widget_size_request (combobox->frame, &child_requisition);

  min_height = child_requisition.height;
  min_width = child_requisition.width;

  work_width = work_height = 0;

  if(work_height+child_requisition.height > avail_height)
    if(work_height + min_height > avail_height &&
       *y - real_height > avail_height)
      	      *y -= (work_height + child_requisition.height + real_height);

  if(work_width+child_requisition.width > avail_width)
    if(work_width + min_width > avail_width &&
       *x - real_width > avail_width)
      	      *x = *x + real_width - (work_width + child_requisition.width);


  *width = work_width + child_requisition.width;
  *height = work_height + child_requisition.height;
  
}


static void
gtk_combobox_popup_display (GtkComboBox * combobox)
{
  gint height, width, x, y;
  gint old_width, old_height;


  old_width = combobox->popwin->allocation.width;
  old_height  = combobox->popwin->allocation.height;


  gtk_combobox_get_pos (combobox, &x, &y, &height, &width);

  gtk_widget_set_uposition (combobox->popwin, x, y);
  gtk_widget_set_usize (combobox->popwin, width, height);
  gtk_widget_realize (combobox->popwin);
  gdk_window_resize (combobox->popwin->window, width, height);
  gtk_widget_show (combobox->popwin);

  gtk_grab_add (combobox->popwin);
  gdk_pointer_grab (combobox->popwin->window, TRUE,
		    GDK_BUTTON_PRESS_MASK | 
		    GDK_BUTTON_RELEASE_MASK |
		    GDK_POINTER_MOTION_MASK, 
		    NULL, NULL, GDK_CURRENT_TIME);

}

void
gtk_combobox_hide_popdown_window(GtkComboBox *combobox)
{
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(combobox->arrow), FALSE);

  gtk_grab_remove(combobox->popwin);
  gdk_pointer_ungrab(GDK_CURRENT_TIME);
  gtk_widget_hide(combobox->popwin);
}

static gint
gtk_combobox_arrow_press (GtkWidget * widget, GtkComboBox * combobox)
{
  GtkToggleButton *button;

  button = GTK_TOGGLE_BUTTON(widget);

  if(!button->active){
     gtk_widget_hide (combobox->popwin);
     gtk_grab_remove (combobox->popwin);
     gdk_pointer_ungrab (GDK_CURRENT_TIME);
     return TRUE;
  }

  gtk_combobox_popup_display(combobox);
  return TRUE;
}


static void
gtk_combobox_init (GtkComboBox * combobox)
{
  GtkWidget *event_box;
  GdkCursor *cursor;
  GtkWidget *widget;
  GtkWidget *arrow;

  widget=GTK_WIDGET(combobox);

  GTK_BOX(widget)->homogeneous = FALSE;

  combobox->button = gtk_button_new ();
  combobox->arrow = gtk_toggle_button_new ();

  arrow = gtk_arrow_new (GTK_ARROW_DOWN, GTK_SHADOW_IN);
  gtk_widget_show (arrow);
  gtk_container_add (GTK_CONTAINER (combobox->arrow), arrow);

  gtk_box_pack_start (GTK_BOX (combobox), combobox->button, TRUE, TRUE, 0);
  gtk_box_pack_end (GTK_BOX (combobox), combobox->arrow, FALSE, FALSE, 0);

  gtk_widget_show (combobox->button);
  gtk_widget_show (combobox->arrow);

  gtk_signal_connect (GTK_OBJECT (combobox->arrow), "toggled",
		      (GtkSignalFunc) gtk_combobox_arrow_press, combobox);

                       
  combobox->popwin = gtk_window_new (GTK_WINDOW_POPUP);

  gtk_widget_ref (combobox->popwin);
  gtk_window_set_policy (GTK_WINDOW (combobox->popwin), 1, 1, 0);

  gtk_widget_set_events (combobox->popwin, GDK_KEY_PRESS_MASK);
 
  event_box = gtk_event_box_new ();
  gtk_container_add (GTK_CONTAINER (combobox->popwin), event_box);
  gtk_widget_show (event_box);

  gtk_widget_realize (event_box);
  cursor = gdk_cursor_new (GDK_TOP_LEFT_ARROW);
  gdk_window_set_cursor (event_box->window, cursor);
  gdk_cursor_destroy (cursor);

  combobox->frame = gtk_frame_new (NULL);
  gtk_container_add (GTK_CONTAINER (event_box), combobox->frame);
  gtk_frame_set_shadow_type (GTK_FRAME (combobox->frame), GTK_SHADOW_OUT);
  gtk_widget_show (combobox->frame);

  gtk_signal_connect (GTK_OBJECT (combobox->popwin), "button_press_event",
		      GTK_SIGNAL_FUNC (gtk_combobox_button_press), combobox);
  

}

GtkType
gtk_combobox_get_type ()
{
  static GtkType combobox_type = 0;

  if (!combobox_type)
    {
      GtkTypeInfo combobox_info =
      {
	"GtkComboBox",
	sizeof (GtkComboBox),
	sizeof (GtkComboBoxClass),
	(GtkClassInitFunc) gtk_combobox_class_init,
	(GtkObjectInitFunc) gtk_combobox_init,
	NULL,
	NULL,
	(GtkClassInitFunc) NULL,
      };
      combobox_type = gtk_type_unique (gtk_hbox_get_type (), &combobox_info);
    }
  return combobox_type;
}

GtkWidget *
gtk_combobox_new ()
{
  GtkComboBox *combobox;

  combobox = gtk_type_new (gtk_combobox_get_type ());

  return(GTK_WIDGET(combobox));

}

static gint
gtk_combobox_button_press (GtkWidget * widget, GdkEvent * event, gpointer data)
{
  GtkWidget *child;

  child = gtk_get_event_widget (event);

  if (child != widget)
    {
      while (child)
	{
	  if (child == widget)
	    return FALSE;
	  child = child->parent;
	}
    }

  gtk_widget_hide (widget);
  gtk_grab_remove (widget);
  gdk_pointer_ungrab (GDK_CURRENT_TIME);
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(GTK_COMBO_BOX(data)->arrow), FALSE);

  return TRUE;
}

static void
gtk_combobox_size_request (GtkWidget *widget,
			   GtkRequisition *requisition)
{
  GtkComboBox *combobox;
  GtkRequisition box_requisition;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (GTK_IS_COMBO_BOX (widget));
  g_return_if_fail (requisition != NULL);

  GTK_WIDGET_CLASS (parent_class)->size_request (widget, &box_requisition);

  combobox=GTK_COMBO_BOX(widget);
/*
  size = MIN(box_requisition.width, box_requisition.height);
  size = MIN(combobox->button->requisition.width, box_requisition.height);
  size = MIN(combobox->button->requisition.width, box_requisition.height);

  widget->requisition.height = size;
  widget->requisition.width = size + combobox->arrow->requisition.width;
*/
  widget->requisition.height = box_requisition.height;
  widget->requisition.width = box_requisition.width;
}


static void
gtk_combobox_size_allocate (GtkWidget     *widget,
			 GtkAllocation *allocation)
{
  GtkComboBox *combobox;
  GtkAllocation button_allocation;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (GTK_IS_COMBO_BOX (widget));
  g_return_if_fail (allocation != NULL);

  GTK_WIDGET_CLASS (parent_class)->size_allocate (widget, allocation);

  combobox = GTK_COMBO_BOX (widget);

  button_allocation = combobox->button->allocation;
/*
  button_allocation.width = MIN(button_allocation.width, 
                                combobox->button->requisition.width);
  button_allocation.height = MIN(button_allocation.height, 
                               combobox->button->requisition.height);
  button_allocation.x += (combobox->button->allocation.width-
                        button_allocation.width) / 2;
  button_allocation.y += (combobox->button->allocation.height-
                        button_allocation.height) / 2;
*/

  gtk_widget_size_allocate (combobox->button, &button_allocation);

  button_allocation.x=combobox->button->allocation.x +
                      combobox->button->allocation.width;
  button_allocation.width=combobox->arrow->requisition.width;
  gtk_widget_size_allocate (combobox->arrow, &button_allocation);

}

