/* gtkcheckitem - widget for gtk+
 * Copyright (C) 1999-2001 Adrian E. Feiguin 
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
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

/*
 * Modified by the GTK+ Team and others 1997-1999.  See the AUTHORS
 * file for a list of people on the GTK+ Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GTK+ at ftp://ftp.gtk.org/pub/gtk/. 
 */

#include "gtkcheckitem.h"


#define INDICATOR_SIZE     14
#define INDICATOR_SPACING  2

#define CHECK_ITEM_CLASS(w)  GTK_CHECK_ITEM_CLASS (GTK_OBJECT (w)->klass)


static void gtk_check_item_class_init          (GtkCheckItemClass *klass);
static void gtk_check_item_init                (GtkCheckItem      *check_item);
static void gtk_check_item_draw                (GtkWidget           *widget,
						  GdkRectangle        *area);
static void gtk_check_item_draw_focus          (GtkWidget           *widget);
static void gtk_check_item_size_request        (GtkWidget           *widget,
						  GtkRequisition      *requisition);
static void gtk_check_item_size_allocate       (GtkWidget           *widget,
						  GtkAllocation       *allocation);
static gint gtk_check_item_expose              (GtkWidget           *widget,
						  GdkEventExpose      *event);
static void gtk_check_item_paint               (GtkWidget           *widget,
						  GdkRectangle        *area);
static void gtk_check_item_draw_indicator      (GtkCheckItem      *check_item,
						  GdkRectangle        *area);
static void gtk_real_check_item_draw_indicator (GtkCheckItem      *check_item,
						  GdkRectangle        *area);

static GtkToggleButtonClass *parent_class = NULL;


GtkType
gtk_check_item_get_type (void)
{
  static GtkType check_item_type = 0;
  
  if (!check_item_type)
    {
      static const GtkTypeInfo check_item_info =
      {
	"GtkCheckItem",
	sizeof (GtkCheckItem),
	sizeof (GtkCheckItemClass),
	(GtkClassInitFunc) gtk_check_item_class_init,
	(GtkObjectInitFunc) gtk_check_item_init,
	/* reserved_1 */ NULL,
        /* reserved_2 */ NULL,
        (GtkClassInitFunc) NULL,
      };
      
      check_item_type = gtk_type_unique (GTK_TYPE_TOGGLE_BUTTON, &check_item_info);
    }
  
  return check_item_type;
}

static void
gtk_check_item_class_init (GtkCheckItemClass *class)
{
  GtkWidgetClass *widget_class;
  
  widget_class = (GtkWidgetClass*) class;
  parent_class = gtk_type_class (gtk_toggle_button_get_type ());
  
  widget_class->draw = gtk_check_item_draw;
  widget_class->draw_focus = gtk_check_item_draw_focus;
  widget_class->size_request = gtk_check_item_size_request;
  widget_class->size_allocate = gtk_check_item_size_allocate;
  widget_class->expose_event = gtk_check_item_expose;
  
  class->indicator_size = INDICATOR_SIZE;
  class->indicator_spacing = INDICATOR_SPACING;
  class->draw_indicator = gtk_real_check_item_draw_indicator;
}

static void
gtk_check_item_init (GtkCheckItem *check_item)
{
  GTK_WIDGET_SET_FLAGS (check_item, GTK_NO_WINDOW);
  GTK_WIDGET_UNSET_FLAGS (check_item, GTK_RECEIVES_DEFAULT);
  GTK_TOGGLE_BUTTON (check_item)->draw_indicator = TRUE;
}

GtkWidget*
gtk_check_item_new (void)
{
  return gtk_widget_new (GTK_TYPE_CHECK_ITEM, NULL);
}


GtkWidget*
gtk_check_item_new_with_label (const gchar *label)
{
  GtkWidget *check_item;
  GtkWidget *label_widget;
  
  check_item = gtk_check_item_new ();
  label_widget = gtk_label_new (label);
  gtk_misc_set_alignment (GTK_MISC (label_widget), 0.0, 0.5);
  
  gtk_container_add (GTK_CONTAINER (check_item), label_widget);
  gtk_widget_show (label_widget);
  
  return check_item;
}

/* This should only be called when toggle_button->draw_indicator
 * is true.
 */
static void
gtk_check_item_paint (GtkWidget    *widget,
			GdkRectangle *area)
{
  GtkCheckItem *check_item;
  
  g_return_if_fail (widget != NULL);
  g_return_if_fail (GTK_IS_CHECK_ITEM (widget));
  
  check_item = GTK_CHECK_ITEM (widget);
  
  if (GTK_WIDGET_DRAWABLE (widget))
    {
      gint border_width;
	  
      gtk_check_item_draw_indicator (check_item, area);
      
      border_width = GTK_CONTAINER (widget)->border_width;
      if (GTK_WIDGET_HAS_FOCUS (widget))
	gtk_paint_focus (widget->style, widget->window,
			 NULL, widget, "checkitem",
			 border_width + widget->allocation.x,
			 border_width + widget->allocation.y,
			 widget->allocation.width - 2 * border_width - 1,
			 widget->allocation.height - 2 * border_width - 1);
    }
}

static void
gtk_check_item_draw (GtkWidget    *widget,
		       GdkRectangle *area)
{
  GtkCheckItem *check_item;
  GtkToggleButton *toggle_button;
  GtkBin *bin;
  GdkRectangle child_area;
  
  g_return_if_fail (widget != NULL);
  g_return_if_fail (GTK_IS_CHECK_ITEM (widget));
  g_return_if_fail (area != NULL);
  
  check_item = GTK_CHECK_ITEM (widget);
  toggle_button = GTK_TOGGLE_BUTTON (widget);
  bin = GTK_BIN (widget);
  
  if (GTK_WIDGET_DRAWABLE (widget))
    {
      if (toggle_button->draw_indicator)
	{
	  gtk_check_item_paint (widget, area);

	  if (bin->child && gtk_widget_intersect (bin->child, area, &child_area))
	    gtk_widget_draw (bin->child, &child_area);
	}
      else
	{
	  if (GTK_WIDGET_CLASS (parent_class)->draw)
	    (* GTK_WIDGET_CLASS (parent_class)->draw) (widget, area);
	}
    }
}

static void
gtk_check_item_draw_focus (GtkWidget *widget)
{
  gint border_width;
  
  g_return_if_fail (widget != NULL);
  g_return_if_fail (GTK_IS_CHECK_ITEM (widget));
  
  border_width = GTK_CONTAINER (widget)->border_width;
  gtk_widget_queue_clear_area (widget->parent, 
			       border_width + widget->allocation.x,
			       border_width + widget->allocation.y,
			       widget->allocation.width - 2 * border_width,
			       widget->allocation.height - 2 * border_width);
}

static void
gtk_check_item_size_request (GtkWidget      *widget,
			       GtkRequisition *requisition)
{
  GtkToggleButton *toggle_button;
  gint temp;
  
  g_return_if_fail (widget != NULL);
  g_return_if_fail (GTK_IS_CHECK_ITEM (widget));
  g_return_if_fail (requisition != NULL);
  
  toggle_button = GTK_TOGGLE_BUTTON (widget);
  
  if (GTK_WIDGET_CLASS (parent_class)->size_request)
    (* GTK_WIDGET_CLASS (parent_class)->size_request) (widget, requisition);
  
  if (toggle_button->draw_indicator)
    {
      requisition->width += (CHECK_ITEM_CLASS (widget)->indicator_size +
			     CHECK_ITEM_CLASS (widget)->indicator_spacing * 3 + 2);
      
      temp = (CHECK_ITEM_CLASS (widget)->indicator_size +
	      CHECK_ITEM_CLASS (widget)->indicator_spacing * 2);
      requisition->height = MAX (requisition->height, temp) + 2;
    }
}

static void
gtk_check_item_size_allocate (GtkWidget     *widget,
				GtkAllocation *allocation)
{
  GtkCheckItem *check_item;
  GtkToggleButton *toggle_button;
  GtkButton *button;
  GtkAllocation child_allocation;
  
  g_return_if_fail (widget != NULL);
  g_return_if_fail (GTK_IS_CHECK_ITEM (widget));
  g_return_if_fail (allocation != NULL);
  
  check_item = GTK_CHECK_ITEM (widget);
  toggle_button = GTK_TOGGLE_BUTTON (widget);

  if (toggle_button->draw_indicator)
    {
      widget->allocation = *allocation;
      if (GTK_WIDGET_REALIZED (widget))
	gdk_window_move_resize (toggle_button->event_window,
				allocation->x, allocation->y,
				allocation->width, allocation->height);
      
      button = GTK_BUTTON (widget);
      
      if (GTK_BIN (button)->child && GTK_WIDGET_VISIBLE (GTK_BIN (button)->child))
	{
	  child_allocation.x = (GTK_CONTAINER (widget)->border_width +
				CHECK_ITEM_CLASS (widget)->indicator_size +
				CHECK_ITEM_CLASS (widget)->indicator_spacing * 3 + 1 +
				widget->allocation.x);
	  child_allocation.y = GTK_CONTAINER (widget)->border_width + 1 +
	    widget->allocation.y;
	  child_allocation.width = MAX (1, allocation->width - 
					(GTK_CONTAINER (widget)->border_width +
					 CHECK_ITEM_CLASS (widget)->indicator_size +
					 CHECK_ITEM_CLASS (widget)->indicator_spacing * 3 + 1)  -
					GTK_CONTAINER (widget)->border_width - 1);
	  child_allocation.height = MAX (1, allocation->height - (GTK_CONTAINER (widget)->border_width + 1) * 2);
	  
	  gtk_widget_size_allocate (GTK_BIN (button)->child, &child_allocation);
	}
    }
  else
    {
      if (GTK_WIDGET_CLASS (parent_class)->size_allocate)
	(* GTK_WIDGET_CLASS (parent_class)->size_allocate) (widget, allocation);
    }
}

static gint
gtk_check_item_expose (GtkWidget      *widget,
			 GdkEventExpose *event)
{
  GtkCheckItem *check_item;
  GtkToggleButton *toggle_button;
  GtkBin *bin;
  GdkEventExpose child_event;
  
  g_return_val_if_fail (widget != NULL, FALSE);
  g_return_val_if_fail (GTK_IS_CHECK_ITEM (widget), FALSE);
  g_return_val_if_fail (event != NULL, FALSE);
  
  check_item = GTK_CHECK_ITEM (widget);
  toggle_button = GTK_TOGGLE_BUTTON (widget);
  bin = GTK_BIN (widget);
  
  if (GTK_WIDGET_DRAWABLE (widget))
    {
      if (toggle_button->draw_indicator)
	{
	  gtk_check_item_paint (widget, &event->area);
	  
	  child_event = *event;
	  if (bin->child && GTK_WIDGET_NO_WINDOW (bin->child) &&
	      gtk_widget_intersect (bin->child, &event->area, &child_event.area))
	    gtk_widget_event (bin->child, (GdkEvent*) &child_event);
	}
      else
	{
	  if (GTK_WIDGET_CLASS (parent_class)->expose_event)
	    (* GTK_WIDGET_CLASS (parent_class)->expose_event) (widget, event);
	}
    }
  
  return FALSE;
}


static void
gtk_check_item_draw_indicator (GtkCheckItem *check_item,
				 GdkRectangle   *area)
{
  GtkCheckItemClass *class;
  
  g_return_if_fail (check_item != NULL);
  g_return_if_fail (GTK_IS_CHECK_ITEM (check_item));
  
  class = CHECK_ITEM_CLASS (check_item);
  
  if (class->draw_indicator)
    (* class->draw_indicator) (check_item, area);
}

static void
gtk_real_check_item_draw_indicator (GtkCheckItem *check_item,
				      GdkRectangle   *area)
{
  GtkWidget *widget;
  GtkToggleButton *toggle_button;
  GtkStateType state_type;
  GdkRectangle restrict_area;
  GdkRectangle new_area;
  GdkGC *fg_gc = NULL;
  gint width, height;
  gint x, y;
  gint border;
  GdkWindow *window;
  
  g_return_if_fail (check_item != NULL);
  g_return_if_fail (GTK_IS_CHECK_ITEM (check_item));
  
  widget = GTK_WIDGET (check_item);
  toggle_button = GTK_TOGGLE_BUTTON (check_item);

  if (GTK_WIDGET_DRAWABLE (check_item))
    {
      window = widget->window;
      
      state_type = GTK_WIDGET_STATE (widget);
      if (state_type != GTK_STATE_NORMAL &&
	  state_type != GTK_STATE_PRELIGHT)
	state_type = GTK_STATE_NORMAL;
      
      restrict_area.x = widget->allocation.x + GTK_CONTAINER (widget)->border_width;
      restrict_area.y = widget->allocation.y + GTK_CONTAINER (widget)->border_width;
      restrict_area.width = widget->allocation.width - ( 2 * GTK_CONTAINER (widget)->border_width);
      restrict_area.height = widget->allocation.height - ( 2 * GTK_CONTAINER (widget)->border_width);
      
      if (gdk_rectangle_intersect (area, &restrict_area, &new_area))
	{
	  if (state_type != GTK_STATE_NORMAL)
	    gtk_paint_flat_box (widget->style, window, state_type, 
				GTK_SHADOW_ETCHED_OUT, 
				area, widget, "checkitem",
				new_area.x, new_area.y,
				new_area.width, new_area.height);
	}
      
      x = widget->allocation.x + CHECK_ITEM_CLASS (widget)->indicator_spacing + GTK_CONTAINER (widget)->border_width;
      y = widget->allocation.y + (widget->allocation.height - CHECK_ITEM_CLASS (widget)->indicator_size) / 2;
      width = CHECK_ITEM_CLASS (widget)->indicator_size;
      height = CHECK_ITEM_CLASS (widget)->indicator_size;
    
      if(!GTK_BIN(widget)->child){
       x = widget->allocation.x + widget->allocation.width/2 - width/2;
       y = widget->allocation.y + widget->allocation.height/2 - height/2;
      }
 
      if (GTK_TOGGLE_BUTTON (widget)->active)
        {
          state_type = GTK_STATE_ACTIVE;
        }
      else
        {
          state_type = GTK_STATE_NORMAL;
        }
 
      fg_gc = gdk_gc_new(window);
      gdk_gc_set_foreground(fg_gc, &widget->style->white);

      gdk_draw_rectangle(window,
                         fg_gc,
                         TRUE,
                         x, y, width, height);

      gtk_draw_shadow (widget->style, window, GTK_STATE_NORMAL,
                       GTK_SHADOW_IN, x, y, width, height);

      if(state_type == GTK_STATE_ACTIVE){
         GdkPoint points[3];
         border = widget->style->klass->xthickness;
         gdk_gc_set_foreground(fg_gc, &widget->style->black);
         x += border;
         y += border;

         points[0].x = x+1; 
         points[0].y = y+6; 
         points[1].x = x+3; 
         points[1].y = y+height-2*border-2; 
         points[2].x = x+width-2*border-2; 
         points[2].y = y+3; 

         gdk_draw_lines(window, fg_gc, points, 3);

         points[0].x = x+1; 
         points[0].y = y+5; 
         points[1].x = x+3; 
         points[1].y = y+height-2*border-3; 
         points[2].x = x+width-2*border-2; 
         points[2].y = y+2; 

         gdk_draw_lines(window, fg_gc, points, 3);

         points[0].x = x+1; 
         points[0].y = y+4; 
         points[1].x = x+3; 
         points[1].y = y+height-2*border-4; 
         points[2].x = x+width-2*border-2; 
         points[2].y = y+1; 

         gdk_draw_lines(window, fg_gc, points, 3);

      }

    }

    gdk_gc_unref(fg_gc);
}
