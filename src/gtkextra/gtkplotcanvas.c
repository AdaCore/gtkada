/* gtkplotcanvas - gtkplot canvas widget for gtk+
 * Copyright 1999-2001  Adrian E. Feiguin <feiguin@ifir.edu.ar>
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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <gtk/gtk.h>
#include "gtkplot.h"
#include "gtkplotlayout.h"
#include "gtkplotcanvas.h"

static void gtk_plot_canvas_class_init 		(GtkPlotCanvasClass *class);
static void gtk_plot_canvas_init 		(GtkPlotCanvas *plot_canvas);
static void gtk_plot_canvas_finalize 		(GtkObject *object);
static gint gtk_plot_canvas_motion 		(GtkWidget *widget, 
                                                 GdkEventMotion *event);
static gint gtk_plot_canvas_button_press	(GtkWidget *widget, 
                                                 GdkEventButton *event);
static gint gtk_plot_canvas_button_release	(GtkWidget *widget, 
                                                 GdkEventButton *event);
/*
static void gtk_plot_canvas_size_request 	(GtkWidget *widget, 
                                                 GtkRequisition *requisition);
static void gtk_plot_canvas_draw 		(GtkWidget *widget, 
						 GdkRectangle *area);
static void gtk_plot_canvas_paint 		(GtkWidget *widget); 
static gint gtk_plot_canvas_expose		(GtkWidget *widget, 
						 GdkEventExpose *event);
*/
static void gtk_plot_canvas_get_pixel		(GtkWidget *widget, 
						 gdouble px, gdouble py,
                          			 gint *x, gint *y);
static void gtk_plot_canvas_get_position	(GtkWidget *widget, 
						 gint x, gint y,
                            			 gdouble *px, gdouble *py);

/* Signals */

enum {
        CLICK_PLOT,
        CLICK_POINT,
        CLICK_TITLE,
        CLICK_TEXT,
        CLICK_AXIS,
	CLICK_LEGENDS,
        MOVE_TEXT,
        MOVE_LEGENDS,
        MOVE_PLOT,
        RESIZE_PLOT,
        SELECT_REGION,
        LAST_SIGNAL
};

typedef gboolean (*GtkPlotCanvasSignal1) (GtkObject *object,
                                          gpointer arg1, 
                                    	  gpointer user_data);

typedef gboolean (*GtkPlotCanvasSignal2) (GtkObject *object,
                                          gpointer arg1, 
					  gpointer arg2,
                                    	  gpointer user_data);

typedef gboolean (*GtkPlotCanvasSignal3) (GtkObject *object,
                                          gdouble arg1, 
                                          gdouble arg2, 
                                          gdouble arg3, 
                                          gdouble arg4, 
                                    	  gpointer user_data);

typedef gboolean (*GtkPlotCanvasSignal4) (GtkObject *object,
                                          gdouble arg1, 
					  gdouble arg2,
                                    	  gpointer user_data);

static void
gtk_plot_canvas_marshal_BOOL__POINTER           (GtkObject *object,
                                                 GtkSignalFunc func,
                                                 gpointer func_data,
                                                 GtkArg * args);

static void
gtk_plot_canvas_marshal_BOOL__POINTER_POINTER   (GtkObject *object,
                                                 GtkSignalFunc func,
                                                 gpointer func_data,
                                                 GtkArg * args);

static void
gtk_plot_canvas_marshal_BOOL__DOUBLE_DOUBLE     (GtkObject *object,
                                                 GtkSignalFunc func,
                                                 gpointer func_data,
                                                 GtkArg * args);

static void
gtk_plot_canvas_marshal_select		        (GtkObject *object,
                                                 GtkSignalFunc func,
                                                 gpointer func_data,
                                                 GtkArg * args);

static GtkPlotCanvasClass *parent_class = NULL;
static guint canvas_signals[LAST_SIGNAL] = {0};

guint
gtk_plot_canvas_get_type (void)
{
  static GtkType plot_canvas_type = 0;

  if (!plot_canvas_type)
    {
      GtkTypeInfo plot_canvas_info =
      {
	"GtkPlotCanvas",
	sizeof (GtkPlotCanvas),
	sizeof (GtkPlotCanvasClass),
	(GtkClassInitFunc) gtk_plot_canvas_class_init,
	(GtkObjectInitFunc) gtk_plot_canvas_init,
	/* reserved 1*/ NULL,
        /* reserved 2 */ NULL,
        (GtkClassInitFunc) NULL,
      };

      plot_canvas_type = gtk_type_unique (gtk_plot_layout_get_type(), &plot_canvas_info);
    }
  return plot_canvas_type;
}

static void
gtk_plot_canvas_class_init (GtkPlotCanvasClass *class)
{
  GtkObjectClass *object_class;
  GtkWidgetClass *widget_class;

  parent_class = gtk_type_class (gtk_layout_get_type ());

  object_class = (GtkObjectClass *) class;
  widget_class = (GtkWidgetClass *) class;

  canvas_signals[CLICK_PLOT] =
    gtk_signal_new ("click_on_plot",
                    GTK_RUN_LAST,
                    object_class->type,
                    GTK_SIGNAL_OFFSET (GtkPlotCanvasClass, click_on_plot),
                    gtk_plot_canvas_marshal_BOOL__POINTER,
                    GTK_TYPE_BOOL, 1, GTK_TYPE_GDK_EVENT);

  canvas_signals[CLICK_POINT] =
    gtk_signal_new ("click_on_point",
                    GTK_RUN_LAST,
                    object_class->type,
                    GTK_SIGNAL_OFFSET (GtkPlotCanvasClass, click_on_point),
                    gtk_plot_canvas_marshal_BOOL__POINTER,
                    GTK_TYPE_BOOL, 1, GTK_TYPE_GDK_EVENT);

  canvas_signals[CLICK_LEGENDS] =
    gtk_signal_new ("click_on_legends",
                    GTK_RUN_LAST,
                    object_class->type,
                    GTK_SIGNAL_OFFSET (GtkPlotCanvasClass, click_on_legends),
                    gtk_plot_canvas_marshal_BOOL__POINTER,
                    GTK_TYPE_BOOL, 1, GTK_TYPE_GDK_EVENT);

  canvas_signals[CLICK_TEXT] =
    gtk_signal_new ("click_on_text",
                    GTK_RUN_LAST,
                    object_class->type,
                    GTK_SIGNAL_OFFSET (GtkPlotCanvasClass, click_on_text),
                    gtk_plot_canvas_marshal_BOOL__POINTER,
                    GTK_TYPE_BOOL, 1, GTK_TYPE_GDK_EVENT);

  canvas_signals[CLICK_TITLE] =
    gtk_signal_new ("click_on_title",
                    GTK_RUN_LAST,
                    object_class->type,
                    GTK_SIGNAL_OFFSET (GtkPlotCanvasClass, click_on_title),
                    gtk_plot_canvas_marshal_BOOL__POINTER_POINTER,
                    GTK_TYPE_BOOL, 2, GTK_TYPE_GDK_EVENT, GTK_TYPE_POINTER);

  canvas_signals[CLICK_AXIS] =
    gtk_signal_new ("click_on_axis",
                    GTK_RUN_LAST,
                    object_class->type,
                    GTK_SIGNAL_OFFSET (GtkPlotCanvasClass, click_on_axis),
                    gtk_plot_canvas_marshal_BOOL__POINTER_POINTER,
                    GTK_TYPE_BOOL, 2, GTK_TYPE_GDK_EVENT, GTK_TYPE_POINTER);

  canvas_signals[SELECT_REGION] =
    gtk_signal_new ("select_region",
                    GTK_RUN_LAST,
                    object_class->type,
                    GTK_SIGNAL_OFFSET (GtkPlotCanvasClass, click_on_axis),
                    gtk_plot_canvas_marshal_select,
                    GTK_TYPE_NONE, 4, 
                    GTK_TYPE_DOUBLE, GTK_TYPE_DOUBLE,
                    GTK_TYPE_DOUBLE, GTK_TYPE_DOUBLE);

  canvas_signals[MOVE_TEXT] =
    gtk_signal_new ("move_text",
                    GTK_RUN_LAST,
                    object_class->type,
                    GTK_SIGNAL_OFFSET (GtkPlotCanvasClass, move_text),
                    gtk_plot_canvas_marshal_BOOL__DOUBLE_DOUBLE,
                    GTK_TYPE_BOOL, 2, GTK_TYPE_DOUBLE, GTK_TYPE_DOUBLE);

  canvas_signals[MOVE_LEGENDS] =
    gtk_signal_new ("move_legends",
                    GTK_RUN_LAST,
                    object_class->type,
                    GTK_SIGNAL_OFFSET (GtkPlotCanvasClass, move_legends),
                    gtk_plot_canvas_marshal_BOOL__DOUBLE_DOUBLE,
                    GTK_TYPE_BOOL, 2, GTK_TYPE_DOUBLE, GTK_TYPE_DOUBLE);

  canvas_signals[MOVE_PLOT] =
    gtk_signal_new ("move_plot",
                    GTK_RUN_LAST,
                    object_class->type,
                    GTK_SIGNAL_OFFSET (GtkPlotCanvasClass, move_plot),
                    gtk_plot_canvas_marshal_BOOL__DOUBLE_DOUBLE,
                    GTK_TYPE_BOOL, 2, GTK_TYPE_DOUBLE, GTK_TYPE_DOUBLE);

  canvas_signals[RESIZE_PLOT] =
    gtk_signal_new ("resize_plot",
                    GTK_RUN_LAST,
                    object_class->type,
                    GTK_SIGNAL_OFFSET (GtkPlotCanvasClass, resize_plot),
                    gtk_plot_canvas_marshal_BOOL__DOUBLE_DOUBLE,
                    GTK_TYPE_BOOL, 2, GTK_TYPE_DOUBLE, GTK_TYPE_DOUBLE);

  gtk_object_class_add_signals (object_class, canvas_signals, LAST_SIGNAL);


  object_class->finalize = gtk_plot_canvas_finalize;

  widget_class->motion_notify_event = gtk_plot_canvas_motion;
  widget_class->button_press_event = gtk_plot_canvas_button_press;
  widget_class->button_release_event = gtk_plot_canvas_button_release;

  class->click_on_plot = NULL;
  class->click_on_text = NULL;
  class->click_on_title = NULL;
  class->click_on_legends = NULL;
  class->click_on_axis = NULL;
  class->click_on_point = NULL;
}

static void
gtk_plot_canvas_marshal_BOOL__POINTER         (GtkObject *object,
                                               GtkSignalFunc func,
                                               gpointer func_data,
                                               GtkArg * args)
{
  GtkPlotCanvasSignal1 rfunc;
  gboolean *veto;
  veto = GTK_RETLOC_BOOL (args[1]);

  rfunc = (GtkPlotCanvasSignal1) func;

  *veto = (*rfunc) (object, 
                    GTK_VALUE_POINTER (args[0]),
                    func_data);
}

static void
gtk_plot_canvas_marshal_BOOL__POINTER_POINTER         (GtkObject *object,
                                                       GtkSignalFunc func,
                                                       gpointer func_data,
                                                       GtkArg * args)
{
  GtkPlotCanvasSignal2 rfunc;
  gboolean *veto;
  veto = GTK_RETLOC_BOOL (args[2]);

  rfunc = (GtkPlotCanvasSignal2) func;

  *veto = (*rfunc) (object, 
                    GTK_VALUE_POINTER (args[0]),
                    GTK_VALUE_POINTER (args[1]),
                    func_data);
}

static void
gtk_plot_canvas_marshal_BOOL__DOUBLE_DOUBLE	      (GtkObject *object,
                                                       GtkSignalFunc func,
                                                       gpointer func_data,
                                                       GtkArg * args)
{
  GtkPlotCanvasSignal4 rfunc;
  gboolean *veto;
  veto = GTK_RETLOC_BOOL (args[2]);

  rfunc = (GtkPlotCanvasSignal4) func;

  *veto = (*rfunc) (object, 
                    GTK_VALUE_DOUBLE (args[0]),
                    GTK_VALUE_DOUBLE (args[1]),
                    func_data);
}


static void
gtk_plot_canvas_marshal_select			      (GtkObject *object,
                                                       GtkSignalFunc func,
                                                       gpointer func_data,
                                                       GtkArg * args)
{
  GtkPlotCanvasSignal3 rfunc;

  rfunc = (GtkPlotCanvasSignal3) func;

  (*rfunc) (object, 
            GTK_VALUE_DOUBLE (args[0]),
            GTK_VALUE_DOUBLE (args[1]),
            GTK_VALUE_DOUBLE (args[2]),
            GTK_VALUE_DOUBLE (args[3]),
            func_data);
}

static void
gtk_plot_canvas_init (GtkPlotCanvas *plot_canvas)
{
  GtkWidget *widget;
  widget = GTK_WIDGET(plot_canvas);

  plot_canvas->flags = 0;
  plot_canvas->action = GTK_PLOT_CANVAS_INACTIVE;

  plot_canvas->active_plot = NULL;
  plot_canvas->active_data = NULL;
  plot_canvas->active_point = -1;
  plot_canvas->active_text = NULL;

  plot_canvas->drag_x = plot_canvas->drag_y = 0;
  plot_canvas->pointer_x = plot_canvas->pointer_y = 0;
}

GtkWidget*
gtk_plot_canvas_new (gint width, gint height)
{
  GtkPlotCanvas *plot_canvas;

  plot_canvas = gtk_type_new (gtk_plot_canvas_get_type ());
  GTK_PLOT_LAYOUT(plot_canvas)->width = width;
  GTK_PLOT_LAYOUT(plot_canvas)->height = height;
  return GTK_WIDGET (plot_canvas);
}


static void
gtk_plot_canvas_finalize (GtkObject *object)
{
  GtkPlotCanvas *plot_canvas;

  g_return_if_fail (object != NULL);
  g_return_if_fail (GTK_IS_PLOT_CANVAS (object));

  plot_canvas = GTK_PLOT_CANVAS (object);

  if (GTK_OBJECT_CLASS (parent_class)->finalize)
    (*GTK_OBJECT_CLASS (parent_class)->finalize) (object);
}

static gint 
gtk_plot_canvas_motion (GtkWidget *widget, GdkEventMotion *event)
{
  GtkPlotCanvas *canvas;
  GtkPlot *active_plot;
  GtkPlotData *active_dataset;
  gint active_point;
  GtkPlotText *active_text;
  GtkAllocation *allocation;
  GtkAllocation internal_allocation;
  GtkAllocation legends_allocation;
  GdkDrawable *drawable;
  GdkGC *xor_gc;
  GdkGCValues values;
  GdkColor color;
  gint x,y;
  gint text_x, text_y;
  gint old_x, old_y;
  gint new_x, new_y;
  gint old_width, old_height;
  gint new_width, new_height;
  gint ascent, descent;
  gdouble px, py;

  canvas = GTK_PLOT_CANVAS(widget);
  active_plot = canvas->active_plot;
  active_dataset = canvas->active_data;
  active_point = canvas->active_point;
  active_text = canvas->active_text;
 
  if(canvas->action == GTK_PLOT_CANVAS_INACTIVE) return TRUE;
  if(active_plot == NULL) return TRUE;
 
  allocation = &GTK_WIDGET(canvas->active_plot)->allocation;
  gdk_gc_get_values(widget->style->fg_gc[0], &values);
  values.function = GDK_INVERT;
  values.foreground = widget->style->white;
  values.subwindow_mode = GDK_INCLUDE_INFERIORS;
  xor_gc = gdk_gc_new_with_values(widget->window,
                                  &values,
                                  GDK_GC_FOREGROUND |
                                  GDK_GC_FUNCTION |
                                  GDK_GC_SUBWINDOW);
  gdk_color_parse("red", &color);
  gdk_color_alloc(gtk_widget_get_colormap(widget), &color);

  gdk_gc_set_foreground(xor_gc, &color);

  gtk_widget_get_pointer(widget, &x, &y);

  if(active_text != NULL)
     gtk_plot_canvas_get_pixel(GTK_WIDGET(active_plot), 
  			       active_text->x, active_text->y,
                               &text_x, &text_y);

  switch(canvas->action){
    case GTK_PLOT_CANVAS_MOVE_TEXT:
         gtk_plot_layout_get_pixel(GTK_PLOT_LAYOUT(widget), 
			           active_text->x, active_text->y,
                                   &text_x, &text_y);
         text_x -= GTK_LAYOUT(widget)->xoffset;
         text_y -= GTK_LAYOUT(widget)->yoffset;
    case GTK_PLOT_CANVAS_MOVE_TITLE:
         old_x = text_x + (canvas->pointer_x - canvas->drag_x); 
         old_y = text_y + (canvas->pointer_y - canvas->drag_y); 
         gtk_plot_text_get_size(*active_text, &old_width, &old_height,
                                &ascent, &descent);

         switch(active_text->justification){
          case GTK_JUSTIFY_LEFT:
           switch(active_text->angle){
             case 0:
                 old_y -= ascent;
                 break;
             case 90:
                 old_y -= old_height;
                 old_x -= ascent;
                 break;
             case 180:
                 old_x -= old_width;
                 old_y -= descent;
                 break;
             case 270:
                 old_x -= descent;
                 break;
           }
           break;
         case GTK_JUSTIFY_RIGHT:
           switch(active_text->angle){
             case 0:
                 old_x -= old_width;
                 old_y -= ascent;
                 break;
             case 90:
                 old_x -= ascent;
                 break;
             case 180:
                 old_y -= descent;
                 break;
             case 270:
                 old_y -= old_height;
                 old_x -= descent;
                 break;
           }
           break;
         case GTK_JUSTIFY_CENTER:
         default:
           switch(active_text->angle){
             case 0:
                 old_x -= old_width / 2.;
                 old_y -= ascent;
                 break;
             case 90:
                 old_x -= ascent;
                 old_y -= old_height / 2.;
                 break;
             case 180:
                 old_x -= old_width / 2.;
                 old_y -= descent;
                 break;
             case 270:
                 old_x -= descent;
                 old_y -= old_height / 2.;
                 break;
           }
         }

         gdk_draw_rectangle (GTK_LAYOUT(canvas)->bin_window,
                             xor_gc,
                             FALSE,
                             old_x, old_y,
                             old_width, old_height);         
         new_x = old_x + x - canvas->pointer_x; 
         new_y = old_y + y - canvas->pointer_y; 
         gdk_draw_rectangle (GTK_LAYOUT(canvas)->bin_window,
                             xor_gc,
                             FALSE,
                             new_x, new_y,
                             old_width, old_height);         
         canvas->pointer_x = x;
         canvas->pointer_y = y;
	 break;
    case GTK_PLOT_CANVAS_MOVE_LEGENDS:
         legends_allocation = gtk_plot_legends_get_allocation (active_plot);
         old_x = legends_allocation.x + (canvas->pointer_x - canvas->drag_x); 
         old_y = legends_allocation.y + (canvas->pointer_y - canvas->drag_y); 
         gdk_draw_rectangle (GTK_LAYOUT(canvas)->bin_window,
                             xor_gc,
                             FALSE,
                             old_x, old_y,
                             legends_allocation.width, 
                             legends_allocation.height);         
         new_x = legends_allocation.x + (x - canvas->drag_x); 
         new_y = legends_allocation.y + (y - canvas->drag_y); 
         gdk_draw_rectangle (GTK_LAYOUT(canvas)->bin_window,
                             xor_gc,
                             FALSE,
                             new_x, new_y,
                             legends_allocation.width, 
                             legends_allocation.height);         
         canvas->pointer_x = x;
         canvas->pointer_y = y;
         break;
    case GTK_PLOT_CANVAS_MOVE_PLOT:
         internal_allocation = gtk_plot_get_internal_allocation (active_plot);
         old_x = internal_allocation.x + (canvas->pointer_x - canvas->drag_x); 
         old_y = internal_allocation.y + (canvas->pointer_y - canvas->drag_y); 
         gdk_draw_rectangle (GTK_LAYOUT(canvas)->bin_window,
                             xor_gc,
                             FALSE,
                             old_x, old_y,
                             internal_allocation.width, 
                             internal_allocation.height);         
         new_x = internal_allocation.x + (x - canvas->drag_x); 
         new_y = internal_allocation.y + (y - canvas->drag_y); 
         gdk_draw_rectangle (GTK_LAYOUT(canvas)->bin_window,
                             xor_gc,
                             FALSE,
                             new_x, new_y,
                             internal_allocation.width, 
                             internal_allocation.height);         
         canvas->pointer_x = x;
         canvas->pointer_y = y;
         break;
    case GTK_PLOT_CANVAS_RESIZE_PLOT:
         internal_allocation = gtk_plot_get_internal_allocation (active_plot);
         old_width = internal_allocation.width + (canvas->pointer_x - canvas->drag_x); 
         old_height = internal_allocation.height + (canvas->pointer_y - canvas->drag_y); 
         gdk_draw_rectangle (GTK_LAYOUT(canvas)->bin_window,
                             xor_gc,
                             FALSE,
                             internal_allocation.x, 
                             internal_allocation.y,
                             old_width, old_height);         
         gdk_draw_rectangle (GTK_LAYOUT(canvas)->bin_window,
                             xor_gc,
                             TRUE,
                             internal_allocation.x+old_width-10, 
                             internal_allocation.y+old_height-10,
                             10, 10);         
         new_width = internal_allocation.width + (x - canvas->drag_x); 
         new_height = internal_allocation.height + (y - canvas->drag_y); 
         gdk_draw_rectangle (GTK_LAYOUT(canvas)->bin_window,
                             xor_gc,
                             FALSE,
                             internal_allocation.x, internal_allocation.y,
                             new_width, new_height);         
         gdk_draw_rectangle (GTK_LAYOUT(canvas)->bin_window,
                             xor_gc,
                             TRUE,
                             internal_allocation.x+new_width-10, 
                             internal_allocation.y+new_height-10,
                             10, 10);         
         canvas->pointer_x = x;
         canvas->pointer_y = y;
 	 break;
    case GTK_PLOT_CANVAS_DND_POINT:
         if(active_dataset == NULL) break; 
         if(active_point == -1) break; 

         gdk_gc_set_foreground(xor_gc, &widget->style->white);

	 drawable = active_plot->drawable;
         gtk_plot_set_drawable(active_plot, GTK_LAYOUT(canvas)->bin_window);

	 gtk_plot_get_point(active_plot, x, y, &px, &py);

	 gtk_plot_draw_dataset(active_plot, xor_gc, active_dataset);

	 active_dataset->x[active_point] = px;
	 active_dataset->y[active_point] = py;

	 gtk_plot_draw_dataset(active_plot, xor_gc, active_dataset);
	 gtk_plot_set_drawable(active_plot, drawable);

         break;
    case GTK_PLOT_CANVAS_IN_SELECTION:
         old_width = abs(canvas->pointer_x - canvas->drag_x);
         old_height = abs(canvas->pointer_y - canvas->drag_y);
         new_width = abs(x - canvas->drag_x);
         new_height = abs(y - canvas->drag_y);
         gdk_draw_rectangle (GTK_LAYOUT(canvas)->bin_window,
                             xor_gc,
                             FALSE,
                             MIN(canvas->pointer_x, canvas->drag_x), 
                             MIN(canvas->pointer_y, canvas->drag_y), 
                             old_width, old_height);         
         gdk_draw_rectangle (GTK_LAYOUT(canvas)->bin_window,
                             xor_gc,
                             FALSE,
                             MIN(x, canvas->drag_x), 
                             MIN(y, canvas->drag_y), 
                             new_width, new_height);         
         canvas->pointer_x = x;
         canvas->pointer_y = y;
 	 break;
  }

  gdk_gc_unref(xor_gc);
  return TRUE;

}

static gint
gtk_plot_canvas_button_press(GtkWidget *widget, GdkEventButton *event)
{
  GtkPlotCanvas *canvas = NULL;
  GtkPlot *active_plot = NULL;
  GList *plots = NULL;
  GList *dataset = NULL;
  GList *text = NULL;
  GtkPlot *plot = NULL;
  GtkPlot *click_plot = NULL;
  GtkPlotData *data = NULL;
  GtkPlotText *child_text = NULL;
  GtkAllocation internal_allocation;
  GtkAllocation legends_allocation;
  GtkPlotAxis *axis[4];
  GtkPlotAxis *active_axis = NULL;
  GdkGC *xor_gc;
  GdkModifierType mods;
  GdkGCValues values;
  GdkColor color;
  gint i = 0;
  gint x = 0, y = 0;
  gint xi = 0, yi = 0;
  gint px = 0, py = 0;
  gint pwidth = 0, pheight = 0;
  gint lx = 0, ly = 0;
  gint lwidth = 0, lheight = 0;
  gint tx = 0, ty = 0;
  gint twidth = 0, theight = 0;
  gint tascent, tdescent;
  gboolean veto;

  gdk_window_get_pointer(widget->window, NULL, NULL, &mods);
  if(!(mods & GDK_BUTTON1_MASK)) return FALSE;
 
  canvas = GTK_PLOT_CANVAS(widget);
  active_plot = canvas->active_plot;
  canvas->active_data = NULL;
  canvas->active_point = -1;
  canvas->active_text = NULL;

  canvas->action = GTK_PLOT_CANVAS_INACTIVE;

  gdk_pointer_ungrab(event->time);

  gdk_gc_get_values(widget->style->fg_gc[0], &values);
  values.function = GDK_INVERT;
  values.foreground = widget->style->white;
  values.subwindow_mode = GDK_INCLUDE_INFERIORS;
  xor_gc = gdk_gc_new_with_values(widget->window,
                                  &values,
                                  GDK_GC_FOREGROUND |
                                  GDK_GC_FUNCTION |
                                  GDK_GC_SUBWINDOW);
  gdk_color_parse("red", &color);
  gdk_color_alloc(gtk_widget_get_colormap(widget), &color);
  gdk_gc_set_foreground(xor_gc, &color);
 
  gtk_widget_get_pointer(widget, &x, &y);

  text = GTK_PLOT_LAYOUT(canvas)->text;

  while(text)
     {
       child_text = (GtkPlotText *)text->data;
       gtk_plot_layout_get_pixel(GTK_PLOT_LAYOUT(widget), 
                                 child_text->x, child_text->y,
                                 &tx, &ty);

       tx -= GTK_LAYOUT(widget)->xoffset;
       ty -= GTK_LAYOUT(widget)->yoffset;

       gtk_plot_text_get_size(*child_text, &twidth, &theight, 
                               &tascent, &tdescent);

       switch(child_text->justification){
          case GTK_JUSTIFY_LEFT:
           switch(child_text->angle){
             case 0:
                 ty -= tascent;
                 break;
             case 90:
                 ty -= theight;
                 tx -= tascent;
                 break;
             case 180:
                 tx -= twidth;
                 ty -= tdescent;
                 break;
             case 270:
                 tx -= tdescent;
                 break;
           }
           break;
         case GTK_JUSTIFY_RIGHT:
           switch(child_text->angle){
             case 0:
                 tx -= twidth;
                 ty -= tascent;
                 break;
             case 90:
                 tx -= tascent;
                 break;
             case 180:
                 ty -= tdescent;
                 break;
             case 270:
                 ty -= theight;
                 tx -= tdescent;
                 break;
           }
           break;
         case GTK_JUSTIFY_CENTER:
         default:
           switch(child_text->angle){
             case 0:
                 tx -= twidth / 2.;
                 ty -= tascent;
                 break;
             case 90:
                 tx -= tascent;
                 ty -= theight / 2.;
                 break;
             case 180:
                 tx -= twidth / 2.;
                 ty -= tdescent;
                 break;
             case 270:
                 tx -= tdescent;
                 ty -= theight / 2.;
                 break;
           }
       }

       if(x >= tx-5 && x <= tx+twidth+5)
         if(y >= ty-5 && y <= ty+theight+5)
           {
              canvas->action = GTK_PLOT_CANVAS_MOVE_TEXT;
              canvas->active_text = child_text;
              break;
           }

       text = text->next;
     }

  if (canvas->action ==  GTK_PLOT_CANVAS_MOVE_TEXT){
          canvas->drag_x = x;
          canvas->drag_y = y;
          canvas->pointer_x = x;
          canvas->pointer_y = y;
          veto = TRUE;
          gtk_signal_emit(GTK_OBJECT(canvas), canvas_signals[CLICK_TEXT],
                          event, &veto);
          if(!veto || (!GTK_PLOT_CANVAS_CAN_MOVE_TEXT(canvas))){ 
                   gdk_pointer_ungrab(event->time);
                   canvas->action = GTK_PLOT_CANVAS_INACTIVE;
          }else{
                   gdk_draw_rectangle (GTK_LAYOUT(canvas)->bin_window,
                                       xor_gc,
                                       FALSE,
                                       tx, ty,
                                       twidth, theight);         
                   gdk_pointer_grab (GTK_LAYOUT(widget)->bin_window, FALSE,
                                     GDK_POINTER_MOTION_HINT_MASK |
                                     GDK_BUTTON1_MOTION_MASK |
                                     GDK_BUTTON_RELEASE_MASK,
                                     NULL, NULL, event->time);
                   gdk_gc_unref(xor_gc);
                   return TRUE;
          }

  }

  plots = GTK_PLOT_LAYOUT(canvas)->plots;
  while(plots)
     {
       plot = (GtkPlot *)plots->data;
  
       axis[0]=&plot->left;
       axis[1]=&plot->right;
       axis[2]=&plot->top;
       axis[3]=&plot->bottom;
 
       for(i = 0; i <= 3; i++){
          if(axis[i]->title_visible){
            child_text = &axis[i]->title;
            gtk_plot_canvas_get_pixel(GTK_WIDGET(plot), 
                                     child_text->x, child_text->y,
                                      &tx, &ty);
            gtk_plot_text_get_size(*child_text, &twidth, &theight,
                                   &tascent, &tdescent);

            switch(child_text->justification){
               case GTK_JUSTIFY_LEFT:
                switch(child_text->angle){
                  case 0:
                      ty -= tascent;
                      break;
                  case 90:
                      ty -= theight;
                      tx -= tascent;
                      break;
                  case 180:
                      tx -= twidth;
                      ty -= tdescent;
                      break;
                  case 270:
                      tx -= tdescent;
                      break;
                }
                break;
              case GTK_JUSTIFY_RIGHT:
                switch(child_text->angle){
                  case 0:
                      tx -= twidth;
                      ty -= tascent;
                      break;
                  case 90:
                      tx -= tascent;
                      break;
                  case 180:
                      ty -= tdescent;
                      break;
                  case 270:
                      ty -= theight;
                      tx -= tdescent;
                      break;
                }
                break;
              case GTK_JUSTIFY_CENTER:
              default:
                switch(child_text->angle){
                  case 0:
                      tx -= twidth / 2.;
                      ty -= tascent;
                      break;
                  case 90:
                      tx -= tascent;
                      ty -= theight / 2.;
                      break;
                  case 180:
                      tx -= twidth / 2.;
                      ty -= tdescent;
                      break;
                  case 270:
                      tx -= tdescent;
                      ty -= theight / 2.;
                      break;
                }
             }


            if(x >= tx-5 && x <= tx+twidth+5)
               if(y >= ty-5 && y <= ty+theight+5)
                  {
                      active_axis = axis[i];
                      canvas->action = GTK_PLOT_CANVAS_MOVE_TITLE;
                      canvas->active_text = &axis[i]->title;
                      click_plot = plot;
                      break;
                  }
          }
         }
 
       if(canvas->action == GTK_PLOT_CANVAS_MOVE_TITLE) break;
       plots = plots->next;
     }

  if(canvas->action == GTK_PLOT_CANVAS_MOVE_TITLE){
         canvas->drag_x = x;
         canvas->drag_y = y;
 	 canvas->pointer_x = x;
 	 canvas->pointer_y = y;
         canvas->active_plot = click_plot;
         veto = TRUE;
         gtk_signal_emit(GTK_OBJECT(canvas), canvas_signals[CLICK_TITLE],
                         event, active_axis, &veto);
         if(!veto || (!GTK_PLOT_CANVAS_CAN_MOVE_TITLES(canvas))){ 
                  canvas->action = GTK_PLOT_CANVAS_INACTIVE;
                  gdk_pointer_ungrab(event->time);
         }else{
                  gdk_draw_rectangle (GTK_LAYOUT(canvas)->bin_window,
                                      xor_gc,
                                      FALSE,
                                      tx, ty,
                                      twidth, theight);         
                  gdk_pointer_grab (GTK_LAYOUT(widget)->bin_window, FALSE,
                                    GDK_POINTER_MOTION_HINT_MASK |
                                    GDK_BUTTON1_MOTION_MASK |
                                    GDK_BUTTON_RELEASE_MASK,
                                    NULL, NULL, event->time);
                  gdk_gc_unref(xor_gc);
                  return TRUE;
         }

  }
 
  plots = GTK_PLOT_LAYOUT(canvas)->plots;
  while(plots)
     {
       plot = (GtkPlot *)plots->data;
  
       legends_allocation = gtk_plot_legends_get_allocation (plot);
       lx = legends_allocation.x;
       ly = legends_allocation.y;
       lwidth = legends_allocation.width;
       lheight = legends_allocation.height;
 
       if(GTK_PLOT_CANVAS_CAN_MOVE_LEGENDS(canvas)) 
         if(x >= lx-5 && x <= lx+lwidth+5)
           if(y >= ly-5 && y <= ly+lheight+5){
                canvas->action = GTK_PLOT_CANVAS_MOVE_LEGENDS;
                click_plot = plot;
                break;
           }
 
       plots=plots->next;
     } 

  if(canvas->action == GTK_PLOT_CANVAS_MOVE_LEGENDS){
         canvas->drag_x = x;
         canvas->drag_y = y;
 	 canvas->pointer_x = x;
 	 canvas->pointer_y = y;
         canvas->active_plot = click_plot;
         veto = TRUE;
         gtk_signal_emit(GTK_OBJECT(canvas), canvas_signals[CLICK_LEGENDS],
                         event, &veto);
         if(!veto || (!GTK_PLOT_CANVAS_CAN_MOVE_LEGENDS(canvas))){
                  canvas->action = GTK_PLOT_CANVAS_INACTIVE;
                  gdk_pointer_ungrab(event->time);
         }else{
                  gdk_draw_rectangle (GTK_LAYOUT(canvas)->bin_window,
                                      xor_gc,
                                      FALSE,
                                      lx, ly,
                                      lwidth, lheight);         
                  gdk_pointer_grab (GTK_LAYOUT(widget)->bin_window, FALSE,
                                    GDK_POINTER_MOTION_HINT_MASK |
                                    GDK_BUTTON1_MOTION_MASK |
                                    GDK_BUTTON_RELEASE_MASK,
                                    NULL, NULL, event->time);
                  gdk_gc_unref(xor_gc);
                  return TRUE;
         }
  }
 
  plots = GTK_PLOT_LAYOUT(canvas)->plots;
  while(plots)
     {
       plot = (GtkPlot *)plots->data;

       internal_allocation = gtk_plot_get_internal_allocation (plot);
       px = internal_allocation.x;
       py = internal_allocation.y;
       pwidth = internal_allocation.width;
       pheight = internal_allocation.height;
       if(x >= px && x <= px + pwidth && y >= py && py <= py + pheight)
            click_plot = plot;

       if(x >= px-5 && x <= px+pwidth+5)
        {
            if(y >= py-5 && y <= py+5) 
                                     active_axis = &active_plot->top;
            if(y >= py+pheight-5 && y <= py+pheight+5) 
                                     active_axis = &active_plot->bottom;
        }
       if(y >= py-5 && y <= py+pheight+5)
        {
            if(x >= px-5 && x <= px+5) 
                                     active_axis = &active_plot->left;
            if(x >= px+pwidth-5 && x <= px+pwidth+5)
                                     active_axis = &active_plot->right;
        }
  
       if(active_axis != NULL) 
               canvas->action = GTK_PLOT_CANVAS_MOVE_PLOT;
   
       if((y >= py+pheight-5 && y <= py+pheight+5) && 
            (x >= px+pwidth-5 && x <= px+pwidth+5))
                  canvas->action = GTK_PLOT_CANVAS_RESIZE_PLOT;
  
       if(canvas->action != GTK_PLOT_CANVAS_INACTIVE){
               click_plot = plot;
               break;
       }

       plots=plots->next;
     } 

  if(canvas->action == GTK_PLOT_CANVAS_MOVE_PLOT ||
     canvas->action == GTK_PLOT_CANVAS_RESIZE_PLOT){
         canvas->active_plot = click_plot;
         canvas->drag_x = x;
         canvas->drag_y = y;
         canvas->pointer_x = x;
         canvas->pointer_y = y;
         if(!veto ||
            (canvas->action == GTK_PLOT_CANVAS_MOVE_PLOT &&
            (!GTK_PLOT_CANVAS_CAN_MOVE_PLOT(canvas))) ||
            (canvas->action == GTK_PLOT_CANVAS_RESIZE_PLOT &&
            (!GTK_PLOT_CANVAS_CAN_RESIZE_PLOT(canvas))))
         {
                 canvas->action = GTK_PLOT_CANVAS_INACTIVE;
                 gdk_pointer_ungrab(event->time);
         }else{
                 gdk_draw_rectangle (GTK_LAYOUT(canvas)->bin_window,
                                     xor_gc,
                                     FALSE,
                                     px, py,
                                     pwidth, pheight);         
                 if(canvas->action == GTK_PLOT_CANVAS_RESIZE_PLOT)
                        gdk_draw_rectangle (GTK_LAYOUT(canvas)->bin_window,
                                            xor_gc,
                                            TRUE,
                                            px+pwidth-10, py+pheight-10,
                                            10, 10);         
                 gdk_pointer_grab (GTK_LAYOUT(widget)->bin_window, FALSE,
                                   GDK_POINTER_MOTION_HINT_MASK |
                                   GDK_BUTTON1_MOTION_MASK |
                                   GDK_BUTTON_RELEASE_MASK,
                                   NULL, NULL, event->time);
                 gdk_gc_unref(xor_gc);
                 return TRUE;
         }
  }


  if(canvas->active_plot == NULL) {
         if(click_plot != NULL){
           canvas->active_plot = click_plot;
         }else{
           gdk_gc_unref(xor_gc);
           return TRUE;
         }
  }

  dataset = active_plot->data_sets;

  while(dataset)
     {
       data = (GtkPlotData *)dataset->data;
       for(i=0; i<data->num_points; i++)
        {
         gtk_plot_get_pixel(active_plot, data->x[i], data->y[i], &xi, &yi);
         if(abs(xi-x) <= 5 && abs(yi-y) <= 5)
          {
              canvas->active_data = data;
              canvas->active_point = i;
              canvas->active_x = data->x[i];
              canvas->active_y = data->y[i];
              canvas->action = GTK_PLOT_CANVAS_DND_POINT;
          }
        }
       dataset = dataset->next;
     }

  if(canvas->action == GTK_PLOT_CANVAS_DND_POINT){
         if(!GTK_PLOT_CANVAS_CAN_DND_POINT(canvas))
                     canvas->action = GTK_PLOT_CANVAS_INACTIVE;
         canvas->active_plot = click_plot;
         veto = TRUE;
         gtk_signal_emit(GTK_OBJECT(canvas), canvas_signals[CLICK_POINT],
                         event, &veto);
         if(!veto){
                 canvas->action = GTK_PLOT_CANVAS_INACTIVE;
                 gdk_pointer_ungrab(event->time);
         } 
         else 
         {
                 if(canvas->action == GTK_PLOT_CANVAS_DND_POINT){
                     gdk_pointer_grab (GTK_LAYOUT(widget)->bin_window, FALSE,
                                       GDK_POINTER_MOTION_HINT_MASK |
                                       GDK_BUTTON1_MOTION_MASK |
                                       GDK_BUTTON_RELEASE_MASK,
                                       NULL, NULL, event->time);
                     gdk_gc_unref(xor_gc);
                     return TRUE;
                 }
         }
  }

  if(canvas->action == GTK_PLOT_CANVAS_INACTIVE){
   plots = GTK_PLOT_LAYOUT(canvas)->plots;
   while(plots)
     {
       plot = (GtkPlot *)plots->data;
       internal_allocation = gtk_plot_get_internal_allocation (plot);
       px = internal_allocation.x;
       py = internal_allocation.y;
       pwidth = internal_allocation.width;
       pheight = internal_allocation.height;

       if(abs(x - px) <= 5 && y >= py+5 && y <= py+pheight-5){ 
         veto = TRUE;
         gtk_signal_emit(GTK_OBJECT(canvas), canvas_signals[CLICK_AXIS],
                         event, &plot->left);
         gdk_gc_unref(xor_gc);
         return TRUE;
       }
       if(abs(x - (px+pwidth)) <= 5 && y >= py+5 && y <= py+pheight-5){ 
         veto = TRUE;
         gtk_signal_emit(GTK_OBJECT(canvas), canvas_signals[CLICK_AXIS],
                         event, &plot->right);
         gdk_gc_unref(xor_gc);
         return TRUE;
       }
       if(abs(y - py) <= 5 && x >= px+5 && x <= px+pwidth-5){ 
         veto = TRUE;
         gtk_signal_emit(GTK_OBJECT(canvas), canvas_signals[CLICK_AXIS],
                         event, &plot->top);
         gdk_gc_unref(xor_gc);
         return TRUE;
       }
       if(abs(y - (py+pheight)) <= 5 && x >= px+5 && x <= px+pwidth-5){ 
         veto = TRUE;
         gtk_signal_emit(GTK_OBJECT(canvas), canvas_signals[CLICK_AXIS],
                         event, &plot->bottom);
         gdk_gc_unref(xor_gc);
         return TRUE;
       }

       plots = plots->next;
     }

   if(click_plot != NULL) {
         canvas->active_plot = click_plot;
         veto = FALSE;
         gtk_signal_emit(GTK_OBJECT(canvas), canvas_signals[CLICK_PLOT],
                         event, &veto);
         if(!veto){
             gdk_pointer_ungrab(event->time);
             gdk_gc_unref(xor_gc);
             return TRUE;
         }

         canvas->drag_x = x;
         canvas->drag_y = y;
         canvas->pointer_x = x;
         canvas->pointer_y = y;

         canvas->action = GTK_PLOT_CANVAS_IN_SELECTION;

         gdk_draw_point(GTK_LAYOUT(canvas)->bin_window, xor_gc, x, y);
         gdk_pointer_grab (GTK_LAYOUT(widget)->bin_window, FALSE,
                           GDK_POINTER_MOTION_HINT_MASK |
                           GDK_BUTTON1_MOTION_MASK |
                           GDK_BUTTON_RELEASE_MASK,
                           NULL, NULL, event->time);

   }
  
  }

  gdk_gc_unref(xor_gc);
  return TRUE;
}
 
static gint
gtk_plot_canvas_button_release(GtkWidget *widget, GdkEventButton *event)
{
  GtkPlotCanvas *canvas;
  GtkPlot *active_plot;
  gdouble new_x, new_y;
  gdouble dx, dy;
  gdouble new_width, new_height;
  gdouble xmin, xmax;
  gdouble ymin, ymax;
  gboolean veto = TRUE;

  canvas = GTK_PLOT_CANVAS(widget); 

  active_plot = canvas->active_plot;

  if(active_plot == NULL) return TRUE;

  gtk_plot_canvas_get_position(GTK_WIDGET(active_plot),
                               canvas->pointer_x - canvas->drag_x, 
                               canvas->pointer_y - canvas->drag_y, 
                               &dx, &dy);
 
  switch(canvas->action){
    case GTK_PLOT_CANVAS_MOVE_TEXT:
         gtk_plot_layout_get_position(GTK_PLOT_LAYOUT(canvas),
                                      canvas->pointer_x - canvas->drag_x, 
                                      canvas->pointer_y - canvas->drag_y, 
                                      &dx, &dy);
    case GTK_PLOT_CANVAS_MOVE_TITLE:
         gtk_signal_emit(GTK_OBJECT(canvas), canvas_signals[MOVE_TEXT],
                         canvas->active_text->x + dx, 
                         canvas->active_text->y + dy, &veto);
         if(!veto) break;
         canvas->active_text->x += dx;
         canvas->active_text->y += dy;
         break;
    case GTK_PLOT_CANVAS_MOVE_LEGENDS:
         gtk_plot_legends_get_position(active_plot, &new_x, &new_y);
         gtk_signal_emit(GTK_OBJECT(canvas), canvas_signals[MOVE_LEGENDS],
                         new_x + dx/active_plot->width, 
                         new_y + dy/active_plot->height,  &veto);
         if(!veto) break;
         new_x += dx / active_plot->width;
         new_y += dy / active_plot->height;
         gtk_plot_legends_move(active_plot, new_x, new_y);
         break;
    case GTK_PLOT_CANVAS_MOVE_PLOT:
         gtk_plot_get_position (active_plot, &new_x, &new_y);
         gtk_signal_emit(GTK_OBJECT(canvas), canvas_signals[MOVE_PLOT],
                         new_x + dx, new_y + dy,  &veto);
         if(!veto) break;
         new_x += dx;
         new_y += dy;
         gtk_plot_move(active_plot, new_x, new_y);
         break;
    case GTK_PLOT_CANVAS_RESIZE_PLOT:
         gtk_plot_get_size (active_plot, &new_width, &new_height);
         gtk_signal_emit(GTK_OBJECT(canvas), canvas_signals[RESIZE_PLOT],
                         new_x + dx, new_y + dy,  &veto);
         if(!veto) break;
         new_width += dx; 
         new_height += dy; 
         gtk_plot_resize(active_plot, new_width, new_height);
         break;
    case GTK_PLOT_CANVAS_IN_SELECTION:
	 gtk_plot_get_point(active_plot, 
                            MIN(canvas->drag_x, canvas->pointer_x),
                            MIN(canvas->drag_y, canvas->pointer_y),
                            &xmin, &ymax);
	 gtk_plot_get_point(active_plot, 
                            MAX(canvas->drag_x, canvas->pointer_x),
                            MAX(canvas->drag_y, canvas->pointer_y),
                            &xmax, &ymin);

         new_width = abs(canvas->pointer_x - canvas->drag_x);
         new_height = abs(canvas->pointer_y - canvas->drag_y);
         gtk_signal_emit(GTK_OBJECT(canvas), canvas_signals[SELECT_REGION],
                         xmin, xmax, ymin, ymax);
         break;
    case GTK_PLOT_CANVAS_INACTIVE:
         return TRUE;
  }
 

  if(veto)
      gtk_plot_layout_refresh(GTK_PLOT_LAYOUT(canvas));
 
  canvas->active_data = NULL;
  canvas->active_point = -1;
  canvas->active_text = NULL;
  canvas->action = GTK_PLOT_CANVAS_INACTIVE;
 
  gdk_pointer_ungrab(event->time);
  return TRUE;

}

void
gtk_plot_canvas_add_plot (GtkPlotCanvas *plot_canvas, 
			  GtkPlot *plot, gdouble x, gdouble y)
{
  gint width, height;

  width = GTK_PLOT_LAYOUT(plot_canvas)->width, 
  height = GTK_PLOT_LAYOUT(plot_canvas)->height;

  gtk_widget_set_usize(GTK_WIDGET(plot), width, height); 
  gtk_plot_move(plot, x, y);

  if(GTK_PLOT_CANVAS_ALLOCATE_TITLES(plot_canvas)){
    plot->left.title.x = plot->x - 45. / (gdouble)width;
    plot->right.title.x = plot->x + plot->width + 45. / (gdouble)width;
    plot->top.title.y = plot->y - 35. / (gdouble)height;
    plot->bottom.title.y = plot->y + plot->height + 35. / (gdouble)height;
  }

  gtk_plot_layout_add_plot(GTK_PLOT_LAYOUT(plot_canvas), plot, 0, 0);
  plot_canvas->active_plot = plot;
}

static void
gtk_plot_canvas_get_pixel(GtkWidget *widget, gdouble px, gdouble py,
                          gint *x, gint *y)
{
  *x = widget->allocation.x + widget->allocation.width * px;
  *y = widget->allocation.y + widget->allocation.height * py;
}

static void
gtk_plot_canvas_get_position(GtkWidget *widget, gint x, gint y,
                             gdouble *px, gdouble *py)
{
  *px = (gdouble) x / (gdouble) widget->allocation.width;
  *py = (gdouble) y / (gdouble) widget->allocation.height;
}

void
gtk_plot_canvas_set_active_plot (GtkPlotCanvas *plot_canvas, GtkPlot *plot)
{
  plot_canvas->active_plot = plot;
}

void
gtk_plot_canvas_cancel_action (GtkPlotCanvas *plot_canvas)
{
  if(plot_canvas->action == GTK_PLOT_CANVAS_DND_POINT){
      plot_canvas->active_data->x[plot_canvas->active_point] = 
                                                       plot_canvas->active_x; 
      plot_canvas->active_data->y[plot_canvas->active_point] = 
                                                       plot_canvas->active_y; 
  }

  plot_canvas->action = GTK_PLOT_CANVAS_INACTIVE;
  plot_canvas->active_text = NULL;

  gtk_widget_queue_draw(GTK_WIDGET(plot_canvas));
}

GtkPlot *
gtk_plot_canvas_get_active_plot(GtkPlotCanvas *canvas)
{
  return canvas->active_plot;
}


GtkPlotData *
gtk_plot_canvas_get_active_dataset(GtkPlotCanvas *canvas)
{
  return canvas->active_data;
}

void
gtk_plot_canvas_get_active_point(GtkPlotCanvas *canvas, gdouble *x, gdouble *y)
{
  x = &canvas->active_data->x[canvas->active_point];
  y = &canvas->active_data->y[canvas->active_point];
}

GtkPlotText *
gtk_plot_canvas_get_active_text(GtkPlotCanvas *canvas)
{
  return canvas->active_text;
}

void
gtk_plot_canvas_set_size(GtkPlotCanvas *canvas, gint width, gint height)
{
  GList *plots;
  GtkPlot *plot;

  plots = GTK_PLOT_LAYOUT(canvas)->plots;
  while(plots)
     {
       plot = (GtkPlot *)plots->data;
 
       gtk_widget_set_usize(GTK_WIDGET(plot), width, height); 

       plots = plots->next;
     }

  gtk_plot_layout_set_size(GTK_PLOT_LAYOUT(canvas), width, height);

}
