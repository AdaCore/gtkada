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
#include "gtkplotpc.h"
#include "gtkplotgdk.h"
#include "gtkplotps.h"
#include "gtkplot.h"
#include "gtkplot3d.h"
#include "gtkplotdata.h"
#include "gtkplotcanvas.h"
#include "gtkextra-marshal.h"

#define DEFAULT_WIDTH 100
#define DEFAULT_HEIGHT 150
#define DEFAULT_MARKER_SIZE 6
#define DEFAULT_FONT_HEIGHT 12
#define SHADOW_WIDTH 3 
#define GRAPH_MASK    (GDK_EXPOSURE_MASK |              \
                       GDK_POINTER_MOTION_MASK |        \
                       GDK_POINTER_MOTION_HINT_MASK |   \
                       GDK_BUTTON_PRESS_MASK |          \
                       GDK_BUTTON_RELEASE_MASK)


static gchar DEFAULT_FONT[] = "Helvetica";


static void gtk_plot_canvas_class_init 		(GtkPlotCanvasClass *klass);
static void gtk_plot_canvas_init 		(GtkPlotCanvas *plot_canvas);
static void gtk_plot_canvas_destroy 		(GtkObject *object);
static void gtk_plot_canvas_map                 (GtkWidget *widget);
static void gtk_plot_canvas_size_request        (GtkWidget *widget, 
                                                 GtkRequisition *requisition);
static gint gtk_plot_canvas_motion 		(GtkWidget *widget, 
                                                 GdkEventMotion *event);
static gint gtk_plot_canvas_button_press	(GtkWidget *widget, 
                                                 GdkEventButton *event);
static gint gtk_plot_canvas_button_release	(GtkWidget *widget, 
                                                 GdkEventButton *event);
static gint gtk_plot_canvas_focus_in		(GtkWidget *widget, 
                                                 GdkEventFocus *event);
static gint gtk_plot_canvas_focus_out		(GtkWidget *widget, 
                                                 GdkEventFocus *event);
static void gtk_plot_canvas_remove		(GtkContainer *container,
						 GtkWidget *child);

/* Drawing functions */
static gint gtk_plot_canvas_expose              (GtkWidget *widget, 
                                                 GdkEventExpose *event);
static void gtk_plot_canvas_create_pixmap       (GtkWidget *widget, 
                                                 gint width, gint height);
static void gtk_plot_canvas_set_plots_pixmap    (GtkPlotCanvas *plot_canvas);
static void gtk_plot_canvas_get_real_pixel      (GtkWidget *widget,
                                                 gdouble px, gdouble py,
                                                 gint *x, gint *y);
static void draw_selection 			(GtkPlotCanvas *canvas, 
                				 GdkRectangle area);
static void draw_marker				(GtkPlotCanvas *canvas, 
						 GdkGC *gc, gint x, gint y);
static void gtk_plot_canvas_draw_text           (GtkPlotCanvas *canvas,
                                                 GtkPlotCanvasChild *child);

static void gtk_plot_canvas_draw_grid		(GtkPlotCanvas *canvas);
static void gtk_plot_canvas_draw_child		(GtkPlotCanvas *canvas,
                           			 GtkPlotCanvasChild *child);
static void gtk_plot_canvas_set_line_attributes	(GtkPlotCanvas *canvas, 
                                                 GtkPlotLine line);
/* Auxiliary functions */
static GtkPlotCanvasPos posible_selection	(GdkRectangle area, 
						 gint x, gint y);
static gint roundint                            (gdouble x);


/* Signals */

extern void 
_gtkextra_signal_emit(GtkObject *object, guint signal_id, ...);

enum {
        SELECT_ITEM,
        MOVE_ITEM,
        RESIZE_ITEM,
        DELETE_ITEM,
        SELECT_REGION,
        CHANGED,
        LAST_SIGNAL
};

static GtkFixedClass *parent_class = NULL;
static guint canvas_signals[LAST_SIGNAL] = {0};

GtkType
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

      plot_canvas_type = gtk_type_unique (gtk_fixed_get_type(), &plot_canvas_info);
    }
  return plot_canvas_type;
}

static GtkPlotCanvasChild*
gtk_plot_canvas_child_copy (GtkPlotCanvasChild *child)
{
  GtkPlotCanvasChild *new_child;

  g_return_val_if_fail (child != NULL, NULL);

  new_child = gtk_plot_canvas_child_new(child->type);

  *new_child = *child;

  return new_child;
}

static void
gtk_plot_canvas_child_free (GtkPlotCanvasChild *child)
{
  g_return_if_fail (child != NULL);

  g_free (child);
}

GType
gtk_plot_canvas_child_get_type (void)
{
  static GType canvas_child_type;

  if(!canvas_child_type)
  {
    canvas_child_type = g_boxed_type_register_static("GtkPlotCanvasChild", (GBoxedCopyFunc)gtk_plot_canvas_child_copy, (GBoxedFreeFunc)gtk_plot_canvas_child_free);
  }
  return canvas_child_type;
}

static void
gtk_plot_canvas_class_init (GtkPlotCanvasClass *klass)
{
  GtkObjectClass *object_class;
  GtkWidgetClass *widget_class;
  GtkContainerClass *container_class;

  parent_class = gtk_type_class (gtk_fixed_get_type ());

  object_class = (GtkObjectClass *) klass;
  widget_class = (GtkWidgetClass *) klass;
  container_class = (GtkContainerClass *) klass;

  canvas_signals[SELECT_ITEM] =
    gtk_signal_new ("select_item",
                    GTK_RUN_LAST,
		    GTK_CLASS_TYPE(object_class),
                    GTK_SIGNAL_OFFSET (GtkPlotCanvasClass, select_item),
                    gtkextra_BOOLEAN__BOXED_BOXED,
                    GTK_TYPE_BOOL, 2, GDK_TYPE_EVENT, 
                    GTK_TYPE_PLOT_CANVAS_CHILD);

  canvas_signals[MOVE_ITEM] =
    gtk_signal_new ("move_item",
                    GTK_RUN_LAST,
		    GTK_CLASS_TYPE(object_class),
                    GTK_SIGNAL_OFFSET (GtkPlotCanvasClass, move_item),
                    gtkextra_BOOLEAN__BOXED_DOUBLE_DOUBLE,
                    GTK_TYPE_BOOL, 3, GTK_TYPE_PLOT_CANVAS_CHILD, 
                    GTK_TYPE_DOUBLE,
                    GTK_TYPE_DOUBLE);

  canvas_signals[RESIZE_ITEM] =
    gtk_signal_new ("resize_item",
                    GTK_RUN_LAST,
		    GTK_CLASS_TYPE(object_class),
                    GTK_SIGNAL_OFFSET (GtkPlotCanvasClass, resize_item),
                    gtkextra_BOOLEAN__BOXED_DOUBLE_DOUBLE,
                    GTK_TYPE_BOOL, 3, GTK_TYPE_PLOT_CANVAS_CHILD,
                    GTK_TYPE_DOUBLE,
                    GTK_TYPE_DOUBLE);

  canvas_signals[DELETE_ITEM] =
    gtk_signal_new ("delete_item",
                    GTK_RUN_LAST,
		    GTK_CLASS_TYPE(object_class),
                    GTK_SIGNAL_OFFSET (GtkPlotCanvasClass, delete_item),
                    gtkextra_BOOLEAN__POINTER,
                    GTK_TYPE_BOOL, 1,  
                    GTK_TYPE_PLOT_CANVAS_CHILD);

  canvas_signals[SELECT_REGION] =
    gtk_signal_new ("select_region",
                    GTK_RUN_LAST,
		    GTK_CLASS_TYPE(object_class),
                    GTK_SIGNAL_OFFSET (GtkPlotCanvasClass, select_region),
                    gtkextra_VOID__DOUBLE_DOUBLE_DOUBLE_DOUBLE,
                    GTK_TYPE_NONE, 4, 
                    GTK_TYPE_DOUBLE, GTK_TYPE_DOUBLE,
                    GTK_TYPE_DOUBLE, GTK_TYPE_DOUBLE);

  canvas_signals[CHANGED] =
    gtk_signal_new("changed",
                   GTK_RUN_LAST,
		   GTK_CLASS_TYPE(object_class),
                   GTK_SIGNAL_OFFSET (GtkPlotCanvasClass, changed),
                   gtkextra_VOID__VOID,
                   GTK_TYPE_NONE, 0);


  object_class->destroy = gtk_plot_canvas_destroy;

  widget_class->map = gtk_plot_canvas_map;
  widget_class->expose_event = gtk_plot_canvas_expose;
  widget_class->size_request = gtk_plot_canvas_size_request;
  widget_class->focus_in_event = gtk_plot_canvas_focus_in;
  widget_class->focus_out_event = gtk_plot_canvas_focus_out;
  widget_class->motion_notify_event = gtk_plot_canvas_motion;
  widget_class->button_press_event = gtk_plot_canvas_button_press;
  widget_class->button_release_event = gtk_plot_canvas_button_release;

  container_class->remove = gtk_plot_canvas_remove;

  klass->move_item = NULL;
  klass->resize_item = NULL;
  klass->select_item = NULL;
  klass->delete_item = NULL;
  klass->select_region = NULL;
}

static void
gtk_plot_canvas_init (GtkPlotCanvas *plot_canvas)
{
  GtkWidget *widget;
  GdkColor color;

  widget = GTK_WIDGET(plot_canvas);
  GTK_WIDGET_SET_FLAGS(widget, GTK_CAN_FOCUS);

  gdk_color_black(gtk_widget_get_colormap(widget), &widget->style->black);
  gdk_color_white(gtk_widget_get_colormap(widget), &widget->style->white);

  gtk_widget_set_events (widget, gtk_widget_get_events(widget)|
                         GRAPH_MASK);

  plot_canvas->freeze_count = 0;
  plot_canvas->cursor = gdk_cursor_new(GDK_TOP_LEFT_ARROW);

  plot_canvas->num_plots = 0;
  plot_canvas->background = widget->style->white;

  plot_canvas->flags = 0;
  plot_canvas->state = GTK_STATE_NORMAL;
  plot_canvas->action = GTK_PLOT_CANVAS_ACTION_INACTIVE;
  plot_canvas->magnification = 1.;

  plot_canvas->show_grid = FALSE;
  plot_canvas->grid_step = 20.;
  plot_canvas->grid.line_style = GTK_PLOT_LINE_SOLID;
  plot_canvas->grid.line_width = 0;

  gdk_color_parse("grey90", &color);
  gdk_color_alloc(gdk_colormap_get_system(), &color);
  plot_canvas->grid.color = color;

  plot_canvas->active_plot = NULL;
  plot_canvas->active_data = NULL;
  plot_canvas->active_point = -1;

  plot_canvas->drag_x = plot_canvas->drag_y = 0;
  plot_canvas->pointer_x = plot_canvas->pointer_y = 0;

  plot_canvas->plots = NULL;
  plot_canvas->childs = NULL;

  plot_canvas->width = DEFAULT_WIDTH;
  plot_canvas->height = DEFAULT_HEIGHT;
  plot_canvas->pixmap_width = DEFAULT_WIDTH;
  plot_canvas->pixmap_height = DEFAULT_HEIGHT;

  gtk_psfont_init();

  plot_canvas->pc = NULL;
  gtk_plot_canvas_set_pc(plot_canvas, NULL);

  plot_canvas->pixmap = NULL;
}

void
gtk_plot_canvas_set_pc(GtkPlotCanvas *canvas, GtkPlotPC *pc)
{
  GList *list;

  if(canvas->pc)
    gtk_object_unref(GTK_OBJECT(canvas->pc));

  if(!pc){
    canvas->pc = GTK_PLOT_PC(gtk_plot_gdk_new(NULL));
    gtk_object_ref(GTK_OBJECT(canvas->pc));
    gtk_object_sink(GTK_OBJECT(canvas->pc));
  } else {
    canvas->pc = pc;
    gtk_object_ref(GTK_OBJECT(pc));
    gtk_object_sink(GTK_OBJECT(pc));
  }

  if(canvas->pc && GTK_IS_PLOT_GDK(canvas->pc)){
       GTK_PLOT_GDK(canvas->pc)->drawable = canvas->pixmap;
  }
  gtk_plot_pc_set_viewport(canvas->pc, canvas->pixmap_width, canvas->pixmap_height);
}

GtkWidget*
gtk_plot_canvas_new (gint width, gint height, gdouble magnification)
{
  GtkPlotCanvas *plot_canvas;

  plot_canvas = gtk_type_new (gtk_plot_canvas_get_type ());

  gtk_plot_canvas_construct(GTK_PLOT_CANVAS(plot_canvas),
			    width, height, magnification);

  return GTK_WIDGET (plot_canvas);
}

void
gtk_plot_canvas_construct(GtkPlotCanvas *plot_canvas,
			  gint width, gint height, gdouble magnification)
{
  gdouble m = magnification;

  plot_canvas->transparent = TRUE;

  plot_canvas->width = width;
  plot_canvas->height = height;
  plot_canvas->pixmap_width = roundint(width * m);
  plot_canvas->pixmap_height = roundint(height * m);
  gtk_plot_canvas_set_magnification(plot_canvas, m);

  gtk_fixed_set_has_window (GTK_FIXED(plot_canvas), TRUE);


}

void
gtk_plot_canvas_freeze(GtkPlotCanvas *canvas)
{
  canvas->freeze_count++;
}

void
gtk_plot_canvas_thaw(GtkPlotCanvas *canvas)
{
  if(canvas->freeze_count == 0) return;
  canvas->freeze_count--;
}

static void
gtk_plot_canvas_destroy (GtkObject *object)
{
  GtkPlotCanvas *plot_canvas;
  GList *list;

  g_return_if_fail (object != NULL);
  g_return_if_fail (GTK_IS_PLOT_CANVAS (object));

  plot_canvas = GTK_PLOT_CANVAS (object);
 
  list = plot_canvas->childs;
  while(list){
    GtkPlotCanvasChild *child;

    child = (GtkPlotCanvasChild *) list->data;

    if(child->type == GTK_PLOT_CANVAS_TEXT){
       GtkPlotText *text;
     
       text = (GtkPlotText *)child->data;
   
       if(text->font) g_free(text->font); 
       if(text->text) g_free(text->text); 
    }
    if(child->type == GTK_PLOT_CANVAS_PIXMAP){
       GdkPixmap *pixmap;
     
       pixmap = (GdkPixmap *)child->data;
       if (pixmap)
         gdk_pixmap_unref(pixmap); 
    } else {
       if(child->data)
           g_free(child->data);
    }

    g_free(child);

    plot_canvas->childs = g_list_remove_link(plot_canvas->childs, list);
    g_list_free_1(list);

    list = plot_canvas->childs;
  }
  plot_canvas->childs = NULL;

  if( plot_canvas->cursor ){
     gdk_cursor_destroy(plot_canvas->cursor);
     plot_canvas->cursor = NULL;
  }
  if( plot_canvas->pc ){
     gtk_object_unref(GTK_OBJECT(plot_canvas->pc));
     plot_canvas->pc = NULL;
  }

  if (GTK_OBJECT_CLASS (parent_class)->destroy)
    (*GTK_OBJECT_CLASS (parent_class)->destroy) (object);

  gtk_psfont_unref();
}

static void
gtk_plot_canvas_remove(GtkContainer *container, GtkWidget *child)
{
  GtkPlotCanvas *canvas;
  GList *list;

  canvas = GTK_PLOT_CANVAS(container);
  gtk_plot_canvas_cancel_action(canvas);

  list = canvas->plots;
  while(list){
   if(list->data == child){
      canvas->plots = g_list_remove_link(canvas->plots, list);
      g_list_free_1(list);
      canvas->num_plots--;
      break;
   }
   list = list->next;
  }
 
  GTK_CONTAINER_CLASS(parent_class)->remove(container, child);
}

void
gtk_plot_canvas_paint (GtkPlotCanvas *canvas)
{
  GtkWidget *widget;
  GList *childs;
  GList *plots;

  widget = GTK_WIDGET(canvas);

  if(GTK_WIDGET_REALIZED(widget) && !canvas->pixmap) return;
  if(canvas->freeze_count > 0) return;

  if(!gtk_plot_pc_init(canvas->pc)) return;

  gtk_plot_pc_gsave(canvas->pc);

  if(!GTK_IS_PLOT_PS(canvas->pc) || !canvas->transparent){
    if(canvas->transparent){
      GdkColor white;
      gdk_color_white(gtk_widget_get_colormap(GTK_WIDGET(canvas)), &white);
      gtk_plot_pc_set_color(canvas->pc, &white);
    } else
      gtk_plot_pc_set_color(canvas->pc, &canvas->background);
    gtk_plot_pc_draw_rectangle(canvas->pc,
                              TRUE,
                              0,0,canvas->pixmap_width, canvas->pixmap_height);
  }

  if(!GTK_IS_PLOT_PS(canvas->pc) || !canvas->transparent){
    gtk_plot_pc_draw_rectangle(canvas->pc,
                              TRUE,
                              0,0,canvas->pixmap_width, canvas->pixmap_height);
  }

  gtk_plot_canvas_draw_grid(canvas);
  plots = canvas->plots;
  while(plots)
    {
      GtkPlot *plot;
      GtkPlotPC *pc;

      plot = GTK_PLOT(plots->data);

      pc = plot->pc;
      plot->pc = canvas->pc;
      plot->magnification = canvas->magnification;

      gtk_plot_set_drawable(plot, canvas->pixmap);

      gtk_plot_paint(GTK_PLOT(plot));

      plot->pc = pc;

      plots = plots->next;
    }

  childs = canvas->childs;
  while(childs)
   {
     GtkPlotCanvasChild *child;

     child = (GtkPlotCanvasChild *) childs->data;
     gtk_plot_canvas_draw_child(canvas, child);
     childs = childs->next;
   }

  gtk_plot_pc_grestore(canvas->pc);
  gtk_plot_pc_leave(canvas->pc);
}

void
gtk_plot_canvas_refresh(GtkPlotCanvas *canvas)
{
  if(!GTK_WIDGET_REALIZED(GTK_WIDGET(canvas))) return;
  if(!canvas->pixmap) return;

  gdk_draw_drawable(GTK_WIDGET(canvas)->window,
                  GTK_WIDGET(canvas)->style->fg_gc[GTK_STATE_NORMAL],
                  canvas->pixmap,
                  0, 0,
                  0, 0,
                  -1, -1);

  if(canvas->state == GTK_STATE_SELECTED){
    draw_selection(canvas, canvas->drag_area); 
  }
}

static void
gtk_plot_canvas_draw_grid(GtkPlotCanvas *canvas)
{
  gdouble x, y;

  if(!canvas->pixmap) return;
  if(!canvas->show_grid) return;

  if(!GTK_IS_PLOT_GDK(canvas->pc)) return;

  gtk_plot_canvas_set_line_attributes(canvas, canvas->grid);

  for(x = 0; x < canvas->pixmap_width; x += canvas->grid_step)
      gtk_plot_pc_draw_line(canvas->pc, 
                            roundint(x), 0, roundint(x), canvas->pixmap_height);

  for(y = 0; y < canvas->pixmap_height; y += canvas->grid_step)
      gtk_plot_pc_draw_line(canvas->pc, 
                            0, roundint(y), canvas->pixmap_width, roundint(y));
}

static void
gtk_plot_canvas_map(GtkWidget *widget)
{
  GtkPlotCanvas *plot_canvas;

  plot_canvas=GTK_PLOT_CANVAS(widget);

  GTK_WIDGET_CLASS(parent_class)->map(widget);

  if(!plot_canvas->pixmap){
      gtk_plot_canvas_create_pixmap(widget, 
                                    plot_canvas->pixmap_width, 
                                    plot_canvas->pixmap_height);
      gtk_plot_canvas_paint(plot_canvas);
      return;
  }

  gdk_window_set_cursor(widget->window, plot_canvas->cursor);
}


static gint 
gtk_plot_canvas_motion (GtkWidget *widget, GdkEventMotion *event)
{
  GtkPlotCanvas *canvas;
  GtkPlot *active_plot;
  GtkPlotData *active_dataset;
  gint active_point;
  GtkAllocation *allocation = NULL;
  GdkRectangle area;
  gint x, y;
  gint new_x, new_y;
  gint new_width, new_height;
  gint cursor = GDK_TOP_LEFT_ARROW;
  gint pivot_x, pivot_y;
  gdouble fx, fy, fz, fa;
  gdouble fdx, fdy, fdz, fda;
  gchar *label;
  gboolean error;

  canvas = GTK_PLOT_CANVAS(widget);
  gtk_widget_get_pointer(widget, &x, &y);

  area = canvas->active_item.allocation;
  new_x = area.x;
  new_y = area.y;
  new_width = area.width;
  new_height = area.height;
  pivot_x = x;
  pivot_y = y;

  if(canvas->action == GTK_PLOT_CANVAS_ACTION_INACTIVE) 
       cursor = GDK_TOP_LEFT_ARROW;
  else if(canvas->action == GTK_PLOT_CANVAS_ACTION_DRAG) 
       cursor = GDK_FLEUR;
  else
       switch(canvas->drag_point){
            case GTK_PLOT_CANVAS_TOP_LEFT: 
                 cursor = GDK_UL_ANGLE; 
                 pivot_x = area.x + area.width;
                 pivot_y = area.y + area.height;
                 break;
            case GTK_PLOT_CANVAS_TOP_RIGHT:
                 cursor = GDK_UR_ANGLE; 
                 pivot_x = area.x;
                 pivot_y = area.y + area.height;
                 break;
            case GTK_PLOT_CANVAS_TOP:
                 cursor = GDK_TOP_SIDE;
                 pivot_y = area.y + area.height;
                 break;
            case GTK_PLOT_CANVAS_BOTTOM_LEFT:
                 cursor = GDK_LL_ANGLE;
                 pivot_x = area.x + area.width;
                 pivot_y = area.y;
                 break;
            case GTK_PLOT_CANVAS_BOTTOM_RIGHT:
                 cursor = GDK_LR_ANGLE;
                 pivot_x = area.x;
                 pivot_y = area.y;
                 break;
            case GTK_PLOT_CANVAS_BOTTOM:
                 cursor = GDK_BOTTOM_SIDE;
                 pivot_y = area.y;
                 break;
            case GTK_PLOT_CANVAS_LEFT:
                 cursor = GDK_LEFT_SIDE;
                 pivot_x = area.x + area.width;
                 break;
            case GTK_PLOT_CANVAS_RIGHT:
                 cursor = GDK_RIGHT_SIDE;
                 pivot_x = area.x;
                 break;
            default:
                 cursor = GDK_TOP_LEFT_ARROW;
       }

  if(cursor != canvas->cursor->type){
         gdk_cursor_destroy(canvas->cursor);
         canvas->cursor = gdk_cursor_new(cursor);
         gdk_window_set_cursor(widget->window, canvas->cursor);
  }    
    

  active_plot = canvas->active_plot;
  active_dataset = canvas->active_data;
  active_point = canvas->active_point;
 
  if(canvas->action == GTK_PLOT_CANVAS_ACTION_INACTIVE) return TRUE;
  if(active_plot == NULL && canvas->action != GTK_PLOT_CANVAS_ACTION_SELECTION)
     return TRUE;

  if(active_plot)
         allocation = &GTK_WIDGET(canvas->active_plot)->allocation;

  switch(canvas->action){
     case GTK_PLOT_CANVAS_ACTION_DRAG:
       if(canvas->active_item.flags & GTK_PLOT_CANVAS_CAN_MOVE){
         gint dx, dy;

         if(canvas->active_item.type == GTK_PLOT_CANVAS_MARKER){
           GtkPlotMarker *marker;
           gboolean move = FALSE;
           gint i;

           marker = (GtkPlotMarker *)canvas->active_item.data;
           for(i = 0; i < marker->data->num_points; i++){
              gdouble px, py;
	      gtk_plot_data_get_point(marker->data, i,
	  			     &fx, &fy, &fz, &fa, &fdx, &fdy, &fdz, &fda,
				     &label, &error); 
              gtk_plot_get_pixel(marker->data->plot,
                                 fx, fy,
                                 &px, &py);
             
              if(fabs(x - px) <= DEFAULT_MARKER_SIZE && 
                 fabs(y - py) <= DEFAULT_MARKER_SIZE) {
                      move = TRUE;
                      x = px;
                      y = py;
                      break;
              }
           }
           if(!move) return TRUE;

         } else if(canvas->active_item.type == GTK_PLOT_CANVAS_DATA){
	   gboolean veto = FALSE;

           draw_selection(canvas, canvas->drag_area);
		
	   gtk_plot_get_point(canvas->active_plot, x, y, &fx, &fy);

           _gtkextra_signal_emit(GTK_OBJECT(canvas), canvas_signals[MOVE_ITEM],
                           &canvas->active_item, fx, fy, &veto);

           if(veto){
              gtk_signal_emit (GTK_OBJECT(canvas), canvas_signals[CHANGED]);

              gtk_plot_data_get_point(canvas->active_data, canvas->active_point,
	    		             &fx, &fy, &fz, &fa, &fdx, &fdy, &fdz, &fda,
			             &label, &error);

              if(fx != canvas->active_x || fy != canvas->active_y){
                canvas->active_x = fx;
                canvas->active_y = fy;

	        canvas->drag_x = x;
	        canvas->drag_y = y;

	        canvas->drag_area.x = x - 10;
	        canvas->drag_area.y = y - 10;

                canvas->active_item.allocation.x = x - 10;
                canvas->active_item.allocation.y = y - 10;
                canvas->active_item.allocation.width = 20;
             }
           }
           draw_selection(canvas, canvas->drag_area);
           return TRUE;
         }
         draw_selection(canvas, canvas->drag_area);
         canvas->pointer_x = x;
         canvas->pointer_y = y;
         dx = x - canvas->drag_x;
         dy = y - canvas->drag_y;
         area.x = canvas->active_item.allocation.x + dx; 
         area.y = canvas->active_item.allocation.y + dy;
         draw_selection(canvas, area);
         canvas->drag_area = area;
       }
       break;
     case GTK_PLOT_CANVAS_ACTION_RESIZE:
       switch(canvas->drag_point){
            case GTK_PLOT_CANVAS_TOP_LEFT: 
            case GTK_PLOT_CANVAS_TOP_RIGHT:
               if(canvas->active_item.flags & GTK_PLOT_CANVAS_CAN_X_RESIZE){
                    new_x = MIN(x, pivot_x);
                    new_width = abs(x - pivot_x);
               }
            case GTK_PLOT_CANVAS_TOP:
               if(canvas->active_item.flags & GTK_PLOT_CANVAS_CAN_Y_RESIZE){
                    new_y = MIN(y, pivot_y);
                    new_height = abs(y - pivot_y);
               }
               gdk_cursor_destroy(canvas->cursor);
               canvas->cursor = gdk_cursor_new(cursor);
               gdk_window_set_cursor(widget->window, canvas->cursor);
               break;
            case GTK_PLOT_CANVAS_BOTTOM_LEFT:
            case GTK_PLOT_CANVAS_BOTTOM_RIGHT:
               if(canvas->active_item.flags & GTK_PLOT_CANVAS_CAN_X_RESIZE){
                    new_x = MIN(x, pivot_x);
                    new_width = abs(x - pivot_x);
               }
            case GTK_PLOT_CANVAS_BOTTOM:
               if(canvas->active_item.flags & GTK_PLOT_CANVAS_CAN_Y_RESIZE){
                    new_y = MIN(y, pivot_y);
                    new_height = abs(y - pivot_y);
               }
               break;
            case GTK_PLOT_CANVAS_LEFT:
            case GTK_PLOT_CANVAS_RIGHT:
               if(canvas->active_item.flags & GTK_PLOT_CANVAS_CAN_X_RESIZE){
                    new_x = MIN(x, pivot_x);
                    new_width = abs(x - pivot_x);
               }
               break;
            case GTK_PLOT_CANVAS_IN:
            case GTK_PLOT_CANVAS_OUT:
            default:
               break;
       }


       if(new_width >= canvas->active_item.min_width &&
          new_height >= canvas->active_item.min_height){ 

                canvas->pointer_x = x;
                canvas->pointer_y = y;
                draw_selection(canvas, canvas->drag_area);
                area.x = new_x;
                area.y = new_y;
                area.width = new_width;
                area.height = new_height;
                draw_selection(canvas, area);
                canvas->drag_area = area;

       }

       break;
     case GTK_PLOT_CANVAS_ACTION_SELECTION:
         draw_selection(canvas, canvas->drag_area);
         canvas->pointer_x = x;
         canvas->pointer_y = y;
         area.x = MIN(canvas->pointer_x, canvas->drag_x); 
         area.y = MIN(canvas->pointer_y, canvas->drag_y); 
         area.width = abs(x - canvas->drag_x);
         area.height = abs(y - canvas->drag_y);
         canvas->drag_area = area;
         draw_selection(canvas, canvas->drag_area);
 	 break;
     case GTK_PLOT_CANVAS_ACTION_INACTIVE:
     default:
         break;
  }

  return TRUE;

}

static gint
gtk_plot_canvas_button_press(GtkWidget *widget, GdkEventButton *event)
{
  GtkPlotCanvas *canvas = NULL;
  GtkPlotCanvasChild active_item;
  GtkPlot *active_plot = NULL;
  GList *plots = NULL;
  GList *dataset = NULL;
  GList *childs = NULL;
  GtkPlot *plot = NULL;
  GtkPlot *click_plot = NULL;
  GtkPlotData *data = NULL;
  GtkPlotText *child_text;
  GtkAllocation internal_allocation;
  GtkAllocation legends_allocation;
  GdkRectangle area, drag_area;
  GtkPlotAxis *axis[4];
  GdkModifierType mods;
  gint i = 0;
  gint x = 0, y = 0;
  gdouble xi = 0.0, yi = 0.0;
  gint px = 0, py = 0;
  gint pwidth = 0, pheight = 0;
  gint tx = 0, ty = 0;
  gint rx = 0, ry = 0;
  gint twidth = 0, theight = 0;
  gboolean veto;
  gboolean new_selection = FALSE;
  gboolean double_click = FALSE;
  guint state = GTK_STATE_NORMAL;
  GtkPlotCanvasPos pos, new_pos = 0;
  gdouble m;

  gdk_window_get_pointer(widget->window, NULL, NULL, &mods);
  if(!(mods & GDK_BUTTON1_MASK)) return FALSE;
  double_click = (event->button == GDK_2BUTTON_PRESS);
 
  canvas = GTK_PLOT_CANVAS(widget);
  active_plot = canvas->active_plot;
  m = canvas->magnification;

/*
  if(double_click && canvas->state == GTK_STATE_SELECTED) return TRUE;
*/
  gdk_pointer_ungrab(event->time);

  if(!GTK_WIDGET_HAS_FOCUS(widget)) gtk_widget_grab_focus(widget);

  gtk_widget_get_pointer(widget, &x, &y);

/**********************************************************************/
  childs = canvas->childs;

  while(childs)
    {
      GtkPlotCanvasChild *child;
      gint x1, x2, y1, y2;
      gint width, height;

      child = (GtkPlotCanvasChild *)childs->data;
      gtk_plot_canvas_get_pixel(GTK_PLOT_CANVAS(widget), 
                                child->rx1, child->ry1,
                                &x1, &y1);
      gtk_plot_canvas_get_pixel(GTK_PLOT_CANVAS(widget), 
                                child->rx2, child->ry2,
                                &x2, &y2);

      width = abs(x2 - x1);
      height = abs(y2 - y1);

      area.x = MIN(x1, x2);
      area.y = MIN(y1, y2);
      area.width = width;
      area.height = height;

      if((pos = posible_selection(area, x, y)) != GTK_PLOT_CANVAS_OUT){ 
          active_item.data = child;
          active_item.allocation = area;
          active_item.min_width = 0;
          active_item.min_height = 0;
          active_item.type = child->type;
          active_item.state = GTK_STATE_SELECTED;
          active_item.flags = child->flags;
          active_item.selection = child->selection;
          active_item.mode = child->mode;
          state = GTK_STATE_SELECTED;
          new_selection = TRUE;
          new_pos = pos;
      }

      childs = childs->next;
    }

/**********************************************************************/
  plots = canvas->plots;
  while(plots)
     {
       plot = (GtkPlot *)plots->data;

       if(!GTK_IS_PLOT3D(plot)){
         axis[0]=plot->left;
         axis[1]=plot->right;
         axis[2]=plot->top;
         axis[3]=plot->bottom;

         for(i = 0; i <= 3; i++){
            if(axis[i]->title_visible){
              child_text = &axis[i]->title;
              gtk_plot_canvas_get_real_pixel(GTK_WIDGET(plot), 
                                             child_text->x, child_text->y,
                                             &tx, &ty);
              gtk_plot_text_get_area(child_text->text,
  				     child_text->angle,
  				     child_text->justification,
  		  	  	     child_text->font,
				     roundint(child_text->height * m),
                                     &rx, &ry, &twidth, &theight);

              if(child_text->border != GTK_PLOT_BORDER_NONE){
                 tx -= child_text->border_space;
                 ty -= child_text->border_space;
                 twidth += 2 * child_text->border_space;
                 theight += 2 * child_text->border_space;
              }


              area.x = tx + rx;
              area.y = ty + ry;
              area.width = twidth;
              area.height = theight;

              if((pos = posible_selection(area, x, y)) != GTK_PLOT_CANVAS_OUT){ 
                active_item.data = axis[i];
                active_item.allocation = area;
                active_item.type = GTK_PLOT_CANVAS_TITLE;
                active_item.state = GTK_STATE_SELECTED;
                active_item.flags = GTK_PLOT_CANVAS_CAN_MOVE;
                active_item.selection = GTK_PLOT_CANVAS_SELECT_MARKERS;
                active_item.mode = GTK_PLOT_CANVAS_SELECT_CLICK_2;
                state = GTK_STATE_SELECTED;
                click_plot = plot;
                new_selection = TRUE;
                new_pos = pos;
                break;
              }
            }
         }
       }

       plots = plots->next;
     }

/**********************************************************************/
 
  plots = canvas->plots;
  while(plots)
   {
     plot = (GtkPlot *)plots->data;

     if(plot->show_legends){

       legends_allocation = gtk_plot_legends_get_allocation (plot);
       area.x = legends_allocation.x;
       area.y = legends_allocation.y;
       area.width = legends_allocation.width;
       area.height = legends_allocation.height;

       if((pos = posible_selection(area, x, y)) != GTK_PLOT_CANVAS_OUT){ 
           active_item.data = plot;
           active_item.allocation = area;
           active_item.type = GTK_PLOT_CANVAS_LEGENDS;
           active_item.state = GTK_STATE_SELECTED;
           active_item.flags = GTK_PLOT_CANVAS_CAN_MOVE;
           active_item.selection = GTK_PLOT_CANVAS_SELECT_MARKERS;
           active_item.mode = GTK_PLOT_CANVAS_SELECT_CLICK_2;
           state = GTK_STATE_SELECTED;
           click_plot = plot;
           new_selection = TRUE;
           new_pos = pos;
           break;
       }
     }
     plots=plots->next;
   } 

/**********************************************************************/
  if(!new_selection){

     plots = canvas->plots;
     while(plots)
        {
          plot = (GtkPlot *)plots->data;

          internal_allocation = gtk_plot_get_internal_allocation (plot);
          px = internal_allocation.x;
          py = internal_allocation.y;
          pwidth = internal_allocation.width;
          pheight = internal_allocation.height;
  
          if(!GTK_IS_PLOT3D(plot)){
            area.x = px - 6;
            area.y = py;
            area.width = 6;
            area.height = pheight;
            if((pos = posible_selection(area, x, y)) != GTK_PLOT_CANVAS_OUT){
                active_item.data = &plot->left;
                active_item.allocation = area;
                active_item.type = GTK_PLOT_CANVAS_AXIS;
                active_item.state = GTK_STATE_SELECTED;
                active_item.flags = GTK_PLOT_CANVAS_FROZEN;
                active_item.selection = GTK_PLOT_CANVAS_SELECT_MARKERS;
                active_item.mode = GTK_PLOT_CANVAS_SELECT_CLICK_2;
                state = GTK_STATE_SELECTED;
                new_pos = pos;
                click_plot = plot;
            }
            area.x = px + pwidth;
            if((pos = posible_selection(area, x, y)) != GTK_PLOT_CANVAS_OUT){
                active_item.data = &plot->right;
                active_item.allocation = area;
                active_item.type = GTK_PLOT_CANVAS_AXIS;
                active_item.state = GTK_STATE_SELECTED;
                active_item.flags = GTK_PLOT_CANVAS_FROZEN;
                active_item.selection = GTK_PLOT_CANVAS_SELECT_MARKERS;
                active_item.mode = GTK_PLOT_CANVAS_SELECT_CLICK_2;
                state = GTK_STATE_SELECTED;
                new_pos = pos;
                click_plot = plot;
            }
            area.x = px;
            area.y = py - 6;
            area.width = pwidth;
            area.height = 6;
            if((pos = posible_selection(area, x, y)) != GTK_PLOT_CANVAS_OUT){
                active_item.data = &plot->top;
                active_item.allocation = area;
                active_item.type = GTK_PLOT_CANVAS_AXIS;
                active_item.state = GTK_STATE_SELECTED;
                active_item.flags = GTK_PLOT_CANVAS_FROZEN;
                active_item.selection = GTK_PLOT_CANVAS_SELECT_MARKERS;
                active_item.mode = GTK_PLOT_CANVAS_SELECT_CLICK_2;
                state = GTK_STATE_SELECTED;
                new_pos = pos;
                click_plot = plot;
            }
            area.y = py + pheight;
            if((pos = posible_selection(area, x, y)) != GTK_PLOT_CANVAS_OUT){
                active_item.data = &plot->bottom;
                active_item.allocation = area;
                active_item.type = GTK_PLOT_CANVAS_AXIS;
                active_item.state = GTK_STATE_SELECTED;
                active_item.flags = GTK_PLOT_CANVAS_FROZEN;
                active_item.selection = GTK_PLOT_CANVAS_SELECT_MARKERS;
                active_item.mode = GTK_PLOT_CANVAS_SELECT_CLICK_2;
                state = GTK_STATE_SELECTED;
                new_pos = pos;
                click_plot = plot;
            }
          }
  
          area.x = px;
          area.y = py;
          area.width = pwidth;
          area.height = pheight;

          if((pos = posible_selection(area, x, y)) != GTK_PLOT_CANVAS_OUT){
              active_item.data = plot;
              active_item.allocation = area;
              active_item.min_width = 2 * DEFAULT_MARKER_SIZE;
              active_item.min_height = 2 * DEFAULT_MARKER_SIZE;
              active_item.type = GTK_PLOT_CANVAS_PLOT;
              active_item.state = GTK_STATE_SELECTED;
              active_item.flags = GTK_PLOT_CANVAS_CAN_MOVE |
                                  GTK_PLOT_CANVAS_CAN_X_RESIZE |
                                  GTK_PLOT_CANVAS_CAN_Y_RESIZE;
              active_item.selection = GTK_PLOT_CANVAS_SELECT_MARKERS;
              active_item.mode = GTK_PLOT_CANVAS_SELECT_CLICK_2;
              state = GTK_STATE_SELECTED;
              new_pos = pos;
              click_plot = plot;
          }

          plots=plots->next;
        } 
  }

/**********************************************************************/
  if(!active_plot) active_plot = click_plot;

/* DATA MARKERS */

  if(active_plot && !GTK_IS_PLOT3D(active_plot)){
    dataset = active_plot->data_sets;
    while(dataset){
      GList *markers;

      data = GTK_PLOT_DATA(dataset->data);
  
      markers = data->markers;
      while(markers){
        GtkPlotMarker *marker;
	gdouble fx, fy, fz, fa, fdx, fdy, fdz, fda;
	gboolean error;
	gchar *label;
  
        marker = (GtkPlotMarker *)markers->data;
      
	gtk_plot_data_get_point(data, marker->point,
				&fx, &fy, &fz, &fa, &fdx, &fdy, &fdz, &fda,
				&label, &error); 
        gtk_plot_get_pixel(GTK_PLOT(active_plot), 
                           fx, fy, &xi, &yi);

        if(abs(xi-x) <= 20 && abs(yi-y) <= 20){
  
           active_item.type = GTK_PLOT_CANVAS_MARKER;
           active_item.state = GTK_STATE_SELECTED;
           active_item.flags = GTK_PLOT_CANVAS_CAN_MOVE;
           x = xi;
           y = yi;
           active_item.allocation.x = xi - 10;
           active_item.allocation.y = yi - 10;
           active_item.allocation.width = 20;
           active_item.allocation.height = 20;
           active_item.data = marker;
           active_item.selection = GTK_PLOT_CANVAS_SELECT_TARGET;
           active_item.mode = GTK_PLOT_CANVAS_SELECT_CLICK_1;
           new_pos = GTK_PLOT_CANVAS_IN;
           state = GTK_STATE_SELECTED;
           break;
        }
        markers = markers->next;
      }
      if(active_item.type == GTK_PLOT_CANVAS_MARKER) break;
      dataset = dataset->next;
    }
  }

/* DATA POINTS */

  if(active_plot && !GTK_IS_PLOT3D(active_plot) &&
     GTK_PLOT_CANVAS_CAN_SELECT_POINT(canvas)){
    dataset = active_plot->data_sets;

    while(dataset)
     {
       data = GTK_PLOT_DATA(dataset->data);

       if(!data->is_function){
	 gdouble fx, fy, fz, fa, fdx, fdy, fdz, fda;
	 gboolean error;
	 gchar *label;

         for(i = 0; i < data->num_points; i++){
	   gtk_plot_data_get_point(data, i,
	  			   &fx, &fy, &fz, &fa, &fdx, &fdy, &fdz, &fda,
				   &label, &error); 
           gtk_plot_get_pixel(GTK_PLOT(active_plot), fx, fy, &xi, &yi);
           if(abs(xi-x) <= DEFAULT_MARKER_SIZE && 
              abs(yi-y) <= DEFAULT_MARKER_SIZE){
                gdouble ax, ay;
                active_item.type = GTK_PLOT_CANVAS_DATA;
                active_item.state = GTK_STATE_SELECTED;
                active_item.flags = GTK_PLOT_CANVAS_CAN_MOVE;
                gtk_plot_get_pixel(GTK_PLOT(active_plot), 
                                   canvas->active_x,
                                   canvas->active_y,
                                   &ax, &ay);
                x = ax;
                y = ay;
                active_item.allocation.x = xi - DEFAULT_MARKER_SIZE;
                active_item.allocation.y = yi - DEFAULT_MARKER_SIZE;
                active_item.allocation.width = 2 * DEFAULT_MARKER_SIZE;
                active_item.allocation.height = 2 * DEFAULT_MARKER_SIZE;
                active_item.data = data;
                active_item.selection = GTK_PLOT_CANVAS_SELECT_TARGET;
                active_item.mode = GTK_PLOT_CANVAS_SELECT_CLICK_2;
                new_pos = GTK_PLOT_CANVAS_IN;
                state = GTK_STATE_SELECTED;
                break;
           }
         }
       }
       if(active_item.type == GTK_PLOT_CANVAS_DATA) break;
       dataset = dataset->next;
     }
  }

/**********************************************************************/
/**********************************************************************/

  if(state == GTK_STATE_SELECTED){
    gboolean new_item;
    GtkPlot *aux_plot = canvas->active_plot;
    GtkPlotData *aux_data = canvas->active_data;
    gint aux_point = canvas->active_point;
    gdouble aux_x = canvas->active_x;
    gdouble aux_y = canvas->active_y;

    if(active_item.type == GTK_PLOT_CANVAS_DATA)
       {
	        gdouble fx, fy, fz, fa, fdx, fdy, fdz, fda;
   	        gboolean error;
	        gchar *label;

	        gtk_plot_data_get_point(data, i,
	  	  		   &fx, &fy, &fz, &fa, &fdx, &fdy, &fdz, &fda,
				   &label, &error); 

                canvas->active_data = data;
                canvas->active_point = i;
                canvas->active_x = fx;
                canvas->active_y = fy;
       }

    if(click_plot) canvas->active_plot = click_plot;

    veto = TRUE;
    _gtkextra_signal_emit(GTK_OBJECT(canvas), canvas_signals[SELECT_ITEM],
                    event, &active_item, &veto);

    if(!veto)
              canvas->active_plot = aux_plot;

    new_item = (canvas->state != GTK_STATE_SELECTED ||
                active_item.type != canvas->active_item.type ||
                active_item.data != canvas->active_item.data);

    if(!new_item && active_item.type != GTK_PLOT_CANVAS_TEXT &&
       active_item.type != GTK_PLOT_CANVAS_ELLIPSE &&
       active_item.type != GTK_PLOT_CANVAS_RECTANGLE &&
       active_item.type != GTK_PLOT_CANVAS_LINE &&
       active_item.type != GTK_PLOT_CANVAS_CUSTOM &&
       active_item.type != GTK_PLOT_CANVAS_PIXMAP &&
       active_item.type != GTK_PLOT_CANVAS_MARKER &&
       click_plot != canvas->active_plot) new_item = TRUE;

    if(canvas->active_item.type == GTK_PLOT_CANVAS_DATA &&
       active_item.type == GTK_PLOT_CANVAS_DATA &&
       active_item.data == canvas->active_item.data)
       {
                new_item = !(aux_point == i); 
       }

    if(new_item &&
       ((active_item.type == GTK_PLOT_CANVAS_DATA &&
        GTK_PLOT_CANVAS_CAN_SELECT_POINT(canvas)) ||
        (active_item.type != GTK_PLOT_CANVAS_DATA &&
         GTK_PLOT_CANVAS_CAN_SELECT_ITEM(canvas)))) {

         if(veto){
           gtk_plot_canvas_unselect(canvas);
           canvas->active_item = active_item;
           canvas->drag_area = active_item.allocation;
           canvas->state = GTK_STATE_SELECTED; 
           canvas->action = GTK_PLOT_CANVAS_ACTION_INACTIVE;

           draw_selection(canvas, active_item.allocation);

           if(active_item.mode == GTK_PLOT_CANVAS_SELECT_CLICK_2)
             return TRUE;

         } else {

           if(active_item.type == GTK_PLOT_CANVAS_DATA){
       	       canvas->active_data = aux_data;
               canvas->active_point = aux_point;
               canvas->active_x = aux_x;
               canvas->active_y = aux_y;
           }

         }
    }

    if((!new_item && active_item.mode == GTK_PLOT_CANVAS_SELECT_CLICK_2) ||   
       active_item.mode == GTK_PLOT_CANVAS_SELECT_CLICK_1) {
       
      if((active_item.type == GTK_PLOT_CANVAS_DATA &&
          GTK_PLOT_CANVAS_CAN_DND_POINT(canvas)) ||
         (active_item.type != GTK_PLOT_CANVAS_DATA &&
          GTK_PLOT_CANVAS_CAN_DND(canvas))) {

         switch(new_pos){
          case GTK_PLOT_CANVAS_IN:
              canvas->action = GTK_PLOT_CANVAS_ACTION_DRAG;
              break;
          default:
              if(active_item.flags & GTK_PLOT_CANVAS_CAN_X_RESIZE ||
                 active_item.flags & GTK_PLOT_CANVAS_CAN_Y_RESIZE)
                 canvas->action = GTK_PLOT_CANVAS_ACTION_RESIZE;
              else
                 canvas->action = GTK_PLOT_CANVAS_ACTION_DRAG;
         }
         gdk_pointer_grab (widget->window, FALSE,
                           GDK_POINTER_MOTION_HINT_MASK |
                           GDK_BUTTON1_MOTION_MASK |
                           GDK_BUTTON_RELEASE_MASK,
                           NULL, NULL, event->time);
         canvas->drag_point = new_pos;

         canvas->drag_x = x;
         canvas->drag_y = y;
         canvas->pointer_x = x;
         canvas->pointer_y = y;

         return TRUE;
      }
    }
  } 

  gtk_plot_canvas_unselect(canvas);

  if(GTK_PLOT_CANVAS_CAN_SELECT(canvas)){
         active_item.data = NULL;
         active_item.allocation.x = x;
         active_item.allocation.y = y;
         active_item.allocation.width = 0;
         active_item.allocation.height = 0;
         drag_area = active_item.allocation;
         active_item.type = GTK_PLOT_CANVAS_NONE;
         active_item.state = GTK_STATE_SELECTED;
         active_item.flags = GTK_PLOT_CANVAS_CAN_X_RESIZE |
                             GTK_PLOT_CANVAS_CAN_Y_RESIZE;
         active_item.selection = GTK_PLOT_CANVAS_SELECT_NONE;
         veto = TRUE;

         _gtkextra_signal_emit(GTK_OBJECT(canvas), canvas_signals[SELECT_ITEM],
                         event, &canvas->active_item, &veto);
         if(veto){
           canvas->active_item = active_item;
           canvas->active_item.data = click_plot;
           canvas->active_plot = click_plot;
           canvas->state = GTK_STATE_SELECTED;
           canvas->action = GTK_PLOT_CANVAS_ACTION_SELECTION;
           canvas->drag_point = new_pos;
           canvas->drag_x = x;
           canvas->drag_y = y;
           canvas->pointer_x = x;
           canvas->pointer_y = y;
           canvas->drag_area = active_item.allocation;
           gdk_pointer_grab (widget->window, FALSE,
                             GDK_POINTER_MOTION_HINT_MASK |
                             GDK_BUTTON1_MOTION_MASK |
                             GDK_BUTTON_RELEASE_MASK,
                             NULL, NULL, event->time);
           draw_selection(canvas, active_item.allocation);
         }
  }

  gtk_widget_queue_draw(GTK_WIDGET(canvas));
  return TRUE;  
}

 
static gint
gtk_plot_canvas_button_release(GtkWidget *widget, GdkEventButton *event)
{
  GtkPlotCanvas *canvas;
  GtkPlotCanvasChild *child;
  GtkPlotMarker *marker = NULL;
  GtkPlotText *text;
  GtkPlotAxis *axis = NULL;
  GtkPlot *active_plot;
  gint active_point = 0;
  gdouble fx, fy, fz, fa, fdx, fdy, fdz, fda;
  gdouble dx, dy;
  gdouble x, y;
  gboolean error;
  gchar *label;
  gdouble new_x, new_y;
  gdouble new_width, new_height;
  gdouble x1 = 0., y1 = 0., x2 = 0., y2 = 0.;
  gboolean veto = TRUE;

  canvas = GTK_PLOT_CANVAS(widget); 

  gdk_pointer_ungrab(event->time);

  if(GTK_WIDGET_MAPPED(widget)){
      gdk_cursor_destroy(canvas->cursor);
      canvas->cursor = gdk_cursor_new(GDK_TOP_LEFT_ARROW);
      gdk_window_set_cursor(widget->window, 
                            canvas->cursor);
  }

  active_plot = canvas->active_plot;
  if(canvas->action == GTK_PLOT_CANVAS_ACTION_INACTIVE) return TRUE;
  if(active_plot == NULL && canvas->action != GTK_PLOT_CANVAS_ACTION_SELECTION)
     return TRUE;


  gtk_plot_canvas_get_position(canvas,
                               canvas->drag_area.width, 
                               canvas->drag_area.height, 
                               &new_width, &new_height);

  gtk_plot_canvas_get_position(canvas,
                               canvas->drag_area.x, 
                               canvas->drag_area.y, 
                               &new_x, &new_y);

  gtk_plot_canvas_get_position(canvas,
                               canvas->drag_area.x - 
                               canvas->active_item.allocation.x, 
                               canvas->drag_area.y - 
                               canvas->active_item.allocation.y, 
                               &dx, &dy);


  if(canvas->action != GTK_PLOT_CANVAS_ACTION_SELECTION) {
   switch(canvas->active_item.type){
    case GTK_PLOT_CANVAS_TEXT:
         child = (GtkPlotCanvasChild *)canvas->active_item.data;
         text = (GtkPlotText *)child->data;
         _gtkextra_signal_emit(GTK_OBJECT(canvas), canvas_signals[MOVE_ITEM],
                         &canvas->active_item, 
                         text->x + dx, text->y + dy, &veto);
         if(!veto) break;
         text->x += dx;
         text->y += dy;
         x1 += dx;
         x2 += dx;
         y1 += dy;
         y2 += dy;

         gtk_signal_emit (GTK_OBJECT(canvas), canvas_signals[CHANGED]);

         break;
    case GTK_PLOT_CANVAS_MARKER:
         marker = (GtkPlotMarker *)canvas->active_item.data;
         active_point = marker->point;
         {
           gboolean move = FALSE;
           gint i;

           x = canvas->pointer_x; 
           y = canvas->pointer_y; 
           for(i = 0; i < marker->data->num_points; i++){
              gdouble px, py;

	      gtk_plot_data_get_point(marker->data, i,
	  	  		   &fx, &fy, &fz, &fa, &fdx, &fdy, &fdz, &fda,
				   &label, &error); 
              gtk_plot_get_pixel(marker->data->plot,
                                 fx, fy, &px, &py);
             
              if(fabs(x - px) <= DEFAULT_MARKER_SIZE && 
                 fabs(y - py) <= DEFAULT_MARKER_SIZE) {
                      move = TRUE;
                      x = px;
                      y = py;
                      break;
              }
           }
           if(!move) break;
           active_point = i;
         }
         if(active_point == marker->point) break;

	 gtk_plot_data_get_point(marker->data, active_point,
	 	  		 &fx, &fy, &fz, &fa, &fdx, &fdy, &fdz, &fda,
				 &label, &error); 
         _gtkextra_signal_emit(GTK_OBJECT(canvas), canvas_signals[MOVE_ITEM],
                         &canvas->active_item, fx, fy, &veto);
         if(!veto) break;
         marker->point = active_point;
         gtk_signal_emit (GTK_OBJECT(canvas), canvas_signals[CHANGED]);
         break;
    case GTK_PLOT_CANVAS_RECTANGLE:
    case GTK_PLOT_CANVAS_LINE:
    case GTK_PLOT_CANVAS_ELLIPSE:
    case GTK_PLOT_CANVAS_PIXMAP:
    case GTK_PLOT_CANVAS_CUSTOM:
         child = (GtkPlotCanvasChild *)canvas->active_item.data;
         x1 = MIN(child->rx1, child->rx2);
         y1 = MIN(child->ry1, child->ry2);
         x2 = MAX(child->rx1, child->rx2);
         y2 = MAX(child->ry1, child->ry2);
         x1 += dx;
         y1 += dy;
         x2 = x1 + new_width;
         y2 = y1 + new_height;
         _gtkextra_signal_emit(GTK_OBJECT(canvas), canvas_signals[MOVE_ITEM],
                         child, 
                         x2, y2, &veto);
         if(canvas->action != GTK_PLOT_CANVAS_ACTION_DRAG)
            _gtkextra_signal_emit(GTK_OBJECT(canvas), 
                            canvas_signals[RESIZE_ITEM],
                            child, new_width, new_height, &veto);
         if(!veto) break;
         if(child->rx1 <= child->rx2) {
              child->rx1 = x1; 
              child->rx2 = x2; 
         } else {
              child->rx1 = x2; 
              child->rx2 = x1; 
         }
         if(child->ry1 <= child->ry2) {
              child->ry1 = y1; 
              child->ry2 = y2; 
         } else {
              child->ry1 = y2; 
              child->ry2 = y1; 
         }
         gtk_signal_emit (GTK_OBJECT(canvas), canvas_signals[CHANGED]);
         break;
    case GTK_PLOT_CANVAS_TITLE:
         axis = (GtkPlotAxis *)canvas->active_item.data;
         text = &axis->title;
         _gtkextra_signal_emit(GTK_OBJECT(canvas), canvas_signals[MOVE_ITEM],
                         &canvas->active_item, 
                         text->x + dx, text->y + dy, &veto);
         if(!veto) break;
         text->x += dx;
         text->y += dy;
         gtk_signal_emit (GTK_OBJECT(canvas), canvas_signals[CHANGED]);
         break;
    case GTK_PLOT_CANVAS_LEGENDS:
         gtk_plot_legends_get_position(active_plot, &new_x, &new_y);
         new_x += dx / active_plot->width;
         new_y += dy / active_plot->height;
         _gtkextra_signal_emit(GTK_OBJECT(canvas), canvas_signals[MOVE_ITEM],
                         &canvas->active_item, new_x, new_y, 
                         &veto);
         if(!veto) break;
         gtk_plot_legends_move(active_plot, new_x, new_y);
         gtk_signal_emit (GTK_OBJECT(canvas), canvas_signals[CHANGED]);
         break;
    case GTK_PLOT_CANVAS_PLOT:
         _gtkextra_signal_emit(GTK_OBJECT(canvas), 
                         canvas_signals[MOVE_ITEM],
                         &canvas->active_item, new_x, new_y, &veto);
         if(canvas->action != GTK_PLOT_CANVAS_ACTION_DRAG)
            _gtkextra_signal_emit(GTK_OBJECT(canvas), 
                            canvas_signals[RESIZE_ITEM],
                            &canvas->active_item, new_width, new_height, &veto);
         if(!veto) break;
         gtk_plot_move_resize(active_plot, 
                              new_x, 
                              new_y,
                              new_width,
                              new_height);

         gtk_signal_emit (GTK_OBJECT(canvas), canvas_signals[CHANGED]);
         break;
    case GTK_PLOT_CANVAS_DATA:
         break;
    case GTK_PLOT_CANVAS_AXIS:
    case GTK_PLOT_CANVAS_NONE:
    default:
         break;
   }
  } else {
    gtk_plot_canvas_get_position(canvas, 
                                 canvas->drag_x, canvas->drag_y,
				 &x1, &y1);
    gtk_plot_canvas_get_position(canvas, 
                                 canvas->pointer_x, canvas->pointer_y,
				 &x2, &y2);
    new_width = abs(canvas->pointer_x - canvas->drag_x);
    new_height = abs(canvas->pointer_y - canvas->drag_y);
    gtk_signal_emit(GTK_OBJECT(canvas), canvas_signals[SELECT_REGION],
                    x1, y1, x2, y2);
    canvas->state = GTK_STATE_NORMAL;
    canvas->action = GTK_PLOT_CANVAS_ACTION_INACTIVE;
    return TRUE;
  }

  canvas->drag_x = canvas->pointer_x; 
  canvas->drag_y = canvas->pointer_y; 
  canvas->active_item.allocation.x = canvas->drag_area.x;
  canvas->active_item.allocation.y = canvas->drag_area.y;

  if(veto){
      gtk_plot_canvas_paint(canvas);
      gtk_widget_queue_draw(GTK_WIDGET(canvas));
      canvas->active_item.allocation = canvas->drag_area;
      if((canvas->active_item.type == GTK_PLOT_CANVAS_DATA &&
          canvas->action == GTK_PLOT_CANVAS_ACTION_SELECTION) ||
         (canvas->active_item.type != GTK_PLOT_CANVAS_DATA &&
          canvas->action != GTK_PLOT_CANVAS_ACTION_SELECTION))
             draw_selection(canvas, canvas->active_item.allocation);
  } else {
      canvas->state = GTK_STATE_NORMAL;
  }  

  if(canvas->active_item.type == GTK_PLOT_CANVAS_DATA &&
     canvas->action != GTK_PLOT_CANVAS_ACTION_SELECTION)
             canvas->state = GTK_STATE_NORMAL;
  canvas->active_data = NULL;
  canvas->active_point = -1;
  canvas->action = GTK_PLOT_CANVAS_ACTION_INACTIVE;
 
  return TRUE;

}

static gint
gtk_plot_canvas_focus_in(GtkWidget *widget, GdkEventFocus *event)
{
  GTK_WIDGET_SET_FLAGS(widget, GTK_HAS_FOCUS);
  return FALSE;
}


static gint
gtk_plot_canvas_focus_out(GtkWidget *widget, GdkEventFocus *event)
{
  GTK_WIDGET_UNSET_FLAGS(widget, GTK_HAS_FOCUS);
  gtk_plot_canvas_unselect(GTK_PLOT_CANVAS(widget));

  return FALSE;
}

void
gtk_plot_canvas_add_plot (GtkPlotCanvas *plot_canvas, 
			  GtkPlot *plot, gdouble x, gdouble y)
{
  gint width, height;

  width = plot_canvas->pixmap_width, 
  height = plot_canvas->pixmap_height;

  gtk_plot_set_magnification(plot, plot_canvas->magnification);
  gtk_widget_set_usize(GTK_WIDGET(plot), width, height); 
  plot_canvas->plots = g_list_append(plot_canvas->plots, plot);
  gtk_plot_canvas_set_plots_pixmap(plot_canvas);

  gtk_fixed_put(GTK_FIXED(plot_canvas), GTK_WIDGET(plot), 0, 0);

  GTK_WIDGET(plot)->allocation.width = width;
  GTK_WIDGET(plot)->allocation.height = height;

  gtk_plot_move(plot, x, y);

  plot->left->title.x = plot->x - .1;
  plot->right->title.x = plot->x + plot->width + .1;
  plot->top->title.y = plot->y - .05;
  plot->bottom->title.y = plot->y + plot->height + .055;

  plot_canvas->active_plot = plot;

  plot_canvas->num_plots++;
  gtk_signal_emit (GTK_OBJECT(plot_canvas), canvas_signals[CHANGED]);
}

static void
gtk_plot_canvas_set_plots_pixmap(GtkPlotCanvas *plot_canvas)
{
  GdkRectangle area;
  GList *plots;
  GtkPlot *plot;

  if(!plot_canvas->pixmap) return;

  plots = plot_canvas->plots;
  while(plots)
    {
      plot = GTK_PLOT(plots->data);
      gtk_plot_set_drawable(plot, plot_canvas->pixmap);
      area.x = GTK_WIDGET(plot)->allocation.x;
      area.y = GTK_WIDGET(plot)->allocation.y;
      area.width = GTK_WIDGET(plot)->allocation.width;
      area.height = GTK_WIDGET(plot)->allocation.height;

      plots = plots->next;
    }

}

void
gtk_plot_canvas_set_transparent (GtkPlotCanvas *canvas, gboolean transparent)
{
  g_return_if_fail (canvas != NULL);
  g_return_if_fail (GTK_IS_PLOT_CANVAS (canvas));

  canvas->transparent = transparent;
}

gboolean
gtk_plot_canvas_transparent (GtkPlotCanvas *canvas)
{
  g_return_val_if_fail (canvas != NULL, TRUE);
  g_return_val_if_fail (GTK_IS_PLOT_CANVAS (canvas), TRUE);

  return(canvas->transparent);
}

void
gtk_plot_canvas_set_background (GtkPlotCanvas *canvas, const GdkColor *color)
{

  g_return_if_fail (canvas != NULL);
  g_return_if_fail (GTK_IS_PLOT_CANVAS (canvas));

  if(!color) {
    canvas->transparent = TRUE;
    return;
  } else {
    canvas->background = *color;
    canvas->transparent = FALSE;
  }

  if(GTK_WIDGET_REALIZED(GTK_WIDGET(canvas)))
       gtk_plot_canvas_paint(canvas);

  gtk_signal_emit (GTK_OBJECT(canvas), canvas_signals[CHANGED]);
}

void
gtk_plot_canvas_get_pixel(GtkPlotCanvas *canvas, gdouble px, gdouble py,
                          gint *x, gint *y)
{
  *x = canvas->pixmap_width * px;
  *y = canvas->pixmap_height * py;
}

void
gtk_plot_canvas_get_position(GtkPlotCanvas *plot_canvas, gint x, gint y,
                             gdouble *px, gdouble *py)
{
  *px = (gdouble) x / (gdouble) plot_canvas->pixmap_width;
  *py = (gdouble) y / (gdouble) plot_canvas->pixmap_height;
}

static void
gtk_plot_canvas_get_real_pixel(GtkWidget *widget, gdouble px, gdouble py,
                               gint *x, gint *y)
{
  *x = widget->allocation.x + widget->allocation.width * px;
  *y = widget->allocation.y + widget->allocation.height * py;
}

void
gtk_plot_canvas_set_active_plot (GtkPlotCanvas *plot_canvas, GtkPlot *plot)
{
  plot_canvas->active_plot = plot;
}

void
gtk_plot_canvas_unselect (GtkPlotCanvas *plot_canvas)
{

  if(plot_canvas->state == GTK_STATE_SELECTED){
    draw_selection(plot_canvas, plot_canvas->drag_area); 
  }

  plot_canvas->action = GTK_PLOT_CANVAS_ACTION_INACTIVE;
  plot_canvas->state = GTK_STATE_NORMAL;
  plot_canvas->active_item.type = GTK_PLOT_CANVAS_NONE;
  plot_canvas->active_item.data = NULL;

  if(GTK_WIDGET_MAPPED(GTK_WIDGET(plot_canvas))){
      gdk_cursor_destroy(plot_canvas->cursor);
      plot_canvas->cursor = gdk_cursor_new(GDK_TOP_LEFT_ARROW);
      gdk_window_set_cursor(GTK_WIDGET(plot_canvas)->window, 
                            plot_canvas->cursor);
  }
}

void
gtk_plot_canvas_cancel_action (GtkPlotCanvas *plot_canvas)
{
  gtk_plot_canvas_unselect(plot_canvas);
  gdk_pointer_ungrab(GDK_CURRENT_TIME);
}


GtkPlot *
gtk_plot_canvas_get_active_plot(GtkPlotCanvas *canvas)
{
  return canvas->active_plot;
}


GtkPlotData *
gtk_plot_canvas_get_active_data(GtkPlotCanvas *canvas)
{
  return canvas->active_data;
}

gint
gtk_plot_canvas_get_active_point(GtkPlotCanvas *canvas, gdouble *x, gdouble *y)
{
  if (canvas->active_point >= 0 && canvas->active_data != NULL) {
    *x = canvas->active_data->x[canvas->active_point];
    *y = canvas->active_data->y[canvas->active_point];
  } else {
    *x = 0.0;
    *y = 0.0;
  }
/* Returns -1 if no point is active. */
  return canvas->active_point;
}

GtkPlotCanvasChild *
gtk_plot_canvas_get_active_item(GtkPlotCanvas *canvas)
{
  return (&canvas->active_item);
}

void
gtk_plot_canvas_set_size(GtkPlotCanvas *canvas, gint width, gint height)
{
  GList *plots;
  GtkPlot *plot;
  gdouble m = canvas->magnification;

  gtk_plot_canvas_cancel_action(canvas);

  canvas->width = width;
  canvas->height = height;
  canvas->pixmap_width = roundint(m * width);
  canvas->pixmap_height = roundint(m * height);

  if(GTK_WIDGET_MAPPED(canvas)){ 
    gtk_plot_canvas_create_pixmap(GTK_WIDGET(canvas), 
                                  canvas->pixmap_width, 
                                  canvas->pixmap_height);
  }

  plots = canvas->plots;
  while(plots)
     {
       plot = (GtkPlot *)plots->data;
 
       gtk_widget_set_usize(GTK_WIDGET(plot), 
                            canvas->pixmap_width, canvas->pixmap_height); 
       gtk_signal_emit_by_name(GTK_OBJECT(plot), "update");

       plots = plots->next;
     }
  gtk_widget_set_usize(GTK_WIDGET(canvas), 
                       canvas->pixmap_width, canvas->pixmap_height);

  gtk_signal_emit (GTK_OBJECT(canvas), canvas_signals[CHANGED]);
}

static void
gtk_plot_canvas_create_pixmap(GtkWidget *widget, gint width, gint height)
{
  GtkPlotCanvas *canvas;
  gint pixmap_width, pixmap_height;

  canvas = GTK_PLOT_CANVAS(widget);
  if (!canvas->pixmap)
    canvas->pixmap = gdk_pixmap_new (widget->window,
                                     width,
                                     height, -1);
  else{
    gdk_window_get_size(canvas->pixmap, &pixmap_width, &pixmap_height);
    if(width != pixmap_width || height != pixmap_height)
        gdk_pixmap_unref(canvas->pixmap);
        canvas->pixmap = gdk_pixmap_new (widget->window,
                                         width,
                                         height, -1);
  }

  if(canvas->pc && GTK_IS_PLOT_GDK(canvas->pc)){
       GTK_PLOT_GDK(canvas->pc)->drawable = canvas->pixmap;
  }
  gtk_plot_pc_set_viewport(canvas->pc, width, height);

  gtk_plot_pc_set_color(canvas->pc, &canvas->background);

  gtk_plot_pc_draw_rectangle(canvas->pc,
                            TRUE,
                            0, 0, 
                            canvas->pixmap_width, 
                            canvas->pixmap_height);

  gtk_plot_canvas_set_plots_pixmap(canvas);

}

static gint
gtk_plot_canvas_expose(GtkWidget *widget, GdkEventExpose *event)
{
  GtkPlotCanvas *canvas;

  if(!GTK_WIDGET_DRAWABLE(widget)) return FALSE;

  canvas = GTK_PLOT_CANVAS(widget);

  if(!canvas->pixmap){
      gtk_plot_canvas_create_pixmap(widget, 
                                    canvas->pixmap_width, 
                                    canvas->pixmap_height);
      gtk_plot_canvas_paint(canvas);
  }

  /* copy the backing pixmap to the window */
  gtk_plot_canvas_refresh(canvas);

  GTK_WIDGET_CLASS(parent_class)->expose_event(widget, event);

  return FALSE;
}


static void
gtk_plot_canvas_size_request (GtkWidget *widget, GtkRequisition *requisition)
{
  GtkPlotCanvas *canvas;

  canvas = GTK_PLOT_CANVAS(widget);

  GTK_WIDGET_CLASS(parent_class)->size_request(widget, requisition);

  widget->requisition.width = MAX(canvas->pixmap_width, requisition->width);
  widget->requisition.height = MAX(canvas->pixmap_height, requisition->height);
}


void
gtk_plot_canvas_set_magnification(GtkPlotCanvas *canvas, 
                                  gdouble magnification)
{
  GList *plots;
  GtkPlot *plot;
  gdouble m;

  m = canvas->magnification = magnification;

  plots = canvas->plots;
  while(plots)
     {
       plot = (GtkPlot *)plots->data;
 
       gtk_plot_set_magnification(plot, m); 

       plots = plots->next;
     }

  gtk_plot_canvas_set_size(canvas, 
                           canvas->width, 
                           canvas->height);
  gtk_signal_emit (GTK_OBJECT(canvas), canvas_signals[CHANGED]);
}

static void
draw_selection (GtkPlotCanvas *canvas, 
                GdkRectangle area)
{
  GdkGC *xor_gc = NULL, *gc = NULL;
  GdkGCValues values;

  gdk_gc_get_values(GTK_WIDGET(canvas)->style->fg_gc[0], &values);
  values.function = GDK_INVERT;
  values.foreground = GTK_WIDGET(canvas)->style->white;
  values.subwindow_mode = GDK_INCLUDE_INFERIORS;
  xor_gc = gdk_gc_new_with_values(GTK_WIDGET(canvas)->window,
                                  &values,
                                  GDK_GC_FOREGROUND |
                                  GDK_GC_FUNCTION |
                                  GDK_GC_SUBWINDOW);

  if(canvas->active_item.type == GTK_PLOT_CANVAS_DATA ||
     canvas->active_item.selection == GTK_PLOT_CANVAS_SELECT_TARGET){

    area.x += area.width / 2 - 10;
    area.y += area.height / 2 - 10;
    area.width = 20;
    area.height = 20;
    gdk_draw_rectangle (GTK_WIDGET(canvas)->window,
                        xor_gc,
                        FALSE,
                        area.x, area.y,
                        area.width, area.height);         

    gdk_draw_line(GTK_WIDGET(canvas)->window, xor_gc,
                  area.x + 1, area.y + area.height/2, 
                  area.x + 6, area.y + area.height/2);
    gdk_draw_line(GTK_WIDGET(canvas)->window, xor_gc,
                  area.x + area.width - 1, area.y + area.height / 2, 
                  area.x + area.width - 6, area.y + area.height / 2);
    gdk_draw_line(GTK_WIDGET(canvas)->window, xor_gc,
                  area.x + area.width/2, area.y + 1, 
                  area.x + area.width/2, area.y + 6);
    gdk_draw_line(GTK_WIDGET(canvas)->window, xor_gc,
                  area.x + area.width/2, area.y + area.height - 1, 
                  area.x + area.width/2, area.y + area.height - 6);

    if(canvas->active_item.type == GTK_PLOT_CANVAS_DATA &&
       canvas->action == GTK_PLOT_CANVAS_ACTION_DRAG){

      gtk_plot_set_drawable(canvas->active_plot, GTK_WIDGET(canvas)->window);
     
      gc = GTK_PLOT_GDK(canvas->active_data->plot->pc)->gc;
      gdk_gc_ref(xor_gc);
      GTK_PLOT_GDK(canvas->active_data->plot->pc)->gc = xor_gc;
 
      gtk_plot_data_draw_points(canvas->active_data, canvas->active_data->num_points);

      GTK_PLOT_GDK(canvas->active_data->plot->pc)->gc = gc;
      gtk_plot_set_drawable(canvas->active_plot, canvas->pixmap);
    }

    if(xor_gc) gdk_gc_destroy(xor_gc);
    return;
  }


  if(canvas->active_item.selection == GTK_PLOT_CANVAS_SELECT_MARKERS){
     gdk_draw_rectangle (GTK_WIDGET(canvas)->window,
                         xor_gc,
                         FALSE,
                         area.x, area.y,
                         area.width, area.height);         
     draw_marker(canvas, xor_gc, area.x, area.y); 
     draw_marker(canvas, xor_gc, area.x, area.y + area.height); 
     draw_marker(canvas, xor_gc, area.x + area.width, area.y); 
     draw_marker(canvas, xor_gc, area.x + area.width, area.y + area.height); 
     if(area.height > DEFAULT_MARKER_SIZE * 2){
       draw_marker(canvas, xor_gc, area.x, area.y + area.height / 2); 
       draw_marker(canvas, xor_gc, area.x + area.width, 
                                   area.y + area.height / 2); 
     }
     if(area.width > DEFAULT_MARKER_SIZE * 2){
       draw_marker(canvas, xor_gc, area.x + area.width / 2, area.y); 
       draw_marker(canvas, xor_gc, area.x + area.width / 2, 
                                   area.y + area.height); 
     }
  } else {
     gdk_gc_set_line_attributes(xor_gc, 1, 1, 0 ,0 );

     gdk_draw_rectangle (GTK_WIDGET(canvas)->window,
                         xor_gc,
                         FALSE,
                         area.x, area.y,
                         area.width, area.height);         
  }
  if(xor_gc) gdk_gc_destroy(xor_gc);
}

static void
draw_marker(GtkPlotCanvas *canvas, GdkGC *gc, gint x, gint y)
{
  GdkDrawable *darea;

  darea = GTK_WIDGET(canvas)->window;

  gdk_draw_rectangle(darea, gc, TRUE,
                     x - DEFAULT_MARKER_SIZE / 2, y - DEFAULT_MARKER_SIZE / 2,
                     DEFAULT_MARKER_SIZE + 1, DEFAULT_MARKER_SIZE + 1);
}

static GtkPlotCanvasPos
posible_selection(GdkRectangle area, gint x, gint y)
{
  GtkPlotCanvasPos return_value = GTK_PLOT_CANVAS_OUT;

  if(x >= area.x - DEFAULT_MARKER_SIZE / 2 &&
     x <= area.x + DEFAULT_MARKER_SIZE / 2){
       if(y >= area.y - DEFAULT_MARKER_SIZE / 2. &&
          y <= area.y + DEFAULT_MARKER_SIZE / 2.)
                      return_value = GTK_PLOT_CANVAS_TOP_LEFT;
       if(y >= area.y + area.height - DEFAULT_MARKER_SIZE / 2. &&
          y <= area.y + area.height + DEFAULT_MARKER_SIZE / 2.)
                      return_value = GTK_PLOT_CANVAS_BOTTOM_LEFT;
       if(y >= area.y + area.height / 2 - DEFAULT_MARKER_SIZE / 2. &&
          y <= area.y + area.height / 2 + DEFAULT_MARKER_SIZE / 2. &&
          area.height > DEFAULT_MARKER_SIZE * 2)
                      return_value = GTK_PLOT_CANVAS_LEFT;
  }

  if(x >= area.x + area.width - DEFAULT_MARKER_SIZE / 2 &&
     x <= area.x + area.width + DEFAULT_MARKER_SIZE / 2){
       if(y >= area.y - DEFAULT_MARKER_SIZE / 2. &&
          y <= area.y + DEFAULT_MARKER_SIZE / 2.)
                      return_value = GTK_PLOT_CANVAS_TOP_RIGHT;
       if(y >= area.y + area.height - DEFAULT_MARKER_SIZE / 2. &&
          y <= area.y + area.height + DEFAULT_MARKER_SIZE / 2.)
                      return_value = GTK_PLOT_CANVAS_BOTTOM_RIGHT;
       if(y >= area.y + area.height / 2 - DEFAULT_MARKER_SIZE / 2. &&
          y <= area.y + area.height / 2 + DEFAULT_MARKER_SIZE / 2. &&
          area.height > DEFAULT_MARKER_SIZE * 2)
                      return_value = GTK_PLOT_CANVAS_RIGHT;
  }

  if(x >= area.x + area.width / 2 - DEFAULT_MARKER_SIZE / 2 &&
     x <= area.x + area.width / 2 + DEFAULT_MARKER_SIZE / 2 &&
     area.width > DEFAULT_MARKER_SIZE * 2){
       if(y >= area.y - DEFAULT_MARKER_SIZE / 2. &&
          y <= area.y + DEFAULT_MARKER_SIZE / 2.)
                      return_value = GTK_PLOT_CANVAS_TOP;
       if(y >= area.y + area.height - DEFAULT_MARKER_SIZE / 2. &&
          y <= area.y + area.height + DEFAULT_MARKER_SIZE / 2.)
                      return_value = GTK_PLOT_CANVAS_BOTTOM;
  }

  if(return_value == GTK_PLOT_CANVAS_OUT){
     if (x >= area.x && x <= area.x + area.width &&
         y >= area.y && y <= area.y + area.height)
                      return_value = GTK_PLOT_CANVAS_IN;
  }

  return (return_value);
}    

/**********************************************************************/

void
gtk_plot_canvas_grid_set_visible(GtkPlotCanvas *canvas, gboolean visible)
{
  canvas->show_grid= visible;
  gtk_signal_emit (GTK_OBJECT(canvas), canvas_signals[CHANGED]);
}

void
gtk_plot_canvas_grid_set_step(GtkPlotCanvas *canvas, gdouble step)
{
  canvas->grid_step = step;
  gtk_signal_emit (GTK_OBJECT(canvas), canvas_signals[CHANGED]);
}

void
gtk_plot_canvas_grid_set_attributes(GtkPlotCanvas *canvas,
		 	            GtkPlotLineStyle style,
			            gint width,
			            const GdkColor *color)
{
  if(color)
      canvas->grid.color = *color;
  canvas->grid.line_width = width;
  canvas->grid.line_style = style;
  gtk_signal_emit (GTK_OBJECT(canvas), canvas_signals[CHANGED]);
}

/**********************************************************************/
GtkPlotCanvasChild *
gtk_plot_canvas_put_text (GtkPlotCanvas *canvas,
                          gdouble x, gdouble y, 
                          const gchar *font, gint height, gint angle,
                          const GdkColor *fg, const GdkColor *bg,
			  gboolean transparent,
                          GtkJustification justification,
                          const gchar *text)
{
  GtkWidget *widget;
  GtkPlotText *text_attr;
  GtkPlotCanvasChild *child;

  widget = GTK_WIDGET(canvas);

  child = gtk_plot_canvas_child_new(GTK_PLOT_CANVAS_TEXT);

  text_attr = (GtkPlotText *)child->data;

  text_attr->x = x;
  text_attr->y = y;
  text_attr->angle = angle;
  text_attr->fg = widget->style->black;
  text_attr->bg = widget->style->white;
  text_attr->justification = justification;
  text_attr->transparent = transparent;
  text_attr->border = 0;
  text_attr->border_width = 0;
  text_attr->shadow_width = 0;

  if(!font) {
    text_attr->font = g_strdup(DEFAULT_FONT);
    text_attr->height = DEFAULT_FONT_HEIGHT;
  } else {
    text_attr->font = g_strdup(font);
    text_attr->height = height;
  }

  text_attr->text = NULL;
  if(text) text_attr->text = g_strdup(text);

  if(fg != NULL)
    text_attr->fg = *fg;

  if(bg != NULL)
    text_attr->bg = *bg;

  canvas->childs = g_list_append(canvas->childs, child);
  gtk_plot_canvas_draw_child(canvas, child);

  gtk_signal_emit (GTK_OBJECT(canvas), canvas_signals[CHANGED]);
  return child;
}

GtkPlotCanvasChild *
gtk_plot_canvas_put_line(GtkPlotCanvas *canvas,
                         gdouble x1, gdouble y1, 
                         gdouble x2, gdouble y2, 
		 	 GtkPlotLineStyle style,
			 gfloat width,
			 const GdkColor *color,
			 GtkPlotCanvasArrow arrow_mask)
{
  GtkPlotCanvasChild *child;
  GtkPlotCanvasLine *line;

  child = gtk_plot_canvas_child_new(GTK_PLOT_CANVAS_LINE);
  line = (GtkPlotCanvasLine *)child->data;

  gdk_color_black(gdk_colormap_get_system(), &line->line.color); 
  
  line->arrow_length = 8;
  line->arrow_width = 8;
  line->arrow_style = GTK_PLOT_SYMBOL_FILLED;

  gtk_plot_canvas_line_set_attributes(child, style, width, color, arrow_mask);

  gtk_plot_canvas_put_child(canvas, child, x1, y1, x2, y2);
  return child;
}

GtkPlotCanvasChild *
gtk_plot_canvas_put_rectangle(GtkPlotCanvas *canvas,
                              gdouble x1, gdouble y1, 
                              gdouble x2, gdouble y2, 
		  	      GtkPlotLineStyle style,
			      gfloat width,
			      const GdkColor *fg,
			      const GdkColor *bg,
			      GtkPlotBorderStyle border,
			      gboolean fill)
{
  GtkPlotCanvasChild *child;
  GtkPlotCanvasRectangle *rectangle;

  child = gtk_plot_canvas_child_new(GTK_PLOT_CANVAS_RECTANGLE);
  rectangle = (GtkPlotCanvasRectangle *)child->data;
 
  gdk_color_black(gdk_colormap_get_system(), &rectangle->line.color); 
  gdk_color_white(gdk_colormap_get_system(), &rectangle->bg); 

  gtk_plot_canvas_rectangle_set_attributes(child, style, width, 
                                           fg, bg, border, fill);
  rectangle->shadow_width = SHADOW_WIDTH;

  gtk_plot_canvas_put_child(canvas, child, x1, y1, x2, y2);
  return child;
}

GtkPlotCanvasChild *
gtk_plot_canvas_put_ellipse(GtkPlotCanvas *canvas,
                            gdouble x1, gdouble y1, 
                            gdouble x2, gdouble y2, 
		  	    GtkPlotLineStyle style,
			    gfloat width,
			    const GdkColor *fg,
			    const GdkColor *bg,
			    gboolean fill)
{
  GtkPlotCanvasChild *child;
  GtkPlotCanvasEllipse *ellipse;

  child = gtk_plot_canvas_child_new(GTK_PLOT_CANVAS_ELLIPSE);
  ellipse = (GtkPlotCanvasEllipse *)child->data;
 
  gdk_color_black(gdk_colormap_get_system(), &ellipse->line.color); 
  gdk_color_white(gdk_colormap_get_system(), &ellipse->bg); 

  gtk_plot_canvas_ellipse_set_attributes(child, style, width, 
                                         fg, bg, fill);

  gtk_plot_canvas_put_child(canvas, child, x1, y1, x2, y2);
  return child;
}

GtkPlotCanvasChild *
gtk_plot_canvas_put_pixmap (GtkPlotCanvas *canvas,
                            GdkPixmap *pixmap,
                            gdouble x1, gdouble y1)
{
  GtkPlotCanvasChild *child;
  gint width, height;
  gdouble x2, y2;

  if(!pixmap) return NULL;

  child = gtk_plot_canvas_child_new(GTK_PLOT_CANVAS_PIXMAP);
  child->data = pixmap;
  gdk_pixmap_ref(pixmap);

  if(pixmap) {
    gdk_window_get_size(pixmap, &width, &height);
    x2 = x1 + (gdouble)width / (gdouble)canvas->width;
    y2 = y1 + (gdouble)height / (gdouble)canvas->height;
  } else {
    x2 = x1 + 0.1;
    y2 = y1 + 0.05;
  }
 
  gtk_plot_canvas_put_child(canvas, child, x1, y1, x2, y2);
  return child;
}

void
gtk_plot_canvas_line_set_attributes(GtkPlotCanvasChild *child,
		 	            GtkPlotLineStyle style,
			            gfloat width,
			            const GdkColor *color,
			            GtkPlotCanvasArrow mask)
{
  GtkPlotCanvasLine *line;

  g_return_if_fail(child->type == GTK_PLOT_CANVAS_LINE);

  line = (GtkPlotCanvasLine *)child->data;  
  if(color)
      line->line.color = *color;
  line->line.line_width = width;
  line->line.line_style = style;
  line->arrow_mask = mask;
}

void
gtk_plot_canvas_rectangle_set_attributes(GtkPlotCanvasChild *child,
		  	                 GtkPlotLineStyle style,
			                 gfloat width,
			                 const GdkColor *fg,
			                 const GdkColor *bg,
				         GtkPlotBorderStyle border,
			                 gboolean fill)
{
  GtkPlotCanvasRectangle *rectangle;

  g_return_if_fail(child->type == GTK_PLOT_CANVAS_RECTANGLE);

  rectangle = (GtkPlotCanvasRectangle *)child->data;  
  if(fg)
    rectangle->line.color = *fg;
  if(bg)
    rectangle->bg = *bg;
  rectangle->line.line_width = width;
  rectangle->line.line_style = style;
  rectangle->filled = fill;
  rectangle->border = border;
}

void
gtk_plot_canvas_ellipse_set_attributes(GtkPlotCanvasChild *child,
		  	               GtkPlotLineStyle style,
			               gfloat width,
			               const GdkColor *fg,
			               const GdkColor *bg,
			               gboolean fill)
{
  GtkPlotCanvasEllipse *ellipse;

  g_return_if_fail(child->type == GTK_PLOT_CANVAS_ELLIPSE);

  ellipse = (GtkPlotCanvasEllipse *)child->data;  
  if(fg)
    ellipse->line.color = *fg;
  if(bg)
    ellipse->bg = *bg;
  ellipse->line.line_width = width;
  ellipse->line.line_style = style;
  ellipse->filled = fill;
}


GtkPlotCanvasChild *
gtk_plot_canvas_child_new(GtkPlotCanvasType type)
{
  GtkPlotCanvasChild *child;
  child = g_new0(GtkPlotCanvasChild, 1);
  child->type = type;

  child->flags = GTK_PLOT_CANVAS_CAN_MOVE | 
                 GTK_PLOT_CANVAS_CAN_X_RESIZE |
                 GTK_PLOT_CANVAS_CAN_Y_RESIZE;

  child->min_width = -1;
  child->min_height = -1;

  child->selection = GTK_PLOT_CANVAS_SELECT_MARKERS;
  child->mode = GTK_PLOT_CANVAS_SELECT_CLICK_2;

  switch(type){
    case GTK_PLOT_CANVAS_LINE:
	child->data = g_new0(GtkPlotCanvasLine, 1);
        break;    
    case GTK_PLOT_CANVAS_RECTANGLE:
	child->data = g_new0(GtkPlotCanvasRectangle, 1);
        break;    
    case GTK_PLOT_CANVAS_ELLIPSE:
	child->data = g_new0(GtkPlotCanvasEllipse, 1);
        break;    
    case GTK_PLOT_CANVAS_TEXT:
	child->data = g_new0(GtkPlotText, 1);
        child->flags = GTK_PLOT_CANVAS_CAN_MOVE;
        break;    
    case GTK_PLOT_CANVAS_PIXMAP:
        child->data = NULL;
        break;
    case GTK_PLOT_CANVAS_CUSTOM:
        child->data = NULL;
        child->flags = GTK_PLOT_CANVAS_CAN_MOVE;
    default:
	break;
  }
  
  child->draw_child = NULL;
  return child;
}

void
gtk_plot_canvas_put_child(GtkPlotCanvas *canvas, 
                          GtkPlotCanvasChild *child,
			  gdouble x1, gdouble y1, 
                          gdouble x2, gdouble y2)
{
  child->rx1 = x1;
  child->ry1 = y1;
  child->rx2 = x2;
  child->ry2 = y2;

  canvas->childs = g_list_append(canvas->childs, child);
  gtk_plot_canvas_draw_child(canvas, child);
  gtk_signal_emit (GTK_OBJECT(canvas), canvas_signals[CHANGED]);
}

void
gtk_plot_canvas_child_move(GtkPlotCanvas *canvas, 
                           GtkPlotCanvasChild *child,
			   gdouble x1, gdouble y1) 
{
  child->rx2 += (x1 - child->rx1);
  child->ry2 += (y1 - child->ry1);
  child->rx1 = x1;
  child->ry1 = y1;

  gtk_plot_canvas_paint(canvas);
  gtk_widget_queue_draw(GTK_WIDGET(canvas));
  gtk_signal_emit (GTK_OBJECT(canvas), canvas_signals[CHANGED]);
}

void
gtk_plot_canvas_child_move_resize(GtkPlotCanvas *canvas, 
                                  GtkPlotCanvasChild *child,
			          gdouble x1, gdouble y1,
				  gdouble x2, gdouble y2) 
{
  child->rx1 = x1;
  child->ry1 = y1;
  child->rx2 = x2;
  child->ry2 = y2;

  gtk_plot_canvas_paint(canvas);
  gtk_widget_queue_draw(GTK_WIDGET(canvas));
  gtk_signal_emit (GTK_OBJECT(canvas), canvas_signals[CHANGED]);
}

gboolean
gtk_plot_canvas_remove_child(GtkPlotCanvas *canvas, 
                             GtkPlotCanvasChild *child)
{
  GList *list;
  gpointer data;

  list = canvas->childs;

  while(list)
   {
     data = list->data;

     if((GtkPlotCanvasChild *)data == child){
        gboolean veto = TRUE;

        gtk_signal_emit(GTK_OBJECT(canvas), canvas_signals[DELETE_ITEM],
                        child, &veto);

        if(veto){
          if(child->type == GTK_PLOT_CANVAS_TEXT){
             GtkPlotText *text;
     
             text = (GtkPlotText *)child->data;
   
             if(text->font) g_free(text->font); 
             if(text->text) g_free(text->text); 
          }
          if(child->type == GTK_PLOT_CANVAS_PIXMAP){
             GdkPixmap *pixmap;
     
             pixmap = (GdkPixmap *)child->data;
             gdk_pixmap_unref(pixmap); 
          } else {
             if(child->data)
                 g_free(child->data);
          }

          g_free(child);

          canvas->childs = g_list_remove_link(canvas->childs, list);
          g_list_free_1(list);
          gtk_signal_emit (GTK_OBJECT(canvas), canvas_signals[CHANGED]);
        }

        return TRUE;
     }
     list = list->next;
   }

  return FALSE;

}

void
gtk_plot_canvas_child_set_selection (GtkPlotCanvasChild *child,
				     GtkPlotCanvasSelection selection)
{
  if(!child) return;
  child->selection = selection;
}

void
gtk_plot_canvas_child_set_selection_mode (GtkPlotCanvasChild *child,
				          GtkPlotCanvasSelectionMode mode)
{
  if(!child) return;
  child->mode = mode;
}

/**********************************************************************/
static void
gtk_plot_canvas_draw_child(GtkPlotCanvas *canvas,
                           GtkPlotCanvasChild *child)
{
  GtkPlotCanvasLine *line = NULL;
  GtkPlotCanvasRectangle *rectangle = NULL;
  GtkPlotCanvasEllipse *ellipse = NULL;
  GtkPlotText *text = NULL;
  GtkPlotPoint arrow[3];
  GdkColormap *colormap = NULL;
  GdkColor white, black;
  gint rx1 = 0, ry1 = 0, rx2 = 0, ry2 = 0;
  gint xmin, xmax, ymin, ymax;
  gint width = 0, height = 0;
  gint xm = 0, ym = 0;
  gdouble angle = 0.;
  gdouble m = canvas->magnification;
  gdouble scale_x, scale_y;

  if(!canvas->pixmap) return;

  gtk_plot_pc_gsave(canvas->pc);

  gtk_plot_canvas_get_pixel(canvas, child->rx1, child->ry1, &rx1, &ry1);
  gtk_plot_canvas_get_pixel(canvas, child->rx2, child->ry2, &rx2, &ry2);

  xmin = MIN(rx1, rx2);
  xmax = MAX(rx1, rx2);
  ymin = MIN(ry1, ry2);
  ymax = MAX(ry1, ry2);

  width = abs(rx2 - rx1);
  height = abs(ry2 - ry1);

  child->allocation.x = xmin;
  child->allocation.y = ymin;
  child->allocation.width = width;
  child->allocation.height = height;

  switch(child->type){
    case GTK_PLOT_CANVAS_LINE:
        line = (GtkPlotCanvasLine *)child->data;

        if(width == 0 && height == 0) return;
        if(width != 0) 
            angle = atan2((gdouble)(ry2 - ry1), (gdouble)(rx2 - rx1));  
        else   
            angle = asin((ry2 - ry1)/height);

        gtk_plot_canvas_set_line_attributes(canvas, line->line);
        gtk_plot_pc_draw_line(canvas->pc, rx1, ry1, rx2, ry2);
        gtk_plot_pc_set_lineattr(canvas->pc, line->line.line_width, 0, 0, 0);
        if(line->arrow_mask & GTK_PLOT_ARROW_END){
           arrow[1].x = rx2;
           arrow[1].y = ry2;
           xm = rx2 - cos(angle) * line->arrow_length * m;
           ym = ry2 - sin(angle) * line->arrow_length * m;
           arrow[0].x = xm - sin(angle)* line->arrow_width * m / 2.0;
           arrow[0].y = ym + cos(angle)* line->arrow_width * m / 2.0;
           arrow[2].x = xm + sin(angle)* line->arrow_width * m / 2.0;
           arrow[2].y = ym - cos(angle)* line->arrow_width * m / 2.0;
           switch(line->arrow_style){
             case GTK_PLOT_SYMBOL_EMPTY:
               gtk_plot_pc_draw_lines (canvas->pc, arrow, 3);
               break;
             case GTK_PLOT_SYMBOL_OPAQUE:
               gtk_plot_pc_set_color(canvas->pc, &canvas->background);
               gtk_plot_pc_draw_polygon (canvas->pc, TRUE, arrow, 3);
               gtk_plot_pc_set_color(canvas->pc, &line->line.color);
               gtk_plot_pc_draw_polygon (canvas->pc, FALSE, arrow, 3);
               break;
             case GTK_PLOT_SYMBOL_FILLED:
               gtk_plot_pc_draw_polygon (canvas->pc, TRUE, arrow, 3);
           }
        }
        if(line->arrow_mask & GTK_PLOT_ARROW_ORIGIN){
           arrow[1].x = rx1;
           arrow[1].y = ry1;
           xm = rx1 + cos(angle) * line->arrow_length * m;
           ym = ry1 + sin(angle) * line->arrow_length * m;
           arrow[0].x = xm + sin(angle)* line->arrow_width * m / 2.0;
           arrow[0].y = ym - cos(angle)* line->arrow_width * m / 2.0;
           arrow[2].x = xm - sin(angle)* line->arrow_width * m / 2.0;
           arrow[2].y = ym + cos(angle)* line->arrow_width * m / 2.0;
           switch(line->arrow_style){
             case GTK_PLOT_SYMBOL_EMPTY:
               gtk_plot_pc_draw_lines (canvas->pc, arrow, 3);
               break;
             case GTK_PLOT_SYMBOL_OPAQUE:
               gtk_plot_pc_set_color(canvas->pc, &canvas->background);
               gtk_plot_pc_draw_polygon (canvas->pc, TRUE, arrow, 3);
               gtk_plot_pc_set_color(canvas->pc, &line->line.color);
               gtk_plot_pc_draw_polygon (canvas->pc, FALSE, arrow, 3);
               break;
             case GTK_PLOT_SYMBOL_FILLED:
               gtk_plot_pc_draw_polygon (canvas->pc, TRUE, arrow, 3);
           }
        }
        break;    
    case GTK_PLOT_CANVAS_RECTANGLE:
        rectangle = (GtkPlotCanvasRectangle *)child->data;
        if(rectangle->filled){
           gtk_plot_pc_set_color(canvas->pc, &rectangle->bg);
           gtk_plot_pc_draw_rectangle(canvas->pc, TRUE, 
                                     xmin, ymin, width, height);
        }
        if(rectangle->line.line_style != GTK_PLOT_LINE_NONE &&
           rectangle->border != GTK_PLOT_BORDER_NONE){

            gtk_plot_canvas_set_line_attributes(canvas, rectangle->line);
            gtk_plot_pc_draw_rectangle(canvas->pc, FALSE, 
                                      xmin, ymin, width, height);
            if(rectangle->border == GTK_PLOT_BORDER_SHADOW){
              gtk_plot_pc_draw_rectangle(canvas->pc,
                                 TRUE,
                                 xmin + roundint(rectangle->shadow_width * m),
                                 ymin + height,
                                 width, roundint(rectangle->shadow_width * m));
              gtk_plot_pc_draw_rectangle(canvas->pc,
                                 TRUE,
                                 xmin + width,
                                 ymin + roundint(rectangle->shadow_width * m),
                                 roundint(rectangle->shadow_width * m), height);
            }
        }
        break;    
    case GTK_PLOT_CANVAS_ELLIPSE:
        ellipse = (GtkPlotCanvasEllipse *)child->data;
        if(ellipse->filled){
           gtk_plot_pc_set_color(canvas->pc, &ellipse->bg);
           gtk_plot_pc_draw_ellipse(canvas->pc, TRUE, 
                                   xmin, ymin, width, height);
        }
        gtk_plot_canvas_set_line_attributes(canvas, ellipse->line);
        if(ellipse->line.line_style != GTK_PLOT_LINE_NONE)
           gtk_plot_pc_draw_ellipse(canvas->pc, FALSE, 
                                   xmin, ymin, width, height);
        break;    
    case GTK_PLOT_CANVAS_TEXT:
        text = (GtkPlotText *)child->data;
        gtk_plot_canvas_draw_text(canvas, child);
        break;
    case GTK_PLOT_CANVAS_PIXMAP:
        if(child->data){
          gdk_window_get_size((GdkPixmap *)child->data, &width, &height);
          scale_x = (gdouble)child->allocation.width / (gdouble)width;
          scale_y = (gdouble)child->allocation.height / (gdouble)height;
          gtk_plot_pc_draw_pixmap(canvas->pc, (GdkPixmap *)child->data, NULL,
                                  0, 0,  
                                  child->allocation.x, 
                                  child->allocation.y, 
                                  width, 
                                  height,
                                  scale_x, scale_y);
        } else {
          colormap = gdk_colormap_get_system();
          gdk_color_black(colormap, &black);
          gdk_color_white(colormap, &white);

          gtk_plot_pc_set_color(canvas->pc, &white);
          gtk_plot_pc_draw_rectangle(canvas->pc, TRUE,
                               child->allocation.x, child->allocation.y,
                               child->allocation.width, child->allocation.height);
          gtk_plot_pc_set_color(canvas->pc, &black);
          gtk_plot_pc_draw_rectangle(canvas->pc, FALSE,
                               child->allocation.x, child->allocation.y,
                               child->allocation.width, child->allocation.height);

        }
        break;
    case GTK_PLOT_CANVAS_CUSTOM:
    default:
        if(child->draw_child) child->draw_child(canvas, child);
        break;
  }

  gtk_plot_pc_grestore(canvas->pc);
  gtk_signal_emit (GTK_OBJECT(canvas), canvas_signals[CHANGED]);
}

/**********************************************************************/

static void
gtk_plot_canvas_draw_text(GtkPlotCanvas *canvas,
                          GtkPlotCanvasChild *child)
{
  GtkPlotText *text;
  gint x, y;
  gint width, height;
  gdouble m = canvas->magnification;
  gint tx, ty;

  if(!canvas->pixmap) return;

  text = (GtkPlotText *)child->data;

  x = text->x * canvas->pixmap_width;
  y = text->y * canvas->pixmap_height;

  gtk_plot_text_get_area(text->text, text->angle, text->justification, 
                         text->font, roundint(m * text->height), 
                         &tx, &ty, &width, &height);

  if(text->border != GTK_PLOT_BORDER_NONE){
     tx -= text->border_space;
     ty -= text->border_space;
     width += 2 * text->border_space;
     height += 2 * text->border_space;
  }

  tx += x;
  ty += y;

  gtk_plot_canvas_get_position(canvas, tx, ty,
                               &child->rx1, &child->ry1);
  gtk_plot_canvas_get_position(canvas, tx + width, ty + height,
                               &child->rx2, &child->ry2);

  gtk_plot_pc_draw_string(canvas->pc,
                         x, y,
                         text->angle,
                         &text->fg,
                         &text->bg,
                         text->transparent,
                         text->border,
                         roundint(m * text->border_space),
                         roundint(m * text->border_width),
                         roundint(m * text->shadow_width),
                         text->font,
                         roundint(m * text->height),
                         text->justification,
                         text->text);
  gtk_signal_emit (GTK_OBJECT(canvas), canvas_signals[CHANGED]);
}

static void
gtk_plot_canvas_set_line_attributes(GtkPlotCanvas *canvas, GtkPlotLine line)
{
  gdouble dot[] = {2., 3.};
  gdouble dash[] = {6., 4.};
  gdouble dot_dash[] = {6., 4., 2., 4.};
  gdouble dot_dot_dash[] = {6., 4., 2., 4., 2., 4.};
  gdouble dot_dash_dash[] = {6., 4., 6., 4., 2., 4.};

  gtk_plot_pc_set_color(canvas->pc, &line.color);

  switch(line.line_style){
   case GTK_PLOT_LINE_SOLID:
        gtk_plot_pc_set_lineattr(canvas->pc, line.line_width, 0, 0, 0);
        break;
   case GTK_PLOT_LINE_DOTTED:
        gtk_plot_pc_set_lineattr(canvas->pc, line.line_width,
                                GDK_LINE_ON_OFF_DASH, 0, 0);
        gtk_plot_pc_set_dash(canvas->pc, 0, dot, 2);
        break;
   case GTK_PLOT_LINE_DASHED:
        gtk_plot_pc_set_lineattr(canvas->pc, line.line_width,
                                GDK_LINE_ON_OFF_DASH, 0, 0);
        gtk_plot_pc_set_dash(canvas->pc, 0, dash, 2);
   case GTK_PLOT_LINE_DOT_DASH:
        gtk_plot_pc_set_lineattr(canvas->pc, line.line_width,
                                GDK_LINE_ON_OFF_DASH, 0, 0);
        gtk_plot_pc_set_dash(canvas->pc, 0, dot_dash, 4);
        break;
   case GTK_PLOT_LINE_DOT_DOT_DASH:
        gtk_plot_pc_set_lineattr(canvas->pc, line.line_width,
                                GDK_LINE_ON_OFF_DASH, 0, 0);
        gtk_plot_pc_set_dash(canvas->pc, 0, dot_dot_dash, 6);
        break;
   case GTK_PLOT_LINE_DOT_DASH_DASH:
        gtk_plot_pc_set_lineattr(canvas->pc, line.line_width,
                                GDK_LINE_ON_OFF_DASH, 0, 0);
        gtk_plot_pc_set_dash(canvas->pc, 0, dot_dash_dash, 6);
        break;
   case GTK_PLOT_LINE_NONE:
   default:
        break;
  } 
}

static gint
roundint (gdouble x)
{
 gint sign = 1;

/* if(x <= 0.) sign = -1;
*/
 return (x+sign*.50999999471);

}

