/* gtkplot - 2d scientific plots widget for gtk+
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
#include "gtkplotdata.h"
#include "gtkpsfont.h"
#include "gtkplotgdk.h"

#define DEFAULT_WIDTH 420
#define DEFAULT_HEIGHT 340
#define DEFAULT_FONT_HEIGHT 12

static gchar DEFAULT_FONT[] = "Helvetica";

/* Signals */

enum
{
  CHANGED,
  MOVED,
  RESIZED,
  LAST_SIGNAL
};

enum
{
  TICK_LABEL,
  LAST_AXIS_SIGNAL
};

static void gtk_plot_class_init 		(GtkPlotClass *klass);
static void gtk_plot_init 			(GtkPlot *plot);
static void gtk_plot_axis_class_init 		(GtkPlotAxisClass *klass);
static void gtk_plot_axis_init 			(GtkPlotAxis *axis);
static void gtk_plot_destroy	 		(GtkObject *object);
static void gtk_plot_axis_destroy	 		(GtkObject *object);
static void gtk_plot_size_request 		(GtkWidget *widget, 
                                                 GtkRequisition *requisition);
static void gtk_plot_draw 			(GtkWidget *widget, 
						 GdkRectangle *area);
static void gtk_plot_calc_ticks			(GtkPlot *plot, 
						 GtkPlotAxis *axis);
static void gtk_plot_draw_grids                 (GtkPlot *plot); 
static void gtk_plot_draw_axis			(GtkPlot *plot, 
					 	 GtkPlotAxis *axis, 
					 	 GtkPlotVector tick_direction);
static void gtk_plot_draw_labels		(GtkPlot *plot, 
						 GtkPlotAxis *axis, 
						 GtkPlotVector tick_direction); 
static void gtk_plot_draw_legends		(GtkWidget *widget);
static void gtk_plot_real_paint			(GtkWidget *widget); 
static void gtk_plot_paint_text			(GtkPlot *plot, 
                   				 gint x, gint y, 
                  				 GtkPlotText text);
static void gtk_plot_real_get_pixel		(GtkWidget *widget, 
						 gdouble xx, gdouble yy , 
						 gdouble *x, gdouble *y); 
static void gtk_plot_real_get_point		(GtkWidget *widget, 
						 gint x, gint y, 
					 	 gdouble *px, gdouble *py);
static gdouble transform_y			(GtkPlot *plot, gdouble y);
static gdouble transform_x		        (GtkPlot *plot, gdouble x);
static gdouble inverse_y			(GtkPlot *plot, gdouble y);
static gdouble inverse_x			(GtkPlot *plot, gdouble x);
static gdouble inverse_dy			(GtkPlot *plot, gdouble dy);
static gdouble inverse_dx			(GtkPlot *plot, gdouble dx);
static gint roundint				(gdouble x);
static void parse_label			        (gdouble val, 
						 gint precision, 
						 gint style,
                                                 gchar *label);

typedef gboolean (*GtkPlotSignal) (GtkObject *object,
                                   gpointer arg1,
                                   gpointer arg2,
                                   gpointer user_data);

static void
gtk_plot_marshal_BOOL__POINTER_POINTER   (GtkObject *object,
                                          GtkSignalFunc func,
                                          gpointer func_data,
                                          GtkArg * args);

static GtkWidgetClass *parent_class = NULL;
static guint plot_signals[LAST_SIGNAL] = {0};
static guint axis_signals[LAST_AXIS_SIGNAL] = {0};


GtkType
gtk_plot_get_type (void)
{
  static GtkType plot_type = 0;

  if (!plot_type)
    {
      GtkTypeInfo plot_info =
      {
	"GtkPlot",
	sizeof (GtkPlot),
	sizeof (GtkPlotClass),
	(GtkClassInitFunc) gtk_plot_class_init,
	(GtkObjectInitFunc) gtk_plot_init,
	/* reserved 1*/ NULL,
        /* reserved 2 */ NULL,
        (GtkClassInitFunc) NULL,
      };

      plot_type = gtk_type_unique (GTK_TYPE_MISC, &plot_info);
    }
  return plot_type;
}

static void
gtk_plot_class_init (GtkPlotClass *klass)
{
  GtkObjectClass *object_class;
  GtkWidgetClass *widget_class;
  GtkPlotClass *plot_class;

  parent_class = gtk_type_class (gtk_widget_get_type ());

  object_class = (GtkObjectClass *) klass;
  widget_class = (GtkWidgetClass *) klass;
  plot_class = (GtkPlotClass *) klass;

  widget_class->draw = gtk_plot_draw;
  widget_class->size_request = gtk_plot_size_request;

  plot_signals[CHANGED] = 
    gtk_signal_new("changed",
                   GTK_RUN_LAST,
                   object_class->type,
                   GTK_SIGNAL_OFFSET (GtkPlotClass, changed),
                   gtk_marshal_NONE__NONE,
                   GTK_TYPE_NONE, 0); 

  plot_signals[MOVED] = 
    gtk_signal_new("moved",
                   GTK_RUN_LAST,
                   object_class->type,
                   GTK_SIGNAL_OFFSET (GtkPlotClass, moved),
                   gtk_plot_marshal_BOOL__POINTER_POINTER,
                   GTK_TYPE_BOOL, 2, GTK_TYPE_POINTER, GTK_TYPE_POINTER); 

  plot_signals[RESIZED] = 
    gtk_signal_new("resized",
                   GTK_RUN_LAST,
                   object_class->type,
                   GTK_SIGNAL_OFFSET (GtkPlotClass, resized),
                   gtk_plot_marshal_BOOL__POINTER_POINTER,
                   GTK_TYPE_BOOL, 2, GTK_TYPE_POINTER, GTK_TYPE_POINTER); 

  gtk_object_class_add_signals (object_class, plot_signals, LAST_SIGNAL);

  object_class->destroy = gtk_plot_destroy;

  klass->changed = NULL;
  klass->moved = NULL;
  klass->resized = NULL;

  plot_class->plot_paint = gtk_plot_real_paint;
  plot_class->draw_legends = gtk_plot_draw_legends;
  plot_class->get_point = gtk_plot_real_get_point;
  plot_class->get_pixel = gtk_plot_real_get_pixel;

}

GtkType
gtk_plot_axis_get_type (void)
{
  static GtkType axis_type = 0;

  if (!axis_type)
    {
      GtkTypeInfo axis_info =
      {
	"GtkPlotAxis",
	sizeof (GtkPlotAxis),
	sizeof (GtkPlotAxisClass),
	(GtkClassInitFunc) gtk_plot_axis_class_init,
	(GtkObjectInitFunc) gtk_plot_axis_init,
	/* reserved 1*/ NULL,
        /* reserved 2 */ NULL,
        (GtkClassInitFunc) NULL,
      };

      axis_type = gtk_type_unique (GTK_TYPE_OBJECT, &axis_info);
    }
  return axis_type;
}


static void
gtk_plot_axis_class_init (GtkPlotAxisClass *klass)
{
  GtkObjectClass *object_class;
  GtkPlotAxisClass *axis_class;

  object_class = (GtkObjectClass *) klass;
  axis_class = (GtkPlotAxisClass *) klass;

  axis_signals[TICK_LABEL] = 
    gtk_signal_new("tick_label",
                   GTK_RUN_LAST,
                   object_class->type,
                   GTK_SIGNAL_OFFSET (GtkPlotAxisClass, tick_label),
                   gtk_plot_marshal_BOOL__POINTER_POINTER,
                   GTK_TYPE_BOOL, 2, GTK_TYPE_POINTER, GTK_TYPE_STRING); 

  gtk_object_class_add_signals (object_class, axis_signals, LAST_AXIS_SIGNAL);

  object_class->destroy = gtk_plot_axis_destroy;

  axis_class->tick_label = NULL;
}

static void
gtk_plot_marshal_BOOL__POINTER_POINTER         (GtkObject *object,
                                                GtkSignalFunc func,
                                                gpointer func_data,
                                                GtkArg * args)
{
  GtkPlotSignal rfunc;
  gboolean *veto;
  veto = GTK_RETLOC_BOOL (args[2]);

  rfunc = (GtkPlotSignal) func;

  *veto = (*rfunc) (object,
                    GTK_VALUE_POINTER (args[0]),
                    GTK_VALUE_POINTER (args[1]),
                    func_data);
}


static void
gtk_plot_init (GtkPlot *plot)
{
  GtkWidget *widget;
  GTK_WIDGET_SET_FLAGS(plot, GTK_NO_WINDOW);

  widget = GTK_WIDGET(plot);
  gdk_color_black(gtk_widget_get_colormap(widget), &widget->style->black);
  gdk_color_white(gtk_widget_get_colormap(widget), &widget->style->white);

  plot->flags = 0;
  plot->clip_data = FALSE;

  plot->magnification = 1.;

  plot->xmin = 0.;
  plot->xmax = 1.000000;
  plot->ymin = 0.;
  plot->ymax = 1.000000;

  plot->show_x0 = FALSE;
  plot->show_y0 = FALSE;

  plot->right = GTK_PLOT_AXIS(gtk_plot_axis_new(GTK_PLOT_AXIS_Y));  
  plot->left = GTK_PLOT_AXIS(gtk_plot_axis_new(GTK_PLOT_AXIS_Y));  
  plot->top = GTK_PLOT_AXIS(gtk_plot_axis_new(GTK_PLOT_AXIS_X));  
  plot->bottom = GTK_PLOT_AXIS(gtk_plot_axis_new(GTK_PLOT_AXIS_X));  

  plot->left->labels_attr.justification = GTK_JUSTIFY_RIGHT;
  plot->right->labels_attr.justification = GTK_JUSTIFY_LEFT;
  plot->right->title.angle = 270;

  gtk_plot_calc_ticks(plot, plot->left);
  gtk_plot_calc_ticks(plot, plot->right);
  gtk_plot_calc_ticks(plot, plot->bottom);
  gtk_plot_calc_ticks(plot, plot->top);

  plot->bottom_align = 0.;
  plot->top_align = 1.;
  plot->left_align = 0.;
  plot->right_align = 1.;

  plot->x0_line.line_style = GTK_PLOT_LINE_SOLID;
  plot->x0_line.line_width = 0;
  plot->x0_line.color = widget->style->black; 

  plot->y0_line.line_style = GTK_PLOT_LINE_SOLID;
  plot->y0_line.line_width = 0;
  plot->y0_line.color = widget->style->black; 

  plot->legends_x = .6;
  plot->legends_y = .1;
  plot->legends_width = 0;
  plot->legends_height = 0;
  plot->legends_line_width = 30;
  plot->legends_border_width = 1;
  plot->legends_shadow_width = 3;
  plot->legends_border = GTK_PLOT_BORDER_LINE;
  plot->show_legends =  TRUE;
  plot->legends_attr.text = NULL;
  plot->legends_attr.font = g_strdup(DEFAULT_FONT);
  plot->legends_attr.height = DEFAULT_FONT_HEIGHT;
  plot->legends_attr.fg = widget->style->black;
  plot->legends_attr.bg = widget->style->white;
  plot->legends_attr.transparent = FALSE;
  plot->legends_attr.border = 0;
  plot->legends_attr.border_width = 0;
  plot->legends_attr.shadow_width = 0;
  
  plot->background = widget->style->white;

  plot->xscale = GTK_PLOT_SCALE_LINEAR;
  plot->yscale = GTK_PLOT_SCALE_LINEAR;

  plot->active_data = NULL;
  plot->data_sets = NULL;
  plot->text = NULL;

  gtk_psfont_init();
  plot->drawable = NULL;
  plot->pc = GTK_PLOT_PC(gtk_plot_gdk_new(NULL));
}

static void
gtk_plot_axis_init (GtkPlotAxis *axis)
{
  GdkColor black, white;

  gdk_color_black(gdk_colormap_get_system(), &black);
  gdk_color_white(gdk_colormap_get_system(), &white);

  axis->ticks.nmajorticks = 0;
  axis->ticks.nminorticks = 0;
  axis->ticks.major = NULL;
  axis->ticks.minor = NULL;
  axis->ticks.major_values = NULL;
  axis->ticks.minor_values = NULL;
  axis->ticks.set_limits = FALSE;
  axis->ticks.begin = 0;
  axis->ticks.end = 0;
  axis->ticks.step = .100000000;
  axis->ticks.nminor = 1;

  axis->min = 0.0;
  axis->max = 1.0;
  axis->major_mask = GTK_PLOT_TICKS_IN;
  axis->minor_mask = GTK_PLOT_TICKS_IN;
  axis->ticks_length = 8;
  axis->ticks_width = 1;
  axis->labels_offset = 10;
  axis->orientation = GTK_PLOT_AXIS_X;
  axis->scale = GTK_PLOT_SCALE_LINEAR;
  axis->is_visible = TRUE;
  axis->custom_labels = FALSE;

  axis->line.line_style = GTK_PLOT_LINE_SOLID;
  axis->line.line_width = 2;
  axis->line.color = black; 
  axis->labels_attr.text = NULL;
  axis->labels_attr.font = g_strdup(DEFAULT_FONT);
  axis->labels_attr.height = DEFAULT_FONT_HEIGHT;
  axis->labels_attr.fg = black;
  axis->labels_attr.bg = white;
  axis->labels_attr.transparent = TRUE;
  axis->labels_attr.angle = 0;
  axis->labels_attr.border = 0;
  axis->labels_attr.border_width = 0;
  axis->labels_attr.shadow_width = 0;
  axis->label_mask = GTK_PLOT_LABEL_OUT;
  axis->label_style = GTK_PLOT_LABEL_FLOAT;
  axis->label_precision = 1;
  axis->labels_attr.justification = GTK_JUSTIFY_CENTER;
  axis->labels_prefix = NULL;
  axis->labels_suffix = NULL;
  axis->title.angle = 0;
  axis->title.justification = GTK_JUSTIFY_CENTER;
  axis->title.font = g_strdup(DEFAULT_FONT);
  axis->title.height = DEFAULT_FONT_HEIGHT;
  axis->title.fg = black;
  axis->title.bg = white;
  axis->title.transparent = TRUE;
  axis->title.text = g_strdup("Title");
  axis->title.border = 0;
  axis->title.border_width = 0;
  axis->title.shadow_width = 0;
  axis->title_visible = TRUE;

  axis->direction.x = 1.;
  axis->direction.y = 0.;
  axis->direction.z = 0.;

  axis->show_major_grid = FALSE;
  axis->show_minor_grid = FALSE;

  axis->major_grid.line_style = GTK_PLOT_LINE_SOLID;
  axis->major_grid.line_width = 0;
  axis->major_grid.color = black; 

  axis->minor_grid.line_style = GTK_PLOT_LINE_DOTTED;
  axis->minor_grid.line_width = 0;
  axis->minor_grid.color = black;
} 

static void
gtk_plot_destroy (GtkObject *object)
{
  GtkPlot *plot;
  GList *list;

  g_return_if_fail (object != NULL);
  g_return_if_fail (GTK_IS_PLOT (object));

  plot = GTK_PLOT (object);

  gtk_object_destroy(GTK_OBJECT(plot->top));
  gtk_object_destroy(GTK_OBJECT(plot->bottom));
  gtk_object_destroy(GTK_OBJECT(plot->left));
  gtk_object_destroy(GTK_OBJECT(plot->right));

  if(plot->legends_attr.font) g_free (plot->legends_attr.font);

  plot->legends_attr.font = NULL;

  list = plot->text;
  while(list){
    GtkPlotText *text;

    text = (GtkPlotText *) list->data;
    if(text->text) g_free(text->text);
    if(text->font) g_free(text->font);

    g_free(text);
    plot->text = g_list_remove_link(plot->text, list);
    g_list_free_1(list);
    list = plot->text;
  }


  list = plot->data_sets;
  while(list){
    gtk_widget_destroy(GTK_WIDGET(list->data));

    plot->data_sets = g_list_remove_link(plot->data_sets, list);
    g_list_free_1(list);
    list = plot->data_sets;
  }

  if ( GTK_OBJECT_CLASS (parent_class)->destroy )
    (* GTK_OBJECT_CLASS (parent_class)->destroy) (object);

  gtk_object_destroy(GTK_OBJECT(plot->pc));

  gtk_psfont_unref();
}

static void
gtk_plot_axis_destroy (GtkObject *object)
{
  GtkPlotAxis *axis;

  g_return_if_fail (object != NULL);
  g_return_if_fail (GTK_IS_PLOT_AXIS (object));

  axis = GTK_PLOT_AXIS (object);

  if(axis->labels_attr.font) g_free (axis->labels_attr.font);
  if(axis->title.font) g_free (axis->title.font);
  if(axis->title.text) g_free (axis->title.text);

  axis->labels_attr.font = NULL;
  axis->title.font = NULL;
  axis->title.text = NULL;

  if(axis->labels_prefix) g_free(axis->labels_prefix);
  if(axis->labels_suffix) g_free(axis->labels_suffix);

  if(axis->ticks.major){
    g_free (axis->ticks.major);
    g_free (axis->ticks.major_values);
  }
}

static void
gtk_plot_draw (GtkWidget *widget, GdkRectangle *area)
{
  gtk_plot_paint(GTK_PLOT(widget));
  gtk_plot_refresh (GTK_PLOT(widget), area);
}

void
gtk_plot_paint (GtkPlot *plot)
{
  if(!GTK_WIDGET_REALIZED(GTK_WIDGET(plot))) return;

  GTK_PLOT_CLASS(GTK_OBJECT(plot)->klass)->plot_paint(GTK_WIDGET(plot)); 
}

static void
gtk_plot_real_paint (GtkWidget *widget)
{
  GtkPlot *plot;
  GtkPlotText *child_text;
  GtkStyle *style;
  GdkPixmap *pixmap;
  GList *dataset;
  GList *text;
  GdkRectangle area;
  gint width, height;
  gint xoffset, yoffset ;

  plot = GTK_PLOT(widget);

  area.x = widget->allocation.x;
  area.y = widget->allocation.y;
  area.width = widget->allocation.width;
  area.height = widget->allocation.height;

  xoffset = area.x + plot->x * widget->allocation.width;
  yoffset = area.y + plot->y * widget->allocation.height;
  width = plot->width * widget->allocation.width;
  height = plot->height * widget->allocation.height;

  style = gtk_widget_get_style(widget);

  pixmap = plot->drawable;

  gtk_plot_pc_gsave(plot->pc);
  gtk_plot_pc_set_color(plot->pc, &plot->background);

  if(!GTK_PLOT_TRANSPARENT(plot))
    gtk_plot_pc_draw_rectangle (plot->pc, TRUE,
  		               xoffset, yoffset,
		               width , height);

  /* draw frame to guide the eyes*/
/*
    gtk_plot_pc_draw_rectangle (plot->pc, FALSE,
  		                xoffset, yoffset,
		                width , height);
*/

  /* draw the ticks & grid lines */

  gtk_plot_calc_ticks(plot, plot->left);
  gtk_plot_calc_ticks(plot, plot->right);
  gtk_plot_calc_ticks(plot, plot->bottom);
  gtk_plot_calc_ticks(plot, plot->top);

  gtk_plot_draw_grids(plot);

  if(plot->bottom->is_visible)
    {
      GtkPlotVector tick;

      tick.x = 0.;
      tick.y = -1.;
      plot->bottom->origin.x = plot->xmin;
      plot->bottom->origin.y = plot->ymin*(1.0-plot->bottom_align) + 
                               plot->ymax*plot->bottom_align;
      gtk_plot_draw_axis(plot, plot->bottom, tick); 
      gtk_plot_draw_labels(plot, plot->bottom, tick); 
    }

  if(plot->top->is_visible)
    {
      GtkPlotVector tick;

      tick.x = 0.;
      tick.y = 1.;
      plot->top->origin.x = plot->xmin;
      plot->top->origin.y = plot->ymin*(1.0-plot->top_align) + 
                            plot->ymax*plot->top_align;
      gtk_plot_draw_axis(plot, plot->top, tick);
      gtk_plot_draw_labels(plot, plot->top, tick);
    }

  if(plot->left->is_visible)
    {
      GtkPlotVector tick;

      tick.x = 1.;
      tick.y = 0.;
      plot->left->origin.x = plot->xmin*(1.0-plot->left_align) +
                             plot->xmax*plot->left_align;
      plot->left->origin.y = plot->ymin;
      gtk_plot_draw_axis(plot, plot->left, tick);
      gtk_plot_draw_labels(plot, plot->left, tick);
    }

  if(plot->right->is_visible)
    {
      GtkPlotVector tick;

      tick.x = -1.;
      tick.y = 0.;
      plot->right->origin.x = plot->xmin*(1.0-plot->right_align) +
                              plot->xmax*plot->right_align;
      plot->right->origin.y = plot->ymin;
      gtk_plot_draw_axis(plot, plot->right, tick);
      gtk_plot_draw_labels(plot, plot->right, tick);
    }

  dataset = plot->data_sets;
  while(dataset)
   {
     if(GTK_IS_PLOT_DATA(dataset->data))
       gtk_plot_data_paint(GTK_PLOT_DATA(dataset->data));
     dataset = dataset->next;
   }

  text = plot->text;
  while(text)
   {
     child_text = (GtkPlotText *) text->data;  
     gtk_plot_draw_text(plot, *child_text);
     text = text->next;
   }

  GTK_PLOT_CLASS(GTK_OBJECT(plot)->klass)->draw_legends(GTK_WIDGET(plot));

  gtk_plot_pc_grestore(plot->pc);
}

void 
gtk_plot_refresh (GtkPlot *plot, GdkRectangle *drawing_area)
{
  GtkWidget *widget;
  GdkPixmap *pixmap;
  GdkRectangle area;

  widget = GTK_WIDGET(plot);
  if(!GTK_WIDGET_DRAWABLE(widget)) return;

  if(!plot->drawable) return;
  pixmap = plot->drawable;

  if(drawing_area == NULL){
     area.x = widget->allocation.x;
     area.y = widget->allocation.y;
     area.width = widget->allocation.width;
     area.height = widget->allocation.height;
  } else {
     area = *drawing_area;
  }

  gdk_draw_pixmap(widget->window,
                  widget->style->fg_gc[GTK_STATE_NORMAL],
                  pixmap,
                  area.x, 
                  area.y, 
                  widget->allocation.x, 
                  widget->allocation.y, 
                  widget->allocation.width, 
                  widget->allocation.height);  
  
}

static void
gtk_plot_size_request (GtkWidget *widget, GtkRequisition *requisition)
{
  GtkPlot *plot;

  plot = GTK_PLOT(widget);

  requisition->width =  DEFAULT_WIDTH;
  requisition->height =  DEFAULT_HEIGHT;
}

GtkObject*
gtk_plot_axis_new (GtkPlotOrientation orientation)
{
  GtkObject *axis;

  axis = gtk_type_new (gtk_plot_axis_get_type ());

  gtk_plot_axis_construct(GTK_PLOT_AXIS(axis), orientation);

  return GTK_OBJECT (axis);
}

void
gtk_plot_axis_construct(GtkPlotAxis *axis, GtkPlotOrientation orientation)
{
  axis->orientation = orientation;

  switch(orientation){
   case GTK_PLOT_AXIS_X:
     axis->direction.x = 1.0;
     axis->direction.y = 0.0;
     axis->direction.z = 0.0;
     g_free(axis->title.text);
     axis->title.text = g_strdup("X Title");
     axis->title.angle = 0;
     break;
   case GTK_PLOT_AXIS_Y:
     axis->direction.x = 0.0;
     axis->direction.y = -1.0;
     axis->direction.z = 0.0;
     g_free(axis->title.text);
     axis->title.text = g_strdup("Y Title");
     axis->title.angle = 90;
     break;
   case GTK_PLOT_AXIS_Z:
     axis->direction.x = 0.0;
     axis->direction.y = 0.0;
     axis->direction.z = 1.0;
     g_free(axis->title.text);
     axis->title.text = g_strdup("Z Title");
     axis->title.angle = 0;
     break;
  }
}

GtkWidget*
gtk_plot_new (GdkDrawable *drawable)
{
  GtkPlot *plot;

  plot = gtk_type_new (gtk_plot_get_type ());

  gtk_plot_construct(GTK_PLOT(plot), drawable);

  return GTK_WIDGET (plot);
}

void
gtk_plot_construct(GtkPlot *plot, GdkDrawable *drawable)
{
  plot->drawable = drawable;

  plot->x = .15;
  plot->y = .1;
  plot->width = .6;
  plot->height = .6;

  plot->left->title.x = plot->x;  
  plot->left->title.y = plot->y + plot->height / 2.;
  plot->right->title.x = plot->x + plot->width;  
  plot->right->title.y = plot->y + plot->height / 2.;
  plot->top->title.x = plot->x + plot->width / 2.;  
  plot->top->title.y = plot->y;
  plot->bottom->title.x = plot->x + plot->width / 2.;  
  plot->bottom->title.y = plot->y + plot->height;

  plot->left->title.x -= 45. / (gdouble)DEFAULT_WIDTH;  
  plot->right->title.x += 45. / (gdouble)DEFAULT_WIDTH;  
  plot->top->title.y -= 35. / (gdouble)DEFAULT_HEIGHT;
  plot->bottom->title.y += 35. / (gdouble)DEFAULT_HEIGHT;
}

GtkWidget*
gtk_plot_new_with_size (GdkDrawable *drawable, gdouble width, gdouble height)
{
  GtkWidget *plot; 

  plot = gtk_type_new (gtk_plot_get_type ());

  gtk_plot_construct_with_size(GTK_PLOT(plot), drawable, width, height);

  return(plot);
}

void
gtk_plot_construct_with_size (GtkPlot *plot,
			      GdkDrawable *drawable, 
                              gdouble width, gdouble height)
{
  gtk_plot_construct(plot, drawable);

  gtk_plot_resize (GTK_PLOT(plot), width, height);
}


void
gtk_plot_set_drawable (GtkPlot *plot, GdkDrawable *drawable)
{
  plot->drawable = drawable;
 
  if(GTK_IS_PLOT_GDK(plot->pc)) GTK_PLOT_GDK(plot->pc)->drawable = drawable;
}

GdkDrawable *
gtk_plot_get_drawable (GtkPlot *plot)
{
  return(plot->drawable);
}

static void
gtk_plot_draw_grids(GtkPlot *plot)
{
  GtkWidget *widget;
  GdkRectangle clip_area;
  gdouble width, height;
  gdouble xp, yp;
  gdouble x1, y1, x2, y2;
  gdouble xx, yy;
  gdouble x_tick;
  gint ntick;

  widget = GTK_WIDGET(plot);

  xp = widget->allocation.x + plot->x * widget->allocation.width;
  yp = widget->allocation.y + plot->y * widget->allocation.height;
  width = plot->width * widget->allocation.width;
  height = plot->height * widget->allocation.height;
 
  clip_area.x = xp;
  clip_area.y = yp;
  clip_area.width = width;
  clip_area.height = height;

  gtk_plot_pc_clip(plot->pc, &clip_area);
 
  if(plot->show_x0)
    {
          if(plot->xmin <= 0. && plot->xmax >= 0.)
            {
              gtk_plot_get_pixel(plot, 0., plot->ymin, &x1, &y1);
              gtk_plot_get_pixel(plot, 0., plot->ymax, &x2, &y2);
              gtk_plot_draw_line(plot, plot->x0_line,
                                 x1, y1, x2, y2); 
            }
    }

  if(plot->show_y0)
    {
          if(plot->ymin <= 0. && plot->ymax >= 0.)
            {
              gtk_plot_get_pixel(plot, plot->xmin, 0., &x1, &y1);
              gtk_plot_get_pixel(plot, plot->xmax, 0., &x2, &y2);
              gtk_plot_draw_line(plot, plot->y0_line,
                                 x1, y1, x2, y2); 
            }
    }

  if(plot->bottom->show_minor_grid)
    {
          for(ntick = 0; ntick < plot->bottom->ticks.nminorticks; ntick++){
            if(plot->bottom->ticks.minor_values[ntick] >= plot->bottom->min){
              x_tick = plot->bottom->ticks.minor_values[ntick];
              xx = x_tick; 
              yy = plot->ymin;
              gtk_plot_get_pixel(plot, xx, yy, &x1, &y1);
              xx = x_tick; 
              yy = plot->ymax;
              gtk_plot_get_pixel(plot, xx, yy, &x2, &y2);
              gtk_plot_draw_line(plot, plot->bottom->minor_grid,
                                 x1, y1, x2, y2);
            }
          }
    }

  if(plot->bottom->show_major_grid)
    {
          for(ntick = 0; ntick < plot->bottom->ticks.nmajorticks; ntick++){
            if(plot->bottom->ticks.major_values[ntick] >= plot->bottom->min){
              x_tick = plot->bottom->ticks.major_values[ntick];
              xx = x_tick; 
              yy = plot->ymin;
              gtk_plot_get_pixel(plot, xx, yy, &x1, &y1);
              xx = x_tick; 
              yy = plot->ymax;
              gtk_plot_get_pixel(plot, xx, yy, &x2, &y2);
              gtk_plot_draw_line(plot, plot->bottom->major_grid,
                                 x1, y1, x2, y2); 
           }
          }
    }

  if(plot->left->show_minor_grid)
    {
          for(ntick = 0; ntick < plot->left->ticks.nminorticks; ntick++){
            if(plot->left->ticks.minor_values[ntick] >= plot->left->min){
              x_tick = plot->left->ticks.minor_values[ntick];
              xx = plot->xmin; 
              yy = x_tick;
              gtk_plot_get_pixel(plot, xx, yy, &x1, &y1);
              xx = plot->xmax; 
              yy = x_tick;
              gtk_plot_get_pixel(plot, xx, yy, &x2, &y2);
              gtk_plot_draw_line(plot, plot->left->minor_grid,
                                 x1, y1, x2, y2); 
            }
          }
    }

  if(plot->left->show_major_grid)
    {
          for(ntick = 0; ntick < plot->left->ticks.nmajorticks; ntick++){
            if(plot->left->ticks.major_values[ntick] >= plot->left->min){
              x_tick = plot->left->ticks.major_values[ntick];
              xx = plot->xmin; 
              yy = x_tick;
              gtk_plot_get_pixel(plot, xx, yy, &x1, &y1);
              xx = plot->xmax; 
              yy = x_tick;
              gtk_plot_get_pixel(plot, xx, yy, &x2, &y2);
              gtk_plot_draw_line(plot, plot->left->major_grid,
                                 x1, y1, x2, y2); 
           }
          }
    }

  gtk_plot_pc_clip(plot->pc, NULL);
}

static void
gtk_plot_draw_axis(GtkPlot *plot, GtkPlotAxis *axis, GtkPlotVector tick_direction)
{
  GtkWidget *widget;
  gdouble x_tick;
  gint line_width;
  gdouble xp, yp, width, height;
  gint ntick;
  gdouble m = plot->magnification;
  gdouble x1, y1, x2, y2;
  gdouble px, py;
  gdouble xx, yy;

  widget = GTK_WIDGET(plot); 
  xp = widget->allocation.x + plot->x * widget->allocation.width;
  yp = widget->allocation.y + plot->y * widget->allocation.height;
  width = plot->width * widget->allocation.width;
  height = plot->height * widget->allocation.height;

  line_width = axis->line.line_width;
  gtk_plot_pc_set_color(plot->pc, &axis->line.color);

  gtk_plot_pc_set_lineattr(plot->pc, axis->line.line_width, 0, 3, 0);

  gtk_plot_get_pixel(plot, axis->origin.x, axis->origin.y, &x1, &y1);
  gtk_plot_get_pixel(plot, 
		     axis->origin.x+axis->direction.x*(plot->xmax - plot->xmin),
		     axis->origin.y-axis->direction.y*(plot->ymax - plot->ymin),
                     &x2, &y2);

  gtk_plot_pc_draw_line(plot->pc, x1, y1, x2, y2); 

  gtk_plot_pc_set_lineattr(plot->pc, axis->ticks_width, 0, 1, 0);

  for(ntick = 0; ntick < axis->ticks.nmajorticks; ntick++){
    x_tick = axis->ticks.major_values[ntick];
    xx = -axis->direction.y * axis->origin.x + x_tick * axis->direction.x; 
    yy = axis->direction.x * axis->origin.y - x_tick * axis->direction.y;
    gtk_plot_get_pixel(plot, xx, yy, &px, &py);
    if(axis->ticks.major_values[ntick] >= axis->min){
      if(axis->major_mask & GTK_PLOT_TICKS_IN)
         gtk_plot_pc_draw_line(plot->pc,
                       px, 
                       py,
                       px + tick_direction.x * m * axis->ticks_length, 
                       py + tick_direction.y * m * axis->ticks_length);
      if(axis->major_mask & GTK_PLOT_TICKS_OUT)
         gtk_plot_pc_draw_line(plot->pc,
                       px, 
                       py,
                       px - tick_direction.x * m * axis->ticks_length, 
                       py - tick_direction.y * m * axis->ticks_length);
    }
  }     
  for(ntick = 0; ntick < axis->ticks.nminorticks; ntick++){
    x_tick = axis->ticks.minor_values[ntick];
    xx = -axis->direction.y * axis->origin.x + x_tick * axis->direction.x; 
    yy = axis->direction.x * axis->origin.y - x_tick * axis->direction.y;
    gtk_plot_get_pixel(plot, xx, yy, &px, &py);
    if(axis->ticks.minor_values[ntick] >= axis->min){
      if(axis->minor_mask & GTK_PLOT_TICKS_IN)
         gtk_plot_pc_draw_line(plot->pc,
                       px, 
                       py,
                       px + tick_direction.x * m * axis->ticks_length/2., 
                       py + tick_direction.y * m * axis->ticks_length/2.);
      if(axis->minor_mask & GTK_PLOT_TICKS_OUT)
         gtk_plot_pc_draw_line(plot->pc,
                       px, 
                       py,
                       px - tick_direction.x * m * axis->ticks_length/2., 
                       py - tick_direction.y * m * axis->ticks_length/2.);
    }
  }     

}


static void
gtk_plot_draw_labels(GtkPlot *plot, 
                     GtkPlotAxis *axis, 
                     GtkPlotVector tick_direction)
{
  GtkWidget *widget;
  GdkFont *font;
  GtkPlotText title, tick;
  gchar label[100], new_label[100];
  gdouble x_tick;
  gdouble xx, yy;
  gint text_height;
  gdouble xp, yp, width, height;
  gint ntick;
  gdouble m = plot->magnification;
  gboolean veto = FALSE;
  gdouble px, py;
  gdouble y;

  widget = GTK_WIDGET(plot); 
  xp = plot->x * widget->allocation.width;
  yp = plot->y * widget->allocation.height;
  width = plot->width * widget->allocation.width;
  height = plot->height * widget->allocation.height;

  gtk_plot_pc_set_color (plot->pc, &axis->labels_attr.fg);

  font = gtk_psfont_get_gdkfont(axis->labels_attr.font, 
                                roundint(axis->labels_attr.height * m));
  text_height = font->ascent + font->descent;

  y = 0.0;
  switch(axis->labels_attr.angle){
    case 0:
           y += text_height / 2.;
           break;
    case 90:
           break;
    case 180:
           y -= text_height / 2.;
           break;
    case 270:
           break;
  }


  tick = axis->labels_attr;
  for(ntick = 0; ntick < axis->ticks.nmajorticks; ntick++){
    x_tick = axis->ticks.major_values[ntick];
    xx = -axis->direction.y * axis->origin.x + x_tick * axis->direction.x; 
    yy = axis->direction.x * axis->origin.y - x_tick * axis->direction.y;
    gtk_plot_get_pixel(plot, xx, yy, &px, &py);
    if(x_tick >= axis->min-1.e-9){
      if(!axis->custom_labels){
        parse_label(x_tick, axis->label_precision, axis->label_style, label);
      }
      else
      {
        gtk_signal_emit(GTK_OBJECT(axis), axis_signals[TICK_LABEL],
                        &x_tick, label, &veto);
        if(!veto)   
          parse_label(x_tick, axis->label_precision, axis->label_style, label);
      }

      if(axis->labels_prefix){
        g_snprintf(new_label, 100, "%s%s", axis->labels_prefix, label);
        g_snprintf(label, 100, "%s", new_label);
      }
      if(axis->labels_suffix){
        g_snprintf(new_label, 100, "%s%s", label, axis->labels_suffix);
        g_snprintf(label, 100, "%s", new_label);
      }

      tick.text = label;
      tick.x = px;
      tick.y = py + y;

      if(axis->label_mask & GTK_PLOT_LABEL_IN){
         tick.x = tick.x + tick_direction.x*roundint(axis->labels_offset * m);
         tick.y = tick.y + tick_direction.y*roundint(axis->labels_offset * m);
         tick.x = (gdouble)tick.x / (gdouble)widget->allocation.width;
         tick.y = (gdouble)tick.y / (gdouble)widget->allocation.height;
         gtk_plot_draw_text(plot, tick);
      }
      if(axis->label_mask & GTK_PLOT_LABEL_OUT){
         tick.x = tick.x - tick_direction.x*roundint(axis->labels_offset * m);
         tick.y = tick.y - tick_direction.y*roundint(axis->labels_offset * m);
         tick.x = (gdouble)tick.x / (gdouble)widget->allocation.width;
         tick.y = (gdouble)tick.y / (gdouble)widget->allocation.height;
         gtk_plot_draw_text(plot, tick);
      }
    }
  }
  if(axis->title_visible && axis->title.text)
       {
         title = axis->title;
         gtk_plot_draw_text(plot, title); 
       }

  gdk_font_unref(font);
}

void
gtk_plot_draw_line(GtkPlot *plot, 
                   GtkPlotLine line,
                   gdouble x1, gdouble y1, gdouble x2, gdouble y2)
{

  if(line.line_style == GTK_PLOT_LINE_NONE) return;


  gtk_plot_set_line_attributes(plot, line);

  gtk_plot_pc_draw_line(plot->pc, x1, y1, x2, y2);

}

void
gtk_plot_set_line_attributes(GtkPlot *plot, 
                             GtkPlotLine line)
{
  gdouble dot[] = {2., 3.};
  gdouble dash[] = {6., 4.};
  gdouble dot_dash[] = {6., 4., 2., 4.};
  gdouble dot_dot_dash[] = {6., 4., 2., 4., 2., 4.};
  gdouble dot_dash_dash[] = {6., 4., 6., 4., 2., 4.};

  gtk_plot_pc_set_color (plot->pc, &line.color);

  switch(line.line_style){
   case GTK_PLOT_LINE_SOLID:
        gtk_plot_pc_set_lineattr(plot->pc, line.line_width, 0, 0, 0);
        break;
   case GTK_PLOT_LINE_DOTTED:
        gtk_plot_pc_set_lineattr(plot->pc, line.line_width, 
                                GDK_LINE_ON_OFF_DASH, 0, 0);
        gtk_plot_pc_set_dash(plot->pc, 0, dot, 2);
        break;
   case GTK_PLOT_LINE_DASHED:
        gtk_plot_pc_set_lineattr(plot->pc, line.line_width, 
                                GDK_LINE_ON_OFF_DASH, 0, 0);
        gtk_plot_pc_set_dash(plot->pc, 0, dash, 2);
        break;
   case GTK_PLOT_LINE_DOT_DASH:
        gtk_plot_pc_set_lineattr(plot->pc, line.line_width, 
                                GDK_LINE_ON_OFF_DASH, 0, 0);
        gtk_plot_pc_set_dash(plot->pc, 0, dot_dash, 4);
        break;
   case GTK_PLOT_LINE_DOT_DOT_DASH:
        gtk_plot_pc_set_lineattr(plot->pc, line.line_width, 
                                GDK_LINE_ON_OFF_DASH, 0, 0);
        gtk_plot_pc_set_dash(plot->pc, 0, dot_dot_dash, 6);
        break;
   case GTK_PLOT_LINE_DOT_DASH_DASH:
        gtk_plot_pc_set_lineattr(plot->pc, line.line_width, 
                                GDK_LINE_ON_OFF_DASH, 0, 0);
        gtk_plot_pc_set_dash(plot->pc, 0, dot_dash_dash, 6);
        break;
   case GTK_PLOT_LINE_NONE:
   default:
        break;
  }  
}

static void
gtk_plot_draw_legends (GtkWidget *widget)
{
  GtkPlot *plot;
  GList *datasets; 
  GtkPlotData *dataset;
  GtkAllocation legend_area;
  gint x, y;
  gint height;
  gint lwidth, lheight;
  gdouble m;

  plot = GTK_PLOT(widget);

  if(!plot->show_legends) return;

  m = plot->magnification;
  gtk_plot_pc_gsave(plot->pc);

/* first draw the white rectangle for the background */
  legend_area = gtk_plot_legends_get_allocation(plot);

  if(!plot->legends_attr.transparent){
     gtk_plot_pc_set_color(plot->pc, &plot->legends_attr.bg);
     gtk_plot_pc_draw_rectangle(plot->pc,
                               TRUE,
                               legend_area.x, legend_area.y,
                               legend_area.width, legend_area.height);
  }

  plot->legends_width = legend_area.width;
  plot->legends_height = legend_area.height;

/* now draw the legends */

  height = roundint(4 * m);

  datasets = plot->data_sets;
  while(datasets)
   {
     dataset = GTK_PLOT_DATA(datasets->data);

     if(GTK_WIDGET_VISIBLE(GTK_WIDGET(dataset)) && dataset->show_legend)
       {
         GTK_PLOT_DATA_CLASS(GTK_OBJECT(dataset)->klass)->get_legend_size(dataset, &lwidth, &lheight);
         x = legend_area.x + roundint(4 * m);
         y = legend_area.y + height;
         GTK_PLOT_DATA_CLASS(GTK_OBJECT(dataset)->klass)->draw_legend(dataset, x, y); 
         height += lheight;
       }
     datasets=datasets->next;
   }
 
   gtk_plot_pc_set_lineattr(plot->pc, plot->legends_border_width, 0, 0, 0);
   gtk_plot_pc_set_color(plot->pc, &plot->legends_attr.fg);

   if(plot->legends_border != GTK_PLOT_BORDER_NONE)
      {
        gtk_plot_pc_draw_rectangle(plot->pc,
                                  FALSE,
                                  legend_area.x, legend_area.y,
                                  legend_area.width, legend_area.height);
      }

   gtk_plot_pc_set_lineattr(plot->pc, 0, 0, 0, 0);
   if(plot->legends_border == GTK_PLOT_BORDER_SHADOW)
      {
        gtk_plot_pc_draw_rectangle(plot->pc,
                                  TRUE,
                                  legend_area.x + roundint(plot->legends_shadow_width * m), 
                                  legend_area.y + legend_area.height,
                                  legend_area.width, 
                                  roundint(plot->legends_shadow_width * m));
        gtk_plot_pc_draw_rectangle(plot->pc,
                                  TRUE,
                                  legend_area.x + legend_area.width, 
                                  legend_area.y + roundint(plot->legends_shadow_width * m),
                                  roundint(plot->legends_shadow_width * m), 
                                  legend_area.height);
      }

  gtk_plot_pc_grestore(plot->pc);
}


static gdouble
transform_y(GtkPlot *plot, gdouble y)
{
  gdouble height, position;

  switch( plot->yscale ) {
    case GTK_PLOT_SCALE_LOG10:
      if( y <= 0.0 || plot->ymin <= 0.0 || plot->ymax <= 0.0 )
        return 0;

      position = log(y/plot->ymin) / log(plot->ymax / plot->ymin);
      break;

    case GTK_PLOT_SCALE_LINEAR:
    default:
      position = (y - plot->ymin) / (plot->ymax - plot->ymin);
      break;
  }

  height = GTK_WIDGET(plot)->allocation.height * plot->height;
  return(height*position);
}

static gdouble
transform_x(GtkPlot *plot, gdouble x)
{
  gdouble width, position;

  switch( plot->xscale ){
    case GTK_PLOT_SCALE_LOG10:
      if( x <= 0.0 || plot->xmin <= 0.0 || plot->xmax <= 0.0 )
        return 0;
      position = log(x/plot->xmin) / log(plot->xmax / plot->xmin);
      break;

    case GTK_PLOT_SCALE_LINEAR:
    default:
      position = (x - plot->xmin) / (plot->xmax - plot->xmin);
      break;
  }

  width = GTK_WIDGET(plot)->allocation.width * plot->width;
  return(width*position);
}


static gdouble
inverse_y(GtkPlot *plot, gdouble y)
{
    return(inverse_dy(plot, y)+plot->ymin);
}

static gdouble
inverse_x(GtkPlot *plot, gdouble x)
{
    return(inverse_dx(plot, x)+plot->xmin);
}

static gdouble
inverse_dy(GtkPlot *plot, gdouble dy)
{
    gdouble height;
 
    height = GTK_WIDGET(plot)->allocation.height * plot->height;
    return(dy*(plot->ymax-plot->ymin)/height);
}

static gdouble
inverse_dx(GtkPlot *plot, gdouble dx)
{
    gdouble width;
 
    width = (gdouble)GTK_WIDGET(plot)->allocation.width * plot->width;
    return(dx*(plot->xmax-plot->xmin)/width);
}

static gint
roundint (gdouble x)
{
 gint sign = 1;

/* if(x <= 0.) sign = -1; 
*/
 return (x+sign*.50999999471);
}

static void
parse_label(gdouble val, gint precision, gint style, gchar *label)
{
  gdouble auxval;
  gint intspace = 0;
  gint power;
  gfloat v;

  auxval = fabs(val);

  power = 0.0;
  if(auxval != 0.0)
       power = (gint)log10(auxval);

  v = val / pow(10.0, power); 
  if(fabs(v) < 1.0 && v != 0.0){
     v *= 10.0;
     power -= 1;
  }
  if(fabs(v) >= 10.0){
     v /= 10.0;
     power += 1;
  }
  if(power < -12){
     power = 0;
     v =0.0;
  }

  if(auxval > 1)
    intspace = (gint)log10(auxval);


  switch(style){
    case GTK_PLOT_LABEL_EXP:    
      sprintf (label, "%*.*E", 1, precision, val);
      break;
    case GTK_PLOT_LABEL_POW:    
      sprintf (label, "%*.*f\\4x\\N10\\S%i", intspace, precision, v, power);
      break;
    case GTK_PLOT_LABEL_FLOAT:
    default:
      sprintf (label, "%*.*f", intspace, precision, val);
  }

}

void
gtk_plot_draw_text(GtkPlot *plot, 
                   GtkPlotText text) 
{
  gint x, y;

  if(!text.text) return;
  if(strlen(text.text) == 0) return;
  if(plot->drawable == NULL) return;

  x = text.x * GTK_WIDGET(plot)->allocation.width;
  y = text.y * GTK_WIDGET(plot)->allocation.height;

  gtk_plot_paint_text(plot, x, y, text);
}

static void
gtk_plot_paint_text(GtkPlot *plot, 
                    gint x, gint y,
                    GtkPlotText text)
{

  if(!text.text) return;
  if(strlen(text.text) == 0) return;
  if(plot->drawable == NULL) return;

  if(GTK_IS_PLOT_GDK(plot->pc)) 
        GTK_PLOT_GDK(plot->pc)->window = GTK_WIDGET(plot)->window;

  gtk_plot_pc_draw_string(plot->pc,
			 x, y,
			 text.angle,
			 &text.fg,
			 &text.bg,
			 text.transparent,
			 text.border,
			 text.border_width,
			 text.shadow_width,
			 text.font,
			 roundint(text.height * plot->magnification),
			 text.justification,
			 text.text);

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

void
gtk_plot_text_get_size(const gchar *text, gint angle, 
                       const gchar* text_font, 
                       gint text_height,   
                       gint *width, gint *height,
                       gint *ascent, gint *descent)
{
  GdkFont *font;
  GtkPSFont *psfont, *tmp_font;
  gint old_width, old_height;
  gboolean italic, bold;
  gint fontsize;
  gint x, y, y0;
  const gchar *aux = 0;
  gchar subs[2];
  GList *family;
  gint numf;
  const gchar *lastchar = NULL;

  gtk_psfont_get_families(&family, &numf);
  font = gtk_psfont_get_gdkfont(text_font, text_height);
  old_width = gdk_string_width (font, text);
  old_height = font->ascent + font->descent;

  font = gtk_psfont_get_gdkfont(text_font, text_height);
  psfont = gtk_psfont_get_font(text_font);
  tmp_font = psfont;
  italic = psfont->italic;
  bold = psfont->bold;
  fontsize = text_height;
  x = 0;
  y0 = y = font->ascent;
  old_width = 0;
  aux = text;
  *ascent = font->ascent;
  *descent = font->descent;

  while(aux && *aux != '\0' && *aux != '\n'){
   if(*aux == '\\'){
     aux++;
     switch(*aux){
       case '0': case '1': case '2': case '3':
       case '4': case '5': case '6': case '7': case '9':
           tmp_font = gtk_psfont_find_by_family((gchar *)g_list_nth_data(family, *aux-'0'), italic, bold);
           font = gtk_psfont_get_gdkfont(tmp_font->psname, fontsize);
           aux++;
           break;
       case '8': case 'g':
           tmp_font = gtk_psfont_find_by_family("Symbol", italic, bold);
           font = gtk_psfont_get_gdkfont(tmp_font->psname, fontsize);
           aux++;
           break;
       case 'B':
           bold = TRUE;
           tmp_font = gtk_psfont_find_by_family(tmp_font->family, italic, bold);
           font = gtk_psfont_get_gdkfont(tmp_font->psname, fontsize);
           aux++;
           break;
       case 'i':
           italic = TRUE;
           tmp_font = gtk_psfont_find_by_family(tmp_font->family, italic, bold);
           font = gtk_psfont_get_gdkfont(tmp_font->psname, fontsize);
           aux++;
           break;
       case 'S': case '^':
           fontsize = (int)((gdouble)fontsize * 0.6 + 0.5);
           font = gtk_psfont_get_gdkfont(tmp_font->psname, fontsize);
           y -= font->ascent;
           aux++;
           break;
       case 's': case '_':
           fontsize = (int)((gdouble)fontsize * 0.6 + 0.5);
           font = gtk_psfont_get_gdkfont(tmp_font->psname, fontsize);
           y += font->descent;
           aux++;
           break;
       case '+':
           fontsize += 3;
           font = gtk_psfont_get_gdkfont(tmp_font->psname, fontsize);
           aux++;
           break;
       case '-':
           fontsize -= 3;
           font = gtk_psfont_get_gdkfont(tmp_font->psname, fontsize);
           aux++;
           break;
       case 'N':
/*
           tmp_font = psfont;
*/
           font = gtk_psfont_get_gdkfont(tmp_font->psname, text_height);
           y = y0;
           italic = psfont->italic;
           bold = psfont->bold;
           fontsize = text_height;
           aux++;
           break;
       case 'b':
           if(lastchar){
              x -= gdk_char_width_wc (font, *lastchar);
              if(lastchar == text)
                 lastchar = NULL;
              else
                 lastchar--;
           } else {
              x -= gdk_char_width_wc (font, 'X');
           }
           aux++;
           break;
       default:
           if(aux && *aux != '\0' && *aux !='\n'){
             subs[0] = *aux;
             subs[1] = '\0';
             x += gdk_char_width_wc (font, *aux);
             lastchar = aux;
             aux++;
           }
           break;
     }
   } else {
     if(aux && *aux != '\0' && *aux !='\n'){
       subs[0] = *aux;
       subs[1] = '\0';
       x += gdk_char_width_wc (font, *aux);
       lastchar = aux;
       aux++;
       if(x > old_width) old_width = x;
       if(y + font->descent - y0 > *descent) *descent = y + font->descent - y0;
       if(y0 - y + font->ascent > *ascent) *ascent = y0 - y + font->ascent;
     }
   }
  }

  old_height = *ascent + *descent;
  *width = old_width;
  *height = old_height;
  if(angle == 90 || angle == 270)
    {
      *width = old_height;
      *height = old_width;
    }

  gdk_font_unref(font);
}

void
gtk_plot_text_get_area(const gchar *text, gint angle, GtkJustification just, 
                       const gchar *font, gint font_height,
                       gint *x, gint *y,
                       gint *width, gint *height)
{
  gint ascent, descent;

  if(text == NULL) return;

  gtk_plot_text_get_size(text, angle, font, 
                         font_height, 
                         width, height, &ascent, &descent);

  *x = 0;
  *y = 0;

  switch(just){
    case GTK_JUSTIFY_LEFT:
      switch(angle){
        case 0:
            *y -= ascent;
            break;
        case 90:
            *y -= *height;
            *x -= ascent;
            break;
        case 180:
            *x -= *width;
            *y -= descent;
            break;
        case 270:
            *x -= descent;
            break;
      }
      break;
    case GTK_JUSTIFY_RIGHT:
      switch(angle){
        case 0:
            *x -= *width;
            *y -= ascent;
            break;
        case 90:
            *x -= ascent;
            break;
        case 180:
            *y -= descent; 
            break;
        case 270:
            *y -= *height;
            *x -= descent; 
            break;
      }
      break;
    case GTK_JUSTIFY_CENTER:
    default:
      switch(angle){
        case 0:
            *x -= *width / 2.;
            *y -= ascent;
	    break;
        case 90:
            *x -= ascent;
            *y -= *height / 2.;
	    break;
        case 180:
            *x -= *width / 2.;
            *y -= descent;
            break;
        case 270:
            *x -= descent;
            *y -= *height / 2.;
            break;
      }
  }
}

/******************************************
 *	gtk_plot_clip_data
 *	gtk_plot_get_position
 *	gtk_plot_get_size
 *      gtk_plot_get_internal_allocation 
 *	gtk_plot_set_background
 *	gtk_plot_move
 *	gtk_plot_resize
 *	gtk_plot_move_resize
 *	gtk_plot_get_pixel
 *	gtk_plot_get_point
 *	gtk_plot_real_get_pixel
 *      gtk_plot_set_xscale
 *      gtk_plot_set_yscale
 *      gtk_plot_draw_text
 ******************************************/
void
gtk_plot_clip_data (GtkPlot *plot, gboolean clip)
{
  plot->clip_data = clip;
}

void
gtk_plot_get_position (GtkPlot *plot, gdouble *x, gdouble *y)
{
  *x = plot->x;
  *y = plot->y;
}

void
gtk_plot_get_size (GtkPlot *plot, gdouble *width, gdouble *height)
{
  *width = plot->width;
  *height = plot->height;
}

GtkAllocation 
gtk_plot_get_internal_allocation (GtkPlot *plot)
{
  GtkAllocation allocation;
  GtkWidget *widget;

  widget = GTK_WIDGET(plot);

  allocation.x = widget->allocation.x + roundint(plot->x * (gdouble)widget->allocation.width);
  allocation.y = widget->allocation.y + roundint(plot->y * (gdouble)widget->allocation.height);
  allocation.width = roundint(plot->width * (gdouble)widget->allocation.width);
  allocation.height = roundint(plot->height * (gdouble)widget->allocation.height);

  return(allocation);
}

void
gtk_plot_set_background(GtkPlot *plot, const GdkColor *color)
{
  plot->background = *color;

  gtk_plot_paint(plot);

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

void
gtk_plot_move (GtkPlot *plot, gdouble x, gdouble y)
{
  gboolean veto = TRUE;

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[MOVED],
                   &x, &y, &veto);

  if(!veto) return;

  plot->left->title.x += (x - plot->x);  
  plot->left->title.y += (y - plot->y);
  plot->right->title.x += (x - plot->x);  
  plot->right->title.y += (y - plot->y);
  plot->top->title.x += (x - plot->x);  
  plot->top->title.y += (y - plot->y);
  plot->bottom->title.x += (x - plot->x);  
  plot->bottom->title.y += (y - plot->y);

  plot->x = x;
  plot->y = y;

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

void
gtk_plot_resize (GtkPlot *plot, gdouble width, gdouble height)
{
  gboolean veto = TRUE;

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[RESIZED],
                   &width, &height, &veto);

  if(!veto) return;

  plot->left->title.y += (height - plot->height) / 2.;
  plot->right->title.x += (width - plot->width);  
  plot->right->title.y += (height - plot->height) / 2.;
  plot->top->title.x += (width - plot->width) / 2.;  
  plot->bottom->title.x += (width - plot->width) / 2.;  
  plot->bottom->title.y += (height - plot->height);

  plot->width = width;
  plot->height = height;

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

void
gtk_plot_set_magnification (GtkPlot *plot, gdouble magnification)
{
  GtkWidget *widget;
 
  widget = GTK_WIDGET(plot); 
  plot->magnification = magnification;
  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}
 
void
gtk_plot_move_resize (GtkPlot *plot, 
	              gdouble x, gdouble y,
                      gdouble width, gdouble height)
{
  gtk_plot_move(plot, x, y);
  gtk_plot_resize(plot, width, height);

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

void
gtk_plot_get_pixel(GtkPlot *plot, gdouble xx, gdouble yy, gdouble *x, gdouble *y)
{
    GTK_PLOT_CLASS(GTK_OBJECT(plot)->klass)->get_pixel (GTK_WIDGET(plot), xx, yy, x, y);
}

void 
gtk_plot_get_point(GtkPlot *plot, gint x, gint y, gdouble *xx, gdouble *yy)
{
    GTK_PLOT_CLASS(GTK_OBJECT(plot)->klass)->get_point (GTK_WIDGET(plot), x, y, xx, yy);
}


static void
gtk_plot_real_get_pixel(GtkWidget *widget, 
                        gdouble xx, gdouble yy , 
			gdouble *x, gdouble *y)
{
    GtkPlot *plot;
    gint xp, yp, width, height;

    plot = GTK_PLOT(widget); 
    xp = plot->x * widget->allocation.width;
    yp = plot->y * widget->allocation.height;
    width = plot->width * widget->allocation.width;
    height = plot->height * widget->allocation.height;

    *y = transform_y(plot, yy);
    *x = transform_x(plot, xx);

    *x = *x + widget->allocation.x + xp;
    *y = widget->allocation.y + yp + height - *y;
}

static void 
gtk_plot_real_get_point(GtkWidget *widget, 
                        gint x, gint y, 
                        gdouble *px, gdouble *py)
{
    GtkPlot *plot;
    gdouble xx, yy;
    gdouble xp, yp, width, height;

    plot = GTK_PLOT(widget); 
    xp = roundint(plot->x * widget->allocation.width);
    yp = roundint(plot->y * widget->allocation.height);
    width = roundint(plot->width * widget->allocation.width);
    height = roundint(plot->height * widget->allocation.height);

    xx = x - widget->allocation.x - xp;
    yy = widget->allocation.y + height + yp - y;

    *px = inverse_x(plot, xx);
    *py = inverse_y(plot, yy);
}

void
gtk_plot_set_xrange (GtkPlot *plot, 
                     gdouble xmin, gdouble xmax)
{
  if(xmin > xmax) return;

  plot->xmin = xmin;
  plot->xmax = xmax;

  plot->bottom->min = xmin;
  plot->bottom->max = xmax;
  plot->top->min = xmin;
  plot->top->max = xmax;

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

void
gtk_plot_set_yrange (GtkPlot *plot, 
                     gdouble ymin, gdouble ymax) 
{
  if(ymin > ymax) return;

  plot->ymin = ymin;
  plot->ymax = ymax;

  plot->left->min = ymin;
  plot->left->max = ymax;
  plot->right->min = ymin;
  plot->right->max = ymax;

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}


void
gtk_plot_set_range (GtkPlot *plot, 
                    gdouble xmin, gdouble xmax, 
                    gdouble ymin, gdouble ymax)
{
  if(xmin > xmax || ymin > ymax) return;

  plot->xmin = xmin;
  plot->xmax = xmax;
  plot->ymin = ymin;
  plot->ymax = ymax;

  plot->bottom->min = xmin;
  plot->bottom->max = xmax;
  plot->top->min = xmin;
  plot->top->max = xmax;
  plot->left->min = ymin;
  plot->left->max = ymax;
  plot->right->min = ymin;
  plot->right->max = ymax;

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

void
gtk_plot_autoscale(GtkPlot *plot)
{
  GtkPlotData *dataset;
  GList *list;
  gdouble xmin, xmax;
  gdouble ymin, ymax;
  gdouble pmin, pmax;
  gdouble dx, dy;
  gint n;
  gboolean change = FALSE;

  if(!plot->data_sets) return;

  xmin = plot->xmax;
  xmax = plot->xmin;
  ymin = plot->ymax;
  ymax = plot->ymin;

  list = plot->data_sets;
  while(list){
     dataset = GTK_PLOT_DATA(list->data);
     if(!dataset->is_function){
       for(n = 0; n < dataset->num_points; n++){
           if(dataset->x[n] < xmin) xmin = dataset->x[n];
           if(dataset->y[n] < ymin) ymin = dataset->y[n];
           if(dataset->x[n] > xmax) xmax = dataset->x[n];
           if(dataset->y[n] > ymax) ymax = dataset->y[n];
           change = TRUE;
       }
     }

     list = list->next;
  } 

  if(!change) return;

  if(plot->bottom->scale == GTK_PLOT_SCALE_LOG10) {
          plot->bottom->ticks.step = 1;
          plot->bottom->ticks.nminor = 8;
          xmin = floor(log10(fabs(xmin))) - 1.;
          xmin = pow(10., xmin);
          xmax = ceil(log10(fabs(xmax)));
          xmax = pow(10., xmax);
  } else {
          dx = (xmax - xmin) / 8.;
          xmin -= dx;
          xmax += dx;
          if(xmin == 0.0) xmin = -dx;
          if(xmax == 0.0) xmin = dx;
          pmin = floor(log10(fabs(xmin))) - 1.;
          pmax = floor(log10(fabs(xmax))) - 1.;
          xmin = floor(xmin/pow(10., pmin)) * pow(10., pmin);
          xmax = ceil(xmax/pow(10., pmax)) * pow(10., pmax);
          plot->bottom->ticks.step = pow(10., pmax);
          dx = floor((xmax - xmin)/plot->bottom->ticks.step);
          while(dx > 10.){
            plot->bottom->ticks.step *= 2;
            dx = floor((xmax - xmin)/plot->bottom->ticks.step);
          }
          xmin = floor(xmin/plot->bottom->ticks.step) * plot->bottom->ticks.step;
          xmax = ceil(xmax/plot->bottom->ticks.step) * plot->bottom->ticks.step;

          plot->top->ticks.step = plot->bottom->ticks.step;
  }

  if(plot->left->scale == GTK_PLOT_SCALE_LOG10) {
          plot->left->ticks.step = 1;
          plot->left->ticks.nminor = 8;
          ymin = floor(log10(fabs(ymin))) - 1.;
          ymin = pow(10., ymin);
          ymax = ceil(log10(fabs(ymax)));
          ymax = pow(10., ymax);
  } else {
          dy = (ymax - ymin) / 8.;
          ymin -= dy;
          ymax += dy;
          if(ymin == 0.0) ymin = -dy;
          if(ymax == 0.0) ymin = dy;
          pmin = floor(log10(fabs(ymin))) - 1.;
          pmax = floor(log10(fabs(ymax))) - 1.;
          ymin = floor(ymin/pow(10., pmin)) * pow(10., pmin);
          ymax = ceil(ymax/pow(10., pmax)) * pow(10., pmax);
          plot->left->ticks.step = pow(10., pmax);
          dy = floor((ymax - ymin)/plot->left->ticks.step);
          while(dy > 10.){
            plot->left->ticks.step *= 2;
            dy = floor((ymax - ymin)/plot->left->ticks.step);
          }
          ymin = floor(ymin/plot->left->ticks.step) * plot->left->ticks.step;
          ymax = ceil(ymax/plot->left->ticks.step) * plot->left->ticks.step;

          plot->right->ticks.step = plot->left->ticks.step;
  }

  gtk_plot_set_range(plot, xmin, xmax, ymin, ymax);

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

void
gtk_plot_get_xrange (GtkPlot *plot, 
                     gdouble *xmin, gdouble *xmax)
{
  *xmax = plot->xmax;
  *xmin = plot->xmin;
}

void
gtk_plot_get_yrange (GtkPlot *plot, 
                     gdouble *ymin, gdouble *ymax)
{
  *ymax = plot->ymax;
  *ymin = plot->ymin;
}

void
gtk_plot_set_xscale (GtkPlot *plot, GtkPlotScale scale_type)
{
  plot->xscale = scale_type;
  plot->bottom->scale = scale_type;
  plot->top->scale = scale_type;

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

void
gtk_plot_set_yscale (GtkPlot *plot, GtkPlotScale scale_type)
{
  plot->yscale = scale_type;
  plot->left->scale = scale_type;
  plot->right->scale = scale_type;

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

GtkPlotScale
gtk_plot_get_xscale (GtkPlot *plot)
{
  return plot->left->scale;
}

GtkPlotScale
gtk_plot_get_yscale (GtkPlot *plot)
{
  return plot->bottom->scale;
}

GtkPlotText *
gtk_plot_put_text (GtkPlot *plot, gdouble x, gdouble y, 
                   const gchar *font, gint height, gint angle,
                   const GdkColor *fg, const GdkColor *bg, 
		   gboolean transparent,
		   GtkJustification justification,
	           const gchar *text)
{
  GtkWidget *widget;
  GtkPlotText *text_attr;
  GdkRectangle area;

  widget = GTK_WIDGET(plot);

  text_attr = g_new0(GtkPlotText, 1);

  area.x = widget->allocation.x;
  area.y = widget->allocation.y;

  text_attr->x = x;
  text_attr->y = y;
  text_attr->angle = angle;
  text_attr->justification = justification;
  text_attr->fg = widget->style->black;
  text_attr->bg = widget->style->white;
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

  plot->text = g_list_append(plot->text, text_attr);
  gtk_plot_draw_text(plot, *text_attr);

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
  return text_attr;
}

void
gtk_plot_text_set_attributes (GtkPlotText *text_attr,
                              const gchar *font,
                              gint height,
                              gint angle,
                              const GdkColor *fg,
                              const GdkColor *bg,
			      gboolean transparent,
		              GtkJustification justification,
	                      const gchar *text)
{
  text_attr->angle = angle;
  gdk_color_black(gdk_colormap_get_system(), &text_attr->fg);
  gdk_color_white(gdk_colormap_get_system(), &text_attr->bg);
  text_attr->justification = justification;
  text_attr->transparent = transparent;

  if(!font) {
    text_attr->font = g_strdup(DEFAULT_FONT);
    text_attr->height = DEFAULT_FONT_HEIGHT;
  } else {
    text_attr->font = g_strdup(font);
    text_attr->height = height;
  }

  if(text_attr->text) g_free(text_attr->text);
  text_attr->text = NULL;
  if(text) text_attr->text = g_strdup(text);

  if(bg != NULL)
    text_attr->bg = *bg;
  
  if(fg != NULL)
    text_attr->fg = *fg;
  
}

/******************************************
 *      gtk_plot_get_axis
 *	gtk_plot_axis_set_visible
 *	gtk_plot_axis_visible
 *	gtk_plot_axis_set_title
 *	gtk_plot_axis_show_title
 *	gtk_plot_axis_hide_title
 *	gtk_plot_axis_move_title
 *	gtk_plot_axis_justify_title
 *	gtk_plot_axis_show_labels
 *	gtk_plot_axis_set_attributes
 *	gtk_plot_axis_get_attributes
 *	gtk_plot_axis_set_ticks
 *	gtk_plot_axis_set_major_ticks
 *	gtk_plot_axis_set_minor_ticks
 *	gtk_plot_axis_set_ticks_length
 *	gtk_plot_axis_show_ticks
 *	gtk_plot_axis_set_ticks_limits
 *	gtk_plot_axis_unset_ticks_limits
 *	gtk_plot_axis_labels_set_attributes
 *	gtk_plot_axis_labels_set_numbers
 ******************************************/

GtkPlotAxis *
gtk_plot_get_axis (GtkPlot *plot, GtkPlotAxisPos axis)
{
  GtkPlotAxis *aux = NULL;

  switch(axis){
    case GTK_PLOT_AXIS_LEFT:
         aux = plot->left;
         break;
    case GTK_PLOT_AXIS_RIGHT:
         aux = plot->right;
         break;
    case GTK_PLOT_AXIS_TOP:
         aux = plot->top;
         break;
    case GTK_PLOT_AXIS_BOTTOM:
         aux = plot->bottom;
         break;
  }
  return aux; 
}

void
gtk_plot_axis_set_visible (GtkPlot *plot, GtkPlotAxisPos axis, gboolean visible)
{
  GtkPlotAxis *aux;

  aux = gtk_plot_get_axis (plot, axis);
  aux->is_visible = visible;

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

gboolean
gtk_plot_axis_visible (GtkPlot *plot, GtkPlotAxisPos axis)
{
  GtkPlotAxis *aux;

  aux = gtk_plot_get_axis (plot, axis);
  return aux->is_visible;
}


void
gtk_plot_axis_set_title (GtkPlot *plot, GtkPlotAxisPos axis, const gchar *title)
{
  GtkPlotAxis *aux;

  aux = gtk_plot_get_axis (plot, axis);

  if(aux->title.text)
     g_free(aux->title.text);

  aux->title.text = g_strdup(title);

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

void
gtk_plot_axis_show_title (GtkPlot *plot, GtkPlotAxisPos axis)
{
  GtkPlotAxis *aux;

  aux = gtk_plot_get_axis (plot, axis);
  aux->title_visible = TRUE;

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

void
gtk_plot_axis_hide_title (GtkPlot *plot, GtkPlotAxisPos axis)
{
  GtkPlotAxis *aux;

  aux = gtk_plot_get_axis (plot, axis);
  aux->title_visible = FALSE;

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

void
gtk_plot_axis_move_title (GtkPlot *plot, GtkPlotAxisPos axis, gint angle, gdouble x, gdouble y)
{
  GtkPlotAxis *aux;

  aux = gtk_plot_get_axis (plot, axis);
  aux->title.angle = angle;
  aux->title.x = x;
  aux->title.y = y;

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

void
gtk_plot_axis_justify_title (GtkPlot *plot, GtkPlotAxisPos axis, GtkJustification justification)
{
  GtkPlotAxis *aux;

  aux = gtk_plot_get_axis (plot, axis);
  aux->title.justification = justification;

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

void 
gtk_plot_axis_set_attributes (GtkPlot *plot, GtkPlotAxisPos axis, 
			      gfloat width, const GdkColor *color)
{
  GtkPlotAxis *aux;

  aux = gtk_plot_get_axis (plot, axis);
  aux->line.line_width = width;

  aux->line.color = *color;

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

void 
gtk_plot_axis_get_attributes (GtkPlot *plot, GtkPlotAxisPos axis, 
			      gfloat *width, GdkColor *color)
{
  GtkPlotAxis *aux;

  aux = gtk_plot_get_axis (plot, axis);
  *width = aux->line.line_width;
  *color = aux->line.color;
}

void
gtk_plot_axis_set_ticks (GtkPlot *plot,
                         GtkPlotOrientation orientation,
		         gdouble major_step,
		         gint nminor)
{
  if(orientation == GTK_PLOT_AXIS_X){
  	plot->bottom->ticks.step = major_step;
  	plot->bottom->ticks.nminor = nminor;
  	plot->top->ticks.step = major_step;
  	plot->top->ticks.nminor = nminor;
        gtk_plot_calc_ticks(plot, plot->bottom);
        gtk_plot_calc_ticks(plot, plot->top);
  }else{
  	plot->left->ticks.step = major_step;
  	plot->left->ticks.nminor = nminor;
  	plot->right->ticks.step = major_step;
  	plot->right->ticks.nminor = nminor;
        gtk_plot_calc_ticks(plot, plot->left);
        gtk_plot_calc_ticks(plot, plot->right);
  }


  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

void
gtk_plot_axis_set_major_ticks (GtkPlot *plot,
                               GtkPlotOrientation orientation,
		               gdouble major_step)
{
  if(orientation == GTK_PLOT_AXIS_X){
  	plot->bottom->ticks.step = major_step;
  	plot->top->ticks.step = major_step;
        gtk_plot_calc_ticks(plot, plot->bottom);
        gtk_plot_calc_ticks(plot, plot->top);
  }else{
  	plot->left->ticks.step = major_step;
  	plot->right->ticks.step = major_step;
        gtk_plot_calc_ticks(plot, plot->left);
        gtk_plot_calc_ticks(plot, plot->right);
  }

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

void
gtk_plot_axis_set_minor_ticks (GtkPlot *plot,
                               GtkPlotOrientation orientation,
		               gint nminor)
{
  if(orientation == GTK_PLOT_AXIS_X){
  	plot->bottom->ticks.nminor = nminor;
  	plot->top->ticks.nminor = nminor;
        gtk_plot_calc_ticks(plot, plot->bottom);
        gtk_plot_calc_ticks(plot, plot->top);
  }else{
  	plot->left->ticks.nminor = nminor;
  	plot->right->ticks.nminor = nminor;
        gtk_plot_calc_ticks(plot, plot->left);
        gtk_plot_calc_ticks(plot, plot->right);
  }

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

void
gtk_plot_axis_set_ticks_length (GtkPlot *plot, GtkPlotAxisPos axis, gint length)
{
  GtkPlotAxis *aux;

  aux = gtk_plot_get_axis (plot, axis);
  aux->ticks_length = length;

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

void
gtk_plot_axis_set_ticks_width (GtkPlot *plot, GtkPlotAxisPos axis, gfloat width)
{
  GtkPlotAxis *aux;

  aux = gtk_plot_get_axis (plot, axis);
  aux->ticks_width = width;

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

void
gtk_plot_axis_show_ticks (GtkPlot *plot,	
                          GtkPlotAxisPos axis,
			  gint major_mask,
                          gint minor_mask)
{
  GtkPlotAxis *aux;

  aux = gtk_plot_get_axis (plot, axis);
  aux->major_mask = major_mask;
  aux->minor_mask = minor_mask;

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

void
gtk_plot_axis_set_ticks_limits (GtkPlot *plot,	
                                GtkPlotOrientation orientation,
                          	gdouble begin, gdouble end)
{
  if(end < begin) return;

  if(orientation == GTK_PLOT_AXIS_X){
  	plot->top->ticks.begin = begin;
  	plot->top->ticks.end = end;
        plot->top->ticks.set_limits = TRUE;
  	plot->bottom->ticks.begin = begin;
  	plot->bottom->ticks.end = end;
        plot->bottom->ticks.set_limits = TRUE;
        gtk_plot_calc_ticks(plot, plot->top);
        gtk_plot_calc_ticks(plot, plot->bottom);
  }else{
  	plot->left->ticks.begin = begin;
  	plot->left->ticks.end = end;
        plot->left->ticks.set_limits = TRUE;
  	plot->right->ticks.begin = begin;
  	plot->right->ticks.end = end;
        plot->right->ticks.set_limits = TRUE;
        gtk_plot_calc_ticks(plot, plot->left);
        gtk_plot_calc_ticks(plot, plot->right);
  }

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

void
gtk_plot_axis_unset_ticks_limits (GtkPlot *plot,	
                                  GtkPlotOrientation orientation)
{
  if(orientation == GTK_PLOT_AXIS_X){
        plot->top->ticks.set_limits = FALSE;
        gtk_plot_calc_ticks(plot, plot->top);
        gtk_plot_calc_ticks(plot, plot->bottom);
  }else{
        plot->right->ticks.set_limits = FALSE;
        plot->left->ticks.set_limits = FALSE;
        gtk_plot_calc_ticks(plot, plot->left);
        gtk_plot_calc_ticks(plot, plot->right);
  }

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

void
gtk_plot_axis_show_labels (GtkPlot *plot, GtkPlotAxisPos axis, gint mask)
{
  GtkPlotAxis *aux;

  aux = gtk_plot_get_axis (plot, axis);
  aux->label_mask = mask;

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}


void
gtk_plot_axis_title_set_attributes (GtkPlot *plot,	
              		            GtkPlotAxisPos axis,
				    const gchar *font,
                                    gint height,
                                    gint angle,
			            const GdkColor *fg,
			            const GdkColor *bg,
				    gboolean transparent,
				    GtkJustification justification)
{
  GtkPlotAxis *aux;

  aux = gtk_plot_get_axis (plot, axis);

  if(!font){
   /* Use previous font */
/*    aux->title.font = g_strdup(DEFAULT_FONT);
    aux->title.height = DEFAULT_FONT_HEIGHT;
*/
  } else {
    if(aux->title.font) g_free(aux->title.font);
    aux->title.font = g_strdup(font);
    aux->title.height = height;
  }

  aux->title.fg = GTK_WIDGET(plot)->style->black;
  aux->title.bg = GTK_WIDGET(plot)->style->white;

  if(fg) aux->title.fg = *fg;
  if(bg) aux->title.bg = *bg;

  aux->title.angle = angle;
  aux->title.transparent = transparent;
  aux->title.justification = justification;

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

void
gtk_plot_axis_set_labels_attributes (GtkPlot *plot,	
              		             GtkPlotAxisPos axis,
				     const gchar *font,
                                     gint height,
                                     gint angle,
			             const GdkColor *fg,
			             const GdkColor *bg,
				     gboolean transparent,
				     GtkJustification justification)
{
  GtkPlotAxis *aux;

  aux = gtk_plot_get_axis (plot, axis);

  if(!font){
   /* Use previous font */
/*    aux->labels_attr.font = g_strdup(DEFAULT_FONT);
    aux->labels_attr.height = DEFAULT_FONT_HEIGHT;
*/
  } else {
    if(aux->labels_attr.font) g_free(aux->labels_attr.font);
    aux->labels_attr.font = g_strdup(font);
    aux->labels_attr.height = height;
  }

  aux->labels_attr.angle = angle;

  aux->labels_attr.fg = GTK_WIDGET(plot)->style->black;
  aux->labels_attr.bg = GTK_WIDGET(plot)->style->white;

  if(fg) aux->labels_attr.fg = *fg;
  if(bg) aux->labels_attr.bg = *bg;

  aux->labels_attr.transparent = transparent;
  aux->labels_attr.justification = justification;

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

void
gtk_plot_axis_set_labels_numbers (GtkPlot *plot,	
              		          GtkPlotAxisPos axis,
              		          gint style,
              		          gint precision)
{
  GtkPlotAxis *aux;

  aux = gtk_plot_get_axis (plot, axis);
  aux->label_style = style;
  aux->label_precision = precision;

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

void
gtk_plot_axis_use_custom_tick_labels (GtkPlot *plot,
                                      GtkPlotAxisPos axispos,
                                      gboolean use)
{
  GtkPlotAxis *axis;
  
  axis = gtk_plot_get_axis(plot, axispos);
  axis->custom_labels = use;

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

void
gtk_plot_axis_set_labels_prefix (GtkPlot *plot,
                                 GtkPlotAxisPos axispos,
				 const gchar *text)
{
  GtkPlotAxis *axis;

  axis = gtk_plot_get_axis(plot, axispos);
  if(axis->labels_prefix) g_free(axis->labels_prefix);

  if(text)
    axis->labels_prefix = g_strdup(text);
  else 
    axis->labels_prefix = NULL;

}

void
gtk_plot_axis_set_labels_suffix (GtkPlot *plot,
                                 GtkPlotAxisPos axispos,
				 const gchar *text)
{
  GtkPlotAxis *axis;

  axis = gtk_plot_get_axis(plot, axispos);
  if(axis->labels_suffix) g_free(axis->labels_suffix);

  if(text)
    axis->labels_suffix = g_strdup(text);
  else 
    axis->labels_suffix = NULL;

}

gchar *
gtk_plot_axis_get_labels_prefix (GtkPlot *plot,
                                 GtkPlotAxisPos axispos)
{
  GtkPlotAxis *axis;

  axis = gtk_plot_get_axis(plot, axispos);

  return (axis->labels_prefix);
}

gchar *
gtk_plot_axis_get_labels_suffix (GtkPlot *plot,
                                 GtkPlotAxisPos axispos)
{
  GtkPlotAxis *axis;

  axis = gtk_plot_get_axis(plot, axispos);

  return (axis->labels_suffix);
}

/******************************************
 *      gtk_plot_x0line_set_attributes
 *      gtk_plot_y0line_set_attributes
 *      gtk_plot_major_vgrid_set_attributes
 *      gtk_plot_minor_vgrid_set_attributes
 *      gtk_plot_major_hgrid_set_attributes
 *      gtk_plot_minor_hgrid_set_attributes
 ******************************************/

void 
gtk_plot_x0_set_visible(GtkPlot *plot, gboolean visible)
{
  plot->show_x0 = visible;

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

gboolean 
gtk_plot_x0_visible(GtkPlot *plot)
{
  return plot->show_x0;
}

void 
gtk_plot_y0_set_visible(GtkPlot *plot, gboolean visible)
{
  plot->show_y0 = visible;

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

gboolean 
gtk_plot_y0_visible(GtkPlot *plot)
{
  return plot->show_y0;
}

void 
gtk_plot_grids_set_visible(GtkPlot *plot, 
                           gboolean vmajor, gboolean vminor,
                           gboolean hmajor, gboolean hminor)
{
  plot->bottom->show_major_grid = vmajor;
  plot->bottom->show_minor_grid = vminor;
  plot->left->show_major_grid = hmajor;
  plot->left->show_minor_grid = hminor;

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}


void 
gtk_plot_grids_visible(GtkPlot *plot, 
                           gboolean *vmajor, gboolean *vminor,
                           gboolean *hmajor, gboolean *hminor)
{
  *vmajor = plot->bottom->show_major_grid;
  *vminor = plot->bottom->show_minor_grid;
  *hmajor = plot->left->show_major_grid;
  *hminor = plot->left->show_minor_grid;
}

void 
gtk_plot_x0line_set_attributes(GtkPlot *plot, 
                               GtkPlotLineStyle line_style,
                               gfloat width,
                               const GdkColor *color)
{
  plot->x0_line.line_style = line_style;
  plot->x0_line.line_width = width;
  if(color) plot->x0_line.color = *color;

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

void 
gtk_plot_y0line_set_attributes(GtkPlot *plot, 
                               GtkPlotLineStyle line_style,
                               gfloat width,
                               const GdkColor *color)
{
  plot->y0_line.line_style = line_style;
  plot->y0_line.line_width = width;
  if(color) plot->y0_line.color = *color;

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

void 
gtk_plot_major_vgrid_set_attributes(GtkPlot *plot, 
                                    GtkPlotLineStyle line_style,
                                    gfloat width,
                                    const GdkColor *color)
{
  plot->bottom->major_grid.line_style = line_style;
  plot->bottom->major_grid.line_width = width;
  if(color) plot->bottom->major_grid.color = *color;

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

void 
gtk_plot_minor_vgrid_set_attributes(GtkPlot *plot, 
                                    GtkPlotLineStyle line_style,
                                    gfloat width,
                                    const GdkColor *color)
{
  plot->bottom->minor_grid.line_style = line_style;
  plot->bottom->minor_grid.line_width = width;
  if(color) plot->bottom->minor_grid.color = *color;

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

void 
gtk_plot_major_hgrid_set_attributes(GtkPlot *plot, 
                                    GtkPlotLineStyle line_style,
                                    gfloat width,
                                    const GdkColor *color)
{
  plot->left->major_grid.line_style = line_style;
  plot->left->major_grid.line_width = width;
  if(color) plot->left->major_grid.color = *color;

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

void 
gtk_plot_minor_hgrid_set_attributes(GtkPlot *plot, 
                                    GtkPlotLineStyle line_style,
                                    gfloat width,
                                    const GdkColor *color)
{
  plot->left->minor_grid.line_style = line_style;
  plot->left->minor_grid.line_width = width;
  if(color) plot->left->minor_grid.color = *color;

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

/******************************************
 * gtk_plot_show_legends
 * gtk_plot_hide_legends
 * gtk_plot_show_legends_border
 * gtk_plot_hide_legends_border
 * gtk_plot_legends_move
 * gtk_plot_legends_get_position
 * gtk_plot_legends_get_allocation
 * gtk_plot_set_legends_attributes
 ******************************************/

void
gtk_plot_show_legends(GtkPlot *plot)
{
  plot->show_legends = TRUE;

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}


void
gtk_plot_hide_legends(GtkPlot *plot)
{
  plot->show_legends = FALSE;

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

void
gtk_plot_set_legends_border(GtkPlot *plot, 
                            GtkPlotBorderStyle legends_border, 
                            gint shadow_width)
{
  plot->legends_border = legends_border;
  plot->legends_shadow_width = shadow_width;

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

void
gtk_plot_legends_move(GtkPlot *plot, gdouble x, gdouble y)
{
  plot->legends_x = x;
  plot->legends_y = y;

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

void
gtk_plot_legends_get_position(GtkPlot *plot, gdouble *x, gdouble *y)
{
  *x = plot->legends_x;
  *y = plot->legends_y;
}

GtkAllocation
gtk_plot_legends_get_allocation(GtkPlot *plot)
{
  GtkAllocation allocation;
  GtkWidget *widget;
  GList *datasets;
  gdouble m;

  widget = GTK_WIDGET(plot);
  m = plot->magnification;

  allocation.x = widget->allocation.x + plot->x * widget->allocation.width +
                 plot->legends_x * plot->width * widget->allocation.width; 
  allocation.y = widget->allocation.y + plot->y * widget->allocation.height +
                 plot->legends_y * plot->height * widget->allocation.height; 

  allocation.width = roundint(24 * m);
  allocation.height = roundint(8 * m);

  datasets = g_list_first(plot->data_sets);
  while(datasets)
   {
     GtkPlotData *dataset;
     gint lwidth, lheight;

     dataset = GTK_PLOT_DATA(datasets->data);
 
     if(GTK_WIDGET_VISIBLE(GTK_WIDGET(dataset)) && dataset->show_legend)
       { 
         GTK_PLOT_DATA_CLASS(GTK_OBJECT(dataset)->klass)->get_legend_size(dataset, &lwidth, &lheight);
         allocation.width = MAX(allocation.width, lwidth); 
         allocation.height += lheight;
       }

     datasets = datasets->next;
   }

  return(allocation);
}

void
gtk_plot_legends_set_attributes(GtkPlot *plot, const gchar *font, gint height, 
			        const GdkColor *foreground, const GdkColor *background)
{

  g_free(plot->legends_attr.font);

  if(!font) {
    plot->legends_attr.font = g_strdup(DEFAULT_FONT);
    plot->legends_attr.height = DEFAULT_FONT_HEIGHT;
  } else {
    plot->legends_attr.font = g_strdup(font);
    plot->legends_attr.height = height;
  }
  plot->legends_attr.fg = GTK_WIDGET(plot)->style->black;
  plot->legends_attr.bg = GTK_WIDGET(plot)->style->white;

  if(foreground != NULL)
    plot->legends_attr.fg = *foreground;

  plot->legends_attr.transparent = TRUE;
  if(background != NULL){
    plot->legends_attr.bg = *background;
    plot->legends_attr.transparent = FALSE;
  }

  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

void
gtk_plot_add_data(GtkPlot *plot,
                  GtkPlotData *dataset)
{
  plot->data_sets = g_list_append(plot->data_sets, dataset);
  dataset->plot = plot;
  gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
}

GtkPlotData *
gtk_plot_add_function(GtkPlot *plot, GtkPlotFunc function)
{
  GtkPlotData *dataset;

  dataset = GTK_PLOT_DATA(gtk_plot_data_new_function(function));

  gtk_plot_add_data(plot, dataset);

  return (dataset);
}

gint
gtk_plot_remove_data(GtkPlot *plot, GtkPlotData *dataset)
{
  GList *datasets;
  gpointer data;

  datasets = plot->data_sets;

  while(datasets)
   {
     data = datasets->data;
     
     if(GTK_PLOT_DATA(data) == dataset){
          plot->data_sets = g_list_remove_link(plot->data_sets, datasets);
          g_list_free_1(datasets);
          gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
	  return TRUE;
     }
     datasets = datasets->next;
   }

  return FALSE;
}

gint
gtk_plot_remove_text(GtkPlot *plot, GtkPlotText *text)
{
  GList *list;
  gpointer data;

  list = plot->text;

  while(list)
   {
     data = list->data;
     
     if((GtkPlotText *)data == text){
              plot->text = g_list_remove_link(plot->text, list);
              g_list_free_1(list);
              gtk_signal_emit (GTK_OBJECT(plot), plot_signals[CHANGED]);
	      return TRUE;
     }
     list = list->next;
   }

   return FALSE;
}


static void
gtk_plot_calc_ticks(GtkPlot *plot, GtkPlotAxis *axis)
{
  GtkPlotTicks *ticks = NULL;
  GtkPlotScale scale;
  gdouble min = 0., max = 0.;
  gint pt;
  gdouble tick = 0.0;
  gdouble tick_step = 0.0;
  gdouble major_step;
  gdouble minor_step;
  gint nmajor, nminor;
  gint n;
  GtkPlotOrientation orientation = axis->orientation;
  gdouble absmin, absmax;
  
 
  scale = axis->scale;
  ticks = &axis->ticks; 

  max = axis->max;
  min = axis->min;

  absmin = min;
  absmax = max;

  if(ticks->set_limits){
       max = MIN(max, ticks->end);
       min = MAX(min, ticks->begin);
       absmin = min;
       absmax = max;
  } else {
       min = floor(min/ticks->step) * ticks->step;
       max = ceil(max/ticks->step) * ticks->step;
  }


  if(scale == GTK_PLOT_SCALE_LOG10){
    if(max <= 1.E-12) max = 1.E-2;
    if(min <= 1.E-12) min = max/1000.;
    if(max <= 0) max = fabs(max);
    if(min <= 0) min = max / 100.;
    min = floor(log10(min));
    min = pow(10., min);
    absmin = axis->min;
    absmax = axis->max;
  }

  if(ticks->major != NULL){
     g_free(ticks->major);
     g_free(ticks->minor);
     g_free(ticks->major_values);
     g_free(ticks->minor_values);
     ticks->major = NULL;
     ticks->minor = NULL;
     ticks->major_values = NULL;
     ticks->minor_values = NULL;
  }

  nmajor = 0; 
  nminor = 0; 
  ticks->nmajorticks = 0; 
  ticks->nminorticks = 0; 
  major_step = ticks->step;
  minor_step = major_step / ((gdouble)ticks->nminor + 1.0);

  if(scale == GTK_PLOT_SCALE_LOG10){
     if(major_step != 0.) 
         major_step = floor(major_step);

     if(major_step == 0.)
         major_step = 1.0;

     ticks->nminor = 8;
  }

  if(ticks->step > 0.){
   tick_step = min;
   tick = min;
   n = 0;
   while(tick <= max + 2*fabs(major_step)){
     if(tick >= absmin-1.E-10 && tick <= absmax+1.E-10){
        if(orientation == GTK_PLOT_AXIS_X)
                                          pt = transform_x(plot, tick);
        else
                                          pt = transform_y(plot, tick);
        nmajor ++;
        ticks->major = (gint *)g_realloc(ticks->major, nmajor*sizeof(gint)); 
        ticks->major_values = (gdouble *)g_realloc(ticks->major_values, nmajor*sizeof(gdouble)); 
        ticks->major[nmajor-1] = pt;
        ticks->major_values[nmajor-1] = tick;
        ticks->nmajorticks = nmajor;
     }
     switch(scale){
        case GTK_PLOT_SCALE_LINEAR:
            tick += major_step; 
            break;
        case GTK_PLOT_SCALE_LOG10:
            n++;
            tick = tick_step * pow(10., n*major_step); 
            break;
     }
   }
  }

  if(ticks->step >0. && ticks->nminor > 0){
   for(nmajor = 0; nmajor <= ticks->nmajorticks; nmajor++){
    switch(scale){
      case GTK_PLOT_SCALE_LINEAR:
        if(nmajor < ticks->nmajorticks)
          tick_step = ticks->major_values[nmajor] - major_step;
        else
          tick_step = ticks->major_values[nmajor-1];
        tick = tick_step;
        break;
      case GTK_PLOT_SCALE_LOG10:
        if(nmajor < ticks->nmajorticks)
          tick_step = ticks->major_values[nmajor]/10.0;
        else
          tick_step = ticks->major_values[nmajor-1];
        tick = tick_step;
        break;
    }
    for(n = 1; n <= ticks->nminor; n++){
     switch(scale){
        case GTK_PLOT_SCALE_LINEAR:
            tick += minor_step; 
            break;
        case GTK_PLOT_SCALE_LOG10:
            tick += tick_step; 
            break;
     }   
     if(tick >= absmin-1.E-10 && tick <= absmax+1.E-10){
        if(orientation == GTK_PLOT_AXIS_X)
                                          pt = transform_x(plot, tick);
        else
                                          pt = transform_y(plot, tick);
        nminor ++;
        ticks->minor = (gint *)g_realloc(ticks->minor, nminor*sizeof(gint)); 
        ticks->minor_values = (gdouble *)g_realloc(ticks->minor_values, nminor*sizeof(gdouble)); 
        ticks->minor[nminor-1] = pt;
        ticks->minor_values[nminor-1] = tick;
        ticks->nminorticks = nminor;
     }
    }
   }
  }

}

