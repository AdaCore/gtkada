/* gtkplotdata - 2d scientific plots widget for gtk+
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
#include "gtkplotpolar.h"
#include "gtkplot3d.h"
#include "gtkplotpc.h"
#include "gtkpsfont.h"

#define DEFAULT_FONT_HEIGHT 18

static gchar DEFAULT_FONT[] = "Helvetica";


static void gtk_plot_data_class_init 		(GtkPlotDataClass *klass);
static void gtk_plot_data_init 			(GtkPlotData *data);
static void gtk_plot_data_destroy		(GtkObject *object);
static void gtk_plot_data_draw 			(GtkWidget *widget, 
						 GdkRectangle *area);
static void gtk_plot_data_draw_private 		(GtkPlotData *data);
static void gtk_plot_data_draw_legend		(GtkPlotData *data, 
						 gint x, gint y);
static void gtk_plot_data_get_legend_size	(GtkPlotData *data, 
						 gint *width, gint *height);
static void gtk_plot_data_draw_gradient		(GtkPlotData *data, 
						 gint x, gint y);
static void gtk_plot_data_real_draw		(GtkPlotData *data, 
						 gint npoints);
static void gtk_plot_data_real_draw_symbol	(GtkPlotData *data, 
						 gdouble x, 
                                                 gdouble y, 
                                                 gdouble z, 
                                                 gdouble a, 
						 gdouble dx, 
                                                 gdouble dy, 
                                                 gdouble dz, 
                                                 gdouble da); 
static void gtk_plot_data_draw_symbol_private	(GtkPlotData *data, 
						 gdouble x, gdouble y, 
						 GtkPlotSymbol symbol);
static void gtk_plot_data_draw_xyz 		(GtkPlotData *data,
						 gint npoints); 
static void gtk_plot_data_draw_errbars		(GtkPlotData *data, 
						 gdouble x, 
                                                 gdouble y, 
                                                 gdouble z, 
						 gdouble dx, 
                                                 gdouble dy, 
                                                 gdouble dz); 
static void gtk_plot_data_draw_down_triangle	(GtkPlotData *data, 
                            		  	 gdouble x,
 						 gdouble y, 
						 gdouble size, 
						 gint filled);
static void gtk_plot_data_draw_up_triangle	(GtkPlotData *data, 
                            		  	 gdouble x,
 						 gdouble y, 
						 gdouble size, 
						 gint filled);
static void gtk_plot_data_draw_right_triangle	(GtkPlotData *data, 
                            		  	 gdouble x,
 						 gdouble y, 
						 gdouble size, 
						 gint filled);
static void gtk_plot_data_draw_left_triangle	(GtkPlotData *data, 
                            		  	 gdouble x,
 						 gdouble y, 
						 gdouble size, 
						 gint filled);
static void gtk_plot_data_draw_diamond		(GtkPlotData *data, 
                            		  	 gdouble x,
 						 gdouble y, 
						 gdouble size, 
						 gint filled);
static void gtk_plot_data_draw_plus		(GtkPlotData *data, 
                            		  	 gdouble x,
 						 gdouble y, 
						 gdouble size); 
static void gtk_plot_data_draw_cross		(GtkPlotData *data, 
                            		  	 gdouble x,
 						 gdouble y, 
						 gdouble size); 
static void gtk_plot_data_draw_star		(GtkPlotData *data, 
                            		  	 gdouble x,
 						 gdouble y, 
						 gdouble size); 

static void gtk_plot_data_connect_points	(GtkPlotData *data, 
						 gint npoints);
static void gtk_plot_data_calc_gradient		(GtkPlotData *data);

static gint roundint				(gdouble x);
static void spline_solve 			(int n, 
                                                 gdouble x[], gdouble y[], 
						 gdouble y2[]);
static gdouble spline_eval 			(int n, 
                                                 gdouble x[], 
                                                 gdouble y[], 
						 gdouble y2[], gdouble val);
static void hsv_to_rgb 				(gdouble  h, 
						 gdouble  s, 
						 gdouble  v,
						 gdouble *r,
						 gdouble *g,
						 gdouble *b);
static void rgb_to_hsv 				(gdouble  r, 
						 gdouble  g, 
						 gdouble  b,
            					 gdouble *h, 
						 gdouble *s, 
						 gdouble *v);

enum {
  DRAW_DATA,
  LAST_SIGNAL
};

static GtkWidgetClass *parent_class = NULL;
static guint data_signals[LAST_SIGNAL] = { 0 };


GtkType
gtk_plot_data_get_type (void)
{
  static GtkType data_type = 0;

  if (!data_type)
    {
      GtkTypeInfo data_info =
      {
	"GtkPlotData",
	sizeof (GtkPlotData),
	sizeof (GtkPlotDataClass),
	(GtkClassInitFunc) gtk_plot_data_class_init,
	(GtkObjectInitFunc) gtk_plot_data_init,
	/* reserved 1*/ NULL,
        /* reserved 2 */ NULL,
        (GtkClassInitFunc) NULL,
      };

      data_type = gtk_type_unique (GTK_TYPE_WIDGET, &data_info);
    }
  return data_type;
}

static void
gtk_plot_data_class_init (GtkPlotDataClass *klass)
{
  GtkObjectClass *object_class;
  GtkWidgetClass *widget_class;
  GtkPlotDataClass *data_class;

  parent_class = gtk_type_class (gtk_widget_get_type ());

  object_class = (GtkObjectClass *) klass;
  widget_class = (GtkWidgetClass *) klass;
  data_class = (GtkPlotDataClass *) klass;

  widget_class->draw = gtk_plot_data_draw;
  object_class->destroy = gtk_plot_data_destroy;

  data_class->draw_symbol = gtk_plot_data_real_draw_symbol;
  data_class->draw_data = gtk_plot_data_draw_private;
  data_class->draw_legend = gtk_plot_data_draw_legend;
  data_class->get_legend_size = gtk_plot_data_get_legend_size;

  data_signals[DRAW_DATA] =
    gtk_signal_new ("draw_data",
                    GTK_RUN_FIRST,
                    object_class->type,
                    GTK_SIGNAL_OFFSET (GtkPlotDataClass, draw_data),
                    gtk_marshal_NONE__NONE,
                    GTK_TYPE_NONE, 0, GTK_TYPE_NONE);

  gtk_object_class_add_signals (object_class, data_signals, LAST_SIGNAL);

}


static void
gtk_plot_data_init (GtkPlotData *dataset)
{
  GtkWidget *widget;
  GdkColormap *colormap;
  GdkColor black, white, color;

  GTK_WIDGET_SET_FLAGS(dataset, GTK_NO_WINDOW);

  widget = GTK_WIDGET(dataset);
  colormap = gtk_widget_get_colormap(widget);

  gdk_color_black(colormap, &black);
  gdk_color_white(colormap, &white);

  gdk_color_parse("red", &color);
  gdk_color_alloc(colormap, &color);
  dataset->color_max = color;

  gdk_color_parse("blue", &color);
  gdk_color_alloc(colormap, &color);
  dataset->color_min = color;

  dataset->gradient.begin = 0.0;
  dataset->gradient.end = 1.0;
  dataset->gradient.nmajorticks = 10;
  dataset->gradient.major = NULL;
  dataset->gradient.major_values = NULL;
  dataset->gradient_mask = GTK_PLOT_GRADIENT_H;
  dataset->show_gradient = FALSE;
  gtk_plot_data_calc_gradient(dataset);
  dataset->legends_precision = 3;

  dataset->plot = NULL;

  dataset->is_function = FALSE;
  dataset->show_legend = TRUE;
  dataset->show_labels = FALSE;
  dataset->fill_area = FALSE;
  dataset->function = NULL;
  dataset->num_points = 0;
  dataset->x = NULL;
  dataset->y = NULL;
  dataset->z = NULL;
  dataset->a = NULL;
  dataset->dx = NULL;
  dataset->dy = NULL;
  dataset->dz = NULL;
  dataset->da = NULL;
  dataset->labels = NULL;
  dataset->x_step = .5;
  dataset->y_step = .5;
  dataset->line_connector = GTK_PLOT_CONNECT_STRAIGHT;

  dataset->line.line_style = GTK_PLOT_LINE_SOLID;
  dataset->line.line_width = 1;
  dataset->line.color = black; 

  dataset->x_line.line_style = GTK_PLOT_LINE_NONE;
  dataset->x_line.line_width = 1;
  dataset->x_line.color = black; 

  dataset->y_line.line_style = GTK_PLOT_LINE_NONE;
  dataset->y_line.line_width = 1;
  dataset->y_line.color = black; 

  dataset->z_line.line_style = GTK_PLOT_LINE_NONE;
  dataset->z_line.line_width = 1;
  dataset->z_line.color = black; 

  dataset->symbol.symbol_type = GTK_PLOT_SYMBOL_NONE;
  dataset->symbol.symbol_style = GTK_PLOT_SYMBOL_EMPTY;
  dataset->symbol.size = 6;
  dataset->symbol.border.line_width = 1;
  dataset->symbol.border.color = black; 
  dataset->symbol.color = black; 

  dataset->show_xerrbars = FALSE;
  dataset->show_yerrbars = FALSE;
  dataset->show_zerrbars = FALSE;
  dataset->xerrbar_width = 1;
  dataset->yerrbar_width = 1;
  dataset->zerrbar_width = 1;
  dataset->xerrbar_caps = 8;
  dataset->yerrbar_caps = 8;
  dataset->zerrbar_caps = 8;

  dataset->legend = NULL;
  dataset->name = NULL;

  dataset->labels_attr.justification = GTK_JUSTIFY_LEFT;
  dataset->labels_attr.transparent = TRUE;
  dataset->labels_attr.font = NULL;
  dataset->labels_attr.text = NULL;
  dataset->labels_offset = 6;

  dataset->link = NULL;

  gtk_plot_data_labels_set_attributes(dataset, 
                                      DEFAULT_FONT,
                                      DEFAULT_FONT_HEIGHT,
                                      90,
                                      &black,
                                      &white);

  gtk_psfont_init();
}

static void
gtk_plot_data_destroy (GtkObject *object)
{
  GtkPlotData *data;

  g_return_if_fail (object != NULL);
  g_return_if_fail (GTK_IS_PLOT_DATA (object));

  data = GTK_PLOT_DATA (object);

  if(data->labels_attr.font) g_free(data->labels_attr.font);
  if(data->legend) g_free(data->legend);
  if(data->name) g_free(data->name);

  if(data->gradient.major){
     g_free(data->gradient.major);
     g_free(data->gradient.major_values);
  }

  gtk_psfont_unref();
}


GtkWidget*
gtk_plot_data_new (void)
{
  GtkWidget *widget;

  widget = gtk_type_new (gtk_plot_data_get_type ());

  return (widget);
}

GtkWidget*
gtk_plot_data_new_function (GtkPlotFunc function)
{
  GtkWidget *dataset;

  dataset = gtk_type_new (gtk_plot_data_get_type ());

  gtk_plot_data_construct_function (GTK_PLOT_DATA(dataset), function);

  return (dataset);
}

void
gtk_plot_data_construct_function (GtkPlotData *data, GtkPlotFunc function)
{
  data->is_function = TRUE;
  data->function = function;
}

static void
gtk_plot_data_draw (GtkWidget *widget, GdkRectangle *area)
{
  if(!GTK_WIDGET_VISIBLE(widget)) return;
  gtk_signal_emit(GTK_OBJECT(widget), data_signals[DRAW_DATA], NULL);
  GTK_PLOT_DATA_CLASS(GTK_OBJECT(widget)->klass)->draw_data(GTK_PLOT_DATA(widget));
}

void
gtk_plot_data_paint (GtkPlotData *data)
{
  if(!GTK_WIDGET_VISIBLE(GTK_WIDGET(data))) return;

  gtk_signal_emit(GTK_OBJECT(data), data_signals[DRAW_DATA], NULL);
}

static void
gtk_plot_data_draw_private (GtkPlotData *data)
{
  gtk_plot_data_real_draw(data, data->num_points);
}

static void
gtk_plot_data_real_draw   (GtkPlotData *dataset,  
			   gint npoints)
{
  GtkWidget *widget;
  GtkPlot *plot = NULL;
  GtkPlotData *function;
  GdkColormap *colormap;
  GdkRectangle area, clip_area;
  gdouble x, y, z = 0., a = 0.;
  gdouble dx = 0., dy = 0., dz = 0., da = 0.;
  gint n;
  gdouble *fx = NULL;
  gdouble *fy = NULL;
  gboolean error;
  gdouble m;

  g_return_if_fail(GTK_IS_PLOT_DATA(dataset));
  g_return_if_fail(dataset->plot != NULL);
  g_return_if_fail(GTK_IS_PLOT(dataset->plot));
  g_return_if_fail(GTK_WIDGET_REALIZED(dataset->plot));

  plot = dataset->plot;
  widget = GTK_WIDGET(plot);
  colormap = gdk_colormap_get_system();

  if(!GTK_WIDGET_DRAWABLE(widget)) return;
  if(!GTK_WIDGET_VISIBLE(widget)) return;
  if(!GTK_WIDGET_VISIBLE(GTK_WIDGET(dataset))) return;

  gtk_plot_pc_gsave(plot->pc);

  m = plot->magnification;
  npoints = MIN(npoints, dataset->num_points);

  area.x = widget->allocation.x;
  area.y = widget->allocation.y;
  area.width = widget->allocation.width;
  area.height = widget->allocation.height;

  clip_area.x = area.x + roundint(plot->x * area.width);
  clip_area.y = area.y + roundint(plot->y * area.height);
  clip_area.width = roundint(plot->width * area.width);
  clip_area.height = roundint(plot->height * area.height);

  if(!dataset->is_function)
    {
       gtk_plot_data_connect_points (dataset, npoints+1);
       gtk_plot_data_draw_xyz(dataset, npoints);
       for(n=dataset->num_points-npoints; n<=dataset->num_points-1; n++)
         {
           x = dataset->x[n];
           y = dataset->y[n];
           if(dataset->z != NULL) z = dataset->z[n];
           if(dataset->a != NULL) a = dataset->a[n];
           if(dataset->dx != NULL) dx = dataset->dx[n];
           if(dataset->dy != NULL) dy = dataset->dy[n];
           if(dataset->dz != NULL) dz = dataset->dz[n];
           if(dataset->da != NULL) da = dataset->da[n];

           if(x >= plot->xmin && x <= plot->xmax){
               GdkColor symbol_color, border_color;
               gint symbol_size;

               symbol_color = dataset->symbol.color;
               border_color = dataset->symbol.border.color;
               symbol_size = dataset->symbol.size;
               if(dataset->da){
                  GdkColor level_color;
                  gtk_plot_data_get_gradient_level(dataset, da, &level_color);
                  gdk_color_alloc(colormap, &level_color);
                  dataset->symbol.color = level_color;
                  dataset->symbol.border.color = level_color;
               }
               if(dataset->a && !GTK_IS_PLOT3D(plot)){
                  gdouble px, py, px0, py0;     
                  gtk_plot_get_pixel(plot, x, y, &px, &py);
                  gtk_plot_get_pixel(plot, x + a, y, &px0, &py0);
                  dataset->symbol.size = fabs(px0 -px);
               }
               GTK_PLOT_DATA_CLASS(GTK_OBJECT(dataset)->klass)->draw_symbol(dataset, x, y, z, a, dx, dy, dz, da); 
               dataset->symbol.color = symbol_color;
               dataset->symbol.border.color = border_color;
               dataset->symbol.size = symbol_size;

               if(dataset->show_labels){
                 if(dataset->labels && dataset->labels[n]){
                    GtkPlotText label;
                    gdouble px, py, pz;

                    if(GTK_IS_PLOT3D(plot))
                        gtk_plot3d_get_pixel(GTK_PLOT3D(plot), x, y, z, 
                                             &px, &py, &pz);
                    else
                        gtk_plot_get_pixel(plot, x, y, &px, &py);

                    label = dataset->labels_attr;
                    label.text = dataset->labels[n];
                    label.x = (gdouble)px / (gdouble)area.width;
                    label.y = (gdouble)(py - roundint((dataset->labels_offset + dataset->symbol.size) * m)) / (gdouble)area.height; 
                    gtk_plot_draw_text(plot, label);	
                 }          
               }
           }
         } 
    } 
  else
    {
       gdouble xmin, xmax, px, py;
       function = dataset;
       function->num_points = 0;
       gtk_plot_get_pixel(plot, plot->xmin, 0, &xmin, &py);
       gtk_plot_get_pixel(plot, plot->xmax, 0, &xmax, &py);
       for(px = xmin; px <= xmax; px += function->x_step) {
            function->num_points++;
            fx = (gdouble *)g_realloc(fx, function->num_points*sizeof(gdouble));
            fy = (gdouble *)g_realloc(fy, function->num_points*sizeof(gdouble)); 
       
            gtk_plot_get_point(plot, px, 0, &x, &y);
            y = function->function (plot, dataset, x, &error);

            if(error)
              {
                 function->x = fx;
                 function->y = fy;
		 function->num_points--;
                 if(function->num_points > 1)
                       gtk_plot_data_connect_points (function, function->num_points);
                 function->num_points = 0;
              }
            else
              {
                fx[function->num_points-1] = x;
                fy[function->num_points-1] = y;
              }
         }
       if(function->num_points > 1 ) 
         {
            function->x = fx;
            function->y = fy;
            gtk_plot_data_connect_points (function, function->num_points);
         }
       g_free(fx);
       g_free(fy);
       function->x = NULL;
       function->y = NULL;
    }
  gtk_plot_pc_grestore(plot->pc);
}

static void
gtk_plot_data_draw_legend(GtkPlotData *data, gint x, gint y)
{
  GtkPlot *plot = NULL;
  GtkPlotText legend;
  GdkRectangle area;
  gint lascent, ldescent, lheight, lwidth;
  gdouble m;

  g_return_if_fail(data->plot != NULL);
  g_return_if_fail(GTK_IS_PLOT(data->plot));
  g_return_if_fail(GTK_WIDGET_REALIZED(data->plot));

  plot = data->plot;
  area.x = GTK_WIDGET(plot)->allocation.x;
  area.y = GTK_WIDGET(plot)->allocation.y;
  area.width = GTK_WIDGET(plot)->allocation.width;
  area.height = GTK_WIDGET(plot)->allocation.height;

  m = plot->magnification; 
  legend = plot->legends_attr;

  if(data->legend)
    legend.text = data->legend;
  else
    legend.text = "";

  gtk_plot_text_get_size(legend.text, legend.angle, legend.font,
                         roundint(legend.height * m), 
                         &lwidth, &lheight,
                         &lascent, &ldescent);


  gtk_plot_pc_gsave(plot->pc);

  lheight = MAX(lheight, data->symbol.size + 2 * data->symbol.border.line_width);
  if(data->line_connector != GTK_PLOT_CONNECT_NONE ||
     data->symbol.symbol_type == GTK_PLOT_SYMBOL_IMPULSE)
       gtk_plot_draw_line(plot, data->line,
                          x,
                          y + lheight / 2,
                          x + roundint(plot->legends_line_width * m),
                          y + lheight / 2);

  if(data->symbol.symbol_type != GTK_PLOT_SYMBOL_IMPULSE){
       GtkPlotSymbol aux_symbol;
       gint x1, y1;

       x1 = x + area.x + roundint(plot->legends_line_width * m) / 2;
       y1 = y + area.y + lheight / 2;

       aux_symbol = data->symbol;
       aux_symbol.color = plot->background;
       aux_symbol.symbol_style = GTK_PLOT_SYMBOL_FILLED;
       aux_symbol.border.line_width = 0;

       if(data->symbol.symbol_style == GTK_PLOT_SYMBOL_OPAQUE)
          gtk_plot_data_draw_symbol_private (data, x1, y1, aux_symbol); 

       if(data->symbol.symbol_style == GTK_PLOT_SYMBOL_FILLED){
          aux_symbol.color = data->symbol.color; 
          gtk_plot_data_draw_symbol_private (data, x1, y1, aux_symbol); 
       }

       aux_symbol = data->symbol;
       aux_symbol.color = data->symbol.border.color; 
       aux_symbol.symbol_style = GTK_PLOT_SYMBOL_EMPTY;
       gtk_plot_data_draw_symbol_private(data, x1, y1, aux_symbol);
  }

  legend.x = (gdouble)(area.x + x + roundint((plot->legends_line_width + 4) * m)) 
             / (gdouble)area.width;
  legend.y = (gdouble)(area.y + y + lheight - ldescent) / (gdouble)area.height;

  gtk_plot_draw_text(plot, legend);

  y += 2 * lheight;

  gtk_plot_data_draw_gradient(data, x, y);

  gtk_plot_pc_grestore(plot->pc);
}

static void
gtk_plot_data_draw_gradient(GtkPlotData *data, gint x, gint y)
{
  GtkPlot *plot = NULL;
  GtkPlotText legend;
  GdkRectangle area;
  GdkColor color;
  gint lascent, ldescent, lheight, lwidth;
  gdouble m;
  gint ncolors;
  gdouble step;
  gint level;
  gdouble h;
  gint cy;

  g_return_if_fail(data->plot != NULL);
  g_return_if_fail(GTK_IS_PLOT(data->plot));
  g_return_if_fail(GTK_WIDGET_REALIZED(data->plot));

  if(!data->show_gradient) return;

  plot = data->plot;
  area.x = GTK_WIDGET(plot)->allocation.x;
  area.y = GTK_WIDGET(plot)->allocation.y;
  area.width = GTK_WIDGET(plot)->allocation.width;
  area.height = GTK_WIDGET(plot)->allocation.height;

  m = plot->magnification; 
  legend = plot->legends_attr;

  if(data->legend)
    legend.text = data->legend;
  else
    legend.text = "";

  gtk_plot_text_get_size(legend.text, legend.angle, legend.font,
                         roundint(legend.height * m), 
                         &lwidth, &lheight,
                         &lascent, &ldescent);

  ncolors = (lascent + ldescent) * data->gradient.nmajorticks;
  step = (data->gradient.end - data->gradient.begin) / ncolors;

  cy = y;
  for(h = data->gradient.end; h >= data->gradient.begin; h -= step){
    gtk_plot_data_get_gradient_level(data, h, &color);
    gdk_color_alloc(gdk_colormap_get_system(), &color);
    gtk_plot_pc_set_color(plot->pc, &color);

    gtk_plot_pc_draw_line(plot->pc, 
                          x, cy,
                          x + roundint(plot->legends_line_width * m), cy);

    cy++;
  }

  gtk_plot_pc_set_color(plot->pc, &plot->legends_attr.fg);
  gtk_plot_pc_draw_rectangle(plot->pc, FALSE,
                             x, y,
                             roundint(plot->legends_line_width * m),
                             ncolors);

  gtk_plot_pc_set_lineattr(plot->pc, plot->legends_border_width, 0, 0, 0);

  y -= (lascent + ldescent) / 2;
  for(level = data->gradient.nmajorticks; level >= 0; level--){
    gchar text[20];

    h = data->gradient.major_values[level];
  
    legend.x = (gdouble)(area.x + x + roundint((plot->legends_line_width + 4) * m))
               / (gdouble)area.width;
    legend.y = (gdouble)(area.y + y + lascent) / (gdouble)area.height;

    sprintf(text, "%.*f", data->legends_precision,  h);
    legend.text = text;

    gtk_plot_pc_draw_line(plot->pc, 
                          x, y + (lascent + ldescent) / 2,
                          x + roundint(4 * m), y + (lascent + ldescent) / 2);
    gtk_plot_pc_draw_line(plot->pc, 
                          x + roundint((plot->legends_line_width - 4)* m), 
                          y + (lascent + ldescent) / 2,
                          x + roundint(plot->legends_line_width * m), 
                          y + (lascent + ldescent) / 2);

    gtk_plot_draw_text(plot, legend);

    y += lascent + ldescent;
  }
}

static void
gtk_plot_data_get_legend_size(GtkPlotData *data, gint *width, gint *height)
{
  GtkPlot *plot = NULL;
  GtkPlotText legend;
  gint lascent, ldescent, lheight, lwidth;
  gdouble m;

  g_return_if_fail(data->plot != NULL);
  g_return_if_fail(GTK_IS_PLOT(data->plot));

  plot = data->plot;
  m = plot->magnification; 

  legend = plot->legends_attr;

  if(data->legend)
    legend.text = data->legend;
  else
    legend.text = "";

  gtk_plot_text_get_size(legend.text, legend.angle, legend.font,
                         roundint(legend.height * m), 
                         &lwidth, &lheight,
                         &lascent, &ldescent);

  *width = lwidth + roundint((plot->legends_line_width + 12) * m);
  *height = MAX(lascent + ldescent, roundint(data->symbol.size * m) + 2 * data->symbol.border.line_width);

  if(data->show_gradient)
    *height += (lascent + ldescent) * (data->gradient.nmajorticks + 2);
}

static void
gtk_plot_data_real_draw_symbol (GtkPlotData *data, 
                                gdouble x, gdouble y, gdouble z, gdouble a,
                                gdouble dx, gdouble dy, gdouble dz, gdouble da)
{
  GtkPlot *plot;
  gdouble px = 0, py = 0, pz = 0;
  
  plot = data->plot;

  if(GTK_IS_PLOT_POLAR(plot)){
    if(plot->clip_data &&
       (y < plot->xmin || y > plot->xmax || x < plot->ymin || x > plot->ymax)) 
             return; 
  } else {
    if(x < plot->xmin || x > plot->xmax) return;
    if(plot->clip_data && data->symbol.symbol_type != GTK_PLOT_SYMBOL_IMPULSE &&
       (y < plot->ymin || y > plot->ymax)) return; 
  }

  if(GTK_IS_PLOT3D(plot))
       gtk_plot3d_get_pixel(GTK_PLOT3D(plot), x, y, z, 
                            &px, &py, &pz);
  else
       gtk_plot_get_pixel(plot, x, y, &px, &py);

  gtk_plot_data_draw_errbars(data, x, y, z, dx, dy, dz); 

  gtk_plot_data_draw_symbol (data, px, py); 
}


void
gtk_plot_data_draw_symbol (GtkPlotData *data, gdouble px, gdouble py) 
{
  GtkPlot *plot;
  GtkPlotSymbol aux_symbol;

  plot = data->plot;

  aux_symbol = data->symbol;
  aux_symbol.color = plot->background;
  aux_symbol.symbol_style = GTK_PLOT_SYMBOL_FILLED;
  aux_symbol.border.line_width = 0;

  if(data->symbol.symbol_style == GTK_PLOT_SYMBOL_OPAQUE)
     gtk_plot_data_draw_symbol_private (data, px, py, aux_symbol); 

  if(data->symbol.symbol_style == GTK_PLOT_SYMBOL_FILLED){
     aux_symbol.color = data->symbol.color; 
     gtk_plot_data_draw_symbol_private (data, px, py, aux_symbol); 
  }

  aux_symbol = data->symbol;
  aux_symbol.color = data->symbol.border.color; 
  aux_symbol.symbol_style = GTK_PLOT_SYMBOL_EMPTY;
  gtk_plot_data_draw_symbol_private (data, px, py, aux_symbol); 
}

static void
gtk_plot_data_draw_symbol_private (GtkPlotData *data, 
                                   gdouble x, gdouble y, 
                                   GtkPlotSymbol symbol)
{
  GtkWidget *widget;
  GtkPlot *plot;
  gdouble x0, y0;
  gdouble px0, py0; 
  GdkRectangle area, clip_area;
  gboolean filled;
  gint size;

  if(symbol.symbol_type == GTK_PLOT_SYMBOL_NONE) return;

  plot = data->plot;
  widget = GTK_WIDGET(plot);

  area.x = GTK_WIDGET(plot)->allocation.x;
  area.y = GTK_WIDGET(plot)->allocation.y;
  area.width = GTK_WIDGET(plot)->allocation.width;
  area.height = GTK_WIDGET(plot)->allocation.height;

  clip_area.x = area.x + roundint(plot->x * area.width);
  clip_area.y = area.y + roundint(plot->y * area.height);
  clip_area.width = roundint(plot->width * area.width);
  clip_area.height = roundint(plot->height * area.height);

/*
  gdk_gc_set_clip_rectangle(gc, &clip_area);
*/

  gtk_plot_pc_set_color(plot->pc, &symbol.color);
  gtk_plot_pc_set_lineattr (plot->pc, symbol.border.line_width, 0, 0, 0);
  filled = (symbol.symbol_style == GTK_PLOT_SYMBOL_FILLED) ? TRUE : FALSE;
  size = symbol.size;

  switch(symbol.symbol_type) {
     case GTK_PLOT_SYMBOL_NONE:
            break;
     case GTK_PLOT_SYMBOL_DOT:
            gtk_plot_pc_draw_point(plot->pc, x, y);
            break;
     case GTK_PLOT_SYMBOL_SQUARE:
            gtk_plot_pc_draw_rectangle (plot->pc,
                                        filled,
                                        x-size/2.0, y-size/2.0,
                                        size, size);
            break;
     case GTK_PLOT_SYMBOL_CIRCLE:
            gtk_plot_pc_draw_circle (plot->pc, filled, x, y, size); 
            break;
     case GTK_PLOT_SYMBOL_UP_TRIANGLE:
            gtk_plot_data_draw_up_triangle (data, x, y, size, filled);                  
            break;
     case GTK_PLOT_SYMBOL_DOWN_TRIANGLE:
            gtk_plot_data_draw_down_triangle (data, x, y, size, filled);                  
            break;
     case GTK_PLOT_SYMBOL_RIGHT_TRIANGLE:
            gtk_plot_data_draw_right_triangle (data, x, y, size, filled);                  
            break;
     case GTK_PLOT_SYMBOL_LEFT_TRIANGLE:
            gtk_plot_data_draw_left_triangle (data, x, y, size, filled);                  
            break;
     case GTK_PLOT_SYMBOL_DIAMOND:
            gtk_plot_data_draw_diamond (data, x, y, size, filled);                  
            break;
     case GTK_PLOT_SYMBOL_PLUS:
            gtk_plot_data_draw_plus (data, x, y, size);                  
            break;
     case GTK_PLOT_SYMBOL_CROSS:
            gtk_plot_data_draw_cross (data, x, y, size);                  
            break;
     case GTK_PLOT_SYMBOL_STAR:
            gtk_plot_data_draw_star (data, x, y, size);                  
            break;
     case GTK_PLOT_SYMBOL_IMPULSE:
            x0 = x;
            y0 = 0.;
            gtk_plot_get_pixel(plot, x0, y0, &px0, &py0);
            gtk_plot_pc_draw_line(plot->pc, 
                          x, MIN(py0,y), 
                          x,
                          MAX(py0,y));
            break;
  }

/*
  gdk_gc_set_clip_rectangle(gc, NULL);
*/
}

static void
gtk_plot_data_draw_xyz (GtkPlotData *dataset, gint npoints) 
{ 
  GtkWidget *widget;
  GtkPlot *plot;
  GdkRectangle clip_area, area;
  gint n;
  gdouble x, y, z = 0.;
  gdouble px, py;
  gdouble x0, y0;

  plot = dataset->plot;
  widget = GTK_WIDGET(plot);
  if(!dataset->x || !dataset->y) return;

  if(dataset->x_line.line_style == GTK_PLOT_LINE_NONE &&
     dataset->y_line.line_style == GTK_PLOT_LINE_NONE &&
     dataset->z_line.line_style == GTK_PLOT_LINE_NONE) return;

  widget = GTK_WIDGET(plot);

  area.x = GTK_WIDGET(plot)->allocation.x;
  area.y = GTK_WIDGET(plot)->allocation.y;
  area.width = GTK_WIDGET(plot)->allocation.width;
  area.height = GTK_WIDGET(plot)->allocation.height;

  clip_area.x = area.x + roundint(plot->x * widget->allocation.width);
  clip_area.y = area.y + roundint(plot->y * widget->allocation.height);
  clip_area.width = roundint(plot->width * widget->allocation.width);
  clip_area.height = roundint(plot->height * widget->allocation.height);

  if(plot->clip_data) gtk_plot_pc_clip(plot->pc, &clip_area);

  for(n=dataset->num_points-npoints; n<=dataset->num_points-1; n++)
    {
      x = dataset->x[n];
      y = dataset->y[n];

      if(x >= plot->xmin && x <= plot->xmax) {

          if(GTK_IS_PLOT3D(plot)){
            gdouble pz, z0;
            if(dataset->z != NULL) z = dataset->z[n];
            if(z >= GTK_PLOT3D(plot)->zmin && z <= GTK_PLOT3D(plot)->zmax){ 
              gtk_plot3d_get_pixel(GTK_PLOT3D(plot), x, y, z, &px, &py, &pz);
              gtk_plot3d_get_pixel(GTK_PLOT3D(plot), 
                                   GTK_PLOT3D(plot)->origin.x, y, z, 
                                   &x0, &y0, &z0);
              gtk_plot_draw_line(plot, dataset->x_line,
                                 px,
                                 py,
                                 x0, 
                                 y0);
              gtk_plot3d_get_pixel(GTK_PLOT3D(plot), 
                                   x, GTK_PLOT3D(plot)->origin.y, z, 
                                   &x0, &y0, &z0);
              gtk_plot_draw_line(plot, dataset->y_line,
                                 px,
                                 py,
                                 x0, 
                                 y0);
              gtk_plot3d_get_pixel(GTK_PLOT3D(plot), 
                                   x, y, GTK_PLOT3D(plot)->origin.z, 
                                   &x0, &y0, &z0);
              gtk_plot_draw_line(plot, dataset->z_line,
                                 px,
                                 py,
                                 x0, 
                                 y0);
            } 
          } else {
            gtk_plot_get_pixel(plot, x, y, &px, &py);
            gtk_plot_get_pixel(plot, x, MAX(0., plot->ymin), &x0, &y0);
            gtk_plot_draw_line(plot, dataset->x_line,
                               px,
                               py,
                               px, 
                               y0);
            gtk_plot_get_pixel(plot, MAX(0., plot->xmin) , y, &x0, &y0);
            gtk_plot_draw_line(plot, dataset->y_line,
                               px, 
                               py, 
                               x0, 
                               py);
          }
      }
    }

  if(plot->clip_data) gtk_plot_pc_clip(plot->pc, NULL);
}

static void
gtk_plot_data_draw_errbars(GtkPlotData *dataset,
                           gdouble x, gdouble y, gdouble z,
                           gdouble dx, gdouble dy, gdouble dz)
{
  GtkWidget *widget;
  GtkPlot *plot;
  GtkPlotPoint errbar[6];
  gdouble px, py;
  gdouble el_x, el_y, er_x, er_y, ed_x, ed_y, eu_x, eu_y;
  gdouble m;

  plot = dataset->plot;
  widget = GTK_WIDGET(plot);
  m = plot->magnification;

  if(!dataset->show_xerrbars && !dataset->show_yerrbars && !dataset->show_zerrbars) return;

  gtk_plot_pc_set_color (plot->pc, &dataset->symbol.color);
  gtk_plot_pc_set_lineattr (plot->pc, 
                            dataset->symbol.border.line_width/2, 0, 0, 0);

  if(GTK_IS_PLOT3D(plot)){
       gdouble pz;
       gtk_plot3d_get_pixel(GTK_PLOT3D(plot), x, y, z, &px, &py, &pz);
       gtk_plot3d_get_pixel(GTK_PLOT3D(plot), x, y, z + dz, &eu_x, &eu_y, &pz);
       gtk_plot3d_get_pixel(GTK_PLOT3D(plot), x, y, z - dz, &ed_x, &ed_y, &pz);
       if(dataset->show_zerrbars)
           {
              errbar[0].x = px - m * dataset->zerrbar_caps/2.; 
              errbar[0].y = eu_y; 
              errbar[1].x = px + m * dataset->zerrbar_caps/2.; 
              errbar[1].y = eu_y; 
              errbar[2].x = px; 
     
              errbar[2].y = eu_y; 
              errbar[3].x = px; 
     
              errbar[3].y = ed_y; 
              errbar[4].x = px - m * dataset->zerrbar_caps/2.; 
              errbar[4].y = ed_y; 
              errbar[5].x = px + m * dataset->zerrbar_caps/2.; 
              errbar[5].y = ed_y; 
              gtk_plot_pc_draw_lines(plot->pc, errbar, 6);
           }
  } else {
       gtk_plot_get_pixel(plot, x, y, &px, &py);
       gtk_plot_get_pixel(plot, x + dx, y, &er_x, &er_y);
       gtk_plot_get_pixel(plot, x - dx, y, &el_x, &el_y);
       gtk_plot_get_pixel(plot, x, y + dy, &eu_x, &eu_y);
       gtk_plot_get_pixel(plot, x, y - dy, &ed_x, &ed_y);

       if(dataset->show_xerrbars)
           {
              errbar[0].x = el_x; 
              errbar[0].y = py - m * dataset->xerrbar_caps/2.; 
              errbar[1].x = el_x; 
              errbar[1].y = py + m * dataset->xerrbar_caps/2.; 
              errbar[2].x = el_x; 
              errbar[2].y = py; 

              errbar[3].x = er_x; 
              errbar[3].y = py; 
     
              errbar[4].x = er_x; 
              errbar[4].y = py - m * dataset->xerrbar_caps/2.; 
              errbar[5].x = er_x; 
              errbar[5].y = py + m * dataset->xerrbar_caps/2.; 
              gtk_plot_pc_draw_lines(plot->pc, errbar, 6);
           }

       if(dataset->show_yerrbars)
           {
              errbar[0].x = px - m * dataset->yerrbar_caps/2.; 
              errbar[0].y = eu_y; 
              errbar[1].x = px + m * dataset->yerrbar_caps/2.; 
              errbar[1].y = eu_y; 
              errbar[2].x = px; 
     
              errbar[2].y = eu_y; 
              errbar[3].x = px; 
     
              errbar[3].y = ed_y; 
              errbar[4].x = px - m * dataset->yerrbar_caps/2.; 
              errbar[4].y = ed_y; 
              errbar[5].x = px + m * dataset->yerrbar_caps/2.; 
              errbar[5].y = ed_y; 
              gtk_plot_pc_draw_lines(plot->pc, errbar, 6);
           }
  }
}
 
static void
gtk_plot_data_draw_down_triangle(GtkPlotData *data,  
                                 gdouble x, gdouble y, gdouble size, gint filled)
{
  GtkPlot *plot;
  GtkPlotPoint point[3];
  gdouble pi = acos(-1.);
  gdouble m;

  plot = data->plot;
  m = plot->magnification;


  point[0].x = x - m*size*cos(pi/6.)/2.;
  point[0].y = y - m*size*sin(pi/6.)/2.;

  point[1].x = x + m*size*cos(pi/6.)/2.;
  point[1].y = y - m*size*sin(pi/6.)/2.;

  point[2].x = x;
  point[2].y = y + size/2.;

  gtk_plot_pc_draw_polygon (plot->pc, filled, point, 3); 
}

static void
gtk_plot_data_draw_up_triangle(GtkPlotData *data,  
                               gdouble x, gdouble y, gdouble size, gint filled)
{
  GtkPlot *plot;
  GtkPlotPoint point[3];
  gdouble pi = acos(-1.);
  gdouble m;

  plot = data->plot;
  m = plot->magnification;

  point[0].x = x - m*size*cos(pi/6.)/2.;
  point[0].y = y + m*size*sin(pi/6.)/2.;

  point[1].x = x + m*size*cos(pi/6.)/2.;
  point[1].y = y + m*size*sin(pi/6.)/2.;

  point[2].x = x;
  point[2].y = y - size/2.;

  gtk_plot_pc_draw_polygon (plot->pc, filled, point, 3); 
}

static void
gtk_plot_data_draw_diamond(GtkPlotData *data,  
                           gdouble x, gdouble y, gdouble size, gint filled)
{
  GtkPlot *plot;
  GtkPlotPoint point[4];
  
  plot = data->plot;
  size = plot->magnification * size;

  point[0].x = x - size/2.;
  point[0].y = y;

  point[1].x = x;
  point[1].y = y - size/2.;

  point[2].x = x + size/2.;
  point[2].y = y;

  point[3].x = x;
  point[3].y = y + size/2.;

  gtk_plot_pc_draw_polygon (plot->pc,
                           filled,
                           point,
                           4); 
}

static void
gtk_plot_data_draw_left_triangle(GtkPlotData *data,  
                                 gdouble x, gdouble y, gdouble size, gint filled)
{
  GtkPlot *plot;
  GtkPlotPoint point[3];
  gdouble pi = acos(-1.);
  gdouble m;

  plot = data->plot;
  m = plot->magnification;

  point[0].x = x + m*size*sin(pi/6.)/2.;
  point[0].y = y - m*size*cos(pi/6.)/2.;

  point[1].x = x + m*size*sin(pi/6.)/2.;
  point[1].y = y + m*size*cos(pi/6.)/2.;

  point[2].x = x - size/2.;
  point[2].y = y;

  gtk_plot_pc_draw_polygon (plot->pc, filled, point, 3); 
}

static void
gtk_plot_data_draw_right_triangle(GtkPlotData *data,  
                                  gdouble x, gdouble y, gdouble size, gint filled)
{
  GtkPlot *plot;
  GtkPlotPoint point[3];
  gdouble pi = acos(-1.);
  gdouble m;

  plot = data->plot;
  m = plot->magnification;

  point[0].x = x - m*(gdouble)size*sin(pi/6.)/2.;
  point[0].y = y - m*(gdouble)size*cos(pi/6.)/2.;

  point[1].x = x - m*(gdouble)size*sin(pi/6.)/2.;
  point[1].y = y + m*(gdouble)size*cos(pi/6.)/2.;

  point[2].x = x + size/2.;
  point[2].y = y;

  gtk_plot_pc_draw_polygon (plot->pc, filled, point, 3); 
}


static void
gtk_plot_data_draw_plus(GtkPlotData *data, gdouble x, gdouble y, gdouble size)
{
  GtkPlot *plot;
  plot = data->plot;
  size = plot->magnification * size;
  gtk_plot_pc_draw_line (plot->pc,
                        x-size/2., y, x+size/2., y);
  
  gtk_plot_pc_draw_line (plot->pc,
                        x, y-size/2., x, y+size/2.);
}

static void
gtk_plot_data_draw_cross(GtkPlotData *data, gdouble x, gdouble y, gdouble size)
{
  GtkPlot *plot;
  plot = data->plot;
  size = plot->magnification * size;
  gtk_plot_pc_draw_line (plot->pc,
                        x-size/2., y-size/2., x+size/2., y+size/2.);
  
  gtk_plot_pc_draw_line (plot->pc,
                        x-size/2., y+size/2., x+size/2., y-size/2.);
}

static void
gtk_plot_data_draw_star(GtkPlotData *data, gdouble x, gdouble y, gdouble size)
{
  GtkPlot *plot;
  gdouble s2;

  plot = data->plot;

  size = plot->magnification * size;
  s2 = size*sqrt(2.)/4.;

  gtk_plot_pc_draw_line (plot->pc,
                        x-size/2., y, x+size/2., y);
  
  gtk_plot_pc_draw_line (plot->pc,
                        x, y-size/2., x, y+size/2.);

  gtk_plot_pc_draw_line (plot->pc,
                        x-s2, y-s2, x+s2, y+s2);
  
  gtk_plot_pc_draw_line (plot->pc,
                        x-s2, y+s2, x+s2, y-s2);
}

static void
gtk_plot_data_connect_points(GtkPlotData *dataset, gint npoints)
{
  GtkPlot *plot;
  GtkWidget *widget;
  GdkRectangle clip_area, area;
  GtkPlotPoint *points;
  GtkPlotData spline;
  GtkPlotPoint *spline_points = NULL;
  gdouble *spline_coef = NULL;
  gdouble x, y;
  gint n, n0;
  gdouble px, py;
  gdouble x1, y1;
  gdouble xmin, xmax;
  gint num_points = dataset->num_points;

  plot = dataset->plot;
  widget = GTK_WIDGET(plot);

  area.x = GTK_WIDGET(plot)->allocation.x;
  area.y = GTK_WIDGET(plot)->allocation.y;
  area.width = GTK_WIDGET(plot)->allocation.width;
  area.height = GTK_WIDGET(plot)->allocation.height;

  clip_area.x = area.x + roundint(plot->x * widget->allocation.width);
  clip_area.y = area.y + roundint(plot->y * widget->allocation.height);
  clip_area.width = roundint(plot->width * widget->allocation.width);
  clip_area.height = roundint(plot->height * widget->allocation.height);

  if(dataset->line.line_style == GTK_PLOT_LINE_NONE) return;
  
  npoints = MIN(npoints, dataset->num_points);
  n0 = dataset->num_points - npoints;
  points = (GtkPlotPoint *)g_malloc(2*(dataset->num_points+1)*sizeof(GtkPlotPoint));

  if(plot->clip_data) gtk_plot_pc_clip(plot->pc, &clip_area);

  gtk_plot_set_line_attributes(plot, dataset->line);
 
  switch(dataset->line_connector){
   case GTK_PLOT_CONNECT_STRAIGHT:
       if(npoints == 1) break;
       num_points = npoints;
       for(n=n0; n<dataset->num_points; n++)
        {
          x = dataset->x[n];
          y = dataset->y[n];
          if(GTK_IS_PLOT3D(plot)){
            gdouble pz;
            gdouble z = 0.;

            if(dataset->z != NULL) z = dataset->z[n];
            gtk_plot3d_get_pixel(GTK_PLOT3D(plot), x, y, z, &px, &py, &pz);
          } else { 
            gtk_plot_get_pixel(plot, x, y, &px, &py);
          }
          points[n-n0].x = px;
          points[n-n0].y = py;
        }
       break;
   case GTK_PLOT_CONNECT_HV_STEP:
       if(GTK_IS_PLOT3D(plot)) break;
       if(dataset->num_points == 1) break;
       num_points=0;
       for(n=0; n < dataset->num_points; n++)
        {
          x = dataset->x[n];
          y = dataset->y[n];
          gtk_plot_get_pixel(plot, x, y, &px, &py);
          points[num_points].x = px;
          points[num_points].y = py;
          num_points++;
          if(n < dataset->num_points-1)
            {
              gtk_plot_get_pixel(plot, 
                                 dataset->x[n+1], 
                                 dataset->y[n+1], 
                                 &px, &py);
              points[num_points].x = px;
              points[num_points].y = points[num_points-1].y;
              num_points++;
            }
        }
       break;
    case GTK_PLOT_CONNECT_VH_STEP:
       if(GTK_IS_PLOT3D(plot)) break;
       if(dataset->num_points == 1) break;
       num_points=0;
       for(n=0; n < dataset->num_points; n++)
        {
          x = dataset->x[n];
          y = dataset->y[n];
          gtk_plot_get_pixel(plot, x, y, &px, &py);
          points[num_points].x = px;
          points[num_points].y = py;
          num_points++;
          if(n < dataset->num_points-1)
            {
              gtk_plot_get_pixel(plot, 
                                 dataset->x[n+1], 
                                 dataset->y[n+1], 
                                 &px, &py);
              points[num_points].x = points[num_points-1].x;
              points[num_points].y = py;
              num_points++;
            }
        }
       break;
     case GTK_PLOT_CONNECT_MIDDLE_STEP:
       if(GTK_IS_PLOT3D(plot)) break;
       if(dataset->num_points == 1) break;
       num_points=1;
       for(n=1; n < dataset->num_points; n++)
        {
          x = dataset->x[n];
          y = dataset->y[n];
          gtk_plot_get_pixel(plot, x, y, &px, &py);
          x = dataset->x[n-1];
          y = dataset->y[n-1];
          gtk_plot_get_pixel(plot, x, y, &x1, &y1);
          points[num_points].x = (px+x1)/2;
          points[num_points].y = y1;
          num_points++;
          points[num_points].x = points[num_points-1].x;
          points[num_points].y = py;
          num_points++;
        }
       x = dataset->x[0];
       y = dataset->y[0];
       gtk_plot_get_pixel(plot, x, y, &px, &py);
       points[0].x = px;
       points[0].y = py;
       x = dataset->x[dataset->num_points-1];
       y = dataset->y[dataset->num_points-1];
       gtk_plot_get_pixel(plot, x, y, &px, &py);
       points[num_points].x = px;
       points[num_points].y = py;
       num_points++;
       break;
     case GTK_PLOT_CONNECT_SPLINE:
       if(GTK_IS_PLOT3D(plot)){
         g_free(points);
         return;
       }
       spline = *dataset;
       spline_points = NULL;
       spline.num_points = 0;
       if(dataset->num_points <= 1) break;
       spline_coef = (gdouble *)g_malloc(dataset->num_points*sizeof(gdouble)); 
       spline_points = (GtkPlotPoint *)g_malloc(sizeof(GtkPlotPoint)); 
       spline_solve(dataset->num_points, dataset->x, dataset->y, spline_coef);
       gtk_plot_get_pixel(plot, dataset->x[0], 0, &xmin, &py);
       gtk_plot_get_pixel(plot, dataset->x[dataset->num_points-1], 0, 
                          &xmax, &py);

       for(x1 = xmin; x1 <= xmax; x1 += spline.x_step) {
         spline.num_points++;
         spline_points = (GtkPlotPoint *)g_realloc(spline_points,
                                 (spline.num_points + 2)*sizeof(GtkPlotPoint));
         gtk_plot_get_point(plot, x1, 0, &x, &y);
         y = spline_eval(dataset->num_points, dataset->x, dataset->y, 
                         spline_coef, x);
         gtk_plot_get_pixel(plot, x, y, &px, &py);
         spline_points[spline.num_points-1].x = px;
         spline_points[spline.num_points-1].y = py;
       }

       if(dataset->fill_area){
         x = dataset->x[dataset->num_points - 1];
         y = 0.0;
         gtk_plot_get_pixel(plot, x, y, &px, &py);
         spline_points[spline.num_points].x = px; 
         spline_points[spline.num_points].y = py; 
         x = dataset->x[0];
         gtk_plot_get_pixel(plot, x, y, &px, &py);
         spline_points[spline.num_points + 1].x = px; 
         spline_points[spline.num_points + 1].y = py; 
         spline.num_points += 2;
         gtk_plot_pc_draw_polygon(plot->pc, TRUE, spline_points, spline.num_points);
       } else {
         gtk_plot_pc_draw_lines(plot->pc, spline_points, spline.num_points);
       }

       g_free(spline_points);
       g_free(spline_coef);
       g_free(points);
       if(plot->clip_data) gtk_plot_pc_clip(plot->pc, NULL);
       return;
     case GTK_PLOT_CONNECT_NONE:
     default:
       if(plot->clip_data) gtk_plot_pc_clip(plot->pc, NULL);
       g_free(points);
       return;
    }

  if(dataset->fill_area){
    x = dataset->x[dataset->num_points-1];
    y = 0.0;
    gtk_plot_get_pixel(plot, x, y, &px, &py);
    points[num_points].x = px; 
    points[num_points].y = py; 
    x = dataset->x[dataset->num_points - npoints];
    gtk_plot_get_pixel(plot, x, y, &px, &py);
    points[num_points + 1].x = px; 
    points[num_points + 1].y = py; 
    num_points += 2;
    gtk_plot_pc_draw_polygon(plot->pc, TRUE, points, num_points);
  } else {
    gtk_plot_pc_draw_lines(plot->pc, points, num_points);
  }

  if(plot->clip_data) gtk_plot_pc_clip(plot->pc, NULL);
  g_free(points);
}


static gint
roundint (gdouble x)
{
 gint sign = 1;

/* if(x <= 0.) sign = -1; 
*/
 return (x+sign*.50999999471);
}


/******************************************
 * gtk_plot_data_set_points
 * gtk_plot_data_get_points
 * gtk_plot_data_set_x
 * gtk_plot_data_set_y
 * gtk_plot_data_set_z
 * gtk_plot_data_set_a
 * gtk_plot_data_set_dx
 * gtk_plot_data_set_dy
 * gtk_plot_data_set_dz
 * gtk_plot_data_set_da
 * gtk_plot_data_get_x
 * gtk_plot_data_get_y
 * gtk_plot_data_get_z
 * gtk_plot_data_get_a
 * gtk_plot_data_get_dx
 * gtk_plot_data_get_dy
 * gtk_plot_data_get_dz
 * gtk_plot_data_get_da
 * gtk_plot_data_set_numpoints
 * gtk_plot_data_get_numpoints
 * gtk_plot_data_set_symbol
 * gtk_plot_data_get_symbol
 * gtk_plot_data_set_connector
 * gtk_plot_data_get_connector
 * gtk_plot_data_set_xyz_attributes
 * gtk_plot_data_show_xerrbars
 * gtk_plot_data_show_yerrbars
 * gtk_plot_data_show_zerrbars
 * gtk_plot_data_hide_xerrbars
 * gtk_plot_data_hide_yerrbars
 * gtk_plot_data_hide_zerrbars
 * gtk_plot_data_set_legend
 * gtk_plot_data_show_legend
 * gtk_plot_data_hide_legend
 * gtk_plot_data_set_name
 * gtk_plot_data_show
 * gtk_plot_data_hide
 ******************************************/

void
gtk_plot_data_draw_points(GtkPlotData *data, gint npoints)
{
  gtk_plot_data_real_draw(data, npoints);
}

void
gtk_plot_data_set_points(GtkPlotData *data, 
                         gdouble *x, gdouble *y,
                         gdouble *dx, gdouble *dy,
                         gint num_points)
{
  data->x = x;
  data->y = y;
  data->dx = dx;
  data->dy = dy;
  data->num_points = num_points;
}

void
gtk_plot_data_get_points(GtkPlotData *data, 
                            gdouble **x, gdouble **y,
                            gdouble **dx, gdouble **dy,
                            gint *num_points)
{
  *x = data->x;
  *y = data->y;
  *dx = data->dx;
  *dy = data->dy;
  *num_points = data->num_points;
}

void
gtk_plot_data_set_x(GtkPlotData *data, 
                    gdouble *x) 
{
  data->x = x;
}


void
gtk_plot_data_set_y(GtkPlotData *data, 
                    gdouble *y) 
{
  data->y = y;
}

void
gtk_plot_data_set_z(GtkPlotData *data, 
                    gdouble *z) 
{
  data->z = z;
}

void
gtk_plot_data_set_a(GtkPlotData *data, 
                    gdouble *a) 
{
  data->a = a;
}


void
gtk_plot_data_set_dx(GtkPlotData *data, 
                     gdouble *dx) 
{
  data->dx = dx;
}

void
gtk_plot_data_set_dy(GtkPlotData *data, 
                     gdouble *dy) 
{
  data->dy = dy;
}


void
gtk_plot_data_set_dz(GtkPlotData *data, 
                     gdouble *dz) 
{
  data->dz = dz;
}

void
gtk_plot_data_set_da(GtkPlotData *data, 
                     gdouble *da) 
{
  data->da = da;
}


void
gtk_plot_data_set_labels(GtkPlotData *data, 
                         gchar **labels) 
{
  data->labels = labels;
}

gdouble *
gtk_plot_data_get_x(GtkPlotData *dataset, gint *num_points)
{
  *num_points = dataset->num_points;
  return(dataset->x);
}

gdouble *
gtk_plot_data_get_y(GtkPlotData *dataset, gint *num_points)
{
  *num_points = dataset->num_points;
  return(dataset->y);
}

gdouble *
gtk_plot_data_get_z(GtkPlotData *dataset, gint *num_points)
{
  *num_points = dataset->num_points;
  return(dataset->z);
}

gdouble *
gtk_plot_data_get_a(GtkPlotData *dataset, gint *num_points)
{
  *num_points = dataset->num_points;
  return(dataset->a);
}

gdouble *
gtk_plot_data_get_dx(GtkPlotData *dataset, gint *num_points)
{
  *num_points = dataset->num_points;
  return(dataset->dx);
}

gdouble *
gtk_plot_data_get_dy(GtkPlotData *dataset, gint *num_points)
{
  *num_points = dataset->num_points;
  return(dataset->dy);
}

gdouble *
gtk_plot_data_get_dz(GtkPlotData *dataset, gint *num_points)
{
  *num_points = dataset->num_points;
  return(dataset->dz);
}

gdouble *
gtk_plot_data_get_da(GtkPlotData *dataset, gint *num_points)
{
  *num_points = dataset->num_points;
  return(dataset->da);
}


gchar **
gtk_plot_data_get_labels(GtkPlotData *dataset, gboolean *show_labels)
{
  *show_labels = dataset->show_labels;
  return(dataset->labels);
}

void
gtk_plot_data_show_labels(GtkPlotData *dataset, gboolean show_labels)
{
  dataset->show_labels = show_labels;
}

void
gtk_plot_data_labels_set_attributes (GtkPlotData *data,	
				     const gchar *font,
                                     gint height,
                                     gint angle,
			             const GdkColor *fg,
			             const GdkColor *bg)
{
  if(!font){
   /* Use previous font */
/*    data->labels_attr.font = g_strdup(DEFAULT_FONT);
    data->labels_attr.height = DEFAULT_FONT_HEIGHT;
*/
  } else {
    if(data->labels_attr.font) g_free(data->labels_attr.font);
    data->labels_attr.font = g_strdup(font);
    data->labels_attr.height = height;
  }

  data->labels_attr.angle = angle;

  if(fg) data->labels_attr.fg = *fg;
  if(bg) data->labels_attr.bg = *bg;

}


void
gtk_plot_data_set_numpoints(GtkPlotData *dataset, gint numpoints)
{
  dataset->num_points = numpoints;
}

gint
gtk_plot_data_get_numpoints(GtkPlotData *dataset)
{
  return(dataset->num_points);
}

void
gtk_plot_data_set_symbol (GtkPlotData *dataset,
		          GtkPlotSymbolType type,
		          GtkPlotSymbolStyle style,
                          gint size, gfloat line_width, 
                          const GdkColor *color, const GdkColor *border_color)
{
  dataset->symbol.symbol_type = type;
  dataset->symbol.symbol_style = style;
  dataset->symbol.size = size;
  dataset->symbol.border.line_width = line_width;
  dataset->symbol.border.color = *border_color;
  dataset->symbol.color = *color;
}

void
gtk_plot_data_get_symbol (GtkPlotData *dataset,
		          GtkPlotSymbolType *type,
		          GtkPlotSymbolStyle *style,
                          gint *size, gfloat *line_width, 
                          GdkColor *color, GdkColor *border_color)
{
  *type = dataset->symbol.symbol_type;
  *style = dataset->symbol.symbol_style;
  *size = dataset->symbol.size;
  *line_width = dataset->symbol.border.line_width;
  *color = dataset->symbol.color;
  *border_color = dataset->symbol.border.color;
}

void
gtk_plot_data_set_line_attributes (GtkPlotData *dataset, 
                                   GtkPlotLineStyle style,
                                   gfloat width,
                                   const GdkColor *color)
{
  dataset->line.line_style = style; 
  dataset->line.line_width = width; 
  dataset->line.color = *color; 
}

void
gtk_plot_data_get_line_attributes (GtkPlotData *dataset, 
                                   GtkPlotLineStyle *style,
                                   gfloat *width,
                                   GdkColor *color)
{
  *style = dataset->line.line_style; 
  *width = dataset->line.line_width; 
  *color = dataset->line.color; 
}


void
gtk_plot_data_set_connector (GtkPlotData *dataset,
		             GtkPlotConnector connector)
{
  dataset->line_connector = connector;
}

gint 
gtk_plot_data_get_connector (GtkPlotData *dataset)
{
  return (dataset->line_connector);
}

void
gtk_plot_data_set_x_attributes (GtkPlotData *dataset, 
                           	GtkPlotLineStyle style,
                            	gfloat width,
                            	const GdkColor *color)
{
  dataset->x_line.line_style = style; 
  dataset->x_line.line_width = width; 
  dataset->x_line.color = *color; 
}

void
gtk_plot_data_set_y_attributes (GtkPlotData *dataset, 
                           	GtkPlotLineStyle style,
                            	gfloat width,
                            	const GdkColor *color)
{
  dataset->y_line.line_style = style; 
  dataset->y_line.line_width = width; 
  dataset->y_line.color = *color; 
}

void
gtk_plot_data_set_z_attributes (GtkPlotData *dataset, 
                           	GtkPlotLineStyle style,
                            	gfloat width,
                            	const GdkColor *color)
{
  dataset->z_line.line_style = style; 
  dataset->z_line.line_width = width; 
  dataset->z_line.color = *color; 
}


void
gtk_plot_data_show_xerrbars(GtkPlotData *dataset) 
{
  dataset->show_xerrbars = TRUE;
}

void
gtk_plot_data_show_yerrbars(GtkPlotData *dataset) 
{
  dataset->show_yerrbars = TRUE;
}

void
gtk_plot_data_show_zerrbars(GtkPlotData *dataset) 
{
  dataset->show_zerrbars = TRUE;
}


void
gtk_plot_data_hide_xerrbars(GtkPlotData *dataset) 
{
  dataset->show_xerrbars = FALSE;
}

void
gtk_plot_data_hide_yerrbars(GtkPlotData *dataset) 
{
  dataset->show_yerrbars = FALSE;
}

void
gtk_plot_data_hide_zerrbars(GtkPlotData *dataset) 
{
  dataset->show_zerrbars = FALSE;
}

void
gtk_plot_data_fill_area(GtkPlotData *dataset, gboolean fill)
{
  dataset->fill_area = fill;
}

gboolean
gtk_plot_data_area_is_filled(GtkPlotData *dataset)
{
  return (dataset->fill_area);
}

void
gtk_plot_data_show_legend(GtkPlotData *dataset)
{
  dataset->show_legend = TRUE;
}

void
gtk_plot_data_hide_legend(GtkPlotData *dataset)
{
  dataset->show_legend = FALSE;
}

void
gtk_plot_data_set_legend(GtkPlotData *dataset,
                            const gchar *legend)
{
  if(legend){
     g_free(dataset->legend);
     dataset->legend = g_strdup(legend);
  }
}

void
gtk_plot_data_set_legend_precision (GtkPlotData *data,
                                     gint precision)
{
  data->legends_precision = precision;
}

gint
gtk_plot_data_get_legend_precision (GtkPlotData *data)
{

  return (data->legends_precision);
}


void
gtk_plot_data_set_name(GtkPlotData *dataset, const gchar *name)
{
  if(dataset->name)
     g_free(dataset->name);

  dataset->name = g_strdup(name);
}

void
gtk_plot_data_set_link(GtkPlotData *dataset, gpointer link)
{
  dataset->link = link; 
}

gpointer
gtk_plot_data_get_link(GtkPlotData *dataset)
{
  return(dataset->link); 
}

void
gtk_plot_data_remove_link(GtkPlotData *dataset)
{
  dataset->link = NULL; 
}

void
gtk_plot_data_set_gradient_mask (GtkPlotData *data, gint mask)
{
  data->gradient_mask = mask;
}

gint
gtk_plot_data_get_gradient_mask (GtkPlotData *data)
{
  return (data->gradient_mask);
}

void
gtk_plot_data_gradient_set_visible (GtkPlotData *data, gboolean visible)
{
  data->show_gradient = visible;
}

gboolean
gtk_plot_data_gradient_visible (GtkPlotData *data)
{
  return(data->show_gradient);
}

void
gtk_plot_data_set_gradient_colors (GtkPlotData *data,
                                   const GdkColor *min,
                                   const GdkColor *max)
{
  data->color_min= *min;
  data->color_max = *max;
}

void
gtk_plot_data_get_gradient_colors (GtkPlotData *data,
                                   GdkColor *min,
                                   GdkColor *max)
{
  min = &data->color_min;
  max = &data->color_max;
}

void
gtk_plot_data_set_gradient (GtkPlotData *data,
                            gdouble min, gdouble max, gint nlevels)
{
  data->gradient.begin = min;
  data->gradient.end = max;
  data->gradient.nmajorticks = nlevels;

  gtk_plot_data_calc_gradient(data);
}

void
gtk_plot_data_get_gradient (GtkPlotData *data,
                            gdouble *min, gdouble *max, gint *nlevels)
{
  *min = data->gradient.begin;
  *max = data->gradient.end;
  *nlevels = data->gradient.nmajorticks;
}

void
gtk_plot_data_get_gradient_level (GtkPlotData *data, gdouble level, GdkColor *color)
{
  GdkColor min, max;
  gdouble red, green, blue;
  gdouble h, s, v;
  gdouble h1, s1, v1;
  gdouble h2, s2, v2;
  gdouble delta, value;

  min = data->color_min; 
  max = data->color_max; 
  delta = data->gradient.end - data->gradient.begin;
  value = fabs(level - data->gradient.begin);

  red = min.red;
  green = min.green;
  blue = min.blue;
  rgb_to_hsv(red, green, blue, &h1, &s1, &v1);
  red = max.red;
  green = max.green;
  blue = max.blue;
  rgb_to_hsv(red, green, blue, &h2, &s2, &v2);

  s = 1.;
  v = 1.;
  h = 1.;
  if(data->gradient_mask & GTK_PLOT_GRADIENT_S)
                    s = s1 + (s2 - s1) * value / delta;
  if(data->gradient_mask & GTK_PLOT_GRADIENT_V)
                    v = v1 + (v2 - v1) * value / delta;
  if(data->gradient_mask & GTK_PLOT_GRADIENT_H)
                    h = h1 + (h2 - h1) * value / delta;

  hsv_to_rgb(h, MIN(s, 1.0), MIN(v, 1.0), &red, &green, &blue);

  color->red = red;
  color->green = green;
  color->blue = blue;
}

static void
hsv_to_rgb (gdouble  h, gdouble  s, gdouble  v,
            gdouble *r, gdouble *g, gdouble *b)
{
  gint i;
  gdouble f, w, q, t;

  if (s == 0.0)
    s = 0.000001;

  if (h == -1.0)
    {
      *r = v;
      *g = v;
      *b = v;
    }
  else
    {
      if (h == 360.0) h = 0.0;
      h = h / 60.0;
      i = (gint) h;
      f = h - i;
      w = v * (1.0 - s);
      q = v * (1.0 - (s * f));
      t = v * (1.0 - (s * (1.0 - f)));

      switch (i)
      {
        case 0:
          *r = v;
          *g = t;
          *b = w;
          break;
        case 1:
          *r = q;
          *g = v;
          *b = w;
          break;
        case 2:
          *r = w;
          *g = v;
          *b = t;
          break;
        case 3:
          *r = w;
          *g = q;
          *b = v;
          break;
        case 4:
          *r = t;
          *g = w;
          *b = v;
          break;
        case 5:
          *r = v;
          *g = w;
          *b = q;
          break;
      }
    }

  *r *= 65535.;
  *g *= 65535.;
  *b *= 65535.;
}


static void
rgb_to_hsv (gdouble  r, gdouble  g, gdouble  b,
            gdouble *h, gdouble *s, gdouble *v)
{
  double max, min, delta;

  r /= 65535.;
  g /= 65535.;
  b /= 65535.;

  max = r;
  if (g > max)
    max = g;
  if (b > max)
    max = b;

  min = r;
  if (g < min)
    min = g;
  if (b < min)
    min = b;

  *v = max;
  if (max != 0.0)
    *s = (max - min) / max;
  else
    *s = 0.0;

  if (*s == 0.0)
    *h = -1.0;
  else
    {
      delta = max - min;

      if (r == max)
        *h = (g - b) / delta;
      else if (g == max)
        *h = 2.0 + (b - r) / delta;
      else if (b == max)
        *h = 4.0 + (r - g) / delta;

      *h = *h * 60.0;

      if (*h < 0.0)
        *h = *h + 360;
    }
}

/* Solve the tridiagonal equation system that determines the second
   derivatives for the interpolation points.  (Based on Numerical
   Recipes 2nd Edition.) */
static void
spline_solve (int n, gdouble x[], gdouble y[], gdouble y2[])
{
  gdouble p, sig, *u;
  gint i, k;

  if(n == 1) return;

  u = g_malloc ((n - 1) * sizeof (u[0]));

  y2[0] = u[0] = 0.0;	/* set lower boundary condition to "natural" */

  for (i = 1; i < n - 1; ++i)
    {
      sig = (x[i] - x[i - 1]) / (x[i + 1] - x[i - 1]);
      p = sig * y2[i - 1] + 2.0;
      y2[i] = (sig - 1.0) / p;
      u[i] = ((y[i + 1] - y[i])
	      / (x[i + 1] - x[i]) - (y[i] - y[i - 1]) / (x[i] - x[i - 1]));
      u[i] = (6.0 * u[i] / (x[i + 1] - x[i - 1]) - sig * u[i - 1]) / p;
    }

  y2[n - 1] = 0.0;
  for (k = n - 2; k >= 0; --k)
    y2[k] = y2[k] * y2[k + 1] + u[k];

  g_free (u);
}

static gdouble
spline_eval (int n, gdouble x[], gdouble y[], gdouble y2[], gdouble val)
{
  gint k_lo, k_hi, k;
  gdouble h, b, a;

  if(n == 1) return y[0];

  /* do a binary search for the right interval: */
  k_lo = 0; k_hi = n - 1;
  while (k_hi - k_lo > 1)
    {
      k = (k_hi + k_lo) / 2;
      if (x[k] > val)
	k_hi = k;
      else
	k_lo = k;
    }

  h = x[k_hi] - x[k_lo];
  g_assert (h > 0.0);

  a = (x[k_hi] - val) / h;
  b = (val - x[k_lo]) / h;
  return a*y[k_lo] + b*y[k_hi] +
    ((a*a*a - a)*y2[k_lo] + (b*b*b - b)*y2[k_hi]) * (h*h)/6.0;
}

static void
gtk_plot_data_calc_gradient(GtkPlotData *data)
{
  GtkPlotTicks *ticks;
  gdouble min = 0., max = 0.;
  gdouble absmin = 0., absmax = 0.;
  gdouble tick;
  gdouble major_step;
  gint nmajor;

  ticks = &data->gradient;

  max = ticks->end;
  min = ticks->begin;
  absmin = min;
  absmax = max;

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
  ticks->step = major_step = (absmax - absmin) / (ticks->nmajorticks);

  ticks->major_values = (gdouble *)g_malloc((ticks->nmajorticks + 1) * sizeof(gdouble));

  if(ticks->step > 0.){
   tick = min - major_step;
   while(tick <= absmax + 2*fabs(major_step)){
     if(tick >= min-1.E-10 && tick <= absmax+1.E-10){
        nmajor ++;
        ticks->major_values[nmajor-1] = tick;
     }
     tick += major_step;
   }
  }

}

