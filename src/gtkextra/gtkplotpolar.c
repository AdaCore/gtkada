/* gtkplotpolar - polar plots widget for gtk+
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
#include "gtkpsfont.h"
#include "gtkplotpc.h"

#ifndef PIP
#define PI 3.14159265358979323846
#endif

static void gtk_plot_polar_class_init 		(GtkPlotPolarClass *klass);
static void gtk_plot_polar_init 		(GtkPlotPolar *plot);
static void gtk_plot_polar_draw 		(GtkWidget *widget, 
						 GdkRectangle *area);
static void gtk_plot_polar_real_paint 		(GtkWidget *widget);
static void gtk_plot_calc_ticks			(GtkPlotPolar *plot, 
						 GtkPlotAxis *axis);
static void gtk_plot_polar_draw_grids           (GtkPlotPolar *plot); 
static void gtk_plot_polar_draw_axis		(GtkPlotPolar *plot, 
					 	 GtkPlotAxis *axis,
                                                 GtkPlotVector tick_direction);
static void gtk_plot_polar_draw_circle		(GtkPlotPolar *polar);
static void gtk_plot_polar_draw_labels		(GtkPlotPolar *plot, 
						 GtkPlotAxis *axis, 
                                                 GtkPlotVector tick_direction );
static void gtk_plot_polar_real_get_pixel	(GtkWidget *widget, 
                          			 gdouble x, 
						 gdouble y, 
                          			 gdouble *px, 
						 gdouble *py); 
static void gtk_plot_polar_real_get_point	(GtkWidget *widget, 
						 gint px,
						 gint py,
                          			 gdouble *x, 
						 gdouble *y); 
static gint roundint				(gdouble x);
static void parse_label			        (gdouble val, 
						 gint precision, 
						 gint style,
                                                 gchar *label);
static gdouble transform			(GtkPlot *plot, gdouble x);

static GtkPlotClass *parent_class = NULL;


GtkType
gtk_plot_polar_get_type (void)
{
  static GtkType plot_type = 0;

  if (!plot_type)
    {
      GtkTypeInfo plot_info =
      {
	"GtkPlotPolar",
	sizeof (GtkPlotPolar),
	sizeof (GtkPlotPolarClass),
	(GtkClassInitFunc) gtk_plot_polar_class_init,
	(GtkObjectInitFunc) gtk_plot_polar_init,
	/* reserved 1*/ NULL,
        /* reserved 2 */ NULL,
        (GtkClassInitFunc) NULL,
      };

      plot_type = gtk_type_unique (gtk_plot_get_type(), &plot_info);
    }
  return plot_type;
}

static void
gtk_plot_polar_class_init (GtkPlotPolarClass *klass)
{
  GtkObjectClass *object_class;
  GtkWidgetClass *widget_class;
  GtkPlotClass *plot_class;

  parent_class = gtk_type_class (gtk_plot_polar_get_type ());

  object_class = (GtkObjectClass *) klass;
  widget_class = (GtkWidgetClass *) klass;
  plot_class = (GtkPlotClass *) klass;

  widget_class->draw = gtk_plot_polar_draw;

  plot_class->plot_paint = gtk_plot_polar_real_paint;
  plot_class->get_point = gtk_plot_polar_real_get_point;
  plot_class->get_pixel = gtk_plot_polar_real_get_pixel;
}


static void
gtk_plot_polar_init (GtkPlotPolar *plot)
{
  GTK_PLOT(plot)->legends_attr.transparent = FALSE;

  GTK_PLOT(plot)->xmin = 0;
  GTK_PLOT(plot)->xmax = 360;

  plot->r = GTK_PLOT(plot)->left;
  plot->angle = GTK_PLOT(plot)->bottom;

  GTK_PLOT(plot)->bottom->min = 0.0;
  GTK_PLOT(plot)->bottom->max = 360.0;
  GTK_PLOT(plot)->bottom->ticks.step = 30.;
  GTK_PLOT(plot)->top->min = 0.0;
  GTK_PLOT(plot)->top->max = 360.0;
  GTK_PLOT(plot)->top->ticks.step = 30.;

  GTK_PLOT(plot)->bottom->labels_offset = 15.;
  GTK_PLOT(plot)->bottom->label_precision = 0;

  GTK_PLOT(plot)->ymin = 0.2;
  GTK_PLOT(plot)->left->min = 0.2;
  GTK_PLOT(plot)->left->ticks.step = 0.2;
  GTK_PLOT(plot)->right->min = 0.2;
  GTK_PLOT(plot)->right->ticks.step = 0.2;

  GTK_PLOT(plot)->left->title.angle = 0.;

  gtk_plot_axis_set_title(GTK_PLOT(plot), GTK_PLOT_AXIS_LEFT, "R");

  GTK_PLOT(plot)->top->is_visible = FALSE;

  GTK_PLOT(plot)->bottom->show_major_grid = TRUE;
  GTK_PLOT(plot)->bottom->show_minor_grid = TRUE;
  GTK_PLOT(plot)->left->show_major_grid = TRUE;
  GTK_PLOT(plot)->left->show_minor_grid = TRUE;

  plot->rotation = 0.0;
}

static void
gtk_plot_polar_draw (GtkWidget *widget, GdkRectangle *area)
{
  gtk_plot_paint(GTK_PLOT(widget));
  gtk_plot_refresh (GTK_PLOT(widget), area);
}

static void
gtk_plot_polar_real_paint (GtkWidget *widget)
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
  gdouble min;

  if(!GTK_WIDGET_REALIZED(widget)) return;

  plot = GTK_PLOT(widget);

  area.x = widget->allocation.x;
  area.y = widget->allocation.y;
  area.width = widget->allocation.width;
  area.height = widget->allocation.height;
  xoffset = area.x + roundint(plot->x * widget->allocation.width);
  yoffset = area.y + roundint(plot->y * widget->allocation.height);
  width = roundint(plot->width * widget->allocation.width);
  height = roundint(plot->height * widget->allocation.height);

  style = gtk_widget_get_style(widget);

  pixmap = plot->drawable;

  gtk_plot_pc_gsave(plot->pc);
  gtk_plot_pc_set_color(plot->pc, &plot->background);

  if(!GTK_PLOT_TRANSPARENT(plot))
    gtk_plot_pc_draw_rectangle (plot->pc, TRUE,
                               xoffset, yoffset,
                               width , height);

  /* draw frame to guide the eyes*/
/*  gdk_draw_rectangle (pixmap, gc, FALSE,
                      xoffset, yoffset,
                      width , height);
*/

  /* draw the ticks & grid lines */

  min = plot->left->min;
  plot->left->min = 0.0;
  gtk_plot_calc_ticks(GTK_PLOT_POLAR(plot), plot->left);
  gtk_plot_calc_ticks(GTK_PLOT_POLAR(plot), plot->bottom);

  if(plot->left->is_visible)
    {
      GtkPlotVector tick_direction;

      tick_direction.x = 1.;
      tick_direction.y = 0.;
      plot->left->origin.x = (gfloat)width*plot->left_align;
      plot->left->origin.y = height;
      gtk_plot_polar_draw_axis(GTK_PLOT_POLAR(plot), plot->left, tick_direction);
      gtk_plot_polar_draw_labels(GTK_PLOT_POLAR(plot), plot->left, tick_direction);
    }


  if(plot->top->is_visible)
    {
      GtkPlotVector tick_direction;

      tick_direction.x = 0.;
      tick_direction.y = 1.;
      plot->left->direction.x = 1;
      plot->left->direction.y = 0;
      plot->left->origin.x = 0;
      plot->left->origin.y = (gfloat)height*plot->left_align;
      gtk_plot_polar_draw_axis(GTK_PLOT_POLAR(plot), plot->left, tick_direction);
      gtk_plot_polar_draw_labels(GTK_PLOT_POLAR(plot), plot->left, tick_direction);
      plot->left->direction.x = 0;
      plot->left->direction.y = -1;
    }

  if(plot->bottom->is_visible)
    {
      gtk_plot_polar_draw_circle(GTK_PLOT_POLAR(plot));
    }

  plot->left->min = min;
  gtk_plot_calc_ticks(GTK_PLOT_POLAR(plot), plot->left);
  gtk_plot_polar_draw_grids(GTK_PLOT_POLAR(plot));

  dataset = plot->data_sets;
  while(dataset)
   {
     GTK_PLOT_DATA_CLASS(GTK_OBJECT(dataset->data)->klass)->draw_data(GTK_PLOT_DATA(dataset->data));
     dataset = dataset->next;
   }

  text = plot->text;
  while(text)
   {
     child_text = (GtkPlotText *) text->data;
     gtk_plot_draw_text(plot, *child_text);
     text = text->next;
   }

  GTK_PLOT_CLASS(GTK_OBJECT(plot)->klass)->draw_legends(widget);;

  gtk_plot_pc_grestore(plot->pc);
}


GtkWidget*
gtk_plot_polar_new (GdkDrawable *drawable)
{
  GtkPlotPolar *plot;

  plot = gtk_type_new (gtk_plot_polar_get_type ());

  gtk_plot_polar_construct(GTK_PLOT_POLAR(plot), drawable);

  return GTK_WIDGET (plot);
}

GtkWidget*
gtk_plot_polar_new_with_size (GdkDrawable *drawable, gdouble width, gdouble height)
{
  GtkWidget *plot; 

  plot = gtk_type_new (gtk_plot_polar_get_type ());

  gtk_plot_polar_construct_with_size(GTK_PLOT_POLAR(plot), drawable,
				     width, height);

  return(plot);
}

void
gtk_plot_polar_construct(GtkPlotPolar *plot, GdkDrawable *drawable)
{
  GTK_PLOT(plot)->drawable = drawable;
}

void
gtk_plot_polar_construct_with_size(GtkPlotPolar *plot, GdkDrawable *drawable, gdouble width, gdouble height)
{
  GTK_PLOT(plot)->drawable = drawable;
  gtk_plot_resize (GTK_PLOT(plot), width, height);
}

static void
gtk_plot_polar_draw_grids(GtkPlotPolar *polar)
{
  GtkWidget *widget;
  GtkPlot *plot;
  gdouble ix, iy;
  gdouble x1, y1, x2, y2;
  gdouble width, height, size;
  gdouble xp, yp;
  gint ntick;
  gdouble ox, oy;
  gdouble rotation;

  widget = GTK_WIDGET(polar);
  plot = GTK_PLOT(polar);

  rotation = polar->rotation;

  xp = widget->allocation.x + plot->x * widget->allocation.width;
  yp = widget->allocation.y + plot->y * widget->allocation.height;
  width = plot->width * widget->allocation.width;
  height = plot->height * widget->allocation.height;

  ox = xp + width / 2.;
  oy = yp + height / 2.;
  size = MIN(width, height) / 2.;

  if(plot->bottom->show_minor_grid)
    {
          for(ntick = 0; ntick < plot->bottom->ticks.nminorticks; ntick++){
            if(plot->bottom->ticks.minor_values[ntick] >= plot->bottom->min){
              gtk_plot_get_pixel(plot,
                                 plot->ymax,
                                 plot->bottom->ticks.minor_values[ntick],
                                 &x1, &y1);
              gtk_plot_get_pixel(plot,
                                 plot->ymin,
                                 plot->bottom->ticks.minor_values[ntick],
                                 &x2, &y2);
              gtk_plot_draw_line(plot, plot->bottom->minor_grid,
                                 x1, y1, x2, y2);
            }
          }
    }
  if(plot->bottom->show_major_grid)
    {
          for(ntick = 0; ntick < plot->bottom->ticks.nmajorticks; ntick++){
           if(plot->bottom->ticks.major_values[ntick] >= plot->bottom->min){
              gtk_plot_get_pixel(plot,
                                 plot->ymax,
                                 plot->bottom->ticks.major_values[ntick],
                                 &x1, &y1);
              gtk_plot_get_pixel(plot,
                                 plot->ymin,
                                 plot->bottom->ticks.major_values[ntick],
                                 &x2, &y2);
              gtk_plot_draw_line(plot, plot->bottom->major_grid,
                                 x1, y1, x2, y2);
           }
          }
    }
  if(plot->left->show_minor_grid)
    {
          gtk_plot_set_line_attributes(plot, plot->left->minor_grid); 
          for(ntick = 0; ntick < plot->left->ticks.nminorticks; ntick++){
            if(plot->left->ticks.minor_values[ntick] >= plot->left->min){
              gtk_plot_get_pixel(plot,
                                 plot->left->ticks.minor_values[ntick],
                                 90.0 - rotation,
                                 &ix, &iy);
              iy = fabs(oy - iy);
              gtk_plot_pc_draw_circle (plot->pc,
                                      FALSE,
                                      ox, oy,
                                      2 * iy);
            }
          }
    }
  if(plot->left->show_major_grid)

    {
          gtk_plot_set_line_attributes(plot, plot->left->major_grid); 
          for(ntick = 0; ntick < plot->left->ticks.nmajorticks; ntick++){
            if(plot->left->ticks.major_values[ntick] >= plot->left->min){
              gtk_plot_get_pixel(plot,
                                 plot->left->ticks.major_values[ntick],
                                 90.0 - rotation,
                                 &ix, &iy);
              iy = fabs(oy - iy);
              gtk_plot_pc_draw_circle (plot->pc,
                                      FALSE,
                                      ox, oy,
                                      2 * iy);
            }
          }
    }

}

static void
gtk_plot_polar_draw_axis(GtkPlotPolar *polar, 
                         GtkPlotAxis *axis, GtkPlotVector tick_direction)
{
  GtkWidget *widget;
  GtkPlot *plot;
  gdouble x, y;
  gdouble xx;
  gint line_width;
  gdouble xp, yp, width, height, size;
  gint ntick;
  gdouble m;
  gdouble x1, y1;
  gdouble ox, oy;

  widget = GTK_WIDGET(polar);
  plot = GTK_PLOT(polar);

  m = plot->magnification;

  xp = widget->allocation.x + plot->x * widget->allocation.width;
  yp = widget->allocation.y + plot->y * widget->allocation.height;
  width = plot->width * widget->allocation.width;
  height = plot->height * widget->allocation.height;

  size = MIN(width, height);

  ox = width / 2.;
  oy = height / 2.;

  x = xp + ox * axis->direction.x + axis->origin.x;
  y = yp + oy * axis->direction.y + axis->origin.y;

  line_width = axis->line.line_width;
  gtk_plot_pc_set_color(plot->pc, &axis->line.color);

  gtk_plot_pc_set_lineattr(plot->pc, axis->line.line_width, 0, 3, 0);
  gtk_plot_pc_draw_line(plot->pc,
                x - size / 2.0 * axis->direction.x,
                y - size / 2.0 * axis->direction.y,
                x + axis->direction.x * size / 2.0,
                y + axis->direction.y * size / 2.0);
  gtk_plot_pc_set_lineattr(plot->pc, axis->ticks_width, 0, 1, 0);

  for(ntick = 0; ntick < axis->ticks.nmajorticks; ntick++){
    xx = axis->ticks.major[ntick];
    if(axis->ticks.major_values[ntick] >= axis->min){
      if(axis->major_mask & GTK_PLOT_TICKS_IN){
         x1 = x + xx * axis->direction.x;
         y1 = y + xx * axis->direction.y;
         gtk_plot_pc_draw_line(plot->pc,
                       x1,
                       y1,
                       x1 + tick_direction.x * m * axis->ticks_length,
                       y1 + tick_direction.y * m * axis->ticks_length);
         x1 = x - xx * axis->direction.x;
         y1 = y - xx * axis->direction.y;
         gtk_plot_pc_draw_line(plot->pc,
                       x1,
                       y1,
                       x1 + tick_direction.x * m * axis->ticks_length,
                       y1 + tick_direction.y * m * axis->ticks_length);
      }
      if(axis->major_mask & GTK_PLOT_TICKS_OUT){
         x1 = x + xx * axis->direction.x;
         y1 = y + xx * axis->direction.y;
         gtk_plot_pc_draw_line(plot->pc,
                       x1,
                       y1,
                       x1 - tick_direction.x * m * axis->ticks_length,
                       y1 - tick_direction.y * m * axis->ticks_length);
         x1 = x - xx * axis->direction.x;
         y1 = y - xx * axis->direction.y;
         gtk_plot_pc_draw_line(plot->pc,
                       x1,
                       y1,
                       x1 - tick_direction.x * m * axis->ticks_length,
                       y1 - tick_direction.y * m * axis->ticks_length);
      }
    }
  }    
  for(ntick = 0; ntick < axis->ticks.nminorticks; ntick++){
    xx = axis->ticks.minor[ntick];
    if(axis->ticks.minor_values[ntick] >= axis->min){
      if(axis->minor_mask & GTK_PLOT_TICKS_IN){
         x1 = x + xx * axis->direction.x;
         y1 = y + xx * axis->direction.y;
         gtk_plot_pc_draw_line(plot->pc,
                       x1,
                       y1,
                       x1 + tick_direction.x * m * axis->ticks_length/2.,
                       y1 + tick_direction.y * m * axis->ticks_length/2.);
         x1 = x - xx * axis->direction.x;
         y1 = y - xx * axis->direction.y;
         gtk_plot_pc_draw_line(plot->pc,
                       x1,
                       y1,
                       x1 + tick_direction.x * m * axis->ticks_length/2.,
                       y1 + tick_direction.y * m * axis->ticks_length/2.);
      }
      if(axis->minor_mask & GTK_PLOT_TICKS_OUT){
         x1 = x + xx * axis->direction.x;
         y1 = y + xx * axis->direction.y;
         gtk_plot_pc_draw_line(plot->pc,
                       x1,
                       y1,
                       x1 - tick_direction.x * m * axis->ticks_length/2.,
                       y1 - tick_direction.y * m * axis->ticks_length/2.);
         x1 = x - xx * axis->direction.x;
         y1 = y - xx * axis->direction.y;
         gtk_plot_pc_draw_line(plot->pc,
                       x1,
                       y1,
                       x1 - tick_direction.x * m * axis->ticks_length/2.,
                       y1 - tick_direction.y * m * axis->ticks_length/2.);
      }
    }
  }    

}


static void
gtk_plot_polar_draw_labels(GtkPlotPolar *polar, 
                           GtkPlotAxis *axis,
                           GtkPlotVector tick_direction)
{
  GtkWidget *widget;
  GtkPlot *plot;
  GdkFont *font;
  GtkPlotText title, tick;
  gchar label[100];
  gdouble x_tick;
  gint x, y;
  gint xx, yy;
  gint ox, oy;
  gint text_height;
  gint xp, yp, width, height;
  gint ntick;
  gdouble m;
  gboolean veto = FALSE;

  widget = GTK_WIDGET(polar);
  plot = GTK_PLOT(polar);

  m = plot->magnification;

  xp = roundint(plot->x * widget->allocation.width);
  yp = roundint(plot->y * widget->allocation.height);
  width = roundint(plot->width * widget->allocation.width);
  height = roundint(plot->height * widget->allocation.height);

  ox = width / 2.;
  oy = height / 2.;

  x = xp + ox * axis->direction.x + axis->origin.x;
  y = yp + oy * axis->direction.y + axis->origin.y;

  gtk_plot_pc_set_color(plot->pc, &axis->labels_attr.fg);

  font = gtk_psfont_get_gdkfont(axis->labels_attr.font,
                                roundint(axis->labels_attr.height * m));
  text_height = font->ascent + font->descent;

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
    xx = axis->direction.x * axis->ticks.major[ntick];
    yy = axis->direction.y * axis->ticks.major[ntick];
    x_tick = axis->ticks.major_values[ntick];
    if(x_tick >= axis->min-1.e-9){
      if(!axis->custom_labels){
        parse_label(x_tick, axis->label_precision, axis->label_style, label);
      }
      else
      {
        gtk_signal_emit_by_name(GTK_OBJECT(axis), "tick_label", 
                                &x_tick, label, &veto);
        if(!veto)
          parse_label(x_tick, axis->label_precision, axis->label_style, label);
      }
      tick.text = label;

      if(axis->label_mask & GTK_PLOT_LABEL_IN){
         tick.x = x + xx;
         tick.y = y + yy;
         tick.x = tick.x + tick_direction.x*roundint(axis->labels_offset * m);
         tick.y = tick.y + tick_direction.y*roundint(axis->labels_offset * m);
         tick.x = (gdouble)tick.x / (gdouble)widget->allocation.width;
         tick.y = (gdouble)tick.y / (gdouble)widget->allocation.height;
         gtk_plot_draw_text(plot, tick);
         tick.x = x - xx;
         tick.y = y - yy;
         tick.x = tick.x + tick_direction.x*roundint(axis->labels_offset * m);
         tick.y = tick.y + tick_direction.y*roundint(axis->labels_offset * m);
         tick.x = (gdouble)tick.x / (gdouble)widget->allocation.width;
         tick.y = (gdouble)tick.y / (gdouble)widget->allocation.height;
         gtk_plot_draw_text(plot, tick);
      }
      if(axis->label_mask & GTK_PLOT_LABEL_OUT){
         tick.x = x + xx;
         tick.y = y + yy;
         tick.x = tick.x - tick_direction.x*roundint(axis->labels_offset * m);
         tick.y = tick.y - tick_direction.y*roundint(axis->labels_offset * m);
         tick.x = (gdouble)tick.x / (gdouble)widget->allocation.width;
         tick.y = (gdouble)tick.y / (gdouble)widget->allocation.height;
         gtk_plot_draw_text(plot, tick);
         tick.x = x - xx;
         tick.y = y - yy;
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

static void
gtk_plot_polar_draw_circle(GtkPlotPolar *polar)
{
  GtkWidget *widget;
  GtkPlot *plot;
  GtkPlotAxis *axis, perp;
  GtkPlotText tick;
  GdkFont *font;
  gchar label[100];
  gdouble x, y;
  gint line_width;
  gdouble xp, yp, width, height, size;
  gint ntick;
  gdouble m;
  gdouble x_tick = 0.;
  gdouble x1, y1;
  gdouble ox, oy;
  gint text_height;
  gdouble rotation;
  gboolean veto = FALSE;

  widget = GTK_WIDGET(polar);
  plot = GTK_PLOT(polar);

  m = plot->magnification;
  rotation = polar->rotation;

  xp = widget->allocation.x + plot->x * widget->allocation.width;
  yp = widget->allocation.y + plot->y * widget->allocation.height;
  width = plot->width * widget->allocation.width;
  height = plot->height * widget->allocation.height;

  ox = width / 2.;
  oy = height / 2.;

  x = xp + ox;
  y = yp + oy;

  axis = plot->bottom;

  line_width = axis->line.line_width;
  gtk_plot_pc_set_color(plot->pc, &axis->line.color);

  gtk_plot_pc_set_lineattr(plot->pc, axis->line.line_width, 0, 3, 0);

  gtk_plot_get_pixel(plot,
                     plot->ymax,
                     90.0 - rotation,
                     &x1, &size);
  size = fabs(size - y);
  gtk_plot_pc_draw_circle (plot->pc,
                          FALSE,
                          x, y,
                          2 * size);

  gtk_plot_pc_set_lineattr(plot->pc, axis->ticks_width, 0, 1, 0);

  font = gtk_psfont_get_gdkfont(axis->labels_attr.font,
                                roundint(axis->labels_attr.height * m));
  text_height = font->ascent + font->descent;

  for(ntick = 0; ntick < axis->ticks.nmajorticks; ntick++){
    if(axis->ticks.major_values[ntick] >= axis->min){
      x_tick = axis->ticks.major_values[ntick];

      if(!axis->custom_labels){
        parse_label(x_tick, axis->label_precision, axis->label_style, label);
      }
      else
      {
        gtk_signal_emit_by_name(GTK_OBJECT(axis), "tick_label", 
                                &x_tick, label, &veto);
        if(!veto)
          parse_label(x_tick, axis->label_precision, axis->label_style, label);
      }

      gtk_plot_get_pixel(plot,
                         plot->ymax,
                         x_tick,
                         &x1, &y1);

      x_tick += rotation;

      if(axis->major_mask & GTK_PLOT_TICKS_IN){
         perp.direction.x = cos(x_tick * PI / 180.);
         perp.direction.y = -sin(x_tick * PI / 180.);
         gtk_plot_pc_draw_line(plot->pc,
                       x1,
                       y1,
                       x1 + perp.direction.x*m*axis->ticks_length,
                       y1 + perp.direction.y*m*axis->ticks_length);
      }
      if(axis->major_mask & GTK_PLOT_TICKS_OUT){
         perp.direction.x = -cos(x_tick * PI / 180.);
         perp.direction.y = sin(x_tick * PI / 180.);
         gtk_plot_pc_draw_line(plot->pc,
                       x1,
                       y1,
                       x1 - perp.direction.x*m*axis->ticks_length,
                       y1 - perp.direction.y*m*axis->ticks_length);
      }

      tick = axis->labels_attr;
      tick.text = label;

      if((x_tick >= 0.0 && x_tick < 90.0) || (x_tick > 270.0 && x_tick <= 360.0)) 
                                  tick.justification = GTK_JUSTIFY_LEFT;
      if(x_tick > 90.0 && x_tick < 270.0) 
                                  tick.justification = GTK_JUSTIFY_RIGHT;
      if(x_tick == 90.0 || x_tick == 270.0) 
                                  tick.justification = GTK_JUSTIFY_CENTER;
      y1 += text_height / 2;

      if((x_tick - rotation) != 360.0 && axis->label_mask != 0){
         perp.direction.x = -cos(x_tick * PI / 180.);
         perp.direction.y = sin(x_tick * PI / 180.);
         tick.x = x1;
         tick.y = y1;
         tick.x -= roundint(perp.direction.x*axis->labels_offset * m);
         tick.y -= roundint(perp.direction.y*axis->labels_offset * m);
         tick.x = (gdouble)tick.x / (gdouble)widget->allocation.width;
         tick.y = (gdouble)tick.y / (gdouble)widget->allocation.height;
         gtk_plot_draw_text(plot, tick);
      }

    }
  }    

  for(ntick = 0; ntick < axis->ticks.nminorticks; ntick++){
    if(axis->ticks.minor_values[ntick] >= axis->min){
      x_tick = axis->ticks.minor_values[ntick];

      gtk_plot_get_pixel(plot,
                         plot->ymax,
                         x_tick,
                         &x1, &y1);

      x_tick += rotation;

      if(axis->minor_mask & GTK_PLOT_TICKS_IN){
         perp.direction.x = cos(x_tick * PI / 180.);
         perp.direction.y = -sin(x_tick * PI / 180.);
         gtk_plot_pc_draw_line(plot->pc,
                       x1,
                       y1,
                       x1 + perp.direction.x*m*axis->ticks_length/2.,
                       y1 + perp.direction.y*m*axis->ticks_length/2.);
      }
      if(axis->minor_mask & GTK_PLOT_TICKS_OUT){
         perp.direction.x = -cos(x_tick * PI / 180.);
         perp.direction.y = sin(x_tick * PI / 180.);
         gtk_plot_pc_draw_line(plot->pc,
                       x1,
                       y1,
                       x1 - perp.direction.x*m*axis->ticks_length/2.,
                       y1 - perp.direction.y*m*axis->ticks_length/2.);
      }
    }
  }    

  gdk_font_unref(font);
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
  if(abs(v) < 1.0 && v != 0.0){
     v *= 10.0;
     power -= 1;
  }
  if(abs(v) >= 10.0){
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


static void
gtk_plot_polar_real_get_pixel(GtkWidget *widget, 
                              gdouble x, gdouble y,
                              gdouble *px, gdouble *py)
{
    GtkPlot *plot;
    GtkPlotPolar *polar;
    gdouble xp, yp, width, height;
    gdouble ox, oy;
    gdouble rx, ry;

    plot = GTK_PLOT(widget);
    polar = GTK_PLOT_POLAR(widget);

    xp = plot->x * widget->allocation.width;
    yp = plot->y * widget->allocation.height;
    width = plot->width * widget->allocation.width;
    height = plot->height * widget->allocation.height;

    ox = xp + width / 2.0;
    oy = yp + height / 2.0;

    rx = x * cos((y + polar->rotation) / 180. * PI); 
    ry = x * sin((y + polar->rotation)/ 180. * PI); 

    *px = transform(plot, rx);
    *py = transform(plot, ry);
   
    *px += ox; 
    *py = oy - *py; 
}

static void
gtk_plot_polar_real_get_point(GtkWidget *widget, 
                              gint px, gint py,
                              gdouble *x, gdouble *y)
{
    GtkPlot *plot;
    gint xp, yp, width, height;
    gdouble r = 0;
    gdouble angle = 0;
    gint ox, oy, size;
    gdouble rotation;

    plot = GTK_PLOT(widget);
    xp = roundint(plot->x * widget->allocation.width);
    yp = roundint(plot->y * widget->allocation.height);
    width = roundint(plot->width * widget->allocation.width);
    height = roundint(plot->height * widget->allocation.height);

    rotation = GTK_PLOT_POLAR(widget)->rotation;

    ox = xp + width / 2;
    oy = yp + height / 2;

    size = MIN(width, height);

    px = px - ox;
    py = oy - py;

    if(px == 0){
      if(py >= 0) angle = 90.0 - rotation; 
      if(py < 0) angle = 270.0 - rotation; 
    }
    else {
      angle = (gdouble) abs(py) / (gdouble) abs(px);
      angle = atan(angle);
      angle = angle * 180.0 / PI;
      if(px >= 0 && py < 0) angle = 360.0 - angle; 
      if(px < 0 && py >= 0) angle = 180.0 - angle; 
      if(px < 0 && py < 0) angle += 180.0; 
      angle -= rotation;
    }

    if(angle >= 360.0) angle -= 360.0;
    if(angle < 0.0) angle = 360.0 + angle;

    r = sqrt(px * px + py * py);

    *x = 2.0 * r * plot->ymax / (gdouble) size;
    *y = angle;
}


/***********************************************************/

static void
gtk_plot_calc_ticks(GtkPlotPolar *polar, GtkPlotAxis *axis)
{
  GtkPlot *plot;
  GtkPlotTicks *ticks = NULL;
  GtkPlotScale scale;
  gdouble min = 0., max = 0.;
  gdouble absmin = 0., absmax = 0.;
  gdouble pt = 0.0;
  gdouble tick;
  gdouble major_step;
  gdouble minor_step;
  gint nmajor, nminor;
  gint n;

  plot = GTK_PLOT(polar); 

  scale = axis->scale;
  ticks = &axis->ticks;

  max = axis->max;
  min = axis->min;

  absmin = axis->min;
  absmax = axis->max;

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

  if(ticks->step > 0.){
   tick = min - major_step;
   while(tick <= absmax + 2*fabs(major_step)){
     if(tick >= min-1.E-10 && tick <= absmax+1.E-10){
        pt = transform(plot, tick);
        nmajor ++;
        ticks->major = (gint *)g_realloc(ticks->major, nmajor*sizeof(gint));
        ticks->major_values = (gdouble *)g_realloc(ticks->major_values, nmajor*sizeof(gdouble));
        ticks->major[nmajor-1] = pt;
        ticks->major_values[nmajor-1] = tick;
        ticks->nmajorticks = nmajor;
     }
     tick += major_step;
   }
  }

  if(ticks->step >0. && ticks->nminor > 0){
   for(nmajor = 0; nmajor < ticks->nmajorticks; nmajor++){
    tick = ticks->major_values[nmajor];
    for(n = 1; n <= ticks->nminor; n++){
     tick += minor_step;
     if(tick >= min-1.E-10 && tick <= absmax+1.E-10){
        pt = transform(plot, tick);
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

static gdouble
transform(GtkPlot *plot, gdouble y)
{
  gdouble width, height, size, position;

  position = y / plot->ymax;

  width = (gdouble)GTK_WIDGET(plot)->allocation.width * plot->width;
  height = (gdouble)GTK_WIDGET(plot)->allocation.height * plot->height;

  size = MIN(width, height) / 2.0;

  return(size * position);
}

/*******************************************
 * gtk_plot_polar_rotate
 *******************************************/
void
gtk_plot_polar_rotate(GtkPlotPolar *polar, gdouble angle)
{
  polar->rotation = angle;
}


