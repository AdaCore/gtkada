/* gtkplot3d - 3d scientific plots widget for gtk+
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
#include "gtkplot3d.h"
#include "gtkpsfont.h"

#define DEFAULT_WIDTH 420
#define DEFAULT_HEIGHT 340
#define DEFAULT_FONT_HEIGHT 10 

#ifndef PI
#define PI 3.14159265358979323846
#endif

static void gtk_plot3d_class_init 		(GtkPlot3DClass *klass);
static void gtk_plot3d_init 			(GtkPlot3D *plot);
static void gtk_plot3d_destroy 			(GtkObject *object);
static void gtk_plot3d_draw 			(GtkWidget *widget, 
						 GdkRectangle *area);
static void gtk_plot3d_real_paint 		(GtkWidget *widget);
static void gtk_plot3d_draw_plane		(GtkPlot3D *plot, 
						 GtkPlotVector v1, 
						 GtkPlotVector v2, 
						 GtkPlotVector v3, 
						 GtkPlotVector v4, 
						 GdkColor background);
static void gtk_plot_calc_ticks			(GtkPlot3D *plot, 
						 GtkPlotAxis *axis);
static void gtk_plot3d_draw_grids               (GtkPlot3D *plot, 
                                                 GtkPlotAxis *axis,
                                                 GtkPlotVector origin);
static void gtk_plot3d_draw_axis		(GtkPlot3D *plot, 
					 	 GtkPlotAxis *axis,
                                                 GtkPlotVector tick,
                                                 GtkPlotVector delta);
static void gtk_plot3d_draw_labels		(GtkPlot3D *plot, 
						 GtkPlotAxis *axis, 
                                                 GtkPlotVector delta);
static void gtk_plot3d_real_get_pixel		(GtkWidget *widget, 
                          			 gdouble x, 
						 gdouble y, 
						 gdouble z,
                          			 gdouble *px, 
						 gdouble *py, 
						 gdouble *pz);
static gint roundint				(gdouble x);
static void parse_label			        (gdouble val, 
						 gint precision, 
						 gint style,
                                                 gchar *label);

static GtkPlotClass *parent_class = NULL;


GtkType
gtk_plot3d_get_type (void)
{
  static GtkType plot_type = 0;

  if (!plot_type)
    {
      GtkTypeInfo plot_info =
      {
	"GtkPlot3D",
	sizeof (GtkPlot3D),
	sizeof (GtkPlot3DClass),
	(GtkClassInitFunc) gtk_plot3d_class_init,
	(GtkObjectInitFunc) gtk_plot3d_init,
	/* reserved 1*/ NULL,
        /* reserved 2 */ NULL,
        (GtkClassInitFunc) NULL,
      };

      plot_type = gtk_type_unique (gtk_plot_get_type(), &plot_info);
    }
  return plot_type;
}

static void
gtk_plot3d_class_init (GtkPlot3DClass *klass)
{
  GtkObjectClass *object_class;
  GtkWidgetClass *widget_class;
  GtkPlotClass *plot_class;
  GtkPlot3DClass *plot3d_class;

  parent_class = gtk_type_class (gtk_plot3d_get_type ());

  object_class = (GtkObjectClass *) klass;
  widget_class = (GtkWidgetClass *) klass;
  plot_class = (GtkPlotClass *) klass;
  plot3d_class = (GtkPlot3DClass *) klass;

  object_class->destroy = gtk_plot3d_destroy;

  widget_class->draw = gtk_plot3d_draw;

  plot_class->plot_paint = gtk_plot3d_real_paint;
  plot3d_class->get_pixel = gtk_plot3d_real_get_pixel;
}


static void
gtk_plot3d_init (GtkPlot3D *plot)
{
  GtkWidget *widget;
  GdkColor color;

  GTK_WIDGET_SET_FLAGS(plot, GTK_NO_WINDOW);

  widget = GTK_WIDGET(plot);
  gdk_color_black(gtk_widget_get_colormap(widget), &widget->style->black);
  gdk_color_white(gtk_widget_get_colormap(widget), &widget->style->white);

  GTK_PLOT(plot)->legends_x = .8;

  plot->ax = GTK_PLOT(plot)->bottom;
  plot->ay = GTK_PLOT(plot)->left;
  plot->az = GTK_PLOT(plot)->top;

  plot->center.x = 0.5;
  plot->center.y = 0.5;
  plot->center.z = 0.5;

  plot->xfactor = 1.0;
  plot->yfactor = 1.0;
  plot->zfactor = 1.0;

  gtk_plot3d_reset_angles(plot);
  gtk_plot3d_rotate_y(plot, 30.);
  gtk_plot3d_rotate_z(plot, 60.);

  GTK_PLOT(plot)->xmin = 0.;
  GTK_PLOT(plot)->xmax = 1.000000;
  GTK_PLOT(plot)->ymin = 0.;
  GTK_PLOT(plot)->ymax = 1.000000;
  plot->zmin = 0.;
  plot->zmax = 1.000000;

  plot->xy_visible = TRUE;
  plot->yz_visible = TRUE;
  plot->zx_visible = TRUE;

  plot->ax->show_major_grid = FALSE;
  plot->ax->show_minor_grid = FALSE;
  plot->ay->show_major_grid = FALSE;
  plot->ay->show_minor_grid = FALSE;
  plot->az->show_major_grid = FALSE;
  plot->az->show_minor_grid = FALSE;
 
  plot->ax->show_major_grid = TRUE;
  plot->ax->show_minor_grid = TRUE;
  plot->ay->show_major_grid = TRUE;
  plot->ay->show_minor_grid = TRUE;
  plot->az->show_major_grid = TRUE;
  plot->az->show_minor_grid = TRUE;

  plot->ax->ticks.nmajorticks = 0;
  plot->ax->ticks.nminorticks = 0;
  plot->ax->ticks.major = NULL;
  plot->ax->ticks.minor = NULL;
  plot->ax->ticks.major_values = NULL;
  plot->ax->ticks.minor_values = NULL;
  plot->ax->ticks.set_limits = FALSE;
  plot->ax->ticks.begin = 0;
  plot->ax->ticks.end = 0;
  plot->ax->ticks.step = .100000000;
  plot->ax->ticks.nminor = 1;

  plot->ay->ticks.nmajorticks = 0;
  plot->ay->ticks.nminorticks = 0;
  plot->ay->ticks.major = NULL;
  plot->ay->ticks.minor = NULL;
  plot->ay->ticks.major_values = NULL;
  plot->ay->ticks.minor_values = NULL;
  plot->ay->ticks.set_limits = FALSE;
  plot->ay->ticks.begin = 0;
  plot->ay->ticks.end = 0;
  plot->ay->ticks.step = .100000000;
  plot->ay->ticks.nminor = 1;

  plot->az->ticks.nmajorticks = 0;
  plot->az->ticks.nminorticks = 0;
  plot->az->ticks.major = NULL;
  plot->az->ticks.minor = NULL;
  plot->az->ticks.major_values = NULL;
  plot->az->ticks.minor_values = NULL;
  plot->az->ticks.set_limits = FALSE;
  plot->az->ticks.begin = 0;
  plot->az->ticks.end = 0;
  plot->az->ticks.step = .100000000;
  plot->az->ticks.nminor = 1;

  plot->ax->min = 0.0;
  plot->ax->max = 1.0;
  plot->ax->labels_offset = 25;
  plot->ax->major_mask = GTK_PLOT_TICKS_OUT;
  plot->ax->minor_mask = GTK_PLOT_TICKS_OUT;
  plot->ax->ticks_length = 8;
  plot->ax->ticks_width = 1;
  plot->ax->orientation = GTK_PLOT_AXIS_X;
  plot->ax->scale = GTK_PLOT_SCALE_LINEAR;
  plot->ax->is_visible = TRUE;
  plot->ax->custom_labels = FALSE;
  plot->ay->min = 0.0;
  plot->ay->max = 1.0;
  plot->ay->major_mask = GTK_PLOT_TICKS_OUT;
  plot->ay->minor_mask = GTK_PLOT_TICKS_OUT;
  plot->ay->ticks_length = 8;
  plot->ay->ticks_width = 1;
  plot->ay->labels_offset = 25;
  plot->ay->orientation = GTK_PLOT_AXIS_Y;
  plot->ay->scale = GTK_PLOT_SCALE_LINEAR;
  plot->ay->is_visible = TRUE;
  plot->ay->custom_labels = FALSE;
  plot->az->min = 0.0;
  plot->az->max = 1.0;
  plot->az->major_mask = GTK_PLOT_TICKS_OUT;
  plot->az->minor_mask = GTK_PLOT_TICKS_OUT;
  plot->az->ticks_length = 8;
  plot->az->ticks_width = 1;
  plot->az->labels_offset = 25;
  plot->az->orientation = GTK_PLOT_AXIS_Z;
  plot->az->scale = GTK_PLOT_SCALE_LINEAR;
  plot->az->is_visible = TRUE;
  plot->az->custom_labels = FALSE;

  plot->az->line.line_style = GTK_PLOT_LINE_SOLID;
  plot->az->line.line_width = 2;
  plot->az->line.color = widget->style->black; 
  plot->az->labels_attr.text = NULL;
  plot->az->labels_attr.height = DEFAULT_FONT_HEIGHT;
  plot->az->labels_attr.fg = widget->style->black;
  plot->az->labels_attr.bg = widget->style->white;
  plot->az->labels_attr.transparent = TRUE;
  plot->az->labels_attr.justification = GTK_JUSTIFY_CENTER;
  plot->az->labels_attr.angle = 0;
  plot->az->label_mask = GTK_PLOT_LABEL_OUT;
  plot->az->label_style = GTK_PLOT_LABEL_FLOAT;
  plot->az->label_precision = 1;
  plot->az->title.angle = 90;
  plot->az->title.justification = GTK_JUSTIFY_CENTER;
  plot->az->title.height = DEFAULT_FONT_HEIGHT;
  plot->az->title.fg = widget->style->black;
  plot->az->title.bg = widget->style->white;
  plot->az->title.transparent = TRUE;
  plot->az->title_visible = TRUE;

  plot->ax->line.line_style = GTK_PLOT_LINE_SOLID;
  plot->ax->line.line_width = 2;
  plot->ax->line.color = widget->style->black; 
  plot->ax->labels_attr.text = NULL;
  plot->ax->labels_attr.height = DEFAULT_FONT_HEIGHT;
  plot->ax->labels_attr.fg = widget->style->black;
  plot->ax->labels_attr.bg = widget->style->white;
  plot->ax->labels_attr.transparent = TRUE;
  plot->ax->labels_attr.justification = GTK_JUSTIFY_CENTER;
  plot->ax->labels_attr.angle = 0;
  plot->ax->label_mask = GTK_PLOT_LABEL_OUT;
  plot->ax->label_style = GTK_PLOT_LABEL_FLOAT;
  plot->ax->label_precision = 1;
  plot->ax->title.angle = 0;
  plot->ax->title.justification = GTK_JUSTIFY_CENTER;
  plot->ax->title.height = DEFAULT_FONT_HEIGHT;
  plot->ax->title.fg = widget->style->black;
  plot->ax->title.bg = widget->style->white;
  plot->ax->title.transparent = TRUE;
  plot->ax->title_visible = TRUE;

  plot->ay->line.line_style = GTK_PLOT_LINE_SOLID;
  plot->ay->line.line_width = 2;
  plot->ay->line.color = widget->style->black; 
  plot->ay->labels_attr.text = NULL;
  plot->ay->labels_attr.height = DEFAULT_FONT_HEIGHT;
  plot->ay->labels_attr.fg = widget->style->black;
  plot->ay->labels_attr.bg = widget->style->white;
  plot->ay->labels_attr.transparent = TRUE;
  plot->ay->labels_attr.angle = 0;
  plot->ay->label_mask = GTK_PLOT_LABEL_OUT;
  plot->ay->label_style = GTK_PLOT_LABEL_FLOAT;
  plot->ay->label_precision = 1;
  plot->ay->labels_attr.justification = GTK_JUSTIFY_CENTER;
  plot->ay->title.angle = 0;
  plot->ay->title.justification = GTK_JUSTIFY_CENTER;
  plot->ay->title.height = DEFAULT_FONT_HEIGHT;
  plot->ay->title.fg = widget->style->black;
  plot->ay->title.bg = widget->style->white;
  plot->ay->title.transparent = TRUE;
  plot->ay->title_visible = TRUE;

  gtk_plot_axis_set_title(GTK_PLOT(plot), GTK_PLOT_AXIS_BOTTOM, "X Title");
  gtk_plot_axis_set_title(GTK_PLOT(plot), GTK_PLOT_AXIS_LEFT, "Y Title");
  gtk_plot_axis_set_title(GTK_PLOT(plot), GTK_PLOT_AXIS_TOP, "Z Title");

  GTK_PLOT(plot)->xscale = GTK_PLOT_SCALE_LINEAR;
  GTK_PLOT(plot)->yscale = GTK_PLOT_SCALE_LINEAR;
  plot->zscale = GTK_PLOT_SCALE_LINEAR;

  plot->xy.major_mask = plot->ax->major_mask;
  plot->xy.minor_mask = plot->ax->minor_mask;
  plot->xy.label_mask = plot->ax->label_mask;
  plot->xz.major_mask = plot->ax->major_mask;
  plot->xz.minor_mask = plot->ax->minor_mask;
  plot->xz.label_mask = plot->ax->label_mask;
  plot->yx.major_mask = plot->ay->major_mask;
  plot->yx.minor_mask = plot->ay->minor_mask;
  plot->yx.label_mask = plot->ay->label_mask;
  plot->yz.major_mask = plot->ay->major_mask;
  plot->yz.minor_mask = plot->ay->minor_mask;
  plot->yz.label_mask = plot->ay->label_mask;
  plot->zx.major_mask = plot->az->major_mask;
  plot->zx.minor_mask = plot->az->minor_mask;
  plot->zx.label_mask = plot->az->label_mask;
  plot->zy.major_mask = plot->az->major_mask;
  plot->zy.minor_mask = plot->az->minor_mask;
  plot->zy.label_mask = plot->az->label_mask;

  plot->xy.title_visible = plot->ax->title_visible;
  plot->xz.title_visible = plot->ax->title_visible;
  plot->yx.title_visible = plot->ay->title_visible;
  plot->yz.title_visible = plot->ay->title_visible;
  plot->zx.title_visible = plot->az->title_visible;
  plot->zy.title_visible = plot->az->title_visible;

  plot->frame.color = widget->style->black;

  plot->corner_visible = FALSE;
  plot->corner.line_style = GTK_PLOT_LINE_SOLID;
  plot->corner.line_width = 0;
  plot->corner.color = widget->style->black;

  plot->ax->direction = plot->e1;
  plot->ay->direction = plot->e2;
  plot->az->direction = plot->e3;

  gdk_color_parse("gray95", &color);
  gdk_color_alloc(gdk_colormap_get_system(), &color);
  plot->color_xy = color;

  gdk_color_parse("gray80", &color);
  gdk_color_alloc(gdk_colormap_get_system(), &color);
  plot->color_yz = color;

  gdk_color_parse("gray65", &color);
  gdk_color_alloc(gdk_colormap_get_system(), &color);
  plot->color_zx = color;

  plot->titles_offset = 60;
  GTK_PLOT(plot)->legends_attr.transparent = FALSE;

  gtk_plot_calc_ticks(plot, plot->ax);
  gtk_plot_calc_ticks(plot, plot->ay);
  gtk_plot_calc_ticks(plot, plot->az);


  gtk_psfont_init();
}

static void
gtk_plot3d_destroy (GtkObject *object)
{
}

static void
gtk_plot3d_draw (GtkWidget *widget, GdkRectangle *area)
{
  gtk_plot_paint(GTK_PLOT(widget));
  gtk_plot_refresh (GTK_PLOT(widget), area);
}

static void
gtk_plot3d_real_paint (GtkWidget *widget)
{
  GtkPlot3D *plot;
  GtkPlotText *child_text;
  GdkPixmap *pixmap;
  GtkPlotPC *pc;
  GList *datasets;
  GList *text;
  GdkRectangle area;
  gint width, height, size;
  gint xoffset, yoffset, xp, yp;
  gint origin;
  gdouble pz;
  GtkPlotVector e[8], o, v[8];
  GtkPlotVector vx, vy, vz;
  gint i;

  if(!GTK_WIDGET_REALIZED(widget)) return;

  plot = GTK_PLOT3D(widget);

  area.x = widget->allocation.x;
  area.y = widget->allocation.y;
  area.width = widget->allocation.width;
  area.height = widget->allocation.height;

  xoffset = area.x + roundint(GTK_PLOT(plot)->x * widget->allocation.width);
  yoffset = area.y + roundint(GTK_PLOT(plot)->y * widget->allocation.height);
  width = roundint(GTK_PLOT(plot)->width * widget->allocation.width);
  height = roundint(GTK_PLOT(plot)->height * widget->allocation.height);

  pixmap = GTK_PLOT(plot)->drawable;
  pc = GTK_PLOT(plot)->pc;

  gtk_plot_pc_gsave(pc);
  gtk_plot_pc_set_color(pc, &GTK_PLOT(plot)->background);

  if(!GTK_PLOT_TRANSPARENT(plot))
    gtk_plot_pc_draw_rectangle (pc, TRUE,
  		        xoffset, yoffset,
		        width , height);

  /* draw frame to guide the eyes*/
/*
  gdk_draw_rectangle (pixmap, widget->style->black_gc, FALSE,
		      xoffset, yoffset,
		      width , height);
*/

  /* find origin */

  xp = roundint(GTK_PLOT(plot)->x * (gdouble)widget->allocation.width);
  yp = roundint(GTK_PLOT(plot)->y * (gdouble)widget->allocation.height);

  size = MIN(width, height) / sqrt(2.);

  /* 8 vertices of the cube */

  e[0].x = 0;
  e[0].y = 0;
  e[0].z = 0;
  e[1].x = 1;
  e[1].y = 0;
  e[1].z = 0;
  e[2].x = 1;
  e[2].y = 1;
  e[2].z = 0;
  e[3].x = 0;
  e[3].y = 1;
  e[3].z = 0;
  e[4].x = 0;
  e[4].y = 0;
  e[4].z = 1;
  e[5].x = 1;
  e[5].y = 0;
  e[5].z = 1;
  e[6].x = 1;
  e[6].y = 1;
  e[6].z = 1;
  e[7].x = 0;
  e[7].y = 1;
  e[7].z = 1;

  for(i = 0; i < 8; i++){
    v[i].x = (1.0 - e[i].x) * plot->ax->min + e[i].x * plot->ax->max;
    v[i].y = (1.0 - e[i].y) * plot->ay->min + e[i].y * plot->ay->max;
    v[i].z = (1.0 - e[i].z) * plot->az->min + e[i].z * plot->az->max;
  }

  /* Origin for drawing the planes */

  origin = 0;
  o.x = 0.0;
  o.y = 0.0;
  o.z = 0.0;

  for(i = 1; i < 8; i++){
     pz = e[i].x * plot->e1.z + e[i].y * plot->e2.z + e[i].z * plot->e3.z ;

     if(pz > o.z){
       o.z = pz;
       origin = i;
     }
  }
 
  plot->origin = v[origin];

  plot->ax->direction.x = 1.0;
  plot->ax->direction.y = 0.0;
  plot->ax->direction.z = 0.0;
  plot->ay->direction.x = 0.0;
  plot->ay->direction.y = 1.0;
  plot->ay->direction.z = 0.0;
  plot->az->direction.x = 0.0;
  plot->az->direction.y = 0.0;
  plot->az->direction.z = 1.0;

  plot->ax->origin.x = 0.0;
  plot->ax->origin.y = v[origin].y;
  plot->ax->origin.z = v[origin].z;

  plot->ay->origin.y = 0.0;
  plot->ay->origin.x = v[origin].x;
  plot->ay->origin.z = v[origin].z;

  plot->az->origin.z = 0.0;
  plot->az->origin.x = v[origin].x;
  plot->az->origin.y = v[origin].y;

  /* Tick directions */

  vx.x = plot->e1.x * (1. - 2. * e[origin].x);
  vx.y = plot->e1.y * (1. - 2. * e[origin].x);
  vx.z = plot->e1.z * (1. - 2. * e[origin].x);
  vy.x = plot->e2.x * (1. - 2. * e[origin].y);
  vy.y = plot->e2.y * (1. - 2. * e[origin].y);
  vy.z = plot->e2.z * (1. - 2. * e[origin].y);
  vz.x = plot->e3.x * (1. - 2. * e[origin].z);
  vz.y = plot->e3.y * (1. - 2. * e[origin].z);
  vz.z = plot->e3.z * (1. - 2. * e[origin].z);

  /* draw planes, ticks & grid lines */

  gtk_plot_calc_ticks(plot, plot->ax);
  gtk_plot_calc_ticks(plot, plot->ay);
  gtk_plot_calc_ticks(plot, plot->az);

  if(plot->xy_visible)
    {
      /* PLANES */

      if(origin == 0 || origin == 1 || origin == 2 || origin == 3)
        gtk_plot3d_draw_plane(plot, v[0], v[1], v[2], v[3], plot->color_xy);
      if(origin == 4 || origin == 5 || origin == 6 || origin == 7)
        gtk_plot3d_draw_plane(plot, v[4], v[5], v[6], v[7], plot->color_xy);

      /* X AXIS */

      plot->ax->major_mask = plot->xy.major_mask;
      plot->ax->minor_mask = plot->xy.minor_mask;
      plot->ax->label_mask = plot->xy.label_mask;
      plot->ax->title_visible = plot->xy.title_visible;

      o.x = 0.0;
      o.y = (1.0 - e[origin].y) * plot->ay->max + e[origin].y * plot->ay->min - 
             plot->ax->origin.y;
      o.z = 0.0;

      gtk_plot3d_draw_grids(plot, plot->ax, o);
      gtk_plot3d_draw_axis(plot, plot->ax, vy, o);
      gtk_plot3d_draw_labels(plot, plot->ax, o); 

      /* Y AXIS */

      plot->ay->major_mask = plot->yx.major_mask;
      plot->ay->minor_mask = plot->yx.minor_mask;
      plot->ay->label_mask = plot->yx.label_mask;
      plot->ay->title_visible = plot->yx.title_visible;

      o.x = (1.0 - e[origin].x) * plot->ax->max + e[origin].x * plot->ax->min - 
             plot->ay->origin.x;
      o.y = 0.0;
      o.z = 0.0;

      gtk_plot3d_draw_grids(plot, plot->ay, o);
      gtk_plot3d_draw_axis(plot, plot->ay, vx, o);
      gtk_plot3d_draw_labels(plot, plot->ay, o); 
    }

  if(plot->yz_visible)
    {
      /* PLANES */

      if(origin == 0 || origin == 3 || origin == 7 || origin == 4)
        gtk_plot3d_draw_plane(plot, v[0], v[3], v[7], v[4], plot->color_yz);
      if(origin == 1 || origin == 2 || origin == 6 || origin == 5)
        gtk_plot3d_draw_plane(plot, v[1], v[2], v[6], v[5], plot->color_yz);

      /* Y AXIS */

      plot->ay->major_mask = plot->yz.major_mask;
      plot->ay->minor_mask = plot->yz.minor_mask;
      plot->ay->label_mask = plot->yz.label_mask;
      plot->ay->title_visible = plot->yz.title_visible;

      o.x = 0.0;
      o.y = 0.0;
      o.z = (1.0 - e[origin].z) * plot->az->max + e[origin].z * plot->az->min - 
             plot->ay->origin.z;

      gtk_plot3d_draw_grids(plot, plot->ay, o);
      gtk_plot3d_draw_axis(plot, plot->ay, vz, o);
      gtk_plot3d_draw_labels(plot, plot->ay, o); 

      /* Z AXIS */

      plot->az->major_mask = plot->zy.major_mask;
      plot->az->minor_mask = plot->zy.minor_mask;
      plot->az->label_mask = plot->zy.label_mask;
      plot->az->title_visible = plot->zy.title_visible;

      o.x = 0.0;
      o.y = (1.0 - e[origin].y) * plot->ay->max + e[origin].y * plot->ay->min - 
             plot->az->origin.y;
      o.z = 0.0;

      gtk_plot3d_draw_grids(plot, plot->az, o);
      gtk_plot3d_draw_axis(plot, plot->az, vy, o);
      gtk_plot3d_draw_labels(plot, plot->az, o); 
    }

  if(plot->zx_visible)
    {
      /* PLANES */

      if(origin == 0 || origin == 4 || origin == 5 || origin == 1)
        gtk_plot3d_draw_plane(plot, v[0], v[4], v[5], v[1], plot->color_zx);
      if(origin == 3 || origin == 7 || origin == 6 || origin == 2)
        gtk_plot3d_draw_plane(plot, v[3], v[7], v[6], v[2], plot->color_zx);

      /* Z AXIS */

      plot->az->major_mask = plot->zx.major_mask;
      plot->az->minor_mask = plot->zx.minor_mask;
      plot->az->label_mask = plot->zx.label_mask;
      plot->az->title_visible = plot->zx.title_visible;

      o.x = (1.0 - e[origin].x) * plot->ax->max + e[origin].x * plot->ax->min - 
             plot->az->origin.x;
      o.y = 0.0;
      o.z = 0.0;

      gtk_plot3d_draw_grids(plot, plot->az, o);
      gtk_plot3d_draw_axis(plot, plot->az, vx, o);
      gtk_plot3d_draw_labels(plot, plot->az, o); 

      /* X AXIS */

      plot->ax->major_mask = plot->xz.major_mask;
      plot->ax->minor_mask = plot->xz.minor_mask;
      plot->ax->label_mask = plot->xz.label_mask;
      plot->ax->title_visible = plot->xz.title_visible;

      o.x = 0.0;
      o.y = 0.0;
      o.z = (1.0 - e[origin].z) * plot->az->max + e[origin].z * plot->az->min - 
             plot->ax->origin.z;

      gtk_plot3d_draw_grids(plot, plot->ax, o);
      gtk_plot3d_draw_axis(plot, plot->ax, vz, o);
      gtk_plot3d_draw_labels(plot, plot->ax, o); 
    }

  datasets = GTK_PLOT(plot)->data_sets;
  while(datasets)
   {
     GtkPlotData *dataset;

     dataset = GTK_PLOT_DATA(datasets->data);

     gtk_plot_data_paint(dataset);

     datasets = datasets->next;
   }

  if(plot->corner_visible){
    gdouble px0, py0, pz0;
    gdouble px, py, pz;
    gint corner;
    gint end[3];
    
    corner = origin + 2;
    if(corner > 3) corner -= 4;

    end[0] = corner;

    corner += 4;
    if(corner > 7) corner -= 8;

    end[1] = corner + 1;
    if(end[1] == 8 || end[1] == 4) end[1] -= 4;

    end[2] = corner - 1;
    if(end[2] == -1 || end[2] == 3) end[2] += 4;

    gtk_plot3d_get_pixel(plot, v[corner].x, v[corner].y, v[corner].z, 
                         &px0, &py0, &pz0);

    for (i = 0; i < 3; i++){
       gtk_plot3d_get_pixel(plot, v[end[i]].x, v[end[i]].y, v[end[i]].z, 
                            &px, &py, &pz);

       gtk_plot_draw_line(GTK_PLOT(plot), plot->corner, px0, py0, px, py);
    }

  }

  text = GTK_PLOT(plot)->text;
  while(text)
   {
     child_text = (GtkPlotText *) text->data;  
     gtk_plot_draw_text(GTK_PLOT(plot), *child_text);
     text = text->next;
   }

  GTK_PLOT_CLASS(GTK_OBJECT(plot)->klass)->draw_legends(GTK_WIDGET(plot));

  gtk_plot_pc_grestore(pc);
}


GtkWidget*
gtk_plot3d_new (GdkDrawable *drawable)
{
  GtkPlot3D *plot;

  plot = gtk_type_new (gtk_plot3d_get_type ());

  gtk_plot3d_construct(GTK_PLOT3D(plot), drawable);

  return GTK_WIDGET (plot);
}

void
gtk_plot3d_construct(GtkPlot3D *plot, GdkDrawable *drawable)
{
  GTK_PLOT(plot)->drawable = drawable;
}

GtkWidget*
gtk_plot3d_new_with_size (GdkDrawable *drawable, gdouble width, gdouble height)
{
  GtkWidget *plot; 

  plot = gtk_type_new (gtk_plot3d_get_type ());

  gtk_plot3d_construct_with_size(GTK_PLOT3D(plot), drawable, width, height);

  return(plot);
}

void
gtk_plot3d_construct_with_size(GtkPlot3D *plot, GdkDrawable *drawable,
			       gdouble width, gdouble height)
{
  gtk_plot3d_construct(plot, drawable);

  gtk_plot_resize (GTK_PLOT(plot), width, height);
}

static void
gtk_plot3d_draw_plane(GtkPlot3D *plot, 
                      GtkPlotVector v1, 
                      GtkPlotVector v2, 
                      GtkPlotVector v3, 
                      GtkPlotVector v4, 
                      GdkColor background)
{
  GtkWidget *widget;
  GdkPixmap *pixmap;
  GtkPlotPC *pc;
  GtkPlotVector v[4];
  GtkPlotPoint p[4];
  gdouble px, py, pz;
  gint i;

  widget = GTK_WIDGET(plot);
  if(!GTK_WIDGET_DRAWABLE(plot)) return;

  pixmap = GTK_PLOT(plot)->drawable;
  pc = GTK_PLOT(plot)->pc;

  gtk_plot_pc_set_color(pc, &background);

  v[0] = v1;
  v[1] = v2;
  v[2] = v3;
  v[3] = v4;
  for(i = 0; i < 4; i++){
    gtk_plot3d_get_pixel(plot, v[i].x, v[i].y, v[i].z, &px, &py, &pz);
    p[i].x = px;
    p[i].y = py;
  }

  gtk_plot_pc_draw_polygon(pc, TRUE, p, 4);
  gtk_plot_pc_set_color(pc, &plot->frame.color);
  gtk_plot_pc_set_lineattr(pc, 
                           plot->frame.line_width, 
                           plot->frame.line_style,
                           0, 0);
  gtk_plot_pc_draw_polygon(pc, FALSE, p, 4);

}

static void
gtk_plot3d_draw_grids(GtkPlot3D *plot, GtkPlotAxis *axis, GtkPlotVector delta)
{
  GtkWidget *widget;
  GtkPlotLine major_grid, minor_grid;
  gdouble xx;
  gdouble x1, x2, y1, y2;
  gint size, width, height;
  gdouble xp, yp, oz;
  gint ntick;

  widget = GTK_WIDGET(plot);

  xp = roundint(GTK_PLOT(plot)->x * widget->allocation.width);
  yp = roundint(GTK_PLOT(plot)->y * widget->allocation.height);
  width = roundint(GTK_PLOT(plot)->width * widget->allocation.width);
  height = roundint(GTK_PLOT(plot)->height * widget->allocation.height);

  size = MIN(width, height) / sqrt(2.);

  switch(axis->orientation){
    case GTK_PLOT_AXIS_X:
      major_grid = GTK_PLOT(plot)->left->major_grid;
      minor_grid = GTK_PLOT(plot)->left->minor_grid;
      break;
    case GTK_PLOT_AXIS_Y:
      major_grid = GTK_PLOT(plot)->bottom->major_grid;
      minor_grid = GTK_PLOT(plot)->bottom->minor_grid;
      break;
    case GTK_PLOT_AXIS_Z:
      major_grid = GTK_PLOT(plot)->top->major_grid;
      minor_grid = GTK_PLOT(plot)->top->minor_grid;
      break;
  }

  if(axis->show_minor_grid)
    {
        for(ntick = 0; ntick < axis->ticks.nminorticks; ntick++){
          if(axis->ticks.minor_values[ntick] >= axis->min){
               xx = axis->ticks.minor_values[ntick];
               gtk_plot3d_get_pixel(plot,
                             axis->origin.x + axis->direction.x * xx,
                             axis->origin.y + axis->direction.y * xx,
                             axis->origin.z + axis->direction.z * xx,
                             &x1, &y1, &oz);
               gtk_plot3d_get_pixel(plot,
                             axis->origin.x + axis->direction.x * xx + delta.x,
                             axis->origin.y + axis->direction.y * xx + delta.y,
                             axis->origin.z + axis->direction.z * xx + delta.z,
                             &x2, &y2, &oz);
               gtk_plot_draw_line(GTK_PLOT(plot),  
                                  minor_grid,
                                  x1, y1, x2, y2);
          }
        }
    }

  if(axis->show_major_grid)
    {
        for(ntick = 0; ntick < axis->ticks.nmajorticks; ntick++){
          if(axis->ticks.major_values[ntick] > axis->min &&
             axis->ticks.major_values[ntick] < axis->max){
               xx = axis->ticks.major_values[ntick];
               gtk_plot3d_get_pixel(plot,
                             axis->origin.x + axis->direction.x * xx,
                             axis->origin.y + axis->direction.y * xx,
                             axis->origin.z + axis->direction.z * xx,
                             &x1, &y1, &oz);
               gtk_plot3d_get_pixel(plot,
                             axis->origin.x + axis->direction.x * xx + delta.x,
                             axis->origin.y + axis->direction.y * xx + delta.y,
                             axis->origin.z + axis->direction.z * xx + delta.z,
                             &x2, &y2, &oz);
               gtk_plot_draw_line(GTK_PLOT(plot),  
                                  major_grid,
                                  x1, y1, x2, y2);
          }
        }
    }

}

static void
gtk_plot3d_draw_axis(GtkPlot3D *plot, 
                     GtkPlotAxis *axis, 
                     GtkPlotVector tick,
                     GtkPlotVector delta)
{
  GtkWidget *widget;
  GtkPlotPC *pc;
  gdouble xx;
  gint line_width;
  gint xp, yp, width, height;
  gint ntick;
  gdouble m;
  gdouble oz;
  gdouble x1, x2, y1, y2;
  gint size;
  gint ticks_length;

  widget = GTK_WIDGET(plot); 
  pc = GTK_PLOT(plot)->pc;

  xp = roundint(GTK_PLOT(plot)->x * widget->allocation.width);
  yp = roundint(GTK_PLOT(plot)->y * widget->allocation.height);
  width = roundint(GTK_PLOT(plot)->width * widget->allocation.width);
  height = roundint(GTK_PLOT(plot)->height * widget->allocation.height);

  m = GTK_PLOT(plot)->magnification;
  size = MIN(width, height) / sqrt(2.);

  line_width = plot->frame.line_width;
  gtk_plot_pc_set_color(pc, &plot->frame.color);
  gtk_plot_pc_set_lineattr(pc, line_width, 0, 3, 0);

/*
  gtk_plot_pc_draw_line(pc, x1, y1, x2, y2);
*/

  gtk_plot_pc_set_lineattr(pc, axis->ticks_width, 0, 1, 0);

  for(ntick = 0; ntick < axis->ticks.nmajorticks; ntick++){
           ticks_length = axis->ticks_length;
           xx = axis->ticks.major_values[ntick];
           gtk_plot3d_get_pixel(plot, 
                          axis->origin.x + axis->direction.x * xx + delta.x,
                          axis->origin.y + axis->direction.y * xx + delta.y,
                          axis->origin.z + axis->direction.z * xx + delta.z,
                          &x1, &y1, &oz);
           if(axis->ticks.major_values[ntick] >= axis->min){
             x2 = x1 + m * ticks_length * tick.x;
             y2 = y1 + m * ticks_length * tick.y;
             if(axis->major_mask == GTK_PLOT_TICKS_OUT)
                gtk_plot_pc_draw_line(pc, x1, y1, x2, y2);
             x2 = x1 - m * axis->ticks_length * tick.x;
             y2 = y1 - m * axis->ticks_length * tick.y;
             if(axis->major_mask == GTK_PLOT_TICKS_IN)
                gtk_plot_pc_draw_line(pc, x1, y1, x2, y2);
           }
         }     
         ticks_length = axis->ticks_length / 2.;
         for(ntick = 0; ntick < axis->ticks.nminorticks; ntick++){
           xx = axis->ticks.minor_values[ntick];
           gtk_plot3d_get_pixel(plot, 
                          axis->origin.x + axis->direction.x * xx + delta.x,
                          axis->origin.y + axis->direction.y * xx + delta.y,
                          axis->origin.z + axis->direction.z * xx + delta.z,
                          &x1, &y1, &oz);
           if(axis->ticks.minor_values[ntick] >= axis->min){
             x2 = x1 + m * ticks_length * tick.x;
             y2 = y1 + m * ticks_length * tick.y;
             if(axis->minor_mask == GTK_PLOT_TICKS_OUT)
                gtk_plot_pc_draw_line(pc, x1, y1, x2, y2);
               
             x2 = x1 - m * axis->ticks_length * tick.x;
             y2 = y1 - m * axis->ticks_length * tick.y;
             if(axis->minor_mask == GTK_PLOT_TICKS_IN) 
                gtk_plot_pc_draw_line(pc, x1, y1, x2, y2);
           }
         }     

}


static void
gtk_plot3d_draw_labels(GtkPlot3D *plot, 
                       GtkPlotAxis *axis,
                       GtkPlotVector delta)
{
  GtkWidget *widget;
  GtkPlotPC *pc;
  GdkFont *font;
  GtkPlotText title, tick;
  gchar label[100];
  gdouble tick_value;
  gdouble xx;
  gint text_height;
  gint xp, yp, width, height;
  gint ntick;
  gdouble m;
  gdouble size, ox, oy, oz;
  gdouble y;
  GtkPlotVector ticks_direction, center, aux;
  gdouble proj;
  gboolean veto = FALSE;

  widget = GTK_WIDGET(plot); 
  pc = GTK_PLOT(plot)->pc;

  xp = roundint(GTK_PLOT(plot)->x * widget->allocation.width);
  yp = roundint(GTK_PLOT(plot)->y * widget->allocation.height);
  width = roundint(GTK_PLOT(plot)->width * widget->allocation.width);
  height = roundint(GTK_PLOT(plot)->height * widget->allocation.height);

  size = MIN(width, height) / sqrt(2.);
  m = GTK_PLOT(plot)->magnification;

  gtk_plot_pc_set_color (pc, &axis->labels_attr.fg);

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

  center.x = (plot->ax->max + plot->ax->min) / 2.0;
  center.y = (plot->ay->max + plot->ay->min) / 2.0;
  center.z = (plot->az->max + plot->az->min) / 2.0;

  ticks_direction.x = axis->origin.x + delta.x - center.x;
  ticks_direction.y = axis->origin.y + delta.y - center.y;
  ticks_direction.z = axis->origin.z + delta.z - center.z;

  proj = ticks_direction.x * axis->direction.x +
         ticks_direction.y * axis->direction.y +
         ticks_direction.z * axis->direction.z;

  ticks_direction.x -= proj * axis->direction.x;
  ticks_direction.y -= proj * axis->direction.y;
  ticks_direction.z -= proj * axis->direction.z;

  proj = sqrt(ticks_direction.x * ticks_direction.x +
              ticks_direction.y * ticks_direction.y +
              ticks_direction.z * ticks_direction.z);

  ticks_direction.x /= proj;
  ticks_direction.y /= proj;
  ticks_direction.z /= proj;

  aux = ticks_direction;

  ticks_direction.x = aux.x*plot->e1.x + aux.y*plot->e2.x + aux.z*plot->e3.x; 
  ticks_direction.y = aux.x*plot->e1.y + aux.y*plot->e2.y + aux.z*plot->e3.y; 
  ticks_direction.z = aux.x*plot->e1.z + aux.y*plot->e2.z + aux.z*plot->e3.z; 

  for(ntick = 0; ntick < axis->ticks.nmajorticks; ntick++){
    xx = axis->ticks.major_values[ntick];
    gtk_plot3d_get_pixel(plot, 
                         axis->origin.x + axis->direction.x * xx + delta.x,
                         axis->origin.y + axis->direction.y * xx + delta.y,
                         axis->origin.z + axis->direction.z * xx + delta.z,
                         &ox, &oy, &oz);
   
    tick.x = ox +  axis->labels_offset * ticks_direction.x; 
    tick.y = oy +  axis->labels_offset * ticks_direction.y; 

    tick_value = axis->ticks.major_values[ntick];

    if(tick_value >= axis->min-1.e-9){
      if(!axis->custom_labels){
        parse_label(tick_value, axis->label_precision, axis->label_style, label);
      }
      else
      {
        gtk_signal_emit_by_name(GTK_OBJECT(axis), "tick_label", 
                                &tick_value, label, &veto);
        if(!veto)
          parse_label(tick_value, axis->label_precision, axis->label_style, label);
      }
      tick.text = label;

      if(axis->label_mask == GTK_PLOT_LABEL_OUT){
         tick.x = (gdouble)tick.x / (gdouble)widget->allocation.width;
         tick.y = (gdouble)tick.y / (gdouble)widget->allocation.height;
         gtk_plot_draw_text(GTK_PLOT(plot), tick);
      }
    }
  }

  if(axis->title_visible && axis->title.text)
       {
         title = axis->title;

         gtk_plot3d_get_pixel(plot,
                       axis->origin.x + center.x * axis->direction.x + delta.x, 
                       axis->origin.y + center.y * axis->direction.y + delta.y, 
                       axis->origin.z + center.z * axis->direction.z + delta.z, 
                       &ox, &oy, &oz);
    
         title.x = ox + plot->titles_offset * ticks_direction.x; 
         title.y = oy + plot->titles_offset * ticks_direction.y; 

         title.x = title.x / (gdouble)widget->allocation.width;
         title.y = title.y / (gdouble)widget->allocation.height;

         gtk_plot_draw_text(GTK_PLOT(plot), title); 
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


/******************************************
 ******************************************/
void
gtk_plot3d_autoscale(GtkPlot3D *plot)
{
  GList *datasets;
  gdouble xmin, xmax, ymin, ymax, zmin, zmax;

  if(!GTK_PLOT(plot)->data_sets) return;

  xmin = GTK_PLOT(plot)->bottom->max;
  xmax = GTK_PLOT(plot)->bottom->min;
  ymin = GTK_PLOT(plot)->left->max;
  ymax = GTK_PLOT(plot)->left->min;
  zmin = GTK_PLOT(plot)->top->max;
  zmax = GTK_PLOT(plot)->top->min;

  datasets = GTK_PLOT(plot)->data_sets;

  while(datasets){
    GtkPlotData *data;
    gint i;

    data = GTK_PLOT_DATA(datasets->data);

    if(!data->is_function){
      for (i = 0; i < data->num_points; i++){
        gdouble x, y, z;
        x = data->x[i];  
        y = data->y[i];  
        z = data->z[i];  
  
        xmin = MIN(xmin, x); 
        xmax = MAX(xmax, x); 
        ymin = MIN(ymin, y); 
        ymax = MAX(ymax, y); 
        zmin = MIN(zmin, z); 
        zmax = MAX(zmax, z); 
      } 
    }

    datasets = datasets->next;
  }

  if(xmin < xmax) gtk_plot3d_set_xrange(plot, xmin, xmax);
  if(ymin < ymax) gtk_plot3d_set_yrange(plot, ymin, ymax);
  if(zmin < zmax) gtk_plot3d_set_zrange(plot, zmin, zmax);
}


void
gtk_plot3d_rotate(GtkPlot3D *plot, gdouble a1, gdouble a2, gdouble a3)
{
  gtk_plot3d_rotate_vector(plot, &plot->e1, a1, a2, a3);
  gtk_plot3d_rotate_vector(plot, &plot->e2, a1, a2, a3);
  gtk_plot3d_rotate_vector(plot, &plot->e3, a1, a2, a3);
}

void
gtk_plot3d_rotate_vector(GtkPlot3D *plot, 
                          GtkPlotVector *vector,
                          gdouble a1, gdouble a2, gdouble a3)
{
  GtkPlotVector v;
  gdouble cos1, sin1;
  gdouble cos2, sin2;
  gdouble cos3, sin3;

  a1 = a1 * PI / 180.;
  a2 = a2 * PI / 180.;
  a3 = a3 * PI / 180.;

  cos1 = cos(a1);
  sin1 = sin(a1);
  cos2 = cos(a2);
  sin2 = sin(a2);
  cos3 = cos(a3);
  sin3 = sin(a3);

  v.y = vector->y*cos1 - vector->z*sin1;
  v.z = vector->z*cos1 + vector->y*sin1;

  vector->y = v.y;
  vector->z = v.z;

  v.z = vector->z*cos2 - vector->x*sin2;
  v.x = vector->x*cos2 + vector->z*sin2;

  vector->x = v.x;
  vector->z = v.z;

  v.x = vector->x*cos3 - vector->y*sin3;
  v.y = vector->y*cos3 + vector->x*sin3;

  vector->x = v.x;
  vector->y = v.y;
}

void
gtk_plot3d_get_pixel(GtkPlot3D *plot, 
                     gdouble x, gdouble y, gdouble z,
                     gdouble *px, gdouble *py, gdouble *pz)
{
  GTK_PLOT3D_CLASS(GTK_OBJECT(plot)->klass)->get_pixel(GTK_WIDGET(plot), x, y, z, px, py, pz);
}
  
static void
gtk_plot3d_real_get_pixel(GtkWidget *widget, 
                          gdouble x, gdouble y, gdouble z,
                          gdouble *px, gdouble *py, gdouble *pz)
{
  GtkPlot3D *plot;
  GtkPlotVector e1, e2, e3, center;
  gint xp, yp, width, height, size;
  gdouble rx, ry, rz;
  gdouble cx, cy, cz;
  gdouble dx, dy, dz;

  plot = GTK_PLOT3D(widget); 
  xp = roundint(GTK_PLOT(plot)->x * widget->allocation.width);
  yp = roundint(GTK_PLOT(plot)->y * widget->allocation.height);
  width = roundint(GTK_PLOT(plot)->width * widget->allocation.width);
  height = roundint(GTK_PLOT(plot)->height * widget->allocation.height);

  size = MIN(width, height) / sqrt(2.);
  e1.x = plot->e1.x * plot->xfactor;
  e1.y = plot->e1.y * plot->xfactor;
  e1.z = plot->e1.z * plot->xfactor;
  e2.x = plot->e2.x * plot->yfactor;
  e2.y = plot->e2.y * plot->yfactor;
  e2.z = plot->e2.z * plot->yfactor;
  e3.x = plot->e3.x * plot->zfactor;
  e3.y = plot->e3.y * plot->zfactor;
  e3.z = plot->e3.z * plot->zfactor;

  dx = GTK_PLOT(plot)->xmax - GTK_PLOT(plot)->xmin;
  dy = GTK_PLOT(plot)->ymax - GTK_PLOT(plot)->ymin;
  dz = plot->zmax - plot->zmin;

  rx = (x - GTK_PLOT(plot)->xmin) / dx;
  ry = (y - GTK_PLOT(plot)->ymin) / dy;
  rz = (z - plot->zmin) / dz;

  center = plot->center;

  cx = -(center.x * e1.x + center.y * e2.x + center.z * e3.x);
  cy = -(center.x * e1.y + center.y * e2.y + center.z * e3.y);
  cz = -(center.x * e1.z + center.y * e2.z + center.z * e3.z);

  *px = xp + width / 2.;
  *py = yp + height / 2.;
  *pz = 0.0;

  *px += roundint((cx + rx * e1.x + ry * e2.x + rz * e3.x) * size);
  *py += roundint((cy + rx * e1.y + ry * e2.y + rz * e3.y) * size);
  *pz += roundint((cz + rx * e1.z + ry * e2.z + rz * e3.z) * size);
}


/***********************************************************/

static void
gtk_plot_calc_ticks(GtkPlot3D *plot, GtkPlotAxis *axis)
{
  GtkPlotTicks *ticks;
  GtkPlotScale scale;
  gdouble min = 0., max = 0.;
  gdouble absmin = 0., absmax = 0.;
  gdouble tick;
  gdouble major_step;
  gdouble minor_step;
  gint nmajor, nminor;
  gint n;

  scale = axis->scale;

  ticks = &axis->ticks;
  max = axis->max;
  min = axis->min;

  if(scale == GTK_PLOT_SCALE_LOG10){
         if(max <= 1.E-12) max = 1.E-2;
         if(min <= 1.E-12) min = max/1000.;
         min = floor(log10(min));
         min = pow(10., min);
         axis->min = min;
         axis->max = max;
  }
  absmin = axis->min;
  absmax = axis->max;

  if(ticks->set_limits){
       max = ticks->end;
       min = ticks->begin;
  } else {
       min = floor(min/ticks->step) * ticks->step;
       max = ceil(max/ticks->step) * ticks->step;
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
         major_step = 1;

     minor_step = major_step / ((gdouble)ticks->nminor + 2.0);
  }

  if(ticks->step > 0.){
   tick = min - major_step;
   while(tick <= absmax + 2*fabs(major_step)){
     if(tick >= min-1.E-10 && tick <= absmax+1.E-10){
        nmajor ++;
        ticks->major = (gint *)g_realloc(ticks->major, nmajor*sizeof(gint));
        ticks->major_values = (gdouble *)g_realloc(ticks->major_values, nmajor*sizeof(gdouble));
        ticks->major_values[nmajor-1] = tick;
        ticks->nmajorticks = nmajor;
     }
     switch(scale){
        case GTK_PLOT_SCALE_LINEAR:
            tick += major_step;
            break;
        case GTK_PLOT_SCALE_LOG10:
            tick = absmin * pow(10., nmajor*major_step);
            break;
     }
   }
  }

  if(ticks->step >0. && ticks->nminor > 0){
   for(nmajor = 0; nmajor < ticks->nmajorticks; nmajor++){
    tick = ticks->major_values[nmajor];
    for(n = 1; n <= ticks->nminor; n++){
     switch(scale){
        case GTK_PLOT_SCALE_LINEAR:
            tick += minor_step;
            break;
        case GTK_PLOT_SCALE_LOG10:
            tick += minor_step*ticks->major_values[nmajor]*10.;
            break;
     }  
     if(tick >= min-1.E-10 && tick <= absmax+1.E-10){
        nminor ++;
        ticks->minor = (gint *)g_realloc(ticks->minor, nminor*sizeof(gint));
        ticks->minor_values = (gdouble *)g_realloc(ticks->minor_values, nminor*sizeof(gdouble));
        ticks->minor_values[nminor-1] = tick;
        ticks->nminorticks = nminor;
     }
    }
   }
  }

}


/***********************************************************
 * gtk_plot3d_set_xrange
 * gtk_plot3d_set_yrange
 * gtk_plot3d_set_zrange
 * gtk_plot3d_set_xfactor
 * gtk_plot3d_set_yfactor
 * gtk_plot3d_set_zfactor
 * gtk_plot3d_get_xfactor
 * gtk_plot3d_get_yfactor
 * gtk_plot3d_get_zfactor
 ***********************************************************/
void
gtk_plot3d_set_xrange(GtkPlot3D *plot, gdouble min, gdouble max)
{
  if(min > max) return;

  GTK_PLOT(plot)->xmin = min;
  GTK_PLOT(plot)->xmax = max;
  plot->ax->min = min;
  plot->ax->max = max;
}

void
gtk_plot3d_set_yrange(GtkPlot3D *plot, gdouble min, gdouble max)
{
  if(min > max) return;

  GTK_PLOT(plot)->ymin = min;
  GTK_PLOT(plot)->ymax = max;
  plot->ay->min = min;
  plot->ay->max = max;
}

void
gtk_plot3d_set_zrange(GtkPlot3D *plot, gdouble min, gdouble max)
{
  if(min > max) return;

  plot->zmin = min;
  plot->zmax = max;
  plot->az->min = min;
  plot->az->max = max;
}

void
gtk_plot3d_set_xfactor(GtkPlot3D *plot, gdouble xfactor)
{
  if(xfactor <= 0.0) return;

  plot->e1.x /= plot->xfactor;
  plot->e1.y /= plot->xfactor;
  plot->e1.z /= plot->xfactor;

  plot->xfactor = xfactor;
  
  plot->e1.x *= plot->xfactor;
  plot->e1.y *= plot->xfactor;
  plot->e1.z *= plot->xfactor;

  plot->ax->direction = plot->e1;
}

void
gtk_plot3d_set_yfactor(GtkPlot3D *plot, gdouble yfactor)
{
  if(yfactor <= 0.0) return;

  plot->e2.x /= plot->yfactor;
  plot->e2.y /= plot->yfactor;
  plot->e2.z /= plot->yfactor;

  plot->yfactor = yfactor;
  
  plot->e2.x *= plot->yfactor;
  plot->e2.y *= plot->yfactor;
  plot->e2.z *= plot->yfactor;

  plot->ay->direction = plot->e1;
}

void
gtk_plot3d_set_zfactor(GtkPlot3D *plot, gdouble zfactor)
{
  if(zfactor <= 0.0) return;

  plot->e3.x /= plot->zfactor;
  plot->e3.y /= plot->zfactor;
  plot->e3.z /= plot->zfactor;

  plot->zfactor = zfactor;
  
  plot->e3.x *= plot->zfactor;
  plot->e3.y *= plot->zfactor;
  plot->e3.z *= plot->zfactor;

  plot->az->direction = plot->e1;
}

gdouble
gtk_plot3d_get_xfactor(GtkPlot3D *plot)
{
  return (plot->xfactor);
}

gdouble
gtk_plot3d_get_yfactor(GtkPlot3D *plot)
{
  return (plot->yfactor);
}

gdouble
gtk_plot3d_get_zfactor(GtkPlot3D *plot)
{
  return (plot->zfactor);
}


/***********************************************************
 * gtk_plot3d_plane_set_color
 * gtk_plot3d_plane_set_visible
 * gtk_plot3d_plane_visible
 ***********************************************************/

void            
gtk_plot3d_plane_set_color      (GtkPlot3D *plot,
                                 GtkPlotPlane plane,
                                 const GdkColor *color)
{

  switch(plane){
    case GTK_PLOT_PLANE_XY:
      plot->color_xy = *color;
      break;
    case GTK_PLOT_PLANE_YZ:
      plot->color_yz = *color;
      break;
    case GTK_PLOT_PLANE_ZX:
      plot->color_zx = *color;
      break;
    default:
      break;
  }    

}

void            
gtk_plot3d_plane_set_visible    (GtkPlot3D *plot,
                                 GtkPlotPlane plane,
                                 gboolean visible)
{

  switch(plane){
    case GTK_PLOT_PLANE_XY:
      plot->xy_visible = visible;
      break;
    case GTK_PLOT_PLANE_YZ:
      plot->yz_visible = visible;
      break;
    case GTK_PLOT_PLANE_ZX:
      plot->zx_visible = visible;
      break;
    default:
      break;
  }    

}

gboolean        
gtk_plot3d_plane_visible    (GtkPlot3D *plot,
                                 GtkPlotPlane plane)
{
  gboolean visible = FALSE;

  switch(plane){
    case GTK_PLOT_PLANE_XY:
      visible = plot->xy_visible;
      break;
    case GTK_PLOT_PLANE_YZ:
      visible = plot->yz_visible;
      break;
    case GTK_PLOT_PLANE_ZX:
      visible = plot->zx_visible;
      break;
    default:
      break;
  }    

  return visible;
}


/***********************************************************
 * gtk_plot3d_corner_set_visible     
 * gtk_plot3d_corner_visible     
 * gtk_plot3d_corner_set_attributes     
 * gtk_plot3d_corner_get_attributes     
 * gtk_plot3d_frame_set_attributes     
 * gtk_plot3d_frame_get_attributes     
 * gtk_plot3d_axis_show_labels     
 * gtk_plot3d_axis_show_title     
 * gtk_plot3d_axis_hide_title     
 * gtk_plot3d_axis_show_major_ticks 
 * gtk_plot3d_axis_show_minor_ticks 
 * gtk_plot3d_axis_set_ticks 
 * gtk_plot3d_axis_set_major_ticks 
 * gtk_plot3d_axis_set_minor_ticks 
 * gtk_plot3d_axis_set_ticks_length
 * gtk_plot3d_axis_set_ticks_width
 * gtk_plot3d_axis_show_ticks
 * gtk_plot3d_set_titles_offset
 * gtk_plot3d_get_titles_offset
 ***********************************************************/
void
gtk_plot3d_corner_set_visible(GtkPlot3D *plot, gboolean visible)
{
  plot->corner_visible = visible;
}

gboolean
gtk_plot3d_corner_visible(GtkPlot3D *plot)
{
  return (plot->corner_visible);
}

void            
gtk_plot3d_corner_set_attributes   (GtkPlot3D *plot,
                                    GtkPlotLineStyle style,
                                    gfloat width,
                                    const GdkColor *color)
{
  plot->corner.line_style = style;
  plot->corner.line_width = width;
  if(color) plot->corner.color = *color;
}


void            
gtk_plot3d_corner_get_attributes   (GtkPlot3D *plot,
                                    GtkPlotLineStyle *style,
                                    gfloat *width,
                                    GdkColor *color)
{
  *style = plot->corner.line_style;
  *width = plot->corner.line_width;
  *color = plot->corner.color;
}

void            
gtk_plot3d_frame_set_attributes   (GtkPlot3D *plot,
                                   GtkPlotLineStyle style,
                                   gfloat width,
                                   const GdkColor *color)
{
  plot->frame.line_style = style;
  plot->frame.line_width = width;
  if(color) plot->frame.color = *color;
}

void            
gtk_plot3d_frame_get_attributes   (GtkPlot3D *plot,
                                   GtkPlotLineStyle *style,
                                   gfloat *width,
                                   GdkColor *color)
{
  *style = plot->frame.line_style;
  *width = plot->frame.line_width;
  *color = plot->frame.color;
}

GtkPlotAxis *
gtk_plot3d_get_axis(GtkPlot3D *plot, GtkPlotOrientation orientation)
{
  GtkPlotAxis *axis = NULL;

  switch(orientation){
    case GTK_PLOT_AXIS_X:
      axis = plot->ax;
      break;
    case GTK_PLOT_AXIS_Y:
      axis = plot->ay;
      break;
    case GTK_PLOT_AXIS_Z:
      axis = plot->az;
      break;
    default:
      axis = NULL;
      break;
  }

  return axis;
} 

GtkPlotAxis *
gtk_plot3d_get_side(GtkPlot3D *plot, GtkPlotSide side)
{
  GtkPlotAxis *axis = NULL;

  switch(side){
    case GTK_PLOT_SIDE_XY:
      axis = &plot->xy;
      break;
    case GTK_PLOT_SIDE_XZ:
      axis = &plot->xz;
      break;
    case GTK_PLOT_SIDE_YX:
      axis = &plot->yx;
      break;
    case GTK_PLOT_SIDE_YZ:
      axis = &plot->yz;
      break;
    case GTK_PLOT_SIDE_ZX:
      axis = &plot->zx;
      break;
    case GTK_PLOT_SIDE_ZY:
      axis = &plot->zy;
      break;
    default:
      axis = NULL;
  }

  return axis;
}

void            
gtk_plot3d_axis_show_labels     (GtkPlot3D *plot,
			         GtkPlotSide side,
                                 gint label_mask)
{
  GtkPlotAxis *axis;

  axis = gtk_plot3d_get_side(plot, side);

  axis->label_mask = label_mask;
}

void            
gtk_plot3d_axis_show_title     (GtkPlot3D *plot,
			        GtkPlotSide side)
{
  GtkPlotAxis *axis;

  axis = gtk_plot3d_get_side(plot, side);

  axis->title_visible = TRUE;
}

void            
gtk_plot3d_axis_hide_title     (GtkPlot3D *plot,
			        GtkPlotSide side)
{
  GtkPlotAxis *axis;

  axis = gtk_plot3d_get_side(plot, side);

  axis->title_visible = FALSE;
}


void            
gtk_plot3d_axis_show_major_ticks(GtkPlot3D *plot,
			         GtkPlotSide side,
                                 gint ticks_mask)
{
  GtkPlotAxis *axis;

  axis = gtk_plot3d_get_side(plot, side);

  axis->major_mask = ticks_mask;
}

void            
gtk_plot3d_axis_show_minor_ticks(GtkPlot3D *plot,
			         GtkPlotSide side,
                                 gint ticks_mask)
{
  GtkPlotAxis *axis;

  axis = gtk_plot3d_get_side(plot, side);

  axis->minor_mask = ticks_mask;
}

void
gtk_plot3d_axis_set_ticks        (GtkPlot3D *plot,
				  GtkPlotOrientation direction,
                                  gdouble major_step,
                                  gint nminor)
{
  GtkPlotAxis *axis;

  axis = gtk_plot3d_get_axis(plot, direction);

  axis->ticks.step = major_step;

  axis->ticks.nminor = nminor;
}

void
gtk_plot3d_axis_set_major_ticks  (GtkPlot3D *plot,
                                  GtkPlotOrientation direction,
                                  gdouble major_step)
{
  GtkPlotAxis *axis;

  axis = gtk_plot3d_get_axis(plot, direction);

  axis->ticks.step = major_step;
}

void
gtk_plot3d_axis_set_minor_ticks  (GtkPlot3D *plot,
                                  GtkPlotOrientation direction,
                                  gint nminor)
{
  GtkPlotAxis *axis;

  axis = gtk_plot3d_get_axis(plot, direction);

  axis->ticks.nminor = nminor;
}

void            
gtk_plot3d_axis_set_ticks_length  (GtkPlot3D *plot,
                                   GtkPlotOrientation direction,
                                   gint length)
{
  GtkPlotAxis *axis;

  axis = gtk_plot3d_get_axis(plot, direction);

  axis->ticks_length = length;
}

void            
gtk_plot3d_axis_set_ticks_width   (GtkPlot3D *plot,
                                   GtkPlotOrientation direction,
                                   gfloat width)
{
  GtkPlotAxis *axis;

  axis = gtk_plot3d_get_axis(plot, direction);

  axis->ticks_width = width;
}

void            
gtk_plot3d_axis_show_ticks        (GtkPlot3D *plot,
                                   GtkPlotSide side,
                                   gint major_mask,
                                   gint minor_mask)
{
  GtkPlotAxis *axis;

  axis = gtk_plot3d_get_side(plot, side);

  axis->major_mask = major_mask;
  axis->minor_mask = minor_mask;
}


void
gtk_plot3d_set_titles_offset    (GtkPlot3D *plot,
                                 gint offset)
{
  plot->titles_offset = offset;
}

gint            
gtk_plot3d_get_titles_offset    (GtkPlot3D *plot)
{
  return (plot->titles_offset);
}

/***********************************************************
 * gtk_plot3d_major_grids_set_visible     
 * gtk_plot3d_major_grids_visible     
 * gtk_plot3d_minor_grids_set_visible     
 * gtk_plot3d_minor_grids_visible     
 * gtk_plot3d_major_grids_set_attributes     
 * gtk_plot3d_minor_grids_set_attributes     
 * gtk_plot3d_major_grids_get_attributes     
 * gtk_plot3d_minor_grids_get_attributes     
 ***********************************************************/

void            
gtk_plot3d_major_grids_set_visible    (GtkPlot3D *plot,
                                       gboolean x,
                                       gboolean y,
                                       gboolean z)
{
  plot->ax->show_major_grid = x;
  plot->ay->show_major_grid = y;
  plot->az->show_major_grid = z;
}

void            
gtk_plot3d_minor_grids_set_visible    (GtkPlot3D *plot,
                                       gboolean x,
                                       gboolean y,
                                       gboolean z)
{
  plot->ax->show_minor_grid = x;
  plot->ay->show_minor_grid = y;
  plot->az->show_minor_grid = z;
}

void            
gtk_plot3d_major_grids_visible    (GtkPlot3D *plot,
                                       gboolean *x,
                                       gboolean *y,
                                       gboolean *z)
{
  *x = plot->ax->show_major_grid;
  *y = plot->ay->show_major_grid;
  *z = plot->az->show_major_grid;
}

void            
gtk_plot3d_minor_grids_visible    (GtkPlot3D *plot,
                                       gboolean *x,
                                       gboolean *y,
                                       gboolean *z)
{
  *x = plot->ax->show_minor_grid;
  *y = plot->ay->show_minor_grid;
  *z = plot->az->show_minor_grid;
}

void            
gtk_plot3d_major_zgrid_set_attributes    (GtkPlot3D *plot,
                                         GtkPlotLineStyle style,
                                         gfloat width,
                                         const GdkColor *color)
{
  plot->az->major_grid.line_style = style;
  plot->az->major_grid.line_width = width;
  if(color) plot->az->major_grid.color = *color;
}

void            
gtk_plot3d_minor_zgrid_set_attributes    (GtkPlot3D *plot,
                                         GtkPlotLineStyle style,
                                         gfloat width,
                                         const GdkColor *color)
{
  plot->az->minor_grid.line_style = style;
  plot->az->minor_grid.line_width = width;
  if(color) plot->az->minor_grid.color = *color;
}

void            
gtk_plot3d_major_zgrid_get_attributes    (GtkPlot3D *plot,
                                         GtkPlotLineStyle *style,
                                         gfloat *width,
                                         GdkColor *color)
{
  *style = plot->az->major_grid.line_style;
  *width = plot->az->major_grid.line_width;
  *color = plot->az->major_grid.color;
}

void            
gtk_plot3d_minor_zgrid_get_attributes    (GtkPlot3D *plot,
                                         GtkPlotLineStyle *style,
                                         gfloat *width,
                                         GdkColor *color)
{
  *style = plot->az->minor_grid.line_style;
  *width = plot->az->minor_grid.line_width;
  *color = plot->az->minor_grid.color;
}

/******************************************* 
 * gtk_plot3d_reset_angles                     
 * gtk_plot3d_rotate_x                     
 * gtk_plot3d_rotate_y                     
 * gtk_plot3d_rotate_z                     
 *******************************************/
void
gtk_plot3d_reset_angles(GtkPlot3D *plot)
{
  plot->e1.x = 0.;
  plot->e1.y = 0.;
  plot->e1.z = 1.;
  plot->e2.x = 1.;
  plot->e2.y = 0.;
  plot->e2.z = 0.;
  plot->e3.x = 0.;
  plot->e3.y = -1.;
  plot->e3.z = 0.;
}

void
gtk_plot3d_rotate_x(GtkPlot3D *plot, gdouble angle)
{
  GtkPlotVector vector, aux, e1, e2, e3;
  gdouble c, s;
 
  angle = -angle * PI / 180.0;
  c = cos(angle);
  s = sin(angle);
 
  e1 = plot->e1;
  e2 = plot->e2;
  e3 = plot->e3;
 
  vector.x = 0.0;
  vector.y = 1.0;
  vector.z = 0.0;
  aux = vector;
  aux.y = vector.y*c - vector.z*s;
  aux.z = vector.z*c + vector.y*s;
 
  plot->e2.x = aux.x * e1.x + aux.y * e2.x + aux.z * e3.x;
  plot->e2.y = aux.x * e1.y + aux.y * e2.y + aux.z * e3.y;
  plot->e2.z = aux.x * e1.z + aux.y * e2.z + aux.z * e3.z;
 
  vector.x = 0.0;
  vector.y = 0.0;
  vector.z = 1.0;
  aux = vector;
  aux.y = vector.y*c - vector.z*s;
  aux.z = vector.z*c + vector.y*s;
 
  plot->e3.x = aux.x * e1.x + aux.y * e2.x + aux.z * e3.x;
  plot->e3.y = aux.x * e1.y + aux.y * e2.y + aux.z * e3.y;
  plot->e3.z = aux.x * e1.z + aux.y * e2.z + aux.z * e3.z;
}

void
gtk_plot3d_rotate_y(GtkPlot3D *plot, gdouble angle)
{
  GtkPlotVector vector, aux, e1, e2, e3;
  gdouble  c, s;
 
  angle = -angle * PI / 180.0;
  c = cos(angle);
  s = sin(angle);

  e1 = plot->e1;
  e2 = plot->e2;
  e3 = plot->e3;

  vector.x = 1.0;
  vector.y = 0.0;
  vector.z = 0.0;
  aux = vector;
  aux.z = vector.z*c - vector.x*s;
  aux.x = vector.x*c + vector.z*s;
 
  plot->e1.x = aux.x * e1.x + aux.y * e2.x + aux.z * e3.x;
  plot->e1.y = aux.x * e1.y + aux.y * e2.y + aux.z * e3.y;
  plot->e1.z = aux.x * e1.z + aux.y * e2.z + aux.z * e3.z;
 
  vector.x = 0.0;
  vector.y = 0.0;
  vector.z = 1.0;
  aux = vector;
  aux.z = vector.z*c - vector.x*s;
  aux.x = vector.x*c + vector.z*s;
 
  plot->e3.x = aux.x * e1.x + aux.y * e2.x + aux.z * e3.x;
  plot->e3.y = aux.x * e1.y + aux.y * e2.y + aux.z * e3.y;
  plot->e3.z = aux.x * e1.z + aux.y * e2.z + aux.z * e3.z;
}

void
gtk_plot3d_rotate_z(GtkPlot3D *plot, gdouble angle)
{
  GtkPlotVector vector, aux, e1, e2, e3;
  gdouble c, s;
 
  angle = -angle * PI / 180.0;
  c = cos(angle);
  s = sin(angle);
 
  e1 = plot->e1;
  e2 = plot->e2;
  e3 = plot->e3;
 
  vector.x = 1.0;
  vector.y = 0.0;
  vector.z = 0.0;
  aux = vector;
  aux.x = vector.x*c - vector.y*s;
  aux.y = vector.y*c + vector.x*s;
 
  plot->e1.x = aux.x * e1.x + aux.y * e2.x + aux.z * e3.x;
  plot->e1.y = aux.x * e1.y + aux.y * e2.y + aux.z * e3.y;
  plot->e1.z = aux.x * e1.z + aux.y * e2.z + aux.z * e3.z;
 
  vector.x = 0.0;
  vector.y = 1.0;
  vector.z = 0.0;
  aux = vector;
  aux.x = vector.x*c - vector.y*s;
  aux.y = vector.y*c + vector.x*s;
 
  plot->e2.x = aux.x * e1.x + aux.y * e2.x + aux.z * e3.x;
  plot->e2.y = aux.x * e1.y + aux.y * e2.y + aux.z * e3.y;
  plot->e2.z = aux.x * e1.z + aux.y * e2.z + aux.z * e3.z;
}
