/* gtkplotsurface - 3d scientific plots widget for gtk+
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
#include "gtkplot3d.h"
#include "gtkplotdata.h"
#include "gtkplotsurface.h"
#include "gtkpsfont.h"
#include "gtkplotpc.h"


static void gtk_plot_surface_class_init 	(GtkPlotSurfaceClass *klass);
static void gtk_plot_surface_init 		(GtkPlotSurface *data);
static void gtk_plot_surface_draw 		(GtkWidget *widget, 
						 GdkRectangle *area);
static void gtk_plot_surface_draw_private 	(GtkPlotData *data);
static void gtk_plot_surface_draw_legend	(GtkPlotData *data, 
						 gint x, gint y);
static void gtk_plot_surface_real_draw		(GtkPlotSurface *data); 
static void gtk_plot_surface_draw_triangle 	(GtkPlotSurface *surface,
                                		 GtkPlotVector *points,
                                                 gint sign);
static void gtk_plot_surface_lighting 		(GdkColor *a, 
						 GdkColor *b, 
						 gdouble normal,
						 gdouble ambient);
static gint roundint				(gdouble x);
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

static GtkDataClass *parent_class = NULL;

GtkType
gtk_plot_surface_get_type (void)
{
  static GtkType data_type = 0;

  if (!data_type)
    {
      GtkTypeInfo data_info =
      {
	"GtkPlotSurface",
	sizeof (GtkPlotSurface),
	sizeof (GtkPlotSurfaceClass),
	(GtkClassInitFunc) gtk_plot_surface_class_init,
	(GtkObjectInitFunc) gtk_plot_surface_init,
	/* reserved 1*/ NULL,
        /* reserved 2 */ NULL,
        (GtkClassInitFunc) NULL,
      };

      data_type = gtk_type_unique (gtk_plot_data_get_type(), &data_info);
    }
  return data_type;
}

static void
gtk_plot_surface_class_init (GtkPlotSurfaceClass *klass)
{
  GtkObjectClass *object_class;
  GtkWidgetClass *widget_class;
  GtkPlotDataClass *data_class;
  GtkPlotSurfaceClass *surface_class;

  parent_class = gtk_type_class (gtk_plot_data_get_type ());

  object_class = (GtkObjectClass *) klass;
  widget_class = (GtkWidgetClass *) klass;
  data_class = (GtkPlotDataClass *) klass;
  surface_class = (GtkPlotSurfaceClass *) klass;

  widget_class->draw = gtk_plot_surface_draw;

  data_class->draw_data = gtk_plot_surface_draw_private;
  data_class->draw_legend = gtk_plot_surface_draw_legend;
  surface_class->draw_triangle = gtk_plot_surface_draw_triangle;

}


static void
gtk_plot_surface_init (GtkPlotSurface *dataset)
{
  GtkWidget *widget;
  GdkColormap *colormap;
  GdkColor color;

  GTK_WIDGET_SET_FLAGS(dataset, GTK_NO_WINDOW);

  widget = GTK_WIDGET(dataset);
  colormap = gtk_widget_get_colormap(widget);

  gdk_color_parse("black", &color);
  gdk_color_alloc(colormap, &color);
  dataset->grid_foreground = color;

  gdk_color_parse("dark green", &color);
  gdk_color_alloc(colormap, &color);
  dataset->grid_background = color;

  gdk_color_parse("gray30", &color);
  gdk_color_alloc(colormap, &color);
  dataset->shadow = color;

  gdk_color_parse("blue", &color);
  gdk_color_alloc(colormap, &color);
  dataset->color = color;

  dataset->light.x = 0.;
  dataset->light.y = 0.;
  dataset->light.z = 1.;

  dataset->nx = 0;
  dataset->ny = 0;

  dataset->show_grid = TRUE;
  dataset->show_mesh = FALSE;

  dataset->ambient = 0.3;

  dataset->xstep = 0.05;
  dataset->ystep = 0.05;
}

GtkWidget*
gtk_plot_surface_new (void)
{
  GtkPlotData *data;

  data = gtk_type_new (gtk_plot_surface_get_type ());

  return GTK_WIDGET (data);
}

GtkWidget*
gtk_plot_surface_new_function (GtkPlotFunc3D function)
{
  GtkWidget *dataset;

  dataset = gtk_type_new (gtk_plot_surface_get_type ());

  gtk_plot_surface_construct_function(GTK_PLOT_SURFACE(dataset), function);

  return dataset;
}

void
gtk_plot_surface_construct_function(GtkPlotSurface *surface, 
				   GtkPlotFunc3D function)
{
  GTK_PLOT_DATA(surface)->is_function = TRUE;
  GTK_PLOT_DATA(surface)->function3d = function;
}

static void
gtk_plot_surface_draw (GtkWidget *widget, GdkRectangle *area)
{
  if(!GTK_WIDGET_VISIBLE(widget)) return;

  gtk_plot_surface_draw_private(GTK_PLOT_DATA(widget));
}

static void
gtk_plot_surface_draw_private   (GtkPlotData *data)  
{
  GtkPlot *plot;
  GtkPlotSurface *surface;
  GtkPlotAxis *ax, *ay;

  g_return_if_fail(GTK_IS_PLOT_SURFACE(data));

  surface = GTK_PLOT_SURFACE(data);
  data = GTK_PLOT_DATA(surface);

  g_return_if_fail(GTK_PLOT_DATA(data)->plot != NULL);
  g_return_if_fail(GTK_IS_PLOT(GTK_PLOT_DATA(data)->plot));
  g_return_if_fail(GTK_WIDGET_REALIZED(GTK_PLOT_DATA(data)->plot));

  plot = GTK_PLOT(data->plot);

  if(GTK_IS_PLOT3D(plot)){
     ax = GTK_PLOT3D(plot)->ax;
     ay = GTK_PLOT3D(plot)->ay;
  }else{
     ax = plot->bottom;
     ay = plot->left;
  }

  if(!data->is_function)
     gtk_plot_surface_real_draw(surface);
  else
   {
     gdouble xstep, ystep;
     gdouble xmin, xmax, ymin, ymax;
     gdouble x, y;
     gdouble *fx = NULL, *fy = NULL, *fz = NULL;
     gdouble *old_x, *old_y, *old_z;
     gint nx, ny;
     gint npoints;

     xmin = GTK_PLOT(plot)->xmin;
     xmax = GTK_PLOT(plot)->xmax;
     ymin = GTK_PLOT(plot)->ymin;
     ymax = GTK_PLOT(plot)->ymax;

     xstep = surface->xstep;
     surface->nx = roundint((xmax - xmin) / xstep) + 1;

     ystep = surface->ystep;
     surface->ny = roundint((ymax - ymin) / ystep) + 1;

     npoints = surface->nx * surface->ny;
     fx = (gdouble *)g_malloc((npoints + 1) * sizeof(gdouble));
     fy = (gdouble *)g_malloc((npoints + 1) * sizeof(gdouble));
     fz = (gdouble *)g_malloc((npoints + 1) * sizeof(gdouble));

     npoints = 0;
     y = ymin;
     for(ny = 0; ny < surface->ny; ny++)
       {
         x = xmin;
         for(nx = 0; nx < surface->nx; nx++)
          {
            gboolean error;
            fx[npoints] = x;
            fy[npoints] = y;
            fz[npoints] = data->function3d(plot, data, x, y, &error);

            x += xstep;
            npoints++;
          }
         y += ystep;
     }

     old_x = data->x; old_y = data->y; old_z = data->z;

     data->x = fx;
     data->y = fy;
     data->z = fz;

     gtk_plot_surface_real_draw(surface);

     g_free(fx);
     g_free(fy);
     g_free(fz);

     data->x = old_x; data->y = old_y; data->z = old_z;
   }

}

static void
gtk_plot_surface_real_draw   (GtkPlotSurface *surface)  
{
  GtkWidget *widget;
  GtkPlot *plot = NULL;
  GtkPlotData *dataset;
  GdkRectangle area;
  gint i;
  gint nx, ny;
  gdouble x1, y1, z1;
  gdouble x2, y2, z2;
  gint xa, xb, xstep;
  gint ya, yb, ystep;
  gdouble m;

  g_return_if_fail(GTK_IS_PLOT_SURFACE(surface));

  dataset = GTK_PLOT_DATA(surface);

  g_return_if_fail(GTK_PLOT_DATA(dataset)->plot != NULL);
  g_return_if_fail(GTK_IS_PLOT(GTK_PLOT_DATA(dataset)->plot));
  g_return_if_fail(GTK_WIDGET_REALIZED(GTK_PLOT_DATA(dataset)->plot));

  plot = dataset->plot;
  widget = GTK_WIDGET(plot);

  gtk_plot_pc_gsave(plot->pc);

  if(!GTK_WIDGET_DRAWABLE(widget)) return;
  if(!GTK_WIDGET_VISIBLE(widget)) return;

  m = plot->magnification;

  area.x = widget->allocation.x;
  area.y = widget->allocation.y;
  area.width = widget->allocation.width;
  area.height = widget->allocation.height;

  xa = 0;
  xb = surface->nx - 2;
  xstep = 1;
  ya = 0;
  yb = surface->ny - 2;
  ystep = 1;

  gtk_plot_pc_set_lineattr(plot->pc, 1, 0, 0, 0);

  if(GTK_IS_PLOT3D(plot)){
    gtk_plot3d_get_pixel(GTK_PLOT3D(plot), 
                         dataset->x[0],
                         dataset->y[0],
                         0.,
                         &x1, &y1, &z1);
    gtk_plot3d_get_pixel(GTK_PLOT3D(plot), 
                         dataset->x[(surface->ny-1)*surface->nx],
                         dataset->y[(surface->ny-1)*surface->nx],
                         0.,
                         &x2, &y2, &z2);

    if(z2 > z1){
      ya = surface->ny - 1;
      yb = 1;
      ystep = -1;
    } 

    gtk_plot3d_get_pixel(GTK_PLOT3D(plot), 
                         dataset->x[surface->nx-1],
                         dataset->y[surface->nx-1],
                         0.,
                         &x2, &y2, &z2);

    if(z2 > z1){
      xa = surface->nx - 1;
      xb = 1;
      xstep = -1;
    } 
  }

  for(nx = xa; nx != xb + xstep; nx += xstep)
    for(ny = ya; ny != yb + ystep; ny += ystep)
    {
      GtkPlotVector center;
      GtkPlotVector v[4];
      gint n[4];

      n[0] = ny*surface->nx + nx;
      n[1] = ny*surface->nx + nx + xstep;
      n[2] = (ny + ystep)*surface->nx + nx + xstep;
      n[3] = (ny + ystep)*surface->nx + nx;

      center.x = center.y = center.z = 0.0;

      for(i = 0; i < 4; i++){
        v[i].x = dataset->x[n[i]];
        v[i].y = dataset->y[n[i]];
        v[i].z = dataset->z[n[i]];

        center.x = center.x + v[i].x;
        center.y = center.y + v[i].y;
        center.z = center.z + v[i].z;
      }

      center.x /= 4.0;
      center.y /= 4.0;
      center.z /= 4.0;

      for(i = 0; i < 4; i++){
         GtkPlotVector point[3];
         gint next;

         next = (i == 3) ? 0 : i + 1;

         point[0] = center;
         point[1] = v[i];
         point[2] = v[next];

         GTK_PLOT_SURFACE_CLASS(GTK_OBJECT(surface)->klass)->draw_triangle(surface, point, -xstep*ystep); 
      }
    }

  gtk_plot_pc_grestore(plot->pc);
}

static void
gtk_plot_surface_draw_triangle (GtkPlotSurface *surface,
                                GtkPlotVector *points,
                                gint sign)
{
  GtkPlot *plot;
  GtkPlotVector side1, side2, light, normal;
  GdkDrawable *drawable;
  GtkPlotPoint t[3];
  GdkColor color;
  gint i;
  gdouble factor, norm;
  gboolean visible = TRUE;

  plot = GTK_PLOT(GTK_PLOT_DATA(surface)->plot);
  drawable = plot->drawable;

  color = surface->color; 

  light = surface->light;
  norm = sqrt(light.x*light.x + light.y*light.y + light.z*light.z); 
  light.x /= norm;
  light.y /= norm;
  light.z /= norm;

  for(i = 0; i < 3; i++){
    gdouble x, y, z;
    if(GTK_IS_PLOT3D(plot))
      gtk_plot3d_get_pixel(GTK_PLOT3D(plot), 
                           points[i].x, points[i].y, points[i].z,
                           &x, &y, &z);
    else
      gtk_plot_get_pixel(plot, 
                         points[i].x, points[i].y, &x, &y);

    t[i].x = x;
    t[i].y = y;
  }

  side1.x = (points[1].x - points[0].x) * sign;
  side1.y = (points[1].y - points[0].y) * sign;
  side1.z = (points[1].z - points[0].z) * sign;
  side2.x = (points[2].x - points[0].x) * sign;
  side2.y = (points[2].y - points[0].y) * sign;
  side2.z = (points[2].z - points[0].z) * sign;
         
  normal.x = side1.y * side2.z - side1.z * side2.y;
  normal.y = side1.z * side2.x - side1.x * side2.z;
  normal.z = side1.x * side2.y - side1.y * side2.x;

  norm = sqrt(normal.x*normal.x + normal.y*normal.y + normal.z*normal.z); 
  factor = (normal.x*light.x + normal.y*light.y + normal.z*light.z) / norm;

  gtk_plot_surface_lighting(&surface->color, &color, 
                            factor, surface->ambient); 
  gdk_color_alloc(gdk_colormap_get_system(), &color);

  if(GTK_IS_PLOT3D(plot))
    if(((t[1].x-t[0].x)*(t[2].y-t[0].y) - (t[1].y-t[0].y)*(t[2].x-t[0].x))*sign > 0)
         visible = FALSE;

  if(visible)
         gtk_plot_pc_set_color(plot->pc, &color);
  else
         gtk_plot_pc_set_color(plot->pc, &surface->shadow);

  gtk_plot_pc_draw_polygon(plot->pc, TRUE, t, 3); 


  if(visible)
         gtk_plot_pc_set_color(plot->pc, &surface->grid_foreground);
  else
         gtk_plot_pc_set_color(plot->pc, &surface->grid_background);

  if(surface->show_mesh)
       gtk_plot_pc_draw_polygon(plot->pc, FALSE, t, 3); 

  if(!surface->show_mesh && surface->show_grid)
       gtk_plot_pc_draw_line(plot->pc,  
                            t[1].x, t[1].y, t[2].x, t[2].y); 
                              
}

static void
gtk_plot_surface_draw_legend(GtkPlotData *data, gint x, gint y)
{
  GtkPlotSurface *surface;
  GtkPlot *plot = NULL;
  GtkPlotText legend;
  GdkRectangle area;
  gint lascent, ldescent, lheight, lwidth;
  gdouble m;

  surface = GTK_PLOT_SURFACE(data);

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


  gtk_plot_pc_set_color(plot->pc, &surface->color);

  gtk_plot_pc_draw_rectangle(plot->pc, TRUE, 
                             x, y,
                             roundint(plot->legends_line_width * m), 
                             lascent + ldescent);


  legend.x = (gdouble)(area.x + x + roundint((plot->legends_line_width + 4) * m))
             / (gdouble)area.width;
  legend.y = (gdouble)(area.y + y + lascent) / (gdouble)area.height;

  gtk_plot_draw_text(plot, legend);
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
gtk_plot_surface_lighting (GdkColor *a, GdkColor *b, 
                           gdouble normal, gdouble ambient)
{
  gdouble red, green, blue;
  gdouble h, s, v;

  normal = MIN(fabs(normal), 1.0);

  red = a->red;
  green = a->green;
  blue = a->blue;

  rgb_to_hsv(red, green, blue, &h, &s, &v);

  s *= normal;
  v *= normal;

  s += ambient;
  v += ambient;

  hsv_to_rgb(h, MIN(s, 1.0), MIN(v, 1.0), &red, &green, &blue);

  b->red = red;
  b->green = green;
  b->blue = blue;
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


/******************************************
 * gtk_plot_surface_set_color
 * gtk_plot_surface_set_shadow
 * gtk_plot_surface_set_grid_foreground
 * gtk_plot_surface_set_grid_background
 * gtk_plot_surface_set_grid_visible
 * gtk_plot_surface_set_mesh_visible
 * gtk_plot_surface_get_grid_visible
 * gtk_plot_surface_get_mesh_visible
 ******************************************/

void            
gtk_plot_surface_set_color      (GtkPlotSurface *data,
                                 GdkColor *color)
{
  data->color = *color;
}

void            
gtk_plot_surface_set_shadow     (GtkPlotSurface *data,
                                 GdkColor *color)
{
  data->shadow = *color;
}

void            
gtk_plot_surface_set_grid_foreground    (GtkPlotSurface *data,
                                         GdkColor *foreground)
{
  data->grid_foreground = *foreground;
}

void            
gtk_plot_surface_set_grid_background    (GtkPlotSurface *data,
                                         GdkColor *background)
{
  data->grid_background = *background;
}

void            
gtk_plot_surface_set_grid_visible    (GtkPlotSurface *data,
                                         gboolean visible)
{
  data->show_grid = visible;
}

gboolean            
gtk_plot_surface_get_grid_visible    (GtkPlotSurface *data)
{
  return (data->show_grid);
}

void            
gtk_plot_surface_set_mesh_visible    (GtkPlotSurface *data,
                                         gboolean visible)
{
  data->show_mesh = visible;
}

gboolean            
gtk_plot_surface_get_mesh_visible    (GtkPlotSurface *data)
{
  return (data->show_mesh);
}

void            
gtk_plot_surface_set_light      (GtkPlotSurface *data,
                                 gdouble x, gdouble y, gdouble z)
{
  data->light.x = x;
  data->light.y = y;
  data->light.z = z;
}

void            
gtk_plot_surface_set_ambient      (GtkPlotSurface *data,
                                   gdouble ambient)
{
  data->ambient = ambient;
}

/******************************************
 * gtk_plot_surface_set_points
 * gtk_plot_surface_get_points
 * gtk_plot_surface_set_x
 * gtk_plot_surface_set_y
 * gtk_plot_surface_set_z
 * gtk_plot_surface_set_dx
 * gtk_plot_surface_set_dy
 * gtk_plot_surface_set_dz
 * gtk_plot_surface_get_x
 * gtk_plot_surface_get_y
 * gtk_plot_surface_get_z
 * gtk_plot_surface_get_dx
 * gtk_plot_surface_get_dy
 * gtk_plot_surface_get_dz
 * gtk_plot_surface_set_nx
 * gtk_plot_surface_set_ny
 * gtk_plot_surface_get_nx
 * gtk_plot_surface_get_ny
 * gtk_plot_surface_set_xstep
 * gtk_plot_surface_set_ystep
 * gtk_plot_surface_get_xstep
 * gtk_plot_surface_get_ystep
 ******************************************/

void
gtk_plot_surface_set_points(GtkPlotSurface *data, 
                         gdouble *x, gdouble *y, gdouble *z,
                         gdouble *dx, gdouble *dy, gdouble *dz,
                         gint nx, gint ny)
{
  GTK_PLOT_DATA(data)->x = x;
  GTK_PLOT_DATA(data)->y = y;
  GTK_PLOT_DATA(data)->z = z;
  GTK_PLOT_DATA(data)->dx = dx;
  GTK_PLOT_DATA(data)->dy = dy;
  GTK_PLOT_DATA(data)->dz = dz;
  data->nx = nx;
  data->ny = ny;
}

void
gtk_plot_surface_get_points(GtkPlotSurface *data, 
                            gdouble **x, gdouble **y, gdouble **z,
                            gdouble **dx, gdouble **dy, gdouble **dz,
                            gint *nx, gint *ny)
{
  *x = GTK_PLOT_DATA(data)->x;
  *y = GTK_PLOT_DATA(data)->y;
  *z = GTK_PLOT_DATA(data)->z;
  *dx = GTK_PLOT_DATA(data)->dx;
  *dy = GTK_PLOT_DATA(data)->dy;
  *dz = GTK_PLOT_DATA(data)->dz;
  *nx = data->nx;
  *ny = data->ny;
}

void
gtk_plot_surface_set_x(GtkPlotSurface *data, 
                       gdouble *x) 
{
  GTK_PLOT_DATA(data)->x = x;
}


void
gtk_plot_surface_set_y(GtkPlotSurface *data, 
                       gdouble *y) 
{
  GTK_PLOT_DATA(data)->y = y;
}

void
gtk_plot_surface_set_z(GtkPlotSurface *data, 
                       gdouble *z) 
{
  GTK_PLOT_DATA(data)->z = z;
}

void
gtk_plot_surface_set_dx(GtkPlotSurface *data, 
                        gdouble *dx) 
{
  GTK_PLOT_DATA(data)->dx = dx;
}

void
gtk_plot_surface_set_dy(GtkPlotSurface *data, 
                        gdouble *dy) 
{
  GTK_PLOT_DATA(data)->dy = dy;
}

void
gtk_plot_surface_set_dz(GtkPlotSurface *data, 
                       gdouble *dz) 
{
  GTK_PLOT_DATA(data)->dz = dz;
}

gdouble *
gtk_plot_surface_get_x(GtkPlotSurface *dataset, gint *nx)
{
  *nx = dataset->nx;
  return(GTK_PLOT_DATA(dataset)->x);
}

gdouble *
gtk_plot_surface_get_y(GtkPlotSurface *dataset, gint *ny)
{
  *ny = dataset->ny;
  return(GTK_PLOT_DATA(dataset)->y);
}

gdouble *
gtk_plot_surface_get_z(GtkPlotSurface *dataset, gint *nx, gint *ny)
{
  *nx = dataset->nx;
  *ny = dataset->ny;
  return(GTK_PLOT_DATA(dataset)->z);
}

gdouble *
gtk_plot_surface_get_dz(GtkPlotSurface *dataset)
{
  return(GTK_PLOT_DATA(dataset)->dz);
}

gdouble *
gtk_plot_surface_get_dx(GtkPlotSurface *dataset)
{
  return(GTK_PLOT_DATA(dataset)->dx);
}

gdouble *
gtk_plot_surface_get_dy(GtkPlotSurface *dataset)
{
  return(GTK_PLOT_DATA(dataset)->dy);
}

void
gtk_plot_surface_set_nx(GtkPlotSurface *dataset, gint nx)
{
  dataset->nx = nx;
}

void
gtk_plot_surface_set_ny(GtkPlotSurface *dataset, gint ny)
{
  dataset->ny = ny;
}

gint
gtk_plot_surface_get_nx(GtkPlotSurface *dataset)
{
  return(dataset->nx);
}

gint
gtk_plot_surface_get_ny(GtkPlotSurface *dataset)
{
  return(dataset->ny);
}

void
gtk_plot_surface_set_xstep(GtkPlotSurface *dataset, gdouble xstep)
{
  dataset->xstep = xstep;
}

void
gtk_plot_surface_set_ystep(GtkPlotSurface *dataset, gdouble ystep)
{
  dataset->ystep = ystep;
}

gdouble
gtk_plot_surface_get_xstep(GtkPlotSurface *dataset)
{
  return (dataset->xstep);
}

gdouble
gtk_plot_surface_get_ystep(GtkPlotSurface *dataset)
{
  return (dataset->ystep);
}
