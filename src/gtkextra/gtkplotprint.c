/* gtkplotprint - gtkplot printing functions
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <time.h>
#include <gtk/gtk.h>

#include "gtkplotpc.h"
#include "gtkplot.h"
#include "gtkplot3d.h"
#include "gtkplotdata.h"
#include "gtkpsfont.h"
#include "gtkplotpc.h"
#include "gtkplotps.h"
#include "gtkplotdt.h"
#include "gtkplotsurface.h"
#include "gtkplotcanvas.h"

static void recalc_pixels(GtkPlot *plot);

gboolean
gtk_plot_export_ps                              (GtkPlot *plot,
                                                 char *psname,
                                                 GtkPlotPageOrientation orient,
                                                 gboolean epsflag,
                                                 GtkPlotPageSize page_size)
{
  GtkPlotPC *pc;
  GtkPlotPS *ps;
  gdouble scalex, scaley;
  gdouble m;


  m = plot->magnification;

  ps = GTK_PLOT_PS(gtk_plot_ps_new(psname, orient, epsflag, page_size, 1.0, 1.0));

  if(orient == GTK_PLOT_PORTRAIT){
    scalex = (gfloat)ps->page_width /
                                (gfloat)GTK_WIDGET(plot)->allocation.width;
    scaley = (gfloat)ps->page_height /
                                (gfloat)GTK_WIDGET(plot)->allocation.height;
  }else{
    scalex = (gfloat)ps->page_width /
                                (gfloat)GTK_WIDGET(plot)->allocation.height;
    scaley = (gfloat)ps->page_height /
                                (gfloat)GTK_WIDGET(plot)->allocation.width;
  }

  gtk_plot_ps_set_scale(ps, scalex, scaley);

  pc = plot->pc;

  plot->pc = GTK_PLOT_PC(ps);
  plot->magnification = 1.0;
  recalc_pixels(plot); 

  gtk_plot_paint(plot);

  plot->pc = pc;
  plot->magnification = m;
  gtk_object_destroy(GTK_OBJECT(ps));
  recalc_pixels(plot); 

  return TRUE;
}

gboolean
gtk_plot_export_ps_with_size                    (GtkPlot *plot,
                                                 char *psname,
                                                 GtkPlotPageOrientation orient,
                                                 gboolean epsflag,
                                                 GtkPlotUnits units,
                                                 gint width,
                                                 gint height)
{
  GtkPlotPC *pc;
  GtkPlotPS *ps;
  gdouble scalex, scaley;
  gdouble m;

  m = plot->magnification;

  ps = GTK_PLOT_PS(gtk_plot_ps_new_with_size(psname, orient, epsflag, 
                                             units, 
                                             width, height, 
                                             1.0 , 1.0));

  if(orient == GTK_PLOT_PORTRAIT){
    scalex = (gfloat)ps->page_width /
                                (gfloat)GTK_WIDGET(plot)->allocation.width;
    scaley = (gfloat)ps->page_height /
                                (gfloat)GTK_WIDGET(plot)->allocation.height;
  }else{
    scalex = (gfloat)ps->page_width /
                                (gfloat)GTK_WIDGET(plot)->allocation.height;
    scaley = (gfloat)ps->page_height /
                                (gfloat)GTK_WIDGET(plot)->allocation.width;
  }

  gtk_plot_ps_set_scale(ps, scalex, scaley);

  pc = plot->pc;
 
  plot->pc = GTK_PLOT_PC(ps);
  plot->magnification = 1.0;
  recalc_pixels(plot); 

  gtk_plot_paint(plot);

  plot->pc = pc;
  plot->magnification = m;
  recalc_pixels(plot); 
  gtk_object_destroy(GTK_OBJECT(ps));

  return TRUE;
}

gboolean
gtk_plot_canvas_export_ps                       (GtkPlotCanvas *canvas,
                                                 char *psname,
                                                 GtkPlotPageOrientation orient,
                                                 gboolean epsflag,
                                                 GtkPlotPageSize page_size)
{
  GtkPlotPC *pc;
  GtkPlotPS *ps;
  GList *plots;
  gint old_width, old_height;
  gdouble scalex, scaley;
  gdouble m;
  GtkAllocation allocation[100];
  GtkAllocation canvas_allocation;
  GtkPlotPC *aux_pc[100];
  gint n = 0;

  m = canvas->magnification;

  ps = GTK_PLOT_PS(gtk_plot_ps_new(psname, orient, epsflag, page_size, 1.0, 1.0));

  if(orient == GTK_PLOT_PORTRAIT){
    scalex = (gfloat)ps->page_width / (gfloat)canvas->width;
    scaley = (gfloat)ps->page_height / (gfloat)canvas->height;
  }else{
    scalex = (gfloat)ps->page_width / (gfloat)canvas->height;
    scaley = (gfloat)ps->page_height / (gfloat)canvas->width;
  }

  gtk_plot_ps_set_scale(ps, scalex, scaley);

  old_width = canvas->pixmap_width;
  old_height = canvas->pixmap_height;

  pc = canvas->pc;
  canvas->pc = GTK_PLOT_PC(ps);
  canvas->magnification = 1.0;
  canvas->pixmap_width = canvas->width;
  canvas->pixmap_height = canvas->height;

  canvas_allocation.x = canvas_allocation.y = 0;
  canvas_allocation.width = canvas->width;
  canvas_allocation.height = canvas->height;

  plots = canvas->plots;
  while(plots){
    GtkPlot *plot;

    plot = GTK_PLOT(plots->data);

    plot->magnification = 1.0;

    allocation[n] = GTK_WIDGET(plot)->allocation;
    aux_pc[n] = plot->pc;
    plot->pc = pc;
    n++;

    gtk_widget_size_allocate(GTK_WIDGET(plot), &canvas_allocation);
    recalc_pixels(plot);

    plots= plots->next;
  }

  gtk_plot_canvas_paint(canvas);

  canvas->pc = pc;
  canvas->magnification = m;
  canvas->pixmap_width = old_width;
  canvas->pixmap_height = old_height;

  n = 0;
  plots = canvas->plots;
  while(plots){
    GtkPlot *plot;
   
    plot = GTK_PLOT(plots->data);

    plot->magnification = m;
    gtk_widget_size_allocate(GTK_WIDGET(plot), &allocation[n]);
    plot->pc = aux_pc[n];
    recalc_pixels(plot); 
    n++;
    
    plots= plots->next;
  }

  gtk_object_destroy(GTK_OBJECT(ps));

  return TRUE;
}

gboolean
gtk_plot_canvas_export_ps_with_size             (GtkPlotCanvas *canvas,
                                                 char *psname,
                                                 GtkPlotPageOrientation orient,
                                                 gboolean epsflag,
                                                 GtkPlotUnits units,
                                                 gint width,
                                                 gint height)
{
  GtkPlotPC *pc;
  GtkPlotPS *ps;
  GList *plots;
  GtkAllocation allocation[100];
  GtkAllocation canvas_allocation;
  GtkPlotPC *aux_pc[100];
  gint old_width, old_height;
  gdouble scalex, scaley;
  gdouble m;
  gint n = 0;

  m = canvas->magnification;

  ps = GTK_PLOT_PS(gtk_plot_ps_new_with_size(psname, orient, epsflag, 
                                             units, 
                                             width, height, 
                                             1.0 , 1.0));

  if(orient == GTK_PLOT_PORTRAIT){
    scalex = (gfloat)ps->page_width / (gfloat)canvas->width;
    scaley = (gfloat)ps->page_height / (gfloat)canvas->height;
  }else{
    scalex = (gfloat)ps->page_width / (gfloat)canvas->height;
    scaley = (gfloat)ps->page_height / (gfloat)canvas->width;
  }

  gtk_plot_ps_set_scale(ps, scalex, scaley);

  old_width = canvas->pixmap_width;
  old_height = canvas->pixmap_height;

  pc = canvas->pc;
  canvas->pc = GTK_PLOT_PC(ps);
  canvas->magnification = 1.0;
  canvas->pixmap_width = canvas->width;
  canvas->pixmap_height = canvas->height;

  canvas_allocation.x = canvas_allocation.y = 0;
  canvas_allocation.width = canvas->width;
  canvas_allocation.height = canvas->height;

  plots = canvas->plots;
  while(plots){
    GtkPlot *plot;
   
    plot = GTK_PLOT(plots->data);

    plot->magnification = 1.0;
    allocation[n] = GTK_WIDGET(plot)->allocation;
    aux_pc[n] = plot->pc;
    plot->pc = pc;
    n++;


    gtk_widget_size_allocate(GTK_WIDGET(plot), &canvas_allocation);
    recalc_pixels(plot); 
    
    plots= plots->next;
  }

  gtk_plot_canvas_paint(canvas);

  canvas->pc = pc;
  canvas->magnification = m;
  canvas->pixmap_width = old_width;
  canvas->pixmap_height = old_height;

  n = 0;
  plots = canvas->plots;
  while(plots){
    GtkPlot *plot;
   
    plot = GTK_PLOT(plots->data);

    plot->magnification = m;
    gtk_widget_size_allocate(GTK_WIDGET(plot), &allocation[n]);
    plot->pc = aux_pc[n];
    recalc_pixels(plot); 
    n++;
    
    plots= plots->next;
  }

  gtk_object_destroy(GTK_OBJECT(ps));

  return TRUE;
}

static void
recalc_pixels(GtkPlot *plot)
{
  GList *list;
  
  list = plot->data_sets;
  while(list){
    GtkPlotData *data;
    data = GTK_PLOT_DATA(list->data);
    if(GTK_IS_PLOT_SURFACE(data)){
      GtkPlotSurface *surface = GTK_PLOT_SURFACE(data);
      gint i;

      for(i = surface->dt->node_0; i < surface->dt->node_cnt; i++){
        GtkPlotDTnode *node;
        node = gtk_plot_dt_get_node(surface->dt,i);
        if(GTK_IS_PLOT3D(plot)){
          gtk_plot3d_get_pixel(GTK_PLOT3D(plot),
                               node->x, node->y, node->z,
                               &node->px, &node->py, &node->pz);
        } else {
          gtk_plot_get_pixel(plot,
                             node->x, node->y,
                             &node->px, &node->py);
          node->pz = 0.0;
        }
      }
    }
    list = list->next;
  }
}

