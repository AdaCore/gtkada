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
#include "gtkpsfont.h"
#include "gtkplotpc.h"
#include "gtkplotps.h"
#include "gtkplotcanvas.h"

gboolean
gtk_plot_export_ps                              (GtkPlot *plot,
                                                 char *psname,
                                                 gint orient,
                                                 gint epsflag,
                                                 gint page_size)
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
 
  if(!gtk_plot_pc_init(plot->pc)) return FALSE; 
  gtk_plot_paint(plot);
  gtk_plot_pc_leave(plot->pc); 

  plot->pc = pc;
  plot->magnification = m;
  gtk_object_destroy(GTK_OBJECT(ps));

  return TRUE;
}

gboolean 
gtk_plot_export_ps_with_size                    (GtkPlot *plot,
                                                 char *psname,
                                                 gint orient,
                                                 gint epsflag,
                                                 gint units,
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

  if(!gtk_plot_pc_init(plot->pc)) return FALSE; 
  gtk_plot_paint(plot);
  gtk_plot_pc_leave(plot->pc); 

  plot->pc = pc;
  plot->magnification = m;
  gtk_object_destroy(GTK_OBJECT(ps));

  return TRUE;
}

gboolean
gtk_plot_canvas_export_ps                       (GtkPlotCanvas *canvas,
                                                 char *psname,
                                                 gint orient,
                                                 gint epsflag,
                                                 gint page_size)
{
  GtkPlotPC *pc;
  GtkPlotPS *ps;
  GList *plots;
  gdouble scalex, scaley;
  gdouble m;

  m = canvas->magnification;

  ps = GTK_PLOT_PS(gtk_plot_ps_new(psname, orient, epsflag, page_size, 1.0, 1.0));

  if(orient == GTK_PLOT_PORTRAIT){
    scalex = (gfloat)ps->page_width /
                                (gfloat)GTK_WIDGET(canvas)->allocation.width;
    scaley = (gfloat)ps->page_height /
                                (gfloat)GTK_WIDGET(canvas)->allocation.height;
  }else{
    scalex = (gfloat)ps->page_width /
                                (gfloat)GTK_WIDGET(canvas)->allocation.height;
    scaley = (gfloat)ps->page_height /
                                (gfloat)GTK_WIDGET(canvas)->allocation.width;
  }

  gtk_plot_ps_set_scale(ps, scalex, scaley);

  pc = canvas->pc;
  canvas->pc = GTK_PLOT_PC(ps);
  canvas->magnification = 1.0;

  plots = canvas->plots;
  while(plots){
    GtkPlot *plot;
   
    plot = GTK_PLOT(plots->data);

    plot->magnification = 1.0;
    
    plots= plots->next;
  }

  if(!gtk_plot_pc_init(canvas->pc)) return FALSE; 
  gtk_plot_canvas_paint(canvas);
  gtk_plot_pc_leave(canvas->pc); 

  canvas->pc = pc;
  canvas->magnification = m;

  plots = canvas->plots;
  while(plots){
    GtkPlot *plot;
   
    plot = GTK_PLOT(plots->data);

    plot->magnification = m;
    
    plots= plots->next;
  }

  gtk_object_destroy(GTK_OBJECT(ps));

  return TRUE;
}

gboolean
gtk_plot_canvas_export_ps_with_size             (GtkPlotCanvas *canvas,
                                                 char *psname,
                                                 gint orient,
                                                 gint epsflag,
                                                 gint units,
                                                 gint width,
                                                 gint height)
{
  GtkPlotPC *pc;
  GtkPlotPS *ps;
  GList *plots;
  gdouble scalex, scaley;
  gdouble m;

  m = canvas->magnification;

  ps = GTK_PLOT_PS(gtk_plot_ps_new_with_size(psname, orient, epsflag, 
                                             units, 
                                             width, height, 
                                             1.0 , 1.0));

  if(orient == GTK_PLOT_PORTRAIT){
    scalex = (gfloat)ps->page_width /
                                (gfloat)GTK_WIDGET(canvas)->allocation.width;
    scaley = (gfloat)ps->page_height /
                                (gfloat)GTK_WIDGET(canvas)->allocation.height;
  }else{
    scalex = (gfloat)ps->page_width /
                                (gfloat)GTK_WIDGET(canvas)->allocation.height;
    scaley = (gfloat)ps->page_height /
                                (gfloat)GTK_WIDGET(canvas)->allocation.width;
  }

  gtk_plot_ps_set_scale(ps, scalex, scaley);

  pc = canvas->pc;
  canvas->pc = GTK_PLOT_PC(ps);
  canvas->magnification = 1.0;

  plots = canvas->plots;
  while(plots){
    GtkPlot *plot;
   
    plot = GTK_PLOT(plots->data);

    plot->magnification = 1.0;
    
    plots= plots->next;
  }

  if(!gtk_plot_pc_init(canvas->pc)) return FALSE; 
  gtk_plot_canvas_paint(canvas);
  gtk_plot_pc_leave(canvas->pc); 

  canvas->pc = pc;
  canvas->magnification = m;

  plots = canvas->plots;
  while(plots){
    GtkPlot *plot;
   
    plot = GTK_PLOT(plots->data);

    plot->magnification = m;
    
    plots= plots->next;
  }

  gtk_object_destroy(GTK_OBJECT(ps));

  return TRUE;
}

