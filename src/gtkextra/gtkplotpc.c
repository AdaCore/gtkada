/* gtkplotpc - gtkplot print context - a renderer for printing functions
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
#include "gtkplotlayout.h"
#include "gtkplotcanvas.h"

void
gtk_plot_pc_set_size 				(GtkPlotPC *pc,
						 gint units,
						 gfloat width,
						 gfloat height)
{
  pc->units = units;
  pc->width = width;
  pc->height = height;

  switch(units){
   case GTK_PLOT_MM:
        pc->page_width = (gdouble)width * 2.835;
        pc->page_height = (gdouble)height * 2.835;
        break;
   case GTK_PLOT_CM:
        pc->page_width = width * 28.35;
        pc->page_height = height * 28.35;
        break;
   case GTK_PLOT_INCHES:
        pc->page_width = width * 72;
        pc->page_height = height * 72;
        break;
   case GTK_PLOT_PSPOINTS:
   default:
        pc->page_width = width;
        pc->page_height = height;
   }

}

GtkPlotPC *
gtk_plot_pc_new				(gchar *pcname,
					 gint orientation,
                                         gint page_size)
{
  GtkPlotPC *pc;
  gint width, height;

  pc = g_new(GtkPlotPC, 1);

  pc->pcname = g_strdup(pcname);
  pc->orientation = orientation;
  pc->units = GTK_PLOT_PSPOINTS;

  switch (page_size){
   case GTK_PLOT_LEGAL:
        width = GTK_PLOT_LEGAL_W;
        height = GTK_PLOT_LEGAL_H;
        break;
   case GTK_PLOT_A4:
        width = GTK_PLOT_A4_W;
        height = GTK_PLOT_A4_H;
        break;
   case GTK_PLOT_EXECUTIVE:
        width = GTK_PLOT_EXECUTIVE_W;
        height = GTK_PLOT_EXECUTIVE_H;
        break;
   case GTK_PLOT_LETTER:
   default:
        width = GTK_PLOT_LETTER_W;
        height = GTK_PLOT_LETTER_H;
  }

        
  gtk_plot_pc_set_size(pc, GTK_PLOT_PSPOINTS, width, height);

  return pc;
}

GtkPlotPC *
gtk_plot_pc_new_with_size			(gchar *pcname,
						 gint orientation,
                                                 gint units,
						 gfloat width, gfloat height)
{
  GtkPlotPC *pc;

  pc = gtk_plot_pc_new(pcname, orientation, GTK_PLOT_CUSTOM);

  gtk_plot_pc_set_size(pc, units, width, height);

  return pc;
}

void gtk_plot_pc_init                                   (GtkPlotPC *pc,
                                                         gfloat scale_x,
                                                         gfloat scale_y)
{
  pc->init(pc, scale_x, scale_y);
}

void gtk_plot_pc_leave                                  (GtkPlotPC *pc)
{
  pc->leave(pc);
}

void gtk_plot_pc_gsave                                  (GtkPlotPC *pc)
{
  pc->gsave(pc);
}

void gtk_plot_pc_grestore                               (GtkPlotPC *pc)
{
  pc->grestore(pc);
}

void gtk_plot_pc_clip                                   (GtkPlotPC *pc,
                                                         GdkRectangle area)
{
  pc->clip(pc, area);
}

void gtk_plot_pc_setcolor                               (GtkPlotPC *pc,
                                                        GdkColor *color)
{
  pc->setcolor(pc, color);
}

void gtk_plot_pc_setdash                                (GtkPlotPC *pc,
                                                         gint num_values,
                                                         gdouble *values,
                                                         gdouble offset)
{
  pc->setdash(pc, num_values, values, offset);
}

void gtk_plot_pc_setlinewidth                           (GtkPlotPC *pc,
                                                         gint width)
{
  pc->setlinewidth(pc, width);
}

void gtk_plot_pc_setlinecaps                            (GtkPlotPC *pc,
                                                         gint caps)
{
  pc->setlinecaps(pc, caps);
}


void gtk_plot_pc_drawline                               (GtkPlotPC *pc,
                                                         gint x1, gint y1,
                                                         gint x2, gint y2)
{
  pc->drawline(pc, x1, y1, x2, y2);
}

void gtk_plot_pc_drawlines                              (GtkPlotPC *pc,
                                                         GdkPoint *points,
                                                         gint numpoints)
{
  pc->drawlines(pc, points, numpoints);
}

void gtk_plot_pc_drawpolygon                            (GtkPlotPC *pc,
                                                         GdkPoint *points,
                                                         gint numpoints,
                                                         gint filled)
{
  pc->drawpolygon(pc, points, numpoints, filled);
}

void gtk_plot_pc_drawcircle                             (GtkPlotPC *pc,
                                                         gint x, gint y,
                                                         gint size,
                                                         gint filled)
{
  pc->drawcircle(pc, x, y, size, filled);
}

void gtk_plot_pc_setfont                                (GtkPlotPC *pc,
                                                         gchar *font,
                                                         gint height)
{
  pc->setfont(pc, font, height);
}

void gtk_plot_pc_drawstring                             (GtkPlotPC *pc,
                                                         gint x, gint y,
                                                         gint justification,
                                                         gint angle,
							 gchar *font,
							 gint height,
                                                         gchar *text)
{
  if(!text) return;
  if(strlen(text) == 0) return;

  pc->drawstring(pc, x, y, justification, angle, font, height, text);
}


