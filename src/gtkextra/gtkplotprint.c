/* gtkplotpc - gtkplot printing functions
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
#include "gtkplotcanvas.h"

#define ARROW_LENGTH 12

static void gtk_plot_print_calc_ticks		(GtkPlot *plot, 
						 gint orientation);
static void gtk_plot_print_draw			(GtkPlot *plot);
static void gtk_plot_print_draw_axis		(GtkPlot *plot, 
						 GtkPlotAxis axis, 
						 int x, int y);
static void gtk_plot_print_draw_grids		(GtkPlot *plot);
static void gtk_plot_print_draw_labels		(GtkPlot *plot,
            					 GtkPlotAxis axis,
            					 gint x, gint y);
static void gtk_plot_print_draw_errbars		(GtkPlot *plot, 
						 GtkPlotData *dataset); 
static void gtk_plot_print_connect_points	(GtkPlot *plot, 
						 GtkPlotData *dataset);
static void gtk_plot_print_draw_xy 		(GtkPlot *plot,
                  				 GtkPlotData *dataset);
static void gtk_plot_print_draw_legends 	(GtkPlot *plot);
static void gtk_plot_print_draw_dataset		(GtkPlot *plot, 
						 GtkPlotData *dataset);
static void gtk_plot_print_draw_point           (GtkPlot *plot,
                                                 gint x, gint y, gdouble dx,
                                                 GtkPlotSymbolType symbol,
                                                 GdkColor color,
                                                 GtkPlotSymbolStyle symbol_style,
                                                 gint symbol_size,
                                                 gint line_width);
static void gtk_plot_print_draw_symbol          (GtkPlot *plot,
                                                 gint x, gint y, gdouble dx,
                                                 GtkPlotSymbolType symbol,
                                                 GdkColor color,
                                                 gint filled,
                                                 gint symbol_size,
                                                 gint line_width);
static void gtk_plot_print_draw_text		(GtkWidget *widget, 
						 GtkPlotPC *pc,
						 GtkPlotText text);
static void gtk_plot_print_draw_child		(GtkWidget *widget, 
						 GtkPlotPC *pc,
						 GtkPlotCanvasChild *child);
static void gtk_plot_print_draw_rectangle	(GtkPlotPC *pc,
                                                 gint x, gint y, 
						 gint x1, gint y1, 
						 gint filled);
static void gtk_plot_print_draw_square          (GtkPlotPC *pc,
                                                 gint x, gint y,
                                                 gint size,
                                                 gint filled);
static void gtk_plot_print_draw_downtriangle    (GtkPlotPC *pc,
                                                 gint x, gint y,
                                                 gint size,
                                                 gint filled);
static void gtk_plot_print_draw_uptriangle      (GtkPlotPC *pc,
                                                 gint x, gint y,
                                                 gint size,
                                                 gint filled);
static void gtk_plot_print_draw_diamond         (GtkPlotPC *pc,
                                                 gint x, gint y,
                                                 gint size,
                                                 gint filled);
static void gtk_plot_print_draw_plus            (GtkPlotPC *pc,
                                                 gint x, gint y,
                                                 gint size);
static void gtk_plot_print_draw_cross           (GtkPlotPC *pc,
                                                 gint x, gint y,
                                                 gint size);
static void gtk_plot_print_draw_star            (GtkPlotPC *pc,
                                                 gint x, gint y,
                                                 gint size);
static void gtk_plot_print_set_line_style	(GtkPlotPC *pc,
                                          	 gint line_style,
                                          	 gint line_width,
                                          	 GdkColor color);
/*********************************************************************/
static gint transform_y                         (GtkPlot *plot, gdouble y);
static gint transform_x                         (GtkPlot *plot, gdouble x);
static gint transform_dy                        (GtkPlot *plot, 
                                                 gdouble y, gdouble dy);
static gint transform_dx                        (GtkPlot *plot, 
                                                 gdouble x, gdouble dx);
/*
static gdouble inverse_y                        (GtkPlot *plot, gint y);
static gdouble inverse_x                        (GtkPlot *plot, gint x);
static gdouble inverse_dy                       (GtkPlot *plot, gint dy);
*/
static gdouble inverse_dx                       (GtkPlot *plot, gint dx);

static gint roundint                            (gdouble x);
static void parse_label                         (gdouble val,
                                                 gint precision,
                                                 gint style,
                                                 gchar *label);
static void spline_solve 			(int n, 
						 gdouble x[], gdouble y[], 
						 gdouble *y2);
static gdouble spline_eval 			(int n, 
						 gdouble x[], gdouble y[], 
						 gdouble *y2, gdouble val);
static void pcgetpoint				(GtkPlot *plot, 
						 gdouble px, gdouble py, 
						 gint *x, gint *y);


static void 
gtk_plot_print_draw_axis(GtkPlot *plot, GtkPlotAxis axis, int x, int y)
{
  GtkPlotPC *pc = plot->pc;
  gint xx, yy;
  gint line_width;
  gint xp, yp, width, height;
  gint ticks_length;
  gint ntick;

  xp = roundint(plot->x * (gdouble)GTK_WIDGET(plot)->allocation.width);
  yp = roundint(plot->y * (gdouble)GTK_WIDGET(plot)->allocation.height);
  width = roundint(plot->width * (gdouble)GTK_WIDGET(plot)->allocation.width);
  height = roundint(plot->height * (gdouble)GTK_WIDGET(plot)->allocation.height);

  line_width = axis.line.line_width;
  ticks_length = axis.ticks_length;

  switch(axis.orientation){
     case GTK_ORIENTATION_HORIZONTAL:
         if(axis.line.line_style == GTK_PLOT_LINE_NONE) break;

         gtk_plot_print_set_line_style(pc, axis.line.line_style, 
                                       axis.line.line_width,
			               axis.line.color);

         pc->setlinecaps(pc, 2);
         pc->drawline(pc, x, y, x+width, y);
         gtk_plot_print_set_line_style(pc, axis.line.line_style, 
                                       axis.line.line_width,
			               axis.line.color);
         pc->setlinewidth(pc, axis.ticks_width);

         for(ntick = 0; ntick < plot->xticks.nmajorticks; ntick++){
           xx = plot->xticks.major[ntick];
           if(plot->xticks.major_values[ntick] >= plot->xmin){
             if(axis.major_mask & GTK_PLOT_TICKS_UP)
                   pc->drawline(pc, x+xx, y, x+xx, y-ticks_length);
             if(axis.major_mask & GTK_PLOT_TICKS_DOWN)
                   pc->drawline(pc, x+xx, y, x+xx, y+ticks_length);
           }
         }
         for(ntick = 0; ntick < plot->xticks.nminorticks; ntick++){
           xx = plot->xticks.minor[ntick];
           if(plot->xticks.minor_values[ntick] >= plot->xmin){
             if(axis.minor_mask & GTK_PLOT_TICKS_UP)
                   pc->drawline(pc, x+xx, y, x+xx, y-ticks_length/2-1);
             if(axis.minor_mask & GTK_PLOT_TICKS_DOWN)
                   pc->drawline(pc, x+xx, y, x+xx, y+ticks_length/2+1);
           }
         }
         break;
     case GTK_ORIENTATION_VERTICAL:
         if(axis.line.line_style == GTK_PLOT_LINE_NONE) break;
         y = y + height;

         gtk_plot_print_set_line_style(pc, axis.line.line_style, 
                                       axis.line.line_width,
			               axis.line.color);
         pc->setlinecaps(pc, 2);
         pc->drawline(pc, x, y, x, y-height);
         gtk_plot_print_set_line_style(pc, axis.line.line_style, 
                                       axis.line.line_width,
			               axis.line.color);
         pc->setlinewidth(pc, axis.ticks_width);

         for(ntick = 0; ntick < plot->yticks.nmajorticks; ntick++){
           yy = plot->yticks.major[ntick];
           if(plot->yticks.major_values[ntick] >= plot->ymin){
             if(axis.major_mask & GTK_PLOT_TICKS_RIGHT)
                   pc->drawline(pc, x, y-yy, x+ticks_length, y-yy);
             if(axis.major_mask & GTK_PLOT_TICKS_LEFT)
                   pc->drawline(pc, x, y-yy, x-ticks_length, y-yy);
           }
         }
         for(ntick = 0; ntick < plot->yticks.nminorticks; ntick++){
           yy = plot->yticks.minor[ntick];
           if(plot->yticks.minor_values[ntick] >= plot->ymin){
             if(axis.minor_mask & GTK_PLOT_TICKS_RIGHT)
                   pc->drawline(pc, x, y-yy, x+ticks_length/2+1, y-yy);
             if(axis.minor_mask & GTK_PLOT_TICKS_LEFT)
                   pc->drawline(pc, x, y-yy, x-ticks_length/2-1, y-yy);
           }
         }
         break;
  }

}

static void
gtk_plot_print_draw_grids(GtkPlot *plot)
{
  GtkWidget *widget;
  GtkPlotPC *pc = plot->pc;
  gint ix, iy;
  gint width, height;
  gint xp, yp;
  gint ntick;

  widget = GTK_WIDGET(plot);

  xp = widget->allocation.x + 
       roundint(plot->x * (gdouble)widget->allocation.width);
  yp = widget->allocation.y + 
       roundint(plot->y * (gdouble)widget->allocation.height);

  width = roundint(plot->width * (gdouble)widget->allocation.width);
  height = roundint(plot->height * (gdouble)widget->allocation.height);

  if(plot->show_x0 && plot->x0_line.line_style != GTK_PLOT_LINE_NONE)
    {
          gtk_plot_print_set_line_style(pc, plot->x0_line.line_style, 
                                        plot->x0_line.line_width,
                                        plot->x0_line.color);
          if(plot->xmin <= 0. && plot->xmax >= 0.)
            {
              ix = transform_x(plot, 0.);
              ix += xp;
              pc->drawline(pc, ix, yp+1, ix, yp+height);
            }
    }

  if(plot->show_y0 && plot->y0_line.line_style != GTK_PLOT_LINE_NONE)
    {
          gtk_plot_print_set_line_style(pc, plot->y0_line.line_style, 
                                        plot->y0_line.line_width,
                                        plot->y0_line.color);
          if(plot->ymin <= 0. && plot->ymax >= 0.)
            {
              iy = transform_y(plot, 0.);
              iy = height+yp-iy;
              pc->drawline(pc, xp, iy, xp + width, iy);
            }
    }
  if(plot->show_vminor)
    {
        if(plot->minor_vgrid.line_style != GTK_PLOT_LINE_NONE){
          gtk_plot_print_set_line_style(pc, plot->minor_vgrid.line_style,
                                        plot->minor_vgrid.line_width,
                                        plot->minor_vgrid.color);
          for(ntick = 0; ntick < plot->xticks.nminorticks; ntick++){
            if(plot->xticks.minor_values[ntick] >= plot->xmin){
              ix = xp+plot->xticks.minor[ntick];
              pc->drawline(pc, ix, yp+1, ix, yp+height);
            }
          }
        }
    }
  if(plot->show_vmajor)
    {
        if(plot->major_vgrid.line_style != GTK_PLOT_LINE_NONE){
          gtk_plot_print_set_line_style(pc, plot->major_vgrid.line_style,
                                        plot->major_vgrid.line_width,
                                        plot->major_vgrid.color);
          for(ntick = 0; ntick < plot->xticks.nmajorticks; ntick++){
            if(plot->xticks.major_values[ntick] >= plot->xmin){
              ix = xp+plot->xticks.major[ntick];
              pc->drawline(pc, ix, yp+1, ix, yp+height);
            }
          }
        }
    }
  if(plot->show_hminor)
    {
        if(plot->minor_hgrid.line_style != GTK_PLOT_LINE_NONE){
          gtk_plot_print_set_line_style(pc, plot->minor_hgrid.line_style,
                                        plot->minor_hgrid.line_width,
                                        plot->minor_hgrid.color);
          for(ntick = 0; ntick < plot->yticks.nminorticks; ntick++){
            if(plot->yticks.minor_values[ntick] >= plot->ymin){
              iy = height+yp-plot->yticks.minor[ntick];
              pc->drawline(pc, xp, iy, xp + width, iy);
            }
          }
        }
    }
  if(plot->show_hmajor)
    {
        if(plot->major_hgrid.line_style != GTK_PLOT_LINE_NONE){
          gtk_plot_print_set_line_style(pc, plot->major_hgrid.line_style,
                                        plot->major_hgrid.line_width,
                                        plot->major_hgrid.color);
          for(ntick = 0; ntick < plot->yticks.nmajorticks; ntick++){
            if(plot->yticks.major_values[ntick] >= plot->ymin){
              iy = height+yp-plot->yticks.major[ntick];
              pc->drawline(pc, xp, iy, xp + width, iy);
            }
          }
        }
    }
}

static void
gtk_plot_print_draw_labels(GtkPlot *plot,
             		GtkPlotAxis axis,
             		gint x, gint y)
{
  GtkPlotPC *pc = plot->pc;
  GtkWidget *widget;
  GdkFont *font;
  gchar *psfont;
  GtkPlotText title;
  gchar label[100];
  gdouble x_tick, y_tick;
  gint xx, yy;
  gint text_height;
  gint xp, yp, width, height;
  gint ntick;

  widget = GTK_WIDGET(plot);
  xp = roundint(plot->x * widget->allocation.width);
  yp = roundint(plot->y * widget->allocation.height);
  width = roundint(plot->width * widget->allocation.width);
  height = roundint(plot->height * widget->allocation.height);

  pc->setcolor (pc, &axis.labels_attr.fg);

  font = gtk_psfont_get_gdkfont(axis.labels_attr.font, axis.labels_attr.height);
  psfont = gtk_psfont_get_psfontname(axis.labels_attr.font);
  pc->setfont(pc, psfont, axis.labels_attr.height);
  text_height = axis.labels_attr.height;

  switch(axis.labels_attr.angle){
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

  switch(axis.orientation){
     case GTK_ORIENTATION_VERTICAL:
       y += height;
       for(ntick = 0; ntick < plot->yticks.nmajorticks; ntick++){
         yy = plot->yticks.major[ntick];
         y_tick = plot->yticks.major_values[ntick];
         if(y_tick >= plot->ymin){
           if(!axis.custom_labels){
             parse_label(y_tick, axis.label_precision, axis.label_style, label);
           }
           else
           {
             label[0]=' ';
             label[1]='\0';
             if(y_tick > 0. && fabs((gdouble)roundint(y_tick)-y_tick) < 1.E-8){
                if(axis.nlabels > roundint(y_tick)-1)
                  if(axis.tick_labels[roundint(y_tick)-1])
                    sprintf(label, "%s", axis.tick_labels[roundint(y_tick)-1]);
             }
           }
           if(axis.label_mask & GTK_PLOT_LABEL_LEFT)
                pc->drawstring(pc,
                             x-axis.labels_offset, 
                             y-yy, 
                             axis.labels_attr.justification, 
                             axis.labels_attr.angle,
                             psfont, text_height,
                             label);
           if(axis.label_mask & GTK_PLOT_LABEL_RIGHT)
                pc->drawstring(pc,
                             x+axis.labels_offset, 
                             y-yy, 
                             axis.labels_attr.justification,
                             axis.labels_attr.angle,
                             psfont, text_height,
                             label);
         }
       }
       if(axis.title_visible && axis.title.text)
          {
            title = axis.title;
            gtk_plot_print_draw_text(GTK_WIDGET(plot), plot->pc, title);
          }
       break;
     case GTK_ORIENTATION_HORIZONTAL:
       for(ntick = 0; ntick < plot->xticks.nmajorticks; ntick++){
         xx = plot->xticks.major[ntick];
         x_tick = plot->xticks.major_values[ntick];
         if(x_tick >= plot->xmin){
           if(!axis.custom_labels){
             parse_label(x_tick, axis.label_precision, axis.label_style, label);
           }
           else
           {
             label[0]=' ';
             label[1]='\0';
             if(x_tick > 0. && fabs((gdouble)roundint(x_tick)-x_tick) < 1.E-8){
                if(axis.nlabels > roundint(x_tick)-1)
                  if(axis.tick_labels[roundint(x_tick)-1])
                    sprintf(label, "%s", axis.tick_labels[roundint(x_tick)-1]);
             }
           }
           if(axis.label_mask & GTK_PLOT_LABEL_TOP)
                pc->drawstring(pc,
                             x+xx, 
                             y-axis.labels_offset, 
                             axis.labels_attr.justification,
                             axis.labels_attr.angle,
                             psfont, text_height,
                             label);
  
           if(axis.label_mask & GTK_PLOT_LABEL_BOTTOM)
                pc->drawstring(pc,
                             x+xx, 
                             y+axis.labels_offset, 
                             axis.labels_attr.justification,
                             axis.labels_attr.angle,
                             psfont, text_height,
                             label);
         }
       }
       if(axis.title_visible && axis.title.text)
          {
            title = axis.title;
            gtk_plot_print_draw_text(GTK_WIDGET(plot), plot->pc, title);
          }
       break;
   }

   gdk_font_unref(font);
}

static gint
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

  height = (gdouble)GTK_WIDGET(plot)->allocation.height * plot->height;
  return(roundint(height*position));
}

static gint
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

  width = (gdouble)GTK_WIDGET(plot)->allocation.width * plot->width;
  return(roundint(width*position));
}


static gint
transform_dx(GtkPlot *plot, gdouble x, gdouble dx)
{
    gdouble width;

    width = (gdouble)GTK_WIDGET(plot)->allocation.width * plot->width;

    switch( plot->xscale ) {
      case GTK_PLOT_SCALE_LOG10:
        if( x <= 0.0 || plot->xmin <= 0.0 || plot->xmax <= 0.0 || x+dx <= 0.0 )
          return 0;
        return roundint(width * log(1.0+dx/x) / log(plot->xmax / plot->xmin));

      case GTK_PLOT_SCALE_LINEAR:
      default:
        return roundint(width*dx/(plot->xmax - plot->xmin));
    }

}

static gint
transform_dy(GtkPlot *plot, gdouble y, gdouble dy)
{
    gdouble height;

    height = (gdouble)GTK_WIDGET(plot)->allocation.height * plot->height;

    switch( plot->yscale ) {
      case GTK_PLOT_SCALE_LOG10:
        if( y <= 0.0 || plot->ymin <= 0.0 || plot->ymax <= 0.0 || y+dy <= 0.0 )
          return 0;
        return roundint(height * log(1.0+dy/y) / log(plot->ymax / plot->ymin));
      case GTK_PLOT_SCALE_LINEAR:
      default:
        return roundint(height*dy/(plot->ymax - plot->ymin));
    }

}

/*
static gdouble
inverse_y(GtkPlot *plot, gint y)
{
    return(inverse_dy(plot, y)+plot->ymin);
}

static gdouble
inverse_x(GtkPlot *plot, gint x)
{
    return(inverse_dx(plot, x)+plot->xmin);
}


static gdouble
inverse_dy(GtkPlot *plot, gint dy)
{
    gdouble height;

    height = (gdouble)GTK_WIDGET(plot)->allocation.height * plot->height;
    return(((gdouble)dy)*(plot->ymax-plot->ymin)/height);
}
*/

static gdouble
inverse_dx(GtkPlot *plot, gint dx)
{
    gdouble width;

    width = (gdouble)GTK_WIDGET(plot)->allocation.width * plot->width;
    return(((gdouble)dx)*(plot->xmax-plot->xmin)/width);
}


static gint
roundint (gdouble x)
{
 return (x+.50999999471);
}

void
gtk_plot_print_set_size 				(GtkPlotPC *pc,
						 gint units,
						 gfloat width,
						 gfloat height)
{
  pc->units = units;
  pc->width = width;
  pc->height = height;

  switch(units){
   case GTK_PLOT_MM:
        pc->page_width = roundint((gdouble)width * 2.8);
        pc->page_height = roundint((gdouble)height * 2.8);
        break;
   case GTK_PLOT_CM:
        pc->page_width = width * 28;
        pc->page_height = height * 28;
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
gtk_plot_print_new				(gchar *pcname,
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

        
  gtk_plot_print_set_size(pc, GTK_PLOT_PSPOINTS, width, height);

  return pc;
}

GtkPlotPC *
gtk_plot_print_new_with_size			(gchar *pcname,
						 gint orientation,
                                                 gint units,
						 gfloat width, gfloat height)
{
  GtkPlotPC *pc;

  pc = gtk_plot_print_new(pcname, orientation, GTK_PLOT_CUSTOM);

  gtk_plot_print_set_size(pc, units, width, height);

  return pc;
}

void
gtk_plot_print(GtkPlot *plot, GtkPlotPC *pc)
{
  GtkPlotPC *plot_pc;
  gfloat scalex, scaley;

  if(pc->orientation == GTK_PLOT_PORTRAIT){
    scalex = (gfloat)pc->page_width /
                                (gfloat)GTK_WIDGET(plot)->allocation.width;
    scaley = (gfloat)pc->page_height / 
                                (gfloat)GTK_WIDGET(plot)->allocation.height;
  }else{
    scalex = (gfloat)pc->page_width /
                                (gfloat)GTK_WIDGET(plot)->allocation.height;
    scaley = (gfloat)pc->page_height / 
                                (gfloat)GTK_WIDGET(plot)->allocation.width;
  }

  pc->init(pc, scalex, scaley);

  plot_pc = plot->pc;
  plot->pc = pc;
  gtk_plot_print_draw(plot);
  plot->pc = plot_pc;

  pc->leave(pc);

}

void
gtk_plot_canvas_print(GtkPlotCanvas *canvas, GtkPlotPC *pc)
{
  GList *plots;
  GtkPlot *plot;
  GtkPlotPC *plot_pc;
  GList *childs;
  GtkPlotCanvasChild *child;
  GtkAllocation allocation;
  gfloat scalex, scaley;

  if(pc->orientation == GTK_PLOT_PORTRAIT){
    scalex = (gfloat)pc->page_width / (gfloat)canvas->width;
    scaley = (gfloat)pc->page_height / (gfloat)canvas->height;
  }else{
    scalex = (gfloat)pc->page_width / (gfloat)canvas->height;
    scaley = (gfloat)pc->page_height / (gfloat)canvas->width;
  }

  pc->init(pc, scalex, scaley);

  pc->setcolor(pc, &canvas->background);
  gtk_plot_print_draw_rectangle(pc, 
                             0, 0,
                             canvas->width, 
                             canvas->height,
                             TRUE);

  plots = canvas->plots;
  while(plots)
   {
     plot = GTK_PLOT(plots->data);
     plot_pc = plot->pc;
     plot->pc = pc;

     allocation = GTK_WIDGET(plot)->allocation;
     GTK_WIDGET(plot)->allocation.x = 0;
     GTK_WIDGET(plot)->allocation.y = 0;
     GTK_WIDGET(plot)->allocation.width = canvas->width;
     GTK_WIDGET(plot)->allocation.height = canvas->height;

     gtk_plot_print_draw(plot);

     GTK_WIDGET(plot)->allocation = allocation;

     plot->pc = plot_pc;

     plots = plots->next;
   }

  allocation = GTK_WIDGET(canvas)->allocation;
  GTK_WIDGET(canvas)->allocation.x = 0;
  GTK_WIDGET(canvas)->allocation.y = 0;
  GTK_WIDGET(canvas)->allocation.width = canvas->width;
  GTK_WIDGET(canvas)->allocation.height = canvas->height;

  childs = canvas->childs;
  while(childs)
   {
     child = (GtkPlotCanvasChild *) childs->data;
     gtk_plot_print_draw_child(GTK_WIDGET(canvas), pc, child);
     childs = childs->next;
   }


  GTK_WIDGET(canvas)->allocation = allocation;

  pc->leave(pc);

}

static void
gtk_plot_print_draw(GtkPlot *plot)
{
  GtkWidget *widget;
  GList *dataset;
  gint xoffset, yoffset;
  gint width, height;

  widget = GTK_WIDGET(plot);
  xoffset = widget->allocation.x + 
            roundint(plot->x * widget->allocation.width);
  yoffset = widget->allocation.y +
            roundint(plot->y * widget->allocation.height);
  width = roundint(plot->width * widget->allocation.width);
  height = roundint(plot->height * widget->allocation.height);

  plot->pc->setcolor(plot->pc, &plot->background);

  gtk_plot_print_calc_ticks(plot, GTK_ORIENTATION_HORIZONTAL);
  gtk_plot_print_calc_ticks(plot, GTK_ORIENTATION_VERTICAL);

  if(!GTK_PLOT_TRANSPARENT(plot))
    gtk_plot_print_draw_rectangle (plot->pc,
                                xoffset, yoffset,
                                xoffset + width , yoffset + height,
                                TRUE);


  gtk_plot_print_draw_grids(plot);

  if(plot->bottom.is_visible)
    {
      gtk_plot_print_draw_axis(plot, plot->bottom,
                            xoffset,
                            yoffset+(gfloat)height*(1.-plot->bottom_align));
      gtk_plot_print_draw_labels(plot, plot->bottom,
                              xoffset,
                              yoffset+(gfloat)height*(1.-plot->bottom_align));
    }

  if(plot->top.is_visible)
    {
      gtk_plot_print_draw_axis(plot, plot->top,
                            xoffset,
                            yoffset+(gfloat)height*(1.-plot->top_align));
      gtk_plot_print_draw_labels(plot, plot->top,
                              xoffset,
                              yoffset+(gfloat)height*(1.-plot->top_align));
    }

  if(plot->left.is_visible)
    {
      gtk_plot_print_draw_axis(plot, plot->left,
                            xoffset+(gfloat)width*plot->left_align,
                            yoffset);
      gtk_plot_print_draw_labels(plot, plot->left,
                              xoffset+(gfloat)width*plot->left_align,
                              yoffset);
    }

  if(plot->right.is_visible)
    {
      gtk_plot_print_draw_axis(plot, plot->right,
                            xoffset+(gfloat)width*plot->right_align,
                            yoffset);
      gtk_plot_print_draw_labels(plot, plot->right,
                              xoffset+(gfloat)width*plot->right_align,
                              yoffset);
    }

  dataset = plot->data_sets;
  while(dataset)
   {
     gtk_plot_print_draw_dataset(plot, (GtkPlotData *)dataset->data);
     dataset = dataset->next;
   }


  gtk_plot_print_draw_legends(plot);

}

static void
gtk_plot_print_draw_dataset(GtkPlot *plot, GtkPlotData *dataset)
{
  GtkPlotPC *pc = plot->pc;
  GtkPlotData function;
  gint n;
  gdouble x, y, dx = 0;
  gdouble *fx;
  gdouble *fy;
  gint px, py;
  gboolean error;

  if(!dataset->is_visible) return;

  if(!dataset->is_function)
    {
       gtk_plot_print_connect_points (plot, dataset);
       gtk_plot_print_draw_xy(plot, dataset);
       gtk_plot_print_draw_errbars(plot, dataset);

       for(n=0; n<=dataset->num_points-1; n++)
         {
           x = dataset->x[n];
           y = dataset->y[n];
           if(dataset->dx) dx = dataset->dx[n];
           if(x >= plot->xmin && x <= plot->xmax &&
              y >= plot->ymin && y <= plot->ymax) {
                 pcgetpoint(plot, x, y, &px, &py);
                 gtk_plot_print_draw_point(plot,
                                        px, py, dx,
                                        dataset->symbol.symbol_type,
                                        dataset->symbol.color,
                                        dataset->symbol.symbol_style,
                                        dataset->symbol.size,
                                        dataset->symbol.line_width);
                 if(dataset->show_labels){
                    if(dataset->labels && dataset->labels[n]){
                          GtkPlotText label;
			  gchar *psfont;

                          label = dataset->labels_attr;
                          label.text = dataset->labels[n];
                          label.x = px;
                          label.y = py - dataset->labels_offset - 
                                    dataset->symbol.size;
                          psfont = gtk_psfont_get_psfontname(label.font);
                          pc->setcolor(pc, &label.fg);
                          pc->setfont(pc, psfont, label.height);
                          pc->drawstring(pc, label.x, label.y, 
                                         label.justification, 
                                         label.angle, psfont, label.height, 
                                         label.text); 
                    }
                 }

           }
         }
    }
  else
    {
       function = *dataset;
       fx = NULL;
       fy = NULL;
       function.num_points = 0;
       for(x=plot->xmin; x<=plot->xmax+inverse_dx(plot, function.x_step);
                         x+=inverse_dx(plot, function.x_step)) {
            function.num_points++;
            fx = (gdouble *)g_realloc(fx, function.num_points*sizeof(gdouble));
            fy = (gdouble *)g_realloc(fy, function.num_points*sizeof(gdouble));
            y = function.function (plot, dataset, x, &error);
            if(error)
              {
                 function.x = fx;
                 function.y = fy;
                 function.num_points--;
                 if(function.num_points > 1)
                       gtk_plot_print_connect_points (plot, &function);
                 function.num_points = 0;
              }
            else
              {
                if(function.num_points >= 2)
                  {
                     if(y > plot->ymax &&
                        fy[function.num_points-2] <= plot->ymin)
                       {
                          function.x = fx;
                          function.y = fy;
                          function.num_points--;
                          gtk_plot_print_connect_points(plot, &function);
                          function.num_points = 1;
                       }
                      if(y < plot->ymin &&
                        fy[function.num_points-2] >= plot->ymax)
                       {
                          function.x = fx;
                          function.y = fy;
                          function.num_points--;
                          gtk_plot_print_connect_points(plot, &function);
                          function.num_points = 1;
                       }
                   }
                fx[function.num_points-1] = x;
                fy[function.num_points-1] = y;

              }
         }
       if(function.num_points > 1 )
         {
            function.x = fx;
            function.y = fy;
            gtk_plot_print_connect_points (plot, &function);
         }
       g_free(fx);
       g_free(fy);
    }
}

static void
gtk_plot_print_draw_errbars(GtkPlot *plot, GtkPlotData *dataset)
{
  GtkPlotPC *pc = plot->pc;
  GdkPoint errbar[6];
  gdouble x, y;
  gint px, py;
  gint ex_r, ex_l, ey_u, ey_d;
  gint n;

  if(!dataset->x || !dataset->y) return;
  if(!dataset->dx || !dataset->dy) return;

  pc->setcolor(plot->pc, &dataset->symbol.color);
  pc->setlinecaps(plot->pc, 0);
  pc->setlinewidth(plot->pc, dataset->symbol.line_width/2);

  for(n=0; n<=dataset->num_points-1; n++)
    {
      x = dataset->x[n];
      y = dataset->y[n];

      if(x >= plot->xmin && x <= plot->xmax &&
         y >= plot->ymin && y <= plot->ymax) {

            pcgetpoint(plot, x, y, &px, &py);
          
            ex_r = transform_dx(plot, x, dataset->dx[n]);
            ey_u = transform_dy(plot, y, dataset->dy[n]);

            if( plot->xscale != GTK_PLOT_SCALE_LINEAR )
              ex_l = transform_dx(plot, x, -dataset->dx[n]);
            else
              ex_l = -ex_r;

            if( plot->yscale != GTK_PLOT_SCALE_LINEAR )
              ey_d = transform_dy(plot, y, -dataset->dy[n]);
            else
              ey_d = -ey_u;

            if(dataset->show_xerrbars)
              {
                errbar[0].x = px+ex_l;
                errbar[0].y = py-dataset->xerrbar_caps/2;
                errbar[1].x = px+ex_l;
                errbar[1].y = py+dataset->xerrbar_caps/2;
                errbar[2].x = px+ex_l;
                errbar[2].y = py;
                errbar[3].x = px+ex_r;
                errbar[3].y = py;
                errbar[4].x = px+ex_r;
                errbar[4].y = py-dataset->xerrbar_caps/2;
                errbar[5].x = px+ex_r;
                errbar[5].y = py+dataset->xerrbar_caps/2;
                plot->pc->drawlines(plot->pc, errbar, 6);
              }
          
            if(dataset->show_yerrbars)
              {
                errbar[0].x = px-dataset->yerrbar_caps/2;
                errbar[0].y = py+ey_u;
                errbar[1].x = px+dataset->yerrbar_caps/2;
                errbar[1].y = py+ey_u;
                errbar[2].x = px;
                errbar[2].y = py+ey_u;
                errbar[3].x = px;
                errbar[3].y = py+ey_d;
                errbar[4].x = px-dataset->yerrbar_caps/2;
                errbar[4].y = py+ey_d;
                errbar[5].x = px+dataset->yerrbar_caps/2;
                errbar[5].y = py+ey_d;
                plot->pc->drawlines(plot->pc, errbar, 6);
              }
      }
    }

}


static void
gtk_plot_print_connect_points(GtkPlot *plot, GtkPlotData *dataset)
{
  GtkWidget *widget;
  GdkPoint *points;
  GtkPlotData spline;
  GdkPoint *spline_points;
  gdouble *spline_coef;
  gdouble spline_x, spline_y;
  gint n;
  gint px, py;
  gint x1, y1;
  gdouble x = 0, y = 0;
  gint num_points = dataset->num_points;
  GdkRectangle clip;

  if(dataset->line.line_style == GTK_PLOT_LINE_NONE) return;

  widget = GTK_WIDGET(plot);
  clip.x = roundint(plot->x * (gdouble)widget->allocation.width);
  clip.y = roundint(plot->y * (gdouble)widget->allocation.height);
  clip.width = roundint(plot->width * (gdouble)widget->allocation.width);
  clip.height = roundint(plot->height * (gdouble)widget->allocation.height);

  points = (GdkPoint *)g_malloc(2*num_points*sizeof(GdkPoint));

  plot->pc->gsave(plot->pc);
  plot->pc->clip(plot->pc, clip);
  gtk_plot_print_set_line_style(plot->pc, 
                                dataset->line.line_style,
                                dataset->line.line_width,
                                dataset->line.color);

  switch(dataset->line_connector){
   case GTK_PLOT_CONNECT_STRAIGHT:
      if(dataset->num_points == 1) break;
      for(n=0; n<dataset->num_points; n++)
        {
          x = dataset->x[n];
          y = dataset->y[n];
          pcgetpoint(plot, x, y, &px, &py);
          points[n].x = px;
          points[n].y = py;
        }
      break;
   case GTK_PLOT_CONNECT_HV_STEP:
       if(dataset->num_points == 1) break;
       num_points=0;
       for(n=0; n < dataset->num_points; n++)
        {
          x = dataset->x[n];
          y = dataset->y[n];
          pcgetpoint(plot, x, y, &px, &py);
          points[num_points].x = px;
          points[num_points].y = py;
          num_points++;
          if(n < dataset->num_points-1)
            {
              pcgetpoint(plot, 
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
       if(dataset->num_points == 1) break;
       num_points=0;
       for(n=0; n < dataset->num_points; n++)
        {
          pcgetpoint(plot, 
                     dataset->x[n],
                     dataset->y[n],
                     &px, &py);
          points[num_points].x = px;
          points[num_points].y = py;
          num_points++;
          if(n < dataset->num_points-1)
            {
              pcgetpoint(plot, 
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
       if(dataset->num_points == 1) break;
       num_points=1;
       for(n=1; n < dataset->num_points; n++)
        {
          pcgetpoint(plot, 
                     dataset->x[n],
                     dataset->y[n],
                     &px, &py);
          pcgetpoint(plot, 
                     dataset->x[n-1],
                     dataset->y[n-1],
                     &x1, &y1);
          points[num_points].x = (x+x1)/2;
          points[num_points].y = y1;
          num_points++;
          points[num_points].x = points[num_points-1].x;
          points[num_points].y = py;
          num_points++;
        }
        pcgetpoint(plot, 
                   dataset->x[0],
                   dataset->y[0],
                   &px, &py);
        points[0].x = px;
        points[0].y = py;
        pcgetpoint(plot, 
                   dataset->x[dataset->num_points-1],
                   dataset->y[dataset->num_points-1],
                   &px, &py);
        points[num_points].x = px;
        points[num_points].y = py;
        num_points++;
        break;
     case GTK_PLOT_CONNECT_SPLINE:
        spline = *dataset;
        spline_points = NULL;
        spline.num_points = 0;
        spline_coef = (gdouble *)g_malloc( num_points * sizeof(gdouble) );

        spline_solve(dataset->num_points, 
                     dataset->x,
                     dataset->y, 
		     spline_coef);
        for(spline_x=dataset->x[0];
            spline_x<=dataset->x[dataset->num_points-1];
            spline_x+=inverse_dx(plot, spline.x_step)) {
              spline_y = spline_eval(dataset->num_points, 
				     dataset->x,
				     dataset->y,
                                     spline_coef, spline_x);
              x = spline_x;
              y = spline_y;
              pcgetpoint(plot, x, y, &px, &py);
              spline.num_points++;
              spline_points = (GdkPoint *)g_realloc(spline_points,
                                                    spline.num_points*
                                                    sizeof(GdkPoint));
              spline_points[spline.num_points-1].x = px;
              spline_points[spline.num_points-1].y = py;
         }
        plot->pc->drawlines(plot->pc, spline_points, spline.num_points);
        plot->pc->grestore(plot->pc);
        g_free(spline_points);
        g_free(spline_coef);
        g_free(points);
        return;
     case GTK_PLOT_CONNECT_NONE:
     default:
        plot->pc->grestore(plot->pc);
        g_free(points);
        return;
    }


  plot->pc->drawlines(plot->pc, points, num_points);
  plot->pc->grestore(plot->pc);
  g_free(points);

}

static void gtk_plot_print_set_line_style(GtkPlotPC *pc,
                                          gint line_style,
                                          gint line_width,
                                          GdkColor color)
{
    gdouble values[6];

    if(line_style == GTK_PLOT_LINE_NONE) return;

    pc->setcolor(pc, &color);
    pc->setlinewidth(pc, line_width);

    switch (line_style) {
      case GTK_PLOT_LINE_SOLID:                         /* solid */
        values[0]=0.;
        pc->setdash(pc, 0, values, 0);
        break;
      case GTK_PLOT_LINE_DOTTED:                        /* dotted */
        values[0]=2.;
        values[1]=3.;
        pc->setdash(pc, 2, values, 0);
        break;
      case GTK_PLOT_LINE_DASHED:                        /* long dash */
        values[0]=6.;
        values[1]=4.;
        pc->setdash(pc, 2, values, 0);
        break;
      case GTK_PLOT_LINE_DOT_DASH:                      /* dot-dash */
        values[0]=6.;
        values[1]=4.;
        values[2]=2.;
        values[3]=4.;
        pc->setdash(pc, 4, values, 0);
        break;
      case GTK_PLOT_LINE_DOT_DOT_DASH:                  /* dot-dot-dash */
        values[0]=6.;
        values[1]=4.;
        values[2]=2.;
        values[3]=4.;
        values[4]=2.;
        values[5]=4.;
        pc->setdash(pc, 6, values, 0);
        break;
      case GTK_PLOT_LINE_DOT_DASH_DASH:                 /* dot-dash-dash */
        values[0]=6.;
        values[1]=4.;
        values[2]=6.;
        values[3]=4.;
        values[4]=2.;
        values[5]=4.;
        pc->setdash(pc, 6, values, 0);
        break;
    }

    pc->setlinecaps(pc, 0);
}


static void
pcgetpoint(GtkPlot *plot, gdouble px, gdouble py, gint *x, gint *y)
{
    gint xp, yp, width, height;

    xp = roundint(plot->x * GTK_WIDGET(plot)->allocation.width);
    yp = roundint(plot->y * GTK_WIDGET(plot)->allocation.height);
    width = roundint(plot->width * GTK_WIDGET(plot)->allocation.width);
    height = roundint(plot->height * GTK_WIDGET(plot)->allocation.height);

    *y = transform_y(plot, py);
    *x = transform_x(plot, px);

    *x = GTK_WIDGET(plot)->allocation.x + *x + xp;
    *y = GTK_WIDGET(plot)->allocation.y + yp + height - *y;

}

/* Solve the tridiagonal equation system that determines the second
   derivatives for the interpolation points.  (Based on Numerical
   Recipes 2nd Edition.) */

static void
spline_solve (int n, gdouble x[], gdouble y[], gdouble y2[])
{
  gfloat p, sig, *u;
  gint i, k;

  u = g_malloc ((n - 1) * sizeof (u[0]));

  y2[0] = u[0] = 0.0;   /* set lower boundary condition to "natural" */

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
  gfloat h, b, a;

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
gtk_plot_print_draw_point (GtkPlot *plot,
             gint x, gint y, gdouble dx,
             GtkPlotSymbolType symbol,
             GdkColor color,
             GtkPlotSymbolStyle symbol_style,
             gint size,
             gint line_width)
{
  gint fill = FALSE;


  if(symbol_style == GTK_PLOT_SYMBOL_OPAQUE && symbol < GTK_PLOT_SYMBOL_PLUS)
     gtk_plot_print_draw_symbol (plot, x, y, dx, symbol,
                   plot->background,
                   TRUE,
                   size,
                   line_width);

  if(symbol_style == GTK_PLOT_SYMBOL_FILLED) fill = TRUE;

  gtk_plot_print_draw_symbol (plot, x, y, dx, symbol,
                color,
                fill,
                size,
                line_width);
}


static void
gtk_plot_print_draw_symbol(GtkPlot *plot,
             gint x, gint y, gdouble dx,
             GtkPlotSymbolType symbol,
             GdkColor color,
             gint filled,
             gint size,
             gint line_width)
{
    gint x0, y0;

    plot->pc->setcolor(plot->pc, &color);
    plot->pc->setlinecaps(plot->pc, 0);
    plot->pc->setlinewidth(plot->pc, line_width);
    
    switch(symbol) {
       case GTK_PLOT_SYMBOL_NONE:
              break;
       case GTK_PLOT_SYMBOL_SQUARE:
              gtk_plot_print_draw_square (plot->pc, x, y, size, filled);
              break;
       case GTK_PLOT_SYMBOL_CIRCLE:
              plot->pc->drawcircle (plot->pc, x, y, size/2, filled);
              break;
       case GTK_PLOT_SYMBOL_UP_TRIANGLE:
              gtk_plot_print_draw_uptriangle (plot->pc, x, y, size, filled);          
              break;
       case GTK_PLOT_SYMBOL_DOWN_TRIANGLE:
              gtk_plot_print_draw_downtriangle (plot->pc, x, y, size, filled);        
              break;
       case GTK_PLOT_SYMBOL_DIAMOND:
              gtk_plot_print_draw_diamond (plot->pc, x, y, size, filled);              
              break;
       case GTK_PLOT_SYMBOL_PLUS:
              gtk_plot_print_draw_plus (plot->pc, x, y, size);
              break;
       case GTK_PLOT_SYMBOL_CROSS:
              gtk_plot_print_draw_cross (plot->pc, x, y, size);
              break;
       case GTK_PLOT_SYMBOL_STAR:
              gtk_plot_print_draw_star (plot->pc, x, y, size);
              break;
       case GTK_PLOT_SYMBOL_BAR:
              pcgetpoint(plot, x, 0., &x0, &y0);
              gtk_plot_print_draw_rectangle (plot->pc, x, MIN(y0,y),
                               transform_dx(plot, x0, dx)+1, abs(y-y0), 
                               filled);
              break;
       case GTK_PLOT_SYMBOL_IMPULSE:
              pcgetpoint(plot, x, 0., &x0, &y0);
              plot->pc->drawline(plot->pc, x, MIN(y0,y), x, MAX(y0,y));
              break;

    }
}


static void
gtk_plot_print_draw_xy (GtkPlot *plot,
                     GtkPlotData *dataset)
{
  GtkWidget *widget;
  GtkPlotPC *pc = plot->pc;
  GdkRectangle clip;
  gdouble x, y;
  gint px, py;
  gint x0, y0;
  gint n;

  widget = GTK_WIDGET(plot);
  clip.x = roundint(plot->x * (gdouble)widget->allocation.width);
  clip.y = roundint(plot->y * (gdouble)widget->allocation.height);
  clip.width = roundint(plot->width * (gdouble)widget->allocation.width);
  clip.height = roundint(plot->height * (gdouble)widget->allocation.height);

  plot->pc->gsave(plot->pc);
  plot->pc->clip(plot->pc, clip);

  if(!dataset->x || !dataset->y) return;

  for(n=0; n<=dataset->num_points-1; n++)
    {
      x = dataset->x[n];
      y = dataset->y[n];

      if(x >= plot->xmin && x <= plot->xmax &&
         y >= plot->ymin && y <= plot->ymax) {

            pcgetpoint(plot, x, y, &px, &py);

            if(dataset->x_line.line_style != GTK_PLOT_LINE_NONE){
               pcgetpoint(plot, px, 0., &x0, &y0);
               gtk_plot_print_set_line_style(plot->pc, 
                                             dataset->x_line.line_style,
                                             dataset->x_line.line_width,
                                             dataset->x_line.color);
               pc->drawline(plot->pc, px, py, px, y0);
            }

            if(dataset->y_line.line_style != GTK_PLOT_LINE_NONE){
               pcgetpoint(plot, 0., py, &x0, &y0);
               gtk_plot_print_set_line_style(plot->pc, 
                                             dataset->y_line.line_style,
                                             dataset->y_line.line_width,
                                             dataset->y_line.color);
               pc->drawline(plot->pc, px, py, x0, py);
            }
      }
    }

  plot->pc->grestore(plot->pc);

}

static void
gtk_plot_print_draw_rectangle(GtkPlotPC *pc, gint x, gint y, gint x1, gint y1, gint filled)
{
  GdkPoint point[4];

  point[0].x = x ;
  point[0].y = y ;

  point[1].x = x1;
  point[1].y = y;

  point[2].x = x1;
  point[2].y = y1;

  point[3].x = x;
  point[3].y = y1;
 
  pc->drawpolygon(pc, point, 4, filled);

}

static void
gtk_plot_print_draw_square(GtkPlotPC *pc, gint x, gint y, gint size, gint filled)
{
  GdkPoint point[4];

  point[0].x = x - size/2;
  point[0].y = y - size/2;

  point[1].x = x + size/2;
  point[1].y = y - size/2.;

  point[2].x = x + size/2;
  point[2].y = y + size/2;

  point[3].x = x - size/2;
  point[3].y = y + size/2.;
 
  pc->drawpolygon(pc, point, 4, filled);

}


static void
gtk_plot_print_draw_downtriangle(GtkPlotPC *pc, gint x, gint y, gint size, gint filled)
{
  GdkPoint point[3];
  gdouble pi = acos(-1.);

  point[0].x = x - roundint((gdouble)size*cos(pi/6.)/2.);
  point[0].y = y - roundint((gdouble)size*sin(pi/6.)/2.);

  point[1].x = x + roundint((gdouble)size*cos(pi/6.)/2.);
  point[1].y = y - roundint((gdouble)size*sin(pi/6.)/2.);

  point[2].x = x;
  point[2].y = y + size/2;

  pc->drawpolygon(pc, point, 3, filled);

}

static void
gtk_plot_print_draw_uptriangle(GtkPlotPC *pc, gint x, gint y, gint size, gint filled)
{
  GdkPoint point[3];
  gdouble pi = acos(-1.);

  point[0].x = x - roundint((gdouble)size*cos(pi/6.)/2.);
  point[0].y = y + roundint((gdouble)size*sin(pi/6.)/2.);

  point[1].x = x + roundint((gdouble)size*cos(pi/6.)/2.);
  point[1].y = y + roundint((gdouble)size*sin(pi/6.)/2.);

  point[2].x = x;
  point[2].y = y - size/2;

  pc->drawpolygon (pc, point, 3, filled);

}

static void
gtk_plot_print_draw_diamond(GtkPlotPC *pc, gint x, gint y, gint size, gint filled)
{
  GdkPoint point[4];

  point[0].x = x - size/2;
  point[0].y = y;

  point[1].x = x;
  point[1].y = y - size/2.;

  point[2].x = x + size/2;
  point[2].y = y;

  point[3].x = x;
  point[3].y = y + size/2.;
 
  pc->drawpolygon(pc, point, 4, filled);

}

static void
gtk_plot_print_draw_plus(GtkPlotPC *pc, gint x, gint y, gint size)
{
  pc->drawline(pc, x-size/2, y, x+size/2, y);

  pc->drawline(pc, x, y-size/2, x, y+size/2);
}

static void
gtk_plot_print_draw_cross(GtkPlotPC *pc, gint x, gint y, gint size)
{
  pc->drawline(pc, x-size/2, y-size/2, x+size/2, y+size/2);

  pc->drawline(pc, x-size/2, y+size/2, x+size/2, y-size/2);
}

static void
gtk_plot_print_draw_star(GtkPlotPC *pc, gint x, gint y, gint size)
{
  gdouble s2 = roundint((gdouble)size*sqrt(2.)/4.);

  pc->drawline(pc, x-size/2, y, x+size/2, y);

  pc->drawline(pc, x, y-size/2, x, y+size/2);

  pc->drawline(pc, x-s2, y-s2, x+s2, y+s2);

  pc->drawline(pc, x-s2, y+s2, x+s2, y-s2);
}

static void
gtk_plot_print_draw_text(GtkWidget *widget, GtkPlotPC *pc, GtkPlotText text)
{
  gchar *psfont;
  gint x, y;

  x = widget->allocation.x + text.x * widget->allocation.width;
  y = widget->allocation.y + text.y * widget->allocation.height;

  psfont = gtk_psfont_get_psfontname(text.font);

  pc->setcolor(pc, &text.fg);
  pc->setfont(pc, psfont, text.height);
  pc->drawstring(pc, x, y, text.justification, text.angle, psfont, text.height, text.text); 
}

static void gtk_plot_print_draw_child		(GtkWidget *widget, 
						 GtkPlotPC *pc,
						 GtkPlotCanvasChild *child)
{
  GtkPlotCanvas *canvas;
  GtkPlotCanvasLine *line = NULL;
  GtkPlotCanvasRectangle *rectangle = NULL;
  GtkPlotCanvasEllipse *ellipse = NULL;
  GtkPlotText *text = NULL;
  GdkPoint arrow[3];
  gint rx1 = 0, ry1 = 0, rx2 = 0, ry2 = 0;
  gint xmin, xmax, ymin, ymax;
  gint width = 0, height = 0;
  gint xm = 0, ym = 0;
  gdouble angle = 0.;
  gint arrow_width = ARROW_LENGTH / 3;

  canvas = GTK_PLOT_CANVAS(widget);

  rx1 = child->rx1 * widget->allocation.width;
  ry1 = child->ry1 * widget->allocation.height;
  rx2 = child->rx2 * widget->allocation.width;
  ry2 = child->ry2 * widget->allocation.height;

  xmin = MIN(rx1, rx2);
  xmax = MAX(rx1, rx2);
  ymin = MIN(ry1, ry2);
  ymax = MAX(ry1, ry2);

  width = abs(rx2 - rx1);
  height = abs(ry2 - ry1);

  switch(child->type){
    case GTK_PLOT_CANVAS_LINE:
        line = (GtkPlotCanvasLine *)child->data;

        if(width == 0 && height == 0) return;
        if(width != 0)
            angle = atan2((gdouble)(ry2-ry1), (gdouble)(rx2 - rx1));
        else  
            angle = asin((ry2 - ry1)/height);
        arrow_width = line->arrow_length / 3;
        gtk_plot_print_set_line_style(pc, 
                                      line->line.line_style,
                                      line->line.line_width,
                                      line->line.color);
        pc->drawline(pc, rx1, ry1, rx2, ry2);
        if(line->arrow_mask & GTK_PLOT_ARROW_END){
           arrow[1].x = rx2;
           arrow[1].y = ry2;
           xm = rx2 - roundint(cos(angle) * line->arrow_length);
           ym = ry2 - roundint(sin(angle) * line->arrow_length);
           arrow[0].x = xm + roundint(sin(angle)* arrow_width);
           arrow[0].y = ym - roundint(cos(angle)* arrow_width);
           arrow[2].x = xm - roundint(sin(angle)* arrow_width);
           arrow[2].y = ym + roundint(cos(angle)* arrow_width);
           pc->drawpolygon (pc, arrow, 3, TRUE);
        }
        if(line->arrow_mask & GTK_PLOT_ARROW_ORIGIN){
           arrow[1].x = rx1;
           arrow[1].y = ry1;
           xm = rx1 + roundint(cos(angle) * line->arrow_length);
           ym = ry1 + roundint(sin(angle) * line->arrow_length);
           arrow[0].x = xm + roundint(sin(angle)* arrow_width);
           arrow[0].y = ym - roundint(cos(angle)* arrow_width);
           arrow[2].x = xm - roundint(sin(angle)* arrow_width);
           arrow[2].y = ym + roundint(cos(angle)* arrow_width);
           pc->drawpolygon (pc, arrow, 3, TRUE);
        }
        break;  
    case GTK_PLOT_CANVAS_RECTANGLE:
        rectangle = (GtkPlotCanvasRectangle *)child->data;
        if(rectangle->filled){
           pc->setcolor(pc, &rectangle->bg);
           gtk_plot_print_draw_rectangle(pc, 
                              xmin, ymin, xmin+width, ymin+height, TRUE);
        }
        if(rectangle->line.line_style != GTK_PLOT_LINE_NONE &&
           rectangle->border != GTK_PLOT_BORDER_NONE){

            gtk_plot_print_set_line_style(pc, 
                                          rectangle->line.line_style,
                                          rectangle->line.line_width,
                                          rectangle->line.color);
            gtk_plot_print_draw_rectangle(pc, 
                                  xmin, ymin, xmin+width, ymin+height, FALSE);

            if(rectangle->border == GTK_PLOT_BORDER_SHADOW){
              gtk_plot_print_draw_rectangle(pc,
                                            xmin + rectangle->shadow_width,
                                            ymin + height,
                                            xmin + rectangle->shadow_width + width, 
                                            ymin + rectangle->shadow_width + height, 
                                            TRUE);
              gtk_plot_print_draw_rectangle(pc,
                                            xmin + width,
                                            ymin + rectangle->shadow_width,
                                            xmin + rectangle->shadow_width + width, 
                                            ymin + rectangle->shadow_width + height,
                                            TRUE);
            }
        }
        break;  
    case GTK_PLOT_CANVAS_ELLIPSE:
        ellipse = (GtkPlotCanvasEllipse *)child->data;
        if(ellipse->filled){
           pc->setcolor(pc, &ellipse->bg);
           pc->drawellipse(pc, xmin, ymin, width, height, TRUE);
        }
        if(ellipse->line.line_style != GTK_PLOT_LINE_NONE){
          gtk_plot_print_set_line_style(pc, 
                                        ellipse->line.line_style,
                                        ellipse->line.line_width,
                                        ellipse->line.color);
          pc->drawellipse(pc, xmin, ymin, width, height, FALSE);
        }
        break;  
        break;  
    case GTK_PLOT_CANVAS_TEXT:
        text = (GtkPlotText *)child->data;
        gtk_plot_print_draw_text(GTK_WIDGET(canvas), pc, *text);
        break;
    case GTK_PLOT_CANVAS_CUSTOM:
    default:
        if(child->print) child->print((gpointer)pc, child);
        break;
  }

}

static void
parse_label(gdouble val, gint precision, gint style, gchar *label)
{
  gdouble auxval;
  gint intspace = 0;

  auxval = abs(val);

  if(auxval > 1)
    intspace = (gint)log10(auxval);

  switch(style){
    case GTK_PLOT_LABEL_EXP:
      sprintf (label, "%*.*E", 1, precision, val);
      break;
    case GTK_PLOT_LABEL_FLOAT:
    default:
      sprintf (label, "%*.*f", intspace, precision, val);
  }

}

static void
gtk_plot_print_draw_legends (GtkPlot *plot)
{
  GtkPlotPC *pc;
  GList *datasets;
  GtkPlotData *dataset;
  GdkRectangle legend_area;
  GtkAllocation allocation;
  GdkFont *font;
  gchar *psfont;
  gint x0, y0;
  gint x, y;
  gint width = 0;
  gint height;

  if(!plot->show_legends) return;

  pc = plot->pc;
  font = gtk_psfont_get_gdkfont(plot->legends_attr.font,
				   plot->legends_attr.height);
  psfont = gtk_psfont_get_psfontname(plot->legends_attr.font);

/* first draw the rectangle for the background */
  allocation = GTK_WIDGET(plot)->allocation;
  x0 = allocation.x + plot->x * allocation.width +
       plot->legends_x * plot->width * allocation.width;
  y0 = allocation.y + plot->y * allocation.height +
       plot->legends_y * plot->height * allocation.height;

  height = 8;

  datasets = g_list_first(plot->data_sets);
  while(datasets)
   {
     dataset = (GtkPlotData *)datasets->data;

     if(dataset->is_visible && dataset->show_legend)
       {
         height += font->ascent + font->descent;
         if(dataset->legend)
            width = MAX(width,
                    gdk_string_width(font, dataset->legend));
       }

     datasets = datasets->next;
   }

  legend_area.x = x0;
  legend_area.y = y0;
  legend_area.width = width + plot->legends_line_width + 12;
  legend_area.height = height;

  if(!plot->legends_attr.transparent){
     pc->setcolor(pc, &plot->legends_attr.bg);
     gtk_plot_print_draw_rectangle(pc, 
                     legend_area.x, legend_area.y,
                     legend_area.x+legend_area.width, 
                     legend_area.y+legend_area.height,
                     TRUE);
  }

  plot->legends_width = legend_area.width;
  plot->legends_height = legend_area.height;

/* now draw the legends */

  height = 4;

  datasets = plot->data_sets;
  while(datasets)
   {
     dataset = (GtkPlotData *)datasets->data;

     if(dataset->is_visible && dataset->show_legend)
       {
         x = x0 + 4;
         height += font->ascent + font->descent;
         y = y0 + height;

         gtk_plot_print_set_line_style(pc, dataset->line.line_style,
                                       dataset->line.line_width,
			               dataset->line.color);

         if(dataset->line_connector != GTK_PLOT_CONNECT_NONE || 
            dataset->symbol.symbol_type == GTK_PLOT_SYMBOL_IMPULSE)
               pc->drawline(pc,
                            x, y - font->ascent / 2,
                            x + plot->legends_line_width, y - font->ascent / 2);

         pc->setcolor(pc, &plot->legends_attr.fg);


         if(dataset->symbol.symbol_type != GTK_PLOT_SYMBOL_BAR &&
            dataset->symbol.symbol_type != GTK_PLOT_SYMBOL_IMPULSE)
                      gtk_plot_print_draw_point (plot,
                                   x + plot->legends_line_width / 2,
                                   y - font->ascent / 2, 0,
                                   dataset->symbol.symbol_type,
                                   dataset->symbol.color,
                                   dataset->symbol.symbol_style,
                                   dataset->symbol.size,
                                   dataset->symbol.line_width);

         if(dataset->legend){
                pc->setcolor(pc, &plot->legends_attr.fg);
                pc->setfont(pc, psfont, plot->legends_attr.height);
                pc->drawstring (pc,
                                x + plot->legends_line_width + 4, y,
                                GTK_JUSTIFY_LEFT, 0, psfont, 
                                plot->legends_attr.height,
                                dataset->legend);
         }
       }
     datasets=datasets->next;
   }

   gtk_plot_print_set_line_style(pc, GTK_PLOT_LINE_SOLID,
				 plot->legends_border_width,
				 plot->legends_attr.fg);

   if(plot->legends_border != GTK_PLOT_BORDER_NONE)
      {
        gtk_plot_print_draw_rectangle(pc,
                        legend_area.x, legend_area.y,
                        legend_area.x + legend_area.width, 
                        legend_area.y + legend_area.height, 
                        FALSE);
      }

   pc->setlinewidth(pc, 0);
   if(plot->legends_border == GTK_PLOT_BORDER_SHADOW)
      {
        gtk_plot_print_draw_rectangle(pc,
                        legend_area.x + plot->legends_shadow_width,
                        legend_area.y + legend_area.height,
                        legend_area.x + plot->legends_shadow_width +
                        legend_area.width, 
                        legend_area.y + legend_area.height +
                        plot->legends_shadow_width,
                        TRUE);
        gtk_plot_print_draw_rectangle(pc,
                        legend_area.x + legend_area.width,
                        legend_area.y + plot->legends_shadow_width,
                        legend_area.x + legend_area.width +
                        plot->legends_shadow_width , 
                        legend_area.y + plot->legends_shadow_width +
                        legend_area.height,
			TRUE);
      }

  gdk_font_unref(font);
}

static void
gtk_plot_print_calc_ticks(GtkPlot *plot, gint orientation)
{
  GtkPlotTicks *ticks = NULL;
  GtkPlotScale scale;
  gdouble min = 0., max = 0.;
  gdouble absmax = 0., absmin = 0.;
  gint pt;
  gdouble tick;
  gdouble major_step;
  gdouble minor_step;
  gint nmajor, nminor;
  gint n;


  if(orientation == GTK_ORIENTATION_HORIZONTAL)
      scale = plot->xscale;
  else
      scale = plot->yscale;

  switch(orientation){
    case GTK_ORIENTATION_HORIZONTAL:
       ticks = &plot->xticks;
       max = plot->xmax;
       min = plot->xmin;
       if(scale == GTK_PLOT_SCALE_LOG10){
         if(max <= 1.E-12) max = 1.E-2;
         if(min <= 1.E-12) min = max/1000.;
         min = floor(log10(min));
         min = pow(10., min);
         plot->xmin = min;
         plot->xmax = max;
       }
       absmin = plot->xmin;
       absmax = plot->xmax;
       break;
    case GTK_ORIENTATION_VERTICAL:
       ticks = &plot->yticks;
       max = plot->ymax;
       min = plot->ymin;
       if(scale == GTK_PLOT_SCALE_LOG10){
         if(max <= 1.E-12) max = 1.E-2;
         if(min <= 1.E-12) min = max/1000.;
         min = floor(log10(min));
         min = pow(10., min);
         plot->ymin = min;
         plot->ymax = max;
       }
       absmin = plot->ymin;
       absmax = plot->ymax;
       break;
  }

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
   tick = min;
   while(tick <= absmax + 2*fabs(major_step)){
     if(tick >= min-1.E-10 && tick <= absmax+1.E-10){
        if(orientation == GTK_ORIENTATION_HORIZONTAL)
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
/********************************************************************/
/***** The following line makes gcc2.8.1 core dump              ****/
#if 0
           tick = pow(10., log10(absmin)+nmajor*major_step);
#endif
 /********************************************************************/
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
        if(orientation == GTK_ORIENTATION_HORIZONTAL)
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


