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

#ifndef __GTK_PLOT_PC_H__
#define __GTK_PLOT_PC_H__

#include <stdio.h>
#include "gtkpsfont.h"

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/* Page size */

enum{
     GTK_PLOT_LETTER	,
     GTK_PLOT_LEGAL	,
     GTK_PLOT_A4	,
     GTK_PLOT_EXECUTIVE	,
     GTK_PLOT_CUSTOM	
};

#define GTK_PLOT_LETTER_W 	612   /* Width and Height in ps points */
#define GTK_PLOT_LETTER_H 	792

#define GTK_PLOT_LEGAL_W	612
#define GTK_PLOT_LEGAL_H	1008

#define GTK_PLOT_A4_W		595
#define GTK_PLOT_A4_H		842

#define GTK_PLOT_EXECUTIVE_W	540
#define GTK_PLOT_EXECUTIVE_H	720


/* Page orientation */
enum{
     GTK_PLOT_PORTRAIT	,
     GTK_PLOT_LANDSCAPE	
};

/* Size units */
enum{
     GTK_PLOT_PSPOINTS	,
     GTK_PLOT_MM	,
     GTK_PLOT_CM	,
     GTK_PLOT_INCHES	
};

typedef struct _GtkPlotPC GtkPlotPC;

struct _GtkPlotPC
{
   FILE *pcfile;
   gchar *pcname;
   gint orientation;
   gint epsflag;

   gint type;
   /* measure units for page size */
   gint units;  
   gfloat width;
   gfloat height;

   /* page size in points (depends on orientation) */
   gint page_width;
   gint page_height;

   void  (* init)					(GtkPlotPC *pc,
							 gfloat scale_x,
							 gfloat scale_y);

   void  (* leave)					(GtkPlotPC *pc);

   void  (* gsave)					(GtkPlotPC *pc);

   void  (* grestore)					(GtkPlotPC *pc);

   void  (* clip)					(GtkPlotPC *pc,
							 GdkRectangle area);

   void  (* setcolor)                     		(GtkPlotPC *pc,
                                                 	GdkColor *color);

   void  (* setdash)					(GtkPlotPC *pc,
							 gint num_values,
							 gdouble *values,
							 gdouble offset);

   void  (* setlinewidth)				(GtkPlotPC *pc,
							 gint width);

   void  (* setlinecaps)				(GtkPlotPC *pc,
							 gint caps);

   void  (* drawline)					(GtkPlotPC *pc,
							 gint x1, gint y1,
							 gint x2, gint y2);

   void  (* drawlines)					(GtkPlotPC *pc,
							 GdkPoint *points,
							 gint numpoints);

   void  (* drawpolygon)	                        (GtkPlotPC *pc,
                                	                 GdkPoint *points,
                                        	         gint numpoints,
                                                	 gint filled);

   void  (* drawcircle) 	                        (GtkPlotPC *pc,
                                                 	 gint x, gint y,
                                                 	 gint size, 
							 gint filled);

   void  (* setfont)					(GtkPlotPC *pc,
							 gchar *font,
							 gint height);

   void  (* drawstring)   	                        (GtkPlotPC *pc,
                                   	             	 gint x, gint y,
                                        	         gint justification,
                                                	 gint angle,
							 gchar *font,
							 gint height,
                                               		 gchar *text);
};

GtkPlotPC *gtk_plot_pc_new				(gchar *psname,
							 gint orientation,
							 gint page_size);
							 
GtkPlotPC *gtk_plot_pc_new_with_size			(gchar *psname,
							 gint orientation,
							 gint units,
							 gfloat width,
							 gfloat height);

void gtk_plot_pc_set_size			        (GtkPlotPC *pc,
							 gint units,
							 gfloat width,
							 gfloat height);

void gtk_plot_pc_init					(GtkPlotPC *pc,
							 gfloat scale_x,
							 gfloat scale_y);

void gtk_plot_pc_leave					(GtkPlotPC *pc);

void gtk_plot_pc_gsave					(GtkPlotPC *pc);

void gtk_plot_pc_grestore				(GtkPlotPC *pc);

void gtk_plot_pc_clip					(GtkPlotPC *pc,
							 GdkRectangle area);

void gtk_plot_pc_setcolor                     		(GtkPlotPC *pc,
                                                 	GdkColor *color);

void gtk_plot_pc_setdash				(GtkPlotPC *pc,
							 gint num_values,
							 gdouble *values,
							 gdouble offset);

void gtk_plot_pc_setlinewidth				(GtkPlotPC *pc,
							 gint width);

void gtk_plot_pc_setlinecaps				(GtkPlotPC *pc,
							 gint caps);

void gtk_plot_pc_drawline				(GtkPlotPC *pc,
							 gint x1, gint y1,
							 gint x2, gint y2);

void gtk_plot_pc_drawlines				(GtkPlotPC *pc,
							 GdkPoint *points,
							 gint numpoints);

void gtk_plot_pc_drawpolygon	                        (GtkPlotPC *pc,
                                	                 GdkPoint *points,
                                        	         gint numpoints,
                                                	 gint filled);

void gtk_plot_pc_drawcircle 	                        (GtkPlotPC *pc,
                                                 	 gint x, gint y,
                                                 	 gint size, 
							 gint filled);

void gtk_plot_pc_setfont				(GtkPlotPC *pc,
							 gchar *font,
							 gint height);

void gtk_plot_pc_drawstring   	                        (GtkPlotPC *pc,
                                   	             	 gint x, gint y,
                                        	         gint justification,
                                                	 gint angle,
							 gchar *font,
							 gint height,
                                               		 gchar *text);


#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* __GTK_PLOT_PC_H__ */

