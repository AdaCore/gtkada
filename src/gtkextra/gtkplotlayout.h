/* gtkplotlayout - gtkplot layout widget for gtk+
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

#ifndef __GTK_PLOT_LAYOUT_H__
#define __GTK_PLOT_LAYOUT_H__


#include <gdk/gdk.h>
#include "gtkplot.h"

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


#define GTK_PLOT_LAYOUT(obj)        GTK_CHECK_CAST (obj, gtk_plot_layout_get_type (), GtkPlotLayout)
#define GTK_PLOT_LAYOUT_CLASS(klass) GTK_CHECK_CLASS_CAST (klass, gtk_plot_layout_get_type, GtkPlotLayoutClass)
#define GTK_IS_PLOT_LAYOUT(obj)     GTK_CHECK_TYPE (obj, gtk_plot_layout_get_type ())

typedef struct _GtkPlotLayout	GtkPlotLayout;
typedef struct _GtkPlotLayoutClass	GtkPlotLayoutClass;

struct _GtkPlotLayout
{
  GtkLayout layout;

  GdkPixmap *pixmap;
  
  guint8 flags;

  GdkColor background;

  gint width;
  gint height;

  GdkRectangle area;

  gint num_plots;
  GList *plots;

  GList *text;
  GList *childs;

  gint cursor_type;
};

struct _GtkPlotLayoutClass
{
  GtkLayoutClass parent_class;
};


guint		gtk_plot_layout_get_type	(void);
GtkWidget*	gtk_plot_layout_new		(gint width, gint height);
void 		gtk_plot_layout_add_plot 	(GtkPlotLayout *plot_layout, 
					 	GtkPlot *plot, 
					 	gint x, gint y);
void		gtk_plot_layout_set_background	(GtkPlotLayout *plot_layout,
						 GdkColor *color);
void		gtk_plot_layout_refresh		(GtkPlotLayout *layout);
GtkPlotText * 	gtk_plot_layout_put_text 	(GtkPlotLayout *layout,
                          			 gdouble x, 
						 gdouble y, 
						 gint angle,
                          			 const gchar *font, 
                                                 gint height,
						 GdkColor *fg, 
						 GdkColor *bg, 
						 GtkJustification justification,
						 const gchar *text);
gint 		gtk_plot_layout_remove_text	(GtkPlotLayout *layout, 
						 GtkPlotText *text);
void 		gtk_plot_layout_get_pixel	(GtkPlotLayout *plot_layout, 
						 gdouble px, gdouble py,
                          			 gint *x, gint *y);
void 		gtk_plot_layout_get_position	(GtkPlotLayout *plot_layout, 
						 gint x, gint y,
                             			 gdouble *px, gdouble *py);
void            gtk_plot_layout_set_size        (GtkPlotLayout *layout,
                                                 gint width, gint height);



#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* __GTK_PLOT_LAYOUT_H__ */
