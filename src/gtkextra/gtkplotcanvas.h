/* gtkplotcanvas - gtkplot canvas widget for gtk+
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

#ifndef __GTK_PLOT_CANVAS_H__
#define __GTK_PLOT_CANVAS_H__


#include <gdk/gdk.h>
#include "gtkplotlayout.h"

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/* canvas flags */
enum
{
      GTK_PLOT_CANVAS_CAN_RESIZE_PLOT	=	1 << 0,
      GTK_PLOT_CANVAS_CAN_MOVE_PLOT	=	1 << 1,
      GTK_PLOT_CANVAS_CAN_MOVE_LEGENDS	=	1 << 2,
      GTK_PLOT_CANVAS_CAN_MOVE_TITLES	=	1 << 3,
      GTK_PLOT_CANVAS_CAN_MOVE_TEXT	=	1 << 4,
      GTK_PLOT_CANVAS_CAN_DND_POINT	=	1 << 5,
      GTK_PLOT_CANVAS_ALLOCATE_TITLES	=	1 << 6
};


/* canvas actions */
enum
{
      GTK_PLOT_CANVAS_INACTIVE,
      GTK_PLOT_CANVAS_RESIZE_PLOT,
      GTK_PLOT_CANVAS_MOVE_PLOT,
      GTK_PLOT_CANVAS_MOVE_LEGENDS,
      GTK_PLOT_CANVAS_MOVE_TITLE,
      GTK_PLOT_CANVAS_MOVE_TEXT,
      GTK_PLOT_CANVAS_DND_POINT,
      GTK_PLOT_CANVAS_IN_SELECTION
};

#define GTK_PLOT_CANVAS_DND_FLAGS      (GTK_PLOT_CANVAS_CAN_RESIZE_PLOT |  \
                                        GTK_PLOT_CANVAS_CAN_MOVE_PLOT |    \
                                        GTK_PLOT_CANVAS_CAN_MOVE_LEGENDS | \
                                        GTK_PLOT_CANVAS_CAN_MOVE_TITLES |  \
                                        GTK_PLOT_CANVAS_CAN_MOVE_TEXT |    \
                                        GTK_PLOT_CANVAS_CAN_DND_POINT)


#define GTK_PLOT_CANVAS(obj)        GTK_CHECK_CAST (obj, gtk_plot_canvas_get_type (), GtkPlotCanvas)
#define GTK_PLOT_CANVAS_CLASS(klass) GTK_CHECK_CLASS_CAST (klass, gtk_plot_canvas_get_type, GtkPlotCanvasClass)
#define GTK_IS_PLOT_CANVAS(obj)     GTK_CHECK_TYPE (obj, gtk_plot_canvas_get_type ())
#define GTK_PLOT_CANVAS_FLAGS(canvas)	  (GTK_PLOT_CANVAS(canvas)->flags)
#define GTK_PLOT_CANVAS_SET_FLAGS(canvas, flags)  (GTK_PLOT_CANVAS_FLAGS(canvas) |= (flags))
#define GTK_PLOT_CANVAS_UNSET_FLAGS(canvas, flags)  (GTK_PLOT_CANVAS_FLAGS(canvas) &= ~(flags))
#define GTK_PLOT_CANVAS_CAN_DND_POINT(canvas)  (GTK_PLOT_CANVAS_FLAGS(canvas) & GTK_PLOT_CANVAS_CAN_DND_POINT)
#define GTK_PLOT_CANVAS_CAN_RESIZE_PLOT(canvas)  (GTK_PLOT_CANVAS_FLAGS(canvas) & GTK_PLOT_CANVAS_CAN_RESIZE_PLOT)
#define GTK_PLOT_CANVAS_CAN_MOVE_PLOT(canvas)  (GTK_PLOT_CANVAS_FLAGS(canvas) & GTK_PLOT_CANVAS_CAN_MOVE_PLOT)
#define GTK_PLOT_CANVAS_CAN_MOVE_TITLES(canvas)  (GTK_PLOT_CANVAS_FLAGS(canvas) & GTK_PLOT_CANVAS_CAN_MOVE_TITLES)
#define GTK_PLOT_CANVAS_CAN_MOVE_TEXT(canvas)  (GTK_PLOT_CANVAS_FLAGS(canvas) & GTK_PLOT_CANVAS_CAN_MOVE_TEXT)
#define GTK_PLOT_CANVAS_CAN_MOVE_LEGENDS(canvas)  (GTK_PLOT_CANVAS_FLAGS(canvas)& GTK_PLOT_CANVAS_CAN_MOVE_LEGENDS)
#define GTK_PLOT_CANVAS_ALLOCATE_TITLES(canvas)  (GTK_PLOT_CANVAS_FLAGS(canvas)& GTK_PLOT_CANVAS_ALLOCATE_TITLES)


typedef struct _GtkPlotCanvas	GtkPlotCanvas;
typedef struct _GtkPlotCanvasClass	GtkPlotCanvasClass;

struct _GtkPlotCanvas
{
  GtkPlotLayout plot_layout;

  guint16 flags;
  guint16 action;

  GtkPlot *active_plot;
  GtkPlotData *active_data;
  GtkPlotText *active_text;
  gint active_point;
  gdouble active_x, active_y;

  gint drag_x, drag_y;
  gint pointer_x, pointer_y;

};

struct _GtkPlotCanvasClass
{
  GtkPlotLayoutClass parent_class;

  gint (*click_on_plot)		(GtkPlotCanvas *canvas,
				 GdkEventButton *event);

  gint (*click_on_point)	(GtkPlotCanvas *canvas,
				 GdkEventButton *event);

  gint (*click_on_legends)	(GtkPlotCanvas *canvas,
				 GdkEventButton *event);

  gint (*click_on_axis)		(GtkPlotCanvas *canvas,
				 GdkEventButton *event,
                                 GtkPlotAxis *axis);

  gint (*click_on_title)	(GtkPlotCanvas *canvas,
				 GdkEventButton *event,
                                 GtkPlotAxis *axis);

  gint (*click_on_text)		(GtkPlotCanvas *canvas,
				 GdkEventButton *event);

  void (*select_region)		(GtkPlotCanvas *canvas,
				 gdouble xmin, gdouble xmax,
				 gdouble ymin, gdouble ymax);
};


guint		gtk_plot_canvas_get_type	(void);
GtkWidget*	gtk_plot_canvas_new		(gint width, gint height);
void 		gtk_plot_canvas_add_plot 	(GtkPlotCanvas *plot_canvas, 
					 	 GtkPlot *plot, 
					 	 gdouble x, gdouble y);
void		gtk_plot_canvas_set_active_plot (GtkPlotCanvas *plot_canvas,
						 GtkPlot *plot);
void		gtk_plot_canvas_cancel_action	(GtkPlotCanvas *plot_canvas);
GtkPlot *       gtk_plot_canvas_get_active_plot (GtkPlotCanvas *canvas);
GtkPlotData *   gtk_plot_canvas_get_active_dataset (GtkPlotCanvas *canvas);
void  		gtk_plot_canvas_get_active_point (GtkPlotCanvas *canvas,
						  gdouble *x, gdouble *y);
GtkPlotText *   gtk_plot_canvas_get_active_text (GtkPlotCanvas *canvas);
void            gtk_plot_canvas_set_size        (GtkPlotCanvas *canvas,
                                                 gint width, gint height);



#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* __GTK_PLOT_CANVAS_H__ */
