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
      GTK_PLOT_CANVAS_CAN_SELECT	=	1 << 0, /* Select region */
      GTK_PLOT_CANVAS_CAN_SELECT_ITEM	=	1 << 1, /* Select item */
      GTK_PLOT_CANVAS_CAN_SELECT_POINT	=	1 << 2, /* Select point */
      GTK_PLOT_CANVAS_CAN_DND		=	1 << 3, /* DnD item */
      GTK_PLOT_CANVAS_CAN_DND_POINT	=	1 << 4, /* DnD point */
};


/* canvas actions */
typedef enum
{
      GTK_PLOT_CANVAS_ACTION_INACTIVE,
      GTK_PLOT_CANVAS_ACTION_SELECTION,
      GTK_PLOT_CANVAS_ACTION_DRAG,
      GTK_PLOT_CANVAS_ACTION_RESIZE,
} GtkPlotCanvasAction;

typedef enum
{
      GTK_PLOT_CANVAS_FROZEN            = 0,
      GTK_PLOT_CANVAS_CAN_MOVE          = 1 << 0,
      GTK_PLOT_CANVAS_CAN_X_RESIZE	= 1 << 1,
      GTK_PLOT_CANVAS_CAN_Y_RESIZE	= 1 << 2,
} GtkPlotCanvasFlag;

typedef enum
{
      GTK_PLOT_CANVAS_NONE,
      GTK_PLOT_CANVAS_PLOT,
      GTK_PLOT_CANVAS_AXIS,
      GTK_PLOT_CANVAS_LEGENDS,
      GTK_PLOT_CANVAS_TITLE,
      GTK_PLOT_CANVAS_TEXT,
      GTK_PLOT_CANVAS_DATA,
} GtkPlotCanvasType;

typedef enum
{
      GTK_PLOT_CANVAS_OUT,
      GTK_PLOT_CANVAS_IN,
      GTK_PLOT_CANVAS_LEFT,
      GTK_PLOT_CANVAS_RIGHT,
      GTK_PLOT_CANVAS_TOP,
      GTK_PLOT_CANVAS_BOTTOM,
      GTK_PLOT_CANVAS_TOP_LEFT,
      GTK_PLOT_CANVAS_TOP_RIGHT,
      GTK_PLOT_CANVAS_BOTTOM_LEFT,
      GTK_PLOT_CANVAS_BOTTOM_RIGHT,
} GtkPlotCanvasPos;

#define GTK_PLOT_CANVAS_DND_FLAGS      (GTK_PLOT_CANVAS_CAN_SELECT_ITEM |  \
                                        GTK_PLOT_CANVAS_CAN_SELECT_POINT | \
                                        GTK_PLOT_CANVAS_CAN_DND | \
                                        GTK_PLOT_CANVAS_CAN_DND_POINT)


#define GTK_PLOT_CANVAS(obj)        GTK_CHECK_CAST (obj, gtk_plot_canvas_get_type (), GtkPlotCanvas)
#define GTK_PLOT_CANVAS_CLASS(klass) GTK_CHECK_CLASS_CAST (klass, gtk_plot_canvas_get_type, GtkPlotCanvasClass)
#define GTK_IS_PLOT_CANVAS(obj)     GTK_CHECK_TYPE (obj, gtk_plot_canvas_get_type ())
#define GTK_PLOT_CANVAS_FLAGS(canvas)	  (GTK_PLOT_CANVAS(canvas)->flags)
#define GTK_PLOT_CANVAS_SET_FLAGS(canvas, flags)  (GTK_PLOT_CANVAS_FLAGS(canvas) |= (flags))
#define GTK_PLOT_CANVAS_UNSET_FLAGS(canvas, flags)  (GTK_PLOT_CANVAS_FLAGS(canvas) &= ~(flags))
#define GTK_PLOT_CANVAS_CAN_DND_POINT(canvas)  (GTK_PLOT_CANVAS_FLAGS(canvas) & GTK_PLOT_CANVAS_CAN_DND_POINT)
#define GTK_PLOT_CANVAS_CAN_DND(canvas)  (GTK_PLOT_CANVAS_FLAGS(canvas) & GTK_PLOT_CANVAS_CAN_DND)
#define GTK_PLOT_CANVAS_CAN_SELECT_POINT(canvas)  (GTK_PLOT_CANVAS_FLAGS(canvas) & GTK_PLOT_CANVAS_CAN_SELECT_POINT)
#define GTK_PLOT_CANVAS_CAN_SELECT_ITEM(canvas)  (GTK_PLOT_CANVAS_FLAGS(canvas) & GTK_PLOT_CANVAS_CAN_SELECT_ITEM)
#define GTK_PLOT_CANVAS_CAN_SELECT(canvas)  (GTK_PLOT_CANVAS_FLAGS(canvas) & GTK_PLOT_CANVAS_CAN_SELECT)


typedef struct _GtkPlotCanvas	GtkPlotCanvas;
typedef struct _GtkPlotCanvasItem	GtkPlotCanvasItem;
typedef struct _GtkPlotCanvasClass	GtkPlotCanvasClass;

struct _GtkPlotCanvasItem
{
  GdkRectangle area, new_area;

  gint min_width, min_height;

  GtkPlotCanvasType type;
  GtkPlotCanvasFlag flags;
  guint state;

  gpointer data;
};

struct _GtkPlotCanvas
{
  GtkPlotLayout plot_layout;

  guint16 flags;
  guint state;
  GtkPlotCanvasAction action;

  GtkPlot *active_plot;
  GtkPlotData *active_data;
  gint active_point;
  gdouble active_x, active_y;

  GtkPlotCanvasItem active_item;

  GtkPlotCanvasPos drag_point;
  gint drag_x, drag_y;
  gint pointer_x, pointer_y;

};

struct _GtkPlotCanvasClass
{
  GtkPlotLayoutClass parent_class;

  gint (*select_item)		(GtkPlotCanvas *canvas,
				 GdkEventButton *event,
                                 GtkPlotCanvasItem *item);

  gint (*move_item)		(GtkPlotCanvas *canvas,
                                 GtkPlotCanvasItem *item,
				 gdouble new_x, gdouble new_y);

  gint (*resize_item)		(GtkPlotCanvas *canvas,
                                 GtkPlotCanvasItem *item,
				 gdouble new_width, gdouble new_height);

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
void		gtk_plot_canvas_unselect	(GtkPlotCanvas *plot_canvas);
GtkPlot *       gtk_plot_canvas_get_active_plot (GtkPlotCanvas *canvas);
GtkPlotData *   gtk_plot_canvas_get_active_dataset (GtkPlotCanvas *canvas);
void  		gtk_plot_canvas_get_active_point (GtkPlotCanvas *canvas,
						  gdouble *x, gdouble *y);
GtkPlotCanvasItem *   gtk_plot_canvas_get_active_item (GtkPlotCanvas *canvas);
void            gtk_plot_canvas_set_size        (GtkPlotCanvas *canvas,
                                                 gint width, gint height);



#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* __GTK_PLOT_CANVAS_H__ */
