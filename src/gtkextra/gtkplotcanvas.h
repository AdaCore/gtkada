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
#include "gtkplotpc.h"

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
      GTK_PLOT_ARROW_NONE            = 0,
      GTK_PLOT_ARROW_ORIGIN          = 1 << 0,
      GTK_PLOT_ARROW_END    	     = 1 << 1
} GtkPlotCanvasArrow;

typedef enum
{
      GTK_PLOT_CANVAS_SELECT_NONE,    
      GTK_PLOT_CANVAS_SELECT_MARKERS,
      GTK_PLOT_CANVAS_SELECT_TARGET,
} GtkPlotCanvasSelection;

typedef enum
{
      GTK_PLOT_CANVAS_SELECT_CLICK_1,    
      GTK_PLOT_CANVAS_SELECT_CLICK_2,
} GtkPlotCanvasSelectionMode;

typedef enum
{
      GTK_PLOT_CANVAS_NONE,
      GTK_PLOT_CANVAS_PLOT,
      GTK_PLOT_CANVAS_AXIS,
      GTK_PLOT_CANVAS_LEGENDS,
      GTK_PLOT_CANVAS_TITLE,
      GTK_PLOT_CANVAS_TEXT,
      GTK_PLOT_CANVAS_DATA,
      GTK_PLOT_CANVAS_LINE,
      GTK_PLOT_CANVAS_RECTANGLE,
      GTK_PLOT_CANVAS_ELLIPSE,
      GTK_PLOT_CANVAS_PIXMAP,
      GTK_PLOT_CANVAS_MARKER,
      GTK_PLOT_CANVAS_CUSTOM,
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
#define GTK_TYPE_PLOT_CANVAS (gtk_plot_canvas_get_type ())
#define GTK_TYPE_PLOT_CANVAS_CHILD (gtk_plot_canvas_child_get_type ())


typedef struct _GtkPlotCanvas	GtkPlotCanvas;
typedef struct _GtkPlotCanvasChild	GtkPlotCanvasChild;
typedef struct _GtkPlotCanvasLine	GtkPlotCanvasLine;
typedef struct _GtkPlotCanvasRectangle	GtkPlotCanvasRectangle;
typedef struct _GtkPlotCanvasEllipse	GtkPlotCanvasEllipse;
typedef struct _GtkPlotCanvasClass	GtkPlotCanvasClass;

struct _GtkPlotCanvasChild
{
  gdouble rx1, rx2, ry1, ry2;
  gint min_width, min_height;

  GdkRectangle allocation;

  guint state;

  GtkPlotCanvasType type;
  GtkPlotCanvasFlag flags;
  GtkPlotCanvasSelection selection;
  GtkPlotCanvasSelectionMode mode;

  gpointer data;

  void (*draw_child) (GtkPlotCanvas *canvas, GtkPlotCanvasChild *child);
};

struct _GtkPlotCanvasLine
{
  GtkPlotLine line;

  GtkPlotCanvasArrow arrow_mask;
  gint arrow_length;
  gint arrow_width;
  GtkPlotSymbolStyle arrow_style;
};

struct _GtkPlotCanvasRectangle
{
  GtkPlotLine line;

  gboolean filled;
  GtkPlotBorderStyle border;

  gint shadow_width;
  GdkColor bg;
};

struct _GtkPlotCanvasEllipse
{
  GtkPlotLine line;

  gboolean filled;

  GdkColor bg;
};

struct _GtkPlotCanvas
{
  GtkFixed fixed;

  guint16 flags;
  guint state;

  guint freeze_count;

  gint pixmap_width, pixmap_height;
  gint width, height;

  gdouble magnification;
  
  gboolean show_grid;
  gdouble grid_step;
  GtkPlotLine grid;

  GtkPlotCanvasAction action;

  GdkPixmap *pixmap;

  GdkColor background;
  gboolean transparent;

  GtkPlot *active_plot;
  GtkPlotData *active_data;
  gint active_point;
  gint active_lpoint, active_rpoint;
  gdouble active_x, active_y;

  GtkPlotCanvasChild active_item;

  GtkPlotCanvasPos drag_point;
  gint drag_x, drag_y;
  gint pointer_x, pointer_y;
  GdkRectangle drag_area;

  GList *plots;
  GList *childs;

  gint num_plots;

  GdkCursor *cursor;
  GtkPlotPC *pc;
};

struct _GtkPlotCanvasClass
{
  GtkLayoutClass parent_class;

  gboolean (*select_item)	(GtkPlotCanvas *canvas,
				 GdkEventButton *event,
                                 GtkPlotCanvasChild *item);

  gboolean (*move_item)		(GtkPlotCanvas *canvas,
                                 GtkPlotCanvasChild *item,
				 gdouble new_x, gdouble new_y);

  gboolean (*resize_item)	(GtkPlotCanvas *canvas,
                                 GtkPlotCanvasChild *item,
				 gdouble new_width, gdouble new_height);

  gboolean (*delete_item)	(GtkPlotCanvas *canvas,
                                 GtkPlotCanvasChild *item);

  void (*select_region)		(GtkPlotCanvas *canvas,
				 gdouble xmin, gdouble ymin,
				 gdouble xmax, gdouble ymax);

  void (*changed)		(GtkPlotCanvas *canvas);
};


GtkType		gtk_plot_canvas_get_type	(void);
GtkType		gtk_plot_canvas_child_get_type	(void);
GtkWidget*	gtk_plot_canvas_new		(gint width, gint height,
                                                 gdouble magnification);
void		gtk_plot_canvas_construct       (GtkPlotCanvas *canvas,
						 gint width, gint height,
                                                 gdouble magnification);
void		gtk_plot_canvas_set_pc          (GtkPlotCanvas *canvas,
						 GtkPlotPC *pc);
void		gtk_plot_canvas_paint           (GtkPlotCanvas *canvas);
void		gtk_plot_canvas_refresh         (GtkPlotCanvas *canvas);
void 		gtk_plot_canvas_freeze		(GtkPlotCanvas *canvas);
void 		gtk_plot_canvas_thaw		(GtkPlotCanvas *canvas);
void		gtk_plot_canvas_grid_set_visible(GtkPlotCanvas *canvas,
						 gboolean visible);
void		gtk_plot_canvas_grid_set_step	(GtkPlotCanvas *canvas,
						 gdouble step);
void		gtk_plot_canvas_grid_set_attributes(GtkPlotCanvas *canvas,
                         			 GtkPlotLineStyle style,
                         			 gint width,
                         			 const GdkColor *color);
void 		gtk_plot_canvas_add_plot 	(GtkPlotCanvas *plot_canvas, 
					 	 GtkPlot *plot, 
					 	 gdouble x, gdouble y);
void		gtk_plot_canvas_set_active_plot (GtkPlotCanvas *plot_canvas,
						 GtkPlot *plot);
void		gtk_plot_canvas_cancel_action	(GtkPlotCanvas *plot_canvas);
void		gtk_plot_canvas_unselect	(GtkPlotCanvas *plot_canvas);
GtkPlot *       gtk_plot_canvas_get_active_plot (GtkPlotCanvas *canvas);
GtkPlotData *   gtk_plot_canvas_get_active_data (GtkPlotCanvas *canvas);
gint  		gtk_plot_canvas_get_active_point (GtkPlotCanvas *canvas,
						  gdouble *x, gdouble *y);
GtkPlotCanvasChild *   
		gtk_plot_canvas_get_active_item (GtkPlotCanvas *canvas);
void            gtk_plot_canvas_set_size        (GtkPlotCanvas *canvas,
                                                 gint width, gint height);
void            gtk_plot_canvas_set_magnification (GtkPlotCanvas *canvas,
                                                 gdouble magnification);
void 		gtk_plot_canvas_set_transparent (GtkPlotCanvas *canvas, 
						 gboolean transparent);
gboolean 	gtk_plot_canvas_transparent 	(GtkPlotCanvas *canvas);
void		gtk_plot_canvas_set_background  (GtkPlotCanvas *canvas,
						 const GdkColor *background);
void            gtk_plot_canvas_get_pixel       (GtkPlotCanvas *plot_canvas,
                                                 gdouble px, gdouble py,
                                                 gint *x, gint *y);
void            gtk_plot_canvas_get_position    (GtkPlotCanvas *plot_canvas,
                                                 gint x, gint y,
                                                 gdouble *px, gdouble *py);

GtkPlotCanvasChild *   
		gtk_plot_canvas_put_text 	(GtkPlotCanvas *canvas,
                                                 gdouble x,
                                                 gdouble y,
                                                 const gchar *font,
                                                 gint height,
                                                 gint angle,
                                                 const GdkColor *fg,
                                                 const GdkColor *bg,
						 gboolean transparent,
                                                 GtkJustification justification,                                                 const gchar *text);
gboolean        gtk_plot_canvas_remove_child    (GtkPlotCanvas *canvas,
                                                 GtkPlotCanvasChild *child);
GtkPlotCanvasChild * 
		gtk_plot_canvas_put_line	(GtkPlotCanvas *canvas,
                         			 gdouble x1, gdouble y1,
                         			 gdouble x2, gdouble y2,
                         			 GtkPlotLineStyle style,
                         			 gfloat width,
                         			 const GdkColor *color,
                         			 GtkPlotCanvasArrow arrow_mask);
GtkPlotCanvasChild *
		gtk_plot_canvas_put_rectangle	(GtkPlotCanvas *canvas,
                              			 gdouble x1, gdouble y1,
                              			 gdouble x2, gdouble y2,
                              			 GtkPlotLineStyle style,
                              			 gfloat width,
                              			 const GdkColor *fg,
                              			 const GdkColor *bg,
                              			 GtkPlotBorderStyle border,
                              			 gboolean fill);
GtkPlotCanvasChild * 
		gtk_plot_canvas_put_ellipse	(GtkPlotCanvas *canvas,
                            			 gdouble x1, gdouble y1,
                            			 gdouble x2, gdouble y2,
                            			 GtkPlotLineStyle style,
                            			 gfloat width,
                            			 const GdkColor *fg,
                            			 const GdkColor *bg,
                            			 gboolean fill);
GtkPlotCanvasChild * 
		gtk_plot_canvas_put_pixmap	(GtkPlotCanvas *canvas,
						 GdkPixmap *pixmap,
                            			 gdouble x1, gdouble y1);

void 	gtk_plot_canvas_line_set_attributes     (GtkPlotCanvasChild *child,
                                    		GtkPlotLineStyle style,
                                    		gfloat width,
                                    		const GdkColor *color,
                                    		GtkPlotCanvasArrow mask);
void gtk_plot_canvas_rectangle_set_attributes	(GtkPlotCanvasChild *child,
                                         	GtkPlotLineStyle style,
                                         	gfloat width,
                                         	const GdkColor *fg,
                                         	const GdkColor *bg,
                                         	GtkPlotBorderStyle border,
                                         	gboolean fill);
void gtk_plot_canvas_ellipse_set_attributes	(GtkPlotCanvasChild *child,
                                       		GtkPlotLineStyle style,
                                       		gfloat width,
                                       		const GdkColor *fg,
                                       		const GdkColor *bg,
                                       		gboolean fill);
GtkPlotCanvasChild * 
gtk_plot_canvas_child_new			(GtkPlotCanvasType type);
void 		gtk_plot_canvas_put_child	(GtkPlotCanvas *canvas,
                          			 GtkPlotCanvasChild *child,
                          			 gdouble x1, gdouble y1,
                          			 gdouble x2, gdouble y2);
void gtk_plot_canvas_child_move			(GtkPlotCanvas *canvas,
                          			 GtkPlotCanvasChild *child,
                          			 gdouble x1, gdouble y1);
void gtk_plot_canvas_child_move_resize		(GtkPlotCanvas *canvas,
                                 		 GtkPlotCanvasChild *child,
                                 		 gdouble x1, gdouble y1,
                                  		 gdouble x2, gdouble y2);

void gtk_plot_canvas_child_set_selection	(GtkPlotCanvasChild *child,
						 GtkPlotCanvasSelection selection);
void gtk_plot_canvas_child_set_selection_mode	(GtkPlotCanvasChild *child,
						 GtkPlotCanvasSelectionMode mode);

#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* __GTK_PLOT_CANVAS_H__ */
