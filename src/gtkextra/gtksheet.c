/* GtkSheet widget for Gtk+.
 * Copyright (C) 1999-2001 Adrian E. Feiguin <adrian@ifir.ifir.edu.ar>
 *
 * Based on GtkClist widget by Jay Painter, but major changes.
 * Memory allocation routines inspired on SC (Spreadsheet Calculator)
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

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <glib.h>
#include <gdk/gdk.h>
#include <gdk/gdkkeysyms.h>
#include <gtk/gtksignal.h>
#include <gtk/gtklabel.h>
#include <gtk/gtkbutton.h>
#include <gtk/gtkadjustment.h>
#include <gtk/gtktable.h>
#include <gtk/gtkbox.h>
#include <gtk/gtkmain.h>
#include <gtk/gtktypeutils.h>
#include <gtk/gtkentry.h>
#include <gtk/gtkcontainer.h>
#include <gtk/gtkpixmap.h>
#include "gtkitementry.h"
#include "gtksheet.h"
 
#define CELL_SPACING 1
#define DRAG_WIDTH 6
#define TIMEOUT_SCROLL 20
#define TIMEOUT_FLASH 200
#define TIME_INTERVAL 8
#define COLUMN_MIN_WIDTH 10
#define MINROWS 1
#define MINCOLS 1
#define MAXLENGTH 30
#define CELLOFFSET 4
#define DEFAULT_COLUMN_WIDTH 80
#define DEFAULT_ROW_HEIGHT(widget) (widget->style->font->ascent+\
				    2 * widget->style->font->descent+\
				    2 * CELLOFFSET)  

/* scrollbar spacing class macro */
#define SCROLLBAR_SPACING(w) (GTK_SHEET_CLASS (GTK_OBJECT (w)->klass)->scrollbar_spacing)

/* gives the top pixel of the given row in context of
 * the sheet's voffset */
static inline gint
ROW_TOP_YPIXEL(GtkSheet *sheet, gint nrow)
{
   return (sheet->voffset + sheet->row[nrow].top_ypixel);
}


/* returns the row index from a y pixel location in the 
 * context of the sheet's voffset */
static inline gint 
ROW_FROM_YPIXEL(GtkSheet *sheet, gint y)
{
  gint i, cy;

  cy = sheet->voffset;
  if(GTK_SHEET_COL_TITLES_VISIBLE(sheet)) cy += sheet->column_title_area.height;
  if(y < cy) return 0;
  for (i = 0; i <= sheet->maxrow; i++)
    {
      if (y >= cy  && y <= (cy + sheet->row[i].height) && sheet->row[i].is_visible)
	return i;
      if(sheet->row[i].is_visible) cy += sheet->row[i].height;

    }

  /* no match */
  return sheet->maxrow;
}


/* gives the left pixel of the given column in context of
 * the sheet's hoffset */
static inline gint
COLUMN_LEFT_XPIXEL(GtkSheet *sheet, gint ncol)
{
   return (sheet->hoffset + sheet->column[ncol].left_xpixel);
}

/* returns the column index from a x pixel location in the 
 * context of the sheet's hoffset */
static inline gint
COLUMN_FROM_XPIXEL (GtkSheet * sheet,
		    gint x)
{
  gint i, cx;

  cx = sheet->hoffset;
  if(GTK_SHEET_ROW_TITLES_VISIBLE(sheet)) cx += sheet->row_title_area.width;
  if(x < cx) return 0;
  for (i = 0; i <= sheet->maxcol; i++)
    {
      if (x >= cx  && x <= (cx + sheet->column[i].width) && sheet->column[i].is_visible)
	return i;
      if(sheet->column[i].is_visible) cx += sheet->column[i].width;

    }

  /* no match */
  return sheet->maxcol;
}

/* returns the total height of the sheet */
static inline gint SHEET_HEIGHT(GtkSheet *sheet)
{
  gint i,cx;
 
  cx = 0;
  if(GTK_SHEET_COL_TITLES_VISIBLE(sheet)) cx += sheet->column_title_area.height;
  for (i=0;i<=sheet->maxrow; i++)
   if(sheet->row[i].is_visible) cx += sheet->row[i].height;
  
  return cx;
}


/* returns the total width of the sheet */
static inline gint SHEET_WIDTH(GtkSheet *sheet)
{
  gint i,cx;
 
  cx = 0;
  if(GTK_SHEET_ROW_TITLES_VISIBLE(sheet)) cx += sheet->row_title_area.width;
  for (i=0;i<=sheet->maxcol; i++)
   if(sheet->column[i].is_visible) cx += sheet->column[i].width;
  
  return cx;
}

#define MIN_VISIBLE_ROW(sheet) sheet->view.row0
#define MAX_VISIBLE_ROW(sheet) sheet->view.rowi
#define MIN_VISIBLE_COLUMN(sheet) sheet->view.col0
#define MAX_VISIBLE_COLUMN(sheet) sheet->view.coli


static inline gint
POSSIBLE_XDRAG(GtkSheet *sheet, gint x, gint *drag_column)
{
 gint column, xdrag;

 column=COLUMN_FROM_XPIXEL(sheet, x);
 *drag_column=column;

 xdrag=COLUMN_LEFT_XPIXEL(sheet,column)+CELL_SPACING;
 if(x <= xdrag+DRAG_WIDTH/2 && column != 0){
   while(!sheet->column[column-1].is_visible && column>0) column--;
   *drag_column=column-1;
   return sheet->column[column-1].is_sensitive;
 }

 xdrag+=sheet->column[column].width;
 if(x >= xdrag-DRAG_WIDTH/2 && x <= xdrag+DRAG_WIDTH/2)
   return sheet->column[column].is_sensitive;

 return FALSE;
} 

static inline gint
POSSIBLE_YDRAG(GtkSheet *sheet, gint y, gint *drag_row)
{
 gint row, ydrag;

 row=ROW_FROM_YPIXEL(sheet, y);
 *drag_row=row;

 ydrag=ROW_TOP_YPIXEL(sheet,row)+CELL_SPACING;
 if(y <= ydrag+DRAG_WIDTH/2 && row != 0){
   while(!sheet->row[row-1].is_visible && row>0) row--;
   *drag_row=row-1;
   return sheet->row[row-1].is_sensitive;
 }

 ydrag+=sheet->row[row].height;

 if(y >= ydrag-DRAG_WIDTH/2 && y <= ydrag+DRAG_WIDTH/2)
   return sheet->row[row].is_sensitive;
 
 
 return FALSE;
}        

static inline gint POSSIBLE_DRAG(GtkSheet *sheet, gint x, gint y,
                            gint *drag_row, gint *drag_column)
{
  gint ydrag, xdrag;

  *drag_column=COLUMN_FROM_XPIXEL(sheet,x);
  *drag_row=ROW_FROM_YPIXEL(sheet,y);

  if(x>=COLUMN_LEFT_XPIXEL(sheet,sheet->range.col0)-DRAG_WIDTH/2 &&
     x<=COLUMN_LEFT_XPIXEL(sheet,sheet->range.coli)+
        sheet->column[sheet->range.coli].width+DRAG_WIDTH/2){
     ydrag=ROW_TOP_YPIXEL(sheet,sheet->range.row0);
     if(y>=ydrag-DRAG_WIDTH/2 && y<=ydrag+DRAG_WIDTH/2){
        *drag_row=sheet->range.row0;
        return TRUE;
     }
     ydrag=ROW_TOP_YPIXEL(sheet,sheet->range.rowi)+
           sheet->row[sheet->range.rowi].height;
     if(y>=ydrag-DRAG_WIDTH/2 && y<=ydrag+DRAG_WIDTH/2){
        *drag_row=sheet->range.rowi;
        return TRUE;
     }
  }

  if(y>=ROW_TOP_YPIXEL(sheet,sheet->range.row0)-DRAG_WIDTH/2 &&
     y<=ROW_TOP_YPIXEL(sheet,sheet->range.rowi)+
        sheet->row[sheet->range.rowi].height+DRAG_WIDTH/2){
     xdrag=COLUMN_LEFT_XPIXEL(sheet,sheet->range.col0);
     if(x>=xdrag-DRAG_WIDTH/2 && x<=xdrag+DRAG_WIDTH/2){
        *drag_column=sheet->range.col0;
        return TRUE;
     }
     xdrag=COLUMN_LEFT_XPIXEL(sheet,sheet->range.coli)+
           sheet->column[sheet->range.coli].width;
     if(x>=xdrag-DRAG_WIDTH/2 && x<=xdrag+DRAG_WIDTH/2){
        *drag_column=sheet->range.coli;
        return TRUE;
     }
  }
  return FALSE;
}

static inline gint POSSIBLE_RESIZE(GtkSheet *sheet, gint x, gint y,
                            gint *drag_row, gint *drag_column)
{
  gint xdrag, ydrag;
  
  xdrag=COLUMN_LEFT_XPIXEL(sheet,sheet->range.coli)+
           sheet->column[sheet->range.coli].width;

  ydrag=ROW_TOP_YPIXEL(sheet,sheet->range.rowi)+
           sheet->row[sheet->range.rowi].height;

  if(sheet->state == GTK_SHEET_COLUMN_SELECTED) 
        ydrag = ROW_TOP_YPIXEL(sheet, sheet->view.row0);

  if(sheet->state == GTK_SHEET_ROW_SELECTED)
        xdrag = COLUMN_LEFT_XPIXEL(sheet, sheet->view.col0);

  *drag_column=COLUMN_FROM_XPIXEL(sheet,x);
  *drag_row=ROW_FROM_YPIXEL(sheet,y);

  if(x>=xdrag-DRAG_WIDTH/2 && x<=xdrag+DRAG_WIDTH/2 &&
     y>=ydrag-DRAG_WIDTH/2 && y<=ydrag+DRAG_WIDTH/2) return TRUE;

  return FALSE;  
}

typedef gboolean (*GtkSheetSignal1) (GtkObject *object,
                                    gint arg1, gint arg2, gint *arg3, gint *arg4,
                                    gpointer user_data);

typedef gboolean (*GtkSheetSignal2) (GtkObject *object,
                                    gint arg1, gint arg2,
                                    gpointer user_data);

static void gtk_sheet_class_init 		(GtkSheetClass * klass);
static void gtk_sheet_init 			(GtkSheet * sheet);
static void gtk_sheet_destroy 			(GtkObject * object);
static void gtk_sheet_finalize 			(GtkObject * object);
static void gtk_sheet_style_set 		(GtkWidget *widget,
		                 		 GtkStyle  *previous_style);
static void gtk_sheet_realize 			(GtkWidget * widget);
static void gtk_sheet_unrealize 		(GtkWidget * widget);
static void gtk_sheet_map 			(GtkWidget * widget);
static void gtk_sheet_unmap 			(GtkWidget * widget);
static void gtk_sheet_draw 			(GtkWidget * widget,
						 GdkRectangle * area);
static gint gtk_sheet_expose 			(GtkWidget * widget,
		  				 GdkEventExpose * event);
/*static void gtk_sheet_forall 			(GtkContainer *container,
                              			 gboolean include_internals,
                              			 GtkCallback  callback, 
                              			 gpointer  callback_data); 
*/
static void gtk_sheet_set_scroll_adjustments	(GtkSheet *sheet,
						 GtkAdjustment *hadjustment,
						 GtkAdjustment *vadjustment);

static gint gtk_sheet_button_press 		(GtkWidget * widget,
						 GdkEventButton * event);
static gint gtk_sheet_button_release 		(GtkWidget * widget,
						 GdkEventButton * event);
static gint gtk_sheet_motion 			(GtkWidget * widget,
		  				 GdkEventMotion * event);
static gint gtk_sheet_entry_key_press		(GtkWidget *widget,
		                		 GdkEventKey *key);
static gint gtk_sheet_key_press			(GtkWidget *widget,
		                		 GdkEventKey *key);
static void gtk_sheet_size_request 		(GtkWidget * widget,
			            	 	 GtkRequisition * requisition);
static void gtk_sheet_size_allocate 		(GtkWidget * widget,
			             		 GtkAllocation * allocation);

/* Sheet queries */

static gint gtk_sheet_range_isvisible 		(GtkSheet * sheet,
			 			 GtkSheetRange range);
static gint gtk_sheet_cell_isvisible 		(GtkSheet * sheet,
			  			 gint row, gint column);
/* Clipped Range */

static gint gtk_sheet_scroll			(gpointer data);
static gint gtk_sheet_flash			(gpointer data);

/* Drawing Routines */

/* draw cell background and frame */
static void gtk_sheet_cell_draw_default 	(GtkSheet *sheet, 
						 gint row, gint column);

/* draw cell border */
static void gtk_sheet_cell_draw_border 		(GtkSheet *sheet, 
						 gint row, gint column, 
						 gint mask);

/* draw cell contents */
static void gtk_sheet_cell_draw_label 		(GtkSheet *sheet, 
						 gint row, gint column);

/* draw visible part of range. If range==NULL then draw the whole screen */
static void gtk_sheet_range_draw		(GtkSheet *sheet, 
						 GtkSheetRange *range);

/* highlight the visible part of the selected range */
static void gtk_sheet_range_draw_selection	(GtkSheet *sheet, 
						 GtkSheetRange range);

/* Selection */

static gint gtk_sheet_move_query		(GtkSheet *sheet, 
						 gint row, gint column);
static void gtk_sheet_real_select_range 	(GtkSheet * sheet,
			                 	 GtkSheetRange * range);
static void gtk_sheet_extend_selection		(GtkSheet *sheet, 
						 gint row, gint column);
static void gtk_sheet_new_selection		(GtkSheet *sheet, 
						 GtkSheetRange *range);
static void gtk_sheet_draw_border 		(GtkSheet *sheet, 
						 GtkSheetRange range);
static void gtk_sheet_draw_corners		(GtkSheet *sheet,
						 GtkSheetRange range);


/* Active Cell handling */

static void gtk_sheet_entry_changed		(GtkWidget *widget, 
						 gpointer data);
static gint gtk_sheet_deactivate_cell		(GtkSheet *sheet);
static void gtk_sheet_hide_active_cell		(GtkSheet *sheet);
static gint gtk_sheet_activate_cell		(GtkSheet *sheet, 
						 gint row, gint col);
static void gtk_sheet_draw_active_cell		(GtkSheet *sheet);
static void gtk_sheet_show_active_cell		(GtkSheet *sheet);
static void gtk_sheet_click_cell		(GtkSheet *sheet, 
                                 		 gint row, 
                                		 gint column,
                                 		 gboolean *veto);

/* Backing Pixmap */

static void gtk_sheet_make_backing_pixmap 	(GtkSheet *sheet, 
						 gint width, gint height);
static void gtk_sheet_draw_backing_pixmap	(GtkSheet *sheet, 
						 GtkSheetRange range);
/* Scrollbars */

static void adjust_scrollbars 			(GtkSheet * sheet);
static void vadjustment_changed		 	(GtkAdjustment * adjustment,
			       			 gpointer data);
static void hadjustment_changed 		(GtkAdjustment * adjustment,
			       			 gpointer data);
static void vadjustment_value_changed 		(GtkAdjustment * adjustment,
				     		 gpointer data);
static void hadjustment_value_changed 		(GtkAdjustment * adjustment,
				     		 gpointer data);


static void draw_xor_vline 			(GtkSheet * sheet);
static void draw_xor_hline 			(GtkSheet * sheet);
static void draw_xor_rectangle			(GtkSheet *sheet, 
						 GtkSheetRange range);
static void gtk_sheet_draw_flashing_range	(GtkSheet *sheet, 
						 GtkSheetRange range);
static gint new_column_width 			(GtkSheet * sheet,
		  				 gint column,
		  				 gint * x);
static gint new_row_height 			(GtkSheet * sheet,
		  				 gint row,
		  				 gint * y);
/* Sheet Button */

static void create_global_button		(GtkSheet *sheet);
static void global_button_clicked		(GtkWidget *widget, 
						 gpointer data);
/* Sheet Entry */

static void create_sheet_entry			(GtkSheet *sheet);
static void gtk_sheet_size_allocate_entry	(GtkSheet *sheet);
static void gtk_sheet_entry_set_max_size	(GtkSheet *sheet);

/* Sheet button gadgets */

static void size_allocate_column_title_buttons 	(GtkSheet * sheet);
static void size_allocate_row_title_buttons 	(GtkSheet * sheet);
static void gtk_sheet_recalc_top_ypixels	(GtkSheet *sheet, 
						 gint row);
static void gtk_sheet_recalc_left_xpixels	(GtkSheet *sheet, 
						 gint column);
static void row_button_set 			(GtkSheet *sheet, 
						 gint row);
static void column_button_set 			(GtkSheet *sheet, 
						 gint column);
static void row_button_release 			(GtkSheet *sheet, 
						 gint row);
static void column_button_release 		(GtkSheet *sheet, 
						 gint column);
static void gtk_sheet_button_draw		(GtkSheet *sheet, 
						 gint row, gint column);
static void size_allocate_global_button 	(GtkSheet *sheet);
static void gtk_sheet_button_size_request	(GtkSheet *sheet,
                                 		 GtkSheetButton *button, 
                                 		 GtkRequisition *requisition);

/* Attributes routines */

static void gtk_sheet_set_cell_attributes	(GtkSheet *sheet, 
						 gint row, gint col,
						 GtkSheetCellAttr attributes);

static void init_attributes			(GtkSheet *sheet, gint col,  
						 GtkSheetCellAttr *attributes);
/* Memory allocation routines */
static void gtk_sheet_real_range_clear 		(GtkSheet *sheet, 
						 const GtkSheetRange *range, 
                            			 gboolean delete);
static void gtk_sheet_real_cell_clear 		(GtkSheet *sheet, 
						 gint row,
						 gint column,
						 gboolean delete);
static GtkSheetCell * gtk_sheet_cell_new 	(void);
static gint AddRow				(GtkSheet *sheet, gint nrows);
static gint AddColumn				(GtkSheet *sheet, gint ncols);
static gint InsertRow				(GtkSheet *sheet, gint row, gint nrows);
static gint InsertColumn			(GtkSheet *sheet, gint col, gint ncols);
static gint DeleteRow				(GtkSheet *sheet, gint row, gint nrows);
static gint DeleteColumn			(GtkSheet *sheet, gint col, gint ncols);
static gint GrowSheet				(GtkSheet *sheet, 
						 gint newrows, gint newcols);
static gint CheckBounds				(GtkSheet *sheet, 
						 gint row, gint col);

/* Container Functions */
static void gtk_sheet_remove			(GtkContainer *container,
						 GtkWidget *widget);
static void gtk_sheet_realize_child		(GtkSheet *sheet,
						 GtkSheetChild *child);
static void gtk_sheet_position_child		(GtkSheet *sheet,
						 GtkSheetChild *child);
static void gtk_sheet_position_children		(GtkSheet *sheet);
static void gtk_sheet_child_show		(GtkSheetChild *child); 
static void gtk_sheet_child_hide		(GtkSheetChild *child); 

/* Signals */

enum {
      SELECT_ROW, 
      SELECT_COLUMN, 
      SELECT_RANGE,
      CLIP_RANGE,
      RESIZE_RANGE,
      MOVE_RANGE,
      TRAVERSE, 
      DEACTIVATE, 
      ACTIVATE,
      SET_CELL,
      CLEAR_CELL,
      CHANGED,
      NEW_COL_WIDTH,
      NEW_ROW_HEIGHT,
      LAST_SIGNAL
};

static void
gtk_sheet_marshal_BOOL__INT_INT (GtkObject *object,
                          	GtkSignalFunc func,
                          	gpointer func_data,
                          	GtkArg * args);
static void
gtk_sheet_marshal_BOOL__INT_INT_POINTER_POINTER (GtkObject *object,
                          			 GtkSignalFunc func,
                          			 gpointer func_data,
                          			 GtkArg * args);

static GtkContainerClass *parent_class = NULL;
static guint sheet_signals[LAST_SIGNAL] = {0};


guint
gtk_sheet_get_type ()
{
  static guint sheet_type = 0;
  if(!sheet_type){
	GtkTypeInfo sheet_info =
        {
		"GtkSheet",
	        sizeof(GtkSheet),
	        sizeof(GtkSheetClass),
	        (GtkClassInitFunc) gtk_sheet_class_init,
	        (GtkObjectInitFunc) gtk_sheet_init,
		/* reserved_1 */ NULL,
	        /* reserved_2 */ NULL,
                (GtkClassInitFunc) NULL,
	};
	sheet_type = gtk_type_unique (GTK_TYPE_CONTAINER, &sheet_info);
  }
  return sheet_type;
}

static void
gtk_sheet_class_init (GtkSheetClass * klass)
{
  GtkObjectClass *object_class;
  GtkWidgetClass *widget_class;
  GtkContainerClass *container_class;

  object_class = (GtkObjectClass *) klass;
  widget_class = (GtkWidgetClass *) klass;
  container_class = (GtkContainerClass *) klass;

  parent_class = gtk_type_class (GTK_TYPE_CONTAINER);

  sheet_signals[SELECT_ROW] =
    gtk_signal_new ("select_row",
		    GTK_RUN_LAST,
		    object_class->type,
		    GTK_SIGNAL_OFFSET (GtkSheetClass, select_row),
		    gtk_marshal_NONE__INT,
                    GTK_TYPE_NONE, 1, GTK_TYPE_INT);
  sheet_signals[SELECT_COLUMN] =
    gtk_signal_new ("select_column",
		    GTK_RUN_LAST,
		    object_class->type,
		    GTK_SIGNAL_OFFSET (GtkSheetClass, select_column),
		    gtk_marshal_NONE__INT,
                    GTK_TYPE_NONE, 1, GTK_TYPE_INT);
  sheet_signals[SELECT_RANGE] =
    gtk_signal_new ("select_range",
		    GTK_RUN_LAST,
		    object_class->type,
		    GTK_SIGNAL_OFFSET (GtkSheetClass, select_range),
                    gtk_marshal_NONE__POINTER,
	            GTK_TYPE_NONE, 1, GTK_TYPE_POINTER);
  sheet_signals[CLIP_RANGE] =
    gtk_signal_new ("clip_range",
		    GTK_RUN_LAST,
		    object_class->type,
		    GTK_SIGNAL_OFFSET (GtkSheetClass, clip_range),
                    gtk_marshal_NONE__POINTER,
	            GTK_TYPE_NONE, 1, GTK_TYPE_POINTER);
  sheet_signals[RESIZE_RANGE] =
    gtk_signal_new ("resize_range",
		    GTK_RUN_LAST,
		    object_class->type,
		    GTK_SIGNAL_OFFSET (GtkSheetClass, resize_range),
		    gtk_marshal_NONE__POINTER_POINTER,
	            GTK_TYPE_NONE, 2, GTK_TYPE_POINTER, GTK_TYPE_POINTER);
  sheet_signals[MOVE_RANGE] =
    gtk_signal_new ("move_range",
		    GTK_RUN_LAST,
		    object_class->type,
		    GTK_SIGNAL_OFFSET (GtkSheetClass, move_range),
		    gtk_marshal_NONE__POINTER_POINTER,
                    GTK_TYPE_NONE, 2, GTK_TYPE_POINTER, GTK_TYPE_POINTER);
  sheet_signals[TRAVERSE] =
    gtk_signal_new ("traverse",
		    GTK_RUN_LAST,
		    object_class->type,
		    GTK_SIGNAL_OFFSET (GtkSheetClass, traverse),
                    gtk_sheet_marshal_BOOL__INT_INT_POINTER_POINTER,
	            GTK_TYPE_BOOL, 4, GTK_TYPE_INT, GTK_TYPE_INT,
                                      GTK_TYPE_POINTER, GTK_TYPE_POINTER);
  sheet_signals[DEACTIVATE] =
    gtk_signal_new ("deactivate",
		    GTK_RUN_LAST,
		    object_class->type,
		    GTK_SIGNAL_OFFSET (GtkSheetClass, deactivate),
                    gtk_sheet_marshal_BOOL__INT_INT,
	            GTK_TYPE_BOOL, 2, GTK_TYPE_INT, GTK_TYPE_INT);
  sheet_signals[ACTIVATE] =
    gtk_signal_new ("activate",
		    GTK_RUN_LAST,
		    object_class->type,
		    GTK_SIGNAL_OFFSET (GtkSheetClass, activate),
                    gtk_sheet_marshal_BOOL__INT_INT,
	            GTK_TYPE_BOOL, 2, GTK_TYPE_INT, GTK_TYPE_INT);
  sheet_signals[SET_CELL] =
    gtk_signal_new ("set_cell",
		    GTK_RUN_LAST,
		    object_class->type,
		    GTK_SIGNAL_OFFSET (GtkSheetClass, set_cell),
                    gtk_marshal_NONE__INT_INT,
	            GTK_TYPE_NONE, 2, GTK_TYPE_INT, GTK_TYPE_INT);
  sheet_signals[CLEAR_CELL] =
    gtk_signal_new ("clear_cell",
		    GTK_RUN_LAST,
		    object_class->type,
		    GTK_SIGNAL_OFFSET (GtkSheetClass, clear_cell),
                    gtk_marshal_NONE__INT_INT,
	            GTK_TYPE_NONE, 2, GTK_TYPE_INT, GTK_TYPE_INT);
  sheet_signals[CHANGED] =
    gtk_signal_new ("changed",
		    GTK_RUN_LAST,
		    object_class->type,
		    GTK_SIGNAL_OFFSET (GtkSheetClass, changed),
                    gtk_marshal_NONE__INT_INT,
	            GTK_TYPE_NONE, 2, GTK_TYPE_INT, GTK_TYPE_INT);
  sheet_signals[NEW_COL_WIDTH] =
    gtk_signal_new ("new_column_width",
		    GTK_RUN_LAST,
		    object_class->type,
		    GTK_SIGNAL_OFFSET (GtkSheetClass, changed),
                    gtk_marshal_NONE__INT_INT,
	            GTK_TYPE_NONE, 2, GTK_TYPE_INT, GTK_TYPE_INT);
  sheet_signals[NEW_ROW_HEIGHT] =
    gtk_signal_new ("new_row_height",
		    GTK_RUN_LAST,
		    object_class->type,
		    GTK_SIGNAL_OFFSET (GtkSheetClass, changed),
                    gtk_marshal_NONE__INT_INT,
	            GTK_TYPE_NONE, 2, GTK_TYPE_INT, GTK_TYPE_INT);


  gtk_object_class_add_signals (object_class, sheet_signals, LAST_SIGNAL);

  container_class->add = NULL;
  container_class->remove = gtk_sheet_remove;
  container_class->forall = NULL;

  object_class->destroy = gtk_sheet_destroy;
  object_class->finalize = gtk_sheet_finalize;

  widget_class->set_scroll_adjustments_signal =
    gtk_signal_new ("set_scroll_adjustments",
                    GTK_RUN_LAST,
                    object_class->type,
                    GTK_SIGNAL_OFFSET (GtkSheetClass, set_scroll_adjustments),
                    gtk_marshal_NONE__POINTER_POINTER,
                    GTK_TYPE_NONE, 2, GTK_TYPE_ADJUSTMENT, GTK_TYPE_ADJUSTMENT);

  widget_class->realize = gtk_sheet_realize;
  widget_class->unrealize = gtk_sheet_unrealize;
  widget_class->map = gtk_sheet_map;
  widget_class->unmap = gtk_sheet_unmap;
  widget_class->draw = gtk_sheet_draw;
  widget_class->style_set = gtk_sheet_style_set;
  widget_class->button_press_event = gtk_sheet_button_press;
  widget_class->button_release_event = gtk_sheet_button_release;
  widget_class->motion_notify_event = gtk_sheet_motion;
  widget_class->key_press_event = gtk_sheet_key_press;
  widget_class->expose_event = gtk_sheet_expose;
  widget_class->size_request = gtk_sheet_size_request;
  widget_class->size_allocate = gtk_sheet_size_allocate;
  widget_class->focus_in_event = NULL;
  widget_class->focus_out_event = NULL;

  klass->set_scroll_adjustments = gtk_sheet_set_scroll_adjustments;
  klass->select_row = NULL;
  klass->select_column = NULL;
  klass->select_range = NULL;
  klass->clip_range = NULL;
  klass->resize_range = NULL;
  klass->move_range = NULL;
  klass->traverse = NULL;
  klass->deactivate = NULL;
  klass->activate = NULL;
  klass->set_cell = NULL;
  klass->clear_cell = NULL;
  klass->changed = NULL;

}

static void
gtk_sheet_marshal_BOOL__INT_INT_POINTER_POINTER (GtkObject *object,
                          			 GtkSignalFunc func,
                          			 gpointer func_data,
                          			 GtkArg * args)
{
  GtkSheetSignal1 rfunc;
  gboolean *veto;
  veto = GTK_RETLOC_BOOL (args[4]);

  rfunc = (GtkSheetSignal1) func;

  *veto = (*rfunc) (object, GTK_VALUE_INT (args[0]),
                    GTK_VALUE_INT (args[1]),
                    GTK_VALUE_POINTER (args[2]),
                    GTK_VALUE_POINTER (args[3]),
                    func_data);
}
         
static void
gtk_sheet_marshal_BOOL__INT_INT (GtkObject *object,
                          	 GtkSignalFunc func,
                          	 gpointer func_data,
                          	 GtkArg * args)
{
  GtkSheetSignal2 rfunc;
  gboolean *veto;
  veto = GTK_RETLOC_BOOL (args[2]);

  rfunc = (GtkSheetSignal2) func;

  *veto = (*rfunc) (object, GTK_VALUE_INT (args[0]),
                    GTK_VALUE_INT (args[1]),
                    func_data);
}
         


static void 
gtk_sheet_init (GtkSheet *sheet)
{
  sheet->children = NULL;

  sheet->flags = 0;
  sheet->selection_mode = GTK_SELECTION_BROWSE;
  sheet->freeze_count = 0;
  sheet->state = GTK_SHEET_NORMAL;

  GTK_WIDGET_UNSET_FLAGS (sheet, GTK_NO_WINDOW);
  GTK_WIDGET_SET_FLAGS (sheet, GTK_CAN_FOCUS);

  sheet->maxrow = 0;
  sheet->maxcol = 0;

  sheet->view.row0 = 0;
  sheet->view.col0 = 0;
  sheet->view.rowi = 0;
  sheet->view.coli = 0;

  sheet->maxallocrow = 0;
  sheet->maxalloccol = 0;

  sheet->column_title_window=NULL;
  sheet->column_title_area.x=0;
  sheet->column_title_area.y=0;
  sheet->column_title_area.width=0;
  sheet->column_title_area.height=DEFAULT_ROW_HEIGHT(GTK_WIDGET(sheet));
 
  sheet->row_title_window=NULL;
  sheet->row_title_area.x=0;
  sheet->row_title_area.y=0;
  sheet->row_title_area.width=DEFAULT_COLUMN_WIDTH;
  sheet->row_title_area.height=0;

  sheet->active_cell.row=0;
  sheet->active_cell.col=0;
  sheet->selection_cell.row=0;
  sheet->selection_cell.col=0;

  sheet->sheet_entry=NULL;
  sheet->pixmap=NULL;

  sheet->range.row0=0;
  sheet->range.rowi=0;
  sheet->range.col0=0;
  sheet->range.coli=0;

  sheet->state=GTK_SHEET_NORMAL;

  sheet->sheet_window = NULL;
  sheet->sheet_window_width = 0;
  sheet->sheet_window_height = 0;
  sheet->sheet_entry = NULL;
  sheet->button = NULL;

  sheet->hoffset = 0;
  sheet->voffset = 0;

  sheet->hadjustment = NULL;
  sheet->vadjustment = NULL;

  sheet->cursor_drag = gdk_cursor_new(GDK_PLUS);
  sheet->xor_gc = NULL;
  sheet->fg_gc = NULL;
  sheet->bg_gc = NULL;
  sheet->x_drag = 0;
  sheet->y_drag = 0;

}

GtkWidget *
gtk_sheet_new (gint rows, gint columns, const gchar *title)
{
  GtkSheet *sheet;

  /* sanity check */
  g_return_val_if_fail (columns >= MINCOLS, NULL);
  g_return_val_if_fail (rows >= MINROWS, NULL);

  sheet = gtk_type_new (gtk_sheet_get_type ());

  sheet->row=(GtkSheetRow *)g_malloc(sizeof(GtkSheetRow));
  sheet->column=(GtkSheetColumn *)g_malloc(sizeof(GtkSheetColumn));
  sheet->data=(GtkSheetCell ***)g_malloc(sizeof(GtkSheetCell **));

  sheet->data[0] = (GtkSheetCell **)g_malloc(sizeof(GtkSheetCell *)+sizeof(gdouble));
  sheet->data[0][0] = NULL;

  GTK_SHEET_SET_FLAGS(sheet, GTK_SHEET_ROW_TITLES_VISIBLE);
  GTK_SHEET_SET_FLAGS(sheet, GTK_SHEET_COL_TITLES_VISIBLE);
  GTK_SHEET_SET_FLAGS(sheet, GTK_SHEET_AUTO_SCROLL);

  /* set number of rows and columns */
  GrowSheet(sheet, MINROWS, MINCOLS);

  /* Init row an column zero */
  AddRow(sheet,-1);
  AddColumn(sheet,-1);

  /* Add rows and columns */
  AddRow(sheet,rows-1);
  AddColumn(sheet,columns-1);

  /* create sheet entry */
  sheet->entry_type = 0;
  create_sheet_entry (sheet);

  /* create global selection button */
  create_global_button(sheet);

  if(title)
     sheet->name = g_strdup(title);

  return GTK_WIDGET (sheet);
}


GtkWidget *
gtk_sheet_new_browser(gint rows, gint columns, const gchar *title)
{
  GtkWidget *sheet;
  
  sheet=gtk_sheet_new(rows, columns, title);
  GTK_SHEET_SET_FLAGS(sheet, GTK_SHEET_IS_LOCKED);
  GTK_SHEET_SET_FLAGS(sheet, GTK_SHEET_AUTORESIZE);
 
  return sheet;
}

GtkWidget *
gtk_sheet_new_with_custom_entry (gint rows, gint columns, const gchar *title,
                                 GtkType entry_type)
{
  GtkWidget *sheet;
  
  sheet=gtk_sheet_new(rows, columns, title);
  GTK_SHEET(sheet)->entry_type = entry_type;

  create_sheet_entry(GTK_SHEET(sheet));
 
  return sheet;
}

void
gtk_sheet_change_entry(GtkSheet *sheet, GtkType entry_type)
{
  gint state;

  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  state = sheet->state;

  if(sheet->state == GTK_SHEET_NORMAL)
      gtk_sheet_hide_active_cell(sheet);

  sheet->entry_type = entry_type;

  create_sheet_entry(sheet);

  if(state == GTK_SHEET_NORMAL)
    {
      gtk_sheet_show_active_cell(sheet); 
      gtk_signal_connect(GTK_OBJECT(gtk_sheet_get_entry(sheet)),
                         "changed",
                         (GtkSignalFunc)gtk_sheet_entry_changed,
                         GTK_OBJECT(GTK_WIDGET(sheet)));
    }
 
}

gint
gtk_sheet_get_state(GtkSheet *sheet)
{
  g_return_val_if_fail (sheet != NULL, 0);
  g_return_val_if_fail (GTK_IS_SHEET (sheet), 0);

  return sheet->state;
}

void
gtk_sheet_set_selection_mode(GtkSheet *sheet, gint mode)
{
  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  if(GTK_WIDGET_REALIZED(sheet))
   gtk_sheet_unselect_range(sheet, NULL);

  sheet->selection_mode = mode;
}

/* This routine has problems with gtk+-1.2 related with the
 * label/button drawing - I think it's a bug in gtk+-1.2 */

void
gtk_sheet_set_title(GtkSheet *sheet, const gchar *title)
{
/*  GtkWidget *old_widget;
*/  GtkWidget *label;

  g_return_if_fail (sheet != NULL);
  g_return_if_fail (title != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  if (sheet->name)
    g_free (sheet->name);

  sheet->name = g_strdup (title);

  if(!GTK_WIDGET_REALIZED(GTK_WIDGET(sheet)) || !title) return;

  if(GTK_BIN(sheet->button)->child)
           label = GTK_BIN(sheet->button)->child;
/*
  gtk_label_set_text(GTK_LABEL(label), title);
*/
  size_allocate_global_button(sheet);

  /* remove and destroy the old widget */
/*
  old_widget = GTK_BIN (sheet->button)->child;
  if (old_widget)
    {
      gtk_container_remove (GTK_CONTAINER (sheet->button), old_widget);
    }

  label = gtk_label_new (title);
  gtk_misc_set_alignment(GTK_MISC(label), 0.5 , 0.5 );

  gtk_container_add (GTK_CONTAINER (sheet->button), label);
  gtk_widget_show (label);

  size_allocate_global_button(sheet);

  gtk_signal_emit(GTK_OBJECT(sheet),sheet_signals[CHANGED], -1, -1);

  if(old_widget)
      gtk_widget_destroy (old_widget);
*/
}

void
gtk_sheet_freeze (GtkSheet *sheet)
{
  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  sheet->freeze_count++;
  GTK_SHEET_SET_FLAGS(sheet, GTK_SHEET_IS_FROZEN);
}

void
gtk_sheet_thaw(GtkSheet *sheet)
{
  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  if(sheet->freeze_count == 0) return;

  sheet->freeze_count--;
  if(sheet->freeze_count > 0) return;

  adjust_scrollbars(sheet);

  GTK_SHEET_UNSET_FLAGS(sheet, GTK_SHEET_IS_FROZEN);

  sheet->old_vadjustment = -1.;
  sheet->old_hadjustment = -1.;

  if(sheet->hadjustment)
      gtk_signal_emit_by_name (GTK_OBJECT (sheet->hadjustment), 
   			      "value_changed");
  if(sheet->vadjustment)
      gtk_signal_emit_by_name (GTK_OBJECT (sheet->vadjustment), 
   			      "value_changed");

}

void
gtk_sheet_set_row_titles_width(GtkSheet *sheet, gint width)
{
 if(width < COLUMN_MIN_WIDTH) return;

 sheet->row_title_area.width = width;
 sheet->view.col0=COLUMN_FROM_XPIXEL(sheet, sheet->row_title_area.width+1);
 sheet->view.coli=COLUMN_FROM_XPIXEL(sheet, sheet->sheet_window_width);
 gtk_sheet_recalc_top_ypixels(sheet, 0);
 gtk_sheet_recalc_left_xpixels(sheet, 0);
 adjust_scrollbars(sheet);

 sheet->old_hadjustment = -1.;
 if(sheet->hadjustment)
     gtk_signal_emit_by_name (GTK_OBJECT (sheet->hadjustment), 
  			      "value_changed");
 size_allocate_global_button(sheet);
}

void
gtk_sheet_set_column_titles_height(GtkSheet *sheet, gint height)
{
 if(height < DEFAULT_ROW_HEIGHT(GTK_WIDGET(sheet))) return;

 sheet->column_title_area.height = height;
 sheet->view.row0=ROW_FROM_YPIXEL(sheet, sheet->column_title_area.height+1);
 sheet->view.rowi=ROW_FROM_YPIXEL(sheet, sheet->sheet_window_height-1);
 gtk_sheet_recalc_top_ypixels(sheet, 0);
 gtk_sheet_recalc_left_xpixels(sheet, 0);
 adjust_scrollbars(sheet);

 sheet->old_vadjustment = -1.;
 if(sheet->vadjustment)
     gtk_signal_emit_by_name (GTK_OBJECT (sheet->vadjustment), 
 			      "value_changed");
 size_allocate_global_button(sheet);
}

void
gtk_sheet_show_column_titles(GtkSheet *sheet)
{
 gint col;

 if(GTK_SHEET_COL_TITLES_VISIBLE(sheet)) return;

 GTK_SHEET_SET_FLAGS(sheet, GTK_SHEET_COL_TITLES_VISIBLE);
 gtk_sheet_recalc_top_ypixels(sheet, 0);
 gtk_sheet_recalc_left_xpixels(sheet, 0);
 if(GTK_WIDGET_REALIZED(GTK_WIDGET(sheet))){
  gdk_window_show(sheet->column_title_window);

  for(col = MIN_VISIBLE_COLUMN(sheet); col <= MAX_VISIBLE_COLUMN(sheet); col++){
    GtkSheetChild *child;
    child = sheet->column[col].button.child;
    if(child){
        gtk_sheet_child_show(child);
    }
  }
  adjust_scrollbars(sheet);
 } 

 sheet->old_vadjustment = -1.;
 if(sheet->vadjustment)
     gtk_signal_emit_by_name (GTK_OBJECT (sheet->vadjustment), 
			      "value_changed");
 size_allocate_global_button(sheet);
}

void
gtk_sheet_show_row_titles(GtkSheet *sheet)
{
 gint row;

 if(GTK_SHEET_ROW_TITLES_VISIBLE(sheet)) return;

 GTK_SHEET_SET_FLAGS(sheet, GTK_SHEET_ROW_TITLES_VISIBLE);
 gtk_sheet_recalc_top_ypixels(sheet, 0);
 gtk_sheet_recalc_left_xpixels(sheet, 0);
 if(GTK_WIDGET_REALIZED(GTK_WIDGET(sheet))){
  gdk_window_show(sheet->row_title_window);

  for(row = MIN_VISIBLE_ROW(sheet); row <= MAX_VISIBLE_ROW(sheet); row++){
    GtkSheetChild *child;
    child = sheet->row[row].button.child;
    if(child){
        gtk_sheet_child_show(child);
    }
  }
  adjust_scrollbars(sheet);
 }

 sheet->old_hadjustment = -1.;
 if(sheet->hadjustment)
     gtk_signal_emit_by_name (GTK_OBJECT (sheet->hadjustment), 
			      "value_changed");
 size_allocate_global_button(sheet);
}

void
gtk_sheet_hide_column_titles(GtkSheet *sheet)
{
 gint col;

 if(!GTK_SHEET_COL_TITLES_VISIBLE(sheet)) return;

 GTK_SHEET_UNSET_FLAGS(sheet, GTK_SHEET_COL_TITLES_VISIBLE);
 gtk_sheet_recalc_top_ypixels(sheet, 0);
 gtk_sheet_recalc_left_xpixels(sheet, 0);
 if(GTK_WIDGET_REALIZED(GTK_WIDGET(sheet))){
  if(sheet->column_title_window) gdk_window_hide(sheet->column_title_window);
  if(GTK_WIDGET_VISIBLE(sheet->button)) gtk_widget_hide(sheet->button);

  for(col = MIN_VISIBLE_COLUMN(sheet); col <= MAX_VISIBLE_COLUMN(sheet); col++){
    GtkSheetChild *child;
    child = sheet->column[col].button.child;
    if(child){
        gtk_sheet_child_hide(child);
    }
  }
  adjust_scrollbars(sheet);
 }
 
 sheet->old_vadjustment = -1.;
 if(sheet->vadjustment)
     gtk_signal_emit_by_name (GTK_OBJECT (sheet->vadjustment), 
			      "value_changed");
}

void
gtk_sheet_hide_row_titles(GtkSheet *sheet)
{
 gint row;

 if(!GTK_SHEET_ROW_TITLES_VISIBLE(sheet)) return;

 GTK_SHEET_UNSET_FLAGS(sheet, GTK_SHEET_ROW_TITLES_VISIBLE);
 gtk_sheet_recalc_top_ypixels(sheet, 0);
 gtk_sheet_recalc_left_xpixels(sheet, 0);
 if(GTK_WIDGET_REALIZED(GTK_WIDGET(sheet))){
  if(sheet->row_title_window) gdk_window_hide(sheet->row_title_window);
  if(GTK_WIDGET_VISIBLE(sheet->button)) gtk_widget_hide(sheet->button);
  for(row = MIN_VISIBLE_ROW(sheet); row <= MAX_VISIBLE_ROW(sheet); row++){
    GtkSheetChild *child;
    child = sheet->row[row].button.child;
    if(child){
        gtk_sheet_child_hide(child);
    }
  }
  adjust_scrollbars(sheet);
 }

 sheet->old_hadjustment = -1.;
 if(sheet->hadjustment)
     gtk_signal_emit_by_name (GTK_OBJECT (sheet->hadjustment), 
			      "value_changed");
}

void
gtk_sheet_set_column_title (GtkSheet * sheet,
			    gint column,
			    const gchar * title)
{
  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  if (sheet->column[column].name)
    g_free (sheet->column[column].name);

  sheet->column[column].name = g_strdup(title);
}

void
gtk_sheet_set_row_title (GtkSheet * sheet,
			 gint row,
			 const gchar * title)
{
  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  if (sheet->row[row].name)
    g_free (sheet->row[row].name);

  sheet->row[row].name = g_strdup (title);
}

void
gtk_sheet_row_button_add_label(GtkSheet *sheet, gint row, const gchar *label)
{
  GtkSheetButton *button;
  gint label_height;
  gchar *words;

  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  if(row < 0 || row > sheet->maxrow) return;


  if (sheet->row[row].button.label)
    g_free (sheet->row[row].button.label);
 
  sheet->row[row].button.label = g_strdup (label);

  button = &sheet->row[row].button;
  label_height = 0;
  if(button->label && strlen(button->label)>0){
           words=button->label;
           while(words && *words != '\0'){
             if(*words == '\n' || *(words+1) == '\0'){
               label_height += GTK_WIDGET(sheet)->style->font->ascent+
                    2*GTK_WIDGET(sheet)->style->font->descent;
             }
             words++;
           }
  }

  if(label_height+2*CELLOFFSET > sheet->column_title_area.height)
     gtk_sheet_set_row_height(sheet, row, label_height+2*CELLOFFSET);

  if(!GTK_SHEET_IS_FROZEN(sheet)){  
    gtk_sheet_button_draw(sheet, row, -1);
    gtk_signal_emit(GTK_OBJECT(sheet),sheet_signals[CHANGED], row, -1);
  }
}  

void
gtk_sheet_column_button_add_label(GtkSheet *sheet, gint column, const gchar *label)
{
  GtkSheetButton *button;
  gint label_height = 0;
  gchar *words;

  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  if(column < 0 || column >sheet->maxcol) return;

  if (sheet->column[column].button.label)
    g_free (sheet->column[column].button.label);
  

  sheet->column[column].button.label = g_strdup (label);

  button = &sheet->column[column].button;
  if(button->label && strlen(button->label)>0){
           words=button->label;
           while(words && *words != '\0'){
             if(*words == '\n' || *(words+1) == '\0'){
               label_height += GTK_WIDGET(sheet)->style->font->ascent+
                    2*GTK_WIDGET(sheet)->style->font->descent;
             }
             words++;
           }
  }

  if(label_height+2*CELLOFFSET > sheet->column_title_area.height)
     gtk_sheet_set_column_titles_height(sheet, label_height+2*CELLOFFSET);

  if(!GTK_SHEET_IS_FROZEN(sheet)){
    gtk_sheet_button_draw(sheet, -1, column);
    gtk_signal_emit(GTK_OBJECT(sheet),sheet_signals[CHANGED], -1, column);
  }
}  

void
gtk_sheet_row_button_justify(GtkSheet *sheet, gint row, gint justification)
{
  GtkSheetButton *button;

  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  if(row < 0 || row > sheet->maxrow) return;

  button = &sheet->row[row].button;
  button->justification = justification;

  if(!GTK_SHEET_IS_FROZEN(sheet)){  
    gtk_sheet_button_draw(sheet, row, -1);
    gtk_signal_emit(GTK_OBJECT(sheet),sheet_signals[CHANGED], row, -1);
  }
}  

void
gtk_sheet_column_button_justify(GtkSheet *sheet, gint column, gint justification)
{
  GtkSheetButton *button;

  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  if(column < 0 || column > sheet->maxcol) return;

  button = &sheet->column[column].button;
  button->justification = justification;

  if(!GTK_SHEET_IS_FROZEN(sheet)){  
    gtk_sheet_button_draw(sheet, -1, column);
    gtk_signal_emit(GTK_OBJECT(sheet),sheet_signals[CHANGED], -1, column);
  }
}  


void
gtk_sheet_moveto (GtkSheet * sheet,
		  gint row,
		  gint column,
	          gfloat row_align,
                  gfloat col_align)
{
  gint x, y;
  gint width, height;
  gint adjust;
  gint min_row, min_col;

  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));
  g_return_if_fail (sheet->hadjustment != NULL);
  g_return_if_fail (sheet->vadjustment != NULL);

  if (row < 0 || row > sheet->maxrow)
    return;
  if (column < 0 || column > sheet->maxcol)
    return;

  height = sheet->sheet_window_height;
  width = sheet->sheet_window_width;

  /* adjust vertical scrollbar */

  if (row >= 0 && row_align >=0.)
    {
      y = ROW_TOP_YPIXEL(sheet, row) - sheet->voffset -
          row_align*height-
          (1.-row_align)*sheet->row[row].height;

      /* This forces the sheet to scroll when you don't see the entire cell */
      min_row = row;
      adjust = 0;
      if(row_align == 1.){
        while(min_row >= 0 && min_row > MIN_VISIBLE_ROW(sheet)){
         if(sheet->row[min_row].is_visible) 
                adjust += sheet->row[min_row].height;
         if(adjust >= height){
           break;
         }
         min_row--;
        }
        min_row = MAX(min_row, 0);
        y = ROW_TOP_YPIXEL(sheet, min_row) - sheet->voffset +
            sheet->row[min_row].height - 1;
      }

      if (y < 0)
	sheet->vadjustment->value = 0.0;
      else
	sheet->vadjustment->value = y;

      sheet->old_vadjustment = -1.;
      gtk_signal_emit_by_name (GTK_OBJECT (sheet->vadjustment), 
			       "value_changed");

    } 
     
  /* adjust horizontal scrollbar */
  if (column >= 0 && col_align >= 0.)
    {
      x = COLUMN_LEFT_XPIXEL (sheet, column) - sheet->hoffset -
          col_align*width -
          (1.-col_align)*sheet->column[column].width;

      /* This forces the sheet to scroll when you don't see the entire cell */
      min_col = column;
      adjust = 0;
      if(col_align == 1.){
        while(min_col >= 0 && min_col > MIN_VISIBLE_COLUMN(sheet)){
         if(sheet->column[min_col].is_visible) 
                adjust += sheet->column[min_col].width;
         if(adjust >= width){
           break;
         }
         min_col--;
        }
        min_col = MAX(min_col, 0);
        x = COLUMN_LEFT_XPIXEL(sheet, min_col) - sheet->hoffset +
            sheet->column[min_col].width - 1;
      }

      if (x < 0)
	sheet->hadjustment->value = 0.0;
      else
	sheet->hadjustment->value = x;

      sheet->old_vadjustment = -1.;
      gtk_signal_emit_by_name (GTK_OBJECT (sheet->hadjustment), 
			       "value_changed");

    }
}

void 
gtk_sheet_column_set_sensitivity(GtkSheet *sheet, gint column, gboolean sensitive)
{
  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  if(column < 0 || column > sheet->maxcol) return;

  sheet->column[column].is_sensitive=sensitive;
  if(!sensitive)
     sheet->column[column].button.state=GTK_STATE_INSENSITIVE;
  else
     sheet->column[column].button.state=GTK_STATE_NORMAL;

  if(GTK_WIDGET_REALIZED(sheet) && !GTK_SHEET_IS_FROZEN(sheet))
      gtk_sheet_button_draw(sheet, -1, column);
}

void
gtk_sheet_columns_set_sensitivity(GtkSheet *sheet, gboolean sensitive)
{
  gint i;

  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  for(i=0; i<=sheet->maxcol; i++)
     gtk_sheet_column_set_sensitivity(sheet, i, sensitive);
}

void 
gtk_sheet_row_set_sensitivity(GtkSheet *sheet, gint row,  gboolean sensitive)
{

  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  if(row < 0 || row > sheet->maxrow) return;

  sheet->row[row].is_sensitive=sensitive;
  if(!sensitive)
     sheet->row[row].button.state=GTK_STATE_INSENSITIVE;
  else
     sheet->row[row].button.state=GTK_STATE_NORMAL;

  if(GTK_WIDGET_REALIZED(sheet) && !GTK_SHEET_IS_FROZEN(sheet))
      gtk_sheet_button_draw(sheet, row, -1);
}

void
gtk_sheet_rows_set_sensitivity(GtkSheet *sheet, gboolean sensitive)
{
  gint i;

  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  for(i=0; i<=sheet->maxrow; i++)
     gtk_sheet_row_set_sensitivity(sheet, i, sensitive);
}

void
gtk_sheet_column_set_visibility(GtkSheet *sheet, gint column, gboolean visible)
{
  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  if(column < 0 || column > sheet->maxcol) return;
  if(sheet->column[column].is_visible == visible) return;

  sheet->column[column].is_visible = visible;

  gtk_sheet_recalc_left_xpixels(sheet, column);

  if(!GTK_SHEET_IS_FROZEN(sheet) && 
    gtk_sheet_cell_isvisible(sheet, MIN_VISIBLE_ROW(sheet), column)){
      gtk_sheet_range_draw(sheet, NULL);
      size_allocate_column_title_buttons(sheet);
  }
}

void
gtk_sheet_row_set_visibility(GtkSheet *sheet, gint row, gboolean visible)
{
  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  if(row < 0 || row > sheet->maxrow) return;
  if(sheet->row[row].is_visible == visible) return;

  sheet->row[row].is_visible = visible;

  gtk_sheet_recalc_top_ypixels(sheet, row);

  if(!GTK_SHEET_IS_FROZEN(sheet) && 
    gtk_sheet_cell_isvisible(sheet, row, MIN_VISIBLE_COLUMN(sheet))){
      gtk_sheet_range_draw(sheet, NULL);
      size_allocate_row_title_buttons(sheet);
  }
}

void
gtk_sheet_select_row (GtkSheet * sheet,
		      gint row)
{
  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  if (row < 0 || row > sheet->maxrow)
    return;

  if(sheet->state != GTK_SHEET_NORMAL) 
     gtk_sheet_unselect_range(sheet, NULL);
  else
     gtk_sheet_deactivate_cell(sheet);

  sheet->state=GTK_SHEET_ROW_SELECTED;                     
  sheet->range.row0=row;
  sheet->range.col0=0;
  sheet->range.rowi=row;
  sheet->range.coli=sheet->maxcol;
  sheet->active_cell.row=row;
  sheet->active_cell.col=0;

  gtk_signal_emit (GTK_OBJECT (sheet), sheet_signals[SELECT_ROW], row);
  gtk_sheet_real_select_range(sheet, NULL);

}


void
gtk_sheet_select_column (GtkSheet * sheet,
		         gint column)
{
  
  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  if (column < 0 || column > sheet->maxcol)
    return;

  if(sheet->state != GTK_SHEET_NORMAL) 
     gtk_sheet_unselect_range(sheet, NULL);
  else
     gtk_sheet_deactivate_cell(sheet);

  sheet->state=GTK_SHEET_COLUMN_SELECTED;                     
  sheet->range.row0=0;
  sheet->range.col0=column;
  sheet->range.rowi=sheet->maxrow;
  sheet->range.coli=column;
  sheet->active_cell.row=0;
  sheet->active_cell.col=column;

  gtk_signal_emit (GTK_OBJECT (sheet), sheet_signals[SELECT_COLUMN], column);
  gtk_sheet_real_select_range(sheet, NULL);

}

void
gtk_sheet_clip_range (GtkSheet *sheet, GtkSheetRange range)
{

  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  if(GTK_SHEET_IN_CLIP(sheet)) return;

  GTK_SHEET_SET_FLAGS(sheet, GTK_SHEET_IN_CLIP);

  sheet->clip_range=range;
  sheet->interval=0;
  sheet->clip_timer=gtk_timeout_add(TIMEOUT_FLASH, gtk_sheet_flash, sheet); 

  gtk_signal_emit(GTK_OBJECT(sheet), sheet_signals[CLIP_RANGE],
                                     &sheet->clip_range);

}

void
gtk_sheet_unclip_range(GtkSheet *sheet)
{

  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  if(!GTK_SHEET_IN_CLIP(sheet)) return;

  GTK_SHEET_UNSET_FLAGS(sheet, GTK_SHEET_IN_CLIP);
  gtk_timeout_remove(sheet->clip_timer);
  gtk_sheet_range_draw(sheet, &sheet->clip_range);

  if(gtk_sheet_range_isvisible(sheet, sheet->range))
    gtk_sheet_range_draw(sheet, &sheet->range);
}

static gint
gtk_sheet_flash(gpointer data)
{
  GtkSheet *sheet;
  gint x,y,width,height;
  GdkRectangle clip_area;

  sheet=GTK_SHEET(data);

  if(!GTK_WIDGET_REALIZED(GTK_WIDGET(sheet))) return TRUE;
  if(!GTK_WIDGET_DRAWABLE(GTK_WIDGET(sheet))) return TRUE;
  if(!gtk_sheet_range_isvisible(sheet, sheet->clip_range)) return TRUE;
  if(GTK_SHEET_IN_XDRAG(sheet)) return TRUE; 
  if(GTK_SHEET_IN_YDRAG(sheet)) return TRUE; 

  GDK_THREADS_ENTER();
 
  x=COLUMN_LEFT_XPIXEL(sheet,sheet->clip_range.col0)+1;
  y=ROW_TOP_YPIXEL(sheet,sheet->clip_range.row0)+1;
  width=COLUMN_LEFT_XPIXEL(sheet,sheet->clip_range.coli)-x+ 
             sheet->column[sheet->clip_range.coli].width-1;
  height=ROW_TOP_YPIXEL(sheet,sheet->clip_range.rowi)-y+
             sheet->row[sheet->clip_range.rowi].height-1;

  clip_area.x=COLUMN_LEFT_XPIXEL(sheet, MIN_VISIBLE_COLUMN(sheet));
  clip_area.y=ROW_TOP_YPIXEL(sheet, MIN_VISIBLE_ROW(sheet));
  clip_area.width=sheet->sheet_window_width;
  clip_area.height=sheet->sheet_window_height;

  if(x<0) {
     width=width+x+1;
     x=-1;
  }
  if(width>clip_area.width) width=clip_area.width+10;
  if(y<0) {
     height=height+y+1;
     y=-1;
  }
  if(height>clip_area.height) height=clip_area.height+10;

  gdk_draw_pixmap(sheet->sheet_window,
                  GTK_WIDGET(sheet)->style->fg_gc[GTK_STATE_NORMAL],
                  sheet->pixmap,
                  x, y,
                  x, y,
                  1, height);

  gdk_draw_pixmap(sheet->sheet_window,
                  GTK_WIDGET(sheet)->style->fg_gc[GTK_STATE_NORMAL],
                  sheet->pixmap,
                  x, y,
                  x, y,
                  width, 1);

  gdk_draw_pixmap(sheet->sheet_window,
                  GTK_WIDGET(sheet)->style->fg_gc[GTK_STATE_NORMAL],
                  sheet->pixmap,
                  x, y+height,
                  x, y+height,
                  width, 1);

  gdk_draw_pixmap(sheet->sheet_window,
                  GTK_WIDGET(sheet)->style->fg_gc[GTK_STATE_NORMAL],
                  sheet->pixmap,
                  x+width, y,
                  x+width, y,
                  1, height);


  sheet->interval=sheet->interval+1;
  if(sheet->interval==TIME_INTERVAL) sheet->interval=0;

  gdk_gc_set_dashes(sheet->xor_gc, sheet->interval, "\4\4", 2);
  gtk_sheet_draw_flashing_range(sheet, sheet->clip_range);
  gdk_gc_set_dashes(sheet->xor_gc, 0, "\4\4", 2);

  GDK_THREADS_LEAVE();

  return TRUE;

}

static void
gtk_sheet_draw_flashing_range(GtkSheet *sheet, GtkSheetRange range)
{
  GdkRectangle clip_area;
  gint x,y,width,height;

  if(!gtk_sheet_range_isvisible(sheet, sheet->clip_range)) return;
  
  clip_area.x=COLUMN_LEFT_XPIXEL(sheet, MIN_VISIBLE_COLUMN(sheet));
  clip_area.y=ROW_TOP_YPIXEL(sheet, MIN_VISIBLE_ROW(sheet));
  clip_area.width=sheet->sheet_window_width;
  clip_area.height=sheet->sheet_window_height;

  gdk_gc_set_clip_rectangle(sheet->xor_gc, &clip_area);  

  x=COLUMN_LEFT_XPIXEL(sheet,sheet->clip_range.col0)+1;
  y=ROW_TOP_YPIXEL(sheet,sheet->clip_range.row0)+1;
  width=COLUMN_LEFT_XPIXEL(sheet,sheet->clip_range.coli)-x+ 
             sheet->column[sheet->clip_range.coli].width-1;
  height=ROW_TOP_YPIXEL(sheet,sheet->clip_range.rowi)-y+
             sheet->row[sheet->clip_range.rowi].height-1;

  if(x<0) {
     width=width+x+1;
     x=-1;
  }
  if(width>clip_area.width) width=clip_area.width+10;
  if(y<0) {
     height=height+y+1;
     y=-1;
  }
  if(height>clip_area.height) height=clip_area.height+10;

  gdk_gc_set_line_attributes(sheet->xor_gc, 1, 1, 0 ,0 );

  gdk_draw_rectangle(sheet->sheet_window, sheet->xor_gc, FALSE, 
                     x, y,
                     width, height);

  gdk_gc_set_line_attributes (sheet->xor_gc, 1, 0, 0, 0);

  gdk_gc_set_clip_rectangle(sheet->xor_gc, NULL);

}

static gint
gtk_sheet_range_isvisible (GtkSheet * sheet,
			 GtkSheetRange range)
{
  g_return_val_if_fail (sheet != NULL, FALSE);

  if (range.row0 < 0 || range.row0 > sheet->maxrow)
    return FALSE;

  if (range.rowi < 0 || range.rowi > sheet->maxrow)
    return FALSE;

  if (range.col0 < 0 || range.col0 > sheet->maxcol)
    return FALSE;

  if (range.coli < 0 || range.coli > sheet->maxcol)
    return FALSE;

  if (range.rowi < MIN_VISIBLE_ROW (sheet))
    return FALSE;

  if (range.row0 > MAX_VISIBLE_ROW (sheet))
    return FALSE;

  if (range.coli < MIN_VISIBLE_COLUMN (sheet))
    return FALSE;

  if (range.col0 > MAX_VISIBLE_COLUMN (sheet))
    return FALSE;

  return TRUE;
}

static gint
gtk_sheet_cell_isvisible (GtkSheet * sheet,
			  gint row, gint column)
{
  GtkSheetRange range;

  range.row0 = row;
  range.col0 = column;
  range.rowi = row;
  range.coli = column;

  return gtk_sheet_range_isvisible(sheet, range);
}

void 
gtk_sheet_get_visible_range(GtkSheet *sheet, GtkSheetRange *range)
{

  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet)) ;
  g_return_if_fail (range != NULL);

  range->row0 = MIN_VISIBLE_ROW(sheet);
  range->col0 = MIN_VISIBLE_COLUMN(sheet);
  range->rowi = MAX_VISIBLE_ROW(sheet);
  range->coli = MAX_VISIBLE_COLUMN(sheet);

}

GtkAdjustment *
gtk_sheet_get_vadjustment (GtkSheet * sheet)
{
  g_return_val_if_fail (sheet != NULL, NULL);
  g_return_val_if_fail (GTK_IS_SHEET (sheet), NULL);

  return sheet->vadjustment;
}

GtkAdjustment *
gtk_sheet_get_hadjustment (GtkSheet * sheet)
{
  g_return_val_if_fail (sheet != NULL, NULL);
  g_return_val_if_fail (GTK_IS_SHEET (sheet), NULL);

  return sheet->hadjustment;
}

void
gtk_sheet_set_vadjustment (GtkSheet      *sheet,
			   GtkAdjustment *adjustment)
{
  GtkAdjustment *old_adjustment;

  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));
  if (adjustment)
    g_return_if_fail (GTK_IS_ADJUSTMENT (adjustment));
  
  if (sheet->vadjustment == adjustment)
    return;
  
  old_adjustment = sheet->vadjustment;

  if (sheet->vadjustment)
    {
      gtk_signal_disconnect_by_data (GTK_OBJECT (sheet->vadjustment), sheet);
      gtk_object_unref (GTK_OBJECT (sheet->vadjustment));
    }

  sheet->vadjustment = adjustment;

  if (sheet->vadjustment)
    {
      gtk_object_ref (GTK_OBJECT (sheet->vadjustment));
      gtk_object_sink (GTK_OBJECT (sheet->vadjustment));

      gtk_signal_connect (GTK_OBJECT (sheet->vadjustment), "changed",
			  (GtkSignalFunc) vadjustment_changed,
			  (gpointer) sheet);
      gtk_signal_connect (GTK_OBJECT (sheet->vadjustment), "value_changed",
			  (GtkSignalFunc) vadjustment_value_changed,
			  (gpointer) sheet);
    }

  if (!sheet->vadjustment || !old_adjustment)
     {
       gtk_widget_queue_resize (GTK_WIDGET (sheet));
       return;
     }

  sheet->old_vadjustment = sheet->vadjustment->value;
}

void
gtk_sheet_set_hadjustment (GtkSheet      *sheet,
			   GtkAdjustment *adjustment)
{
  GtkAdjustment *old_adjustment;

  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));
  if (adjustment)
    g_return_if_fail (GTK_IS_ADJUSTMENT (adjustment));
  
  if (sheet->hadjustment == adjustment)
    return;
  
  old_adjustment = sheet->hadjustment;

  if (sheet->hadjustment)
    {
      gtk_signal_disconnect_by_data (GTK_OBJECT (sheet->hadjustment), sheet);
      gtk_object_unref (GTK_OBJECT (sheet->hadjustment));
    }

  sheet->hadjustment = adjustment;

  if (sheet->hadjustment)
    {
      gtk_object_ref (GTK_OBJECT (sheet->hadjustment));
      gtk_object_sink (GTK_OBJECT (sheet->hadjustment));

      gtk_signal_connect (GTK_OBJECT (sheet->hadjustment), "changed",
			  (GtkSignalFunc) hadjustment_changed,
			  (gpointer) sheet);
      gtk_signal_connect (GTK_OBJECT (sheet->hadjustment), "value_changed",
			  (GtkSignalFunc) hadjustment_value_changed,
			  (gpointer) sheet);
    }

  if (!sheet->hadjustment || !old_adjustment)
     {
       gtk_widget_queue_resize (GTK_WIDGET (sheet));
       return;
     }

  sheet->old_hadjustment = sheet->hadjustment->value;
}

static void
gtk_sheet_set_scroll_adjustments (GtkSheet *sheet,
				  GtkAdjustment *hadjustment,
				  GtkAdjustment *vadjustment)
{
   if(sheet->hadjustment != hadjustment)
         gtk_sheet_set_hadjustment (sheet, hadjustment);
   if(sheet->vadjustment != vadjustment)
         gtk_sheet_set_vadjustment (sheet, vadjustment);
}

static void
gtk_sheet_destroy (GtkObject * object)
{
  GtkSheet *sheet;

  g_return_if_fail (object != NULL);
  g_return_if_fail (GTK_IS_SHEET (object));

  sheet = GTK_SHEET (object);

  /* get rid of all the cells */
  gtk_sheet_range_clear (sheet, NULL);

  /* destroy the entry */
  gtk_widget_destroy (sheet->sheet_entry);

  /* destroy the global selection button */
  gtk_widget_destroy (sheet->button);

  if(sheet->timer){
     gtk_timeout_remove(sheet->timer);
     sheet->timer = 0;
  }

  if(sheet->clip_timer){
     gtk_timeout_remove(sheet->clip_timer);
     sheet->clip_timer = 0;
  }

  /* unref adjustments */
  if (sheet->hadjustment)
    {
      gtk_signal_disconnect_by_data (GTK_OBJECT (sheet->hadjustment), sheet);
      gtk_object_unref (GTK_OBJECT (sheet->hadjustment));
      sheet->hadjustment = NULL;
    }
  if (sheet->vadjustment)
    {
      gtk_signal_disconnect_by_data (GTK_OBJECT (sheet->vadjustment), sheet);
      gtk_object_unref (GTK_OBJECT (sheet->vadjustment));
      sheet->vadjustment = NULL;
    }


  if (GTK_OBJECT_CLASS (parent_class)->destroy)
    (*GTK_OBJECT_CLASS (parent_class)->destroy) (object);

  g_list_free(sheet->children);
}

static void
gtk_sheet_finalize (GtkObject * object)
{
  GtkSheet *sheet;

  g_return_if_fail (object != NULL);
  g_return_if_fail (GTK_IS_SHEET (object));

  sheet = GTK_SHEET(object);

  gtk_sheet_range_delete(sheet, NULL);

  DeleteRow (sheet, 0, sheet->maxrow + 1);
  DeleteColumn (sheet, 0, sheet->maxcol + 1);

  g_free(sheet->row);
  g_free(sheet->column);
  g_free(sheet->data);

  if(sheet->name)
      g_free(sheet->name);

  if (GTK_OBJECT_CLASS (parent_class)->finalize)
    (*GTK_OBJECT_CLASS (parent_class)->finalize) (object);

}

static void
gtk_sheet_style_set (GtkWidget *widget,
		     GtkStyle  *previous_style)
{
  GtkSheet *sheet;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (GTK_IS_SHEET (widget));

  if (GTK_WIDGET_CLASS (parent_class)->style_set)
    (*GTK_WIDGET_CLASS (parent_class)->style_set) (widget, previous_style);

  sheet = GTK_SHEET (widget);

  if(GTK_WIDGET_REALIZED(widget))
     {
       gtk_style_set_background (widget->style, widget->window, widget->state);
     }

}

static void
gtk_sheet_realize (GtkWidget * widget)
{
  GtkSheet *sheet;
  GdkWindowAttr attributes;
  gint attributes_mask;
  GdkGCValues values, auxvalues;
  GdkColormap *colormap;
  gchar *name;
  GtkSheetChild *child;
  GList *children;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (GTK_IS_SHEET (widget));

  sheet = GTK_SHEET (widget);

  GTK_WIDGET_SET_FLAGS (widget, GTK_REALIZED);

  attributes.window_type = GDK_WINDOW_CHILD;
  attributes.x = widget->allocation.x;
  attributes.y = widget->allocation.y;
  attributes.width = widget->allocation.width;
  attributes.height = widget->allocation.height;
  attributes.wclass = GDK_INPUT_OUTPUT;

  attributes.visual = gtk_widget_get_visual (widget);
  attributes.colormap = gtk_widget_get_colormap (widget);

  attributes.event_mask = gtk_widget_get_events (widget);
  attributes.event_mask |= (GDK_EXPOSURE_MASK |
			    GDK_BUTTON_PRESS_MASK |
			    GDK_BUTTON_RELEASE_MASK |
			    GDK_KEY_PRESS_MASK |
			    GDK_POINTER_MOTION_MASK |
			    GDK_POINTER_MOTION_HINT_MASK);
  attributes_mask = GDK_WA_X | GDK_WA_Y | GDK_WA_VISUAL | GDK_WA_COLORMAP |
                    GDK_WA_CURSOR;

  attributes.cursor = gdk_cursor_new(GDK_TOP_LEFT_ARROW);

  /* main window */
  widget->window = gdk_window_new (gtk_widget_get_parent_window (widget), &attributes, attributes_mask);

  gdk_window_set_user_data (widget->window, sheet);

  widget->style = gtk_style_attach (widget->style, widget->window);

  gtk_style_set_background (widget->style, widget->window, GTK_STATE_NORMAL);

  attributes.x = 0;
  if(GTK_SHEET_ROW_TITLES_VISIBLE(sheet))
       attributes.x = sheet->row_title_area.width;
  attributes.y = 0;
  attributes.width = sheet->column_title_area.width;
  attributes.height = sheet->column_title_area.height;

  /* column-title window */
  sheet->column_title_window = gdk_window_new (widget->window, &attributes, attributes_mask);
  gdk_window_set_user_data (sheet->column_title_window, sheet);
  gtk_style_set_background (widget->style, sheet->column_title_window, GTK_STATE_NORMAL);

  attributes.x = 0;
  attributes.y = 0;
  if(GTK_SHEET_COL_TITLES_VISIBLE(sheet))
       attributes.y = sheet->column_title_area.height;
  attributes.width = sheet->row_title_area.width;
  attributes.height = sheet->row_title_area.height;

  /* row-title window */
  sheet->row_title_window = gdk_window_new (widget->window, &attributes, attributes_mask);
  gdk_window_set_user_data (sheet->row_title_window, sheet);
  gtk_style_set_background (widget->style, sheet->row_title_window, GTK_STATE_NORMAL);

  /* sheet-window */
  attributes.cursor = gdk_cursor_new(GDK_PLUS);

  attributes.x = 0;
  attributes.y = 0;
  attributes.width = sheet->sheet_window_width, 
  attributes.height = sheet->sheet_window_height;

  sheet->sheet_window = gdk_window_new (widget->window, &attributes, attributes_mask);
  gdk_window_set_user_data (sheet->sheet_window, sheet);

  gdk_window_set_background (sheet->sheet_window, &widget->style->white);
  gdk_window_show (sheet->sheet_window);

  /* backing_pixmap */
  gtk_sheet_make_backing_pixmap(sheet, 0, 0);  

  /* GCs */
  if(sheet->fg_gc) 
      gdk_gc_unref(sheet->fg_gc);
  if(sheet->bg_gc) 
      gdk_gc_unref(sheet->bg_gc);
  sheet->fg_gc = gdk_gc_new (widget->window);
  sheet->bg_gc = gdk_gc_new (widget->window);

  colormap = gtk_widget_get_colormap(widget);

  gdk_color_white(colormap, &widget->style->white);
  gdk_color_black(colormap, &widget->style->black);

  gdk_gc_get_values(sheet->fg_gc, &auxvalues);

  values.foreground = widget->style->white;
  values.function = GDK_INVERT;
  values.subwindow_mode = GDK_INCLUDE_INFERIORS;
  if(sheet->xor_gc)
    gdk_gc_unref(sheet->xor_gc);
  sheet->xor_gc = gdk_gc_new_with_values (widget->window,
					  &values,
					  GDK_GC_FOREGROUND |
					  GDK_GC_FUNCTION |
					  GDK_GC_SUBWINDOW);

/* create sheet_entry_window */

  if(GTK_WIDGET_NO_WINDOW(sheet->sheet_entry)){

    attributes.window_type = GDK_WINDOW_CHILD;
    attributes.x = 0;
    attributes.y = 0;
    attributes.width = sheet->sheet_entry->requisition.width;
    attributes.height = sheet->sheet_entry->requisition.height;
    attributes.wclass = GDK_INPUT_OUTPUT;
    attributes.visual = gtk_widget_get_visual (widget);
    attributes.colormap = gtk_widget_get_colormap (widget);
    attributes.event_mask = GDK_EXPOSURE_MASK;
 
    attributes_mask = GDK_WA_X | GDK_WA_Y | GDK_WA_VISUAL | GDK_WA_COLORMAP;
    sheet->sheet_entry_window =  gdk_window_new (sheet->sheet_window,
   				                 &attributes, attributes_mask);
    gdk_window_set_user_data (sheet->sheet_entry_window, widget);
  }

  if(sheet->sheet_entry->parent){
          gtk_widget_ref(sheet->sheet_entry);
          gtk_widget_unparent(sheet->sheet_entry);
  }
  gtk_widget_set_parent(sheet->sheet_entry, GTK_WIDGET(sheet));
  gtk_widget_set_parent_window (sheet->sheet_entry,
				sheet->sheet_entry_window ? 
                                sheet->sheet_entry_window : 
                                sheet->sheet_window);

  if(sheet->button && sheet->button->parent){
          gtk_widget_ref(sheet->button);
          gtk_widget_unparent(sheet->button);
  }
  gtk_widget_set_parent(sheet->button, GTK_WIDGET(sheet));
  gtk_widget_set_parent_window(sheet->button, sheet->sheet_window);

  gtk_sheet_activate_cell(sheet, sheet->active_cell.row, sheet->active_cell.col);
  if(!sheet->cursor_drag)
       sheet->cursor_drag = gdk_cursor_new(GDK_PLUS);
 
  if(GTK_SHEET_COL_TITLES_VISIBLE(sheet))
     gdk_window_show(sheet->column_title_window);
  if(GTK_SHEET_ROW_TITLES_VISIBLE(sheet))
     gdk_window_show(sheet->row_title_window);

  size_allocate_row_title_buttons(sheet);
  size_allocate_column_title_buttons(sheet);

  name = g_strdup(sheet->name);
  gtk_sheet_set_title(sheet, name);

  g_free(name);

  children = sheet->children;
  while(children)
    {
      child = children->data;
      children = children->next;
 
      gtk_sheet_realize_child(sheet, child);
    }
}

static void
create_global_button(GtkSheet *sheet)
{
   sheet->button = gtk_button_new_with_label(" ");

   gtk_widget_ensure_style(sheet->button);
   gtk_widget_show(sheet->button);

   gtk_signal_connect (GTK_OBJECT (sheet->button),
		      "pressed",
		      (GtkSignalFunc) global_button_clicked,
		      (gpointer) sheet);
}

static void
size_allocate_global_button(GtkSheet *sheet)
{
  GtkAllocation allocation;

  if(!GTK_SHEET_COL_TITLES_VISIBLE(sheet)) return;
  if(!GTK_SHEET_ROW_TITLES_VISIBLE(sheet)) return;

  gtk_widget_size_request(sheet->button, NULL);

  allocation.x=0;
  allocation.y=0;
  allocation.width=sheet->row_title_area.width;
  allocation.height=sheet->column_title_area.height;

  gtk_widget_size_allocate(sheet->button, &allocation);
  gtk_widget_show(sheet->button);
}

static void
global_button_clicked(GtkWidget *widget, gpointer data)
{
 gboolean veto;

 gtk_sheet_click_cell(GTK_SHEET(data), -1, -1, &veto);
 gtk_widget_grab_focus(GTK_WIDGET(data));
}


static void
gtk_sheet_unrealize (GtkWidget * widget)
{
  GtkSheet *sheet;
  GtkSheetChild *child;
  GList *children;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (GTK_IS_SHEET (widget));

  sheet = GTK_SHEET (widget);
  GTK_WIDGET_UNSET_FLAGS (widget, GTK_REALIZED | GTK_MAPPED);

  gdk_cursor_destroy (sheet->cursor_drag);

  gdk_gc_destroy (sheet->xor_gc);
  gdk_gc_destroy (sheet->fg_gc);
  gdk_gc_destroy (sheet->bg_gc);

  gtk_style_detach (widget->style);

  gdk_window_destroy (sheet->sheet_window);
  gdk_window_destroy (sheet->column_title_window);
  gdk_window_destroy (sheet->row_title_window);
  gdk_window_set_user_data (widget->window, NULL);
  gdk_window_destroy (widget->window);

  if (sheet->pixmap){
    g_free (sheet->pixmap);
    sheet->pixmap = NULL;
  }

  widget->window = NULL;
  sheet->column_title_window=NULL;
  sheet->sheet_window = NULL;
  sheet->sheet_entry_window = NULL;
  sheet->cursor_drag = NULL;
  sheet->xor_gc = NULL;
  sheet->fg_gc = NULL;
  sheet->bg_gc = NULL;

  children = sheet->children;
  while (children)
   {
     child = children->data;
     children = children->next;

     if(child->window)
        {
            gdk_window_set_user_data(child->window, NULL);
            gdk_window_destroy(child->window);
            child->window = NULL;
        }

   }

}

static void
gtk_sheet_map (GtkWidget * widget)
{
  GtkSheet *sheet;
  GtkSheetChild *child;
  GList *children;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (GTK_IS_SHEET (widget));

  sheet = GTK_SHEET (widget);

  if (!GTK_WIDGET_MAPPED (widget))
    {
      GTK_WIDGET_SET_FLAGS (widget, GTK_MAPPED);

      if(!sheet->cursor_drag) sheet->cursor_drag=gdk_cursor_new(GDK_PLUS);

      gdk_window_show (widget->window);

      gdk_window_show (sheet->sheet_window);

      if(sheet->sheet_entry_window)
          gdk_window_show(sheet->sheet_entry_window);

      if(GTK_SHEET_COL_TITLES_VISIBLE(sheet)){
           gdk_window_show (sheet->column_title_window);
      }
      if(GTK_SHEET_ROW_TITLES_VISIBLE(sheet)){
           gdk_window_show (sheet->row_title_window);
      }

      if(!GTK_WIDGET_MAPPED (sheet->sheet_entry)){
      	          gtk_widget_show (sheet->sheet_entry);
   	          gtk_widget_map (sheet->sheet_entry);
      }

      if (GTK_WIDGET_VISIBLE (sheet->button) &&
	  !GTK_WIDGET_MAPPED (sheet->button)){
                  gtk_widget_show(sheet->button);
	          gtk_widget_map (sheet->button);
      }

      if(GTK_BIN(sheet->button)->child)
        if (GTK_WIDGET_VISIBLE (GTK_BIN(sheet->button)->child) &&
  	   !GTK_WIDGET_MAPPED (GTK_BIN(sheet->button)->child))
  	          gtk_widget_map (GTK_BIN(sheet->button)->child);

      gtk_sheet_range_draw(sheet, NULL);

      children = sheet->children;
      while (children)
      {
        child = children->data;
        children = children->next;

        if (GTK_WIDGET_VISIBLE (child->widget) &&
    	    !GTK_WIDGET_MAPPED (child->widget)){
	  gtk_widget_map (child->widget);
          gtk_sheet_position_child(sheet, child);
          if (GTK_WIDGET_NO_WINDOW(child->widget) && child->window) 
                         gdk_window_show(child->window);
        }
      }

    }
}

static void
gtk_sheet_unmap (GtkWidget * widget)
{
  GtkSheet *sheet;
  GtkSheetChild *child;
  GList *children;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (GTK_IS_SHEET (widget));

  sheet = GTK_SHEET (widget);

  if (GTK_WIDGET_MAPPED (widget))
    {
      GTK_WIDGET_UNSET_FLAGS (widget, GTK_MAPPED);

      gdk_window_hide (sheet->sheet_window);
      if(GTK_SHEET_COL_TITLES_VISIBLE(sheet))
          gdk_window_hide (sheet->column_title_window);
      if(GTK_SHEET_ROW_TITLES_VISIBLE(sheet))
          gdk_window_hide (sheet->row_title_window);
      gdk_window_hide (widget->window);

      if(sheet->sheet_entry_window)
          gdk_window_hide (sheet->sheet_entry_window);

      if (GTK_WIDGET_MAPPED (sheet->sheet_entry))
	gtk_widget_unmap (sheet->sheet_entry);

      if (GTK_WIDGET_MAPPED (sheet->button))
	gtk_widget_unmap (sheet->button);

      children = sheet->children;
      while (children)
        {
          child = children->data;
          children = children->next;

          if (GTK_WIDGET_VISIBLE (child->widget) &&
	      GTK_WIDGET_MAPPED (child->widget))
                {
  	             gtk_widget_unmap (child->widget);
                     if(child->window) gdk_window_hide(child->window);
                }
        }

    }
}


static void
gtk_sheet_cell_draw_default (GtkSheet *sheet, gint row, gint col)
{
  GtkWidget *widget;
  GdkGC *fg_gc, *bg_gc;
  GtkSheetCellAttr attributes;
  GdkRectangle area;

  g_return_if_fail (sheet != NULL);

  /* bail now if we arn't drawable yet */
  if (!GTK_WIDGET_DRAWABLE (sheet)) return;

  if (row < 0 || row > sheet->maxrow) return;
  if (col < 0 || col > sheet->maxcol) return;
  if (!sheet->column[col].is_visible) return;
  if (!sheet->row[row].is_visible) return;

  widget = GTK_WIDGET (sheet);

  gtk_sheet_get_attributes(sheet, row, col, &attributes);
 
  /* select GC for background rectangle */
  gdk_gc_set_foreground (sheet->fg_gc, &attributes.foreground);
  gdk_gc_set_foreground (sheet->bg_gc, &attributes.background);

  fg_gc = sheet->fg_gc;
  bg_gc = sheet->bg_gc;

  area.x=COLUMN_LEFT_XPIXEL(sheet,col);
  area.y=ROW_TOP_YPIXEL(sheet,row);
  area.width=sheet->column[col].width;
  area.height=sheet->row[row].height;

  gdk_draw_rectangle (sheet->pixmap,
  	              bg_gc,
	              TRUE,
	              area.x,
                      area.y,
	              area.width,
                      area.height);

  gdk_gc_set_line_attributes (sheet->fg_gc, 1, 0, 0, 0);

  if(attributes.background.pixel == GTK_WIDGET(sheet)->style->white.pixel)
        gdk_draw_rectangle (sheet->pixmap,
  	                    GTK_WIDGET(sheet)->style->bg_gc[GTK_STATE_NORMAL],
	                    FALSE,
	                    area.x, area.y,
	                    area.width, area.height);
}

static void
gtk_sheet_cell_draw_border (GtkSheet *sheet, gint row, gint col, gint mask)
{
  GtkWidget *widget;
  GdkGC *fg_gc, *bg_gc;
  GtkSheetCellAttr attributes;
  GdkRectangle area;
  gint width;

  g_return_if_fail (sheet != NULL);

  /* bail now if we arn't drawable yet */
  if (!GTK_WIDGET_DRAWABLE (sheet)) return;

  if (row < 0 || row > sheet->maxrow) return;
  if (col < 0 || col > sheet->maxcol) return;
  if (!sheet->column[col].is_visible) return;
  if (!sheet->row[row].is_visible) return;

  widget = GTK_WIDGET (sheet);

  gtk_sheet_get_attributes(sheet, row, col, &attributes);

  /* select GC for background rectangle */
  gdk_gc_set_foreground (sheet->fg_gc, &attributes.border.color);
  gdk_gc_set_foreground (sheet->bg_gc, &attributes.background);

  fg_gc = sheet->fg_gc;
  bg_gc = sheet->bg_gc;

  area.x=COLUMN_LEFT_XPIXEL(sheet,col);
  area.y=ROW_TOP_YPIXEL(sheet,row);
  area.width=sheet->column[col].width;
  area.height=sheet->row[row].height;

  width = attributes.border.width;
  gdk_gc_set_line_attributes(sheet->fg_gc, attributes.border.width,
                                           attributes.border.line_style,
                                           attributes.border.cap_style,
                                           attributes.border.join_style);

  if(width>0){
   if(attributes.border.mask & GTK_SHEET_LEFT_BORDER & mask)
      gdk_draw_line(sheet->pixmap, sheet->fg_gc,
                    area.x, area.y-width/2,
                    area.x, area.y+area.height+width/2+1);

   if(attributes.border.mask & GTK_SHEET_RIGHT_BORDER & mask)
      gdk_draw_line(sheet->pixmap, sheet->fg_gc,
                    area.x+area.width, area.y-width/2,
                    area.x+area.width, 
                    area.y+area.height+width/2+1);

   if(attributes.border.mask & GTK_SHEET_TOP_BORDER & mask)
      gdk_draw_line(sheet->pixmap, sheet->fg_gc,
                    area.x-width/2,area.y,
                    area.x+area.width+width/2+1, 
                    area.y);

   if(attributes.border.mask & GTK_SHEET_BOTTOM_BORDER & mask)
      gdk_draw_line(sheet->pixmap, sheet->fg_gc,
                    area.x-width/2, area.y+area.height,
                    area.x+area.width+width/2+1, 
                    area.y+area.height);

  }

}


static void
gtk_sheet_cell_draw_label (GtkSheet *sheet, gint row, gint col)
{
  GtkWidget *widget;
  GdkRectangle area, clip_area;
  gint i;
  gint text_width, text_height, y;
  gint xoffset=0;  
  int size, sizel, sizer;
  GdkGC *fg_gc, *bg_gc;
  GtkSheetCellAttr attributes;
  char *label;

  g_return_if_fail (sheet != NULL);

   /* bail now if we arn't drawable yet */
   if (!GTK_WIDGET_DRAWABLE (sheet))
    return;

  if (row > sheet->maxallocrow) return;
  if (col > sheet->maxalloccol) return;
  if (!sheet->data[row][col]) return;
  if (!sheet->data[row][col]->text || strlen(sheet->data[row][col]->text)==0)
      return;

  if (row < 0 || row > sheet->maxrow) return;
  if (col < 0 || col > sheet->maxcol) return;
  if (!sheet->column[col].is_visible) return;
  if (!sheet->row[row].is_visible) return;


  widget = GTK_WIDGET(sheet);

  label = sheet->data[row][col]->text;

  gtk_sheet_get_attributes(sheet, row, col, &attributes);

  /* select GC for background rectangle */
  gdk_gc_set_foreground (sheet->fg_gc, &attributes.foreground);
  gdk_gc_set_foreground (sheet->bg_gc, &attributes.background);
  gdk_gc_set_font(sheet->fg_gc, attributes.font);

  fg_gc = sheet->fg_gc;
  bg_gc = sheet->bg_gc;

  area.x=COLUMN_LEFT_XPIXEL(sheet,col);
  area.y=ROW_TOP_YPIXEL(sheet,row);
  area.width=sheet->column[col].width;
  area.height=sheet->row[row].height;

  clip_area = area;

  text_width = gdk_string_width (attributes.font, label);
  text_height = attributes.font->ascent + attributes.font->descent;
  y = area.y + area.height - CELLOFFSET;
  y = y - text_height + attributes.font->ascent;

  switch(attributes.justification){
    case GTK_JUSTIFY_RIGHT:
          size=area.width;
          area.x+=area.width;
          if(!GTK_SHEET_CLIP_TEXT(sheet)){          
           for(i=col-1; i>=MIN_VISIBLE_COLUMN(sheet); i--){
             if(gtk_sheet_cell_get_text(sheet, row, i)) break;
             if(size>=text_width+CELLOFFSET) break;
             size+=sheet->column[i].width;
             sheet->column[i].right_text_column = MAX(col, sheet->column[i].right_text_column);
           }
           area.width=size;
          }
          area.x-=size;
          xoffset+=area.width-text_width - 2 * CELLOFFSET -
                   attributes.border.width/2;
          break;
     case GTK_JUSTIFY_CENTER:
          sizel=area.width/2;
          sizer=area.width/2;
	  area.x+=area.width/2;
          if(!GTK_SHEET_CLIP_TEXT(sheet)){          
           for(i=col+1; i<=MAX_VISIBLE_COLUMN(sheet); i++){
             if(gtk_sheet_cell_get_text(sheet, row, i)) break;
             if(sizer>=text_width/2) break;
             sizer+=sheet->column[i].width;
             sheet->column[i].left_text_column = MIN(col, sheet->column[i].left_text_column);
           }
           for(i=col-1; i>=MIN_VISIBLE_COLUMN(sheet); i--){
             if(gtk_sheet_cell_get_text(sheet, row, i)) break;
             if(sizel>=text_width/2) break;
             sizel+=sheet->column[i].width;
             sheet->column[i].right_text_column = MAX(col, sheet->column[i].right_text_column);
           }
           size=MIN(sizel, sizer);
          }
	  area.x-=sizel;
          xoffset+= sizel - text_width/2 - CELLOFFSET;
	  area.width=sizel+sizer;
          break;
      case GTK_JUSTIFY_LEFT:
      default:
          size=area.width;
          if(!GTK_SHEET_CLIP_TEXT(sheet)){          
           for(i=col+1; i<=MAX_VISIBLE_COLUMN(sheet); i++){
             if(gtk_sheet_cell_get_text(sheet, row, i)) break;
             if(size>=text_width+CELLOFFSET) break;
             size+=sheet->column[i].width;
             sheet->column[i].left_text_column = MIN(col, sheet->column[i].left_text_column);
           }
           area.width=size;
          }
          xoffset += attributes.border.width/2;
          break;
   }

  if(!GTK_SHEET_CLIP_TEXT(sheet)) clip_area = area;
  gdk_gc_set_clip_rectangle(fg_gc, &clip_area);

  gdk_draw_string (sheet->pixmap, 
		   attributes.font,
		   fg_gc,
                   area.x + xoffset + CELLOFFSET,
		   y,
	      	   label);

  gdk_gc_set_clip_rectangle(fg_gc, NULL);

  gdk_draw_pixmap(sheet->sheet_window,
                  GTK_WIDGET(sheet)->style->fg_gc[GTK_STATE_NORMAL],
                  sheet->pixmap,
                  area.x,
                  area.y,
                  area.x,
                  area.y,
                  area.width,
                  area.height);      


}



static void
gtk_sheet_range_draw(GtkSheet *sheet, GtkSheetRange *range)
{
 gint i,j;
 GtkSheetRange drawing_range;
 GdkRectangle area;

 g_return_if_fail(sheet != NULL);
 g_return_if_fail(GTK_SHEET(sheet));
 
 if(!GTK_WIDGET_DRAWABLE(GTK_WIDGET(sheet))) return;
 if(!GTK_WIDGET_REALIZED(GTK_WIDGET(sheet))) return;
 if(!GTK_WIDGET_MAPPED(GTK_WIDGET(sheet))) return;

 if(range == NULL)
 {
   drawing_range.row0=MIN_VISIBLE_ROW(sheet);
   drawing_range.col0=MIN_VISIBLE_COLUMN(sheet);
   drawing_range.rowi=MAX_VISIBLE_ROW(sheet);
   drawing_range.coli=MAX_VISIBLE_COLUMN(sheet);
/*
   gdk_draw_rectangle (sheet->pixmap,
	               GTK_WIDGET(sheet)->style->white_gc,
	               TRUE,
	               0,0,
	               sheet->sheet_window_width,sheet->sheet_window_height);
*/
 }
 else
 {
   drawing_range.row0=MAX(range->row0, MIN_VISIBLE_ROW(sheet));
   drawing_range.col0=MAX(range->col0, MIN_VISIBLE_COLUMN(sheet));
   drawing_range.rowi=MIN(range->rowi, MAX_VISIBLE_ROW(sheet));
   drawing_range.coli=MIN(range->coli, MAX_VISIBLE_COLUMN(sheet));
 }

 if(drawing_range.coli == sheet->maxcol){
  area.x=COLUMN_LEFT_XPIXEL(sheet,sheet->maxcol)+
         sheet->column[sheet->maxcol].width+1;
  area.y=0;

  gdk_draw_rectangle (sheet->pixmap,
	              GTK_WIDGET(sheet)->style->white_gc,
	              TRUE,
	              area.x,area.y,
	              sheet->sheet_window_width - area.x, 
                      sheet->sheet_window_height);
  gdk_draw_pixmap(sheet->sheet_window,
                  GTK_WIDGET(sheet)->style->fg_gc[GTK_STATE_NORMAL],
                  sheet->pixmap,
                  area.x,
                  area.y,
                  area.x,
                  area.y,
	          sheet->sheet_window_width - area.x, 
                  sheet->sheet_window_height);                  

 }
 if(drawing_range.rowi == sheet->maxrow){
  area.x=0;
  area.y=ROW_TOP_YPIXEL(sheet,sheet->maxrow)+sheet->row[sheet->maxrow].height+1;

  gdk_draw_rectangle (sheet->pixmap,
	              GTK_WIDGET(sheet)->style->white_gc,
	              TRUE,
	              area.x,area.y,
	              sheet->sheet_window_width,
                      sheet->sheet_window_height - area.y);

  gdk_draw_pixmap(sheet->sheet_window,
                  GTK_WIDGET(sheet)->style->fg_gc[GTK_STATE_NORMAL],
                  sheet->pixmap,
                  area.x,
                  area.y,
                  area.x,
                  area.y,
                  sheet->sheet_window_width,
                  sheet->sheet_window_height - area.y);
 }

 for(i=drawing_range.row0; i<=drawing_range.rowi; i++)
  for(j=drawing_range.col0; j<=drawing_range.coli; j++){
     gtk_sheet_cell_draw_default(sheet, i, j);
  }

 for(i=drawing_range.row0; i<=drawing_range.rowi; i++)
  for(j=drawing_range.col0; j<=drawing_range.coli; j++){
     gtk_sheet_cell_draw_border(sheet, i-1, j, GTK_SHEET_BOTTOM_BORDER);
     gtk_sheet_cell_draw_border(sheet, i+1, j, GTK_SHEET_TOP_BORDER);
     gtk_sheet_cell_draw_border(sheet, i, j-1, GTK_SHEET_RIGHT_BORDER);
     gtk_sheet_cell_draw_border(sheet, i, j+1, GTK_SHEET_LEFT_BORDER);
     gtk_sheet_cell_draw_border(sheet, i, j, 15);
  }

 for(i=drawing_range.row0; i<=drawing_range.rowi; i++)
  for(j=drawing_range.col0; j<=drawing_range.coli; j++)
     if(i<=sheet->maxallocrow && j<=sheet->maxalloccol && sheet->data[i][j])
       gtk_sheet_cell_draw_label (sheet, i, j);
     
 for(i=drawing_range.row0; i<=drawing_range.rowi; i++)
  for(j=sheet->column[drawing_range.col0].left_text_column; j<drawing_range.col0; j++)
     if(i<=sheet->maxallocrow && j<=sheet->maxalloccol && sheet->data[i][j])
       gtk_sheet_cell_draw_label (sheet, i, j);
    
 for(i=drawing_range.row0; i<=drawing_range.rowi; i++)
  for(j=drawing_range.coli+1; j<=sheet->column[drawing_range.coli].right_text_column; j++)
     if(i<=sheet->maxallocrow && j<=sheet->maxalloccol && sheet->data[i][j])
       gtk_sheet_cell_draw_label (sheet, i, j); 

  gtk_sheet_draw_backing_pixmap(sheet, drawing_range);

  if(sheet->state != GTK_SHEET_NORMAL && gtk_sheet_range_isvisible(sheet, sheet->range))
       gtk_sheet_range_draw_selection(sheet, drawing_range);
    
   
  if(sheet->state == GTK_STATE_NORMAL && 
     sheet->active_cell.row >= drawing_range.row0 &&
     sheet->active_cell.row <= drawing_range.rowi &&
     sheet->active_cell.col >= drawing_range.col0 &&
     sheet->active_cell.col <= drawing_range.coli)    
                            gtk_sheet_show_active_cell(sheet);
  
}

static void
gtk_sheet_range_draw_selection(GtkSheet *sheet, GtkSheetRange range)
{
  GdkRectangle area;
  gint i,j;
  GtkSheetRange aux;

  if(range.col0 > sheet->range.coli || range.coli < sheet->range.col0 ||
     range.row0 > sheet->range.rowi || range.rowi < sheet->range.row0)
     return;

  if(!gtk_sheet_range_isvisible(sheet, range)) return;
  if(!GTK_WIDGET_REALIZED(GTK_WIDGET(sheet))) return;

  aux=range;

  range.col0=MAX(sheet->range.col0, range.col0);
  range.coli=MIN(sheet->range.coli, range.coli);
  range.row0=MAX(sheet->range.row0, range.row0);
  range.rowi=MIN(sheet->range.rowi, range.rowi);

  range.col0=MAX(range.col0, MIN_VISIBLE_COLUMN(sheet));
  range.coli=MIN(range.coli, MAX_VISIBLE_COLUMN(sheet));
  range.row0=MAX(range.row0, MIN_VISIBLE_ROW(sheet));
  range.rowi=MIN(range.rowi, MAX_VISIBLE_ROW(sheet));

  for(i=range.row0; i<=range.rowi; i++){
   for(j=range.col0; j<=range.coli; j++){

    if(gtk_sheet_cell_get_state(sheet, i, j)==GTK_STATE_SELECTED && 
       sheet->column[j].is_visible && sheet->row[i].is_visible){

      row_button_set(sheet, i);
      column_button_set(sheet, j);

      area.x=COLUMN_LEFT_XPIXEL(sheet,j);
      area.y=ROW_TOP_YPIXEL(sheet,i);
      area.width=sheet->column[j].width;
      area.height=sheet->row[i].height;

      if(i==sheet->range.row0){
            area.y=area.y+2;
            area.height=area.height-2;
      }
      if(i==sheet->range.rowi) area.height=area.height-3;
      if(j==sheet->range.col0){
            area.x=area.x+2;
            area.width=area.width-2;
      }
      if(j==sheet->range.coli) area.width=area.width-3;

      if(i!=sheet->active_cell.row || j!=sheet->active_cell.col){
       gdk_draw_rectangle (sheet->sheet_window,
  	                   sheet->xor_gc,
	   	           TRUE,
	                   area.x+1,area.y+1,
	                   area.width,area.height);
      }
    }

   }
  }

  gtk_sheet_draw_border(sheet, sheet->range);

}

static void
gtk_sheet_draw_backing_pixmap(GtkSheet *sheet, GtkSheetRange range)
{
  gint x,y,width,height;

  if(!GTK_WIDGET_REALIZED(GTK_WIDGET(sheet))) return;
 
  x=COLUMN_LEFT_XPIXEL(sheet,range.col0);
  y=ROW_TOP_YPIXEL(sheet, range.row0);  
  width=COLUMN_LEFT_XPIXEL(sheet, range.coli)-x+sheet->column[range.coli].width;
  height=ROW_TOP_YPIXEL(sheet, range.rowi)-y+sheet->row[range.rowi].height;

  if(range.row0==sheet->range.row0){
          y=y-5;
          height=height+5;
  }
  if(range.rowi==sheet->range.rowi) height=height+5;
  if(range.col0==sheet->range.col0){
            x=x-5;
            width=width+5;
  }
  if(range.coli==sheet->range.coli) width=width+5;

  
  width=MIN(width, sheet->sheet_window_width-x);
  height=MIN(height, sheet->sheet_window_height-y);

  x--; 
  y--;
  width+=2;
  height+=2;

  x = (GTK_SHEET_ROW_TITLES_VISIBLE(sheet))
       ? MAX(x, sheet->row_title_area.width) : MAX(x, 0);
  y = (GTK_SHEET_COL_TITLES_VISIBLE(sheet))
       ? MAX(y, sheet->column_title_area.height) : MAX(y, 0);

  if(range.coli==sheet->maxcol) width=sheet->sheet_window_width-x;
  if(range.rowi==sheet->maxrow) height=sheet->sheet_window_height-y;

  gdk_draw_pixmap(sheet->sheet_window,
                  GTK_WIDGET(sheet)->style->fg_gc[GTK_STATE_NORMAL],
                  sheet->pixmap,
                  x,
                  y,
                  x,
                  y,
                  width+1,
                  height+1);                  

}

static GtkSheetCell *
gtk_sheet_cell_new()
{
 GtkSheetCell *cell;
 cell = g_new(GtkSheetCell, 1);
 cell->text = NULL;
 cell->link = NULL;
 cell->attributes = NULL;
 return cell;
}

void 
gtk_sheet_set_cell_text(GtkSheet *sheet, int row, int col, const gchar *text)
{
 GtkSheetCellAttr attributes;

 g_return_if_fail (sheet != NULL);
 g_return_if_fail (GTK_IS_SHEET (sheet));
 if (col > sheet->maxcol || row > sheet->maxrow) return;
 if (col < 0 || row < 0) return;

 gtk_sheet_get_attributes(sheet, row, col, &attributes);
 gtk_sheet_set_cell(sheet, row, col, attributes.justification, text);
}

void 
gtk_sheet_set_cell(GtkSheet *sheet, int row, int col, int justification,
                   const gchar *text)
{
 GtkSheetCell **cell;
 GtkSheetRange range;
 gint text_width;
 GtkSheetCellAttr attributes;

 g_return_if_fail (sheet != NULL);
 g_return_if_fail (GTK_IS_SHEET (sheet));
 if (col > sheet->maxcol || row > sheet->maxrow) return;
 if (col < 0 || row < 0) return;

 CheckBounds(sheet, row, col);

 cell=&sheet->data[row][col];

 if(*cell==NULL)
  (*cell) = gtk_sheet_cell_new();

 gtk_sheet_get_attributes(sheet, row, col, &attributes);

 (*cell)->row = row;
 (*cell)->col = col;

 attributes.justification = justification;
 gtk_sheet_set_cell_attributes(sheet, row, col, attributes);

 if((*cell)->text){
    g_free((*cell)->text);
 }

 (*cell)->text=g_strdup(text);

 if(attributes.is_visible){

   text_width = gdk_string_width (attributes.font, (*cell)->text);

   range.row0 = row;
   range.rowi = row;
   range.col0 = sheet->view.col0;
   range.coli = sheet->view.coli;

   if(GTK_SHEET_AUTORESIZE(sheet) && !GTK_SHEET_IS_FROZEN(sheet) &&
      text_width > sheet->column[col].width-2*CELLOFFSET-attributes.border.width){
      gtk_sheet_set_column_width(sheet, col, text_width+2*CELLOFFSET+attributes.border.width);
   }
   else
     if(!GTK_SHEET_IS_FROZEN(sheet))
      gtk_sheet_range_draw(sheet, &range);
 }
 gtk_signal_emit(GTK_OBJECT(sheet),sheet_signals[CHANGED], row, col);


}

void
gtk_sheet_cell_clear (GtkSheet *sheet, gint row, gint column)
{
  GtkSheetRange range;

  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));
  if (column > sheet->maxcol || row > sheet->maxrow) return;
  if (column > sheet->maxalloccol || row > sheet->maxallocrow) return;
  if (column < 0 || row < 0) return;

  range.row0 = row;
  range.rowi = row;
  range.col0 = sheet->view.col0;
  range.coli = sheet->view.coli;

  gtk_sheet_real_cell_clear(sheet, row, column, FALSE);

  if(!GTK_SHEET_IS_FROZEN(sheet)){
     gtk_sheet_range_draw(sheet, &range);
  }
}

void
gtk_sheet_cell_delete (GtkSheet *sheet, gint row, gint column)
{
  GtkSheetRange range;

  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));
  if (column > sheet->maxcol || row > sheet->maxrow) return;
  if (column > sheet->maxalloccol || row > sheet->maxallocrow) return;
  if (column < 0 || row < 0) return;

  range.row0 = row;
  range.rowi = row;
  range.col0 = sheet->view.col0;
  range.coli = sheet->view.coli;

  gtk_sheet_real_cell_clear(sheet, row, column, TRUE);

  if(!GTK_SHEET_IS_FROZEN(sheet)){
     gtk_sheet_range_draw(sheet, &range);
  }
}

static void
gtk_sheet_real_cell_clear (GtkSheet *sheet, gint row, gint column, gboolean delete)
{
  gchar *text;
  gpointer link;

  if(row > sheet->maxallocrow || column > sheet->maxalloccol) return;
  if(!sheet->data[row][column]) return;

  text = gtk_sheet_cell_get_text(sheet, row, column); 
  link = gtk_sheet_get_link(sheet, row, column); 

  if(text){ 
      g_free(sheet->data[row][column]->text);
      sheet->data[row][column]->text = NULL;

      gtk_signal_emit(GTK_OBJECT(sheet),sheet_signals[CLEAR_CELL], row, column);
  }  

  if(delete){ 
     if(sheet->data[row][column]->attributes){
         g_free(sheet->data[row][column]->attributes);
         sheet->data[row][column]->attributes = NULL;
     }
     sheet->data[row][column]->link = NULL;
/*
     if(sheet->data[row][column]) g_free(sheet->data[row][column]);
*/
     sheet->data[row][column] = NULL;
  }

}
    
void
gtk_sheet_range_clear (GtkSheet *sheet, const GtkSheetRange *range)
{
  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  gtk_sheet_real_range_clear(sheet, range, FALSE);
}

void
gtk_sheet_range_delete (GtkSheet *sheet, const GtkSheetRange *range)
{
  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  gtk_sheet_real_range_clear(sheet, range, TRUE);
}
 
static void
gtk_sheet_real_range_clear (GtkSheet *sheet, const GtkSheetRange *range, 
                            gboolean delete)
{
  gint i, j;
  GtkSheetRange clear;

  if(!range){
    clear.row0=0;
    clear.rowi=sheet->maxallocrow;
    clear.col0=0;
    clear.coli=sheet->maxalloccol;
  }else
    clear=*range;  

  clear.row0=MAX(clear.row0, 0);
  clear.col0=MAX(clear.col0, 0);
  clear.rowi=MIN(clear.rowi, sheet->maxallocrow);
  clear.coli=MIN(clear.coli, sheet->maxalloccol);

  for(i=clear.row0; i<=clear.rowi; i++)
    for(j=clear.col0; j<=clear.coli; j++){
        gtk_sheet_real_cell_clear(sheet, i, j, delete);
    }

  gtk_sheet_range_draw(sheet, NULL);
}


gchar *     
gtk_sheet_cell_get_text (GtkSheet *sheet, gint row, gint col)
{
  g_return_val_if_fail (sheet != NULL, NULL);
  g_return_val_if_fail (GTK_IS_SHEET (sheet), NULL);

  if(col > sheet->maxcol || row > sheet->maxrow) return NULL;
  if(col < 0 || row < 0) return NULL;
  if(row > sheet->maxallocrow || col > sheet->maxalloccol) return NULL;
  if(!sheet->data[row]) return NULL;
  if(!sheet->data[row][col]) return NULL;
  if(!sheet->data[row][col]->text) return NULL;
  if(strlen(sheet->data[row][col]->text) == 0) return NULL;

  return (sheet->data[row][col]->text);
}

void 
gtk_sheet_link_cell(GtkSheet *sheet, gint row, gint col, gpointer link)
{
 g_return_if_fail (sheet != NULL);
 g_return_if_fail (GTK_IS_SHEET (sheet));
 if(col > sheet->maxcol || row > sheet->maxrow) return;
 if(col < 0 || row < 0) return;

 if(row > sheet->maxallocrow || col > sheet->maxalloccol ||
    !sheet->data[row][col])
       gtk_sheet_set_cell_text(sheet, row, col, "");

 sheet->data[row][col]->link = link;
}

gpointer 
gtk_sheet_get_link(GtkSheet *sheet, gint row, gint col)
{
 g_return_val_if_fail (sheet != NULL, NULL);
 g_return_val_if_fail (GTK_IS_SHEET (sheet), NULL);
 if(col > sheet->maxcol || row > sheet->maxrow) return NULL;
 if(col < 0 || row < 0) return NULL;

 if (row > sheet->maxallocrow || col > sheet->maxalloccol) return NULL; 
 if (!sheet->data[row][col]) return NULL; /* Added by Bob Lissner */ 

 return(sheet->data[row][col]->link);
}

void
gtk_sheet_remove_link(GtkSheet *sheet, gint row, gint col)
{
 g_return_if_fail (sheet != NULL);
 g_return_if_fail (GTK_IS_SHEET (sheet));
 if(col > sheet->maxcol || row > sheet->maxrow) return;
 if(col < 0 || row < 0) return;
 
 /* Fixed by Andreas Voegele */
 if(row < sheet->maxallocrow && col < sheet->maxalloccol &&
    sheet->data[row][col]->link)
                            sheet->data[row][col]->link = NULL;
}


int
gtk_sheet_cell_get_state (GtkSheet *sheet, gint row, gint col)
{
 gint state;
 GtkSheetRange *range;

 g_return_val_if_fail (sheet != NULL, 0);
 g_return_val_if_fail (GTK_IS_SHEET (sheet), 0);
 if(col > sheet->maxcol || row > sheet->maxrow) return 0;
 if(col < 0 || row < 0) return 0;

 state = sheet->state;
 range = &sheet->range;

 switch (state){
                case GTK_SHEET_NORMAL:
                     return GTK_STATE_NORMAL;
		     break;
		case GTK_SHEET_ROW_SELECTED:
                     if(row>=range->row0 && row<=range->rowi) 
                                        return GTK_STATE_SELECTED;
		     break;
                case GTK_SHEET_COLUMN_SELECTED:
                     if(col>=range->col0 && col<=range->coli) 
                                        return GTK_STATE_SELECTED;
		     break;
		case GTK_SHEET_RANGE_SELECTED:
                     if(row >= range->row0 && row <= range->rowi && \
                        col >= range->col0 && col <= range->coli)
                                        return GTK_STATE_SELECTED;
		     break;
 }
 return GTK_STATE_NORMAL;
}

gint
gtk_sheet_get_pixel_info (GtkSheet * sheet,
			  gint x,
			  gint y,
			  gint * row,
			  gint * column)
{
  gint trow, tcol;

  g_return_val_if_fail (sheet != NULL, 0);
  g_return_val_if_fail (GTK_IS_SHEET (sheet), 0);

  /* bounds checking, return false if the user clicked 
   * on a blank area */
  trow = ROW_FROM_YPIXEL (sheet, y);
  if (trow > sheet->maxrow)
    return FALSE;

  *row = trow;

  tcol = COLUMN_FROM_XPIXEL (sheet, x);
  if (tcol > sheet->maxcol)
    return FALSE;

 *column = tcol;

  return TRUE;
}

gint
gtk_sheet_get_cell_area  (GtkSheet * sheet,
			  gint row,
                          gint column,
			  GdkRectangle *area)
{
  g_return_val_if_fail (sheet != NULL, 0);
  g_return_val_if_fail (GTK_IS_SHEET (sheet), 0);

  if(row > sheet->maxrow || column > sheet->maxcol) return FALSE;

  area->x = (column == -1) ? 0 : (COLUMN_LEFT_XPIXEL(sheet, column) -
                                 ((GTK_SHEET_ROW_TITLES_VISIBLE(sheet))
                                   ? sheet->row_title_area.width
                                   : 0));
  area->y = (row == -1) ? 0 : (ROW_TOP_YPIXEL(sheet, row) -
                              ((GTK_SHEET_COL_TITLES_VISIBLE(sheet))
                               ? sheet->column_title_area.height
                               : 0));
  area->width= (column == -1) ? sheet->row_title_area.width
                              : sheet->column[column].width;
  area->height= (row == -1) ? sheet->column_title_area.height
                            : sheet->row[row].height;

/*
  if(row < 0 || column < 0) return FALSE;

  area->x = COLUMN_LEFT_XPIXEL(sheet, column);
  area->y = ROW_TOP_YPIXEL(sheet, row);
  if(GTK_SHEET_ROW_TITLES_VISIBLE(sheet))
           area->x -= sheet->row_title_area.width;
  if(GTK_SHEET_COL_TITLES_VISIBLE(sheet))
           area->y -= sheet->column_title_area.height;

  area->width=sheet->column[column].width;
  area->height=sheet->row[row].height;  
*/
  return TRUE;
}

gint 
gtk_sheet_set_active_cell (GtkSheet *sheet, gint row, gint column)
{
 g_return_val_if_fail (sheet != NULL, 0);
 g_return_val_if_fail (GTK_IS_SHEET (sheet), 0);

 if(row < 0 || column < 0) return FALSE;
 if(row > sheet->maxrow || column > sheet->maxcol) return FALSE;

 if(GTK_WIDGET_REALIZED(GTK_WIDGET(sheet)))
   {
       if(!gtk_sheet_deactivate_cell(sheet)) return FALSE;
   }

 sheet->active_cell.row=row;
 sheet->active_cell.col=column;
 
 if(!gtk_sheet_activate_cell(sheet, row, column)) return FALSE;
 
 return TRUE;
}

void
gtk_sheet_get_active_cell (GtkSheet *sheet, gint *row, gint *column)
{
  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  *row = sheet->active_cell.row;
  *column = sheet->active_cell.col;
}

static void
gtk_sheet_entry_changed(GtkWidget *widget, gpointer data)
{
 GtkSheet *sheet;
 gint row,col;
 char *text;
 gint justification;
 GtkSheetCellAttr attributes;

 g_return_if_fail (data != NULL);
 g_return_if_fail (GTK_IS_SHEET (data));

 sheet=GTK_SHEET(data);

 if(!GTK_WIDGET_VISIBLE(widget)) return;
 if(sheet->state != GTK_STATE_NORMAL) return;

 row=sheet->active_cell.row;
 col=sheet->active_cell.col;

 if(row<0 || col<0) return;

 sheet->active_cell.row=-1;
 sheet->active_cell.col=-1;

 text=gtk_entry_get_text(GTK_ENTRY(gtk_sheet_get_entry(sheet)));

 GTK_SHEET_SET_FLAGS(sheet, GTK_SHEET_IS_FROZEN);

 if(text && strlen(text)!=0){
      gtk_sheet_get_attributes(sheet, row, col, &attributes); 
      justification=attributes.justification;
      gtk_sheet_set_cell(sheet, row, col, justification, text);
 }
 else
 {
 /* Added by Matias Mutchinick */
      gtk_sheet_cell_clear(sheet, row, col);
 }

 if(sheet->freeze_count == 0)
        GTK_SHEET_UNSET_FLAGS(sheet, GTK_SHEET_IS_FROZEN);
 
 sheet->active_cell.row=row;;
 sheet->active_cell.col=col;

}


static gint
gtk_sheet_deactivate_cell(GtkSheet *sheet)
{
 gint veto = TRUE;

 g_return_val_if_fail (sheet != NULL, FALSE);
 g_return_val_if_fail (GTK_IS_SHEET (sheet), FALSE);

 if(!GTK_WIDGET_REALIZED(GTK_WIDGET(sheet))) return FALSE;
 if(sheet->state != GTK_SHEET_NORMAL) return FALSE;
 gtk_signal_emit(GTK_OBJECT(sheet),sheet_signals[DEACTIVATE], 
                                   sheet->active_cell.row,
                                   sheet->active_cell.col, &veto);

 if(!veto) return FALSE;

 gtk_signal_disconnect_by_func(GTK_OBJECT(gtk_sheet_get_entry(sheet)),
        	               (GtkSignalFunc) gtk_sheet_entry_changed,
                	       GTK_OBJECT(GTK_WIDGET(sheet)));

 gtk_sheet_hide_active_cell(sheet);

 sheet->active_cell.row=-1;
 sheet->active_cell.col=-1;

 return TRUE;
}	

static void
gtk_sheet_hide_active_cell(GtkSheet *sheet){
 char *text;
 gint row,col;
 gint justification;
 GtkSheetCellAttr attributes;

 if(!GTK_WIDGET_REALIZED(GTK_WIDGET(sheet))) return;

 row=sheet->active_cell.row;
 col=sheet->active_cell.col;

 if(row < 0 || col < 0) return;

 if(sheet->freeze_count == 0)
     GTK_SHEET_UNSET_FLAGS(sheet, GTK_SHEET_IS_FROZEN);

 text=gtk_entry_get_text(GTK_ENTRY(gtk_sheet_get_entry(sheet)));

 gtk_sheet_get_attributes(sheet, row, col, &attributes); 
 justification=attributes.justification;

 if(text && strlen(text)!=0){
      gtk_sheet_set_cell(sheet, row, col, justification, text);
      gtk_signal_emit(GTK_OBJECT(sheet),sheet_signals[SET_CELL], row, col);
 }
 else
 {
      gtk_sheet_cell_clear(sheet, row, col);
 }

 row=sheet->active_cell.row;
 col=sheet->active_cell.col;

 column_button_release(sheet, col);
 row_button_release(sheet, row);

 if(sheet->sheet_entry_window) 
      gdk_window_hide(sheet->sheet_entry_window);
 else
      gdk_window_hide(sheet->sheet_entry->window);

 if(row != -1 && col != -1)
   gdk_draw_pixmap(sheet->sheet_window,
                   GTK_WIDGET(sheet)->style->fg_gc[GTK_STATE_NORMAL],
                   sheet->pixmap,
                   COLUMN_LEFT_XPIXEL(sheet,col)-1,
                   ROW_TOP_YPIXEL(sheet,row)-1,
                   COLUMN_LEFT_XPIXEL(sheet,col)-1,
                   ROW_TOP_YPIXEL(sheet,row)-1,
                   sheet->column[col].width+4,
                   sheet->row[row].height+4);   

 GTK_WIDGET_UNSET_FLAGS(sheet->sheet_entry, GTK_HAS_FOCUS);
 GTK_WIDGET_SET_FLAGS(GTK_WIDGET(sheet), GTK_HAS_FOCUS);
 gtk_widget_grab_focus(GTK_WIDGET(sheet));

 GTK_WIDGET_UNSET_FLAGS(GTK_WIDGET(sheet->sheet_entry), GTK_VISIBLE);

}

static gint
gtk_sheet_activate_cell(GtkSheet *sheet, gint row, gint col)
{
 gint veto = TRUE;

 g_return_val_if_fail (sheet != NULL, FALSE);
 g_return_val_if_fail (GTK_IS_SHEET (sheet), FALSE);

 if(row < 0 || col < 0) return FALSE;
 if(row > sheet->maxrow || col > sheet->maxcol) return FALSE;


/* gtk_signal_emit(GTK_OBJECT(sheet),sheet_signals[ACTIVATE], row, col, &veto);
 if(!GTK_WIDGET_REALIZED(GTK_WIDGET(sheet))) return veto;
*/

 if(!veto) return FALSE;
 if(sheet->state != GTK_SHEET_NORMAL){
        sheet->state=GTK_SHEET_NORMAL;
        gtk_sheet_unselect_range(sheet, NULL);
 }

 sheet->range.row0=row;
 sheet->range.col0=col;
 sheet->range.rowi=row;
 sheet->range.coli=col;
 sheet->active_cell.row=row;
 sheet->active_cell.col=col;
 sheet->selection_cell.row=row;
 sheet->selection_cell.col=col;
 row_button_set(sheet, row);
 column_button_set(sheet, col); 

 GTK_SHEET_UNSET_FLAGS(sheet, GTK_SHEET_IN_SELECTION);
 gtk_sheet_show_active_cell(sheet);

 gtk_signal_connect(GTK_OBJECT(gtk_sheet_get_entry(sheet)),
        	    "changed",
                    (GtkSignalFunc)gtk_sheet_entry_changed,
                    GTK_OBJECT(GTK_WIDGET(sheet)));

 gtk_signal_emit(GTK_OBJECT(sheet),sheet_signals[ACTIVATE], row, col, &veto);

 return TRUE;
}

static void
gtk_sheet_show_active_cell(GtkSheet *sheet)
{
 GtkSheetCell *cell;
 GtkEntry *sheet_entry;
 GtkSheetCellAttr attributes;
 gchar *text = NULL;
 gint justification;
 gint row, col;

 g_return_if_fail (sheet != NULL);
 g_return_if_fail (GTK_IS_SHEET (sheet));

 if(!GTK_WIDGET_REALIZED(GTK_WIDGET(sheet))) return;
 if(sheet->state != GTK_SHEET_NORMAL) return;
 if(GTK_SHEET_IN_SELECTION(sheet)) return;

 GTK_WIDGET_SET_FLAGS(GTK_WIDGET(sheet->sheet_entry), GTK_VISIBLE);

 sheet_entry = GTK_ENTRY(gtk_sheet_get_entry(sheet));
 row = sheet->active_cell.row;
 col = sheet->active_cell.col;

 gtk_sheet_get_attributes(sheet, row, col, &attributes); 

 justification = attributes.justification;

 if(row <= sheet->maxallocrow && col <= sheet->maxalloccol){
  if(sheet->data[row][col]!=NULL){
    cell=sheet->data[row][col];
    if(cell->text) text = g_strdup(cell->text);
  }
 }

 if(!text) text = g_strdup("");

 if(!GTK_IS_IENTRY(sheet_entry))
    gtk_entry_set_text(GTK_ENTRY(sheet_entry), text);
 else
    gtk_item_entry_set_text(GTK_IENTRY(sheet_entry), text, justification);

 if(GTK_SHEET_IS_LOCKED(sheet) || !attributes.is_editable) 
            gtk_entry_set_editable(GTK_ENTRY(sheet_entry), FALSE);
 else
            gtk_entry_set_editable(GTK_ENTRY(sheet_entry), TRUE);

 gtk_entry_set_visibility(GTK_ENTRY(sheet_entry), attributes.is_visible);

 gtk_sheet_entry_set_max_size(sheet);
 gtk_sheet_size_allocate_entry(sheet);

 if(GTK_WIDGET_REALIZED(sheet->sheet_entry)){
     if(sheet->sheet_entry_window)           
            gdk_window_show(sheet->sheet_entry_window);
     else
            gdk_window_show(sheet->sheet_entry->window);
     gtk_widget_queue_draw(sheet->sheet_entry);
 }
 gtk_sheet_draw_active_cell(sheet);

 gtk_widget_grab_focus(GTK_WIDGET(sheet_entry));
 GTK_WIDGET_SET_FLAGS(GTK_WIDGET(sheet_entry), GTK_HAS_FOCUS);
 GTK_WIDGET_UNSET_FLAGS(GTK_WIDGET(sheet), GTK_HAS_FOCUS);

 g_free(text);
}

static void
gtk_sheet_draw_active_cell(GtkSheet *sheet)
{
    gint row, col;

    if(!GTK_WIDGET_DRAWABLE(GTK_WIDGET(sheet))) return;
    if(!GTK_WIDGET_REALIZED(GTK_WIDGET(sheet))) return;

    row = sheet->active_cell.row;
    col = sheet->active_cell.col;
 
    if(row<0 || col<0) return;

    if(!gtk_sheet_cell_isvisible(sheet, row, col)) return;
 
    row_button_set(sheet, row);
    column_button_set(sheet, col);

    gtk_sheet_draw_backing_pixmap(sheet, sheet->range);
    gtk_sheet_draw_border(sheet, sheet->range);

}


static void
gtk_sheet_make_backing_pixmap (GtkSheet *sheet, gint width, gint height)
{
  gint pixmap_width, pixmap_height;

  if(!GTK_WIDGET_REALIZED(GTK_WIDGET(sheet))) return;

  if(width == 0 && height == 0){
     width=sheet->sheet_window_width+80;
     height=sheet->sheet_window_height+80;
  }

  if (!sheet->pixmap)
    {
      /* allocate */
      sheet->pixmap = gdk_pixmap_new (sheet->sheet_window,
			              width, height,
				      -1);
      if(!GTK_SHEET_IS_FROZEN(sheet)) gtk_sheet_range_draw(sheet, NULL);
    }
  else
    {
      /* reallocate if sizes don't match */
      gdk_window_get_size (sheet->pixmap,
			   &pixmap_width, &pixmap_height);
      if ((pixmap_width != width) || (pixmap_height != height))
	{
          g_free(sheet->pixmap);
	  sheet->pixmap = gdk_pixmap_new (sheet->sheet_window,
					       width, height,
					       -1);
          if(!GTK_SHEET_IS_FROZEN(sheet)) gtk_sheet_range_draw(sheet, NULL);
	}
    }
}

static void
gtk_sheet_new_selection(GtkSheet *sheet, GtkSheetRange *range)
{
  gint i,j, mask1, mask2;
  gint state, selected;
  gint x,y,width,height;
  GtkSheetRange new_range, aux_range;

  g_return_if_fail (sheet != NULL);

  if(range==NULL) range=&sheet->range;

  new_range=*range;

  range->row0=MIN(range->row0, sheet->range.row0);
  range->rowi=MAX(range->rowi, sheet->range.rowi);
  range->col0=MIN(range->col0, sheet->range.col0);
  range->coli=MAX(range->coli, sheet->range.coli);

  range->row0=MAX(range->row0, MIN_VISIBLE_ROW(sheet));
  range->rowi=MIN(range->rowi, MAX_VISIBLE_ROW(sheet));
  range->col0=MAX(range->col0, MIN_VISIBLE_COLUMN(sheet));
  range->coli=MIN(range->coli, MAX_VISIBLE_COLUMN(sheet));

  aux_range.row0=MAX(new_range.row0, MIN_VISIBLE_ROW(sheet));
  aux_range.rowi=MIN(new_range.rowi, MAX_VISIBLE_ROW(sheet));
  aux_range.col0=MAX(new_range.col0, MIN_VISIBLE_COLUMN(sheet));
  aux_range.coli=MIN(new_range.coli, MAX_VISIBLE_COLUMN(sheet));

  for(i=range->row0; i<=range->rowi; i++){
   for(j=range->col0; j<=range->coli; j++){     

    state=gtk_sheet_cell_get_state(sheet, i, j);
    selected=(i<=new_range.rowi && i>=new_range.row0 && 
        j<=new_range.coli && j>=new_range.col0) ? TRUE : FALSE;

    if(state==GTK_STATE_SELECTED && selected &&
       sheet->column[j].is_visible && sheet->row[i].is_visible &&
       (i==sheet->range.row0 || i==sheet->range.rowi ||
        j==sheet->range.col0 || j==sheet->range.coli ||
        i==new_range.row0 || i==new_range.rowi ||
        j==new_range.col0 || j==new_range.coli)){

       mask1 = i==sheet->range.row0 ? 1 : 0;
       mask1 = i==sheet->range.rowi ? mask1+2 : mask1;
       mask1 = j==sheet->range.col0 ? mask1+4 : mask1;
       mask1 = j==sheet->range.coli ? mask1+8 : mask1;

       mask2 = i==new_range.row0 ? 1 : 0;
       mask2 = i==new_range.rowi ? mask2+2 : mask2;
       mask2 = j==new_range.col0 ? mask2+4 : mask2;
       mask2 = j==new_range.coli ? mask2+8 : mask2;     

       if(mask1 != mask2){
         x=COLUMN_LEFT_XPIXEL(sheet,j);
         y=ROW_TOP_YPIXEL(sheet, i);  
         width=COLUMN_LEFT_XPIXEL(sheet, j)-x+sheet->column[j].width;
         height=ROW_TOP_YPIXEL(sheet, i)-y+sheet->row[i].height;

         if(i==sheet->range.row0){
            y=y-3;
            height=height+3;
         }
         if(i==sheet->range.rowi) height=height+3;
         if(j==sheet->range.col0){
            x=x-3;
            width=width+3;
         }
         if(j==sheet->range.coli) width=width+3;

         gdk_draw_pixmap(sheet->sheet_window,
                  GTK_WIDGET(sheet)->style->fg_gc[GTK_STATE_NORMAL],
                  sheet->pixmap,
                  x+1,
                  y+1,
                  x+1,
                  y+1,
                  width,
                  height);           

         if(i != sheet->active_cell.row || j != sheet->active_cell.col){
           x=COLUMN_LEFT_XPIXEL(sheet,j);
           y=ROW_TOP_YPIXEL(sheet, i);  
           width=COLUMN_LEFT_XPIXEL(sheet, j)-x+sheet->column[j].width;
           height=ROW_TOP_YPIXEL(sheet, i)-y+sheet->row[i].height;

           if(i==new_range.row0){
               y=y+2;
               height=height-2;
            }
            if(i==new_range.rowi) height=height-3;
            if(j==new_range.col0){
               x=x+2;
               width=width-2;
            }
            if(j==new_range.coli) width=width-3;

            gdk_draw_rectangle (sheet->sheet_window,
  	                   sheet->xor_gc,
	   	           TRUE,
	                   x+1,y+1,
	                   width,height);
          }
       }
    }
   }
  }

  for(i=range->row0; i<=range->rowi; i++){
   for(j=range->col0; j<=range->coli; j++){     

    state=gtk_sheet_cell_get_state(sheet, i, j);
    selected=(i<=new_range.rowi && i>=new_range.row0 && 
        j<=new_range.coli && j>=new_range.col0) ? TRUE : FALSE;

    if(state==GTK_STATE_SELECTED && !selected &&   
       sheet->column[j].is_visible && sheet->row[i].is_visible){

      x=COLUMN_LEFT_XPIXEL(sheet,j);
      y=ROW_TOP_YPIXEL(sheet, i);  
      width=COLUMN_LEFT_XPIXEL(sheet, j)-x+sheet->column[j].width;
      height=ROW_TOP_YPIXEL(sheet, i)-y+sheet->row[i].height;

      if(i==sheet->range.row0){
            y=y-3;
            height=height+3;
      }
      if(i==sheet->range.rowi) height=height+3;
      if(j==sheet->range.col0){
            x=x-3;
            width=width+3;
      }
      if(j==sheet->range.coli) width=width+3;

      gdk_draw_pixmap(sheet->sheet_window,
                  GTK_WIDGET(sheet)->style->fg_gc[GTK_STATE_NORMAL],
                  sheet->pixmap,
                  x+1,
                  y+1,
                  x+1,
                  y+1,
                  width,
                  height);           
    }
   }
  }

  for(i=range->row0; i<=range->rowi; i++){
   for(j=range->col0; j<=range->coli; j++){     

    state=gtk_sheet_cell_get_state(sheet, i, j);
    selected=(i<=new_range.rowi && i>=new_range.row0 && 
        j<=new_range.coli && j>=new_range.col0) ? TRUE : FALSE;

    if(state!=GTK_STATE_SELECTED && selected &&
       sheet->column[j].is_visible && sheet->row[i].is_visible &&
       (i != sheet->active_cell.row || j != sheet->active_cell.col)){

      x=COLUMN_LEFT_XPIXEL(sheet,j);
      y=ROW_TOP_YPIXEL(sheet, i);  
      width=COLUMN_LEFT_XPIXEL(sheet, j)-x+sheet->column[j].width;
      height=ROW_TOP_YPIXEL(sheet, i)-y+sheet->row[i].height;

      if(i==new_range.row0){
            y=y+2;
            height=height-2;
       }
       if(i==new_range.rowi) height=height-3;
       if(j==new_range.col0){
            x=x+2;
            width=width-2;
       }
       if(j==new_range.coli) width=width-3;

       gdk_draw_rectangle (sheet->sheet_window,
  	                   sheet->xor_gc,
	   	           TRUE,
	                   x+1,y+1,
	                   width,height);

    }   

   }
  }

  for(i=aux_range.row0; i<=aux_range.rowi; i++){
   for(j=aux_range.col0; j<=aux_range.coli; j++){     

    if(sheet->column[j].is_visible && sheet->row[i].is_visible){

       state=gtk_sheet_cell_get_state(sheet, i, j);

       mask1 = i==sheet->range.row0 ? 1 : 0;
       mask1 = i==sheet->range.rowi ? mask1+2 : mask1;
       mask1 = j==sheet->range.col0 ? mask1+4 : mask1;
       mask1 = j==sheet->range.coli ? mask1+8 : mask1;

       mask2 = i==new_range.row0 ? 1 : 0;
       mask2 = i==new_range.rowi ? mask2+2 : mask2;
       mask2 = j==new_range.col0 ? mask2+4 : mask2;
       mask2 = j==new_range.coli ? mask2+8 : mask2;    
       if(mask2!=mask1 || (mask2==mask1 && state!=GTK_STATE_SELECTED)){
         x=COLUMN_LEFT_XPIXEL(sheet,j);
         y=ROW_TOP_YPIXEL(sheet, i);  
         width=sheet->column[j].width;
         height=sheet->row[i].height;
         if(mask2 & 1)
               gdk_draw_rectangle (sheet->sheet_window,
  	                           sheet->xor_gc,
	   	                   TRUE,
	                           x+1,y-1,
	                           width,3);

           
         if(mask2 & 2)
               gdk_draw_rectangle (sheet->sheet_window,
  	                           sheet->xor_gc,
	   	                   TRUE,
	                           x+1,y+height-1,
	                           width,3);

         if(mask2 & 4)
               gdk_draw_rectangle (sheet->sheet_window,
  	                           sheet->xor_gc,
	   	                   TRUE,
	                           x-1,y+1,
	                           3,height);


         if(mask2 & 8)
               gdk_draw_rectangle (sheet->sheet_window,
  	                           sheet->xor_gc,
	   	                   TRUE,
	                           x+width-1,y+1,
	                           3,height);

       

       }         

    } 

   }
  } 


  *range=new_range;
  gtk_sheet_draw_corners(sheet, new_range);

}

static void
gtk_sheet_draw_border (GtkSheet *sheet, GtkSheetRange new_range)
{
  GtkWidget *widget;
  GdkRectangle area;
  gint i;
  gint x,y,width,height;

  widget = GTK_WIDGET(sheet);

  x=COLUMN_LEFT_XPIXEL(sheet,new_range.col0);
  y=ROW_TOP_YPIXEL(sheet,new_range.row0);
  width=COLUMN_LEFT_XPIXEL(sheet,new_range.coli)-x+ 
             sheet->column[new_range.coli].width;
  height=ROW_TOP_YPIXEL(sheet,new_range.rowi)-y+
             sheet->row[new_range.rowi].height;

  area.x=COLUMN_LEFT_XPIXEL(sheet, MIN_VISIBLE_COLUMN(sheet));
  area.y=ROW_TOP_YPIXEL(sheet, MIN_VISIBLE_ROW(sheet));
  area.width=sheet->sheet_window_width;
  area.height=sheet->sheet_window_height;

  if(x<0) {
      width=width+x;
      x=0;
  }
  if(width>area.width) width=area.width+10;
  if(y<0) {
      height=height+y;
      y=0;
  }
  if(height>area.height) height=area.height+10;

  gdk_gc_set_clip_rectangle(sheet->xor_gc, &area);

  for(i=-1; i<=1; i++)
     gdk_draw_rectangle (sheet->sheet_window,
  	                 sheet->xor_gc,
                         FALSE,
	                 x+i,y+i,
	                 width-2*i,height-2*i);

  gdk_gc_set_clip_rectangle(sheet->xor_gc, NULL);
  
  gtk_sheet_draw_corners(sheet, new_range);

}

static void
gtk_sheet_draw_corners(GtkSheet *sheet, GtkSheetRange range)
{
  gint x,y;
  gint width = 1;

  if(gtk_sheet_cell_isvisible(sheet, range.row0, range.col0)){
       x=COLUMN_LEFT_XPIXEL(sheet,range.col0);
       y=ROW_TOP_YPIXEL(sheet,range.row0);
       gdk_draw_pixmap(sheet->sheet_window,
                       GTK_WIDGET(sheet)->style->fg_gc[GTK_STATE_NORMAL],
                       sheet->pixmap,
                       x-1,
                       y-1,
                       x-1,
                       y-1,
                       3,
                       3);         
       gdk_draw_rectangle (sheet->sheet_window,
  	                   sheet->xor_gc,
                           TRUE,
	                   x-1,y-1,
	                   3,3);
  }

  if(gtk_sheet_cell_isvisible(sheet, range.row0, range.coli) ||
     sheet->state == GTK_SHEET_COLUMN_SELECTED){
       x=COLUMN_LEFT_XPIXEL(sheet,range.coli)+
         sheet->column[range.coli].width;
       y=ROW_TOP_YPIXEL(sheet,range.row0);
       width = 1;
       if(sheet->state == GTK_SHEET_COLUMN_SELECTED)
         {
             y = ROW_TOP_YPIXEL(sheet, sheet->view.row0)+3;
             width = 3;
         }
       gdk_draw_pixmap(sheet->sheet_window,
                       GTK_WIDGET(sheet)->style->fg_gc[GTK_STATE_NORMAL],
                       sheet->pixmap,
                       x-width,
                       y-width,
                       x-width,
                       y-width,
                       2*width+1,
                       2*width+1);         
       gdk_draw_rectangle (sheet->sheet_window,
  	                   sheet->xor_gc,
                           TRUE,
	                   x-width+width/2,y-width+width/2,
	                   2+width,2+width);
  }

  if(gtk_sheet_cell_isvisible(sheet, range.rowi, range.col0) ||
     sheet->state == GTK_SHEET_ROW_SELECTED){
       x=COLUMN_LEFT_XPIXEL(sheet,range.col0);
       y=ROW_TOP_YPIXEL(sheet,range.rowi)+
         sheet->row[range.rowi].height;
       width = 1;
       if(sheet->state == GTK_SHEET_ROW_SELECTED) 
         {
             x = COLUMN_LEFT_XPIXEL(sheet, sheet->view.col0)+3;
             width = 3;
         }
       gdk_draw_pixmap(sheet->sheet_window,
                       GTK_WIDGET(sheet)->style->fg_gc[GTK_STATE_NORMAL],
                       sheet->pixmap,
                       x-width,
                       y-width,
                       x-width,
                       y-width,
                       2*width+1,
                       2*width+1);         
       gdk_draw_rectangle (sheet->sheet_window,
  	                   sheet->xor_gc,
                           TRUE,
	                   x-width+width/2,y-width+width/2,
	                   2+width,2+width);
  }

  if(gtk_sheet_cell_isvisible(sheet, range.rowi, range.coli)){
       x=COLUMN_LEFT_XPIXEL(sheet,range.coli)+
         sheet->column[range.coli].width;
       y=ROW_TOP_YPIXEL(sheet,range.rowi)+
         sheet->row[range.rowi].height;
       width = 1;
       if(sheet->state == GTK_SHEET_RANGE_SELECTED) width = 3;
       if(sheet->state == GTK_SHEET_NORMAL) width = 3;
       gdk_draw_pixmap(sheet->sheet_window,
                       GTK_WIDGET(sheet)->style->fg_gc[GTK_STATE_NORMAL],
                       sheet->pixmap,
                       x-width,
                       y-width,
                       x-width,
                       y-width,
                       2*width+1,
                       2*width+1);         
       gdk_draw_rectangle (sheet->sheet_window,
  	                   sheet->xor_gc,
                           TRUE,
	                   x-width+width/2,y-width+width/2,
	                   2+width,2+width);

  }

}



static void
gtk_sheet_real_select_range (GtkSheet * sheet,
			GtkSheetRange * range)
{
  gint i;
  gint state;

  g_return_if_fail (sheet != NULL);

  if(range==NULL) range=&sheet->range;

  if(range->row0 < 0 || range->rowi < 0) return;
  if(range->col0 < 0 || range->coli < 0) return;

  state=sheet->state;

  if(state==GTK_SHEET_COLUMN_SELECTED || state==GTK_SHEET_RANGE_SELECTED){
   for(i=sheet->range.col0; i< range->col0; i++)
    column_button_release(sheet, i);
   for(i=range->coli+1; i<= sheet->range.coli; i++)
    column_button_release(sheet, i);
   for(i=range->col0; i<=range->coli; i++){
    column_button_set(sheet, i);
   }
  }
 
  if(state==GTK_SHEET_ROW_SELECTED || state==GTK_SHEET_RANGE_SELECTED){
   for(i=sheet->range.row0; i< range->row0; i++)
    row_button_release(sheet, i);
   for(i=range->rowi+1; i<= sheet->range.rowi; i++)
    row_button_release(sheet, i);
   for(i=range->row0; i<=range->rowi; i++){
    row_button_set(sheet, i);
   }
  }

  if(range->coli != sheet->range.coli || range->col0 != sheet->range.col0 ||
     range->rowi != sheet->range.rowi || range->row0 != sheet->range.row0)
         {

           gtk_sheet_new_selection(sheet, range);

	   sheet->range.col0=range->col0;
	   sheet->range.coli=range->coli;
	   sheet->range.row0=range->row0;
	   sheet->range.rowi=range->rowi;

	 }
  else
         {
	   gtk_sheet_draw_backing_pixmap(sheet, sheet->range);
           gtk_sheet_range_draw_selection(sheet, sheet->range);
         }

  gtk_signal_emit(GTK_OBJECT(sheet), sheet_signals[SELECT_RANGE], range);
}

void
gtk_sheet_select_range(GtkSheet * sheet, const GtkSheetRange *range)
{
  g_return_if_fail (sheet != NULL);

  if(range==NULL) range=&sheet->range;

  if(range->row0 < 0 || range->rowi < 0) return;
  if(range->col0 < 0 || range->coli < 0) return;

  if(sheet->state != GTK_SHEET_NORMAL) 
       gtk_sheet_unselect_range(sheet, NULL);
  else
       gtk_sheet_deactivate_cell(sheet);

  sheet->range.row0=range->row0;
  sheet->range.rowi=range->rowi;
  sheet->range.col0=range->col0;
  sheet->range.coli=range->coli;
  sheet->active_cell.row=range->row0;
  sheet->active_cell.col=range->col0;
  sheet->selection_cell.row=range->rowi;
  sheet->selection_cell.col=range->coli;

  sheet->state = GTK_SHEET_RANGE_SELECTED;
  gtk_sheet_real_select_range(sheet, NULL);

}

void
gtk_sheet_unselect_range (GtkSheet * sheet,
			  const GtkSheetRange *range)
{
  gint i;
 
  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_WIDGET_REALIZED(GTK_WIDGET(sheet)));

  if(range==NULL){
     range=&sheet->range;
  }

  if(range->row0 < 0 || range->rowi < 0) return;
  if(range->col0 < 0 || range->coli < 0) return;

  if (gtk_sheet_range_isvisible (sheet, *range)){
    gtk_sheet_draw_backing_pixmap(sheet, *range);
  }

  for(i=range->col0; i<=range->coli; i++){
     column_button_release(sheet, i);
  }

  for(i=range->row0; i<=range->rowi; i++){
     row_button_release(sheet, i);
  }

}


static void
gtk_sheet_draw (GtkWidget * widget,
		GdkRectangle * area)
{
  GtkSheet *sheet;
  GtkSheetRange range;
  GtkSheetChild *child;
  GdkRectangle child_area;
  GList *children;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (GTK_IS_SHEET (widget));
  g_return_if_fail (area != NULL);

  if (GTK_WIDGET_DRAWABLE (widget))
    {
      sheet = GTK_SHEET (widget);

      range.row0=ROW_FROM_YPIXEL(sheet, area->y);
      range.rowi=ROW_FROM_YPIXEL(sheet, area->y+area->height);
      range.col0=COLUMN_FROM_XPIXEL(sheet, area->x);
      range.coli=COLUMN_FROM_XPIXEL(sheet, area->x+area->width);

      gtk_sheet_range_draw (sheet, &range);

      if(sheet->state != GTK_SHEET_NORMAL && gtk_sheet_range_isvisible(sheet, sheet->range)){          
              gtk_sheet_draw_backing_pixmap(sheet, sheet->range);
              gtk_sheet_range_draw_selection(sheet, sheet->range);
      }

      if(GTK_SHEET_ROW_TITLES_VISIBLE(sheet))
          gdk_window_show(sheet->row_title_window);

      if(GTK_SHEET_COL_TITLES_VISIBLE(sheet))
          gdk_window_show(sheet->column_title_window);

      children = sheet->children;
      while (children)
	{
	  child = children->data;
	  children = children->next;
	     
	  if (gtk_widget_intersect (child->widget, area, &child_area))
	    gtk_widget_draw (child->widget, &child_area);
	}

      if(GTK_SHEET_ROW_TITLES_VISIBLE(sheet)
         && GTK_SHEET_COL_TITLES_VISIBLE(sheet))
                  gtk_widget_draw(sheet->button, NULL);

    }

}


static gint
gtk_sheet_expose (GtkWidget * widget,
		  GdkEventExpose * event)
{
  GtkSheet *sheet;
  GtkSheetRange range;
  GtkSheetChild *child;
  GList *children;
  GdkEventExpose child_event;

  g_return_val_if_fail (widget != NULL, FALSE);
  g_return_val_if_fail (GTK_IS_SHEET (widget), FALSE);
  g_return_val_if_fail (event != NULL, FALSE);

  sheet = GTK_SHEET (widget);

  if (GTK_WIDGET_DRAWABLE (widget))
  {
      range.row0=ROW_FROM_YPIXEL(sheet,event->area.y);
      range.col0=COLUMN_FROM_XPIXEL(sheet,event->area.x);
      range.rowi=ROW_FROM_YPIXEL(sheet,event->area.y+event->area.height);
      range.coli=COLUMN_FROM_XPIXEL(sheet,event->area.x+event->area.width);

      /* exposure events on the sheet */
 
      if(event->window == sheet->row_title_window){
                     size_allocate_row_title_buttons(sheet);
                     gdk_window_show(sheet->row_title_window);
      }

      if(event->window == sheet->column_title_window){
                     size_allocate_column_title_buttons(sheet);
                     gdk_window_show(sheet->column_title_window);
      }

      if (event->window == sheet->sheet_window){
        gtk_sheet_draw_backing_pixmap(sheet, range);
              
        if(sheet->state != GTK_SHEET_NORMAL){
                if(gtk_sheet_range_isvisible(sheet, sheet->range))          
                   gtk_sheet_draw_backing_pixmap(sheet, sheet->range);
                if(GTK_SHEET_IN_RESIZE(sheet) || GTK_SHEET_IN_DRAG(sheet))
                   gtk_sheet_draw_backing_pixmap(sheet, sheet->drag_range);

                if(gtk_sheet_range_isvisible(sheet, sheet->range))          
                   gtk_sheet_range_draw_selection(sheet, sheet->range);
                if(GTK_SHEET_IN_RESIZE(sheet) || GTK_SHEET_IN_DRAG(sheet))
                   draw_xor_rectangle(sheet, sheet->drag_range);
        }

        if((!GTK_SHEET_IN_XDRAG(sheet)) && (!GTK_SHEET_IN_YDRAG(sheet))){
             if(sheet->state == GTK_SHEET_NORMAL){ 
                 gtk_sheet_draw_active_cell(sheet);
                 if(!GTK_SHEET_IN_SELECTION(sheet))
                         gtk_widget_queue_draw(sheet->sheet_entry);
             }
        }

        /* sheet children events */
        child_event = *event;
        children = sheet->children;
        while (children)
          {
            child = children->data;
	    children = children->next;
	
            if (GTK_WIDGET_NO_WINDOW (child->widget))
                {
                   GdkRectangle child_area;

                   child_area.x = child->x;
                   child_area.y = child->y;
                   child_area.width = child->widget->allocation.width;
                   child_area.height = child->widget->allocation.height;
	           gdk_rectangle_intersect (&child_area, &event->area, &child_event.area);
                   child_event.window = event->window;
                   if(child->window) child_event.window = child->window;
                   gtk_widget_event (child->widget, (GdkEvent*) &child_event);
                }
          }

      }

  }


  if(sheet->state != GTK_SHEET_NORMAL && GTK_SHEET_IN_SELECTION(sheet))
     gtk_widget_grab_focus(GTK_WIDGET(sheet));

  return FALSE;
}

/*
static void 
gtk_sheet_forall (GtkContainer *container,
                  gboolean include_internals,
                  GtkCallback  callback, 
                  gpointer  callback_data) 
{
   GtkSheet *sheet;

   sheet = GTK_SHEET(container);

   (*callback) (sheet->button, callback_data);
}
*/

static gint
gtk_sheet_button_press (GtkWidget * widget,
			GdkEventButton * event)
{
  GtkSheet *sheet;
  GdkModifierType mods;
  gint x, y, row, column;
  gboolean veto;

  g_return_val_if_fail (widget != NULL, FALSE);
  g_return_val_if_fail (GTK_IS_SHEET (widget), FALSE);
  g_return_val_if_fail (event != NULL, FALSE);

  if(event->type != GDK_BUTTON_PRESS) return TRUE;
  gdk_window_get_pointer(widget->window, NULL, NULL, &mods);
  if(!(mods & GDK_BUTTON1_MASK)) return TRUE;

  sheet = GTK_SHEET (widget);

  /* press on resize windows */
  if (event->window == sheet->column_title_window &&
     !GTK_SHEET_COLUMN_FROZEN(sheet))
      {
	gtk_widget_get_pointer (widget, &sheet->x_drag, NULL);
        if(POSSIBLE_XDRAG(sheet, sheet->x_drag, &sheet->drag_cell.col)){

	  GTK_SHEET_SET_FLAGS (sheet, GTK_SHEET_IN_XDRAG);
	  gdk_pointer_grab (sheet->column_title_window, FALSE,
			    GDK_POINTER_MOTION_HINT_MASK |
			    GDK_BUTTON1_MOTION_MASK |
			    GDK_BUTTON_RELEASE_MASK,
			    NULL, NULL, event->time);

	  draw_xor_vline (sheet);
	  return TRUE;
        }
      }

  if (event->window == sheet->row_title_window && !GTK_SHEET_ROW_FROZEN(sheet))
      {
	gtk_widget_get_pointer (widget, NULL, &sheet->y_drag);

        if(POSSIBLE_YDRAG(sheet, sheet->y_drag, &sheet->drag_cell.row)){
	  GTK_SHEET_SET_FLAGS (sheet, GTK_SHEET_IN_YDRAG);
	  gdk_pointer_grab (sheet->row_title_window, FALSE,
			    GDK_POINTER_MOTION_HINT_MASK |
			    GDK_BUTTON1_MOTION_MASK |
			    GDK_BUTTON_RELEASE_MASK,
			    NULL, NULL, event->time);

	  draw_xor_hline (sheet);
	  return TRUE;
        }
      }

  /* selections on the sheet */
    if(event->window == sheet->sheet_window){
     gtk_widget_get_pointer (widget, &x, &y);
     gtk_sheet_get_pixel_info (sheet, x, y, &row, &column);
     gdk_pointer_grab (sheet->sheet_window, FALSE,
		       GDK_POINTER_MOTION_HINT_MASK |
		       GDK_BUTTON1_MOTION_MASK |
		       GDK_BUTTON_RELEASE_MASK,
		       NULL, NULL, event->time);
     gtk_grab_add(GTK_WIDGET(sheet));
     sheet->timer=gtk_timeout_add(TIMEOUT_SCROLL, gtk_sheet_scroll, sheet); 
     GTK_WIDGET_UNSET_FLAGS(sheet->sheet_entry, GTK_HAS_FOCUS);
     GTK_WIDGET_SET_FLAGS(GTK_SHEET(sheet), GTK_HAS_FOCUS);
     gtk_widget_grab_focus(GTK_WIDGET(sheet));

     if(sheet->selection_mode != GTK_SELECTION_SINGLE &&
        sheet->cursor_drag->type==GDK_SIZING &&
        !GTK_SHEET_IN_SELECTION(sheet) && !GTK_SHEET_IN_RESIZE(sheet)){
        if(sheet->state==GTK_STATE_NORMAL) {
          row=sheet->active_cell.row;
          column=sheet->active_cell.col;
          if(!gtk_sheet_deactivate_cell(sheet)) return FALSE;
          sheet->active_cell.row=row;
          sheet->active_cell.col=column;
          sheet->drag_range=sheet->range;
          sheet->state=GTK_SHEET_RANGE_SELECTED;
          gtk_sheet_select_range(sheet, &sheet->drag_range);
        }
        sheet->x_drag=x;
        sheet->y_drag=y;
        if(row > sheet->range.rowi) row--;
        if(column > sheet->range.coli) column--;
        sheet->drag_cell.row = row;
        sheet->drag_cell.col = column;
        sheet->drag_range=sheet->range;
        draw_xor_rectangle(sheet, sheet->drag_range);
        GTK_SHEET_SET_FLAGS(sheet, GTK_SHEET_IN_RESIZE);
     }
     else if(sheet->cursor_drag->type==GDK_TOP_LEFT_ARROW &&
            !GTK_SHEET_IN_SELECTION(sheet) && !GTK_SHEET_IN_DRAG(sheet)) {
            if(sheet->state==GTK_STATE_NORMAL) {
              row=sheet->active_cell.row;
              column=sheet->active_cell.col;
              if(!gtk_sheet_deactivate_cell(sheet)) return FALSE;
              sheet->active_cell.row=row;
              sheet->active_cell.col=column;
              sheet->drag_range=sheet->range;
              sheet->state=GTK_SHEET_RANGE_SELECTED;
              gtk_sheet_select_range(sheet, &sheet->drag_range);
            }
            sheet->x_drag=x;
            sheet->y_drag=y;
            if(row < sheet->range.row0) row++;
            if(row > sheet->range.rowi) row--;
            if(column < sheet->range.col0) column++;
            if(column > sheet->range.coli) column--;
            sheet->drag_cell.row=row;
            sheet->drag_cell.col=column;
            sheet->drag_range=sheet->range;
            draw_xor_rectangle(sheet, sheet->drag_range);
            GTK_SHEET_SET_FLAGS(sheet, GTK_SHEET_IN_DRAG);
          }
          else 
          {
           gtk_sheet_click_cell(sheet, row, column, &veto);
           if(veto) GTK_SHEET_SET_FLAGS(sheet, GTK_SHEET_IN_SELECTION);
          }

    }

    if(event->window == sheet->column_title_window){
     gtk_widget_get_pointer (widget, &x, &y);
     column = COLUMN_FROM_XPIXEL(sheet, x);
     if(sheet->column[column].is_sensitive){;
       gtk_sheet_click_cell(sheet, -1, column, &veto);
       gtk_grab_add(GTK_WIDGET(sheet));
       sheet->timer=gtk_timeout_add(TIMEOUT_SCROLL, gtk_sheet_scroll, sheet); 
       GTK_WIDGET_UNSET_FLAGS(sheet->sheet_entry, GTK_HAS_FOCUS);
       GTK_WIDGET_SET_FLAGS(GTK_SHEET(sheet), GTK_HAS_FOCUS);
       gtk_widget_grab_focus(GTK_WIDGET(sheet));
       GTK_SHEET_SET_FLAGS(sheet, GTK_SHEET_IN_SELECTION);
     }
    }

    if(event->window == sheet->row_title_window){
     gtk_widget_get_pointer (widget, &x, &y);
     row = ROW_FROM_YPIXEL(sheet, y);
     if(sheet->row[row].is_sensitive){
       gtk_sheet_click_cell(sheet, row, -1, &veto);
       gtk_grab_add(GTK_WIDGET(sheet));
       sheet->timer=gtk_timeout_add(TIMEOUT_SCROLL, gtk_sheet_scroll, sheet); 
       GTK_WIDGET_UNSET_FLAGS(sheet->sheet_entry, GTK_HAS_FOCUS);
       GTK_WIDGET_SET_FLAGS(GTK_SHEET(sheet), GTK_HAS_FOCUS);
       gtk_widget_grab_focus(GTK_WIDGET(sheet));
       GTK_SHEET_SET_FLAGS(sheet, GTK_SHEET_IN_SELECTION);
     }
    }

    return TRUE;
}

static gint
gtk_sheet_scroll(gpointer data)
{
 GtkSheet *sheet;
 gint x,y,row,column;
 gint move;
  
 sheet=GTK_SHEET(data);

 GDK_THREADS_ENTER();

 gtk_widget_get_pointer (GTK_WIDGET(sheet), &x, &y);
 gtk_sheet_get_pixel_info (sheet, x, y, &row, &column);

 move=TRUE;

 if(GTK_SHEET_IN_SELECTION(sheet))
      gtk_sheet_extend_selection(sheet, row, column);

 if(GTK_SHEET_IN_DRAG(sheet) || GTK_SHEET_IN_RESIZE(sheet)){
       move=gtk_sheet_move_query(sheet, row, column);
       if(move) draw_xor_rectangle(sheet, sheet->drag_range);      
 }       

 GDK_THREADS_LEAVE();

 return TRUE;
      
}

static void
gtk_sheet_click_cell(GtkSheet *sheet, gint row, gint column, gboolean *veto)
{
      *veto = TRUE;

      if(row > sheet->maxrow || column > sheet->maxcol){
          veto = FALSE;
          return;
      }

      if(column >= 0 && row >= 0)
       if(!sheet->column[column].is_visible || !sheet->row[row].is_visible) 
         {
           veto = FALSE;
           return;
         }

      gtk_signal_emit(GTK_OBJECT(sheet), sheet_signals[TRAVERSE],
                                         sheet->active_cell.row, 
                                         sheet->active_cell.col, 
                                         &row,
                                         &column,
                                         veto);
      if(!*veto){
           if(sheet->state == GTK_STATE_NORMAL) return;

           row = sheet->active_cell.row;
           column = sheet->active_cell.col;
           gtk_sheet_activate_cell(sheet, row, column);
           return;
      }

      if(GTK_SHEET_AUTO_SCROLL(sheet))
         gtk_sheet_move_query(sheet, row, column);

      if(row == -1 && column >= 0){
	  gtk_sheet_select_column(sheet, column);
          return;
      }
      if(column == -1 && row >= 0){
 	  gtk_sheet_select_row(sheet, row);
          return;
      }

      if(row!=-1 && column !=-1){
          if(sheet->state != GTK_SHEET_NORMAL){
            sheet->state = GTK_SHEET_NORMAL;
            gtk_sheet_unselect_range(sheet, NULL);
          }
          else
            gtk_sheet_deactivate_cell(sheet);
          sheet->active_cell.row=row;
          sheet->active_cell.col=column;
	  sheet->selection_cell.row=row;
          sheet->selection_cell.col=column;
          sheet->range.row0=row;
          sheet->range.col0=column;
          sheet->range.rowi=row;
          sheet->range.coli=column;
	  sheet->state=GTK_SHEET_NORMAL;
          GTK_SHEET_SET_FLAGS(sheet, GTK_SHEET_IN_SELECTION);
	  gtk_sheet_draw_active_cell(sheet);
	  return;
      }

      if(row==-1 && column ==-1){
          sheet->state=GTK_SHEET_RANGE_SELECTED;                     
          sheet->range.row0=0;
          sheet->range.col0=0;
          sheet->range.rowi=sheet->maxrow;
          sheet->range.coli=sheet->maxcol;
	  sheet->active_cell.row=0;
	  sheet->active_cell.col=0;
	  gtk_sheet_select_range(sheet, NULL);
	  return;
      }

      gtk_sheet_activate_cell(sheet, sheet->active_cell.row,
                                     sheet->active_cell.col);
}

static gint
gtk_sheet_button_release (GtkWidget * widget,
			GdkEventButton * event)
{
  GtkSheet *sheet;
  gint x,y;
 
  sheet=GTK_SHEET(widget);

  /* release on resize windows */
  if (GTK_SHEET_IN_XDRAG (sheet)){
	  GTK_SHEET_UNSET_FLAGS (sheet, GTK_SHEET_IN_XDRAG);
          GTK_SHEET_UNSET_FLAGS (sheet, GTK_SHEET_IN_SELECTION);
	  gtk_widget_get_pointer (widget, &x, NULL);
	  gdk_pointer_ungrab (event->time);
	  draw_xor_vline (sheet);
	  
	  gtk_sheet_set_column_width (sheet, sheet->drag_cell.col, new_column_width (sheet, sheet->drag_cell.col, &x));
          sheet->old_hadjustment = -1.;
          gtk_signal_emit_by_name (GTK_OBJECT (sheet->hadjustment), "value_changed");
	  return TRUE;
  }

  if (GTK_SHEET_IN_YDRAG (sheet)){
	  GTK_SHEET_UNSET_FLAGS (sheet, GTK_SHEET_IN_YDRAG);
          GTK_SHEET_UNSET_FLAGS (sheet, GTK_SHEET_IN_SELECTION);
	  gtk_widget_get_pointer (widget, NULL, &y);
	  gdk_pointer_ungrab (event->time);
	  draw_xor_hline (sheet);
	  
	  gtk_sheet_set_row_height (sheet, sheet->drag_cell.row, new_row_height (sheet, sheet->drag_cell.row, &y));
          sheet->old_vadjustment = -1.;
          gtk_signal_emit_by_name (GTK_OBJECT (sheet->vadjustment), "value_changed");
	  return TRUE;
  }

  
  if (GTK_SHEET_IN_DRAG(sheet)){
      GtkSheetRange old_range;
      draw_xor_rectangle(sheet, sheet->drag_range);
      GTK_SHEET_UNSET_FLAGS(sheet, GTK_SHEET_IN_DRAG);
      gdk_pointer_ungrab (event->time);

      gtk_sheet_unselect_range(sheet, NULL);
      
      sheet->active_cell.row = sheet->active_cell.row +
                               (sheet->drag_range.row0 - sheet->range.row0);
      sheet->active_cell.col = sheet->active_cell.col +
                               (sheet->drag_range.col0 - sheet->range.col0);
      sheet->selection_cell.row = sheet->selection_cell.row +
                                  (sheet->drag_range.row0 - sheet->range.row0);
      sheet->selection_cell.col = sheet->selection_cell.col +
                                  (sheet->drag_range.col0 - sheet->range.col0);
      old_range=sheet->range;
      sheet->range=sheet->drag_range;
      sheet->drag_range=old_range;
      gtk_signal_emit(GTK_OBJECT(sheet),sheet_signals[MOVE_RANGE],
                      &sheet->drag_range, &sheet->range);
      gtk_sheet_select_range(sheet, &sheet->range);
  }

  if (GTK_SHEET_IN_RESIZE(sheet)){
      GtkSheetRange old_range;
      draw_xor_rectangle(sheet, sheet->drag_range);
      GTK_SHEET_UNSET_FLAGS(sheet, GTK_SHEET_IN_RESIZE);
      gdk_pointer_ungrab (event->time);

      gtk_sheet_unselect_range(sheet, NULL);
      
      sheet->active_cell.row = sheet->active_cell.row +
                               (sheet->drag_range.row0 - sheet->range.row0);
      sheet->active_cell.col = sheet->active_cell.col +
                               (sheet->drag_range.col0 - sheet->range.col0);
      if(sheet->drag_range.row0 < sheet->range.row0)
                     sheet->selection_cell.row = sheet->drag_range.row0;
      if(sheet->drag_range.rowi >= sheet->range.rowi)
                     sheet->selection_cell.row = sheet->drag_range.rowi;
      if(sheet->drag_range.col0 < sheet->range.col0)
                     sheet->selection_cell.col = sheet->drag_range.col0;
      if(sheet->drag_range.coli >= sheet->range.coli)
                     sheet->selection_cell.col = sheet->drag_range.coli;
      old_range = sheet->range;
      sheet->range = sheet->drag_range;
      sheet->drag_range = old_range;

      if(sheet->state==GTK_STATE_NORMAL) sheet->state=GTK_SHEET_RANGE_SELECTED;
      gtk_signal_emit(GTK_OBJECT(sheet),sheet_signals[RESIZE_RANGE],
                      &sheet->drag_range, &sheet->range);
      gtk_sheet_select_range(sheet, &sheet->range);
  }

  if(sheet->state == GTK_SHEET_NORMAL && GTK_SHEET_IN_SELECTION(sheet)){
      GTK_SHEET_UNSET_FLAGS(sheet, GTK_SHEET_IN_SELECTION);
      gdk_pointer_ungrab (event->time);
      gtk_sheet_activate_cell(sheet, sheet->active_cell.row, 
                                     sheet->active_cell.col);
  }

  if(GTK_SHEET_IN_SELECTION)
         gdk_pointer_ungrab (event->time);
  if(sheet->timer)
         gtk_timeout_remove(sheet->timer);
  gtk_grab_remove(GTK_WIDGET(sheet));

  GTK_SHEET_UNSET_FLAGS(sheet, GTK_SHEET_IN_SELECTION);

  return TRUE;
}

static gint
gtk_sheet_motion (GtkWidget * widget,
		  GdkEventMotion * event)
{
  GtkSheet *sheet;
  GdkModifierType mods;
  GdkCursorType new_cursor;
  gint x, y, row, column;

  g_return_val_if_fail (widget != NULL, FALSE);
  g_return_val_if_fail (GTK_IS_SHEET (widget), FALSE);
  g_return_val_if_fail (event != NULL, FALSE);


  sheet = GTK_SHEET (widget);


  /* selections on the sheet */
  x = event->x;
  y = event->y;

  if(event->window == sheet->column_title_window && !GTK_SHEET_COLUMN_FROZEN(sheet)){
    gtk_widget_get_pointer(widget, &x, &y);
    if(!GTK_SHEET_IN_SELECTION(sheet) && POSSIBLE_XDRAG(sheet, x, &column)){
      new_cursor=GDK_SB_H_DOUBLE_ARROW;
      if(new_cursor != sheet->cursor_drag->type){
        gdk_cursor_destroy(sheet->cursor_drag);
        sheet->cursor_drag=gdk_cursor_new(GDK_SB_H_DOUBLE_ARROW);
        gdk_window_set_cursor(sheet->column_title_window,sheet->cursor_drag);
      }
    }else{
      new_cursor=GDK_TOP_LEFT_ARROW;
      if(!GTK_SHEET_IN_XDRAG(sheet) && new_cursor != sheet->cursor_drag->type){
        gdk_cursor_destroy(sheet->cursor_drag);
        sheet->cursor_drag=gdk_cursor_new(GDK_TOP_LEFT_ARROW);
        gdk_window_set_cursor(sheet->column_title_window,sheet->cursor_drag);
      }
    }
  }      

  if(event->window == sheet->row_title_window && !GTK_SHEET_ROW_FROZEN(sheet)){
    gtk_widget_get_pointer(widget, &x, &y);
    if(!GTK_SHEET_IN_SELECTION(sheet) && POSSIBLE_YDRAG(sheet,y, &column)){
      new_cursor=GDK_SB_V_DOUBLE_ARROW;
      if(new_cursor != sheet->cursor_drag->type){
        gdk_cursor_destroy(sheet->cursor_drag);
        sheet->cursor_drag=gdk_cursor_new(GDK_SB_V_DOUBLE_ARROW);
        gdk_window_set_cursor(sheet->row_title_window,sheet->cursor_drag);
      }
    }else{
      new_cursor=GDK_TOP_LEFT_ARROW;
      if(!GTK_SHEET_IN_YDRAG(sheet) && new_cursor != sheet->cursor_drag->type){
        gdk_cursor_destroy(sheet->cursor_drag);
        sheet->cursor_drag=gdk_cursor_new(GDK_TOP_LEFT_ARROW);
        gdk_window_set_cursor(sheet->row_title_window,sheet->cursor_drag);
      }
    }
  }      

  new_cursor=GDK_PLUS;
  if(!POSSIBLE_DRAG(sheet,x,y,&row,&column) && !GTK_SHEET_IN_DRAG(sheet) &&
     !POSSIBLE_RESIZE(sheet,x,y,&row,&column) && !GTK_SHEET_IN_RESIZE(sheet) &&
     event->window == sheet->sheet_window && 
     new_cursor != sheet->cursor_drag->type){
         gdk_cursor_destroy(sheet->cursor_drag);
         sheet->cursor_drag=gdk_cursor_new(GDK_PLUS);
         gdk_window_set_cursor(sheet->sheet_window,sheet->cursor_drag);
  }

  new_cursor=GDK_TOP_LEFT_ARROW;
  if(!(POSSIBLE_RESIZE(sheet,x,y,&row,&column) || GTK_SHEET_IN_RESIZE(sheet)) &&
     (POSSIBLE_DRAG(sheet, x,y,&row,&column) || GTK_SHEET_IN_DRAG(sheet)) && 
     event->window == sheet->sheet_window && 
     new_cursor != sheet->cursor_drag->type){
         gdk_cursor_destroy(sheet->cursor_drag);
         sheet->cursor_drag=gdk_cursor_new(GDK_TOP_LEFT_ARROW);
         gdk_window_set_cursor(sheet->sheet_window,sheet->cursor_drag);
  }

  new_cursor=GDK_SIZING;
  if(!GTK_SHEET_IN_DRAG(sheet) &&
     (POSSIBLE_RESIZE(sheet,x,y,&row,&column) || GTK_SHEET_IN_RESIZE(sheet)) &&
     event->window == sheet->sheet_window && 
     new_cursor != sheet->cursor_drag->type){
         gdk_cursor_destroy(sheet->cursor_drag);
         sheet->cursor_drag=gdk_cursor_new(GDK_SIZING);
         gdk_window_set_cursor(sheet->sheet_window,sheet->cursor_drag);
  }

  gdk_window_get_pointer (widget->window, &x, &y, &mods);
  if(!(mods & GDK_BUTTON1_MASK)) return FALSE;

  if (GTK_SHEET_IN_XDRAG (sheet)){
	if (event->is_hint || event->window != widget->window)
	    gtk_widget_get_pointer (widget, &x, NULL);
	  else
	    x = event->x;

	  new_column_width (sheet, sheet->drag_cell.col, &x);
	  if (x != sheet->x_drag)
	    {
	      draw_xor_vline (sheet);
	      sheet->x_drag = x;
	      draw_xor_vline (sheet);
	    }
          return TRUE;
  }

  if (GTK_SHEET_IN_YDRAG (sheet)){
	  if (event->is_hint || event->window != widget->window)
	    gtk_widget_get_pointer (widget, NULL, &y);
	  else
	    y = event->y;

	  new_row_height (sheet, sheet->drag_cell.row, &y);
	  if (y != sheet->y_drag)
	    {
	      draw_xor_hline (sheet);
	      sheet->y_drag = y;
	      draw_xor_hline (sheet);
	    }
          return TRUE;
  }

  if (GTK_SHEET_IN_DRAG(sheet)){
       GtkSheetRange aux;
       column=COLUMN_FROM_XPIXEL(sheet,x)-sheet->drag_cell.col;
       row=ROW_FROM_YPIXEL(sheet,y)-sheet->drag_cell.row;
       if(sheet->state==GTK_SHEET_COLUMN_SELECTED) row=0;
       if(sheet->state==GTK_SHEET_ROW_SELECTED) column=0;
       sheet->x_drag=x;
       sheet->y_drag=y;
       aux=sheet->range;
       if(aux.row0+row >= 0 && aux.rowi+row <= sheet->maxrow &&
          aux.col0+column >= 0 && aux.coli+column <= sheet->maxcol){
             aux=sheet->drag_range;
             sheet->drag_range.row0=sheet->range.row0+row;
             sheet->drag_range.col0=sheet->range.col0+column;
             sheet->drag_range.rowi=sheet->range.rowi+row;
             sheet->drag_range.coli=sheet->range.coli+column;
             if(aux.row0 != sheet->drag_range.row0 ||
                aux.col0 != sheet->drag_range.col0){
                draw_xor_rectangle (sheet, aux);
                draw_xor_rectangle (sheet, sheet->drag_range);
             }
       }
       return TRUE;
  }

  if (GTK_SHEET_IN_RESIZE(sheet)){
       GtkSheetRange aux;
       gint v_h;
       v_h=1;
       if(abs(x-COLUMN_LEFT_XPIXEL(sheet,sheet->drag_cell.col)) >
          abs(y-ROW_TOP_YPIXEL(sheet,sheet->drag_cell.row))) v_h=2;

       column=COLUMN_FROM_XPIXEL(sheet,x)-sheet->drag_cell.col;
       row=ROW_FROM_YPIXEL(sheet,y)-sheet->drag_cell.row;
       if(sheet->state==GTK_SHEET_COLUMN_SELECTED) row=0;
       if(sheet->state==GTK_SHEET_ROW_SELECTED) column=0;
       sheet->x_drag=x;
       sheet->y_drag=y;
       aux=sheet->range;

       if(row < sheet->range.row0 - sheet->range.rowi - 1) 
          row=row+(sheet->range.rowi-sheet->range.row0 + 1);
       else if(row<0) row=0;

       if(column < sheet->range.col0 - sheet->range.coli - 1)
          column=column+(sheet->range.coli-sheet->range.col0 + 1);
       else if(column<0) column=0;

       if(v_h==1) 
           column=0;
       else
           row=0;

       if(aux.row0+row >= 0 && aux.rowi+row <= sheet->maxrow &&
          aux.col0+column >= 0 && aux.coli+column <= sheet->maxcol){

             aux=sheet->drag_range;
             sheet->drag_range=sheet->range;

             if(row<0) sheet->drag_range.row0=sheet->range.row0+row;
             if(row>0) sheet->drag_range.rowi=sheet->range.rowi+row;
             if(column<0) sheet->drag_range.col0=sheet->range.col0+column;
             if(column>0) sheet->drag_range.coli=sheet->range.coli+column;
             
             if(aux.row0 != sheet->drag_range.row0 ||
                aux.rowi != sheet->drag_range.rowi ||
                aux.col0 != sheet->drag_range.col0 ||
                aux.coli != sheet->drag_range.coli){
                     draw_xor_rectangle (sheet, aux);
                     draw_xor_rectangle (sheet, sheet->drag_range);
             }
       }
       return TRUE;
  }

  

  gtk_sheet_get_pixel_info (sheet, x, y, &row, &column);

  if(sheet->state==GTK_SHEET_NORMAL && row==sheet->active_cell.row &&
     column==sheet->active_cell.col) return TRUE;

  if(GTK_SHEET_IN_SELECTION(sheet) && mods&GDK_BUTTON1_MASK)
                          gtk_sheet_extend_selection(sheet, row, column);

  return TRUE;
}

static gint
gtk_sheet_move_query(GtkSheet *sheet, gint row, gint column)
{
  gint row_move, column_move;
  gfloat row_align, col_align;
  gint height, width;
  gint new_row = row;
  gint new_col = column;

  row_move=FALSE;
  column_move=FALSE;
  row_align=-1.;
  col_align=-1.;

  height = sheet->sheet_window_height;
  width = sheet->sheet_window_width;

  if(row>=MAX_VISIBLE_ROW(sheet) && sheet->state!=GTK_SHEET_COLUMN_SELECTED) {
          row_align = 1.;
	  new_row = MIN(sheet->maxrow, row + 1);
          row_move = TRUE;
          if(MAX_VISIBLE_ROW(sheet) == sheet->maxrow &&
             ROW_TOP_YPIXEL(sheet, sheet->maxrow) + 
             sheet->row[sheet->maxrow].height < height){
                 row_move = FALSE;
		 row_align = -1.;
          }
  }
  if(row<MIN_VISIBLE_ROW(sheet) && sheet->state!=GTK_SHEET_COLUMN_SELECTED) {
          row_align= 0.;
          row_move = TRUE;
  }
  if(column>=MAX_VISIBLE_COLUMN(sheet) && sheet->state!=GTK_SHEET_ROW_SELECTED) {
          col_align = 1.;
          new_col = MIN(sheet->maxcol, column + 1);
          column_move = TRUE;
          if(MAX_VISIBLE_COLUMN(sheet) == sheet->maxcol &&
             COLUMN_LEFT_XPIXEL(sheet, sheet->maxcol) + 
             sheet->column[sheet->maxcol].width < width){
                 column_move = FALSE;
		 col_align = -1.;
          }
  } 
  if(column<MIN_VISIBLE_COLUMN(sheet) && sheet->state!=GTK_SHEET_ROW_SELECTED) {
	  col_align = 0.;
          column_move = TRUE;
  }

  if(row_move || column_move){
        gtk_sheet_moveto(sheet, new_row, new_col, row_align, col_align);
  }

  return(row_move || column_move);
}

static void
gtk_sheet_extend_selection(GtkSheet *sheet, gint row, gint column)
{
   GtkSheetRange range;
   gint state;
   gint r,c;

   if(row == sheet->selection_cell.row && column == sheet->selection_cell.col)
        return;

   if(sheet->selection_mode == GTK_SELECTION_SINGLE) return;

   gtk_sheet_move_query(sheet, row, column);
   gtk_widget_grab_focus(GTK_WIDGET(sheet));

   if(GTK_SHEET_IN_DRAG(sheet)) return;

   state=sheet->state;

   switch(sheet->state){
    case GTK_SHEET_ROW_SELECTED:
	 column = sheet->maxcol;
         break;
    case GTK_SHEET_COLUMN_SELECTED:
	 row = sheet->maxrow;
         break; 
    case GTK_SHEET_NORMAL:
	 sheet->state=GTK_SHEET_RANGE_SELECTED;
         r=sheet->active_cell.row;
         c=sheet->active_cell.col;
         sheet->range.col0=c;
         sheet->range.row0=r;
         sheet->range.coli=c;
         sheet->range.rowi=r;
         gdk_draw_pixmap(sheet->sheet_window,
                   GTK_WIDGET(sheet)->style->fg_gc[GTK_STATE_NORMAL],
                   sheet->pixmap,
                   COLUMN_LEFT_XPIXEL(sheet,c)-1,
                   ROW_TOP_YPIXEL(sheet,r)-1,
                   COLUMN_LEFT_XPIXEL(sheet,c)-1,
                   ROW_TOP_YPIXEL(sheet,r)-1,
                   sheet->column[c].width+4,
                   sheet->row[r].height+4);   
         gtk_sheet_range_draw_selection(sheet, sheet->range);
    case GTK_SHEET_RANGE_SELECTED:
         sheet->state=GTK_SHEET_RANGE_SELECTED;
   }

   sheet->selection_cell.row = row;
   sheet->selection_cell.col = column;

   range.col0=MIN(column,sheet->active_cell.col);
   range.coli=MAX(column,sheet->active_cell.col);
   range.row0=MIN(row,sheet->active_cell.row);
   range.rowi=MAX(row,sheet->active_cell.row);

   if(range.row0 != sheet->range.row0 || range.rowi != sheet->range.rowi ||
      range.col0 != sheet->range.col0 || range.coli != sheet->range.coli ||
      state==GTK_SHEET_NORMAL)
               gtk_sheet_real_select_range(sheet, &range);

}

static gint
gtk_sheet_entry_key_press(GtkWidget *widget,
		          GdkEventKey *key)
{
  gboolean focus;
  gtk_signal_emit_by_name(GTK_OBJECT(widget), "key_press_event", key, &focus);
  return focus;
}

static gint
gtk_sheet_key_press(GtkWidget *widget,
		    GdkEventKey *key)
{
  GtkSheet *sheet;
  gint row, col;
  gint state;
  gboolean extend_selection = FALSE;
  gboolean force_move = FALSE;
  gboolean in_selection = FALSE;
  gboolean veto = TRUE;
  gint scroll = 1;

  sheet = GTK_SHEET(widget);

  if(key->state & GDK_CONTROL_MASK || key->keyval==GDK_Control_L ||
     key->keyval==GDK_Control_R) return FALSE;

/*
  {
    if(key->keyval=='c' || key->keyval == 'C' && sheet->state != GTK_STATE_NORMAL)
            gtk_sheet_clip_range(sheet, sheet->range);
    if(key->keyval=='x' || key->keyval == 'X')
            gtk_sheet_unclip_range(sheet);    
    return FALSE;
  }
*/

  extend_selection = (key->state & GDK_SHIFT_MASK) || key->keyval==GDK_Shift_L 
|| key->keyval==GDK_Shift_R;

  state=sheet->state;
  in_selection = GTK_SHEET_IN_SELECTION(sheet);
  GTK_SHEET_UNSET_FLAGS(sheet, GTK_SHEET_IN_SELECTION);

  switch(key->keyval){
    case GDK_Return: case GDK_KP_Enter:
      if(sheet->state == GTK_SHEET_NORMAL && 
         !GTK_SHEET_IN_SELECTION(sheet))
         gtk_signal_emit_stop_by_name(GTK_OBJECT(gtk_sheet_get_entry(sheet)), 
                                     "key_press_event");
      row = sheet->active_cell.row;
      col = sheet->active_cell.col;
      if(sheet->state == GTK_SHEET_COLUMN_SELECTED)
           row = MIN_VISIBLE_ROW(sheet)-1;
      if(sheet->state == GTK_SHEET_ROW_SELECTED)
           col = MIN_VISIBLE_COLUMN(sheet);
      if(row < sheet->maxrow){
           row = row + scroll;
           while(!sheet->row[row].is_visible && row<sheet->maxrow) row++;
      }
      gtk_sheet_click_cell(sheet, row, col, &veto);
      extend_selection = FALSE;
      break;
   case GDK_ISO_Left_Tab:
      row = sheet->active_cell.row;
      col = sheet->active_cell.col;
      if(sheet->state == GTK_SHEET_ROW_SELECTED) 
           col = MIN_VISIBLE_COLUMN(sheet)-1;
      if(sheet->state == GTK_SHEET_COLUMN_SELECTED) 
           row = MIN_VISIBLE_ROW(sheet);
      if(col > 0){
           col = col - scroll; 
           while(!sheet->column[col].is_visible && col>0) col--;
	   col=MAX(0, col);
      }       
      gtk_sheet_click_cell(sheet, row, col, &veto);
      extend_selection = FALSE;
      break;
   case GDK_Tab:
      row = sheet->active_cell.row;
      col = sheet->active_cell.col;
      if(sheet->state == GTK_SHEET_ROW_SELECTED) 
           col = MIN_VISIBLE_COLUMN(sheet)-1;
      if(sheet->state == GTK_SHEET_COLUMN_SELECTED) 
           row = MIN_VISIBLE_ROW(sheet);
      if(col < sheet->maxcol){
           col = col + scroll; 
           while(!sheet->column[col].is_visible && col<sheet->maxcol) col++;
      }       
      gtk_sheet_click_cell(sheet, row, col, &veto);
      extend_selection = FALSE;
      break;
/*    case GDK_BackSpace:
      if(sheet->active_cell.row >= 0 && sheet->active_cell.col >= 0){
       if(sheet->active_cell.col > 0){
            col = sheet->active_cell.col - scroll; 
	    row = sheet->active_cell.row;
            while(!sheet->column[col].is_visible && col > 0) col--;
       }       
      }
      gtk_sheet_click_cell(sheet, row, col, &veto);
      extend_selection = FALSE;
      break;
*/
    case GDK_Page_Up:
      scroll=MAX_VISIBLE_ROW(sheet)-MIN_VISIBLE_ROW(sheet)+1;
    case GDK_Up:
      if(extend_selection){
        if(state==GTK_STATE_NORMAL){
           row=sheet->active_cell.row;
           col=sheet->active_cell.col;
           gtk_sheet_click_cell(sheet, row, col, &veto);
           if(!veto) break;
        }
        if(sheet->selection_cell.row > 0){
          row = sheet->selection_cell.row - scroll;
          while(!sheet->row[row].is_visible && row > 0) row--;
          row = MAX(0, row);
          gtk_sheet_extend_selection(sheet, row, sheet->selection_cell.col);
        }
        return TRUE;
      }
      col = sheet->active_cell.col;
      row = sheet->active_cell.row;
      if(state==GTK_SHEET_COLUMN_SELECTED) 
             row = MIN_VISIBLE_ROW(sheet);
      if(state==GTK_SHEET_ROW_SELECTED) 
             col = MIN_VISIBLE_COLUMN(sheet);
      row = row - scroll;
      while(!sheet->row[row].is_visible && row > 0) row--;
      row = MAX(0,row);
      gtk_sheet_click_cell(sheet, row, col, &veto);
      extend_selection = FALSE;
      break;
    case GDK_Page_Down:
      scroll=MAX_VISIBLE_ROW(sheet)-MIN_VISIBLE_ROW(sheet)+1;
    case GDK_Down:
      if(extend_selection){
        if(state==GTK_STATE_NORMAL){
           row=sheet->active_cell.row;
           col=sheet->active_cell.col;
           gtk_sheet_click_cell(sheet, row, col, &veto);
           if(!veto) break;
        }
        if(sheet->selection_cell.row < sheet->maxrow){
          row = sheet->selection_cell.row + scroll;
          while(!sheet->row[row].is_visible && row < sheet->maxrow) row++;
          row = MIN(sheet->maxrow, row);
          gtk_sheet_extend_selection(sheet, row, sheet->selection_cell.col);
        }
        return TRUE;
      }
      col = sheet->active_cell.col;
      row = sheet->active_cell.row;
      if(sheet->active_cell.row < sheet->maxrow){
	   if(state==GTK_SHEET_COLUMN_SELECTED) 
                row = MIN_VISIBLE_ROW(sheet)-1;
	   if(state==GTK_SHEET_ROW_SELECTED) 
                col = MIN_VISIBLE_COLUMN(sheet);
	   row = row + scroll;
           while(!sheet->row[row].is_visible && row < sheet->maxrow) row++;
           row = MIN(sheet->maxrow, row);
      }
      gtk_sheet_click_cell(sheet, row, col, &veto);
      extend_selection = FALSE;
      break;
    case GDK_Right:
      if(extend_selection){
        if(state==GTK_STATE_NORMAL){
           row=sheet->active_cell.row;
           col=sheet->active_cell.col;
           gtk_sheet_click_cell(sheet, row, col, &veto);
           if(!veto) break;
        }
        if(sheet->selection_cell.col < sheet->maxcol){
          col = sheet->selection_cell.col + 1;
          while(!sheet->column[col].is_visible && col < sheet->maxcol) col++;
          gtk_sheet_extend_selection(sheet, sheet->selection_cell.row, col);
        }
        return TRUE;
      }
      col = sheet->active_cell.col;
      row = sheet->active_cell.row;
      if(sheet->active_cell.col < sheet->maxcol){
           col ++;
	   if(state==GTK_SHEET_ROW_SELECTED) 
                col = MIN_VISIBLE_COLUMN(sheet)-1;
	   if(state==GTK_SHEET_COLUMN_SELECTED) 
                row = MIN_VISIBLE_ROW(sheet);
           while(!sheet->column[col].is_visible && col < sheet->maxcol) col++;
           if(strlen(gtk_entry_get_text(GTK_ENTRY(gtk_sheet_get_entry(sheet)))) == 0 
              || force_move) {
	        gtk_sheet_click_cell(sheet, row, col, &veto);
           }
           else
              return FALSE;
      }
      extend_selection = FALSE;
      break;
    case GDK_Left:
      if(extend_selection){
        if(state==GTK_STATE_NORMAL){
           row=sheet->active_cell.row;
           col=sheet->active_cell.col;
           gtk_sheet_click_cell(sheet, row, col, &veto);
           if(!veto) break;
        }
        if(sheet->selection_cell.col > 0){
          col = sheet->selection_cell.col - 1;
          while(!sheet->column[col].is_visible && col > 0) col--;          
          gtk_sheet_extend_selection(sheet, sheet->selection_cell.row, col);
        }
	return TRUE;
      }
      col = sheet->active_cell.col - 1;
      row = sheet->active_cell.row;
      if(state==GTK_SHEET_ROW_SELECTED) 
                col = MIN_VISIBLE_COLUMN(sheet)-1;
      if(state==GTK_SHEET_COLUMN_SELECTED) 
                row = MIN_VISIBLE_ROW(sheet);
      while(!sheet->column[col].is_visible && col > 0) col--;
      col = MAX(0, col);

      if(strlen(gtk_entry_get_text(GTK_ENTRY(gtk_sheet_get_entry(sheet)))) == 0
         || force_move){
                gtk_sheet_click_cell(sheet, row, col, &veto);
      }
      else
         return FALSE;
      extend_selection = FALSE;
      break;
    case GDK_Home:
      row=0;
      while(!sheet->row[row].is_visible && row < sheet->maxrow) row++;
      gtk_sheet_click_cell(sheet, row, sheet->active_cell.col, &veto);
      extend_selection = FALSE;
      break;
    case GDK_End:
      row=sheet->maxrow;
      while(!sheet->row[row].is_visible && row > 0) row--;
      gtk_sheet_click_cell(sheet, row, sheet->active_cell.col, &veto);
      extend_selection = FALSE;
      break;
    default:
      if(in_selection) GTK_SHEET_SET_FLAGS(sheet, GTK_SHEET_IN_SELECTION);
      if(extend_selection) return TRUE;
      if(state == GTK_SHEET_ROW_SELECTED) 
        sheet->active_cell.col=MIN_VISIBLE_COLUMN(sheet);
      if(state == GTK_SHEET_COLUMN_SELECTED)
        sheet->active_cell.row=MIN_VISIBLE_ROW(sheet);
      return FALSE;
  }

  if(extend_selection) return TRUE;

  gtk_sheet_activate_cell(sheet, sheet->active_cell.row,
                                 sheet->active_cell.col);

  return TRUE;
} 

static void
gtk_sheet_size_request (GtkWidget * widget,
			GtkRequisition * requisition)
{
  GtkSheet *sheet;
  GList *children;
  GtkSheetChild *child;
  GtkRequisition child_requisition;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (GTK_IS_SHEET (widget));
  g_return_if_fail (requisition != NULL);

  sheet = GTK_SHEET (widget);

  requisition->width = 3*DEFAULT_COLUMN_WIDTH;
  requisition->height = 3*DEFAULT_ROW_HEIGHT(widget);

  /* compute the size of the column title area */
  if(GTK_SHEET_COL_TITLES_VISIBLE(sheet)) 
     requisition->height += sheet->column_title_area.height;

  /* compute the size of the row title area */
  if(GTK_SHEET_ROW_TITLES_VISIBLE(sheet)) 
     requisition->width += sheet->row_title_area.width;

  sheet->view.row0=ROW_FROM_YPIXEL(sheet, sheet->column_title_area.height+1);
  sheet->view.rowi=ROW_FROM_YPIXEL(sheet, sheet->sheet_window_height-1);
  sheet->view.col0=COLUMN_FROM_XPIXEL(sheet, sheet->row_title_area.width+1);
  sheet->view.coli=COLUMN_FROM_XPIXEL(sheet, sheet->sheet_window_width);

  if(!GTK_SHEET_COL_TITLES_VISIBLE(sheet)) 
     sheet->view.row0=ROW_FROM_YPIXEL(sheet, 1);

  if(!GTK_SHEET_ROW_TITLES_VISIBLE(sheet)) 
     sheet->view.col0=COLUMN_FROM_XPIXEL(sheet, 1);

  children = sheet->children;
  while (children)
  {
    child = children->data;
    children = children->next;

    gtk_widget_size_request(child->widget, &child_requisition);
  }
}

 
static void
gtk_sheet_size_allocate (GtkWidget * widget,
			 GtkAllocation * allocation)
{
  GtkSheet *sheet;
  GtkAllocation sheet_allocation;
  gint border_width;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (GTK_IS_SHEET (widget));
  g_return_if_fail (allocation != NULL);

  sheet = GTK_SHEET (widget);
  widget->allocation = *allocation;
  border_width = GTK_CONTAINER(widget)->border_width;

  if (GTK_WIDGET_REALIZED (widget))
    gdk_window_move_resize (widget->window,
	      	  	    allocation->x + border_width,
	                    allocation->y + border_width,
                            allocation->width - 2*border_width,
	                    allocation->height - 2*border_width);

  /* use internal allocation structure for all the math
   * because it's easier than always subtracting the container
   * border width */
  sheet->internal_allocation.x = 0;
  sheet->internal_allocation.y = 0;
  sheet->internal_allocation.width = allocation->width - 2*border_width;
  sheet->internal_allocation.height = allocation->height - 2*border_width;
	
  sheet_allocation.x = 0;
  sheet_allocation.y = 0;
  sheet_allocation.width = allocation->width - 2*border_width;
  sheet_allocation.height = allocation->height - 2*border_width;

  sheet->sheet_window_width = sheet_allocation.width;
  sheet->sheet_window_height = sheet_allocation.height;

  if (GTK_WIDGET_REALIZED (widget))
    gdk_window_move_resize (sheet->sheet_window,
			    sheet_allocation.x,
			    sheet_allocation.y,
			    sheet_allocation.width,
			    sheet_allocation.height);

    /* position the window which holds the column title buttons */
  sheet->column_title_area.x = 0;
  sheet->column_title_area.y = 0;
  if(GTK_SHEET_ROW_TITLES_VISIBLE(sheet))
       sheet->column_title_area.x = sheet->row_title_area.width;
  sheet->column_title_area.width = sheet_allocation.width - 
                                     sheet->column_title_area.x;
  if(GTK_WIDGET_REALIZED(widget) && GTK_SHEET_COL_TITLES_VISIBLE(sheet))
      gdk_window_move_resize (sheet->column_title_window,
			      sheet->column_title_area.x,
			      sheet->column_title_area.y,
			      sheet->column_title_area.width,
			      sheet->column_title_area.height);

  sheet->sheet_window_width = sheet_allocation.width;
  sheet->sheet_window_height = sheet_allocation.height;

  /* column button allocation */
  size_allocate_column_title_buttons (sheet);

  /* position the window which holds the row title buttons */
  sheet->row_title_area.x = 0;
  sheet->row_title_area.y = 0;
  if(GTK_SHEET_COL_TITLES_VISIBLE(sheet))
       sheet->row_title_area.y = sheet->column_title_area.height;
  sheet->row_title_area.height = sheet_allocation.height -
                                   sheet->row_title_area.y;

  if(GTK_WIDGET_REALIZED(widget) && GTK_SHEET_ROW_TITLES_VISIBLE(sheet))
      gdk_window_move_resize (sheet->row_title_window,
			      sheet->row_title_area.x,
			      sheet->row_title_area.y,
			      sheet->row_title_area.width,
			      sheet->row_title_area.height);


  /* row button allocation */
  size_allocate_row_title_buttons (sheet);

  sheet->view.row0=ROW_FROM_YPIXEL(sheet, sheet->column_title_area.height+1);
  sheet->view.rowi=ROW_FROM_YPIXEL(sheet, sheet->sheet_window_height-1);
  sheet->view.col0=COLUMN_FROM_XPIXEL(sheet, sheet->row_title_area.width+1);
  sheet->view.coli=COLUMN_FROM_XPIXEL(sheet, sheet->sheet_window_width);

  if(!GTK_SHEET_COL_TITLES_VISIBLE(sheet))
       sheet->view.row0=ROW_FROM_YPIXEL(sheet, 1);
      
  if(!GTK_SHEET_ROW_TITLES_VISIBLE(sheet))
       sheet->view.col0=COLUMN_FROM_XPIXEL(sheet, 1);

  size_allocate_column_title_buttons(sheet);
  size_allocate_row_title_buttons(sheet);

  /* re-scale backing pixmap */
  gtk_sheet_make_backing_pixmap(sheet, 0, 0); 
  gtk_sheet_position_children(sheet);

  /* set the scrollbars adjustments */
  adjust_scrollbars (sheet);
}

static void
size_allocate_column_title_buttons (GtkSheet * sheet)
{
  gint i;
  gint x,width;

  if (!GTK_SHEET_COL_TITLES_VISIBLE(sheet)) return;
  if (!GTK_WIDGET_REALIZED (sheet))
    return;

  width = sheet->sheet_window_width;
  x = 0;

  if(GTK_SHEET_ROW_TITLES_VISIBLE(sheet))
    {
      width -= sheet->row_title_area.width;
      x = sheet->row_title_area.width;
    }

  if(sheet->column_title_area.width != width || sheet->column_title_area.x != x)
  {
     sheet->column_title_area.width = width;
     sheet->column_title_area.x = x;
     gdk_window_move_resize (sheet->column_title_window,
  		  	     sheet->column_title_area.x,
			     sheet->column_title_area.y,
			     sheet->column_title_area.width,
			     sheet->column_title_area.height);
  }


  if(MAX_VISIBLE_COLUMN(sheet) == sheet->maxcol)
     gdk_window_clear_area (sheet->column_title_window,
	   		    0,0,
			    sheet->column_title_area.width, 
                            sheet->column_title_area.height);

  if(!GTK_WIDGET_DRAWABLE(sheet)) return;

  for (i = MIN_VISIBLE_COLUMN(sheet); i <= MAX_VISIBLE_COLUMN(sheet); i++)
      gtk_sheet_button_draw(sheet,-1,i);
}
	
static void
size_allocate_row_title_buttons (GtkSheet * sheet)
{
  gint i;
  gint y, height;

  if (!GTK_SHEET_ROW_TITLES_VISIBLE(sheet)) return;
  if (!GTK_WIDGET_REALIZED (sheet))
    return;

  height = sheet->sheet_window_height;
  y = 0;

  if(GTK_SHEET_COL_TITLES_VISIBLE(sheet))
    {
      height -= sheet->column_title_area.height;
      y = sheet->column_title_area.height;
    }
    
  if(sheet->row_title_area.height != height || sheet->row_title_area.y != y){
     sheet->row_title_area.y = y;
     sheet->row_title_area.height = height;
     gdk_window_move_resize (sheet->row_title_window,
  		  	     sheet->row_title_area.x,
			     sheet->row_title_area.y,
			     sheet->row_title_area.width,
			     sheet->row_title_area.height);
  }
  if(MAX_VISIBLE_ROW(sheet) == sheet->maxrow)
    gdk_window_clear_area (sheet->row_title_window,
			   0,0,
			   sheet->row_title_area.width, 
                           sheet->row_title_area.height);

  if(!GTK_WIDGET_DRAWABLE(sheet)) return;

  for(i = MIN_VISIBLE_ROW(sheet); i <= MAX_VISIBLE_ROW(sheet); i++)
      gtk_sheet_button_draw(sheet,i,-1);
}
	  
static void
gtk_sheet_recalc_top_ypixels(GtkSheet *sheet, gint row)
{
  gint i, cy;

  cy = sheet->column_title_area.height;
  if(!GTK_SHEET_COL_TITLES_VISIBLE(sheet)) cy = 0;
  for(i=0; i<=sheet->maxrow; i++){
      sheet->row[i].top_ypixel=cy;
      if(sheet->row[i].is_visible) cy+=sheet->row[i].height;
  }
}

static void
gtk_sheet_recalc_left_xpixels(GtkSheet *sheet, gint column)
{
  gint i, cx;

  cx = sheet->row_title_area.width;
  if(!GTK_SHEET_ROW_TITLES_VISIBLE(sheet)) cx = 0;
  for(i=0; i<=sheet->maxcol; i++){
      sheet->column[i].left_xpixel=cx;
      if(sheet->column[i].is_visible) cx+=sheet->column[i].width;
  }

}



static void
gtk_sheet_size_allocate_entry(GtkSheet *sheet)
{
 GtkAllocation shentry_allocation;
 GtkSheetCellAttr attributes;
 GtkEntry *sheet_entry;
 GtkStyle *style, *previous_style;
 gint row, col;
 gint size, max_size, text_size, column_width;

 if(!GTK_WIDGET_REALIZED(GTK_WIDGET(sheet))) return;
 if(!GTK_WIDGET_MAPPED(GTK_WIDGET(sheet))) return;

 sheet_entry = GTK_ENTRY(gtk_sheet_get_entry(sheet));

 gtk_sheet_get_attributes(sheet, sheet->active_cell.row, sheet->active_cell.col, &attributes); 

 if(GTK_WIDGET_REALIZED(sheet->sheet_entry)){

  if(!GTK_WIDGET(sheet_entry)->style) 
        gtk_widget_ensure_style(GTK_WIDGET(sheet_entry));

  previous_style = GTK_WIDGET(sheet_entry)->style;

  style = gtk_style_copy(previous_style);
  style->bg[GTK_STATE_NORMAL] = attributes.background;

  if(style->font != attributes.font){
   gdk_font_unref(style->font);
   style->font = attributes.font;
   gdk_font_ref(style->font);
  }

/* I'm emulating gtk_widget_size_request to avoid the redraw */
  GTK_WIDGET(sheet_entry)->style = style;
  gtk_widget_size_request(sheet->sheet_entry, NULL);
  GTK_WIDGET(sheet_entry)->style = previous_style;

  if(style != previous_style)
                    gtk_widget_set_style(GTK_WIDGET(sheet_entry), style);

  if(GTK_IS_IENTRY(sheet_entry)){
    gdk_gc_set_foreground(GTK_IENTRY(sheet_entry)->fg_gc, 
                          &attributes.foreground);
    gdk_gc_set_foreground(GTK_IENTRY(sheet_entry)->bg_gc,  
                          &attributes.background);
  }

  gdk_window_set_background(GTK_ENTRY(sheet_entry)->text_area, 
                            &attributes.background);

  if(sheet->sheet_entry_window)
        gdk_window_set_background(sheet->sheet_entry_window,  
                                  &attributes.background);
  else
        gdk_window_set_background(GTK_WIDGET(sheet_entry)->window,  
                                  &attributes.background);
 }

 if(GTK_IS_IENTRY(sheet_entry))
    max_size = GTK_IENTRY(sheet_entry)->text_max_size;
 else
    max_size = 0;

 text_size=GTK_ENTRY(sheet_entry)->text == NULL ? 0 :
           gdk_string_width(attributes.font,
           gtk_entry_get_text(GTK_ENTRY(sheet_entry)));
 column_width=sheet->column[sheet->active_cell.col].width;

 size=MIN(text_size, max_size);
 size=MAX(size,column_width);

 row=sheet->active_cell.row;
 col=sheet->active_cell.col;

 shentry_allocation.x=COLUMN_LEFT_XPIXEL(sheet,sheet->active_cell.col);
 shentry_allocation.y=ROW_TOP_YPIXEL(sheet,sheet->active_cell.row);
 shentry_allocation.width=column_width;
 shentry_allocation.height=sheet->row[sheet->active_cell.row].height;

 if(GTK_IS_IENTRY(sheet->sheet_entry) && !GTK_SHEET_CLIP_TEXT(sheet)){
  switch(GTK_IENTRY(sheet_entry)->justification){
   case GTK_JUSTIFY_CENTER:
     shentry_allocation.width=size;
     shentry_allocation.x=shentry_allocation.x+
                          column_width/2-size/2;
     break;
   case GTK_JUSTIFY_RIGHT:
     shentry_allocation.x=shentry_allocation.x+shentry_allocation.width-size+1;
     shentry_allocation.width=size;
     break;
   case GTK_JUSTIFY_LEFT:
   case GTK_JUSTIFY_FILL:
     break;
  }

 }

 if(!GTK_IS_IENTRY(sheet->sheet_entry)){
   shentry_allocation.x += 2;
   shentry_allocation.y += 2;
   shentry_allocation.width -= 3;
   shentry_allocation.height -= 3;
 }

 if(sheet->sheet_entry_window){
   gdk_window_move_resize(sheet->sheet_entry_window,
                          shentry_allocation.x,
                          shentry_allocation.y,
                          shentry_allocation.width,
                          shentry_allocation.height);
   shentry_allocation.x = 0;
   shentry_allocation.y = 0;
   gtk_widget_size_allocate(sheet->sheet_entry, &shentry_allocation);
 } else 
   gtk_widget_size_allocate(sheet->sheet_entry, &shentry_allocation);

}

static void
gtk_sheet_entry_set_max_size(GtkSheet *sheet)
{
 gint i;
 gint size=0;
 gint sizel=0, sizer=0;
 gint row,col;
 gint justification;

 row=sheet->active_cell.row;
 col=sheet->active_cell.col;

 if(!GTK_IS_IENTRY(sheet->sheet_entry) || GTK_SHEET_CLIP_TEXT(sheet)) return;

 justification = GTK_IENTRY(sheet->sheet_entry)->justification;

 switch(justification){
  case GTK_JUSTIFY_FILL:
  case GTK_JUSTIFY_LEFT:
    for(i=col+1; i<=MAX_VISIBLE_COLUMN(sheet); i++){
     if(gtk_sheet_cell_get_text(sheet, row, i)) break;
     size+=sheet->column[i].width;
    }
    size = MIN(size, sheet->sheet_window_width - COLUMN_LEFT_XPIXEL(sheet, col));
    break;
  case GTK_JUSTIFY_RIGHT:
    for(i=col-1; i>=MIN_VISIBLE_COLUMN(sheet); i--){
     if(gtk_sheet_cell_get_text(sheet, row, i)) break;
     size+=sheet->column[i].width;
    }
    break;
  case GTK_JUSTIFY_CENTER:
    for(i=col+1; i<=MAX_VISIBLE_COLUMN(sheet); i++){
/*     if(gtk_sheet_cell_get_text(sheet, row, i)) break;
*/
     sizer+=sheet->column[i].width;
    }
    for(i=col-1; i>=MIN_VISIBLE_COLUMN(sheet); i--){
     if(gtk_sheet_cell_get_text(sheet, row, i)) break;
     sizel+=sheet->column[i].width;
    }
    size=2*MIN(sizel, sizer);
    break;
 }

 if(size!=0) size+=sheet->column[col].width;
 GTK_IENTRY(sheet->sheet_entry)->text_max_size=size;

}

static void
create_sheet_entry(GtkSheet *sheet)
{
 GtkWidget *widget;
 GtkWidget *parent;
 GtkWidget *entry;
 GtkStyle *style;
 gint found_entry = FALSE;

 widget = GTK_WIDGET(sheet);

 style = gtk_style_copy(GTK_WIDGET(sheet)->style); 
 gtk_widget_push_style(style);

 if(sheet->sheet_entry){
    if(sheet->sheet_entry_window){
        gdk_window_set_user_data(sheet->sheet_entry_window, NULL);
        gdk_window_destroy(sheet->sheet_entry_window);
        sheet->sheet_entry_window = NULL;
    }
    gtk_widget_ref(sheet->sheet_entry); /* avoids warnings */
    gtk_widget_unparent(sheet->sheet_entry);
    gtk_widget_destroy(sheet->sheet_entry);
 }

 if(sheet->entry_type){

   if(!gtk_type_is_a (sheet->entry_type, GTK_TYPE_ENTRY)){

     parent = GTK_WIDGET(gtk_type_new(sheet->entry_type));

     sheet->sheet_entry = parent;

     entry = gtk_sheet_get_entry (sheet);
     if(GTK_IS_ENTRY(entry)) found_entry = TRUE;

   } else {

       parent = GTK_WIDGET(gtk_type_new(sheet->entry_type));
       entry = parent;
       found_entry = TRUE;

   }             
                                    
   if(!found_entry){

     g_warning ("Entry type must be GtkEntry subclass, using default");
     entry=GTK_WIDGET(gtk_type_new(GTK_TYPE_IENTRY));
     sheet->sheet_entry = entry;

   } else {

     sheet->sheet_entry = parent;

   }


 } else {

     entry=GTK_WIDGET(gtk_type_new(GTK_TYPE_IENTRY));
     sheet->sheet_entry = entry;

 }
 
 gtk_widget_size_request(sheet->sheet_entry, NULL);
  
 if (GTK_WIDGET_REALIZED(sheet) && GTK_WIDGET_NO_WINDOW (sheet->sheet_entry))
    {
      GdkWindowAttr attributes;
      gint attributes_mask;
      
      attributes.window_type = GDK_WINDOW_CHILD;
      attributes.x = 0;
      attributes.y = 0;
      attributes.width = sheet->sheet_entry->requisition.width;
      attributes.height = sheet->sheet_entry->requisition.height;
      attributes.wclass = GDK_INPUT_OUTPUT;
      attributes.visual = gtk_widget_get_visual (widget);
      attributes.colormap = gtk_widget_get_colormap (widget);
      attributes.event_mask = GDK_EXPOSURE_MASK;
 
      attributes_mask = GDK_WA_X | GDK_WA_Y | GDK_WA_VISUAL | GDK_WA_COLORMAP;
      sheet->sheet_entry_window =  gdk_window_new (sheet->sheet_window,
 				               &attributes, attributes_mask);
      gdk_window_set_user_data (sheet->sheet_entry_window, widget);

      if (sheet->sheet_entry_window)
	gtk_style_set_background (widget->style, 
				  sheet->sheet_entry_window, 
				  GTK_STATE_NORMAL);
     }

 if(GTK_WIDGET_REALIZED(sheet))
   {
      gtk_widget_set_parent(sheet->sheet_entry, GTK_WIDGET(sheet));
      gtk_widget_set_parent_window (sheet->sheet_entry,
				    sheet->sheet_entry_window ? 
                                    sheet->sheet_entry_window : 
                                    sheet->sheet_window);
      gtk_widget_realize(sheet->sheet_entry);
   }

 gtk_signal_connect_object(GTK_OBJECT(entry),"key_press_event",
                           (GtkSignalFunc) gtk_sheet_entry_key_press,
                           GTK_OBJECT(sheet)); 

 gtk_widget_pop_style();
 gtk_widget_show (sheet->sheet_entry); 
}


GtkWidget * 
gtk_sheet_get_entry(GtkSheet *sheet)
{
 GtkWidget *parent;
 GtkWidget *entry = NULL;
 GtkTableChild *table_child;
 GtkBoxChild *box_child;
 GList *children = NULL;

 g_return_val_if_fail (sheet != NULL, NULL);
 g_return_val_if_fail (GTK_IS_SHEET (sheet), NULL);
 g_return_val_if_fail (sheet->sheet_entry != NULL, NULL);

 if(GTK_IS_ENTRY(sheet->sheet_entry)) return (sheet->sheet_entry);

 parent = GTK_WIDGET(sheet->sheet_entry);

 if(GTK_IS_TABLE(parent)) children = GTK_TABLE(parent)->children;
 if(GTK_IS_BOX(parent)) children = GTK_BOX(parent)->children;

 if(!children) return NULL;

 while(children){
      if(GTK_IS_TABLE(parent)) {
                 table_child = children->data;
                 entry = table_child->widget;
      }
      if(GTK_IS_BOX(parent)){
                 box_child = children->data; 
                 entry = box_child->widget;
      }

      if(GTK_IS_ENTRY(entry))  
                                break;
      children = children->next;                        
 } 


 if(!GTK_IS_ENTRY(entry))   return NULL;

 return (entry);

}

/* BUTTONS */
static void
row_button_set (GtkSheet *sheet, gint row)
{
  if(sheet->row[row].button.state == GTK_STATE_ACTIVE) return;

  sheet->row[row].button.state = GTK_STATE_ACTIVE;
  gtk_sheet_button_draw(sheet, row, -1);
 
}

static void
column_button_set (GtkSheet *sheet, gint column)
{
  if(sheet->column[column].button.state == GTK_STATE_ACTIVE) return;

  sheet->column[column].button.state = GTK_STATE_ACTIVE;
  gtk_sheet_button_draw(sheet, -1, column);
 
}

static void
row_button_release (GtkSheet *sheet, gint row)
{
  if(sheet->row[row].button.state == GTK_STATE_NORMAL) return;

  sheet->row[row].button.state = GTK_STATE_NORMAL;
  gtk_sheet_button_draw(sheet, row, -1);
}

static void
column_button_release (GtkSheet *sheet, gint column)
{
  if(sheet->column[column].button.state == GTK_STATE_NORMAL) return;

  sheet->column[column].button.state = GTK_STATE_NORMAL;
  gtk_sheet_button_draw(sheet, -1, column);
}

static void
gtk_sheet_button_draw (GtkSheet *sheet, gint row, gint column)
{
  GdkWindow *window = NULL;
  GtkShadowType shadow_type;
  gint width = 0, height = 0;
  gint x = 0, y = 0;
  gint index = 0;
  int text_width, text_height;
  GtkSheetButton *button = NULL;
  GtkSheetChild *child = NULL;
  GdkRectangle allocation;
  gboolean is_sensitive = FALSE;
  gint state;
  gint len;
  gchar *line;
  gchar *words;
  gchar label[10];

  if(!GTK_WIDGET_REALIZED(GTK_WIDGET(sheet))) return;

  if(row >= 0 && !sheet->row[row].is_visible) return;
  if(column >= 0 && !sheet->column[column].is_visible) return;
  if(row >= 0 && !GTK_SHEET_ROW_TITLES_VISIBLE(sheet)) return;
  if(column >= 0 && !GTK_SHEET_COL_TITLES_VISIBLE(sheet)) return;
  if(column>=0 && column <MIN_VISIBLE_COLUMN(sheet)) return;
  if(column>=0 && column >MAX_VISIBLE_COLUMN(sheet)) return;
  if(row>=0 && row <MIN_VISIBLE_ROW(sheet)) return;
  if(row>=0 && row >MAX_VISIBLE_ROW(sheet)) return;

  if(row==-1){
     window=sheet->column_title_window;
     button=&sheet->column[column].button;
     index=column;
     x = COLUMN_LEFT_XPIXEL(sheet, column)+CELL_SPACING;
     if(GTK_SHEET_ROW_TITLES_VISIBLE(sheet)) x -= sheet->row_title_area.width;
     y = 0;
     width = sheet->column[column].width;
     height = sheet->column_title_area.height;
     is_sensitive=sheet->column[column].is_sensitive;
  }
  if(column==-1){
     window=sheet->row_title_window;
     button=&sheet->row[row].button;
     index=row;
     x = 0;
     y = ROW_TOP_YPIXEL(sheet, row)+CELL_SPACING;
     if(GTK_SHEET_COL_TITLES_VISIBLE(sheet)) y-=sheet->column_title_area.height;
     width = sheet->row_title_area.width;
     height = sheet->row[row].height;
     is_sensitive=sheet->row[row].is_sensitive;
  }

  allocation.x = x;
  allocation.y = y;
  allocation.width = width;
  allocation.height = height;
 
  gdk_window_clear_area (window,
                         x, y,
	                 width, height);

  gtk_paint_box (sheet->button->style, window,
                 GTK_STATE_NORMAL, GTK_SHADOW_OUT, 
                 &allocation, GTK_WIDGET(sheet),
                 "buttondefault", x, y, width, height);

  state = button->state;
  if(!is_sensitive) state=GTK_STATE_INSENSITIVE;

  if (state == GTK_STATE_ACTIVE)
     shadow_type = GTK_SHADOW_IN;
  else
     shadow_type = GTK_SHADOW_OUT;

  if(state != GTK_STATE_NORMAL && state != GTK_STATE_INSENSITIVE)
  gtk_paint_box (sheet->button->style, window,
                 button->state, shadow_type, 
                 &allocation, GTK_WIDGET(sheet),
                 "button", x, y, width, height);

  text_height=GTK_WIDGET(sheet)->style->font->ascent + 
              GTK_WIDGET(sheet)->style->font->descent;
    
  y += DEFAULT_ROW_HEIGHT(GTK_WIDGET(sheet))/2+
       GTK_WIDGET(sheet)->style->font->ascent/2; 
 
  gdk_gc_set_clip_rectangle(GTK_WIDGET(sheet)->style->fg_gc[button->state], 
                            &allocation);
  gdk_gc_set_clip_rectangle(GTK_WIDGET(sheet)->style->white_gc, &allocation);

  if(button->label && strlen(button->label)>0){
           words=button->label;
           line = g_new(gchar, 1);
           line[0]='\0';

           while(words && *words != '\0'){
             if(*words != '\n'){
                len=strlen(line);
                line=g_realloc(line, len+2);
                line[len]=*words;
                line[len+1]='\0';
             }
             if(*words == '\n' || *(words+1) == '\0'){
               text_width = gdk_string_width (GTK_WIDGET (sheet)->style->font,
   	    		      line);

               switch(button->justification){
                 case GTK_JUSTIFY_LEFT:
                   gtk_paint_string (GTK_WIDGET(sheet)->style, window, state,
                                     &allocation, GTK_WIDGET(sheet), "label",
                                     x + CELLOFFSET, y,
                                     line);
                   break;
                 case GTK_JUSTIFY_RIGHT:
                   gtk_paint_string (GTK_WIDGET(sheet)->style, window, state,
                                     &allocation, GTK_WIDGET(sheet), "label",
                                     x + width - text_width - CELLOFFSET, y,
                                     line);
                   break;
                 case GTK_JUSTIFY_CENTER:
                 default:
                   gtk_paint_string (GTK_WIDGET(sheet)->style, window, state,
                                     &allocation, GTK_WIDGET(sheet), "label",
                                     x + (width - text_width) /2, y,
                                     line);
               }

               y += GTK_WIDGET(sheet)->style->font->ascent+
                    GTK_WIDGET(sheet)->style->font->descent + 2;

               g_free(line);
               line = g_new(gchar, 1);
               line[0]='\0';
             }
             words++;
           }
           g_free(line);
  }else{
           sprintf(label,"%d",index);
           text_width = gdk_string_width (GTK_WIDGET (sheet)->style->font,
   			      label);
           switch(button->justification){
             case GTK_JUSTIFY_LEFT:
               gtk_paint_string (GTK_WIDGET(sheet)->style, window, state,
                                 &allocation, GTK_WIDGET(sheet), "label",
                                 x + CELLOFFSET, y,
                                 label);
               break;
             case GTK_JUSTIFY_RIGHT:
               gtk_paint_string (GTK_WIDGET(sheet)->style, window, state,
                                 &allocation, GTK_WIDGET(sheet), "label",
                                 x + width - text_width - CELLOFFSET, y,
                                 label);
               break;
             case GTK_JUSTIFY_CENTER:
             default:
               gtk_paint_string (GTK_WIDGET(sheet)->style, window, state,
                                 &allocation, GTK_WIDGET(sheet), "label",
                                 x + (width - text_width) /2, y,
                                 label);
           }

  }

  gdk_gc_set_clip_rectangle(GTK_WIDGET(sheet)->style->fg_gc[button->state],
                            NULL);
  gdk_gc_set_clip_rectangle(GTK_WIDGET(sheet)->style->white_gc, NULL);

  if((child = button->child)){
      child->x = allocation.x;
      child->y = allocation.y;

      child->x += (width - child->widget->requisition.width) * child->x_align; 
      child->y += (height - child->widget->requisition.height) * child->y_align;
      child->widget->allocation.width = child->widget->requisition.width;
      child->widget->allocation.height = child->widget->requisition.height;

      x = child->x;
      y = child->y;

      gtk_widget_set_state(child->widget, button->state);
      if(GTK_WIDGET_NO_WINDOW(child->widget))
        {
           child->widget->allocation.x = 0;
           child->widget->allocation.y = 0;
        }

      if(GTK_WIDGET_REALIZED(GTK_WIDGET(sheet)) &&
         GTK_WIDGET_MAPPED(child->widget))
            {
              gtk_widget_size_allocate(child->widget, 
                                       &child->widget->allocation);
              if(GTK_WIDGET_NO_WINDOW(child->widget) && child->window)
                 {
		    gdk_window_move_resize(child->window,
                                           x, y,
                                           child->widget->allocation.width,
					   child->widget->allocation.height);
                    gtk_widget_draw(child->widget, NULL);
                 }
            }

  }
   
}


/* SCROLLBARS
 *
 * functions:
 *   adjust_scrollbars
 *   vadjustment_changed
 *   hadjustment_changed
 *   vadjustment_value_changed
 *   hadjustment_value_changed */

static void
adjust_scrollbars (GtkSheet * sheet)
{

 if(sheet->vadjustment){ 
  sheet->vadjustment->page_size = sheet->sheet_window_height;
  sheet->vadjustment->page_increment = sheet->sheet_window_height / 2;
  sheet->vadjustment->step_increment = DEFAULT_ROW_HEIGHT(GTK_WIDGET(sheet));
  sheet->vadjustment->lower = 0;
  sheet->vadjustment->upper = SHEET_HEIGHT (sheet) + 80;
/*
  if (sheet->sheet_window_height - sheet->voffset > SHEET_HEIGHT (sheet))
    {
      sheet->vadjustment->value = MAX(0, SHEET_HEIGHT (sheet) - 
	sheet->sheet_window_height);
      gtk_signal_emit_by_name (GTK_OBJECT (sheet->vadjustment), 
			       "value_changed");
    }
*/
    gtk_signal_emit_by_name (GTK_OBJECT(sheet->vadjustment), "changed");

 }

 if(sheet->hadjustment){
  sheet->hadjustment->page_size = sheet->sheet_window_width;
  sheet->hadjustment->page_increment = sheet->sheet_window_width / 2;
  sheet->hadjustment->step_increment = DEFAULT_COLUMN_WIDTH;
  sheet->hadjustment->lower = 0;
  sheet->hadjustment->upper = SHEET_WIDTH (sheet)+ 80;
/*
  if (sheet->sheet_window_width - sheet->hoffset > SHEET_WIDTH (sheet))
    {
      sheet->hadjustment->value = MAX(0, SHEET_WIDTH (sheet) - 
	sheet->sheet_window_width);
      gtk_signal_emit_by_name (GTK_OBJECT(sheet->hadjustment), 
			       "value_changed");
    }
*/
    gtk_signal_emit_by_name (GTK_OBJECT(sheet->hadjustment), "changed");

 }
/*
 if(GTK_WIDGET_REALIZED(sheet)) 
   {
     if(GTK_SHEET_ROW_TITLES_VISIBLE(sheet)){
                 size_allocate_row_title_buttons(sheet);
                 gdk_window_show(sheet->row_title_window);
     }

     if(GTK_SHEET_COL_TITLES_VISIBLE(sheet)){
                 size_allocate_column_title_buttons(sheet);
                 gdk_window_show(sheet->column_title_window);
     }

     gtk_sheet_range_draw(sheet, NULL);
   }
*/
}


static void
vadjustment_changed (GtkAdjustment * adjustment,
			       gpointer data)
{
  GtkSheet *sheet;

  g_return_if_fail (adjustment != NULL);
  g_return_if_fail (data != NULL);

  sheet = GTK_SHEET (data);

}

static void
hadjustment_changed (GtkAdjustment * adjustment,
			       gpointer data)
{
  GtkSheet *sheet;

  g_return_if_fail (adjustment != NULL);
  g_return_if_fail (data != NULL);

  sheet = GTK_SHEET (data);

}


static void
vadjustment_value_changed (GtkAdjustment * adjustment,
				     gpointer data)
{
  GtkSheet *sheet;
  gint diff, value, old_value;
  gint i;
  gint row, new_row;
  gint y=0;

  g_return_if_fail (adjustment != NULL);
  g_return_if_fail (data != NULL);
  g_return_if_fail (GTK_IS_SHEET (data));

  sheet = GTK_SHEET (data);

  if(GTK_SHEET_IS_FROZEN(sheet)) return;

  row=ROW_FROM_YPIXEL(sheet,sheet->column_title_area.height + CELL_SPACING);
  if(!GTK_SHEET_COL_TITLES_VISIBLE(sheet))
     row=ROW_FROM_YPIXEL(sheet,CELL_SPACING);
    
  old_value = -sheet->voffset;

  for(i=0; i<= sheet->maxrow; i++){
   if(sheet->row[i].is_visible) y+=sheet->row[i].height;
   if(y > adjustment->value) break;
  }
  y-=sheet->row[i].height;
  new_row=i;

  if (adjustment->value > sheet->old_vadjustment && sheet->old_vadjustment > 0. &&
      sheet->row[i].height > sheet->vadjustment->step_increment){
/* This avoids embarrassing twitching */
          if(row == new_row && row != sheet->maxrow &&
             adjustment->value - sheet->old_vadjustment >= 
                          sheet->vadjustment->step_increment &&
             new_row + 1 != MIN_VISIBLE_ROW(sheet)){
                new_row+=1;
                y=y+sheet->row[row].height;
          }
  }

/* Negative old_adjustment enforces the redraw, otherwise avoid spureous redraw */
  if(sheet->old_vadjustment >= 0. && row == new_row){
      sheet->old_vadjustment = sheet->vadjustment->value;
      return;
  }

  sheet->old_vadjustment = sheet->vadjustment->value;
  adjustment->value=y;

 
  if(new_row == 0){
   sheet->vadjustment->step_increment=
   sheet->row[0].height;
  }else{
   sheet->vadjustment->step_increment=
   MIN(sheet->row[new_row].height, sheet->row[new_row-1].height);
  }

  sheet->vadjustment->value=adjustment->value;

  value = adjustment->value;

  if (value >= -sheet->voffset)
	{
	  /* scroll down */
	  diff = value + sheet->voffset;
	}
  else
	{
	  /* scroll up */
	  diff = -sheet->voffset - value;
	}

      sheet->voffset = -value;
 
  sheet->view.row0=ROW_FROM_YPIXEL(sheet, sheet->column_title_area.height+1);
  sheet->view.rowi=ROW_FROM_YPIXEL(sheet, sheet->sheet_window_height-1);
  if(!GTK_SHEET_COL_TITLES_VISIBLE(sheet))
     sheet->view.row0=ROW_FROM_YPIXEL(sheet, 1);

  if(GTK_WIDGET_REALIZED(sheet->sheet_entry) &&
     sheet->state == GTK_SHEET_NORMAL && 
     sheet->active_cell.row >= 0 && sheet->active_cell.col >= 0 &&
     !gtk_sheet_cell_isvisible(sheet, sheet->active_cell.row,
                                      sheet->active_cell.col))
    {
      gchar *text;

      text = gtk_entry_get_text(GTK_ENTRY(gtk_sheet_get_entry(sheet)));
      if(!text || strlen(text)==0) 
             gtk_sheet_cell_clear(sheet,
                                  sheet->active_cell.row,
                                  sheet->active_cell.col);
      if(sheet->sheet_entry_window) 
            gdk_window_hide(sheet->sheet_entry_window);
      else
            gdk_window_hide(sheet->sheet_entry->window);
    }

  gtk_sheet_position_children(sheet);

  gtk_sheet_range_draw(sheet, NULL);
  size_allocate_row_title_buttons(sheet);
  size_allocate_global_button(sheet);
}

static void
hadjustment_value_changed (GtkAdjustment * adjustment,
			   gpointer data)
{
  GtkSheet *sheet;
  gint i, diff, value, old_value;
  gint column, new_column;
  gint x=0;

  g_return_if_fail (adjustment != NULL);
  g_return_if_fail (data != NULL);
  g_return_if_fail (GTK_IS_SHEET (data));

  sheet = GTK_SHEET (data);

  if(GTK_SHEET_IS_FROZEN(sheet)) return;

  column=COLUMN_FROM_XPIXEL(sheet,sheet->row_title_area.width + CELL_SPACING);
  if(!GTK_SHEET_ROW_TITLES_VISIBLE(sheet))
     column=COLUMN_FROM_XPIXEL(sheet, CELL_SPACING);

  old_value = -sheet->hoffset;

  for(i=0; i<= sheet->maxcol; i++){
   if(sheet->column[i].is_visible) x+=sheet->column[i].width;
   if(x > adjustment->value) break;
  }
  x-=sheet->column[i].width;
  new_column=i;

  if (adjustment->value > sheet->old_hadjustment && sheet->old_hadjustment > 0 &&
      sheet->column[i].width > sheet->hadjustment->step_increment){
/* This avoids embarrassing twitching */
          if(column == new_column && column != sheet->maxcol &&
             adjustment->value - sheet->old_hadjustment >= 
                          sheet->hadjustment->step_increment &&
             new_column + 1 != MIN_VISIBLE_COLUMN(sheet)){
             new_column+=1;
             x=x+sheet->column[column].width;
          }
  }

/* Negative old_adjustment enforces the redraw, otherwise avoid spureous redraw */
  if(sheet->old_hadjustment >= 0. && new_column == column){
     sheet->old_hadjustment = sheet->hadjustment->value;
     return;
  }

  sheet->old_hadjustment = sheet->hadjustment->value;
  adjustment->value=x;

  if(new_column == 0){
   sheet->hadjustment->step_increment=
   sheet->column[0].width;
  }else{
   sheet->hadjustment->step_increment=
   MIN(sheet->column[new_column].width, sheet->column[new_column-1].width);
  }


  sheet->hadjustment->value=adjustment->value;

  value = adjustment->value;

  if (value >= -sheet->hoffset)
        {
	  /* scroll right */
	  diff = value + sheet->hoffset;
	}
  else
	{
	  /* scroll left */
	  diff = -sheet->hoffset - value;
	}

  sheet->hoffset = -value;

  sheet->view.col0=COLUMN_FROM_XPIXEL(sheet, sheet->row_title_area.width+1);
  sheet->view.coli=COLUMN_FROM_XPIXEL(sheet, sheet->sheet_window_width);
  if(!GTK_SHEET_ROW_TITLES_VISIBLE(sheet))
    sheet->view.col0=COLUMN_FROM_XPIXEL(sheet, 1);

  if(GTK_WIDGET_REALIZED(sheet->sheet_entry) &&
     sheet->state == GTK_SHEET_NORMAL && 
     sheet->active_cell.row >= 0 && sheet->active_cell.col >= 0 &&
     !gtk_sheet_cell_isvisible(sheet, sheet->active_cell.row,
                                      sheet->active_cell.col))
    {
      gchar *text;

      text = gtk_entry_get_text(GTK_ENTRY(gtk_sheet_get_entry(sheet)));
      if(!text || strlen(text)==0) 
             gtk_sheet_cell_clear(sheet,
                                  sheet->active_cell.row,
                                  sheet->active_cell.col);
      if(sheet->sheet_entry_window) 
            gdk_window_hide(sheet->sheet_entry_window);
      else
            gdk_window_hide(sheet->sheet_entry->window);
    }

  gtk_sheet_position_children(sheet);

  gtk_sheet_range_draw(sheet, NULL);
  size_allocate_column_title_buttons(sheet);
}
	

/* COLUMN RESIZING */
static void                          
draw_xor_vline (GtkSheet * sheet)
{
  GtkWidget *widget;
  
  g_return_if_fail (sheet != NULL);
  
  widget = GTK_WIDGET (sheet);

  gdk_draw_line (widget->window, sheet->xor_gc,  
                 sheet->x_drag,                                       
                 sheet->column_title_area.height,                               
                 sheet->x_drag,                                             
                 sheet->sheet_window_height + 1);
}

/* ROW RESIZING */
static void                          
draw_xor_hline (GtkSheet * sheet)
{
  GtkWidget *widget;
  
  g_return_if_fail (sheet != NULL);
  
  widget = GTK_WIDGET (sheet);

  gdk_draw_line (widget->window, sheet->xor_gc,  
		 sheet->row_title_area.width,
                 sheet->y_drag,                                       
                        
	         sheet->sheet_window_width + 1,                      
                 sheet->y_drag);                                             
}

/* SELECTED RANGE */
static void
draw_xor_rectangle(GtkSheet *sheet, GtkSheetRange range)
{
   gint i;
   GdkRectangle clip_area, area;
   GdkGCValues values;

   area.x=COLUMN_LEFT_XPIXEL(sheet, range.col0);
   area.y=ROW_TOP_YPIXEL(sheet, range.row0);
   area.width=COLUMN_LEFT_XPIXEL(sheet, range.coli)-area.x+
                                        sheet->column[range.coli].width;
   area.height=ROW_TOP_YPIXEL(sheet, range.rowi)-area.y+
                                        sheet->row[range.rowi].height;

   clip_area.x=sheet->row_title_area.width;
   clip_area.y=sheet->column_title_area.height;
   clip_area.width=sheet->sheet_window_width;
   clip_area.height=sheet->sheet_window_height;

   if(!GTK_SHEET_ROW_TITLES_VISIBLE(sheet)) clip_area.x = 0;
   if(!GTK_SHEET_COL_TITLES_VISIBLE(sheet)) clip_area.y = 0;

   if(area.x<0) {
      area.width=area.width+area.x;
      area.x=0;
   }
   if(area.width>clip_area.width) area.width=clip_area.width+10;
   if(area.y<0) {
      area.height=area.height+area.y;
      area.y=0;
   }
   if(area.height>clip_area.height) area.height=clip_area.height+10;

   clip_area.x--;
   clip_area.y--;
   clip_area.width+=3;
   clip_area.height+=3;

   gdk_gc_get_values(sheet->xor_gc, &values);

   gdk_gc_set_clip_rectangle(sheet->xor_gc, &clip_area);

   for(i=-1;i<=1;i=i++)
     gdk_draw_rectangle(sheet->sheet_window,
                        sheet->xor_gc,
		        FALSE,
		        area.x+i, area.y+i,
                        area.width-2*i, area.height-2*i);


   gdk_gc_set_clip_rectangle(sheet->xor_gc, NULL);

   gdk_gc_set_foreground(sheet->xor_gc, &values.foreground);

}                      

  
/* this function returns the new width of the column being resized given
 * the column and x position of the cursor; the x cursor position is passed
 * in as a pointer and automaticaly corrected if it's beyond min/max limits */
static gint
new_column_width (GtkSheet * sheet,
		  gint column,
		  gint * x)
{
  gint cx, width;
  GtkRequisition requisition;

  cx = *x;

  gtk_sheet_button_size_request(sheet, &sheet->column[column].button,
                                &requisition);

  /* you can't shrink a column to less than its minimum width */
  if (cx < COLUMN_LEFT_XPIXEL (sheet, column) + requisition.width)
    {
      *x = cx = COLUMN_LEFT_XPIXEL (sheet, column) + requisition.width;
    }

  /* don't grow past the end of the window */
  /*
  if (cx > sheet->sheet_window_width)
    {
      *x = cx = sheet->sheet_window_width;
    }
    */
  /* calculate new column width making sure it doesn't end up
   * less than the minimum width */
  width = cx - COLUMN_LEFT_XPIXEL (sheet, column);
  if (width < requisition.width)
    width = requisition.width;

  sheet->column[column].width = width;
  gtk_sheet_recalc_left_xpixels(sheet, column+1);
  sheet->view.coli=COLUMN_FROM_XPIXEL(sheet, sheet->sheet_window_width);
  size_allocate_column_title_buttons (sheet);
  
  return width;
}

/* this function returns the new height of the row being resized given
 * the row and y position of the cursor; the y cursor position is passed
 * in as a pointer and automaticaly corrected if it's beyond min/max limits */
static gint
new_row_height (GtkSheet * sheet,
		  gint row,
		  gint * y)
{
  GtkRequisition requisition;
  gint cy, height;

  cy = *y;

  gtk_sheet_button_size_request(sheet, &sheet->row[row].button,
                                &requisition);

  /* you can't shrink a row to less than its minimum height */
  if (cy < ROW_TOP_YPIXEL (sheet, row) + requisition.height)

    {
      *y = cy = ROW_TOP_YPIXEL (sheet, row) + requisition.height;
    }

  /* don't grow past the end of the window */
  /*
  if (cy > sheet->sheet_window_height)
    {
      *y = cy = sheet->sheet_window_height;
    }
    */
  /* calculate new row height making sure it doesn't end up
   * less than the minimum height */
  height = (cy - ROW_TOP_YPIXEL (sheet, row));
  if (height < requisition.height)
    height = requisition.height;

  sheet->row[row].height = height;
  gtk_sheet_recalc_top_ypixels(sheet, row);
  sheet->view.rowi=ROW_FROM_YPIXEL(sheet, sheet->sheet_window_height-1);
  size_allocate_row_title_buttons (sheet);

  return height;
}

void
gtk_sheet_set_column_width (GtkSheet * sheet,
			    gint column,
			    gint width)
{
  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  if (column < 0 || column > sheet->maxcol)
    return;

  sheet->column[column].width = width;

  gtk_sheet_recalc_left_xpixels(sheet, column+1);

  if(GTK_WIDGET_REALIZED(GTK_WIDGET(sheet)) && !GTK_SHEET_IS_FROZEN(sheet)){
    size_allocate_column_title_buttons (sheet);
    adjust_scrollbars (sheet);
    gtk_sheet_size_allocate_entry(sheet);
    gtk_sheet_range_draw (sheet, NULL);
  }

  gtk_signal_emit(GTK_OBJECT(sheet), sheet_signals[CHANGED], -1, column);
  gtk_signal_emit(GTK_OBJECT(sheet), sheet_signals[NEW_COL_WIDTH], column, width);

}

void
gtk_sheet_set_row_height (GtkSheet * sheet,
			    gint row,
			    gint height)
{
  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  if (row < 0 || row > sheet->maxrow)
    return;

  sheet->row[row].height = height;

  gtk_sheet_recalc_top_ypixels(sheet, row+1);

  if(GTK_WIDGET_REALIZED(GTK_WIDGET(sheet)) && !GTK_SHEET_IS_FROZEN(sheet)){
    size_allocate_row_title_buttons (sheet);
    adjust_scrollbars (sheet);
    gtk_sheet_size_allocate_entry(sheet);
    gtk_sheet_range_draw (sheet, NULL);
  }

  gtk_signal_emit(GTK_OBJECT(sheet), sheet_signals[CHANGED], row, -1);
  gtk_signal_emit(GTK_OBJECT(sheet), sheet_signals[NEW_ROW_HEIGHT], row, height);

}


void
gtk_sheet_add_column(GtkSheet *sheet, gint ncols)
{

 g_return_if_fail (sheet != NULL);
 g_return_if_fail (GTK_IS_SHEET (sheet));

 AddColumn(sheet, ncols);

 if(!GTK_WIDGET_REALIZED(sheet)) return;

 adjust_scrollbars(sheet);

 if(sheet->state==GTK_SHEET_ROW_SELECTED) sheet->range.coli+=ncols;

 sheet->old_hadjustment = -1.;
 if(!GTK_SHEET_IS_FROZEN(sheet) && sheet->hadjustment)
      gtk_signal_emit_by_name (GTK_OBJECT (sheet->hadjustment), 
			       "value_changed");
}

void
gtk_sheet_add_row(GtkSheet *sheet, gint nrows)
{

 g_return_if_fail (sheet != NULL);
 g_return_if_fail (GTK_IS_SHEET (sheet));

 AddRow(sheet, nrows);

 if(!GTK_WIDGET_REALIZED(sheet)) return;

 if(sheet->state==GTK_SHEET_COLUMN_SELECTED) sheet->range.rowi+=nrows;

 adjust_scrollbars(sheet);

 sheet->old_vadjustment = -1.;
 if(!GTK_SHEET_IS_FROZEN(sheet) && sheet->vadjustment)
      gtk_signal_emit_by_name (GTK_OBJECT (sheet->vadjustment), 
			       "value_changed");
}

void
gtk_sheet_insert_rows(GtkSheet *sheet, gint row, gint nrows)
{
 GList *children;
 GtkSheetChild *child;

 g_return_if_fail (sheet != NULL);
 g_return_if_fail (GTK_IS_SHEET (sheet));

 if(GTK_WIDGET_REALIZED(sheet))
   gtk_sheet_unselect_range(sheet, NULL);

 InsertRow(sheet, row, nrows);

 children = sheet->children;
 while(children)
   {
     child = (GtkSheetChild *)children->data;

     if(child->attached_to_cell)
        if(child->row >= row) child->row += nrows; 

     children = children->next;
   }

 if(!GTK_WIDGET_REALIZED(sheet)) return;

 if(sheet->state==GTK_SHEET_COLUMN_SELECTED) sheet->range.rowi+=nrows;
 adjust_scrollbars(sheet);
 
 sheet->old_vadjustment = -1.;
 if(!GTK_SHEET_IS_FROZEN(sheet) && sheet->vadjustment)
      gtk_signal_emit_by_name (GTK_OBJECT (sheet->vadjustment), 
			       "value_changed");

}

void
gtk_sheet_insert_columns(GtkSheet *sheet, gint col, gint ncols)
{
 GList *children;
 GtkSheetChild *child;

 g_return_if_fail (sheet != NULL);
 g_return_if_fail (GTK_IS_SHEET (sheet));

 if(GTK_WIDGET_REALIZED(sheet))
   gtk_sheet_unselect_range(sheet, NULL);

 InsertColumn(sheet, col, ncols);

 children = sheet->children;
 while(children)
   {
     child = (GtkSheetChild *)children->data;

     if(child->attached_to_cell)
        if(child->col >= col) child->col += ncols; 

     children = children->next;
   }

 if(!GTK_WIDGET_REALIZED(sheet)) return;

 if(sheet->state==GTK_SHEET_ROW_SELECTED) sheet->range.coli+=ncols;
 adjust_scrollbars(sheet);

 sheet->old_hadjustment = -1.;
 if(!GTK_SHEET_IS_FROZEN(sheet) && sheet->hadjustment)
      gtk_signal_emit_by_name (GTK_OBJECT (sheet->hadjustment), 
			       "value_changed");

}

void
gtk_sheet_delete_rows(GtkSheet *sheet, gint row, gint nrows)
{
 GList *children;
 GtkSheetChild *child;
 gint irow, icol;
 gboolean veto;

 g_return_if_fail (sheet != NULL);
 g_return_if_fail (GTK_IS_SHEET (sheet));

 if(GTK_WIDGET_REALIZED(sheet))
   gtk_sheet_unselect_range(sheet, NULL);

 DeleteRow(sheet, row, nrows);

 children = sheet->children;
 while(children)
   {
     child = (GtkSheetChild *)children->data;

     if(child->attached_to_cell){
        if(child->row >= row && child->row < row+nrows){  
              gtk_container_remove(GTK_CONTAINER(sheet), child->widget);
              children = sheet->children;
        } 
        else
        {
              if(child->row >= row) child->row -= nrows; 
              children = children->next;
        }
     }
     else
        children = children->next;
   }


 if(!GTK_WIDGET_REALIZED(sheet)) return;

 irow = sheet->active_cell.row;
 icol = sheet->active_cell.col;

 sheet->active_cell.row = -1;
 sheet->active_cell.col = -1;

/* if(sheet->state == GTK_SHEET_ROW_SELECTED)
*/           gtk_sheet_click_cell(sheet, irow, icol, &veto);

 gtk_sheet_activate_cell(sheet, sheet->active_cell.row,
                                       sheet->active_cell.col);

 adjust_scrollbars(sheet);

 sheet->old_vadjustment = -1.;
 if(!GTK_SHEET_IS_FROZEN(sheet) && sheet->vadjustment)
      gtk_signal_emit_by_name (GTK_OBJECT (sheet->vadjustment), 
			       "value_changed");

}

void
gtk_sheet_delete_columns(GtkSheet *sheet, gint col, gint ncols)
{
 GList *children;
 GtkSheetChild *child;
 gint irow, icol;
 gboolean veto;

 g_return_if_fail (sheet != NULL);
 g_return_if_fail (GTK_IS_SHEET (sheet));

 if(GTK_WIDGET_REALIZED(sheet))
   gtk_sheet_unselect_range(sheet, NULL);

 DeleteColumn(sheet, col, ncols);

 children = sheet->children;
 while(children)
   {
     child = (GtkSheetChild *)children->data;

     if(child->attached_to_cell){
        if(child->col >= col && child->col < col+ncols){  
              gtk_container_remove(GTK_CONTAINER(sheet), child->widget);
              children = sheet->children;
        } 
        else
        {
              if(child->col >= col) child->col -= ncols; 
              children = children->next;
        }
     }
     else
        children = children->next;
   }


 if(!GTK_WIDGET_REALIZED(sheet)) return;

 irow = sheet->active_cell.row;
 icol = sheet->active_cell.col;

 sheet->active_cell.row = -1;
 sheet->active_cell.col = -1;

/* if(sheet->state == GTK_SHEET_COLUMN_SELECTED)
*/           gtk_sheet_click_cell(sheet, irow, icol, &veto);

 gtk_sheet_activate_cell(sheet, sheet->active_cell.row,
                                       sheet->active_cell.col);

 adjust_scrollbars(sheet);

 sheet->old_hadjustment = -1.;
 if(!GTK_SHEET_IS_FROZEN(sheet) && sheet->hadjustment)
      gtk_signal_emit_by_name (GTK_OBJECT (sheet->hadjustment), 
			       "value_changed");

}

void
gtk_sheet_range_set_background(GtkSheet *sheet, GtkSheetRange range, GdkColor *color)
{
  gint i, j;
  GtkSheetCellAttr attributes;

  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  for (i=range.row0; i<=range.rowi; i++)
    for (j=range.col0; j<=range.coli; j++){
      gtk_sheet_get_attributes(sheet, i, j, &attributes);
      if(color != NULL)
        attributes.background = *color;
      else
        gdk_color_white(gdk_colormap_get_system(), &attributes.background);
 
      gtk_sheet_set_cell_attributes(sheet, i, j, attributes); 
    }

  range.row0--;
  range.col0--;
  range.rowi++;
  range.coli++;

  if(!GTK_SHEET_IS_FROZEN(sheet))
      gtk_sheet_range_draw(sheet, &range);

}

void
gtk_sheet_range_set_foreground(GtkSheet *sheet, GtkSheetRange range, GdkColor *color)
{
  gint i, j;
  GtkSheetCellAttr attributes;

  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  for (i=range.row0; i<=range.rowi; i++)
    for (j=range.col0; j<=range.coli; j++){
      gtk_sheet_get_attributes(sheet, i, j, &attributes);

      if(color != NULL)
        attributes.foreground = *color;
      else
        gdk_color_black(gdk_colormap_get_system(), &attributes.foreground);
 
      gtk_sheet_set_cell_attributes(sheet, i, j, attributes); 
    }

  if(!GTK_SHEET_IS_FROZEN(sheet))
      gtk_sheet_range_draw(sheet, &range);

}

void
gtk_sheet_range_set_justification(GtkSheet *sheet, GtkSheetRange range, gint just)
{
  gint i, j;
  GtkSheetCellAttr attributes;

  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  for (i=range.row0; i<=range.rowi; i++)
    for (j=range.col0; j<=range.coli; j++){
      gtk_sheet_get_attributes(sheet, i, j, &attributes);
      attributes.justification = just;
      gtk_sheet_set_cell_attributes(sheet, i, j, attributes); 
    }

  range.col0 = sheet->view.col0;    
  range.coli = sheet->view.coli;    

  if(!GTK_SHEET_IS_FROZEN(sheet))
      gtk_sheet_range_draw(sheet, &range);

}

void
gtk_sheet_column_set_justification(GtkSheet *sheet, gint col, 
                                   gint justification)
{
  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  if(col > sheet->maxcol) return;

  sheet->column[col].justification = justification;
  
  if(GTK_WIDGET_REALIZED(sheet) && !GTK_SHEET_IS_FROZEN(sheet) &&
     col >= MIN_VISIBLE_COLUMN(sheet) && col <= MAX_VISIBLE_COLUMN(sheet))
          gtk_sheet_range_draw(sheet, NULL);

}

void
gtk_sheet_range_set_editable(GtkSheet *sheet, GtkSheetRange range, gboolean editable)
{
  gint i, j;
  GtkSheetCellAttr attributes;
 
  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  for (i=range.row0; i<=range.rowi; i++)
    for (j=range.col0; j<=range.coli; j++){
      gtk_sheet_get_attributes(sheet, i, j, &attributes);
      attributes.is_editable = editable;
      gtk_sheet_set_cell_attributes(sheet, i, j, attributes); 
    }
 
  if(!GTK_SHEET_IS_FROZEN(sheet))
      gtk_sheet_range_draw(sheet, &range);

}

void
gtk_sheet_range_set_visible(GtkSheet *sheet, GtkSheetRange range, gint visible)
{
  gint i, j;
  GtkSheetCellAttr attributes;

  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  for (i=range.row0; i<=range.rowi; i++)
    for (j=range.col0; j<=range.coli; j++){
      gtk_sheet_get_attributes(sheet, i, j, &attributes);
      attributes.is_visible=visible;
      gtk_sheet_set_cell_attributes(sheet, i, j, attributes); 
    }
 
  if(!GTK_SHEET_IS_FROZEN(sheet))
      gtk_sheet_range_draw(sheet, &range);

}

void
gtk_sheet_range_set_border(GtkSheet *sheet, GtkSheetRange range, gint mask, 
gint width, gint line_style)
{
  gint i, j;
  GtkSheetCellAttr attributes;

  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  for (i=range.row0; i<=range.rowi; i++)
    for (j=range.col0; j<=range.coli; j++){
      gtk_sheet_get_attributes(sheet, i, j, &attributes);
      attributes.border.mask = mask;
      attributes.border.width = width;
      attributes.border.line_style=line_style;
      attributes.border.cap_style=GDK_CAP_NOT_LAST;
      attributes.border.join_style=GDK_JOIN_MITER;
      gtk_sheet_set_cell_attributes(sheet, i, j, attributes);      
    }

  range.row0--; 
  range.col0--; 
  range.rowi++; 
  range.coli++; 

  if(!GTK_SHEET_IS_FROZEN(sheet))
      gtk_sheet_range_draw(sheet, &range);

}

void
gtk_sheet_range_set_border_color(GtkSheet *sheet, GtkSheetRange range, GdkColor *color)
{
  gint i, j;
  GtkSheetCellAttr attributes;

  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));


  for (i=range.row0; i<=range.rowi; i++)
    for (j=range.col0; j<=range.coli; j++){
      gtk_sheet_get_attributes(sheet, i, j, &attributes);
      attributes.border.color = *color;
      gtk_sheet_set_cell_attributes(sheet, i, j, attributes); 
    }
 
  if(!GTK_SHEET_IS_FROZEN(sheet))
      gtk_sheet_range_draw(sheet, &range);

}

void
gtk_sheet_range_set_font(GtkSheet *sheet, GtkSheetRange range, GdkFont *font)
{
  gint i, j;
  gint font_height;
  GtkSheetCellAttr attributes;

  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  gtk_sheet_freeze(sheet);

  for (i=range.row0; i<=range.rowi; i++)
    for (j=range.col0; j<=range.coli; j++){
      gtk_sheet_get_attributes(sheet, i, j, &attributes);
      attributes.font = font;
      font_height=attributes.font->ascent +
                  2 * attributes.font->descent + 2*CELLOFFSET;
      if(font_height > sheet->row[i].height){
          sheet->row[i].height = font_height;
          gtk_sheet_recalc_top_ypixels(sheet, i);
      }

      gtk_sheet_set_cell_attributes(sheet, i, j, attributes);  
    }

  gtk_sheet_thaw(sheet);
}

static void
gtk_sheet_set_cell_attributes(GtkSheet *sheet, gint row, gint col, GtkSheetCellAttr attributes)
{
  GtkSheetCell **cell;

  if(row > sheet->maxrow || col >sheet->maxcol) return;

  CheckBounds(sheet, row, col);

  cell = &sheet->data[row][col];

  if(*cell==NULL){
   (*cell) = gtk_sheet_cell_new();
   (*cell)->row = row;
   (*cell)->col = col;
  }

  if((*cell)->attributes == NULL) 
      (*cell)->attributes = g_new(GtkSheetCellAttr, 1);

  *((*cell)->attributes) = attributes;
}

gboolean
gtk_sheet_get_attributes(GtkSheet *sheet, gint row, gint col, GtkSheetCellAttr *attributes)
{
 GtkSheetCell **cell;

 g_return_val_if_fail (sheet != NULL, FALSE);
 g_return_val_if_fail (GTK_IS_SHEET (sheet), FALSE);

 if(row < 0 || col < 0) return FALSE;

 if(row > sheet->maxallocrow || col > sheet->maxalloccol){
    init_attributes(sheet, col, attributes);
    return FALSE;
 }

 if(row <= sheet->maxallocrow && col <= sheet->maxalloccol){
    cell = &sheet->data[row][col];
    if(*cell == NULL){
      init_attributes(sheet, col, attributes);
      return FALSE;
    } else
      if((*cell)->attributes == NULL){
         init_attributes(sheet, col, attributes);
         return FALSE;
      }else{
         *attributes = *(sheet->data[row][col]->attributes);
         if(sheet->column[col].justification != GTK_JUSTIFY_FILL)
              attributes->justification = sheet->column[col].justification;
      }
 }   
 
 return TRUE;
}

static void
init_attributes(GtkSheet *sheet, gint col, GtkSheetCellAttr *attributes)
{
 /* DEFAULT VALUES */    
 attributes->foreground = GTK_WIDGET(sheet)->style->black;
 attributes->background = GTK_WIDGET(sheet)->style->white;
 if(!GTK_WIDGET_REALIZED(GTK_WIDGET(sheet))){
   GdkColormap *colormap;
   colormap=gdk_colormap_get_system();
   gdk_color_black(colormap, &attributes->foreground);
   gdk_color_white(colormap, &attributes->background);
 }
 attributes->justification = sheet->column[col].justification;
 attributes->border.width = 0;
 attributes->border.line_style = GDK_LINE_SOLID;
 attributes->border.cap_style = GDK_CAP_NOT_LAST;
 attributes->border.join_style = GDK_JOIN_MITER;
 attributes->border.mask = 0;
 attributes->border.color = GTK_WIDGET(sheet)->style->black;
 attributes->font = GTK_WIDGET(sheet)->style->font;
 attributes->is_editable = TRUE;
 attributes->is_visible = TRUE;

}       
 
/**********************************************************************
 * Memory allocation routines: 
 * AddRow & AddColumn allocate memory for GtkSheetColumn & GtkSheetRow structs.
 * InsertRow 
 * InsertColumn
 * DeleteRow
 * DeleteColumn
 * GrowSheet allocates memory for the sheet cells contents using an array of 
 * pointers. Alternative to this could be a linked list or a hash table.
 * CheckBounds checks whether the given cell is currently allocated or not. 
 * If not, it calls to GrowSheet.
 **********************************************************************/

static gint
AddColumn(GtkSheet *tbl, gint ncols)
{
   gint i;

   if(ncols == -1 && tbl->maxcol == 0)
     {
       ncols = 1;
     }
   else
     {
       tbl->maxcol += ncols;
       tbl->column = (GtkSheetColumn *)g_realloc(tbl->column,(tbl->maxcol+1)*
                                                 sizeof(GtkSheetColumn));
     }

   for(i=tbl->maxcol-ncols+1; i<= tbl->maxcol; i++){
        tbl->column[i].width=DEFAULT_COLUMN_WIDTH;
	tbl->column[i].button.label=NULL;
	tbl->column[i].button.child=NULL;
        tbl->column[i].button.state=GTK_STATE_NORMAL;
        tbl->column[i].button.justification=GTK_JUSTIFY_CENTER;
        tbl->column[i].name=NULL;
        tbl->column[i].is_visible=TRUE;
        tbl->column[i].is_sensitive=TRUE;
        tbl->column[i].left_text_column=i;
        tbl->column[i].right_text_column=i;
        tbl->column[i].justification=GTK_JUSTIFY_FILL;
        if(i>0)
        {
           tbl->column[i].left_text_column=tbl->column[i-1].left_text_column;
           tbl->column[i].left_xpixel=tbl->column[i-1].left_xpixel +
                                     tbl->column[i-1].width;
	}
        else
        {
	   tbl->column[i].left_xpixel=tbl->row_title_area.width;
	   if(!GTK_SHEET_ROW_TITLES_VISIBLE(tbl)) 
                        tbl->column[i].left_xpixel=0;
        }
   }
   return TRUE;
}

static gint
AddRow(GtkSheet *tbl, gint nrows)
{
   gint i;

   if(nrows == -1 && tbl->maxrow == 0)
     {
       nrows = 1;
     }
   else
     {
       tbl->maxrow += nrows;
       tbl->row = (GtkSheetRow *)g_realloc(tbl->row,(tbl->maxrow+1)*
                                            sizeof(GtkSheetRow));
     }

   for(i=tbl->maxrow-nrows+1; i<= tbl->maxrow; i++){
        tbl->row[i].height=DEFAULT_ROW_HEIGHT(GTK_WIDGET(tbl));
	tbl->row[i].button.label=NULL;
	tbl->row[i].button.child=NULL;
        tbl->row[i].button.state=GTK_STATE_NORMAL;
        tbl->row[i].button.justification=GTK_JUSTIFY_CENTER;
        tbl->row[i].name=NULL;
        tbl->row[i].is_visible=TRUE;
        tbl->row[i].is_sensitive=TRUE;
        if(i>0)
           tbl->row[i].top_ypixel=tbl->row[i-1].top_ypixel+tbl->row[i-1].height;
	else
        {
	   tbl->row[i].top_ypixel=tbl->column_title_area.height;
           if(!GTK_SHEET_COL_TITLES_VISIBLE(tbl)) 
                        tbl->row[i].top_ypixel=0;
        } 
   }
   return TRUE;
}

static gint
InsertRow(GtkSheet *tbl, gint row, gint nrows)
{
  GtkSheetCell **pp;
  gint i,j;
  GtkSheetCell **auxdata;
  GtkSheetRow auxrow;

  AddRow(tbl,nrows);

  for(i=tbl->maxrow; i>=row+nrows; i--){
    auxrow = tbl->row[i];  
    tbl->row[i]=tbl->row[i-nrows];    
    tbl->row[i].is_visible=tbl->row[i-nrows].is_visible;
    tbl->row[i].is_sensitive=tbl->row[i-nrows].is_sensitive;
    if(auxrow.is_visible) 
               tbl->row[i].top_ypixel+=nrows*DEFAULT_ROW_HEIGHT(GTK_WIDGET(tbl));
    tbl->row[i-nrows]=auxrow;
  }

  if(row <= tbl->maxallocrow){
   
    GrowSheet(tbl,nrows,0);

    for(i=tbl->maxallocrow; i>=row+nrows; i--){
      auxdata = tbl->data[i];
      tbl->data[i]=tbl->data[i-nrows];

      pp= tbl->data[i];
      for(j=0; j<=tbl->maxalloccol; j++,pp++){
        if(*pp!=(GtkSheetCell *)NULL)
                                    (*pp)->row=i;
      
      }
      tbl->data[i-nrows]=auxdata;
    }
  }
  gtk_sheet_recalc_top_ypixels(tbl, 0);
  return TRUE;
}  

static gint 
InsertColumn(GtkSheet *tbl, gint col, gint ncols)
{
  gint i,j;
  GtkSheetColumn auxcol;

  AddColumn(tbl,ncols);

  for(i=tbl->maxcol; i>=col+ncols; i--){
    auxcol = tbl->column[i];
    tbl->column[i]=tbl->column[i-ncols];
    tbl->column[i].is_visible=tbl->column[i-ncols].is_visible;
    tbl->column[i].is_sensitive=tbl->column[i-ncols].is_sensitive;
    tbl->column[i].left_text_column=tbl->column[i-ncols].left_text_column;
    tbl->column[i].right_text_column=tbl->column[i-ncols].right_text_column;
    tbl->column[i].justification=tbl->column[i-ncols].justification;
    if(auxcol.is_visible) tbl->column[i].left_xpixel+=ncols*DEFAULT_COLUMN_WIDTH;
    tbl->column[i-ncols]=auxcol;
  }

  if(col <= tbl->maxalloccol){
   
    GrowSheet(tbl,0,ncols);

    for(i=0; i<=tbl->maxallocrow; i++){
      for(j=tbl->maxalloccol; j>=col+ncols; j--){
        gtk_sheet_real_cell_clear(tbl, i, j, TRUE);
        tbl->data[i][j]=tbl->data[i][j-ncols];
        if(tbl->data[i][j]) tbl->data[i][j]->col=j;
        tbl->data[i][j-ncols]=NULL;
      }
    }
  }
  gtk_sheet_recalc_left_xpixels(tbl, 0);
  return TRUE;
}

static gint
DeleteRow(GtkSheet *tbl, gint row, gint nrows)
{
  GtkSheetCell **auxdata = NULL;
  gint i,j;

  if(nrows <= 0) return TRUE;

  nrows=MIN(nrows,tbl->maxrow-row+1);

  for(i=row; i<=tbl->maxrow-nrows; i++){
    if(tbl->row[i].name){
            g_free(tbl->row[i].name);
            tbl->row[i].name = NULL;
    }
    if(tbl->row[i].button.label){
            g_free(tbl->row[i].button.label);
            tbl->row[i].button.label = NULL;
    }
            
    tbl->row[i]=tbl->row[i+nrows];
    tbl->row[i].is_visible=tbl->row[i+nrows].is_visible;
    tbl->row[i].is_sensitive=tbl->row[i+nrows].is_sensitive;
  }

  if(row <= tbl->maxallocrow){

    for(i=row; i<=tbl->maxrow-nrows; i++){
      if(i<=tbl->maxallocrow){
        auxdata=tbl->data[i];
        for(j=0; j<=tbl->maxalloccol; j++){
              gtk_sheet_real_cell_clear(tbl, i, j, TRUE);
        }
      }
      if(i+nrows<=tbl->maxallocrow){
        tbl->data[i]=tbl->data[i+nrows];
        tbl->data[i+nrows]=auxdata;
        for(j=0; j<=tbl->maxalloccol; j++){
            if(tbl->data[i][j]) tbl->data[i][j]->row=i;
        }
      }
    }

    for(i=tbl->maxallocrow-nrows+1; i<=tbl->maxallocrow; i++){
           if(i > 0 && tbl->data[i]){
                g_free(tbl->data[i]);
                tbl->data[i] = NULL;
           }
    }

    tbl->maxallocrow-=MIN(nrows,tbl->maxallocrow-row);

  }

  tbl->maxrow-=nrows;
  gtk_sheet_recalc_top_ypixels(tbl, 0);
  return TRUE;
} 

static gint
DeleteColumn(GtkSheet *tbl, gint column, gint ncols)
{
  gint i,j;
  GtkSheetColumn auxcol;

  ncols=MIN(ncols,tbl->maxcol-column+1);

  if(ncols <= 0) return TRUE;

  for(i=column; i<=tbl->maxcol-ncols; i++){
    auxcol=tbl->column[i];
    if(tbl->column[i].name){
             g_free(tbl->column[i].name);
             tbl->column[i].name = NULL;
    }
    if(tbl->column[i].button.label){
             g_free(tbl->column[i].button.label);
             tbl->column[i].button.label = NULL;
    }

    tbl->column[i]=tbl->column[i+ncols];    
    tbl->column[i].is_visible=tbl->column[i+ncols].is_visible;
    tbl->column[i].is_sensitive=tbl->column[i+ncols].is_sensitive;
    tbl->column[i].left_text_column=tbl->column[i+ncols].left_text_column;
    tbl->column[i].right_text_column=tbl->column[i+ncols].right_text_column;
    tbl->column[i].justification=tbl->column[i+ncols].justification;
/*    tbl->column[tbl->maxcol-(i-column)]=auxcol;
*/  
  }

  if(column <= tbl->maxalloccol){

    for(i=column; i<=tbl->maxcol-ncols; i++){
      if(i<=tbl->maxalloccol){
        for(j=0; j<=tbl->maxallocrow; j++){
              gtk_sheet_real_cell_clear(tbl, j, i, TRUE);
              if(i+ncols <= tbl->maxalloccol){
                  tbl->data[j][i] = tbl->data[j][i+ncols];
                  tbl->data[j][i+ncols] = NULL;
	          if(tbl->data[j][i]) tbl->data[j][i]->col=i;
              }
        }
      }

    }

    tbl->maxalloccol-=MIN(ncols,tbl->maxalloccol-column);
  }
  tbl->maxcol-=ncols;
  gtk_sheet_recalc_left_xpixels(tbl, 0);
  return TRUE;
}  

static gint
GrowSheet(GtkSheet *tbl, gint newrows, gint newcols)
{
  gint i,j;
  gint inirow, inicol;

  inirow = tbl->maxallocrow + 1;  
  inicol = tbl->maxalloccol + 1;  

  tbl->maxalloccol = tbl->maxalloccol + newcols;
  tbl->maxallocrow = tbl->maxallocrow + newrows;

  if(newrows>0){
      tbl->data= (GtkSheetCell***)
                 g_realloc(tbl->data,(tbl->maxallocrow+1)*sizeof(GtkSheetCell **)+sizeof(double));

      for(i=inirow; i<= tbl->maxallocrow; i++){
        tbl->data[i]= (GtkSheetCell **) \
                       g_malloc((tbl->maxcol+1)*sizeof(GtkSheetCell *)+sizeof(double));
        for(j=0; j<inicol; j++) {
          tbl->data[i][j] = NULL;
        }
      }
          
  }

  if(newcols>0){
      for(i=0; i<= tbl->maxallocrow; i++) {
        tbl->data[i]= (GtkSheetCell **) \
                       g_realloc(tbl->data[i],(tbl->maxalloccol+1)*sizeof(GtkSheetCell *)+sizeof(double));
        for(j=inicol; j <= tbl->maxalloccol; j++) {
          tbl->data[i][j] = NULL;
	}
      }
  }

  return(0);
}	   

static gint
CheckBounds(GtkSheet *tbl, gint row, gint col)
{
  gint newrows=0,newcols=0;

  if(col>tbl->maxalloccol) newcols=col-tbl->maxalloccol;
  if(row>tbl->maxallocrow) newrows=row-tbl->maxallocrow;
  if(newrows>0 || newcols>0) GrowSheet(tbl, newrows, newcols);
  return(0);
} 

/********************************************************************
 * Container Functions:
 * gtk_sheet_add
 * gtk_sheet_put
 * gtk_sheet_attach
 * gtk_sheet_remove
 * gtk_sheet_move_child
 * gtk_sheet_position_child
 * gtk_sheet_position_children 
 * gtk_sheet_realize_child
 * gtk_sheet_get_child_at
 ********************************************************************/ 

GtkSheetChild *
gtk_sheet_put(GtkSheet *sheet, GtkWidget *child, gint x, gint y)
{
  GtkRequisition child_requisition;
  GtkSheetChild *child_info;

  g_return_val_if_fail(sheet != NULL, NULL);
  g_return_val_if_fail(GTK_IS_SHEET(sheet), NULL);
  g_return_val_if_fail(child != NULL, NULL);
  g_return_val_if_fail(child->parent == NULL, NULL);

  child_info = g_new (GtkSheetChild, 1);
  child_info->widget = child;
  child_info->x = x;  
  child_info->y = y;
  child_info->window = NULL;  
  child_info->attached_to_cell = FALSE;

  sheet->children = g_list_append(sheet->children, child_info);

  gtk_widget_set_parent (child, GTK_WIDGET(sheet));

  gtk_widget_size_request(child, &child_requisition);

  if (GTK_WIDGET_VISIBLE(GTK_WIDGET(sheet)))
    {

       if(GTK_WIDGET_REALIZED(GTK_WIDGET(sheet)) && 
          !GTK_WIDGET_REALIZED(child))
        gtk_sheet_realize_child(sheet, child_info);

       if(GTK_WIDGET_MAPPED(GTK_WIDGET(sheet)) && 
          !GTK_WIDGET_MAPPED(child))
        gtk_widget_map(child);
    }

  gtk_sheet_position_child(sheet, child_info);

/* This will avoid drawing on the titles */

  if(GTK_WIDGET_REALIZED(GTK_WIDGET(sheet)))
   {
      if(GTK_SHEET_ROW_TITLES_VISIBLE(sheet))
             gdk_window_show(sheet->row_title_window);
      if(GTK_SHEET_COL_TITLES_VISIBLE(sheet))
             gdk_window_show(sheet->column_title_window);
   }

  return (child_info);
}

void
gtk_sheet_attach		(GtkSheet *sheet, 
				 GtkWidget *widget, 
				 gint row, gint col,
               			 gfloat x_align, gfloat y_align)
{
  GdkRectangle area;
  GtkSheetChild *child;

  if(row < 0 || col < 0){
    gtk_sheet_button_attach(sheet, widget, row, col, x_align, y_align);
    return;
  }
 
  gtk_sheet_get_cell_area(sheet, row, col, &area);  

  child = gtk_sheet_put(sheet, widget, area.x, area.y);

  child->attached_to_cell = TRUE;
  child->row = row;
  child->col = col;
  child->x_align = x_align;
  child->y_align = y_align;
}

void
gtk_sheet_button_attach		(GtkSheet *sheet, 
				 GtkWidget *widget, 
				 gint row, gint col,
               			 gfloat x_align, gfloat y_align)
{
  GtkSheetButton *button;
  GtkSheetChild *child;
  GtkRequisition button_requisition;

  if(row >= 0 && col >= 0) return;
  if(row < 0 && col < 0) return;

  child = g_new (GtkSheetChild, 1);
  child->widget = widget;
  child->x = 0;  
  child->y = 0;  
  child->window = NULL;  
  child->attached_to_cell = TRUE;
  child->row = row;
  child->col = col;
  child->x_align = x_align;
  child->y_align = y_align;

  if(row == -1){
     button = &sheet->column[col].button;
     button->child = child;
  }
  else
  {
     button = &sheet->row[row].button;
     button->child = child;
  }

  sheet->children = g_list_append(sheet->children, child);

  gtk_widget_set_parent (widget, GTK_WIDGET(sheet));
  gtk_sheet_button_size_request(sheet, button, &button_requisition);

  if(row == -1){
       if(button_requisition.height > sheet->column_title_area.height)
             sheet->column_title_area.height = button_requisition.height; 
       if(button_requisition.width > sheet->column[col].width)
             sheet->column[col].width = button_requisition.width; 
  }

  if(col == -1){
       if(button_requisition.width > sheet->row_title_area.width)
             sheet->row_title_area.width = button_requisition.width; 
       if(button_requisition.height > sheet->row[row].height)
             sheet->row[row].height = button_requisition.height; 
  }

  if (GTK_WIDGET_VISIBLE(GTK_WIDGET(sheet)))
    {

       if(GTK_WIDGET_REALIZED(GTK_WIDGET(sheet)) && 
          !GTK_WIDGET_REALIZED(widget))
        gtk_sheet_realize_child(sheet, child);

       if(GTK_WIDGET_MAPPED(GTK_WIDGET(sheet)) && 
          !GTK_WIDGET_MAPPED(widget))
        gtk_widget_map(widget);
    }

  if(row == -1) size_allocate_column_title_buttons(sheet);
  if(col == -1) size_allocate_row_title_buttons(sheet);

}

static void
gtk_sheet_button_size_request	(GtkSheet *sheet,
                                 GtkSheetButton *button, 
                                 GtkRequisition *button_requisition)
{
  GtkRequisition requisition;

  if(button->child)
  {
     gtk_widget_size_request(button->child->widget, &requisition);
  }
  else
  {
     requisition.height = DEFAULT_ROW_HEIGHT(GTK_WIDGET(sheet));
     requisition.width = COLUMN_MIN_WIDTH;
  }

  *button_requisition = requisition;
  button_requisition->width += sheet->button->style->klass->xthickness;
  button_requisition->height += sheet->button->style->klass->ythickness;

  
}

void
gtk_sheet_move_child(GtkSheet *sheet, GtkWidget *widget, gint x, gint y)
{
  GtkSheetChild *child;
  GList *children;

  g_return_if_fail(sheet != NULL);
  g_return_if_fail(GTK_IS_SHEET(sheet));

  children = sheet->children;
  while(children)
    {
       child = children->data;

       if(child->widget == widget){
         child->x = x;
         child->y = y;
         child->row = ROW_FROM_YPIXEL(sheet, y);
	 child->col = COLUMN_FROM_XPIXEL(sheet, x);
         gtk_sheet_position_child(sheet, child);
         return;
       }

       children = children->next;
    }

  g_warning("Widget must be a GtkSheet child"); 

}

static void
gtk_sheet_position_child(GtkSheet *sheet, GtkSheetChild *child)
{
   GtkRequisition child_requisition;
   gint xoffset = 0; 
   gint yoffset = 0;
   gint x = 0, y = 0;
   gint width = 0, height = 0;   

   gtk_widget_get_child_requisition(child->widget, &child_requisition);

   if(GTK_SHEET_COL_TITLES_VISIBLE(sheet)) 
             yoffset = sheet->column_title_area.height;

   if(GTK_SHEET_ROW_TITLES_VISIBLE(sheet))
             xoffset = sheet->row_title_area.width;

   if(child->attached_to_cell){
      child->x = COLUMN_LEFT_XPIXEL(sheet, child->col);
      child->y = ROW_TOP_YPIXEL(sheet, child->row);

      if(GTK_SHEET_ROW_TITLES_VISIBLE(sheet)) 
                                    child->x-=sheet->row_title_area.width;
      if(GTK_SHEET_COL_TITLES_VISIBLE(sheet)) 
                                    child->y-=sheet->column_title_area.height;

      width = sheet->column[child->col].width;
      height = sheet->row[child->row].height;

      child->x += (width - child_requisition.width) * child->x_align; 
      child->y += (height - child_requisition.height) * child->y_align;
      x = child->widget->allocation.x = child->x + xoffset;   
      y = child->widget->allocation.y = child->y + yoffset;
   }
   else
   {
      x = child->widget->allocation.x = child->x + sheet->hoffset + xoffset;   
      y = child->widget->allocation.y = child->y + sheet->voffset + yoffset;
   }

   child->widget->allocation.width = child_requisition.width;
   child->widget->allocation.height = child_requisition.height;

   if(GTK_WIDGET_NO_WINDOW(child->widget))
     {
        child->widget->allocation.x = 0;
        child->widget->allocation.y = 0;
     }

   if(GTK_WIDGET_REALIZED(GTK_WIDGET(sheet)) &&
      GTK_WIDGET_MAPPED(child->widget))
         {
              gtk_widget_size_allocate(child->widget, 
                                       &child->widget->allocation);
              if(GTK_WIDGET_NO_WINDOW(child->widget) && child->window)
                 {
		    gdk_window_move_resize(child->window,
                                           x, y,
                                           child->widget->allocation.width,
					   child->widget->allocation.height);
                    gtk_widget_draw(child->widget, NULL);
                 }
         }

}

static void
gtk_sheet_position_children(GtkSheet *sheet)
{
  GList *children;
  GtkSheetChild *child;

  children = sheet->children;

  while(children)
   {
     child = (GtkSheetChild *)children->data;

     if(child->col !=-1 && child->row != -1)
           gtk_sheet_position_child(sheet, child);

     if(child->row == -1){
        if(child->col < MIN_VISIBLE_COLUMN(sheet) || 
           child->col > MAX_VISIBLE_COLUMN(sheet))
              gtk_sheet_child_hide(child);
        else
              gtk_sheet_child_show(child);
     }
     if(child->col == -1){
        if(child->row < MIN_VISIBLE_ROW(sheet) ||
           child->row > MAX_VISIBLE_ROW(sheet))
              gtk_sheet_child_hide(child);
        else
              gtk_sheet_child_show(child);
     }
 
     children = children->next;
   }
    
}

static void
gtk_sheet_remove (GtkContainer *container, GtkWidget *widget)
{
  GtkSheet *sheet;
  GList *children;
  GtkSheetChild *child;

  g_return_if_fail(container != NULL);
  g_return_if_fail(GTK_IS_SHEET(container));

  sheet = GTK_SHEET(container);

  children = sheet->children;

  while(children)
   {
     child = (GtkSheetChild *)children->data;

     if(child->widget == widget) break;

     children = children->next;
   }

  if (children)
   {
     if(child->row == -1)
        sheet->row[child->row].button.child = NULL;

     if(child->col == -1)
        sheet->column[child->col].button.child = NULL;

     if(child->window) gdk_window_destroy(child->window);

     gtk_widget_unparent (widget);
     child->widget = NULL;

     sheet->children = g_list_remove_link (sheet->children, children);
     g_list_free_1 (children);
   }

}

static void
gtk_sheet_realize_child(GtkSheet *sheet, GtkSheetChild *child)
{
  gint attributes_mask;
  GtkWidget *widget;

  widget = GTK_WIDGET(sheet);

  if (GTK_WIDGET_NO_WINDOW (child->widget))
    {
      GdkWindowAttr attributes;
      
      gint x = child->x - sheet->hoffset;
      gint y = child->y - sheet->voffset;

      attributes.window_type = GDK_WINDOW_CHILD;
      attributes.x = x;
      attributes.y = y;
      attributes.width = child->widget->requisition.width;
      attributes.height = child->widget->requisition.height;
      attributes.wclass = GDK_INPUT_OUTPUT;
      attributes.visual = gtk_widget_get_visual (widget);
      attributes.colormap = gtk_widget_get_colormap (widget);
      attributes.event_mask = GDK_EXPOSURE_MASK;
 
      attributes_mask = GDK_WA_X | GDK_WA_Y | GDK_WA_VISUAL | GDK_WA_COLORMAP;

      if(child->row == -1)
        child->window =  gdk_window_new (sheet->column_title_window,
   	  			         &attributes, attributes_mask);
      else if(child->col == -1)
        child->window =  gdk_window_new (sheet->row_title_window,
   	  			         &attributes, attributes_mask);
      else
        child->window =  gdk_window_new (widget->window,
   	  			         &attributes, attributes_mask);

      if(GTK_IS_PIXMAP(child->widget)){
            gdk_window_shape_combine_mask(child->window, 
                                          GTK_PIXMAP(child->widget)->mask,
                                          0, 0);
      }

      gdk_window_set_user_data (child->window, widget);

      if (child->window)
	gtk_style_set_background (widget->style, 
				  child->window, 
				  GTK_STATE_NORMAL);

      gtk_widget_set_parent_window(child->widget, child->window);
      gdk_window_show(child->window);

    }

    gtk_widget_realize(child->widget);

}


   
GtkSheetChild *
gtk_sheet_get_child_at(GtkSheet *sheet, gint row, gint col)
{
  GList *children;
  GtkSheetChild *child;

  g_return_val_if_fail(sheet != NULL, NULL);
  g_return_val_if_fail(GTK_IS_SHEET(sheet), NULL);

  children = sheet->children;

  while(children)
   {
     child = (GtkSheetChild *)children->data;

     if(child->attached_to_cell)
        if(child->row == row && child->col == col) break; 
     
     children = children->next;
   }

  if(children) return child; 

  return NULL;
}

static void
gtk_sheet_child_hide(GtkSheetChild *child) 
{
  g_return_if_fail(child != NULL);
  gtk_widget_hide(child->widget);
  if(child->window)   
      gdk_window_hide(child->window);
}

static void
gtk_sheet_child_show(GtkSheetChild *child) 
{
  g_return_if_fail(child != NULL);

  gtk_widget_show(child->widget);
  if(child->window)   
      gdk_window_show(child->window);
}
