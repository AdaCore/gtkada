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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <gtk/gtk.h>
#include "gtkplot.h"
#include "gtkplotlayout.h"
#include "gtkpsfont.h"
#include <math.h>

#define DEFAULT_OFFSET 25
#define DEFAULT_WIDTH 150
#define DEFAULT_HEIGHT 120
#define DEFAULT_FONT_HEIGHT 12

static gchar DEFAULT_FONT[] = "Helvetica";

#define GRAPH_MASK    (GDK_EXPOSURE_MASK |		\
                       GDK_POINTER_MOTION_MASK |	\
                       GDK_POINTER_MOTION_HINT_MASK |	\
                       GDK_BUTTON_PRESS_MASK |		\
                       GDK_BUTTON_RELEASE_MASK)
                       
static void gtk_plot_layout_class_init 		(GtkPlotLayoutClass *class);
static void gtk_plot_layout_init 		(GtkPlotLayout *plot_layout);
static void gtk_plot_layout_remove		(GtkContainer *container, 
						 GtkWidget *child);
static void gtk_plot_layout_finalize 		(GtkObject *object);
static void gtk_plot_layout_map			(GtkWidget *widget);
static void gtk_plot_layout_size_request 	(GtkWidget *widget, 
                                                 GtkRequisition *requisition);
static void gtk_plot_layout_draw 		(GtkWidget *widget, 
						 GdkRectangle *area);
static void gtk_plot_layout_paint 		(GtkWidget *widget); 
static gint gtk_plot_layout_expose		(GtkWidget *widget, 
						 GdkEventExpose *event);
static void gtk_plot_layout_create_pixmap	(GtkWidget *widget, 
						 gint width, gint height);
static void gtk_plot_layout_set_plots_pixmap    (GtkPlotLayout *plot_layout);
static void gtk_plot_layout_draw_text		(GtkPlotLayout *layout,
                          			 GtkPlotText text);
static void rotate_text                         (GtkPlotLayout *layout,
                                                 GtkPlotText text,
                                                 gint *width, gint *height,
                                                 GdkPixmap **pixmap,
                                                 GdkBitmap **mask);
static GtkLayoutClass *parent_class = NULL;


guint
gtk_plot_layout_get_type (void)
{
  static GtkType plot_layout_type = 0;

  if (!plot_layout_type)
    {
      GtkTypeInfo plot_layout_info =
      {
	"GtkPlotLayout",
	sizeof (GtkPlotLayout),
	sizeof (GtkPlotLayoutClass),
	(GtkClassInitFunc) gtk_plot_layout_class_init,
	(GtkObjectInitFunc) gtk_plot_layout_init,
	/* reserved 1*/ NULL,
        /* reserved 2 */ NULL,
        (GtkClassInitFunc) NULL,
      };

      plot_layout_type = gtk_type_unique (GTK_TYPE_LAYOUT, &plot_layout_info);
    }
  return plot_layout_type;
}

static void
gtk_plot_layout_class_init (GtkPlotLayoutClass *class)
{
  GtkObjectClass *object_class;
  GtkWidgetClass *widget_class;
  GtkContainerClass *container_class;

  parent_class = gtk_type_class (gtk_layout_get_type ());

  object_class = (GtkObjectClass *) class;
  widget_class = (GtkWidgetClass *) class;
  container_class = (GtkContainerClass *) class;

  object_class->finalize = gtk_plot_layout_finalize;

  widget_class->draw = gtk_plot_layout_draw;
  widget_class->map = gtk_plot_layout_map;
  widget_class->expose_event = gtk_plot_layout_expose;

  container_class->remove = gtk_plot_layout_remove;

  widget_class->size_request = gtk_plot_layout_size_request;

}

static void
gtk_plot_layout_init (GtkPlotLayout *plot_layout)
{
  GtkWidget *widget;
  widget = GTK_WIDGET(plot_layout);

  gdk_color_black(gtk_widget_get_colormap(widget), &widget->style->black);
  gdk_color_white(gtk_widget_get_colormap(widget), &widget->style->white);

  gtk_widget_set_events (widget, gtk_widget_get_events(widget)|
                         GRAPH_MASK);

  plot_layout->num_plots = 0;
  plot_layout->background = widget->style->white;

  plot_layout->plots = NULL;
  plot_layout->text = NULL;
}

static void
gtk_plot_layout_remove(GtkContainer *container, GtkWidget *child)
{
  GtkPlotLayout *layout;
  GList *list;

  layout = GTK_PLOT_LAYOUT(container);

  list = g_list_find(layout->plots, child);
  if(list) {
     layout->plots = g_list_remove(list, child);
     layout->num_plots --;
  }

  GTK_CONTAINER_CLASS(parent_class)->remove(container, child);
}

static void
gtk_plot_layout_draw (GtkWidget *widget, GdkRectangle *area)
{
  gtk_plot_layout_paint(widget);
}

static void
gtk_plot_layout_paint (GtkWidget *widget)
{
  GtkPlotLayout *plot_layout;
  GtkLayout *layout;
  GdkGC *gc;
  GList *text;
  GtkPlotText *child_text;

  plot_layout = GTK_PLOT_LAYOUT(widget);
  layout = GTK_LAYOUT(widget);

  if(!plot_layout->pixmap) return;

  gc = gdk_gc_new(plot_layout->pixmap);
  gdk_gc_set_foreground(gc, &plot_layout->background);

  gdk_draw_rectangle(plot_layout->pixmap,
                     gc,
                     TRUE,
                     0,0,plot_layout->width, plot_layout->height);

  gtk_plot_layout_set_plots_pixmap(plot_layout);

  gdk_draw_pixmap(GTK_LAYOUT(plot_layout)->bin_window,
                  widget->style->fg_gc[GTK_STATE_NORMAL],
                  GTK_PLOT_LAYOUT(widget)->pixmap,
                  layout->xoffset, layout->yoffset, 
                  0, 0, 
                  -1, -1);  

  text = plot_layout->text;
  while(text)
   {
     child_text = (GtkPlotText *) text->data;
     gtk_plot_layout_draw_text(plot_layout, *child_text);
     text = text->next;
   }

  gdk_gc_unref(gc);
}

void
gtk_plot_layout_refresh(GtkPlotLayout *layout)
{
  GList *plots;
  GList *text;
  GtkPlot *plot;
  GdkRectangle area;
  GdkGC *gc;
  GtkPlotText *child_text;

  gc = gdk_gc_new(layout->pixmap);
  gdk_gc_set_foreground(gc, &layout->background);

  gdk_draw_rectangle(layout->pixmap,
                     gc,
                     TRUE,
                     0,0,layout->width, layout->height);

  plots = layout->plots; 
  while(plots) 
    {
      plot = GTK_PLOT(plots->data);
      gtk_plot_set_drawable(plot, layout->pixmap);
      area.x = GTK_WIDGET(plot)->allocation.x+GTK_LAYOUT(layout)->xoffset;
      area.y = GTK_WIDGET(plot)->allocation.y+GTK_LAYOUT(layout)->yoffset;
      area.width = GTK_WIDGET(plot)->allocation.width;	
      area.height = GTK_WIDGET(plot)->allocation.height;	
      gtk_plot_paint(plot, &area);
      plots = plots->next;
    }


  text = layout->text;
  while(text)
   {
     child_text = (GtkPlotText *) text->data;
     gtk_plot_layout_draw_text(layout, *child_text);
     text = text->next;
   }


  gdk_draw_pixmap(GTK_LAYOUT(layout)->bin_window,
                  GTK_WIDGET(layout)->style->fg_gc[GTK_STATE_NORMAL],
                  layout->pixmap,
                  GTK_LAYOUT(layout)->xoffset, GTK_LAYOUT(layout)->yoffset,
                  0, 0,
                  -1, -1);

  gdk_gc_unref(gc);
}


void
gtk_plot_layout_set_size(GtkPlotLayout *layout, gint width, gint height)
{
  layout->width = width;
  layout->height = height;
  GTK_LAYOUT(layout)->width = width;
  GTK_LAYOUT(layout)->height = height;

  if(GTK_WIDGET_MAPPED(layout))
  {
    gdk_window_clear_area(GTK_LAYOUT(layout)->bin_window, 0, 0, -1, -1);
    gtk_layout_set_size(GTK_LAYOUT(layout), width, height);
    gtk_plot_layout_create_pixmap(GTK_WIDGET(layout), width, height);
    gtk_plot_layout_paint(GTK_WIDGET(layout));
  }
}

static void 
gtk_plot_layout_create_pixmap(GtkWidget *widget, gint width, gint height)
{
  GtkPlotLayout *plot_layout;
  GdkGC* gc;
  gint pixmap_width, pixmap_height;

  plot_layout=GTK_PLOT_LAYOUT(widget);
  if (!plot_layout->pixmap)
    plot_layout->pixmap = gdk_pixmap_new (widget->window,
				    width,
				    height, -1);
  else{
    gdk_window_get_size(plot_layout->pixmap, &pixmap_width, &pixmap_height);
    if(width != pixmap_width || height != pixmap_height)        
        gdk_pixmap_unref(plot_layout->pixmap);
        plot_layout->pixmap = gdk_pixmap_new (widget->window,
				        width,
				        height, -1);
  }

  gc = gdk_gc_new(plot_layout->pixmap);
  gdk_gc_set_foreground(gc, &plot_layout->background);

  gdk_draw_rectangle(plot_layout->pixmap,
                     gc,
                     TRUE,
                     0, 0, plot_layout->width, plot_layout->height);

  gtk_plot_layout_set_plots_pixmap(plot_layout);

  gdk_gc_unref(gc);
}


static void
gtk_plot_layout_map(GtkWidget *widget)
{
  GtkPlotLayout *plot_layout;
  GtkLayout *layout;

  plot_layout=GTK_PLOT_LAYOUT(widget);
  layout=GTK_LAYOUT(widget);

  GTK_WIDGET_CLASS(parent_class)->map(widget);

  if(!plot_layout->pixmap){
      gtk_plot_layout_create_pixmap(widget, plot_layout->width, plot_layout->height);
      gtk_plot_layout_paint(widget);
      return;
  }

  gtk_plot_layout_refresh(plot_layout);

}

static gint
gtk_plot_layout_expose(GtkWidget *widget, GdkEventExpose *event)
{
  GtkPlotLayout *plot_layout;
  GtkLayout *layout;
  GdkPixmap *pixmap;
  GList *text;
  GtkPlotText *child_text;

  if(!GTK_WIDGET_DRAWABLE(widget)) return FALSE;

  plot_layout=GTK_PLOT_LAYOUT(widget);
  layout=GTK_LAYOUT(widget);

  if(!plot_layout->pixmap){
      gtk_plot_layout_create_pixmap(widget, plot_layout->width, plot_layout->height);
      gtk_plot_layout_paint(widget);
      return FALSE;
  }

  pixmap = plot_layout->pixmap;
  gdk_draw_pixmap(GTK_LAYOUT(plot_layout)->bin_window,
                  widget->style->fg_gc[GTK_STATE_NORMAL],
                  pixmap,
                  layout->xoffset+event->area.x, layout->yoffset+event->area.y, 
                  event->area.x, event->area.y, 
                  event->area.width, event->area.height); 

  text = plot_layout->text;
  while(text)
   {
     child_text = (GtkPlotText *) text->data;
     gtk_plot_layout_draw_text(plot_layout, *child_text);
     text = text->next;
   }

  return FALSE;
}


static void
gtk_plot_layout_size_request (GtkWidget *widget, GtkRequisition *requisition)
{
  GtkPlotLayout *plot_layout;

  plot_layout=GTK_PLOT_LAYOUT(widget);

  GTK_WIDGET_CLASS(parent_class)->size_request(widget, requisition);

  widget->requisition.width = MAX(plot_layout->width, requisition->width);
  widget->requisition.height = MAX(plot_layout->height, requisition->height);

}

GtkWidget*
gtk_plot_layout_new (gint width, gint height)
{
  GtkPlotLayout *plot_layout;

  plot_layout = gtk_type_new (gtk_plot_layout_get_type ());
  plot_layout->width = width;
  plot_layout->height = height;
  return GTK_WIDGET (plot_layout);
}


static void
gtk_plot_layout_finalize (GtkObject *object)
{
  GtkPlotLayout *plot_layout;
  GList *list;

  g_return_if_fail (object != NULL);
  g_return_if_fail (GTK_IS_PLOT_LAYOUT (object));

  plot_layout = GTK_PLOT_LAYOUT (object);

  list = plot_layout->text;
  while(list){
    GtkPlotText *text;

    text = (GtkPlotText *) list->data;
    if(text->text) g_free(text->text);
    if(text->font) g_free(text->font);

    g_free(text);
    plot_layout->text = g_list_remove_link(plot_layout->text, list);
    g_list_free_1(list);
    list = plot_layout->text;
  }

  if (GTK_OBJECT_CLASS (parent_class)->finalize)
    (*GTK_OBJECT_CLASS (parent_class)->finalize) (object);
}

void
gtk_plot_layout_add_plot(GtkPlotLayout *plot_layout, GtkPlot *plot, gint x, gint y)
{
  plot_layout->num_plots += 1;

  plot_layout->plots = g_list_append(plot_layout->plots, plot);
 
  gtk_layout_put(GTK_LAYOUT(plot_layout), GTK_WIDGET(plot), x, y);

  gtk_plot_layout_set_plots_pixmap(plot_layout);
}

static void
gtk_plot_layout_set_plots_pixmap(GtkPlotLayout *plot_layout)
{
  GdkRectangle area;
  GList *plots;
  GtkPlot *plot;
  GList *text;
  GtkPlotText *child_text;

  if(!plot_layout->pixmap) return;
  plots = plot_layout->plots; 
  while(plots) 
    {
      plot = GTK_PLOT(plots->data);
      gtk_plot_set_drawable(plot, plot_layout->pixmap);
      area.x = GTK_WIDGET(plot)->allocation.x+GTK_LAYOUT(plot_layout)->xoffset;
      area.y = GTK_WIDGET(plot)->allocation.y+GTK_LAYOUT(plot_layout)->yoffset;
      area.width = GTK_WIDGET(plot)->allocation.width;	
      area.height = GTK_WIDGET(plot)->allocation.height;	
      gtk_widget_draw(GTK_WIDGET(plot), &area);
      plots = plots->next;
    }

  text = plot_layout->text;
  while(text)
   {
     child_text = (GtkPlotText *) text->data;
     gtk_plot_layout_draw_text(plot_layout, *child_text);
     text = text->next;
   }

}

void
gtk_plot_layout_set_background (GtkPlotLayout *layout, GdkColor *color)
{
  
  g_return_if_fail (layout != NULL);
  g_return_if_fail (GTK_IS_PLOT_LAYOUT (layout));

  layout->background = *color;

  if(GTK_WIDGET_REALIZED(GTK_WIDGET(layout)))
       gtk_widget_queue_draw(GTK_WIDGET(layout));
}

GtkPlotText *
gtk_plot_layout_put_text (GtkPlotLayout *layout, 
                          gdouble x, gdouble y, gint angle,
                          const gchar *font, gint height,
                          GdkColor *fg, GdkColor *bg, 
			  GtkJustification justification,
                          const gchar *text)
{
  GtkWidget *widget;
  GtkPlotText *text_attr;

  widget = GTK_WIDGET(layout);

  text_attr = g_new(GtkPlotText, 1);

  text_attr->x = x;
  text_attr->y = y;
  text_attr->angle = angle;
  text_attr->fg = widget->style->black;
  text_attr->bg = widget->style->white;
  text_attr->justification = justification;

  if(!font) {
    text_attr->font = g_strdup(DEFAULT_FONT);
    text_attr->height = DEFAULT_FONT_HEIGHT;
  } else {
    text_attr->font = g_strdup(font);
    text_attr->height = height;
  }

  text_attr->text = NULL;
  if(text) text_attr->text = g_strdup(text);

  if(fg != NULL)
    text_attr->fg = *fg;

  text_attr->transparent = TRUE;
  if(bg != NULL){
    text_attr->bg = *bg;
    text_attr->transparent = FALSE;
  }

  layout->text = g_list_append(layout->text, text_attr);
  gtk_plot_layout_draw_text(layout, *text_attr);

  return text_attr;
}

gint
gtk_plot_layout_remove_text(GtkPlotLayout *layout, GtkPlotText *text)
{
  GList *list;
  gpointer data;

  list = layout->text;

  while(list)
   {
     data = list->data;
    
     if((GtkPlotText *)data == text){
              layout->text = g_list_remove_link(layout->text, list);
              g_list_free_1(list);
              return TRUE;
     }
     list = list->next;
   }

   return FALSE;
}

static void
gtk_plot_layout_draw_text(GtkPlotLayout *plot_layout,
                          GtkPlotText text)
{
  GdkPixmap *text_pixmap;
  GdkBitmap *text_mask;
  GtkLayout *layout;
  GdkGC *gc;
  GdkColormap *colormap;
  gint x, y;
  gint width, height;
  gint ascent, descent;

  if(plot_layout->pixmap == NULL) return;
  layout = GTK_LAYOUT(plot_layout);

  x = text.x * plot_layout->width;
  y = text.y * plot_layout->height;

  gtk_plot_text_get_size(text, 1.0, &width, &height, &ascent, &descent);

  switch(text.justification){
    case GTK_JUSTIFY_LEFT:
      switch(text.angle){
        case 0:
            y -= ascent;
            break;
        case 90:
            y -= height;
            x -= ascent;
            break;
        case 180:
            x -= width;
            y -= descent;
            break;
        case 270:
            x -= descent;
            break;
      }
      break;
    case GTK_JUSTIFY_RIGHT:
      switch(text.angle){
        case 0:
            x -= width;
            y -= ascent;
            break;
        case 90:
            x -= ascent;
            break;
        case 180:
            y -= descent;
            break;
        case 270:
            y -= height;
            x -= descent;
            break;
      }
      break;
    case GTK_JUSTIFY_CENTER:
    default:
      switch(text.angle){
        case 0:
            x -= width / 2.;
            y -= ascent;
            break;
        case 90:
            x -= ascent;
            y -= height / 2.;
            break;
        case 180:
            x -= width / 2.;
            y -= descent;
            break;
        case 270:
            x -= descent;
            y -= height / 2.;
            break;
      }
  }

  rotate_text(plot_layout, text, &width, &height, &text_pixmap, &text_mask);

  colormap = gtk_widget_get_colormap (GTK_WIDGET(layout));
  gc = gdk_gc_new(plot_layout->pixmap);

  if(text.transparent){
    gdk_gc_set_clip_mask (gc, text_mask);
    gdk_gc_set_clip_origin (gc, x, y);
  }


  gdk_draw_pixmap(plot_layout->pixmap, gc,
                  text_pixmap, 0, 0,
                  x, y, -1, -1);

  gdk_draw_pixmap(layout->bin_window,
                  GTK_WIDGET(layout)->style->fg_gc[GTK_STATE_NORMAL],
                  plot_layout->pixmap,
                  layout->xoffset + x, layout->yoffset + y, 
                  x, y, 
                  width, height); 

  gdk_pixmap_unref(text_pixmap);
  gdk_bitmap_unref(text_mask);
  gdk_gc_unref(gc);
}

static void
rotate_text(GtkPlotLayout *layout,
            GtkPlotText text,
            gint *width, gint *height,
            GdkPixmap **new_pixmap, GdkBitmap **mask)
{
  GdkWindow *window;
  GdkPixmap *old_pixmap;
  GdkImage *image;
  GdkGC *gc, *mask_gc;
  GdkColormap *colormap;
  GdkColorContext *cc;
  GdkVisual *visual;
  GdkColor color, black, mask_color;
  GdkFont *font;
  GtkPSFont *psfont, *tmp_font;
  GList *family = NULL;
  gint x, y, y0;
  gint old_width, old_height;
  gboolean bold, italic;
  gchar *aux, subs[2];
  gint fontsize;
  gint ascent, descent;
  gint numf;
  gint xp = 0, yp = 0;
  gchar *lastchar = NULL;

  window = GTK_WIDGET(layout)->window;
  colormap = gtk_widget_get_colormap (GTK_WIDGET(layout));
  visual = gtk_widget_get_visual (GTK_WIDGET(layout));
  cc = gdk_color_context_new(visual, colormap);
  gc = gdk_gc_new (window);

  gtk_plot_text_get_size(text, 1.0, width, height, &ascent, &descent);
  old_width = *width;
  old_height = *height;
  if(text.angle == 90 || text.angle == 270)
    {
      old_width = *height;
      old_height = *width;
    }

  gtk_psfont_get_families(&family, &numf);
  font = gtk_psfont_get_gdkfont(text.font, text.height);
  psfont = gtk_psfont_get_font(text.font);
  tmp_font = psfont;
  italic = psfont->italic;
  bold = psfont->bold;
  fontsize = text.height;
  x = 0;
  y0 = y = ascent;
  aux = text.text;

  old_pixmap = gdk_pixmap_new(window, old_width, old_height, -1);
  gdk_color_white (colormap, &color);
  gdk_gc_set_foreground(gc, &color);
  gdk_draw_rectangle(old_pixmap, gc, TRUE,
                     0, 0, -1, -1);
  gdk_color_black (colormap, &black);
  gdk_gc_set_foreground(gc, &black);

  while(aux && *aux != '\0' && *aux != '\n'){
   if(*aux == '\\'){
     aux++;
     switch(*aux){
       case '0': case '1': case '2': case '3':
       case '4': case '5': case '6': case '7': case '9':
           tmp_font = gtk_psfont_find_by_family((gchar *)g_list_nth_data(family, atoi(aux)), italic, bold);
           font = gtk_psfont_get_gdkfont(tmp_font->psname, fontsize);
           aux++;
           break;
       case '8': case 'g':
           tmp_font = gtk_psfont_find_by_family("Symbol", italic, bold);
           font = gtk_psfont_get_gdkfont(tmp_font->psname, fontsize);
           aux++;
           break;
       case 'B':
           bold = TRUE;
           tmp_font = gtk_psfont_find_by_family(tmp_font->family, italic, bold);           font = gtk_psfont_get_gdkfont(tmp_font->psname, fontsize);
           aux++;
           break;
       case 'i':
           italic = TRUE;
           tmp_font = gtk_psfont_find_by_family(tmp_font->family, italic, bold);           font = gtk_psfont_get_gdkfont(tmp_font->psname, fontsize);
           aux++;
           break;
       case 'S': case '^':
           fontsize = (int)((gdouble)fontsize * 0.6 + 0.5);
           font = gtk_psfont_get_gdkfont(tmp_font->psname, fontsize);
           y -= font->ascent;
           aux++;
           break;
       case 's': case '_':
           fontsize = (int)((gdouble)fontsize * 0.6 + 0.5);
           font = gtk_psfont_get_gdkfont(tmp_font->psname, fontsize);
           y += font->descent;
           aux++;
           break;
       case '+':
           fontsize += 3;
           font = gtk_psfont_get_gdkfont(tmp_font->psname, fontsize);
           aux++;
           break;
       case '-':
           fontsize -= 3;
           font = gtk_psfont_get_gdkfont(tmp_font->psname, fontsize);
           aux++;
           break;
       case 'N':
/*
           tmp_font = psfont;
*/
           font = gtk_psfont_get_gdkfont(tmp_font->psname, text.height);
           y = y0;
           italic = psfont->italic;
           bold = psfont->bold;
           fontsize = text.height;
           aux++;
           break;
       case 'b':
           if(lastchar){
              x -= gdk_char_width_wc (font, *lastchar);
              if(lastchar == text.text)
                 lastchar = NULL;
              else
                 lastchar--;
           } else {
              x -= gdk_char_width_wc (font, 'X');
           }
           aux++;
           break;
       default:
           if(aux && *aux != '\0' && *aux !='\n'){
             subs[0] = *aux;
             subs[1] = '\0';
             gdk_draw_string (old_pixmap, font,
                              gc,
                              x, y,
                              subs);
             x += gdk_char_width_wc (font, *aux);
             lastchar = aux;
             aux++;
           }
           break;
     }
   } else {
     if(aux && *aux != '\0' && *aux !='\n'){
       subs[0] = *aux;
       subs[1] = '\0';
       gdk_draw_string (old_pixmap, font,
                        gc,
                        x, y,
                        subs);

       x += gdk_char_width_wc (font, *aux);
       lastchar = aux;
       aux++;
     }
   }
  }

  image = gdk_image_get(old_pixmap, 0, 0, old_width, old_height);

  *new_pixmap = gdk_pixmap_new(window, *width, *height, -1);
  gdk_gc_set_foreground(gc, &text.bg);
  gdk_draw_rectangle(*new_pixmap, gc, TRUE,
                     0, 0, -1, -1);

  *mask = gdk_pixmap_new(window, *width, *height, 1);
  mask_gc = gdk_gc_new(*mask);
  mask_color.pixel = 0;
  gdk_gc_set_foreground(mask_gc, &mask_color);
  gdk_draw_rectangle(*mask, mask_gc, TRUE,
                     0, 0, -1, -1);

  mask_color.pixel = 1;

  gdk_gc_set_foreground(gc, &text.fg);
  gdk_gc_set_foreground(mask_gc, &mask_color);

  for(y = 0; y < old_height; y++)
    for(x = 0; x < old_width; x++)
       {
         color.pixel = gdk_image_get_pixel(image, x, y);
         gdk_color_context_query_color(cc, &color);
         if(gdk_color_equal(&color, &black)){
         switch(text.angle){
          case 0:
              xp = x;
              yp = y;
              break;
          case 90:
              xp = y;
              yp = old_width - x;
              break;
          case 180:
              xp = old_width - x;
              yp = old_height - y;
              break;
          case 270:
              xp = old_height - y;
              yp = x;
              break;
          }
          gdk_draw_point(*new_pixmap, gc, xp, yp);
          gdk_draw_point(*mask, mask_gc, xp, yp);
         }
       }

  gdk_font_unref(font);
  gdk_gc_unref(gc);
  gdk_gc_unref(mask_gc);
  gdk_color_context_free(cc);
  gdk_image_destroy(image);

  return;

}


void
gtk_plot_layout_get_pixel(GtkPlotLayout *plot_layout, gdouble px, gdouble py,
                          gint *x, gint *y)
{
  *x = plot_layout->width * px;
  *y = plot_layout->height * py;
}

void
gtk_plot_layout_get_position(GtkPlotLayout *plot_layout, gint x, gint y,
                             gdouble *px, gdouble *py)
{
  *px = (gdouble) x / (gdouble) plot_layout->width;
  *py = (gdouble) y / (gdouble) plot_layout->height;
}

