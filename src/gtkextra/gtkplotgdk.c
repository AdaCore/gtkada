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
#include "gtkplotgdk.h"
#include "gtkplot.h"
#include "gtkpsfont.h"
#include "gtkplotcanvas.h"

static void gtk_plot_gdk_init 			    (GtkPlotGdk *pc);
static void gtk_plot_gdk_class_init                 (GtkPlotGdkClass *klass);
static void gtk_plot_gdk_finalize 		    (GtkObject *object);
static gboolean gtk_plot_gdk_real_init 		    (GtkPlotPC *pc);
static void gtk_plot_gdk_leave 		    	    (GtkPlotPC *pc);
static void gtk_plot_gdk_gsave                      (GtkPlotPC *pc);
static void gtk_plot_gdk_grestore                   (GtkPlotPC *pc);
static void gtk_plot_gdk_clip                       (GtkPlotPC *pc,
                                                     const GdkRectangle *area);
static void gtk_plot_gdk_set_color                   (GtkPlotPC *pc,
                                                     const GdkColor *color);
static void gtk_plot_gdk_set_lineattr        	(GtkPlotPC *pc,
                                                 gfloat line_width,
                                                 GdkLineStyle line_style,
                                                 GdkCapStyle cap_style,
                                                 GdkJoinStyle join_style);
static void gtk_plot_gdk_set_dash                    (GtkPlotPC *pc,
                                                     gdouble offset_,
                                                     gdouble *values,
                                                     gint num_values);
static void gtk_plot_gdk_draw_point                  (GtkPlotPC *pc,
                                                     gdouble x, gdouble y);
static void gtk_plot_gdk_draw_line                   (GtkPlotPC *pc,
                                                     gdouble x1, gdouble y1,
                                                     gdouble x2, gdouble y2);
static void gtk_plot_gdk_draw_lines                  (GtkPlotPC *pc,
                                                     GtkPlotPoint *points,
                                                     gint numpoints);
static void gtk_plot_gdk_draw_rectangle              (GtkPlotPC *pc,
                                                     gint filled,
                                                     gdouble x, gdouble y,
                                                     gdouble width, 
                                                     gdouble height);
static void gtk_plot_gdk_draw_polygon                (GtkPlotPC *pc,
                                                     gint filled,
                                                     GtkPlotPoint *points,
                                                     gint numpoints);
static void gtk_plot_gdk_draw_circle                 (GtkPlotPC *pc,
                                                     gint filled,
                                                     gdouble x, gdouble y,
                                                     gdouble size);
static void gtk_plot_gdk_draw_ellipse                (GtkPlotPC *pc,
                                                     gint filled,
                                                     gdouble x, gdouble y,
                                                     gdouble width, 
                                                     gdouble height);
static void gtk_plot_gdk_set_font                    (GtkPlotPC *pc,
                                                     const gchar *font,
                                                     gint height);
static void gtk_plot_gdk_draw_string                (GtkPlotPC *pc,
                                                     gint x, gint y,
                                                     gint angle,
                                                     const GdkColor *fg,
                                                     const GdkColor *bg,
                                                     gboolean transparent,
                                                     gint border,
                                                     gint border_width,
                                                     gint shadow_width,
                                                     const gchar *font,
                                                     gint height,
                                                     GtkJustification just,
                                                     const gchar *text);
static gint roundint                                (gdouble x);

static GtkPlotPCClass *parent_class = NULL;

GtkType
gtk_plot_gdk_get_type (void)
{
  static GtkType pc_type = 0;

  if (!pc_type)
    {
      GtkTypeInfo pc_info =
      {
        "GtkPlotGdk",
        sizeof (GtkPlotGdk),
        sizeof (GtkPlotGdkClass),
        (GtkClassInitFunc) gtk_plot_gdk_class_init,
        (GtkObjectInitFunc) gtk_plot_gdk_init,
        /* reserved 1*/ NULL,
        /* reserved 2 */ NULL,
        (GtkClassInitFunc) NULL,
      };

      pc_type = gtk_type_unique (GTK_TYPE_PLOT_PC, &pc_info);
    }
  return pc_type;
}

static void
gtk_plot_gdk_init (GtkPlotGdk *pc)
{
  pc->gc = NULL;
  pc->drawable = NULL;
  pc->window = NULL;
  pc->ref_count = 0;
}


static void
gtk_plot_gdk_class_init (GtkPlotGdkClass *klass)
{
  GtkObjectClass *object_class;
  GtkPlotPCClass *pc_class;

  parent_class = gtk_type_class (gtk_plot_pc_get_type ());

  object_class = (GtkObjectClass *) klass;
  pc_class = (GtkPlotPCClass *) klass;

  object_class->finalize = gtk_plot_gdk_finalize;

  pc_class->init = gtk_plot_gdk_real_init;
  pc_class->leave = gtk_plot_gdk_leave;
  pc_class->gsave = gtk_plot_gdk_gsave;
  pc_class->grestore = gtk_plot_gdk_grestore;
  pc_class->clip = gtk_plot_gdk_clip;
  pc_class->set_color = gtk_plot_gdk_set_color;
  pc_class->set_dash = gtk_plot_gdk_set_dash;
  pc_class->set_lineattr = gtk_plot_gdk_set_lineattr;
  pc_class->draw_point = gtk_plot_gdk_draw_point;
  pc_class->draw_line = gtk_plot_gdk_draw_line;
  pc_class->draw_lines = gtk_plot_gdk_draw_lines;
  pc_class->draw_rectangle = gtk_plot_gdk_draw_rectangle;
  pc_class->draw_polygon = gtk_plot_gdk_draw_polygon;
  pc_class->draw_circle = gtk_plot_gdk_draw_circle;
  pc_class->draw_ellipse = gtk_plot_gdk_draw_ellipse;
  pc_class->set_font = gtk_plot_gdk_set_font;
  pc_class->draw_string = gtk_plot_gdk_draw_string;
}


GtkObject *
gtk_plot_gdk_new				(GdkDrawable *drawable)
{
  GtkObject *object;

  object = gtk_type_new(gtk_plot_gdk_get_type());

  gtk_plot_gdk_construct(GTK_PLOT_GDK(object), drawable);

  return (object);
}

void
gtk_plot_gdk_construct(GtkPlotGdk *pc, GdkDrawable *drawable)
{
  if(drawable){ 
    pc->drawable = drawable;
    gtk_plot_pc_gsave(GTK_PLOT_PC(pc));
  }
}


static void
gtk_plot_gdk_finalize (GtkObject *object)
{
  if(GTK_PLOT_GDK(object)->ref_count > 0)
          gdk_gc_destroy(GTK_PLOT_GDK(object)->gc);
}

void
gtk_plot_gdk_set_drawable(GtkPlotGdk *pc, GdkDrawable *drawable)
{
  pc->drawable = drawable;
}

static gboolean 
gtk_plot_gdk_real_init (GtkPlotPC *pc)
{
  return TRUE;
}

static void
gtk_plot_gdk_leave (GtkPlotPC *pc)
{
}

static void 
gtk_plot_gdk_gsave                                  (GtkPlotPC *pc)
{
  if(GTK_PLOT_GDK(pc)->gc) 
    gdk_gc_ref(GTK_PLOT_GDK(pc)->gc);
  else
    GTK_PLOT_GDK(pc)->gc = gdk_gc_new(GTK_PLOT_GDK(pc)->drawable);

  GTK_PLOT_GDK(pc)->ref_count++;
}

static void 
gtk_plot_gdk_grestore                                  (GtkPlotPC *pc)
{
  gdk_gc_unref(GTK_PLOT_GDK(pc)->gc);

  GTK_PLOT_GDK(pc)->ref_count--;
  if(GTK_PLOT_GDK(pc)->ref_count == 0) GTK_PLOT_GDK(pc)->gc = NULL;
}

static void 
gtk_plot_gdk_clip                                   (GtkPlotPC *pc,
                                                     const GdkRectangle *area)
{
  if(!GTK_PLOT_GDK(pc)->gc) return;

  /* discard GdkRectangle* const: 
   * gdk_gc_set_clip_rectangle should have a const arg.
   * I've checked the code and it doesn't change it or keep it. murrayc.
   */

  gdk_gc_set_clip_rectangle(GTK_PLOT_GDK(pc)->gc, (GdkRectangle*)area);  
}

static void 
gtk_plot_gdk_set_color                               (GtkPlotPC *pc,
                                                     const GdkColor *color)
{
  if(!GTK_PLOT_GDK(pc)->gc) return;

  /* discard GdkColor* const: gdk_gc_set_foreground should have a const arg.
   * I've checked the code and it doesn't change it or keep it. murrayc.
   */
  gdk_gc_set_foreground(GTK_PLOT_GDK(pc)->gc, (GdkColor*)color);
}

static void 
gtk_plot_gdk_set_dash                               (GtkPlotPC *pc,
                                                    gdouble offset_,
                                                    gdouble *values,
                                                    gint num_values)
{
  gchar list[] = {'\0','\1','\2','\3','\4','\5','\6','\7'};
  gchar dash[1000] = "";
  gint i;

  if(!GTK_PLOT_GDK(pc)->gc) return;

  for(i = 0; i < num_values; i++){
     gint value;
     value = values[i];
     dash[i] = list[value];
  }  

  gdk_gc_set_dashes(GTK_PLOT_GDK(pc)->gc, 0, dash, num_values);
}

static void gtk_plot_gdk_set_lineattr        	(GtkPlotPC *pc,
                                                 gfloat line_width,
                                                 GdkLineStyle line_style,
                                                 GdkCapStyle cap_style,
                                                 GdkJoinStyle join_style)
{
  if(!GTK_PLOT_GDK(pc)->gc) return;

  gdk_gc_set_line_attributes(GTK_PLOT_GDK(pc)->gc,  
		             roundint(line_width),
		             line_style,
		             cap_style,
		             join_style);
}

static void 
gtk_plot_gdk_draw_point                              (GtkPlotPC *pc,
                                                     gdouble x, gdouble y)
{
  if(!GTK_PLOT_GDK(pc)->gc) return;

  gdk_draw_point(GTK_PLOT_GDK(pc)->drawable, GTK_PLOT_GDK(pc)->gc, 
                 roundint(x), roundint(y));
}

static void 
gtk_plot_gdk_draw_line                               (GtkPlotPC *pc,
                                                     gdouble x1, gdouble y1,
                                                     gdouble x2, gdouble y2)
{
  if(!GTK_PLOT_GDK(pc)->gc) return;

  gdk_draw_line(GTK_PLOT_GDK(pc)->drawable, GTK_PLOT_GDK(pc)->gc, 
                roundint(x1), roundint(y1), roundint(x2), roundint(y2));
}

static void 
gtk_plot_gdk_draw_lines                              (GtkPlotPC *pc,
                                                     GtkPlotPoint *points,
                                                     gint numpoints)
{
  GdkPoint *p = NULL;
  gint i;

  if(!GTK_PLOT_GDK(pc)->gc) return;

  p = (GdkPoint *)g_malloc(numpoints * sizeof(GdkPoint));
  for(i = 0; i < numpoints; i++){
    p[i].x = roundint(points[i].x);
    p[i].y = roundint(points[i].y);
  }

  gdk_draw_lines(GTK_PLOT_GDK(pc)->drawable, GTK_PLOT_GDK(pc)->gc, p, numpoints);
}

static void 
gtk_plot_gdk_draw_rectangle                          (GtkPlotPC *pc,
                                                     gint filled,
                                                     gdouble x, gdouble y,
                                                     gdouble width, gdouble height)
{
  if(!GTK_PLOT_GDK(pc)->gc) return;

  gdk_draw_rectangle (GTK_PLOT_GDK(pc)->drawable, GTK_PLOT_GDK(pc)->gc,
                      filled,
                      roundint(x), roundint(y), 
                      roundint(width), roundint(height));

}

static void 
gtk_plot_gdk_draw_polygon                            (GtkPlotPC *pc,
                                                     gint filled,
                                                     GtkPlotPoint *points,
                                                     gint numpoints)
{
  GdkPoint *p = NULL;
  gint i;

  if(!GTK_PLOT_GDK(pc)->gc) return;

  p = (GdkPoint *)g_malloc(numpoints * sizeof(GdkPoint));
  for(i = 0; i < numpoints; i++){
    p[i].x = roundint(points[i].x);
    p[i].y = roundint(points[i].y);
  }

  gdk_draw_polygon(GTK_PLOT_GDK(pc)->drawable, GTK_PLOT_GDK(pc)->gc, 
                   filled, p, numpoints);
}

static void 
gtk_plot_gdk_draw_circle                             (GtkPlotPC *pc,
                                                     gint filled,
                                                     gdouble x, gdouble y,
                                                     gdouble size)
{
  if(!GTK_PLOT_GDK(pc)->gc) return;

  gdk_draw_arc (GTK_PLOT_GDK(pc)->drawable, GTK_PLOT_GDK(pc)->gc,
                filled,
                roundint(x-size/2.0), roundint(y-size/2.0),
                roundint(size), roundint(size), 0, 25000);

}

static void 
gtk_plot_gdk_draw_ellipse                            (GtkPlotPC *pc,
                                                     gint filled,
                                                     gdouble x, gdouble y,
                                                     gdouble width, gdouble height)
{
  if(!GTK_PLOT_GDK(pc)->gc) return;

  gdk_draw_arc (GTK_PLOT_GDK(pc)->drawable, GTK_PLOT_GDK(pc)->gc,
                filled,
                roundint(x), roundint(y),
                roundint(width), roundint(height), 0, 25000);
}

static void 
gtk_plot_gdk_set_font                                (GtkPlotPC *pc,
                                                     const gchar *font,
                                                     gint height)
{

}

static void 
gtk_plot_gdk_draw_string                        (GtkPlotPC *pc,
                                                gint tx, gint ty,
                                                gint angle,
                                                const GdkColor *fg,
                                                const GdkColor *bg,
                                                gboolean transparent,
                                                gint border,
                                                gint border_width,
                                                gint shadow_width,
                                                const gchar *font_name,
                                                gint font_height,
                                                GtkJustification just,
                                                const gchar *text)
{
  GdkPixmap *old_pixmap;
  GdkPixmap *text_pixmap;
  GdkBitmap *text_mask;
  GdkImage *image;
  GdkGC *gc, *mask_gc;
  GdkColormap *colormap;
  GdkColorContext *cc;
  GdkVisual *visual;
  GdkColor color, black, mask_color;
  GdkFont *font;
  GtkPSFont *psfont, *tmp_font;
  GList *family = NULL;
  gint y0;
  gint old_width, old_height;
  gboolean bold, italic;
  const gchar *aux;
  gchar subs[2], num[4], insert_char;
  const gchar *xaux;
  gint fontsize;
  gint ascent, descent;
  gint numf;
  gint xp = 0, yp = 0;
  const gchar *lastchar = NULL;
  gint width, height;
  gint x, y;
  gint i;

  if(!GTK_PLOT_GDK(pc)->drawable) return;
  if(!GTK_PLOT_GDK(pc)->window) return;
  if(!GTK_PLOT_GDK(pc)->gc) return;
  if(!text || strlen(text) == 0) return;

  colormap = gdk_colormap_get_system ();
  visual = gdk_window_get_visual (GTK_PLOT_GDK(pc)->window);
  cc = gdk_color_context_new(visual, colormap);
  gc = GTK_PLOT_GDK(pc)->gc;

  if(!gc) return;

  gtk_plot_text_get_size(text, angle, font_name, font_height, &width, &height, &ascent, &descent);

  if(height == 0 || width == 0) return;

  old_width = width;
  old_height = height;
  if(angle == 90 || angle == 270)
    {
      old_width = height;
      old_height = width;
    }

  gtk_psfont_get_families(&family, &numf);
  font = gtk_psfont_get_gdkfont(font_name, font_height);
  psfont = gtk_psfont_get_font(font_name);
  tmp_font = psfont;
  italic = psfont->italic;
  bold = psfont->bold;
  fontsize = font_height;
  x = 0;
  y0 = y = ascent;
  aux = text;

  old_pixmap = gdk_pixmap_new(GTK_PLOT_GDK(pc)->window, old_width, old_height, -1); 
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
           tmp_font = gtk_psfont_find_by_family((gchar *)g_list_nth_data(family, *aux-'0'), italic, bold);
           gdk_font_unref(font);
           font = gtk_psfont_get_gdkfont(tmp_font->psname, fontsize);
           aux++;
           break;
       case '8': case 'g':
           tmp_font = gtk_psfont_find_by_family("Symbol", italic, bold);
           gdk_font_unref(font);
           font = gtk_psfont_get_gdkfont(tmp_font->psname, fontsize);
           aux++;
           break;
       case 'B':
           bold = TRUE;
           tmp_font = gtk_psfont_find_by_family(tmp_font->family, italic, bold);
           gdk_font_unref(font);
           font = gtk_psfont_get_gdkfont(tmp_font->psname, fontsize);
           aux++;
           break;
       case 'x':
           xaux = aux + 1;
           for (i=0; i<3; i++){
            if (xaux[i] > 47 && xaux[i] < 58)
              num[i] = xaux[i];
            else
              break;
           }
           if (i < 3){
              aux++;
              break;
           }
           num[3] = '\0';
           insert_char = (gchar)atoi(num);
           subs[0] = insert_char;
           subs[1] = '\0';
           gdk_draw_string (old_pixmap, font,
                            gc,
                            x, y,
                            subs);

           x += gdk_char_width_wc (font, insert_char);
           aux += 4;
           lastchar = aux - 1;
           break;
       case 'i':
           italic = TRUE;
           tmp_font = gtk_psfont_find_by_family(tmp_font->family, italic, bold);
           gdk_font_unref(font);
           font = gtk_psfont_get_gdkfont(tmp_font->psname, fontsize);
           aux++;
           break;
       case 'S': case '^':
           fontsize = (int)((gdouble)fontsize * 0.6 + 0.5);
           gdk_font_unref(font);
           font = gtk_psfont_get_gdkfont(tmp_font->psname, fontsize);
           y -= font->ascent;
           aux++;
           break;
       case 's': case '_':
           fontsize = (int)((gdouble)fontsize * 0.6 + 0.5);
           gdk_font_unref(font);
           font = gtk_psfont_get_gdkfont(tmp_font->psname, fontsize);
           y += font->descent;
           aux++;
           break;
       case '+':
           fontsize += 3;
           gdk_font_unref(font);
           font = gtk_psfont_get_gdkfont(tmp_font->psname, fontsize);
           aux++;
           break;
       case '-':
           fontsize -= 3;
           gdk_font_unref(font);
           font = gtk_psfont_get_gdkfont(tmp_font->psname, fontsize);
           aux++;
           break;
       case 'N':
           tmp_font = psfont;
           gdk_font_unref(font);
           fontsize = font_height;
           font = gtk_psfont_get_gdkfont(tmp_font->psname, fontsize);
           y = y0;
           italic = psfont->italic;
           bold = psfont->bold;
           aux++;
           break;
       case 'b':
           if(lastchar){
              x -= gdk_char_width_wc (font, *lastchar);
              if(lastchar == text)
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

  text_pixmap = gdk_pixmap_new(GTK_PLOT_GDK(pc)->window, width, height, -1);

  /* Discard const on bg:
   * gdk_gc_set_foreground should have const arg.
   * The code doesn't change or keep bg.
   */

  gdk_gc_set_foreground(gc, (GdkColor*)bg);
  gdk_draw_rectangle(text_pixmap, gc, TRUE,
                     0, 0, -1, -1);

  text_mask = gdk_pixmap_new(GTK_PLOT_GDK(pc)->window, width, height, 1);
  mask_gc = gdk_gc_new(text_mask);
  mask_color.pixel = 0;
  gdk_gc_set_foreground(mask_gc, (GdkColor*)&mask_color);
  gdk_draw_rectangle(text_mask, mask_gc, TRUE,
                     0, 0, -1, -1);
  mask_color.pixel = 1;

  /* Discard const on fg:
   * gdk_gc_set_foreground should have const arg.
   * The code doesn't change or keep fg.
   */

  gdk_gc_set_foreground(gc, (GdkColor*)fg);
  gdk_gc_set_foreground(mask_gc, &mask_color);

  for(y = 0; y < old_height; y++)
    for(x = 0; x < old_width; x++)
       {
         color.pixel = gdk_image_get_pixel(image, x, y);
         gdk_color_context_query_color(cc, &color);
         if(gdk_color_equal(&color, &black)){
         switch(angle){
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
          gdk_draw_point(text_pixmap, gc, xp, yp);
          gdk_draw_point(text_mask, mask_gc, xp, yp);
         }
       }

  gtk_plot_text_get_area(text, angle, just, font_name, font_height,
                         &x, &y, &width, &height);

  tx += x;
  ty += y;

  if(transparent){
    gdk_gc_set_clip_mask (gc, text_mask);
    gdk_gc_set_clip_origin (gc, tx, ty);
  }


  gdk_draw_pixmap(GTK_PLOT_GDK(pc)->drawable, gc,
                  text_pixmap, 0, 0,
                  tx, ty, -1, -1);

  gdk_pixmap_unref(text_pixmap);
  gdk_bitmap_unref(text_mask);
  gdk_font_unref(font);
  gdk_gc_unref(mask_gc);
  gdk_color_context_free(cc);
  gdk_image_destroy(image);
  gdk_pixmap_unref(old_pixmap);
  gdk_gc_set_clip_mask(gc, NULL);

  return;
}

static gint
roundint(gdouble x)
{
 return (x+.50999999471);
}
