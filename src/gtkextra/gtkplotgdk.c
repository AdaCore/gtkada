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
#include <gdk/gdk.h>

#include "gtkplotpc.h"
#include "gtkplotgdk.h"
#include "gtkplot.h"
#include "gtkpsfont.h"
#include "gtkplotcanvas.h"

static void gtk_plot_gdk_init                       (GtkPlotGdk *pc);
static void gtk_plot_gdk_class_init                 (GtkPlotGdkClass *klass);
static void gtk_plot_gdk_finalize                   (GObject *object);
static void gtk_plot_gdk_real_set_drawable          (GtkPlotGdk *gdk,
						     GdkDrawable *drawable);
static gboolean gtk_plot_gdk_real_init              (GtkPlotPC *pc);
static void gtk_plot_gdk_set_viewport               (GtkPlotPC *pc,
						     gdouble w, gdouble h);
static void gtk_plot_gdk_leave                      (GtkPlotPC *pc);
static void gtk_plot_gdk_gsave                      (GtkPlotPC *pc);
static void gtk_plot_gdk_grestore                   (GtkPlotPC *pc);
static void gtk_plot_gdk_clip                       (GtkPlotPC *pc,
                                                     const GdkRectangle *area);
static void gtk_plot_gdk_clip_mask                  (GtkPlotPC *pc,
						     gdouble x,
						     gdouble y,
                                                     const GdkBitmap *mask);
static void gtk_plot_gdk_set_color                   (GtkPlotPC *pc,
                                                     const GdkColor *color);
static void gtk_plot_gdk_set_lineattr           (GtkPlotPC *pc,
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
						     GtkPSFont *psfont,
                                                     gint height);
static void gtk_plot_gdk_draw_string                (GtkPlotPC *pc,
                                                     gint x, gint y,
                                                     gint angle,
                                                     const GdkColor *fg,
                                                     const GdkColor *bg,
                                                     gboolean transparent,
                                                     gint border,
                                                     gint border_space,
                                                     gint border_width,
                                                     gint shadow_width,
                                                     const gchar *font,
                                                     gint height,
                                                     GtkJustification just,
                                                     const gchar *text);
static void gtk_plot_gdk_draw_pixmap                (GtkPlotPC *pc,
                                                     GdkPixmap *pixmap,
                                                     GdkBitmap *mask,
                                                     gint xsrc, gint ysrc,
                                                     gint xdest, gint ydest,
                                                     gint width, gint height,
                                                     gdouble scale_x, 
                                                     gdouble scale_y);

static GdkPixmap * scale_pixmap 		    (GdkWindow *window, 
						     GdkPixmap *pixmap, 
						     gdouble scale_x, 
						     gdouble scale_y);
static GdkBitmap * scale_bitmap 		    (GdkWindow *window, 
						     GdkBitmap *bitmap, 
						     gdouble scale_x, 
						     gdouble scale_y);

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
  GdkWindowAttr attributes;
  gint attributes_mask;

  attributes.window_type = GDK_WINDOW_CHILD;
  attributes.title = NULL;
  attributes.wclass = GDK_INPUT_OUTPUT;
  attributes.visual = gdk_visual_get_system ();
  attributes.colormap = gdk_colormap_get_system ();
  attributes.event_mask = 0;
  attributes_mask = GDK_WA_VISUAL | GDK_WA_COLORMAP;

  pc->gc = NULL;
  pc->drawable = NULL;
  pc->ref_count = 0;

  pc->window = gdk_window_new (NULL, &attributes, attributes_mask);
}


static void
gtk_plot_gdk_class_init (GtkPlotGdkClass *klass)
{
  GtkObjectClass *object_class;
  GObjectClass *gobject_class;
  GtkPlotPCClass *pc_class;
  GtkPlotGdkClass *gdk_class;

  parent_class = gtk_type_class (gtk_plot_pc_get_type ());

  object_class = (GtkObjectClass *) klass;
  gobject_class = (GObjectClass *) klass;

  pc_class = (GtkPlotPCClass *) klass;
  gdk_class = (GtkPlotGdkClass *) klass;

  gobject_class->finalize = gtk_plot_gdk_finalize;

  gdk_class->set_drawable = gtk_plot_gdk_real_set_drawable;

  pc_class->init = gtk_plot_gdk_real_init;
  pc_class->leave = gtk_plot_gdk_leave;
  pc_class->set_viewport = gtk_plot_gdk_set_viewport;
  pc_class->gsave = gtk_plot_gdk_gsave;
  pc_class->grestore = gtk_plot_gdk_grestore;
  pc_class->clip = gtk_plot_gdk_clip;
  pc_class->clip_mask = gtk_plot_gdk_clip_mask;
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
  pc_class->draw_pixmap = gtk_plot_gdk_draw_pixmap;
}


GtkObject *
gtk_plot_gdk_new                                (GdkDrawable *drawable)
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
    gtk_plot_gdk_set_drawable(GTK_PLOT_GDK(pc), drawable);
    gtk_plot_pc_gsave(GTK_PLOT_PC(pc));
  }
}


static void
gtk_plot_gdk_finalize (GObject *object)
{
  gdk_window_unref(GTK_PLOT_GDK(object)->window);
  GTK_PLOT_GDK(object)->window = NULL;

  if(GTK_PLOT_GDK(object)->ref_count > 0 && GTK_PLOT_GDK(object)->gc){
          gdk_gc_destroy(GTK_PLOT_GDK(object)->gc);
          GTK_PLOT_GDK(object)->gc = NULL;
  }
}

static void
gtk_plot_gdk_real_set_drawable(GtkPlotGdk *pc, GdkDrawable *drawable)
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

void 
gtk_plot_gdk_set_drawable               (GtkPlotGdk *gdk, GdkDrawable *drawable)
{
  GTK_PLOT_GDK_CLASS(GTK_OBJECT_GET_CLASS(GTK_OBJECT(gdk)))->set_drawable(gdk, drawable);
}

static void 
gtk_plot_gdk_set_viewport               (GtkPlotPC *pc, gdouble w, gdouble h)
{
}

static void 
gtk_plot_gdk_gsave                                  (GtkPlotPC *pc)
{
  if(GTK_PLOT_GDK(pc)->gc) 
    gdk_gc_ref(GTK_PLOT_GDK(pc)->gc);
  else{
    GTK_PLOT_GDK(pc)->gc = gdk_gc_new(GTK_PLOT_GDK(pc)->window);
  }

  GTK_PLOT_GDK(pc)->ref_count++;
}

static void 
gtk_plot_gdk_grestore                                  (GtkPlotPC *pc)
{
  if(GTK_PLOT_GDK(pc)->gc) gdk_gc_unref(GTK_PLOT_GDK(pc)->gc);

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
gtk_plot_gdk_clip_mask                              (GtkPlotPC *pc,
						     gdouble x,
						     gdouble y,
                                                     const GdkBitmap *mask)
{
  if(!GTK_PLOT_GDK(pc)->gc) return;

  if(x >= 0 && y >= 0)
    gdk_gc_set_clip_origin(GTK_PLOT_GDK(pc)->gc, x, y);

  gdk_gc_set_clip_mask(GTK_PLOT_GDK(pc)->gc, (GdkBitmap*)mask);  
}

static void 
gtk_plot_gdk_set_color                               (GtkPlotPC *pc,
                                                     const GdkColor *color)
{
  GdkColor new_color;

  if(!GTK_PLOT_GDK(pc)->gc) return;

  new_color = *color;
  gdk_color_alloc(gdk_colormap_get_system(), &new_color);
  gdk_gc_set_foreground(GTK_PLOT_GDK(pc)->gc, &new_color);
}

static void 
gtk_plot_gdk_set_dash                               (GtkPlotPC *pc,
                                                    gdouble offset,
                                                    gdouble *values,
                                                    gint num_values)
{
  gchar list[] = {'\0','\1','\2','\3','\4','\5','\6','\7'};
  gchar dash[1000] = "";
  gint i;

  if(!GTK_PLOT_GDK(pc)->gc) return;

  if(num_values == 0){
    return;
  }

  for(i = 0; i < num_values; i++){
     gint value;
     value = values[i];
     dash[i] = list[value];
  }  

  gdk_gc_set_dashes(GTK_PLOT_GDK(pc)->gc, 0, dash, num_values);
}

static void gtk_plot_gdk_set_lineattr           (GtkPlotPC *pc,
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
  if(!GTK_PLOT_GDK(pc)->drawable) return;

  gdk_draw_point(GTK_PLOT_GDK(pc)->drawable, GTK_PLOT_GDK(pc)->gc, 
                 roundint(x), roundint(y));
}

static void 
gtk_plot_gdk_draw_line                               (GtkPlotPC *pc,
                                                     gdouble x1, gdouble y1,
                                                     gdouble x2, gdouble y2)
{
  if(!GTK_PLOT_GDK(pc)->gc) return;
  if(!GTK_PLOT_GDK(pc)->drawable) return;

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
  if(!GTK_PLOT_GDK(pc)->drawable) return;

  p = (GdkPoint *)g_malloc(numpoints * sizeof(GdkPoint));
  for(i = 0; i < numpoints; i++){
    p[i].x = roundint(points[i].x);
    p[i].y = roundint(points[i].y);
  }

  gdk_draw_lines(GTK_PLOT_GDK(pc)->drawable, GTK_PLOT_GDK(pc)->gc, p, numpoints);

  g_free(p);
}

static void 
gtk_plot_gdk_draw_rectangle                          (GtkPlotPC *pc,
                                                     gint filled,
                                                     gdouble x, gdouble y,
                                                     gdouble width, gdouble height)
{
  if(!GTK_PLOT_GDK(pc)->gc) return;
  if(!GTK_PLOT_GDK(pc)->drawable) return;

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
  if(!GTK_PLOT_GDK(pc)->drawable) return;

  p = (GdkPoint *)g_malloc(numpoints * sizeof(GdkPoint));
  for(i = 0; i < numpoints; i++){
    p[i].x = roundint(points[i].x);
    p[i].y = roundint(points[i].y);
  }

  gdk_draw_polygon(GTK_PLOT_GDK(pc)->drawable, GTK_PLOT_GDK(pc)->gc, 
                   filled, p, numpoints);

  g_free(p);
}

static void 
gtk_plot_gdk_draw_circle                             (GtkPlotPC *pc,
                                                     gint filled,
                                                     gdouble x, gdouble y,
                                                     gdouble size)
{
  if(!GTK_PLOT_GDK(pc)->gc) return;
  if(!GTK_PLOT_GDK(pc)->drawable) return;

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
  if(!GTK_PLOT_GDK(pc)->drawable) return;

  gdk_draw_arc (GTK_PLOT_GDK(pc)->drawable, GTK_PLOT_GDK(pc)->gc,
                filled,
                roundint(x), roundint(y),
                roundint(width), roundint(height), 0, 25000);
}

static void 
gtk_plot_gdk_set_font                                (GtkPlotPC *pc,
						     GtkPSFont *psfont,
                                                     gint height)
{

}

/* subfunction of gtk_plot_gdk_draw_string(). */
static gint
drawstring(GtkPlotPC *pc,
	   GdkBitmap *dest,
	   GdkGC *gc,
	   gint dx, gint dy,
	   GtkPSFont *psfont, gint height,
	   GdkWChar wc)
{
  GdkBitmap *tmp;
  GdkFont *font;
  GdkImage *image;
  GdkColor mask_color;
  gint w, h, a, d, x, y, d2;
  guint32 pixel;

  font = gtk_psfont_get_gdkfont(psfont, height);
 
  if (psfont->i18n_latinfamily && psfont->vertical && (0 > wc || wc > 0x7f)) {
    /* vertical-writing CJK postscript fonts. */

    w = gdk_char_width_wc(font, wc);
    a = font->ascent;
    d = font->descent;
    h = a + d;
    d2 = w * d / h;

    tmp = gdk_pixmap_new(GTK_PLOT_GDK(pc)->window, w, h, 1);

    mask_color.pixel = 0;
    gdk_gc_set_foreground(gc, &mask_color);
    gdk_draw_rectangle(tmp, gc, TRUE, 0, 0, -1, -1);
    mask_color.pixel = 1;
    gdk_gc_set_foreground(gc, &mask_color);

    gdk_draw_text_wc(tmp, font, gc, 0, a, &wc, 1);

    image = gdk_image_get(tmp, 0, 0, w, h);

    for (y = 0; y < h; y++) {
      for (x = 0; x < w; x++) {
	pixel = gdk_image_get_pixel(image, x, y);
	if (pixel == 1)
	  gdk_draw_point(dest, gc, dx + y, dy + d2 - x);
      }
    }

    gdk_image_destroy(image);
    gdk_pixmap_unref(tmp);

    return h;
  } else {
    /* horizontal writing */

    gdk_draw_text_wc(dest, font, gc, dx, dy, &wc, 1);
    w = gdk_char_width_wc(font, wc);
    
    return w;
  }
}

static void 
gtk_plot_gdk_draw_string                        (GtkPlotPC *pc,
                                                gint tx, gint ty,
                                                gint angle,
                                                const GdkColor *fg,
                                                const GdkColor *bg,
                                                gboolean transparent,
                                                gint border,
                                                gint border_space,
                                                gint border_width,
                                                gint shadow_width,
                                                const gchar *font_name,
                                                gint font_height,
                                                GtkJustification just,
                                                const gchar *text)
{
  GdkBitmap *text_bitmap;
  GdkPixmap *text_pixmap;
  GdkBitmap *text_mask;
  GdkImage *image;
  GdkGC *gc, *bitmap_gc;
  GdkColormap *colormap;
  GdkColor mask_color;
  GList *family = NULL;
  gint y0;
  gint old_width, old_height;
  gboolean bold, italic;
  gint fontsize;
  gint ascent, descent, w;
  gint numf;
  gint xp = 0, yp = 0;
  gint width, height;
  gint x, y;
  gint i;
  GdkFont *font = NULL, *latin_font = NULL;
  GtkPSFont *psfont, *base_psfont, *latin_psfont;
  gchar subs[2], insert_char;
  GdkWChar *aux, *wtext, *lastchar = NULL, *xaux;
  gchar num[4];

  if(!GTK_PLOT_GDK(pc)->drawable) return;
  if(!GTK_PLOT_GDK(pc)->window) return;
  if(!GTK_PLOT_GDK(pc)->gc) return;
  if(!text || strlen(text) == 0) return;

  colormap = gdk_colormap_get_system ();
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
  base_psfont = psfont = gtk_psfont_get_by_name(font_name);
  font = gtk_psfont_get_gdkfont(psfont, font_height);
  italic = psfont->italic;
  bold = psfont->bold;
  fontsize = font_height;
  x = 0;
  y0 = y = ascent;

  if (psfont->i18n_latinfamily) {
    latin_psfont = gtk_psfont_get_by_family(psfont->i18n_latinfamily, italic,
                                             bold);
    latin_font = gtk_psfont_get_gdkfont(latin_psfont, fontsize);
  } else {
    latin_psfont = NULL; 
    latin_font = NULL;
  }

  i = strlen(text) + 2;
  aux = wtext = g_malloc0(sizeof(GdkWChar) * i);
  gdk_mbstowcs(wtext, text, i - 1);

  /* initializing text bitmap - ajd */
  text_bitmap = gdk_pixmap_new(GTK_PLOT_GDK(pc)->window,
                              old_width, old_height, 1);
  bitmap_gc = gdk_gc_new(text_bitmap);
  mask_color.pixel = 0;
  gdk_gc_set_foreground(bitmap_gc, &mask_color);
  gdk_draw_rectangle(text_bitmap, bitmap_gc, TRUE,
                     0, 0, -1, -1);
  mask_color.pixel = 1;
  gdk_gc_set_foreground(bitmap_gc, &mask_color);

  while(aux && *aux != '\0' && *aux != '\n'){
   if(*aux == '\\'){
     aux++;
     switch(*aux){
       case '0': case '1': case '2': case '3':
       case '4': case '5': case '6': case '7': case '9':
           psfont = gtk_psfont_get_by_family((gchar *)g_list_nth_data(family, *aux-'0'), italic, bold);
           gdk_font_unref(font);
           font = gtk_psfont_get_gdkfont(psfont, fontsize);
           aux++;
           break;
       case '8': case 'g':
           psfont = gtk_psfont_get_by_family("Symbol", italic, bold);
           gdk_font_unref(font);
           font = gtk_psfont_get_gdkfont(psfont, fontsize);
           aux++;
           break;
       case 'B':
           bold = TRUE;
           gdk_font_unref(font);
           psfont = gtk_psfont_get_by_family(psfont->family, italic, bold);
           font = gtk_psfont_get_gdkfont(psfont, fontsize);
           if(latin_font){
             gdk_font_unref(latin_font);
             latin_font = NULL;
           }
           if (psfont->i18n_latinfamily) {
             latin_psfont = gtk_psfont_get_by_family(psfont->i18n_latinfamily,
                                                      italic, bold);
             latin_font = gtk_psfont_get_gdkfont(latin_psfont, fontsize);
           }
           aux++;
           break;
       case 'x':
           xaux = aux + 1;
           for (i=0; i<3; i++){
            if (xaux[i] >= '0' && xaux[i] <= '9')
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
           x += gdk_char_width(font, insert_char);
           aux += 4;
           lastchar = aux - 1;
           break;
       case 'i':
           italic = TRUE;
           psfont = gtk_psfont_get_by_family(psfont->family, italic, bold);
           gdk_font_unref(font);
           font = gtk_psfont_get_gdkfont(psfont, fontsize);
           if (latin_font) {
             gdk_font_unref(latin_font);
             latin_font = NULL;
           }
           if (psfont->i18n_latinfamily) {
             latin_psfont = gtk_psfont_get_by_family(psfont->i18n_latinfamily,
                                                      italic, bold);
             latin_font = gtk_psfont_get_gdkfont(latin_psfont, fontsize);
           }
           aux++;
           break;
       case 'S': case '^':
           fontsize = (int)((gdouble)fontsize * 0.6 + 0.5);
           gdk_font_unref(font);
           font = gtk_psfont_get_gdkfont(psfont, fontsize);
           y -= font->ascent;
           if (latin_font) {
             gdk_font_unref(latin_font);
             latin_font = NULL;
           }
           if (psfont->i18n_latinfamily) {
             latin_font = gtk_psfont_get_gdkfont(latin_psfont, fontsize);
           }
           aux++;
           break;
       case 's': case '_':
           fontsize = (int)((gdouble)fontsize * 0.6 + 0.5);
           gdk_font_unref(font);
           font = gtk_psfont_get_gdkfont(psfont, fontsize);
           y += font->descent;
           if (latin_font) {
             gdk_font_unref(latin_font);
             latin_font = NULL;
           }
           if (psfont->i18n_latinfamily) {
             latin_font = gtk_psfont_get_gdkfont(latin_psfont, fontsize);
           }
           aux++;
           break;
       case '+':
           fontsize += 3;
           gdk_font_unref(font);
           font = gtk_psfont_get_gdkfont(psfont, fontsize);
           if (latin_font) {
             gdk_font_unref(latin_font);
             latin_font = NULL;
           }
           if (psfont->i18n_latinfamily) {
             latin_font = gtk_psfont_get_gdkfont(latin_psfont, fontsize);
           }
           aux++;
           break;
       case '-':
           fontsize -= 3;
           gdk_font_unref(font);
           font = gtk_psfont_get_gdkfont(psfont, fontsize);
           if (latin_font) {
             gdk_font_unref(latin_font);
             latin_font = NULL;
           }
           if (psfont->i18n_latinfamily) {
             latin_font = gtk_psfont_get_gdkfont(latin_psfont, fontsize);
           }
           aux++;
           break;
       case 'N':
	   psfont = base_psfont;
           gdk_font_unref(font);
           fontsize = font_height;
           font = gtk_psfont_get_gdkfont(psfont, fontsize);
           y = y0;
           italic = psfont->italic;
           bold = psfont->bold;
           aux++;
           break;
       case 'b':
	   if (lastchar) {
	     gtk_psfont_get_char_size(psfont, font, latin_font, *lastchar, &w,
				      NULL, NULL);
	     x -= w;

	     if (lastchar == wtext)
	       lastchar = NULL;
	     else
	       lastchar--;
	   } else {
	     gtk_psfont_get_char_size(psfont, font, latin_font, 'X', &w, NULL,
				    NULL);
	     x -= w;
	   }
           aux++;
           break;
       default:
           if(aux && *aux != '\0' && *aux !='\n'){
	     x += drawstring(pc, text_bitmap, bitmap_gc, x, y,
			     psfont, fontsize, *aux);
	     lastchar = aux;
	     aux++;
	   }
	   break;
     }
   } else {
     if(aux && *aux != '\0' && *aux !='\n'){
       x += drawstring(pc, text_bitmap, bitmap_gc, x, y,
		       psfont, fontsize, *aux);
       lastchar = aux;
       aux++;
     }
   }
  }

  g_free(wtext);

  /* initializing clip mask bitmap - ajd */
  text_mask = gdk_pixmap_new(GTK_PLOT_GDK(pc)->window, width, height, 1);
  mask_color.pixel = 0;
  gdk_gc_set_foreground(bitmap_gc, &mask_color);
  gdk_draw_rectangle(text_mask, bitmap_gc, TRUE, 0, 0, -1, -1);
  mask_color.pixel = 1;
  gdk_gc_set_foreground(bitmap_gc, &mask_color);

  /* performing text rotation and saving it onto clip mask bitmap - ajd */
  image = gdk_image_get(text_bitmap, 0, 0, old_width, old_height);
  for(y = 0; y < old_height; y++)
      for(x = 0; x < old_width; x++)
         {
           if( gdk_image_get_pixel(image, x, y) == 1 ){
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
            gdk_draw_point(text_mask, bitmap_gc, xp, yp);
           }
         }
  gdk_image_destroy(image);


  /* initializing text pixmap - ajd */
  text_pixmap = gdk_pixmap_new(GTK_PLOT_GDK(pc)->window, width, height, -1);
  gdk_gc_set_foreground(gc, (GdkColor *) bg);
  gdk_draw_rectangle(text_pixmap, gc, TRUE, 0, 0, -1, -1);
  gdk_gc_set_foreground(gc, (GdkColor *) fg);

  /* copying clip mask bitmap onto text pixmap - ajd */
  gdk_gc_set_clip_mask(gc, text_mask);
  gdk_gc_set_clip_origin(gc, 0, 0);
  gdk_draw_rectangle(text_pixmap, gc, TRUE, 0, 0, -1, -1);
  gdk_gc_set_clip_mask(gc, NULL);

  gtk_plot_text_get_area(text, angle, just, font_name, font_height,
                         &x, &y, &width, &height);
  tx += x;
  ty += y;

  if(transparent){
    gdk_gc_set_clip_mask (gc, text_mask);
    gdk_gc_set_clip_origin (gc, tx, ty);
  } else {
    gdk_gc_set_foreground(gc, (GdkColor *) bg);
    gtk_plot_pc_draw_rectangle(pc,
   		         TRUE, 
                         tx - border_space, ty - border_space, 
                         width + 2*border_space, height + 2*border_space);
  }

  gdk_draw_pixmap(GTK_PLOT_GDK(pc)->drawable, gc,
                  text_pixmap, 0, 0,
                  tx, ty, -1, -1);
  gdk_gc_set_clip_mask(gc, NULL);

  gdk_pixmap_unref(text_pixmap);
  gdk_bitmap_unref(text_mask);
  gdk_font_unref(font);
  gdk_gc_unref(bitmap_gc);
  gdk_pixmap_unref(text_bitmap);


/* border */

  gdk_gc_set_foreground(gc, (GdkColor *) fg);
  gtk_plot_pc_set_dash(pc, 0, NULL, 0);
  gtk_plot_pc_set_lineattr(pc, border_width, 0, 0, 0);
  switch(border){
    case GTK_PLOT_BORDER_SHADOW: 
      gtk_plot_pc_draw_rectangle(pc,
   		         TRUE, 
                         tx - border_space + shadow_width, 
                         ty + height + border_space, 
                         width + 2 * border_space, shadow_width);
      gtk_plot_pc_draw_rectangle(pc,
   		         TRUE, 
                         tx + width + border_space, 
                         ty - border_space + shadow_width, 
                         shadow_width, height + 2 * border_space);
    case GTK_PLOT_BORDER_LINE: 
      gtk_plot_pc_draw_rectangle(pc,
   		         FALSE, 
                         tx - border_space, ty - border_space, 
                         width + 2*border_space, height + 2*border_space);
    case GTK_PLOT_BORDER_NONE: 
    default:
	break; 
  }

  return;
}

static void gtk_plot_gdk_draw_pixmap                (GtkPlotPC *pc,
                                                     GdkPixmap *pixmap,
                                                     GdkBitmap *mask,
                                                     gint xsrc, gint ysrc,
                                                     gint xdest, gint ydest,
                                                     gint width,
                                                     gint height,
                                                     gdouble scale_x, 
                                                     gdouble scale_y)
{
  GdkGC *gc;
  GdkPixmap *new_pixmap;
  GdkBitmap *new_mask = NULL;

  if(!GTK_PLOT_GDK(pc)->drawable) return;
  if(!GTK_PLOT_GDK(pc)->window) return;
  if(!GTK_PLOT_GDK(pc)->gc) return;

  gc = GTK_PLOT_GDK(pc)->gc;

  if(!gc) return;

  new_pixmap = scale_pixmap(GTK_PLOT_GDK(pc)->window, pixmap, scale_x, scale_y);
  
  if(mask)
     new_mask = scale_bitmap(GTK_PLOT_GDK(pc)->window, mask, scale_x, scale_y);

  gtk_plot_pc_clip_mask(pc, xdest, ydest, new_mask);
  gdk_draw_pixmap(GTK_PLOT_GDK(pc)->drawable, gc, new_pixmap,
                  xsrc, ysrc, xdest, ydest, width*scale_x, height*scale_y);
  gtk_plot_pc_clip_mask(pc, xdest, ydest, NULL);

  if(new_mask) gdk_bitmap_unref(new_mask);
  gdk_pixmap_unref(new_pixmap);
}

static GdkPixmap *
scale_pixmap (GdkWindow *window, GdkPixmap *pixmap, gdouble scale_x, gdouble scale_y)
{
  GdkGC *gc;
  GdkVisual *visual;
  GdkImage *image, *new_image;
  GdkPixmap *new_pixmap;
  gint x, y, width, height, new_width, new_height;
  guint32 pixel;

  if(!pixmap) return NULL;
  if(!window) return NULL;

  gc = gdk_gc_new(pixmap);
  visual = gdk_visual_get_system ();

  gdk_window_get_size(pixmap, &width, &height);

  if(scale_x == 1.0 && scale_y == 1.0){
    new_pixmap = gdk_pixmap_new(window, width, height, -1);
    gdk_draw_pixmap(new_pixmap,
                    gc,
                    pixmap,
                    0, 0,
                    0, 0,
                    width, height);
    return new_pixmap;
  }

  new_width = roundint(width * scale_x);
  new_height = roundint(height * scale_y);
 
  /* make a client side image of the pixmap, and
   * scale the data into a another client side image */
  new_image = gdk_image_new(GDK_IMAGE_FASTEST,visual,new_width,new_height);
  image = gdk_drawable_get_image(pixmap,
                        0, 0,
                        width, height);

  for(x = 0; x < new_width; x++){
    for(y = 0; y < new_height; y++){
      gint px, py;

      px = MIN(roundint(x / scale_x), width - 1);
      py = MIN(roundint(y / scale_y), height - 1);

      pixel = gdk_image_get_pixel(image, px, py);
      gdk_image_put_pixel(new_image, x, y, pixel);
    }
  }

  /* draw the image into a new pixmap */
  new_pixmap = gdk_pixmap_new(window, new_width, new_height, -1);
  gdk_draw_image(new_pixmap,gc,new_image,0,0,0,0,new_width,new_height);

  gdk_image_destroy(image);
  gdk_image_destroy(new_image);

  return new_pixmap;
}

static GdkBitmap *
scale_bitmap (GdkWindow *window, GdkBitmap *bitmap, gdouble scale_x, gdouble scale_y)
{
  GdkGC *gc;
  GdkVisual *visual;
  GdkImage *image, *new_image;
  GdkBitmap *new_bitmap;
  gint x, y, width, height, new_width, new_height;
  GdkColor color;

  if(!bitmap) return NULL;
  if(!window) return NULL;

  gc = gdk_gc_new(bitmap);
  visual = gdk_visual_get_system ();

  gdk_window_get_size(bitmap, &width, &height);

  if(scale_x == 1.0 && scale_y == 1.0){
    new_bitmap = gdk_pixmap_new(window, width, height, 1);
    color.pixel = 0;
    gdk_gc_set_foreground(gc, &color);
    gdk_draw_rectangle(new_bitmap, gc, TRUE, 0, 0, width, height); 
    color.pixel = 1;
    gdk_gc_set_foreground(gc, &color);

    gdk_draw_pixmap(new_bitmap,
                    gc,
                    bitmap,
                    0, 0,
                    0, 0,
                    width, height);
    return new_bitmap;
  }

  new_width = roundint(width * scale_x);
  new_height = roundint(height * scale_y);

  /* make a client side image of the bitmap, and
   * scale the data into a another client side image */
  image = gdk_drawable_get_image(bitmap,
                        0, 0,
                        width, height);

  new_image = gdk_image_new(GDK_IMAGE_FASTEST,visual,new_width,new_height);
  new_bitmap = gdk_pixmap_new(window, new_width, new_height, 1);

  color.pixel = 0;
  gdk_gc_set_foreground(gc, &color);
  gdk_draw_rectangle(new_bitmap, gc, TRUE, 0, 0, width, height); 
  color.pixel = 1;
  gdk_gc_set_foreground(gc, &color);

  for(x = 0; x < new_width; x++){
    for(y = 0; y < new_height; y++){
      gint px, py;
      gulong pixel;

      px = MIN(roundint(x / scale_x), width - 1);
      py = MIN(roundint(y / scale_y), height - 1);

      pixel = gdk_image_get_pixel(image, px, py);
      gdk_image_put_pixel(new_image, x, y, pixel);

    }
  }

  /* draw the image into a new pixmap */
  gdk_draw_image(new_bitmap,gc,new_image,0,0,0,0,new_width,new_height);

  gdk_image_destroy(image);
  gdk_image_destroy(new_image);

  return new_bitmap;
}
static gint
roundint(gdouble x)
{
 return (x+.50999999471);
}
