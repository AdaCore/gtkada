/* gtkpsfont - PostScript Fonts handling
 * Copyright 1999-2001  Adrian E. Feiguin <feiguin@ifir.edu.ar>
 *
 * Some code borrowed from
 * DiaCanvas -- a technical canvas widget
 * Copyright (C) 1999 Arjan Molenaar
 * Dia -- an diagram creation/manipulation program
 * Copyright (C) 1998 Alexander Larsson
 *
 * and Xfig
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

#include <gtk/gtk.h>
#include <stdio.h>
#include <string.h>
#include <pango/pango.h>
#include "gtkpsfont.h"

#define FONTCACHE_SIZE 17
#define NUM_X11_FONTS 2


static GtkPSFont font_data[] = 
{
  { "Times-Roman",
    "Times-Roman",
    "Times-Roman",
    "times, Medium",
    NULL,
    FALSE, FALSE
  }, 
  { "Times-Italic",
    "Times-Italic",
    "Times-Roman",
    "times, Medium Italic",
    NULL,
    TRUE, FALSE
  }, 
  { "Times-Bold",
    "Times-Bold",
    "Times-Roman",
    "times, Bold",
    NULL,
    FALSE, TRUE
  }, 
  { "Times-BoldItalic",
    "Times-BoldItalic",
    "Times-Roman",
    "times, Bold Italic",
    NULL,
    TRUE, TRUE
  }, 
  { "AvantGarde-Book",
    "AvantGarde-Book",
    "AvantGarde",
    "avantgarde",
    NULL,
    FALSE, FALSE
  },
  { "AvantGarde-BookOblique",
    "AvantGarde-BookOblique",
    "AvantGarde",
    "avantgarde, Oblique",
    NULL,
    TRUE, FALSE
  },
  { "AvantGarde-Demi",
    "AvantGarde-Demi",
    "AvantGarde",
    "avantgarde, Semi-Bold",
    NULL,
    FALSE, TRUE
  },
  { "AvantGarde-DemiOblique",
    "AvantGarde-DemiOblique",
    "AvantGarde",
    "avantgarde, Semi-Bold Oblique",
    NULL,
    TRUE, TRUE
  },
  { "Bookman-Light",
    "Bookman-Light",
    "Bookman",
    "bookman",
    NULL,
    FALSE, FALSE
  },
  { "Bookman-LightItalic",
    "Bookman-LightItalic",
    "Bookman",
    "bookman, Light Italic",
    NULL,
    TRUE, FALSE
  },
  { "Bookman-Demi",
    "Bookman-Demi",
    "Bookman",
    "bookman, Semi-Bold", 
    NULL,
    FALSE, TRUE
  },
  { "Bookman-DemiItalic",
    "Bookman-DemiItalic",
    "Bookman",
    "bookman, Semi-Bold Italic", 
    NULL,
    TRUE, TRUE
  },
  { "Courier",
    "Courier",
    "Courier",
    "courier",
    NULL,
    FALSE, FALSE
  },
  { "Courier-Oblique",
    "Courier-Oblique",
    "Courier",
    "courier, Italic",
    NULL,
    TRUE, FALSE
  },
  { "Courier-Bold",
    "Courier-Bold",
    "Courier",
    "courier, Bold",
    NULL,
    FALSE, TRUE
  },
  { "Courier-BoldOblique",
    "Courier-BoldOblique",
    "Courier",
    "courier, Bold Oblique",
    NULL,
    TRUE, TRUE
  },
  { "Helvetica",
    "Helvetica",
    "Helvetica",
    "helvetica, Medium",
    NULL,
    FALSE, FALSE
  },
  { "Helvetica-Oblique",
    "Helvetica-Oblique",
    "Helvetica",
    "helvetica, Medium Oblique",
    NULL,
    TRUE, FALSE
  },
  { "Helvetica-Bold",
    "Helvetica-Bold",
    "Helvetica",
    "helvetica, Bold",
    NULL,
    FALSE, TRUE
  },
  { "Helvetica-BoldOblique",
    "Helvetica-BoldOblique",
    "Helvetica",
    "helvetica, Bold Oblique",
    NULL,
    TRUE, TRUE
  },
  { "Helvetica-Narrow",
    "Helvetica-Narrow",
    "Helvetica-Narrow",
    "helvetica, Medium Condensed",
    NULL,
    FALSE, FALSE
  },
  { "Helvetica-Narrow-Oblique",
    "Helvetica-Narrow-Oblique",
    "Helvetica-Narrow",
    "helvetica, Medium Oblique Condensed",
    NULL,
    TRUE, FALSE
  },
  { "Helvetica-Narrow-Bold",
    "Helvetica-Narrow-Bold",
    "Helvetica-Narrow",
    "helvetica, Bold Condensed",
    NULL,
    FALSE, TRUE
  },
  { "Helvetica-Narrow-BoldOblique",
    "Helvetica-Narrow-BoldOblique",
    "Helvetica-Narrow",
    "helvetica, Bold Oblique Condensed",
    NULL,
    TRUE, TRUE
  },
  { "NewCenturySchoolbook-Roman",
    "NewCenturySchlbk-Roman",
    "NewCenturySchlbk",
    "new century schoolbook, Medium",
    NULL,
    FALSE, FALSE
  },
  { "NewCenturySchoolbook-Italic",
    "NewCenturySchlbk-Italic",
    "NewCenturySchlbk",
    "new century schoolbook, Medium Italic",
    NULL,
    TRUE, FALSE
  },
  { "NewCenturySchoolbook-Bold",
    "NewCenturySchlbk-Bold",
    "NewCenturySchlbk",
    "new century schoolbook, Bold",
    NULL,
    FALSE, TRUE
  },
  { "NewCenturySchoolbook-BoldItalic",
    "NewCenturySchlbk-BoldItalic",
    "NewCenturySchlbk",
    "new century schoolbook, Bold Italic",
    NULL,
    TRUE, TRUE
  },
  { "Palatino-Roman",
    "Palatino-Roman",
    "Palatino",
    "palatino, Medium",
    NULL,
    FALSE, FALSE
  },
  { "Palatino-Italic",
    "Palatino-Italic",
    "Palatino",
    "palatino, Medium Italic",
    NULL,
    TRUE, FALSE
  },
  { "Palatino-Bold",
    "Palatino-Bold",
    "Palatino",
    "palatino, Bold",
    NULL,
    FALSE, TRUE
  },
  { "Palatino-BoldItalic",
    "Palatino-BoldItalic",
    "Palatino",
    "palatino, Bold Italic",
    NULL,
    TRUE, TRUE
  },
  { "Symbol",
    "Symbol",
    "Symbol",
    "symbol",
    NULL,
    FALSE, FALSE
  },
  { "ZapfChancery-MediumItalic",
    "ZapfChancery-MediumItalic",
    "ZapfChancery",
    "zapf chancery, Medium Italic",
    NULL,
    FALSE, FALSE
  },
  { "ZapfDingbats",
    "ZapfDingbats",
    "ZapfDingbats",
    "zapf dingbats, Medium",
    NULL,
    FALSE, FALSE
  },
};

#define NUM_FONTS (sizeof(font_data)/sizeof(GtkPSFont))

static GList *user_fonts;
static gboolean psfont_init = FALSE;
static GList *psfont_families;
static gint numf;
static gint psfont_refcount = 0;

#ifdef G_PLATFORM_WIN32
static const char *default_font = "sans";
#else
static const char *default_font = "fixed";
#endif

static GtkPSFont *find_psfont		(const gchar *name);

gint 
gtk_psfont_init()
{
  GtkPSFont *data = NULL;
  GList *fonts;
  gint i, j;
  gboolean new_family = TRUE;

  psfont_refcount++;

/*  if(psfont_refcount > 1) printf("PS fonts already initilized\n");;
*/
  if(psfont_refcount > 1) return FALSE;

/*  printf("Initializing PS fonts\n");;
*/

  psfont_init = TRUE;
  psfont_families = NULL;
  numf = 0;

  for(i = 0; i < NUM_FONTS; i++){
    new_family = TRUE;
    for(j = 0; j < numf; j++){
       if(strcmp(font_data[i].family, (gchar *)g_list_nth_data(psfont_families, j)) == 0)
         new_family = FALSE;
    }
    if(new_family){
         numf = numf + 1;
         psfont_families = g_list_append(psfont_families, font_data[i].family);
    }     
  }

  fonts = user_fonts;
  while(fonts){
    data = (GtkPSFont *) fonts->data;
    new_family = TRUE;
    for(j = 0; j < numf; j++){
       if(strcmp(data->family, (gchar *)g_list_nth_data(psfont_families, j)) == 0) 
         new_family = FALSE;
    }
    if(new_family){
         numf = numf + 1;
         psfont_families = g_list_append(psfont_families, data->family);
    }     
    fonts = fonts->next;
  }

  return TRUE;
}


void 
gtk_psfont_unref()
{
  GList *list;

  psfont_refcount--;

  if(psfont_refcount > 0) return;

  list = psfont_families;
  while(list){
    psfont_families = g_list_remove_link(psfont_families, list);
    g_list_free_1(list);
    list = psfont_families;
  }

  list = user_fonts;
  while(list){
    user_fonts = g_list_remove_link(user_fonts, list);
    g_list_free_1(list);
    list = user_fonts;
  }

  psfont_init = FALSE;
}

GtkPSFont *
gtk_psfont_get_by_name(const gchar *name)
{
  GtkPSFont *font;

  font = find_psfont(name);

  if (font == NULL) {
    font = find_psfont(default_font);
    if (font == NULL) {
      g_warning ("Error, couldn't locate default font. Shouldn't happen.");
    } else {
      g_message ("Postscript font %s not found, using %s instead.",
		 name, default_font);
    }
  }

  return (GtkPSFont *)font;
}

GdkFont *
gtk_psfont_get_gdkfont(GtkPSFont *font, gint height)
{
  PangoFontDescription *font_desc;
  gchar *font_string;
  GdkFont *gdkfont;

  g_return_val_if_fail (font != NULL, NULL);

  if (height <= 0) height = 1;

  font_desc = gtk_psfont_get_font_description(font, height);
  gdkfont = font_desc ? gdk_font_from_description(font_desc) : NULL;
  if (font_desc)
    pango_font_description_free(font_desc);

  if (!gdkfont) {
    font_string = g_strdup_printf("%s %i", default_font, height);
    font_desc = pango_font_description_from_string(font_string);
    g_free(font_string);
    gdkfont = font_desc ? gdk_font_from_description(font_desc) : NULL;
    if (font_desc)
      pango_font_description_free(font_desc);

    if (gdkfont)
      g_message ("Pango font %s %i (PS font %s) not found, using %s instead.",
		 font->pango_description, height, font->fontname, default_font);
    else
      g_warning ("Error, couldn't locate default font. Shouldn't happen.");
  }

  return gdkfont;
}

PangoFontDescription *
gtk_psfont_get_font_description(GtkPSFont *font, gint height)
{
  PangoFontDescription *font_desc, *desc;
  PangoContext *context = gdk_pango_context_get();
  PangoFontset *pffontset;
  PangoFont *pffont;
  gchar *font_string;

  g_return_val_if_fail (font != NULL, NULL);

  if (height <= 0) height = 1;
 
  font_string = g_strdup_printf("%s %i", font->pango_description, height);
  font_desc = pango_font_description_from_string(font_string);
  g_free(font_string);

  if (!font_desc) {
    font_string = g_strdup_printf("%s %i", default_font, height);
    font_desc = pango_font_description_from_string(font_string);
    g_free(font_string);
    if (font_desc)
      g_message ("Font %s not describable, using %s instead.",
		 font->fontname, default_font);
    else
      g_warning ("Error, couldn't describe default font. Shouldn't happen.");
  }

  /* Loading via the pango fontset facility means that pango.aliases is used */
  if (font_desc) {
    pffontset = pango_context_load_fontset(context, font_desc,
      pango_context_get_language(context));
    if (pffontset) {
      pffont = pango_fontset_get_font(pffontset, g_utf8_get_char(" "));
      if (pffont) {
	desc = pango_font_describe(pffont);
	g_object_unref(G_OBJECT(pffont));
	if (desc) {
	  pango_font_description_free(font_desc);
	  font_desc = desc;
	}
      }
      g_object_unref(G_OBJECT(pffontset));
    }
  }

  return font_desc;
}


const gchar *
gtk_psfont_get_psfontname(GtkPSFont *font)
{

  g_return_val_if_fail (font != NULL, NULL);

  return font->psname;
}

void
gtk_psfont_add_font (const gchar *fontname, 
                     const gchar *psname, const gchar *family,
                     const gchar *pango_description,
                     gboolean italic, gboolean bold)
{
  GtkPSFont *font;

  font = g_new0(GtkPSFont, 1);

  font->fontname = g_strdup(fontname); 
  font->psname = g_strdup(psname); 
  font->family = g_strdup(family); 
  font->pango_description = g_strdup(pango_description); 
  font->i18n_latinfamily = NULL;
  font->italic = italic;
  font->bold = bold;
  font->vertical = FALSE;

  user_fonts = g_list_append(user_fonts, font);
}

void
gtk_psfont_add_i18n_font (const gchar *fontname, 
                         const gchar *psname, const gchar *family,
                         const gchar *i18n_latinfamily, 
                         const gchar *pango_description,
                         gboolean italic, gboolean bold, gboolean vertical)
{
  GtkPSFont *font;

  font = g_new0(GtkPSFont, 1);

  font->fontname = g_strdup(fontname); 
  font->psname = g_strdup(psname); 
  font->family = g_strdup(family); 
  font->pango_description = g_strdup(pango_description); 
  font->i18n_latinfamily = g_strdup(i18n_latinfamily);
  font->italic = italic;
  font->bold = bold;
  font->vertical = vertical;

  user_fonts = g_list_append(user_fonts, font);
}


static GtkPSFont *
find_psfont(const gchar *name)
{
  GtkPSFont *fontdata = NULL;
  GtkPSFont *data = NULL;
  GList *fonts;
  gint i;

  /* user_fonts should be superior to font_data, the built-in default
     settings because user_fonts is supposed to store a more appropriate
     existent xfont than font_data.
  */

  fonts = user_fonts;
  while(fonts){
    data = (GtkPSFont *) fonts->data;
    if(strcmp(name, data->fontname) == 0) {
      fontdata = data;
      break;
    }
    if(strcmp(name, data->psname) == 0) {
      fontdata = data;
      break;
    }
    fonts = fonts->next;
  }

  if(fontdata == NULL) {
    for(i = 0; i < NUM_FONTS; i++){
      if(strcmp(name, font_data[i].fontname) == 0) { 
	fontdata = &font_data[i];
	break;
      }
      if(strcmp(name, font_data[i].psname) == 0) { 
	fontdata = &font_data[i];
       break;
      }
    }
  }
  
  return fontdata;
}

GtkPSFont *
gtk_psfont_get_by_family(const gchar *name, gboolean italic, gboolean bold)
{
  GtkPSFont *fontdata = NULL;
  GtkPSFont *data = NULL;
  GtkPSFont *return_data = NULL;
  GList *fonts;
  gint i;

  /* user_fonts should be superior to font_data, the built-in default
     settings because user_fonts is supposed to store a more appropriate
     existent xfont than font_data.
  */

  fonts = user_fonts;
  while(fonts){
    data = (GtkPSFont *) fonts->data;
    if(strcmp(name, data->family) == 0) {
      return_data = data;
      if(data->italic == italic && data->bold == bold){
	fontdata = data;
	break;
      }
    }
    fonts = fonts->next;
  }
    
  if(fontdata == NULL) {
    for(i = 0; i < NUM_FONTS; i++){
      if(strcmp(name, font_data[i].family) == 0) { 
	return_data = &font_data[i];
	if(font_data[i].italic == italic && font_data[i].bold == bold){
	  fontdata = &font_data[i];
	  break;
	}
      }
    }
  }

  if(!fontdata) fontdata = return_data;
  return fontdata;
}


void
gtk_psfont_get_families(GList **families, gint *num_families)
{
  if(!psfont_init || psfont_refcount == 0){
    g_warning("PS fonts have not been initialized. Use gtk_psfont_init first.");
    return;
  }

  *families = psfont_families;
  *num_families = numf;
}

/* get the width, ascent and descent of a character. */
void
gtk_psfont_get_char_size(GtkPSFont *psfont,
                         GdkFont *font,
                         GdkFont *latin_font,
                         GdkWChar wc,
                         gint *width,
                         gint *ascent,
                         gint *descent)
{
  GdkFont *dfont;
  gint w, a, d, w0;
  
  if (psfont->i18n_latinfamily && psfont->vertical && (0 > wc || wc > 0x7f)) {
    /* vertical-writing CJK postscript fonts. */
    w = (font->ascent + font->descent);
    w0 = gdk_char_width_wc(font, wc);
    d = w0 * font->descent / w;
    a = w0 - d;
  } else {
    if (psfont->i18n_latinfamily && 0 <= wc && wc <= 0x7f)
      dfont = latin_font;
    else
      dfont = font;
    w = gdk_char_width_wc(dfont, wc);
    a = dfont->ascent;
    d = dfont->descent;
  }

  if (width != NULL) *width = w;
  if (ascent != NULL) *ascent = a;
  if (descent != NULL) *descent = d;
}

