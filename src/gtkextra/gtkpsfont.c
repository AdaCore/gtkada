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
#include "gtkpsfont.h"

#define FONTCACHE_SIZE 17
#define NUM_X11_FONTS 2


static GtkPSFont font_data[] = 
{
  { "Times-Roman",
    "Times-Roman",
    "Times-Roman",
    { "-adobe-times-medium-r-normal",
      NULL
    },
    FALSE, FALSE
  }, 
  { "Times-Italic",
    "Times-Italic",
    "Times-Roman",
    { "-adobe-times-medium-i-normal",
      NULL
    },
    TRUE, FALSE
  }, 
  { "Times-Bold",
    "Times-Bold",
    "Times-Roman",
    { "-adobe-times-bold-r-normal",
      NULL
    },
    FALSE, TRUE
  }, 
  { "Times-BoldItalic",
    "Times-BoldItalic",
    "Times-Roman",
    { "-adobe-times-bold-i-normal",
      NULL
    },
    TRUE, TRUE,
  }, 
  { "AvantGarde-Book",
    "AvantGarde-Book",
    "AvantGarde",
    { "-adobe-avantgarde-book-r-normal",
      "-schumacher-clean-medium-r-normal"
    },
    FALSE, FALSE,
  },
  { "AvantGarde-BookOblique",
    "AvantGarde-BookOblique",
    "AvantGarde",
    { "-adobe-avantgarde-book-o-normal",
      "-schumacher-clean-medium-i-normal"
    },
    TRUE, FALSE
  },
  { "AvantGarde-Demi",
    "AvantGarde-Demi",
    "AvantGarde",
    { "-adobe-avantgarde-demibold-r-normal",
      "-schumacher-clean-bold-r-normal"
    },
    FALSE, TRUE
  },
  { "AvantGarde-DemiOblique",
    "AvantGarde-DemiOblique",
    "AvantGarde",
    { "-adobe-avantgarde-demibold-o-normal",
      "-schumacher-clean-bold-i-normal"
    },
    TRUE, TRUE
  },
  { "Bookman-Light",
    "Bookman-Light",
    "Bookman",
    { "-adobe-bookman-light-r-normal",
      "-adobe-times-medium-r-normal"
    },
    FALSE, FALSE,
  },
  { "Bookman-LightItalic",
    "Bookman-LightItalic",
    "Bookman",
    { "-adobe-bookman-light-i-normal",
      "-adobe-times-medium-i-normal"
    },
    TRUE, FALSE,
  },
  { "Bookman-Demi",
    "Bookman-Demi",
    "Bookman",
    { "-adobe-bookman-demibold-r-normal",
      "-adobe-times-bold-r-normal"
    },
    FALSE, TRUE,
  },
  { "Bookman-DemiItalic",
    "Bookman-DemiItalic",
    "Bookman",
    { "-adobe-bookman-demibold-i-normal",
      "-adobe-times-bold-i-normal"
    },
    TRUE, TRUE,
  },
  { "Courier",
    "Courier",
    "Courier",
    { "-adobe-courier-medium-r-normal",
      NULL
    },
    FALSE, FALSE
  },
  { "Courier-Oblique",
    "Courier-Oblique",
    "Courier",
    { "-adobe-courier-medium-o-normal",
      NULL
    },
    TRUE, FALSE
  },
  { "Courier-Bold",
    "Courier-Bold",
    "Courier",
    { "-adobe-courier-bold-r-normal",
      NULL
    },
    FALSE, TRUE
  },
  { "Courier-BoldOblique",
    "Courier-BoldOblique",
    "Courier",
    { "-adobe-courier-bold-o-normal",
      NULL
    },
    TRUE, TRUE
  },
  { "Helvetica",
    "Helvetica",
    "Helvetica",
    { "-adobe-helvetica-medium-r-normal",
      NULL
    },
    FALSE, FALSE
  },
  { "Helvetica-Oblique",
    "Helvetica-Oblique",
    "Helvetica",
    { "-adobe-helvetica-medium-o-normal",
      NULL
    },
    TRUE, FALSE
  },
  { "Helvetica-Bold",
    "Helvetica-Bold",
    "Helvetica",
    { "-adobe-helvetica-bold-r-normal",
      NULL
    },
    FALSE, TRUE
  },
  { "Helvetica-BoldOblique",
    "Helvetica-BoldOblique",
    "Helvetica",
    { "-adobe-helvetica-bold-o-normal",
      NULL
    },
    TRUE, TRUE
  },
  { "Helvetica-Narrow",
    "Helvetica-Narrow",
    "Helvetica-Narrow",
    { "-adobe-helvetica-medium-r-normal",
      NULL
    },
    FALSE, FALSE
  },
  { "Helvetica-Narrow-Oblique",
    "Helvetica-Narrow-Oblique",
    "Helvetica-Narrow",
    { "-adobe-helvetica-medium-o-normal",
      NULL
    },
    TRUE, FALSE
  },
  { "Helvetica-Narrow-Bold",
    "Helvetica-Narrow-Bold",
    "Helvetica-Narrow",
    { "-adobe-helvetica-bold-r-normal",
      NULL
    },
    FALSE, TRUE
  },
  { "Helvetica-Narrow-BoldOblique",
    "Helvetica-Narrow-BoldOblique",
    "Helvetica-Narrow",
    { "-adobe-helvetica-bold-o-normal",
      NULL
    },
    TRUE, TRUE
  },
  { "NewCenturySchoolbook-Roman",
    "NewCenturySchlbk-Roman",
    "NewCenturySchlbk",
    { "-adobe-new century schoolbook-medium-r-normal",
      NULL
    },
    FALSE, FALSE
  },
  { "NewCenturySchoolbook-Italic",
    "NewCenturySchlbk-Italic",
    "NewCenturySchlbk",
    { "-adobe-new century schoolbook-medium-i-normal",
      NULL
    },
    TRUE, FALSE
  },
  { "NewCenturySchoolbook-Bold",
    "NewCenturySchlbk-Bold",
    "NewCenturySchlbk",
    { "-adobe-new century schoolbook-bold-r-normal",
      NULL
    },
    FALSE, TRUE
  },
  { "NewCenturySchoolbook-BoldItalic",
    "NewCenturySchlbk-BoldItalic",
    "NewCenturySchlbk",
    { "-adobe-new century schoolbook-bold-i-normal",
      NULL
    },
    TRUE, TRUE
  },
  { "Palatino-Roman",
    "Palatino-Roman",
    "Lucida",
    { "-adobe-palatino-medium-r-normal",
      "-*-lucidabright-medium-r-normal"
    },
    FALSE, FALSE
  },
  { "Palatino-Italic",
    "Palatino-Italic",
    "Lucida",
    { "-adobe-palatino-medium-i-normal",
      "-*-lucidabright-medium-i-normal"
    },
    TRUE, FALSE
  },
  { "Palatino-Bold",
    "Palatino-Bold",
    "Lucida",
    { "-adobe-palatino-bold-r-normal",
      "-*-lucidabright-demibold-r-normal"
    },
    FALSE, TRUE
  },
  { "Palatino-BoldItalic",
    "Palatino-BoldItalic",
    "Lucida",
    { "-adobe-palatino-bold-i-normal",
      "-*-lucidabright-demibold-i-normal"
    },
    TRUE, TRUE
  },
  { "Symbol",
    "Symbol",
    "Symbol",
    {
      "-adobe-symbol-medium-r-normal",
      "-*-symbol-medium-r-normal"
    },
    FALSE, FALSE
  },
  { "ZapfChancery-MediumItalic",
    "ZapfChancery-MediumItalic",
    "ZapfChancery",
    { "-adobe-zapf chancery-medium-i-normal",
      "-*-itc zapf chancery-medium-i-normal"
    },
    FALSE, FALSE
  },
  { "ZapfDingbats",
    "ZapfDingbats",
    "ZapfDingbats",
    { "-adobe-zapf dingbats-medium-r-normal",
      "-*-itc zapf dingbats-*-*-*"
    },
    FALSE, FALSE
  },
};

#define NUM_FONTS (sizeof(font_data)/sizeof(GtkPSFont))

gchar *last_resort_fonts[] = {
  "-adobe-courier-medium-r-normal",
  "fixed" /* Must be last. This is guaranteed to exist on an X11 system. */
};

#define NUM_LAST_RESORT_FONTS 2

static GList *user_fonts;
static gboolean psfont_init = FALSE;
static GList *psfont_families;
static gint numf;
static gint psfont_refcount = 0;

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
/*
  for(i = 0; i < NUM_FONTS; i++){
    gtk_psfont_add_font(font_data[i].fontname,
			font_data[i].psname,
			font_data[i].family,
			font_data[i].xfont,
			font_data[i].italic,
			font_data[i].bold);
  }
*/  
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
gtk_psfont_get_font(const gchar *name)
{
  GtkPSFont *font;

  font = find_psfont(name);

  if (font == NULL) {
    font = find_psfont("Courier");
    if (font == NULL) {
      g_warning ("Error, couldn't locate font. Shouldn't happend.");
    } else {
      g_message ("Font %s not found, using Courier instead.", name);
    }
  }

  return (GtkPSFont *)font;
}

GdkFont *
gtk_psfont_get_gdkfont(const gchar *name, gint height)
{
  GtkPSFont *fontdata;
  GdkFont *gdk_font = NULL;
  gchar *x11_font;
  gint bufsize;
  gchar *buffer = NULL;
  gint i;
  gint auxheight;
  gint min_height = 1;

  if (height <= 0) height = 1;
 
  fontdata = gtk_psfont_get_font(name);
 
  for (i = 0; i < NUM_X11_FONTS; i++) {
    x11_font = fontdata->xfont[i];
    if (x11_font != NULL) {
     bufsize = strlen(x11_font)+25;  /* Should be enought*/
     buffer = (gchar *)g_malloc(bufsize);

     for(auxheight = MAX(height, min_height); auxheight >= min_height; auxheight--){
      g_snprintf(buffer, bufsize, "%s-*-%d-*-*-*-*-*-*-*", x11_font, auxheight);
    
      gdk_font = gdk_font_load(buffer);
      if (gdk_font != NULL) {
         g_free(buffer);
         break;
      }
     }

     if(gdk_font != NULL) break;
    }

    g_free(buffer);
  }

  if (gdk_font == NULL) {
    for (i=0; i < NUM_LAST_RESORT_FONTS; i++) {
      x11_font = last_resort_fonts[i];
      bufsize = strlen(x11_font)+25;  /* Should be enought*/
      buffer = (char *)g_malloc(bufsize);
      
      for(auxheight = MAX(height, min_height); auxheight >= min_height; auxheight--){
       g_snprintf(buffer, bufsize, "%s-*-%d-*-*-*-*-*-*-*", x11_font, auxheight);
    
       gdk_font = gdk_font_load(buffer);
        if (gdk_font != NULL) {
          g_free(buffer);
          break;
       }
      }

      if (gdk_font != NULL) {
	g_warning("Could not find X Font for %s, using %s instead.",
		  name, x11_font);
	break;
      }

      g_free(buffer);
    }
  }

  if (gdk_font == NULL) 
	g_warning("Could not find X Font for %s", name);
	
  return gdk_font;
}


gchar *
gtk_psfont_get_psfontname(const gchar *fontname)
{
  GtkPSFont *font = NULL;
 
  font = find_psfont(fontname); 
  if(!font) 
     font = find_psfont("Courier");  

  return font->psname;
}

void
gtk_psfont_add_font (const gchar *fontname, const gchar *psname, const gchar *family,
                     gchar *x_string[],
                     gboolean italic, gboolean bold)
{
  GtkPSFont *font;

  font = g_new(GtkPSFont, 1);

  font->fontname = g_strdup(fontname); 
  font->psname = g_strdup(psname); 
  font->family = g_strdup(family); 
  font->xfont[0] = g_strdup(x_string[0]);
  font->xfont[1] = g_strdup(x_string[1]);
  font->italic = italic;
  font->bold = bold;

  user_fonts = g_list_append(user_fonts, font);
}

static GtkPSFont *
find_psfont(const gchar *name)
{
  GtkPSFont *fontdata = NULL;
  GtkPSFont *data = NULL;
  GList *fonts;
  gint i;

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


  if(fontdata == NULL) {
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
  }


  return fontdata;
}

GtkPSFont *
gtk_psfont_find_by_family(const gchar *name, gboolean italic, gboolean bold)
{
  GtkPSFont *fontdata = NULL;
  GtkPSFont *data = NULL;
  GtkPSFont *return_data = NULL;
  GList *fonts;
  gint i;

  for(i = 0; i < NUM_FONTS; i++){
    if(strcmp(name, font_data[i].family) == 0) { 
       return_data = &font_data[i];
       if(font_data[i].italic == italic && font_data[i].bold == bold){
           fontdata = &font_data[i];
           break;
       }
    }
  }

  if(fontdata == NULL) {
    fonts = user_fonts;
    while(fonts){
      data = (GtkPSFont *) fonts->data;
      if(strcmp(name, data->family) == 0) {
        return_data = &font_data[i];
        if(data->italic == italic && data->bold == bold){
            fontdata = data;
            break;
        }
      }
      fonts = fonts->next;
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
