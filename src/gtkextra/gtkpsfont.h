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


#ifndef __GTK_PS_FONT_H__
#define __GTK_PS_FONT_H__

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


/* List of names of the 35 default Adobe fonts 
   "Times-Roman",
   "Times-Italic",
   "Times-Bold",
   "Times-BoldItalic",
   "AvantGarde-Book",
   "AvantGarde-BookOblique",
   "AvantGarde-Demi",
   "AvantGarde-DemiOblique",
   "Bookman-Light",
   "Bookman-LightItalic",
   "Bookman-Demi",
   "Bookman-DemiItalic",
   "Courier",
   "Courier-Oblique",
   "Courier-Bold",
   "Courier-BoldOblique",
   "Helvetica",
   "Helvetica-Oblique",
   "Helvetica-Bold",
   "Helvetica-BoldOblique",
   "Helvetica-Narrow",
   "Helvetica-Narrow-Oblique",
   "Helvetica-Narrow-Bold",
   "Helvetica-Narrow-BoldOblique",
   "NewCenturySchoolbook-Roman",
   "NewCenturySchoolbook-Italic",
   "NewCenturySchoolbook-Bold",
   "NewCenturySchoolbook-BoldItalic",
   "Palatino-Roman",
   "Palatino-Italic",
   "Palatino-Bold",
   "Palatino-BoldItalic",
   "Symbol",
   "ZapfChancery-MediumItalic",
   "ZapfDingbats",
*/


typedef struct _GtkPSFont GtkPSFont;

struct _GtkPSFont {
  gchar *fontname;
  gchar *psname;
  gchar *family;
  gchar *xfont[2];
  gboolean italic;
  gboolean bold;
};

gint		gtk_psfont_init			();
void		gtk_psfont_unref		();
GtkPSFont* 	gtk_psfont_get_font 		(gchar *name);
GdkFont*	gtk_psfont_get_gdkfont 		(gchar *name, gint height);
gchar *		gtk_psfont_get_psfontname	(gchar *name);
void		gtk_psfont_add_font		(gchar *fontname,
						 gchar *psname,
						 gchar *family,
						 gchar *x_string[],
                                                 gboolean italic,
                                                 gboolean bold);
GtkPSFont * 	gtk_psfont_find_by_family       (gchar *name, 
                                                 gboolean italic, 
                                                 gboolean bold);
void 		gtk_psfont_get_families		(GList **family, gint *numf);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GTK_PS_FONT_H__ */
