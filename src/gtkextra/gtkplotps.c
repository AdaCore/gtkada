/* gtkplotps - postscript driver
 * Copyright 1999-2001  Adrian E. Feiguin <feiguin@ifir.edu.ar>
 *
 * Some few lines of code borrowed from
 * DiaCanvas -- a technical canvas widget
 * Copyright (C) 1999 Arjan Molenaar
 * Dia -- an diagram creation/manipulation program
 * Copyright (C) 1998 Alexander Larsson
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

#include "gtkplot.h"
#include "gtkpsfont.h"
#include "gtkplotpc.h"
#include "gtkplotps.h"

static void gtk_plot_ps_class_init 		(GtkPlotPSClass *klass);
static void gtk_plot_ps_init 			(GtkPlotPS *ps);
static void gtk_plot_ps_finalize 		(GtkObject *object);
/*********************************************************************/
/* Postscript specific functions */
static gboolean psinit				(GtkPlotPC *pc); 
static void psleave				(GtkPlotPC *pc);
static void psgsave				(GtkPlotPC *pc);
static void psgrestore				(GtkPlotPC *pc);
static void psclip				(GtkPlotPC *pc,
						 const GdkRectangle *area);
static void psdrawlines				(GtkPlotPC *pc,
						 GtkPlotPoint *points, 
						 gint numpoints);
static void psdrawpoint				(GtkPlotPC *pc, 
                				 gdouble x, gdouble y); 
static void psdrawline				(GtkPlotPC *pc,
						 gdouble x0, gdouble y0, 
						 gdouble xf, gdouble yf);
static void psdrawpolygon			(GtkPlotPC *pc,
						 gboolean filled,
						 GtkPlotPoint *points, 
						 gint numpoints); 
static void psdrawrectangle			(GtkPlotPC *pc, 
						 gboolean filled, 
                				 gdouble x, gdouble y, 
						 gdouble width, gdouble height);
static void psdrawcircle			(GtkPlotPC *pc,
						 gboolean filled,
                                                 gdouble x, gdouble y, 
						 gdouble size);
static void psdrawellipse			(GtkPlotPC *pc, 
              					 gboolean filled,
						 gdouble x, gdouble y, 
						 gdouble width, gdouble height); 
static void pssetcolor				(GtkPlotPC *pc, 
						 const GdkColor *color); 
static void pssetlineattr			(GtkPlotPC *pc, 
                                                 gfloat line_width,
                                                 GdkLineStyle line_style,
                                                 GdkCapStyle cap_style,
                                                 GdkJoinStyle join_style);
static void psdrawstring			(GtkPlotPC *pc,
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
static void pssetfont				(GtkPlotPC *pc, 
						 const gchar *font, 
						 gint height);
static void pssetdash				(GtkPlotPC *pc, 
						 gdouble offset,
						 gdouble *values,
						 gint num_values);
/*********************************************************************/
static GtkPlotPCClass *parent_class = NULL;

GtkType
gtk_plot_ps_get_type (void)
{
  static GtkType pc_type = 0;

  if (!pc_type)
    {
      GtkTypeInfo pc_info =
      {
        "GtkPlotPS",
        sizeof (GtkPlotPS),
        sizeof (GtkPlotPSClass),
        (GtkClassInitFunc) gtk_plot_ps_class_init,
        (GtkObjectInitFunc) gtk_plot_ps_init,
        /* reserved 1*/ NULL,
        /* reserved 2 */ NULL,
        (GtkClassInitFunc) NULL,
      };

      pc_type = gtk_type_unique (GTK_TYPE_PLOT_PC, &pc_info);
    }
  return pc_type;
}

static void
gtk_plot_ps_init (GtkPlotPS *ps)
{
  ps->psname = NULL;
  ps->gsaved = FALSE;
}


static void
gtk_plot_ps_class_init (GtkPlotPSClass *klass)
{
  GtkObjectClass *object_class;
  GtkPlotPCClass *pc_class;

  parent_class = gtk_type_class (gtk_plot_pc_get_type ());

  object_class = (GtkObjectClass *) klass;
  pc_class = (GtkPlotPCClass *) klass;

  pc_class->init = psinit;
  pc_class->leave = psleave;
  pc_class->gsave = psgsave;
  pc_class->grestore = psgrestore;
  pc_class->clip = psclip;
  pc_class->set_color = pssetcolor;
  pc_class->set_dash = pssetdash;
  pc_class->set_lineattr = pssetlineattr;
  pc_class->draw_point = psdrawpoint;
  pc_class->draw_line = psdrawline;
  pc_class->draw_lines = psdrawlines;
  pc_class->draw_rectangle = psdrawrectangle;
  pc_class->draw_polygon = psdrawpolygon;
  pc_class->draw_circle = psdrawcircle;
  pc_class->draw_ellipse = psdrawellipse;
  pc_class->set_font = pssetfont;
  pc_class->draw_string = psdrawstring;

  object_class->finalize = gtk_plot_ps_finalize;
}

static void
gtk_plot_ps_finalize(GtkObject *object)
{
  GtkPlotPS *ps;

  ps = GTK_PLOT_PS(object);

  if(ps->psname) g_free(ps->psname);
}

GtkObject *
gtk_plot_ps_new                         (const gchar *psname,
                                         gint orientation,
                                         gint epsflag,
                                         gint page_size,
                                         gdouble scalex,
					 gdouble scaley)
{
  GtkObject *object;
  GtkPlotPS *ps;

  object = gtk_type_new(gtk_plot_ps_get_type());

  ps = GTK_PLOT_PS(object);

  gtk_plot_ps_construct(ps, psname, orientation, epsflag, page_size, scalex, scaley);

  return (object);
}

void
gtk_plot_ps_construct                   (GtkPlotPS *ps,
					 const gchar *psname,
                                         gint orientation,
                                         gint epsflag,
                                         gint page_size,
                                         gdouble scalex,
					 gdouble scaley)
{
  gint width, height;

  ps->psname = g_strdup(psname);
  ps->orientation = orientation;
  ps->epsflag = epsflag;
  ps->page_size = page_size;
  ps->scalex = scalex;
  ps->scaley = scaley;

  switch (page_size){
   case GTK_PLOT_LEGAL:
        width = GTK_PLOT_LEGAL_W;
        height = GTK_PLOT_LEGAL_H;
        break;
   case GTK_PLOT_A4:
        width = GTK_PLOT_A4_W;
        height = GTK_PLOT_A4_H;
        break;
   case GTK_PLOT_EXECUTIVE:
        width = GTK_PLOT_EXECUTIVE_W;
        height = GTK_PLOT_EXECUTIVE_H;
        break;
   case GTK_PLOT_LETTER:
   default:
        width = GTK_PLOT_LETTER_W;
        height = GTK_PLOT_LETTER_H;
  }

  gtk_plot_ps_set_size(ps, GTK_PLOT_PSPOINTS, width, height);
}

GtkObject *
gtk_plot_ps_new_with_size                       (const gchar *psname,
                                                 gint orientation,
                                                 gint epsflag,
                                                 gint units,
                                                 gdouble width, gdouble height,
						 gdouble scalex, gdouble scaley)
{
  GtkObject *object;
  GtkPlotPS *ps;

  object = gtk_type_new(gtk_plot_ps_get_type());

  ps = GTK_PLOT_PS(object);

  gtk_plot_ps_construct_with_size (ps, psname, orientation, epsflag, units, width, height, scalex, scaley);

  return object;
}

void
gtk_plot_ps_construct_with_size                 (GtkPlotPS *ps,
						 const gchar *psname,
                                                 gint orientation,
                                                 gint epsflag,
                                                 gint units,
                                                 gdouble width, gdouble height,
						 gdouble scalex, gdouble scaley)
{
  gtk_plot_ps_construct(ps, psname, orientation, epsflag, GTK_PLOT_CUSTOM, scalex, scaley);

  gtk_plot_ps_set_size(ps, units, width, height);
}

void
gtk_plot_ps_set_size                            (GtkPlotPS *ps,
                                                 gint units,
                                                 gdouble width,
                                                 gdouble height)
{
  ps->units = units;
  ps->width = width;
  ps->height = height;

  switch(units){
   case GTK_PLOT_MM:
        ps->page_width = (gdouble)width * 2.835;
        ps->page_height = (gdouble)height * 2.835;
        break;
   case GTK_PLOT_CM:
        ps->page_width = width * 28.35;
        ps->page_height = height * 28.35;
        break;
   case GTK_PLOT_INCHES:
        ps->page_width = width * 72;
        ps->page_height = height * 72;
        break;
   case GTK_PLOT_PSPOINTS:
   default:
        ps->page_width = width;
        ps->page_height = height;
   }

}

void
gtk_plot_ps_set_scale                           (GtkPlotPS *ps,
                                                 gdouble scalex,
                                                 gdouble scaley)
{
  ps->scalex = scalex;
  ps->scaley = scaley; 
}

static void pssetlineattr			(GtkPlotPC *pc, 
                                                 gfloat line_width,
                                                 GdkLineStyle line_style,
                                                 GdkCapStyle cap_style,
                                                 GdkJoinStyle join_style)
{
    FILE *psout = GTK_PLOT_PS(pc)->psfile;

    fprintf(psout, "%f slw\n", line_width);
    fprintf(psout, "%d slc\n", abs(cap_style - 1));
    fprintf(psout, "%d slj\n", join_style);

    if(line_style == 0)
            fprintf(psout,"[] 0 sd\n");  /* solid line */
}

static void 
pssetdash(GtkPlotPC *pc,
          gdouble offset, 
          gdouble *values,
          gint num_values)
{
    FILE *psout = GTK_PLOT_PS(pc)->psfile;

    switch(num_values){
      case 0:
        fprintf(psout,"[] 0 sd\n");
        break;
      case 2:
        fprintf(psout, "[%g %g] %g sd\n", values[0], values[1], offset);
        break;
      case 4:
        fprintf(psout, "[%g %g %g %g] %g sd\n", values[0], values[1],
                                                values[2], values[3], 
                                                offset);
        break;
      case 6:
        fprintf(psout, "[%g %g %g %g %g %g] %g sd\n",  values[0], values[1],
                                                       values[2], values[3], 
                                                       values[4], values[5], 
                                                       offset);
        break;
      default:
        break;
    }
}

static void 
psleave(GtkPlotPC *pc)
{
    fprintf(GTK_PLOT_PS(pc)->psfile, "showpage\n");
    fprintf(GTK_PLOT_PS(pc)->psfile, "%%%%Trailer\n");
    fprintf(GTK_PLOT_PS(pc)->psfile, "%%%%EOF\n");
    fclose(GTK_PLOT_PS(pc)->psfile);
}

static gboolean 
psinit						(GtkPlotPC *pc)
{
    time_t now;
    FILE *psout;
    GtkPlotPS *ps;

    now = time(NULL);

    ps = GTK_PLOT_PS(pc);
    psout = ps->psfile;

    if ((psout = fopen(ps->psname, "w")) == NULL){
       g_warning("ERROR: Cannot open file: %s", ps->psname); 
       return FALSE;
    }

    ps->psfile = psout;

    if(ps->epsflag)
       fprintf (psout, "%%!PS-Adobe-2.0 PCF-2.0\n");
    else
       fprintf (psout, "%%!PS-Adobe-2.0\n");

    fprintf (psout,
             "%%%%Title: %s\n"
             "%%%%Creator: %s v%s Copyright (c) 1999 Adrian E. Feiguin\n"
             "%%%%CreationDate: %s"
             "%%%%Magnification: 1.0000\n",
             ps->psname,
             "GtkPlot", "3.x",
             ctime (&now));

    if(ps->orientation == GTK_PLOT_PORTRAIT)
             fprintf(psout,"%%%%Orientation: Portrait\n");
    else
             fprintf(psout,"%%%%Orientation: Landscape\n");

    if(ps->epsflag)
          fprintf (psout,
                   "%%%%BoundingBox: 0 0 %d %d\n"
                   "%%%%Pages: 1\n"
                   "%%%%EndComments\n",
                   ps->page_width,
                   ps->page_height);


    fprintf (psout,
             "/cp {closepath} bind def\n"
             "/c {curveto} bind def\n"
             "/f {fill} bind def\n"
             "/a {arc} bind def\n"
             "/ef {eofill} bind def\n"
             "/ex {exch} bind def\n"
             "/gr {grestore} bind def\n"
             "/gs {gsave} bind def\n"
             "/sa {save} bind def\n"
             "/rs {restore} bind def\n"
             "/l {lineto} bind def\n"
             "/m {moveto} bind def\n"
             "/rm {rmoveto} bind def\n"
             "/n {newpath} bind def\n"
             "/s {stroke} bind def\n"
             "/sh {show} bind def\n"
             "/slc {setlinecap} bind def\n"
             "/slj {setlinejoin} bind def\n"
             "/slw {setlinewidth} bind def\n"
             "/srgb {setrgbcolor} bind def\n"
             "/rot {rotate} bind def\n"
             "/sc {scale} bind def\n"
             "/sd {setdash} bind def\n"
             "/ff {findfont} bind def\n"
             "/sf {setfont} bind def\n"
             "/scf {scalefont} bind def\n"
             "/sw {stringwidth pop} bind def\n"
             "/tr {translate} bind def\n"

             "/JR {\n"
             " neg 0\n"
             " rmoveto\n"
             "} bind def\n"

             "/JC {\n"
             " 2 div neg 0\n"
             " rmoveto\n"
             "} bind def\n"
  
             "\n/ellipsedict 8 dict def\n"
             "ellipsedict /mtrx matrix put\n"
             "/ellipse\n"
             "{ ellipsedict begin\n"
             "   /endangle exch def\n"
             "   /startangle exch def\n"
             "   /yrad exch def\n"
             "   /xrad exch def\n"
             "   /y exch def\n"
             "   /x exch def"
             "   /savematrix mtrx currentmatrix def\n"
             "   x y tr xrad yrad sc\n"
             "   0 0 1 startangle endangle arc\n"
             "   savematrix setmatrix\n"
             "   end\n"
             "} def\n\n"
    ); 
    
    if(ps->orientation == GTK_PLOT_PORTRAIT)
             fprintf(psout, "%d %d translate\n"
                            "%f %f scale\n",
                            0, ps->page_height,
                            ps->scalex, -ps->scaley);

    if(ps->orientation == GTK_PLOT_LANDSCAPE)
             fprintf(psout, "%f %f scale\n"
                            "-90 rotate \n",
                            ps->scalex, -ps->scaley);

    fprintf(psout,"%%%%EndProlog\n\n\n");

    return TRUE;
}

static void pssetcolor(GtkPlotPC *pc, const GdkColor *color)
{
    FILE *psout = GTK_PLOT_PS(pc)->psfile;

    fprintf(psout, "%f %f %f setrgbcolor\n",
	    (gdouble) color->red / 65535.0,
	    (gdouble) color->green / 65535.0,
	    (gdouble) color->blue / 65535.0);
}

static void
psdrawpoint(GtkPlotPC *pc, gdouble x, gdouble y)
{
  FILE *psout = GTK_PLOT_PS(pc)->psfile;

  fprintf(psout, "%f %f m\n", x, y);
  fprintf(psout, "%f %f l\n", x, y);
  fprintf(psout, "s\n");
}

static void
psdrawlines(GtkPlotPC *pc, GtkPlotPoint *points, gint numpoints)
{
  gint i;
  FILE *psout = GTK_PLOT_PS(pc)->psfile;
 
  fprintf(psout,"n\n");
  fprintf(psout,"%f %f m\n", points[0].x, points[0].y);
  for(i = 1; i < numpoints; i++)
        fprintf(psout,"%f %f l\n", points[i].x, points[i].y);

  fprintf(psout,"s\n");
}

static void
psdrawpolygon(GtkPlotPC *pc, gboolean filled, GtkPlotPoint *points, gint numpoints)
{
  gint i;
  FILE *psout = GTK_PLOT_PS(pc)->psfile;

  fprintf(psout,"n\n");
  fprintf(psout,"%f %f m\n", points[0].x, points[0].y);
  for(i = 1; i < numpoints; i++)
      fprintf(psout,"%f %f l\n", points[i].x, points[i].y);

  if(filled)
     fprintf(psout,"f\n");
  else
     fprintf(psout,"cp\n");

  fprintf(psout,"s\n");
}

static void psdrawline(GtkPlotPC *pc, gdouble x0, gdouble y0, gdouble xf, gdouble yf)
{
  FILE *psout = GTK_PLOT_PS(pc)->psfile;

  fprintf(psout, "%f %f m\n", x0, y0);
  fprintf(psout, "%f %f l\n", xf, yf);
  fprintf(psout, "s\n");
}

static void
psdrawrectangle(GtkPlotPC *pc, gboolean filled, 
                gdouble x, gdouble y, gdouble width, gdouble height)
{
  GtkPlotPoint point[4];

  point[0].x = x;
  point[0].y = y;
  point[1].x = x + width;
  point[1].y = y;
  point[2].x = x + width;
  point[2].y = y + height;
  point[3].x = x;
  point[3].y = y + height;

  psdrawpolygon(pc, filled, point, 4);
}

static void
psdrawcircle(GtkPlotPC *pc, gboolean filled, gdouble x, gdouble y, gdouble size)
{
  FILE *psout = GTK_PLOT_PS(pc)->psfile;

  fprintf(psout,"n %f %f %f %f 0 360 ellipse\n", 
          x, y, size / 2., size / 2.);

  if(filled)
     fprintf(psout,"f\n");
  else
     fprintf(psout,"cp\n");

  fprintf(psout,"s\n");
}

static void
psdrawellipse(GtkPlotPC *pc, 
              gboolean filled, 
              gdouble x, gdouble y, 
              gdouble width, gdouble height)
{
  FILE *psout = GTK_PLOT_PS(pc)->psfile;

  fprintf(psout,"n %f %f %f %f 0 360 ellipse\n", 
          x+width/2., y+height/2., 
          width/2., height/2.);

  if(filled)
     fprintf(psout,"f\n");
  else
     fprintf(psout,"cp\n");

  fprintf(psout,"s\n");
}

static void
psdrawstring	(GtkPlotPC *pc,
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
                 GtkJustification justification,
                 const gchar *text)

{
  gchar *curstr;
  gchar *currfont;
  const gchar *aux = 0;
  GtkPSFont *psfont;
  gchar bkspchar, insert_char, num[4];
  const gchar *xaux = 0;
  const gchar *lastchar = NULL;
  gint curcnt = 0, offset = 0;
  gint numf;
  gdouble scale;
  gboolean italic, bold;
  gboolean special = FALSE;
  GList *family;
  FILE *psout;
  gint twidth, theight, tdescent, tascent;
  gint i;

  if (text == NULL || strlen(text) == 0) return;

  psout = GTK_PLOT_PS(pc)->psfile;

  curstr = (gchar *)g_malloc(2*strlen(text)*sizeof(gchar));
 
  gtk_psfont_get_families(&family, &numf);
  psfont = gtk_psfont_get_font(font);
  italic = psfont->italic;
  bold = psfont->bold;

  currfont = psfont->psname;

  gtk_plot_text_get_size(text, angle, psfont->psname, height, 
                         &twidth, &theight, &tascent, &tdescent);

  if(angle == 90 || angle == 270) angle = 360 - angle;
  psgsave(pc);
  fprintf(psout, "%d %d translate\n", x, y);
  fprintf(psout, "%d rotate\n", angle);

  fprintf(psout, "0 0 m\n");
  fprintf(psout, "1 -1 sc\n");

  pssetcolor(pc, fg);
  pssetfont(pc, currfont, height);

  aux = text;
  while(aux && *aux != '\0' && *aux != '\n') {
     if(*aux == '\\'){
         aux++;
         switch(*aux){
           case '0': case '1': case '2': case '3':
           case '4': case '5': case '6': case '7': case '9':
           case '8': case'g': case 'B': case 'b': case 'x': case 'N':
           case 's': case 'S': case 'i': case '-': case '+': case '^':
             special = TRUE;
             break;
           default:
             break;
         }
     } else {
         aux++;
     }
  }

  if(special){
    switch (justification) {
      case GTK_JUSTIFY_LEFT:
        break;
      case GTK_JUSTIFY_RIGHT:
        if(angle == 0 || angle == 180)
               fprintf(psout, "%d JR\n", twidth);
        else
               fprintf(psout, "%d JR\n", theight);
        break;
      case GTK_JUSTIFY_CENTER:
      default:
        if(angle == 0 || angle == 180)
               fprintf(psout, "%d JC\n", twidth);
        else
               fprintf(psout, "%d JC\n", theight);
        break;
    }
  } else {
    switch (justification) {
      case GTK_JUSTIFY_LEFT:
        break;
      case GTK_JUSTIFY_RIGHT:
        fprintf(psout, "(%s) sw JR\n", text);
        break;
      case GTK_JUSTIFY_CENTER:
      default:
        fprintf(psout, "(%s) sw JC\n", text);
        break;
    }
    fprintf(psout, "(%s) show\n", text);

    psgrestore(pc);  
    fprintf(psout, "n\n");  
    return;
  }

  aux = text;
  scale = height;
  curcnt = 0;

  while(aux && *aux != '\0' && *aux != '\n') {
     if(*aux == '\\'){
         aux++;
         switch(*aux){
           case '0': case '1': case '2': case '3':
           case '4': case '5': case '6': case '7': case '9':
                  curstr[curcnt] = 0;
                  if (curcnt >= 1) {
                      fprintf(psout, "(%s) show\n", curstr);
                  }
                  curcnt = 0;
                  psfont = gtk_psfont_find_by_family((gchar *)g_list_nth_data(family, *aux-'0'), italic, bold);
                  currfont = psfont->psname;
                  pssetfont(pc, currfont, (gint)scale);
                  aux++;
                  break;
           case '8':case'g':
                  curstr[curcnt] = 0;
                  if (curcnt >= 1) {
                      fprintf(psout, "(%s) show\n", curstr);
                  }
                  curcnt = 0;
                  psfont = gtk_psfont_find_by_family("Symbol", italic, bold);
                  currfont = psfont->psname;
                  pssetfont(pc, currfont, (gint)scale);
                  aux++;
                  break;
           case 'B':
                  curstr[curcnt] = 0;
                  if (curcnt >= 1) {
                      fprintf(psout, "(%s) show\n", curstr);
                  }
                  curcnt = 0;
  		  bold = TRUE;
                  psfont = gtk_psfont_find_by_family(psfont->family, italic, bold);
                  currfont = psfont->psname;
                  pssetfont(pc, currfont, (gint)scale);
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
                  curstr[curcnt++] = insert_char;
                  aux += 4;
                  break;
           case 'i':
                  curstr[curcnt] = 0;
                  if (curcnt >= 1) {
                      fprintf(psout, "(%s) show\n", curstr);
                  }
                  curcnt = 0;
      	    	  italic = TRUE;
                  psfont = gtk_psfont_find_by_family(psfont->family, italic, bold);
                  currfont = psfont->psname;
                  pssetfont(pc, currfont, (gint)scale);
                  aux++;
                  break;
           case 's':case'_':
                  curstr[curcnt] = 0;
                  if (curcnt >= 1) {
                      fprintf(psout, "(%s) show\n", curstr);
                  }
                  curcnt = 0;
                  scale = 0.6 * height;
                  pssetfont(pc, currfont, (gint)scale);
                  offset -= (gint)scale / 2;
                  fprintf(psout, "0 %d rmoveto\n", -((gint)scale / 2));
                  aux++;
                  break;
           case 'S':case '^':
                  curstr[curcnt] = 0;
                  if (curcnt >= 1) {
                      fprintf(psout, "(%s) show\n", curstr);
                  }
                  curcnt = 0;
                  scale = 0.6 * height;
                  pssetfont(pc, currfont, (gint)scale);
                  offset += 0.5*height;
                  fprintf(psout, "0 %d rmoveto\n", (gint)(0.5*height));
                  aux++;
                  break;
           case 'N':
                  curstr[curcnt] = 0;
                  if (curcnt >= 1) {
                      fprintf(psout, "(%s) show\n", curstr);
                  }
                  curcnt = 0;
                  scale = height;
                  psfont = gtk_psfont_get_font(font);
                  currfont = psfont->psname;
                  pssetfont(pc, currfont, (gint)scale);
                  fprintf(psout, "0 %d rmoveto\n", -offset);
                  offset = 0;
                  aux++;
                  break;
           case 'b':
                  curstr[curcnt] = '\0';
                  if(curcnt >= 1)
                      fprintf(psout, "(%s) show\n", curstr);
                  if (lastchar) {
                      bkspchar = *lastchar;
                      lastchar--;
                  } else {
                      bkspchar = 'X';
                      lastchar = NULL;
                  }
                  fprintf(psout,
                  "(%c) stringwidth pop 0 exch neg exch rmoveto\n", bkspchar );
                  curcnt = 0;
                  aux++;
                  break;
           case '-':
                  curstr[curcnt] = 0;
                  if (curcnt >= 1) {
                      fprintf(psout, "(%s) show\n", curstr);
                  }
                  curcnt = 0;
                  scale -= 3;
                  if (scale < 6) {
                      scale = 6;
                  }
                  pssetfont(pc, currfont, (gint)scale);
                  aux++;
                  break;
           case '+':
                  curstr[curcnt] = 0;
                  if (curcnt >= 1) {
                      fprintf(psout, "(%s) show\n", curstr);
                  }
                  curcnt = 0;
                  scale += 3;
                  pssetfont(pc, currfont, (gint)scale);
                  aux++;
                  break;
           default:
                  if(aux && *aux != '\0' && *aux !='\n'){
                    curstr[curcnt++] = *aux;
                    aux++;
                  }
                  break;
         }
     } else {

       if(*aux == ')' || *aux == '(')
                curstr[curcnt++] = '\\';

       if(aux && *aux != '\0' && *aux !='\n'){
                curstr[curcnt++] = *aux;
		lastchar = aux;
                aux++;

       }
     }
  }
  curstr[curcnt] = 0;

  fprintf(psout, "(%s) show\n", curstr);

  psgrestore(pc);  
  fprintf(psout, "n\n");  

  g_free(curstr);
}

static void
pssetfont(GtkPlotPC *pc, const gchar *font, gint height)
{
  FILE *psout = GTK_PLOT_PS(pc)->psfile;

  fprintf(psout, "/%s ff %f scf sf\n", font, (double)height);
}


static void
psgsave(GtkPlotPC *pc)
{
  GtkPlotPS *ps;
  FILE *psout;

  ps = GTK_PLOT_PS(pc);

  psout = ps->psfile;

  fprintf(psout,"gsave\n");
  ps->gsaved = TRUE;
}

static void
psgrestore(GtkPlotPC *pc)
{
  GtkPlotPS *ps;
  FILE *psout;

  ps = GTK_PLOT_PS(pc);

  psout = ps->psfile;

  if(!ps->gsaved) return;

  fprintf(psout,"grestore\n");
  ps->gsaved = FALSE;
}

static void
psclip(GtkPlotPC *pc, const GdkRectangle *clip)
{
  FILE *psout = GTK_PLOT_PS(pc)->psfile;

  if(!clip){ 
    fprintf(psout,"grestore\n");
    return;
  }

  fprintf(psout,"gsave\n");
  fprintf(psout,"%d %d %d %d rectclip\n", clip->x, clip->y, clip->width, clip->height);
}


