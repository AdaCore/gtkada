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

#include "gtkplotpc.h"
#include "gtkplot.h"
#include "gtkpsfont.h"
#include "gtkplotcanvas.h"
#include "gtkplotprint.h"

/*********************************************************************/
/* Postscript specific functions */
static void psinit				(GtkPlotPC *pc, 
						 gfloat scalex, 
						 gfloat scaley);
static void psleave				(GtkPlotPC *pc);
static void psgsave				(GtkPlotPC *pc);
static void psgrestore				(GtkPlotPC *pc);
static void psclip				(GtkPlotPC *pc,
						 GdkRectangle area);
static void psdrawlines				(GtkPlotPC *pc,
						 GdkPoint *points, 
						 gint numpoints);
static void psdrawline				(GtkPlotPC *pc,
						 gint x0, gint y0, 
						 gint xf, gint yf);
static void psdrawpolygon			(GtkPlotPC *pc,
						 GdkPoint *points, 
						 gint numpoints, 
						 gint filled);
static void psdrawcircle			(GtkPlotPC *pc,
                                                 gint x, gint y, 
						 gint size, gint filled);
static void psdrawellipse			(GtkPlotPC *pc, 
						 gint x, gint y, 
						 gint width, gint height, 
              					 gint filled);
static void pssetcolor				(GtkPlotPC *pc, 
						 GdkColor *color); 
static void pssetlinewidth			(GtkPlotPC *pc, 
						 gint width);
static void pssetlinecaps			(GtkPlotPC *pc, 
						 gint caps);
static void psdrawstring			(GtkPlotPC *pc,
             					 gint x, gint y,
             					 GtkJustification justification,
             					 gint angle,
						 gchar *font,
						 gint height,
             					 gchar *text);
static void pssetfont				(GtkPlotPC *pc, 
						 gchar *font, 
						 gint height);
static void pssetdash				(GtkPlotPC *pc, 
						 gint num_values,
						 gdouble *values,
						 gdouble offset);
/*********************************************************************/
static void pssetcolor(GtkPlotPC *pc, GdkColor *color)
{
    FILE *psout = pc->pcfile;

    fprintf(psout, "%f %f %f setrgbcolor\n",
	    (gdouble) color->red / 65535.0,
	    (gdouble) color->green / 65535.0,
	    (gdouble) color->blue / 65535.0);
}

static void pssetlinewidth(GtkPlotPC *pc, gint width)
{
    FILE *psout = pc->pcfile;

    fprintf(psout, "%d slw\n", (int)width);
}

static void 
pssetdash(GtkPlotPC *pc,
          gint num_values,
          gdouble *values,
          gdouble offset) 
{
    FILE *psout = pc->pcfile;

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
    fprintf(pc->pcfile, "showpage\n");
    fprintf(pc->pcfile, "%%%%Trailer\n");
    fclose(pc->pcfile);
}

static void 
psinit						(GtkPlotPC *pc,
                                                 gfloat scalex,
                                                 gfloat scaley)
{
    time_t now;
    FILE *psout;

    now = time(NULL);

    if ((psout = fopen(pc->pcname, "w")) == NULL){
       g_warning("ERROR: Cannot open file: %s", pc->pcname); 
       return;
    }

    pc->pcfile = psout;

    if(pc->epsflag)
       fprintf (psout, "%%!PS-Adobe-2.0 PCF-2.0\n");
    else
       fprintf (psout, "%%!PS-Adobe-2.0\n");

    fprintf (psout,
             "%%%%Title: %s\n"
             "%%%%Creator: %s v%s Copyright (c) 1999 Adrian E. Feiguin\n"
             "%%%%CreationDate: %s"
             "%%%%Magnification: 1.0000\n",
             pc->pcname,
             "GtkPlot", "3.x",
             ctime (&now));

    if(pc->orientation == GTK_PLOT_PORTRAIT)
             fprintf(psout,"%%%%Orientation: Portrait\n");
    else
             fprintf(psout,"%%%%Orientation: Landscape\n");

    if(pc->epsflag)
          fprintf (psout,
                   "%%%%BoundingBox: 0 0 %d %d\n"
                   "%%%%Pages: 1\n"
                   "%%%%EndComments\n",
                   pc->page_width,
                   pc->page_height);


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
    
    if(pc->orientation == GTK_PLOT_PORTRAIT)
             fprintf(psout, "%d %d translate\n"
                            "%f %f scale\n",
                            0, pc->page_height,
                            scalex, -scaley);

    if(pc->orientation == GTK_PLOT_LANDSCAPE)
             fprintf(psout, "%f %f scale\n"
                            "-90 rotate \n",
                            scalex, -scaley);

    fprintf(psout,"%%%%EndProlog\n\n\n");

}

void 
gtk_plot_export_ps			        (GtkPlot *plot, 
					 	 char *pcname, 
						 int orient, 
						 int epsflag, 
						 gint page_size)
{
  GtkPlotPC *pc;

  pc = gtk_plot_pc_new(pcname, orient, page_size);

  pc->epsflag = epsflag;

  pc->init = psinit;
  pc->leave = psleave;
  pc->gsave = psgsave;
  pc->grestore = psgrestore;
  pc->clip = psclip;
  pc->setcolor = pssetcolor;
  pc->setdash = pssetdash;
  pc->setlinewidth = pssetlinewidth;
  pc->setlinecaps = pssetlinecaps;
  pc->drawline = psdrawline;
  pc->drawlines = psdrawlines;
  pc->drawpolygon = psdrawpolygon;
  pc->drawcircle = psdrawcircle;
  pc->drawellipse = psdrawellipse;
  pc->setfont = pssetfont;
  pc->drawstring = psdrawstring;

  gtk_plot_print(plot, pc);

  g_free(pc->pcname);
  g_free(pc);
}

void 
gtk_plot_export_ps_with_size			(GtkPlot *plot, 
					 	 char *pcname, 
						 gint orient, 
						 int epsflag, 
						 gint units,
						 gint width,
                                                 gint height)
{
  GtkPlotPC *pc;

  pc = gtk_plot_pc_new_with_size(pcname, 
				 orient, 
                                 units, width, height);

  pc->epsflag = epsflag;

  pc->init = psinit;
  pc->leave = psleave;
  pc->gsave = psgsave;
  pc->grestore = psgrestore;
  pc->clip = psclip;
  pc->setcolor = pssetcolor;
  pc->setdash = pssetdash;
  pc->setlinewidth = pssetlinewidth;
  pc->setlinecaps = pssetlinecaps;
  pc->drawline = psdrawline;
  pc->drawlines = psdrawlines;
  pc->drawpolygon = psdrawpolygon;
  pc->drawcircle = psdrawcircle;
  pc->drawellipse = psdrawellipse;
  pc->setfont = pssetfont;
  pc->drawstring = psdrawstring;

  gtk_plot_print(plot, pc);

  g_free(pc->pcname);
  g_free(pc);
}

void 
gtk_plot_canvas_export_ps			(GtkPlotCanvas *canvas, 
					 	 char *pcname, 
						 int orient, 
						 int epsflag, 
						 gint page_size)
{
  GtkPlotPC *pc;

  pc = gtk_plot_pc_new(pcname, orient, page_size);

  pc->init = psinit;
  pc->leave = psleave;
  pc->gsave = psgsave;
  pc->grestore = psgrestore;
  pc->clip = psclip;
  pc->setcolor = pssetcolor;
  pc->setdash = pssetdash;
  pc->setlinewidth = pssetlinewidth;
  pc->setlinecaps = pssetlinecaps;
  pc->drawline = psdrawline;
  pc->drawlines = psdrawlines;
  pc->drawpolygon = psdrawpolygon;
  pc->drawcircle = psdrawcircle;
  pc->drawellipse = psdrawellipse;
  pc->setfont = pssetfont;
  pc->drawstring = psdrawstring;

  gtk_plot_canvas_print(canvas, pc);

  g_free(pc->pcname);
  g_free(pc);
}


void 
gtk_plot_canvas_export_ps_with_size		(GtkPlotCanvas *canvas, 
					 	 char *pcname, 
						 gint orient, 
						 gint epsflag, 
						 gint units,
						 gint width, 
						 gint height)
{
  GtkPlotPC *pc;

  pc = gtk_plot_pc_new_with_size(pcname, 
                                 orient, 
                                 units, width, height);

  pc->init = psinit;
  pc->leave = psleave;
  pc->gsave = psgsave;
  pc->grestore = psgrestore;
  pc->clip = psclip;
  pc->setcolor = pssetcolor;
  pc->setdash = pssetdash;
  pc->setlinewidth = pssetlinewidth;
  pc->setlinecaps = pssetlinecaps;
  pc->drawline = psdrawline;
  pc->drawlines = psdrawlines;
  pc->drawpolygon = psdrawpolygon;
  pc->drawcircle = psdrawcircle;
  pc->drawellipse = psdrawellipse;
  pc->setfont = pssetfont;
  pc->drawstring = psdrawstring;
 
  gtk_plot_canvas_print(canvas, pc);

  g_free(pc->pcname);
  g_free(pc);
}

static void
psdrawlines(GtkPlotPC *pc, GdkPoint *points, gint numpoints)
{
  gint i;
  FILE *psout = pc->pcfile;
  
  fprintf(psout,"n\n");
  fprintf(psout,"%d %d m\n", points[0].x, points[0].y);
  for(i = 1; i < numpoints; i++)
        fprintf(psout,"%d %d l\n", points[i].x, points[i].y);

  fprintf(psout,"s\n");
}

static void
psdrawpolygon(GtkPlotPC *pc, GdkPoint *points, gint numpoints, gint filled)
{
  gint i;
  FILE *psout = pc->pcfile;

  fprintf(psout,"n\n");
  fprintf(psout,"%d %d m\n", points[0].x, points[0].y);
  for(i = 1; i < numpoints; i++)
      fprintf(psout,"%d %d l\n", points[i].x, points[i].y);

  if(filled)
     fprintf(psout,"f\n");
  else
     fprintf(psout,"cp\n");

  fprintf(psout,"s\n");
}

static void psdrawline(GtkPlotPC *pc, gint x0, gint y0, gint xf, gint yf)
{
  FILE *psout = pc->pcfile;

  fprintf(psout, "%d %d m\n", x0, y0);
  fprintf(psout, "%d %d l\n", xf, yf);
  fprintf(psout, "s\n");
}

static void
psdrawcircle(GtkPlotPC *pc, gint x, gint y, gint size, gint filled)
{
  FILE *psout = pc->pcfile;

  fprintf(psout,"n %f %f %f %f 0 360 ellipse\n", 
          (gdouble)x, (gdouble)y, (gdouble)size, (gdouble)size);

  if(filled)
     fprintf(psout,"f\n");
  else
     fprintf(psout,"cp\n");

  fprintf(psout,"s\n");
}

static void
psdrawellipse(GtkPlotPC *pc, gint x, gint y, gint width, gint height, 
              gint filled)
{
  FILE *psout = pc->pcfile;

  fprintf(psout,"n %f %f %f %f 0 360 ellipse\n", 
          (gdouble)x+width/2, (gdouble)y+height/2, 
          (gdouble)width/2, (gdouble)height/2);

  if(filled)
     fprintf(psout,"f\n");
  else
     fprintf(psout,"cp\n");

  fprintf(psout,"s\n");
}

static void
psdrawstring(GtkPlotPC *pc,
             gint x, gint y,
             GtkJustification justification, 
             gint angle,
             gchar *font,
             gint height,
             gchar *text)
{
  gchar *curstr;
  gchar *currfont;
  gchar *aux;
  GtkPSFont *psfont;
  GtkPlotText ptext;
  gchar bkspchar;
  gchar *lastchar = NULL;
  gint curcnt = 0, offset = 0;
  gint numf;
  gdouble scale;
  gboolean italic, bold;
  GList *family;
  FILE *psout = pc->pcfile;
  gint twidth, theight, tdescent, tascent;
  
  if (text == NULL || strlen(text) == 0) return;

  curstr = (gchar *)g_malloc(2*strlen(text)*sizeof(gchar));
 
  gtk_psfont_get_families(&family, &numf);
  psfont = gtk_psfont_get_font(font);
  italic = psfont->italic;
  bold = psfont->bold;

  currfont = psfont->psname;

  ptext.font = psfont->psname;
  ptext.height = height;
  ptext.text = text;
  ptext.angle = angle;
  gtk_plot_text_get_size(&ptext, 1., &twidth, &theight, &tascent, &tdescent);

  if(angle == 90 || angle == 270) angle = 360 - angle;

  fprintf(psout, "gs\n");
  fprintf(psout, "%d %d translate\n", x, y);
  fprintf(psout, "%d rotate\n", angle);

  fprintf(psout, "0 0 m\n");
  fprintf(psout, "1 -1 sc\n");

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

  aux = text;
  scale = height;

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
                  psfont = gtk_psfont_find_by_family((gchar *)g_list_nth_data(family, atoi(aux)), italic, bold);
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
       if(aux && *aux != '\0' && *aux !='\n'){
                curstr[curcnt++] = *aux;
		lastchar = aux;
                aux++;
       }
     }
  }
  curstr[curcnt] = 0;

  fprintf(psout, "(%s) show\n", curstr);

  fprintf(psout, "gr\n");  
  fprintf(psout, "n\n");  

  g_free(curstr);
}

static void
pssetfont(GtkPlotPC *pc, gchar *font, gint height)
{
  FILE *psout = pc->pcfile;

  fprintf(psout, "/%s ff %f scf sf\n", font, (double)height);
}


static void
psgsave(GtkPlotPC *pc)
{
  fprintf(pc->pcfile,"gsave\n");
}

static void
psgrestore(GtkPlotPC *pc)
{
  fprintf(pc->pcfile,"grestore\n");
}

static void
psclip(GtkPlotPC *pc, GdkRectangle clip)
{
  fprintf(pc->pcfile,"%d %d %d %d rectclip\n", clip.x, clip.y, clip.width, clip.height);
}

static void
pssetlinecaps(GtkPlotPC *pc, gint caps)
{
  fprintf(pc->pcfile,"%d slc\n", caps);
}


