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

#ifndef __GTK_PLOT_PS_H__
#define __GTK_PLOT_PS_H__


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

void            gtk_plot_export_ps              (GtkPlot *plot,
                                                 gchar *psfile,
                                                 gint orientation,
                                                 gint epsflag,
                                                 gint page_size);
void            gtk_plot_export_ps_with_size    (GtkPlot *plot,
                                                 gchar *psfile,
                                                 gint orientation,
                                                 gint epsflag,
                                                 gint units,
                                                 gint width, gint height);
void            gtk_plot_layout_export_ps       (GtkPlotLayout *layout,
                                                 char *file_name,
                                                 gint orientation,
                                                 gint epsflag,
                                                 gint page_size);
void            gtk_plot_layout_export_ps_with_size     (GtkPlotLayout *layout,
                                                 char *file_name,
                                                 gint orientation,
                                                 gint epsflag,
                                                 gint units,
                                                 gint width, gint height);


#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* __GTK_PLOT_PS_H__ */

