/* gtkplotpc - gtkplot printing functions
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

#ifndef __GTK_PLOT_PRINT_H__
#define __GTK_PLOT_PRINT_H__


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


void            gtk_plot_print                      	(GtkPlot *plot,
                                                         GtkPlotPC *pc);

void            gtk_plot_canvas_print       		(GtkPlotCanvas *canvas,
                                                 	 GtkPlotPC *pc);

#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* __GTK_PLOT_PRINT_H__ */

