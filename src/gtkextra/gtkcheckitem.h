/* gtkcheckitem - widget for gtk+
 * Copyright (C) 1999-2001 Adrian E. Feiguin
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
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

/*
 * Modified by the GTK+ Team and others 1997-1999.  See the AUTHORS
 * file for a list of people on the GTK+ Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GTK+ at ftp://ftp.gtk.org/pub/gtk/. 
 */

#ifndef __GTK_CHECK_ITEM_H__
#define __GTK_CHECK_ITEM_H__


#include <gdk/gdk.h>
#include <gtk/gtk.h>


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


#define GTK_TYPE_CHECK_ITEM                  (gtk_check_item_get_type ())
#define GTK_CHECK_ITEM(obj)                  (GTK_CHECK_CAST ((obj), GTK_TYPE_CHECK_ITEM, GtkCheckItem))
#define GTK_CHECK_ITEM_CLASS(klass)          (GTK_CHECK_CLASS_CAST ((klass), GTK_TYPE_CHECK_ITEM, GtkCheckItemClass))
#define GTK_IS_CHECK_ITEM(obj)               (GTK_CHECK_TYPE ((obj), GTK_TYPE_CHECK_ITEM))
#define GTK_IS_CHECK_ITEM_CLASS(klass)       (GTK_CHECK_CLASS_TYPE ((klass), GTK_TYPE_CHECK_ITEM))


typedef struct _GtkCheckItem       GtkCheckItem;
typedef struct _GtkCheckItemClass  GtkCheckItemClass;

struct _GtkCheckItem
{
  GtkToggleButton toggle_button;
};

struct _GtkCheckItemClass
{
  GtkToggleButtonClass parent_class;

  guint16 indicator_size;
  guint16 indicator_spacing;

  void (* draw_indicator) (GtkCheckItem *check_item,
			   GdkRectangle   *area);
};


GtkType    gtk_check_item_get_type       (void);
GtkWidget* gtk_check_item_new            (void);
GtkWidget* gtk_check_item_new_with_label (const gchar *label);


#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* __GTK_CHECK_ITEM_H__ */
