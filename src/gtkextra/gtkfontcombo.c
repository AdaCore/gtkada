/* gtkfontcombo - font_combo widget for gtk+
 * Copyright 1999-2001 Adrian E. Feiguin <feiguin@ifir.edu.ar>
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

#include <string.h>
#include <stdlib.h>
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include "gtkfontcombo.h"

/* Signals */
enum {
      CHANGED,
      LAST_SIGNAL 
};

/* XPM */
static char * bold_xpm[] = {
"16 16 2 1",
" 	c None",
".	c #000000000000",
"                ",
"  .........     ",
"   ...   ...    ",
"   ...    ...   ",
"   ...    ...   ",
"   ...    ...   ",
"   ...   ...    ",
"   ........     ",
"   ...    ...   ",
"   ...     ...  ",
"   ...     ...  ",
"   ...     ...  ",
"   ...     ...  ",
"   ...    ...   ",
"  .........     ",
"                "};


/* XPM */
static char * italic_xpm[] = {
"16 16 2 1",
" 	c None",
".	c #000000000000",
"                ",
"        .....   ",
"         ...    ",
"         ...    ",
"        ...     ",
"        ...     ",
"       ...      ",
"       ...      ",
"      ...       ",
"      ...       ",
"     ...        ",
"     ...        ",
"    ...         ",
"    ...         ",
"   .....        ",
"                "};

#define NUM_SIZES 20

static gchar *default_sizes[] = {"8","9","10","12","13","14","16","18",
                                 "20","22","24","26","28","32","36","40",
                                 "48","56","64","72"};

static void         gtk_font_combo_class_init      (GtkFontComboClass *klass);
static void         gtk_font_combo_init            (GtkFontCombo      *font_combo);
static void         gtk_font_combo_destroy         (GtkObject     *font_combo);
static void         new_font			   (GtkEntry *entry, 
                                                    gpointer data);

static GtkToolbarClass *parent_class = NULL;
static guint font_combo_signals[LAST_SIGNAL] = {0};

static void
gtk_font_combo_class_init (GtkFontComboClass * klass)
{
  GtkObjectClass *object_class;
  GtkWidgetClass *widget_class;

  parent_class = gtk_type_class (gtk_toolbar_get_type ());
  object_class = (GtkObjectClass *) klass;
  widget_class = (GtkWidgetClass *) klass;

  object_class->destroy = gtk_font_combo_destroy;
  
  font_combo_signals[CHANGED] =
    gtk_signal_new ("changed",
                    GTK_RUN_LAST,
                    object_class->type,
                    GTK_SIGNAL_OFFSET (GtkFontComboClass, changed),
                    gtk_marshal_NONE__NONE,
                    GTK_TYPE_NONE, 0);

  gtk_object_class_add_signals (object_class, font_combo_signals, LAST_SIGNAL);

}

static void
gtk_font_combo_destroy (GtkObject * font_combo)
{

  if (GTK_OBJECT_CLASS (parent_class)->destroy)
    (*GTK_OBJECT_CLASS (parent_class)->destroy) (font_combo);

}


static void
gtk_font_combo_init (GtkFontCombo * font_combo)
{
  GtkWidget *widget;
  GtkToolbar *toolbar;
  GdkColormap *colormap;
  GdkPixmap *pixmap;
  GtkWidget *tpixmap;
  GdkBitmap *mask;
  GtkRequisition req;
  GList *family = NULL;
  GList *size = NULL;
  gint numf, i;

  widget=GTK_WIDGET(font_combo);

  toolbar = GTK_TOOLBAR(font_combo);

  colormap = gdk_colormap_get_system();

  font_combo->name_combo = gtk_combo_new ();
  gtk_entry_set_editable(GTK_ENTRY(GTK_COMBO(font_combo->name_combo)->entry), FALSE);
  font_combo->size_combo = gtk_combo_new ();
  gtk_entry_set_editable(GTK_ENTRY(GTK_COMBO(font_combo->size_combo)->entry), FALSE);
  font_combo->bold_button = gtk_toggle_button_new ();
  gtk_widget_set_usize(font_combo->bold_button, 24, 24);
  font_combo->italic_button = gtk_toggle_button_new ();
  gtk_widget_set_usize(font_combo->italic_button, 24, 24);

  pixmap = gdk_pixmap_colormap_create_from_xpm_d(NULL, colormap, &mask, NULL,
                                                 bold_xpm);
  tpixmap = gtk_pixmap_new(pixmap, mask);
  gtk_container_add(GTK_CONTAINER(font_combo->bold_button), tpixmap);
  gtk_widget_show(tpixmap);

  pixmap = gdk_pixmap_colormap_create_from_xpm_d(NULL, colormap, &mask, NULL,
                                                 italic_xpm);
  tpixmap = gtk_pixmap_new(pixmap, mask);
  gtk_container_add(GTK_CONTAINER(font_combo->italic_button), tpixmap);
  gtk_widget_show(tpixmap);

  gtk_toolbar_append_widget(toolbar, font_combo->name_combo, NULL, NULL);

  gtk_widget_size_request(font_combo->size_combo, &req);
  req.width = 56;
  gtk_widget_set_usize(font_combo->size_combo, req.width, req.height);
  gtk_toolbar_append_widget(toolbar, font_combo->size_combo, NULL, NULL);

  gtk_toolbar_set_space_size(toolbar, 20);
  gtk_toolbar_append_space(toolbar);
  gtk_toolbar_append_widget(toolbar, font_combo->bold_button, "Bold", "Bold");
  gtk_toolbar_append_widget(toolbar, font_combo->italic_button, "Italic", "Italic");

  gtk_widget_show (font_combo->name_combo);
  gtk_widget_show (font_combo->size_combo);
  gtk_widget_show (font_combo->bold_button);
  gtk_widget_show (font_combo->italic_button);

  gtk_psfont_get_families(&family, &numf);
  gtk_combo_set_popdown_strings(GTK_COMBO(font_combo->name_combo), family);

  for(i = 0; i < NUM_SIZES; i++)
     size = g_list_append(size, default_sizes[i]);
  gtk_combo_set_popdown_strings(GTK_COMBO(font_combo->size_combo), size);

  gtk_signal_connect(GTK_OBJECT(GTK_COMBO(GTK_FONT_COMBO(font_combo)->name_combo)->entry),
                     "changed",
                     GTK_SIGNAL_FUNC(new_font), font_combo);

  gtk_signal_connect(GTK_OBJECT(GTK_COMBO(GTK_FONT_COMBO(font_combo)->size_combo)->entry),
                     "changed",
                     GTK_SIGNAL_FUNC(new_font), font_combo);

  gtk_signal_connect(GTK_OBJECT(GTK_FONT_COMBO(font_combo)->italic_button),
                     "clicked",
                     GTK_SIGNAL_FUNC(new_font), font_combo);

  gtk_signal_connect(GTK_OBJECT(GTK_FONT_COMBO(font_combo)->bold_button),
                     "clicked",
                     GTK_SIGNAL_FUNC(new_font), font_combo);

  font_combo->psfont = gtk_psfont_find_by_family((gchar *)family->data, 0, 0);
  font_combo->font = gtk_psfont_get_gdkfont(font_combo->psfont->fontname, 12);
  font_combo->height = 12;
  font_combo->italic = FALSE;
  font_combo->bold = FALSE;
}

guint
gtk_font_combo_get_type ()
{
  static guint font_combo_type = 0;

  if (!font_combo_type)
    {
      GtkTypeInfo font_combo_info =
      {
	"GtkFontCombo",
	sizeof (GtkFontCombo),
	sizeof (GtkFontComboClass),
	(GtkClassInitFunc) gtk_font_combo_class_init,
	(GtkObjectInitFunc) gtk_font_combo_init,
	NULL,
	NULL,
	(GtkClassInitFunc) NULL,
      };
      font_combo_type = gtk_type_unique (gtk_toolbar_get_type (), &font_combo_info);
    }
  return font_combo_type;
}

GtkWidget *
gtk_font_combo_new ()
{
  GtkFontCombo *font_combo;

  font_combo = gtk_type_new (gtk_font_combo_get_type ());

  return(GTK_WIDGET(font_combo));
}

static void
new_font(GtkEntry *entry, gpointer data)
{
  GtkFontCombo *font_combo;
  GtkPSFont *psfont;
  gchar *text;
  gchar *size;
  gboolean italic;
  gboolean bold;
  gint height;

  font_combo = GTK_FONT_COMBO(data);

  text=gtk_entry_get_text(GTK_ENTRY(GTK_COMBO(font_combo->name_combo)->entry));

  size=gtk_entry_get_text(GTK_ENTRY(GTK_COMBO(font_combo->size_combo)->entry));

  italic = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(GTK_FONT_COMBO(font_combo)->italic_button));
  bold = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(GTK_FONT_COMBO(font_combo)->bold_button));

  height = atoi(size);
  font_combo->psfont = psfont = gtk_psfont_find_by_family(text, italic, bold);
  font_combo->font = gtk_psfont_get_gdkfont(psfont->fontname, height);
  font_combo->height = height;
  font_combo->italic = italic;
  font_combo->bold = bold;

  gtk_signal_emit(GTK_OBJECT(font_combo), font_combo_signals[CHANGED]);
}

void
gtk_font_combo_select (GtkFontCombo *combo, 
		       gchar *family,
                       gboolean bold,
		       gboolean italic,
		       gint height)
{
  GtkItem *item;
  GList *children;
  gchar *text;
  gint n = 0;

  children = GTK_LIST(GTK_COMBO(combo->name_combo)->list)->children;

  while(children){
    item = GTK_ITEM(children->data);
    text = GTK_LABEL(GTK_BIN(item)->child)->label;
    if(strcmp(text, family) == 0) break;
    n++;
    children = children->next;
  }

  gtk_font_combo_select_nth(combo, n, bold, italic, height);
}

void
gtk_font_combo_select_nth (GtkFontCombo *combo, 
		           gint n,
                           gboolean bold,
		           gboolean italic,
		           gint height)
{
  gint i;

  gtk_list_select_item(GTK_LIST(GTK_COMBO(combo->name_combo)->list), n);

  for(i = 0; i < NUM_SIZES; i++){
     if(atoi(default_sizes[i]) >= height) break;
  }

  if(i < NUM_SIZES)
    gtk_list_select_item(GTK_LIST(GTK_COMBO(combo->size_combo)->list), i);

  gtk_toggle_button_set_state(GTK_TOGGLE_BUTTON(combo->bold_button), bold);
  gtk_toggle_button_set_state(GTK_TOGGLE_BUTTON(combo->italic_button), italic);
}
