/* gtkcolorcombo - color_combo widget for gtk+
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
#include <stdio.h>
#include <math.h>
#include <gtk/gtkarrow.h>
#include <gtk/gtkhbox.h>
#include <gtk/gtktable.h>
#include <gtk/gtktogglebutton.h>
#include <gtk/gtkpixmap.h>
#include <gtk/gtkeventbox.h>
#include <gtk/gtkbutton.h>
#include <gtk/gtkmain.h>
#include <gtk/gtksignal.h>
#include <gtk/gtkwindow.h>
#include <gtk/gtkframe.h>
#include <gdk/gdkkeysyms.h>
#include "gtkcombobox.h"
#include "gtkcolorcombo.h"


/* SIGNALS */
enum {
   CHANGED,
   LAST_SIGNAL
};

static gint color_combo_signals[LAST_SIGNAL] = {0};


static char *default_colors[]={
"black",
"brown",
"olive drab",
"dark green",
"dark sea green",
"dark blue",
"dark cyan",
"grey80",
"dark red",
"coral",
"yellow green",
"sea green",
"aquamarine3",
"blue",
"steel blue",
"grey50",
"red",
"orange",
"lime green",
"green",
"aquamarine1",
"light blue",
"violet",
"grey40",
"pale violet red",
"gold1",
"yellow1",
"lawn green",
"turquoise1",
"SkyBlue1",
"purple2",
"grey25",
"light pink",
"light goldenrod",
"light yellow",
"light green",
"pale turquoise",
"light steel blue",
"lavender",
"white"
};

static char *xpm_color[]={
"18 18 3 1",
"      c None",
".     c #000000000000",
"X     c #111111111111",
"                  ",
" ................ ",
" .XXXXXXXXXXXXXX. ",
" .XXXXXXXXXXXXXX. ",
" .XXXXXXXXXXXXXX. ",
" .XXXXXXXXXXXXXX. ",
" .XXXXXXXXXXXXXX. ",
" .XXXXXXXXXXXXXX. ",
" .XXXXXXXXXXXXXX. ",
" .XXXXXXXXXXXXXX. ",
" .XXXXXXXXXXXXXX. ",
" .XXXXXXXXXXXXXX. ",
" .XXXXXXXXXXXXXX. ",
" .XXXXXXXXXXXXXX. ",
" .XXXXXXXXXXXXXX. ",
" .XXXXXXXXXXXXXX. ",
" ................ ",
"                  "};

static void         gtk_color_combo_class_init      (GtkColorComboClass *klass);
static void         gtk_color_combo_init            (GtkColorCombo      *color_combo);
static void         gtk_color_combo_destroy         (GtkObject     *color_combo);
static void         gtk_color_combo_realize         (GtkWidget *widget);
static void         color_to_hex(gint color, gchar string[5]);

static GtkComboBoxClass *parent_class = NULL;

static void
gtk_color_combo_class_init (GtkColorComboClass * klass)
{
  GtkObjectClass *object_class;
  GtkWidgetClass *widget_class;

  parent_class = gtk_type_class (gtk_hbox_get_type ());
  object_class = (GtkObjectClass *) klass;
  widget_class = (GtkWidgetClass *) klass;

  object_class->destroy = gtk_color_combo_destroy;

  widget_class->realize = gtk_color_combo_realize;
  
  color_combo_signals[CHANGED]=gtk_signal_new("changed",
                                      GTK_RUN_FIRST,
                                      object_class->type,
                                      GTK_SIGNAL_OFFSET(GtkColorComboClass,
                                      changed),
                                      gtk_marshal_NONE__INT_POINTER,
                                      GTK_TYPE_NONE,  
                                      2, GTK_TYPE_INT, GTK_TYPE_STRING);

  gtk_object_class_add_signals (object_class, color_combo_signals,LAST_SIGNAL);
  klass->changed = NULL;
}

static void
gtk_color_combo_destroy (GtkObject * color_combo)
{
  gint i,j;

  GtkColorCombo *combo;
  combo=GTK_COLOR_COMBO(color_combo);

  if(combo && combo->button) /* patched by Mario Motta <mmotta@guest.net> */
   for(i=0; i<combo->nrows; i++)
    for(j=0; j<combo->ncols; j++)
      if(combo->button[i][j])
        gtk_widget_destroy(combo->button[i][j]);
  
  if(combo->color_name) g_free(combo->color_name);
 
  if(GTK_COLOR_COMBO(color_combo)->table)
    gtk_widget_destroy (GTK_COLOR_COMBO(color_combo)->table);

  if (GTK_OBJECT_CLASS (parent_class)->destroy)
    (*GTK_OBJECT_CLASS (parent_class)->destroy) (color_combo);
}



static void
gtk_color_combo_update (GtkWidget * widget, GtkColorCombo * color_combo)
{
  gint i,j;
  gint focus_row = -1, focus_col = -1;
  gint new_row = -1, new_col = -1;
  gint new_selection=FALSE;
  gint row,column;

  row=color_combo->row;
  column=color_combo->column;

  for(i=0 ; i<color_combo->nrows; i++)
    for(j=0; j<color_combo->ncols; j++){    
      if(GTK_WIDGET_HAS_FOCUS(color_combo->button[i][j])){
            focus_row=i;
            focus_col=j;
      }
      if(color_combo->button[i][j]->state==GTK_STATE_ACTIVE){
        if(i != row || j != column){
            new_selection=TRUE;
            new_row=i;
            new_col=j;
        }
      }
    }

  if(!new_selection && focus_row >= 0 && focus_col >= 0){
     if(focus_row != row && focus_col != column){
       new_selection = TRUE;
       new_row=focus_row;
       new_col=focus_col;
     }
  }

  if(new_selection){
      if(row >= 0 && column >= 0){
          GTK_BUTTON(color_combo->button[row][column])->button_down=FALSE; 
          GTK_TOGGLE_BUTTON(color_combo->button[row][column])->active=FALSE;
          gtk_widget_set_state(color_combo->button[row][column], GTK_STATE_NORMAL);
          gtk_widget_queue_draw(color_combo->button[row][column]);
      }
      color_combo->row=new_row;
      color_combo->column=new_col;
      
      gtk_signal_emit (GTK_OBJECT(color_combo), color_combo_signals[CHANGED],
                  new_row * color_combo->ncols + new_col,
                  color_combo->color_name[new_row*color_combo->ncols+new_col]);

  }

  if(!new_selection && row >= 0 && column >= 0){
          GTK_BUTTON(color_combo->button[row][column])->button_down=TRUE;     
          GTK_TOGGLE_BUTTON(color_combo->button[row][column])->active=TRUE;
          gtk_widget_set_state(color_combo->button[row][column], GTK_STATE_ACTIVE);
          gtk_widget_queue_draw(color_combo->button[row][column]);

          gtk_signal_emit (GTK_OBJECT(color_combo), 
                           color_combo_signals[CHANGED],
                           row * color_combo->ncols + column,
                        color_combo->color_name[row*color_combo->ncols+column]);

  }



  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(GTK_COMBO_BOX(color_combo)->arrow), FALSE);

  gtk_grab_remove(GTK_COMBO_BOX(color_combo)->popwin);
  gdk_pointer_ungrab(GDK_CURRENT_TIME);
  gtk_widget_hide(GTK_COMBO_BOX(color_combo)->popwin);
  return;
}

static void
gtk_color_combo_init (GtkColorCombo * color_combo)
{
  GtkWidget *widget;

  widget=GTK_WIDGET(color_combo);

  color_combo->row = -1;
  color_combo->column = -1;

}

static void
gtk_color_combo_realize(GtkWidget *widget)
{
  GtkComboBox *combo;
  GtkColorCombo *color_combo;
  GdkPixmap *color_pixmap;
  GtkWidget *pixmap;
  gchar color_string[21];
  gint i,j,n;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (GTK_IS_COLOR_COMBO (widget));

  GTK_WIDGET_CLASS (parent_class)->realize (widget);

  combo = GTK_COMBO_BOX(widget);
  color_combo = GTK_COLOR_COMBO(widget);

  color_combo->table = gtk_table_new (color_combo->nrows, color_combo->ncols, TRUE);

  color_combo->button = (GtkWidget ***)g_malloc(color_combo->nrows*sizeof(GtkWidget **));

  for(i = 0; i < color_combo->nrows; i++){

    color_combo->button[i] = (GtkWidget **)g_malloc(color_combo->ncols*sizeof(GtkWidget *));

    for(j = 0; j < color_combo->ncols; j++){

        color_combo->button[i][j] = gtk_toggle_button_new();
        gtk_button_set_relief(GTK_BUTTON(color_combo->button[i][j]),
                              GTK_RELIEF_NONE);
        gtk_table_attach (GTK_TABLE(color_combo->table), 
                          color_combo->button[i][j],
                          j, j+1, i, i+1, GTK_SHRINK, GTK_SHRINK, 0, 0);

        gtk_widget_set_usize(color_combo->button[i][j], 24, 24);
        gtk_widget_show(color_combo->button[i][j]); 
        gtk_signal_connect (GTK_OBJECT (color_combo->button[i][j]), "toggled",
		            (GtkSignalFunc) gtk_color_combo_update, 
                            color_combo);

    }
  }

/*  color_combo->custom_button = gtk_button_new_with_label ("Customize");
  gtk_table_attach (GTK_TABLE(color_combo->table),
                    color_combo->custom_button,
                    0, color_combo->ncols,
                    color_combo->nrows, color_combo->nrows+1,
                    GTK_FILL, GTK_FILL, 0, 0);
  gtk_widget_show(color_combo->custom_button); 

  gtk_signal_connect (GTK_OBJECT(color_combo->custom_button),"clicked",
                      (GtkSignalFunc) gtk_color_combo_customize, color_combo);
*/

  gtk_container_add(GTK_CONTAINER(GTK_COMBO_BOX(color_combo)->frame), 
                    color_combo->table);
  gtk_widget_show(color_combo->table);

  n=0;

  for(i=0; i<color_combo->nrows; i++)
   for(j=0; j<color_combo->ncols; j++){

       sprintf(color_string,"X     c %s",color_combo->color_name[n++]);

       xpm_color[3]=color_string;
       
       color_pixmap=gdk_pixmap_create_from_xpm_d(
                             widget->window,
                             NULL,
                             &(widget->style->bg[GTK_STATE_NORMAL]),
                             xpm_color);    
       pixmap=gtk_pixmap_new(color_pixmap, NULL);
       gtk_container_add(GTK_CONTAINER(color_combo->button[i][j]), pixmap);
       gtk_widget_show(pixmap);
       gdk_pixmap_unref(color_pixmap);
    }

  gtk_signal_connect (GTK_OBJECT (combo->button), "clicked",
	              (GtkSignalFunc) gtk_color_combo_update, 
                      color_combo);
}

static void
color_to_hex(gint color, gchar string[5])
{
  gint i,n;

  for(i=3; i>=0; i--){
     n=color/pow(16,i);
     color-=n*pow(16,i);
     if(n < 10)
       string[3-i]='0'+n;
     else
       string[3-i]='A'+n-10;
  }
  string[4]='\0';
}     


guint
gtk_color_combo_get_type ()
{
  static guint color_combo_type = 0;

  if (!color_combo_type)
    {
      GtkTypeInfo color_combo_info =
      {
	"GtkColorCombo",
	sizeof (GtkColorCombo),
	sizeof (GtkColorComboClass),
	(GtkClassInitFunc) gtk_color_combo_class_init,
	(GtkObjectInitFunc) gtk_color_combo_init,
	NULL,
	NULL,
	(GtkClassInitFunc) NULL,
      };
      color_combo_type = gtk_type_unique (gtk_combobox_get_type (), &color_combo_info);
    }
  return color_combo_type;
}

GtkWidget *
gtk_color_combo_new ()
{
  GtkColorCombo *color_combo;
  GdkColor color;
  gchar color_string[21];
  gint i,j,n;
  gchar red[5], green[5], blue[5];


  color_combo = gtk_type_new (gtk_color_combo_get_type ());

  color_combo->default_flag=TRUE;

  color_combo->nrows = 5;
  color_combo->ncols = 8;
  n = color_combo->nrows * color_combo->ncols;
  color_combo->color_name = (gchar **)g_malloc(n*sizeof(gchar *));

  n=0;
  
  for(i=0; i<color_combo->nrows; i++)
   for(j=0; j<color_combo->ncols; j++){

       gdk_color_parse(default_colors[n], &color);
       n++;

       color_to_hex(color.red, red);
       color_to_hex(color.green, green);
       color_to_hex(color.blue, blue);

       sprintf(color_string,"#%s%s%s",red,green,blue);    
       color_combo->color_name[n-1]=g_strdup(color_string);
   }

  return(GTK_WIDGET(color_combo));

}

GtkWidget *
gtk_color_combo_new_with_values (gint nrows, gint ncols, gchar **color_names)
{
  GtkColorCombo *color_combo;
  GtkWidget *widget;
  GdkColor color;
  gchar color_string[21];
  gint i,j,n;
  gchar red[5], green[5], blue[5];

  widget = gtk_color_combo_new();

  color_combo = GTK_COLOR_COMBO(widget);

  color_combo->default_flag=FALSE;

  color_combo->nrows=nrows;
  color_combo->ncols=ncols;
  n = color_combo->nrows * color_combo->ncols;
  color_combo->color_name = (gchar **)g_malloc(n*sizeof(gchar *));

  n=0;
  
  for(i=0; i<color_combo->nrows; i++)
   for(j=0; j<color_combo->ncols; j++){

       gdk_color_parse(color_names[n++], &color);

       color_to_hex(color.red, red);
       color_to_hex(color.green, green);
       color_to_hex(color.blue, blue);

       sprintf(color_string,"#%s%s%s",red,green,blue);    
       color_combo->color_name[n-1]=g_strdup(color_string);
   }

  return(widget);
}


gchar *
gtk_color_combo_get_color_at(GtkColorCombo *color_combo, gint row, gint col)
{
   gchar *name;

   name = color_combo->color_name[row*color_combo->ncols+col];

   return name;
}

void
gtk_color_combo_find_color(GtkColorCombo *color_combo,
                           GdkColor *color, gint *row, gint *col)
{
   GdkColor combo_color;
   gchar *name;
   gint i, j;

   *row = -1;
   *col = -1;

   for(i = 0; i < color_combo->nrows; i++){ 
     for(j = 0; j < color_combo->ncols; j++){ 
        name = gtk_color_combo_get_color_at(color_combo, i, j);
        gdk_color_parse(name, &combo_color);
        if(gdk_color_equal(color, &combo_color)){
                   *row = i;
                   *col = j;
        }
     }
   }
} 
