
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <libart_lgpl/art_misc.h>
#include <libart_lgpl/art_pixbuf.h>
#include <libart_lgpl/art_filterlevel.h>

/********************************************
 **  Gdk_Pixbuf
 ********************************************/

ArtPixBuf *
ada_gdk_pixbuf_get_art_pixbuf (GdkPixbuf* pix) {
  return pix->art_pixbuf;
}


