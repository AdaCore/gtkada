/* 
 * Copyright (C) 1998 Janne Löf <jlof@mail.student.oulu.fi>
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
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */


#include "gdkgl.h"
#include <gdk/gdkx.h>
#include <GL/gl.h>
#include <GL/glx.h>
#include <string.h>


static XVisualInfo *get_xvisualinfo(GdkVisual *visual)
{
  Display *dpy;
  XVisualInfo vinfo_template;
  XVisualInfo *vi;
  int nitems_return;

  dpy = GDK_DISPLAY();

  /* TODO: is this right way to get VisualInfo from Visual ?? */
  /* AFAIK VisualID and depth should be enough to uniquely identify visual */
  vinfo_template.visual   = GDK_VISUAL_XVISUAL(visual);
  vinfo_template.visualid = XVisualIDFromVisual(vinfo_template.visual);
  vinfo_template.depth    = visual->depth;
  vi = XGetVisualInfo(dpy, VisualIDMask|VisualDepthMask, &vinfo_template, &nitems_return);

  g_assert( vi!=0  && nitems_return==1 ); /* visualinfo needs to be unique */

  /* remember to XFree returned XVisualInfo !!! */
  return vi;
}


struct _GdkGLContextPrivate {
  Display    *xdisplay;
  GLXContext glxcontext;
  guint ref_count;
};

typedef struct _GdkGLContextPrivate GdkGLContextPrivate;


gint gdk_gl_query(void)
{
  return (glXQueryExtension(GDK_DISPLAY(),NULL,NULL) == True) ? TRUE : FALSE;
}

GdkVisual *gdk_gl_choose_visual(int *attrList)
{
  Display *dpy;
  XVisualInfo *vi;
  GdkVisual  *visual;

  g_return_val_if_fail(attrList != NULL, NULL);

  dpy = GDK_DISPLAY();
  /* TODO:  translate GDK_GL_ to GLX_ */
  if ((vi = glXChooseVisual(dpy,DefaultScreen(dpy), attrList)) == NULL) {
    return NULL;
  }
  visual = gdkx_visual_get(vi->visualid);
  XFree(vi);
  return visual;
}


int gdk_gl_get_config(GdkVisual *visual, int attrib)
{
  Display *dpy;
  XVisualInfo *vi;
  int value;
  
  g_return_val_if_fail(visual != NULL, -1);

  dpy = GDK_DISPLAY();
 
  vi = get_xvisualinfo(visual);

  /* TODO:  translate GDK_GL_ to GLX_ */
  if (glXGetConfig(dpy, vi, attrib, &value) == 0) {
    XFree(vi);
    return value;
  }
  XFree(vi);
  return -1;
}


GdkGLContext *gdk_gl_context_new(GdkVisual *visual)
{
  return gdk_gl_context_share_new(visual, NULL, FALSE);
}

GdkGLContext *gdk_gl_context_share_new(GdkVisual *visual, GdkGLContext *sharelist, gint direct)
{
  Display *dpy;
  XVisualInfo *vi;
  GLXContext glxcontext;
  GdkGLContextPrivate *private;

  g_return_val_if_fail(visual != NULL, NULL);

  dpy = GDK_DISPLAY();

  vi = get_xvisualinfo(visual);

  if (sharelist) {
    glxcontext = glXCreateContext(dpy, vi, ((GdkGLContextPrivate*)sharelist)->glxcontext, direct ? True : False);
  } else {
    glxcontext = glXCreateContext(dpy, vi, 0, direct ? True : False);
  }
  XFree(vi);
  if (glxcontext == NULL) {
    return NULL;
  }

  private = g_new(GdkGLContextPrivate, 1);
  private->xdisplay = dpy;
  private->glxcontext = glxcontext;
  private->ref_count = 1;

  return (GdkGLContext*)private;
}

GdkGLContext *gdk_gl_context_ref(GdkGLContext *context)
{
  GdkGLContextPrivate *private = (GdkGLContextPrivate*)context;

  g_return_val_if_fail(context != NULL, NULL);
  private->ref_count += 1;

  return context;
}

void gdk_gl_context_unref(GdkGLContext *context)
{
  GdkGLContextPrivate *private = (GdkGLContextPrivate*)context;

  g_return_if_fail(context != NULL);

  if (private->ref_count > 1) {
    private->ref_count -= 1;
  } else {
    if (private->glxcontext == glXGetCurrentContext())
      glXMakeCurrent(private->xdisplay, None, NULL);
    glXDestroyContext(private->xdisplay, private->glxcontext);
    memset(context, 0, sizeof(GdkGLContextPrivate));
    g_free(context);
  }
}

gint gdk_gl_make_current(GdkDrawable *drawable, GdkGLContext *context)
{
  GdkGLContextPrivate *private = (GdkGLContextPrivate*)context;

  g_return_val_if_fail(drawable != NULL, FALSE);
  g_return_val_if_fail(context  != NULL, FALSE);

  return (glXMakeCurrent(private->xdisplay, GDK_WINDOW_XWINDOW(drawable), private->glxcontext) == True) ? TRUE : FALSE;
}

void gdk_gl_swap_buffers(GdkDrawable *drawable)
{
  g_return_if_fail(drawable != NULL);

  glXSwapBuffers(GDK_WINDOW_XDISPLAY(drawable), GDK_WINDOW_XWINDOW(drawable));
}

void gdk_gl_wait_gdk(void)
{
  glXWaitX();
}

void gdk_gl_wait_gl (void)
{
  glXWaitGL();
}


/* glpixmap stuff */

struct _GdkGLPixmapPrivate {
  Display   *xdisplay;
  GLXPixmap glxpixmap;
  GdkPixmap *front_left;
  guint     ref_count;
};

typedef struct _GdkGLPixmapPrivate GdkGLPixmapPrivate;


GdkGLPixmap *gdk_gl_pixmap_new(GdkVisual *visual, GdkPixmap *pixmap)
{
  Display *dpy;
  XVisualInfo *vi;
  Pixmap xpixmap;
  GdkGLPixmapPrivate *private;
  GLXPixmap glxpixmap;
  gint depth;

  g_return_val_if_fail(pixmap != NULL, NULL);
  g_return_val_if_fail(visual != NULL, NULL);
  g_return_val_if_fail(gdk_window_get_type(pixmap) == GDK_WINDOW_PIXMAP, NULL);

  gdk_window_get_geometry(pixmap, 0,0,0,0, &depth);
  g_return_val_if_fail(gdk_gl_get_config(visual, GDK_GL_BUFFER_SIZE) == depth, NULL);

  dpy = GDK_DISPLAY();

  vi = get_xvisualinfo(visual);
  xpixmap = ((GdkPixmapPrivate*)pixmap)->xwindow;
  glxpixmap = glXCreateGLXPixmap(dpy, vi, xpixmap);
  XFree(vi);

  g_return_val_if_fail(glxpixmap != None, NULL);

  private = g_new(GdkGLPixmapPrivate, 1);
  private->xdisplay  = dpy;
  private->glxpixmap = glxpixmap;
  private->front_left = gdk_pixmap_ref(pixmap);
  private->ref_count = 1;

  return (GdkGLPixmap*)private;
}


GdkGLPixmap *gdk_gl_pixmap_ref(GdkGLPixmap *glpixmap)
{
  GdkGLPixmapPrivate *private = (GdkGLPixmapPrivate*)glpixmap;

  g_return_val_if_fail(glpixmap != NULL, NULL);
  private->ref_count += 1;

  return glpixmap;
}

void gdk_gl_pixmap_unref(GdkGLPixmap *glpixmap)
{
  GdkGLPixmapPrivate *private = (GdkGLPixmapPrivate*)glpixmap;

  g_return_if_fail(glpixmap != NULL);

  if (private->ref_count > 1) {
    private->ref_count -= 1;
  } else {
    glXDestroyGLXPixmap(private->xdisplay, private->glxpixmap);
    glXWaitGL();
    gdk_pixmap_unref(private->front_left);
    glXWaitX();
    memset(glpixmap, 0, sizeof(GdkGLPixmapPrivate));
    g_free(glpixmap);
  }
}

gint gdk_gl_pixmap_make_current(GdkGLPixmap *glpixmap, GdkGLContext *context)
{
  Display  *dpy;
  GLXPixmap glxpixmap;
  GLXContext glxcontext;

  g_return_val_if_fail(glpixmap != NULL, FALSE);
  g_return_val_if_fail(context  != NULL, FALSE);

  dpy        = ((GdkGLContextPrivate*)context)->xdisplay;
  glxpixmap  = ((GdkGLPixmapPrivate*)glpixmap)->glxpixmap;
  glxcontext = ((GdkGLContextPrivate*)context)->glxcontext;

  return (glXMakeCurrent(dpy, glxpixmap, glxcontext) == True) ? TRUE : FALSE;
}

/* fonts */
void gdk_gl_use_gdk_font(GdkFont *font, int first, int count, int list_base)
{
  g_return_if_fail(font != NULL);
  glXUseXFont(gdk_font_id(font), first, count, list_base);
}

