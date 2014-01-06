/*
 * Copyright (C) 2001-2014, AdaCore
 * Copyright (C) 1998 Janne L�f <jlof@mail.student.oulu.fi>
 *           (c) 2008, 2009 Sam Hocevar <sam@hocevar.net>
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

#include <string.h>

#include "gdkgl.h"

#include <GL/gl.h>
#if defined GDK_WINDOWING_WIN32
#   include <gdk/gdkwin32.h>
#   define PLATFORM "GDK_WINDOWING_WIN32"
#elif defined GDK_WINDOWING_X11
#   include <gdk/gdkx.h>
#   include <GL/glx.h>
#   define PLATFORM "GDK_WINDOWING_X11"
#elif defined GDK_WINDOWING_FB
#   define PLATFORM "GDK_WINDOWING_FB"
#elif defined GDK_WINDOWING_QUARTZ
#   define PLATFORM "GDK_WINDOWING_QUARTZ"
#elif defined GDK_WINDOWING_DIRECTFB
#   define PLATFORM "GDK_WINDOWING_DIRECTFB"
#endif

/*
 *  The GdkGLContext class
 */
struct _GdkGLContext {
  GObject     parent;
#if defined GDK_WINDOWING_WIN32
  gboolean  initialised;
  HGLRC     hglrc;
  HDC       hdc;
  HWND      hwnd;
  GdkGLContext *share;
  PIXELFORMATDESCRIPTOR pfd;
#elif defined GDK_WINDOWING_X11
  Display    *xdisplay;
  GLXContext  glxcontext;
#endif
};

struct _GdkGLContextClass {
  GObjectClass parent_class;
};
typedef struct _GdkGLContextClass GdkGLContextClass;

static GObjectClass *glcontext_parent_class;
static void gdk_gl_context_class_init (GdkGLContextClass *class);

/*
 *  Local helper functions
 */
#if defined GDK_WINDOWING_WIN32
static void fill_pfd(PIXELFORMATDESCRIPTOR *pfd, int *attriblist);
#elif defined GDK_WINDOWING_X11
static XVisualInfo *get_xvisualinfo(GdkVisual *visual);
#endif


/*
 *  Generic GL support
 */

gint gdk_gl_query(void)
{
#if defined GDK_WINDOWING_WIN32
  return TRUE;
#elif defined GDK_WINDOWING_X11
  return (glXQueryExtension(gdk_x11_get_default_xdisplay(),NULL,NULL) == True) ? TRUE : FALSE;
#else
  return FALSE;
#endif
}


gchar *gdk_gl_get_info()
{
  char const *vendor, *version, *extensions;
#if defined GDK_WINDOWING_WIN32
  vendor = glGetString (GL_VENDOR);
  version = glGetString (GL_VERSION);
  extensions = glGetString (GL_EXTENSIONS);
#elif defined GDK_WINDOWING_X11
  vendor = glXGetClientString(gdk_x11_get_default_xdisplay(), GLX_VENDOR);
  version = glXGetClientString(gdk_x11_get_default_xdisplay(), GLX_VERSION);
  extensions = glXGetClientString(gdk_x11_get_default_xdisplay(), GLX_EXTENSIONS);
#else
  vendor = version = extensions = "unknown";
#endif
  return g_strdup_printf("VENDOR     : %s\n"
                         "VERSION    : %s\n"
                         "EXTENSIONS : %s\n",
                         vendor, version, extensions);
}


GdkVisual *gdk_gl_choose_visual(int *attrlist)
{
#if defined GDK_WINDOWING_WIN32
  return gdk_visual_get_system ();
#elif defined GDK_WINDOWING_X11
  Display *dpy;
  XVisualInfo *vi;
  GdkVisual *visual;

  g_return_val_if_fail(attrlist != NULL, NULL);

  dpy = gdk_x11_get_default_xdisplay();
  vi = glXChooseVisual(dpy, DefaultScreen(dpy), attrlist);
  if (!vi)
    return NULL;

  visual = gdk_x11_screen_lookup_visual(gdk_screen_get_default(), vi->visualid);
  XFree(vi);
  return visual;
#else
  g_warning ("gdk_gl_choose_visual not implemented on " PLATFORM);
  return NULL;
#endif
}


int gdk_gl_get_config(GdkVisual *visual, int attrib)
{
#if defined GDK_WINDOWING_X11
  Display *dpy;
  XVisualInfo *vi;
  int value;

  g_return_val_if_fail(visual != NULL, -1);

  dpy = gdk_x11_get_default_xdisplay();

  vi = get_xvisualinfo(visual);

  if (glXGetConfig(dpy, vi, attrib, &value) == 0)
    {
      XFree(vi);
      return value;
    }
  XFree(vi);
  return -1;
#else
  g_warning ("gdk_gl_get_config not implemented on " PLATFORM);
  return 0;
#endif
}


/*
 *  GL context support
 */

GType
gdk_gl_context_get_type (void)
{
  static GType object_type = 0;

  if (!object_type)
    {
      static const GTypeInfo object_info =
      {
        sizeof (GdkGLContextClass),
        (GBaseInitFunc) NULL,
        (GBaseFinalizeFunc) NULL,
        (GClassInitFunc) gdk_gl_context_class_init,
        NULL,           /* class_finalize */
        NULL,           /* class_data */
        sizeof (GdkGLContext),
        0,              /* n_preallocs */
        (GInstanceInitFunc) NULL,
      };

      object_type = g_type_register_static (G_TYPE_OBJECT,
                                            "GdkGLContext",
                                            &object_info, 0);
    }
  return object_type;
}

static void
gdk_gl_context_finalize(GObject *object)
{
  GdkGLContext *context;

  context = GDK_GL_CONTEXT(object);

#if defined GDK_WINDOWING_WIN32
  if (context->hglrc == wglGetCurrentContext ())
    wglMakeCurrent (NULL, NULL);

  wglDeleteContext (context->hglrc);

  if (context->hwnd)
    ReleaseDC (context->hwnd, context->hdc);
  else
    DeleteDC (context->hdc);
#elif defined GDK_WINDOWING_X11
  if (context->glxcontext) {
    if (context->glxcontext == glXGetCurrentContext())
      glXMakeCurrent(context->xdisplay, None, NULL);

    glXDestroyContext(context->xdisplay, context->glxcontext);
  }
  context->glxcontext = NULL;
#endif

  (* glcontext_parent_class->finalize)(object);
}


static void
gdk_gl_context_class_init(GdkGLContextClass *class)
{
  GObjectClass *gobject_class;

  gobject_class = G_OBJECT_CLASS(class);
  glcontext_parent_class = g_type_class_peek_parent(class);

  gobject_class->finalize = gdk_gl_context_finalize;
}


GdkGLContext *
gdk_gl_context_new(GdkVisual *visual)
{
#if defined GDK_WINDOWING_WIN32 || defined GDK_WINDOWING_X11
  return gdk_gl_context_share_new(visual, NULL, FALSE);
#else
  g_warning ("gdk_gl_context_new not implemented on " PLATFORM);
  return NULL;
#endif
}


GdkGLContext *
gdk_gl_context_share_new(GdkVisual *visual, GdkGLContext *sharelist, gint direct)
{
#if defined GDK_WINDOWING_WIN32
  GdkGLContext *context;
#elif defined GDK_WINDOWING_X11
  Display *dpy;
  XVisualInfo *vi;
  GLXContext glxcontext;
  GdkGLContext *context;
#else
  g_warning ("gdk_gl_context_share_new not implemented on " PLATFORM);
  return NULL;
#endif

  g_return_val_if_fail (visual != NULL, NULL);

  context = g_object_new(GDK_TYPE_GL_CONTEXT, NULL);
  if (!context)
    return NULL;

#if defined GDK_WINDOWING_WIN32
  context->initialised = FALSE;
  context->hglrc   = NULL;
  context->hdc     = NULL;
  context->hwnd    = NULL;
  context->share   = sharelist ? g_object_ref(sharelist) : NULL;

  memset (&(context->pfd), 0, sizeof(PIXELFORMATDESCRIPTOR));

  /* if direct is TRUE, we create a context which renders to the screen,
     otherwise we create one to render to an offscreen bitmap */
  context->pfd.nSize = sizeof(PIXELFORMATDESCRIPTOR);
  context->pfd.nVersion = 1;
  if (direct)
    context->pfd.dwFlags = PFD_SUPPORT_OPENGL | PFD_DRAW_TO_WINDOW | PFD_DOUBLEBUFFER;
  else
    context->pfd.dwFlags = PFD_SUPPORT_OPENGL | PFD_DRAW_TO_BITMAP | PFD_SUPPORT_GDI;
  context->pfd.iPixelType = PFD_TYPE_RGBA;
  context->pfd.cColorBits = 24;
  context->pfd.cDepthBits = 32;
  context->pfd.iLayerType = PFD_MAIN_PLANE;
#elif defined GDK_WINDOWING_X11
  dpy = gdk_x11_get_default_xdisplay();

  vi = get_xvisualinfo(visual);

  glxcontext = glXCreateContext(dpy, vi, sharelist ? sharelist->glxcontext : 0,
                                direct ? True : False);

  XFree(vi);
  if (glxcontext == NULL) {
    g_object_unref(context);
    return NULL;
  }

  context->xdisplay = dpy;
  context->glxcontext = glxcontext;
#endif

  return context;
}

GdkGLContext *gdk_gl_context_attrlist_share_new(int *attrlist, GdkGLContext *sharelist, gint direct)
{
#if defined GDK_WINDOWING_WIN32
  GdkGLContext *context;
#elif defined GDK_WINDOWING_X11
  GdkVisual *visual;
#else
  g_warning ("gdk_gl_context_attrlist_share_new not implemented on " PLATFORM);
  return NULL;
#endif

  g_return_val_if_fail(attrlist != NULL, NULL);

#if defined GDK_WINDOWING_WIN32
  context = g_object_new(GDK_TYPE_GL_CONTEXT, NULL);
  if (!context)
    return NULL;

  context->initialised = FALSE;
  context->hglrc    = NULL;
  context->hdc      = NULL;
  context->hwnd     = NULL;
  context->share    = sharelist ? g_object_ref(sharelist) : NULL;
  fill_pfd(&context->pfd, attrlist);

  return context;
#elif defined GDK_WINDOWING_X11
  visual = gdk_gl_choose_visual(attrlist);
  if (!visual)
    return NULL;

  return gdk_gl_context_share_new(visual, sharelist, direct);
#endif
}


gint gdk_gl_make_current(GdkWindow *window, GdkGLContext *context)
{
//  g_return_val_if_fail (GDK_IS_DRAWABLE(drawable), FALSE);
//  g_return_val_if_fail (GDK_IS_GL_CONTEXT(context), FALSE);

#if defined GDK_WINDOWING_WIN32
  if (!context->initialised)
  {
    int pf;
    HWND hwnd = (HWND) gdk_win32_drawable_get_handle (drawable);

    context->hdc = GetDC (hwnd);

    pf = ChoosePixelFormat (context->hdc, &context->pfd);

    if (pf != 0)
      {
        SetPixelFormat (context->hdc, pf, &context->pfd);
        context->hglrc = wglCreateContext (context->hdc);
      }

    if (context->share)
      {
        if (context->share->hglrc)
          {
            if (wglShareLists (context->share->hglrc, context->hglrc) != TRUE)
                g_warning ("failed sharing context");
          }
        g_object_unref (context->share);
      }

    context->initialised = TRUE;
  }

  g_return_val_if_fail (context->hdc    != NULL, FALSE);
  g_return_val_if_fail (context->hglrc  != NULL, FALSE);

  wglMakeCurrent (context->hdc, context->hglrc);

  return TRUE;
#elif defined GDK_WINDOWING_X11
  return (glXMakeCurrent(context->xdisplay, gdk_x11_window_get_xid (window),
			 context->glxcontext) == True) ? TRUE : FALSE;

#if 0
  if (context->glxcontext != None && context->glxcontext == glXGetCurrentContext())
    {
      glFlush();
      return TRUE;
    }
  else
    {
      return (glXMakeCurrent(context->xdisplay, GDK_WINDOW_XWINDOW(drawable), context->glxcontext) == True) ? TRUE : FALSE;
    }
#endif
#else
  g_warning ("gdk_gl_make_current not implemented on " PLATFORM);
#endif
}

void gdk_gl_swap_buffers(GdkWindow *window)
{
#if defined GDK_WINDOWING_WIN32
  HDC   hdc;
  HWND  hwnd;
#endif

  //g_return_if_fail (GDK_IS_DRAWABLE(drawable));

#if defined GDK_WINDOWING_WIN32
  hwnd = (HWND) gdk_win32_drawable_get_handle (drawable);
  hdc  = GetDC (hwnd);
  if (hdc  == NULL)
  {
     g_warning ("gdk_gl_swap_buffers: GetDC failed");
     return;
  }
  SwapBuffers (hdc);
  ReleaseDC (hwnd, hdc);
#elif defined GDK_WINDOWING_X11
  glXSwapBuffers(GDK_WINDOW_XDISPLAY(window), gdk_x11_window_get_xid(window));
#else
  g_warning ("gdk_gl_swap_buffers not implemented on " PLATFORM);
#endif
}

void gdk_gl_wait_gdk(void)
{
#if defined GDK_WINDOWING_WIN32
  GdiFlush();
#elif defined GDK_WINDOWING_X11
  glXWaitX();
#endif
}

void gdk_gl_wait_gl (void)
{
#if defined GDK_WINDOWING_WIN32
  glFinish();
#elif defined GDK_WINDOWING_X11
  glXWaitGL();
#endif
}

/*
 *  Helper functions
 */

#if defined GDK_WINDOWING_WIN32
static void fill_pfd(PIXELFORMATDESCRIPTOR *pfd, int *attriblist)
{
  /*
   * Ripped from glut's win32_x11.c
   */

  int *p = attriblist;

  memset(pfd, 0, sizeof(PIXELFORMATDESCRIPTOR));
  pfd->nSize = (sizeof(PIXELFORMATDESCRIPTOR));
  pfd->nVersion = 1;

  /* Defaults. */
  pfd->dwFlags = PFD_SUPPORT_OPENGL | PFD_DRAW_TO_WINDOW;
  pfd->iPixelType = PFD_TYPE_COLORINDEX;
  pfd->cColorBits = 32;
  pfd->cDepthBits = 0;
  pfd->cAccumBits = 0;

  while (*p) {
    switch (*p) {
    case GDK_GL_USE_GL:
      pfd->dwFlags |= PFD_SUPPORT_OPENGL;
      break;
    case GDK_GL_BUFFER_SIZE:
      pfd->cColorBits = *(++p);
      break;
    case GDK_GL_LEVEL:
      /* the bReserved flag of the pfd contains the
         overlay/underlay info. */
      pfd->bReserved = *(++p);
      break;
    case GDK_GL_RGBA:
      pfd->iPixelType = PFD_TYPE_RGBA;
      break;
    case GDK_GL_DOUBLEBUFFER:
      pfd->dwFlags |= PFD_DOUBLEBUFFER;
      break;
    case GDK_GL_STEREO:
      pfd->dwFlags |= PFD_STEREO;
      break;
    case GDK_GL_AUX_BUFFERS:
      pfd->cAuxBuffers = *(++p);
      break;
    case GDK_GL_RED_SIZE:
      pfd->cRedBits = 8; /* Try to get the maximum. */
      ++p;
      break;
    case GDK_GL_GREEN_SIZE:
      pfd->cGreenBits = 8;
      ++p;
      break;
    case GDK_GL_BLUE_SIZE:
      pfd->cBlueBits = 8;
      ++p;
      break;
    case GDK_GL_ALPHA_SIZE:
      pfd->cAlphaBits = 8;
      ++p;
      break;
    case GDK_GL_DEPTH_SIZE:
      pfd->cDepthBits = 32;
      ++p;
      break;
    case GDK_GL_STENCIL_SIZE:
      pfd->cStencilBits = *(++p);
      break;
    case GDK_GL_ACCUM_RED_SIZE:
    case GDK_GL_ACCUM_GREEN_SIZE:
    case GDK_GL_ACCUM_BLUE_SIZE:
    case GDK_GL_ACCUM_ALPHA_SIZE:
      /* I believe that WGL only used the cAccumRedBits,
         cAccumBlueBits, cAccumGreenBits, and cAccumAlphaBits fields
         when returning info about the accumulation buffer precision.
         Only cAccumBits is used for requesting an accumulation
         buffer. */
      pfd->cAccumBits += *(++p);
                break;
    }
    ++p;
  }
}


#elif defined GDK_WINDOWING_X11
static XVisualInfo *get_xvisualinfo(GdkVisual *visual)
{
  Display *dpy;
  XVisualInfo vinfo_template;
  XVisualInfo *vi;
  int nitems_return;

  dpy = gdk_x11_get_default_xdisplay();

  /* 'GLX uses VisualInfo records because they uniquely identify
   * a (VisualID,screen,depth) tuple.'
   */
  vinfo_template.visual   = GDK_VISUAL_XVISUAL(visual);
  vinfo_template.visualid = XVisualIDFromVisual(vinfo_template.visual);
  vinfo_template.depth    = gdk_visual_get_depth (visual);
  vinfo_template.screen   = DefaultScreen(dpy);
  vi = XGetVisualInfo(dpy, VisualIDMask|VisualDepthMask|VisualScreenMask,
		      &vinfo_template, &nitems_return);

  g_assert(vi!=0  && nitems_return==1); /* visualinfo needs to be unique */

  /* remember to XFree returned XVisualInfo !!! */
  return vi;
}
#endif

