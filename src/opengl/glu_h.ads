--  Generated from glu.h
--  Date: Wed Sep 22 12:47:09 1999
--
with Interfaces.C.Extensions;
with gl_h;

package glu_h is
   --   $Id$
   --
   --  Mesa 3-D graphics library
   --  Version:  3.0
   --  Copyright (C) 1995-1998  Brian Paul
   --
   --  This library is free software; you can redistribute it and/or
   --  modify it under the terms of the GNU Library General Public
   --  License as published by the Free Software Foundation; either
   --  version 2 of the License, or (at your option) any later version.
   --
   --  This library is distributed in the hope that it will be useful,
   --  but WITHOUT ANY WARRANTY; without even the implied warranty of
   --  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   --  Library General Public License for more details.
   --
   --  You should have received a copy of the GNU Library General Public
   --  License along with this library; if not, write to the Free
   --  Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
   --

   --
   --  $Log$
   --  Revision 1.1  1999/09/24 08:37:15  briot
   --  These two files provide a very basic binding to the openGL library (they
   --  were generated from the Mesa files). These are mainly provided as examples,
   --  not as a full binding
   --
   --  Revision 3.2  1998/07/26 01:36:27  brianp
   --  changes for Windows compilation per Ted Jump
   --
   --  Revision 3.1  1998/06/23 00:33:08  brianp
   --  added some WIN32 APIENTRY, CALLBACK stuff (Eric Lassauge)
   --
   --  Revision 3.0  1998/02/20 05:06:01  brianp
   --  initial rev
   --

            --   to facilitate clean DLL building ...
     --  tag specifying we're building for DLL runtime support

   GLU_VERSION_1_1 : constant := 1;
   GLU_TRUE        : constant := 1;
   GLU_FALSE       : constant := 0;

   type GLUenum is new Integer;
   for GLUenum 'Size use 32;

   GLU_SMOOTH : constant GLUenum := 100000;
   GLU_FLAT : constant GLUenum := 100001;
   GLU_NONE : constant GLUenum := 100002;
   GLU_POINT : constant GLUenum := 100010;
   GLU_LINE : constant GLUenum := 100011;
   GLU_FILL : constant GLUenum := 100012;
   GLU_SILHOUETTE : constant GLUenum := 100013;
   GLU_OUTSIDE : constant GLUenum := 100020;
   GLU_INSIDE : constant GLUenum := 100021;
   GLU_BEGIN : constant GLUenum := 100100;
   GLU_VERTEX : constant GLUenum := 100101;
   GLU_END : constant GLUenum := 100102;
   GLU_ERROR : constant GLUenum := 100103;
   GLU_EDGE_FLAG : constant GLUenum := 100104;
   GLU_CW : constant GLUenum := 100120;
   GLU_CCW : constant GLUenum := 100121;
   GLU_INTERIOR : constant GLUenum := 100122;
   GLU_EXTERIOR : constant GLUenum := 100123;
   GLU_UNKNOWN : constant GLUenum := 100124;
   GLU_TESS_ERROR1 : constant GLUenum := 100151;
   GLU_TESS_ERROR2 : constant GLUenum := 100152;
   GLU_TESS_ERROR3 : constant GLUenum := 100153;
   GLU_TESS_ERROR4 : constant GLUenum := 100154;
   GLU_TESS_ERROR5 : constant GLUenum := 100155;
   GLU_TESS_ERROR6 : constant GLUenum := 100156;
   GLU_TESS_ERROR7 : constant GLUenum := 100157;
   GLU_TESS_ERROR8 : constant GLUenum := 100158;
   GLU_TESS_ERROR9 : constant GLUenum := 100159;
   GLU_AUTO_LOAD_MATRIX : constant GLUenum := 100200;
   GLU_CULLING : constant GLUenum := 100201;
   GLU_PARAMETRIC_TOLERANCE : constant GLUenum := 100202;
   GLU_SAMPLING_TOLERANCE : constant GLUenum := 100203;
   GLU_DISPLAY_MODE : constant GLUenum := 100204;
   GLU_SAMPLING_METHOD : constant GLUenum := 100205;
   GLU_U_STEP : constant GLUenum := 100206;
   GLU_V_STEP : constant GLUenum := 100207;
   GLU_PATH_LENGTH : constant GLUenum := 100215;
   GLU_PARAMETRIC_ERROR : constant GLUenum := 100216;
   GLU_DOMAIN_DISTANCE : constant GLUenum := 100217;
   GLU_MAP1_TRIM_2 : constant GLUenum := 100210;
   GLU_MAP1_TRIM_3 : constant GLUenum := 100211;
   GLU_OUTLINE_POLYGON : constant GLUenum := 100240;
   GLU_OUTLINE_PATCH : constant GLUenum := 100241;
   GLU_NURBS_ERROR1 : constant GLUenum := 100251;
   GLU_NURBS_ERROR2 : constant GLUenum := 100252;
   GLU_NURBS_ERROR3 : constant GLUenum := 100253;
   GLU_NURBS_ERROR4 : constant GLUenum := 100254;
   GLU_NURBS_ERROR5 : constant GLUenum := 100255;
   GLU_NURBS_ERROR6 : constant GLUenum := 100256;
   GLU_NURBS_ERROR7 : constant GLUenum := 100257;
   GLU_NURBS_ERROR8 : constant GLUenum := 100258;
   GLU_NURBS_ERROR9 : constant GLUenum := 100259;
   GLU_NURBS_ERROR10 : constant GLUenum := 100260;
   GLU_NURBS_ERROR11 : constant GLUenum := 100261;
   GLU_NURBS_ERROR12 : constant GLUenum := 100262;
   GLU_NURBS_ERROR13 : constant GLUenum := 100263;
   GLU_NURBS_ERROR14 : constant GLUenum := 100264;
   GLU_NURBS_ERROR15 : constant GLUenum := 100265;
   GLU_NURBS_ERROR16 : constant GLUenum := 100266;
   GLU_NURBS_ERROR17 : constant GLUenum := 100267;
   GLU_NURBS_ERROR18 : constant GLUenum := 100268;
   GLU_NURBS_ERROR19 : constant GLUenum := 100269;
   GLU_NURBS_ERROR20 : constant GLUenum := 100270;
   GLU_NURBS_ERROR21 : constant GLUenum := 100271;
   GLU_NURBS_ERROR22 : constant GLUenum := 100272;
   GLU_NURBS_ERROR23 : constant GLUenum := 100273;
   GLU_NURBS_ERROR24 : constant GLUenum := 100274;
   GLU_NURBS_ERROR25 : constant GLUenum := 100275;
   GLU_NURBS_ERROR26 : constant GLUenum := 100276;
   GLU_NURBS_ERROR27 : constant GLUenum := 100277;
   GLU_NURBS_ERROR28 : constant GLUenum := 100278;
   GLU_NURBS_ERROR29 : constant GLUenum := 100279;
   GLU_NURBS_ERROR30 : constant GLUenum := 100280;
   GLU_NURBS_ERROR31 : constant GLUenum := 100281;
   GLU_NURBS_ERROR32 : constant GLUenum := 100282;
   GLU_NURBS_ERROR33 : constant GLUenum := 100283;
   GLU_NURBS_ERROR34 : constant GLUenum := 100284;
   GLU_NURBS_ERROR35 : constant GLUenum := 100285;
   GLU_NURBS_ERROR36 : constant GLUenum := 100286;
   GLU_NURBS_ERROR37 : constant GLUenum := 100287;
   GLU_INVALID_ENUM : constant GLUenum := 100900;
   GLU_INVALID_VALUE : constant GLUenum := 100901;
   GLU_OUT_OF_MEMORY : constant GLUenum := 100902;
   GLU_INCOMPATIBLE_GL_VERSION : constant GLUenum := 100903;
   GLU_VERSION : constant GLUenum := 100800;
   GLU_EXTENSIONS : constant GLUenum := 100801;

            --   Normal vectors
            --   Quadric draw styles
            --   Quadric orientation
            --   Tesselator
            --   Contour types
            --   Tesselation errors
            --   NURBS
            --   Errors
            --   New in GLU 1.1
   --
   --  These are the GLU 1.1 typedefs.  GLU 1.2 has different ones!
   --

   type GLUquadricObj is new Interfaces.C.Extensions.opaque_structure_def;
   type GLUquadricObj_Ptr      is access GLUquadricObj;
   type GLUtriangulatorObj is new     Interfaces.C.Extensions.opaque_structure_def;
   type GLUtriangulatorObj_Ptr is access GLUtriangulatorObj;
   type GLUnurbsObj is new Interfaces.C.Extensions.opaque_structure_def;
   type GLUnurbsObj_Ptr        is access GLUnurbsObj;

   --
   --  Miscellaneous functions
   --

   procedure gluLookAt (eyex    : Gl_H.GLdouble;
                        eyey    : Gl_H.GLdouble;
                        eyez    : Gl_H.GLdouble;
                        centerx : Gl_H.GLdouble;
                        centery : Gl_H.GLdouble;
                        centerz : Gl_H.GLdouble;
                        upx     : Gl_H.GLdouble;
                        upy     : Gl_H.GLdouble;
                        upz     : Gl_H.GLdouble);
   procedure gluOrtho2D (left   : Gl_H.GLdouble;
                         right  : Gl_H.GLdouble;
                         bottom : Gl_H.GLdouble;
                         top    : Gl_H.GLdouble);
   procedure gluPerspective (fovy   : Gl_H.GLdouble;
                             aspect : Gl_H.GLdouble;
                             zNear  : Gl_H.GLdouble;
                             zFar   : Gl_H.GLdouble);
   procedure gluPickMatrix (x        : Gl_H.GLdouble;
                            y        : Gl_H.GLdouble;
                            width    : Gl_H.GLdouble;
                            height   : Gl_H.GLdouble;
                            viewport : Gl_H.GLint_Vec_4);
   function gluProject (objx        : Gl_H.GLdouble;
                        objy        : Gl_H.GLdouble;
                        objz        : Gl_H.GLdouble;
                        modelMatrix : Gl_H.GLdouble_Vec_16;
                        projMatrix  : Gl_H.GLdouble_Vec_16;
                        viewport    : Gl_H.GLint_Vec_4;
                        winx        : access Gl_H.GLdouble;
                        winy        : access Gl_H.GLdouble;
                        winz        : access Gl_H.GLdouble) return Gl_H.GLint;
   function gluUnProject
       (winx        : Gl_H.GLdouble;
        winy        : Gl_H.GLdouble;
        winz        : Gl_H.GLdouble;
        modelMatrix : Gl_H.GLdouble_Vec_16;
        projMatrix  : Gl_H.GLdouble_Vec_16;
        viewport    : Gl_H.GLint_Vec_4;
        objx        : access Gl_H.GLdouble;
        objy        : access Gl_H.GLdouble;
        objz        : access Gl_H.GLdouble) return Gl_H.GLint;
   function gluErrorString (errorCode : Gl_H.GLenum) return Gl_H.GLubyte_Ptr;

   --
   --  Mipmapping and image scaling
   --

   function gluScaleImage
       (format    : Gl_H.GLenum;
        widthin   : Gl_H.GLint;
        heightin  : Gl_H.GLint;
        typein    : Gl_H.GLenum;
        datain    : Interfaces.C.Extensions.Void_Ptr;
        widthout  : Gl_H.GLint;
        heightout : Gl_H.GLint;
        typeout   : Gl_H.GLenum;
        dataout   : Interfaces.C.Extensions.Void_Ptr) return Gl_H.GLint;
   function gluBuild1DMipmaps
       (target     : Gl_H.GLenum;
        components : Gl_H.GLint;
        width      : Gl_H.GLint;
        format     : Gl_H.GLenum;
        type_Id    : Gl_H.GLenum;
        data       : Interfaces.C.Extensions.Void_Ptr) return Gl_H.GLint;
   function gluBuild2DMipmaps
       (target     : Gl_H.GLenum;
        components : Gl_H.GLint;
        width      : Gl_H.GLint;
        height     : Gl_H.GLint;
        format     : Gl_H.GLenum;
        type_Id    : Gl_H.GLenum;
        data       : Interfaces.C.Extensions.Void_Ptr) return Gl_H.GLint;

   --
   --  Quadrics
   --

   function gluNewQuadric return GLUquadricObj_Ptr;
   procedure gluDeleteQuadric (state : access GLUquadricObj);
   procedure gluQuadricDrawStyle (quadObject : access GLUquadricObj;
                                  drawStyle  : Gl_H.GLenum);
   procedure gluQuadricOrientation (quadObject  : access GLUquadricObj;
                                    orientation : Gl_H.GLenum);
   procedure gluQuadricNormals (quadObject : access GLUquadricObj;
                                normals    : Gl_H.GLenum);
   procedure gluQuadricTexture (quadObject    : access GLUquadricObj;
                                textureCoords : Gl_H.GLboolean);

   type glu_h_proc_1 is access procedure;

   procedure gluQuadricCallback (qobj  : access GLUquadricObj;
                                 which : Gl_H.GLenum;
                                 fn    : glu_h_proc_1);
   procedure gluCylinder (qobj       : access GLUquadricObj;
                          baseRadius : Gl_H.GLdouble;
                          topRadius  : Gl_H.GLdouble;
                          height     : Gl_H.GLdouble;
                          slices     : Gl_H.GLint;
                          stacks     : Gl_H.GLint);
   procedure gluSphere (qobj   : access GLUquadricObj;
                        radius : Gl_H.GLdouble;
                        slices : Gl_H.GLint;
                        stacks : Gl_H.GLint);
   procedure gluDisk (qobj        : access GLUquadricObj;
                      innerRadius : Gl_H.GLdouble;
                      outerRadius : Gl_H.GLdouble;
                      slices      : Gl_H.GLint;
                      loops       : Gl_H.GLint);
   procedure gluPartialDisk (qobj        : access GLUquadricObj;
                             innerRadius : Gl_H.GLdouble;
                             outerRadius : Gl_H.GLdouble;
                             slices      : Gl_H.GLint;
                             loops       : Gl_H.GLint;
                             startAngle  : Gl_H.GLdouble;
                             sweepAngle  : Gl_H.GLdouble);

   --
   --  Nurbs
   --

   function gluNewNurbsRenderer return GLUnurbsObj_Ptr;
   procedure gluDeleteNurbsRenderer (nobj : access GLUnurbsObj);
   procedure gluLoadSamplingMatrices (nobj        : access GLUnurbsObj;
                                      modelMatrix : Gl_H.GLfloat_Vec_16;
                                      projMatrix  : Gl_H.GLfloat_Vec_16;
                                      viewport    : Gl_H.GLint_Vec_4);
   procedure gluNurbsProperty (nobj     : access GLUnurbsObj;
                               property : Gl_H.GLenum;
                               value    : Gl_H.GLfloat);
   procedure gluGetNurbsProperty (nobj     : access GLUnurbsObj;
                                  property : Gl_H.GLenum;
                                  value    : access Gl_H.GLfloat);
   procedure gluBeginCurve (nobj : access GLUnurbsObj);
   procedure gluEndCurve (nobj : access GLUnurbsObj);
   procedure gluNurbsCurve (nobj     : access GLUnurbsObj;
                            nknots   : Gl_H.GLint;
                            knot     : access Gl_H.GLfloat;
                            stride   : Gl_H.GLint;
                            ctlarray : access Gl_H.GLfloat;
                            order    : Gl_H.GLint;
                            type_Id  : Gl_H.GLenum);
   procedure gluBeginSurface (nobj : access GLUnurbsObj);
   procedure gluEndSurface (nobj : access GLUnurbsObj);
   procedure gluNurbsSurface (nobj        : access GLUnurbsObj;
                              sknot_count : Gl_H.GLint;
                              sknot       : access Gl_H.GLfloat;
                              tknot_count : Gl_H.GLint;
                              tknot       : access Gl_H.GLfloat;
                              s_stride    : Gl_H.GLint;
                              t_stride    : Gl_H.GLint;
                              ctlarray    : access Gl_H.GLfloat;
                              sorder      : Gl_H.GLint;
                              torder      : Gl_H.GLint;
                              type_Id     : Gl_H.GLenum);
   procedure gluBeginTrim (nobj : access GLUnurbsObj);
   procedure gluEndTrim (nobj : access GLUnurbsObj);
   procedure gluPwlCurve (nobj     : access GLUnurbsObj;
                          count    : Gl_H.GLint;
                          array_Id : access Gl_H.GLfloat;
                          stride   : Gl_H.GLint;
                          type_Id  : Gl_H.GLenum);

   type glu_h_proc_2 is access procedure;

   procedure gluNurbsCallback (nobj  : access GLUnurbsObj;
                               which : Gl_H.GLenum;
                               fn    : glu_h_proc_2);

   --
   --  Polygon tesselation
   --

   function gluNewTess return GLUtriangulatorObj_Ptr;

   type glu_h_proc_3 is access procedure;

   procedure gluTessCallback (tobj  : access GLUtriangulatorObj;
                              which : Gl_H.GLenum;
                              fn    : glu_h_proc_3);
   procedure gluDeleteTess (tobj : access GLUtriangulatorObj);
   procedure gluBeginPolygon (tobj : access GLUtriangulatorObj);
   procedure gluEndPolygon (tobj : access GLUtriangulatorObj);
   procedure gluNextContour (tobj    : access GLUtriangulatorObj;
                             type_Id : Gl_H.GLenum);
   procedure gluTessVertex (tobj : access GLUtriangulatorObj;
                            v    : Gl_H.GLdouble_Vec_3;
                            data : Interfaces.C.Extensions.Void_Ptr);

   --
   --  New functions in GLU 1.1
   --

   function gluGetString (name : Gl_H.GLenum) return Gl_H.GLubyte_Ptr;

private

   pragma Import (C, gluLookAt, "gluLookAt");
   pragma Import (C, gluOrtho2D, "gluOrtho2D");
   pragma Import (C, gluPerspective, "gluPerspective");
   pragma Import (C, gluPickMatrix, "gluPickMatrix");
   pragma Import (C, gluProject, "gluProject");
   pragma Import (C, gluUnProject, "gluUnProject");
   pragma Import (C, gluErrorString, "gluErrorString");
   pragma Import (C, gluScaleImage, "gluScaleImage");
   pragma Import (C, gluBuild1DMipmaps, "gluBuild1DMipmaps");
   pragma Import (C, gluBuild2DMipmaps, "gluBuild2DMipmaps");
   pragma Import (C, gluNewQuadric, "gluNewQuadric");
   pragma Import (C, gluDeleteQuadric, "gluDeleteQuadric");
   pragma Import (C, gluQuadricDrawStyle, "gluQuadricDrawStyle");
   pragma Import (C, gluQuadricOrientation, "gluQuadricOrientation");
   pragma Import (C, gluQuadricNormals, "gluQuadricNormals");
   pragma Import (C, gluQuadricTexture, "gluQuadricTexture");
   pragma Import (C, gluQuadricCallback, "gluQuadricCallback");
   pragma Import (C, gluCylinder, "gluCylinder");
   pragma Import (C, gluSphere, "gluSphere");
   pragma Import (C, gluDisk, "gluDisk");
   pragma Import (C, gluPartialDisk, "gluPartialDisk");
   pragma Import (C, gluNewNurbsRenderer, "gluNewNurbsRenderer");
   pragma Import (C, gluDeleteNurbsRenderer, "gluDeleteNurbsRenderer");
   pragma Import (C, gluLoadSamplingMatrices, "gluLoadSamplingMatrices");
   pragma Import (C, gluNurbsProperty, "gluNurbsProperty");
   pragma Import (C, gluGetNurbsProperty, "gluGetNurbsProperty");
   pragma Import (C, gluBeginCurve, "gluBeginCurve");
   pragma Import (C, gluEndCurve, "gluEndCurve");
   pragma Import (C, gluNurbsCurve, "gluNurbsCurve");
   pragma Import (C, gluBeginSurface, "gluBeginSurface");
   pragma Import (C, gluEndSurface, "gluEndSurface");
   pragma Import (C, gluNurbsSurface, "gluNurbsSurface");
   pragma Import (C, gluBeginTrim, "gluBeginTrim");
   pragma Import (C, gluEndTrim, "gluEndTrim");
   pragma Import (C, gluPwlCurve, "gluPwlCurve");
   pragma Import (C, gluNurbsCallback, "gluNurbsCallback");
   pragma Import (C, gluNewTess, "gluNewTess");
   pragma Import (C, gluTessCallback, "gluTessCallback");
   pragma Import (C, gluDeleteTess, "gluDeleteTess");
   pragma Import (C, gluBeginPolygon, "gluBeginPolygon");
   pragma Import (C, gluEndPolygon, "gluEndPolygon");
   pragma Import (C, gluNextContour, "gluNextContour");
   pragma Import (C, gluTessVertex, "gluTessVertex");
   pragma Import (C, gluGetString, "gluGetString");

end glu_h;

