------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2010-2022, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with System;
with System.Assertions; use System.Assertions;
with Ada.Unchecked_Conversion;

with Glib.Object; use Glib.Object;
with Glib.Values; use Glib.Values;

with Gtk.Arguments;  use Gtk.Arguments;
with Gtk.Handlers;

package body Gtkada.Printing is

   package Object_Callback is new Gtk.Handlers.Callback
     (Gtkada_Print_Operation_Record);

   package Object_Boolean_Return_Callback is new Gtk.Handlers.Return_Callback
     (Gtkada_Print_Operation_Record, Boolean);

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Begin_Print_Handler_Wrapper
     (Op   : access Gtkada_Print_Operation_Record'Class;
      Args : Glib.Values.GValues);
   --  Wrapper around callback for "begin-print".

   procedure Done_Handler_Wrapper
     (Op     : access Gtkada_Print_Operation_Record'Class;
      Args : Glib.Values.GValues);
   --  Wrapper around callback for "done".

   procedure Draw_Page_Handler_Wrapper
     (Op   : access Gtkada_Print_Operation_Record'Class;
      Args : Glib.Values.GValues);
   --  Wrapper around callback for "draw-page".

   procedure End_Print_Handler_Wrapper
     (Op   : access Gtkada_Print_Operation_Record'Class;
      Args : Glib.Values.GValues);
   --  Wrapper around callback for "end-print".

   function Paginate_Handler_Wrapper
     (Op   : access Gtkada_Print_Operation_Record'Class;
      Args : Glib.Values.GValues)
     return Boolean;
   --  Wrapper around callback for "paginate".

   function Preview_Handler_Wrapper
     (Op   : access Gtkada_Print_Operation_Record'Class;
      Args : Glib.Values.GValues)
     return Boolean;
   --  Wrapper around callback for "preview".

   procedure Request_Page_Setup_Handler_Wrapper
     (Op   : access Gtkada_Print_Operation_Record'Class;
      Args : Glib.Values.GValues);
   --  Wrapper around callback for "request-page-setup".

   procedure Status_Changed_Handler_Wrapper
     (Op   : access Gtkada_Print_Operation_Record'Class;
      Args : Glib.Values.GValues);
   --  Wrapper around callback for "status-changed".

   -----------------------------
   -- Default implementations --
   -----------------------------

   ---------------
   -- Draw_Page --
   ---------------

   procedure Draw_Page
     (Op          : access Gtkada_Print_Operation_Record;
      Context     : Gtk_Print_Context;
      Page_Number : Gint) is
   begin
      --  This procedure should be overridden.
      Raise_Assert_Failure ("Subprogram ""Draw_Page"" should be overridden");
   end Draw_Page;

   -----------------
   -- Begin_Print --
   -----------------

   procedure Begin_Print
     (Op      : access Gtkada_Print_Operation_Record;
      Context : Gtk_Print_Context) is
   begin
      --  Default implementation : do nothing
      null;
   end Begin_Print;

   ----------
   -- Done --
   ----------

   procedure Done
     (Op     : access Gtkada_Print_Operation_Record;
      Result : Gtk_Print_Operation_Result) is
   begin
      --  Default implementation : do nothing
      null;
   end Done;

   ---------------
   -- End_Print --
   ---------------

   procedure End_Print
     (Op      : access Gtkada_Print_Operation_Record;
      Context : Gtk_Print_Context) is
   begin
      --  Default implementation : do nothing
      null;
   end End_Print;

   --------------
   -- Paginate --
   --------------

   function Paginate
     (Op      : access Gtkada_Print_Operation_Record;
      Context : Gtk_Print_Context) return Boolean is
      pragma Unreferenced (Op, Context);
   begin
      --  Default implementation : do nothing
      return True;
   end Paginate;

   -------------
   -- Preview --
   -------------

   function Preview
     (Op          : access Gtkada_Print_Operation_Record;
      Preview     : Gtk_Print_Operation_Preview;
      Context     : Gtk_Print_Context;
      Parent      : Gtk_Window) return Boolean is
      pragma Unreferenced (Op, Preview, Context, Parent);
   begin
      --  Default implementation: do not override the Gtk+ preview mechanism.
      return False;
   end Preview;

   ------------------------
   -- Request_Page_Setup --
   ------------------------

   procedure Request_Page_Setup
     (Op          : access Gtkada_Print_Operation_Record;
      Context     : Gtk_Print_Context;
      Page_Number : Gint;
      Setup       : Gtk_Page_Setup) is
   begin
      --  Default implementation : do nothing
      null;
   end Request_Page_Setup;

   --------------------
   -- Status_Changed --
   --------------------

   procedure Status_Changed (Op : access Gtkada_Print_Operation_Record) is
   begin
      --  Default implementation : do nothing
      null;
   end Status_Changed;

   ---------------------------------
   -- Begin_Print_Handler_Wrapper --
   ---------------------------------

   procedure Begin_Print_Handler_Wrapper
     (Op   : access Gtkada_Print_Operation_Record'Class;
      Args : Glib.Values.GValues)
   is
      Context_Addr : constant System.Address := To_Address (Args, 1);
      Context_Stub : Gtk_Print_Context_Record;
      Context      : constant Gtk_Print_Context :=
        Gtk_Print_Context (Get_User_Data (Context_Addr, Context_Stub));
   begin
      Begin_Print (Op, Context);
   end Begin_Print_Handler_Wrapper;

   --------------------------
   -- Done_Handler_Wrapper --
   --------------------------

   procedure Done_Handler_Wrapper
     (Op     : access Gtkada_Print_Operation_Record'Class;
      Args : Glib.Values.GValues)
   is
      Result : constant Gtk_Print_Operation_Result :=
        Gtk_Print_Operation_Result'Val (Get_Enum (Nth (Args, 1)));
   begin
      Done (Op, Result);
   end Done_Handler_Wrapper;

   -------------------------------
   -- Draw_Page_Handler_Wrapper --
   -------------------------------

   procedure Draw_Page_Handler_Wrapper
     (Op   : access Gtkada_Print_Operation_Record'Class;
      Args : Glib.Values.GValues)
   is
      Context_Addr : constant System.Address := To_Address (Args, 1);
      Context_Stub : Gtk_Print_Context_Record;
      Context  : constant Gtk_Print_Context :=
        Gtk_Print_Context (Get_User_Data (Context_Addr, Context_Stub));
      Page_Num : constant Gint := To_Gint (Args, 2);

   begin
      Draw_Page (Op, Context, Page_Num);
   end Draw_Page_Handler_Wrapper;

   -------------------------------
   -- End_Print_Handler_Wrapper --
   -------------------------------

   procedure End_Print_Handler_Wrapper
     (Op   : access Gtkada_Print_Operation_Record'Class;
      Args : Glib.Values.GValues)
   is
      Context_Addr : constant System.Address := To_Address (Args, 1);
      Context_Stub : Gtk_Print_Context_Record;
      Context      : constant Gtk_Print_Context :=
        Gtk_Print_Context (Get_User_Data (Context_Addr, Context_Stub));
   begin
      End_Print (Op, Context);
   end End_Print_Handler_Wrapper;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Op : out Gtkada_Print_Operation) is
   begin
      Op := new Gtkada_Print_Operation_Record;
      Gtkada.Printing.Initialize (Op);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget : access Gtkada_Print_Operation_Record'Class) is
   begin
      Gtk.Print_Operation.Initialize (Widget);
   end Initialize;

   ------------------------------
   -- Paginate_Handler_Wrapper --
   ------------------------------

   function Paginate_Handler_Wrapper
     (Op   : access Gtkada_Print_Operation_Record'Class;
      Args : Glib.Values.GValues)
     return Boolean
   is
      Context_Addr : constant System.Address := To_Address (Args, 1);
      Context_Stub : Gtk_Print_Context_Record;
      Context      : constant Gtk_Print_Context :=
        Gtk_Print_Context (Get_User_Data (Context_Addr, Context_Stub));
   begin
      return Paginate (Op, Context);
   end Paginate_Handler_Wrapper;

   -----------------------------
   -- Preview_Handler_Wrapper --
   -----------------------------

   function Preview_Handler_Wrapper
     (Op   : access Gtkada_Print_Operation_Record'Class;
      Args : Glib.Values.GValues)
     return Boolean
   is
      --  ???  ugly, but otherwise correct.
      function To_Preview is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Print_Operation_Preview);
      Preview_Addr : constant System.Address := To_Address (Args, 1);
      Preview_Op   : constant Gtk_Print_Operation_Preview :=
        To_Preview (Preview_Addr);

      Context_Addr : constant System.Address := To_Address (Args, 2);
      Context_Stub : Gtk_Print_Context_Record;
      Context      : constant Gtk_Print_Context :=
        Gtk_Print_Context (Get_User_Data (Context_Addr, Context_Stub));

      Parent_Addr  : constant System.Address := To_Address (Args, 3);
      Parent_Stub  : Gtk_Window_Record;
      Parent       : constant Gtk_Window :=
        Gtk_Window (Get_User_Data (Parent_Addr, Parent_Stub));

   begin
      return Preview (Op, Preview_Op, Context, Parent);
   end Preview_Handler_Wrapper;

   ----------------------------------------
   -- Request_Page_Setup_Handler_Wrapper --
   ----------------------------------------

   procedure Request_Page_Setup_Handler_Wrapper
     (Op   : access Gtkada_Print_Operation_Record'Class;
      Args : Glib.Values.GValues)
   is
      Context_Addr : constant System.Address := To_Address (Args, 1);
      Context_Stub : Gtk_Print_Context_Record;
      Context      : constant Gtk_Print_Context :=
        Gtk_Print_Context (Get_User_Data (Context_Addr, Context_Stub));
      Page_Num     : constant Gint := To_Gint (Args, 2);
      Setup_Addr   : constant System.Address := To_Address (Args, 3);
      Setup_Stub   : Gtk_Page_Setup_Record;
      Setup        : constant Gtk_Page_Setup :=
         Gtk_Page_Setup (Get_User_Data (Setup_Addr, Setup_Stub));

   begin
      Request_Page_Setup (Op, Context, Page_Num, Setup);
   end Request_Page_Setup_Handler_Wrapper;

   ------------------------------------
   -- Status_Changed_Handler_Wrapper --
   ------------------------------------

   procedure Status_Changed_Handler_Wrapper
     (Op   : access Gtkada_Print_Operation_Record'Class;
      Args : Glib.Values.GValues)
   is
      pragma Unreferenced (Args);
   begin
      Status_Changed (Op);
   end Status_Changed_Handler_Wrapper;

   ---------
   -- Run --
   ---------

   function Connect_And_Run
     (Op        : access Gtkada_Print_Operation_Record'Class;
      Action    : Gtk_Print_Operation_Action;
      Parent    : access Gtk.Window.Gtk_Window_Record'Class;
      Error     : Glib.Error.GError := null)
      return Gtk_Print_Operation_Result
   is
      pragma Unreferenced (Error);
   begin
      --  Connect all handlers

      Object_Callback.Connect
        (Op, "begin-print", Begin_Print_Handler_Wrapper'Access);

      Object_Callback.Connect
        (Op, "done", Done_Handler_Wrapper'Access);

      Object_Callback.Connect
        (Op, "draw-page",
         Draw_Page_Handler_Wrapper'Access);

      Object_Callback.Connect
        (Op, "end-print", End_Print_Handler_Wrapper'Access);

      Object_Boolean_Return_Callback.Connect
        (Op, "paginate", Paginate_Handler_Wrapper'Access);

      Object_Boolean_Return_Callback.Connect
           (Op, "preview", Preview_Handler_Wrapper'Access);

      Object_Callback.Connect
        (Op, "request-page-setup",
         Request_Page_Setup_Handler_Wrapper'Access);

      Object_Callback.Connect
        (Op, "status-changed", Status_Changed_Handler_Wrapper'Access);

      return Gtk.Print_Operation.Run
        (Gtk_Print_Operation (Op), Action, Parent);
   end Connect_And_Run;

end Gtkada.Printing;
