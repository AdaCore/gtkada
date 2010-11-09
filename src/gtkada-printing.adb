-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                    Copyright (C) 2010, AdaCore                    --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Glib.Object; use Glib.Object;
with Glib.Values; use Glib.Values;

with Gtk.Arguments;  use Gtk.Arguments;
with Gtk.Handlers;

package body Gtkada.Printing is

   package Object_Callback is new Gtk.Handlers.Callback
     (Gtkada_Print_Operation_Record);

   package Object_Boolean_Return_Callback is new Gtk.Handlers.Return_Callback
     (Gtkada_Print_Operation_Record, Gboolean);

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
     return Gboolean;
   --  Wrapper around callback for "paginate".

   procedure Request_Page_Setup_Handler_Wrapper
     (Op   : access Gtkada_Print_Operation_Record'Class;
      Args : Glib.Values.GValues);
   --  Wrapper around callback for "request-page-setup".

   procedure Status_Changed_Handler_Wrapper
     (Op   : access Gtkada_Print_Operation_Record'Class;
      Args : Glib.Values.GValues);
   --  Wrapper around callback for "status-changed".

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
      if Op.Begin_Print /= null then
         Op.Begin_Print (Gtkada_Print_Operation (Op), Context);
      end if;
   end Begin_Print_Handler_Wrapper;

   --------------------------
   -- Done_Handler_Wrapper --
   --------------------------

   procedure Done_Handler_Wrapper
     (Op     : access Gtkada_Print_Operation_Record'Class;
      Args : Glib.Values.GValues)
   is
      Result : constant Gtk_Print_Operation_Result :=
        Gtk_Print_Operation_Result'Val (To_Guint (Args, 1));
   begin
      if Op.Done /= null then
         Op.Done (Gtkada_Print_Operation (Op), Result);
      end if;
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
      if Op.Draw_Page /= null then
         Op.Draw_Page (Gtkada_Print_Operation (Op), Context, Page_Num);
      end if;
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
      if Op.End_Print /= null then
         Op.End_Print (Gtkada_Print_Operation (Op), Context);
      end if;
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

   ---------------------------------
   -- Install_Begin_Print_Handler --
   ---------------------------------

   procedure Install_Begin_Print_Handler
     (Op      : access Gtkada_Print_Operation_Record'Class;
      Handler : Begin_Print_Handler)
   is
   begin
      Op.Begin_Print := Handler;
   end Install_Begin_Print_Handler;

   --------------------------
   -- Install_Done_Handler --
   --------------------------

   procedure Install_Done_Handler
     (Op      : access Gtkada_Print_Operation_Record'Class;
      Handler : Done_Handler)
   is
   begin
      Op.Done := Handler;
   end Install_Done_Handler;

   -------------------------------
   -- Install_Draw_Page_Handler --
   -------------------------------

   procedure Install_Draw_Page_Handler
     (Op      : access Gtkada_Print_Operation_Record'Class;
      Handler : Draw_Page_Handler)
   is
   begin
      Op.Draw_Page := Handler;
   end Install_Draw_Page_Handler;

   -------------------------------
   -- Install_End_Print_Handler --
   -------------------------------

   procedure Install_End_Print_Handler
     (Op      : access Gtkada_Print_Operation_Record'Class;
      Handler : End_Print_Handler)
   is
   begin
      Op.End_Print := Handler;
   end Install_End_Print_Handler;

   ------------------------------
   -- Install_Paginate_Handler --
   ------------------------------

   procedure Install_Paginate_Handler
     (Op      : access Gtkada_Print_Operation_Record'Class;
      Handler : Paginate_Handler)
   is
   begin
      Op.Paginate := Handler;
   end Install_Paginate_Handler;

   ----------------------------------------
   -- Install_Request_Page_Setup_Handler --
   ----------------------------------------

   procedure Install_Request_Page_Setup_Handler
     (Op      : access Gtkada_Print_Operation_Record'Class;
      Handler : Request_Page_Setup_Handler)
   is
   begin
      Op.Request_Page_Setup := Handler;
   end Install_Request_Page_Setup_Handler;

   ------------------------------------
   -- Install_Status_Changed_Handler --
   ------------------------------------

   procedure Install_Status_Changed_Handler
     (Op      : access Gtkada_Print_Operation_Record'Class;
      Handler : Status_Changed_Handler)
   is
   begin
      Op.Status_Changed := Handler;
   end Install_Status_Changed_Handler;

   ------------------------------
   -- Paginate_Handler_Wrapper --
   ------------------------------

   function Paginate_Handler_Wrapper
     (Op   : access Gtkada_Print_Operation_Record'Class;
      Args : Glib.Values.GValues)
     return Gboolean
   is
      Context_Addr : constant System.Address := To_Address (Args, 1);
      Context_Stub : Gtk_Print_Context_Record;
      Context      : constant Gtk_Print_Context :=
        Gtk_Print_Context (Get_User_Data (Context_Addr, Context_Stub));
   begin
      if Op.Paginate /= null then
         return Boolean'Pos
           (Op.Paginate (Gtkada_Print_Operation (Op), Context));
      else
         return Boolean'Pos (False);
      end if;
   end Paginate_Handler_Wrapper;

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
      User_Data    : constant System.Address := To_Address (Args, 4);

   begin
      if Op.Request_Page_Setup /= null then
         Op.Request_Page_Setup
           (Gtkada_Print_Operation (Op), Context, Page_Num, Setup, User_Data);
      end if;
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
      if Op.Status_Changed /= null then
         Op.Status_Changed (Gtkada_Print_Operation (Op));
      end if;
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
   begin
      --  Connect all handlers

      if Op.Begin_Print /= null then
         Object_Callback.Connect
           (Op, "begin-print", Begin_Print_Handler_Wrapper'Access);
      end if;

      if Op.Done /= null then
         Object_Callback.Connect (Op, "done", Done_Handler_Wrapper'Access);
      end if;

      if Op.Draw_Page /= null then
         Object_Callback.Connect
           (Op, "draw-page", Draw_Page_Handler_Wrapper'Access);
      end if;

      if Op.End_Print /= null then
         Object_Callback.Connect
           (Op, "end-print", End_Print_Handler_Wrapper'Access);
      end if;

      if Op.Paginate /= null then
         Object_Boolean_Return_Callback.Connect
           (Op, "paginate", Paginate_Handler_Wrapper'Access);
      end if;

      if Op.Request_Page_Setup /= null then
         Object_Callback.Connect
           (Op,
            "request-page-setup",
            Request_Page_Setup_Handler_Wrapper'Access);
      end if;

      if Op.Status_Changed /= null then
         Object_Callback.Connect
           (Op, "status-changed", Status_Changed_Handler_Wrapper'Access);
      end if;

      --  ??? Add other connections here

      return Gtk.Print_Operation.Run
        (Gtk_Print_Operation (Op), Action, Parent, Error);
   end Connect_And_Run;

end Gtkada.Printing;
